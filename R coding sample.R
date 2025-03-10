# Set the random seed
set.seed(123)

# Generate data
n <- 50
x <- rnorm(n, mean = 1, sd = 4)  # x ~ N(1, 4^2)
epsilon <- rnorm(n, mean = 0, sd = 1)  # epsilon ~ N(0, 1)

# Calculate y = 2 + (1/18)*x^2 + epsilon
y <- 2 + (1/18) * x^2 + epsilon

# Question 3.1
# Remove noise (ignore epsilon)
y_hat <- 2 + (1/18) * x^2  

# Set save path
save_path <- "/Users/alan/Desktop/海外申请/Coding test/OI/Junbo_Datatask/Junbo_Datatask_Part3/Junbo_Datatask_Part3_figure/Question 1.png"  

# Set resolution and size
png(save_path, width = 3200, height = 2400, res = 300)

# Plot scatter plot
plot(x, y, main = "Question 3.1: Scatter plot", 
     xlab = "x", ylab = "y", pch = 19, col = rgb(0, 0, 1, 0.6),  # Blue, with transparency
     cex = 1.5,  # Adjust the point size
     xlim = c(min(x) - 1, max(x) + 1), ylim = c(min(y) - 1, max(y) + 1))  # Expand coordinate range

# Add y without noise (red)
points(x, y_hat, pch = 19, col = rgb(1, 0, 0, 0.6), cex = 1.5)  # Red, with transparency

# Add legend
legend("topleft", legend = c("y with epsilon", "y without epsilon"),
       col = c(rgb(0, 0, 1, 0.6), rgb(1, 0, 0, 0.6)), pch = 19, cex = 1.2)

dev.off()

# Question 3.2
# Fit the linear regression model
lm_model <- lm(y ~ x)

# Extract the estimated coefficients (beta_hat)
beta_hat <- coef(lm_model)

# Output the results
cat("Estimated beta_hat (from regression) is:", beta_hat[2], "\n")

# Question 3.3
# Set seed
set.seed(123)

# Set the number of simulations
n_1000 <- 1000

# Store the estimated beta_hat values and whether each regression is significant
beta_hat_values <- numeric(n_1000) 
significance_values <- logical(n_1000) 

# Run the simulation loop
for (i in 1:n_1000) {
  # Generate 50 x and y values
  x_sim <- rnorm(50, mean = 1, sd = 4)
  epsilon_sim <- rnorm(50, mean = 0, sd = 1)
  y_sim <- 2 + (1/18) * x_sim^2 + epsilon_sim
  
  # Run linear regression
  lm_sim <- lm(y_sim ~ x_sim)
  
  # Extract the beta_hat value
  beta_hat_values[i] <- coef(lm_sim)[2]
  
  # Check if beta_hat is significant at the 0.05 significance level
  p_value <- summary(lm_sim)$coefficients[2, 4]  # [2, 4] extracts the p-value for the x coefficient
  
  significance_values[i] <- p_value < 0.05
}

# Calculate the proportion of times beta_hat is marked as significant at the 0.05 significance level
significant_level <- mean(significance_values) * 100

# Output the significance proportion
cat("The percentage of times beta_hat is marked as significant at the 0.05 significance level is:", significant_level, "%\n")

# Set save path
save_path <- "/Users/alan/Desktop/海外申请/Coding test/OI/Junbo_Datatask/Junbo_Datatask_Part3/Junbo_Datatask_Part3_figure/Question 3 histogram.png"

# Set resolution and size
png(save_path, width = 3200, height = 2400, res = 300)


# Generate a histogram of beta_hat
hist(beta_hat_values, main = "Distribution of beta_hat across 1000 Simulations", 
     xlab = "Estimated beta", col = "lightblue", border = "black", breaks = 30)
# Add a vertical line indicating the beta value calculated in Question 2
abline(v = 1/9, col = "red", lwd = 2)
dev.off()

# Set save path
save_path <- "/Users/alan/Desktop/海外申请/Coding test/OI/Junbo_Datatask/Junbo_Datatask_Part3/Junbo_Datatask_Part3_figure/Question 3 qqplot.png"

# Set resolution and size
png(save_path, width = 3200, height = 2400, res = 300)

# Shapiro-Wilk test (check if the beta_hat distribution is normal)
shapiro_test <- shapiro.test(beta_hat_values)
cat("The p-value of the Shapiro-Wilk test is:", shapiro_test$p.value, "\n")

# Generate a Q-Q plot
qqnorm(beta_hat_values, main = "Q-Q Plot for beta_hat")
qqline(beta_hat_values, col = "red")
dev.off()

# Question 4
# Set the seed
set.seed(123)

# Define the sample sizes
sample_sizes <- c(16, 32, 64, 128, 256, 512, 1024, 2048)

# Save the significance percentage data
significant_percentages <- numeric(length(sample_sizes))

# Run the simulation
for (j in 1:length(sample_sizes)) {
  n <- sample_sizes[j]  # Current sample size
  significance_level <- logical(1000)  # To store whether each regression is significant
  
  for (i in 1:1000) {
    # Generate 1000 x and y values
    x_sim <- rnorm(n, mean = 1, sd = 4)
    epsilon_sim <- rnorm(n, mean = 0, sd = 1)
    y_sim <- 2 + (1/18) * x_sim^2 + epsilon_sim
    
    # Run the linear regression
    lm_sim <- lm(y_sim ~ x_sim)
    
    # Check if beta_hat is significant at the 0.05 significance level
    p_value <- summary(lm_sim)$coefficients[2, 4]
    significance_level[i] <- p_value < 0.05
  }
  
  # Calculate the percentage of significance
  significant_percentages[j] <- mean(significance_level) * 100
}

# Set save path
save_path <- "/Users/alan/Desktop/海外申请/Coding test/OI/Junbo_Datatask/Junbo_Datatask_Part3/Junbo_Datatask_Part3_figure/Question 4.png"

# Set resolution and size
png(save_path, width = 3200, height = 2400, res = 300)

# Plot the relationship between sample size and significance percentage
plot(sample_sizes, significant_percentages, type = "b", 
     main = "Impact of Sample Size on Statistical Significance", 
     xlab = "Sample Size", ylab = "Percentage of Significance", 
     col = "blue", pch = 19)
dev.off()
