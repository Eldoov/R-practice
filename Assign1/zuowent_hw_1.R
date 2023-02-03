# """
# Zuowen Tang
# Class: CS555 A3 SPRG23
# Date: Feb 2, 2023
# Homework Problem # 1 - 4
# Description of Problem: Analyse given data and generate histogram and probabilities of certain curcirstance.
# """

# Problem 1
csv_data <- read.csv(file = "/Users/zuowen/Documents/BU Spring 2023/CS555 Machine Learning/Homework/Assign1/data.csv", header = TRUE)
H_data <- c(csv_data$X0, csv_data$X1, csv_data$X2, csv_data$X3, csv_data$X4, csv_data$X5, csv_data$X6, csv_data$X7, csv_data$X8, csv_data$X9)
print(H_data)

# Problem 2
hist(H_data, breaks = 16, xlab = "Days of hospital stays", ylim = c(0, 25), main = "", col = "forestgreen")
axis(1,at=seq(0,16,by=1))
axis(2,at=seq(0,25,by=1))

# Problem 3
cat("Mean: ", mean(H_data))
cat("Median: ", median(H_data))
cat("Standard Deviation: ", sd(H_data))
res <- quantile(H_data, probs = c(0,0.25,0.5,0.75,1))
res
cat("Max: ", max(H_data))
cat("Min: ", min(H_data))

# Problem 4-a
x <- seq(1,15,by = 0.05)
y <- pnorm(x, mean = 5, sd = 3)
plot(x,y,xlab = "Days stayed", ylab = "Probability")
axis(1,at=seq(0,15,by=1))
axis(2,at=seq(0,1,by=0.1))

# Problem 4-b
n = 10000
sample_means = rep(NA, n)
for(i in 1:n){
  sample_means[i] = mean(rnorm(35, mean=5, sd=3))
}
hist(sample_means, xlab = "Sample Means", main = "", col = "forestgreen")
mean(sample_means)
sum(sample_means >= 6) / length(sample_means)
