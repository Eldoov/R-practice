csv.data <- read.csv(file = "/Users/zuowen/Documents/BU Spring 2023/CS555 Machine Learning/Homework/Assign3/data.csv", header = TRUE)
NumFish <-csv.data$Number_of_meals_with_fish
Mercury <-csv.data$Total_Mercury_in_mg.g
Mercury

#Question 1:
plot(NumFish, Mercury, main = "Number of Meals with Fish vs. Mercury Levels", xlab= "Number of meals with fish", ylab = "Total Mercury in mg/g", col='forestgreen')

#Question 2:
cor(csv.data)

#Question 3&4:
model
model <- lm(Mercury ~ NumFish)
abline(model)
summary(model)
xbar <- mean(NumFish)
Sx <- sd(NumFish)
ybar <- mean(Mercury)
Sy <- sd(Mercury)
b1 <- r * (Sy / Sx)
b0 <- ybar - (b1 * xbar)
b0 # 1.687643
b1 # 0.2759503

#Question 5:
anova(model)
qf(0.95, df1 = 1, df2 = 98)
model.table <- data.frame(anova(model))
model.table['NumFish', 'Sum.Sq']/sum(model.table$Sum.Sq)
confint(model, 'NumFish', level=0.9)
