# CS555 Assignment 5
# Spring 2023
# by Zuowen Tang
# ---------------------------------------------------------
# PART ONE
# ---------------------------------------------------------
feed <- read.csv(file = "/Users/zuowen/Documents/BU Spring 2023/CS555 Machine Learning/Homework/Assign5/cattle_feed.csv", header = TRUE)
feed
protein <- feed$PROTEIN
antib <- feed$ANTIBIO
sup <- feed$SUPPLEM
ftime <- feed$TIME

# (1) Obtain the regression equation relating feedlot time to the three diet variables. 
lm.model <- lm(ftime~protein+antib+sup, feed)
summary(lm.model)
print("regression equation is 102.7 - 0.83(protein) - 4(antibio) - 1.375(supplement)")

#c(2) Find the value of Residual Standard Deviation 𝑆. 
sqrt(deviance(lm.model)/df.residual(lm.model))

#c(3) Find the 𝑅2 value.
summary(lm.model)$r.squared

# (4) How much of a collinearity problem is there with these data?
#install.packages("car")
#library(car)
vif(lm.model)

# (5) Predict the feedlot time required for a steer fed 15% protein, 1.5% antibiotic concentration, and 5%
#supplement. 
new.feed <- data.frame(protein = 15, antib = 1.5, sup = 5)
predict(lm.model, newdata = new.feed)

# (7) Give a 95% confidence interval for the mean time predicted in part (5).
n <- 30
xbar <- 77.33
s <- 2.3
margin <- qt(0.975,df=n-1)*s/sqrt(n)
lowerinterval <- xbar - margin
lowerinterval 
upperinterval <- xbar + margin
upperinterval 

# (8) Analyze the data using a regression model with only protein content as an independent variable.
lm.model.p <- lm(ftime~protein, feed)
summary(lm.model.p)

# (8a) Obtain the regression equation. 
print("regression equation is 89.9 - 0.83(protein)")

# (8b) Find the 𝑅2 value. 
summary(lm.model.p)$r.squared

# ---------------------------------------------------------
# PART TWO
# ---------------------------------------------------------
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",sep=",")
wine_data <- wine[,2:14]

install.packages("corrr")
install.packages("ggcorrplot")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("car")
library('corrr')
library("ggcorrplot")
library("FactoMineR")
library("factoextra")
library("car")

#(1) A Matrix Scatterplot of all 13 variables of chemical concentration. 
wine
scatterplotMatrix(wine[2:14],  cex=0.1 , col="forestgreen")

#(2) Summary Statistics (mean, standard deviation) of all 13 variables of chemical concentration using
#sapply() function. 
sapply(wine[2:14], mean)
sapply(wine[2:14], sd)

#(3) After standardizing the data, use the cor() function from the corrr package to calculate the
#correlation matrix, and uset he ggcorrplot() to generate a visualization. 
std.wine <- as.data.frame(scale(wine[2:14]))
sapply(std.wine,mean)
sapply(std.wine,sd)
cor(std.wine)
ggcorrplot(std.wine)

#(4) Use princomp() to obtain PCA results, use summary table and scree plot to decide how many
#Principal Components to retain, and explain why.
summary(princomp(std.wine))

#(5) Make a biplot combined with cos2 (attributes importance), and combined with (4) to discuss the
#contributions by important variables of chemical contributions in the new space spanned by the
#retained Principal Components. 

