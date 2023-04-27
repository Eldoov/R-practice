# """
# Zuowen Tang
# Class: CS555 A3 SPRG23
# Date: April 20th, 2023
# Final Project
# Description: This project is an implementation of Machine Learning and Data Science on Humanity Study 
#              that based on the dataset of World Happiness Report from 2017-2019.
#              Several analytic methods will be use to test and virtualize the datasets.
#              We want to ask: What is the most important thing that determine people's happiness?
#                              Can money buy happiness? Or there's something more important?
#              We also want to know, if it is possible for us to predict the future of happiness.
# """


# install.packages("scatterplot3d")
# install.packages("heplots")
# install.packages("naivebayes")
# install.packages("MASS")
# install.packages('ggplot2')
# install.packages("corrgram")
# install.packages("tidyverse")
# install.packages("plyr")
# install.packages('reshape2')
library(scatterplot3d)
library(heplots)
library(naivebayes)
library(MASS)
library(ggplot2)
library(corrgram)
library(tidyverse)
library(plyr)
require(reshape2)


###########################
### Part 0: Preparation ###
###########################

### Functions for predicting / cleaning the data
getLDA <- function(train, test){
  lda.model <- lda(happy ~ ., data=train)
  lda.pred <- predict(lda.model, newdata=test)
  print(table(test$happy, lda.pred$class, dnn=c('Actual Group', 'Predicted Group')))
  print(mean(lda.pred$class == test$happy))
}

getQDA <- function(train, test){
  qda.model <- qda(happy ~ ., data=train)
  qda.pred <- predict(qda.model, newdata=test)
  print(table(test$happy, qda.pred$class, dnn=c('Actual Group', 'Predicted Group')))
  print(mean(qda.pred$class == test$happy))
}

getNB <- function(train, test){
  nb.model <- naive_bayes(happy ~ ., data=train)
  nb.pred <- predict(nb.model, test[,-1])
  print(table(test$happy, nb.pred, dnn=c('Actual Group', 'Predicted Group')))
  print(mean(nb.pred == test$happy))
}

removeOutlier <- function(data, category){
  Q <- quantile(category, probs=c(.25, .75), na.rm = FALSE)
  iqr <- IQR(category)
  up <-  Q[2]+1.5*iqr # Upper Range  
  low<- Q[1]-1.5*iqr # Lower Range
  data <- subset(data, category>low & category<up)
  return (data)
}

### Prepare Data / Combine 3 datasets (2017-2019) together
# df2017 <- read.csv('/R-practice/Term Project/dataset/2017.csv', header = TRUE, stringsAsFactors = FALSE)
# df2018 <- read.csv('/R-practice/Term Project/dataset/2018.csv', header = TRUE, stringsAsFactors = FALSE)
# df2019 <- read.csv('/R-practice/Term Project/dataset/2019.csv', header = TRUE, stringsAsFactors = FALSE)
# 
# df2017$Year <- 2017
# df2018$Year <- 2018
# df2019$Year <- 2019
# 
# df <- rbind(df2017,df2018)
# df <- rbind(df,df2019)
# 
# write.csv(df, "/R-practice/Term Project/dataset/world_happiness.csv", row.names=FALSE)


###################################
### Part 1: Data Virtualization ###
###################################

### Read data into R / clean the data
df <- read.csv('/Users/zuowen/Documents/GitHub/R-practice/Term Project/dataset/world_happiness.csv', header = TRUE, stringsAsFactors = FALSE)
df <- subset(df, select = -c(Country.or.region, Overall.rank)) # Overall Rank is same with Score
df <- as.data.frame(lapply(df, as.numeric))
df <- na.omit(df)
head(df)

### Remove outliers (repeat few times)
df <- removeOutlier(df, df$Social.support)
df <- removeOutlier(df, df$Freedom.to.make.life.choices)
df <- removeOutlier(df, df$Generosity)
df <- removeOutlier(df, df$Perceptions.of.corruption)
df.box <- subset(df, select = -c(Year, Score))
ggplot(data = melt(df.box), aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=variable)) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) # make the element text tilted so people can read
attach(df)

### Check data
summary(df)
# Overall histogram 
ggplot(gather(df), aes(value)) + 
  geom_histogram(color='black', fill="forestgreen", alpha=0.8, bins = 15) + 
  facet_wrap(~key, scales = 'free_x')
# Check to see if score is all normally distributed
shapiro.test(Score)
summary(Score)
y <- dnorm(Score, mean = mean(Score), sd = sd(Score))
plot(Score,y, col="red")

### Data analysis based on year
# Check to see if scores from different years are all normally distributed
vec <- as.character(Year)
ggplot(df, aes(x=Score, fill=vec)) +
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", alpha=0.7, position='identity') +
  scale_fill_manual(values=c('orangered', 'steelblue', 'forestgreen')) +
  geom_density(aes(x=Score, fill=vec), alpha=0.2)  # Overlay with transparent density plot



############################
### Part 2: Data Testing ###
############################

### Correlation tests
corrgram(df)

# Filter positive correlations
correlation_matrix <- round(cor(df),5)
cor.df <- as.data.frame(as.table(correlation_matrix))
subset(cor.df, !(abs(Freq) == 1 | abs(Freq) < 0.6 ))
# Filter positive correlations with overall happiness score
cor.score <- as.data.frame(as.table(correlation_matrix[1,]))
subset(cor.score, !(abs(Freq) == 1 | abs(Freq) < 0.6 ))


### Linear Regression & Anova table
lm.all <- lm(Score ~ ., data = df)
summary(lm.all)
anova(lm.all)

lm.top3 <- lm(Score ~ GDP.per.capita+Social.support+Healthy.life.expectancy, data=df)
summary(lm.top3)
anova(lm.top3)
confint(lm.top3, 'GDP.per.capita', level=0.9)
confint(lm.top3, 'Social.support', level=0.9)
confint(lm.top3, 'Healthy.life.expectancy', level=0.9)

# Single linear regression 
lm.gdp <- lm(Score ~ GDP.per.capita)
summary(lm.gdp)
anova(lm.gdp)

# plot linear regression between Score and Top3 factors
plot(GDP.per.capita, Score, col='orangered', type='p')
abline(lm.gdp, col='orangered')
plot(Social.support, Score, col="steelblue", type='p')
abline(lm(Score ~ Social.support), col='steelblue')
plot(Healthy.life.expectancy, Score, col="forestgreen", type='p')
abline(lm(Score ~ Healthy.life.expectancy), col='forestgreen')


### Hypothesis Test (Two sample mean test + Anova)

## Data preparation
# Separate data into two catergories: rich and poor
summary(GDP.per.capita)
rich <- subset(df, GDP.per.capita >= 1.161)
poor <- subset(df, GDP.per.capita <= 0.6045)
df2 <- df
df2$rich.lv <- with(df, ifelse(GDP.per.capita >= 1.161, 1,
                            ifelse(GDP.per.capita >= 0.9595, 2,
                                   ifelse(GDP.per.capita > 0.6045, 3, 4))))
df2$rich <- with(df, ifelse(GDP.per.capita >= 1.161, 'Rich',
                            ifelse(GDP.per.capita >= 0.9595, 'Above Ave',
                                   ifelse(GDP.per.capita > 0.6045, 'Below Ave', 'Poor'))))
write.csv(df2, "/Users/zuowen/Documents/BU Spring 2023/CS555 Machine Learning/Homework/Term Project/rich.csv", row.names=FALSE)

# Hypothesis (Overall happiness and wealth)
# Two sample mean test (overall happiness and wealth)
t.test(rich$Score, poor$Score)
# Anova
rich.aov <- aov(Score ~ rich, data=df2)
summary(rich.aov)
TukeyHSD(rich.aov)

# Hypothesis (Generosity and wealth)
t.test(rich$Generosity, poor$Generosity)
rich2.aov <- aov(Generosity ~ rich, data=df2)
summary(rich2.aov)
TukeyHSD(rich2.aov)

# Separate data into two catergories: free and not free
summary(Freedom.to.make.life.choices)
free <- subset(df, Freedom.to.make.life.choices >= 0.5)
not.free <- subset(df, Freedom.to.make.life.choices <= 0.3032)
df2$free <- with(df, ifelse(Freedom.to.make.life.choices >= 0.5, 'Great',
                           ifelse(Freedom.to.make.life.choices >= 0.4175, 'Good',
                                  ifelse(Freedom.to.make.life.choices > 0.3032, 'Okay', 'Bad'))))

# Hypothesis (overall happiness and freedom)
t.test(free$Score, not.free$Score)
free.aov <- aov(Score ~ free, data=df2)
summary(free.aov)
TukeyHSD(free.aov)


###############################
### Part 2: Prediction Test ###
###############################

### Data Preparation 
summary(Score)
df3 <- data.frame(GDP.per.capita,Social.support,Healthy.life.expectancy,Score)
## Label data with 'happy', 'okay', and 'unhappy' based on the score
df3$happy.lv <- with(df, ifelse(Score >= 5.949, 1,
                             ifelse(Score >= 4.507, 2, 3)))
df3$happy <- with(df, ifelse(Score >= 5.949, 'Happy',
                             ifelse(Score >= 4.507, 'Okay', 'Unhappy')))

### Visualizing the relation between overall happiness and top three variables
colors <- c("#EA047E", "#00F5FF", "#FF6D28")
colors <- colors[as.numeric(df3$happy.lv)]
plot(df3[, c(1,2,3)], col = colors)
scatterplot3d(df3[,1:3], pch = 19, color=colors)
legend("topright", legend=c("Happy", "Okay", "Unhappy"), pch=19, col=c("#EA047E", "#00F5FF", "#FF6D28"))


### Prediction with 8:2 radio
df3 <- subset(df3, select = -c(happy.lv))
sample = floor(0.8 * nrow(df3))
set.seed(1234)
train.ind = sample(seq_len(nrow(df3)), size = sample)
train1 = df3[train.ind, ]
test1= df3[-train.ind, ]

getLDA(train1, test1)
getQDA(train1, test1)
getNB(train1, test1)


### Prediction with 5:5 radio
sample = floor(0.5 * nrow(df3))
train.ind = sample(seq_len(nrow(df3)), size = sample)
train2 = df3[train.ind, ]
test2 = df3[-train.ind, ]

getLDA(train2, test2)
getQDA(train2, test2)
getNB(train2, test2)


################################################################################
### OPTIONAL SECTION
### Description: Test the accuracy of this project on dataset 2022 to see if it can predict the future.
### Note: The following dataset is from World Happiness Report 2022, which is not attached with the project submission.
###       If you wish to access the data of 2022, please contact me or download it from my Github (https://github.com/Eldoov)
###       The result of this test can be found in the Final Project Report.
################################################################################

# test2022 <- read.csv('/GitHub/R-practice/Term Project/dataset/2022.csv', header = TRUE, stringsAsFactors = FALSE)
# summary(test2022$Score)
# test2022$happy <- with(test2022, ifelse(Score >= 6.305, 'Happy',
#                              ifelse(Score >= 4.889, 'Okay', 'Unhappy')))

# getLDA(df3, test2022) # ACU: 0.71
# getQDA(df3, test2022) # ACU: 0.81
# getNB(df3, test2022) # ACU: 0.68

