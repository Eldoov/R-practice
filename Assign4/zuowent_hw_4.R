data <- read.csv(file = "/Users/zuowen/Documents/BU Spring 2023/CS555 Machine Learning/Homework/Assign4/data.csv", header = TRUE)
iq <- data$IQ
age <- data$Age
group <- data$Group
data

data2 <- split(data , f = group)
phy.stud <- data2$`Physics student`
math.stud <- data2$`Math student`
chem.stud <- data2$`Chemistry student`

#Question 1:
print(aggregate(iq, list(group), summary))
print(aggregate(age, list(group), summary))
hist(phy.stud$Age, breaks = 15, xlab = "Blue:Phy.Student, Red:Chem.Student, Green:Math.Student", ylim = c(0, 4), xlim=c(10, 50), main = "Age Frequency based on Subject Group", col=rgb(0,0,1,0.2))
hist(chem.stud$Age, breaks = 15, xlab = "", ylim = c(0, 4), main = "", col=rgb(1,0,0,0.2), add=TRUE)
hist(math.stud$Age, breaks = 15, xlab = "", ylim = c(0, 4), main = "", col=rgb(0,1,0,0.2), add=TRUE)
hist(phy.stud$IQ, breaks = 20, xlab = "Blue:Phy.Student, Red:Chem.Student, Green:Math.Student", ylim = c(0, 4), xlim=c(10, 60), main = "IQ Frequency based on Subject Group", col=rgb(0,0,1,0.2))
hist(chem.stud$IQ, breaks = 15, xlab = "", ylim = c(0, 4), main = "", col=rgb(1,0,0,0.2), add=TRUE)
hist(math.stud$IQ, breaks = 15, xlab = "", ylim = c(0, 4), main = "", col=rgb(0,1,0,0.2), add=TRUE)

#Question 2:
aov.model <- aov(iq~group,data)
summary(aov.model)
TukeyHSD(aov.model)

#Question 3:
lm.model <- lm(iq~group,data)
summary(lm.model)

#Question 4:
data3 <- read.csv(file = "/Users/zuowen/Documents/BU Spring 2023/CS555 Machine Learning/Homework/Assign4/data3.csv", header = TRUE)
data3
chisq.test(data3)

#Extra:
data4 <- read.csv(file = "/Users/zuowen/Documents/BU Spring 2023/CS555 Machine Learning/Homework/Assign4/data4.csv", header = TRUE)
data4
model <- lm(X39~X52,data4)
model
anova(model)


