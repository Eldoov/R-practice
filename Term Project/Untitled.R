# """
# Zuowen Tang
# Class: CS555 A3 SPRG23
# Date: April 20th, 2023
# Final Project
# Description: data analysis about Boston crime rate based on dataset from Boston Police Department.
# """


# install.packages("dplyr")
# install.packages("scales")
# install.packages("tidyverse")
library(ggplot2)
library(dplyr)


# Read data and separate it into different groups.
cr <- read.csv('/Users/zuowen/Documents/GitHub/R-practice/Term Project/dataset/boston_crime_rate_2019-2021.csv', header = TRUE, stringsAsFactors = FALSE)
cr[cr==""]<-NA
cr.year <- cr$'YEAR'
cr.temp <- split(cr , f = cr.year) # split dataframes based on year
g2019 <- table(cr.temp$'2019'$'GROUP')
g2020 <- table(cr.temp$'2020'$'GROUP')
g2021 <- table(cr.temp$'2021'$'GROUP')
g.all <- table(cr$"GROUP")


# Task 1: What sorts of crimes happen the most often overall and in each year? 
ggplot(as.data.frame(g2019), aes(x = reorder(Var1,-Freq), y = Freq)) + # create the bar chart in decreasing order
  xlab("Type of Incidents") + ylab("Num. of Incidents") + geom_col(fill = "#669933") + 
  theme(axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5), # make the element text tilted so people can read
        plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=Freq),size=3,vjust=-0.5) + # change the title position
  ggtitle("Crime Incidents in 2019")
# following codes are simular to above
ggplot(as.data.frame(g2020), aes(x = reorder(Var1,-Freq), y = Freq)) + xlab("Type of Incidents") + ylab("Num. of Incidents") + geom_col(fill = "#669933") + theme(axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5), plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=Freq),size=3,vjust=-0.5) + ggtitle("Crime Incidents in 2020")
ggplot(as.data.frame(g2021), aes(x = reorder(Var1,-Freq), y = Freq)) + xlab("Type of Incidents") + ylab("Num. of Incidents") + geom_col(fill = "#669933") + theme(axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5), plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=Freq),size=3,vjust=-0.5) + ggtitle("Crime Incidents in 2021")
ggplot(as.data.frame(g.all), aes(x = reorder(Var1,-Freq), y = Freq)) + xlab("Type of Incidents") + ylab("Num. of Incidents") + geom_col(fill = "#669933") + theme(axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5), plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=Freq),size=3,vjust=-0.5) + ggtitle("Crime Incidents during 2019-2021")


# Generate a grouped bar chart
df1 <- as.data.frame(table(cr$"GROUP", cr$"YEAR")) # load a new dataframe based on types of crime
colnames(df1)[1] <- "Type" # change the column name
colnames(df1)[2] <- "Year"
ggplot(df1,aes(x = reorder(Type,-Freq), y = Freq, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") + # create a grouped bar chart
  xlab("Type of Incidents") + ylab("Num. of Incidents") + 
  theme(axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5), 
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Crime Incidents During 2017-2021")


# Generate a pie chart
df2 <- as.data.frame(table(cr$'GROUP')) # load new dataframe
colnames(df2)[1] <- "Type"

# we only want to show top 8 elements or the pie chart will be messy
cr.sum <- sum(df2[which(df2$'Freq' < 5000), 2]) 
df2 <- df2[df2$'Freq' > 5000, ] # remove the results that is smaller than 5000
df2$'Type' <- as.character(df2$'Type') # change the data type in order to add new row
df2[nrow(df2) + 1,] <- list('OTHER', cr.sum)
df2$'Type' <- as.factor(df2$'Type')
df2 <- df2[order(df2$'Freq', decreasing = TRUE), ]

# create variables for pie charts
slices <- df2$'Freq'
lbls <- df2$'Type'
pct <- round(slices/sum(slices)*100) # show the percentage on the pie chart
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="") 

par(mar=c(5, 7, 5, 7)) # change the picture frame size
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Crime Rate During 2019-2021") 



