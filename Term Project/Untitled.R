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
# install.packages("scatterplot3d")
library(scatterplot3d)
library(tidyverse)
library(ggplot2)
library(dplyr)

# Read data and remove all undefind data
cr <- read.csv('/Users/zuowen/Documents/GitHub/R-practice/Term Project/dataset/boston_crime_rate_2019-2021.csv', header = TRUE, stringsAsFactors = FALSE)
cr[cr==""]<-NA 


# ----------------------------------------------------------------------------
# Part 0: Set up functions

getBarChart <- function(df,title_text){
  ggplot(as.data.frame(df), aes(x = reorder(Var1,-Freq), y = Freq)) + # create the bar chart in decreasing order
    xlab("Type of Incidents") + ylab("Num. of Incidents") + geom_col(fill = "#669933") + 
    theme(axis.text.x = element_text(angle = 80, hjust = 0.5, vjust = 0.5), # make the element text tilted so people can read
          plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=Freq),size=3,vjust=-0.5) + # change the title position
    ggtitle(title_text)
}

getMinMaxBarChart <- function(data, x_text, title_text, tilted, titled.angle) {
  p <- ggplot(data = data) + 
    geom_bar(aes(x=Name, y=Value), stat="identity", fill = "#1f8bd6", width=0.6) +
    geom_bar(data=subset(data, Value==min(Value)), aes(Name, Value),
             fill="#669933", width=0.6, stat="identity") +
    geom_bar(data=subset(data, Value==max(Value)), aes(Name, Value),
             fill="#bf3106",  width=0.6, stat="identity") +
    xlab(x_text) + ylab("Num. of Incidents") + theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title_text) + geom_text(aes(x=Name, label=Value, y=Value),stat="identity",size=3,vjust=-0.5)
  if (tilted){
    p <- p + theme(axis.text.x = element_text(angle = titled.angle, hjust = 0.5, vjust = 0.5))
  }
  p
}

getPieChart <- function(slices, labels, title_text){
  pct <- round(slices/sum(slices)*100) # show the percentage on the pie chart
  labels <- paste(labels, pct)
  labels <- paste(labels,"%",sep="") 
  pie(slices,labels = labels, col=rainbow(length(labels)),
      main=title_text) 
}

getCleanDf <- function(df, num){
  cr.sum <- sum(df[which(df$'Freq' < num), 2]) 
  df <- df[df$'Freq' > num, ] # remove the results that is smaller than 5000
  df$'Var1' <- as.character(df$'Var1') # change the data type in order to add new row
  df[nrow(df) + 1,] <- list('OTHER', cr.sum)
  df$'Var1' <- as.factor(df$'Var1')
  new_df <- df[order(df$'Freq', decreasing = TRUE), ]
  return (new_df)
}


# ----------------------------------------------------------------------------
# Part 1:

# Task 1: WHAT sorts of crimes happen the most often overall and in each year? 
cr.year <- cr$'YEAR'
cr.temp <- split(cr , f = cr.year) # split dataframes based on year
g2019 <- table(cr.temp$'2019'$'GROUP')
g2020 <- table(cr.temp$'2020'$'GROUP')
g2021 <- table(cr.temp$'2021'$'GROUP')
g.all <- table(cr$"GROUP")

getBarChart(g2019, "Crime Incidents in 2019")
getBarChart(g2020, "Crime Incidents in 2020")
getBarChart(g2021, "Crime Incidents in 2021")
getBarChart(g.all, "Crime Incidents During 2019-2021")


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
df2 <- getCleanDf(df2, 5000)
colnames(df2)[1] <- "Type"

# create variables for pie charts
par(mar=c(5, 7, 5, 7)) # change the picture frame size
getPieChart(df2$'Freq', df2$'Type', "Pie Chart of Crime Rate During 2019-2021")


# Task 2: WHEN does the crime happen most often? List by month, week, and hour. Show Min/Max

# by Month
df3 <- as.data.frame(table(cr$'MONTH'))
colnames(df3)[1] <- "Month"
data = data.frame( Name = df3$'Month' , Value = df3$'Freq' )
getMinMaxBarChart(data, "Month", "Crime Incidents in Every Month", FALSE, 0)

# by Weekday
df4 <- as.data.frame(table(cr$'DAY_OF_WEEK'))
colnames(df4)[1] <- "Weekday"
df4$'Weekday' <- factor(df4$'Weekday', # reorder weekdays (R order them alphabetically by default)
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
data = data.frame( Name = df4$'Weekday' , Value = df4$'Freq' )
getMinMaxBarChart(data, "Weekday", "Crime Incidents in Every Day of Week", FALSE, 0)

# by Hour
df5 <- as.data.frame(table(cr$'HOUR'))
colnames(df5)[1] <- "Hour"
data = data.frame( Name = df5$'Hour' , Value = df5$'Freq' )
getMinMaxBarChart(data, "Hour", "Crime Incidents in Every Hour in One Day", FALSE, 0)

# Task 3: WHERE does crime happen most often?
cr.street <- as.data.frame(table(cr$'STREET'))
cr.street <- getCleanDf(cr.street, 3000)
colnames(cr.street)[1] <- 'Location'
street.num <- length(unique(cr.street$'Location'))

# create variables for pie charts
par(mar=c(5, 3, 3, 9)) # change the picture frame size
getPieChart(cr.street$'Freq', cr.street$'Location', 
            "Pie Chart of Street where Crime Happen Most often")
print("There are total 13297 streets, yet almost 20% crimes happened on these 9 streets.")

cr.street <- cr.street[cr.street$'Freq' < 187280, ] 
cr.street

data = data.frame( Name = cr.street$'Location' , Value = cr.street$'Freq' )
getMinMaxBarChart(data, "Location", "Crime Incidents in Different Location", TRUE, 65)
summary(cr.street)


# Task 4: For top 3 streets that crimes happen most often, analysis the relation with Hours of the day
cr.location <- cr$'STREET'
cr.temp <- split(cr , f = cr.location) # split dataframes based on year
washington <- as.data.frame(table(cr.temp$'WASHINGTON ST'$'HOUR'))
blue.hill <- as.data.frame(table(cr.temp$'BLUE HILL AVE'$'HOUR'))
harrison <- as.data.frame(table(cr.temp$'HARRISON AVE'$'HOUR'))

df6 <- list(washington, blue.hill, harrison)
df6 <- df6 %>% reduce(full_join, by='Var1')
colnames(df6)[1] <- 'Hour'
colnames(df6)[2] <- 'Washington St'
colnames(df6)[3] <- 'Blue Hill Ave'
colnames(df6)[4] <- 'Harrison Ave'
df6 <- na.omit(df6)
df6$'Hour' <- as.integer(df6$'Hour')
cor(df6)
model <- lm(df6$Hour~df6$`Washington St`)
model


