# https://habrahabr.ru/post/234303/

install.packages("quantmod")
install.packages("lubridate")
install.packages("e1071")

library("quantmod")
library("lubridate")
#Makes it easier to work with the dates
library("e1071")
#Gives us access to the Na??ve Bayes classifier

startDate = as.Date("2012-01-01")
endDate = as.Date("2014-01-01")
getSymbols("AAPL",
           src = "yahoo",
           from = startDate,
           to = endDate)
# Retrieving Apple???s daily OHLCV from Yahoo Finance
DayofWeek <- wday(AAPL, label = TRUE)
#Находим день недели

PriceChange <- Cl(AAPL) - Op(AAPL)
# Find the difference between the close price and open price
# Находим разницу между ценой закрытия и ценой открытия.

Class <- ifelse(PriceChange > 0, "UP", "DOWN")
#Convert to a binary classification. (In our data set, there are no bars with an exactly 0 price change so, for simplicity sake, we will not address bars that had the same open and close price.)
DataSet <- data.frame(DayofWeek, Class)
#Create our data set

MyModel <- naiveBayes(DataSet[, 1], DataSet[, 2])

EMA5 <- EMA(Op(AAPL), n = 5)

EMA10 <- EMA(Op(AAPL), n = 10)

EMACross <- EMA5 - EMA10

DataSet2 <- data.frame(DayofWeek, EMACross, Class)
DataSet2 <- DataSet2[-c(1:10), ]

TrainingSet <- DataSet2[1:328, ]

TestSet <- DataSet2[329:492, ]


EMACrossModel <- naiveBayes(TrainingSet[, 1:2], TrainingSet[, 3])


table(predict(EMACrossModel, TestSet),
      TestSet[, 3],
      dnn = list('predicted', 'actual'))
