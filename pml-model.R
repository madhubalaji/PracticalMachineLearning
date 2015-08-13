## Author - Madhu Balaji
library(caret)

trainingData <- read.csv("data/pml-training.csv", header=TRUE, as.is = TRUE, na.strings = c('NA','','#DIV/0!'))
testData <- read.csv("data/pml-testing.csv", header=TRUE, as.is = TRUE, na.strings = c('NA','','#DIV/0!'))

inTrain <- createDataPartition(trainingData$classe, p=0.75, list = FALSE)
