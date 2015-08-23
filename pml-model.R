## Author - Madhu Balaji
library(AppliedPredictiveModeling)
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
set.seed(4680)

## Details/Summary is provided in the RMD file

trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))


##training <- read.csv("data/pml-training.csv", header=TRUE, as.is = TRUE, na.strings = c('NA','','#DIV/0!'))
##testing <- read.csv("data/pml-testing.csv", header=TRUE, as.is = TRUE, na.strings = c('NA','','#DIV/0!'))

inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
myTrain <- training[inTrain, ]
myTest <- training[-inTrain, ]

myData1 <- nearZeroVar(myTrain, saveMetrics=TRUE)

myNZV <- names(myTrain) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
                                      "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
                                      "max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
                                      "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
                                      "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
                                      "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
                                      "max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
                                      "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
                                      "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
                                      "amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
                                      "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
                                      "max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
                                      "amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
                                      "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
                                      "stddev_yaw_forearm", "var_yaw_forearm")
myTrain <- myTrain[!myNZV]

myTrain <- myTrain[c(-1)]

trainingVal <- myTrain #creating another subset to iterate in loop
for(i in 1:length(myTrain)) { #for every column in the training dataset
        if( sum( is.na( myTrain[, i] ) ) /nrow(myTrain) >= .6 ) { #if n?? NAs > 60% of total observations
                for(j in 1:length(trainingVal)) {
                        if( length( grep(names(myTrain[i]), names(trainingVal)[j]) ) ==1)  { #if the columns are the same:
                                trainingVal <- trainingVal[ , -j] #Remove that column
                        }   
                } 
        }
}

myTrain <- trainingVal
rm(trainingVal)

clean1 <- colnames(myTrain)
clean2 <- colnames(myTrain[, -58]) #already with classe column removed
myTest <- myTest[clean1]
testing <- testing[clean2]

for (i in 1:length(testing) ) {
        for(j in 1:length(myTrain)) {
                if( length( grep(names(myTrain[i]), names(testing)[j]) ) ==1)  {
                        class(testing[j]) <- class(myTrain[i])
                }      
        }      
}

testing <- rbind(myTrain[2, -58] , testing) #note row 2 does not mean anything, this will be removed right.. now:
testing <- testing[-1,]

## Decision Tree

mFitDT <- rpart(classe ~ ., data=myTrain, method="class")

fancyRpartPlot(mFitDT)

predictDT <- predict(mFitDT, myTest, type = "class")

confusionMatrix(predictDT, myTest$classe)

## Random Forests

mFitRF <- randomForest(classe ~. , data=myTrain)

predictRF <- predict(mFitRF, myTest, type = "class")

confusionMatrix(predictRF, myTest$classe)

predictionFinal <- predict(mFitRF, testing, type = "class")


pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(predictionFinal)



