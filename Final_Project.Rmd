---
title: "Final Project"
author: "Fabrizio Veneziano"
date: "Thursday, July 16, 2015"
output: html_document
---

##Getting the data

The training data set can be found on the following URL:

```{r LoadTrain, cache=TRUE}
trainUrl <- read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")

```


The testing data set can be found on the following URL:

```{r LoadTest, cache=TRUE}

testUrl <- read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

```

## Data CleanUp
  
Before proceedeing with the model-bulding section, we take a look at the variables of both data set and remove the ones which we deem irrelevant to our predictions ("user", etc.), and also get rid of columns containing NAs: we are planning to use the Rattle/RandomForest method to build our model, and Rby experience we know it doesn't handle NAs very well.  
We also recode variables and tur them into numeric ones (this step could also be performed in Rattle, but it would add new variable names, making the model inconsistent with the Test set).  


```{r CleanUp}

# names(trainUrl)
# names(testUrl)
 
training <- trainUrl[,c(8:160)]
classe <- training$classe
training <- training[, sapply(training, is.numeric)]
training <- training[, colSums(is.na(training)) == 0] 

training <- cbind(training,classe)

finalTest <- testUrl[,c(8:160)]
finalTest <- finalTest[,sapply(finalTest, is.numeric)]
finalTest <- finalTest[, colSums(is.na(finalTest)) == 0] 



```

## Model building with RandomForest and Rattle package  

The Rattle package/GUI makes it easier to visualize the model building process and has some useful built-in functions to 'tweak', analyze and optimize the model.  

Its output is very informative, although somewhat verbose:

```{r Rattle}

library(rattle)

#rattle()


# Rattle output


# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

 

# Load our R data frame.

pml_dataset <- training
```
 
 
 

### Build the training/validate/test datasets
  
For the cross-validation we split the training set in 80-20.  

  
```{r CrossVal}

set.seed(crv$seed) 
pml_nobs <- nrow(pml_dataset) # 19622 observations 
pml_sample <- pml_train <- sample(nrow(pml_dataset), 0.8*pml_nobs) # 15697 observations
pml_validate <- NULL
pml_test <- setdiff(setdiff(seq_len(nrow(pml_dataset)), pml_train), pml_validate) # 3925 observations

# The following variable selections have been noted.

pml_input <- c("roll_belt", "pitch_belt", "yaw_belt", "total_accel_belt",
     "gyros_belt_x", "gyros_belt_y", "gyros_belt_z", "accel_belt_x",
     "accel_belt_y", "accel_belt_z", "magnet_belt_x", "magnet_belt_y",
     "magnet_belt_z", "roll_arm", "pitch_arm", "yaw_arm",
     "total_accel_arm", "gyros_arm_x", "gyros_arm_y", "gyros_arm_z",
     "accel_arm_x", "accel_arm_y", "accel_arm_z", "magnet_arm_x",
     "magnet_arm_y", "magnet_arm_z", "roll_dumbbell", "pitch_dumbbell",
     "yaw_dumbbell", "total_accel_dumbbell", "gyros_dumbbell_x", "gyros_dumbbell_y",
     "gyros_dumbbell_z", "accel_dumbbell_x", "accel_dumbbell_y", "accel_dumbbell_z",
     "magnet_dumbbell_x", "magnet_dumbbell_y", "magnet_dumbbell_z", "roll_forearm",
     "pitch_forearm", "yaw_forearm", "total_accel_forearm", "gyros_forearm_x",
     "gyros_forearm_y", "gyros_forearm_z", "accel_forearm_x", "accel_forearm_y",
     "accel_forearm_z", "magnet_forearm_x", "magnet_forearm_y", "magnet_forearm_z")

pml_numeric <- c("roll_belt", "pitch_belt", "yaw_belt", "total_accel_belt",
     "gyros_belt_x", "gyros_belt_y", "gyros_belt_z", "accel_belt_x",
     "accel_belt_y", "accel_belt_z", "magnet_belt_x", "magnet_belt_y",
     "magnet_belt_z", "roll_arm", "pitch_arm", "yaw_arm",
     "total_accel_arm", "gyros_arm_x", "gyros_arm_y", "gyros_arm_z",
     "accel_arm_x", "accel_arm_y", "accel_arm_z", "magnet_arm_x",
     "magnet_arm_y", "magnet_arm_z", "roll_dumbbell", "pitch_dumbbell",
     "yaw_dumbbell", "total_accel_dumbbell", "gyros_dumbbell_x", "gyros_dumbbell_y",
     "gyros_dumbbell_z", "accel_dumbbell_x", "accel_dumbbell_y", "accel_dumbbell_z",
     "magnet_dumbbell_x", "magnet_dumbbell_y", "magnet_dumbbell_z", "roll_forearm",
     "pitch_forearm", "yaw_forearm", "total_accel_forearm", "gyros_forearm_x",
     "gyros_forearm_y", "gyros_forearm_z", "accel_forearm_x", "accel_forearm_y",
     "accel_forearm_z", "magnet_forearm_x", "magnet_forearm_y", "magnet_forearm_z")

pml_categoric <- NULL

pml_target  <- "classe"
pml_risk    <- NULL
pml_ident   <- NULL
pml_ignore  <- NULL
pml_weights <- NULL

```




### Random Forest Model  

We load the randomForest package and proceed to build our model using 500 trees and 7 variables tried at each split.  


```{r RFModel}
# The 'randomForest' package provides the 'randomForest' function.

library(randomForest, quietly=TRUE)

# Build the Random Forest model.

set.seed(crv$seed)
pml_rf <- randomForest::randomForest(classe ~ .,
      data=pml_dataset[pml_sample,c(pml_input, pml_target)], 
      ntree=500,
      mtry=7,
      importance=TRUE,
      na.action=randomForest::na.roughfix,
      replace=FALSE)

# Generate textual output of 'Random Forest' model.

pml_rf


```

The OOB estimate of  error rate (0.41%) is very encouraging and so is the confusion matrix on the training set. We therefore move on to check the performance of our mdel on our cross-validated test-set.  


### Evaluate model performance.

```{r Evaluation, message=FALSE}

# Generate an Error Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

pml_pr <- predict(pml_rf, newdata=na.omit(pml_dataset[pml_test, c(pml_input, pml_target)]))

# Generate the confusion matrix showing counts.

table(na.omit(pml_dataset[pml_test, c(pml_input, pml_target)])$classe, pml_pr,
        dnn=c("Actual", "Predicted"))

library(caret)

accuracy <- postResample(pml_pr, pml_dataset[pml_test, c(pml_input, pml_target)]$classe)
accuracy
error <- 1 - as.numeric(confusionMatrix(pml_dataset[pml_test, c(pml_input, pml_target)]$classe, pml_pr)$overall[1])
error


```

The accuracy and the out of sample error are, again, satisfying, therefore we generate the predictions on the orginal Test set (which will be separately uploaded and submitted).

```{r Answers}
answers <- predict(pml_rf, finalTest)
answers
```

 






