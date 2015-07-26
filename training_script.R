# Rattle is Copyright (c) 2006-2015 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2015-07-26 15:06:56 x86_64-w64-mingw32 

# Rattle version 3.5.0 user 'utente'

# Export this log textview to a file using the Export button or the Tools 
# menu to save a log of all activity. This facilitates repeatability. Exporting 
# to file 'myrf01.R', for example, allows us to the type in the R Console 
# the command source('myrf01.R') to repeat the process automatically. 
# Generally, we may want to edit the file to suit our needs. We can also directly 
# edit this current log textview to record additional information before exporting. 
 
# Saving and loading projects also retains this log.

library(rattle)

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building


# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2015-07-26 15:07:05 x86_64-w64-mingw32 

# Load an R data frame.

pml_dataset <- training

# Display a simple summary (structure) of the dataset.

str(pml_dataset)


#============================================================
# Rattle timestamp: 2015-07-26 15:07:21 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

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

#============================================================
# Rattle timestamp: 2015-07-26 15:07:33 x86_64-w64-mingw32 

# Random Forest 

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

# List the importance of the variables.

rn <- round(randomForest::importance(pml_rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Time taken: 4.39 mins

#============================================================
# Rattle timestamp: 2015-07-26 15:13:26 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

pml_pr <- predict(pml_rf, newdata=na.omit(pml_dataset[pml_test, c(pml_input, pml_target)]))

# Generate the confusion matrix showing counts.

table(na.omit(pml_dataset[pml_test, c(pml_input, pml_target)])$classe, pml_pr,
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  tbl <- cbind(round(x/length(actual), 2),
               Error=round(c(x[1,2]/sum(x[1,]),
                             x[2,1]/sum(x[2,])), 2))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
};
pcme(na.omit(pml_dataset[pml_test, c(pml_input, pml_target)])$classe, pml_pr)
