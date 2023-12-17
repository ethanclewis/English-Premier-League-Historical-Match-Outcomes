################################################################################

# CATEGORICAL EDA
# LOAD: 'Clean Training and Test Sets'

################################################################################

library(ggplot2)
library(boot)
library(caret)

################################################################################

# Categorical Variable Plots (FTR vs. ___)

################################################################################

# Day

total_train$Day <- factor(total_train$Day, 
                          levels = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

mosaicplot(Day~FTR, data = total_train, xlab = "Day", 
           ylab = "Home Team Win?" , las = 2)

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
model <- train(as.factor(FTR)~Day, data = total_train, trControl = train_control, 
               method = "glm", family=binomial())
model$results$Accuracy

################################################################################

# Month
total_train$Month <- factor(total_train$Month, 
                            levels = c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May"))

mosaicplot(Month~FTR, data = total_train, xlab = "Month", 
           ylab = "Home Team Win?" , las = 2)

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
model <- train(as.factor(FTR)~Month, data = total_train, trControl = train_control, 
               method = "glm", family=binomial())
model$results$Accuracy

################################################################################

# HomeTeam

total_train$HomeTeam <- factor(total_train$HomeTeam, 
                               levels = names(sort(table(total_train$HomeTeam), decreasing = TRUE)))

mosaicplot(HomeTeam~FTR, data = total_train, xlab = "Home Team", 
           ylab = "Home Team Win?" , las = 2)
# Useful

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
model <- train(as.factor(FTR)~HomeTeam, data = total_train, trControl = train_control, 
               method = "glm", family=binomial())
model$results$Accuracy

################################################################################

# AwayTeam

total_train$AwayTeam <- factor(total_train$AwayTeam, 
                               levels = names(sort(table(total_train$AwayTeam), decreasing = TRUE)))

mosaicplot(AwayTeam~FTR, data = total_train, xlab = "Away Team", 
           ylab = "Home Team Win?" , las = 2)
# Useful

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
model <- train(as.factor(FTR)~AwayTeam, data = total_train, trControl = train_control, 
               method = "glm", family=binomial())
model$results$Accuracy

################################################################################

# Referee

total_train$Referee <- factor(total_train$Referee, 
                              levels = names(sort(table(total_train$Referee), decreasing = TRUE)))

mosaicplot(Referee~FTR, data = total_train, xlab = "Referee", 
           ylab = "Home Team Win?" , las = 2)

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
model <- train(as.factor(FTR)~Referee, data = total_train, trControl = train_control, 
               method = "glm", family=binomial())
model$results$Accuracy

################################################################################

# HTR

mosaicplot(HTR~FTR, data = total_train, xlab = "Halftime Result", 
           ylab = "Home Team Win?" , las = 2)

ggplot(total_train, aes(x= FTR, fill = factor(HTR))) + geom_histogram(bins=10, color = "black") + 
  facet_wrap( ~ HTR, ncol=10) + 
  labs(title="Halftime Result", x="Home Team Win?") + 
  scale_x_continuous(breaks=c(0,1))

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
model <- train(as.factor(FTR)~HTR, data = total_train, trControl = train_control, 
               method = "glm", family=binomial())
model$results$Accuracy

################################################################################

# Clean Up Environment
rm(model)
rm(train_control)

################################################################################

# SAVE: 'EDA Checkpoint 2'

################################################################################