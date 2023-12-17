################################################################################

# NUMERIC EDA
# LOAD: 'Clean Training and Test Sets'

################################################################################

library(ggplot2)
library(boot)
library(caret)

# Create Logit Function (STA 279)
get_logitplot <- function(x, y, xname, bins, formulahere){
  
  nbins <- length(bins)
  probs.each <-NULL
  
  for(i in 1:nbins){
    if( i < nbins){
      scores.in <- which(x< bins[i+1] & x >= bins[i])
    } else{
      scores.in <- which(x> bins[i])
    }
    numerator  <- length(which(y[scores.in]==1))
    denominator      <- length(which(y[scores.in]==0))
    probs.each <- c(probs.each,ifelse(numerator>0 & denominator>0,numerator/denominator,0))
  }
  
  log.RR.each <- log(probs.each)
  
  to.remove <- which(log.RR.each=="-Inf")
  log.RR.each <-log.RR.each[-to.remove]
  bins <- bins[-to.remove]
  
  dataHere <-data.frame(c(bins), c(log.RR.each))
  
  ggplot(dataHere, aes(x =bins, y = log.RR.each)) + geom_point() + geom_smooth(method = "lm", formula = formulahere, se = FALSE)+labs(x = xname, y = "Log Odds")
}

################################################################################
################################################################################
################################################################################

# Response Count Table 

knitr::kable(table(total_train$FTR), col.names=c("Match Result", "Count"), 
             caption = "Home Team Win Distribution")
# 2,787 Home Wins
# 3,215 Away Wins or Draws
# Training P(Home Win) = 46.43%

################################################################################

# Pairs Plot

pairs(total_train[,-c(1,2,3,4,7,8,21,22)],pch=19,col=total_train$FTR+2)

cor(total_train[,-c(1,2,3,4,7,8,21,22)])

################################################################################

# Numeric Variable Plots (FTR vs. ___)

################################################################################

# HTHG

# Logit
HTHG_EDA <- get_logitplot(x=total_train$HTHG , y=total_train$FTR, xname = "Goals", 
                          bins = seq(from = 0, to = 5, by = 1), 
                          formula = y ~ x) + labs(title="Home Team Halftime Goals")
HTHG_EDA
# 1 Degree

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
accuracy <- rep(0,4)
for (i in 1:4){
  formula <- as.formula(sprintf("as.factor(FTR) ~ poly(HTHG, %d)", i))
  model <- train(formula,
                 data = total_train,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())
  accuracy[i] <- model$results$Accuracy
}
accuracy

################################################################################

# HTAG

# Logit
HTAG_EDA <- get_logitplot(x=total_train$HTAG , y=total_train$FTR, xname = "Goals", 
                          bins = seq(from = 0, to = 5, by = 1), 
                          formula = y ~ x) + labs(title="Away Team Halftime Goals")
HTAG_EDA
# 1 Degree

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
accuracy <- rep(0,4)
for (i in 1:4){
  formula <- as.formula(sprintf("as.factor(FTR) ~ poly(HTAG, %d)", i))
  model <- train(formula,
                 data = total_train,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())
  accuracy[i] <- model$results$Accuracy
}
accuracy

################################################################################

# HS

# Logit
HS_EDA <- get_logitplot(x=total_train$HS , y=total_train$FTR, xname = "Shots", 
                        bins = seq(from = 0, to = 39, by = 1), 
                        formula = y ~ poly(x,2)) + labs(title="Home Team Total Shots")
HS_EDA
# 2 Degree

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
accuracy <- rep(0,4)
for (i in 1:4){
  formula <- as.formula(sprintf("as.factor(FTR) ~ poly(HS, %d)", i))
  model <- train(formula,
                 data = total_train,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())
  accuracy[i] <- model$results$Accuracy
}
accuracy

################################################################################

# AS

# Logit
AS_EDA <- get_logitplot(x=total_train$AS , y=total_train$FTR, xname = "Shots", 
                        bins = seq(from = 0, to = 31, by = 1), 
                        formula = y ~ poly(x,1)) + labs(title="Away Team Total Shots")
AS_EDA
# 1 Degree

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
accuracy <- rep(0,4)
for (i in 1:4){
  formula <- as.formula(sprintf("as.factor(FTR) ~ poly(AS, %d)", i))
  model <- train(formula,
                 data = total_train,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())
  accuracy[i] <- model$results$Accuracy
}
accuracy

################################################################################

# HST

# Logit
HST_EDA <- get_logitplot(x=total_train$HST , y=total_train$FTR, xname = "Shots", 
                         bins = seq(from = 0, to = 24, by = 1), 
                         formula = y ~ poly(x,2)) + labs(title="Home Team Total Shots on Target")
HST_EDA
# 2 Degree

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
accuracy <- rep(0,4)
for (i in 1:4){
  formula <- as.formula(sprintf("as.factor(FTR) ~ poly(HST, %d)", i))
  model <- train(formula,
                 data = total_train,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())
  accuracy[i] <- model$results$Accuracy
}
accuracy

################################################################################

# AST 

# Logit
AST_EDA <- get_logitplot(x=total_train$AST , y=total_train$FTR, xname = "Shots", 
                         bins = seq(from = 0, to = 20, by = 1), 
                         formula = y ~ poly(x,2)) + labs(title="Away Team Total Shots on Target")
AST_EDA
# 2 Degree

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
accuracy <- rep(0,4)
for (i in 1:4){
  formula <- as.formula(sprintf("as.factor(FTR) ~ poly(AST, %d)", i))
  model <- train(formula,
                 data = total_train,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())
  accuracy[i] <- model$results$Accuracy
}
accuracy

################################################################################

# HC

# Logit
HC_EDA <- get_logitplot(x=total_train$HC , y=total_train$FTR, xname = "Corners", 
                        bins = seq(from = 0, to = 20, by = 1), 
                        formula = y ~ poly(x,2)) + labs(title="Home Team Total Corner Kicks")
HC_EDA
# 2 Degree

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
accuracy <- rep(0,4)
for (i in 1:4){
  formula <- as.formula(sprintf("as.factor(FTR) ~ poly(HC, %d)", i))
  model <- train(formula,
                 data = total_train,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())
  accuracy[i] <- model$results$Accuracy
}
accuracy

################################################################################

# AC

# Logit
AC_EDA <- get_logitplot(x=total_train$AC , y=total_train$FTR, xname = "Corners", 
                        bins = seq(from = 0, to = 20, by = 1), 
                        formula = y ~ poly(x,2)) + labs(title="Away Team Total Corner Kicks")
AC_EDA
# 2 Degree

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
accuracy <- rep(0,4)
for (i in 1:4){
  formula <- as.formula(sprintf("as.factor(FTR) ~ poly(AC, %d)", i))
  model <- train(formula,
                 data = total_train,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())
  accuracy[i] <- model$results$Accuracy
}
accuracy

################################################################################

# HF

# Logit
HF_EDA <- get_logitplot(x=total_train$HF , y=total_train$FTR, xname = "Fouls", 
                        bins = seq(from = 0, to = 28, by = 1), 
                        formula = y ~ poly(x,1)) + labs(title="Home Team Total Fouls Committed")
HF_EDA
# 1 Degree
# ???

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
accuracy <- rep(0,4)
for (i in 1:4){
  formula <- as.formula(sprintf("as.factor(FTR) ~ poly(HF, %d)", i))
  model <- train(formula,
                 data = total_train,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())
  accuracy[i] <- model$results$Accuracy
}
accuracy

################################################################################

# AF

# Logit
AF_EDA <- get_logitplot(x=total_train$AF , y=total_train$FTR, xname = "Fouls", 
                        bins = seq(from = 0, to = 28, by = 1), 
                        formula = y ~ poly(x,1)) + labs(title="Away Team Total Fouls Committed")
AF_EDA
# 1 Degree
# ???

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
accuracy <- rep(0,4)
for (i in 1:4){
  formula <- as.formula(sprintf("as.factor(FTR) ~ poly(AF, %d)", i))
  model <- train(formula,
                 data = total_train,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())
  accuracy[i] <- model$results$Accuracy
}
accuracy

################################################################################

# HY

# Logit
HY_EDA <- get_logitplot(x=total_train$HY , y=total_train$FTR, xname = "Cards", 
                        bins = seq(from = 0, to = 10, by = 1), 
                        formula = y ~ poly(x,1)) + labs(title="Home Team Total Yellow Cards")
HY_EDA
# 1 Degree

ggplot(total_train, aes(x= FTR, fill = factor(HY))) + geom_histogram(bins=10, color = "black") + 
  facet_wrap( ~ HY, ncol=10) + 
  labs(title="Home Team Total Yellow Cards", x="Home Team Win?") + 
  scale_x_continuous(breaks=c(0,1))

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
accuracy <- rep(0,4)
for (i in 1:4){
  formula <- as.formula(sprintf("as.factor(FTR) ~ poly(HY, %d)", i))
  model <- train(formula,
                 data = total_train,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())
  accuracy[i] <- model$results$Accuracy
}
accuracy

################################################################################

# AY

# Logit
AY_EDA <- get_logitplot(x=total_train$AY , y=total_train$FTR, xname = "Cards", 
                        bins = seq(from = 0, to = 10, by = 1), 
                        formula = y ~ poly(x,1)) + labs(title="Away Team Total Yellow Cards")
AY_EDA
# 1 Degree
# ???

ggplot(total_train, aes(x= FTR, fill = factor(AY))) + geom_histogram(bins=10, color = "black") + 
  facet_wrap( ~ AY, ncol=10) + 
  labs(title="Away Team Total Yellow Cards", x="Home Team Win?") + 
  scale_x_continuous(breaks=c(0,1))

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
accuracy <- rep(0,4)
for (i in 1:4){
  formula <- as.formula(sprintf("as.factor(FTR) ~ poly(AY, %d)", i))
  model <- train(formula,
                 data = total_train,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())
  accuracy[i] <- model$results$Accuracy
}
accuracy

################################################################################

# HR

# Logit
HR_EDA <- get_logitplot(x=total_train$HR , y=total_train$FTR, xname = "Cards", 
                        bins = seq(from = 0, to = 2, by = 1), 
                        formula = y ~ poly(x,1)) + labs(title="Home Team Total Red Cards")
HR_EDA
# 1 Degree
# ???

ggplot(total_train, aes(x= FTR, fill = factor(HR))) + geom_histogram(bins=10, color = "black") + 
  facet_wrap( ~ HR, ncol=10) + 
  labs(title="Home Team Total Red Cards", x="Home Team Win?") + 
  scale_x_continuous(breaks=c(0,1))

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
accuracy <- rep(0,4)
for (i in 1:4){
  formula <- as.formula(sprintf("as.factor(FTR) ~ poly(HR, %d)", i))
  model <- train(formula,
                 data = total_train,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())
  accuracy[i] <- model$results$Accuracy
}
accuracy

################################################################################

# AR

# Logit
AR_EDA <- get_logitplot(x=total_train$AR , y=total_train$FTR, xname = "Cards", 
                        bins = seq(from = 0, to = 2, by = 1), 
                        formula = y ~ poly(x,1)) + labs(title="Away Team Total Red Cards")
AR_EDA
# 1 Degree
# ???

ggplot(total_train, aes(x= FTR, fill = factor(AR))) + geom_histogram(bins=10, color = "black") + 
  facet_wrap( ~ AR, ncol=10) + 
  labs(title="Away Team Total Red Cards", x="Home Team Win?") + 
  scale_x_continuous(breaks=c(0,1))

######################################

#CV
set.seed(1)
train_control <- trainControl(method = "cv", number = 10)
accuracy <- rep(0,4)
for (i in 1:4){
  formula <- as.formula(sprintf("as.factor(FTR) ~ poly(AR, %d)", i))
  model <- train(formula,
                 data = total_train,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())
  accuracy[i] <- model$results$Accuracy
}
accuracy

################################################################################

# Clean Up Environment
rm(glm.fit)
rm(model)
rm(train_control)
rm(accuracy)
rm(cv.error)
rm(formula)
rm(i)

################################################################################

# SAVE: 'EDA Checkpoint 1'

################################################################################