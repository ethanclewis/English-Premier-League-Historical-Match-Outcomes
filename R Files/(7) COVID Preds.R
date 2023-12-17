################################################################################

# COVID Predictions

s_COVID$Month <- factor(s_COVID$Month, levels = levels(total_train$Month))

################################################################################
################################################################################
################################################################################

# Logistic Regression 

################################################################################

# Full

LRFullCOVIDProbs <- predict(LRFull, s_COVID, type = "response")
LRFullCOVIDPreds <- rep(0, nrow(s_COVID))
LRFullCOVIDPreds[LRFullCOVIDProbs > 0.5] <- 1

LRFullCOVID_CER <- mean(LRFullCOVIDPreds != s_COVID$FTR)
LRFullCOVID_CER
table(LRFullCOVIDPreds, s_COVID$FTR)


################################################################################

# CV

LREDAProbs <- predict(LREDA, s_COVID, type = "response")
LREDAPreds <- rep(0, 452)
LREDAPreds[LREDAProbs > 0.5] <- 1

LR_EDA_COVID_CER <- mean(LREDAPreds != s_COVID$FTR)
LR_EDA_COVID_CER
table(LREDAPreds, s_COVID$FTR)

################################################################################

# Best AIC Subset 
formula(LRSubsetAIC)

LRSubsetAICProbs <- predict(LRSubsetAIC, s_COVID, type = "response")
LRSubsetAICPreds <- rep(0, 452)
LRSubsetAICPreds[LRSubsetAICProbs > 0.5] <- 1

LRSubsetAIC_COVID_CER <- mean(LRSubsetAICPreds != s_COVID$FTR)
LRSubsetAIC_COVID_CER
table(LRSubsetAICPreds, s_COVID$FTR)

################################################################################

# Best BIC Subset 
formula(LRSubsetBIC)

LRSubsetBICProbs <- predict(LRSubsetBIC, s_COVID, type = "response")
LRSubsetBICPreds <- rep(0, 452)
LRSubsetBICPreds[LRSubsetBICProbs > 0.5] <- 1

LRSubsetBIC_COVID_CER <- mean(LRSubsetBICPreds != s_COVID$FTR)
LRSubsetBIC_COVID_CER
table(LRSubsetBICPreds, s_COVID$FTR)



################################################################################
################################################################################
################################################################################

# LDA 
library(MASS)

################################################################################

# Full 
LDAFullPreds <- predict(LDAFull, s_COVID)
LDAFullClass <- LDAFullPreds$class

LDAFullCOVID_CER <- mean(LDAFullClass != s_COVID$FTR)
LDAFullCOVID_CER
table(LDAFullClass, s_COVID$FTR)



################################################################################
################################################################################
################################################################################

# QDA

################################################################################

# Full 
QDAFullPreds <- predict(QDAFull, s_COVID)
QDAFullClass <- QDAFullPreds$class

QDAFullCOVID_CER <- mean(QDAFullClass != s_COVID$FTR)
QDAFullCOVID_CER
table(QDAFullClass, s_COVID$FTR)


################################################################################
################################################################################
################################################################################

# KNN
library(class)

################################################################################

KNN_Train <- scale(total_train[,-c(1,2,3,4,7,8,13,14,15,16,18,19,21,22)])
KNN_COVID_Test <- scale(s_COVID[,-c(1,2,3,4,7,8,13,14,15,16,18,19,21,22)])

set.seed(1)
knn.pred <- knn(KNN_Train, KNN_COVID_Test, total_train$FTR, k = 5)

mean(knn.pred != s_COVID$FTR)

table(knn.pred, s_COVID$FTR)

# Need to do this for every season test set

################################################################################

# SAVE: 'COVID Preds'

################################################################################