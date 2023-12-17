################################################################################

# Logistic Regression 

################################################################################

# Full
formula(LRFull)

# CV
formula(LREDA)

# Best AIC Subset 
LRSubsetAIC <- glm(FTR ~ HomeTeam + AwayTeam + HTHG + HTAG + HTR + 
                     HST + AST + HC + AC + HY + HR + AR, family = "binomial", 
                   data = total_train)
summary(LRSubsetAIC)

# Best BIC Subset 
LRSubsetBIC <- glm(FTR ~ HST + AST + HC + HR + HTHG + HTAG + AR + AC, family = "binomial", data = total_train)
summary(LRSubsetBIC)


################################################################################
################################################################################
################################################################################

# LDA 
library(MASS)

################################################################################

# Full 
LDAFull <- lda(FTR~.-Date-Month, data = total_train)
LDAFull$means




# Best AIC Subset 
LDASubsetAIC <- lda(FTR ~ HomeTeam + AwayTeam + HTHG + HTAG + HTR + 
                      HST + AST + HC + AC + HY + HR + AR, data = total_train)

# Best BIC Subset 
LDASubsetBIC <- lda(FTR ~ HST + AST + HC + HR + HTHG + HTAG + AR + AC, data = total_train)

################################################################################
################################################################################
################################################################################

# QDA

################################################################################

# Full 
QDAFull <- qda(FTR~.-Date-Month-Referee, data = total_train)



# Best AIC Subset 
QDASubsetAIC <- qda(FTR ~ HomeTeam + AwayTeam + HTHG + HTAG + HTR + 
                      HST + AST + HC + AC + HY + HR + AR, data = total_train)

# Best BIC Subset 
QDASubsetBIC <- qda(FTR ~ HST + AST + HC + HR + HTHG + HTAG + AR + AC, data = total_train)

################################################################################
################################################################################
################################################################################

# KNN
library(class)

################################################################################

KNN_Train <- scale(total_train[,-c(1,2,3,4,7,8,21,22)])
KNN_COVID_Test <- scale(s_COVID[,-c(1,2,3,4,7,8,21,22)])

set.seed(1)
knn.pred <- knn(KNN_Train, KNN_COVID_Test, total_train$FTR, k = 3)
table(knn.pred, s_COVID$FTR)

# Need to do this for every season test set

################################################################################

# SAVE: 'Models'

################################################################################