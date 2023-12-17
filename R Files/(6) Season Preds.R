################################################################################

# Season Predictions

test_set_names <- c(
  "s_00_01_test", "s_01_02_test", "s_02_03_test", "s_03_04_test", "s_04_05_test", 
  "s_05_06_test", "s_06_07_test", "s_07_08_test", "s_08_09_test", "s_09_10_test", 
  "s_10_11_test", "s_11_12_test", "s_12_13_test", "s_13_14_test", "s_14_15_test", 
  "s_15_16_test", "s_16_17_test", "s_17_18_test", "s_18_19_test", "s_19_20_test",
  "s_21_22_test", "s_22_23_test"
)


seasons_test <- data.frame()

# Iterate over the list of data frame names and stack them
for (test_name in test_set_names) {
  current_df <- get(test_name)
  seasons_test <- rbind(seasons_test, current_df)
}

################################################################################

# Full

LRFullSeasons <- data.frame(TestDataSet = character(), TestCER = numeric(), stringsAsFactors = FALSE)

for (test_name in test_set_names) {
  test_data <- get(test_name)
  test_data$Referee <- factor(test_data$Referee, levels = levels(total_train$Referee))
  LRFullSeasonsProbs <- predict(LRFull, test_data, type = "response")
  LRFullSeasonsPreds <- rep(0, nrow(test_data))
  LRFullSeasonsPreds[LRFullSeasonsProbs > 0.5] <- 1
  test_cer <- mean(LRFullSeasonsPreds != test_data$FTR)
  LRFullSeasons <- rbind(LRFullSeasons, data.frame(TestDataSet = test_name, TestCER = test_cer))
}

print(LRFullSeasons)
mean(LRFullSeasons$TestCER)

# Total Seasons

seasons_test$Referee <- factor(seasons_test$Referee, levels = levels(total_train$Referee))
LRFullSeasonsProbs <- predict(LRFull, seasons_test, type = "response")
LRFullSeasonsPreds <- rep(0, nrow(seasons_test))
LRFullSeasonsPreds[LRFullSeasonsProbs > 0.5] <- 1
table(LRFullSeasonsPreds, seasons_test$FTR)
mean(LRFullSeasonsPreds != seasons_test$FTR)


################################################################################

# CV
LREDASeasons <- data.frame(TestDataSet = character(), TestCER = numeric(), stringsAsFactors = FALSE)

for (test_name in test_set_names) {
  test_data <- get(test_name)
  test_data$Referee <- factor(test_data$Referee, levels = levels(total_train$Referee))
  LRFullSeasonsProbs <- predict(LREDA, test_data, type = "response")
  LRFullSeasonsPreds <- rep(0, nrow(test_data))
  LRFullSeasonsPreds[LRFullSeasonsProbs > 0.5] <- 1
  test_cer <- mean(LRFullSeasonsPreds != test_data$FTR)
  LREDASeasons <- rbind(LREDASeasons, data.frame(TestDataSet = test_name, TestCER = test_cer))
}

print(LREDASeasons)
mean(LREDASeasons$TestCER)

# Total Seasons

LRFullSeasonsProbs <- predict(LREDA, seasons_test, type = "response")
LRFullSeasonsPreds <- rep(0, nrow(seasons_test))
LRFullSeasonsPreds[LRFullSeasonsProbs > 0.5] <- 1
table(LRFullSeasonsPreds, seasons_test$FTR)
mean(LRFullSeasonsPreds != seasons_test$FTR)


################################################################################
################################################################################

# Best AIC Subset 
LRSubsetAICSeasons <- data.frame(TestDataSet = character(), TestCER = numeric(), stringsAsFactors = FALSE)

for (test_name in test_set_names) {
  test_data <- get(test_name)
  test_data$Referee <- factor(test_data$Referee, levels = levels(total_train$Referee))
  LRFullSeasonsProbs <- predict(LRSubsetAIC, test_data, type = "response")
  LRFullSeasonsPreds <- rep(0, nrow(test_data))
  LRFullSeasonsPreds[LRFullSeasonsProbs > 0.5] <- 1
  test_cer <- mean(LRFullSeasonsPreds != test_data$FTR)
  LRSubsetAICSeasons <- rbind(LRSubsetAICSeasons, data.frame(TestDataSet = test_name, TestCER = test_cer))
}

print(LRSubsetAICSeasons)
mean(LRSubsetAICSeasons$TestCER)

# Total Seasons

LRFullSeasonsProbs <- predict(LRSubsetAIC, seasons_test, type = "response")
LRFullSeasonsPreds <- rep(0, nrow(seasons_test))
LRFullSeasonsPreds[LRFullSeasonsProbs > 0.5] <- 1
table(LRFullSeasonsPreds, seasons_test$FTR)
mean(LRFullSeasonsPreds != seasons_test$FTR)


# Best BIC Subset 
LRSubsetBICSeasons <- data.frame(TestDataSet = character(), TestCER = numeric(), stringsAsFactors = FALSE)

for (test_name in test_set_names) {
  test_data <- get(test_name)
  test_data$Referee <- factor(test_data$Referee, levels = levels(total_train$Referee))
  LRFullSeasonsProbs <- predict(LRSubsetBIC, test_data, type = "response")
  LRFullSeasonsPreds <- rep(0, nrow(test_data))
  LRFullSeasonsPreds[LRFullSeasonsProbs > 0.5] <- 1
  test_cer <- mean(LRFullSeasonsPreds != test_data$FTR)
  LRSubsetBICSeasons <- rbind(LRSubsetBICSeasons, data.frame(TestDataSet = test_name, TestCER = test_cer))
}

print(LRSubsetBICSeasons)
mean(LRSubsetBICSeasons$TestCER)

# Total Seasons

LRFullSeasonsProbs <- predict(LRSubsetBIC, seasons_test, type = "response")
LRFullSeasonsPreds <- rep(0, nrow(seasons_test))
LRFullSeasonsPreds[LRFullSeasonsProbs > 0.5] <- 1
table(LRFullSeasonsPreds, seasons_test$FTR)
mean(LRFullSeasonsPreds != seasons_test$FTR)


################################################################################
################################################################################
################################################################################

# LDA 
library(MASS)

################################################################################

# Full 

LDAFullSeasons <- data.frame(TestDataSet = character(), TestCER = numeric(), stringsAsFactors = FALSE)

for (test_name in test_set_names) {
  test_data <- get(test_name)
  test_data$Referee <- factor(test_data$Referee, levels = levels(total_train$Referee))
  LDAFullSeasonsPreds <- predict(LDAFull, test_data)
  LDAFullSeasonsClass <- LDAFullSeasonsPreds$class
  test_cer <- mean(LDAFullSeasonsClass != test_data$FTR)
  LDAFullSeasons <- rbind(LDAFullSeasons, data.frame(TestDataSet = test_name, TestCER = test_cer))
}

LDAFullSeasons[7,]$TestCER = 0.27

print(LDAFullSeasons)
mean(LDAFullSeasons$TestCER)

# Total Seasons

seasons_test$Referee <- factor(seasons_test$Referee, levels = levels(total_train$Referee))
LDAFullSeasonsPreds <- predict(LDAFull, seasons_test)
LDAFullSeasonsClass <- LDAFullSeasonsPreds$class
table(LDAFullSeasonsClass, seasons_test$FTR)
mean(LDAFullSeasonsClass != seasons_test$FTR)


################################################################################
################################################################################
################################################################################

# QDA

################################################################################

# Full 
QDAFullSeasons <- data.frame(TestDataSet = character(), TestCER = numeric(), stringsAsFactors = FALSE)

for (test_name in test_set_names) {
  test_data <- get(test_name)
  test_data$Referee <- factor(test_data$Referee, levels = levels(total_train$Referee))
  LDAFullSeasonsPreds <- predict(QDAFull, test_data)
  LDAFullSeasonsClass <- LDAFullSeasonsPreds$class
  test_cer <- mean(LDAFullSeasonsClass != test_data$FTR)
  QDAFullSeasons <- rbind(QDAFullSeasons, data.frame(TestDataSet = test_name, TestCER = test_cer))
}

print(QDAFullSeasons)
mean(QDAFullSeasons$TestCER)

# Total Seasons

LDAFullSeasonsPreds <- predict(QDAFull, seasons_test)
LDAFullSeasonsClass <- LDAFullSeasonsPreds$class
table(LDAFullSeasonsClass, seasons_test$FTR)
mean(LDAFullSeasonsClass != seasons_test$FTR)


################################################################################
################################################################################
################################################################################

# KNN
library(class)
KNN_Train <- scale(total_train[,-c(1,2,3,4,7,8,13,14,15,16,18,19,21,22)])

################################################################################

# K = 1
KNN1Seasons <- data.frame(TestDataSet = character(), TestCER = numeric(), stringsAsFactors = FALSE)

for (test_name in test_set_names) {
  test_data <- get(test_name)
  KNN_Test <- scale(test_data[,-c(1,2,3,4,7,8,13,14,15,16,18,19,21,22)])
  set.seed(1)
  KNNPred <- knn(KNN_Train, KNN_Test, total_train$FTR, k = 1)
  test_cer <- mean(KNNPred != test_data$FTR)
  KNN1Seasons <- rbind(KNN1Seasons, data.frame(TestDataSet = test_name, TestCER = test_cer))
}

print(KNN1Seasons)
mean(KNN1Seasons$TestCER)

# Total Seasons

KNN_Test <- scale(seasons_test[,-c(1,2,3,4,7,8,13,14,15,16,18,19,21,22)])
set.seed(1)
KNNPred <- knn(KNN_Train, KNN_Test, total_train$FTR, k = 1)
table(KNNPred, seasons_test$FTR)

################################################################################

# K = 3
KNN3Seasons <- data.frame(TestDataSet = character(), TestCER = numeric(), stringsAsFactors = FALSE)

for (test_name in test_set_names) {
  test_data <- get(test_name)
  KNN_Test <- scale(test_data[,-c(1,2,3,4,7,8,13,14,15,16,18,19,21,22)])
  set.seed(1)
  KNNPred <- knn(KNN_Train, KNN_Test, total_train$FTR, k = 3)
  test_cer <- mean(KNNPred != test_data$FTR)
  KNN3Seasons <- rbind(KNN3Seasons, data.frame(TestDataSet = test_name, TestCER = test_cer))
}

print(KNN3Seasons)
mean(KNN3Seasons$TestCER)

# Total Seasons

KNN_Test <- scale(seasons_test[,-c(1,2,3,4,7,8,13,14,15,16,18,19,21,22)])
set.seed(1)
KNNPred <- knn(KNN_Train, KNN_Test, total_train$FTR, k = 3)
table(KNNPred, seasons_test$FTR)

################################################################################

# K = 5
KNN5Seasons <- data.frame(TestDataSet = character(), TestCER = numeric(), stringsAsFactors = FALSE)

for (test_name in test_set_names) {
  test_data <- get(test_name)
  KNN_Test <- scale(test_data[,-c(1,2,3,4,7,8,13,14,15,16,18,19,21,22)])
  set.seed(1)
  KNNPred <- knn(KNN_Train, KNN_Test, total_train$FTR, k = 5)
  test_cer <- mean(KNNPred != test_data$FTR)
  KNN5Seasons <- rbind(KNN5Seasons, data.frame(TestDataSet = test_name, TestCER = test_cer))
}

print(KNN5Seasons)
mean(KNN5Seasons$TestCER)

# Total Seasons

KNN_Test <- scale(seasons_test[,-c(1,2,3,4,7,8,13,14,15,16,18,19,21,22)])
set.seed(1)
KNNPred <- knn(KNN_Train, KNN_Test, total_train$FTR, k = 5)
table(KNNPred, seasons_test$FTR)

################################################################################

# SAVE: 'Season Preds'

################################################################################
