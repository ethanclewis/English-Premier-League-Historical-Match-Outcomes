library(ggplot2)

data <- data.frame(
  Season = c("2000/01", "2001/02", "2002/03", "2003/04", "2004/05", "2005/06", "2006/07", 
             "2007/08", "2008/09", "2009/10", "2010/11", "2011/12", "2012/13", "2013/14", 
             "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "COVID", "2021/22", "2022/23")
)

################################################################################

# Test CER Plots 

# Full
LRFullSeasons

COVID_row <- data.frame(
  TestDataSet = "COVID",
  TestCER = 0.1902655
)

LRFullSeasons <- rbind(LRFullSeasons[1:20,], COVID_row, LRFullSeasons[21:22,])
LRFullSeasons <- cbind(data, LRFullSeasons)
COVID_Highlight <- LRFullSeasons[LRFullSeasons$Season == "COVID",]


LRFullSeasons$Season <- factor(LRFullSeasons$Season, levels = unique(LRFullSeasons$Season))

ggplot(data = LRFullSeasons, aes(x = Season, y = TestCER, group = 1)) + 
  geom_line() + geom_point() +
  geom_point(data = COVID_Highlight, aes(x = Season, y = TestCER), color = "red", size = 3) +
  geom_hline(yintercept = 0.2295455, linetype = "dashed", color = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Logistic Regression (Full Model)", y = "Test CER")


################################################################################

# CV
LREDASeasons

COVID_row <- data.frame(
  TestDataSet = "COVID",
  TestCER = 0.1725664
)

LREDASeasons <- rbind(LREDASeasons[1:20,], COVID_row, LREDASeasons[21:22,])
LREDASeasons <- cbind(data, LREDASeasons)
COVID_Highlight <- LREDASeasons[LREDASeasons$Season == "COVID",]

LREDASeasons$Season <- factor(LREDASeasons$Season, levels = unique(LREDASeasons$Season))

ggplot(data = LREDASeasons, aes(x = Season, y = TestCER, group = 1)) + 
  geom_line() + geom_point() +
  geom_point(data = COVID_Highlight, aes(x = Season, y = TestCER), color = "red", size = 3) +
  geom_hline(yintercept = 0.2254545, linetype = "dashed", color = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Logistic Regression (10-Fold Cross-Validation Subset)", y = "Test CER")

################################################################################

# AIC 
LRSubsetAICSeasons

COVID_row <- data.frame(
  TestDataSet = "COVID",
  TestCER = 0.1792035
)

LRSubsetAICSeasons <- rbind(LRSubsetAICSeasons[1:20,], COVID_row, LRSubsetAICSeasons[21:22,])
LRSubsetAICSeasons <- cbind(data, LRSubsetAICSeasons)
COVID_Highlight <- LRSubsetAICSeasons[LRSubsetAICSeasons$Season == "COVID",]

LRSubsetAICSeasons$Season <- factor(LRSubsetAICSeasons$Season, levels = unique(LRSubsetAICSeasons$Season))

ggplot(data = LRSubsetAICSeasons, aes(x = Season, y = TestCER, group = 1)) + 
  geom_line() + geom_point() +
  geom_point(data = COVID_Highlight, aes(x = Season, y = TestCER), color = "red", size = 3) +
  geom_hline(yintercept = 0.2231818, linetype = "dashed", color = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Logistic Regression (Stepwise Selection Subset on AIC)", y = "Test CER")

################################################################################

# BIC 
LRSubsetBICSeasons

COVID_row <- data.frame(
  TestDataSet = "COVID",
  TestCER = 0.1836283
)

LRSubsetBICSeasons <- rbind(LRSubsetBICSeasons[1:20,], COVID_row, LRSubsetBICSeasons[21:22,])
LRSubsetBICSeasons <- cbind(data, LRSubsetBICSeasons)
COVID_Highlight <- LRSubsetBICSeasons[LRSubsetBICSeasons$Season == "COVID",]

LRSubsetBICSeasons$Season <- factor(LRSubsetBICSeasons$Season, levels = unique(LRSubsetBICSeasons$Season))

ggplot(data = LRSubsetBICSeasons, aes(x = Season, y = TestCER, group = 1)) + 
  geom_line() + geom_point() +
  geom_point(data = COVID_Highlight, aes(x = Season, y = TestCER), color = "red", size = 3) +
  geom_hline(yintercept = 0.2209091, linetype = "dashed", color = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Logistic Regression (Stepwise Selection Subset on BIC)", y = "Test CER")

################################################################################

# LDA 
LDAFullSeasons

COVID_row <- data.frame(
  TestDataSet = "COVID",
  TestCER = 0.1747788
)

LDAFullSeasons <- rbind(LDAFullSeasons[1:20,], COVID_row, LDAFullSeasons[21:22,])
LDAFullSeasons <- cbind(data, LDAFullSeasons)
COVID_Highlight <- LDAFullSeasons[LDAFullSeasons$Season == "COVID",]

LDAFullSeasons$Season <- factor(LDAFullSeasons$Season, levels = unique(LDAFullSeasons$Season))

ggplot(data = LDAFullSeasons, aes(x = Season, y = TestCER, group = 1)) + 
  geom_line() + geom_point() +
  geom_point(data = COVID_Highlight, aes(x = Season, y = TestCER), color = "red", size = 3) +
  geom_hline(yintercept = 0.2281818, linetype = "dashed", color = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Linear Discriminant Analysis", y = "Test CER")

################################################################################

# QDA
QDAFullSeasons

COVID_row <- data.frame(
  TestDataSet = "COVID",
  TestCER = 0.3119469
)

QDAFullSeasons <- rbind(QDAFullSeasons[1:20,], COVID_row, QDAFullSeasons[21:22,])
QDAFullSeasons <- cbind(data, QDAFullSeasons)
COVID_Highlight <- QDAFullSeasons[QDAFullSeasons$Season == "COVID",]

QDAFullSeasons$Season <- factor(QDAFullSeasons$Season, levels = unique(QDAFullSeasons$Season))

ggplot(data = QDAFullSeasons, aes(x = Season, y = TestCER, group = 1)) + 
  geom_line() + geom_point() +
  geom_point(data = COVID_Highlight, aes(x = Season, y = TestCER), color = "red", size = 3) +
  geom_hline(yintercept = 0.3104545, linetype = "dashed", color = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Quadratic Discriminant Analysis", y = "Test CER")

################################################################################

# 1-NN
KNN1Seasons

COVID_row <- data.frame(
  TestDataSet = "COVID",
  TestCER = 0.3075221
)

KNN1Seasons <- rbind(KNN1Seasons[1:20,], COVID_row, KNN1Seasons[21:22,])
KNN1Seasons <- cbind(data, KNN1Seasons)
COVID_Highlight <- KNN1Seasons[KNN1Seasons$Season == "COVID",]

KNN1Seasons$Season <- factor(KNN1Seasons$Season, levels = unique(KNN1Seasons$Season))

ggplot(data = KNN1Seasons, aes(x = Season, y = TestCER, group = 1)) + 
  geom_line() + geom_point() +
  geom_point(data = COVID_Highlight, aes(x = Season, y = TestCER), color = "red", size = 3) +
  geom_hline(yintercept = 0.2995455, linetype = "dashed", color = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "1-Nearest Neighbor", y = "Test CER")

################################################################################

# 3-NN
KNN3Seasons

COVID_row <- data.frame(
  TestDataSet = "COVID",
  TestCER = 0.2345133
)

KNN3Seasons <- rbind(KNN3Seasons[1:20,], COVID_row, KNN3Seasons[21:22,])
KNN3Seasons <- cbind(data, KNN3Seasons)
COVID_Highlight <- KNN3Seasons[KNN3Seasons$Season == "COVID",]

KNN3Seasons$Season <- factor(KNN3Seasons$Season, levels = unique(KNN3Seasons$Season))

ggplot(data = KNN3Seasons, aes(x = Season, y = TestCER, group = 1)) + 
  geom_line() + geom_point() +
  geom_point(data = COVID_Highlight, aes(x = Season, y = TestCER), color = "red", size = 3) +
  geom_hline(yintercept = 0.2668182, linetype = "dashed", color = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "3-Nearest Neighbor", y = "Test CER")

################################################################################

# 5-NN
KNN5Seasons

COVID_row <- data.frame(
  TestDataSet = "COVID",
  TestCER = 0.2323009
)

KNN5Seasons <- rbind(KNN5Seasons[1:20,], COVID_row, KNN5Seasons[21:22,])
KNN5Seasons <- cbind(data, KNN5Seasons)
COVID_Highlight <- KNN5Seasons[KNN5Seasons$Season == "COVID",]

KNN5Seasons$Season <- factor(KNN5Seasons$Season, levels = unique(KNN5Seasons$Season))

ggplot(data = KNN5Seasons, aes(x = Season, y = TestCER, group = 1)) + 
  geom_line() + geom_point() +
  geom_point(data = COVID_Highlight, aes(x = Season, y = TestCER), color = "red", size = 3) +
  geom_hline(yintercept = 0.2513636, linetype = "dashed", color = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "5-Nearest Neighbor", y = "Test CER")
