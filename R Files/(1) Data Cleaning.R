################################################################################

# IMPORT .csv FILES AS DATA FRAMES

################################################################################

library(tidyverse)
file_list <- list.files(path = "/Volumes/Ultra Touch/CofC MATH 540 EPL Project/Clean Data", 
                        pattern = "\\.csv$", full.names = TRUE)
data_list <- map(file_list, read_csv)
cleaned_names <- paste0("s_", tools::file_path_sans_ext(basename(file_list)))
names(data_list) <- cleaned_names
list2env(data_list, envir = .GlobalEnv)

################################################################################

# SAVE: 'Raw Data from Jupyter Notebook'

################################################################################
################################################################################
################################################################################

# VALIDATION SET CREATION
# LOAD: 'Raw Data from Jupyter Notebook'

################################################################################

# TEST SETS

# Drop main COVID season
rm(s_20_21)

df_names <- c("s_00_01", "s_01_02", "s_02_03", "s_03_04", "s_04_05", 
              "s_05_06", "s_06_07", "s_07_08", "s_08_09", "s_09_10", 
              "s_10_11", "s_11_12", "s_12_13", "s_13_14", "s_14_15", 
              "s_15_16", "s_16_17", "s_17_18", "s_18_19", "s_19_20",
              "s_21_22", "s_22_23")  

# Loop through each data frame to create season test sets
for (df_name in df_names) {
  
  df <- get(df_name)
  
  df <- df[complete.cases(df), ]
  
  set.seed(123)
  
  test_indices <- sample(1:nrow(df), 100)
  test_set <- df[test_indices, ]
  
  training_set <- df[-test_indices, ]
  
  assign(df_name, training_set)
  
  test_df_name <- paste0(df_name, "_test")
  assign(test_df_name, test_set)
}

################################################################################

# TRAINING SETS

total_train <- data.frame()

# Loop through each data frame to create stacked training set
for (df_name in df_names) {
  
  df <- get(df_name)
  
  total_train <- rbind(total_train, df)
  
  rm(list = df_name)
}

# Remove NA Rows
sum(is.na(total_train))
total_train <- na.omit(total_train)

# Clean up environment
rm(df)
rm(test_set)
rm(training_set)
rm(data_list)
rm(cleaned_names)
rm(df_name)
rm(df_names)
rm(file_list)
rm(test_df_name)
rm(test_indices)

################################################################################

# SAVE: 'Clean Training and Test Sets'

################################################################################