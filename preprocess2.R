library(readxl)
library(skimr)
library(tidyverse)



dataset <- read_excel("stat_consultancy.xlsx")

View(dataset)
skim(dataset)

# convert NA in real missing
unique(dataset$Rice_variety)

# Replace the string "NA" with actual NA
dataset$Rice_variety[dataset$Rice_variety == "NA"] <- NA

# Confirm replacement
sum(is.na(dataset$Rice_variety))  # Shows number of missing values
unique(dataset$Rice_variety)      # Should no longer display "NA" as a string

# List all character variables
char_vars <- names(dataset)[sapply(dataset, is.character)]

# Check for unusual "NA-like" strings in each character column
for (var in char_vars) {
  cat("Unique values in", var, ":\n")
  print(unique(dataset[[var]]))
  cat("\n")
}

# Replace all "NA", "N/A", "", or " " with actual NA in all character columns
for (var in char_vars) {
  dataset[[var]] <- ifelse(trimws(dataset[[var]]) %in% c("NA", "N/A", "", " "), NA, dataset[[var]])
}


#### --- Terrain ---
unique(dataset$TERRAIN)

library(dplyr)

dataset <- dataset %>%
  mutate(TERRAIN = case_when(
    TERRAIN %in% c("Rolling, Undulating, Flat", "Rolling, Undulating & flat") ~ "Rolling, Undulating, Flat",
    TERRAIN %in% c("Rolling & Undulating", "Undulating & rolling") ~ "Rolling & Undulating",
    TRUE ~ TERRAIN  # Keep other values as they are
  ))

unique(dataset$TERRAIN)


dataset %>%
  count(TERRAIN, sort = TRUE)


# First, count frequencies
terrain_counts <- dataset %>%
  count(TERRAIN)

# Define threshold for 'rare' categories
threshold <- 70

# Create new variable TERRAIN_ADJ with rare categories replaced by "Other"
dataset <- dataset %>%
  left_join(terrain_counts, by = "TERRAIN") %>%
  mutate(TERRAIN_ADJ = ifelse(n < threshold, "Other", TERRAIN)) %>%
  select(-n)  # remove helper count column

dataset %>% count(TERRAIN_ADJ, sort = TRUE)

#### --- Rice Variety ---
unique(dataset$Rice_variety)

# First, count frequencies
rice_variety_counts <- dataset %>%
  count(Rice_variety)

skim(dataset)

### create variable infected
dataset$Infection_Status <- factor(ifelse(dataset$Gall_count > 0, 1, 0), levels = c(0, 1), labels = c("No", "Yes"))

# Assuming your dataset is named dataset_1
colnames(dataset)[17] <- "Stage_of_Maturity"




### make the dataset 
dataset_1 <- dataset %>%
  select(-"Province", -"District",-"AGRO_ECO_R",
         -"MAJOR_SOIL", -"GPS_location", -"Season",
         -"Date_of_maturity", -"Prediction_on suseptibility of rice variety",
         -"Irrigation", -"X", -"Y", -"Age_DAS", -"TERRAIN_ADJ")


dataset_2 <- dataset %>%
  select(-"Province", -"District",-"AGRO_ECO_R",
         -"MAJOR_SOIL", -"GPS_location", -"Season",
          -"Date_of_maturity", -"Prediction_on suseptibility of rice variety",
         -"Irrigation", -"X", -"Y", -"Age_DAS", -"TERRAIN")


dataset_1 <- dataset_1 %>%
  mutate(across(where(is.character), as.factor))


# Step 1: Clean names with janitor (e.g., replace spaces and special chars with _)
dataset_1 <- dataset_1 %>% 
  janitor::clean_names()


# Step 3: Convert all character columns to factor
dataset_1 <- dataset_1 %>%
  mutate(across(where(is.character), as.factor))

# Check cleaned variable names
names(dataset_1)


write.csv(dataset_1, "dataset1.csv", row.names = FALSE)

#### --- dataset 2 ---
dataset_2 <- dataset_2 %>%
  mutate(across(where(is.character), as.factor))


# Step 1: Clean names with janitor (e.g., replace spaces and special chars with _)
dataset_2 <- dataset_2 %>% 
  janitor::clean_names()


# Step 3: Convert all character columns to factor
dataset_2 <- dataset_2 %>%
  mutate(across(where(is.character), as.factor))

# Check cleaned variable names
names(dataset_2)
