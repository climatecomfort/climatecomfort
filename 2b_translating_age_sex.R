# R Script to Translate 'age' and 'sex' columns to English
#
# This script reads the 'final_adjusted_points.csv' file, translates the
# unique values in the 'age' and 'sex' columns from various languages
# (Portuguese, Spanish, French, Chinese, Thai) into English, and
# then saves the result to a new CSV file.

# 1. SETUP
# Install and load necessary libraries. 'readr' is good for reading CSV files.
# If you don't have it, uncomment the next line to install it.
# install.packages("readr")
library(readr)

# --- IMPORTANT ---
# Set your working directory to the folder where your CSV file is located.
# You MUST change the path below to match the location on your computer.
# setwd("C:/Your/Folder/Path")

# 2. LOAD DATA
# Read the CSV file into a data frame.
# This assumes 'final_adjusted_points.csv' is in your working directory.
#final_adjusted_points <- read.csv("final_adjusted_points.csv")
final_adjusted_points <- readRDS("final_adjusted_points.rds")

# OPTIONAL: VIEW UNIQUE VALUES
# To ensure you are translating all necessary categories, you can
# uncomment the lines below to see the unique values in each column.
# print("Unique values in 'sex' column before translation:")
# print(unique(final_adjusted_points$sex))
# print("Unique values in 'age' column before translation:")
# print(unique(final_adjusted_points$age))

# 3. TRANSLATE THE 'sex' COLUMN
# This section replaces non-English entries with their English equivalents.

# Portuguese & Spanish translations
final_adjusted_points$sex <- gsub("Masculino", "Male", final_adjusted_points$sex)
final_adjusted_points$sex <- gsub("Feminino", "Female", final_adjusted_points$sex)
final_adjusted_points$sex <- gsub("Femenino", "Female", final_adjusted_points$sex) # Spanish variant

# French translations
final_adjusted_points$sex <- gsub("Homme", "Male", final_adjusted_points$sex)
final_adjusted_points$sex <- gsub("Femme", "Female", final_adjusted_points$sex)
final_adjusted_points$sex <- gsub("Le mâle", "Male", final_adjusted_points$sex)
final_adjusted_points$sex <- gsub("Le féminin", "Female", final_adjusted_points$sex)

# Chinese translations
final_adjusted_points$sex <- gsub("男", "Male", final_adjusted_points$sex)
final_adjusted_points$sex <- gsub("女", "Female", final_adjusted_points$sex)

# Thai translations
final_adjusted_points$sex <- gsub("ชาย", "Male", final_adjusted_points$sex)
final_adjusted_points$sex <- gsub("หญิง", "Female", final_adjusted_points$sex)


# 4. TRANSLATE THE 'age' COLUMN
# This section replaces non-English age ranges with their English equivalents.

# Portuguese translations
final_adjusted_points$age <- gsub("até 29 anos", "up to 29 years", final_adjusted_points$age)
final_adjusted_points$age <- gsub("Até 29 anos", "up to 29 years", final_adjusted_points$age)
final_adjusted_points$age <- gsub("30 a 44 anos", "30 to 44 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("30 a 40 anos", "30 to 44 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("45 a 59 anos", "45 to 59 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("45 a 60 anos", "45 to 60 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("60 anos ou mais", "more than 60 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("acima de 60 anos", "more than 60 years old", final_adjusted_points$age)

# Spanish translations
final_adjusted_points$age <- gsub("hasta 29 años", "up to 29 years", final_adjusted_points$age)
final_adjusted_points$age <- gsub("30 a 44 años", "30 to 44 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("45 a 59 años", "45 to 59 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("45 a 60 años", "45 to 60 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("60 años o más", "more than 60 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("más de 60 años", "more than 60 years old", final_adjusted_points$age)

# French translations
final_adjusted_points$age <- gsub("18 à 29 ans", "up to 29 years", final_adjusted_points$age)
final_adjusted_points$age <- gsub("Ou jusqu'à 29 ans", "up to 29 years", final_adjusted_points$age)
final_adjusted_points$age <- gsub("Les 30 à 44 ans", "30 to 44 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("30 à 44 ans", "30 to 44 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("45 à 59 ans", "45 to 59 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("Les 45 à 60 ans", "45 to 60 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("60 ans ou plus", "more than 60 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("Ou plus de 60 ans", "more than 60 years old", final_adjusted_points$age)

# Chinese translations
final_adjusted_points$age <- gsub("30-44岁", "30 to 44 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("45-59岁", "45 to 59 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("60岁或以上", "more than 60 years old", final_adjusted_points$age)

# Thai translations
final_adjusted_points$age <- gsub("น้อยกว่า 30 ปี", "up to 29 years", final_adjusted_points$age)
final_adjusted_points$age <- gsub("18 ถึง 29 ปี", "up to 29 years", final_adjusted_points$age)
final_adjusted_points$age <- gsub("30 ถึง 44 ปี", "30 to 44 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("30 - 44 ปี", "30 to 44 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("45 ถึง 59 ปี", "45 to 59 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("45 - 60 ปี", "45 to 60 years old", final_adjusted_points$age)
final_adjusted_points$age <- gsub("60 ปีขึ้นไป", "more than 60 years old", final_adjusted_points$age)

# Category Consolidation
final_adjusted_points$age <- gsub("less to 29 years", "up to 29 years", final_adjusted_points$age)
final_adjusted_points$age <- gsub("Les 30 to 44 years old", "30 to 44 years old", final_adjusted_points$age)


# 5. VERIFY TRANSLATIONS
# Print the unique values for the columns to the console to check the results.
# You should only see the English-language categories now.
print("Unique values in 'sex' column after translation:")
unique(final_adjusted_points$sex)

print("Unique values in 'age' column after translation:")
unique(final_adjusted_points$age)

# 6. SAVE THE TRANSLATED DATA
# Write the modified data frame to a new CSV file.
# Using row.names = FALSE prevents R from writing an extra column for row numbers.
View(final_adjusted_points)
#write.csv(final_adjusted_points, "final_adjusted_points_translated.csv", row.names = FALSE)
saveRDS(final_adjusted_points, "final_adjusted_points_translated.rds")
print("Translation complete. The new file 'final_adjusted_points_translated.csv' has been saved.")

# R Script to Create Numerical Codes for Age and Sex
#
# This script reads the 'final_adjusted_points_translated.csv' file,
# which is the output of the previous translation script. It then creates
# three new columns:
#   1. 'sex_number': A numerical code for sex (1 = Male, 2 = Female).
#   2. 'age_number': A sequential numerical code for each age category.
#   3. 'combined_code': A calculated code based on sex and age numbers.
# The final table is saved to a new CSV file.

# 1. SETUP
# Install and load necessary libraries. 'dplyr' is excellent for data manipulation.
# If you don't have it, uncomment the next line to install it.
# install.packages("dplyr")
library(dplyr)

# --- IMPORTANT ---
# Set your working directory to the folder where your CSV file is located.
# You MUST change the path below to match the location on your computer.
# setwd("C:/Your/Folder/Path")

# 2. LOAD DATA
# Read the translated CSV file into a data frame.
# This assumes 'final_adjusted_points_translated.csv' is in your working directory.
#translated_data <- read.csv("final_adjusted_points_translated.csv")
translated_data <- readRDS("final_adjusted_points_translated.rds")

# 3. CREATE NUMERICAL COLUMNS
# Use the 'mutate' function from dplyr to add the new columns.

final_data <- translated_data %>%
  mutate(
    # Create 'sex_number' column: 1 for Male, 2 for Female
    sex_number = case_when(
      sex == "Male"   ~ 1,
      sex == "Female" ~ 2,
      TRUE ~ NA_integer_ # Assign NA if neither Male nor Female
    ),
    
    # Create 'age_number' with sequential numbers for each category
    age_number = case_when(
      age == "up to 29 years"       ~ 1,
      age == "30 to 44 years old"   ~ 2,
      age == "45 to 59 years old"   ~ 3,
      age == "45 to 60 years old"   ~ 4,
      age == "more than 60 years old"  ~ 5,
      TRUE ~ NA_integer_ # Assign NA for any other value
    ),
    
    # Create the 'combined_code' column based on the formula
    combined_code = (sex_number * 10) + age_number
  )


# 4. VERIFY THE NEW COLUMNS
# Print the first few rows of the data frame to see the new columns.
print("Head of the final data frame with new coded columns:")
head(final_data)

# You can also check the unique values to ensure everything was coded correctly.
print("Unique values in 'sex_number':")
unique(final_data$sex_number)

print("Unique values in 'age_number':")
unique(final_data$age_number)


# 5. SAVE THE CODED DATA
# Write the final data frame with the new columns to a new CSV file.
# Using row.names = FALSE prevents R from writing an extra column for row numbers.
View(final_data)
#write.csv(final_data, "final_adjusted_points_coded.csv", row.names = FALSE)
saveRDS(final_data, "final_adjusted_points_coded.rds")

print("Processing complete. The new file 'final_adjusted_points_coded.csv' has been saved.")

# R Script to Calculate Post-Stratification Weights
#
# This script reads the coded data and calculates weights for each respondent
# to align the sample's age and gender distribution with a target
# global population distribution. This helps to correct for over- or
# under-sampling of specific demographic groups.

# 1. SETUP
# Install and load necessary libraries. 'dplyr' is essential for this task.
# if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# --- IMPORTANT ---
# Set your working directory to the folder where your CSV file is located.
# You MUST change the path below to match the location on your computer.
# setwd("C:/Your/Folder/Path")

# 2. LOAD CODED DATA
# This assumes 'final_adjusted_points_coded.csv' is in your working directory.
#final_data <- read.csv("final_adjusted_points_coded.csv")
final_data <- readRDS("final_adjusted_points_coded.rds")

# 3. DEFINE TARGET POPULATION DISTRIBUTION
# Create a data frame containing the global population percentages you provided.
# Proportions are converted from percentages to decimals (e.g., 25.35% -> 0.2535).
target_population <- data.frame(
  age_group = c("0–29", "0–29", "30–44", "30–44", "45–60", "45–60", "60+", "60+"),
  sex = c("Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male"),
  target_prop = c(0.2535, 0.2603, 0.1046, 0.1018, 0.0733, 0.0715, 0.0730, 0.0620)
)
View(target_population)

# 4. STANDARDIZE AGE GROUPS IN YOUR DATA
# Create a new column 'target_age_group' in your data to match the
# categories in the 'target_population' data frame. This is crucial for joining.
final_data <- final_data %>%
  mutate(
    target_age_group = case_when(
      age == "up to 29 years"       ~ "0–29",
      age == "30 to 44 years old"   ~ "30–44",
      # Both of these original groups map to the same target group
      age == "45 to 59 years old"   ~ "45–60",
      age == "45 to 60 years old"   ~ "45–60",
      age == "more than 60 years old"  ~ "60+",
      TRUE ~ NA_character_
    )
  )

# 5. CALCULATE SAMPLE PROPORTIONS
# Calculate the proportion of your sample in each demographic group.
sample_proportions <- final_data %>%
  # Filter out any rows that didn't match an age group
  filter(!is.na(target_age_group)) %>%
  # Group by the standardized age and sex
  group_by(target_age_group, sex) %>%
  # Count the number of people in each group
  summarise(n = n(), .groups = 'drop') %>%
  # Calculate the proportion for each group relative to the total sample size
  mutate(sample_prop = n / sum(n))

View(sample_proportions)
# 6. CALCULATE WEIGHTS
# Join the sample proportions with the target proportions and calculate the weight.
weights_table <- left_join(sample_proportions, target_population, by = c("target_age_group" = "age_group", "sex")) %>%
  mutate(
    # The weighting formula
    weight = target_prop / sample_prop
  )
View(weights_table)
# 7. APPLY WEIGHTS TO THE MAIN DATASET
# Join the calculated weights back to the main data frame.
final_weighted_data <- left_join(final_data, st_drop_geometry(weights_table), by = c("target_age_group", "sex"))
View(final_weighted_data)


# 8. VERIFY THE RESULTS
# Display the calculated weights for each group.
print("Calculated Weights for Each Demographic Group:")
print(select(weights_table, target_age_group, sex, sample_prop, target_prop, weight))


# Display the first few rows of the final data frame with the new 'weight' column.
print("Head of the final data frame with the new 'weight' column:")
head(select(final_weighted_data, age, sex, target_age_group, weight.x))


# 9. SAVE THE WEIGHTED DATA
# Write the final data frame with the new 'weight' column to a new CSV file.
write.csv(final_weighted_data, "final_adjusted_points_weighted.csv", row.names = FALSE)
saveRDS(final_weighted_data, "final_adjusted_points.rds")

weights_no_geoometry<-as.data.frame(weights_table)
weights_no_geoometry<-weights_no_geoometry[,-4]
write.csv(weights_no_geoometry, "weights_table.csv", row.names = FALSE)

print("Weighting complete. The new file 'final_adjusted_points_weighted.csv' has been saved.")

