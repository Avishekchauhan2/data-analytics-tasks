## =============================================================
## Week 1 Task: Data Cleaning and Preliminary Analysis with R
## Dataset: Titanic Passenger Data (Kaggle / Data Science Dojo,
##          publicly available: https://raw.githubusercontent.com/
##          datasciencedojo/datasets/master/titanic.csv)
## =============================================================

## ---- 0. Setup ----
# install.packages(c("dplyr","ggplot2","corrplot"))  # run once if needed
library(dplyr)
library(ggplot2)
library(corrplot)

titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE)

## ---- 1. Initial Inspection ----
str(titanic)
summary(titanic)
dim(titanic)
head(titanic, 5)

## ---- 2. Missing Value Assessment ----
colSums(is.na(titanic))                              # numeric NAs
sapply(titanic, function(x) sum(x == "" | is.na(x)))  # blanks + NAs (covers Cabin/Embarked)

missing_pct <- round(100 * sapply(titanic, function(x) sum(x == "" | is.na(x))) / nrow(titanic), 1)
missing_pct[missing_pct > 0]

## ---- 3. Data Cleaning: Handling Missing Values ----

# 3a. Age (numeric, ~20% missing) -> impute with MEDIAN
#     Median chosen over mean because Age is right-skewed and
#     median is robust to the outliers detected in step 4.
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)

# 3b. Embarked (categorical, few missing) -> impute with MODE
mode_embarked <- names(sort(table(titanic$Embarked[titanic$Embarked != ""]), decreasing = TRUE))[1]
titanic$Embarked[titanic$Embarked == ""] <- mode_embarked

# 3c. Cabin (~77% missing) -> too sparse to impute meaningfully.
#     Converted into a binary indicator instead of being dropped outright,
#     since "cabin recorded" is itself informative (correlates with class/fare).
titanic$Cabin_Known <- ifelse(titanic$Cabin == "" | is.na(titanic$Cabin), 0, 1)
titanic$Cabin <- NULL

## ---- 4. Outlier Detection (IQR method on Fare) ----
Q1 <- quantile(titanic$Fare, 0.25)
Q3 <- quantile(titanic$Fare, 0.75)
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

outliers <- titanic$Fare[titanic$Fare < lower_bound | titanic$Fare > upper_bound]
length(outliers)                 # count of flagged outliers
range(outliers)

# Flag rather than delete -- high fares are legitimate (1st class suites),
# not data-entry errors, so they are retained but marked for transparency.
titanic$Fare_Outlier <- ifelse(titanic$Fare < lower_bound | titanic$Fare > upper_bound, 1, 0)

boxplot(titanic$Fare, main = "Fare Distribution with Outliers", ylab = "Fare ($)")

## ---- 5. Encoding Categorical Variables ----

# 5a. Label encode binary Sex
titanic$Sex_Encoded <- ifelse(titanic$Sex == "male", 1, 0)

# 5b. One-hot encode Embarked (3 levels: C, Q, S)
titanic$Embarked_C <- ifelse(titanic$Embarked == "C", 1, 0)
titanic$Embarked_Q <- ifelse(titanic$Embarked == "Q", 1, 0)
titanic$Embarked_S <- ifelse(titanic$Embarked == "S", 1, 0)

# 5c. Convert Pclass and Survived to factors (ordinal / categorical, not continuous)
titanic$Pclass   <- factor(titanic$Pclass, levels = c(1, 2, 3), labels = c("1st", "2nd", "3rd"))
titanic$Survived <- factor(titanic$Survived, levels = c(0, 1), labels = c("No", "Yes"))

## ---- 6. Normalization (Min-Max scaling) ----
normalize <- function(x) (x - min(x)) / (max(x) - min(x))
titanic$Age_Scaled  <- normalize(titanic$Age)
titanic$Fare_Scaled <- normalize(titanic$Fare)

## ---- 7. Exploratory Data Analysis ----

str(titanic)
summary(titanic)

# Descriptive statistics for key numeric variables
summary(titanic$Age)
summary(titanic$Fare)
sd(titanic$Age)
sd(titanic$Fare)

# Survival rate overall and by group
prop.table(table(titanic$Survived))
table(titanic$Pclass, titanic$Survived)
prop.table(table(titanic$Pclass, titanic$Survived), margin = 1)
table(titanic$Sex, titanic$Survived)
prop.table(table(titanic$Sex, titanic$Survived), margin = 1)

# Correlation matrix (numeric variables only)
numeric_vars <- titanic %>% select(Age, Fare, SibSp, Parch, Cabin_Known)
cor_matrix <- cor(numeric_vars, use = "complete.obs")
round(cor_matrix, 2)
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black")

## ---- 8. Visualizations ----

# Age distribution
ggplot(titanic, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#2E5395", color = "white") +
  labs(title = "Age Distribution of Passengers (post-imputation)",
       x = "Age", y = "Count") +
  theme_minimal()

# Survival count by sex
ggplot(titanic, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival Count by Sex", x = "Sex", y = "Passenger Count") +
  theme_minimal()

# Fare by passenger class
ggplot(titanic, aes(x = Pclass, y = Fare, fill = Pclass)) +
  geom_boxplot() +
  labs(title = "Fare Distribution by Passenger Class", x = "Class", y = "Fare ($)") +
  theme_minimal()

# Survival rate by class
ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill") +
  labs(title = "Survival Rate by Passenger Class", x = "Class", y = "Proportion") +
  theme_minimal()

## ---- 9. Export cleaned dataset ----
write.csv(titanic, "titanic_cleaned.csv", row.names = FALSE)

## =============================================================
## End of script
## =============================================================
