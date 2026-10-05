## =============================================================
## Week 2 Task: Data Visualization and Insight Communication using R
## Dataset: Titanic Passenger Data (continued from Week 1)
##          Source: https://raw.githubusercontent.com/
##          datasciencedojo/datasets/master/titanic.csv
## =============================================================

library(dplyr)
library(ggplot2)
library(scales)

titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE)

## ---- Minimal cleaning carried over from Week 1 ----
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)
titanic$Embarked[titanic$Embarked == ""] <- names(sort(table(titanic$Embarked), decreasing = TRUE))[1]
titanic$Cabin_Known <- ifelse(titanic$Cabin == "" | is.na(titanic$Cabin), 0, 1)

titanic$Pclass   <- factor(titanic$Pclass, levels = c(1,2,3), labels = c("1st","2nd","3rd"))
titanic$Survived_Label <- factor(titanic$Survived, levels = c(0,1), labels = c("Did Not Survive","Survived"))

theme_report <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "grey40"),
        legend.position = "bottom")

## =============================================================
## VISUAL 1 -- Bar Chart: Passenger Count by Class and Sex
## =============================================================
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("female" = "#D9534F", "male" = "#2E5395")) +
  labs(title = "Passenger Count by Class and Sex",
       subtitle = "Who was travelling, and in which class?",
       x = "Passenger Class", y = "Number of Passengers", fill = "Sex") +
  theme_report

## =============================================================
## VISUAL 2 -- Histogram: Age Distribution
## =============================================================
ggplot(titanic, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#2E5395", color = "white") +
  geom_vline(aes(xintercept = median(Age)), color = "#D9534F", linetype = "dashed", linewidth = 1) +
  labs(title = "Age Distribution of Passengers",
       subtitle = "Dashed line marks the median age",
       x = "Age (years)", y = "Number of Passengers") +
  theme_report

## =============================================================
## VISUAL 3 -- Boxplot: Fare by Passenger Class
## =============================================================
ggplot(titanic, aes(x = Pclass, y = Fare, fill = Pclass)) +
  geom_boxplot(outlier.color = "#D9534F", outlier.alpha = 0.6) +
  scale_fill_manual(values = c("1st" = "#1F3864", "2nd" = "#2E5395", "3rd" = "#9FB8DE")) +
  labs(title = "Fare Distribution by Passenger Class",
       subtitle = "Dots above the whiskers are statistical outliers",
       x = "Passenger Class", y = "Fare ($)") +
  theme_report + theme(legend.position = "none")

## =============================================================
## VISUAL 4 -- Scatter Plot: Age vs Fare, coloured by Survival
## =============================================================
ggplot(titanic, aes(x = Age, y = Fare, color = Survived_Label)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("Did Not Survive" = "#D9534F", "Survived" = "#2E5395")) +
  labs(title = "Age vs. Fare, Coloured by Survival Outcome",
       subtitle = "Is there a relationship between what passengers paid, their age, and survival?",
       x = "Age (years)", y = "Fare ($)", color = "Outcome") +
  theme_report

## =============================================================
## VISUAL 5 -- Line Chart: Survival Rate Trend Across Age Groups
## =============================================================
age_breaks <- c(0, 10, 20, 30, 40, 50, 60, 80)
age_labels <- c("0-10","11-20","21-30","31-40","41-50","51-60","61+")
titanic$AgeGroup <- cut(titanic$Age, breaks = age_breaks, labels = age_labels, include.lowest = TRUE)

survival_by_age <- titanic %>%
  group_by(AgeGroup) %>%
  summarise(SurvivalRate = mean(Survived), Count = n())

ggplot(survival_by_age, aes(x = AgeGroup, y = SurvivalRate, group = 1)) +
  geom_line(color = "#2E5395", linewidth = 1.2) +
  geom_point(color = "#1F3864", size = 3) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Survival Rate Trend Across Age Groups",
       subtitle = "Does survival likelihood change with age?",
       x = "Age Group", y = "Survival Rate") +
  theme_report

## =============================================================
## VISUAL 6 -- Stacked Bar: Survival Proportion by Class
## =============================================================
ggplot(titanic, aes(x = Pclass, fill = Survived_Label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Did Not Survive" = "#D9534F", "Survived" = "#2E5395")) +
  labs(title = "Survival Rate by Passenger Class",
       subtitle = "Proportion of each class that survived",
       x = "Passenger Class", y = "Proportion of Passengers", fill = "Outcome") +
  theme_report

## =============================================================
## VISUAL 7 -- Overlaid Density: Age Distribution by Survival
## =============================================================
ggplot(titanic, aes(x = Age, fill = Survived_Label)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Did Not Survive" = "#D9534F", "Survived" = "#2E5395")) +
  labs(title = "Age Distribution by Survival Outcome",
       subtitle = "Comparing the age profile of survivors vs. non-survivors",
       x = "Age (years)", y = "Density", fill = "Outcome") +
  theme_report

## =============================================================
## End of script
## =============================================================
