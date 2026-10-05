## =============================================================
## Week 3 Task: Statistical Analysis and Predictive Modeling using R
## Dataset: Titanic Passenger Data (continued from Weeks 1-2)
##          Source: https://raw.githubusercontent.com/
##          datasciencedojo/datasets/master/titanic.csv
## =============================================================

library(dplyr)
library(caret)     # train/test split, cross-validation, confusion matrix
library(pROC)      # ROC curve and AUC
library(car)       # VIF for multicollinearity diagnostics

set.seed(42)

titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE)

## ---- Cleaning (carried over from Week 1) ----
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)
titanic$Embarked[titanic$Embarked == ""] <- names(sort(table(titanic$Embarked), decreasing = TRUE))[1]
titanic$Cabin_Known <- ifelse(titanic$Cabin == "" | is.na(titanic$Cabin), 0, 1)

titanic$Survived <- factor(titanic$Survived, levels = c(0,1), labels = c("No","Yes"))
titanic$Pclass   <- factor(titanic$Pclass, levels = c(1,2,3), labels = c("1st","2nd","3rd"))
titanic$Sex      <- factor(titanic$Sex)
titanic$Embarked <- factor(titanic$Embarked)
titanic$FamilySize <- titanic$SibSp + titanic$Parch + 1

## =============================================================
## PART A -- EXPLORATORY STATISTICAL ANALYSIS
## =============================================================

## ---- A1. Normality Testing (Shapiro-Wilk) ----
shapiro.test(titanic$Age)
shapiro.test(titanic$Fare)
# Both expected to reject H0 of normality (p < 0.05), given the
# imputation spike in Age and the heavy right-skew in Fare.

qqnorm(titanic$Age, main = "Q-Q Plot: Age"); qqline(titanic$Age, col = "#D9534F")
qqnorm(titanic$Fare, main = "Q-Q Plot: Fare"); qqline(titanic$Fare, col = "#D9534F")

## ---- A2. Hypothesis Test 1: Age by Survival (Welch Two-Sample t-test) ----
# H0: mean Age is equal for survivors and non-survivors
# H1: mean Age differs between the two groups
t.test(Age ~ Survived, data = titanic)

## ---- A3. Hypothesis Test 2: Sex and Survival (Chi-Square Test of Independence) ----
# H0: Sex and Survived are independent
# H1: Sex and Survived are associated
chisq.test(table(titanic$Sex, titanic$Survived))

## ---- A4. Hypothesis Test 3: Passenger Class and Survival (Chi-Square) ----
# H0: Pclass and Survived are independent
# H1: Pclass and Survived are associated
chisq.test(table(titanic$Pclass, titanic$Survived))

## ---- A5. Correlation Test: Age vs. Fare (Pearson) ----
cor.test(titanic$Age, titanic$Fare, method = "pearson")

## =============================================================
## PART B -- PREDICTIVE MODEL: LOGISTIC REGRESSION (CLASSIFICATION)
## =============================================================

## ---- B1. Train/Test Split (75/25) ----
train_index <- createDataPartition(titanic$Survived, p = 0.75, list = FALSE)
train_data  <- titanic[train_index, ]
test_data   <- titanic[-train_index, ]

## ---- B2. Multicollinearity Check (VIF) before fitting ----
vif_check <- glm(Survived ~ Pclass + Sex + Age + Fare + SibSp + Parch + Embarked,
                  data = train_data, family = binomial)
vif(vif_check)   # all VIF < 5 indicates no serious multicollinearity

## ---- B3. Fit Logistic Regression Model ----
model <- glm(Survived ~ Pclass + Sex + Age + Fare + FamilySize + Embarked,
             data = train_data, family = binomial)
summary(model)
exp(coef(model))              # odds ratios
exp(confint(model))           # 95% CI for odds ratios

## ---- B4. 10-Fold Cross-Validation ----
cv_control <- trainControl(method = "cv", number = 10)
cv_model <- train(Survived ~ Pclass + Sex + Age + Fare + FamilySize + Embarked,
                   data = train_data, method = "glm", family = "binomial",
                   trControl = cv_control)
print(cv_model)
cv_model$resample          # per-fold accuracy

## ---- B5. Predict on Held-Out Test Set ----
test_probs <- predict(model, newdata = test_data, type = "response")
test_pred  <- factor(ifelse(test_probs > 0.5, "Yes", "No"), levels = c("No","Yes"))

## ---- B6. Confusion Matrix and Performance Metrics ----
conf_matrix <- confusionMatrix(test_pred, test_data$Survived, positive = "Yes")
print(conf_matrix)
# Accuracy, Precision (Pos Pred Value), Recall (Sensitivity), F1 all
# reported directly in the confusionMatrix() output above.

## ---- B7. ROC Curve and AUC ----
roc_obj <- roc(test_data$Survived, test_probs, levels = c("No","Yes"), direction = "<")
plot(roc_obj, main = "ROC Curve - Survival Prediction Model", col = "#2E5395", lwd = 2)
auc(roc_obj)

## =============================================================
## PART C -- MODEL DIAGNOSTICS
## =============================================================

## ---- C1. Deviance Residuals vs. Fitted ----
plot(model$fitted.values, residuals(model, type = "deviance"),
     xlab = "Fitted Probabilities", ylab = "Deviance Residuals",
     main = "Deviance Residuals vs. Fitted Values")
abline(h = 0, col = "#D9534F", lty = 2)

## ---- C2. Binned Residual Plot (via arm-style manual binning) ----
fitted_vals <- model$fitted.values
resid_vals  <- residuals(model, type = "response")
bins <- cut(fitted_vals, breaks = quantile(fitted_vals, probs = seq(0,1,0.1)), include.lowest = TRUE)
binned <- aggregate(resid_vals, by = list(bin = bins), FUN = mean)
plot(binned$x, main = "Binned Residual Plot", ylab = "Average Residual", xlab = "Bin")
abline(h = 0, col = "#D9534F", lty = 2)

## ---- C3. Model Comparison: Null vs. Fitted (Likelihood Ratio Test) ----
anova(model, test = "Chisq")

## McFadden's Pseudo R-squared
null_model <- glm(Survived ~ 1, data = train_data, family = binomial)
pseudo_r2 <- 1 - (logLik(model) / logLik(null_model))
print(pseudo_r2)

## =============================================================
## End of script
## =============================================================
