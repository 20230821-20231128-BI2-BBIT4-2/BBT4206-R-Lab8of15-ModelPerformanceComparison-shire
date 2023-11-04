.libPaths()

lapply(.libPaths(), list.files)

if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# Introduction ----
# The performance of the trained models can be compared visually. This is done
# to help you to identify and choose the top performing models.

# STEP 1. Install and Load the Required Packages ----
## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## kernlab ----
if (require("kernlab")) {
  require("kernlab")
} else {
  install.packages("kernlab", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## randomForest ----
if (require("randomForest")) {
  require("randomForest")
} else {
  install.packages("randomForest", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
library(readr)
HeartDiseaseTrain_Test <- read_csv("data/HeartDiseaseTrain-Test.csv")
View(HeartDiseaseTrain_Test)

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

### LDA ----
set.seed(7)
sex_model_lda <- train(sex ~ ., data = HeartDiseaseTrain_Test,
                            method = "lda", trControl = train_control)
### CART ----
set.seed(7)
sex_model_cart <- train(sex ~ ., data = HeartDiseaseTrain_Test,
                             method = "rpart", trControl = train_control)

### KNN ----
set.seed(7)
sex_model_knn <- train(sex ~ ., data = HeartDiseaseTrain_Test,
                            method = "knn", trControl = train_control)

### SVM ----
set.seed(7)
sex_model_svm <- train(sex ~ ., data = HeartDiseaseTrain_Test,
                            method = "svmRadial", trControl = train_control)

### Random Forest ----
set.seed(7)
sex_model_rf <- train(sex ~ ., data = HeartDiseaseTrain_Test,
                           method = "rf", trControl = train_control)

results <- resamples(list(LDA = sex_model_lda, CART = sex_model_cart,
                          KNN = sex_model_knn, SVM = sex_model_svm,
                          RF = sex_model_rf))

# STEP 4. Display the Results ----


summary(results)

## 2. Box and Whisker Plot ----


scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results, scales = scales)


## 3. Dot Plots ----


scales <- list(x = list(relation = "free"), y = list(relation = "free"))
dotplot(results, scales = scales)

## 4. Scatter Plot Matrix ----


splom(results)

## 5. Pairwise xyPlots ----

xyplot(results, models = c("LDA", "SVM"))


xyplot(results, models = c("SVM", "CART"))

## 6. Statistical Significance Tests ----

diffs <- diff(results)

summary(diffs)

