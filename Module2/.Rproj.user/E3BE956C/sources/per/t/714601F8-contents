# load libraries ----
library(caret)
library(dplyr)
library(party)
library(randomForest)
library(xgboost)
library(glue)
library(tictoc)
# read data ----
data <- read.table("wine.data", sep=",")
head(data)

# rename target variable
names(data)[names(data) == 'V1'] <- 'target'
head(data)

# convert to factor
data$target <- as.factor(data$target)

# quick EDA ----
str(data)

# hypothesis ----
# The 3 are all grown from the same region, so I believe they will have subtle differences
# that the alcohol level alone is probably not enough to descriptor to determine which
# wine we are looking at. 

# I believe the Random Forest model will be best.
# Decision tree will be too simple, and XGBoost is too complex for this simple dataset

# I believe the max leaf nodes for Random Forest will have to be set to a low number to 
# keep the model from over-fitting.

# split data ----
# split into train-test using createDataPartition by the target
set.seed(21)
train.index <- createDataPartition(data$target, p = 0.75, list = FALSE)
train <- data[train.index, ]
test <- data[-train.index, ]

# check the distribution for train, test, and original dataset
data %>% group_by(target) %>% summarise(counts = length(target) / nrow(data)*100)
train %>% group_by(target) %>% summarise(counts = length(target) / nrow(train)*100)
test %>% group_by(target) %>% summarise(counts = length(target) / nrow(test)*100)

# we can see that the split did a good job, ~33% are class 1, ~40% are class 2 and ~27% are class 3
# on all 3 datasets

model_names <- c('Decision Tree', 'Random Forest', 'XGBoost')

# DECISION TREE ----
set.seed(21)
tree <- ctree(data = train, target ~ .)
plot(tree)

# predict
preds <- predict(tree, train)

conf_mat <- table(train$target, preds)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
accuracy

# the decision tree is 92% accurate on the training set.

# RANDOM FOREST ----
set.seed(21)
rf <- randomForest(data = train, target ~ .)

# predict
rf_preds <- predict(rf, train)

rf_conf_mat <- table(train$target, rf_preds)
rf_accuracy <- sum(diag(rf_conf_mat))/sum(rf_conf_mat)
rf_accuracy

# random forest model has 100% accuracy on the training set. This implies over-fitting.

# XGBOOST ----
# convert features to list and target to list
set.seed(21)
train_xgb_prep <- train
train_xgb_prep$target <- as.integer(train_xgb_prep$target) - 1
xgb_train <- xgb.DMatrix(data = as.matrix(subset(train_xgb_prep, select = -target)), label = train_xgb_prep$target)

xg <- xgboost(data = xgb_train, max.depth = 2, eta = 1, nthread = 2, nrounds = 5, objective = "multi:softprob", num_class = 3)

xg_preds <- predict(xg, xgb_train)

# extract prediction from highest probability
prediction <- matrix(xg_preds, nrow = 3, ncol = length(xg_preds)/3) %>%
  t() %>%
  data.frame() %>%
  mutate(label = train_xgb_prep$target, max_prob = max.col(., "last")-1)


xg_conf_mat <- table(train_xgb_prep$target, prediction$label)
xg_accuracy <- sum(diag(rf_conf_mat))/sum(rf_conf_mat)
xg_accuracy

# evaluate all models against test data ----
model_comparer <- function(models, test_data, model_names){
  print("Accuracy scores for fitted models using test set ")
  print("************************************************")
  for (ind in seq_along(model_names)){
    if (ind == 3){
      model_name <- model_names[[ind]]
      test_data$target <- as.integer(test_data$target) - 1
      xgb_test <- xgb.DMatrix(data = as.matrix(subset(test_data, select = -target)), label = test_data$target)
      xg_preds = predict(models[[ind]], xgb_test)
      prediction <- matrix(xg_preds, nrow = 3, ncol = length(xg_preds)/3) %>%
        t() %>%
        data.frame() %>%
        mutate(label = test_data$target, max_prob = max.col(., "last")-1)
      xg_conf_mat <- table(test_data$target, prediction$label)
      accuracy <- sum(diag(rf_conf_mat))/sum(rf_conf_mat)
    } else {
      model_name <- model_names[[ind]]
      preds = predict(models[[ind]], test_data)
      conf_mat <- table(test_data$target, preds)
      accuracy <- sum(diag(conf_mat))/sum(conf_mat)
    }
    print(glue("Accuracy for {model_name}: {accuracy}"))
  }
  print("************************************************")
}


models <- list(tree, rf, xg)
model_comparer(models, test, model_names)
