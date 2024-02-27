# Gradient boosting analysis given in https://bradleyboehmke.github.io/HOML/gbm.html

# Helper packages
library(dplyr)     # for data manipulation
library(ggplot2)   # for awesome graphics

# Modeling process packages
library(rsample)   # for resampling procedures
library(caret)     # for resampling and model training
library(AmesHousing)

# Modeling packages
library(gbm)      # for original implementation of regular and stochastic GBMs


# We’ll continue working with the ames_train data set created in Section 2.7 to illustrate the main concepts. We’ll also demonstrate h2o functionality using the same setup from Section 11.5.

# # h2o set-up 
# h2o.no_progress()  # turn off h2o progress bars
# h2o.init()         # launch h2o
# 
# h2o.init(max_mem_size = "10g")

# Ames housing data
ames <- AmesHousing::make_ames()
# ames.h2o <- as.h2o(ames)

# Stratified sampling with the rsample package
set.seed(123)
split <- initial_split(ames, prop = 0.7, 
                       strata = "Sale_Price")
# Training and testing data:
ames_train  <- training(split)
ames_test   <- testing(split)


# train_h2o <- as.h2o(ames_train)
response <- "Sale_Price"
predictors <- setdiff(colnames(ames_train), response)

###################################################
### GBM - run a basic model using the gbm R package
###################################################

# run a basic GBM model
set.seed(123)  # for reproducibility
ames_gbm1 <- gbm(
  formula = Sale_Price ~ .,
  data = ames_train,
  distribution = "gaussian",  # SSE loss function
  n.trees = 3000,
  shrinkage = 0.1, # learning rate
  interaction.depth = 3,
  n.minobsinnode = 10, # minimal number of observations in terminal nodes
  cv.folds = 10,
  bag.fraction=1
)

# find index for number trees with minimum CV error
(best <- which.min(ames_gbm1$cv.error))

# get MSE and compute RMSE
sqrt(ames_gbm1$cv.error[best])
## [1] 22402.07

# Our results show a cross-validated SSE of 23240 which was achieved with 1119 trees.

# plot error curve with training black and cross-validated MSE (green)
gbm.perf(ames_gbm1, method = "cv")

# Here the only parameter that is tuned is the number of trees. However, we would need to tune essentially all combinations of hyperparameters


###################################################
### Stochastic GBM: Idea to train the algorithm on a random subsample of the training data set in each step of the boosting process. 
###################################################
# Idea: additional reduction in tree correlation
 
# run a basic GBM model
set.seed(123)  # for reproducibility
ames_gbm2 <- gbm(
  formula = Sale_Price ~ .,
  data = ames_train,
  distribution = "gaussian",   
  n.trees = 3000,
  shrinkage = 0.1,  
  interaction.depth = 3,
  n.minobsinnode = 10, 
  cv.folds = 10,
  bag.fraction=0.5 # Note: this is the default!
)

# find index for number trees with minimum CV error
best2 <- which.min(ames_gbm2$cv.error)

# get MSE and compute RMSE
sqrt(ames_gbm2$cv.error[best2])
## [1] 22402.07

# Our results show a cross-validated SSE of 23240 which was achieved with 1119 trees.

# plot error curve with training black and cross-validated MSE (green)
gbm.perf(ames_gbm2, method = "cv")

##############################################
### XGBoost
##############################################
# Additional data preparation to obtain matrices that xgboost requires:

library(recipes)
library(xgboost)

# Training data and response
xgb_prep <- recipe(Sale_Price ~ ., data = ames_train) %>%
  step_integer(all_nominal()) %>%
  prep(training = ames_train, retain = TRUE) %>%
  juice()

X <- as.matrix(xgb_prep[setdiff(names(xgb_prep), "Sale_Price")])
Y <- xgb_prep$Sale_Price

# Test data and test response
xgb_prep_test <- recipe(Sale_Price ~ ., data = ames_test) %>%
  step_integer(all_nominal()) %>%
  prep(training = ames_test, retain = TRUE) %>%
  juice()
X_test <- as.matrix(xgb_prep_test[setdiff(names(xgb_prep_test), "Sale_Price")])
Y_test <- xgb_prep_test$Sale_Price


# Run on training data with a set of hyperparameters that were pre-optimized
set.seed(123)
ames_xgb <- xgboost(
  data = X,
  label = Y,
  nrounds = 6000,
  objective = "reg:squarederror",
  early_stopping_rounds = 50, 
  params = list(
    eta = 0.1,
    max_depth = 3,
    min_child_weight = 3,
    subsample = 0.8,
    colsample_bytree = 0.5,
    nthread=12),
  verbose = 0
)  

X.pred.test <- predict(ames_xgb,newdata = X_test)
sqrt(sum((X.pred.test - Y_test)^2)/nrow(X_test))
