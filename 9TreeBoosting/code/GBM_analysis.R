# Gradient boosting analysis given in https://bradleyboehmke.github.io/HOML/gbm.html

# Helper packages
library(dplyr)     # for data manipulation
library(ggplot2)   # for awesome graphics

# Modeling process packages
library(rsample)   # for resampling procedures
library(caret)     # for resampling and model training
library(h2o)       # for resampling and model training
library(AmesHousing)



# Modeling packages
library(gbm)      # for original implementation of regular and stochastic GBMs
library(h2o)      # for a java-based implementation of GBM variants
library(xgboost)  # for fitting extreme gradient boosting


# We’ll continue working with the ames_train data set created in Section 2.7 to illustrate the main concepts. We’ll also demonstrate h2o functionality using the same setup from Section 11.5.

# h2o set-up 
h2o.no_progress()  # turn off h2o progress bars
h2o.init()         # launch h2o

h2o.init(max_mem_size = "10g")

# Ames housing data
ames <- AmesHousing::make_ames()
ames.h2o <- as.h2o(ames)

# Stratified sampling with the rsample package
set.seed(123)
split <- initial_split(ames, prop = 0.7, 
                       strata = "Sale_Price")
# Training and testing data:
ames_train  <- training(split)
ames_test   <- testing(split)


train_h2o <- as.h2o(ames_train)
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
  n.trees = 5000,
  shrinkage = 0.1, # learning rate
  interaction.depth = 3,
  n.minobsinnode = 10, # minimual number of observations in terminal nodes
  cv.folds = 10
)

# find index for number trees with minimum CV error
best <- which.min(ames_gbm1$cv.error)

# get MSE and compute RMSE
sqrt(ames_gbm1$cv.error[best])
## [1] 22402.07

# Our results show a cross-validated SSE of 23240 which was achieved with 1119 trees.

# plot error curve
gbm.perf(ames_gbm1, method = "cv")

# Here the only parameter that is tuned is the number of trees. However, we would need to tune essentially all combinations of hyperparameters


###################################################
### Stochastic GBM: Idea to train the algorithm on a random subsample of the training data set in each step of the boosting process. 
###################################################
# Idea: additional reduction in tree correlation
 


