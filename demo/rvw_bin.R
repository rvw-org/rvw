library(rvw)
library(mlbench) # For a dataset

# First, switch to a temporary directory
curr_dir <- getwd()
setwd(tempdir())

set.seed(42)

# We will try to identify benign or malignant class of a tumour using its histology characteristics.
data("BreastCancer", package = "mlbench")
data_full <- BreastCancer

# First, start with data preprocessing
data_full <- data_full[complete.cases(data_full),]
ind_train <- sample(1:nrow(data_full), 0.8*nrow(data_full))

str(data_full)
summary(data_full)
# We can see that "benign" cases appear more often in our dataset
# This will be used to set up a baseline model

data_full <- data_full[,-1]
data_full$Class <- ifelse(data_full$Class == "malignant", 1, -1)

data_train <- data_full[ind_train,]
data_test <- data_full[-ind_train,]

# Our baseline model simply reports every tumour class as benign
baseline_pred <- rep(-1, length(data_test$Class))

# Accuracy for binary classification case
acc_prc <- function(y_pred, y_true){sum(y_pred == y_true) / length(y_pred) * 100}

acc_prc(data_test$Class, baseline_pred)
# With our baseline model, we get an accuracy of around 65%

# Now we a ready to use Vowpal Wabbit models
# Setup model
test_vwmodel <-  vwsetup(dir = "./", model = "mdl.vw",
                         option = "binary") # Convert predictions to {-1,+1}

# Basic training and testing
vwtrain(vwmodel = test_vwmodel,
        data = data_train,
        passes = 10,
        targets = "Class")

vw_output <- vwtest(vwmodel = test_vwmodel, data = data_test)

acc_prc(data_test$Class, vw_output)
# Now we get much better results with an accuracy of around 97%

# Switch back
setwd(curr_dir)
