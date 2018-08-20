library(rvw)

curr_dir <- getwd()
setwd(tempdir())

# Get VW format datafiles
ext_train_data <- system.file("extdata", "binary_train.vw", package = "rvw")
ext_test_data <- system.file("extdata", "binary_valid.vw", package = "rvw")
multiclass_train_data <- system.file("extdata", "multiclass_train.vw", package = "rvw")
multiclass_test_data <- system.file("extdata", "multiclass_valid.vw", package = "rvw")

# Setup model
test_vwmodel <-  vwsetup(dir = "./", model = "mdl.vw",
                         feature_params = list(hash="all", bit_precision=25),
                         optimization_params = list(adaptive=FALSE, learning_rate=0.1))
# Basic training and testing
vwtrain(test_vwmodel, data = ext_train_data)
vw_output <- vwtest(test_vwmodel, data = ext_test_data, probs_path = "./probs.vw")

# Printing readable model
test_vwmodel <- vwsetup()
vwtrain(test_vwmodel, data = ext_train_data, readable_model = "hashed")
vwtest(test_vwmodel, data = ext_test_data, readable_model = "inverted")
# Model audit
vwaudit(test_vwmodel)
# No console output
vwtrain(test_vwmodel, data = ext_train_data, quiet = T)
vwtest(test_vwmodel, data = ext_train_data, quiet = T)

# Add learning options
library(magrittr)
test_vwmodel <-  vwsetup(dir = "./", model = "mdl.vw",
                         option = "ect", num_classes=3) %>%
    add_option(option = "boosting", num_learners=10)
# Print vwmodel contents
test_vwmodel
# Access vw parameters
vwparams(test_vwmodel, "num_classes")
# Modify parameters
vwparams(test_vwmodel, "num_learners") <- 100

vwtrain(test_vwmodel, data = multiclass_train_data)
vwtest(test_vwmodel, data = multiclass_test_data)

setwd(curr_dir)
