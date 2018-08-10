library(rvwgsoc)

curr_dir <- getwd()
setwd(tempdir())

# Right now using only files in .vw format, later will also accept R formats
ext_train_data <- system.file("extdata", "binary_train.vw", package = "rvwgsoc")
ext_test_data <- system.file("extdata", "binary_valid.vw", package = "rvwgsoc")
multiclass_train_data <- system.file("extdata", "multiclass_train.vw", package = "rvwgsoc")
multiclass_test_data <- system.file("extdata", "multiclass_valid.vw", package = "rvwgsoc")

test_vwmodel <-  vwsetup(dir = "./", model = "mdl.vw",
                         feature_params = list(hash="all", bit_precision=25),
                         optimization_params = list(adaptive=FALSE, learning_rate=0.1))
vwtrain(test_vwmodel, data = ext_train_data)
vw_output <- vwtest(test_vwmodel, data = ext_test_data, probs_path = "./probs.vw")

# Printing readable model
test_vwmodel <- vwsetup()
vwtrain(test_vwmodel, data = ext_train_data, readable_model = "hashed")
vwtest(test_vwmodel, data = ext_test_data, readable_model = "inverted")
# No console output
vwtrain(test_vwmodel, data = ext_train_data, quiet = T)
vwtest(test_vwmodel, data = ext_train_data, quiet = T)

# Add reductions via new interface
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
