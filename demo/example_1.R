library(rvwgsoc)
library(magrittr)


curr_dir <- getwd()
setwd(tempdir())

# Right now using only files in .vw format, later will also accept R formats
ext_train_data <- system.file("extdata", "binary_train.vw", package = "rvwgsoc")
ext_test_data <- system.file("extdata", "binary_valid.vw", package = "rvwgsoc")

test_vwmodel <-  vwsetup(dir = "./", model = "mdl.vw",
                         general_params = list(cache = TRUE, hash="all", passes=10),
                         optimization_params = list(adaptive=FALSE, learning_rate=0.1))
vwtrain(test_vwmodel, data_path = ext_train_data)
vw_output <- vwtest(test_vwmodel, data_path = ext_test_data, probs_path = "./probs.vw")

# Printing readable model
test_vwmodel <- vwsetup()
vwtrain(test_vwmodel, data_path = ext_train_data, readable_model = "hashed")
vwtest(test_vwmodel, data_path = ext_test_data, readable_model = "inverted")
# No console output
vwtrain(test_vwmodel, data_path = ext_train_data, quiet = T)
vwtest(test_vwmodel, data_path = ext_train_data, quiet = T)

setwd(curr_dir)


test_vwmodel <-  vwsetup(dir = "./", model = "mdl.vw") 
test_vwmodel <- add_reduction(vwmodel = test_vwmodel, name = "bootstrap", rounds=10, bs_type="mean")
test_vwmodel <- add_reduction(vwmodel = test_vwmodel, name = "oaa", num_classes=3)
test_vwmodel$params_str
