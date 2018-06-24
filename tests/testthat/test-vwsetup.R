context("vwsetup")
library(rvwgsoc)

test_model <- list(params = list(learning_mode = "binary",
                                 algorithm = "sgd",
                                 general_params = list(cache=FALSE,
                                                       passes=1,
                                                       bit_precision=18,
                                                       quadratic=FALSE,
                                                       cubic=FALSE,
                                                       interactions="",
                                                       permutations=FALSE,
                                                       holdout_period=10,
                                                       early_terminate=3,
                                                       sort_features=FALSE,
                                                       noconstant=FALSE,
                                                       ngram="",
                                                       skips="",
                                                       hash="",
                                                       affix="",
                                                       random_weights=FALSE,
                                                       sparse_weights=FALSE,
                                                       initial_weight=0),
                                 learning_params = list(binary = FALSE),
                                 optimization_params = list(
                                   adaptive=TRUE,
                                   normalized=TRUE,
                                   invariant=TRUE,
                                   hessian_on=FALSE,
                                   initial_pass_length="",
                                   l1=0,
                                   l2=0,
                                   decay_learning_rate=1,
                                   initial_t=0,
                                   power_t=0.5,
                                   learning_rate=0.5,
                                   loss_function="squared",
                                   quantile_tau=0.5)

),
dir = "../my_tmp/",
model = "mdl.vw",
update_model = TRUE,
params_str = paste("  --passes 1 --bit_precision 18 --holdout_period 10",
                   " --early_terminate 3 --initial_weight 0 --adaptive",
                   " --normalized --invariant --l1 0 --l2 0",
                   " --decay_learning_rate 1 --initial_t 0 --power_t 0.5",
                   " --learning_rate 0.5 --loss_function squared",
                   " --quantile_tau 0.5", sep = ""),
data = list(train = "",
            test = ""),
cache = list(train = "",
             test = ""),
eval = "")
class(test_model) <- "vw"

test_that("vwsetup correctly setup model with different learning modes", {
  # Empty setup
  expect_identical(vwsetup(dir = "../my_tmp/"), test_model)
  
  # Reference test model for nn mode
  nn_test_model <- test_model
  nn_test_model$params$learning_mode = "nn"
  nn_test_model$params$learning_params = list(hidden=3,
                                           inpass=FALSE,
                                           multitask=FALSE,
                                           dropout=FALSE,
                                           meanfield=FALSE)
  nn_test_model$params_str = paste("--nn 3  --passes 1 --bit_precision 18 --holdout_period 10",
                                " --early_terminate 3 --initial_weight 0 --adaptive",
                                " --normalized --invariant --l1 0 --l2 0",
                                " --decay_learning_rate 1 --initial_t 0 --power_t 0.5",
                                " --learning_rate 0.5 --loss_function squared",
                                " --quantile_tau 0.5", sep = "")
  expect_identical(
    vwsetup(dir = "../my_tmp/", learning_mode = "nn"),
    nn_test_model
  )
  
  # Reference test model for lda mode
  lda_test_model <- test_model
  lda_test_model$params$learning_mode = "lda"
  lda_test_model$params$learning_params = list(num_topics=0,
                                           lda_alpha=0.100000001,
                                           lda_rho=0.100000001,
                                           lda_D=10000,
                                           lda_epsilon=0.00100000005,
                                           math_mode=0,
                                           minibatch=1)
  lda_test_model$params_str = paste("--lda 0  --passes 1 --bit_precision 18",
                                " --holdout_period 10 --early_terminate 3",
                                " --initial_weight 0 --lda_alpha 0.100000001",
                                " --lda_rho 0.100000001 --lda_D 10000",
                                " --lda_epsilon 0.00100000005 --math_mode 0",
                                " --minibatch 1 --adaptive --normalized --invariant",
                                " --l1 0 --l2 0 --decay_learning_rate 1 --initial_t 0",
                                " --power_t 0.5 --learning_rate 0.5 --loss_function squared",
                                " --quantile_tau 0.5", sep = "")
  
  expect_identical(
    vwsetup(dir = "../my_tmp/", learning_mode = "lda"),
    lda_test_model
  )
  
  # Reference test model with custom parameters
  custom_test_model <- test_model
  custom_test_model$params$general_params$cache = TRUE
  custom_test_model$params$general_params$passes = 10
  custom_test_model$params$optimization_params$adaptive = FALSE
  custom_test_model$params$learning_params$binary = TRUE
  custom_test_model$params_str <- paste("  --passes 10 --bit_precision 18",
                                 " --holdout_period 10 --early_terminate 3",
                                 " --initial_weight 0 --binary --normalized",
                                 " --invariant --l1 0 --l2 0 --decay_learning_rate 1",
                                 " --initial_t 0 --power_t 0.5 --learning_rate 0.5",
                                 " --loss_function squared --quantile_tau 0.5", sep = "")
  # Package vwmodel setup
  test_vwmodel <- vwsetup(
    dir = "../my_tmp/",
    general_params = list(cache = TRUE, passes=10),
    optimization_params = list(adaptive=FALSE),
    learning_params = list(binary=TRUE)
  )
  expect_identical(test_vwmodel, custom_test_model)
  
})