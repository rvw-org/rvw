context("vwsetup")
library(rvwgsoc)

test_model <- list(params = list(algorithm = "sgd",
                                 general_params = list(random_seed=0,
                                                       ring_size="",
                                                       holdout_off=FALSE,
                                                       holdout_period=10,
                                                       holdout_after=0,
                                                       early_terminate=3,
                                                       loss_function="squared",
                                                       link="identity",
                                                       quantile_tau=0.5,
                                                       confidence=FALSE,
                                                       confidence_after_training=FALSE),
                                 feature_params = list(bit_precision=18,
                                                       quadratic="",
                                                       cubic="",
                                                       interactions="",
                                                       permutations=FALSE,
                                                       leave_duplicate_interactions=FALSE,
                                                       noconstant=FALSE, 
                                                       feature_limit="",
                                                       ngram="",
                                                       skips="",
                                                       hash="",
                                                       affix="",
                                                       spelling=""),
                                 optimization_params = list(adaptive=TRUE,
                                                            normalized=TRUE,
                                                            invariant=TRUE,
                                                            adax=FALSE,
                                                            sparse_l2=0,
                                                            l1_state=0,
                                                            l2_state=1,
                                                            learning_rate=0.5,
                                                            initial_pass_length="",
                                                            l1=0,
                                                            l2=0,
                                                            no_bias_regularization=FALSE,
                                                            feature_mask="",
                                                            decay_learning_rate=1,
                                                            initial_t=0,
                                                            power_t=0.5,
                                                            initial_weight=0,
                                                            random_weights="",
                                                            normal_weights="",
                                                            truncated_normal_weights="",
                                                            sparse_weights=FALSE,
                                                            input_feature_regularizer=""),
                                 reductions = list()

),
dir = "../my_tmp/",
model = "mdl.vw",
params_str = paste0(" --random_seed 0 --holdout_period 10 --holdout_after 0",
                    " --early_terminate 3 --loss_function squared --link identity",
                    " --quantile_tau 0.5 --bit_precision 18 --adaptive --normalized --invariant",
                    " --sparse_l2 0 --l1_state 0 --l2_state 1 --learning_rate 0.5",
                    " --l1 0 --l2 0 --decay_learning_rate 1 --initial_t 0 --power_t 0.5",
                    " --initial_weight 0  "),
data_md5sum = list(train = "",
            test = ""),
train_file = "",
eval = "")
class(test_model) <- "vw"

test_that("vwsetup correctly setup model with different learning modes", {
  # Empty setup
  expect_identical(vwsetup(dir = "../my_tmp/"), test_model)
  
  # Reference test model for nn mode
  nn_test_model <- test_model
  nn_test_model$params$reductions = list(nn = list(hidden=3,
                                           inpass=FALSE,
                                           multitask=FALSE,
                                           dropout=FALSE,
                                           meanfield=FALSE))
  nn_test_model$params_str = paste0(" --random_seed 0 --holdout_period 10 --holdout_after 0",
                                    " --early_terminate 3 --loss_function squared --link identity",
                                    " --quantile_tau 0.5 --bit_precision 18 --adaptive --normalized --invariant",
                                    " --sparse_l2 0 --l1_state 0 --l2_state 1 --learning_rate 0.5",
                                    " --l1 0 --l2 0 --decay_learning_rate 1 --initial_t 0 --power_t 0.5",
                                    " --initial_weight 0 --nn 3 ")
  expect_identical(
    vwsetup(dir = "../my_tmp/", reduction = "nn"),
    nn_test_model
  )
  
  # Reference test model for lda mode
  lda_test_model <- test_model
  lda_test_model$params$reductions = list(lda = list(num_topics=0,
                                           lda_alpha=0.100000001,
                                           lda_rho=0.100000001,
                                           lda_D=10000,
                                           lda_epsilon=0.00100000005,
                                           math_mode=0,
                                           minibatch=1))
  lda_test_model$params_str = paste0(" --random_seed 0 --holdout_period 10 --holdout_after 0",
                                     " --early_terminate 3 --loss_function squared --link identity",
                                     " --quantile_tau 0.5 --bit_precision 18 --adaptive --normalized --invariant",
                                     " --sparse_l2 0 --l1_state 0 --l2_state 1 --learning_rate 0.5",
                                     " --l1 0 --l2 0 --decay_learning_rate 1 --initial_t 0 --power_t 0.5",
                                     " --initial_weight 0 --lda 0 --lda_alpha 0.100000001",
                                     " --lda_rho 0.100000001 --lda_D 10000",
                                     " --lda_epsilon 0.00100000005 --math_mode 0",
                                     " --minibatch 1")
  
  expect_identical(
    vwsetup(dir = "../my_tmp/", reduction = "lda"),
    lda_test_model
  )
  
  # Reference test model with custom parameters
  custom_test_model <- test_model
  custom_test_model$params$optimization_params$adaptive = FALSE
  custom_test_model$params$reductions = list(binary = list())
  custom_test_model$params_str <- paste0(" --random_seed 0 --holdout_period 10 --holdout_after 0",
                                         " --early_terminate 3 --loss_function squared --link identity",
                                         " --quantile_tau 0.5 --bit_precision 18 --normalized --invariant",
                                         " --sparse_l2 0 --l1_state 0 --l2_state 1 --learning_rate 0.5",
                                         " --l1 0 --l2 0 --decay_learning_rate 1 --initial_t 0 --power_t 0.5",
                                         " --initial_weight 0 --binary ")
  # Package vwmodel setup
  test_vwmodel <- vwsetup(
    dir = "../my_tmp/",
    optimization_params = list(adaptive=FALSE),
    reduction = "binary"
  )
  expect_identical(test_vwmodel, custom_test_model)
  
})