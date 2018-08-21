context("vwsetup")
library(rvw)

test_model <- list(params = list(algorithm = "sgd",
                                 general_params = list(random_seed=0,
                                                       ring_size=NA_real_,
                                                       holdout_off=FALSE,
                                                       holdout_period=10,
                                                       holdout_after=0,
                                                       early_terminate=3,
                                                       loss_function="squared",
                                                       link="identity",
                                                       quantile_tau=0.5),
                                 feature_params = list(bit_precision=18,
                                                       quadratic=NA_character_,
                                                       cubic=NA_character_,
                                                       interactions=NA_character_,
                                                       permutations=FALSE,
                                                       leave_duplicate_interactions=FALSE,
                                                       noconstant=FALSE,
                                                       feature_limit=NA_character_,
                                                       ngram=NA_character_,
                                                       skips=NA_character_,
                                                       hash=NA_character_,
                                                       affix=NA_character_,
                                                       spelling=NA_character_,
                                                       interact=NA_character_),
                                 optimization_params = list(adaptive=TRUE,
                                                            normalized=TRUE,
                                                            invariant=TRUE,
                                                            adax=FALSE,
                                                            sparse_l2=0,
                                                            l1_state=0,
                                                            l2_state=1,
                                                            learning_rate=0.5,
                                                            initial_pass_length=NA_real_,
                                                            l1=0,
                                                            l2=0,
                                                            no_bias_regularization=NA_character_,
                                                            feature_mask=NA_character_,
                                                            decay_learning_rate=1,
                                                            initial_t=0,
                                                            power_t=0.5,
                                                            initial_weight=0,
                                                            random_weights=NA_character_,
                                                            normal_weights=NA_character_,
                                                            truncated_normal_weights=NA_character_,
                                                            sparse_weights=FALSE,
                                                            input_feature_regularizer=NA_character_),
                                 options = list()

),
dir = "../my_tmp",
model = "mdl.vw",
params_str = paste0(""),
data_md5sum = list(train = "",
            test = ""),
train_file = "",
eval = list(
    train=list(
        num_examples = NA_real_,
        weighted_example_sum = NA_real_,
        weighted_label_sum = NA_real_,
        avg_loss = NA_real_,
        avg_multiclass_log_loss = NA_real_,
        best_const = NA_real_,
        best_const_loss = NA_real_,
        total_feature = NA_real_
    ),
    test=list(
        num_examples = NA_real_,
        weighted_example_sum = NA_real_,
        weighted_label_sum = NA_real_,
        avg_loss = NA_real_,
        avg_multiclass_log_loss = NA_real_,
        best_const = NA_real_,
        best_const_loss = NA_real_,
        total_feature = NA_real_
    )
),
parser_opts=NA
)
class(test_model) <- "vw"

test_that("vwsetup correctly setup model with different learning modes", {
  # Empty setup
    expect_equal(vwsetup(dir = "../my_tmp/", model = "mdl.vw"), test_model)

  # Reference test model for nn mode
  nn_test_model <- test_model
  nn_test_model$params$options = list(nn = list(num_hidden=3,
                                           inpass=FALSE,
                                           multitask=FALSE,
                                           dropout=FALSE,
                                           meanfield=FALSE))
  nn_test_model$params_str = paste0("--nn 3")
  expect_equal(
    vwsetup(dir = "../my_tmp/", model = "mdl.vw", option = "nn", num_hidden=3),
    nn_test_model
  )

  # Reference test model for lda mode
  lda_test_model <- test_model
  lda_test_model$params$options = list(lda = list(num_topics=5,
                                           lda_alpha=0.100000001,
                                           lda_rho=0.100000001,
                                           lda_D=10000,
                                           lda_epsilon=0.00100000005,
                                           math_mode=0,
                                           minibatch=1))
  lda_test_model$params_str = paste0("--lda 5")

  expect_equal(
    vwsetup(dir = "../my_tmp/", model = "mdl.vw", option = "lda", num_topics=5),
    lda_test_model
  )

  # Reference test model with custom parameters
  custom_test_model <- test_model
  custom_test_model$params$optimization_params$adaptive = FALSE
  custom_test_model$params$options = list(binary = list(binary=TRUE))
  custom_test_model$params_str <- paste0("--binary")
  # Package vwmodel setup
  test_vwmodel <- vwsetup(
    dir = "../my_tmp/",
    model = "mdl.vw",
    optimization_params = list(adaptive=FALSE),
    option = "binary"
  )
  expect_equal(test_vwmodel, custom_test_model)

  # Reference test model with Experience Replay
  replay_test_model <- test_model
  replay_test_model$params$options = list(replay = list(level="m",
                                                        buffer=200,
                                                        count=1))
  replay_test_model$params_str <- paste0("--replay_m 200 --replay_m_count 1")
  # Package vwmodel setup
  test_vwmodel <- vwsetup(
      dir = "../my_tmp/",
      model = "mdl.vw",
      option = "replay",
      level="m",
      buffer=200
  )
  expect_equal(test_vwmodel, replay_test_model)

  # Reference test model with Contextual Bandit Exploration with Action Dependent Features
  cb_explore_test_model <- test_model
  cb_explore_test_model$params$options = list(cb_explore = list(num_actions=0,
                                                        explore_type="bag",
                                                        explore_arg=10,
                                                        psi=1,
                                                        nounif=FALSE,
                                                        mellowness=0.1,
                                                        greedify=FALSE,
                                                        lambda=-1,
                                                        cb_min_cost=0,
                                                        cb_max_cost=1,
                                                        first_only=FALSE))
  cb_explore_test_model$params_str <- paste0("--cb_explore_adf --bag 10")
  # Package vwmodel setup
  test_vwmodel <- vwsetup(
      dir = "../my_tmp/",
      model = "mdl.vw",
      option = "cb_explore",
      num_actions=0,
      explore_type="bag",
      explore_arg=10
  )
  expect_equal(test_vwmodel, cb_explore_test_model)
})
