context("vwsetup")
library(rvwgsoc)

test_model <- list(params = list(learning_mode = "binary",
                                 algorithm = "sgd",
                                 general_params = list(passes=1,
                                                       bit_precision=18,
                                                       qudratic=FALSE,
                                                       cubic=FALSE,
                                                       interactions=NA,
                                                       permutations=FALSE,
                                                       holdout_period=10,
                                                       early_terminate=3,
                                                       sort_features=FALSE,
                                                       noconstant=FALSE,
                                                       ngram=NA,
                                                       skips=NA,
                                                       hash=NA,
                                                       affix=NA,
                                                       random_weights=FALSE,
                                                       sparse_weights=FALSE,
                                                       initial_weight=0),
                                 learning_params = list(binary = FALSE),
                                 optimization_params = list(
                                   adaptive=TRUE,
                                   normalized=TRUE,
                                   invariant=TRUE,
                                   hessian=FALSE,
                                   initial_pass_length=NA,
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
params_str = paste("--binary  --passes 1 --bit_precision 18 --holdout_period 10 --early_terminate 3 ",
                   "--initial_weight 0 --adaptive --normalized --invariant --l1 0 --l2 0 --decay_learning_rate 1",
                   " --initial_t 0 --power_t 0.5 --learning_rate 0.5 --loss_function squared --quantile_tau 0.5", sep = ""),
data = list(train = "",
            test = ""),
eval = "")
class(test_model) <- "vw"

test_that("vwsetup correctly setup model with different learning modes", {
  expect_identical(vwsetup(dir = "../my_tmp/"), test_model)
  expect_identical(vwsetup(dir = "../my_tmp/", learning_mode = "nn"), {test_model$params$learning_mode = "nn";
  test_model$params$learning_params = list(hidden=3,
                                           inpass=FALSE,
                                           multitask=FALSE,
                                           dropout=FALSE,
                                           meanfield=FALSE);
  test_model$params_str = paste("--nn 3  --passes 1 --bit_precision 18 --holdout_period 10 --early_terminate 3 ",
                                "--initial_weight 0 --adaptive --normalized --invariant --l1 0 --l2 0 --decay_learning_rate 1",
                                " --initial_t 0 --power_t 0.5 --learning_rate 0.5 --loss_function squared --quantile_tau 0.5", sep = "");
  test_model})
  expect_identical(vwsetup(dir = "../my_tmp/", learning_mode = "lda"), {test_model$params$learning_mode = "lda";
  test_model$params$learning_params = list(num_topics=0,
                                           lda_alpha=0.100000001,
                                           lda_rho=0.100000001,
                                           lda_D=10000,
                                           lda_epsilon=0.00100000005,
                                           math_mode=0,
                                           minibatch=1);
  test_model$params_str = paste("--lda 0  --passes 1 --bit_precision 18 --holdout_period 10 --early_terminate 3 ",
                                "--initial_weight 0 --lda_alpha 0.100000001 --lda_rho 0.100000001 --lda_D 10000",
                                " --lda_epsilon 0.00100000005 --math_mode 0 --minibatch 1 --adaptive --normalized",
                                " --invariant --l1 0 --l2 0 --decay_learning_rate 1 --initial_t 0 --power_t 0.5",
                                " --learning_rate 0.5 --loss_function squared --quantile_tau 0.5", sep = "");
  test_model})
})