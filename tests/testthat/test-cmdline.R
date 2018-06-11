context("Check predictions against command line version of VW")
library(rvwgsoc)

test_that("empty vwsetup produces the same predictions as CL version", {
  expect_equal(
    {test_vwmodel <- vwsetup(dir = "./", train_data = "X_train.vw", test_data = "X_valid.vw", model = "tmp_mdl.vw")
    vwtrain(test_vwmodel)
    vwtest(test_vwmodel)},
    {vw_cl_string <- paste0("vw --passes 1 --bit_precision 18", 
                            " --holdout_period 10 --early_terminate 3 --initial_weight 0", 
                            " --adaptive --normalized --invariant --l1 0 --l2 0 --decay_learning_rate 1", 
                            " --initial_t 0 --power_t 0.5 --learning_rate 0.5 --loss_function squared", 
                            " --quantile_tau 0.5", collapse = " ")
    system(paste0(vw_cl_string, " -d ./X_train.vw -f ./tmp_vwcl_mdl.vw"),
           intern = FALSE,
           ignore.stderr = TRUE)
    vwcl_output <- as.numeric(system(paste0(vw_cl_string, " -t -d ./X_valid.vw -i ./tmp_vwcl_mdl.vw -p /dev/stdout"),
                      intern = TRUE,
                      ignore.stderr = TRUE))
    file.remove("tmp_vwcl_mdl.vw", "tmp_mdl.vw")
    vwcl_output
    },
    tolerance=1e-7) 
})
