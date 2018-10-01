context("Check predictions against command line version of VW")
library(rvw)

# Switch to temporary directory
curr_dir <- getwd()
setwd(tempdir())

ext_train_data <- system.file("extdata", "binary_train.vw", package = "rvw")
ext_test_data <- system.file("extdata", "binary_valid.vw", package = "rvw")

lda_data <- system.file("extdata", "lda_data.vw", package = "rvw")

multiclass_train_data <- system.file("extdata", "multiclass_train.vw", package = "rvw")
multiclass_test_data <- system.file("extdata", "multiclass_valid.vw", package = "rvw")

test_that("empty vwsetup works as CL version", {
  # Package session
  test_vwmodel <- vwsetup(
    dir = "./",
    model = "pk_mdl.vw"
  )
  vwtrain(test_vwmodel, data = ext_train_data, quiet = T)
  vw_pk_output <- vwtest(test_vwmodel, data = ext_test_data, quiet = T)
  vw_pk_mdl_checksum <- unname(tools::md5sum("pk_mdl.vw"))
  file.remove("pk_mdl.vw")

  # Command Line session
  system(
    paste0("vw -d ", ext_train_data," -f ./cl_mdl.vw"),
    intern = FALSE,
    ignore.stderr = TRUE
  )
  vw_cl_output <- as.numeric(
    system(
      paste0("vw -t -d ", ext_test_data," -i ./cl_mdl.vw -p /dev/stdout"),
      intern = TRUE,
      ignore.stderr = TRUE
    )
  )
  vw_cl_mdl_checksum <- unname(tools::md5sum("cl_mdl.vw"))
  file.remove("cl_mdl.vw")

  # Results comparison
  expect_equal(vw_pk_mdl_checksum, vw_cl_mdl_checksum, tolerance=1e-7)
  expect_equal(vw_pk_output, vw_cl_output, tolerance=1e-7)
})

test_that("nn vwsetup works as CL version", {
  # Package session
  test_vwmodel <- vwsetup(
    dir = "./",
    model = "pk_mdl.vw",
    option = "nn",
    num_hidden = 4
  )
  vwtrain(test_vwmodel, data = ext_train_data, quiet = T)
  vw_pk_output <- vwtest(test_vwmodel, data = ext_test_data, quiet = T)
  vw_pk_mdl_checksum <- unname(tools::md5sum("pk_mdl.vw"))
  file.remove("pk_mdl.vw")

  # Command Line session
  system(
    paste0("vw --nn 4 -d ", ext_train_data," -f ./cl_mdl.vw"),
    intern = FALSE,
    ignore.stderr = TRUE
  )
  vw_cl_output <- as.numeric(
    system(
      paste0("vw --nn 4 -t -d ", ext_test_data," -i ./cl_mdl.vw -p /dev/stdout"),
      intern = TRUE,
      ignore.stderr = TRUE
    )
  )
  vw_cl_mdl_checksum <- unname(tools::md5sum("cl_mdl.vw"))
  file.remove("cl_mdl.vw")

  # Results comparison
  expect_equal(vw_pk_mdl_checksum, vw_cl_mdl_checksum)
  expect_equal(vw_pk_output, vw_cl_output, tolerance=1e-7)
})


test_that("vwsetup with custom arguments and cache works as CL version", {
  # Skip for now, because not yet finished fix for this case
  # skip("Problem is being fixed")
  # Package session
  test_vwmodel <- vwsetup(
    dir = "./",
    model = "pk_mdl.vw",
    general_params = list(random_seed = 42, loss_function="logistic", link="logistic"),
    feature_params = list(bit_precision=20, ngram="A2"),
    optimization_params = list(adaptive=FALSE, l1=1E-8)
  )
  test_vwmodel <- add_option(test_vwmodel, option = "boosting", num_learners=4)
  vwtrain(test_vwmodel, data = ext_train_data, quiet = T, passes = 10)
  vw_pk_output <- vwtest(test_vwmodel, data = ext_test_data, quiet = T)
  vw_pk_mdl_checksum <- unname(tools::md5sum("pk_mdl.vw"))
  file.remove("pk_mdl.vw","binary_train.vw.cache", "binary_valid.vw.cache")

  # Command Line session
  system(
    paste0("vw --random_seed 42 --loss_function logistic --link logistic ",
           "--bit_precision 20 --ngram A2 --l1 1e-08 --boosting 4 --passes 10 -c",
           " -d ", ext_train_data, " -f ./cl_mdl.vw"),
    intern = FALSE,
    ignore.stderr = TRUE
  )
  vw_cl_output <- as.numeric(
    system(
      paste0("vw -t -d ", ext_test_data, " -i ./cl_mdl.vw -p /dev/stdout"),
      intern = TRUE,
      ignore.stderr = TRUE
    )
  )
  vw_cl_mdl_checksum <- unname(tools::md5sum("cl_mdl.vw"))
  file.remove("cl_mdl.vw","binary_train.vw.cache", "binary_valid.vw.cache")

  # Results comparison
  expect_equal(vw_pk_mdl_checksum, vw_cl_mdl_checksum)
  expect_equal(vw_pk_output, vw_cl_output, tolerance=1e-7)
})

test_that("Updating model with new data works as CL version", {
    test_dir <- getwd()
    # Package session
    pk_mdl_file <- paste0(test_dir, "/", "pk_mdl.vw")
    test_vwmodel <- vwsetup(
        dir = test_dir,
        model = "pk_mdl.vw"
    )
    vwtrain(test_vwmodel, data = ext_train_data, update_model = TRUE, quiet = T)
    vw_pk_initial_mdl_checksum <- unname(tools::md5sum(pk_mdl_file))
    vwtrain(test_vwmodel, data = ext_test_data, update_model = TRUE, quiet = T)
    vw_pk_updated_mdl_checksum <- unname(tools::md5sum(pk_mdl_file))
    vw_pk_output <- predict.vw(test_vwmodel, data = ext_test_data, quiet = T)
    file.remove(pk_mdl_file)

    # Command Line session
    cl_mdl_file <- paste0(test_dir, "/", "cl_mdl.vw")
    system(
        paste0("vw",
               " -d ", ext_train_data, " -f ", cl_mdl_file, " --save_resume --quiet"),
        intern = FALSE,
        ignore.stderr = TRUE
    )
    vw_cl_initial_mdl_checksum <- unname(tools::md5sum(cl_mdl_file))
    system(
        paste0("vw",
               " -d ", ext_test_data, " -i ", cl_mdl_file, " -f ", cl_mdl_file, " --save_resume --quiet"),
        intern = FALSE,
        ignore.stderr = TRUE
    )
    vw_cl_updated_mdl_checksum <- unname(tools::md5sum(cl_mdl_file))
    vw_cl_output <- as.numeric(
        system(
            paste0("vw",
                   " -t -d ", ext_test_data, " -i ", cl_mdl_file, " -p /dev/stdout --quiet"),
            intern = TRUE,
            ignore.stderr = TRUE
        )
    )
    file.remove(cl_mdl_file)

    # Results comparison
    expect_equal(vw_pk_initial_mdl_checksum, vw_cl_initial_mdl_checksum)
    expect_equal(vw_pk_updated_mdl_checksum, vw_cl_updated_mdl_checksum)
    expect_equal(vw_pk_output, vw_cl_output, tolerance=1e-7)
})

test_that("vwsetup with multiclass classification setup works as CL version", {
    # Package session
    test_vwmodel <- vwsetup(
        dir = "./",
        model = "pk_mdl.vw",
        option = "ect",
        num_classes = 3
    )
    vwtrain(test_vwmodel, data = multiclass_train_data, quiet = T, passes = 4)
    vw_pk_output <- vwtest(test_vwmodel, data = multiclass_test_data, quiet = T)
    vw_pk_mdl_checksum <- unname(tools::md5sum("pk_mdl.vw"))
    file.remove("pk_mdl.vw","multiclass_train.vw.cache", "multiclass_valid.vw.cache")

    # Command Line session
    system(
        paste0("vw --passes 4 --cache --ect 3",
               " -d ", multiclass_train_data, " -f ./cl_mdl.vw"),
        intern = FALSE,
        ignore.stderr = TRUE
    )
    system(
        paste0("vw",
               " -t -d ", multiclass_test_data, " -i ./cl_mdl.vw -p ./cl_probs.out"),
        intern = FALSE,
        ignore.stderr = TRUE
    )
    vw_cl_output <- read.table(file = "./cl_probs.out", sep = " ", header = F)
    vw_cl_output <- vw_cl_output$V1
    vw_cl_mdl_checksum <- unname(tools::md5sum("cl_mdl.vw"))
    file.remove("cl_mdl.vw", "multiclass_train.vw.cache", "multiclass_valid.vw.cache", "cl_probs.out")

    # Results comparison
    expect_equal(vw_pk_mdl_checksum, vw_cl_mdl_checksum)
    expect_equal(vw_pk_output, vw_cl_output, tolerance=1e-7)
})

test_that("vwsetup with lda setup works as CL version", {
    # Package session
    test_vwmodel <- vwsetup(
        dir = "./",
        model = "pk_mdl.vw",
        option = "lda",
        num_topics = 7,
        lda_D = 100,
        minibatch = 16
    )
    vwtrain(test_vwmodel, data = lda_data, quiet = T, passes = 2,
            readable_model = "hashed", readable_model_path = "pk_readable_mdl.vw")
    vw_pk_mdl_checksum <- unname(tools::md5sum("pk_mdl.vw"))
    vw_pk_readable_mdl_checksum <- unname(tools::md5sum("pk_readable_mdl.vw"))
    file.remove("pk_mdl.vw", "pk_readable_mdl.vw", "lda_data.vw.cache")
    
    # Command Line session
    system(
        paste0("vw --lda 7 --lda_D 100 --minibatch 16 --passes 2",
               " --cache_file ./lda_data.vw.cache",
               " --readable_model ./cl_readable_mdl.vw",
               " -d ", lda_data, " -f ./cl_mdl.vw"),
        intern = FALSE,
        ignore.stderr = TRUE
    )
    vw_cl_mdl_checksum <- unname(tools::md5sum("cl_mdl.vw"))
    vw_cl_readable_mdl_checksum <- unname(tools::md5sum("cl_readable_mdl.vw"))
    file.remove("cl_mdl.vw", "cl_readable_mdl.vw", "lda_data.vw.cache")
    
    # Results comparison
    expect_equal(vw_pk_mdl_checksum, vw_cl_mdl_checksum)
    expect_equal(vw_pk_readable_mdl_checksum, vw_cl_readable_mdl_checksum)
})

test_that("vwsetup with multicolumn output", {
    # Package session
    test_vwmodel <- vwsetup(
        dir = "./",
        model = "pk_mdl.vw",
        option = "oaa",
        num_classes = 3
    )
    vwtrain(test_vwmodel, data = multiclass_train_data, quiet = T, passes = 2)
    vw_pk_output <- vwtest(test_vwmodel, data = multiclass_test_data, quiet = T, full_probs = T)
    write.table(vw_pk_output, file = "pk_probs.out", sep = " ", quote = F, row.names = F, col.names = F)
    vw_pk_probs_checksum <- unname(tools::md5sum("pk_probs.out"))
    
    file.remove("pk_mdl.vw", "pk_probs.out", "multiclass_train.vw.cache")
    
    # Command Line session
    system(
        paste0("vw --oaa 3 --passes 2 --cache",
               " -d ", multiclass_train_data, " -f ./cl_mdl.vw"),
        intern = FALSE,
        ignore.stderr = TRUE
    )
    system(
        paste0("vw",
               " -t -d ", multiclass_test_data, " -i ./cl_mdl.vw -p ./cl_probs.out"),
        intern = FALSE,
        ignore.stderr = TRUE
    )
    vw_cl_probs_checksum <- unname(tools::md5sum("cl_probs.out"))
    
    file.remove("cl_mdl.vw", "cl_probs.out", "multiclass_train.vw.cache")
    
    # Results comparison
    expect_equal(vw_pk_probs_checksum, vw_cl_probs_checksum)
})

test_that("vwsetup with df2vw conversion works as CL version", {
    # Package session
    test_vwmodel <- vwsetup(
        dir = "./",
        model = "pk_mdl.vw",
        option = "oaa",
        num_classes = 3
    )
    
    data_full <- iris
    levels(data_full$Species) <- c(1, 2, 3)
    ind_train <- sample(1:nrow(data_full), 0.8*nrow(data_full))
    
    vwtrain(test_vwmodel, data = data_full[ind_train,], quiet = T,
            targets = "Species")
    vw_pk_output <- vwtest(test_vwmodel, data = data_full[-ind_train,], quiet = T,
                           targets = "Species")
    vw_pk_mdl_checksum <- unname(tools::md5sum("pk_mdl.vw"))
    file.remove("pk_mdl.vw")
    
    # Command Line session
    
    df2vw(data = data_full[ind_train,], file_path = "cl_train.vw",
          targets = "Species")
    df2vw(data = data_full[-ind_train,], file_path = "cl_test.vw",
          targets = "Species")
    
    system(
        paste0("vw --oaa 3 -d ./cl_train.vw -f ./cl_mdl.vw"),
        intern = FALSE,
        ignore.stderr = TRUE
    )
    vw_cl_output <- as.numeric(
        system(
            paste0("vw -t -d ./cl_test.vw -i ./cl_mdl.vw -p /dev/stdout"),
            intern = TRUE,
            ignore.stderr = TRUE
        )
    )
    vw_cl_mdl_checksum <- unname(tools::md5sum("cl_mdl.vw"))
    file.remove("cl_mdl.vw", "cl_train.vw", "cl_test.vw")
    
    # Results comparison
    expect_equal(vw_pk_mdl_checksum, vw_cl_mdl_checksum, tolerance=1e-7)
    expect_equal(vw_pk_output, vw_cl_output, tolerance=1e-7)
})

test_that("print.vw outputs correct results to console", {
    test_vwmodel <- vwsetup(
        dir = "./",
        model = "pk_mdl.vw",
        option = "boosting",
        num_learners = 10
    )
    vwtrain(test_vwmodel, data = ext_train_data, quiet = T)
    vw_pk_output <- vwtest(test_vwmodel, data = ext_test_data, quiet = T)
    
    capture.output(test_vwmodel, file = "pk_print.out")
    
    vw_pk_print_checksum <- unname(tools::md5sum("pk_print.out"))
    ref_print_file <- system.file("extdata", "ref_print.out", package = "rvw")
    vw_ref_print_checksum <- unname(tools::md5sum(ref_print_file))
    
    file.remove("pk_mdl.vw", "pk_print.out")
    
    expect_equal(vw_pk_print_checksum, vw_ref_print_checksum)
})

# Return back
setwd(curr_dir)
