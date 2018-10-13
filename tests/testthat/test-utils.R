context("Check auxiliary functionality")
library(rvw)

# Switch to temporary directory
curr_dir <- getwd()
setwd(tempdir())

ext_train_data <- system.file("extdata", "binary_train.vw", package = "rvw")
ext_test_data <- system.file("extdata", "binary_valid.vw", package = "rvw")

test_that("vwtrain and vwtest output correct readable model", {
    # Package session
    test_vwmodel <- vwsetup(dir = "./", model = "pk_mdl.vw")
    # vwtrain
    vwtrain(test_vwmodel, data = ext_train_data, readable_model = "hashed", quiet = T)
    vw_pk_train_hashed_mdl_checksum = unname(tools::md5sum("readable_pk_mdl.vw"))
    test_vwmodel <- vwsetup(dir = "./", model = "pk_mdl.vw")
    vwtrain(test_vwmodel, data = ext_train_data, readable_model = "inverted", quiet = T)
    vw_pk_train_inverted_mdl_checksum <- unname(tools::md5sum("readable_pk_mdl.vw"))
    # vwtest
    vwtest(test_vwmodel, data = ext_test_data, readable_model = "hashed", quiet = T)
    vw_pk_test_hashed_mdl_checksum = unname(tools::md5sum("readable_pk_mdl.vw"))
    vwtest(test_vwmodel, data = ext_test_data, readable_model = "inverted", quiet = T)
    vw_pk_test_inverted_mdl_checksum <- unname(tools::md5sum("readable_pk_mdl.vw"))

    file.remove("pk_mdl.vw","readable_pk_mdl.vw")

    # Command Line session
    # train
    system(
        paste0("vw -d ", ext_train_data, " -f ./cl_mdl.vw --readable_model ./readable_cl_mdl.vw"),
        intern = FALSE,
        ignore.stderr = TRUE
    )
    vw_cl_train_hashed_mdl_checksum = unname(tools::md5sum("readable_cl_mdl.vw"))
    system(
        paste0("vw -d ", ext_train_data, " -f ./cl_mdl.vw --invert_hash ./readable_cl_mdl.vw"),
        intern = FALSE,
        ignore.stderr = TRUE
    )
    vw_cl_train_inverted_mdl_checksum = unname(tools::md5sum("readable_cl_mdl.vw"))
    # test
    system(
        paste0("vw -t -d ", ext_test_data, " -i ./cl_mdl.vw --readable_model ./readable_cl_mdl.vw"),
        intern = FALSE,
        ignore.stderr = TRUE
    )
    vw_cl_test_hashed_mdl_checksum = unname(tools::md5sum("readable_cl_mdl.vw"))
    system(
        paste0("vw -t -d ", ext_test_data, " -i ./cl_mdl.vw --invert_hash ./readable_cl_mdl.vw"),
        intern = FALSE,
        ignore.stderr = TRUE
    )
    vw_cl_test_inverted_mdl_checksum = unname(tools::md5sum("readable_cl_mdl.vw"))
    file.remove("cl_mdl.vw","readable_cl_mdl.vw")

    # Results comparison
    unique_checksums_hashed <- length(unique(c(vw_pk_train_hashed_mdl_checksum,
                                               vw_pk_test_hashed_mdl_checksum,
                                               vw_cl_train_hashed_mdl_checksum,
                                               vw_cl_test_hashed_mdl_checksum)))

    expect_equal(vw_pk_train_hashed_mdl_checksum, vw_cl_train_hashed_mdl_checksum)
    expect_equal(vw_pk_test_hashed_mdl_checksum, vw_cl_test_hashed_mdl_checksum)
    expect_equal(vw_pk_train_inverted_mdl_checksum, vw_cl_train_inverted_mdl_checksum)
    expect_equal(vw_pk_test_inverted_mdl_checksum, vw_cl_test_inverted_mdl_checksum)
})

test_that("vwaudit outputs correct audit data.frame", {
    ref_df <- data.frame(Names = c("A^carat", "A^depth", "A^table", "A^price", "A^x",
                                   "A^z", "A^cut_Very_Good", "A^color_G", "A^clarity_SI1",
                                   "Constant", "A^cut_Premium", "A^color_I", "A^clarity_SI2",
                                   "A^cut_Good", "A^color_E", "A^clarity_VS2", "A^cut_Ideal",
                                   "A^color_D", "A^color_H", "A^color_F", "A^clarity_IF",
                                   "A^clarity_VS1", "A^cut_Fair", "A^color_J", "A^clarity_VVS2",
                                   "A^clarity_VVS1", "A^clarity_I1"),
                         Hashes = c(161523, 255131, 191106, 174484, 157305, 71870,
                                    197774, 147043, 202990, 116060, 179903, 131053,
                                    102042, 1176, 164360, 113391, 116290, 58595, 87066,
                                    240073, 1556, 114685, 151473, 32836, 101424, 80982, 141904),
                         V1 = c(-0.345847010612488, 0.00200122990645468,
                                          0.00143359997309744, -5.60191983822733e-05,
                                          -0.0254513993859291, -0.0392897985875607, 0.159768000245094,
                                          0.130854994058609, 0.0361801981925964, 0.119374997913837,
                                          0.0958541035652161, -0.0784583017230034, -0.191651001572609,
                                          0.144849002361298, 0.349283009767532, 0.0694333985447884,
                                          0.00745435990393162, -0.0727915987372398, -0.0811441987752914,
                                          0.273036986589432, 0.126379996538162, 0.171755000948906,
                                          -0.108182996511459, -0.328087002038956, 0.246926993131638,
                                          0.451092004776001, -0.148938998579979))

    test_vwmodel <- vwsetup()
    vwtrain(test_vwmodel, data = ext_train_data, quiet = T)
    aud_df <- vwaudit(test_vwmodel, quiet = T)

    expect_equal(aud_df, ref_df)
})

test_that("vwparams correctly returns and sets parameter values", {
    
    test_vwmodel <- vwsetup(general_params = list(link="identity", holdout_off=FALSE),
                            feature_params = list(bit_precision=10),
                            option = "nn", num_hidden = 5)
    
    
    
    # Character value
    vwparams(test_vwmodel, name = "link") <- "logistic"
    expect_equal(vwparams(test_vwmodel, name = "link"), "logistic")
    
    # Numerical value
    vwparams(test_vwmodel, name = "bit_precision") <- 25
    expect_equal(vwparams(test_vwmodel, name = "bit_precision"), 25)
    
    # Logical value
    vwparams(test_vwmodel, name = "holdout_off") <- TRUE
    expect_equal(vwparams(test_vwmodel, name = "holdout_off"), TRUE)
    
    # Option value
    vwparams(test_vwmodel, name = "num_hidden") <- 10
    expect_equal(vwparams(test_vwmodel, name = "num_hidden"), 10)
})

# Return back
setwd(curr_dir)
