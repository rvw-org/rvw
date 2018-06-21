context("Check auxiliary functionality")
library(rvwgsoc)

ext_train_data <- system.file("extdata", "X_train.vw", package = "rvwgsoc")
ext_test_data <- system.file("extdata", "X_valid.vw", package = "rvwgsoc")

test_that("vwtrain and vwtest output correct readable model", {
    # Package session
    test_vwmodel <- vwsetup(
        dir = "./",
        model = "pk_mdl.vw"
    )
    # vwtrain
    vwtrain(test_vwmodel, data_path = ext_train_data, readable_model = "hashed")
    vw_pk_train_hashed_mdl_checksum = unname(tools::md5sum("readable_pk_mdl.vw"))
    vwtrain(test_vwmodel, data_path = ext_train_data, readable_model = "inverted")
    vw_pk_train_inverted_mdl_checksum <- unname(tools::md5sum("readable_pk_mdl.vw"))
    # vwtest
    vwtest(test_vwmodel, data_path = ext_test_data, readable_model = "hashed")
    vw_pk_test_hashed_mdl_checksum = unname(tools::md5sum("readable_pk_mdl.vw"))
    vwtest(test_vwmodel, data_path = ext_test_data, readable_model = "inverted")
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
        paste0("vw -t -d ", ext_test_data, " -i ./cl_mdl.vw -p /dev/stdout --readable_model ./readable_cl_mdl.vw"),
        intern = FALSE,
        ignore.stderr = TRUE
    )
    vw_cl_test_hashed_mdl_checksum = unname(tools::md5sum("readable_cl_mdl.vw"))
    system(
        paste0("vw -t -d ", ext_test_data, " -i ./cl_mdl.vw -p /dev/stdout --invert_hash ./readable_cl_mdl.vw"),
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