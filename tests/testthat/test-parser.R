context("Check df2vw parser")
library(rvw)

# Switch to temporary directory
curr_dir <- getwd()
setwd(tempdir())

test_that("df2vw correctly parses data", {
    df2vw_path <- "df2vw.vw"
    ref_path <- "ref.vw"

    test_df = data.frame(
        num_v1 = c(0.00005, 0.333333334, 10, 100000.314),
        fact_v2 = factor(c("a", "a", "b", "c")),
        text_v3 = rep("Et harum| (quid)em: rerum facilis!", 4),
        regular_label = c(1, 1.2, 4, 5.4),
        base = c(1, 1, 1, 1),
        multiline_label = c(0, 1, 1, 0),
        multilabel_1 = c(0.25, 0.25, 0.25, 0.25),
        multilabel_2 = c(0.25, 0.25, 0.25, 0.25),
        multilabel_3 = c(0.25, 0.25, 0.25, 0.25),
        multilabel_4 = c(0.25, 0.25, 0.25, 0.25),
        na_multilabel_1 = c(NA, 0.25, NA, 0.25),
        na_multilabel_2 = c(NA, NA, 0.25, 0.25),
        na_multilabel_3 = c(NA, NA, NA, NA),
        tag = c("ex1", "ex2", "ex3", "ex4"),
        importance = c("10", "0.5", "0.5", "4")
    )

    ref_df = data.frame(
        features = c("|NS1 num_v1:5e-05 fact_v2^a |NS2 fact_v2^a Et harum_ _quid_em_ rerum facilis!",
                     "|NS1 num_v1:0.333333334 fact_v2^a |NS2 fact_v2^a Et harum_ _quid_em_ rerum facilis!",
                     "|NS1 num_v1:10 fact_v2^b |NS2 fact_v2^b Et harum_ _quid_em_ rerum facilis!",
                     "|NS1 num_v1:100000.314 fact_v2^c |NS2 fact_v2^c Et harum_ _quid_em_ rerum facilis!"),
        regular_labels = c("1 10 1 ex1", "1.2 0.5 1 ex2", "4 0.5 1 ex3", "5.4 4 1 ex4"),
        csoaa_labels = c("1:0.25 2:0.25 3:0.25 4:0.25 ex1", "1:0.25 2:0.25 3:0.25 4:0.25 ex2",
                         "1:0.25 2:0.25 3:0.25 4:0.25 ex3", "1:0.25 2:0.25 3:0.25 4:0.25 ex4"),
        cb_labels = c("1:0.25:0.25 2:0.25:0.25 3:0.25:0.25 4:0.25:0.25 ex1",
                      "1:0.25:0.25 2:0.25:0.25 3:0.25:0.25 4:0.25:0.25 ex2",
                      "1:0.25:0.25 2:0.25:0.25 3:0.25:0.25 4:0.25:0.25 ex3",
                      "1:0.25:0.25 2:0.25:0.25 3:0.25:0.25 4:0.25:0.25 ex4"),
        na_labels = c(" ex1",
                      "1:0.25:0.25 ex2",
                      "2:0.25:0.25 ex3",
                      "1:0.25:0.25 2:0.25:0.25 ex4"),
        multiline_labels = c("1:0 ", "2:1 ", "1:1 ", "2:0 ")
    )

    # Regular labels
    cat("Regular labels\n")
    ref_file <- file(ref_path,"w")
    apply(ref_df, MARGIN = 1, function(x) {
        writeLines(text = paste0(x[["regular_labels"]], x[["features"]]), con = ref_file)
    })
    close(ref_file)
    regular_ref_checksum <- unname(tools::md5sum(ref_path))

    df2vw(data = test_df, file_path = df2vw_path,
                                    namespaces = list(NS1 = c("num_v1", "fact_v2"),
                                                      NS2 = c("fact_v2", "text_v3")),
                                    keep_space = "text_v3", base = "base",
                                    targets = "regular_label", tag = "tag", weight = "importance")
    regular_df2vw_checksum <- unname(tools::md5sum(df2vw_path))

    # CSOAA labels
    cat("CSOAA labels\n")
    ref_file <- file(ref_path,"w")
    apply(ref_df, MARGIN = 1, function(x) {
        writeLines(text = paste0(x[["csoaa_labels"]], x[["features"]]), con = ref_file)
    })
    close(ref_file)
    csoaa_ref_checksum <- unname(tools::md5sum(ref_path))

    df2vw(data = test_df, file_path = df2vw_path,
                                  namespaces = list(NS1 = c("num_v1", "fact_v2"),
                                                    NS2 = c("fact_v2", "text_v3")),
                                  keep_space = "text_v3",
                                  targets = c("multilabel_1", "multilabel_2", "multilabel_3", "multilabel_4"),
                                  tag = "tag", weight = "importance")
    csoaa_df2vw_checksum <- unname(tools::md5sum(df2vw_path))

    # Context Bandit labels
    cat("Context Bandit labels\n")
    ref_file <- file(ref_path,"w")
    apply(ref_df, MARGIN = 1, function(x) {
        writeLines(text = paste0(x[["cb_labels"]], x[["features"]]), con = ref_file)
    })
    close(ref_file)
    cb_ref_checksum <- unname(tools::md5sum(ref_path))

    df2vw(data = test_df, file_path = df2vw_path,
                                    namespaces = list(NS1 = c("num_v1", "fact_v2"),
                                                      NS2 = c("fact_v2", "text_v3")),
                                    keep_space = "text_v3",
                                    targets = c("multilabel_1", "multilabel_2", "multilabel_3", "multilabel_4"),
                                    probabilities = c("multilabel_1", "multilabel_2", "multilabel_3", "multilabel_4"),
                                    tag = "tag", weight = "importance")
    cb_df2vw_checksum <- unname(tools::md5sum(df2vw_path))

    # CSOAA when not all labels are allowed
    cat("CSOAA when not all labels are allowed\n")
    ref_file <- file(ref_path,"w")
    apply(ref_df, MARGIN = 1, function(x) {
        writeLines(text = paste0(x[["na_labels"]], x[["features"]]), con = ref_file)
    })
    close(ref_file)
    na_ref_checksum <- unname(tools::md5sum(ref_path))

    df2vw(data = test_df, file_path = df2vw_path,
          namespaces = list(NS1 = c("num_v1", "fact_v2"),
                            NS2 = c("fact_v2", "text_v3")),
          keep_space = "text_v3",
          targets = c("na_multilabel_1", "na_multilabel_2", "na_multilabel_3"),
          probabilities = c("multilabel_1", "multilabel_2", "multilabel_3"),
          tag = "tag", weight = "importance")
    na_df2vw_checksum <- unname(tools::md5sum(df2vw_path))

    # Multiline CSOAA
    cat("Multiline CSOAA\n")
    ref_file <- file(ref_path,"w")
    ref_df$lines <- apply(ref_df, MARGIN = 1, function(x) {
        paste0(x[["multiline_labels"]], x[["features"]])
    })
    writeLines(text = paste0(ref_df$lines, c("", "\n"), collapse = "\n"), con = ref_file)
    close(ref_file)
    mult_ref_checksum <- unname(tools::md5sum(ref_path))

    df2vw(data = test_df, file_path = df2vw_path,
          namespaces = list(NS1 = c("num_v1", "fact_v2"),
                            NS2 = c("fact_v2", "text_v3")),
          keep_space = "text_v3",
          targets = "multiline_label",
          multiline = 2)
    mult_df2vw_checksum <- unname(tools::md5sum(df2vw_path))


    file.remove(ref_path, df2vw_path)

    # Results comparison
    expect_equal(regular_df2vw_checksum, regular_ref_checksum)
    expect_equal(csoaa_df2vw_checksum, csoaa_ref_checksum)
    expect_equal(cb_df2vw_checksum, cb_ref_checksum)
    expect_equal(na_df2vw_checksum, na_ref_checksum)
    expect_equal(mult_df2vw_checksum, mult_ref_checksum)

})

# Return back
setwd(curr_dir)
