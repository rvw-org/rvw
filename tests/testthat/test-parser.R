context("Check df2vw parser")
library(rvwgsoc)

test_that("df2vw correctly parses data", {
    df2vw_path <- "df2vw.vw"
    ref_path <- "ref.vw"
    
    test_df = data.frame(
        num_v1 = c(0.00005, 0.333333334, 10, 100000.314),
        fact_v2 = factor(c("a", "a", "b", "c")),
        text_v3 = rep("Et harum| (quid)em: rerum facilis!", 4),
        regular_label = c(1, 1.2, 4, 5.4),
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
        features = c("|NS1 num_v1:5.000000e-05 fact_v2^a |NS2 fact_v2^a Et harum_ _quid_em_ rerum facilis!",
                     "|NS1 num_v1:3.333333e-01 fact_v2^a |NS2 fact_v2^a Et harum_ _quid_em_ rerum facilis!",
                     "|NS1 num_v1:1.000000e+01 fact_v2^b |NS2 fact_v2^b Et harum_ _quid_em_ rerum facilis!",
                     "|NS1 num_v1:1.000003e+05 fact_v2^c |NS2 fact_v2^c Et harum_ _quid_em_ rerum facilis!"),
        regular_labels = c("1.0 10 'ex1 ", "1.2 0.5 'ex2 ", "4.0 0.5 'ex3 ", "5.4 4 'ex4 "),
        csoaa_labels = c("1:0.25 2:0.25 3:0.25 4:0.25 10 'ex1 ", "1:0.25 2:0.25 3:0.25 4:0.25 0.5 'ex2 ",
                         "1:0.25 2:0.25 3:0.25 4:0.25 0.5 'ex3 ", "1:0.25 2:0.25 3:0.25 4:0.25 4 'ex4 "),
        cb_labels = c("1:0.25:0.25 2:0.25:0.25 3:0.25:0.25 4:0.25:0.25 10 'ex1 ",
                      "1:0.25:0.25 2:0.25:0.25 3:0.25:0.25 4:0.25:0.25 0.5 'ex2 ",
                      "1:0.25:0.25 2:0.25:0.25 3:0.25:0.25 4:0.25:0.25 0.5 'ex3 ",
                      "1:0.25:0.25 2:0.25:0.25 3:0.25:0.25 4:0.25:0.25 4 'ex4 "),
        na_labels = c(" ",
                      "1:0.25:0.25 0.5 'ex2 ",
                      "2:0.25:0.25 0.5 'ex3 ",
                      "1:0.25:0.25 2:0.25:0.25 4 'ex4 ")
        
    )
    
    # Regular labels
    ref_file <- file(ref_path,"w")
    apply(ref_df, MARGIN = 1, function(x) {
        writeLines(text = paste0(x[["regular_labels"]], x[["features"]]), con = ref_file)
    })
    close(ref_file)
    regular_ref_checksum <- unname(md5sum(ref_path))
    
    regular_df2vw_checksum <- df2vw(data = test_df, file_path = df2vw_path,
                                    namespaces = list(NS1 = c("num_v1", "fact_v2"),
                                                      NS2 = c("fact_v2", "text_v3")),
                                    keep_space = "text_v3",
                                    targets = "regular_label", tag = "tag", weight = "importance")
    regular_df2vw_checksum <- unname(md5sum(df2vw_path))
    
    # CSOAA labels
    ref_file <- file(ref_path,"w")
    apply(ref_df, MARGIN = 1, function(x) {
        writeLines(text = paste0(x[["csoaa_labels"]], x[["features"]]), con = ref_file)
    })
    close(ref_file)
    csoaa_ref_checksum <- unname(md5sum(ref_path))
    
    df2vw(data = test_df, file_path = df2vw_path,
                                  namespaces = list(NS1 = c("num_v1", "fact_v2"),
                                                    NS2 = c("fact_v2", "text_v3")),
                                  keep_space = "text_v3",
                                  targets = c("multilabel_1", "multilabel_2", "multilabel_3", "multilabel_4"),
                                  tag = "tag", weight = "importance")
    csoaa_df2vw_checksum <- unname(md5sum(df2vw_path))
    
    # Context Bandit labels
    ref_file <- file(ref_path,"w")
    apply(ref_df, MARGIN = 1, function(x) {
        writeLines(text = paste0(x[["cb_labels"]], x[["features"]]), con = ref_file)
    })
    close(ref_file)
    cb_ref_checksum <- unname(md5sum(ref_path))
    
    df2vw(data = test_df, file_path = df2vw_path,
                                    namespaces = list(NS1 = c("num_v1", "fact_v2"),
                                                      NS2 = c("fact_v2", "text_v3")),
                                    keep_space = "text_v3",
                                    targets = c("multilabel_1", "multilabel_2", "multilabel_3", "multilabel_4"),
                                    probabilities = c("multilabel_1", "multilabel_2", "multilabel_3", "multilabel_4"),
                                    tag = "tag", weight = "importance")
    cb_df2vw_checksum <- unname(md5sum(df2vw_path))
    
    # CSOAA when not all labels are allowed
    ref_file <- file(ref_path,"w")
    apply(ref_df, MARGIN = 1, function(x) {
        writeLines(text = paste0(x[["na_labels"]], x[["features"]]), con = ref_file)
    })
    close(ref_file)
    na_ref_checksum <- unname(md5sum(ref_path))
    
    df2vw(data = test_df, file_path = df2vw_path,
          namespaces = list(NS1 = c("num_v1", "fact_v2"),
                            NS2 = c("fact_v2", "text_v3")),
          keep_space = "text_v3",
          targets = c("na_multilabel_1", "na_multilabel_2", "na_multilabel_3"),
          probabilities = c("multilabel_1", "multilabel_2", "multilabel_3"),
          tag = "tag", weight = "importance")
    na_df2vw_checksum <- unname(md5sum(df2vw_path))

    
    file.remove(ref_path, df2vw_path)
    
    # Results comparison
    expect_equal(regular_df2vw_checksum, regular_ref_checksum)
    expect_equal(csoaa_df2vw_checksum, csoaa_ref_checksum)
    expect_equal(cb_df2vw_checksum, cb_ref_checksum)
    expect_equal(na_df2vw_checksum, na_ref_checksum)
    
})