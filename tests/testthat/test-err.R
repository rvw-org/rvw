context("vw error messages")
library(rvw)

fake_vwmodel <- list(params = list(algorithm = "sgd",
                                   general_params = list(),
                                   feature_params = list(),
                                   optimization_params = list(),
                                   options = list()
                                   
),
dir = "../my_tmp",
model = "mdl.vw",
params_str = paste0(""),
is_cl = FALSE,
data_md5sum = list(train = "",
                   test = ""),
train_file = "",
eval = list(
    train=list(),
    test=list()
),
parser_opts=NA
)


test_that(".check_parameters raises correct errors", {
    # Wrong argument names
    
    # General params
    expect_error(
        vwsetup(general_params = list(wrong_param_1=10, ting_size=10)),
        "Wrong argument names: wrong_param_1, ting_size",
        fixed = T
    )
    # Feature params
    expect_error(
        vwsetup(feature_params = list(bit_precision=25, featurelimit=100)),
        "Wrong argument names: featurelimit",
        fixed = T
    )
    # Optimization params
    expect_error(
        vwsetup(optimization_params = list(initial_p=0.1, l1=1E-7)),
        "Wrong argument names: initial_p",
        fixed = T
    )
    # Option
    expect_error(
        vwsetup(option = "nn", num_hidden = 10, inpas = TRUE),
        "Wrong argument names: inpas",
        fixed = T
    )
    
    
    # Wrong argument values 
    
    # General params
    expect_error(
        vwsetup(general_params = list(random_seed="10", ring_size=10)),
        "Wrong argument values: random_seed",
        fixed = T
    )
    # Feature params
    # This test should be changed in future, because we want to accept both real and integer numbers
    expect_error(
        vwsetup(feature_params = list(bit_precision=25L, noconstant="foo")),
        "Wrong argument values: bit_precision, noconstant",
        fixed = T
    )
    # Optimization params
    expect_error(
        vwsetup(optimization_params = list(no_bias_regularization="on", feature_mask=1E-7)),
        "Wrong argument values: feature_mask",
        fixed = T
    )
    # Option
    expect_error(
        vwsetup(option = "nn", num_hidden = "10", inpass = "TRUE"),
        "Wrong argument values: num_hidden, inpass",
        fixed = T
    )
    
    
    # Missing first argument value in option parameters
    expect_error(
        vwsetup(option = "nn"),
        "Missing value for argument: num_hidden",
        fixed = T
    )
})

test_that("vwsetup raises correct errors", {
    
    # Whitespace characters in dir path
    expect_error(
        vwsetup(dir = "./some folder/"),
        "Whitespace characters are not allowed in `dir` path",
        fixed = T
    )
    
    # Whitespace characters in model path
    expect_error(
        vwsetup(model = "./some folder/mdl.vw"),
        "Whitespace characters are not allowed in `model` path",
        fixed = T
    )
    
    # Forbidden flags in cmd line parameters
    expect_error(
        vwsetup(params_str = "--passes 10"),
        "Following cmd line parameters are defined in other functions:",
        fixed = T
    )
})

test_that("add_option raises correct errors", {
    
    # vwmodel should be of class vw
    expect_error(
        add_option(fake_vwmodel, option = "nn", num_hidden = 10),
        "vwmodel should be of class vw",
        fixed = T
    )
    
    # add_option can't be used with direct cmd line parameters
    test_vwmodel <- vwsetup(params_str = "--bit_precision 25")
    
    expect_error(
        add_option(test_vwmodel, option = "nn", num_hidden = 10),
        "add_option can't be used when cmd line parameters are used",
        fixed = T
    )
    
    # Overwrite option
    test_vwmodel <- vwsetup(option = "nn", num_hidden = 5)
    
    expect_error(
        add_option(test_vwmodel, option = "nn", num_hidden = 10),
        "Trying to overwrite option",
        fixed = T
    )
})

test_that("vwparams raises correct errors", {
    
    # vwmodel should be of class vw
    expect_error(
        vwparams(fake_vwmodel, name = "bit_precision"),
        "vwmodel should be of class vw",
        fixed = T
    )
    
    # add_option can't be used with direct cmd line parameters
    test_vwmodel <- vwsetup(params_str = "--bit_precision 25")
    
    expect_error(
        vwparams(test_vwmodel, name = "bit_precision"),
        "vwparams can't be used when cmd line parameters are used",
        fixed = T
    )
})
