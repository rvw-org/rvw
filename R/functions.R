#'Create Vowpal Wabbit model, setup model parameters and data
#'
#'@param dir Working directory, default is tempdir()
#'@param train_data Train data file name. File should be in .vw plain text format
#'@param test_data Validation data file name. File should be in .vw plain text format
#'@param model File name for model weights
#'@param cache Create cache files
#'@param eval Compute model evaluation
#'@param learning_mode Learning method or reduction:
#'binary
#'multiclass
#'lda - Latent Dirichlet Allocation
#'factorization - matrix factorization mode
#'bootstrap - bootstrap with K rounds by online importance resampling
#'ksvm - online kernel Support Vector Machine
#'nn - sigmoidal feedforward network
#'boosting - online boosting with  weak learners
#'@param algorithm Optimzation algorithm
#'@param general_params List of parameters:
#'cache - Use a cache
#'passes - Number of Training Passes
#'bit_precision - number of bits in the feature table
#'quadratic - Create and use quadratic features
#'cubic - Create and use cubic features
#'interactions - Create feature interactions of any level between namespaces
#'permutations - Use permutations instead of combinations for feature interactions of same namespace
#'holdout_period - holdout period for test only
#'early_terminate - Specify the number of passes tolerated when holdout loss doesn't decrease before early termination
#'sort_features - Turn this on to disregard order in which features have been defined. This will lead  to smaller cache sizes
#'noconstant - Don't add a constant feature
#'ngram - Generate N grams
#'skips - Generate skips in N grams
#'hash - How to hash the features. Available options: strings, all
#'affix - Generate prefixes/suffixes of features
#'random_weights - Make initial weights random
#'sparse_weights - Use a sparse datastructure for weights
#'initial_weight - Set all weights to initial value
#'@param optimization_params List of parameters:
#'hessian_on - Use second derivative in line search
#'initial_pass_length - Initial number of examples per pass
#'l1 - L1 regularization
#'l2 - L2 regularization
#'decay_learning_rate - Set Decay factor for learning_rate between passes
#'initial_t - initial t value
#'power_t - t power value
#'learning_rate - Set initial learning Rate
#'loss_function - Specify the loss function to be used
#'quantile_tau - Parameter Tau associated with Quantile loss
#'@param learning_params List of parametrs associated with learning_mode
#'binary: 
#'    binary - Reports loss as binary classification with -1,1 labels
#'multiclass: 
#'    reduction - csoaa, oaa, ect, wap, csoaa_ldf multiclass learning
#'    num_classes - Number of classes
#'lda: 
#'    num_topics - Number of topics
#'    lda_alpha - Prior on sparsity of per-document topic weights
#'    lda_rho - Prior on sparsity of topic distributions
#'    lda_D - Number of documents
#'    lda_epsilon - Loop convergence threshold
#'    math_mode - Math mode: simd, accuracy, fast-approx
#'    minibatch - Minibatch size
#'factorization:
#'    rank - rank for matrix factorization
#'bootstrap:
#'    rounds - number of rounds
#'    bs_type - the bootstrap mode: 'mean' or 'vote'
#'nn:
#'    hidden - number of hidden units
#'    inpass - Train or test sigmoidal feedforward network with input passthrough
#'    multitask - Share hidden layer across all reduced tasks
#'    dropout - Train or test sigmoidal feedforward network using dropout.
#'    meanfield - Train or test sigmoidal feedforward network using mean field.
#'boosting:
#'    num_learners - number of weak learners
#'    num_learners - number of weak learners
#'    num_learners - number of weak learners
#'    num_learners - number of weak learners
#'@return vwmodel list class 
#'@examples
#'vwsetup(
#'  dir = "../my_tmp/",
#'  train_data = "X_train.vw",
#'  test_data = "X_valid.vw",
#'  model = "pk_mdl.vw",
#'  cache = TRUE,
#'  general_params = list(passes=10),
#'  optimization_params = list(adaptive=FALSE),
#'  learning_params = list(binary=TRUE)
#')
#'
vwsetup <- function(learning_mode = c("binary", "multiclass", "lda", "factorization", "bootstrap", "nn"),
                    algorithm = c("sgd", "bfgs", "ftrl"),
                    general_params = list(),
                    optimization_params = list(),
                    learning_params = list(),
                    dir = tempdir(),
                    train_data = "",
                    test_data = "",
                    model = "mdl.vw",
                    eval = FALSE,
                    cache = FALSE
) {
  train_cache = ""
  test_cache = ""
  eval_results = ""
  if(substr(dir, nchar(dir), nchar(dir)) != "/") {
    dir <- paste0(dir, "/")
  }
  
  learning_mode <- match.arg(learning_mode)
  algorithm <- match.arg(algorithm)
  
  params <- list(learning_mode = learning_mode,
                 algorithm = algorithm,
                 general_params = general_params,
                 learning_params = learning_params,
                 optimization_params = optimization_params,
                 cache = cache
  )
  # Parse parameters and write them to string
  # Check parameters
  params <- .check_parameters(params)
  # Write to string
  params_str <- .create_parameters_string(params)
  # Create cache
  if (cache) {
    # If have data, create cache
    if (nchar(train_data) != 0) {
      train_cache <- paste0(train_data, ".cache")
      .create_cache(dir=dir, data_file=train_data, cache_file=train_cache)
    }
    if (nchar(test_data) != 0) {
      test_cache <- paste0(test_data, ".cache")
      .create_cache(dir=dir, data_file=test_data, cache_file=test_cache)
    }
  }
  if(eval) {
    eval_results = "palaceholder for eveluation results"
  }
  vwmodel <- list(params = params,
                  dir = dir,
                  model = model,
                  params_str = params_str,
                  data = list(train = train_data,
                               test = test_data),
                  cache = list(train = train_cache,
                              test = test_cache),
                  eval = eval_results)
  class(vwmodel) <- "vw"
  return(vwmodel)
}

#'Print VW model
#'
#'@description Print information about Vowpal Wabbit model
#'@param x Model of vw class
#'@param ... Not used currently
#'@examples 
#'vwmodel <- vwsetup()
#'print(vwmodel)
#'
print.vw <- function(x, ...) {
  cat("\tVowpal Wabbit model\n")
  cat("Learning mode:  ", x$params$learning_mode, "\n")
  cat("Learning algorithm:  ", x$params$algorithm, "\n")
  cat("Working directory:  ", x$dir, "\n")
  cat("General parameters:", "\n")
  sapply(names(x$params$general_params), FUN = function(i) {
    if(x$params$general_params[[i]] == "") {
      cat("\t", i, ":  Not defined\n")
    } else {
      cat("\t", i, ":  ", x$params$general_params[[i]], "\n")
    }
  })
  cat("Learning parameters:", "\n")
  sapply(names(x$params$learning_params), FUN = function(i) {
    if(x$params$learning_params[[i]] == "") {
      cat("\t", i, ":  Not defined\n")
    } else {
      cat("\t", i, ":  ", x$params$learning_params[[i]], "\n")
    }
  })
  cat("Optimization parameters:", "\n")
  sapply(names(x$params$optimization_params), FUN = function(i) {
    if(x$params$optimization_params[[i]] == "") {
      cat("\t", i, ":  Not defined\n")
    } else {
      cat("\t", i, ":  ", x$params$optimization_params[[i]], "\n")
    }
  })
  cat("Data:", "\n")
  cat("\tTrain data file path:  ", x$data$train, "\n")
  cat("\tTest data file path:  ", x$data$test, "\n")
  if (x$params$general_params$cache) {
    cat("Cache:", "\n")
    cat("\tTrain data file path:  ", x$cache$train, "\n")
    cat("\tTest data file path:  ", x$cache$test, "\n")
  }
  }

# Helper functions
.check_parameters <- function(params) {
  # Helper function to check parameters
  check_param_values <- function(input, check) {
    if(!all(names(input) %in% names(check))) {
      stop("Wrong argument names!")
    }
    
    valid_input <- check
    if(!all(sapply(names(input), FUN = function(i) {
      # First check if types of input argument values are correct (same as of check lists)
      bool_check <- (typeof(input[[i]]) == typeof(check[[i]])) | (is.na(input[[i]]))
      # Replace default/check values with values from input
      valid_input[[i]] <<- input[[i]]
      # And return bool values to raise errors
      bool_check
    }))) {
      stop("Wrong argument values!")
    }
    
    # Return check with modified values
    return(valid_input)
  }
  # Initialise default/check lists 
  general_check <- list(cache=params$cache,
                        passes=1,
                        bit_precision=18,
                        quadratic=FALSE,
                        cubic=FALSE,
                        interactions="",
                        permutations=FALSE,
                        holdout_period=10,
                        early_terminate=3,
                        sort_features=FALSE,
                        noconstant=FALSE,
                        ngram="",
                        skips="",
                        hash="",
                        affix="",
                        random_weights=FALSE,
                        sparse_weights=FALSE,
                        initial_weight=0)
  # Learning parameters/reductions default/check lists
  binary_check <- list(binary=FALSE)
  multiclass_check <- list(reduction="csoaa",
                           num_classes="3")
  lda_check <- list(num_topics=0,
                    lda_alpha=0.100000001,
                    lda_rho=0.100000001,
                    lda_D=10000,
                    lda_epsilon=0.00100000005,
                    math_mode=0,
                    minibatch=1)
  factorization_check <- list(rank=0)
  bootstrap_check <- list(rounds="",
                          bs_type="mean")
  ksvm_check <- list()
  nn_check <- list(hidden=3,
                   inpass=FALSE,
                   multitask=FALSE,
                   dropout=FALSE,
                   meanfield=FALSE)
  boosting_check <- list(num_learners=5)
  # Learning algorithm default/check lists
  sgd_check <- list(adaptive=TRUE,
                    normalized=TRUE,
                    invariant=TRUE)
  bfgs_check <- list(conjugate_gradient=FALSE)
  ftrl_check <- list(ftrl_alpha=0.005,
                     ftrl_beta=0.1)
  optimization_check <- list(hessian_on=FALSE,
                             initial_pass_length="",
                             l1=0,
                             l2=0,
                             decay_learning_rate=1,
                             initial_t=0,
                             power_t=0.5,
                             learning_rate=0.5,
                             loss_function="squared",
                             quantile_tau=0.5)
  
  # Create default parameters list if no parameters provided
  # Else check parameters and return validated parameters
  if(length(params$learning_params) == 0) {
    params$learning_params <- get(paste0(params$learning_mode, "_check"))
  } else {
    learning_check_type <- get(paste0(params$learning_mode, "_check"))
    params$learning_params <- check_param_values(
      input = params$learning_params,
      check = learning_check_type
    )
  }
  if(length(params$general_params) == 0) {
    params$general_params <- general_check
  } else {
    params$general_params <- check_param_values(
      input = c(list(cache=params$cache), params$general_params),
      check = general_check
    )
  }
  if(length(params$optimization_params) == 0) {
    algorithm_parameters <- get(paste0(params$algorithm, "_check"))
    params$optimization_params <- c(algorithm_parameters, optimization_check)
  } else {
    algorithm_check_type <- get(paste0(params$algorithm, "_check"))
    params$optimization_params <- check_param_values(
      input = params$optimization_params,
      check = c(algorithm_check_type, optimization_check)
    )
  }
  
  # Return validated parameters
  return(list(learning_mode = params$learning_mode,
              algorithm = params$algorithm,
              general_params = params$general_params,
              learning_params = params$learning_params,
              optimization_params = params$optimization_params))
}

.create_parameters_string <- function(params) {
  flatten <- function(x) {
    repeat {
      if(!any(vapply(x,is.list, logical(1)))) return(x)
      x <- Reduce(c, x)
    }
  }
  temp_params <- params
  #Set learning mode string argument
  mode_string <- switch (temp_params$learning_mode,
    multiclass = {
      tmp <- paste0("--", temp_params$learning_params$reduction, " ", temp_params$learning_params$num_classes); 
      temp_params$learning_params$reduction <- NA; temp_params$learning_params$num_classes <- NA; tmp
    },
    lda = {tmp <- paste0("--lda ", temp_params$learning_params$num_topics); temp_params$learning_params$num_topics <- NA; tmp},
    factorization = {tmp <- paste0("--rank ", temp_params$learning_params$rank); temp_params$learning_params$rank <- NA; tmp},
    bootstrap = {tmp <- paste0("--bootstrap ", temp_params$learning_params$rounds); temp_params$learning_params$rounds <- NA; tmp},
    nn = {tmp <- paste0("--nn ", temp_params$learning_params$hidden); temp_params$learning_params$hidden <- NA; tmp},
    boosting = {
      tmp <- paste0("--boosting ", temp_params$learning_params$num_learners);
      temp_params$learning_params$num_learners <- NA; tmp
    },
    ksvm = "--ksvm"
  )
  #Set learning mode string argument
  algorithm_string <- switch (temp_params$algorithm,
                              sgd = {tmp <- ""; tmp},
                              bfgs = {tmp <- "--bfgs"; tmp},
                              ftrl = {tmp <- "--ftrl"; tmp}
  )
  # Flatten list
  temp_params <- flatten(temp_params[-c(1,2)])
  # Convert parameters list to "--arg _" list
  temp_params <- sapply(names(temp_params), FUN = function(i) {
    if(is.na(temp_params[i]) | temp_params[i] == "") {
      return("")
    };
    if(is.logical(temp_params[i][[1]]) & temp_params[i][[1]] == TRUE) {
      return(paste0("--",i))
    }; 
    if(is.logical(temp_params[i][[1]]) & temp_params[i][[1]] == FALSE) {
      return("")
    } else {
      return(paste0("--",i," ",temp_params[i]))
    }
  })
  # Filter empty strings
  temp_params <- Filter(temp_params, f = function(x) nchar(x) > 0)
  # Create string "--passes 0 --bit_precision 18" for parser
  parameters_string <- paste0(temp_params, collapse = " ")
  parameters_string <- paste(mode_string, algorithm_string, parameters_string, sep = " ")
  
  return(parameters_string)
}


  