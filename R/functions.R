#'Create Vowpal Wabbit model, setup model parameters and data
#'
#'Sets up VW model together with parameters and data
#'
#'@param dir Working directory, default is tempdir()
#'@param train_data Train data file name. File should be in .vw plain text format
#'@param test_data Validation data file name. File should be in .vw plain text format
#'@param model File name for model weights
#'@param eval Compute model evaluation
#'@param learning_mode Learning method or reduction:
#'\itemize{
#'  \item \code{binary}
#'  \item \code{multiclass}
#'  \item \code{lda} - Latent Dirichlet Allocation
#'  \item \code{factorization} - matrix factorization mode
#'  \item \code{bootstrap} - bootstrap with K rounds by online importance resampling
#'  \item \code{ksvm} - online kernel Support Vector Machine
#'  \item \code{nn} - sigmoidal feedforward network
#'  \item \code{boosting} - online boosting with  weak learners
#'}
#'@param algorithm Optimzation algorithm
#'\itemize{
#'  \item \code{sgd} adaptive, normalized, invariant stochastic gradient descent
#'  \item \code{bfgs}
#'  \item \code{ftrl}
#'}
#'@param general_params List of parameters:
#'\itemize{
#'  \item \code{cache} - Create and use cache files
#'  \item \code{passes} - Number of Training Passes
#'  \item \code{bit_precision} - number of bits in the feature table
#'  \item \code{quadratic} - Create and use quadratic features
#'  \item \code{cubic} - Create and use cubic features
#'  \item \code{interactions} - Create feature interactions of any level between namespaces
#'  \item \code{permutations} - Use permutations instead of combinations for feature interactions of same namespace
#'  \item \code{holdout_period} - holdout period for test only
#'  \item \code{early_terminate} - Specify the number of passes tolerated when holdout loss doesn't decrease before early termination
#'  \item \code{sort_features} - Turn this on to disregard order in which features have been defined. This will lead  to smaller cache sizes
#'  \item \code{noconstant} - Don't add a constant feature
#'  \item \code{ngram} - Generate N grams
#'  \item \code{skips} - Generate skips in N grams
#'  \item \code{hash} - How to hash the features. Available options: strings, all
#'  \item \code{affix} - Generate prefixes/suffixes of features
#'  \item \code{random_weights} - Make initial weights random
#'  \item \code{sparse_weights} - Use a sparse datastructure for weights
#'  \item \code{initial_weight} - Set all weights to initial value
#'}
#'@param optimization_params List of parameters:
#'\itemize{
#'  \item \code{hessian_on} - Use second derivative in line search
#'  \item \code{initial_pass_length} - Initial number of examples per pass
#'  \item \code{l1} - L1 regularization
#'  \item \code{l2} - L2 regularization
#'  \item \code{decay_learning_rate} - Set Decay factor for learning_rate between passes
#'  \item \code{initial_t} - initial t value
#'  \item \code{power_t} - t power value
#'  \item \code{learning_rate} - Set initial learning Rate
#'  \item \code{loss_function} - Specify the loss function to be used
#'  \item \code{quantile_tau} - Parameter Tau associated with Quantile loss
#'}
#'Additional parameters depending on \code{algorithm} choice:
#'\itemize{
#'  \item \code{sgd}: 
#'    \itemize{
#'      \item \code{adaptive} - Use adaptive, individual learning rates (on by default)
#'      \item \code{normalized} - Use per feature normalized updates (on by default)
#'      \item \code{invariant} - Use safe/importance aware updates (on by default)
#'    }
#'  \item \code{bfgs}: 
#'    \itemize{
#'      \item \code{conjugate_gradient} - Use conjugate gradient based optimization
#'    }
#'  \item \code{ftrl}: 
#'    \itemize{
#'      \item \code{ftrl_alpha}
#'      \item \code{ftrl_beta}
#'    }
#'}
#'@param learning_params List of parametrs associated with learning_mode
#'\itemize{
#'  \item \code{binary}: 
#'    \itemize{
#'      \item \code{binary} - Reports loss as binary classification with -1,1 labels
#'    }
#'  \item \code{multiclass}:
#'    \itemize{ 
#'      \item \code{reduction} - csoaa, oaa, ect, wap, csoaa_ldf multiclass learning
#'      \item \code{num_classes} - Number of classes
#'    }
#'  \item \code{lda}:
#'    \itemize{ 
#'      \item \code{num_topics} - Number of topics
#'      \item \code{lda_alpha} - Prior on sparsity of per-document topic weights
#'      \item \code{lda_rho} - Prior on sparsity of topic distributions
#'      \item \code{lda_D} - Number of documents
#'      \item \code{lda_epsilon} - Loop convergence threshold
#'      \item \code{math_mode} - Math mode: simd, accuracy, fast-approx
#'      \item \code{minibatch} - Minibatch size
#'    }
#'  \item \code{factorization}:
#'    \itemize{
#'    \item \code{rank} - rank for matrix factorization
#'    }
#'  \item \code{bootstrap}:
#'    \itemize{
#'      \item \code{rounds} - number of rounds
#'      \item \code{bs_type} - the bootstrap mode: 'mean' or 'vote'
#'    }
#'  \item \code{nn}:
#'    \itemize{
#'      \item \code{hidden} - number of hidden units
#'      \item \code{inpass} - Train or test sigmoidal feedforward network with input passthrough
#'      \item \code{multitask} - Share hidden layer across all reduced tasks
#'      \item \code{dropout} - Train or test sigmoidal feedforward network using dropout.
#'      \item \code{meanfield} - Train or test sigmoidal feedforward network using mean field.
#'    }
#'  \item \code{boosting}:
#'    \itemize{
#'      \item \code{num_learners} - number of weak learners
#'    }
#'}
#'@return vwmodel list class 
#'@examples
#'vwsetup(
#'  dir = tempdir(),
#'  train_data = "X_train.vw",
#'  test_data = "X_valid.vw",
#'  model = "pk_mdl.vw",
#'  general_params = list(cache = TRUE, passes=10),
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
                    eval = FALSE
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
                 optimization_params = optimization_params
  )
  # Parse parameters and write them to string
  # Check parameters
  params <- .check_parameters(params)
  # Write to string
  params_str <- .create_parameters_string(params)
  # Create cache
  if (params$general_params$cache) {
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

vwparams <- function(vwmodel, name) {
    key_string <- grep(pattern = paste0("\\b", name, "\\b"), x = names(unlist(vwmodel$params)), value = T)
    if(length(key_string) > 0) {
        key_params <- unlist(strsplit(key_string, split = ".", fixed = TRUE))
        if(is.list(vwmodel$params[[key_params[1]]])) {
            return(vwmodel$params[[key_params[1]]][[key_params[2]]])
        } else {
            return(vwmodel$params[[key_params[1]]])
        }
    } else {
        stop("Wrong parameter name")
    }
}

`vwparams<-` <- function(vwmodel, name, value) {
    key_string <- grep(pattern = paste0("\\b", name, "\\b"), x = names(unlist(vwmodel$params)), value = T)
    if(length(key_string) > 0) {
        key_params <- unlist(strsplit(key_string, split = ".", fixed = TRUE))
        if(is.list(vwmodel$params[[key_params[1]]])) {
            vwmodel$params[[key_params[1]]][[key_params[2]]] <- value
        } else {
            vwmodel$params[[key_params[1]]] <- value
        }
        vwmodel$params <- .check_parameters(vwmodel$params)
        vwmodel$params_str <- .create_parameters_string(vwmodel$params)
        return(vwmodel)
    } else {
        stop("Wrong parameter name")
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
  general_check <- list(cache=FALSE,
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
          input = params$general_params,
          # input = c(list(cache=params$cache), params$general_params),
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
  
  # Cache should be created, if passes > 1
  if(params$general_params$passes > 1) {
      params$general_params$cache <- TRUE
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
    # Disable cache here, because it's checked in vwtrain and vwtest
    if (temp_params$general_params$cache) {
        temp_params$general_params$cache <- NA
    }
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


