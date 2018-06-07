

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
                     cache = TRUE
) {
  train_cache = ""
  test_cache = ""
  eval_results = ""
  
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
  if (cache) {
    # If have data, create cache
    if (!is.null(train_data)) {
      train_cache <- .create_cache(train_data, dir)
    }
    if (!is.null(test_data)) {
      test_cache <- .create_cache(test_data, dir)
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
                  eval = eval_results)
  class(vwmodel) <- "vw"
  return(vwmodel)
}


print.vw <- function(vwmodel) {
  cat("\tVowpal Wabbit model\n")
  cat("Learning mode:  ", vwmodel$params$learning_mode, "\n")
  cat("Learning algorithm:  ", vwmodel$params$algorithm, "\n")
  cat("Working directory:  ", vwmodel$dir, "\n")
  cat("General parameters:", "\n")
  sapply(names(vwmodel$params$general_params), FUN = function(i) cat("\t", i, ":  ", vwmodel$params$general_params[[i]], "\n"))
  cat("Learning parameters:", "\n")
  sapply(names(vwmodel$params$learning_params), FUN = function(i) cat("\t", i, ":  ", vwmodel$params$learning_params[[i]], "\n"))
  cat("Optimization parameters:", "\n")
  sapply(names(vwmodel$params$optimization_params), FUN = function(i) cat("\t", i, ":  ", vwmodel$params$optimization_params[[i]], "\n"))
  cat("Data:", "\n")
  cat("\tTrain data file path:  ", vwmodel$data$train, "\n")
  cat("\tTest data file path:  ", vwmodel$data$test, "\n")
  }

# Helper functions
.check_parameters <- function(params) {
  # Helper function to check parameters
  check_param_values <- function(input, check) {
    if(!all(sort(names(input)) == sort(names(check)))) {
      stop("Wrong learning parameters!")
    }
    
    if(!all(sapply(names(input), FUN = function(i) {
      (typeof(input[[i]]) == check[[i]]) | (is.na(input[[i]]))
    }))) {
      stop("Wrong learning parameters!")
    }
  }
  # Initialise check lists
  general_check <- list(passes="double",
                        bit_precision="double",
                        qudratic="logical",
                        cubic="logical",
                        interactions="character",
                        permutations="logical",
                        holdout_period="double",
                        early_terminate="double",
                        sort_features="logical",
                        noconstant="logical",
                        ngram="double",
                        skips="double",
                        hash="character",
                        affix="character",
                        random_weights="logical",
                        sparse_weights="logical",
                        initial_weight="double")
  binary_check <- list(binary="logical")
  multiclass_check <- list(reduction="character",
                           num_classes="double")
  lda_check <- list(num_topics="double",
                    lda_alpha="double",
                    lda_rho="double",
                    lda_D="double",
                    lda_epsilon="double",
                    math_mode="double",
                    minibatch="double")
  factorization_check <- list(rank="double")
  bootstrap_check <- list(rounds="double",
                          type="character")
  nn_check <- list(hidden="double",
                   inpass="logical",
                   multitask="logical",
                   dropout="logical",
                   meanfield="logical")
  # Learning algorithm check
  sgd_check <- list(adaptive="logical",
           normalized="logical",
           invariant="logical")
  bfgs_check <- list(conjugate_gradient="logical")
  ftrl_check <- list(ftrl_alpha="double",
            ftrl_beta="double")
  optimization_check <- list(
                             # ftrl_alpha="double",
                             # ftrl_beta="double",
                             # mem="double",
                             # termination="double",
                             hessian="logical",
                             initial_pass_length="double",
                             l1="double",
                             l2="double",
                             decay_learning_rate="double",
                             initial_t="double",
                             power_t="double",
                             learning_rate="double",
                             loss_function="character",
                             quantile_tau="double")
  
  
  # Create default parameters list if no parameters provided
  if(length(params$learning_params) == 0) {
    params$learning_params <- switch(params$learning_mode,
                              binary=list(binary=FALSE),
                              multiclass=list(reduction="csoaa",
                                              num_classes=3),
                              lda=list(num_topics=0,
                                       lda_alpha=0.100000001,
                                       lda_rho=0.100000001,
                                       lda_D=10000,
                                       lda_epsilon=0.00100000005,
                                       math_mode=0,
                                       minibatch=1),
                              factorization=list(rank=0),
                              bootstrap=list(rounds=NA,
                                             type="mean"),
                              nn=list(hidden=3,
                                      inpass=FALSE,
                                      multitask=FALSE,
                                      dropout=FALSE,
                                      meanfield=FALSE)
    )
  }
  if(length(params$general_params) == 0) {
    params$general_params <- list(passes=1,
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
                           initial_weight=0)
  }
  # Create empty list in case user provided anything in optimization_params
  algorithm_parameters <- list()
  if(length(params$optimization_params) == 0) {
    params$optimization_params <- list(
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
    algorithm_parameters <- switch(params$algorithm,
                                   sgd=list(adaptive=TRUE,
                                            normalized=TRUE,
                                            invariant=TRUE),
                                   bfgs=list(conjugate_gradient=FALSE),
                                   ftrl=list(ftrl_alpha=0.005,
                                             ftrl_beta=0.1)
    )
  }
  
  # Check general parameters
  check_param_values(input = params$general_params, check = general_check)
  # Check learning parameters
  switch(params$learning_mode,
         binary=check_param_values(input = params$learning_params, check = binary_check),
         multiclass=check_param_values(input = params$learning_params, check = multiclass_check),
         lda=check_param_values(input = params$learning_params, check = lda_check),
         factorization=check_param_values(input = params$learning_params, check = factorization_check),
         bootstrap=check_param_values(input = params$learning_params, check = bootstrap_check),
         nn=check_param_values(input = params$learning_params, check = nn_check)
  )
  # Check learning algorithm parameters and optimization parameters
  params$optimization_params <- c(algorithm_parameters, params$optimization_params)
  switch(params$algorithm,
         sgd=check_param_values(input =  params$optimization_params, check = c(sgd_check, optimization_check)),
         bfgs=check_param_values(input =  params$optimization_params, check = c(bfgs_check, optimization_check)),
         ftrl=check_param_values(input =  params$optimization_params, check = c(ftrl_check, optimization_check))
  )
  
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
    binary = {tmp <- "--binary"; tmp},
    multiclass = {tmp <- paste0("--", temp_params$learning_params$reduction, " ", temp_params$learning_params$num_classes); 
      temp_params$learning_params$reduction <- NA; temp_params$learning_params$num_classes <- NA; tmp},
    lda = {tmp <- paste0("--lda ", temp_params$learning_params$num_topics); temp_params$learning_params$num_topics <- NA; tmp},
    factorization = {tmp <- paste0("--rank ", temp_params$learning_params$rank); temp_params$learning_params$rank <- NA; tmp},
    bootstrap = {tmp <- paste0("--bootstrap ", temp_params$learning_params$rounds); temp_params$learning_params$rounds <- NA; tmp},
    nn = {tmp <- paste0("--nn ", temp_params$learning_params$hidden); temp_params$learning_params$hidden <- NA; tmp}
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


  