# source("R/rhelpers.R")

#'Create Vowpal Wabbit model, setup model parameters and data
#'
#'Sets up VW model together with parameters and data
#'
#'@param dir Working directory, default is tempdir()
#'@param model File name for model weights
#'@param eval Compute model evaluation
#'@param reduction Add reduction: 
#'\itemize{
#'  \item \code{binary} - Reports loss as binary classification with -1,1 labels
#'  \item \code{oaa} - One-against-all multiclass learning with  labels
#'  \item \code{ect} - Error correcting tournament with  labels
#'  \item \code{csoaa} - One-against-all multiclass learning with  costs
#'  \item \code{wap} - Weighted all-pairs multiclass learning with  costs
#'  \item \code{log_multi} - Online (decision) trees for  classes
#'  \item \code{lda} - Latent Dirichlet Allocation
#'  \item \code{mf} - Matrix factorization mode
#'  \item \code{lrq} - Low rank quadratic features
#'  \item \code{stage_poly} - Stagewise polynomial features
#'  \item \code{bootstrap} - bootstrap with K rounds by online importance resampling
#'  \item \code{autolink} - Create link function with polynomial N
#'  \item \code{cb} - Contextual bandit learning with K costs
#'  \item \code{cbify} - Convert multiclass on K classes into a contextual bandit problem
#'  \item \code{nn} - Sigmoidal feedforward network
#'  \item \code{topk} - Top K recommendation
#'  \item \code{struct_search} - Search-based structured prediction (SEARN or DAgger)
#'  \item \code{boosting} - online boosting with weak learners
#'}
#'@param algorithm Optimzation algorithm
#'\itemize{
#'  \item \code{sgd} adaptive, normalized, invariant stochastic gradient descent
#'  \item \code{bfgs}
#'  \item \code{ftrl}
#'  \item \code{ksvm}
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
#'  \item \code{link} - Convert output predictions using specified link function. Options: identity, logistic, glf1 or poisson.
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
#'@param ... Options for reduction
#'\itemize{
#'  \item \code{oaa} or \code{ect} or \code{log_multi}:
#'    \itemize{ 
#'      \item \code{num_classes} - Number of classes
#'    }
#'  \item \code{csoaa} or \code{wap}:
#'    \itemize{ 
#'      \item \code{num_classes} - Number of classes
#'      \item \code{csoaa_ldf} or \code{wap_ldf} - \code{singleline} (Default) or \code{multiline} label dependent features
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
#'  \item \code{mf}:
#'    \itemize{
#'      \item \code{rank} - rank for matrix factorization
#'    }
#'  \item \code{lrq}:
#'    \itemize{
#'      \item \code{features} - low rank quadratic features
#'      \item \code{lrqdropout} - use dropout training for low rank quadratic features
#'    }
#'  \item \code{stage_poly}:
#'    \itemize{
#'      \item \code{sched_exponent} - exponent controlling quantity of included features
#'      \item \code{batch_sz} - multiplier on batch size before including more features
#'      \item \code{batch_sz_no_doubling} - batch_sz does not double
#'    }
#'  \item \code{bootstrap}:
#'    \itemize{
#'      \item \code{rounds} - number of rounds
#'      \item \code{bs_type} - the bootstrap mode: 'mean' or 'vote'
#'    }
#'  \item \code{autolink}:
#'    \itemize{
#'      \item \code{degree} - polynomial degree
#'    }
#'  \item \code{cb}:
#'    \itemize{
#'      \item \code{costs} - number of costs
#'    }
#'  \item \code{cbify}:
#'    \itemize{
#'      \item \code{num_classes} - number of classes
#'    }
#'  \item \code{nn}:
#'    \itemize{
#'      \item \code{hidden} - number of hidden units
#'      \item \code{inpass} - Train or test sigmoidal feedforward network with input passthrough
#'      \item \code{multitask} - Share hidden layer across all reduced tasks
#'      \item \code{dropout} - Train or test sigmoidal feedforward network using dropout.
#'      \item \code{meanfield} - Train or test sigmoidal feedforward network using mean field.
#'    }
#'  \item \code{topk}:
#'    \itemize{
#'      \item \code{k} - number of top k recomendations
#'    }
#'  \item \code{struct_search}:
#'    \itemize{
#'      \item \code{id} - maximum action id or 0 for LDF
#'    }
#'  \item \code{boosting}:
#'    \itemize{
#'      \item \code{num_learners} - number of weak learners
#'    }
#'}
#'@return vwmodel list class 
#'@import tools 
#'@examples
#'vwsetup(
#'  dir = tempdir(),
#'  model = "pk_mdl.vw",
#'  general_params = list(cache = TRUE, passes=10),
#'  optimization_params = list(adaptive=FALSE),
#'  reduction = "binary"
#')
#'
vwsetup <- function(algorithm = c("sgd", "bfgs", "ftrl", "ksvm"),
                    general_params = list(),
                    optimization_params = list(),
                    dir = tempdir(),
                    model = "mdl.vw",
                    eval = FALSE,
                    reduction = c("", "binary", "oaa", "ect", "csoaa", "wap", "log_multi",
                                  "lda", "mf", "lrq", "stage_poly", "bootstrap",
                                  "autolink", "cb", "cbify", "nn", "topk",
                                  "struct_search", "boosting"),
                    ...
) {
    
  # library(tools)
  train_md5sum = ""
  test_md5sum = ""
  eval_results = ""
  if(substr(dir, nchar(dir), nchar(dir)) != "/") {
    dir <- paste0(dir, "/")
  }
  
  # Delete model files from previous setups
  if (file.exists(paste0(dir, model))) {
      file.remove(paste0(dir, model))
  }
  
  algorithm <- match.arg(algorithm)
  reduction <- match.arg(reduction)
  
  # Setreductions
  if(reduction == "") {
      reductions = list()
  } else {
      reductions <- setNames(list(list(...)), reduction)
  }

  
  
  params <- list(
      algorithm = algorithm,
      general_params = general_params,
      optimization_params = optimization_params,
      reductions = reductions
      # input_mode = input_mode
  )
  # Parse parameters and write them to string
  # Check parameters
  params <- .check_parameters(params)
  # Write to string
  params_str <- .create_parameters_string(params)
  # Create cache
  # if (params$general_params$cache) {
  #   # If have data, create cache
  #   if (nchar(train_data) != 0) {
  #     train_cache <- paste0(train_data, ".cache")
  #     .create_cache(dir=dir, data_file=train_data, cache_file=train_cache)
  #   }
  #   if (nchar(test_data) != 0) {
  #     test_cache <- paste0(test_data, ".cache")
  #     .create_cache(dir=dir, data_file=test_data, cache_file=test_cache)
  #   }
  # }
  if(eval) {
    eval_results = "palaceholder for eveluation results"
  }
  
  vwmodel <- list(params = params,
                  dir = dir,
                  model = model,
                  params_str = params_str,
                  data_md5sum = list(train = train_md5sum,
                               test = test_md5sum),
                  eval = eval_results)
  class(vwmodel) <- "vw"
  return(vwmodel)
}

#'Add reduction to the model
#'
#'@description Add reduction to the reduction stack inside model
#'@param vwmodel Model of vw class
#'@param reduction Name of reduction
#'@param ... Reduction options
add_reduction <- function(vwmodel, reduction = c("binary", "oaa", "ect", "csoaa", "wap", "log_multi",
                                "lda", "mf", "lrq", "stage_poly", "bootstrap",
                                "autolink", "cb", "cbify", "nn", "topk",
                                "struct_search", "boosting"), ...) {
    
    reduction <- match.arg(reduction)
    
    if (reduction %in% names(vwmodel$params$reductions)) {
        stop("Trying to overwrite reduction")
    }
    
    new_reduction <- setNames(list(list(...)), reduction)
    vwmodel$params$reductions <- c(vwmodel$params$reductions, new_reduction)
    vwmodel$params <- .check_parameters(vwmodel$params)
    vwmodel$params_str <- .create_parameters_string(vwmodel$params)
    vwmodel
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
  cat("Reductions:", "\n")
  sapply(names(x$params$reductions), function(reduction_name) {
      cat("\t", reduction_name, ":\n")
      sapply(names(x$params$reductions[[reduction_name]]), FUN = function(i) {
          if(x$params$reductions[[reduction_name]][[i]] == "") {
              cat("\t\t", i, ":  Not defined\n")
          } else {
              cat("\t\t", i, ":  ", x$params$reductions[[reduction_name]][[i]], "\n")
          }
      })
  })
  cat("Optimization parameters:", "\n")
  sapply(names(x$params$optimization_params), FUN = function(i) {
    if(x$params$optimization_params[[i]] == "") {
      cat("\t", i, ":  Not defined\n")
    } else {
      cat("\t", i, ":  ", x$params$optimization_params[[i]], "\n")
    }
  })
  }
#'Access and modify parameters of VW model
#'
#'@description These functions allow to access VW model parameters by name and correctly modify them
#'@param vwmodel Model of vw class
#'@param name Name of VW parameter
#'@param value Replacment value of a parameter
#'@return Value of a parameter
#'@examples 
#'vwmodel <- vwsetup()
#'# Access parameter 
#'vwparams(vwmodel, "passes")
#'# Modify parameter
#'vwparams(vwmodel, "passes") <- 10
#'
#'@rdname vwmodel
vwparams <- function(vwmodel, name) {
    if(!inherits(vwmodel, "vw")) {
        stop("vwmodel should be of class vw")
    }
    
    key_string <- grep(pattern = paste0("\\b", name, "\\b"), x = names(unlist(vwmodel$params)), value = T)
    # print(key_string)
    if(length(key_string) > 0) {
        key_params <- unlist(strsplit(key_string, split = ".", fixed = TRUE))
        if(is.list(vwmodel$params[[key_params[1]]])) {
            if(is.list(vwmodel$params[[key_params[1]]][[key_params[2]]])) {
                return(vwmodel$params[[key_params[1]]][[key_params[2]]][[key_params[3]]])
            } else {
                return(vwmodel$params[[key_params[1]]][[key_params[2]]])
            }
        } else {
            return(vwmodel$params[[key_params[1]]])
        }
    } else {
        stop("Wrong parameter name")
    }
}

#'@rdname vwmodel
`vwparams<-` <- function(vwmodel, name, value) {
    if(!inherits(vwmodel, "vw")) {
        stop("vwmodel should be of class vw")
    }
    
    key_string <- grep(pattern = paste0("\\b", name, "\\b"), x = names(unlist(vwmodel$params)), value = T)
    if(length(key_string) > 0) {
        key_params <- unlist(strsplit(key_string, split = ".", fixed = TRUE))
        if(is.list(vwmodel$params[[key_params[1]]])) {
            if(is.list(vwmodel$params[[key_params[1]]][[key_params[2]]])) {
                vwmodel$params[[key_params[1]]][[key_params[2]]][[key_params[3]]] <- value
            } else {
                vwmodel$params[[key_params[1]]][[key_params[2]]] <- value
            }
        } else {
            vwmodel$params[[key_params[1]]] <- value
        }
        vwmodel$params <- .check_parameters(vwmodel$params)
        vwmodel$params_str <- .create_parameters_string(vwmodel$params)
        # file.remove(paste0(vwmodel$dir, vwmodel$model))
        return(vwmodel)
    } else {
        stop("Wrong parameter name")
    }
}


## original source:  vowpal_wabbit/R/r.vw/dt2vw.R
##
## written by (per 'git log'):
##   Selim Raboudi <selim.raboudi@gmail.com>
##   Jongbin Jung <olorin86@gmail.com>
## and by Dirk Eddelbuettel as part of rvw
## released under (3 clause) BSD like rest of vowpal_wabbit
##
## now maintained here by Ivan Pavlov as part of rvwgsoc

#'Create a VW data file from a R data.frame object 
#'
#'@param data [data.frame] data.frame object to be converted
#'@param file_path [string] file name of the resulting data in
#'  VW-friendly format
#'@param namespaces [list or yaml file] name of each namespace and
#'  each variable for each namespace can be a R list, or a YAML
#'  file example namespace with the IRIS database: namespaces =
#'  list(sepal = list('Sepal.Length', 'Sepal.Width'), petal = list('Petal.Length',
#'  'Petal.Width') this creates 2 namespaces (sepal
#'  and petal) containing the features defined by elements of this lists.
#'@param keep_space [string vector] keep spaces for this features
#'Example:"FERRARI 4Si"
#'With \code{keep_space} will be "FERRARI 4Si" and will be treated as two features
#'Without \code{keep_space} will be "FERRARI_4Si" and will be treated as one feature
#'@param targets [string or string vector]
#'If \code{[string]} then will be treated as vector with real number labels for regular VW input format. 
#'If \code{[string vector]} then will be treated as vectors with class costs for wap and csoaa 
#'multi-class classification algorithms or as vectors with actions for Contextual Bandit algorithm. 
#'@param probabilities [string vector] vectors with action probabilities for Contextual Bandit algorithm.
#'@param weight [string] weight (importance) of each line of the dataset.
#'@param base [string] base of each line of the dataset. Used for residual regression.
#'@param tag [string] tag of each line of the dataset.
#'@param append [bool] data to be appended to result file
#'@import yaml
#'@import tools 
df2vw <- function(data, file_path, namespaces = NULL, keep_space = NULL,
                  targets = NULL, probabilities = NULL,
                  weight = NULL, base = NULL, tag = NULL,
                  append = FALSE) {
    # library(tools)
    # if namespaces = NULL, define a unique namespace
    if (is.null(namespaces)) {
        all_vars <- colnames(data)[!colnames(data) %in% c(targets, probabilities, weight, base, tag)]
        namespaces <- list(A = list(all_vars))
    }
    
    # parse variable names
    specChar      <- "\\(|\\)|\\||\\:|'"
    specCharSpace <- "\\(|\\)|\\||\\:| |'"
    
    parsingNames <- function(x) {
        ret <- c()
        for (el in x)
            ret <- append(ret, gsub(specCharSpace,'_', el))
        ret
    }
    
    # parse categorical variables
    parsingVar <- function(x, keepSpace) {
        # remove leading and trailing spaces, then remove special characters
        # then remove isolated underscores.
        if (!keepSpace)
            spch <- specCharSpace
        else
            spch <- specChar
        gsub(spch, '_', x)
    }
    
    # namespace load with a yaml file
    if (typeof(namespaces) == "character" && length(namespaces) == 1 &&
        grepl("yaml$", namespaces)) {
        requireNamespace(yaml)
        print("Using YAML file for loading the namespaces")
        if (requireNamespace("yaml", quiet=TRUE, as.character=TRUE)) {
            namespaces <- yaml::yaml.load_file(namespaces)
        } else {
            stop("The 'yaml' package is needed.", .Call=FALSE)
        }
    }
    
    # replace all names to avoid conflicts with VW file format
    names(data) <- parsingNames(names(data))
    names(namespaces) <- parsingNames(names(namespaces))
    for (x in names(namespaces)) namespaces[[x]] <- parsingNames(namespaces[[x]])
    targets <- parsingNames(targets)
    if (!is.null(probabilities)) probabilities <- parsingNames(probabilities)
    if (!is.null(weight)) weight <- parsingNames(weight)
    if (!is.null(base)) base <- parsingNames(base)
    if (!is.null(tag)) tag <- parsingNames(tag)
    
    # Preparing file
    if(!append)
        vw_file <- file(file_path,"w")
    else
        vw_file <- file(file_path,"a")
    
    # Construct vw format for labels, weights, base and tag
    vw_format <- ""
    column_names <- c()
    
    if(!is.null(targets)) {
        if(length(targets) > 1) {
            if(!is.null(probabilities) & (length(targets) == length(probabilities))) {
                vw_format <- paste0(vw_format, 1:length(targets), rep(":%s:%s ", length(targets)), collapse = "")
                column_names <- c(column_names,
                                  strsplit(paste(targets, probabilities, collapse = " "), split = " "))
            } else {
                vw_format <- paste0(vw_format, 1:length(targets), rep(":%s ", length(targets)), collapse = "")
                column_names <- c(column_names, targets)
            }
        } else {
            vw_format <- paste0(vw_format, "%s ")
            column_names <- c(column_names, targets)
        }
        if(!is.null(weight)) {
            vw_format <- paste0(vw_format, "%s ")
            column_names <- c(column_names, weight)
        }
        if(!is.null(base)) {
            vw_format <- paste0(vw_format, "%s ")
            column_names <- c(column_names, base)
        }
        if(!is.null(tag)) {
            vw_format <- paste0(vw_format, "'%s ")
            column_names <- c(column_names, tag)
        }
    }
    vw_format <- trimws(vw_format, which = "right")
    
    numeric_value <- sapply(data, is.numeric)
    column_names <- unlist(column_names)
    apply(data, MARGIN = 1, function(row) {
        features_line = ""
        for(namespace_name in names(namespaces)) {
            features_line = paste(features_line, paste0("|", namespace_name))
            for(feature_name in namespaces[[namespace_name]]) {
                if(numeric_value[feature_name]) {
                    features_line = paste(features_line, paste0(feature_name, ":", row[feature_name]))
                } else if(feature_name %in% keep_space) {
                    features_line = paste(features_line, parsingVar(row[feature_name], keepSpace = T))
                } else {
                    features_line = paste(features_line, paste0(feature_name, "^", parsingVar(row[feature_name], keepSpace = F)))
                }
                
            }
        }
        labels_line <- do.call(sprintf, c(list(vw_format), row[column_names]))
        writeLines(text = paste0(labels_line, features_line), con = vw_file)
        
    })
    close(vw_file)
    # Return check sum
    # unname(tools::md5sum(file_path))
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
                          quadratic="",
                          cubic="",
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
    binary_check <- list()
    oaa_check <- list(num_classes=3)
    ect_check <- list(num_classes=3)
    csoaa_check <- list(num_classes=3,
                        csoaa_ldf="")
    wap_check <- list(num_classes=3,
                      wap_ldf="")
    log_multi <- list(num_classes=3)
    lda_check <- list(num_topics=0,
                      lda_alpha=0.100000001,
                      lda_rho=0.100000001,
                      lda_D=10000,
                      lda_epsilon=0.00100000005,
                      math_mode=0,
                      minibatch=1)
    mf_check <- list(rank=0)
    lrq_check <- list(features="", 
                      lrqdropout=FALSE)
    stage_poly <- list(sched_exponent = 1.0,
                       batch_sz = 1000,
                       batch_sz_no_doubling = TRUE)
    bootstrap_check <- list(rounds=10,
                            bs_type="mean")
    autolink <- list(degree=2)
    cb <- list(costs=2)
    cbify <- list(num_classes=3)
    nn_check <- list(hidden=3,
                     inpass=FALSE,
                     multitask=FALSE,
                     dropout=FALSE,
                     meanfield=FALSE)
    topk <- list(k=3)
    struct_search <- list(id=0)
    boosting_check <- list(num_learners=5)
    # Learning algorithm default/check lists
    sgd_check <- list(adaptive=TRUE,
                      normalized=TRUE,
                      invariant=TRUE)
    bfgs_check <- list(conjugate_gradient=FALSE)
    ftrl_check <- list(ftrl_alpha=0.005,
                       ftrl_beta=0.1)
    ksvm_check <- list(reprocess=1,
                       kernel="linear",
                       bandwidth=1.0)
    optimization_check <- list(hessian_on=FALSE,
                               initial_pass_length="",
                               l1=0,
                               l2=0,
                               decay_learning_rate=1,
                               initial_t=0,
                               power_t=0.5,
                               learning_rate=0.5,
                               link="",
                               loss_function="squared",
                               quantile_tau=0.5)
    
    # Create default parameters list if no parameters provided
    # Else check parameters and return validated parameters
    if(length(params$reductions) != 0) {
        valid_reductions <- list()
        params$reductions <- sapply(names(params$reductions), function(reduction_name) {
            reduction_check_type <- get(paste0(reduction_name, "_check"))
            valid_reduction <- check_param_values(
                input = params$reductions[[reduction_name]],
                check = reduction_check_type
            )
            valid_reduction <- setNames(list(valid_reduction), reduction_name)
            valid_reductions <<- c(valid_reductions, valid_reduction)
        })
        params$reductions <- valid_reductions
        
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
    return(list(algorithm = params$algorithm,
                general_params = params$general_params,
                optimization_params = params$optimization_params,
                reductions = params$reductions))
}

.create_parameters_string <- function(params) {
    params_to_strings <- function(i) {
        if(is.na(flat_params[[i]]) | flat_params[[i]] == "") {
            return("")
        };
        if(is.logical(flat_params[[i]][[1]]) & flat_params[[i]][[1]] == TRUE) {
            return(paste0("--",i))
        }; 
        if(is.logical(flat_params[[i]][[1]]) & flat_params[[i]][[1]] == FALSE) {
            return("")
        } else {
            return(paste0("--",i," ",flat_params[[i]]))
        }
    }
    
    flatten <- function(x) {
        repeat {
            if(!any(vapply(x,is.list, logical(1)))) return(x)
            x <- Reduce(c, x)
        }
    }
    temp_params <- params
    
    # Convert different reductions into string with CL arguments
    reductions_params <- sapply(names(temp_params$reductions), function(reduction_name) {
        if (length(temp_params$reductions[[reduction_name]]) == 0) {
            tmp <- paste0("--", reduction_name)
        } else {
            tmp <- paste0("--", reduction_name, " ", temp_params$reductions[[reduction_name]][1])  
        }
        temp_params$reductions[[reduction_name]][1] <<- NA
        tmp
    })
    
    # temp_params$reductions <- list()
    # Filter empty strings
    reductions_params <- Filter(reductions_params, f = function(x) nchar(x) > 0)
    reductions_string <-  paste0(reductions_params, collapse = " ")
    
    # Flatten reduction
    flat_params <- flatten(temp_params$reductions)
    # Convert reduction parameters list to "--arg _" list
    flat_reduction_params <- sapply(names(flat_params), FUN = params_to_strings)
    # Filter empty strings
    flat_reduction_params <- Filter(flat_reduction_params, f = function(x) nchar(x) > 0)
    # Create string "--passes 0 --bit_precision 18" for parser
    reduction_params_string <- paste0(flat_reduction_params, collapse = " ")
    
    temp_params$reductions <- list()
    
    #Set learning mode string argument
    algorithm_string <- switch (temp_params$algorithm,
                                sgd = {tmp <- ""; tmp},
                                bfgs = {tmp <- "--bfgs"; tmp},
                                ftrl = {tmp <- "--ftrl"; tmp},
                                ksvm = {tmp <- "--ksvm"; tmp}
    )
    # Disable cache here, because it's checked in vwtrain and vwtest
    if (temp_params$general_params$cache) {
        temp_params$general_params$cache <- NA
    }
    # Flatten list
    flat_params <- flatten(temp_params[-c(1)])
    # Convert parameters list to "--arg _" list
    flat_params <- sapply(names(flat_params), FUN = params_to_strings)
    # Filter empty strings
    flat_params <- Filter(flat_params, f = function(x) nchar(x) > 0)
    # Create string "--passes 0 --bit_precision 18" for parser
    parameters_string <- paste0(flat_params, collapse = " ")
    parameters_string <- paste(algorithm_string, parameters_string, reductions_string, reduction_params_string, sep = " ")
    
    return(parameters_string)
}
