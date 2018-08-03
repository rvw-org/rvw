# Helper functions

.sprintf2 <- function(fmt, ...) {
    MAX_NVAL <- 99L
    args <- c(...)
    if (length(args) <= MAX_NVAL)
        return(do.call(sprintf, c(list(fmt), args)))
    stopifnot(length(fmt) == 1L)
    not_a_spec_at <- gregexpr("%%", fmt, fixed=TRUE)[[1L]]
    not_a_spec_at <- c(not_a_spec_at, not_a_spec_at + 1L)
    spec_at <- setdiff(gregexpr("%", fmt, fixed=TRUE)[[1L]], not_a_spec_at)
    nspec <- length(spec_at)
    if (length(args) < nspec)
        stop("too few arguments")
    if (nspec <= MAX_NVAL) {
        break_points <- integer(0)
    } else {
        break_points <- seq(MAX_NVAL + 1L, nspec, by=MAX_NVAL)
    }
    break_from <- c(1L, break_points)
    break_to <- c(break_points - 1L, nspec)
    fmt_break_at <- spec_at[break_points]
    fmt_chunks <- substr(rep.int(fmt, length(fmt_break_at) + 1L),
                         c(1L, fmt_break_at),
                         c(fmt_break_at - 1L, nchar(fmt)))
    ans_chunks <- mapply(
        function(fmt_chunk, from, to)
            do.call(sprintf, c(list(fmt_chunk), args[from:to])),
        fmt_chunks,
        break_from,
        break_to
    )
    paste(apply(ans_chunks,1, paste, collapse = ""), collapse = "\n")
}

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
    general_check <- list(random_seed=0,
                          ring_size="",
                          holdout_off=FALSE,
                          holdout_period=10,
                          holdout_after=0,
                          early_terminate=3,
                          loss_function="squared",
                          link="identity",
                          quantile_tau=0.5,
                          confidence=FALSE,
                          confidence_after_training=FALSE)
    feature_check <- list(bit_precision=18,
                          quadratic="",
                          cubic="",
                          interactions="",
                          permutations=FALSE,
                          leave_duplicate_interactions=FALSE,
                          noconstant=FALSE, 
                          feature_limit="",
                          ngram="",
                          skips="",
                          hash="",
                          affix="",
                          spelling="")
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
    ksvm_check <- list(reprocess=1,
                       kernel="linear",
                       bandwidth=1.0)
    # Learning algorithm default/check lists
    sgd_check <- list(adaptive=TRUE,
                      normalized=TRUE,
                      invariant=TRUE,
                      adax=FALSE,
                      sparse_l2=0,
                      l1_state=0,
                      l2_state=1)
    bfgs_check <- list(conjugate_gradient=FALSE)
    ftrl_check <- list(ftrl_alpha=0.005,
                       ftrl_beta=0.1)
    optimization_check <- list(learning_rate=0.5,
                               initial_pass_length="",
                               l1=0,
                               l2=0,
                               no_bias_regularization=FALSE,
                               feature_mask="",
                               decay_learning_rate=1,
                               initial_t=0,
                               power_t=0.5,
                               initial_weight=0,
                               random_weights="",
                               normal_weights="",
                               truncated_normal_weights="",
                               sparse_weights=FALSE,
                               input_feature_regularizer="")
    
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
    if(length(params$feature_params) == 0) {
        params$feature_params <- feature_check
    } else {
        params$feature_params <- check_param_values(
            input = params$feature_params,
            check = feature_check
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
    
    # # Cache should be created, if passes > 1
    # if(params$general_params$passes > 1) {
    #     params$general_params$cache <- TRUE
    # }
    # Return validated parameters
    return(list(algorithm = params$algorithm,
                general_params = params$general_params,
                feature_params = params$feature_params,
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
                                ftrl = {tmp <- "--ftrl"; tmp}
    )
    # # Disable cache here, because it's checked in vwtrain and vwtest
    # if (temp_params$general_params$cache) {
    #     temp_params$general_params$cache <- NA
    # }
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
