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
        bool_check_names <- names(input) %in% names(check)
        if(!all(bool_check_names)) {
            error_msg <- paste0("Wrong argument names: ",
                                paste0(names(input)[!bool_check_names], collapse = ", "))
            stop(error_msg, call. = FALSE)
        }
        
        valid_input <- check
        bool_check_values <- sapply(names(input), FUN = function(i) {
            # First check if types of input argument values are correct (same as of check lists)
            bool_check <- (typeof(input[[i]]) == typeof(check[[i]])) | (is.na(input[[i]]))
            # Replace default/check values with values from input
            valid_input[[i]] <<- input[[i]]
            # And return bool values to raise errors
            bool_check
        })
        if(!all(bool_check_values)) {
            error_msg <- paste0("Wrong argument values: ",
                                paste0(names(input)[!bool_check_values], collapse = ", "))
            stop(error_msg, call. = FALSE)
        }
        
        # Return check with modified values
        return(valid_input)
    }
    
    # Create default parameters list if no parameters provided
    # Else check parameters and return validated parameters
    if(length(params$options) != 0) {
        valid_options <- list()
        params$options <- sapply(names(params$options), function(option_name) {
            option_check_type <- .rvw_global[["check_lists"]][[paste0(option_name, "_check")]]
            valid_option <- check_param_values(
                input = params$options[[option_name]],
                check = option_check_type
            )
            # Check for missing first argument value in option parameters
            if(is.na(valid_option[[1]])){
                error_msg <- paste0("Missing value for argument: ",
                                    names(valid_option)[[1]],
                                    "\nFor option: ",
                                    option_name, "\n"
                )
                stop(error_msg, call. = FALSE)
            }
            valid_option <- setNames(list(valid_option), option_name)
            valid_options <<- c(valid_options, valid_option)
        })
        params$options <- valid_options
        
    }
    if(length(params$general_params) == 0) {
        params$general_params <- .rvw_global[["check_lists"]][["general_check"]]
    } else {
        params$general_params <- check_param_values(
            input = params$general_params,
            # input = c(list(cache=params$cache), params$general_params),
            check = .rvw_global[["check_lists"]][["general_check"]]
        )
    }
    if(length(params$feature_params) == 0) {
        params$feature_params <- .rvw_global[["check_lists"]][["feature_check"]]
    } else {
        params$feature_params <- check_param_values(
            input = params$feature_params,
            check = .rvw_global[["check_lists"]][["feature_check"]]
        )
    }
    if(length(params$optimization_params) == 0) {
        algorithm_parameters <- .rvw_global[["check_lists"]][[paste0(params$algorithm, "_check")]]
        params$optimization_params <- c(algorithm_parameters, .rvw_global[["check_lists"]][["optimization_check"]])
    } else {
        algorithm_check_type <- .rvw_global[["check_lists"]][[paste0(params$algorithm, "_check")]]
        params$optimization_params <- check_param_values(
            input = params$optimization_params,
            check = c(algorithm_check_type, .rvw_global[["check_lists"]][["optimization_check"]])
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
                options = params$options))
}

.create_parameters_string <- function(params) {
    params_to_strings <- function(i) {
        if(is.na(flat_params[[i]]) || isTRUE(flat_params[[i]] == .rvw_global[["flatten_check_lists"]][[i]])) {
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
    
    temp_params <- params
    
    # Options exceptions
    exceptions_params <- c()
    # cb exception
    if(isTRUE(temp_params$options[["cb"]][1] == 0)){
        # Use --cb_adf if num_costs == 0
        exceptions_params <- c(
            exceptions_params,
            paste0("--cb_adf")
        )
        temp_params$options[["cb"]][1] <- NA 
    }
    # cb_explore exception
    if("cb_explore" %in% names(temp_params$options)) {
        # Use --cb_explore_adf if num_actions == 0
        if(isTRUE(temp_params$options[["cb_explore"]][1] == 0)){
            exceptions_params <- c(
                exceptions_params,
                paste0("--cb_explore_adf")
            )
            temp_params$options[["cb_explore"]][1] <- NA 
        }
        # create exploration type string like "--first arg"
        exceptions_params <- c(
            exceptions_params,
            paste0("--", temp_params$options[["cb_explore"]][2], " ", temp_params$options[["cb_explore"]][3])
        )
        temp_params$options[["cb_explore"]][2] <- NA
        temp_params$options[["cb_explore"]][3] <- NA
    }
    # Experience Replay exception
    if("replay" %in% names(temp_params$options)) {
        # --replay_c 100, --replay_m 100, --replay_b 100 like exception
        exceptions_params <- c(
            exceptions_params,
            paste0("--replay_", temp_params$options[["replay"]][1], " ", temp_params$options[["replay"]][2]),
            paste0("--replay_", temp_params$options[["replay"]][1], "_count ", temp_params$options[["replay"]][3])
        )
        temp_params$options[["replay"]][1] <- NA
        temp_params$options[["replay"]][2] <- NA
        temp_params$options[["replay"]][3] <- NA
    }
    exceptions_params <- Filter(exceptions_params, f = function(x) nchar(x) > 0)
    exceptions_string <-  paste0(exceptions_params, collapse = " ")
    
    
    # Convert different options into string with CL arguments
    options_params <- sapply(names(temp_params$options), function(option_name) {
        if(is.na(temp_params$options[[option_name]][1])) {
            tmp <- ""
        } else if (option_name == names(temp_params$options[[option_name]])[1]) {
            tmp <- paste0("--", option_name)
        } else {
            tmp <- paste0("--", option_name, " ", temp_params$options[[option_name]][1])
        }
        temp_params$options[[option_name]][1] <<- NA
        tmp
    })
    
    # temp_params$options <- list()
    # Filter empty strings
    options_params <- Filter(options_params, f = function(x) nchar(x) > 0)
    options_string <-  paste0(options_params, collapse = " ")
    
    # Flatten option
    flat_params <- .flatten(temp_params$options)
    
    # Exception for "--math_mode" -> "--math-mode"
    if("math_mode" %in% names(flat_params)) {
        names(flat_params) <- gsub(pattern = "math_mode", replacement = "math-mode", x = names(flat_params))
    }
    
    # Convert option parameters list to "--arg _" list
    flat_option_params <- sapply(names(flat_params), FUN = params_to_strings)
    # Filter empty strings
    flat_option_params <- Filter(flat_option_params, f = function(x) nchar(x) > 0)
    # Create string "--passes 0 --bit_precision 18" for parser
    option_params_string <- paste0(flat_option_params, collapse = " ")
    
    temp_params$options <- list()
    
    #Set learning mode string argument
    algorithm_string <- switch (temp_params$algorithm,
                                sgd = {tmp <- ""; tmp},
                                bfgs = {tmp <- "--bfgs"; tmp},
                                ftrl = {tmp <- "--ftrl"; tmp},
                                pistol = {tmp <- "--pistol"; tmp},
                                ksvm = {tmp <- "--ksvm"; tmp},
                                OjaNewton = {tmp <- "--OjaNewton"; tmp},
                                svrg = {tmp <- "--svrg"; tmp}
    )
    # # Disable cache here, because it's checked in vwtrain and vwtest
    # if (temp_params$general_params$cache) {
    #     temp_params$general_params$cache <- NA
    # }
    # Flatten list
    flat_params <- .flatten(temp_params[-c(1)])
    # Convert parameters list to "--arg _" list
    flat_params <- sapply(names(flat_params), FUN = params_to_strings)
    # Filter empty strings
    flat_params <- Filter(flat_params, f = function(x) nchar(x) > 0)
    # Create string "--passes 0 --bit_precision 18" for parser
    parameters_string <- paste0(flat_params, collapse = " ")
    final_params <- c(algorithm_string, parameters_string, exceptions_string, options_string, option_params_string)
    # parameters_string <- paste(algorithm_string, parameters_string, exceptions_string, options_string, option_params_string, sep = " ")
    parameters_string <-  paste0(
        Filter(final_params,f = function(x) nchar(x) > 0),
        collapse = " "
    )
    
    return(parameters_string)
}

# Flatten parameters list
.flatten <- function(x) {
    repeat {
        if(!any(vapply(x,is.list, logical(1)))) return(x)
        x <- Reduce(c, x)
    }
}
