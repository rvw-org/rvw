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
            cat(paste0("Wrong argument names: ",
                       paste0(names(input)[!bool_check_names], collapse = ", ")))
            stop("Wrong argument names!")
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
            cat(paste0("Wrong argument values: ",
                        paste0(names(input)[!bool_check_values], collapse = ", ")))
            stop("Wrong argument values!")
        }
        
        # Return check with modified values
        return(valid_input)
    }
    
    # Create default parameters list if no parameters provided
    # Else check parameters and return validated parameters
    if(length(params$reductions) != 0) {
        valid_reductions <- list()
        params$reductions <- sapply(names(params$reductions), function(reduction_name) {
            reduction_check_type <- .pkgenv[["check_lists"]][[paste0(reduction_name, "_check")]]
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
        params$general_params <- .pkgenv[["check_lists"]][["general_check"]]
    } else {
        params$general_params <- check_param_values(
            input = params$general_params,
            # input = c(list(cache=params$cache), params$general_params),
            check = .pkgenv[["check_lists"]][["general_check"]]
        )
    }
    if(length(params$feature_params) == 0) {
        params$feature_params <- .pkgenv[["check_lists"]][["feature_check"]]
    } else {
        params$feature_params <- check_param_values(
            input = params$feature_params,
            check = .pkgenv[["check_lists"]][["feature_check"]]
        )
    }
    if(length(params$optimization_params) == 0) {
        algorithm_parameters <- .pkgenv[["check_lists"]][[paste0(params$algorithm, "_check")]]
        params$optimization_params <- c(algorithm_parameters, .pkgenv[["check_lists"]][["optimization_check"]])
    } else {
        algorithm_check_type <- .pkgenv[["check_lists"]][[paste0(params$algorithm, "_check")]]
        params$optimization_params <- check_param_values(
            input = params$optimization_params,
            check = c(algorithm_check_type, .pkgenv[["check_lists"]][["optimization_check"]])
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
        if(is.na(flat_params[[i]]) | (flat_params[[i]] == .pkgenv[["flatten_check_lists"]][[i]])) {
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
    flat_params <- .flatten(temp_params$reductions)
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
    flat_params <- .flatten(temp_params[-c(1)])
    # Convert parameters list to "--arg _" list
    flat_params <- sapply(names(flat_params), FUN = params_to_strings)
    # Filter empty strings
    flat_params <- Filter(flat_params, f = function(x) nchar(x) > 0)
    # Create string "--passes 0 --bit_precision 18" for parser
    parameters_string <- paste0(flat_params, collapse = " ")
    parameters_string <- paste(algorithm_string, parameters_string, reductions_string, reduction_params_string, sep = " ")
    
    return(trimws(parameters_string))
}

# Flatten parameters list
.flatten <- function(x) {
    repeat {
        if(!any(vapply(x,is.list, logical(1)))) return(x)
        x <- Reduce(c, x)
    }
}
