# For VW v8.6.1
headers <- c('action_score.h', 'allreduce.h', 'array_parameters_dense.h',
             'array_parameters.h', 'cb_explore.h', 'cb.h', 'comp_io.h',
             'config.h', 'constant.h', 'cost_sensitive.h', 'crossplat_compat.h',
             'error_reporting.h', 'example_predict.h', 'example.h', 'ezexample.h',
             'feature_group.h', 'floatbits.h', 'global_data.h', 'hash.h',
             'io_buf.h', 'label_parser.h', 'learner.h', 'loss_functions.h',
             'memory.h', 'multiclass.h', 'multilabel.h', 'no_label.h',
             'parse_example.h', 'parse_primitives.h', 'parser_helper.h',
             'parser.h', 'simple_label.h', 'v_array.h', 'v_hashmap.h',
             'vw_exception.h', 'vw_validate.h', 'vw.h', 'vwdll.h')

path_prefix_list <- c("/usr/local", "/usr", "/opt")
path_suffix_list <- c("vw", "vowpalwabbit", "")

search_path_list <- unlist(lapply(path_suffix_list, FUN = function(x) file.path(path_prefix_list, "include", x)))

for (search_path in search_path_list) {
    headers_in_path <- headers %in% list.files(search_path)

    if (all(headers_in_path)) { # All headers are found
        valid_path <- search_path
        break
    } else if ( (sum(headers_in_path) < length(headers_in_path)) && (sum(headers_in_path) > 0) ) { # Some headers are found
        valid_path <- NULL
        cat(paste0("Missing headers in ", search_path, "\n"))
        cat(paste0(headers[!headers_in_path], collapse = ", "), "\n")
        break
    } else { # No headers are found
        valid_path <- NULL
    }
}

if(is.null(valid_path)) {
    stop("Can't find the proper 'include/vowpalwabbit' directory containing Vowpal Wabbit header files.", call. = FALSE)
} else {
    
    # Valid path found
    # cat(paste0("Valid path: ", valid_path, "\n"))

    if(!file.exists(file.path("src", "Makevars.in"))){ 
        stop("No 'Makevars.in' file", call. = FALSE)
    }

    makevars_in_lines <- readLines(file.path("src", "Makevars.in"))
    makevars_out <- file(file.path("src", "Makevars"), "w")
    

    include_line <- paste0("PKG_CPPFLAGS = -I", valid_path)
    writeLines(include_line, con = makevars_out)
    for (line in makevars_in_lines) {
        writeLines(line, con = makevars_out)
    }
    close(makevars_out)
    
}
