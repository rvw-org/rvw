## original source:  vowpal_wabbit/R/r.vw/dt2vw.R
##
## written by (per 'git log'):
##   Selim Raboudi <selim.raboudi@gmail.com>
##   Jongbin Jung <olorin86@gmail.com>
## and released under (3 clause) BSD like rest of vowpal_wabbit
##
## now maintained here by Dirk Eddelbuettel as part of rvw

#' Create a vw data file from a R data.frame object
#'
#' @param data [data.table] data.table format (to be transformed)
#' @param fileName [string] file name of the resulting data in
#'  VW-friendly format
#' @param namespaces [list / yaml file] name of each namespace and
#'  each variable for each namespace can be a R list, or a YAML
#'  file example namespace with the IRIS database: namespaces =
#'  list(sepal = list(varName = c('Sepal.Length', 'Sepal.Width'),
#'  keepSpace=F), petal = list(varName = c('Petal.Length',
#'  'Petal.Width'), keepSpace=F)) this creates 2 namespaces (sepal
#'  and petal) containing the variables defined by varName.
#'  keepSpace allows to keep or remove spaces in categorical
#'  variables example: "FERRARI 4Si" ==> "FERRARI_4Si" with
#'  keepSpace = F ==> "FERRARI 4Si" with keepSpace = T (interpreted
#'  by VW as two distinct categorical variables)
#' @param target [string] target of the data (target)
#' @param weight [string] weight of each line of the dataset
#'  (importance)
#' @param tag [string] tag of each line of the dataset
#' @param hard_parse [bool] if equals true, parses the data more
#'  strictly to avoid feeding VW with false categorical variables
#'  like '_', or same variables perceived differently like "_var"
#'  and "var"
#' @param append [bool] data to be appended to result file
dt2vw <- function(data, fileName, namespaces = NULL, target = NULL, weight = NULL, tag = NULL,
                  hard_parse = FALSE, append = FALSE) {

    if (any(is.na(data))) stop("Please remove any 'NA' values from data first.", call. = FALSE)
    
    data <- data.table::copy(data.table::setDT(data))

    ## change target if its boolean to take values in {-1,1}
    if (!is.null(target)) {
        if (!is.numeric(data[[target]])) {
            if (is.logical(data[[target]]) |
                sum(levels(factor(data[[target]])) == levels(factor(c(0,1)))) == 2) {
                data[[target]][data[[target]] == TRUE]  <- 1
                data[[target]][data[[target]] == FALSE] <- -1
            }
        }
    }

    ## if namespaces = NULL, define a unique namespace
    if (is.null(namespaces)) {
        all_vars <- colnames(data)[!colnames(data) %in% c(target, weight, tag)]
        namespaces <- list(A = list(varName = all_vars, keepSpace=F))
    }

    ## parse variable names
    specChar      <- "\\(|\\)|\\||\\:|'"
    specCharSpace <- "\\(|\\)|\\||\\:| |'"

    parsingNames <- function(x) {
        ret <- c()
        for (el in x)
            ret <- append(ret, gsub(specCharSpace,'_', el))
        ret
    }

    ## parse categorical variables
    parsingVar <- function(x, keepSpace, hard_parse) {
        ## remove leading and trailing spaces, then remove special characters
        ## then remove isolated underscores.
        if (!keepSpace)
            spch <- specCharSpace
        else
            spch <- specChar

        if (hard_parse)
            gsub('(^_( *|_*)+)|(^_$)|(( *|_*)+_$)|( +_+ +)',' ',
                 gsub(specChar,'_', gsub('(^ +)|( +$)', '',x)))
        else
            gsub(spch, '_', x)
    }

    ## NAMESPACE LOAD WITH A YAML FILE
    if (typeof(namespaces) == "character" && length(namespaces) == 1 &&
        ##str_sub(namespaces, -4, -1) == "yaml")
        grepl("yaml$", namespaces)) {
        print("###############  USING YAML FILE FOR LOADING THE NAMESPACES  ###############")
        if (requireNamespace("yaml", quiet=TRUE, as.character=TRUE)) {
            namespaces <- yaml::yaml.load_file(namespaces)
        } else {
            stop("The 'yaml' package is needed.", .Call=FALSE)
        }
    }

    ## AVOIDING DATA FORMAT PROBLEMS
    setnames(data, names(data), parsingNames(names(data)))
    names(namespaces) <- parsingNames(names(namespaces))
    for (x in names(namespaces)) namespaces[[x]]$varName <- parsingNames(namespaces[[x]]$varName)
    target <- parsingNames(target)
    if (!is.null(tag)) tag <- parsingNames(tag)
    if (!is.null(weight)) weight <- parsingNames(weight)


    ## INITIALIZING THE HEADER AND INDEX
    ##Header: list of variables'name for each namespace
    ##Index: check if the variable is numerical (->TRUE) or categorical (->FALSE)
    Header <- list()
    Index <- list()

    for(nsN in names(namespaces)) {
        Index[[nsN]] <- sapply(data[,namespaces[[nsN]][['varName']],with=F], is.numeric)
        ##Header[[nsN]][Index[[nsN]]] <- namespaces[[nsN]][['varName']][Index[[nsN]]]
        Header[[nsN]] <- namespaces[[nsN]][['varName']]

        ##   ESCAPE THE CATEGORICAL VARIABLES
        if(namespaces[[nsN]]$keepSpace)
            Header[[nsN]][!Index[[nsN]]] <- paste0("eval(parse(text = 'parsingVar(",
                                                   Header[[nsN]][!Index[[nsN]]],
                                                   ", keepSpace = T, hard_parse = hard_parse)'))")
        else
            Header[[nsN]][!Index[[nsN]]] <- paste0("eval(parse(text = 'parsingVar(",
                                                   Header[[nsN]][!Index[[nsN]]],
                                                   ", keepSpace = F, hard_parse = hard_parse)'))")
    }

    ##appending the name of the variable to its value for each categorical variable
    sapply(Index,
           FUN = function(x) { sapply(names(x),
                                      FUN = function(y) { if (x[[y]] == F) {
                                                              set(data, i=NULL, y,
                                                                  paste0(y,"_",data[[y]]))
                                                          }})})

    ##   FIRST PART OF THE VW DATA FORMAT: target, weight, tag
    formatDataVW <- ''
    argexpr <- character(0)

    ## Label can be null, no training is performed
    if (!is.null(target)) {
        ## Both weight and tag are not null
        if (!is.null(weight) && !is.null(tag)) {
            formatDataVW <- '%f %f %s'
            argexpr <- paste(target, weight, tag, sep = ', ')
        }
        ## Weight is null, tag is not null
        else if(is.null(weight) && !is.null(tag)) {
            formatDataVW <- '%f %s'
            argexpr <- paste(target, tag, sep = ', ')
        }
        ## Weight is not null, tag is null
        else if(!is.null(weight) && is.null(tag)) {
            formatDataVW <- '%f %f'
            argexpr <- paste(target, weight, sep = ', ')
        }
        ## We just output target
        else {
            formatDataVW <- '%f'
            argexpr <- target
        }
    }

    ## ADDING THE FORMAT FOR THE VARIABLES OF EACH NAMESPACE, AND CREATING THE ARGUMENT VECTOR
    for (nsN in names(namespaces)) {
        header <- Header[[nsN]]
        index <- Index[[nsN]]
        formatNumeric <- paste0(header[index], rep(":%f ", sum(index)), collapse = "")
        formatCategorical <- paste0(rep("%s", sum(!index)), collapse = " ")

        formatDataVW <- c(formatDataVW, paste0(nsN, ' ', formatNumeric, formatCategorical))

        paramexpr <- paste0(c(header[index], header[!index] ), collapse=', ')

        argexpr <- paste0(c(argexpr, paramexpr), collapse = ', ')
    }

    ## FULL VW DATA STRING (NOT FORMATTED YET) : (%target %weight |A num1:%f %s |B num2:%f %s)
    if (!is.null(tag)) {
        formatDataVW <- paste0(formatDataVW, collapse = '|')
    } else {
        formatDataVW <- paste0(formatDataVW, collapse = ' |')
    }

    formatDataVW <- paste0("sprintf2('", formatDataVW, "',",argexpr, ")")
    ## FORMATTING USING THE DATA.TABLE DYNAMICS TO OBTAIN THE FINAL VW DATA STRING
    temp <- data[, eval(parse(text = formatDataVW))]
    temp <- paste0(temp, collapse = '\n')

    ## WRITING THE DATA TO A FILE
    if(!append)
        con <- file(fileName,"w")
    else
        con <- file(fileName,"a")
    writeLines(temp,con = con)
    close(con)
}


## Work around the "only 100 arguments are allowed" error
## in base::sprintf(). Only works with 'fmt' of length 1.
sprintf2 <- function(fmt, ...) {
    MAX_NVAL <- 99L
    args <- list(...)
    if (length(args) <= MAX_NVAL)
        return(sprintf(fmt, ...))
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





