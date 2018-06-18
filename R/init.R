
.pkgenv <- new.env(parent=emptyenv())

.onAttach <- function(libname, pkgname) {

    ## default to "" for both
    vw <- perf <- ""

    packageStartupMessage("Loading rvw.")
    if ("" != (res <- unname(Sys.which("vw")))) {
        vw <- res
        packageStartupMessage("Good: seeing 'vw' in '", vw, "'.")
    } else {
        packageStartupMessage("Bad: no 'vw' in your path.")
    }

    if ("" != (res <- unname(Sys.which("perf")))) {
        perf <- res
        packageStartupMessage("Good: seeing 'perf' in '", perf, "'.")
    } else {
        packageStartupMessage("Not so good: no (optional) 'perf' in your path.")
    }

    assign("vw", vw, envir=.pkgenv)
    assign("perf", perf, envir=.pkgenv)
}


getVW   <- function() .pkgenv$vw
getPerf <- function() .pkgenv$perf
