
##' Simple density plotting function for classification outcome
##'
##' @title Plot a Density Function for the Given Prediction and Actual
##' @param x A data.frame object containing (at least) two columns
##'  \sQuote{predicted} and \sQuote{actual}
##' @return A ggplot2 object is returned, but the function is invoked
##'  for the side effect of displaying the plot
##' @references See Zumel and Mount, Practical Data Science with R,
##'  page 101
##' @author Dirk Eddelbuettel
plotDensity <- function(x) {
    x$actual <- as.factor(x$actual)
    p <- ggplot(data=x) + geom_density(aes(x=predicted,linetype=actual,color=actual))
    p
}

## to silence R CMD check --as-cran
utils::globalVariables(c("predicted", "actual"))
