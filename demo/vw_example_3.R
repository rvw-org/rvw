
## derived from:  vowpal_wabbit/R/vw_example_2.R
##
## written by (per 'git log'):
##   Selim Raboudi <selim.raboudi@gmail.com>
## and released under (3 clause) BSD like rest of vowpal_wabbit
##
## now maintained here by Dirk Eddelbuettel as part of rvw

library(rvw)                            # changed from r.vw

## data
data("diamonds", package = "ggplot2")
dt <- diamonds
dt$y <- with(dt, ifelse(y < 5.71, 1, -1))

set.seed(123)                           # arbitrary but fixed seed
ind_train <- sample(1:nrow(dt), 40000)  # separate train and validation data
dt_train <- dt[ind_train,]
dt_val <- dt[-ind_train,]

## to not randomly leaves files behind, change to
## temporary directory of the current R session
cwd <- getwd()
setwd(tempdir())

## use data directly
res <- vw(training_data = dt_train,
          validation_data = dt_val,
          target = "y",
          use_perf = rvw:::getPerf() != "",
          verbose = TRUE)
res[["data"]][, actual:=as.factor(dt_val$y)]

setwd(cwd)                              # go back

if (requireNamespace("caret", quietly=TRUE)) {
    caret::confusionMatrix(ifelse(res[["data"]][,predicted] >= 0.5, 1, -1), res[["data"]][,actual])
}

rvw:::plotDensity(res[["data"]])
