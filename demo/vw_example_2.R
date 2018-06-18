
## original source:  vowpal_wabbit/R/vw_example_2.R
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

## separate train and validation data
ind_train <- sample(1:nrow(dt), 40000)
dt_train <- dt[ind_train,]
dt_val <- dt[-ind_train,]

## to not randomly leaves files behind, change to
## temporary directory of the current R session
cwd <- getwd()
setwd(tempdir())

## first method: creating the vw data files before training
dt2vw(data <- dt_train, fileName = "diamond_train.vw", target = "y")
dt2vw(data <- dt_val, fileName = "diamond_val.vw", target = "y")

write.table(x <- dt_val$y, file = "valid_labels.txt",
            row.names = FALSE, col.names = FALSE)

auc1 <- vw(training_data = "diamond_train.vw", validation_data = "diamond_val.vw",
           validation_labels = "valid_labels.txt", use_perf = FALSE)

## 2 method: use directly data.frames
auc2 <- vw(training_data = dt_train, validation_data = dt_val,
           target = "y", use_perf = FALSE)

setwd(cwd)                              # go back
