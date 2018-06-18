
library(rvw)
library(ggplot2)
library(data.table)

boston <- read.table(system.file("examples","bostonHousing","housing.data",package="rvw"),
                     col.names=c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE', 'DIS',
                                 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT', 'y'))


set.seed(123)                           # arbitrary but fixed seed
ind_train <- sample(1:nrow(boston), 0.8*nrow(boston))  # separate train and validation data
bostonTrain <- boston[ind_train,]
bostonVal <- boston[-ind_train,]

## to not randomly leaves files behind, change to
## temporary directory of the current R session
cwd <- getwd()
setwd(tempdir())

res <- vw(training_data=bostonTrain,
          validation_data=bostonVal,
          target="y",
          passes=10,
          loss="squared",
          link_function="--link=identity",
          keep_tempfiles=TRUE,
          do_evaluation=FALSE)

dt <- setDT(data.frame(actual=bostonVal[,y], predicted=res[["data"]][,predicted]))

ggplot(data=dt, aes(x=actual, y=predicted)) +
       geom_point(alpha=0.5, color="black") +
       geom_smooth(aes(x=actual, y=predicted), color="darkgrey") +
       geom_smooth(method="lm", se=FALSE, color="orange")
