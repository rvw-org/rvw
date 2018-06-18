
library(rvw)
suppressMessages(library(ggplot2))
suppressMessages(library(pROC))
stopifnot(requireNamespace("caret", quietly=TRUE))
stopifnot(requireNamespace("randomForest", quietly=TRUE))
stopifnot(requireNamespace("ranger", quietly=TRUE))
stopifnot(requireNamespace("Rborist", quietly=TRUE))
stopifnot(requireNamespace("party", quietly=TRUE))
stopifnot(requireNamespace("gbm", quietly=TRUE))
stopifnot(requireNamespace("xgboost", quietly=TRUE))
stopifnot(requireNamespace("lightgbm", quietly=TRUE))

stopifnot(requireNamespace("RColorBrewer", quietly=TRUE))
cols <- rev(RColorBrewer::brewer.pal(9, "Blues"))

data("etitanic", package="earth")
dt <- dt2 <- etitanic
dt[, "survived"] <- ifelse(dt[, "survived"] == 1, 1, -1)

set.seed(123)                                  # arbitrary but fixed seed
ind_train <- sample(1:nrow(dt), 0.8*nrow(dt))  # separate train and validation data
dt_train <- dt[ind_train,]
dt_val <- dt[-ind_train,]
dt2_train <- dt2[ind_train,]
dt2_val <- dt2[-ind_train,]

## to not randomly leaves files behind, change to
## temporary directory of the current R session
cwd <- getwd()
setwd(tempdir())

## use data directly
resvw <- vw(training_data = dt_train,
            validation_data = dt_val,
            target = "survived",
            use_perf = rvw:::getPerf() != "",
            passes = 10,
            keep_tempfiles=TRUE,
            verbose = TRUE)
resvw[["data"]][, actual:=as.factor(dt_val$survived)]
dd <- resvw[["data"]]
setwd(cwd)                              # go back

print(confvw <- caret::confusionMatrix(ifelse(resvw[["data"]][,predicted] >= 0.5, 1, -1), resvw[["data"]][,actual], positive="1"))

rvw:::plotDensity(resvw[["data"]])   ## TODO: plot method of a class vw

## recode to (0, 1) rather than (-1, 1)
predictions <- data.table::copy(dd)
predictions[ actual=="-1", actual:="0" ]
data.table::setnames(predictions, "predicted", "rvw")
lattice::xyplot(caret::calibration(actual ~ rvw, data=predictions))

rocvw <- roc(dd[,actual], dd[, predicted])





## glm
resglm <- glm(survived ~ pclass + sex + age + sibsp + parch, family = binomial(logit), data = dt2_train)
predglm <- predict(resglm, dt2_val, type="response")
print(confglm <- caret::confusionMatrix(ifelse(predglm >= 0.5, 1, 0), dt2_val$survived, positive="1"))

predictions[, glm:=predglm]
lattice::xyplot(caret::calibration(actual ~ rvw + glm, data=predictions), auto.key=list(columns=2))

rocglm <- roc(dd[,actual], predglm)


## rf
resrf <- randomForest::randomForest(as.factor(survived) ~ pclass + sex + age + sibsp + parch,
                                    data=dt_train,
                                    ntree=5000, importance=TRUE, keep.forest=TRUE)
predrf <- predict(resrf, dt_val)
predrfprob <- predict(resrf, dt_val, type="prob")
print(confrf <- caret::confusionMatrix(as.integer(as.character(predrf)), dt_val$survived, positive="1"))

predictions[, rf:=predrfprob[,2]]
lattice::xyplot(caret::calibration(actual ~ rvw + glm + rf, data=predictions), auto.key=list(columns=2))

rocrf <- roc(dd[,actual], predrfprob[,2])


## ranger
resranger <- ranger::ranger(as.factor(survived) ~ pclass + sex + age + sibsp + parch,
                            data=dt_train, write.forest=TRUE, probability=TRUE)
predranger <- predict(resranger, dt_val, type="response")
print(confranger <- caret::confusionMatrix(ifelse(predranger$predictions[,2] >= 0.5, 1, -1), dt_val$survived, positive="1"))

predictions[, ranger:=predranger$predictions[,2]]
lattice::xyplot(caret::calibration(actual ~ rvw + glm + rf + ranger, data=predictions), auto.key=list(columns=2))

rocranger <- roc(dd[,actual], predranger$predictions[,1])


## Rborist
resrborist <- Rborist::Rborist(dt_train[, .(pclass,sex,age,sibsp,parch)],
                               as.factor(dt_train[,survived]),
                               ctgCensus = "prob")
predrborist <- predict(resrborist,
                       dt_val[, .(pclass,sex,age,sibsp,parch)],
                       ctgCensus = "prob")
print(confrborist <- caret::confusionMatrix(ifelse(predrborist$prob[,1] >= 0.5, -1, 1),
                                            dt_val$survived, positive="1"))

predictions[, rborist:=predrborist$prob[,2]]
lattice::xyplot(caret::calibration(actual ~ rvw + glm + rf + ranger + rborist, data=predictions), auto.key=list(columns=2))

rocrborist <- roc(dd[,actual], predrborist$prob[,2])




## party
resparty <- party::ctree(as.factor(survived) ~ pclass + sex + age + sibsp + parch,
                         data=dt_train)
predparty <- predict(resparty, dt_val, type="prob")
predparty <- do.call(rbind, lapply(predparty, "[[", 2))
print(confparty <- caret::confusionMatrix(ifelse(predparty <= 0.5, -1, 1), dt_val$survived, positive="1"))

predictions[, party:=predparty]
lattice::xyplot(caret::calibration(actual ~ rvw + glm + rf + ranger + rborist + party, data=predictions, cut=10), auto.key=list(columns=2))

rocparty <- roc(dd[,actual], predparty[,1])


## gbm
resgbm <- gbm::gbm(survived ~ pclass + sex + age + sibsp + parch,
                   distribution="bernoulli", data=dt2_train, n.trees=500)
predgbm <- predict(resgbm, dt2_val, n.trees=500, type="response")
print(confgbm <- caret::confusionMatrix(ifelse(predgbm >= 0.5, 1, -1), dt_val$survived))

#predictions[, gbm:=predgbm]
#lattice::xyplot(caret::calibration(actual ~ rvw + glm + rf + ranger + rborist + party + gbm,
#                                   data=predictions, cut=10),
#                auto.key=list(columns=2))

rocgbm <- roc(dd[,actual], predgbm)


## xgboost
dt_train_dgc <- Matrix::sparse.model.matrix(survived ~ . - 1, data=dt_train)
dt_val_dgc <- Matrix::sparse.model.matrix(survived ~ . - 1, data=dt_val)
targetvector <- data.table::data.table(dt_train)[, Y:=0][survived==1, Y:=1][,Y]
resxgboost <- xgboost::xgboost(data = dt_train_dgc, label=targetvector,
                               objective="binary:logistic", nrounds=25, eta=0.75, max.depth=5,
                               verbose=0)
#predxgboost <- xgboost::predict(resxgboost, dt_val_dgc)
predxgboost <- predict(resxgboost, dt_val_dgc)

predictions[, xgboost:=predxgboost]
lattice::xyplot(caret::calibration(actual ~ rvw + glm + rf + ranger + rborist + party + xgboost,
                                   data=predictions, cut=10),
                auto.key=list(columns=2))

print(confxgboost <- caret::confusionMatrix(ifelse(predxgboost >= 0.5, 1, -1), dt_val$survived))
rocxgboost <- roc(dd[,actual], predxgboost)
lattice::xyplot(caret::calibration(actual ~ rvw + glm + rf + ranger + rborist + party + xgboost,
                                   data=predictions, cut=10),
                auto.key=list(columns=2))


## lightgbm
reslgbm <- lightgbm(data=dt_train_dgc, label=targetvector, obj="binary", verbose=0)
predlgbm <- predict(reslgbm, dt_val_dgc)
predictions[, lightgbm:=predlgbm]
lattice::xyplot(caret::calibration(actual ~ rvw + glm + rf + #ranger + rborist + party +
                                        xgboost + lightgbm,
                                   data=predictions, cut=10),
                auto.key=list(columns=2))
print(conflgbm <- caret::confusionMatrix(ifelse(predlgbm >= 0.5, 1, -1), dt_val$survived))
roclgbm <- roc(dd[,actual], predlgbm)

cfmats <- list(vw=confvw, glm=confglm, rf=confrf, ranger=confranger, rborist=confrborist,
               party=confparty, gbm=confgbm, xgboost=confxgboost, lightgbm=conflgbm)
df <- do.call(rbind, lapply(names(cfmats), function(n) {
                           M <- cfmats[[n]]$table
                           rownames(M) <- c("pred:dead", "pred:alive")
                           colnames(M) <- c("ref:dead", "ref:alive")
                           data.frame(M, Method=n)
                           }))
p <- ggplot(df, aes(y=Freq,x=Method,fill=Method)) +
    facet_grid(Prediction + Reference ~ .) +
    geom_bar(stat="identity") +
    scale_fill_brewer() +
    theme_light()
p

## roc plot
plot(rocvw, col=cols[1])
plot(rocglm, col=cols[2], add=TRUE)
plot(rocrf, col=cols[3], add=TRUE)
plot(rocranger, col=cols[4], add=TRUE)
plot(rocrborist, col=cols[5], add=TRUE)
plot(rocparty, col=cols[6], add=TRUE)
plot(rocgbm, col=cols[7], add=TRUE)
plot(rocxgboost, col=cols[8], add=TRUE)
plot(roclgbm, col=cols[9], add=TRUE)
legend("bottomright",
       legend=c(paste("vw",      format(as.numeric(rocvw$auc), digits=4)),
                paste("glm",     format(as.numeric(rocglm$auc), digits=4)),
                paste("rf",      format(as.numeric(rocrf$auc), digits=4)),
                paste("ranger",  format(as.numeric(rocranger$auc), digits=4)),
                paste("rborist", format(as.numeric(rocrborist$auc), digits=4)),
                paste("ctree",   format(as.numeric(rocparty$auc), digits=4)),
                paste("gbm",     format(as.numeric(rocgbm$auc), digits=4)),
                paste("xgboost", format(as.numeric(rocxgboost$auc), digits=4)),
                paste("lightgbm",format(as.numeric(roclgbm$auc), digits=4))),
       col=cols[1:9], bty="n", lwd=2)
