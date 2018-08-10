library(mltools)
library(rvwgsoc)

curr_dir <- getwd()
setwd(tempdir())
# We will use abalone dataset and will try to predict age groups (based on number of abalone shell rings) from physical measurements
aburl = 'http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data'
abnames = c('sex','length','diameter','height','weight.w','weight.s','weight.v','weight.sh','rings')
abalone = read.table(aburl, header = F , sep = ',', col.names = abnames)
data_full <- abalone

# Split number of rings into groups with equal (as possible) number of observations
data_full$group <- bin_data(data_full$rings, bins=3, binType = "quantile")
group_lvls <- levels(data_full$group)
levels(data_full$group) <- c(1, 2, 3)
# Prepare variables for CSOAA algorithm
data_full$cost_class_1 <- ifelse(data_full$group == 1, 0.8, 0.1)
data_full$cost_class_2 <- ifelse(data_full$group == 2, 0.8, 0.1)
data_full$cost_class_3 <- ifelse(data_full$group == 3, 0.8, 0.1)
data_full$rings <- factor(data_full$rings)
data_full$tag <- sapply(1:nrow(data_full), function(x) paste0("ex",x))
# Prepare indices to split data
ind_train <- sample(1:nrow(data_full), 0.8*nrow(data_full))
# Split data into train and test subsets
df_train <- data_full[ind_train,]
df_test <- data_full[-ind_train,]

vwmodel <- vwsetup(dir = "./", 
                   option = "csoaa", num_classes = 3)

vwtrain(vwmodel, data = df_train,
        namespaces = list(NS1 = list("sex", "rings"), NS2 = list("diameter", "length", "height")),
        targets = c("cost_class_1", "cost_class_2", "cost_class_3"), tag = "tag"
)
predict.vw(vwmodel, data = df_test)

setwd(curr_dir)   

