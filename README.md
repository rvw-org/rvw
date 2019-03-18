[![Build Status](https://travis-ci.org/rvw-org/rvw.svg?branch=master)](https://travis-ci.org/rvw-org/rvw)

## rvw

Development of **rvw** package started as R Vowpal Wabbit (Google Summer of Code 2018) [project](https://summerofcode.withgoogle.com/projects/#5511455416254464).

**Vowpal Wabbit** is an online machine learning system that is known for its speed and scalability and is widely used in research and industry.

This package aims to bring its functionality to **R**.

## Installation

### From Source 

First you have to install **Vowpal Wabbit** from [here](https://github.com/JohnLangford/vowpal_wabbit#getting-the-code).

Next, once the required library isinstalled, you can install the **rvw** package using `remotes`:

```r
install.packages("remotes")  ## or devtools
remotes::install_github("rvw-org/rvw")
```

### Using Docker

We use [Docker](https://www.docker.com) for the [Travis CI](https://www.travis-ci.org) tests, and also provide a container
for deployment. Do 

```sh
docker pull rvowpalwabbit/run                 ## one time 
docker run --rm -ti rvowpalwabbit/run bash    ## launch container
```

to start the container with `rvw` installed.  See the 
[Boettiger and Eddelbuettel RJournal paper](https://journal.r-project.org/archive/2017/RJ-2017-065/index.html)
for more on Docker for R, and the [Rocker Project](https://www.rocker-project.org) used here.

## Getting Started
[Introduction](https://github.com/rvw-org/rvw/wiki/Introduction)

Examples:

* [Binary classification](https://github.com/rvw-org/rvw/wiki/Binary-classification)
* [CSOAA multiclass classification](https://github.com/rvw-org/rvw/wiki/CSOAA-multiclass-classification)
* [Topic modeling with Latent Dirichlet Allocation (LDA)](https://github.com/rvw-org/rvw/wiki/Topic-modeling-with-Latent-Dirichlet-Allocation-(LDA))


## Example 

In this example we will try to predict age groups (based on number of abalone shell rings) from physical measurements. We will use Abalone Data Set from [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Abalone).

First we prepare our data:

```r
library(mltools)
library(rvw)

set.seed(1)
aburl = 'http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data'
abnames = c('sex','length','diameter','height','weight.w','weight.s','weight.v','weight.sh','rings')
abalone = read.table(aburl, header = F , sep = ',', col.names = abnames)
data_full <- abalone

# Split number of rings into groups with equal (as possible) number of observations
data_full$group <- bin_data(data_full$rings, bins=3, binType = "quantile")
group_lvls <- levels(data_full$group)
levels(data_full$group) <- c(1, 2, 3)

# Prepare indices to split data
ind_train <- sample(1:nrow(data_full), 0.8*nrow(data_full))
# Split data into train and test subsets
df_train <- data_full[ind_train,]
df_test <- data_full[-ind_train,]
```

Then we set up a *Vowpal Wabbit* model:
```r
vwmodel <- vwsetup(option = "ect", num_classes = 3)
```

* *option = "ect"* - we will use [Error Correcting Tournament](https://github.com/JohnLangford/vowpal_wabbit/wiki/Error-Correcting-Tournament-(ect)-multi-class-example) algorithm to train multiclass classification model;
* *num_classes = 3* - number of classes in our data;

Now we start training:

```r
vwtrain(vwmodel, data = df_train,
        namespaces = list(NS1 = list("sex", "rings"),
                          NS2 = list("weight.w","weight.s","weight.v","weight.sh", "diameter", "length", "height")),
        targets = "group"
)
```
And we get: `average loss = 0.278060`

* *namespaces* - We will split our features into two namespaces `NS1` and `NS2`;
* *targets = "group"* - ground truth labels;


And finally compute predictions using trained model:

```r
predict.vw(vwmodel, data = df_test)
```
Here we get: `average loss = 0.221292`

We can add more learning algorithms to our model. For example we want to use *boosting* algorithm with 100 "weak" learners. Then we will just add this option to our model and train again:

```r
vwmodel <- add_option(vwmodel, option = "boosting", num_learners=100)

vwtrain(vwmodel, data = df_train,
        namespaces = list(NS1 = list("sex", "rings"),
                          NS2 = list("weight.w","weight.s","weight.v","weight.sh", "diameter", "length", "height")),
        targets = "group"
)
```
We get: `average loss = 0.229273`

And compute predictions:

```r
predict.vw(vwmodel, data = df_test)
```
Finally we get: `average loss = 0.081340`

In order to inspect parameters of our model we can simply print it:

```r
vwmodel
```

```
	Vowpal Wabbit model
Learning algorithm:   sgd 
Working directory:   /var/folders/yx/6949djdd3yb4qsw7x_95wfjr0000gn/T//RtmpjO3DD1 
Model file:   /var/folders/yx/6949djdd3yb4qsw7x_95wfjr0000gn/T//RtmpjO3DD1/vw_1534253637_mdl.vw 
General parameters: 
	 random_seed :   0 
	 ring_size :  Not defined
	 holdout_off :   FALSE 
	 holdout_period :   10 
	 holdout_after :   0 
	 early_terminate :   3 
	 loss_function :   squared 
	 link :   identity 
	 quantile_tau :   0.5 
Feature parameters: 
	 bit_precision :   18 
	 quadratic :  Not defined
	 cubic :  Not defined
	 interactions :  Not defined
	 permutations :   FALSE 
	 leave_duplicate_interactions :   FALSE 
	 noconstant :   FALSE 
	 feature_limit :  Not defined
	 ngram :  Not defined
	 skips :  Not defined
	 hash :  Not defined
	 affix :  Not defined
	 spelling :  Not defined
Learning algorithms / Reductions: 
	 ect :
		 num_classes :   3 
	 boosting :
		 num_learners :   100 
		 gamma :   0.1 
		 alg :   BBM 
Optimization parameters: 
	 adaptive :   TRUE 
	 normalized :   TRUE 
	 invariant :   TRUE 
	 adax :   FALSE 
	 sparse_l2 :   0 
	 l1_state :   0 
	 l2_state :   1 
	 learning_rate :   0.5 
	 initial_pass_length :  Not defined
	 l1 :   0 
	 l2 :   0 
	 no_bias_regularization :  Not defined
	 feature_mask :  Not defined
	 decay_learning_rate :   1 
	 initial_t :   0 
	 power_t :   0.5 
	 initial_weight :   0 
	 random_weights :  Not defined
	 normal_weights :  Not defined
	 truncated_normal_weights :  Not defined
	 sparse_weights :   FALSE 
	 input_feature_regularizer :  Not defined
Model evaluation. Training: 
	 num_examples :   3341 
	 weighted_example_sum :   3341 
	 weighted_label_sum :   0 
	 avg_loss :   0.2292727 
	 total_feature :   33408 
Model evaluation. Testing: 
	 num_examples :   836 
	 weighted_example_sum :   836 
	 weighted_label_sum :   0 
	 avg_loss :   0.08133971 
	 total_feature :   8360 
```
