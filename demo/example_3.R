library(rvw)

# In this demo, we will take a look at the topic modeling problem.
# For this, we will use Latent Dirichlet Allocation (LDA) method implemented in Vowpal Wabbit (VW).

# First, switch to a temporary directory.
curr_dir <- getwd()
setwd(tempdir())

# Here we prepare our dataset. We consider the WebKB dataset. 
# It consists of web pages collected from various Universities and manually classified into seven different classes (topics).
# Original reference: The 4 Universities Data Set
# http://www.cs.cmu.edu/afs/cs.cmu.edu/project/theo-20/www/data/
# We use a preprocessed version of this dataset from Ana Cardoso-Cachopo PhD thesis:
# http://ana.cachopo.org/datasets-for-single-label-text-categorization
data_url <- "http://ana.cachopo.org/datasets-for-single-label-text-categorization/webkb-test-stemmed.txt"
lda_data <- read.delim(file = data_url, header = F, stringsAsFactors = F)
names(lda_data) <- c("topic", "text")

# Clear out empty lines.
lda_data <- lda_data[!(lda_data$text == ""), ]
# Prepare a vocabulary from all documents.
lda_vocab <- sort(unique(unlist(strsplit(lda_data$text, " "))))


# In order to use VW LDA algorithm, we have to convert plain text to "word:word_count word:word_count ..." format.
# Also, we replace the words with their indexes in the vocabulary.
# This is needed if we want to easily decode feature hashes later and show topics in a human-readable format.
lda_data$features <- sapply(lda_data$text, function(x) {
    splitted_words <- unlist(strsplit(x, " "))
    counted_words <- aggregate(data.frame(count=splitted_words), list(word=splitted_words), length)
    res_str <- paste0(apply(counted_words, 1, function(x){
        paste0( (which(lda_vocab == x[["word"]]) - 1) , ":", as.numeric(x[["count"]]))
        # Or use this if no replacement with index is needed:
        # paste0(x[["word"]], ":", as.numeric(x[["count"]]))
    }),
    collapse = " ")
    res_str
})

# Calculate required number of bits
bits <- ceiling(log2(length(lda_vocab)))

# Now we can set up a LDA model
lda_model <- vwsetup(feature_params = list(bit_precision=bits),
    option = "lda", # Enable LDA algorithm
                     num_topics = 7) # Specify the number of topics to learn (the same as were manually classified)

# And start learning a set of topics
vwtrain(vwmodel = lda_model,
        data = lda_data,
        namespaces = list(" " = "features"),
        fixed = "features",
        readable_model = "hashed",
        readable_model_path = "r_mdl.vw")



# vwout <- vwaudit(vwmodel = lda_model)

