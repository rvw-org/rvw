#include "vowpalwabbit/vw.h"
#include <Rcpp.h>


// Helper functions

// Check if data from vwmodel should be used or from function arguments
Rcpp::CharacterVector check_data(Rcpp::List & vwmodel, std::string & valid_data_str, SEXP & data=R_NilValue, std::string mode="train",
                       SEXP & namespaces=R_NilValue, SEXP & keep_space=R_NilValue,
                       SEXP & targets=R_NilValue, SEXP & probabilities=R_NilValue,
                       SEXP & weight=R_NilValue, SEXP & base=R_NilValue, SEXP & tag=R_NilValue, SEXP & multiline=R_NilValue);

// Get number of examples used in model
int get_num_example(vw& all);

// Custom driver to test example creation using libvw
void custom_driver(vw& model, std::string & file_path);
