#include "vowpalwabbit/vw.h"
#include <Rcpp.h>




// Helper functions

// Check if data from vwmodel should be used or from function arguments
Rcpp::String check_data(Rcpp::List & vwmodel, std::string & valid_data_str, SEXP data, std::string mode="train",
                                 Rcpp::Nullable<SEXP *> namespaces=R_NilValue, Rcpp::Nullable<Rcpp::CharacterVector> keep_space=R_NilValue,
                                 Rcpp::Nullable<Rcpp::CharacterVector> targets=R_NilValue, Rcpp::Nullable<Rcpp::CharacterVector> probabilities=R_NilValue,
                                 Rcpp::Nullable<Rcpp::String> weight=R_NilValue, Rcpp::Nullable<Rcpp::String> base=R_NilValue,
                                 Rcpp::Nullable<Rcpp::String> tag=R_NilValue, Rcpp::Nullable<int> multiline=R_NilValue);

// Get number of examples used in model
int get_num_example(vw& all);

// Custom driver to test example creation using libvw
void custom_driver(vw& model, std::string & file_path);

bool file_exists(std::string file_name);

// setup function from VW main.cc file
// modified to work in library mode using Rcpp
vw* setup_model(std::string args_str);

// Collect final performance evaluation results
Rcpp::List get_eval(vw& all);

// Copy of get_best_constant function from best_constant.cc file
bool copy_get_best_constant(vw& all, float& best_constant, float& best_constant_loss);