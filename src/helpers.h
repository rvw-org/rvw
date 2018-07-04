#include "vowpalwabbit/vw.h"
#include <Rcpp.h>

// Helper functions

// Check if data from vwmodel should be used or from function arguments
std::string check_data(Rcpp::List & vwmodel, std::string & data, std::string mode="train");

// Get number of examples used in model
int get_num_example(vw& all);

// Custom driver to test example creation using libvw
void custom_driver(vw& model, std::string & file_path);