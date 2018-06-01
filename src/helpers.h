#include "vowpalwabbit/vw.h"
#include <Rcpp.h>

// Helper functions

// Check if data from vwmodel should be used or from function arguments
std::string check_data(Rcpp::List & vwmodel, std::string & data);

// Get number of examples used in model
int get_num_example(vw& all);
