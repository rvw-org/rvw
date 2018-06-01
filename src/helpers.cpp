#include "vowpalwabbit/vw.h"
#include <Rcpp.h>

std::string check_data(Rcpp::List & vwmodel, std::string & data) {
  Rcpp::List vwmodel_data = vwmodel["data"];
  std::string valid_data_str;
  // DEBUG
  // Rcpp::Rcout << vwmodel["params"] << std::endl;
  Rcpp::Rcout << Rcpp::as<std::string>(vwmodel["dir"]) << std::endl;
  Rcpp::Rcout << Rcpp::as<std::string>(vwmodel_data["train"]) << std::endl;
  Rcpp::Rcout << data << std::endl;
  // END DEBUG
  if(data.empty()) {
    if(Rcpp::as<std::string>(vwmodel_data["train"]).empty()) {
      std::cerr << "No data provided" << std::endl;
    } else {
      valid_data_str = Rcpp::as<std::string>(vwmodel["dir"]) + Rcpp::as<std::string>(vwmodel_data["train"]);
    }
  } else {
    valid_data_str = Rcpp::as<std::string>(vwmodel["dir"]) + data;
  }
  return valid_data_str;
}

// Get number of examples used in model
int get_num_example(vw& all) {
    return all.sd->example_number / all.current_pass;
  }
