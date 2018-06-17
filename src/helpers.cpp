#include "vowpalwabbit/vw.h"
#include <Rcpp.h>

std::string check_data(Rcpp::List & vwmodel, std::string & data_path, std::string mode="train") {
  Rcpp::List vwmodel_data = vwmodel["data"];
  std::string valid_data_str;
  // // DEBUG
  // // Rcpp::Rcout << vwmodel["params"] << std::endl;
  // Rcpp::Rcout << Rcpp::as<std::string>(vwmodel["dir"]) << std::endl;
  // Rcpp::Rcout << Rcpp::as<std::string>(vwmodel_data["train"]) << std::endl;
  // Rcpp::Rcout << data_path << std::endl;
  // // END DEBUG

  std::string vwmodel_data_file;
  if (mode == "test") {
    vwmodel_data_file = Rcpp::as<std::string>(vwmodel_data["test"]);
  } else {
    vwmodel_data_file = Rcpp::as<std::string>(vwmodel_data["train"]);
  }

  if(data_path.empty()) {
    if(vwmodel_data_file.empty()) {
      Rcpp::Rcerr << "No data provided" << std::endl;
    } else {
      valid_data_str = Rcpp::as<std::string>(vwmodel["dir"]) + vwmodel_data_file;
    }
  } else {
    // valid_data_str = Rcpp::as<std::string>(vwmodel["dir"]) + data;
    valid_data_str = data_path;
  }
  return valid_data_str;
}

// Get number of examples used in model
int get_num_example(vw& all) {
    return all.sd->example_number + all.sd->weighted_holdout_examples;
  }
