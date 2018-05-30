#include "vowpalwabbit/vw.h"
#include <Rcpp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


// [[Rcpp::export(".create_cache")]]
void create_cache(Rcpp::String data, Rcpp::String dir) {
  Rcpp::Rcout << "create_cache placeholder" << std::endl;
}

// [[Rcpp::export]]
void vw_train(Rcpp::List vwmodel, std::string data="") {
  // if (!Rcpp::mod.inherits("vw")) Rcpp::stop("Input model is not VW model");
  // Rcpp::Rcout << vwmodel.attr("class") << std::endl;
  Rcpp::List vwmodel_data = vwmodel["data"];
  std::string data_str;
  std::string model_str = Rcpp::as<std::string>(vwmodel["dir"]) + Rcpp::as<std::string>(vwmodel["model"]);
  
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
      data_str = Rcpp::as<std::string>(vwmodel["dir"]) + Rcpp::as<std::string>(vwmodel_data["train"]);
    }
  } else {
    data_str = Rcpp::as<std::string>(vwmodel["dir"]) + data;
  }
  Rcpp::Rcout << "Using data file: " << data_str << std::endl;
  Rcpp::Rcout << "Using model file: " << model_str << std::endl;
  
  std::string train_init_str = Rcpp::as<std::string>(vwmodel["params_str"]);
  train_init_str += " -d " + data_str;
  Rcpp::Rcout << "Command line parameters: " << std::endl << train_init_str << std::endl;
  vw* train_model = VW::initialize(train_init_str);
  VW::start_parser(*train_model);
  std::cerr << "average  since         example        example  current  current  current" << std::endl;
  std::cerr << "loss     last          counter         weight    label  predict features" << std::endl;
  LEARNER::generic_driver(*train_model);
  VW::end_parser(*train_model);
  VW::save_predictor(*train_model, model_str);
  VW::finish(*train_model);
  
}
