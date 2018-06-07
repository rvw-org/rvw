#include <fstream>

#include "helpers.h"
#include "vowpalwabbit/vw.h"
#include <Rcpp.h>


// [[Rcpp::export(".create_cache")]]
void create_cache(Rcpp::String data, Rcpp::String dir) {
  Rcpp::Rcout << "create_cache placeholder" << std::endl;
}

// [[Rcpp::export]]
void vwtrain(Rcpp::List vwmodel, std::string data="") {
  // if (!Rcpp::mod.inherits("vw")) Rcpp::stop("Input model is not VW model");
  // Rcpp::Rcout << vwmodel.attr("class") << std::endl;
  std::string data_str = check_data(vwmodel, data);
  std::string model_str = Rcpp::as<std::string>(vwmodel["dir"]) + Rcpp::as<std::string>(vwmodel["model"]);

  Rcpp::Rcout << "Using data file: " << data_str << std::endl;
  Rcpp::Rcout << "Using model file: " << model_str << std::endl;
  
  std::string train_init_str = Rcpp::as<std::string>(vwmodel["params_str"]);
  train_init_str += " -d " + data_str;
  Rcpp::Rcout << "Command line parameters: " << std::endl << train_init_str << std::endl;
  vw* train_model = VW::initialize(train_init_str);
  VW::start_parser(*train_model);
      // Try to redirect cerr
  std::stringstream new_buf;
  auto old_buf = std::cerr.rdbuf(new_buf.rdbuf());

  Rcpp::Rcout << "average  since         example        example  current  current  current" << std::endl;
  Rcpp::Rcout << "loss     last          counter         weight    label  predict features" << std::endl;
  LEARNER::generic_driver(*train_model);
  VW::end_parser(*train_model);
  VW::save_predictor(*train_model, model_str);
  VW::finish(*train_model);

  Rcpp::Rcout << new_buf.str() << std::endl;
  std::cerr.rdbuf(old_buf);
}

// [[Rcpp::export]]
Rcpp::NumericVector vwtest(Rcpp::List vwmodel, std::string data="", std::string probs_file = "probs_out.vw") {
  // if (!Rcpp::mod.inherits("vw")) Rcpp::stop("Input model is not VW model");
  // Rcpp::Rcout << vwmodel.attr("class") << std::endl;
  std::string data_str = check_data(vwmodel, data);
  std::string model_str = Rcpp::as<std::string>(vwmodel["dir"]) + Rcpp::as<std::string>(vwmodel["model"]);
  std::string probs_str = Rcpp::as<std::string>(vwmodel["dir"]) + probs_file;

  Rcpp::Rcout << "Using data file: " << data_str << std::endl;
  Rcpp::Rcout << "Using model file: " << model_str << std::endl;
  
  std::string train_init_str = Rcpp::as<std::string>(vwmodel["params_str"]);
  train_init_str += " -d " + data_str + " -p " + probs_str;
  Rcpp::Rcout << "Command line parameters: " << std::endl << train_init_str << std::endl;
  vw* predict_model = VW::initialize(train_init_str);
  VW::start_parser(*predict_model);
      // Try to redirect cerr
  std::stringstream new_buf;
  auto old_buf = std::cerr.rdbuf(new_buf.rdbuf());

  Rcpp::Rcout << "average  since         example        example  current  current  current" << std::endl;
  Rcpp::Rcout << "loss     last          counter         weight    label  predict features" << std::endl;
  LEARNER::generic_driver(*predict_model);
  VW::end_parser(*predict_model);
  int num_of_examples = get_num_example(*predict_model);
  VW::finish(*predict_model);

  Rcpp::Rcout << new_buf.str() << std::endl;
  std::cerr.rdbuf(old_buf);

  Rcpp::NumericVector data_vec(num_of_examples);
  std::ifstream probs_stream (probs_str);
  std::string line;
  for (int i = 0; i < num_of_examples; ++i)
  {
    getline(probs_stream, line);
    if (!line.empty())
    {
      data_vec[i] = std::stof(line);
    } else {
      data_vec[i] = R_NaReal;
    }
  }
  return data_vec;
}


