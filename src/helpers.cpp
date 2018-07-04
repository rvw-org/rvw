#include "vowpalwabbit/vw.h"
#include <Rcpp.h>
#include <fstream>

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
            Rcpp::stop("No data provided");
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

// Custom driver to test example creation using libvw
void custom_driver(vw& model, std::string & file_path) {
    std::ifstream input_file_stream (file_path);
    std::string input_file_line;
    if (input_file_stream.is_open())
    {
        while ( getline (input_file_stream, input_file_line) )
        {
            // Rcpp::Rcout << "Line = " << input_file_line << std::endl;
            example *ec = VW::read_example(model, input_file_line);
            model.learn(ec);
            // Rcpp::Rcout << "Pred = " << ec->pred.scalar << std::endl;
            VW::finish_example(model, ec);
        }
        input_file_stream.close();
        Rcpp::Rcout << std::endl;
    }
}
