#include "vowpalwabbit/vw.h"
#include <Rcpp.h>
#include <fstream>

Rcpp::CharacterVector check_data(Rcpp::List & vwmodel, std::string & valid_data_str, SEXP & data=R_NilValue, std::string mode="train",
                       SEXP & namespaces=R_NilValue, SEXP & keep_space=R_NilValue,
                       SEXP & targets=R_NilValue, SEXP & probabilities=R_NilValue,
                       SEXP & weight=R_NilValue, SEXP & base=R_NilValue, SEXP & tag=R_NilValue) {
    Rcpp::CharacterVector data_md5sum("");
    if(TYPEOF(data) == STRSXP) {
        // Use path to file as model input
        valid_data_str = Rcpp::as<std::string>(data);
        // Compute cache
        Rcpp::Environment env("package:tools");
        Rcpp::Function r_md5sum = env["md5sum"];
        data_md5sum = r_md5sum(valid_data_str);
    } else if(TYPEOF(data) == VECSXP) {
        // Parse data frame and use VW file as model input
        Rcpp::DataFrame input_dataframe(data);
        Rcpp::Environment env("package:rvwgsoc");
        Rcpp::Function r_df2vw = env["df2vw"];
        valid_data_str = Rcpp::as<std::string>(vwmodel["dir"]) + mode + ".vw";
        
        // Convert and compute cache
        data_md5sum = r_df2vw(data, valid_data_str,
                namespaces, keep_space,
                targets, probabilities,
                weight, base, tag,
                false
        );
    } else {
        Rcpp::stop("Only String and data.frame types are supported");
    }
    return data_md5sum;
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
