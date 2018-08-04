#include "vowpalwabbit/vw.h"
#include <Rcpp.h>

#include <fstream>
#include <stdio.h>
#include <string.h>

#include <RApiSerializeAPI.h>

#include <R.h>
#include <R_ext/Rdynload.h>

extern "C" {
#include "md5.h"
}


// Based on code from R digest package http://dirk.eddelbuettel.com/code/digest.html
// Copyright (C) 2003 - 2016  Dirk Eddelbuettel <edd@debian.org>
std::string md5sum(char * char_x, uint32_t nChar) {
    char output[33+1];
    int output_length = -1;
    md5_context ctx;
    output_length = 16;
    unsigned char md5sum[16];
    int j;
    md5_starts( &ctx );
    md5_update( &ctx, (uint8 *) char_x, nChar);
    md5_finish( &ctx, md5sum );
    memcpy(output, md5sum, 16);
    
    for (j = 0; j < 16; j++)
        sprintf(output + j * 2, "%02x", md5sum[j]);
    
    std::string output_str(output);
    
    return(output_str);
}

Rcpp::String check_data(Rcpp::List & vwmodel, std::string & valid_data_str, SEXP data, std::string mode="train",
                                 Rcpp::Nullable<SEXP *> namespaces=R_NilValue, Rcpp::Nullable<Rcpp::CharacterVector> keep_space=R_NilValue,
                                 Rcpp::Nullable<Rcpp::CharacterVector> targets=R_NilValue, Rcpp::Nullable<Rcpp::CharacterVector> probabilities=R_NilValue,
                                 Rcpp::Nullable<Rcpp::String> weight=R_NilValue, Rcpp::Nullable<Rcpp::String> base=R_NilValue,
                                 Rcpp::Nullable<Rcpp::String> tag=R_NilValue, Rcpp::Nullable<int> multiline=R_NilValue) {
    Rcpp::String data_md5sum("");
    uint32_t nChar;
    char * char_x;
    if(TYPEOF(data) == STRSXP) {
        // Use path to file as model input
        valid_data_str = Rcpp::as<std::string>(data);
        
        std::ifstream data_instream(valid_data_str);
        std::string data_contents((std::istreambuf_iterator<char>(data_instream)), 
                             std::istreambuf_iterator<char>());
        
        char_x = &data_contents[0u];
        nChar = data_contents.length();
        data_md5sum = md5sum(char_x, nChar);
    } else if(TYPEOF(data) == VECSXP) {
        // Parse data frame and use VW file as model input
        
        // Update valid data string
        valid_data_str = Rcpp::as<std::string>(vwmodel["dir"]) + mode + ".vw";
        // Compute md5sum of data.frame
        Rcpp::RawVector x = serializeToRaw(data);
        char_x = (char*) RAW(x);
        nChar = XLENGTH(x);
        
        data_md5sum = md5sum(char_x, nChar);
        
        // Compare new md5sum with old md5sum
        Rcpp::List vwmodel_md5sums = vwmodel["data_md5sum"];
        Rcpp::String model_md5sum = vwmodel_md5sums[mode];
        
        
        if (model_md5sum != data_md5sum) {
            Rcpp::Rcout << "Converting data.frame to VW format" << std::endl;
            Rcpp::Environment env("package:rvwgsoc");
            Rcpp::Function r_df2vw = env["df2vw"];
            // Convert data.frame to VW
            r_df2vw(data, valid_data_str,
                    namespaces, keep_space,
                    targets, probabilities,
                    weight, base, tag, multiline,
                    false
            );
        }
        
    } else {
        Rcpp::stop("Only String and data.frame types are supported");
    }
    return data_md5sum;
}

// Get number of examples used in model
int get_num_example(vw& all) {
    return all.sd->example_number + all.sd->weighted_holdout_examples;
}

bool file_exists(std::string file_name)
{
    std::ifstream infile (file_name.c_str());
    return infile.good();
}
