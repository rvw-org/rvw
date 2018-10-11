#include "vowpalwabbit/vw.h"

#include <Rcpp.h>
#include "helpers.h"

#include <fstream>
#include <stdio.h>
#include <string.h>
#include <iostream>
#include <streambuf>
#include <sstream>
#include <stdlib.h>


#include <RApiSerializeAPI.h>

#include <R.h>
#include <R_ext/Rdynload.h>

extern "C" {
#include "md5.h"
}

#ifdef _WIN32
#define PATH_SEPARATOR '\\' 
#else 
#define PATH_SEPARATOR '/' 
#endif 


// Based on code from R digest package http://dirk.eddelbuettel.com/code/digest.html
// Copyright (C) 2003 - 2016  Dirk Eddelbuettel <edd@debian.org>
std::string md5sum(char * char_x, uint32_t nChar) {
    char output[33+1];
    md5_context ctx;
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

Rcpp::String check_data(Rcpp::List & vwmodel, std::string & valid_data_str, SEXP data, bool quiet, std::string mode,
                                 Rcpp::Nullable<SEXP *> namespaces, Rcpp::Nullable<Rcpp::CharacterVector> keep_space,
                                 Rcpp::Nullable<Rcpp::CharacterVector> fixed, 
                                 Rcpp::Nullable<Rcpp::CharacterVector> targets, Rcpp::Nullable<Rcpp::CharacterVector> probabilities,
                                 Rcpp::Nullable<Rcpp::String> weight, Rcpp::Nullable<Rcpp::String> base,
                                 Rcpp::Nullable<Rcpp::String> tag, Rcpp::Nullable<int> multiline) {
    
    // Check if:
    //  no previous parser options are in the model
    //  OR
    //  any of passed parser options is NOT NULL (possible when using "predict.vw" function)
    if(Rf_isNull(vwmodel["parser_opts"]) || (!Rf_isNull(namespaces) || !Rf_isNull(keep_space) || !Rf_isNull(fixed) || !Rf_isNull(targets) ||
       !Rf_isNull(probabilities) || !Rf_isNull(weight) || !Rf_isNull(base) ||
       !Rf_isNull(tag) || !Rf_isNull(multiline))) { 
       
       // In this case we want to use parser options that were passed with "vwtest" so we save parser options
       vwmodel["parser_opts"] = Rcpp::List::create(Rcpp::Named("namespaces") = namespaces , Rcpp::Named("keep_space") = keep_space,
                                                    Rcpp::Named("fixed") = fixed,
                                                    Rcpp::Named("targets") = targets, Rcpp::Named("probabilities") = probabilities,
                                                    Rcpp::Named("weight") = weight, Rcpp::Named("base") = base,
                                                    Rcpp::Named("tag") = tag, Rcpp::Named("multiline") = multiline);
       
    } else {
        // In this case we use previously saved parser options
        if(!quiet){
            Rcpp::Rcout << "Using parser options from the previous session" << std::endl;
        }
    }
    
    Rcpp::String data_md5sum("");
    uint32_t nChar;
    char * char_x;
    if(TYPEOF(data) == STRSXP) {
        // Use path to file as model input
        valid_data_str = Rcpp::as<std::string>(data);
        
        // Check path for whitespace
        if(valid_data_str.find_first_of("\t\n ") != valid_data_str.npos) {
            Rcpp::stop("Whitespace characters are not allowed in `data` path");
        }
        
        std::ifstream data_instream(valid_data_str);
        std::string data_contents((std::istreambuf_iterator<char>(data_instream)), 
                             std::istreambuf_iterator<char>());
        
        char_x = &data_contents[0u];
        nChar = data_contents.length();
        data_md5sum = md5sum(char_x, nChar);
    } else if(TYPEOF(data) == VECSXP) {
        // Parse data frame and use VW file as model input
        
        // Update valid data string
        valid_data_str = Rcpp::as<std::string>(vwmodel["dir"]) + PATH_SEPARATOR + mode + ".vw";
        // Compute md5sum of data.frame
        Rcpp::RawVector x = serializeToRaw(data);
        char_x = (char*) RAW(x);
        nChar = XLENGTH(x);
        
        data_md5sum = md5sum(char_x, nChar);
        
        // Compare new md5sum with old md5sum
        Rcpp::List vwmodel_md5sums = vwmodel["data_md5sum"];
        Rcpp::String model_md5sum = vwmodel_md5sums[mode];
        
        
        if (model_md5sum != data_md5sum) {
            if(!quiet){
                Rcpp::Rcout << "Converting data.frame to VW format" << std::endl;
            }
            Rcpp::Environment env("package:rvw");
            Rcpp::Function r_df2vw = env["df2vw"];
            // Convert data.frame to VW
            
            Rcpp::List saved_parser_opts = vwmodel["parser_opts"];
            
            // Rcpp::Nullable<SEXP *> valid_namespaces = saved_parser_opts["namespaces"];
            // Rcpp::Nullable<Rcpp::CharacterVector> valid_keep_space = saved_parser_opts["keep_space"];
            // Rcpp::Nullable<Rcpp::CharacterVector> valid_targets = saved_parser_opts["targets"];
            // Rcpp::Nullable<Rcpp::CharacterVector> valid_probabilities = saved_parser_opts["probabilities"];
            // Rcpp::Nullable<Rcpp::String> valid_weight = saved_parser_opts["weight"];
            // Rcpp::Nullable<Rcpp::String> valid_base = saved_parser_opts["base"];
            // Rcpp::Nullable<Rcpp::String> valid_tag = saved_parser_opts["tag"]; 
            // Rcpp::Nullable<int> valid_multiline = saved_parser_opts["multiline"];
            
            r_df2vw(data, valid_data_str,
                    saved_parser_opts["namespaces"], saved_parser_opts["keep_space"], saved_parser_opts["fixed"],
                    saved_parser_opts["targets"], saved_parser_opts["probabilities"],
                    saved_parser_opts["weight"], saved_parser_opts["base"], saved_parser_opts["tag"], saved_parser_opts["multiline"],
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

// setup function from VW main.cc file
// modified to work in library mode using Rcpp
vw* setup_model(std::string args_str) {
    
    int argc;
    char** argv = VW::get_argv_from_string(args_str, argc);
    
    vw* all = nullptr;
    try { all = VW::initialize(argc, argv);
    }
    catch(const VW::vw_exception& ex){
        Rcpp::Rcout << ex.what() << std::endl;
        throw;
    }
    catch(...)
    {
        Rcpp::Rcout << "unknown exception" << std::endl;
        throw;
    }
    
    if (!all->quiet && !all->bfgs && !all->searchstr && !all->opts_n_args.vm.count("audit_regressor"))
    {
        Rcpp::Rcout << std::left
                    << std::setw(shared_data::col_avg_loss) << std::left << "average"
                    << " "
                    << std::setw(shared_data::col_since_last) << std::left << "since"
                    << " "
                    << std::right
                    << std::setw(shared_data::col_example_counter) << "example"
                    << " "
                    << std::setw(shared_data::col_example_weight) << "example"
                    << " "
                    << std::setw(shared_data::col_current_label) << "current"
                    << " "
                    << std::setw(shared_data::col_current_predict) << "current"
                    << " "
                    << std::setw(shared_data::col_current_features) << "current"
                    << std::endl;
        Rcpp::Rcout << std::left
                    << std::setw(shared_data::col_avg_loss) << std::left << "loss"
                    << " "
                    << std::setw(shared_data::col_since_last) << std::left << "last"
                    << " "
                    << std::right
                    << std::setw(shared_data::col_example_counter) << "counter"
                    << " "
                    << std::setw(shared_data::col_example_weight) << "weight"
                    << " "
                    << std::setw(shared_data::col_current_label) << "label"
                    << " "
                    << std::setw(shared_data::col_current_predict) << "predict"
                    << " "
                    << std::setw(shared_data::col_current_features) << "features"
                    << std::endl;
    }
    
    return all;
}	

// Collect final performance evaluation results
Rcpp::List get_eval(vw& all)
{
    int num_examples = all.sd->example_number;
    double weighted_example_sum = all.sd->weighted_examples();
    double weighted_label_sum = all.sd->weighted_labels;
    double avg_loss = NA_REAL;
    double avg_multiclass_log_loss = NA_REAL;
    float best_const = NA_REAL;
    float best_const_loss = NA_REAL;
    int total_feature = all.sd->total_features;
    
    if(all.holdout_set_off) {
        if (all.sd->weighted_labeled_examples > 0) {
            avg_loss = all.sd->sum_loss / all.sd->weighted_labeled_examples; 
        } else {
            avg_loss = NA_REAL; 
        }
    } else if((all.sd->holdout_best_loss == FLT_MAX) || (all.sd->holdout_best_loss == FLT_MAX * 0.5)) {
        avg_loss = NA_REAL;
    } else {
        avg_loss = all.sd->holdout_best_loss;
    }
    if (all.sd->report_multiclass_log_loss)
    {
        if (all.holdout_set_off) {
            avg_multiclass_log_loss = all.sd->multiclass_log_loss / all.sd->weighted_labeled_examples; 
        } else {
            avg_multiclass_log_loss = all.sd->holdout_multiclass_log_loss / all.sd->weighted_labeled_examples;
        }
    }
    // Get best_const and best_const_loss
    copy_get_best_constant(all, best_const, best_const_loss);
    
    Rcpp::List eval_list = Rcpp::List::create(
        Rcpp::Named("num_examples") = num_examples,
        Rcpp::Named("weighted_example_sum") = weighted_example_sum,
        Rcpp::Named("weighted_label_sum") = weighted_label_sum,
        Rcpp::Named("avg_loss") = avg_loss,
        Rcpp::Named("avg_multiclass_log_loss") = avg_multiclass_log_loss,
        Rcpp::Named("best_const") = best_const,
        Rcpp::Named("best_const_loss") = best_const_loss,
        Rcpp::Named("total_feature") = total_feature
        );
    
    return(eval_list);
}

// Copy of get_best_constant function from best_constant.cc file
bool copy_get_best_constant(vw& all, float& best_constant, float& best_constant_loss)
{
    if (all.sd->first_observed_label == FLT_MAX || // no non-test labels observed or function was never called
        (all.loss == nullptr) || (all.sd == nullptr)) return false;
    
    float label1 = all.sd->first_observed_label; // observed labels might be inside [sd->Min_label, sd->Max_label], so can't use Min/Max
    float label2 = (all.sd->second_observed_label == FLT_MAX)?0: all.sd->second_observed_label; // if only one label observed, second might be 0
    if (label1 > label2) {float tmp = label1; label1 = label2; label2 = tmp;} // as don't use min/max - make sure label1 < label2
    
    float label1_cnt;
    float label2_cnt;
    
    if (label1 != label2)
    {
        label1_cnt = (float) (all.sd->weighted_labels - label2*all.sd->weighted_labeled_examples)/(label1 - label2);
        label2_cnt = (float)all.sd->weighted_labeled_examples - label1_cnt;
    }
    else
        return false;
    
    if ( (label1_cnt + label2_cnt) <= 0.) return false;
    
    
    po::variables_map& vm = all.opts_n_args.vm;
    
    std::string funcName;
    if(vm.count("loss_function"))
        funcName = vm["loss_function"].as<std::string>();
    else
        funcName = "squared";
    
    if(funcName.compare("squared") == 0 || funcName.compare("Huber") == 0 || funcName.compare("classic") == 0)
        best_constant = (float) all.sd->weighted_labels / (float) (all.sd->weighted_labeled_examples);
    else if (all.sd->is_more_than_two_labels_observed)
    {
        //loss functions below don't have generic formuas for constant yet.
        return false;
        
    }
    else if(funcName.compare("hinge") == 0)
    {
        
        best_constant = label2_cnt <= label1_cnt ? -1.f: 1.f;
        
    }
    else if(funcName.compare("logistic") == 0)
    {
        
        label1 = -1.; //override {-50, 50} to get proper loss
        label2 =  1.;
        
        if (label1_cnt <= 0) best_constant = 1.;
        else if (label2_cnt <= 0) best_constant = -1.;
        else
            best_constant = std::log(label2_cnt/label1_cnt);
        
    }
    else if(funcName.compare("quantile") == 0 || funcName.compare("pinball") == 0 || funcName.compare("absolute") == 0)
    {
        
        float tau = 0.5;
        if(vm.count("quantile_tau"))
            tau = vm["quantile_tau"].as<float>();
        
        float q = tau*(label1_cnt + label2_cnt);
        if (q < label2_cnt) best_constant = label2;
        else best_constant = label1;
    }
    else
        return false;
    
    if (!all.sd->is_more_than_two_labels_observed)
    {
        best_constant_loss =  (label1_cnt>0)?all.loss->getLoss(all.sd, best_constant, label1) * label1_cnt:0.0f;
        best_constant_loss += (label2_cnt>0)?all.loss->getLoss(all.sd, best_constant, label2) * label2_cnt:0.0f;
        best_constant_loss /= label1_cnt + label2_cnt;
    }
    else best_constant_loss = FLT_MIN;
    
    return true;
}

std::vector<std::string> split_str(const std::string &s, char del) {
    std::stringstream s_stream(s);
    std::string item;
    std::vector<std::string> elems;
    while ( getline(s_stream, item, del) ) {
        elems.push_back(item);
    }
    return elems;
}
