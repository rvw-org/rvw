#include <fstream>

#include "helpers.h"
#include "vowpalwabbit/vw.h"
#include "vowpalwabbit/global_data.h"
#include <Rcpp.h>


// [[Rcpp::export(".create_cache")]]
void create_cache(std::string dir="", std::string data_file="", std::string cache_file="") {
    std::string data_str = dir + data_file;
    std::string cache_str = dir + cache_file;
    
    std::string cache_init_str = "-d " + data_str + " --cache_file " + cache_str;
    vw* cache_model = VW::initialize(cache_init_str);
    VW::start_parser(*cache_model);
    
    // Modified version of LEARNER::generic_driver that only creates cache and doesn't train
    example* ec = nullptr;
    while (cache_model->early_terminate == false) {
        if ((ec = VW::get_example(cache_model->p)) != nullptr) {
            VW::finish_example(*cache_model, ec);
        } else {
            break;
        }
    }
    if (cache_model->early_terminate) { //drain any extra examples from parser.
        while ((ec = VW::get_example(cache_model->p)) != nullptr) {
            VW::finish_example(*cache_model, ec);
        }
    } 
    VW::end_parser(*cache_model);
    VW::finish(*cache_model);
}

//'Train Vowpal Wabbit model
//'
//'vwtrain is an interface to train VW model from \code{\link{vwsetup}}
//'
//'@param vwmodel Model of vw class to train
//'@param data_path Path to training data in .vw plain text format
//'@param readable_model Print trained model in human readable format ("hashed") 
//'and also with human readable features ("inverted")
//'@param quiet Do not print anything to the console 
//'@param update_model Update an existing model, when training with new data. \code{TRUE} by default.
//'@examples
//'ext_train_data <- system.file("extdata", "binary_train.vw", package = "rvwgsoc")
//'test_vwmodel <- vwsetup()
//'vwtrain(test_vwmodel, data_path = ext_train_data)
// [[Rcpp::export]]
void vwtrain(Rcpp::List vwmodel, std::string data_path="", Rcpp::Nullable<Rcpp::String> readable_model=R_NilValue, bool quiet=false, bool update_model=true) {
    // if (!Rcpp::mod.inherits("vw")) Rcpp::stop("Input model is not VW model");
    Rcpp::Rcout << Rf_inherits(vwmodel, "vw") << std::endl;
    if(!Rf_inherits(vwmodel, "vw")) {
        Rcpp::stop("vwmodel should be of class vw");
    }
        
    std::string data_str = check_data(vwmodel, data_path, "train");
    std::string model_str = Rcpp::as<std::string>(vwmodel["dir"]) + Rcpp::as<std::string>(vwmodel["model"]);
    std::string readable_model_str = Rcpp::as<std::string>(vwmodel["dir"]) + "readable_" + Rcpp::as<std::string>(vwmodel["model"]);
    Rcpp::List vwmodel_params = vwmodel["params"];
    Rcpp::List vwmodel_general_params = vwmodel_params["general_params"];
    
    std::string train_init_str = Rcpp::as<std::string>(vwmodel["params_str"]);
    // Commented for testing
    // train_init_str += " -d " + data_str;
    
    // Use existing train file to continue training
    if (update_model) {
        std::ifstream model_file(model_str);
        if (model_file.good()) {
            train_init_str += " -i " + model_str;
        }
    }
    
    // Check readable model output mode: hashed features and original features (invert hash)
    bool read_model_file = false;
    if (readable_model.isNotNull()) {
        if (Rcpp::as<std::string>(readable_model) == "hashed")
        {
            train_init_str += " --readable_model " + readable_model_str;
            read_model_file = true;
        } else if (Rcpp::as<std::string>(readable_model) == "inverted")
        {
            if (Rcpp::as<int>(vwmodel_general_params["passes"]) == 1) {
                train_init_str += " --invert_hash " + readable_model_str;
                read_model_file = true;
            } else {
                Rcpp::Rcerr << "Invert hash is not supported with passes > 1" << std::endl;
            }
        } else {
            Rcpp::Rcerr << "Wrong readable_model argument" << std::endl << "Should be \"hashed\" or \"inverted\"" << std::endl;
            return;
        }
    }
    // Use correct cache file
    if (vwmodel_general_params["cache"]) {
        std::string cache_str = Rcpp::as<std::string>(vwmodel["dir"]) + data_str.substr(data_str.find_last_of("\\/") + 1) + ".cache";
        train_init_str += " --cache_file " + cache_str;
    }
    // Ignore output from VW
    if (!quiet)
    {
        Rcpp::Rcout << "Starting VW training session" << std::endl;
        Rcpp::Rcout << "Using data file: " << data_str << std::endl;
        Rcpp::Rcout << "Using model file: " << model_str << std::endl;
        Rcpp::Rcout << "Command line parameters: " << std::endl << train_init_str << std::endl;
    } else {
        train_init_str += " --quiet";
    }
    
    // For testing
    train_init_str += " --no_stdin";
    
    Rcpp::Rcout << "Before init" << std::endl;
    vw* train_model = VW::initialize(train_init_str);
    Rcpp::Rcout << "After init" << std::endl;
    
    custom_driver(*train_model, data_str);
    
    // Commented for testing
    // VW::start_parser(*train_model);
    // if (!quiet)
    // {
    //     Rcpp::Rcout << "average  since         example        example  current  current  current" << std::endl;
    //     Rcpp::Rcout << "loss     last          counter         weight    label  predict features" << std::endl;
    // }
    // LEARNER::generic_driver(*train_model);
    // VW::end_parser(*train_model);
    
    VW::save_predictor(*train_model, model_str);
    VW::finish(*train_model);
    
    if (!quiet && read_model_file)
    {
        // Reading from temporary files and printing to console
        std::ifstream readable_model_stream (readable_model_str);
        std::string readable_model_line;
        if (readable_model_stream.is_open())
        {
            Rcpp::Rcout << std::endl << "Readable model from file: " + readable_model_str << std::endl;
            while ( getline (readable_model_stream, readable_model_line) )
            {
                Rcpp::Rcout << readable_model_line << std::endl;
            }
            readable_model_stream.close();
            // remove(readable_model_str.c_str());
            Rcpp::Rcout << std::endl;
        }
    }
    
}

//'Compute predictions using Vowpal Wabbit model
//'
//'vwtest computes predictions using VW model from \code{\link{vwsetup}}
//'
//'@param vwmodel Model of vw class to train
//'@param data_path Path to training data in .vw plain text format
//'@param probs_path Path to file where to save predictions
//'@param readable_model Print trained model in human readable format ("hashed") 
//'and also with human readable features ("inverted")
//'@param quiet Do not print anything to the console 
//'@return Numerical vector containing predictions
//'@examples
//'ext_train_data <- system.file("extdata", "binary_train.vw", package = "rvwgsoc")
//'ext_test_data <- system.file("extdata", "binary_valid.vw", package = "rvwgsoc") 
//'test_vwmodel <- vwsetup()
//'vwtrain(test_vwmodel, data_path = ext_train_data)
//'vwtrain(test_vwmodel, data_path = ext_test_data)
// [[Rcpp::export]]
Rcpp::NumericVector vwtest(Rcpp::List vwmodel, std::string data_path="", std::string probs_path = "", Rcpp::Nullable<Rcpp::String> readable_model=R_NilValue, bool quiet=false) {
    // if (!Rcpp::mod.inherits("vw")) Rcpp::stop("Input model is not VW model");
    // Rcpp::Rcout << vwmodel.attr("class") << std::endl;
    std::string data_str = check_data(vwmodel, data_path, "test");
    std::string model_str = Rcpp::as<std::string>(vwmodel["dir"]) + Rcpp::as<std::string>(vwmodel["model"]);
    std::string probs_str;
    std::string readable_model_str = Rcpp::as<std::string>(vwmodel["dir"]) + "readable_" + Rcpp::as<std::string>(vwmodel["model"]);
    Rcpp::List vwmodel_params = vwmodel["params"];
    Rcpp::List vwmodel_general_params = vwmodel_params["general_params"];
    
    // If no probs_path was provided we should create temporary here and then delete
    if (probs_path.empty()) {
        probs_str = Rcpp::as<std::string>(vwmodel["dir"]) + std::to_string(std::time(nullptr)) + "_probs_out.vw";
    } else {
        probs_str = probs_path;
    }
    
    std::string test_init_str = Rcpp::as<std::string>(vwmodel["params_str"]);
    test_init_str += " -t -d " + data_str + " -p " + probs_str + " -i " + model_str;
    
    // Check readable model output mode: hashed features and original features (invert hash)
    bool read_model_file = false;
    if (readable_model.isNotNull()) {
        if (Rcpp::as<std::string>(readable_model) == "hashed")
        {
            test_init_str += " --readable_model " + readable_model_str;
            read_model_file = true;
        } else if (Rcpp::as<std::string>(readable_model) == "inverted")
        {
            if (Rcpp::as<int>(vwmodel_general_params["passes"]) == 1) {
                test_init_str += " --invert_hash " + readable_model_str;
                read_model_file = true;
            } else {
                Rcpp::Rcerr << "Invert hash is not supported with passes > 1" << std::endl;
            }
        } else {
            Rcpp::Rcerr << "Wrong readable_model argument" << std::endl << "Should be \"hashed\" or \"inverted\"" << std::endl;
            Rcpp::NumericVector data_vec(0);
            return data_vec;
        }
    }
    // Use correct cache file
    if (vwmodel_general_params["cache"]) {
        std::string cache_str = Rcpp::as<std::string>(vwmodel["dir"]) + data_str.substr(data_str.find_last_of("\\/") + 1) + ".cache";
        test_init_str += " --cache_file " + cache_str;
    }
    // Ignore output from VW
    if (!quiet)
    {
        Rcpp::Rcout << "Starting VW training session" << std::endl;
        Rcpp::Rcout << "Using data file: " << data_str << std::endl;
        Rcpp::Rcout << "Using model file: " << model_str << std::endl;
        Rcpp::Rcout << "Command line parameters: " << std::endl << test_init_str << std::endl;
    } else {
        test_init_str += " --quiet";
    }
    
    vw* predict_model = VW::initialize(test_init_str);
    
    VW::start_parser(*predict_model);
    if (!quiet)
    {
        Rcpp::Rcout << "average  since         example        example  current  current  current" << std::endl;
        Rcpp::Rcout << "loss     last          counter         weight    label  predict features" << std::endl;
    }
    LEARNER::generic_driver(*predict_model);
    VW::end_parser(*predict_model);
    int num_of_examples = get_num_example(*predict_model);
    VW::finish(*predict_model);
    
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
    // Delete temporary probs file
    if (probs_path.empty()) {
        remove(probs_str.c_str());
    }
    
    if (!quiet && read_model_file)
    {
        // Reading from temporary files and printing to console
        std::ifstream readable_model_stream (readable_model_str);
        std::string readable_model_line;
        if (readable_model_stream.is_open())
        {   
            Rcpp::Rcout << std::endl << "Readable model from file: " + readable_model_str << std::endl;
            while ( getline (readable_model_stream, readable_model_line) )
            {
                Rcpp::Rcout << readable_model_line << std::endl;
            }
            readable_model_stream.close();
            // remove(readable_model_str.c_str());
            Rcpp::Rcout << std::endl;
        }
    }
    
    return data_vec;
}
