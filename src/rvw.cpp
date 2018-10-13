#include <fstream>
#include <iostream>

#include "vw.h"
#include "global_data.h"

#include "helpers.h"
#include <Rcpp.h>

#ifdef _WIN32
#define PATH_SEPARATOR '\\' 
#else 
#define PATH_SEPARATOR '/' 
#endif 


// [[Rcpp::export(".get_vw_version")]]
std::string get_vw_version() {
    return version.to_string();
}

// // [[Rcpp::export(".create_cache")]]
// void create_cache(std::string dir="", std::string data_file="", std::string cache_file="") {
//     std::string data_str = dir + data_file;
//     std::string cache_str = dir + cache_file;
//     
//     std::string cache_init_str = "-d " + data_str + " --cache_file " + cache_str;
//     vw* cache_model = VW::initialize(cache_init_str);
//     VW::start_parser(*cache_model);
//     
//     // Modified version of LEARNER::generic_driver that only creates cache and doesn't train
//     example* ec = nullptr;
//     while (cache_model->early_terminate == false) {
//         if ((ec = VW::get_example(cache_model->p)) != nullptr) {
//             VW::finish_example(*cache_model, *ec);
//         } else {
//             break;
//         }
//     }
//     if (cache_model->early_terminate) { //drain any extra examples from parser.
//         while ((ec = VW::get_example(cache_model->p)) != nullptr) {
//             VW::finish_example(*cache_model, *ec);
//         }
//     } 
//     VW::end_parser(*cache_model);
//     VW::finish(*cache_model);
// }

//'Train Vowpal Wabbit model
//'
//'vwtrain is an interface to train VW model from \code{\link{vwsetup}}
//'
//'@param vwmodel [vw] Model of vw class to train
//'@param data [string or data.frame] Path to training data in .vw plain text format or data.frame.
//'If \code{[data.frame]} then will be parsed using \code{df2vw} function.
//'@param readable_model [string] Print trained model in human readable format ("hashed") 
//'and also with human readable features ("inverted")
//'@param readable_model_path [string] Path to file where to save readable model.
//'@param quiet [logical] Do not print anything to the console 
//'@param update_model [logical] Update an existing model, when training with new data. \code{FALSE} by default.
//'@param passes [int] Number of times the algorithm will cycle over the data (epochs).
//'@param cache [bool] Use a cache for a data file.
//'@param progress [int/real] Progress update frequency. int: additive, real: multiplicative
//'@param namespaces [list or yaml file] For \code{df2vw}. Name of each namespace and
//'  each variable for each namespace can be a R list, or a YAML
//'  file example namespace with the IRIS database: namespaces =
//'  list(sepal = list('Sepal.Length', 'Sepal.Width'), petal = list('Petal.Length',
//'  'Petal.Width') this creates 2 namespaces (sepal
//'  and petal) containing the features defined by elements of this lists.
//'@param keep_space [string vector] For \code{df2vw}. Keep spaces for this features
//'Example:"FERRARI 4Si"
//'With \code{keep_space} will be "FERRARI 4Si" and will be treated as two features
//'Without \code{keep_space} will be "FERRARI_4Si" and will be treated as one feature
//'@param fixed [string vector] fixed parsing for this features
//'Similar to \code{keep_space}, but parse features exactly without replacement of special characters ("(", ")", "|", ":", "'").
//'Can be used for LDA ("word_1:2 word_2:3" will stay the same),
//'but should be used carefully, because special characters can ruin final VW format file.
//'@param targets [string or string vector] For \code{df2vw}.
//'If \code{[string]} then will be treated as vector with real number labels for regular VW input format. 
//'If \code{[string vector]} then will be treated as vectors with class costs for wap and csoaa 
//'multi-class classification algorithms or as vectors with actions for Contextual Bandit algorithm. 
//'@param probabilities [string vector] For \code{df2vw}. vectors with action probabilities for Contextual Bandit algorithm.
//'@param weight [string] For \code{df2vw}. Weight (importance) of each line of the dataset.
//'@param base [string] For \code{df2vw}. base of each line of the dataset. Used for residual regression.
//'@param tag [string] For \code{df2vw}. Tag of each line of the dataset.
//'@param multiline [integer] number of labels (separate lines) for multilines examle
//'@import tools
//'@examples
//'ext_train_data <- system.file("extdata", "binary_train.vw", package = "rvw")
//'test_vwmodel <- vwsetup()
//'vwtrain(test_vwmodel, data = ext_train_data)
// [[Rcpp::export]]
void vwtrain(Rcpp::List & vwmodel, SEXP data, Rcpp::Nullable<Rcpp::String> readable_model=R_NilValue, std::string readable_model_path = "",
             bool quiet=false, bool update_model=false, int passes=1, bool cache=false, Rcpp::Nullable<SEXP *> progress=R_NilValue,
             Rcpp::Nullable<SEXP *> namespaces=R_NilValue, Rcpp::Nullable<Rcpp::CharacterVector> keep_space=R_NilValue,
             Rcpp::Nullable<Rcpp::CharacterVector> fixed=R_NilValue,
             Rcpp::Nullable<Rcpp::CharacterVector> targets=R_NilValue, Rcpp::Nullable<Rcpp::CharacterVector> probabilities=R_NilValue,
             Rcpp::Nullable<Rcpp::String> weight=R_NilValue, Rcpp::Nullable<Rcpp::String> base=R_NilValue,
             Rcpp::Nullable<Rcpp::String> tag=R_NilValue, Rcpp::Nullable<int> multiline=R_NilValue) {
    // vwmodel should be of class vw
    if(!Rf_inherits(vwmodel, "vw")) {
        Rcpp::stop("vwmodel should be of class vw");
    }
    
    // check if data is in string or data.frame format
    // if in data.frame format then convert it to VW format
    std::string data_str = "";
    Rcpp::String new_data_md5sum = check_data(vwmodel, data_str, data, quiet, "train",
                                              namespaces, keep_space, fixed,
                                              targets, probabilities,
                                              weight, base, tag, multiline);
    // Update train data file name
    vwmodel["train_file"] = data_str;
    
    Rcpp::List vwmodel_md5sums = vwmodel["data_md5sum"];
    Rcpp::List vwmodel_eval = vwmodel["eval"];
    std::string model_dir = Rcpp::as<std::string>(vwmodel["dir"]) + PATH_SEPARATOR;
    std::string model_str = model_dir + Rcpp::as<std::string>(vwmodel["model"]);
    std::string readable_model_str = model_dir + "readable_" + Rcpp::as<std::string>(vwmodel["model"]);
    Rcpp::List vwmodel_params = vwmodel["params"];
    Rcpp::List vwmodel_general_params = vwmodel_params["general_params"];
    
    // If no readable_model_path was provided we should create temporary here and then delete
    if (readable_model_path.empty()) {
        readable_model_str = model_dir + "readable_" + Rcpp::as<std::string>(vwmodel["model"]);
    } else {
        // Check readable_model_path for whitespace
        if(readable_model_path.find_first_of("\t\n ") != readable_model_path.npos) {
            Rcpp::stop("Whitespace characters are not allowed in readable_model_path");
        }
        readable_model_str = readable_model_path;
    }
    
    std::string train_init_str = Rcpp::as<std::string>(vwmodel["params_str"]);
    train_init_str += " -d " + data_str + " -f " + model_str + " --passes " + std::to_string(passes);
    
    // Cache should be created, if passes > 1
    if(passes > 1) {
        cache = true;
    }
    
    // Use existing train file to continue training
    if (update_model) {
        if(file_exists(model_str)) {
            std::ifstream model_file(model_str);
            if (model_file.good()) {
                // Drop model parameters because they are recorded in model file
                train_init_str = " -d " + data_str + " -f " + model_str + " --passes " + std::to_string(passes) + " -i " + model_str + " --save_resume";
            } else {
                Rcpp::Rcerr << "Something is wrong with the model file" << std::endl;
            }
        } else {
            train_init_str += " --save_resume";
        }
    }
    
    // Check readable model output mode: hashed features and original features (invert hash)
    if (readable_model.isNotNull()) {
        if (Rcpp::as<std::string>(readable_model) == "hashed")
        {
            train_init_str += " --readable_model " + readable_model_str;
        } else if (Rcpp::as<std::string>(readable_model) == "inverted")
        {
            if (passes == 1) {
                train_init_str += " --invert_hash " + readable_model_str;
            } else {
                Rcpp::stop("Invert hash is not supported with passes > 1");
            }
        } else {
            Rcpp::stop("Wrong readable_model argument. Should be \"hashed\" or \"inverted\"");
            return;
        }
    }
    // Use correct cache file
    if (cache) {
        Rcpp::String model_md5sum = vwmodel_md5sums["train"];
        if(model_md5sum != new_data_md5sum) {
            // kill cache if data is in data.frame format because we need to convert it first and then prepare new cache
            train_init_str += " --kill_cache";
        }
        std::string cache_str = model_dir + data_str.substr(data_str.find_last_of("\\/") + 1) + ".cache";
        train_init_str += " --cache_file " + cache_str;
    }
    // Update model_md5sum
    vwmodel_md5sums["train"] = new_data_md5sum;
    
    if (progress.isNotNull()) {
        if(TYPEOF(progress) == INTSXP) {
            train_init_str += " --progress " + std::to_string(Rcpp::as<int>(progress));
        } else if(TYPEOF(progress) == REALSXP) {
            train_init_str += " --progress " + std::to_string(Rcpp::as<float>(progress));
        } else {
            Rcpp::stop("Wrong type for progress argument (should be integer or real)");
        }
    }
    
    // Ignore output from VW
    if (!quiet)
    {
        Rcpp::Rcout << "Starting VW training session" << std::endl;
        Rcpp::Rcout << "VW v" << get_vw_version() << std::endl;
        Rcpp::Rcout << "Using data file: " << data_str << std::endl;
        Rcpp::Rcout << "Using model file: " << model_str << std::endl;
        Rcpp::Rcout << "Command line parameters: " << std::endl << train_init_str << std::endl;
    } else {
        train_init_str += " --quiet";
    }
    
    // Start VW run
    vw* train_model = setup_model(train_init_str);
    VW::start_parser(*train_model);
    LEARNER::generic_driver(*train_model);
    VW::end_parser(*train_model);
    Rcpp::List eval_list = get_eval(*train_model);
    VW::finish(*train_model);
    
    // Update eval list
    vwmodel_eval["train"] = eval_list;
    
    if (!quiet)
    {
        if (readable_model_path.empty()){
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
                // Delete temporary readable_mode file
                remove(readable_model_str.c_str());
                Rcpp::Rcout << std::endl;
            }
        } else {
            Rcpp::Rcout << "Readable model is written to file: " + readable_model_str << std::endl;
        }
    }
    
}

//'Compute predictions using Vowpal Wabbit model
//'
//'\code{vwtest} computes predictions using VW model from \code{\link{vwsetup}}
//'\code{predict.vw} compute predictions using parser settings from \code{\link{vwtrain}}
//'
//'@param vwmodel [vw] Model of vw class to train.
//'@param object Model of vw class to train for \code{predict.vw}
//'@param data [string or data.frame] Path to training data in .vw plain text format or data.frame.
//'If \code{[data.frame]} then will be parsed using \code{df2vw} function.
//'@param probs_path [string] Path to file where to save predictions.
//'@param full_probs [bool] Output full predictions in data.frame format. If not, force predictions into a single vector (default).
//'@param readable_model [string] Print trained model in human readable format ("hashed") 
//'and also with human readable features ("inverted").
//'@param readable_model_path [string] Path to file where to save readable model.
//'@param quiet [bool] Do not print anything to the console.
//'@param passes [int] Number of times the algorithm will cycle over the data (epochs).
//'@param cache [bool] Use a cache for a data file.
//'@param raw [bool] Output unnormalized predictions. Default is FALSE.
//'@param progress [int/real] Progress update frequency. int: additive, real: multiplicative
//'@param namespaces [list or yaml file] For \code{df2vw}. Name of each namespace and
//'  each variable for each namespace can be a R list, or a YAML
//'  file example namespace with the IRIS database: namespaces =
//'  list(sepal = list('Sepal.Length', 'Sepal.Width'), petal = list('Petal.Length',
//'  'Petal.Width') this creates 2 namespaces (sepal
//'  and petal) containing the features defined by elements of this lists.
//'@param keep_space [string vector] For \code{df2vw}. Keep spaces for this features
//'Example:"FERRARI 4Si"
//'With \code{keep_space} will be "FERRARI 4Si" and will be treated as two features
//'Without \code{keep_space} will be "FERRARI_4Si" and will be treated as one feature
//'@param fixed [string vector] fixed parsing for this features
//'Similar to \code{keep_space}, but parse features exactly without replacement of special characters ("(", ")", "|", ":", "'").
//'Can be used for LDA ("word_1:2 word_2:3" will stay the same),
//'but should be used carefully, because special characters can ruin final VW format file.
//'@param targets [string or string vector] For \code{df2vw}.
//'If \code{[string]} then will be treated as vector with real number labels for regular VW input format. 
//'If \code{[string vector]} then will be treated as vectors with class costs for wap and csoaa 
//'multi-class classification algorithms or as vectors with actions for Contextual Bandit algorithm. 
//'@param probabilities [string vector] For \code{df2vw}. Vectors with action probabilities for Contextual Bandit algorithm.
//'@param weight [string] For \code{df2vw}. Weight (importance) of each line of the dataset.
//'@param base [string] For \code{df2vw}. Base of each line of the dataset. Used for residual regression.
//'@param tag [string] For \code{df2vw}. Tag of each line of the dataset.
//'@param multiline [integer] Number of labels (separate lines) for multilines example
//'@param ... Parameters passed to \code{predict.vw}
//'@return Numerical vector containing predictions
//'@import tools
//'@examples
//'ext_train_data <- system.file("extdata", "binary_train.vw", package = "rvw")
//'ext_test_data <- system.file("extdata", "binary_valid.vw", package = "rvw") 
//'test_vwmodel <- vwsetup()
//'vwtrain(test_vwmodel, data = ext_train_data)
//'vwtest(test_vwmodel, data = ext_test_data)
//'@rdname vwtest
// [[Rcpp::export]]
SEXP vwtest(Rcpp::List & vwmodel, SEXP data, std::string probs_path="", bool full_probs=false, Rcpp::Nullable<Rcpp::String> readable_model=R_NilValue, std::string readable_model_path = "",
                           bool quiet=false, int passes=1, bool cache=false, bool raw=false, Rcpp::Nullable<SEXP *> progress=R_NilValue,
                           Rcpp::Nullable<SEXP *> namespaces=R_NilValue, Rcpp::Nullable<Rcpp::CharacterVector> keep_space=R_NilValue,
                           Rcpp::Nullable<Rcpp::CharacterVector> fixed=R_NilValue,
                           Rcpp::Nullable<Rcpp::CharacterVector> targets=R_NilValue, Rcpp::Nullable<Rcpp::CharacterVector> probabilities=R_NilValue,
                           Rcpp::Nullable<Rcpp::String> weight=R_NilValue, Rcpp::Nullable<Rcpp::String> base=R_NilValue,
                           Rcpp::Nullable<Rcpp::String> tag=R_NilValue, Rcpp::Nullable<int> multiline=R_NilValue) {
    // vwmodel should be of class vw
    if(!Rf_inherits(vwmodel, "vw")) {
        Rcpp::stop("vwmodel should be of class vw");
    }
    // check if data is in string or data.frame format
    // if in data.frame format then convert it to VW format
    std::string data_str = "";
    Rcpp::String data_md5sum = check_data(vwmodel, data_str, data, quiet, "test",
                                          namespaces, keep_space, fixed,
                                          targets, probabilities,
                                          weight, base, tag, multiline);
    
    Rcpp::List vwmodel_md5sums = vwmodel["data_md5sum"];
    Rcpp::List vwmodel_eval = vwmodel["eval"];
    std::string model_dir = Rcpp::as<std::string>(vwmodel["dir"]) + PATH_SEPARATOR;
    std::string model_str = model_dir + Rcpp::as<std::string>(vwmodel["model"]);
    std::string probs_str;
    std::string readable_model_str;
    Rcpp::List vwmodel_params = vwmodel["params"];
    Rcpp::List vwmodel_general_params = vwmodel_params["general_params"];
    
    // If no probs_path was provided we should create temporary here and then delete
    if (probs_path.empty()) {
        // probs_str = model_dir + std::to_string(std::time(nullptr)) + "_probs_out.vw";
        probs_str = model_dir + "temp_probs_out.vw";
    } else {
        // Check probs_path for whitespace
        if(probs_path.find_first_of("\t\n ") != probs_path.npos) {
            Rcpp::stop("Whitespace characters are not allowed in probs_path");
        }
        probs_str = probs_path;
    }
    // The same for readable_model_path
    if (readable_model_path.empty()) {
        readable_model_str = model_dir + "readable_" + Rcpp::as<std::string>(vwmodel["model"]);
    } else {
        // Check readable_model_path for whitespace
        if(readable_model_path.find_first_of("\t\n ") != readable_model_path.npos) {
            Rcpp::stop("Whitespace characters are not allowed in readable_model_path");
        }
        readable_model_str = readable_model_path;
    }
    
    std::string test_init_str;
    // std::string test_init_str = Rcpp::as<std::string>(vwmodel["params_str"]);
    test_init_str += " -t -d " + data_str + " -i " + model_str + " --passes " + std::to_string(passes);
    
    // Normalized predictions (default)
    if (!raw) {
        test_init_str += " -p " + probs_str;
    } else {
        test_init_str += " -r " + probs_str;
    }
    
    // Cache should be created, if passes > 1
    if(passes > 1) {
        cache = true;
    }
    
    // Check readable model output mode: hashed features and original features (invert hash)
    if (readable_model.isNotNull()) {
        if (Rcpp::as<std::string>(readable_model) == "hashed")
        {
            test_init_str += " --readable_model " + readable_model_str;
        } else if (Rcpp::as<std::string>(readable_model) == "inverted")
        {
            if (passes == 1) {
                test_init_str += " --invert_hash " + readable_model_str;
            } else {
                Rcpp::stop("Invert hash is not supported with passes > 1");
            }
        } else {
            Rcpp::stop("Wrong readable_model argument. Should be \"hashed\" or \"inverted\"");
            Rcpp::NumericVector data_vec(0);
            return data_vec;
        }
    }
    // Use correct cache file
    if (cache) {
        Rcpp::String model_md5sum = vwmodel_md5sums["test"];
        if(model_md5sum != data_md5sum) {
            // kill cache if data is in data.frame format because we need to convert it first and then prepare new cache
            test_init_str += " --kill_cache";
        }
        std::string cache_str = model_dir + data_str.substr(data_str.find_last_of("\\/") + 1) + ".cache";
        test_init_str += " --cache_file " + cache_str;
    }
    // Update model_md5sum
    vwmodel_md5sums["test"] = data_md5sum;
    
    if (progress.isNotNull()) {
        if(TYPEOF(progress) == INTSXP) {
            test_init_str += " --progress " + std::to_string(Rcpp::as<int>(progress));
        } else if(TYPEOF(progress) == REALSXP) {
            test_init_str += " --progress " + std::to_string(Rcpp::as<float>(progress));
        } else {
            Rcpp::stop("Wrong type for progress argument (should be integer or real)");
        }
    }
    
    // Ignore output from VW
    if (!quiet)
    {
        Rcpp::Rcout << "Starting VW testing session" << std::endl;
        Rcpp::Rcout << "VW v" << get_vw_version() << std::endl;
        Rcpp::Rcout << "Using data file: " << data_str << std::endl;
        Rcpp::Rcout << "Using model file: " << model_str << std::endl;
        Rcpp::Rcout << "Command line parameters: " << std::endl << test_init_str << std::endl;
    } else {
        test_init_str += " --quiet";
    }
    
    // Start VW run
    vw* test_model = setup_model(test_init_str);
    VW::start_parser(*test_model);
    LEARNER::generic_driver(*test_model);
    VW::end_parser(*test_model);
    int num_of_examples = get_num_example(*test_model);
    Rcpp::List eval_list = get_eval(*test_model);
    VW::finish(*test_model);
    
    // Update eval list
    vwmodel_eval["test"] = eval_list;
    
    if (!quiet)
    {
        if (readable_model_path.empty()){
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
                // Delete temporary readable_mode file
                remove(readable_model_str.c_str());
                Rcpp::Rcout << std::endl;
            }
        } else {
            Rcpp::Rcout << "Readable model is written to file: " + readable_model_str << std::endl;
        }
    }
    
    // Write predictions to vector / data.frame
    std::ifstream probs_stream (probs_str);
    std::string probs_line;
    std::vector<std::string> split_line;
    
    // std::vector<double> data_vec(num_of_examples);
    if (probs_stream.is_open())
    {
        // Check first line for number of elements
        getline(probs_stream, probs_line);
        split_line = split_str(probs_line, ' ');
        int preds_num_col = split_line.size();
        // Rcpp::Rcout << preds_num_col << std::endl;
        
        if (!full_probs) 
        {
            // Default mode
            // Single vector output
            Rcpp::NumericVector preds_vec(num_of_examples);
            preds_vec[0] = std::stof(split_line[0]);
            if (preds_num_col > 1) 
            {
                Rcpp::Rcerr << "Warning: Predictions contain multiple elements per example." 
                            << "\nTruncated results obtained. Use 'full_probs=T' for non-truncated results."
                            << std::endl;
            }
            
            for (int i = 1; i < num_of_examples; ++i) 
            {
                getline(probs_stream, probs_line);
                if (!probs_line.empty())
                {
                    preds_vec[i] = std::stof(probs_line);
                } else {
                    preds_vec[i] = R_NaReal;
                }
            }
            
            probs_stream.close();
            
            // Delete temporary probs file
            if (probs_path.empty()) {
                remove(probs_str.c_str());
            }
            
            return preds_vec;
            
        } else {
            // Full mode
            // Data.frame output
            std::vector<std::vector<std::string>> pred_vectors(preds_num_col);
            
            // // For string to float conversion
            // std::stringstream ss;
            // float float_elem;
            
            // Write elements from the first line
            for (int i = 0; i < preds_num_col; i++) {
                pred_vectors[i].push_back(split_line[i]);
            }
            
            for (int i = 1; i < num_of_examples; ++i) 
            {
                getline(probs_stream, probs_line);
                if (!probs_line.empty())
                {
                    split_line = split_str(probs_line, ' ');
                    for (int j = 0; j < preds_num_col; j++) {
                        
                        // // Try to convert element to float
                        // float_elem =  std::numeric_limits<float>::quiet_NaN();
                        // ss << split_line[j];
                        // ss >> float_elem;
                        // 
                        // if(std::isnan(float_elem)) {
                        //     pred_vectors[j].push_back(float_elem);
                        // } else {
                        //     pred_vectors[j].push_back(split_line[j]);
                        // }
                        
                        pred_vectors[j].push_back(split_line[j]);
                    }
                } else {
                    for (int j = 0; j < preds_num_col; j++) {
                        pred_vectors[j].push_back("");
                    }
                }
            }
            
            probs_stream.close();
            
            // Delete temporary probs file
            if (probs_path.empty()) {
                remove(probs_str.c_str());
            }
            
            // Temporary list (and its names) for DataFrame constructor
            Rcpp::List tmp_preds_output(preds_num_col);
            Rcpp::CharacterVector column_names(preds_num_col);
            
            for (int i = 0; i < preds_num_col; i++) {
                tmp_preds_output[i] = pred_vectors[i];
                column_names[i] =  "V" + std::to_string(i + 1);
            }
            
            // Rcpp::DataFrame preds_output(tmp_preds_output);
            Rcpp::DataFrame preds_output = Rcpp::DataFrame::create(tmp_preds_output,
                                                                   Rcpp::Named("stringsAsFactors") = false);
            preds_output.attr("names") = column_names;
            
            return(preds_output);
            
        }
    } else {
        Rcpp::stop("Predictions file can not be opened");
    }
    
}


//'Audit Vowpal Wabbit model
//'
//'Get feature names and their model values. 
//'
//'@param vwmodel Model of vw class to train
//'@param quiet [bool] Do not print anything to the console.
//'@return Data.frame containing feature names, feature hashes and model values
//'@examples
//'ext_train_data <- system.file("extdata", "binary_train.vw", package = "rvw")
//'test_vwmodel <- vwsetup()
//'vwtrain(test_vwmodel, data = ext_train_data)
//'vwaudit(test_vwmodel)
// [[Rcpp::export]]
Rcpp::DataFrame vwaudit(Rcpp::List & vwmodel, bool quiet = false) {
    // vwmodel should be of class vw
    if(!Rf_inherits(vwmodel, "vw")) {
        Rcpp::stop("vwmodel should be of class vw");
    }
    
    // Initialize file names
    std::string model_dir = Rcpp::as<std::string>(vwmodel["dir"]) + PATH_SEPARATOR;
    std::string data_str = Rcpp::as<std::string>(vwmodel["train_file"]);
    std::string audit_str = model_dir + "aud.vw";
    std::string model_str = model_dir + Rcpp::as<std::string>(vwmodel["model"]);
    
    // Reading from audit file and write results to data.frame
    if(file_exists(data_str)){
        // std::string aud_init_str = Rcpp::as<std::string>(vwmodel["params_str"]);
        std::string aud_init_str = "-d " + data_str + " --audit_regressor " + audit_str + " -i " + model_str;
        if (quiet) {
            aud_init_str += " --quiet";
        }
        vw* aud_model = VW::initialize(aud_init_str);
        VW::start_parser(*aud_model);
        LEARNER::generic_driver(*aud_model);
        VW::end_parser(*aud_model);
        VW::finish(*aud_model);
        
        std::ifstream audit_stream (audit_str);
        std::string audit_line;
        std::vector<std::string> splitted_line;
        
        int num_val_columns = 1;
        std::vector<std::string> feature_names;
        std::vector<float> feature_hashes;
        
        if (audit_stream.is_open())
        {   
            // Split lines from audit file and write results to vectors
            // Also get the number of elements from the first row
            getline(audit_stream, audit_line);
            splitted_line = split_str(audit_line, ':');
            // Number of feature value columns in the model
            num_val_columns = splitted_line.size() - 2;
            std::vector<std::vector<float>> model_values(num_val_columns);
            
            // Push line elements to vectors
            feature_names.push_back(splitted_line[0]);
            feature_hashes.push_back(std::stof(splitted_line[1]));
            for (int i = 0; i < num_val_columns; i++) {
                model_values[i].push_back(std::stof(splitted_line[i + 2]));
            }
            
            while ( getline(audit_stream, audit_line) )
            {
                splitted_line = split_str(audit_line, ':');
                feature_names.push_back(splitted_line[0]);
                feature_hashes.push_back(std::stof(splitted_line[1]));
                for (int i = 0; i < num_val_columns; i++) {
                    model_values[i].push_back(std::stof(splitted_line[i + 2]));
                }
            }
            audit_stream.close();
            
            // Temporary list (and its names) for DataFrame constructor
            Rcpp::List tmp_audit_output(num_val_columns + 2);
            Rcpp::CharacterVector column_names(num_val_columns + 2);
            
            tmp_audit_output[0] = feature_names;
            column_names[0] = "Names";
            tmp_audit_output[1] = feature_hashes;
            column_names[1] = "Hashes";
            for (int i = 0; i < num_val_columns; i++) {
                tmp_audit_output[i + 2] = model_values[i];
                column_names[i + 2] = "V" + std::to_string(i + 1);
            }
            
            Rcpp::DataFrame audit_output(tmp_audit_output);
            audit_output.attr("names") = column_names;
            
            return(audit_output);
            
        } else {
            Rcpp::stop("Something's wrong with the audit file");
        }
    } else {
        Rcpp::stop("No training datafile avaliable");
    }
}
