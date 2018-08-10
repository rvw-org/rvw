#include <fstream>
#include <iostream>

#include "helpers.h"
#include "vowpalwabbit/vw.h"
#include "vowpalwabbit/global_data.h"
#include <Rcpp.h>

#ifdef _WIN32
#define PATH_SEPARATOR '\\' 
#else 
#define PATH_SEPARATOR '/' 
#endif 



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
            VW::finish_example(*cache_model, *ec);
        } else {
            break;
        }
    }
    if (cache_model->early_terminate) { //drain any extra examples from parser.
        while ((ec = VW::get_example(cache_model->p)) != nullptr) {
            VW::finish_example(*cache_model, *ec);
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
//'@param data [string or data.frame] Path to training data in .vw plain text format or data.frame.
//'If \code{[data.frame]} then will be parsed using \code{df2vw} function.
//'@param readable_model [string] Print trained model in human readable format ("hashed") 
//'and also with human readable features ("inverted")
//'@param quiet [logical] Do not print anything to the console 
//'@param update_model [logical] Update an existing model, when training with new data. \code{FALSE} by default.
//'@param passes [int] Number of times the algorithm will cycle over the data (epochs).
//'@param cache [bool] Use a cache for a data file.
//'@param progress [integer/real] Progress update frequency. int: additive, real: multiplicative
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
//'ext_train_data <- system.file("extdata", "binary_train.vw", package = "rvwgsoc")
//'test_vwmodel <- vwsetup()
//'vwtrain(test_vwmodel, data = ext_train_data)
// [[Rcpp::export]]
void vwtrain(Rcpp::List & vwmodel, SEXP data, Rcpp::Nullable<Rcpp::String> readable_model=R_NilValue, bool quiet=false, bool update_model=false,
             int passes=1, bool cache=false, Rcpp::Nullable<float> progress=R_NilValue,
             Rcpp::Nullable<SEXP *> namespaces=R_NilValue, Rcpp::Nullable<Rcpp::CharacterVector> keep_space=R_NilValue,
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
    Rcpp::String new_data_md5sum = check_data(vwmodel, data_str, data, "train",
                                      namespaces, keep_space,
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
    bool read_model_file = false;
    if (readable_model.isNotNull()) {
        if (Rcpp::as<std::string>(readable_model) == "hashed")
        {
            train_init_str += " --readable_model " + readable_model_str;
            read_model_file = true;
        } else if (Rcpp::as<std::string>(readable_model) == "inverted")
        {
            if (passes == 1) {
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
    // Ignore output from VW
    if (!quiet)
    {
        Rcpp::Rcout << "Starting VW training session" << std::endl;
        Rcpp::Rcout << "VW v" << version.to_string() << std::endl;
        Rcpp::Rcout << "Using data file: " << data_str << std::endl;
        Rcpp::Rcout << "Using model file: " << model_str << std::endl;
        Rcpp::Rcout << "Command line parameters: " << std::endl << train_init_str << std::endl;
    } else {
        train_init_str += " --quiet";
    }
    
    if (progress.isNotNull()) {
        train_init_str += " --progress " + Rcpp::as<std::string>(progress);
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
        if(read_model_file){
            // Reading from temporary files and printing to console
            std::ifstream readable_model_stream (readable_model_str);
            std::string readable_model_line;
            if (readable_model_stream.is_open())
            {
                Rcpp::Rcout << std::endl << "Readable model from file: " + readable_model_str << std::endl;
                while(getline (readable_model_stream, readable_model_line))
                {
                    Rcpp::Rcout << readable_model_line << std::endl;
                }
                readable_model_stream.close();
                // remove(readable_model_str.c_str());
                Rcpp::Rcout << std::endl;
            }
        }
    }
    
}

//'Compute predictions using Vowpal Wabbit model
//'
//'\code{vwtest} computes predictions using VW model from \code{\link{vwsetup}}
//'\code{predict.vw} compute predictions using parser settings from \code{\link{vwtrain}}
//'
//'@param vwmodel Model of vw class to train.
//'@param object Model of vw class to train for \code{predict.vw}
//'@param data [string or data.frame] Path to training data in .vw plain text format or data.frame.
//'If \code{[data.frame]} then will be parsed using \code{df2vw} function.
//'@param probs_path [string] Path to file where to save predictions.
//'@param readable_model [string] Print trained model in human readable format ("hashed") 
//'and also with human readable features ("inverted").
//'@param quiet [bool] Do not print anything to the console.
//'@param passes [int] Number of times the algorithm will cycle over the data (epochs).
//'@param cache [bool] Use a cache for a data file.
//'@param raw [bool] Output unnormalized predictions. Default is FALSE.
//'@param progress [integer/real] Progress update frequency. int: additive, real: multiplicative
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
//'ext_train_data <- system.file("extdata", "binary_train.vw", package = "rvwgsoc")
//'ext_test_data <- system.file("extdata", "binary_valid.vw", package = "rvwgsoc") 
//'test_vwmodel <- vwsetup()
//'vwtrain(test_vwmodel, data = ext_train_data)
//'vwtest(test_vwmodel, data = ext_test_data)
//'@rdname vwtest
// [[Rcpp::export]]
Rcpp::NumericVector vwtest(Rcpp::List & vwmodel, SEXP data, std::string probs_path = "", Rcpp::Nullable<Rcpp::String> readable_model=R_NilValue, bool quiet=false,
                           int passes=1, bool cache=false, bool raw=false, Rcpp::Nullable<float> progress=R_NilValue,
                           Rcpp::Nullable<SEXP *> namespaces=R_NilValue, Rcpp::Nullable<Rcpp::CharacterVector> keep_space=R_NilValue,
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
    Rcpp::String data_md5sum = check_data(vwmodel, data_str, data, "test",
                                      namespaces, keep_space,
                                      targets, probabilities,
                                      weight, base, tag, multiline);
    
    Rcpp::List vwmodel_md5sums = vwmodel["data_md5sum"];
    Rcpp::List vwmodel_eval = vwmodel["eval"];
    std::string model_dir = Rcpp::as<std::string>(vwmodel["dir"]) + PATH_SEPARATOR;
    std::string model_str = model_dir + Rcpp::as<std::string>(vwmodel["model"]);
    std::string probs_str;
    std::string readable_model_str = model_dir + "readable_" + Rcpp::as<std::string>(vwmodel["model"]);
    Rcpp::List vwmodel_params = vwmodel["params"];
    Rcpp::List vwmodel_general_params = vwmodel_params["general_params"];
    
    // If no probs_path was provided we should create temporary here and then delete
    if (probs_path.empty()) {
        // probs_str = model_dir + std::to_string(std::time(nullptr)) + "_probs_out.vw";
        probs_str = model_dir + "temp_probs_out.vw";
    } else {
        probs_str = probs_path;
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
    bool read_model_file = false;
    if (readable_model.isNotNull()) {
        if (Rcpp::as<std::string>(readable_model) == "hashed")
        {
            test_init_str += " --readable_model " + readable_model_str;
            read_model_file = true;
        } else if (Rcpp::as<std::string>(readable_model) == "inverted")
        {
            if (passes == 1) {
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
    // Ignore output from VW
    if (!quiet)
    {
        Rcpp::Rcout << "Starting VW testing session" << std::endl;
        Rcpp::Rcout << "VW v" << version.to_string() << std::endl;
        Rcpp::Rcout << "Using data file: " << data_str << std::endl;
        Rcpp::Rcout << "Using model file: " << model_str << std::endl;
        Rcpp::Rcout << "Command line parameters: " << std::endl << test_init_str << std::endl;
    } else {
        test_init_str += " --quiet";
    }
    
    if (progress.isNotNull()) {
        test_init_str += " --progress " + Rcpp::as<std::string>(progress);
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
    
    if (!quiet)
    {
        if(read_model_file) {
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
    
    return data_vec;
}


//'Audit Vowpal Wabbit model
//'
//'Get feature names and their model values. 
//'
//'@param vwmodel Model of vw class to train
//'@return Data.frame containing feature names, feature hashes and model values
//'@examples
//'ext_train_data <- system.file("extdata", "binary_train.vw", package = "rvwgsoc")
//'test_vwmodel <- vwsetup()
//'vwtrain(test_vwmodel, data = ext_train_data)
//'vwaudit(test_vwmodel)
// [[Rcpp::export]]
Rcpp::DataFrame vwaudit(Rcpp::List & vwmodel) {
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
        
        std::string test_init_str = "-d " + data_str + " --audit_regressor " + audit_str + " -i " + model_str;
        vw* aud_model = VW::initialize(test_init_str);
        VW::start_parser(*aud_model);
        LEARNER::generic_driver(*aud_model);
        VW::end_parser(*aud_model);
        VW::finish(*aud_model);
        
        std::ifstream audit_stream (audit_str);
        std::string audit_line;
        std::stringstream audit_line_stream;
        std::string audit_line_elem;
        std::vector<std::string> splitted_line;

        Rcpp::CharacterVector feature_names;
        Rcpp::NumericVector feature_hashes;
        Rcpp::NumericVector model_values;
        
        if (audit_stream.is_open())
        {   
            // Split lines from audit file and write results to vectors
            while ( getline(audit_stream, audit_line) )
            {
                std::stringstream().swap(audit_line_stream);
                audit_line_stream << audit_line;
                while( getline(audit_line_stream, audit_line_elem, ':') )
                {
                    splitted_line.push_back(audit_line_elem);
                }
                feature_names.push_back(splitted_line[0]);
                feature_hashes.push_back(std::stof(splitted_line[1]));
                model_values.push_back(std::stof(splitted_line[2]));
                
                splitted_line.clear();

            }
            audit_stream.close();
            
            Rcpp::DataFrame audit_output = Rcpp::DataFrame::create(Rcpp::Named("Names") = feature_names,
                                                                   Rcpp::Named("Hashes") = feature_hashes,
                                                                   Rcpp::Named("Model.values") = model_values);
            return(audit_output);
            
        } else {
            Rcpp::stop("Something's wrong with the audit file");
        }
    } else {
        Rcpp::stop("No training datafile avaliable");
    }
}
