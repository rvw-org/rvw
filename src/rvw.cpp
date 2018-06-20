#include <fstream>

#include "helpers.h"
#include "vowpalwabbit/vw.h"
#include <Rcpp.h>


// [[Rcpp::export(".create_cache")]]
void create_cache(std::string dir="", std::string data_file="", std::string cache_file="") {
    std::string data_str = dir + data_file;
    std::string cache_str = dir + cache_file;
    
    // Rcpp::Rcout << "Creating cache from: " << data_str << std::endl;
    
    // Redirect cerr
    // std::stringstream new_buf;
    // auto old_buf = std::cerr.rdbuf(new_buf.rdbuf());
    
    std::string cache_init_str = "-d " + data_str + " -c --cache_file " + cache_str;
    vw* cache_model = VW::initialize(cache_init_str);
    VW::start_parser(*cache_model);
    
    // Modified version of LEARNER::generic_driver that only creates cache and doesn't train
    example* ec = nullptr;
    while ( cache_model->early_terminate == false )
        if ((ec = VW::get_example(cache_model->p)) != nullptr) {
            VW::finish_example(*cache_model, ec);
        } else {
            break;
        }
        if (cache_model->early_terminate) //drain any extra examples from parser.
            while ((ec = VW::get_example(cache_model->p)) != nullptr)
                VW::finish_example(*cache_model, ec);
        VW::end_parser(*cache_model);
        VW::finish(*cache_model);
        
        // std::cerr.rdbuf(old_buf);
        
        // Rcpp::Rcout << "Cache file created: " << cache_str << std::endl;
        
        
}

// [[Rcpp::export]]
void vwtrain(Rcpp::List vwmodel, std::string data_path="", Rcpp::Nullable<Rcpp::String> readable_model=R_NilValue, bool quiet=false) {
    // if (!Rcpp::mod.inherits("vw")) Rcpp::stop("Input model is not VW model");
    // Rcpp::Rcout << vwmodel.attr("class") << std::endl;
    std::string data_str = check_data(vwmodel, data_path, "train");
    std::string model_str = Rcpp::as<std::string>(vwmodel["dir"]) + Rcpp::as<std::string>(vwmodel["model"]);
    std::string readable_model_str = Rcpp::as<std::string>(vwmodel["dir"]) + "readable_" + Rcpp::as<std::string>(vwmodel["model"]);
    
    // Output buffer to control "quiet" function execution easily
    std::stringstream output_buf;
    
    output_buf << "Starting VW training session" << std::endl;
    output_buf << "Using data file: " << data_str << std::endl;
    output_buf << "Using model file: " << model_str << std::endl;
    
    std::string train_init_str = Rcpp::as<std::string>(vwmodel["params_str"]);
    train_init_str += " -d " + data_str;
    
    // Check readable model output mode: hashed features and original features (invert hash)
    if (readable_model.isNotNull()) {
        if (Rcpp::as<std::string>(readable_model) == "hashed")
        {
            train_init_str += " --readable_model " + readable_model_str;
        } else if (Rcpp::as<std::string>(readable_model) == "inverted")
        {
            train_init_str += " --invert_hash " + readable_model_str;
        } else {
            Rcpp::Rcerr << "Wrong readable_model argument" << std::endl << "Should be \"hashed\" or \"inverted\"" << std::endl;
            return;
        }
    }

    // Ignore output from VW
    if (quiet)
    {
        train_init_str += " --quiet";
    }
    
    output_buf << "Command line parameters: " << std::endl << train_init_str << std::endl;
    
    vw* train_model = VW::initialize(train_init_str);
    VW::start_parser(*train_model);
    output_buf << "average  since         example        example  current  current  current" << std::endl;
    output_buf << "loss     last          counter         weight    label  predict features" << std::endl;
    LEARNER::generic_driver(*train_model);
    VW::end_parser(*train_model);
    VW::save_predictor(*train_model, model_str);
    VW::finish(*train_model);
    
    if (!quiet)
    {
        // Reading from temporary files and printing to console
        std::ifstream readable_model_stream (readable_model_str);
        std::string readable_model_line;
        if (readable_model_stream.is_open())
        {
            output_buf << "Readable model from file: " + readable_model_str << std::endl;
            while ( getline (readable_model_stream, readable_model_line) )
            {
                output_buf << readable_model_line << std::endl;
            }
            readable_model_stream.close();
        }
        
        Rcpp::Rcout << output_buf.str() << std::endl;
    }
    
}

// [[Rcpp::export]]
Rcpp::NumericVector vwtest(Rcpp::List vwmodel, std::string data_path="", std::string probs_path = "", Rcpp::Nullable<Rcpp::String> readable_model=R_NilValue, bool quiet=false) {
    // if (!Rcpp::mod.inherits("vw")) Rcpp::stop("Input model is not VW model");
    // Rcpp::Rcout << vwmodel.attr("class") << std::endl;
    std::string data_str = check_data(vwmodel, data_path, "test");
    std::string model_str = Rcpp::as<std::string>(vwmodel["dir"]) + Rcpp::as<std::string>(vwmodel["model"]);
    std::string probs_str;
    std::string readable_model_str = Rcpp::as<std::string>(vwmodel["dir"]) + "readable_" + Rcpp::as<std::string>(vwmodel["model"]);
    
    // Output buffer to control "quiet" function execution easily
    std::stringstream output_buf;
    
    // If no probs_path was provided we should create temporary here and then delete
    if (probs_path.empty()) {
        probs_str = Rcpp::as<std::string>(vwmodel["dir"]) + std::to_string(std::time(nullptr)) + "_probs_out.vw";
    } else {
        probs_str = probs_path;
    }
    
    output_buf << "Starting VW test session" << std::endl;
    output_buf << "Using data file: " << data_str << std::endl;
    output_buf << "Using model file: " << model_str << std::endl;
    
    
    std::string test_init_str = Rcpp::as<std::string>(vwmodel["params_str"]);
    test_init_str += " -t -d " + data_str + " -p " + probs_str + " -i " + model_str;
    
    // Check readable model output mode: hashed features and original features (invert hash)
    if (readable_model.isNotNull()) {
        if (Rcpp::as<std::string>(readable_model) == "hashed")
        {
            test_init_str += " --readable_model " + readable_model_str;
        } else if (Rcpp::as<std::string>(readable_model) == "inverted")
        {
            test_init_str += " --invert_hash " + readable_model_str;
        } else {
            Rcpp::Rcerr << "Wrong readable_model argument" << std::endl << "Should be \"hashed\" or \"inverted\"" << std::endl;
            Rcpp::NumericVector data_vec(0);
            return data_vec;
        }
    }
    
    // Ignore output from VW
    if (quiet)
    {
        test_init_str += " --quiet";
    }
    
    output_buf << "Command line parameters: " << std::endl << test_init_str << std::endl;
    
    vw* predict_model = VW::initialize(test_init_str);
    VW::start_parser(*predict_model);
    output_buf << "average  since         example        example  current  current  current" << std::endl;
    output_buf << "loss     last          counter         weight    label  predict features" << std::endl;
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
    
    if (!quiet)
    {
        // Reading from temporary files and printing to console
        std::ifstream readable_model_stream (readable_model_str);
        std::string readable_model_line;
        if (readable_model_stream.is_open())
        {
            output_buf << "Readable model from file: " + readable_model_str << std::endl;
            while ( getline (readable_model_stream, readable_model_line) )
            {
                output_buf << readable_model_line << std::endl;
            }
            readable_model_stream.close();
        }
        
        Rcpp::Rcout << output_buf.str() << std::endl;
    }
    
    return data_vec;
}
