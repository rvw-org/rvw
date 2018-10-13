#'Create Vowpal Wabbit model, setup model parameters and data
#'
#'Sets up VW model together with parameters and data
#'
#'@param dir [string] Working directory path, default is tempdir()
#'@param model [string] File name for model weights or path to existng model file.
#'@param params_str [string] Pass cmd line parameters directly, bypassing the default approach.
#'For compatibility, parameters from vwtrain,vwtest, predict.vw can't be used here and functions add_option, vwparams aren't supported.
#'@param option [string] Add Learning algorithm / reduction option:
#'\itemize{
#'  \item \code{binary} - Reports loss as binary classification with -1,1 labels
#'  \item \code{oaa} - One-against-all multiclass learning with  labels
#'  \item \code{ect} - Error correcting tournament with  labels
#'  \item \code{csoaa} - One-against-all multiclass learning with costs
#'  \item \code{wap} - Weighted all-pairs multiclass learning with costs
#'  \item \code{multilabel_oaa} - One-against-all multilabel with multiple labels
#'  \item \code{log_multi} - Online (decision) trees for  classes
#'  \item \code{classweight} - Importance weight classes
#'  \item \code{lda} - Latent Dirichlet Allocation
#'  \item \code{recall_tree} - Use online tree for multiclass
#'  \item \code{new_mf} - Matrix factorization mode
#'  \item \code{lrq} - Low rank quadratic features
#'  \item \code{stage_poly} - Stagewise polynomial features
#'  \item \code{bootstrap} - bootstrap with K rounds by online importance resampling
#'  \item \code{autolink} - Create link function with polynomial N
#'  \item \code{replay} - Experience Replay
#'  \item \code{explore_eval} - Explore evaluation
#'  \item \code{cb} - Contextual bandit learning
#'  \item \code{cb_explore} - Contextual Bandit Exploration
#'  \item \code{cbify} - Convert multiclass on K classes into a contextual bandit problem
#'  \item \code{multiworld_test} - Multiworld Testing
#'  \item \code{nn} - Sigmoidal feedforward network
#'  \item \code{topk} - Top K recommendation
#'  \item \code{struct_search} - Search-based structured prediction (SEARN or DAgger)
#'  \item \code{boosting} - Online boosting with weak learners
#'  \item \code{marginal} - Substitute marginal label estimates for ids
#'}
#'@param algorithm [string] Optimzation algorithm
#'\itemize{
#'  \item \code{sgd} - adaptive, normalized, invariant stochastic gradient descent
#'  \item \code{bfgs} - Limited-memory Broyden-Fletcher-Goldfarb-Shanno optimization algorithm
#'  \item \code{ftrl} - FTRL: Follow the Regularized Leader optimization algorithm
#'  \item \code{pistol} - FTRL: Parameter-free Stochastic Learning
#'  \item \code{ksvm} - Kernel svm
#'  \item \code{OjaNewton} - Online Newton with Oja's Sketch
#'  \item \code{svrg} - Stochastic Variance Reduced Gradient
#'}
#'@param general_params List of parameters:
#'\itemize{
#'  \item \code{random_seed} [int] - Seed random number generator (default: 0)
#'  \item \code{ring_size} [int] - Size of example ring
#'  \item \code{holdout_off} [bool] - No holdout data in multiple passes (default: FALSE)
#'  \item \code{holdout_period} [int] - Holdout period for test only (default: 10)
#'  \item \code{holdout_after} [int] - Holdout after n training examples, default off (disables holdout_period) (default: 0)
#'  \item \code{early_terminate} [int] - Specify the number of passes tolerated when holdout loss doesn't decrease before early termination (default: 3)
#'  \item \code{loss_function} [string] - Specify the loss function to be used, uses squared by default. Currently available ones are: squared, classic, hinge, logistic, quantile and poisson. (default: squared)
#'  \item \code{link} [string] - Specify the link function: identity, logistic, glf1 or poisson. (default: identity)
#'  \item \code{quantile_tau} [real] - Parameter "tau" associated with Quantileloss. (default: 0.5)
#'}
#'@param feature_params List of parameters:
#'More information about "interactions" option (also "quadratic", "cubic") avaliable here \url{https://github.com/VowpalWabbit/vowpal_wabbit/wiki/Command-line-arguments#example-manipulation-options}
#'\itemize{
#'  \item \code{bit_precision} [int] - Number of bits in the feature table (default: 18)
#'  \item \code{quadratic} [string] - Create and use quadratic features (Specify 2 namespaces)
#'  \item \code{cubic} [string] - Create and use cubic features (Specify 3 namespaces)
#'  \item \code{interactions} [string] - Create feature interactions of any level between namespaces (Specify several namespaces)
#'  \item \code{permutations} [bool] - Use permutations instead of combinations for feature interactions of same namespace (default: FALSE)
#'  \item \code{leave_duplicate_interactions} [bool] - Don't remove interactions with duplicate combinations of namespaces. For ex. this is a duplicate: 'quadratic="ab", quadratic="ba"' and a lot more in 'quadratic="::"'. (default: FALSE)
#'  \item \code{noconstant} [bool] - Don't add a constant feature (default: FALSE)
#'  \item \code{feature_limit} [string] - limit to N features. To apply to a single namespace 'foo', arg should be "fN"
#'  \item \code{ngram} [string] - Generate N grams. To generate N grams for a single namespace 'foo', arg should be "fN".
#'  \item \code{skips} [string] - Use second derivative in line searchGenerate skips in N grams. This in conjunction with the ngram tag can be used to generate generalized n-skip-k-gram. To generate n-skips for a single namespace 'foo', arg should be "fN".
#'  \item \code{hash} [string] - How to hash the features. Available options: "strings", "all" (default: "strings")
#'  \item \code{affix} [string] - Generate prefixes/suffixes of features; argument "+2a,-3b,+1" means generate 2-char prefixes for namespace a, 3-char suffixes for b and 1 char prefixes for default namespace
#'  \item \code{spelling} [string] - Compute spelling features for a given namespace (use '_' for default namespace)
#'  \item \code{interact} [string] - Put weights on feature products from namespaces <n1> and <n2>
#'}
#'@param optimization_params List of parameters:
#'\itemize{
#'  \item \code{learning_rate} [real] - Set initial learning Rate (default: 0.5)
#'  \item \code{initial_pass_length} [int] - Initial number of examples per pass
#'  \item \code{l1} [real] - L1 regularization (default: 0)
#'  \item \code{l2} [real] - L2 regularization (default: 0)
#'  \item \code{no_bias_regularization} [string] - no bias in regularization (Available options: "on", "off")
#'  \item \code{feature_mask} [string] - Use existing regressor to determine which parameters may be updated.  If no initial_regressor given, also used for initial weights.
#'  \item \code{decay_learning_rate} [real] - Set Decay factor for learning_rate between passes (default: 1)
#'  \item \code{initial_t} [real] - initial t value (default: 0)
#'  \item \code{power_t} [real] - t power value (default: 0.5)
#'  \item \code{initial_weight} [int] - Set all weights to an initial value of arg (default: 0)
#'  \item \code{random_weights} [string] - Make initial weights random (Available options: "on", "off") (default: "off")
#'  \item \code{normal_weights} [string] - Make initial weights normal (Available options: "on", "off") (default: "off")
#'  \item \code{truncated_normal_weights} [string] - Make initial weights truncated normal (Available options: "on", "off") (default: "off")
#'  \item \code{sparse_weights} [bool] - Use a sparse datastructure for weights.
#'  \item \code{input_feature_regularizer} [string] - Per feature regularization input file.
#'}
#'Additional parameters depending on \code{algorithm} choice:
#'\itemize{
#'  \item \code{sgd}:
#'    \itemize{
#'      \item \code{adaptive} [bool] - Use adaptive, individual learning rates (default: TRUE)
#'      \item \code{normalized} [bool] - Use per feature normalized updates (default: TRUE)
#'      \item \code{invariant} [bool] - Use safe/importance aware updates (default: TRUE)
#'      \item \code{adax} [bool] - Use adaptive learning rates with x^2 instead of g^2x^2 (default: FALSE)
#'      \item \code{sparse_l2} [real] - use per feature normalized updates (default: 0)
#'      \item \code{l1_state} [real] - use per feature normalized updates (default: 0)
#'      \item \code{l2_state} [real] - use per feature normalized updates (default: 1)
#'    }
#'  \item \code{bfgs}:
#'    \itemize{
#'      \item \code{conjugate_gradient} [bool] - Use conjugate gradient based optimization (default: FALSE)
#'      \item \code{hessian_on} [bool] - Use second derivative in line search (default: FALSE)
#'      \item \code{mem} [int] - Memory in bfgs. (default: 15)
#'      \item \code{termination} [real] - Termination threshold. (default: 0.00100000005)
#'    }
#'  \item \code{ftrl}:
#'    \itemize{
#'      \item \code{ftrl_alpha} [real] - Learning rate for FTRL optimization (default: 0.005)
#'      \item \code{ftrl_beta} [real] - FTRL beta parameter (default: 0.1)
#'    }
#'  \item \code{pistol}:
#'    \itemize{
#'      \item \code{ftrl_alpha} [real] - Learning rate for FTRL optimization (default: 0.005)
#'      \item \code{ftrl_beta} [real] - FTRL beta parameter (default: 0.1)
#'    }
#'  \item \code{ksvm}:
#'    \itemize{
#'      \item \code{reprocess} [int] - number of reprocess steps for LASVM (default: 1)
#'      \item \code{kernel} [string] - type of kernel (rbf or linear) (default: "linear")
#'      \item \code{bandwidth} [real] - bandwidth of rbf kernel (default: 1.0)
#'      \item \code{degree} [int] - degree of poly kernel (default: 2)
#'      \item \code{lambda} [real] - saving regularization for test time (default: -1)
#'    }
#'  \item \code{OjaNewton}:
#'    \itemize{
#'      \item \code{sketch_size} [int] - size of sketch (default: 10)
#'      \item \code{epoch_size} [int] - size of epoch (default: 1)
#'      \item \code{alpha} [real] - multiplicative constant for identity (default: 1)
#'      \item \code{alpha_inverse} [real] - one over alpha, similar to learning rate
#'      \item \code{learning_rate_cnt} - constant for the learning rate 1/t (default: 2)
#'      \item \code{normalize} [string] - normalize the features or not (Available options: "on", "off") (default: "on")
#'      \item \code{random_init} [string] - randomize initialization of Oja or not (Available options: "on", "off") (default: "on")
#'    }
#'  \item \code{svrg}:
#'    \itemize{
#'      \item \code{stage_size} [int] - Number of passes per SVRG stage (default: 1)
#'    }
#'}
#'@param ... Additional options for a learning algorithm / reduction
#'\itemize{
#'  \item \code{oaa} or \code{ect}:
#'    \itemize{
#'      \item \code{num_classes} [int] - Number of classes
#'      \item \code{oaa_subsample} [int] - Subsample this number of negative examples when learning
     # \item \code{probabilities} - Predict probabilites of all classes
     # \item \code{scores} - Output raw scores per class
#'    }
#'  \item \code{multilabel_oaa}:
#'    \itemize{
#'      \item \code{num_labels} [int] - Number of labels
#'    }
#'  \item \code{csoaa} or \code{wap}:
#'    \itemize{
#'      \item \code{num_classes} [int] - Number of classes
#'      \item \code{csoaa_ldf} or \code{wap_ldf} - \code{singleline} (Default) or \code{multiline} label dependent features
     # \item \code{csoaa_rank} - Return actions sorted by score order
     # \item \code{probabilities} - Predict probabilites of all classes
#'    }
#'  \item \code{log_multi}:
#'    \itemize{
#'      \item \code{num_classes} [int] - Number of classes
#'      \item \code{no_progress} [bool] - Disable progressive validation (default: FALSE)
#'      \item \code{swap_resistance} [int] - Higher = more resistance to swap, (default: 4)
#'    }
#'  \item \code{classweight}:
#'    \itemize{
#'      \item \code{class_multiplier} [real] - importance weight multiplier for class
#'    }
#'  \item \code{recall_tree}:
#'    \itemize{
#'      \item \code{num_classes} [int] - Number of classes
#'      \item \code{max_candidates} [int] - Maximum number of labels per leaf in the tree
#'      \item \code{bern_hyper} [real] - Recall tree depth penalty (default: 1)
#'      \item \code{max_depth} [int] - Maximum depth of the tree, (default: log_2(number of classes) )
#'      \item \code{node_only} [string] - Only use node features, not full path (Available options: "on", "off") (default: "off")
#'      \item \code{randomized_routing} [string] - Randomized routing (Available options: "on", "off") (default: "off")
#'    }
#'  \item \code{lda}:
#'    \itemize{
#'      \item \code{num_topics} [int] - Number of topics
#'      \item \code{lda_alpha} [real] - Prior on sparsity of per-document topic weights (default: 0.100000001)
#'      \item \code{lda_rho} [real] - Prior on sparsity of topic distributions (default: 0.100000001)
#'      \item \code{lda_D} [int] - Number of documents (default: 10000)
#'      \item \code{lda_epsilon} [real] - Loop convergence threshold (default: 0.00100000005)
#'      \item \code{math-mode} [string] - Math mode: simd, accuracy, fast-approx
#'      \item \code{minibatch} [int] - Minibatch size (default: 1)
#'      \item \code{metrics} [string] - Compute metrics (Available options: "on", "off") (default: "off")
#'    }
#'  \item \code{new_mf}:
#'    \itemize{
#'      \item \code{rank} [int] - rank for matrix factorization
#'    }
#'  \item \code{lrq}:
#'    \itemize{
#'      \item \code{features} [string] - low rank quadratic features
#'      \item \code{lrqdropout} [bool] - use dropout training for low rank quadratic features (default: FALSE)
#'    }
#'  \item \code{stage_poly}:
#'    \itemize{
#'      \item \code{sched_exponent} [real] - exponent controlling quantity of included features (default: 1.0)
#'      \item \code{batch_sz} [int] - multiplier on batch size before including more features (default: 1000)
#'      \item \code{batch_sz_no_doubling} [bool] - batch_sz does not double (default: TRUE)
#'    }
#'  \item \code{bootstrap}:
#'    \itemize{
#'      \item \code{num_rounds} [int] - number of rounds
#'      \item \code{bs_type} [string] - the bootstrap mode: 'mean' or 'vote' (default: "mean")
#'    }
#'  \item \code{autolink}:
#'    \itemize{
#'      \item \code{degree} [int] - polynomial degree (default: 2)
#'    }
#'  \item \code{replay}:
#'    \itemize{
#'      \item \code{level} [string] - Use experience replay at a specified level (b=classification/regression, m=multiclass, c=cost sensitive)
#'      \item \code{buffer} [int] - Buffer size (default: 100)
#'      \item \code{count} [int] - how many times (in expectation) should each example be played (default: 1 = permuting)
#'    }
#'  \item \code{explore_eval}:
#'    \itemize{
#'      \item \code{multiplier} [real]  - Multiplier used to make all rejection sample probabilities <= 1
#'    }
#'  \item \code{cb}:
#'    \itemize{
#'      \item \code{num_costs} [int] - number of num_costs If costs=0, contextual bandit learning
#'      with multiline action dependent features (ADF) is triggered ("--cb_adf").
#'      \item \code{cb_type} [string] - contextual bandit method to use in {ips,dm,dr, mtr (for ADF)} (default: "dr")
#'      \item \code{eval} [bool] - Evaluate a policy rather than optimizing (default: FALSE)
#'      \item \code{rank_all} [bool] - Return actions sorted by score order. (for ADF) (default: FALSE)
#'      \item \code{no_predict} [bool] - Do not do a prediction when training. (for ADF) (default: FALSE)
#'    }
#'  \item \code{cb_explore}:
#'    \itemize{
#'      \item \code{num_actions} [bool] - number of actions in online explore-exploit for a <k> action contextual bandit problem.
#'      If num_actions=0, online explore-exploit for a contextual bandit problem with multiline action dependent features (ADF) is triggered ("--cb_explore_adf").
#'      \item \code{explore_type} [string] - Type of exploration to use: "epsilon" (epsilon-greedy exploration) (default),
#'       "first" (tau-first exploration), "bag" (bagging-based exploration), "cover" (Online cover based exploration), "softmax" (softmax exploration),
#'       "regcb" (RegCB-elim exploration), "regcbopt" (RegCB optimistic exploration). "softmax", "regcb" and "regcbopt" types are only avaliable for exploration with ADF. (default: "epsilon")
#'      \item \code{explore_arg} [real] - Parameter for exploration algorithm. Applicable for "epsilon", "first", "bag" and "cover" types of exploration. (default: 0.05)
#'      \item \code{psi} [real] - Disagreement parameter for "cover" algorithm. (default: 1)
#'      \item \code{nounif} [bool] - Do not explore uniformly on zero-probability actions in "cover" algorithm. (default: FALSE)
#'      \item \code{mellowness} [real] - "RegCB" mellowness parameter c_0. (default: 0.1)
#'      \item \code{greedify} [bool] - Always update first policy once in "bag" (default: FALSE)
#'      \item \code{lambda} [real] - Parameter for "softmax". (default: -1)
#'      \item \code{cb_min_cost} [real] - Lower bound on cost. (default: 0) For ADF only
#'      \item \code{cb_max_cost} [real] - Upper bound on cost. (default: 1) For ADF only
#'      \item \code{first_only} [bool] - Only explore the first action in a tie-breaking event. For ADF only (default: FALSE)
#'    }
#'  \item \code{cbify}:
#'    \itemize{
#'      \item \code{num_classes} [int] - number of classes
#'      \item \code{cbify_cs} [bool] - consume cost-sensitive classification examples instead of multiclass (default: FALSE)
#'      \item \code{loss0} [real] - loss for correct label (default: 0)
#'      \item \code{loss1} [real] - loss for incorrect label (default: 1)
#'    }
#'  \item \code{multiworld_test}:
#'    \itemize{
#'      \item \code{features} [string] - Evaluate features as a policies
#'      \item \code{learn} [int] - Do Contextual Bandit learning on <n> classes.
#'      \item \code{num_classes} [bool] - Discard mwt policy features before learning (default: FALSE)
#'    }
#'  \item \code{nn}:
#'    \itemize{
#'      \item \code{num_hidden} [int] - number of hidden units
#'      \item \code{inpass} [bool] - Train or test sigmoidal feedforward network with input passthrough (default: FALSE)
#'      \item \code{multitask} [bool] - Share hidden layer across all reduced tasks (default: FALSE)
#'      \item \code{dropout} [bool] - Train or test sigmoidal feedforward network using dropout (default: FALSE)
#'      \item \code{meanfield} [bool] - Train or test sigmoidal feedforward network using mean field (default: FALSE)
#'    }
#'  \item \code{topk}:
#'    \itemize{
#'      \item \code{num_k} [int] - number of top k recomendations
#'    }
#'  \item \code{struct_search}:
#'    \itemize{
#'      \item \code{id} [int] - maximum action id or 0 for LDF
#'      \item \code{search_task} [string] - search task: sequence, sequencespan, sequence_ctg, argmax, sequence_demoldf, multiclasstask, dep_parser, entity_relation, hook, graph
#'      \item \code{search_interpolation} [string] - at what level should interpolation happen? (data or policy)
#'      \item \code{search_rollout} [string] - how should rollouts be executed? (policy, oracle, mix_per_state, mix_per_roll, none)
#'      \item \code{search_rollin} [string] - how should past trajectories be generated? (policy, oracle, mix_per_state, mix_per_roll)
#'      \item \code{search_passes_per_policy} [int] - number of passes per policy (only valid for search_interpolation=policy). (default: 1)
#'      \item \code{search_beta} [real] - interpolation rate for policies (only valid for search_interpolation=policy). (default: 0.5)
#'      \item \code{search_alpha} [real] - annealed beta = 1-(1-alpha)^t (only valid for search_interpolation=data). (default: 1e-10)
#'      \item \code{search_total_nb_policies} [int] - if we are going to train the policies through multiple separate calls to vw, we need to specify this parameter and tell vw how many policies are eventually going to be trained
#'      \item \code{search_trained_nb_policies} [int] - the number of trained policies in a file
#'      \item \code{search_allowed_transitions} [string] - read file of allowed transitions. default: all transitions are allowed
#'      \item \code{search_subsample_time} [real] - instead of training at all timesteps, use a subset. if value in (0,1), train on a random v%. if v>=1, train on precisely v steps per example, if v<=-1, use active learning
#'      \item \code{search_neighbor_features} [string] - copy features from neighboring lines. argument looks like: '-1:a,+2' meaning copy previous line from namespace "a" and next line from namespace "unnamed", where ',' separates them
#'      \item \code{search_rollout_num_steps} [int] - how many calls of "loss" before we stop really predicting on rollouts and switch to oracle (default means "infinite")
#'      \item \code{search_history_length} [int] - some tasks allow you to specify how much history their depend on; specify that here. (default: 1)
#'      \item \code{search_no_caching} [bool] - turn off the built-in caching ability (makes things slower, but technically more safe) (default: FALSE)
#'      \item \code{search_xv} [bool] - train two separate policies, alternating prediction/learning. (default: FALSE)
#'      \item \code{search_perturb_oracle} [real] - perturb the oracle on rollin with this probability. (default: 0)
#'      \item \code{search_linear_ordering} [bool]  - insist on generating examples in linear order. (default: FALSE and using hoopla permutation)
#'      \item \code{search_active_verify} [real] - verify that active learning is doing the right thing (arg = multiplier, should be = cost_range * range_c)
#'      \item \code{search_save_every_k_runs} [int] - save model every k runs
#'    }
#'  \item \code{boosting}:
#'    \itemize{
#'      \item \code{num_learners} [int] - number of weak learners
#'      \item \code{gamma} [real] - weak learner's edge (=0.1), used only by online BBM (default: 0.100000001)
#'      \item \code{alg} - specify the boosting algorithm: BBM (default), logistic (AdaBoost.OL.W), adaptive (AdaBoost.OL) (default: "BBM")
#'    }
#'  \item \code{marginal}:
#'    \itemize{
#'      \item \code{ids} [string] - Substitute marginal label estimates for ids
#'      \item \code{initial_denominator} [real] - Initial denominator (default: 1)
#'      \item \code{initial_numerator} [real] - Initial numerator (default: 0.5)
#'      \item \code{compete} [bool] - Enable competition with marginal features (default: FALSE)
#'      \item \code{update_before_learn} [string] - Update marginal values before learning (Available options: "on", "off") (default: "off")
#'      \item \code{unweighted_marginals} [string] - Ignore importance weights when computing marginals (Available options: "on", "off") (default: "off")
#'      \item \code{decay} [real] - Decay multiplier per event (1e-3 for example) (default=0)
#'    }
#'}
#'@return vwmodel list class
#'@examples
#'vwsetup(
#'  dir = tempdir(),
#'  model = "pk_mdl.vw",
#'  general_params = list(loss_function="logistic", link="logistic"),
#'  optimization_params = list(adaptive=FALSE),
#'  option = "binary"
#')
#'
vwsetup <- function(algorithm = c("sgd", "bfgs", "ftrl", "pistol", "ksvm", "OjaNewton", "svrg"),
                    general_params = list(),
                    feature_params = list(),
                    optimization_params = list(),
                    dir = tempdir(),
                    model = NULL,
                    params_str = NULL,
                    option = c("", "binary", "oaa", "ect", "csoaa", "wap",
                               "log_multi", "recall_tree", "lda",
                               "multilabel_oaa", "classweight",
                               "new_mf", "lrq", "stage_poly", "bootstrap",
                               "autolink", "replay", "explore_eval", "cb",
                               "cb_explore", "cbify", "multiworld_test_check",
                               "nn", "topk", "search", "boosting", "marginal"),
                    ...
) {

  # Initialize defaults
  train_md5sum = ""
  test_md5sum = ""
  train_file = ""
  is_cl = FALSE

  empty_eval_list = list(
      num_examples = NA_integer_,
      weighted_example_sum = NA_real_,
      weighted_label_sum = NA_real_,
      avg_loss = NA_real_,
      avg_multiclass_log_loss = NA_real_,
      best_const = NA_real_,
      best_const_loss = NA_real_,
      total_feature = NA_integer_
  )
  eval_results = list(
      train=empty_eval_list,
      test=empty_eval_list
  )

  # Check dir
  if(grepl("[[:space:]]", dir)) {
      stop("Whitespace characters are not allowed in `dir` path", call. = FALSE)
  }

  # Remove last trailing path separator, because it's handled in vwtrain and vwtest
  last_char_dir <-  substr(dir, nchar(dir), nchar(dir))
  if(last_char_dir == "/" || last_char_dir == "\\") {
      dir <- substr(dir, 1, nchar(dir) - 1)
  }

  # If user provides model file path, use it for setup
  if(!is.null(model)) {
      # Check model
      if(grepl("[[:space:]]", model)) {
          stop("Whitespace characters are not allowed in `model` path", call. = FALSE)
      }
      # If just filename provided, use it in model's directory
      # If path provided, copy file to model's directory first
      if(dirname(model) != ".") {
          file.copy(from = model, to = dir)
          model <- basename(model)
      }
  } else {
      model <- paste0("vw_", floor(as.numeric(Sys.time())), "_mdl.vw")
  }

  algorithm <- match.arg(algorithm)
  option <- match.arg(option)

  # Set options
  if(option == "") {
      options = list()
  } else {
      options <- setNames(list(list(...)), option)
  }

  params <- list(
      algorithm = algorithm,
      general_params = general_params,
      feature_params = feature_params,
      optimization_params = optimization_params,
      options = options
      # input_mode = input_mode
  )
  
  if (!is.null(params_str)) {
      is_cl = TRUE
      params$algorithm <- NA
      # Check params string
      prohibited_params <- c("-d", "--data", "--readable_model", "-r",
                             "--invert_hash", "--quiet", "--cache", "-c",
                             "--cache_file", "-k", "--kill_cache", "-p",
                             "--passes", "--save_resume", "--progress", "-P",
                             "--audit_regressor")
      if(grepl(paste0("\\b", prohibited_params, "\\b", collapse = "|"),params_str)) {
          stop(paste0("Following cmd line parameters are defined in other functions:\n",
                   paste0(prohibited_params, collapse = ", ")), call. = FALSE)
      }

      
  } else {
      # Parse parameters and write them to string
      # Check parameters
      params <- .check_parameters(params)
      # Write to string
      params_str <- .create_parameters_string(params)
  }
  
  vwmodel <- list(params = params,
                  dir = dir,
                  model = model,
                  params_str = params_str,
                  is_cl = is_cl,
                  data_md5sum = list(train = train_md5sum,
                               test = test_md5sum),
                  train_file = train_file,
                  eval = eval_results,
                  parser_opts = NA)
  class(vwmodel) <- "vw"
  return(vwmodel)
}

#'Add option to the model
#'
#'@description Add a learning algorithm / reduction to the option stack inside model
#'@param vwmodel [vw] Model of vw class
#'@param option [string] Name of an option
#'@param ... Additional options for a learning algorithm / reduction
add_option <- function(vwmodel, option = c("binary", "oaa", "ect", "csoaa", "wap",
                                           "log_multi", "recall_tree", "lda",
                                           "multilabel_oaa", "classweight",
                                           "new_mf", "lrq", "stage_poly", "bootstrap",
                                           "autolink", "replay", "explore_eval", "cb",
                                           "cb_explore", "cbify", "multiworld_test_check",
                                           "nn", "topk", "search", "boosting", "marginal"),
                       ...) {
    
    if(!inherits(vwmodel, "vw")) {
        stop("vwmodel should be of class vw", call. = FALSE)
    }
    
    if (vwmodel$is_cl) {
        stop("add_option can't be used when cmd line parameters are used", call. = FALSE)
    }
    

    option <- match.arg(option)

    if (option %in% names(vwmodel$params$options)) {
        stop("Trying to overwrite option", call. = FALSE)
    }

    new_option <- setNames(list(list(...)), option)
    vwmodel$params$options <- c(vwmodel$params$options, new_option)
    vwmodel$params <- .check_parameters(vwmodel$params)
    vwmodel$params_str <- .create_parameters_string(vwmodel$params)
    vwmodel
}


#'Print VW model
#'
#'@description Print information about Vowpal Wabbit model
#'@param x [vw] Model of vw class
#'@param ... Not used currently
#'@examples
#'vwmodel <- vwsetup()
#'print(vwmodel)
#'
print.vw <- function(x, ...) {
  cat("\tVowpal Wabbit model\n")
  cat("Working directory:  ", x$dir, "\n")
  cat("Model file:  ", file.path(x$dir, x$model), "\n")
  
  if (!x$is_cl) {
      cat("Learning algorithm:  ", x$params$algorithm, "\n")
      cat("General parameters:", "\n")
      sapply(names(x$params$general_params), FUN = function(i) {
          if(is.na(x$params$general_params[[i]])) {
              cat("\t", i, ":  Not defined\n")
          } else {
              cat("\t", i, ":  ", x$params$general_params[[i]], "\n")
          }
      })
      cat("Feature parameters:", "\n")
      sapply(names(x$params$feature_params), FUN = function(i) {
          if(is.na(x$params$feature_params[[i]])) {
              cat("\t", i, ":  Not defined\n")
          } else {
              cat("\t", i, ":  ", x$params$feature_params[[i]], "\n")
          }
      })
      cat("Learning algorithms / Reductions:", "\n")
      sapply(names(x$params$options), function(option_name) {
          cat("\t", option_name, ":\n")
          sapply(names(x$params$options[[option_name]]), FUN = function(i) {
              if(is.na(x$params$options[[option_name]][[i]])) {
                  cat("\t\t", i, ":  Not defined\n")
              } else {
                  cat("\t\t", i, ":  ", x$params$options[[option_name]][[i]], "\n")
              }
          })
      })
      cat("Optimization parameters:", "\n")
      sapply(names(x$params$optimization_params), FUN = function(i) {
          if(is.na(x$params$optimization_params[[i]])) {
              cat("\t", i, ":  Not defined\n")
          } else {
              cat("\t", i, ":  ", x$params$optimization_params[[i]], "\n")
          }
      })
  } else {
      cat("Command line parameters:  ", x$params_str, "\n")
  }
  
  if(!all(is.na(.flatten(x$eval$train)))) {
      cat("Model evaluation. Training:", "\n")
      sapply(names(x$eval$train), FUN = function(i) {
          if(!is.na(x$eval$train[[i]])) {
              cat("\t", i, ":  ", x$eval$train[[i]], "\n")
          }
      })
  }
  if(!all(is.na(.flatten(x$eval$test)))) {
      cat("Model evaluation. Testing:", "\n")
      sapply(names(x$eval$test), FUN = function(i) {
          if(!is.na(x$eval$test[[i]])) {
              cat("\t", i, ":  ", x$eval$test[[i]], "\n")
          }
      })
  }
}

#'@rdname vwtest
predict.vw <- function(object, data, probs_path = "", full_probs = FALSE,
                       readable_model = NULL, quiet = FALSE, ...) {
    vwtest(object, data = data, probs_path = probs_path, full_probs = full_probs,
           readable_model = readable_model, quiet = quiet)
}

#'Access and modify parameters of VW model
#'
#'@description These functions allow to access VW model parameters by name and correctly modify them
#'@param vwmodel [vw] Model of vw class
#'@param name [string] Name of VW parameter
#'@param value [string/int/real/bool] Replacment value of a parameter
#'@return Value of a parameter
#'@examples
#'vwmodel <- vwsetup()
#'# Access parameter
#'vwparams(vwmodel, "bit_precision")
#'# Modify parameter
#'vwparams(vwmodel, "bit_precision") <- 25
#'
#'@rdname vwparams
vwparams <- function(vwmodel, name) {
    if(!inherits(vwmodel, "vw")) {
        stop("vwmodel should be of class vw", call. = FALSE)
    }
    if (vwmodel$is_cl) {
        stop("vwparams can't be used when cmd line parameters are used", call. = FALSE)
    }

    key_string <- grep(pattern = paste0("\\b", name, "\\b"), x = names(unlist(vwmodel$params)), value = T)
    # print(key_string)
    if(length(key_string) > 0) {
        key_params <- unlist(strsplit(key_string, split = ".", fixed = TRUE))
        if(is.list(vwmodel$params[[key_params[1]]])) {
            if(is.list(vwmodel$params[[key_params[1]]][[key_params[2]]])) {
                return(vwmodel$params[[key_params[1]]][[key_params[2]]][[key_params[3]]])
            } else {
                return(vwmodel$params[[key_params[1]]][[key_params[2]]])
            }
        } else {
            return(vwmodel$params[[key_params[1]]])
        }
    } else {
        stop("Wrong parameter name", call. = FALSE)
    }
}

#'@rdname vwparams
`vwparams<-` <- function(vwmodel, name, value) {
    if(!inherits(vwmodel, "vw")) {
        stop("vwmodel should be of class vw", call. = FALSE)
    }
    if (vwmodel$is_cl) {
        stop("vwparams can't be used when cmd line parameters are used", call. = FALSE)
    }

    key_string <- grep(pattern = paste0("\\b", name, "\\b"), x = names(unlist(vwmodel$params)), value = T)
    if(length(key_string) > 0) {
        key_params <- unlist(strsplit(key_string, split = ".", fixed = TRUE))
        if(is.list(vwmodel$params[[key_params[1]]])) {
            if(is.list(vwmodel$params[[key_params[1]]][[key_params[2]]])) {
                vwmodel$params[[key_params[1]]][[key_params[2]]][[key_params[3]]] <- value
            } else {
                vwmodel$params[[key_params[1]]][[key_params[2]]] <- value
            }
        } else {
            vwmodel$params[[key_params[1]]] <- value
        }
        vwmodel$params <- .check_parameters(vwmodel$params)
        vwmodel$params_str <- .create_parameters_string(vwmodel$params)
        # file.remove(paste0(vwmodel$dir, vwmodel$model))
        return(vwmodel)
    } else {
        stop("Wrong parameter name", call. = FALSE)
    }
}


## original source:  vowpal_wabbit/R/r.vw/dt2vw.R
##
## written by (per 'git log'):
##   Selim Raboudi <selim.raboudi@gmail.com>
##   Jongbin Jung <olorin86@gmail.com>
## and by Dirk Eddelbuettel as part of rvw
## released under (3 clause) BSD like rest of vowpal_wabbit
##
## now maintained here by Ivan Pavlov as part of rvw

#'Create a VW data file from a R data.frame object
#'
#'@param data [data.frame] data.frame object to be converted
#'@param file_path [string] file name of the resulting data in
#'  VW-friendly format
#'@param namespaces [list or yaml file] name of each namespace and
#'  each variable for each namespace can be a R list, or a YAML
#'  file example namespace with the IRIS database: namespaces =
#'  list(sepal = list('Sepal.Length', 'Sepal.Width'), petal = list('Petal.Length',
#'  'Petal.Width') this creates 2 namespaces (sepal
#'  and petal) containing the features defined by elements of this lists.
#'@param keep_space [string vector] keep spaces for this features
#'Example:"FERRARI 4Si"
#'With \code{keep_space} will be "FERRARI 4Si" and will be treated as two features
#'Without \code{keep_space} will be "FERRARI_4Si" and will be treated as one feature
#'@param fixed [string vector] fixed parsing for this features
#'Similar to \code{keep_space}, but parse features exactly without replacement of special characters ("(", ")", "|", ":", "'").
#'Can be used for LDA ("word_1:2 word_2:3" will stay the same),
#'but should be used carefully, because special characters can ruin final VW format file.
#'@param targets [string or string vector]
#'If \code{[string]} then will be treated as vector with real number labels for regular VW input format.
#'If \code{[string vector]} then will be treated as vectors with class costs for wap and csoaa
#'multi-class classification algorithms or as vectors with actions for Contextual Bandit algorithm.
#'@param probabilities [string vector] vectors with action probabilities for Contextual Bandit algorithm.
#'@param weight [string] weight (importance) of each line of the dataset.
#'@param base [string] base of each line of the dataset. Used for residual regression.
#'@param tag [string] tag of each line of the dataset.
#'@param multiline [integer] number of labels (separate lines) for multilines example
#'@param append [bool] data to be appended to the result file
#'@import yaml
#'@import tools
df2vw <- function(data, file_path, namespaces = NULL,
                  keep_space = NULL, fixed = NULL,
                  targets = NULL, probabilities = NULL,
                  weight = NULL, base = NULL, tag = NULL,
                  multiline = NULL, append = FALSE) {

    # Check data
    if(grepl("[[:space:]]", file_path)) {
        stop("Whitespace characters are not allowed in `file_path` path", call. = FALSE)
    }

    # if namespaces = NULL, define a unique namespace
    if (is.null(namespaces)) {
        all_vars <- colnames(data)[!colnames(data) %in% c(targets, probabilities, weight, base, tag)]
        namespaces <- list(A = list(all_vars))
    }

    data <- data.table::copy(data.table::setDT(data))

    # Special characters in VW input file format
    specChar      <- "\\(|\\)|\\||\\:|'"
    specCharSpace <- "\\(|\\)|\\||\\:| |'"
    
    # parsing variable names
    parsingNames <- function(x, keepSpace=F) {
        ret <- c()
        if(keepSpace){
            for (el in x)
                ret <- append(ret, gsub(specChar,'_', el))
        } else {
            for (el in x)
                ret <- append(ret, gsub(specCharSpace,'_', el))
        }
        ret
    }

    # parsing values of categorical variables
    # "keepSpace=T" when we want to keep spaces for text variables
    parsingVar <- function(x, keepSpace) {
        if (!keepSpace) # Replace special characters with "_"
            spch <- specCharSpace
        else # Replace special characters, but keep spaces
            spch <- specChar
        gsub(spch, '_', x)
    }
    
    # namespace load with a yaml file
    if (typeof(namespaces) == "character" && length(namespaces) == 1 &&
        grepl("yaml$", namespaces)) {
        if(requireNamespace("yaml", quietly = TRUE)) {
            namespaces <- yaml::yaml.load_file(namespaces)
        } else {
            stop("The 'yaml' package is needed.", call. = FALSE)
        }
    }

    # replace all names to avoid conflicts with VW file format
    names(data) <- parsingNames(names(data))
    names(namespaces) <- parsingNames(names(namespaces), keepSpace = T)
    for (x in names(namespaces)) namespaces[[x]] <- parsingNames(namespaces[[x]])
    targets <- parsingNames(targets)
    if (!is.null(probabilities)) probabilities <- parsingNames(probabilities)
    if (!is.null(weight)) weight <- parsingNames(weight)
    if (!is.null(base)) base <- parsingNames(base)
    if (!is.null(tag)) tag <- parsingNames(tag)

    # Preparing file
    if(!append)
        vw_file <- file(file_path,"w")
    else
        vw_file <- file(file_path,"a")

    # Construct vw format for labels, weights, base and tag
    formatDataVW <- ""
    argexpr <- c()
    names_indices <- seq_len(ncol(data))
    names(names_indices) <- names(data)

    if(!is.null(targets)) {
        formatDataVW <- paste0(formatDataVW, "%s")
        if(length(targets) > 1) {

            # Initialize empty labels
            data[["parsed_labels"]] <- rep("", nrow(data))

            if(!is.null(probabilities)) {

                if(length(targets) != length(probabilities)) {
                    stop("targets and probabilities should be of the same length", call. = FALSE)
                }

                # Construct Labels for multilabel examples with probabilities (e.g. 1:0.6:0.3 2:0.4:0.7)
                # Iterate cost vectors names
                for(i in seq_along(targets)) {
                    elem_targets <- data[[targets[i]]]
                    elem_probability <- data[[probabilities[i]]]
                    # Iterate cost vectors individualy (nrow of data)
                    vapply(X = seq_len(nrow(data)), FUN = function(j) {
                        ifelse(is.na(elem_targets[j]),
                               "",
                               data[["parsed_labels"]][j] <<- paste(data[["parsed_labels"]][j],
                                                paste(i, elem_targets[j], elem_probability[j], sep = ":")))
                    },
                    FUN.VALUE = "character")
                }
                data[["parsed_labels"]] <- trimws(data[["parsed_labels"]])

            } else {
                # Construct Labels for multilabel examples without probabilities (e.g. 1:0.6 2:0.4)
                # Iterate cost vectors names
                for(i in seq_along(targets)) {
                    elem_targets <- data[[targets[i]]]
                    # Iterate cost vectors individualy (nrow of data)
                    vapply(X = seq_len(nrow(data)), FUN = function(j) {
                        ifelse(is.na(elem_targets[j]),
                               "",
                               data[["parsed_labels"]][j] <<- paste(data[["parsed_labels"]][j],
                                                paste(i, elem_targets[j], sep = ":")))
                    },
                    FUN.VALUE = "character")
                }
                data[["parsed_labels"]] <- trimws(data[["parsed_labels"]])

            }
            formatDataVW <- paste0(formatDataVW, " ")
            argexpr <- c(argexpr, "parsed_labels")

        } else {
            # Append regular labels
            argexpr <- c(argexpr, targets)

            # For multiline examples
            if(!is.null(multiline) ) {
                formatDataVW <- paste0("%s:", formatDataVW)
                argexpr <- c("eval(parse(text = 'rep(1:multiline, nrow(data)/multiline)'))",
                             argexpr)
                if(!is.null(probabilities) ) {
                    if(length(probabilities) == 1){
                        formatDataVW <- paste0(formatDataVW, ":%s")
                        argexpr <- c(argexpr, probabilities)
                    } else {
                        stop("probabilities should be of length 1", call. = FALSE)
                    }
                }
            } else {
                if(!is.null(weight)) {
                    formatDataVW <- paste0(formatDataVW, " %s")
                    argexpr <- c(argexpr, weight)
                }
            }
            formatDataVW <- paste0(formatDataVW, " ")
        }
        if(!is.null(base)) {
            formatDataVW <- paste0(formatDataVW, "%s ")
            argexpr <- c(argexpr, base)
        }
        if(!is.null(tag)) {
            formatDataVW <- paste0(formatDataVW, "'%s ")
            argexpr <- c(argexpr, tag)
        }
    }
    formatDataVW <- trimws(formatDataVW, which = "right")
    argexpr <- unlist(argexpr)

    # Constructing features
    ## INITIALIZING THE HEADER AND INDEX
    ##Header: list of variables'name for each namespace
    ##Index: check if the variable is numerical (->TRUE) or categorical (->FALSE)
    Header <- list()
    Index <- list()

    for(nsN in names(namespaces)) {
        # Index[[nsN]] <- sapply(data[,namespaces[[nsN]]], is.numeric)
        Index[[nsN]] <- sapply(data[,namespaces[[nsN]],with=F], is.numeric)
        Header[[nsN]] <- namespaces[[nsN]]

        ## ESCAPE THE CATEGORICAL VARIABLES
        # Enable special parsing for features stated in "keep_space" and "fixed" arguments
        Header[[nsN]][!Index[[nsN]]] <- ifelse(
            test = Header[[nsN]][!Index[[nsN]]] %in% fixed,
            yes = Header[[nsN]][!Index[[nsN]]],
            no = paste0("eval(parse(text = 'parsingVar(",
                        Header[[nsN]][!Index[[nsN]]],
                        ", keepSpace = ", Header[[nsN]][!Index[[nsN]]] %in% keep_space, ")'))")
        )
    }

    ## ADDING THE FORMAT FOR THE VARIABLES OF EACH NAMESPACE, AND CREATING THE ARGUMENT VECTOR
    for (nsN in names(namespaces)) {
        header <- namespaces[[nsN]]
        eval_header <- Header[[nsN]]
        index <- Index[[nsN]]
        special_parse_index <- header[!index] %in% c(keep_space, fixed)
        formatNumeric <- paste0(header[index], rep(":%s ", sum(index)), collapse = "")
        # Categorical features with regular and special parsing (in "keep_space" or "fixed")
        formatCategoricalRegular <- paste0(header[!index][!special_parse_index], rep("^%s ", sum(!special_parse_index)), collapse = " ")
        formatCategoricalSpecial <- paste0(rep("%s", sum(special_parse_index)), collapse = " ")

        formatDataVW <- trimws(c(formatDataVW, paste0(nsN, ' ', formatNumeric, formatCategoricalRegular, formatCategoricalSpecial)), which = "right")
        argexpr <- c(argexpr, eval_header[index], eval_header[!index][!special_parse_index], eval_header[!index][special_parse_index])
    }

    # Add namespaces saparator
    formatDataVW <- paste0(formatDataVW, collapse = " |")
    
    # For debugging
    # print("formatDataVW:")
    # print(formatDataVW)
    # print("argexpr:")
    # print(argexpr)
    
    if(!is.null(multiline)) {
        formatDataVW <-
            writeLines(text = paste0(data[, .sprintf2(formatDataVW, lapply(argexpr, function(.x) eval(parse(text=.x))))],
                                     c(rep("", multiline - 1), "\n"),
                                     collapse = '\n'),
                       con = vw_file)
    } else {
        writeLines(text = paste0(data[, .sprintf2(formatDataVW, lapply(argexpr, function(.x) eval(parse(text=.x))))], collapse = '\n'),
                   con = vw_file)
    }

    close(vw_file)
}
