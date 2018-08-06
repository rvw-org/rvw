
.pkgenv <- new.env(parent=emptyenv())

.onAttach <- function(libname, pkgname) {

    # Initialise default/check lists
    general_check <- list(random_seed=0,
                          ring_size="",
                          holdout_off=FALSE,
                          holdout_period=10,
                          holdout_after=0,
                          early_terminate=3,
                          loss_function="squared",
                          link="identity",
                          quantile_tau=0.5,
                          confidence=FALSE,
                          confidence_after_training=FALSE)
    feature_check <- list(bit_precision=18,
                          quadratic="",
                          cubic="",
                          interactions="",
                          permutations=FALSE,
                          leave_duplicate_interactions=FALSE,
                          noconstant=FALSE, 
                          feature_limit="",
                          ngram="",
                          skips="",
                          hash="",
                          affix="",
                          spelling="")
    # Learning algorithm default/check lists
    sgd_check <- list(adaptive=TRUE,
                      normalized=TRUE,
                      invariant=TRUE,
                      adax=FALSE,
                      sparse_l2=0,
                      l1_state=0,
                      l2_state=1)
    bfgs_check <- list(conjugate_gradient=FALSE)
    ftrl_check <- list(ftrl_alpha=0.005,
                       ftrl_beta=0.1)
    optimization_check <- list(learning_rate=0.5,
                               initial_pass_length="",
                               l1=0,
                               l2=0,
                               no_bias_regularization=FALSE,
                               feature_mask="",
                               decay_learning_rate=1,
                               initial_t=0,
                               power_t=0.5,
                               initial_weight=0,
                               random_weights="",
                               normal_weights="",
                               truncated_normal_weights="",
                               sparse_weights=FALSE,
                               input_feature_regularizer="")

    # Learning parameters/reductions default/check lists
    binary_check <- list()
    oaa_check <- list(num_classes=3)
    ect_check <- list(num_classes=3)
    csoaa_check <- list(num_classes=3,
                        csoaa_ldf="")
    wap_check <- list(num_classes=3,
                      wap_ldf="")
    log_multi <- list(num_classes=3)
    lda_check <- list(num_topics=0,
                      lda_alpha=0.100000001,
                      lda_rho=0.100000001,
                      lda_D=10000,
                      lda_epsilon=0.00100000005,
                      math_mode=0,
                      minibatch=1)
    mf_check <- list(rank=0)
    lrq_check <- list(features="",
                      lrqdropout=FALSE)
    stage_poly <- list(sched_exponent = 1.0,
                       batch_sz = 1000,
                       batch_sz_no_doubling = TRUE)
    bootstrap_check <- list(rounds=10,
                            bs_type="mean")
    autolink <- list(degree=2)
    cb <- list(costs=2)
    cbify <- list(num_classes=3)
    nn_check <- list(hidden=3,
                     inpass=FALSE,
                     multitask=FALSE,
                     dropout=FALSE,
                     meanfield=FALSE)
    topk <- list(k=3)
    search <- list(id=0,
                   search_task=NA_character_,
                   search_interpolation=NA_character_,
                   search_rollout=NA_character_,
                   search_passes_per_policy=1,
                   search_beta=0.5,
                   search_alpha=1e-10,
                   search_total_nb_policies=NA_integer_,
                   search_trained_nb_policies=NA_integer_,
                   search_allowed_transitions=NA_character_,
                   search_subsample_time=NA_real_,
                   search_neighbor_features=NA_character_,
                   search_rollout_num_steps=NA_integer_,
                   search_history_length=1,
                   search_no_caching=FALSE,
                   search_xv=FALSE,
                   search_perturb_oracle=0,
                   search_linear_ordering=FALSE,
                   search_active_verify=NA_real_,
                   search_save_every_k_runs=NA_integer_)
    boosting_check <- list(num_learners=5)
    ksvm_check <- list(reprocess=1,
                       kernel="linear",
                       bandwidth=1.0)

    check_lists <- list(general_check=general_check, feature_check=feature_check, optimization_check=optimization_check,
        sgd_check=sgd_check, bfgs_check=bfgs_check, ftrl_check=ftrl_check,
        binary_check=binary_check, oaa_check=oaa_check, ect_check=ect_check, csoaa_check=csoaa_check, wap_check=wap_check,
        log_multi=log_multi, lda_check=lda_check, mf_check=mf_check, lrq_check=lrq_check, stage_poly=stage_poly, bootstrap_check=bootstrap_check, autolink=autolink,
        cb=cb, cbify=cbify,
        nn_check=nn_check, topk=topk, search=search, boosting_check=boosting_check, ksvm_check=ksvm_check)
    flatten_check_lists <- .flatten(check_lists)

    assign("check_lists", check_lists, envir=.pkgenv)
    assign("flatten_check_lists", flatten_check_lists, envir=.pkgenv)
}
