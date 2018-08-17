
.rvw_global <- new.env(parent=emptyenv())

.onAttach <- function(libname, pkgname) {
    # Initialise default/check lists
    general_check <- list(random_seed=0,
                          ring_size=NA_real_,
                          holdout_off=FALSE,
                          holdout_period=10,
                          holdout_after=0,
                          early_terminate=3,
                          loss_function="squared",
                          link="identity",
                          quantile_tau=0.5)
    feature_check <- list(bit_precision=18,
                          quadratic=NA_character_,
                          cubic=NA_character_,
                          interactions=NA_character_,
                          permutations=FALSE,
                          leave_duplicate_interactions=FALSE,
                          noconstant=FALSE, 
                          feature_limit=NA_character_,
                          ngram=NA_character_,
                          skips=NA_character_,
                          hash=NA_character_,
                          affix=NA_character_,
                          spelling=NA_character_,
                          interact=NA_character_)
    
    optimization_check <- list(learning_rate=0.5,
                               initial_pass_length=NA_real_,
                               l1=0,
                               l2=0,
                               no_bias_regularization=NA_character_,
                               feature_mask=NA_character_,
                               decay_learning_rate=1,
                               initial_t=0,
                               power_t=0.5,
                               initial_weight=0,
                               random_weights=NA_character_,
                               normal_weights=NA_character_,
                               truncated_normal_weights=NA_character_,
                               sparse_weights=FALSE,
                               input_feature_regularizer=NA_character_)
    
    if (.get_vw_version() == "8.6.1") {
        # Learning algorithm default/check lists
        sgd_check <- list(adaptive=TRUE,
                          normalized=TRUE,
                          invariant=TRUE,
                          adax=FALSE,
                          sparse_l2=0,
                          l1_state=0,
                          l2_state=1)
        bfgs_check <- list(conjugate_gradient=FALSE,
                           hessian_on=FALSE,
                           mem=15,
                           termination=0.00100000005)
        ftrl_check <- list(ftrl_alpha=0.005,
                           ftrl_beta=0.1)
        pistol_check <- list(ftrl_alpha=0.005,
                             ftrl_beta=0.1)
        ksvm_check <- list(reprocess=1,
                           kernel="linear",
                           bandwidth=1.0,
                           degree=2,
                           lambda=-1)
        OjaNewton_check <- list(sketch_size=10,
                                epoch_size=1,
                                alpha=1,
                                alpha_inverse=NA_real_,
                                learning_rate_cnt=2,
                                normalize=1,
                                random_init=1)
        svrg_check <- list(stage_size=1)
        
        # Learning parameters/reductions default/check lists
        binary_check <- list(binary=TRUE)
        oaa_check <- list(num_classes=NA_real_)
        ect_check <- list(num_classes=NA_real_)
        csoaa_check <- list(num_classes=NA_real_,
                            csoaa_ldf="")
        wap_check <- list(num_classes=NA_real_,
                          wap_ldf="")
        log_multi_check <- list(num_classes=NA_real_,
          no_progress=FALSE,
          swap_resistance=4)
        recall_tree_check <- list(num_classes=NA_real_,
          max_candidates=NA_real_,
          bern_hyper=1,
          max_depth=NA_real_,
          node_only=0,
          randomized_routing=0)
        lda_check <- list(num_topics=NA_real_,
                          lda_alpha=0.100000001,
                          lda_rho=0.100000001,
                          lda_D=10000,
                          lda_epsilon=0.00100000005,
                          math_mode=0,
                          minibatch=1)
        multilabel_oaa_check <- list(num_labels=NA_real_)
        classweight_check <- list(class_multiplier=NA_character_)
        mf_check <- list(rank=NA_real_)
        lrq_check <- list(features=NA_character_,
                          lrqdropout=FALSE)
        stage_poly_check <- list(sched_exponent = 1.0,
                            batch_sz = 1000,
                            batch_sz_no_doubling = TRUE)
        bootstrap_check <- list(num_rounds=NA_real_,
                                bs_type="mean")
        autolink_check <- list(degree=2)
        replay_check <- list(level="b",
                        buffer=100,
                        count=1)
        explore_eval_check <- list(explore_eval=TRUE,
          multiplier=NA_real_)
        cb_check<- list(num_costs=NA_real_,
                   cb_type="dr",
                   eval=FALSE,
                   rank_all=FALSE,
                   no_predict=FALSE)
        cb_explore_check <- list(num_actions=NA_real_,
                            explore_type="epsilon",
                            explore_arg=0.05,
                            psi=1,
                            nounif=FALSE,
                            mellowness=0.1,
                            greedify=FALSE,
                            lambda=-1,
                            cb_min_cost=0,
                            cb_max_cost=1,
                            first_only=FALSE)
        cbify_check <- list(num_classes=NA_real_,
                      cbify_cs=FALSE,
                      loss0=0,
                      loss1=1)
        multiworld_test_check <- list(features=NA_character_,
          learn=NA_real_,
          exclude_eval=FALSE)
        nn_check <- list(num_hidden=NA_real_,
                         inpass=FALSE,
                         multitask=FALSE,
                         dropout=FALSE,
                         meanfield=FALSE)
        topk_check <- list(num_k=NA_real_)
        search_check <- list(id=0,
                       search_task=NA_character_,
                       search_interpolation=NA_character_,
                       search_rollout=NA_character_,
                       search_passes_per_policy=1,
                       search_beta=0.5,
                       search_alpha=1e-10,
                       search_total_nb_policies=NA_real_,
                       search_trained_nb_policies=NA_real_,
                       search_allowed_transitions=NA_character_,
                       search_subsample_time=NA_real_,
                       search_neighbor_features=NA_character_,
                       search_rollout_num_steps=NA_real_,
                       search_history_length=1,
                       search_no_caching=FALSE,
                       search_xv=FALSE,
                       search_perturb_oracle=0,
                       search_linear_ordering=FALSE,
                       search_active_verify=NA_real_,
                       search_save_every_k_runs=NA_real_)
        boosting_check <- list(num_learners=NA_real_,
                               gamma=0.100000001,
                               alg="BBM")
        marginal_check <- list(ids=NA_character_,
                               initial_denominator=1,
                               initial_numerator=0.5,
                               compete=FALSE,
                               update_before_learn=0,
                               unweighted_marginals=0,
                               decay=0)
        check_lists <- list(general_check=general_check, feature_check=feature_check, optimization_check=optimization_check,
                                    sgd_check=sgd_check, bfgs_check=bfgs_check, ftrl_check=ftrl_check, pistol_check=pistol_check, ksvm_check=ksvm_check,
                                    OjaNewton_check=OjaNewton_check, svrg_check=svrg_check,
                                    binary_check=binary_check, oaa_check=oaa_check, ect_check=ect_check, csoaa_check=csoaa_check, wap_check=wap_check,
                                    log_multi_check=log_multi_check, recall_tree_check=recall_tree_check, lda_check=lda_check, multilabel_oaa_check=multilabel_oaa_check,
                                    mf_check=mf_check, classweight_check=classweight_check, lrq_check=lrq_check, stage_poly_check=stage_poly_check,
                                    bootstrap_check=bootstrap_check, autolink_check=autolink_check, replay_check=replay_check,
                                    cb_check=cb_check, explore_eval_check=explore_eval_check, cb_explore_check=cb_explore_check, cbify_check=cbify_check,
                                    multiworld_test_check=multiworld_test_check, nn_check=nn_check, topk_check=topk_check, search_check=search_check,
                                    boosting_check=boosting_check, marginal_check=marginal_check)
    } else {
        stop("Vowpal Wabbit v8.6.1 or newer required")
    }

    flatten_check_lists <- .flatten(check_lists)

    assign("check_lists", check_lists, envir=.rvw_global)
    assign("flatten_check_lists", flatten_check_lists, envir=.rvw_global)
}
