#!/usr/bin/env Rscript

# Rudolf Cardinal for JK, 19 Dec 2017
# . ~/activate_venv.sh
# pip install cardinal_pythonlib>=1.0.9
# sbatch slurm_submit_qiang.py all
# squeue -u ql296
# =============================================================================
# Notes
# =============================================================================
# - Groups: stimulant abusers ("Drug" in original),  controls, OCD
# - Drug: placebo, pramipexole, amisulpride (WITHIN subjects)
# - task: probabilistic reversal learning
#
# each subject run in two parts
#   Part 1 = stim 1 vs stim 2
#   Part 2 = stim 3 vs stim 4
# (NB a couple of subjects get a repeat of part 1)
# 20 blocks per subject in whole experiment; 18 reversals
# e.g. part 1: block 1 = discrim, block 2 = first reversal, block 3 = re-reversal
#      part 1: block 1 = start again...
#
# Reversal criteria varies a bit, so as to be unpredictable.
# Can stop after 200th trial if subject doesn't succeed.
#
# OTHER NOTABLE MODELS OF SIMILAR TASKS
#
# - Hauser 2017 Psych Med, PMID 28065182
#   ====================================
#   - Task = probabilistic reversal learning
#   - Learning rule
#   (A) Rescorla-Wagner with single reinforcement rate
#   (B) ... with two learning rates (one for positive error, one for negative
#       error, i.e. a reward and a punishment rate)
#   NOTE that they alter the value of the UNCHOSEN stimulus too.
#   - Choice rule
#   (1) Softmax with a single inverse temperature (beta)
#   (2) ... with a perseveration parameter, i.e. softmax of
#       (value + gamma * is_repeat)
#   - All combinations of {A, B} and {1, 2}.
#     Of these, A2 won, i.e. single reinforcement rate, plus perseveration
#     parameter (stimulus stickiness -- I presume stimulus, anyway; "option";
#     see Supplementary re "rep(X)". "Rescorla-Wagner perseveration" in
#     Supplementary Table 2.
#   - They also checked and discarded a "Bayesian hierarchical model which
#     includes a volatility hierarchy; Mathys et al. 2011" -- less good.
#
# - den Ouden 2013 Neuron PMID 24267657
#   ===================================
#   - Task = probabilistic reversal learning
#       - on each trial, 2 stimuli in 2 from 4 locations
#           [= 4 locations = Nikolina/escitalopram and tryptophan depletion method]
#   - Learning rules
#     (1) EWA (experience-weighted attraction)
#       - learning rate-style parameter phi
#       - "experience" weight parameter rho
#         - balances standard TD model with one that weights all trials equally
#       - softmax inv. temp beta
#     (2) RP (reward prediction)
#         standard TD model with separate reward/punishment rate and softmax
#         inverse temperature beta
#         ... no update to unchosen stimulus (p1098)
#     ... EWA was better.
#     Specifies their priors (p1094).
#
# MODELS TO DO
# ============
# - Model 1: reward rate, punishment rate, reinf sensitivity [= softmax inv. temp], side stickiness (with common intersubject SDs)
# - Model 2: reward rate, punishment rate, reinf_sens, side stickiness, stimulus stickiness
#           = adds stimulus stickiness to model 1
# - Model 3: reward, punishment, reinf_sens, stimulus stickiness
#           = model 1 but stimulus, not side, stickiness
# - Model 4: reward, punishment, reinf_sens
#           = model 1 without side stickiness
#           = den Ouden 2013 "RP" model
# - Model 5: combined reward rate, reinf_sens, stimulus stickiness
#           = Hauser 2017 winning model
# - Model 6: den Ouden 2013 "EWA" (their winning) model
#
# SOFTMAX CODING
# ==============
# - When you just have reinforcement, "reinforcement sensitivity" is the same
#   as "softmax inverse temperature". For values VA, VB:
#           p(A) = exp(beta * VA)
#                  ------------------------------
#                  exp(beta * VA) + exp(beta * VB)
#
# - When you also have e.g. side stickiness, you can parameterize it in two
#   ways:
#
#   Firstly, e.g. Hauser 2017:
#
#               beta * (VA + gamma * side_measure)
#
#       and people will continue to speak of beta as "softmax inverse
#       temperature", and of gamma as the side stickiness sensitivity (which
#       is implicitly relative to the effects of reinforcement).
#
#   Secondly, e.g. Christakou 2013:
#
#               reinf_sens * VA + side_stickiness_sensitivity * side_measure
#
#   Obviously there is an equivalence, i.e.
#               reinf_sens ≡ beta
#               side_stickiness_sensitivity ≡ beta * gamma
#
#   Is there a *computational* reason to prefer one or the other
#   parameterization?
#   Christakou 2013 use priors of reinf_sens ~ Gamma(2, 1) and
#   stickiness_sens ~ N(0, 1).
#   Not obviously so...
#   ... maybe something to return to if we can't get proper convergence.
#   ... and not relevant whenever we fix beta to 1.

# =============================================================================
# Command-line arguments
# =============================================================================
# Keep it simple

possible_cmd_args <- c(
    "all",
    "1", "2", "3", "4",
    "2s"
)
cmdargs <- commandArgs(trailingOnly=TRUE)
cat("Command-line arguments:", paste(cmdargs, collapse=" "), "\n")
if (!all(cmdargs %in% possible_cmd_args)) {
    stop("Invalid arguments; must be one or more of: ",
         paste(possible_cmd_args, collapse=", "))
}

# =============================================================================
# Libraries
# =============================================================================

#library(Rcpp)  # or Stan will fail with: could not find function "cpp_object_initializer"  -- also, load it early on osprey (version conflicts)
#library(data.table)
#library(ggplot2)
#library(lme4)
#library(lmerTest)
#library(parallel)
#library(plyr)
#library(readr)
#library(rstan)  # to install: install.packages("rstan")
#library(semver)
#library(shinystan)
#library(bridgesampling)

version

for (package in c('Rcpp', 'data.table', 'ggplot2', 'lme4', 'lmerTest', 'parallel', 'plyr', 'readr', 'rstan', 'semver', 'shinystan', 'bridgesampling')) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package, repos="http://cran.us.r-project.org")
        library(package, character.only=T)
    }
}


# Remote access:
# RLIB_PREFIX = "http://egret.psychol.cam.ac.uk/rlib"
# Local access:
# RLIB_PREFIX = "/home/rudolf/Documents/code/rlib"
RLIB_PREFIX = "~/projects/behmodeling/new/compbehmodeling/LSD/support"

source(paste(RLIB_PREFIX, "listassign.R", sep="/"))
source(paste(RLIB_PREFIX, "listfunc.R", sep="/"))
source(paste(RLIB_PREFIX, "miscfile.R", sep="/"))
source(paste(RLIB_PREFIX, "miscmath.R", sep="/"))
source(paste(RLIB_PREFIX, "miscstat.R", sep="/"))
source(paste(RLIB_PREFIX, "rpm.R", sep="/"))
source(paste(RLIB_PREFIX, "stanfunc.R", sep="/"))

CODE_COMMON_FUNCTIONS <- readr::read_file(paste(
    RLIB_PREFIX, "commonfunc.stan", sep="/"))

sem <- miscstat$sem

# Check versions:

# sessionInfo()  # which versions?
STAN_VERSION = sessionInfo(package="rstan")$otherPkgs$rstan$Version
cat("Found Stan version", STAN_VERSION, "\n")
if (semver::parse_version(STAN_VERSION) < "2.16.0") {
    stop(paste("Expecting Stan v2.16 or higher; found", STAN_VERSION))
}

# As advised by Stan:

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# =============================================================================
# Directories
# =============================================================================

THIS_SCRIPT_DIR = miscfile$current_script_directory()
setwd(THIS_SCRIPT_DIR)
FIT_CACHE_DIR <- paste(THIS_SCRIPT_DIR, "fitcache", sep="/")
if (!file.exists(FIT_CACHE_DIR)) {
    dir.create(FIT_CACHE_DIR)
}

# =============================================================================
# Data in
# =============================================================================


SOURCEDATA_FILENAME <- "cleaned_depletion_bari.csv"
fulldata <- data.table(read.csv(SOURCEDATA_FILENAME))

# Rename columns for legibility:
setnames(
    fulldata,
    old=c(
        "session",
        "patient",
        "Group",
        "MsGiven",
        "Opt",
        "SideChosen"
    ),
    new=c(
        "sess",        # 1-7  # within subject
        "subject",
        "group",       # 0--Sham, 1--depletion    # between subject
        "reinforced",
        "choice_was_optimal",
        "side_chosen"  # 0/1 is left/right or right/left
    )
)
# There are rows for trials that weren't performed. Remove them:
fulldata <- fulldata[!is.na(reinforced)]

# Recode dose for Stan as an integer:
fulldata[, group := mapvalues(
    x = group,
    from = c(0, 1),   
    to = c(1, 2)  # so we can use 1-based indexing for Stan
)]

# Make side_chosen into a 1-based index:
fulldata[, side_chosen_1_based := side_chosen + 1]
n_subjects <- length(unique(fulldata$subject))

setkeyv(fulldata, c("subject", "sess", "trial_number"))


# =============================================================================
# Basic plot + ANOVA
# =============================================================================

SHOW_BASICS <- TRUE

if (SHOW_BASICS) {

    # Summary for graph
    s1 <- fulldata[
        ,
        list(
            p_chose_correctly = mean(choice_was_optimal)
        ),
        by = .(subject, sess, group)
        ]
    s2 <- s1[
        ,
        list(
            n = length(p_chose_correctly),
            chose_correctly_mean = mean(p_chose_correctly),
            chose_correctly_sem = sem(p_chose_correctly)),
        by = .(sess, group)
        ]
    s3 <- s1[
        ,
        list(
            n = length(p_chose_correctly),
            chose_correctly_mean = mean(p_chose_correctly),
            chose_correctly_sem = sem(p_chose_correctly)),
        by = .(group)
        ]
    

    #COLOUR_SCALE = rep(c("red", "green", "blue", "black", ""), each=3, times=3)
    #SHAPE_SCALE = rep(c(21, 22, 24), times=6)
    #LINETYPE_SCALE = rep(c("solid", "dotted"), each=9)

    # Graph
    p1 <- ggplot(s2,
               aes(y = chose_correctly_mean,
                   x = sess,
                   group = group) ) +
                   geom_line() +
                   geom_point() +
                   geom_errorbar(aes(ymin = chose_correctly_mean - chose_correctly_sem,
                                     ymax = chose_correctly_mean + chose_correctly_sem))
    p1
    m1 <- lmer(
        p_chose_correctly
        ~ group * sess  + (1 | subject),
        data=s1
        # MAY NOT HAVE CORRECT RANDOM EFFECTS STRUCTURE
    )
    anova(m1)
    
    p3 <- ggplot(s3,
                 aes(y = chose_correctly_mean,
                     x = group,
                     group = group) ) +
        geom_line() +
        geom_point() +
        geom_errorbar(aes(ymin = chose_correctly_mean - chose_correctly_sem,
                          ymax = chose_correctly_mean + chose_correctly_sem))
    p3
    m3 <- lm(
        p_chose_correctly
        ~ group,
        data=s1
        # MAY NOT HAVE CORRECT RANDOM EFFECTS STRUCTURE
    )
    p3
    anova(m3)
}

# =============================================================================
# Recode for Stan
# =============================================================================

cat("Recoding for Stan...\n")

groupmap <- fulldata[, list(group=unique(group)), by=.(subject)]
setorder(groupmap, subject)  # reorder groupmap by the subject ID

standata <- list(
    N_SUBJECTS = n_subjects,
    N_SESS = 7, # 7 sessions within subject
    N_TRIALS = nrow(fulldata),  # TOTAL number of trials
    N_STIMULI = 2,
    N_GROUPS = 2, # sham and depletion 2 groups between subject
    
    group_membership = as.numeric(groupmap$group),
    subject = as.numeric(fulldata$subject),
    sess = as.numeric(fulldata$sess),
    side_chosen = as.numeric(fulldata$side_chosen_1_based),  # 1 left, 2 right
    responded_right = as.numeric(fulldata$side_chosen),  # 1 for right
    reinforced = as.numeric(fulldata$reinforced)
)

cat("... recoding done\n")





# =============================================================================
# Run!
# =============================================================================

CODE_CONSTANTS <- readr::read_file(paste(
    THIS_SCRIPT_DIR, "constants.stan", sep="/"))
CODE_DATA <- readr::read_file(paste(
    THIS_SCRIPT_DIR, "data.stan", sep="/"))


subCommonFunc <- function(code)
{
    # Since rstan doesn't yet support telling stan properly about the paths it
    # should use for #include, the #include command fails. So, the manual way:
    code <- gsub("// #include commonfunc.stan", CODE_COMMON_FUNCTIONS, code)
    code <- gsub("// #include constants.stan", CODE_CONSTANTS, code)
    code <- gsub("// #include data.stan", CODE_DATA, code)
    return(code)
}

run_models <- function(cmdargs)
{
    # WRITES TO GLOBAL NAMESPACE

    CHAINS <- 8  # rstan default: 4
    ITER <- 4000  # rstan default: 2000
    INIT <- "0"  # rstan default: "random"
    SEED <- 1234  # rstan default: a random number

    SLOW <- FALSE
    if (SLOW) {
        ADAPT_DELTA <- 0.999  # range 0-1; rstan default: 0.8
        STEPSIZE <- 0.001  # rstan default: 1 [https://github.com/stan-dev/rstan/blob/develop/rstan3/R/AllClass.R]
        MAX_TREEDEPTH <- 20  # rstan default: 10 [https://github.com/stan-dev/rstan/blob/develop/rstan3/R/AllClass.R]
    } else {
        ADAPT_DELTA <- 0.99
        STEPSIZE <- 1  # rstan default: 1 [https://github.com/stan-dev/rstan/blob/develop/rstan3/R/AllClass.R]
        MAX_TREEDEPTH <- 10  # rstan default: 10 [https://github.com/stan-dev/rstan/blob/develop/rstan3/R/AllClass.R]
    }

    runner <- function(modelnum) {
        cat(paste("Running model ", modelnum, "...\n", sep=""))
        fit_filename <- paste(
            FIT_CACHE_DIR, "/reversals_fit", modelnum, ".rds", sep="")
        bridge_filename <- paste(
            FIT_CACHE_DIR, "/reversals_fit", modelnum, "_bridge.rds", sep="")
        code_filename <- paste(
            FIT_CACHE_DIR, "/temp_code_", modelnum, ".cpp", sep="")

        code <- readr::read_file(paste(
            THIS_SCRIPT_DIR, "/reversals_", modelnum, ".stan", sep=""))
        code <- subCommonFunc(code)

        fit <- stanfunc$load_or_run_stan(
            data=standata,
            model_code=code,
            fit_filename=fit_filename,
            model_name=paste("reversals_", modelnum, sep=""),
            save_code_filename=code_filename,
            chains=CHAINS,
            iter=ITER,
            init=INIT,
            seed=SEED,
            control = list(
                adapt_delta=ADAPT_DELTA,
                stepsize=STEPSIZE,
                max_treedepth=MAX_TREEDEPTH
            )
        )

        # View the model in ShinyStan
        cat("Making ShinyStan object...\n")
        ss <- shinystan::as.shinystan(fit)
        cat("... made\n")
        # shinystan::launch_shinystan(ss)

        b <- stanfunc$load_or_run_bridge_sampler(
            stanfit=fit,
            filename=bridge_filename,
            model_code=code,
            data=standata
        )

        fit_name <- paste("fit", modelnum, sep="")
        ss_name <- paste("ss", modelnum, sep="")
        b_name <- paste("b", modelnum, sep="")
        listfunc$list_assign(c(fit_name, ss_name, b_name),
                             list(fit, ss, b), global=TRUE)
    }

    for (jobnum in c(1:4)) {
        if (jobnum %in% cmdargs || "all" %in% cmdargs) {
            runner(jobnum)
        }
    }
    # Specials:
    if ("2s" %in% cmdargs) {
        runner("2s")
    }
    if ("3s" %in% cmdargs) {
        runner("3s")
    }
}


human_comparison <- function()
{
    # WRITES TO GLOBAL NAMESPACE
    run_models("all")
    
    comparison <<- stanfunc$compare_model_evidence(
        list(
            list(
                name="Model 1: reward rate, punishment rate, reinf sensitivity [= softmax inv. temp]",
                bridgesample=b1, stanfit=fit1),
            list(
                name="Model 2: reward rate, punishment rate, reinf_sens, side stickiness",
                bridgesample=b2, stanfit=fit2),
            list(
                name="Model 3: combined reward rate, reinf_sens, side stickiness",
                bridgesample=b3, stanfit=fit3),
            list(
                name="Model 4: den Ouden 2013 'EWA' model",
                bridgesample=b4, stanfit=fit4)
        ),
        detail=TRUE
    )
    
}


test_extra_contrasts_winner <- function()
{
    stanfit <- fit2
    # REMBER CODING, as above and in constants.stan
    # int DOSE_0 = 1;
    # int DOSE_25 = 2;
    # int DOSE_100 = 3;


    # Want HDI for the following contrasts

    # quinpirole 100 - 0

    # note that the "permuted" option to rstan::extract is OK -- this permutes
    # (= indifferent), and merges across chains (= good), and ensures the
    # permutation order is fixed for a given stanfit object (= good).

    stim_stickiness_by_dose_0 <- stanfunc$sampled_values_from_stanfit(stanfit, "side_stickiness_by_group[1]")
    stim_stickiness_by_dose_75 <- stanfunc$sampled_values_from_stanfit(stanfit, "side_stickiness_by_group[2]")
    stim_stickiness_by_dose_75_m_0 <- stim_stickiness_by_dose_75 - stim_stickiness_by_dose_0
    #hdi_stim_stickiness_by_dose_75_m_0 <- stanfunc$hdi(stim_stickiness_by_dose_75_m_0, hdi_proportion = 0.99)
    #print(hdi_stim_stickiness_by_dose_75_m_0)

    hdi_stim_stickiness_by_dose_75_m_0 <- stanfunc$hdi(stim_stickiness_by_dose_75_m_0, hdi_proportion = 0.75)
    print(hdi_stim_stickiness_by_dose_75_m_0)

    reward_rate_dose_0 <- stanfunc$sampled_values_from_stanfit(stanfit, "reward_rate_by_group[1]")
    reward_rate_dose_75 <- stanfunc$sampled_values_from_stanfit(stanfit, "reward_rate_by_group[2]")
    reward_rate_contrast_75_m_0 <- reward_rate_dose_75 - reward_rate_dose_0
    #hdi_reward_rate_contrast_75_m_0 <- stanfunc$hdi(reward_rate_contrast_75_m_0, hdi_proportion = 0.90)
    #print(hdi_reward_rate_contrast_75_m_0)
    
    hdi_reward_rate_contrast_75_m_0 <- stanfunc$hdi(reward_rate_contrast_75_m_0, hdi_proportion = 0.75)
    print(hdi_reward_rate_contrast_75_m_0)

    punish_rate_dose_0 <- stanfunc$sampled_values_from_stanfit(stanfit, "punish_rate_by_group[1]")
    punish_rate_dose_75 <- stanfunc$sampled_values_from_stanfit(stanfit, "punish_rate_by_group[2]")
    punish_rate_contrast_75_m_0 <- punish_rate_dose_75 - punish_rate_dose_0
    #reward_punish_contrast <- reward_rate_contrast_75_m_0 - punish_rate_contrast_75_m_0
    hdi_reward_punish_contrast <- stanfunc$hdi(punish_rate_contrast_75_m_0, hdi_proportion = 0.75)
    print(hdi_reward_punish_contrast)


    reinf_sensitivity_dose_0 <- stanfunc$sampled_values_from_stanfit(stanfit, "reinf_sensitivity_by_group[1]")
    reinf_sensitivity_dose_75 <- stanfunc$sampled_values_from_stanfit(stanfit, "reinf_sensitivity_by_group[2]")
    reinf_sensitivity_contrast_75_m_0 <- reinf_sensitivity_dose_75 - reinf_sensitivity_dose_0
    hdi_reinf_sensitivity_contrast_75_m_0 <- stanfunc$hdi(reinf_sensitivity_contrast_75_m_0, hdi_proportion = 0.75)
    print(hdi_reward_punish_contrast)

    
    # etc.!
}

summarize_for_group_level_params <- function(stanfit)
{
    ap <- stanfunc$annotated_parameters(stanfit)
    ap <- ap[parameter != "lp__"]
    ap <- ap[!grep(".*_subject_effect.*", parameter)]
    return(ap)
}

save_group_level_params <- function(stanfit)
{
    d <- summarize_for_group_level_params(stanfit)
    write.csv(d, file="output/grp_para_bariDHT.csv")
}


extract_per_subject_params <- function(model = NULL)
{
    if (is.null(model)) {
        model <- fit2s  # winning model, per-subject parameter version
    }
    
    d <- stanfunc$summary_data_table(model)
    d <- d[like(parameter, "final")]
    splitter <- "[[,\\]]"  # regex for these three characters: [ , ]
    d[, baseparam := NA_character_]
    d[, subject := NA_integer_]
    d[, group := NA_integer_]
    for (i in 1:nrow(d)) {
        baseparam_subj <- strsplit(x=d$parameter[i], split=splitter,
                                        fixed=FALSE, perl=TRUE)[[1]]
        d$baseparam[i] <- baseparam_subj[1]
        d$subject[i]   <- as.integer(baseparam_subj[2])
        d$group[i]   <- as.integer(groupmap[d$subject[i]]$group)
    }
    d[, groupname := plyr::mapvalues(
        group,
        from=c(1, 2),
        to=c("Sham", "Depletion")
    )]
    d <- d[]  # fix data.table nonprinting bug
    return(d)
}

save_individual_subject_params <- function()
{
    d <- extract_per_subject_params()
    write.csv(d, file="output/indi_para_bariDHT.csv")
}


# star-wars-plot
presentation2 <- function(model = NULL)
{
    # 2018-06-25: of the 6, winning model is #2. Rhat not perfect but is 1.47.
    # So:
    
    if (is.null(model)) {
        model <- fit2  # winning model
    }
    
    summary_winning_model <<- stanfunc$annotated_parameters(model)
    
    p2_ct <- stanfunc$plot_multiple_stanfit_parameters_vstack(
        model,
        list(
            list(name="ct_reward_rate_sham_minus_depletion", desc="Reward rate: Depletion - Sham"),
            list(name="ct_punish_rate_sham_minus_depletion", desc="Punishment rate: Depletion - Sham"),
            list(name="ct_reinf_sensitivity_sham_minus_depletion", desc="Reinf. sensitivity: Depletion - Sham"),
            list(name="ct_side_stickiness_sham_minus_depletion", desc="Side (location) stickiness: Depletion - Sham")
        ),
        title="Rat depletion experiment",
        reverse=TRUE
    )
    ggsave("output/ct_bariDHT7595.pdf", p2_ct, width=17.5, height=7.5, units="cm")
}

presentation3 <- function(model = NULL)
{
    # 2018-06-25: of the 6, winning model is #2. Rhat not perfect but is 1.47.
    # So:
    
    if (is.null(model)) {
        model <- fit3  # winning model
    }
    
    summary_winning_model <<- stanfunc$annotated_parameters(model)
    
    p2_ct <- stanfunc$plot_multiple_stanfit_parameters_vstack(
        model,
        list(
            list(name="ct_reinforcement_rate_sham_minus_depletion", desc="Reward rate: Depletion - Sham"),
            list(name="ct_reinf_sensitivity_sham_minus_depletion", desc="Reinf. sensitivity: Depletion - Sham"),
            list(name="ct_side_stickiness_sham_minus_depletion", desc="Side (location) stickiness: Depletion - Sham")
        ),
        title="Rat depletion experiment",
        reverse=TRUE
    )
    ggsave("output/ct_bariDHTmodel3.pdf", p2_ct, width=17.5, height=7.5, units="cm")
}

cmdargs <- "all"
run_models(cmdargs)

# Now try e.g.
# human_comparison()
# print(comparison)
# write.csv(comparison, file = "output/comp_bariDHT.csv")
# launch_shinystan(ss2)
# x<- stanfunc$annotated_parameters(fit2)
# x[grep("^ct.*", parameter)]

