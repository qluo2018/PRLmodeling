#!/usr/bin/env Rscript

# adapted from Rudolf Cardinal for Ben Phillips, 17 July 2018
# 
# =============================================================================
# Notes
# =============================================================================
# - Rats
# - Reversal learning task
#   80% v 20% rewarded
# - Two stimuli throughout, which are spatial
#   i.e. stimuli are "left" and "right"
# - Up to 200 trials
# - 5-s timeout; 30s without response,  5-s timeout 
# - Reversal when criterion of 8-in-a-row met
# - Drug: quinpirole 0, 1, 3, and 10 mg/kg (within subjects).
#   Counterbalanced.
#
# SEE OTHER WORK
# ==============
# e.g. Documents/hphi/kanen_reversals/bayesian_reversals_jk.R
#
# MODELS TO DO
# ============
# ... see human_comparison()

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
# RLIB_PREFIX = "/Users/qiang/Documents/MyProgrames/cambridge/behaviouralmodeling/support"
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
FIT_CACHE_DIR <- file.path(THIS_SCRIPT_DIR, "fitcache")
if (!file.exists(FIT_CACHE_DIR)) {
    dir.create(FIT_CACHE_DIR)
}

# =============================================================================
# Data in
# =============================================================================

SOURCEDATA_FILENAME <- "cleaned_data_bari.csv"
fulldata <- data.table(read.csv(SOURCEDATA_FILENAME))
# Rename columns for legibility:
setnames(
    fulldata,
    old=c(
        "ID",
        "Dose",
        "MsGiven",
        "Opt",
        "SideChosen"
    ),
    new=c(
        "subject",
        "dose_mg_kg",
        "reinforced",
        "choice_was_optimal",
        "side_chosen"  # 0/1 is left/right or right/left
    )
)
# There are rows for trials that weren't performed. Remove them:
fulldata <- fulldata[!is.na(reinforced)]
# Recode dose for Stan as an integer:
fulldata[, dose_as_int := mapvalues(
    x = dose_mg_kg,
    from = c(0, 1, 10),
    to = c(1, 2, 3)  # so we can use 1-based indexing for Stan
)]
# Make side_chosen into a 1-based index:
fulldata[, side_chosen_1_based := side_chosen + 1]

n_subjects <- length(unique(fulldata$subject))

# Note: helpfully pre-sorted by subject and dose. But just to be safe:
setkeyv(fulldata, c("subject", "dose_as_int", "trial_number"))

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
        by = .(subject, dose_mg_kg)
    ]
    s2 <- s1[
        ,
        list(
            n = length(p_chose_correctly),
            chose_correctly_mean = mean(p_chose_correctly),
            chose_correctly_sem = sem(p_chose_correctly)),
        by = .(dose_mg_kg)
    ]

    # Graph
    p1 <- (
        ggplot(s2,
               aes(y = chose_correctly_mean,
                   x = dose_mg_kg)) +
            geom_line() +
            geom_point() +
            geom_errorbar(aes(ymin = chose_correctly_mean - chose_correctly_sem,
                              ymax = chose_correctly_mean + chose_correctly_sem))
    )
}

# want to see the group comparison of the number of reversals completed

number.reversal <- s1
colnames(number.reversal)[3]  <- 'number_reversal'
for(subject in unique(fulldata$subject)){
    for(dose in unique(fulldata$dose_mg_kg)){
        number_reversal <- 0
        opt <- matrix()
        opt <- fulldata$choice_was_optimal[which(fulldata$subject==subject & fulldata$dose_mg_kg==dose)]
        count <- 0
        for(i in c(1:length(opt))){
           if(opt[i]==1){
               count <- count + 1
               if(count == 8){
                   number_reversal <- number_reversal + 1
                   count = 0
               }
           }else{
               count = 0
           }
        }
        number.reversal$number_reversal[which(number.reversal$subject==subject & number.reversal$dose_mg_kg==dose)] <- number_reversal
    }
}

s3 <- number.reversal[
    ,
    list(
        n = length(number_reversal),
        number_reversal_mean = mean(number_reversal),
        number_reversal_sem = sem(number_reversal)),
    by = .(dose_mg_kg)
    ]

# Graph
p3 <- (
    ggplot(s3,
           aes(y = number_reversal_mean,
               x = dose_mg_kg)) +
        geom_line() +
        geom_point() +
        geom_errorbar(aes(ymin = number_reversal_mean - number_reversal_sem,
                          ymax = number_reversal_mean + number_reversal_sem))
)

# =============================================================================
# Recode for Stan
# =============================================================================

cat("Recoding for Stan...\n")

standata <- list(
    N_SUBJECTS = n_subjects,
    N_DOSES = 3,
    N_TRIALS = nrow(fulldata),  # TOTAL number of trials
    N_STIMULI = 2,

    subject = as.numeric(fulldata$subject),
    dose = as.numeric(fulldata$dose_as_int),
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
    ITER <- 2000  # rstan default: 2000
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
            model_name=paste("Qiangs_reversals_", modelnum, sep=""),
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

    for (jobnum in 1:4) {
        if (jobnum %in% cmdargs || "all" %in% cmdargs) {
            runner(jobnum)
        }
    }
    # Specials:
    if ("2s" %in% cmdargs) {
        runner("2s")
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

test_extra_contrasts_winner <- function(cutoff)
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

    reward_rate_dose_0 <- stanfunc$sampled_values_from_stanfit(stanfit, "reward_rate_by_dose[1]")
    reward_rate_dose_100 <- stanfunc$sampled_values_from_stanfit(stanfit, "reward_rate_by_dose[3]")
    reward_rate_contrast_100_m_0 <- reward_rate_dose_100 - reward_rate_dose_0
    hdi_reward_rate_contrast_100_m_0 <- stanfunc$hdi(reward_rate_contrast_100_m_0, hdi_proportion = cutoff)
    print(hdi_reward_rate_contrast_100_m_0)
    
    punish_rate_dose_0 <- stanfunc$sampled_values_from_stanfit(stanfit, "punish_rate_by_dose[1]")
    punish_rate_dose_100 <- stanfunc$sampled_values_from_stanfit(stanfit, "punish_rate_by_dose[3]")
    punish_rate_contrast_100_m_0 <- punish_rate_dose_100 - punish_rate_dose_0
    hdi_punish_rate_contrast_100_m_0 <- stanfunc$hdi(punish_rate_contrast_100_m_0, hdi_proportion = cutoff)
    print(hdi_punish_rate_contrast_100_m_0)
    
    reinf_sensitivity_dose_0 <- stanfunc$sampled_values_from_stanfit(stanfit, "reinf_sensitivity_by_dose[1]")
    reinf_sensitivity_dose_100 <- stanfunc$sampled_values_from_stanfit(stanfit, "reinf_sensitivity_by_dose[3]")
    reinf_sensitivity_contrast_100_m_0 <- reinf_sensitivity_dose_100 - reinf_sensitivity_dose_0
    hdi_reinf_sensitivity_contrast_100_m_0 <- stanfunc$hdi(reinf_sensitivity_contrast_100_m_0, hdi_proportion = cutoff)
    print(hdi_reinf_sensitivity_contrast_100_m_0)
    
    side_stickiness_dose_0 <- stanfunc$sampled_values_from_stanfit(stanfit, "side_stickiness_by_dose[1]")
    side_stickiness_dose_100 <- stanfunc$sampled_values_from_stanfit(stanfit, "side_stickiness_by_dose[3]")
    side_stickiness_contrast_100_m_0 <- side_stickiness_dose_100 - side_stickiness_dose_0
    hdi_side_stickiness_contrast_100_m_0 <- stanfunc$hdi(side_stickiness_contrast_100_m_0, hdi_proportion = cutoff)
    print(hdi_side_stickiness_contrast_100_m_0)
    
    
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
    write.csv(d, file="output/grp_para_bariSSRI.csv")
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
    d[, drugnum := NA_integer_]
    for (i in 1:nrow(d)) {
        baseparam_subj_drug <- strsplit(x=d$parameter[i], split=splitter,
                                        fixed=FALSE, perl=TRUE)[[1]]
        d$baseparam[i] <- baseparam_subj_drug[1]
        d$subject[i]   <- as.integer(baseparam_subj_drug[2])
        d$drugnum[i]   <- as.integer(baseparam_subj_drug[3])
    }
    d[, drugname := plyr::mapvalues(
        drugnum,
        from=c(1, 2, 3),
        to=c("Placebo", "1mg", "10mg")
    )]
    d <- d[]  # fix data.table nonprinting bug
    return(d)
}

save_individual_subject_params <- function()
{
    d <- extract_per_subject_params()
    write.csv(d, file="output/indi_para_bariSSRI.csv")
}

# star-wars-plot
presentation <- function(model = NULL)
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
            list(name="ct_reward_rate_100_minus_25", desc="Reward rate: 10 mg/kg - 1 mg/kg"),
            list(name="ct_reward_rate_25_minus_0", desc="Reward rate: 1 mg/kg - Vehicle"),
            list(name="ct_punish_rate_100_minus_25", desc="Punishment rate: 10 mg/kg - 1 mg/kg"),
            list(name="ct_punish_rate_25_minus_0", desc="Punishment rate: 1 mg/kg - Vehicle"),
            list(name="ct_reinf_sensitivity_100_minus_25", desc="Reinf. sensitivity: 10 mg/kg - 1 mg/kg"),
            list(name="ct_reinf_sensitivity_25_minus_0", desc="Reinf. sensitivity: 1 mg/kg - Vehicle"),
            list(name="ct_side_stickiness_100_minus_25", desc="Side (location) stickiness: 10 mg/kg - 1 mg/kg"),
            list(name="ct_side_stickiness_25_minus_0", desc="Side (location) stickiness: 1 mg/kg - Vehicle")
        ),
        title="Rat SSRI experiment"
    )
    ggsave("output/ct_bariSSRI.pdf", p2_ct, width=17.5, height=7.5, units="cm")
}

# star-wars-plot
presentation1 <- function(model = NULL)
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
            list(name="ct_reward_rate_25_minus_0", desc="Reward rate: 1 mg/kg - Vehicle"),
            list(name="ct_punish_rate_25_minus_0", desc="Punishment rate: 1 mg/kg - Vehicle"),
            list(name="ct_reinf_sensitivity_25_minus_0", desc="Reinf. sensitivity: 1 mg/kg - Vehicle"),
            list(name="ct_side_stickiness_25_minus_0", desc="Side (location) stickiness: 1 mg/kg - Vehicle")
        ),
        title="Rat SSRI experiment"
    )
    ggsave("output/ct_bariSSRI_1mg_7595.pdf", p2_ct, width=17.5, height=7.5, units="cm")
}

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
            list(name="reward_rate_by_dose", desc="Reward rate: 10 mg/kg - Vehicle"),
            list(name="punish_rate_by_dose", desc="Punishment rate: 10 mg/kg - Vehicle"),
            list(name="reinf_sensitivity_by_dose", desc="Reinf. sensitivity: 10 mg/kg - Vehicle"),
            list(name="side_stickiness_by_dose", desc="Side (location) stickiness: 10 mg/kg - Vehicle")
        ),
        title="Rat SSRI experiment"
    )
    ggsave("output/ct_bariSSRI_10mg7595.pdf", p2_ct, width=17.5, height=7.5, units="cm")
}

cmdargs <- "all"
run_models(cmdargs)

# Now try e.g.
# human_comparison()
# print(comparison)
# write.csv(comparison, file = "output/comp_bariSSRI.csv")
# launch_shinystan(ss2)
# x <- stanfunc$annotated_parameters(fit2)
# x[grep("^ct.*", parameter)]






