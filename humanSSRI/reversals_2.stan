/*

Code for Ben Phillips's reversal task, by RNC, 17 Jul 2018.

Model 2: reward rate, punishment rate, reinf_sens, side stickiness

*/

functions {

    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // Common functions
    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // ... substitute in R while there's a bug in rstan's path specification to stan's #include
// #include commonfunc.stan

}

data {
    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // Data
    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // ... substitute in R while there's a bug in rstan's path specification to stan's #include
// #include data.stan
}

transformed data {
    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // Constants
    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // ... substitute in R while there's a bug in rstan's path specification to stan's #include
// #include constants.stan

}

parameters {

    // ========================================================================
    // Fixed effects
    // ========================================================================
    // Some lengthy thought into the best way to code this (e.g. cells means
    // vs effect/dummy coding); see scanned paper document.

    real<lower=0, upper=1> reward_rate_by_group[N_GROUPS];
    real<lower=0, upper=1> punish_rate_by_group[N_GROUPS];
    real<lower=0> reinf_sensitivity_by_group[N_GROUPS];
    real side_stickiness_by_group[N_GROUPS];

    // ========================================================================
    // Random effects
    // ========================================================================

    real<lower=0> reward_rate_intersubject_sd;
    real<lower=0> punish_rate_intersubject_sd;
    real<lower=0> reinf_sensitivity_intersubject_sd;
    real<lower=0> side_stickiness_intersubject_sd;

    vector[N_SUBJECTS] reward_rate_subject_effect_raw_unit_normal;
    vector[N_SUBJECTS] punish_rate_subject_effect_raw_unit_normal;
    vector[N_SUBJECTS] reinf_sensitivity_subject_effect_raw_unit_normal;
    vector[N_SUBJECTS] side_stickiness_subject_effect_raw_unit_normal;
}

transformed parameters {

    vector[N_SUBJECTS] reward_rate_subject_effect;
    vector[N_SUBJECTS] punish_rate_subject_effect;
    vector[N_SUBJECTS] reinf_sensitivity_subject_effect;
    vector[N_SUBJECTS] side_stickiness_subject_effect;

    reward_rate_subject_effect = getReparameterizedNormal_VRR_lp(
        reward_rate_subject_effect_raw_unit_normal,
        0, reward_rate_intersubject_sd);
    punish_rate_subject_effect = getReparameterizedNormal_VRR_lp(
        punish_rate_subject_effect_raw_unit_normal,
        0, punish_rate_intersubject_sd);
    reinf_sensitivity_subject_effect = getReparameterizedNormal_VRR_lp(
        reinf_sensitivity_subject_effect_raw_unit_normal,
        0, reinf_sensitivity_intersubject_sd);
    side_stickiness_subject_effect = getReparameterizedNormal_VRR_lp(
        side_stickiness_subject_effect_raw_unit_normal,
        0, side_stickiness_intersubject_sd);
}

model {

    // Calculated probability of choosing the side labelled 1:
    vector[N_TRIALS] p_choose_stim1;

    // ========================================================================
    // Specify priors
    // ========================================================================

    sampleBeta_ARR_lp(reward_rate_by_group, PRIOR_BETA_SHAPE1, PRIOR_BETA_SHAPE2);
    sampleBeta_ARR_lp(punish_rate_by_group, PRIOR_BETA_SHAPE1, PRIOR_BETA_SHAPE2);
    sampleGamma_ARR_lp(reinf_sensitivity_by_group, PRIOR_GAMMA_ALPHA_FOR_REINF_SENSITIVITY_MEAN, PRIOR_GAMMA_BETA_FOR_REINF_SENSITIVITY_MEAN);  // positive only
    sampleNormal_ARR_lp(side_stickiness_by_group, 0, 1);  // can be negative; see Christakou (2013)

    sampleNormalLowerBound_RRR_lp(reward_rate_intersubject_sd, 0, PRIOR_HALF_NORMAL_SD_FOR_SD_IN_RANGE_0_1, 0);  // lower bound of 0
    sampleNormalLowerBound_RRR_lp(punish_rate_intersubject_sd, 0, PRIOR_HALF_NORMAL_SD_FOR_SD_IN_RANGE_0_1, 0);  // lower bound of 0
    sampleNormalLowerBound_RRR_lp(reinf_sensitivity_intersubject_sd, 0, PRIOR_HALF_NORMAL_SIGMA_FOR_REINF_SENSITIVITY_SD, 0);  // lower bound of 0
    sampleNormalLowerBound_RRR_lp(side_stickiness_intersubject_sd, 0, PRIOR_HALF_NORMAL_SD_FOR_SD_IN_RANGE_0_1, 0);  // lower bound of 0

    // ========================================================================
    // Reinforcement learning model: calculates p_choose_rhs
    // ========================================================================
    {
        // Start a new block because we will define local-only variables.
        // NO RANGE CONSTRAINTS allowed here.

        // Keeping track of the current subject/drug
        int s = 0; // current subject, starting with an invalid value

        // Value tracking
        vector[N_STIMULI] stim_value;  // value of each stimulus; range 0 to 1
        vector[N_STIMULI] side_stickiness_trace;  // integer contents, but needs to be real for vector multiplication

        // Temporary variables: RL calculations
        vector[N_STIMULI] softmax_inputs;
        real predicted_outcome;
        real actual_outcome;
        real prediction_error;
        real value_change;

        // Temporary variables: final parameter values
        // NO RANGE CONSTRAINTS PERMITTED HERE.
        real reward_rate_final;
        real punish_rate_final;
        real reinf_sensitivity_final;
        real side_stickiness_final;

        int chosen_stimulus_index;
        real temp_new_value;
        int g;
        int d;
        int first_trial_of_dose;
        
        // Iterate through trials
        for (t in 1:N_TRIALS) {
            // Starting a new subject? Read subject parameters and reset discrimination.
            if (s != subject[t]) {
                s = subject[t];
                g = group_membership[s];
                d = -1;   // invalid value; will trigger the "new sess" code below
            }
            if (d != sess[t]) {
                // Starting a new session? Reset starting values.
                d = sess[t];
                first_trial_of_dose = 1;
                
                for (stim in 1:N_STIMULI) {  // better way to assign to a vector??
                     stim_value[stim] = 0.5;  // all stimuli start with neutral value
                }
                
                reward_rate_final = reward_rate_by_group[g] + reward_rate_subject_effect[s];
                punish_rate_final = punish_rate_by_group[g] + punish_rate_subject_effect[s];
                reinf_sensitivity_final = reinf_sensitivity_by_group[g]  + reinf_sensitivity_subject_effect[s];
                side_stickiness_final = side_stickiness_by_group[g] + side_stickiness_subject_effect[s];
                
                reward_rate_final = bound(reward_rate_final, 0, 1);
                punish_rate_final = bound(punish_rate_final, 0, 1);
                reinf_sensitivity_final = boundLower(reinf_sensitivity_final, 0);
                
                
            }else{
                first_trial_of_dose = 0;
            }
            
            // Side stickiness:
            if (!first_trial_of_dose) {
                // Assign 1 to the side the subject responded to on the last
                // trial, and 0 to the other side.
                side_stickiness_trace[STIM1] = responded_stim1[t - 1];
                side_stickiness_trace[STIM2] = 1 - responded_stim1[t - 1];
            } else {
                // no better way of assigning constant to vector?
                side_stickiness_trace[STIM1] = 0;
                side_stickiness_trace[STIM2] = 0;
            }
            // side_stickiness_final may be negative
            
            // ----------------------------------------------------------------
            // Calculate p
            // ----------------------------------------------------------------
            
            softmax_inputs = (
                reinf_sensitivity_final * stim_value +
                side_stickiness_final * side_stickiness_trace
            );
                
            p_choose_stim1[t] = softmaxNth(softmax_inputs, STIM1);
                
            // ----------------------------------------------------------------
            // Update
            // ----------------------------------------------------------------
                
            // We update the value of the thing we chose based on the outcome
            // we received.
                
            chosen_stimulus_index = stim_chosen[t];
                
            predicted_outcome = stim_value[chosen_stimulus_index];
            actual_outcome = reinforced[t];
            prediction_error = actual_outcome - predicted_outcome;
                
            if (prediction_error > 0) {
                // better than expected: reward, or extinction of punishment
                value_change = prediction_error * reward_rate_final;
            } else {
                // worse than expected: punishment, or extinction of reward
                // (or prediction_error == 0, in which case it
                // doesn't matter which rate constant we're using!)
                value_change = prediction_error * punish_rate_final;
            }
            // The actual update:
            temp_new_value = predicted_outcome + value_change;
            //               ^^^^^^^^^^^^^^^^^
            // ... NB this is stim_value[chosen_stimulus_index], but it saves
            //     an extra array lookup step.
            if (temp_new_value < 0.0 || temp_new_value > 1.0) {
                reject("Error: stimulus value out of range [0, 1]!");
            }
            stim_value[chosen_stimulus_index] = temp_new_value;
        }

    }

    // ========================================================================
    // Final fit to behaviour
    // ========================================================================

    sampleBernoulli_AV_lp(responded_stim1, p_choose_stim1);

    // ... actual choices predicted from our model-calculated probabilities
    // ... Stan will try to match the model to the behaviour via this.

}

generated quantities {

    // ========================================================================
    // Other stuff we're interested in measuring
    // ========================================================================
    // gmd = group mean difference; ct = contrast

    // ------------------------------------------------------------------------
    // Specific interactions/contrasts of interest
    // ------------------------------------------------------------------------

    // SHAM or DEPLETION two groups 
    
    
    real ct_reward_rate_sham_minus_depletion  =
    reward_rate_by_group[SHAM_GROUP] -
    reward_rate_by_group[DEPL_GROUP];
    
    
    real ct_punish_rate_sham_minus_depletion =
    punish_rate_by_group[SHAM_GROUP] -
    punish_rate_by_group[DEPL_GROUP];
    
    
    real ct_reinf_sensitivity_sham_minus_depletion =
    reinf_sensitivity_by_group[SHAM_GROUP] -
    reinf_sensitivity_by_group[DEPL_GROUP];
    
    real ct_side_stickiness_sham_minus_depletion =
    side_stickiness_by_group[SHAM_GROUP] -
    side_stickiness_by_group[DEPL_GROUP];
}
