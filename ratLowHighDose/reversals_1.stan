/*

Code for Ben Phillips's reversal task, by RNC, 17 Jul 2018.

Model 1: reward rate, punishment rate, reinf sensitivity [= softmax inv. temp]

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

    real<lower=0, upper=1> reward_rate_by_dose[N_DOSES];
    real<lower=0, upper=1> punish_rate_by_dose[N_DOSES];
    real<lower=0> reinf_sensitivity_by_dose[N_DOSES];

    // ========================================================================
    // Random effects
    // ========================================================================

    real<lower=0> reward_rate_intersubject_sd;
    real<lower=0> punish_rate_intersubject_sd;
    real<lower=0> reinf_sensitivity_intersubject_sd;

    vector[N_SUBJECTS] reward_rate_subject_effect_raw_unit_normal;
    vector[N_SUBJECTS] punish_rate_subject_effect_raw_unit_normal;
    vector[N_SUBJECTS] reinf_sensitivity_subject_effect_raw_unit_normal;
}

transformed parameters {

    vector[N_SUBJECTS] reward_rate_subject_effect;
    vector[N_SUBJECTS] punish_rate_subject_effect;
    vector[N_SUBJECTS] reinf_sensitivity_subject_effect;

    reward_rate_subject_effect = getReparameterizedNormal_VRR_lp(
        reward_rate_subject_effect_raw_unit_normal,
        0, reward_rate_intersubject_sd);
    punish_rate_subject_effect = getReparameterizedNormal_VRR_lp(
        punish_rate_subject_effect_raw_unit_normal,
        0, punish_rate_intersubject_sd);
    reinf_sensitivity_subject_effect = getReparameterizedNormal_VRR_lp(
        reinf_sensitivity_subject_effect_raw_unit_normal,
        0, reinf_sensitivity_intersubject_sd);
}

model {

    // Calculated probability of choosing the side labelled 1:
    vector[N_TRIALS] p_choose_rhs;

    // ========================================================================
    // Specify priors
    // ========================================================================

    sampleBeta_ARR_lp(reward_rate_by_dose, PRIOR_BETA_SHAPE1, PRIOR_BETA_SHAPE2);
    sampleBeta_ARR_lp(punish_rate_by_dose, PRIOR_BETA_SHAPE1, PRIOR_BETA_SHAPE2);
    sampleGamma_ARR_lp(reinf_sensitivity_by_dose, PRIOR_GAMMA_ALPHA_FOR_REINF_SENSITIVITY_MEAN, PRIOR_GAMMA_BETA_FOR_REINF_SENSITIVITY_MEAN);  // positive only

    sampleNormalLowerBound_RRR_lp(reward_rate_intersubject_sd, 0, PRIOR_HALF_NORMAL_SD_FOR_SD_IN_RANGE_0_1, 0);  // lower bound of 0
    sampleNormalLowerBound_RRR_lp(punish_rate_intersubject_sd, 0, PRIOR_HALF_NORMAL_SD_FOR_SD_IN_RANGE_0_1, 0);  // lower bound of 0
    sampleNormalLowerBound_RRR_lp(reinf_sensitivity_intersubject_sd, 0, PRIOR_HALF_NORMAL_SIGMA_FOR_REINF_SENSITIVITY_SD, 0);  // lower bound of 0

    // ========================================================================
    // Reinforcement learning model: calculates p_choose_rhs
    // ========================================================================
    {
        // Start a new block because we will define local-only variables.
        // NO RANGE CONSTRAINTS allowed here.

        // Keeping track of the current subject/drug
        int s = 0; // current subject, starting with an invalid value
        int d; // current dose

        // Value tracking
        vector[N_STIMULI] stim_value;  // value of each stimulus; range 0 to 1

        // Temporary variables: RL calculations
        vector[N_STIMULI] softmax_inputs;
        int first_trial_of_dose;
        real predicted_outcome;
        real actual_outcome;
        real prediction_error;
        real value_change;

        // Temporary variables: final parameter values
        // NO RANGE CONSTRAINTS PERMITTED HERE.
        real reward_rate_final;
        real punish_rate_final;
        real reinf_sensitivity_final;
        int chosen_stimulus_index;
        real temp_new_value;

        // Iterate through trials
        for (t in 1:N_TRIALS) {
            // Starting a new subject? Read subject parameters and reset discrimination.
            if (s != subject[t]) {
                s = subject[t];
                d = -1; // invalid value; will trigger the "new dose" code below
            }

            // Starting a new drug? Reset starting values.
            if (d != dose[t]) {
                d = dose[t];
                first_trial_of_dose = 1;

                for (stim in 1:N_STIMULI) {  // better way to assign to a vector??
                    stim_value[stim] = 0.5;  // all stimuli start with neutral value
                }

                reward_rate_final = reward_rate_by_dose[d] + reward_rate_subject_effect[s];
                punish_rate_final = punish_rate_by_dose[d] + punish_rate_subject_effect[s];
                reinf_sensitivity_final = reinf_sensitivity_by_dose[d] + reinf_sensitivity_subject_effect[s];

                reward_rate_final = bound(reward_rate_final, 0, 1);
                punish_rate_final = bound(punish_rate_final, 0, 1);
                reinf_sensitivity_final = boundLower(reinf_sensitivity_final, 0);

            } else {
                first_trial_of_dose = 0;
            }

            // ----------------------------------------------------------------
            // Calculate p
            // ----------------------------------------------------------------

            softmax_inputs = (
                reinf_sensitivity_final * stim_value
            );

            p_choose_rhs[t] = softmaxNth(softmax_inputs, RIGHT);

            // ----------------------------------------------------------------
            // Update
            // ----------------------------------------------------------------

            // We update the value of the thing we chose based on the outcome
            // we received.

            chosen_stimulus_index = side_chosen[t];

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

    sampleBernoulli_AV_lp(responded_right, p_choose_rhs);

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

    // Name doses as micrograms per kg: 0, 1, 10.

    real ct_reward_rate_100_minus_25 =
        reward_rate_by_dose[DOSE_100] -
        reward_rate_by_dose[DOSE_25];
    real ct_reward_rate_25_minus_0 =
        reward_rate_by_dose[DOSE_25] -
        reward_rate_by_dose[DOSE_0];

    real ct_punish_rate_100_minus_25 =
        punish_rate_by_dose[DOSE_100] -
        punish_rate_by_dose[DOSE_25];
    real ct_punish_rate_25_minus_0 =
        punish_rate_by_dose[DOSE_25] -
        punish_rate_by_dose[DOSE_0];

    real ct_reinf_sensitivity_100_minus_25 =
        reinf_sensitivity_by_dose[DOSE_100] -
        reinf_sensitivity_by_dose[DOSE_25];
    real ct_reinf_sensitivity_25_minus_0 =
        reinf_sensitivity_by_dose[DOSE_25] -
        reinf_sensitivity_by_dose[DOSE_0];
}
