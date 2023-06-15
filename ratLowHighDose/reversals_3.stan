/*

Code for Ben Phillips's reversal task, by RNC, 17 Jul 2018.

Model 3: combined reward rate, reinf_sens, side stickiness

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

    real<lower=0, upper=1> reinforcement_rate_by_dose[N_DOSES];
    real<lower=0> reinf_sensitivity_by_dose[N_DOSES];
    real side_stickiness_by_dose[N_DOSES];

    // ========================================================================
    // Random effects
    // ========================================================================

    real<lower=0> reinforcement_rate_intersubject_sd;
    real<lower=0> reinf_sensitivity_intersubject_sd;
    real<lower=0> side_stickiness_intersubject_sd;

    vector[N_SUBJECTS] reinforcement_rate_subject_effect_raw_unit_normal;
    vector[N_SUBJECTS] reinf_sensitivity_subject_effect_raw_unit_normal;
    vector[N_SUBJECTS] side_stickiness_subject_effect_raw_unit_normal;
}

transformed parameters {

    vector[N_SUBJECTS] reinforcement_rate_subject_effect;
    vector[N_SUBJECTS] reinf_sensitivity_subject_effect;
    vector[N_SUBJECTS] side_stickiness_subject_effect;

    reinforcement_rate_subject_effect = getReparameterizedNormal_VRR_lp(
        reinforcement_rate_subject_effect_raw_unit_normal,
        0, reinforcement_rate_intersubject_sd);
    reinf_sensitivity_subject_effect = getReparameterizedNormal_VRR_lp(
        reinf_sensitivity_subject_effect_raw_unit_normal,
        0, reinf_sensitivity_intersubject_sd);
    side_stickiness_subject_effect = getReparameterizedNormal_VRR_lp(
        side_stickiness_subject_effect_raw_unit_normal,
        0, side_stickiness_intersubject_sd);
}

model {

    // Calculated probability of choosing the side labelled 1:
    vector[N_TRIALS] p_choose_rhs;

    // ========================================================================
    // Specify priors
    // ========================================================================

    sampleBeta_ARR_lp(reinforcement_rate_by_dose, PRIOR_BETA_SHAPE1, PRIOR_BETA_SHAPE2);
    sampleGamma_ARR_lp(reinf_sensitivity_by_dose, PRIOR_GAMMA_ALPHA_FOR_REINF_SENSITIVITY_MEAN, PRIOR_GAMMA_BETA_FOR_REINF_SENSITIVITY_MEAN);  // positive only
    sampleNormal_ARR_lp(side_stickiness_by_dose, 0, 1);  // can be negative; see Christakou (2013)

    sampleNormalLowerBound_RRR_lp(reinforcement_rate_intersubject_sd, 0, PRIOR_HALF_NORMAL_SD_FOR_SD_IN_RANGE_0_1, 0);  // lower bound of 0
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
        int d; // current dose

        // Value tracking
        vector[N_STIMULI] stim_value;  // value of each stimulus; range 0 to 1
        vector[N_STIMULI] side_stickiness_trace;  // integer contents, but needs to be real for vector multiplication

        // Temporary variables: RL calculations
        vector[N_STIMULI] softmax_inputs;
        int first_trial_of_dose;
        real predicted_outcome;
        real actual_outcome;
        real prediction_error;
        real value_change;

        // Temporary variables: final parameter values
        // NO RANGE CONSTRAINTS PERMITTED HERE.
        real reinforcement_rate_final;
        real reinf_sensitivity_final;
        real side_stickiness_final;

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

                reinforcement_rate_final = reinforcement_rate_by_dose[d] + reinforcement_rate_subject_effect[s];
                reinf_sensitivity_final = reinf_sensitivity_by_dose[d] + reinf_sensitivity_subject_effect[s];
                side_stickiness_final = side_stickiness_by_dose[d] + side_stickiness_subject_effect[s];

                reinforcement_rate_final = bound(reinforcement_rate_final, 0, 1);
                reinf_sensitivity_final = boundLower(reinf_sensitivity_final, 0);
                // side_stickiness_final may be negative

            } else {
                first_trial_of_dose = 0;
            }

            // Side stickiness:
            if (!first_trial_of_dose) {
                // Assign 1 to the side the subject responded to on the last
                // trial, and 0 to the other side.
                side_stickiness_trace[RIGHT] = responded_right[t - 1];
                side_stickiness_trace[LEFT] = 1 - responded_right[t - 1];
            } else {
                // no better way of assigning constant to vector?
                side_stickiness_trace[RIGHT] = 0;
                side_stickiness_trace[LEFT] = 0;
            }

            // ----------------------------------------------------------------
            // Calculate p
            // ----------------------------------------------------------------

            softmax_inputs = (
                reinf_sensitivity_final * stim_value +
                side_stickiness_final * side_stickiness_trace
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

            // Unified reinforcement rate:
            value_change = prediction_error * reinforcement_rate_final;

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


    real ct_reinforcement_rate_100_minus_25 =
        reinforcement_rate_by_dose[DOSE_100] -
        reinforcement_rate_by_dose[DOSE_25];
    real ct_reinforcement_rate_25_minus_0 =
        reinforcement_rate_by_dose[DOSE_25] -
        reinforcement_rate_by_dose[DOSE_0];


    real ct_reinf_sensitivity_100_minus_25 =
        reinf_sensitivity_by_dose[DOSE_100] -
        reinf_sensitivity_by_dose[DOSE_25];
    real ct_reinf_sensitivity_25_minus_0 =
        reinf_sensitivity_by_dose[DOSE_25] -
        reinf_sensitivity_by_dose[DOSE_0];



    real ct_side_stickiness_100_minus_25 =
        side_stickiness_by_dose[DOSE_100] -
        side_stickiness_by_dose[DOSE_25];
    real ct_side_stickiness_25_minus_0 =
        side_stickiness_by_dose[DOSE_25] -
        side_stickiness_by_dose[DOSE_0];

}
