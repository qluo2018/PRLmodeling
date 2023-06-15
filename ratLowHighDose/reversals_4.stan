/*

Code for Ben Phillips's reversal task, by RNC, 17 Jul 2018.

- Model 4: den Ouden (2013) EWA (experience-weighted attraction) model
    ... itself after Camerer & Ho 1999

The model is as follows.

PARAMETERS

    ϕ or φ, phi, range 0 < ϕ < 1    -- decay factor for previous payoffs (equivalent to learning rate)
    ρ, rho, range 0 < ϕ < 1         -- experience decay factor
    β, beta, range –∞ ≤ β ≤ +∞      -- softmax inverse temperature

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

    real<lower=0, upper=1> phi_by_dose[N_DOSES];
    real<lower=0, upper=1> rho_by_dose[N_DOSES];
    real<lower=0> beta_by_dose[N_DOSES];

    // ========================================================================
    // Random effects
    // ========================================================================

    real<lower=0> phi_intersubject_sd;
    real<lower=0> rho_intersubject_sd;
    real<lower=0> beta_intersubject_sd;

    vector[N_SUBJECTS] phi_subject_effect_raw_unit_normal;
    vector[N_SUBJECTS] rho_subject_effect_raw_unit_normal;
    vector[N_SUBJECTS] beta_subject_effect_raw_unit_normal;
}

transformed parameters {

    vector[N_SUBJECTS] phi_subject_effect;
    vector[N_SUBJECTS] rho_subject_effect;
    vector[N_SUBJECTS] beta_subject_effect;

    phi_subject_effect = getReparameterizedNormal_VRR_lp(
        phi_subject_effect_raw_unit_normal,
        0, phi_intersubject_sd);
    rho_subject_effect = getReparameterizedNormal_VRR_lp(
        rho_subject_effect_raw_unit_normal,
        0, rho_intersubject_sd);
    beta_subject_effect = getReparameterizedNormal_VRR_lp(
        beta_subject_effect_raw_unit_normal,
        0, beta_intersubject_sd);
}

model {

    // Calculated probability of choosing the right-hand-side (RHS) stimulus:
    vector[N_TRIALS] p_choose_rhs;

    // ========================================================================
    // Specify priors
    // ========================================================================

    sampleBeta_ARR_lp(phi_by_dose, PRIOR_BETA_SHAPE1, PRIOR_BETA_SHAPE2);
    sampleBeta_ARR_lp(rho_by_dose, PRIOR_BETA_SHAPE1, PRIOR_BETA_SHAPE2);
    sampleGamma_ARR_lp(beta_by_dose, PRIOR_GAMMA_ALPHA_FOR_REINF_SENSITIVITY_MEAN, PRIOR_GAMMA_BETA_FOR_REINF_SENSITIVITY_MEAN);  // positive only

    sampleNormalLowerBound_RRR_lp(phi_intersubject_sd, 0, PRIOR_HALF_NORMAL_SD_FOR_SD_IN_RANGE_0_1, 0);  // lower bound of 0
    sampleNormalLowerBound_RRR_lp(rho_intersubject_sd, 0, PRIOR_HALF_NORMAL_SD_FOR_SD_IN_RANGE_0_1, 0);  // lower bound of 0
    sampleNormalLowerBound_RRR_lp(beta_intersubject_sd, 0, PRIOR_HALF_NORMAL_SIGMA_FOR_REINF_SENSITIVITY_SD, 0);  // lower bound of 0

    // ========================================================================
    // Reinforcement learning model: calculates p_choose_rhs
    // ========================================================================
    {
        // Start a new block because we will define local-only variables.
        // NO RANGE CONSTRAINTS allowed here.

        // Keeping track of the current subject/drug
        int s = 0; // current subject, starting with an invalid value
        int d; // current drug

        // Value/experience tracking
        vector[N_STIMULI] n_experience_weight_now;
        vector[N_STIMULI] n_experience_weight_prev;
        vector[N_STIMULI] v_stim_value;

        // Temporary variables: RL calculations
        vector[N_STIMULI] softmax_inputs;
        int chosen_stimulus_index;  // takes values 1 ... N_STIMULI

        // Temporary variables: final parameter values
        // NO RANGE CONSTRAINTS PERMITTED HERE.
        real phi;
        real rho;
        real beta;

        // Iterate through trials
        for (t in 1:N_TRIALS) {
            // Starting a new subject? Read subject parameters and reset discrimination.
            if (s != subject[t]) {
                s = subject[t];
                d = -1; // invalid value; will trigger the "new drug" code below
            }

            // Starting a new drug? Reset starting values.
            if (d != dose[t]) {
                d = dose[t];

                // "First trial" code:

                for (stim in 1:N_STIMULI) {
                    n_experience_weight_now[stim] = 0;
                    n_experience_weight_prev[stim] = 0;  // MUST BE THE SAME as n_experience_weight_now
                    v_stim_value[stim] = 0;  // all stimuli start with neutral value
                }

                phi = phi_by_dose[d] + phi_subject_effect[s];
                rho = rho_by_dose[d] + rho_subject_effect[s];
                beta = beta_by_dose[d] + beta_subject_effect[s];

                phi = bound(phi, 0, 1);
                rho = bound(rho, 0, 1);
                beta = boundLower(beta, 0);

            }

            // ----------------------------------------------------------------
            // Calculate p
            // ----------------------------------------------------------------

            p_choose_rhs[t] = softmaxNthInvTemp(v_stim_value, beta, RIGHT);

            // ----------------------------------------------------------------
            // Update
            // ----------------------------------------------------------------
            // "Only the chosen stimulus is updated" (den Ouden 2013, p1098).

            chosen_stimulus_index = side_chosen[t];

            // n[c,t] = n[c,t-1] × ρ + 1           [den Ouden, Equation 1]
            // ... and Camerer & Ho (1999), equation 2.1
            n_experience_weight_now[chosen_stimulus_index] = (
                n_experience_weight_prev[chosen_stimulus_index] * rho + 1
            );
            // For the other stimuli, n_experience_weight_now remains where it
            // was for the previous trial, i.e. n_experience_weight_prev (see
            // below). As long as n_experience_weight_now and
            // n_experience_weight_prev start at the same value (see above),
            // this therefore guarantees "no update for stimuli not chosen".

            // v[c,t] = (v[c,t-1] × φ × n[c,t-1] + λ[t-1]) / n[c,t]    [Equation 2]
            // ... compare Camerer & Ho (1999), equation 2.2
            v_stim_value[chosen_stimulus_index] = (  // new value
                (
                    v_stim_value[chosen_stimulus_index] *  // old value
                        phi *  // decay factor for previous payoffs
                        n_experience_weight_prev[chosen_stimulus_index] +
                    reinforced[t]  // 0 or 1
                ) /
                n_experience_weight_now[chosen_stimulus_index]
            );

            // For next time:
            n_experience_weight_prev = n_experience_weight_now;
            // ... for unchosen stimuli, this means "no change".
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


    real ct_phi_100_minus_25 =
        phi_by_dose[DOSE_100] -
        phi_by_dose[DOSE_25];
    real ct_phi_25_minus_0 =
        phi_by_dose[DOSE_25] -
        phi_by_dose[DOSE_0];



    real ct_rho_100_minus_25 =
        rho_by_dose[DOSE_100] -
        rho_by_dose[DOSE_25];
    real ct_rho_25_minus_0 =
        rho_by_dose[DOSE_25] -
        rho_by_dose[DOSE_0];



    real ct_beta_100_minus_25 =
        beta_by_dose[DOSE_100] -
        beta_by_dose[DOSE_25];
    real ct_beta_25_minus_0 =
        beta_by_dose[DOSE_25] -
        beta_by_dose[DOSE_0];


}
