    // Constants:
    int<lower=1> N_SUBJECTS;  // number of subjects
    int<lower=1> N_DOSES;  // number of drug doses [WITHIN SUBJECTS]
    int<lower=1> N_TRIALS;  // TOTAL number of trials
    int<lower=1> N_STIMULI;  // number of stimuli (= sides!)

    // Arrays:
    int<lower=1, upper=N_SUBJECTS> subject[N_TRIALS];  // which subject?
    int<lower=1, upper=N_DOSES> dose[N_TRIALS];  // which drug dose is the subject on?
    int<lower=1, upper=2> side_chosen[N_TRIALS];  // did the subject respond left (1) or right (2)?
    int<lower=0, upper=1> responded_right[N_TRIALS];  // did the subject choose right (1) or not (0)?
    int<lower=0, upper=1> reinforced[N_TRIALS];  // outcome, coded 0 (nonreward), 1 reward
