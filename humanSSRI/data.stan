    // Constants:
    int<lower=1> N_SUBJECTS;    // number of subjects
    int<lower=1> N_TRIALS;      // TOTAL number of trials
    int<lower=1> N_STIMULI;     // number of stimuli (= sides!)
    int<lower=1> N_GROUPS;       // number of groups [BETWEEN SUBJECTS]


    // Arrays:
    int<lower=1, upper=N_GROUPS> group_membership[N_SUBJECTS];  // which group is each subject in?
    int<lower=1, upper=N_SUBJECTS> subject[N_TRIALS];  // which subject?
    int<lower=1, upper=2> stim_chosen[N_TRIALS];  // did the subject respond left (1) or right (2)?
    int<lower=1, upper=1> sess[N_TRIALS];         // which session is the subject on?
    int<lower=0, upper=1> responded_stim1[N_TRIALS];  // did the subject choose right (1) or not (0)?
    int<lower=0, upper=1> reinforced[N_TRIALS];  // outcome, coded 0 (nonreward), 1 reward
