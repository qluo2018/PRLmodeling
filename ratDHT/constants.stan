    // ========================================================================
    // Constants: indices
    // ========================================================================
    int LEFT = 1;
    int RIGHT = 2;  // for side_chosen
    
    int SHAM_GROUP = 1;
    int DEPL_GROUP = 2;


    // ========================================================================
    // Values used to set priors
    // ========================================================================
    // Lots of things in the range 0-1 have a beta(1.1, 1.1) distribution:
    real PRIOR_BETA_SHAPE1 = 1.2;  // den Ouden 2013
    real PRIOR_BETA_SHAPE2 = 1.2;  // den Ouden 2013
    // plot(function(x) dbeta(x, shape1 = 1.1, shape2 = 1.1))  # beta(1.1, 1.1)
    // plot(function(x) dbeta(x, shape1 = 1.2, shape2 = 1.2))  # beta(1.2, 1.2); den Ouden 2013

    // When we have an SD that we believe should be in the range 0-1, it will
    // be given a prior of a half-normal(mean=0, sd=what's below)
    real PRIOR_HALF_NORMAL_SD_FOR_SD_IN_RANGE_0_1 = 0.05;

    real PRIOR_GAMMA_ALPHA_FOR_REINF_SENSITIVITY_MEAN = 4.82;  // Gershman (2016)
    real PRIOR_GAMMA_BETA_FOR_REINF_SENSITIVITY_MEAN = 0.88;  // Gershman (2016)
    // plot(function(x) dgamma(x, shape = 4.82, rate = 0.88), -0.1, 11)
    real PRIOR_HALF_NORMAL_SIGMA_FOR_REINF_SENSITIVITY_SD = 1;  // Gershman (2016) Cauchy -> Stan prior choice recommendations

    // Note that den Ouden (2013) use a normal distribution for softmax inverse
    // temperature, "Gaussian(0, 10)", though it's not clear if that's using a
    // variance or a standard deviation parameterization. Anyway, we want
    // some consistency here, and a negative softmax inverse temperature (beta)
    // is pretty stupid; beta=0 gives total indifference, and beta < 0 makes
    // the least-preferred thing win. So we'll stick with a gamma distribution
    // and the same priors as above. (Note that their final value for beta was
    // 4.69, with an interquartile range of 2.62-7.35, so that's perfectly
    // compatible with the gamma function shown above.)
