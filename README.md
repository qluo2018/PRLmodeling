# PRLmodeling
computational modeling of the probabilistic reversal learning task

This package is related to the following manuscript 
"Common roles for serotonin in rats and humans for computations underlying flexible decision-making" 
by Q Luo, JW Kanen, A Bari, N Skadanli, C Langley, GM Knudsen, J Alsio, BU Phillips, BJ Sahakian, RN Cardinal, and TW Robbins,
that has been submitted to Neuropsychopharmocology 2023.

This package included the RStan code for the computaional modeling of probabilistic reversal learning task in four experiments. 

1) Serotonin depletion in rats;            /ratDHT
2) Acute SSRI with low/high dose in rats;  /ratLowHighDose
3) Repeated and sub-chronic SSRI in rats;  /ratRepChro
4) Acute SSRI in humans.                   /humanSSRI

The models are coded in reversals_x.stan & data.stan & constants.stan
The individualized versions of the models are coded by reversals_xs.stan
The models are fitted by bayesian_reversals_xxx.R
The method for model simulations is provided in /support/toolkits.R
The comparisons between simulations and empirical data are condcted by readCSVcompare_xxx.R


R version 4.1.3 (2022-03-10) and a few libraries
rstan version 2.26.11 (Stan version 2.26.1)
Custermized tools and functions are provided in the folder support/


The code was adpated by Dr. Qiang Luo from the codes written by Dr. Rudolf Cardinal in 2017 for the following paper
Kanen JW, Ersche KD, Fineberg NA, Robbins TW, Cardinal RN. 
Computational modelling reveals contrasting effects on reinforcement 
learning and cognitive flexibility in stimulant use disorder and obsessive-compulsive disorder: 
remediating effects of dopaminergic D2/3 receptor agents. Psychopharmacology (Berl). 
2019 Aug;236(8):2337-2358. 
doi: 10.1007/s00213-019-05325-w. Epub 2019 Jul 20. PMID: 31324936; PMCID: PMC6820481.
The original code for the above manuscript has also been made available as the online supplemenatry files. 


Last edited by Qiang Luo, 2023.6.16


