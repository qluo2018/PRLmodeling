is_in <- function(ids, idlist){

	bool <- rep(0, length(ids))
	t <- 0
	for (i in ids) {
		t <- t + 1
		bool[t] <- (length(grep(i, idlist))>0)
	}
	return(bool)
}



simulatewithstats.cri <- function(para.mean, para.sd, simDataFile) { 

      #Testing out code for simulating data based on estimated parameters
      #Version 0.0.3 20.9.2019 BJA
      #This code is for the model with stickiness, inverse temperature and
      #separate learning rates for wins and losses
      #
      #The way it is written, you plug in the average parameter value and a
      #STANDARD DEVIATION for each of your parameters and for each of
      #your conditions (i.e., drug doses) separately. I then write this out
      #to csv files, but of course it would be better to just keep it all
      #in R.

      #Define how many rats to simulate (any number)
      groupSize <- 40

      reversals <- vector(mode="double", length=groupSize)
      wins <- vector(mode="double", length=groupSize)
      winStay <- vector(mode="double", length=groupSize)
      loseShift <- vector(mode="double", length=groupSize)
      losses <- vector(mode="double", length=groupSize)

      winStayRate<- vector(mode="double", length=groupSize)
      loseShiftRate <- vector(mode="double", length=groupSize)

      wins.acquisation <- vector(mode="double", length=groupSize)
      winStay.acquisation <- vector(mode="double", length=groupSize)
      loseShift.acquisation <- vector(mode="double", length=groupSize)
      losses.acquisation <- vector(mode="double", length=groupSize)

      winStayRate.acquisation <- vector(mode="double", length=groupSize)
      loseShiftRate.acquisation <- vector(mode="double", length=groupSize)

      wins.reversal1 <- vector(mode="double", length=groupSize)
      winStay.reversal1  <- vector(mode="double", length=groupSize)
      loseShift.reversal1  <- vector(mode="double", length=groupSize)
      losses.reversal1  <- vector(mode="double", length=groupSize)

      winStayRate.reversal1  <- vector(mode="double", length=groupSize)
      loseShiftRate.reversal1 <- vector(mode="double", length=groupSize)


      for (rat in 1:groupSize) {

            #Sets up vectors for P's and Q's
            Pleft <- vector(mode="double", length=200)
            Pright <- vector(mode="double", length=200)
            Qleft <- vector(mode="double", length=200)
            Qright <- vector(mode="double", length=200)
            choice <- vector(mode="double", length=200)
            outcome <- vector(mode="double", length=200)
            active <- vector(mode="double", length=200)
            wCrit <- vector(mode="double", length=201)


            #Criterion and probabilities
            critCorrInRow <- TRUE
            crit <- 8
            rewardRateCorr <- 0.8
            rewardRateIncorr <- 0.2

            #Random vectors
            set.seed(rat)
            r.outcome <- runif(200,0,1)
            r.choice <- runif(200,0,1)

            #Starting values for Q's, assuming no bias i.e. 50/50
            Qleft[1] <- 0.5
            Qright[1] <- 0.5
            wCrit[1] <- 0

            #starting side
            active[1] <- rbinom(1, 1, 0.5) + 1
            #active <- c(rep(2, 200)) #for testing purposes

            #Starting values to simulate data [truncated distributions; not ideal]
            #PLEASE NOTE THIS IS WHERE YOU PLUG IN THE AVERAGE DATA AND STANDARD DEVIATIONS
            #(for each condition separately)
            # =============================================================================
            # Data from the winning model
            # =============================================================================


            #Below are group mean values, from the Bayesian analysis.
            #To retrieve them, from within R:

            #.. code-block:: R

            #    source("bayesian_reversals_ql.R")
            #    run_models("2")
            #    s <- summarize_for_group_level_params(fit2)
            #    cols <- c("parameter", "mean", "sd")  # use the mean

            #    s[grep("^reward_rate_by_dose.*", parameter), ..cols]
            #    s[grep("^punish_rate_by_dose.*", parameter), ..cols]
            #    s[grep("^reinf_sensitivity_by_dose.*", parameter), ..cols]
            #    s[grep("^side_stickiness_by_dose.*", parameter), ..cols]
                
            #Note that group 1 = control, 2 = SUD, 3 = OCD.
            #Note that drug 1 = placebo, 2 = amisulpride, 3 = pramipexole.
            #So ``reward_rate_by_group_drug[1,1]`` means "control, placebo"; etc.


            #beta = 2.22   #0mg 2.22, 1mg 2.15, 10mg 2.52
            #kappa = 0.22   #0mg 0.22, 1mg 0.03, 10mg 0.29
            #alpha_g = 0.67 #0mg 0.67, 1mg 0.88 10mg 0.52 
            #alpha_l = 0.95 #0mg 0.95 1mg 0.95  10mg 0.90 

            repeat {
              beta <- rnorm(1, para.mean$beta, para.sd$beta)       
              if (beta >= 0){
               break
              }
            }
            
            repeat {
              kappa <- rnorm(1, para.mean$kappa, para.sd$kappa)      
              if (kappa >= 0){
                break
              }
            }
            
            repeat {
              alpha_g <- rnorm(1, para.mean$alpha_g, para.sd$alpha_g)      
              if (alpha_g >= 0 & alpha_g <= 1) {
                break
              }
            }
            
            repeat {
              alpha_l <- rnorm(1, para.mean$alpha_l, para.sd$alpha_l)      
              if (alpha_l >= 0 & alpha_l <= 1){
                break
              }
            }

            first.reversal = 0
            second.reversal = 0

            for (t in 1:200) {
              
              #Choice rule WITH STICKINESS
              #to translate Qleft and Qright into a choice probability Pleft
              #Corrected for beta
              if (t==1) {
                Pleft[t] <- exp(Qleft[t]*beta) / 
                              (exp(Qleft[t]*beta) + 
                                exp(Qright[t]*beta))
                Pright[t] <- 1 - Pleft[t]
              } else if (t>1) {
                Pleft[t] <- exp(Qleft[t]*beta+kappa*as.integer(choice[t-1]==1)) / 
                  (exp(Qleft[t]*beta+kappa*as.integer(choice[t-1]==1)) + 
                     exp(Qright[t]*beta+kappa*as.integer(choice[t-1]==2)))
                Pright[t] <- 1 - Pleft[t]
              }
              
              #simulate choice left (1) or right (2)
              if (Pleft[t] >= r.choice[t]) {
                choice[t] <- 1
              } else if (Pleft[t] < r.choice[t]) {
                choice[t] <- 2
              }
              
              #simulate outcome reward (1) or no reward (2)
              if (choice[t] == active[t]) {
                outcome[t] <- rbinom(1, 1, rewardRateCorr)
              } else if (choice[t] != active[t]) {
                outcome[t] <- rbinom(1, 1, rewardRateIncorr)
              }
              
              #update working criterion
              if (choice[t] == active[t]) {
                wCrit[t+1] <- wCrit[t] + 1
              } else {
                wCrit[t+1] = 0
              }
              
              #update active side at criterion
              if (wCrit[t] < crit) {
                active[t+1] <- active[t] 
              } else if (wCrit[t] == crit) {
                if (first.reversal == 0){
                  first.reversal <- t-1
                }else if (second.reversal == 0){
                  second.reversal <- t-1
                }
                if (active[t] == 1) {
                  active[t+1] <- 2
                  wCrit[t+1] <- 0
                } else if (active[t] == 2) {
                  active[t+1] <- 1
                  wCrit[t+1] <- 0
                }
              }
              
              #After choice and outcome, update Q for next trial
              if (choice[t] == 1) {
                Qright[t+1] <- Qright[t]
                if (outcome[t] == 1) {
                  Qleft[t+1] <- Qleft[t] + alpha_g * (outcome[t] - Qleft[t])
                } else if (outcome[t] == 0) {
                  Qleft[t+1] <- Qleft[t] + alpha_l * (outcome[t] - Qleft[t])  
                }
              } else if (choice[t] == 2) {
                Qleft[t+1] <- Qleft[t]
                if (outcome[t] == 1) {
                  Qright[t+1] <- Qright[t] + alpha_g * (outcome[t] - Qright[t])
                } else if (outcome[t] == 0) {
                  Qright[t+1] <- Qright[t] + alpha_l * (outcome[t] - Qright[t]) 
                }
              }
              
            } #end of trial loop

            #Win-Stay and Lose-Shift
            reversals[rat] <- sum(wCrit==8)
            wins[rat] <- sum(outcome[1:199])
            winStay[rat] <- sum(outcome[1:199]==1 & choice[1:199]==choice[2:200])
            loseShift[rat] <- sum(outcome[1:199]==0 & choice[1:199]!=choice[2:200])
            losses[rat] <- sum(outcome[1:199]==0 & choice[1:199]>0)
            winStayRate[rat] <- winStay[rat] / wins[rat]
            loseShiftRate[rat] <- loseShift[rat] / losses[rat]

            # acquisation 
            if (first.reversal == 0 ) {
                rat <- rat - 1
                next
            } 
            wins.acquisation[rat] <- sum(outcome[1:(first.reversal-1)])
            winStay.acquisation[rat] <- sum(outcome[1:(first.reversal-1)]==1 & choice[1:(first.reversal-1)]==choice[2:first.reversal])
            loseShift.acquisation[rat] <- sum(outcome[1:(first.reversal-1)]==0 & choice[1:(first.reversal-1)]!=choice[2:first.reversal])
            losses.acquisation[rat] <- sum(outcome[1:(first.reversal-1)]==0 & choice[1:(first.reversal-1)]>0)

            winStayRate.acquisation[rat] <- winStay.acquisation[rat] / wins.acquisation[rat]
            loseShiftRate.acquisation[rat] <- loseShift.acquisation[rat] / losses.acquisation[rat]

            # reversal 1
            if (second.reversal == 0 ) {
                second.reversal = 200
            } 
            wins.reversal1[rat] <- sum(outcome[(first.reversal+1):(second.reversal-1)])
            winStay.reversal1[rat] <- sum(outcome[(first.reversal+1):(second.reversal-1)]==1 & choice[(first.reversal+1):(second.reversal-1)]==choice[(first.reversal+2):second.reversal])
            loseShift.reversal1[rat] <- sum(outcome[(first.reversal+1):(second.reversal-1)]==0 & choice[(first.reversal+1):(second.reversal-1)]!=choice[(first.reversal+2):second.reversal])
            losses.reversal1[rat] <- sum(outcome[(first.reversal+1):(second.reversal-1)]==0 & choice[(first.reversal+1):(second.reversal-1)]>0)
            
            winStayRate.reversal1[rat] <- winStay.reversal1[rat] / wins.reversal1[rat]
            loseShiftRate.reversal1[rat] <- loseShift.reversal1[rat] / losses.reversal1[rat]

      } #end of rat loop

      result <- data.frame(reversals, winStay, wins, loseShift, losses, winStayRate, loseShiftRate,
                           winStay.acquisation, wins.acquisation, loseShift.acquisation, losses.acquisation, winStayRate.acquisation, loseShiftRate.acquisation,
                           winStay.reversal1, wins.reversal1, loseShift.reversal1, losses.reversal1, winStayRate.reversal1, loseShiftRate.reversal1)

      names(result) <- c("reversals", "winStay", "wins", "loseShift", "losses", "winStayRate", "loseShiftRate",
                         "winStay.ac", "wins.ac", "loseShift.ac", "losses.ac", "winStayRate.ac", "loseShiftRate.ac",
                         "winStay.r1", "wins.r1", "loseShift.r1", "losses.r1", "winStayRate.r1", "loseShiftRate.r1")
                    

      write.csv(result, file = simDataFile, row.names = F)

}



simulatewithstats <- function(para.mean, para.sd, simDataFile) { 

    #Define how many rats to simulate (any number)
    groupSize <- 40
    NTRIAL <- 80

    perseveration <- vector(mode="double", length=groupSize)
    wins <- vector(mode="double", length=groupSize)
    winStay <- vector(mode="double", length=groupSize)
    loseShift <- vector(mode="double", length=groupSize)
    losses <- vector(mode="double", length=groupSize)

    winStayRate<- vector(mode="double", length=groupSize)
    loseShiftRate <- vector(mode="double", length=groupSize)

    wins.acquisation <- vector(mode="double", length=groupSize)
    winStay.acquisation <- vector(mode="double", length=groupSize)
    loseShift.acquisation <- vector(mode="double", length=groupSize)
    losses.acquisation <- vector(mode="double", length=groupSize)

    winStayRate.acquisation <- vector(mode="double", length=groupSize)
    loseShiftRate.acquisation <- vector(mode="double", length=groupSize)

    wins.reversal1 <- vector(mode="double", length=groupSize)
    winStay.reversal1  <- vector(mode="double", length=groupSize)
    loseShift.reversal1  <- vector(mode="double", length=groupSize)
    losses.reversal1  <- vector(mode="double", length=groupSize)

    winStayRate.reversal1  <- vector(mode="double", length=groupSize)
    loseShiftRate.reversal1 <- vector(mode="double", length=groupSize)

    calculateperseveration <- function(choicesafterreversal){
      calculateperseveration <- 0
      for (i in choicesafterreversal) {
        if (i == 1){
          calculateperseveration <- calculateperseveration + 1
        } else {
          break
        }
      }
      return(calculateperseveration)
    }

    for (rat in 1:groupSize) {
      
      #Sets up vectors for P's and Q's
      Pstim1 <- vector(mode="double", length=NTRIAL)
      Pstim2 <- vector(mode="double", length=NTRIAL)
      Qstim1 <- vector(mode="double", length=NTRIAL)
      Qstim2 <- vector(mode="double", length=NTRIAL)
      choice <- vector(mode="double", length=NTRIAL)
      outcome <- vector(mode="double", length=NTRIAL)
      active <- as.vector(c(rep(1,40),rep(2,40)))  # has one reversal only at the 41st trial
      first.reversal <- 40
      second.reversal <- 80
      
      #Criterion and probabilities
      rewardRateCorr <- 0.8
      rewardRateIncorr <- 0.2
      
      #Random vectors
      set.seed(rat)
      r.outcome <- runif(NTRIAL,0,1)
      r.choice <- runif(NTRIAL,0,1)
      
      #Starting values for Q's, assuming no bias i.e. 50/50
      Qstim1[1] <- 0.5
      Qstim2[1] <- 0.5
      
      
      #Starting values to simulate data [truncated distributions; not ideal]
      #PLEASE NOTE THIS IS WHERE YOU PLUG IN THE AVERAGE DATA AND STANDARD DEVIATIONS
      #(for each condition separately)
      # =============================================================================
      # Data from the winning model
      # =============================================================================
      
      
      #Below are group mean values, from the Bayesian analysis.
      #To retrieve them, from within R:
      
      #.. code-block:: R
      
      #    source("bayesian_reversals_ql.R")
      #    run_models("2")
      #    s <- summarize_for_group_level_params(fit2)
      #    cols <- c("parameter", "mean", "sd")  # use the mean
      
      #    s[grep("^reward_rate_by_dose.*", parameter), ..cols]
      #    s[grep("^punish_rate_by_dose.*", parameter), ..cols]
      #    s[grep("^reinf_sensitivity_by_dose.*", parameter), ..cols]
      #    s[grep("^side_stickiness_by_dose.*", parameter), ..cols]
      
      #Note that group 1 = control, 2 = SUD, 3 = OCD.
      #Note that drug 1 = placebo, 2 = amisulpride, 3 = pramipexole.
      #So ``reward_rate_by_group_drug[1,1]`` means "control, placebo"; etc.
      
      
      
      #beta = 5.46    #sensitivity   placebo 5.46 (2.42); SSRI 5.48 (2.48)
      #kappa = 2.02   #stikiness     pla: 2.02 (0.11); SSRI: 1.49 (0.11)
      #alpha_g = 0.5  #reward learning rate 0.5 (0.27)
      #alpha_l = 0.5  #punishment learning rate 0.5 (0.27)
      
      repeat {
        beta <- rnorm(1, para.mean$beta, para.sd$beta)       
        if (beta >= 0){
         break
        }
      }
      
      repeat {
        kappa <- rnorm(1, para.mean$kappa, para.sd$kappa)      
        if (kappa >= 0){
          break
        }
      }
      
      repeat {
        alpha_g <- rnorm(1, para.mean$alpha_g, para.sd$alpha_g)      
        if (alpha_g >= 0 & alpha_g <= 1) {
          break
        }
      }
      
      repeat {
        alpha_l <- rnorm(1, para.mean$alpha_l, para.sd$alpha_l)      
        if (alpha_l >= 0 & alpha_l <= 1){
          break
        }
      }
      
      
      for (t in 1:NTRIAL) {
        
        #Choice rule WITH STICKINESS
        #to translate Qstim1 and Qstim2 into a choice probability Pstim1
        #Corrected for beta
        if (t==1) {
          Pstim1[t] <- exp(Qstim1[t]*beta) / 
            (exp(Qstim1[t]*beta) + 
               exp(Qstim2[t]*beta))
          Pstim2[t] <- 1 - Pstim1[t]
        } else if (t>1) {
          Pstim1[t] <- exp(Qstim1[t]*beta+kappa*as.integer(choice[t-1]==1)) / 
            (exp(Qstim1[t]*beta+kappa*as.integer(choice[t-1]==1)) + 
               exp(Qstim2[t]*beta+kappa*as.integer(choice[t-1]==2)))
          Pstim2[t] <- 1 - Pstim1[t]
        }
        
        #simulate choice left (1) or right (2)
        if (Pstim1[t] >= r.choice[t]) {
          choice[t] <- 1
        } else if (Pstim1[t] < r.choice[t]) {
          choice[t] <- 2
        }
        

        #IMPORTANT: the program defines the first choice made by a subject to be the correct option 
        if (t==1){
          active <- as.vector(c(rep(choice[1],40), rep(setdiff(c(1,2), choice[1]),40)))
        } 

        #simulate outcome reward (1) or no reward (2)
        if (choice[t] == active[t]) {
          outcome[t] <- rbinom(1, 1, rewardRateCorr)
        } else if (choice[t] != active[t]) {
          outcome[t] <- rbinom(1, 1, rewardRateIncorr)
        }
        
        #After choice and outcome, update Q for next trial
        if (choice[t] == 1) {
          Qstim2[t+1] <- Qstim2[t]
          if (outcome[t] == 1) {
            Qstim1[t+1] <- Qstim1[t] + alpha_g * (outcome[t] - Qstim1[t])
          } else if (outcome[t] == 0) {
            Qstim1[t+1] <- Qstim1[t] + alpha_l * (outcome[t] - Qstim1[t])  
          }
        } else if (choice[t] == 2) {
          Qstim1[t+1] <- Qstim1[t]
          if (outcome[t] == 1) {
            Qstim2[t+1] <- Qstim2[t] + alpha_g * (outcome[t] - Qstim2[t])
          } else if (outcome[t] == 0) {
            Qstim2[t+1] <- Qstim2[t] + alpha_l * (outcome[t] - Qstim2[t]) 
          }
        }
        
      } #end of trial loop
      
      perseveration[rat] <- calculateperseveration(choice[c((NTRIAL/2+1):NTRIAL)])  
      
      #Win-Stay and Lose-Shift
      wins[rat] <- sum(outcome[1:(NTRIAL-1)])
      winStay[rat] <- sum(outcome[1:(NTRIAL-1)]==1 & choice[1:(NTRIAL-1)]==choice[2:NTRIAL])
      loseShift[rat] <- sum(outcome[1:(NTRIAL-1)]==0 & choice[1:(NTRIAL-1)]!=choice[2:NTRIAL])
      losses[rat] <- sum(outcome[1:(NTRIAL-1)]==0 & choice[1:(NTRIAL-1)]>0)
      winStayRate[rat] <- winStay[rat] / wins[rat]
      loseShiftRate[rat] <- loseShift[rat] / losses[rat]
      
      # acquisation 
      wins.acquisation[rat] <- sum(outcome[1:(first.reversal-1)])
      winStay.acquisation[rat] <- sum(outcome[1:(first.reversal-1)]==1 & choice[1:(first.reversal-1)]==choice[2:first.reversal])
      loseShift.acquisation[rat] <- sum(outcome[1:(first.reversal-1)]==0 & choice[1:(first.reversal-1)]!=choice[2:first.reversal])
      losses.acquisation[rat] <- sum(outcome[1:(first.reversal-1)]==0 & choice[1:(first.reversal-1)]>0)
      
      winStayRate.acquisation[rat] <- winStay.acquisation[rat] / wins.acquisation[rat]
      loseShiftRate.acquisation[rat] <- loseShift.acquisation[rat] / losses.acquisation[rat]
      
      # reversal 1
      wins.reversal1[rat] <- sum(outcome[(first.reversal+1):(second.reversal-1)])
      winStay.reversal1[rat] <- sum(outcome[(first.reversal+1):(second.reversal-1)]==1 & choice[(first.reversal+1):(second.reversal-1)]==choice[(first.reversal+2):second.reversal])
      loseShift.reversal1[rat] <- sum(outcome[(first.reversal+1):(second.reversal-1)]==0 & choice[(first.reversal+1):(second.reversal-1)]!=choice[(first.reversal+2):second.reversal])
      losses.reversal1[rat] <- sum(outcome[(first.reversal+1):(second.reversal-1)]==0 & choice[(first.reversal+1):(second.reversal-1)]>0)
      
      winStayRate.reversal1[rat] <- winStay.reversal1[rat] / wins.reversal1[rat]
      loseShiftRate.reversal1[rat] <- loseShift.reversal1[rat] / losses.reversal1[rat]
      
    } #end of rat loop

    result <- data.frame(perseveration, winStay, wins, loseShift, losses, winStayRate, loseShiftRate,
                         winStay.acquisation, wins.acquisation, loseShift.acquisation, losses.acquisation, winStayRate.acquisation, loseShiftRate.acquisation,
                         winStay.reversal1, wins.reversal1, loseShift.reversal1, losses.reversal1, winStayRate.reversal1, loseShiftRate.reversal1)

    names(result) <- c("perseveration", "winStay", "wins", "loseShift", "losses", "winStayRate", "loseShiftRate",
                       "winStay.ac", "wins.ac", "loseShift.ac", "losses.ac", "winStayRate.ac", "loseShiftRate.ac",
                       "winStay.r1", "wins.r1", "loseShift.r1", "losses.r1", "winStayRate.r1", "loseShiftRate.r1")
                  

    write.csv(result, file = simDataFile, row.names = F)

}
