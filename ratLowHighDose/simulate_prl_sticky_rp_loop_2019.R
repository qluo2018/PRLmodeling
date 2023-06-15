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
Qleft[1] <- 0.0
Qright[1] <- 0.0
wCrit[1] <- 0

#starting side
active[1] <- rbinom(1, 1, 0.5) + 1
#active <- c(rep(2, 200)) #for testing purposes

#Starting values to simulate data [truncated distributions; not ideal]
#PLEASE NOTE THIS IS WHERE YOU PLUG IN THE AVERAGE DATA AND STANDARD DEVIATIONS
#(for each condition separately)
repeat {
  beta <- rnorm(1, 2, 0.2)
  if (beta >= 0){
    break
  }
}

repeat {
  kappa <- rnorm(1, 0.8, 0.1)
  if (kappa >= 0){
    break
  }
}

repeat {
  alpha_g <- rnorm(1, 0.7, 0.1)
  if (alpha_g >= 0 & alpha_g <= 1) {
    break
  }
}

repeat {
  alpha_l <- rnorm(1, 0.1, 0.1)
  if (alpha_l >= 0 & alpha_l <= 1){
    break
  }
}

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
wins[rat] <- sum(outcome[2:200])
winStay[rat] <- sum(outcome[1:199]==1 & choice[1:199]==choice[2:200])
loseShift[rat] <- sum(outcome[1:199]==0 & choice[1:199]!=choice[2:200])
losses[rat] <- sum(outcome[2:200]==0 & choice[2:200]>0)

} #end of rat loop

result <- data.frame(reversals, winStay, wins, loseShift, losses)

names(result) <- c("reversals", "winStay", "wins", "loseShift", "losses")

write.csv(result, file = "test_ql_0.1.csv")

#So this is where I open the data in Excel and graph it in GraphPad Prism
#This will be updated in the future with ggplot2 just didn't have time ...
plot.new()
plot(1:200, choice-1)
#points(1:200, Pright, col="red")
points(1:200, Pleft, col="magenta")
title(main = c("alpha_g=", alpha_g, alpha_l, beta, sum(wCrit==8)))

