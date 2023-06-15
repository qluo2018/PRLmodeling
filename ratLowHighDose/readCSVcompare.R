# install.packages("ggpubr")
# install.packages("reshape2")
library(ggpubr)
library(reshape2)
library(gdata)
library(ggplot2)

source('bayesian_reversals_ql.R')
x <- stanfunc$annotated_parameters(fit2, par_exclude_regex = c("p_choose_stim1", "subject_effect"))

source('simulation_ql.R')

# 0mg
x.mean <- as.data.frame(t(x[c(1,4,7,10), mean])) 
colnames(x.mean) <- c('alpha_g', 'alpha_l', 'beta', 'kappa')
x.sd <- as.data.frame(t(x[c(1,4,7,10), sd]), nrow=1, ncol=4) 
colnames(x.sd) <- c('alpha_g', 'alpha_l', 'beta', 'kappa')
x.file<- 'test_ql_0dosageV3.csv'
simulatewithstats.cri(x.mean, x.sd, x.file)

# 1mg
x.mean <- as.data.frame(t(x[c(2,5,8,11), mean])) 
colnames(x.mean) <- c('alpha_g', 'alpha_l', 'beta', 'kappa')
x.sd <- as.data.frame(t(x[c(2,5,8,11), sd]), nrow=1, ncol=4) 
colnames(x.sd) <- c('alpha_g', 'alpha_l', 'beta', 'kappa')
x.file<- 'test_ql_1dosageV3.csv'
simulatewithstats.cri(x.mean, x.sd, x.file)


# 10mg
x.mean <- as.data.frame(t(x[c(3,6,9,12), mean]), nrow=1, ncol=4) 
colnames(x.mean) <- c('alpha_g', 'alpha_l', 'beta', 'kappa')
x.sd <- as.data.frame(t(x[c(3,6,9,12), sd]), nrow=1, ncol=4) 
colnames(x.sd) <- c('alpha_g', 'alpha_l', 'beta', 'kappa')
x.file<- 'test_ql_10dosageV3.csv'
simulatewithstats(x.mean, x.sd, x.file)




data0 <- read.csv("test_ql_0dosageV3.csv", header = T)
data1 <- read.csv("test_ql_1dosageV3.csv", header = T)
data10 <- read.csv("test_ql_10dosageV3.csv", header = T)

#
data0 <- cbind(data0,rep(0,40)) 
colnames(data0)[20] <- 'dose'
data1 <- cbind(data1,rep(1,40)) 
colnames(data1)[20] <- 'dose'
data10 <- cbind(data10,rep(10,40)) 
colnames(data10)[20] <- 'dose'
data <- rbind(data0, data1, data10)

# Global test
#compare_means(reversals ~ dose,  data = data) #method = "anova"

# Default method = "kruskal.test" for multiple groups
#ggboxplot(data, x = "dose", y = "reversals",
#          color = "dose", palette = "jco")+
#  stat_compare_means()

# Change method to anova
#ggboxplot(data, x = "dose", y = "reversals",
#          color = "dose", palette = "jco")+
#  stat_compare_means(method = "anova")


# Perorm pairwise comparisons
compare_means(reversals ~ dose,  data = data)

# Visualize: Specify the comparisons you want
my_comparisons <- list( c("0", "1"), c("1", "10"), c("0", "10") )
#ggboxplot(data, x = "dose", y = "reversals",
#          color = "dose", palette = "jco")+ 
#  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
#  stat_compare_means(label.y = 20)     # Add global p-value


p1 <- ggbarplot(data, x = "dose", y = "reversals",
          color = "dose", palette = "jco", add = c("mean_se", "jitter"), legend = "", legend.title = "")+ 
  stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 20) +   # Add global p-value
  theme(legend.position = "none") + labs(title="Computational results") 
    

p2 <- ggbarplot(data, x = "dose", y = "loseShiftRate.ac",
                color = "dose", palette = "jco", add = c("mean_se", "jitter"), legend = "", legend.title = "")+ 
  stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1.5) +   # Add global p-value
  theme(legend.position = "none") + labs(title="   ") 



p3 <- ggbarplot(data, x = "dose", y = "loseShiftRate.r1",
                color = "dose", palette = "jco", add = c("mean_se", "jitter"), legend = "", legend.title = "")+ 
  stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1.5) +   # Add global p-value
  theme(legend.position = "none") + labs(title="   ") 




# reshape your data into long format
datanow <- data[,c("dose","winStayRate.ac","loseShiftRate.ac")]
datalong <- melt(datanow, id=c("dose"))
#ggbarplot(datalong, x = "dose", y = "value", add = "mean_se",
#          color = "variable", palette = "jco", 
#          position = position_dodge(0.8)) +
#  stat_compare_means(aes(group = variable), label = "p.signif", label.y = 1)



# Box plot facetted by "variable"
p2 <- ggbarplot(datalong, x = "dose", y = "value",
               color = "dose", palette = "jco",
               add = c("mean_se", "jitter"),
               facet.by = "variable", short.panel.labs = FALSE) 
# Use only p.format as label. Remove method name.
p2 <- p2 + stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label =  "p.format", label.y = 1.5) +  # Add global p-value
  theme(legend.position = "none") + labs(title="   ") 



datanow <- data[,c("dose","winStayRate.r1","loseShiftRate.r1")]
datalong <- melt(datanow, id=c("dose"))
#ggbarplot(datalong, x = "dose", y = "value", add = "mean_se",
#          color = "variable", palette = "jco", 
#          position = position_dodge(0.8)) +
#  stat_compare_means(aes(group = variable), label = "p.signif", label.y = 1)

# Box plot facetted by "dose"
p3 <- ggbarplot(datalong, x = "dose", y = "value",
               color = "dose", palette = "jco",
               add = c("mean_se", "jitter"),
               facet.by = "variable", short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
p3 <- p3 + stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label =  "p.format", label.y = 1.5) + # Add global p-value
  theme(legend.position = "none") + labs(title="   ")   

#p123 <- ggarrange(p1, p2, p3, ncol = 3) + theme(text=element_text(size=16,  family="Arial"))
#ggsave("simbehComp.pdf", p123, width=17.5, height=7.5)


# Perorm pairwise comparisons
simcomp <- array(rep(0,19*2*3), dim = c(19,2,3), dimnames = list(c(colnames(data)[c(1:19)]),
             c('wt', 'p.value'), c('0 vs 1','1 vs 10','0 vs 10')))

for (i in colnames(data)[c(1:19)]) {
    for (j in c(1:length(my_comparisons))){
         y <- wilcox.test(data[which(data$dose==my_comparisons[[j]][1]), i], 
          data[which(data$dose==my_comparisons[[j]][2]), i])
         simcomp[i,"wt",j] <- y$statistic
         simcomp[i,"p.value",j] <- y$p.value
   }
}

write.csv(simcomp, file = 'simbehresult.csv')



# test the association between the individual parameters and the behavioural indeces

data.beh <- read.csv("cleaned_data_bari.csv")
# reversals, winStayRate, loseShiftRate
data.subject <- data.frame()
NDOSE <- length(unique(data.beh$Dose))
NRATS <- length(unique(data.beh$ID))

reversals <- as.vector(rep(0,NDOSE*NRATS))
ID <- as.vector(rep(unique(data.beh$ID),NDOSE))
Dose  <-  as.matrix(rep(unique(data.beh$Dose)[1],NRATS), 1, NRATS)
for (dose in c(2:length(unique(data.beh$Dose)))){
  Dose <- rbind(Dose, as.matrix(rep(unique(data.beh$Dose)[dose],NRATS),1,NRATS))
}
Dose <- as.vector(Dose)
  
winStayRate <- as.vector(rep(0,NDOSE*NRATS))
loseShiftRate <- as.vector(rep(0,NDOSE*NRATS))
winStay <- as.vector(rep(0,NDOSE*NRATS))
loseShift  <- as.vector(rep(0,NDOSE*NRATS))
wins <- as.vector(rep(0,NDOSE*NRATS))
losses <- as.vector(rep(0,NDOSE*NRATS))

winStayRate.acquisation <- as.vector(rep(0,NDOSE*NRATS))
loseShiftRate.acquisation <- as.vector(rep(0,NDOSE*NRATS))
winStay.acquisation<- as.vector(rep(0,NDOSE*NRATS))
loseShift.acquisation  <- as.vector(rep(0,NDOSE*NRATS))
wins.acquisation<- as.vector(rep(0,NDOSE*NRATS))
losses.acquisation <- as.vector(rep(0,NDOSE*NRATS))


winStayRate.reversal1 <- as.vector(rep(0,NDOSE*NRATS))
loseShiftRate.reversal1 <- as.vector(rep(0,NDOSE*NRATS))
winStay.reversal1 <- as.vector(rep(0,NDOSE*NRATS))
loseShift.reversal1  <- as.vector(rep(0,NDOSE*NRATS))
wins.reversal1 <- as.vector(rep(0,NDOSE*NRATS))
losses.reversal1 <- as.vector(rep(0,NDOSE*NRATS))
loseShift.reversal1.mislead <- as.vector(rep(0,NDOSE*NRATS))
loseShiftRate.reversal1.mislead <- as.vector(rep(0,NDOSE*NRATS))
losses.reversal1.mislead <- as.vector(rep(0,NDOSE*NRATS))

seriel.number <- 0
for (dose in unique(data.beh$Dose)){
  for (rat in unique(data.beh$ID)){
    seriel.number <- seriel.number+1
    
    rm(data.subject)
    data.subject <- data.frame()
    data.subject <- data.beh[which(data.beh$ID==rat & data.beh$Dose==dose),]
    
    NTRIAL <- length(data.subject$trial_number)
    wCrit <- as.vector(rep(0,NTRIAL))
    first.reversal   <- 0
    second.reversal   <- 0
    rm(outcome)
    rm(choice)
    rm(optimal)
    rm(trial_number)
    outcome <- data.subject$MsGiven
    choice <- data.subject$SideChose
    optimal <- data.subject$Opt
    trial_number <- data.subject$trial_number
    
    for (trial in trial_number){
      id <- which(trial_number==trial)
      if (optimal[id] == 1){  #optimal means this choice was a correct one
        if (id  ==  1){
          wCrit[id] <- 1
        }else{
          if (wCrit[id-1] == 8) {
            wCrit[id]  <- 1
          }else{
            wCrit[id]  <-  wCrit[id-1] + 1
          }
        }
        if (wCrit[id]==8)  {
          if (first.reversal ==  0){
            first.reversal <- id
          }else if(second.reversal== 0){
            second.reversal <- id
          }
        }
      }
    }
    if (second.reversal == 0){
      second.reversal = NTRIAL
    }
    
    
    reversals[seriel.number] <- sum(wCrit==8)
    
    #Win-Stay and Lose-Shift
    wins[seriel.number] <- sum(outcome[1:(NTRIAL-1)])
    winStay[seriel.number] <- sum(outcome[1:(NTRIAL-1)]==1 & choice[1:(NTRIAL-1)]==choice[2:NTRIAL])
    loseShift[seriel.number] <- sum(outcome[1:(NTRIAL-1)]==0 & choice[1:(NTRIAL-1)]!=choice[2:NTRIAL])
    losses[seriel.number] <- sum(outcome[1:(NTRIAL-1)]==0)
    winStayRate[seriel.number] <- winStay[seriel.number] / wins[seriel.number]
    if (losses[seriel.number] > 0){
      loseShiftRate[seriel.number] <- loseShift[seriel.number] / losses[seriel.number]
    }else{
      loseShiftRate[seriel.number] <- NA
    }
      
    
    # acquisation 
    wins.acquisation[seriel.number] <- sum(outcome[1:(first.reversal-1)])
    winStay.acquisation[seriel.number] <- sum(outcome[1:(first.reversal-1)]==1 & choice[1:(first.reversal-1)]==choice[2:first.reversal])
    loseShift.acquisation[seriel.number] <- sum(outcome[1:(first.reversal-1)]==0 & choice[1:(first.reversal-1)]!=choice[2:first.reversal])
    losses.acquisation[seriel.number] <- sum(outcome[1:(first.reversal-1)]==0)
    
    winStayRate.acquisation[seriel.number] <- winStay.acquisation[seriel.number] / wins.acquisation[seriel.number]

    if (losses.acquisation[seriel.number] > 0){
      loseShiftRate.acquisation[seriel.number] <- loseShift.acquisation[seriel.number] / losses.acquisation[seriel.number]
    }else{
      loseShiftRate.acquisation[seriel.number] <- NA
    }
    
    # reversal 1
    wins.reversal1[seriel.number] <- sum(outcome[(first.reversal+1):(second.reversal-1)])
    winStay.reversal1[seriel.number] <- sum(outcome[(first.reversal+1):(second.reversal-1)]==1 & choice[(first.reversal+1):(second.reversal-1)]==choice[(first.reversal+2):second.reversal])
    loseShift.reversal1[seriel.number] <- sum(outcome[(first.reversal+1):(second.reversal-1)]==0 & choice[(first.reversal+1):(second.reversal-1)]!=choice[(first.reversal+2):second.reversal])
    losses.reversal1[seriel.number] <- sum(outcome[(first.reversal+1):(second.reversal-1)]==0)
    
    # lose shift after misleading feedbacks; the choice was a correct one but was not rewarded
    loseShift.reversal1.mislead[seriel.number] <- sum(outcome[(first.reversal+1):(second.reversal-1)]==0  & optimal[(first.reversal+1):(second.reversal-1)]==1 &
                                                        choice[(first.reversal+1):(second.reversal-1)]!=choice[(first.reversal+2):second.reversal])
    losses.reversal1.mislead[seriel.number] <- sum(outcome[(first.reversal+1):(second.reversal-1)]==0  & optimal[(first.reversal+1):(second.reversal-1)]==1)
    
    winStayRate.reversal1[seriel.number] <- winStay.reversal1[seriel.number] / wins.reversal1[seriel.number]

    if (losses.reversal1[seriel.number] > 0){
      loseShiftRate.reversal1[seriel.number] <- loseShift.reversal1[seriel.number] / losses.reversal1[seriel.number]
    }else{
      loseShiftRate.reversal1[seriel.number] <- NA
    }
    if (losses.reversal1.mislead[seriel.number] > 0){
      loseShiftRate.reversal1.mislead[seriel.number] <- loseShift.reversal1.mislead[seriel.number] / losses.reversal1.mislead[seriel.number]
    }else{
      loseShiftRate.reversal1.mislead[seriel.number] <- NA
    }
  }
}

dose <- as.factor(Dose)
behaviour <- data.frame(ID, dose, reversals, winStayRate, loseShiftRate, winStayRate.acquisation,
                        loseShiftRate.acquisation, winStayRate.reversal1, loseShiftRate.reversal1, loseShiftRate.reversal1.mislead)


compare_means(loseShiftRate.reversal1.mislead ~ dose,  data = behaviour)



p01 <- ggbarplot(behaviour, x = "dose", y = "reversals",
                color = "dose", palette = "jco", add = c("mean_se", "jitter"), legend = "", legend.title = "")+ 
  stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 15) + 
  theme(legend.position = "none") + labs(title="Empirical results")   # Add global p-value
p02 <- ggbarplot(behaviour, x = "dose", y = "loseShiftRate.acquisation",
                color = "dose", palette = "jco", add = c("mean_se", "jitter"), legend = "", legend.title = "")+ 
  stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1.5)  + 
  theme(legend.position = "none") + labs(title="   ")   # Add global p-value
p03 <- ggbarplot(behaviour, x = "dose", y = "loseShiftRate.reversal1",
                color = "dose", palette = "jco", add = c("mean_se", "jitter"), legend = "", legend.title = "")+ 
  stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1.5)   + 
  theme(legend.position = "none") + labs(title="   ")  # Add global p-value

ggarrange(p01, p02, p03, p1, p2, p3, nrow = 2, ncol = 3) + theme(text=element_text(size=16,  family="Arial"))

# p0123 <- ggarrange(p01, p02, p03, nrow = 1) + theme(text=element_text(size=16,  family="Arial"))

###### figure for reversals
# ggsave("BariSSRIexp.pdf", p012, width=17.5, height=7.5)
############

# Perorm pairwise comparisons
simcomp_exp <- array(rep(0,8*2*3), dim = c(8,2,3), dimnames = list(c(colnames(behaviour)[c(3:10)]),
             c('wt', 'p.value'), c('0 vs 1','1 vs 10','0 vs 10')))

for (i in colnames(behaviour)[c(3:10)]) {
    for (j in c(1:length(my_comparisons))){
         y <- wilcox.test(behaviour[which(behaviour$dose==my_comparisons[[j]][1]), i], 
          behaviour[which(behaviour$dose==my_comparisons[[j]][2]), i])
         simcomp_exp[i,"wt",j] <- y$statistic
         simcomp_exp[i,"p.value",j] <- y$p.value
   }
}

write.csv(simcomp_exp, file = 'expbehresult.csv')


# datanow <- behaviour[,c("dose","winStayRate.acquisation","loseShiftRate.acquisation")]
# datalong <- melt(datanow, id=c("dose"))

# # Box plot facetted by "dose"
# p <- ggbarplot(datalong, x = "dose", y = "value",
#                color = "dose", palette = "jco",
#                add = c("mean_se", "jitter"),
#                facet.by = "variable", short.panel.labs = FALSE)
# # Use only p.format as label. Remove method name.
# p + stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
#   stat_compare_means(label =  "p.format", label.y = 1.5)   # Add global p-value


# datanow <- behaviour[,c("dose","winStayRate.reversal1","loseShiftRate.reversal1")]
# datalong <- melt(datanow, id=c("dose"))

# # Box plot facetted by "dose"
# p <- ggbarplot(datalong, x = "dose", y = "value",
#                color = "dose", palette = "jco",
#                add = c("mean_se", "jitter"),
#                facet.by = "variable", short.panel.labs = FALSE)
# # Use only p.format as label. Remove method name.
# p + stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
#   stat_compare_means(label =  "p.format", label.y = 1.5)   # Add global p-value




# indiviudal parameters
run_models("2s")
para.indiviudal <- extract_per_subject_params()
drugnum <- as.factor(para.indiviudal$drugnum)
levels(drugnum) <- c(0,1,10)
para.indiviudal$drugnum <- drugnum
NPARA <- length(unique(para.indiviudal$baseparam))
NDOSE <- length(unique(para.indiviudal$drugnum))
NBEH <- length(colnames(behaviour)[c(3:9)])
p <- array(0, c(NPARA,NBEH,NDOSE), dimnames=list(c(unique(para.indiviudal$baseparam)), 
                                                   c(colnames(behaviour)[c(3:9)]),
                                                   levels(unique(para.indiviudal$drugnum))))
r <- p
for (paras in unique(para.indiviudal$baseparam)){
  for (dose in unique(para.indiviudal$drugnum)){
    para.now <- para.indiviudal$mean[which(para.indiviudal$baseparam==paras & para.indiviudal$drugnum==dose)]
    for (beh in colnames(behaviour)[c(3:9)]){
      beh.now <- behaviour[which(behaviour$dose == dose),beh]
      cor.result <- cor.test(para.now, beh.now)
      p[paras,beh,dose] <- cor.result$p.value
      r[paras,beh,dose] <- cor.result$estimate
    }
  }
}

write.csv(r, "indiviudalBehParaCorr_R.csv")
write.csv(p, "indiviudalBehParaCorr_pvalue.csv")

q <- array(p.adjust(p, method = "BH"), dim = dim(p), dimnames = dimnames(p))

write.csv(q, "indiviudalBehParaCorr_fdr.csv")

dose  <- 0    # 1, 10
paras <- "punish_rate_final" # "reinf_sensitivity_final" "side_stickiness_final"  
para.punish <- para.indiviudal$mean[which(para.indiviudal$baseparam==paras & para.indiviudal$drugnum==dose)]
paras <- "reinf_sensitivity_final" #"side_stickiness_final"  
para.sensit <- para.indiviudal$mean[which(para.indiviudal$baseparam==paras & para.indiviudal$drugnum==dose)]
paras <- "side_stickiness_final"  
para.sticki <- para.indiviudal$mean[which(para.indiviudal$baseparam==paras & para.indiviudal$drugnum==dose)]
beh <- "winStayRate" # reversals"  "winStayRate"  "loseShiftRate"
beh.winStay <- behaviour[which(behaviour$dose == dose),beh]
beh <- "loseShiftRate"
beh.losShif <- behaviour[which(behaviour$dose == dose),beh]
dataplot <- data.frame(para.punish, para.sensit, para.sticki, beh.winStay, beh.losShif)
colnames(dataplot) <- c("punish_rate_final", "reinf_sensitivity_final", "side_stickiness_final","winStayRate", "loseShiftRate")
p1 <- ggplot(
  dataplot, aes(x = punish_rate_final, y = winStayRate)
) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) 

p2 <- ggplot(
  dataplot, aes(x = reinf_sensitivity_final, y = winStayRate)
) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) 

p3 <- ggplot(
  dataplot, aes(x = side_stickiness_final, y = loseShiftRate)
) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) 

p123 <- ggarrange(p1, p2, p3, nrow = 1) + theme(text=element_text(size=16,  family="Arial"))
ggsave("plot1.pdf", p123, width=4, height=4)


