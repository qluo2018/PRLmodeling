# install.packages("ggpubr")
# install.packages("reshape2")
library(ggpubr)
library(reshape2)
library(gdata)
library(ggplot2)
library(data.table)

# test the association between the individual parameters and the behavioural indeces

data.beh <- read.csv("cleaned_depletion_bari.csv")

# ##################
# # 7 sessions separately 
# ##################
# # reversals, winStayRate, loseShiftRate
# data.subject <- data.frame()
# NDOSE <- length(unique(data.beh$session))
# NRATS <- length(unique(data.beh$patient))

# reversals <- as.vector(rep(0,NDOSE*NRATS))
# ID <- as.vector(rep(unique(data.beh$patient),NDOSE))

# Dose  <-  as.matrix(rep(unique(data.beh$session)[1],NRATS), 1, NRATS)
# for (dose in c(2:length(unique(data.beh$session)))){
#   Dose <- rbind(Dose, as.matrix(rep(unique(data.beh$session)[dose],NRATS),1,NRATS))
# }
# Dose <- as.vector(Dose)
  
# winStayRate <- as.vector(rep(0,NDOSE*NRATS))
# loseShiftRate <- as.vector(rep(0,NDOSE*NRATS))
# winStay <- as.vector(rep(0,NDOSE*NRATS))
# loseShift  <- as.vector(rep(0,NDOSE*NRATS))
# wins <- as.vector(rep(0,NDOSE*NRATS))
# losses <- as.vector(rep(0,NDOSE*NRATS))

# winStayRate.acquisation <- as.vector(rep(0,NDOSE*NRATS))
# loseShiftRate.acquisation <- as.vector(rep(0,NDOSE*NRATS))
# winStay.acquisation<- as.vector(rep(0,NDOSE*NRATS))
# loseShift.acquisation  <- as.vector(rep(0,NDOSE*NRATS))
# wins.acquisation<- as.vector(rep(0,NDOSE*NRATS))
# losses.acquisation <- as.vector(rep(0,NDOSE*NRATS))


# winStayRate.reversal1 <- as.vector(rep(0,NDOSE*NRATS))
# loseShiftRate.reversal1 <- as.vector(rep(0,NDOSE*NRATS))
# winStay.reversal1 <- as.vector(rep(0,NDOSE*NRATS))
# loseShift.reversal1  <- as.vector(rep(0,NDOSE*NRATS))
# wins.reversal1 <- as.vector(rep(0,NDOSE*NRATS))
# losses.reversal1 <- as.vector(rep(0,NDOSE*NRATS))

# group <- as.vector(rep(0,NDOSE*NRATS))
  
# seriel.number <- 0
# for (dose in unique(data.beh$session)){
#   for (rat in unique(data.beh$patient)){
#     seriel.number <- seriel.number+1
    
#     rm(data.subject)
#     data.subject <- data.frame()
#     data.subject <- data.beh[which(data.beh$patient==rat & data.beh$session==dose),]
    
#     group[seriel.number] <- data.subject$Group[1]
    
    
#     NTRIAL <- length(data.subject$trial_number)
#     wCrit <- as.vector(rep(0,NTRIAL))
#     first.reversal   <- 0
#     second.reversal   <- 0
#     rm(outcome)
#     rm(choice)
#     rm(optimal)
#     rm(trial_number)
#     outcome <- data.subject$MsGiven
#     choice <- data.subject$SideChose
#     optimal <- data.subject$Opt
#     trial_number <- data.subject$trial_number
    
#     for (trial in trial_number){
#       id <- which(trial_number==trial)
#       if (optimal[id] == 1){
#         if (id  ==  1){
#           wCrit[id] <- 1
#         }else{
#           if (wCrit[id-1] == 8) {
#             wCrit[id]  <- 1
#           }else{
#             wCrit[id]  <-  wCrit[id-1] + 1
#           }
#         }
#         if (wCrit[id]==8)  {
#           if (first.reversal ==  0){
#             first.reversal <- id
#           }else if(second.reversal== 0){
#             second.reversal <- id
#           }
#         }
#       }
#     }
#     if (second.reversal == 0){
#       second.reversal = NTRIAL
#     }
    
    
#     reversals[seriel.number] <- sum(wCrit==8)
    
#     #Win-Stay and Lose-Shift
#     wins[seriel.number] <- sum(outcome[1:(NTRIAL-1)])
#     winStay[seriel.number] <- sum(outcome[1:(NTRIAL-1)]==1 & choice[1:(NTRIAL-1)]==choice[2:NTRIAL])
#     loseShift[seriel.number] <- sum(outcome[1:(NTRIAL-1)]==0 & choice[1:(NTRIAL-1)]!=choice[2:NTRIAL])
#     losses[seriel.number] <- sum(outcome[1:(NTRIAL-1)]==0)
#     winStayRate[seriel.number] <- winStay[seriel.number] / wins[seriel.number]
#     if (losses[seriel.number] > 0){
#       loseShiftRate[seriel.number] <- loseShift[seriel.number] / losses[seriel.number]
#     }else{
#       loseShiftRate[seriel.number] <- NA
#     }
#   }
# }
# dose <- as.factor(Dose)
# behaviour <- data.frame(ID, dose, group, reversals, winStayRate, loseShiftRate)
# colnames(behaviour)[2] <- "sess"
# behaviour <- data.table(behaviour)
# rm(s2)
# s2 <- behaviour[
#   ,
#   list(
#     reversals_mean = mean(reversals),
#     reversals_sem = sem(reversals),
#     winStayRate_mean = mean(winStayRate),
#     winStayRate_sem = sem(winStayRate),
#     loseShiftRate_mean = mean(loseShiftRate),
#     loseShiftRate_sem = sem(loseShiftRate)),
#   by = .(sess, group)
#   ]
# s2$group <- as.factor(s2$group)
# levels(s2$group) <- c("Sham", "Lesion")
# p1 <- ggplot(s2,
#              aes(y = reversals_mean,
#                  x = sess,
#                  group = group,
#                  colour = group)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = reversals_mean - reversals_sem,
#                     ymax = reversals_mean + reversals_sem)) + 
#   scale_colour_discrete(drop=TRUE,
#                            limits = levels(s2$group))
# p2 <- ggplot(s2,
#              aes(y = winStayRate_mean,
#                  x = sess,
#                  group = group,
#                  colour = group)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = winStayRate_mean - winStayRate_sem,
#                     ymax = winStayRate_mean + winStayRate_sem)) + 
#   scale_colour_discrete(drop=TRUE,
#                            limits = levels(s2$group))


# p3 <- ggplot(s2,
#              aes(y = loseShiftRate_mean,
#                  x = sess,
#                  group = group,
#                  colour = group)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = loseShiftRate_mean - loseShiftRate_sem,
#                     ymax = loseShiftRate_mean + loseShiftRate_sem)) + 
#   scale_colour_discrete(drop=TRUE,
#                            limits = levels(s2$group))
# ggarrange(p1,p2,p3, ncol=1)

##############################
# if we pool the 7 sessions together
#############################

data.subject <- data.frame()
NRATS <- length(unique(data.beh$patient))

reversals <- as.vector(rep(0,NRATS))
ID <- as.vector(unique(data.beh$patient))

winStayRate <- as.vector(rep(0,NRATS))
loseShiftRate <- as.vector(rep(0,NRATS))
winStay <- as.vector(rep(0,NRATS))
loseShift  <- as.vector(rep(0,NRATS))
wins <- as.vector(rep(0,NRATS))
losses <- as.vector(rep(0,NRATS))

winStayRate.acquisation <- as.vector(rep(0,NRATS))
loseShiftRate.acquisation <- as.vector(rep(0,NRATS))
winStay.acquisation<- as.vector(rep(0,NRATS))
loseShift.acquisation  <- as.vector(rep(0,NRATS))
wins.acquisation<- as.vector(rep(0,NRATS))
losses.acquisation <- as.vector(rep(0,NRATS))


winStayRate.reversal1 <- as.vector(rep(0,NRATS))
loseShiftRate.reversal1 <- as.vector(rep(0,NRATS))
winStay.reversal1 <- as.vector(rep(0,NRATS))
loseShift.reversal1  <- as.vector(rep(0,NRATS))
wins.reversal1 <- as.vector(rep(0,NRATS))
losses.reversal1 <- as.vector(rep(0,NRATS))

group <- as.vector(rep(0,NRATS))

seriel.number <- 0 # an ID for rat
for (rat in unique(data.beh$patient)){
  seriel.number <- seriel.number+1
  
  data.subject <- data.frame()
  data.subject <- data.beh[which(data.beh$patient==rat),]
  
  group[seriel.number] <- data.subject$Group[1]
  
  
  for (sess in unique(data.subject$session)){
    
    # data of the current session
    data.subject.sess <- data.subject[which(data.subject$session==sess),]
      
    outcome <- data.subject.sess$MsGiven
    choice <- data.subject.sess$SideChose
    optimal <- data.subject.sess$Opt
    trial_number <- data.subject.sess$trial_number
    NTRIAL <- length(trial_number)
    wCrit <- as.vector(rep(0,NTRIAL))
    
    for (trial in trial_number){
      id <- which(trial_number==trial)
      if (optimal[id] == 1){
        if (id  ==  1){
          wCrit[id] <- 1
        }else{
          if (wCrit[id-1] == 8) {
            wCrit[id]  <- 1
          }else{
            wCrit[id]  <-  wCrit[id-1] + 1
          }
        }
      }
    }
  
    # accumulating across 7 sessions for each rat
    
    # Number of reversals completed
    reversals[seriel.number] <- reversals[seriel.number] + sum(wCrit==8)  
    
    # Win-Stay and Lose-Shift
    wins[seriel.number] <- wins[seriel.number] + sum(outcome[1:(NTRIAL-1)])
    winStay[seriel.number] <- winStay[seriel.number] + sum(outcome[1:(NTRIAL-1)]==1 & choice[1:(NTRIAL-1)]==choice[2:NTRIAL])
    loseShift[seriel.number] <- loseShift[seriel.number] + sum(outcome[1:(NTRIAL-1)]==0 & choice[1:(NTRIAL-1)]!=choice[2:NTRIAL])
    losses[seriel.number] <- losses[seriel.number] + sum(outcome[1:(NTRIAL-1)]==0)
  }
  # overall rate among 7 sessions
  winStayRate[seriel.number] <- winStay[seriel.number] / wins[seriel.number]
  if (losses[seriel.number] > 0){
    loseShiftRate[seriel.number] <- loseShift[seriel.number] / losses[seriel.number]
  }else{
    loseShiftRate[seriel.number] <- NA
  }
  reversals[seriel.number]  <- reversals[seriel.number]  / length(unique(data.subject$session))
}

behaviour <- data.frame(ID, group, reversals, winStayRate, loseShiftRate, wins, losses)
behaviour <- data.table(behaviour)
behaviour$group <- as.factor(behaviour$group)
levels(behaviour$group) <- c("Sham", "Depletion")
behaviour.outliers <-  behaviour[which(behaviour$reversals>1),]
p1 <- ggboxplot(behaviour.outliers, x = "group", y = "reversals",
          color = "group", add = "jitter")+
  stat_compare_means() + theme(legend.position = "none") + labs(title="Empirical results") 
p2 <- ggboxplot(behaviour.outliers, x = "group", y = "winStayRate",
                color = "group", add = "jitter") +
  stat_compare_means() + theme(legend.position = "none") + labs(title="   ")
p3 <- ggboxplot(behaviour.outliers, x = "group", y = "loseShiftRate",
                color = "group", add = "jitter")+
  stat_compare_means()+ theme(legend.position = "none") + labs(title="   ")
# ggarrange(p11, p21, p31, nrow = 1) + theme(text=element_text(size=16,  family="Arial"))


# p1 <- ggboxplot(behaviour, x = "group", y = "reversals",
#                 color = "group", add = "jitter", outlier.shape = NA)+
#   stat_compare_means() 
# p2 <- ggboxplot(behaviour, x = "group", y = "winStayRate",
#                 color = "group", add = "jitter", outlier.shape = NA)+
#   stat_compare_means()
# p3 <- ggboxplot(behaviour, x = "group", y = "loseShiftRate",
#                 color = "group", add = "jitter", outlier.shape = NA)+
#   stat_compare_means()
# # ggarrange(p1, p2, p3, nrow = 2, ncol = 3) + theme(text=element_text(size=16,  family="Arial"))
# 
# p16 <- ggarrange(p1, p2, p3, p11, p21, p31, nrow = 2, ncol = 3) + theme(text=element_text(size=16,  family="Arial"))
# ggsave('expwithwithoutoutliers.pdf', p16, width=17, height=7)

############################
# individual parameters
############################

source('~/projects/behmodeling/new/compbehmodeling/LSD/support/toolkits.R')

run_models("2s")
para.indiviudal <- extract_per_subject_params()
groupname <- as.factor(para.indiviudal$groupname)
groupname <- relevel(groupname, "Sham")
NPARA <- length(unique(para.indiviudal$baseparam))
NBEH <- length(colnames(behaviour)[c(3:5)])
NGRUP <- length(levels(groupname))
behaviour <- as.data.frame(behaviour)

behaviour.outliers.id <-  behaviour$ID[which(behaviour$reversals<1)]

p <- array(0, c(NPARA,NBEH, NGRUP), dimnames=list(c(unique(para.indiviudal$baseparam)), 
                                                 c(colnames(behaviour)[c(3:5)]),
                                                 c(levels(groupname))))
r <- p
for (paras in unique(para.indiviudal$baseparam)){
  for (beh in colnames(behaviour)[c(3:5)]){
    for (grp in levels(groupname)){
      para.now <- para.indiviudal$mean[which(para.indiviudal$baseparam==paras & groupname==grp & !is_in(para.indiviudal$subject, behaviour.outliers.id))]
      row.id <- which(behaviour$group == grp & !is_in(behaviour$ID, behaviour.outliers.id))
      
      beh.now <- behaviour[[which(colnames(behaviour) == beh)]][row.id]
      
      cor.result <- cor.test(para.now, beh.now)
      p[paras,beh,grp] <- cor.result$p.value
      r[paras,beh,grp] <- cor.result$estimate
    }
  }
}


write.csv(r, "indiviudalBehParaCorr_R_depletion.csv")
write.csv(p, "indiviudalBehParaCorr_p_depletion.csv")

q <- array(p.adjust(p, method = "BH"), dim = dim(p), dimnames = dimnames(p))

write.csv(q, "indiviudalBehParaCorr_fdr.csv")


# dataplot <- behaviour
# colnames(dataplot)[c(1,2)] <- c("subject","groupname")
# for (paras in unique(para.indiviudal$baseparam)){
#     para.ind.reformate <- para.indiviudal[which(para.indiviudal$baseparam==paras), c("mean", "groupname", "subject")]
#     colnames(para.ind.reformate)[1] <- paras
#     dataplot <- merge(dataplot, para.ind.reformate, by  = c("subject","groupname"))
# }
    

# ggplot(
#   dataplot[!is_in(dataplot$subject,behaviour.outliers.id),], aes(x = side_stickiness_final, y = winStayRate, facet = groupname, color = groupname)
# ) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) 

# p1 <- ggboxplot(dataplot[!is_in(dataplot$subject,behaviour.outliers.id),], x = "groupname", y = "side_stickiness_final",
#                 color = "groupname", add = "jitter")+
#   stat_compare_means()


# ggarrange(p1, p2, p3, nrow = 1) + theme(text=element_text(size=16,  family="Arial"))
# ggsave("plot1.pdf", width=4, height=4)


##############################
# simulations
#############################

source('bayesian_reversals_ql_depl.R')
run_models('2')
x <- stanfunc$annotated_parameters(fit2, par_exclude_regex = c("p_choose_stim1", "subject_effect"))

source('~/projects/behmodeling/new/compbehmodeling/LSD/support/toolkits.R')

# sham
x.mean <- as.data.frame(t(x[c(1,3,5,7), mean])) 
colnames(x.mean) <- c('alpha_g', 'alpha_l', 'beta', 'kappa')
x.sd <- as.data.frame(t(x[c(1,3,5,7), sd]), nrow=1, ncol=4) 
colnames(x.sd) <- c('alpha_g', 'alpha_l', 'beta', 'kappa')
x.file<- 'test_ql_shamV3.csv'
simulatewithstats.cri(x.mean, x.sd, x.file)

# DHT
x.mean <- as.data.frame(t(x[c(2,4,6,8), mean])) 
colnames(x.mean) <- c('alpha_g', 'alpha_l', 'beta', 'kappa')
x.sd <- as.data.frame(t(x[c(2,4,6,8), sd]), nrow=1, ncol=4) 
colnames(x.sd) <- c('alpha_g', 'alpha_l', 'beta', 'kappa')
x.file<- 'test_ql_deplV3.csv'
simulatewithstats.cri(x.mean, x.sd, x.file)



data0 <- read.csv("test_ql_shamV3.csv", header = T)
data1 <- read.csv("test_ql_deplV3.csv", header = T)

#
data0 <- cbind(data0,rep(1,40)) 
colnames(data0)[20] <- 'group'
data1 <- cbind(data1,rep(2,40)) 
colnames(data1)[20] <- 'group'
data <- rbind(data0, data1)

data$group <- as.factor(data$group)
levels(data$group) <- c("Sham", "Depletion")

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
compare_means(reversals ~ group,  data = data)
#wilcox.test(data$loseShiftRate[which(data$group=='Sham')], data$loseShiftRate[which(data$group=='Depletion')])

# Perorm pairwise comparisons
simcomp <- array(rep(0,19*2*1), dim = c(19,2,1), dimnames = list(c(colnames(data)[c(1:19)]),
             c('wt', 'p.value'), c('Sham vs Depletion')))

my_comparisons <- list(c('Sham', 'Depletion'))
for (i in colnames(data)[c(1:19)]) {
    for (j in c(1:length(my_comparisons))){
         y <- wilcox.test(data[which(data$group==my_comparisons[[j]][1]), i], 
          data[which(data$group==my_comparisons[[j]][2]), i])
         simcomp[i,"wt",j] <- y$statistic
         simcomp[i,"p.value",j] <- y$p.value
   }
}

write.csv(simcomp, file = 'simbehresult.csv')


# Visualize: Specify the comparisons you want


p31 = ggboxplot(data, x = "group", y = "reversals",
          color = "group", add = "jitter")+ 
  stat_compare_means(label.y = 20) +
  theme(legend.position = "none") + labs(title="Computational results") # Add global p-value

p32 = ggboxplot(data, x = "group", y = "winStayRate",
          color = "group", add = "jitter")+ 
  stat_compare_means(label.y = 1) +
  theme(legend.position = "none") + labs(title="   ")  # Add global p-value


p33 = ggboxplot(data, x = "group", y = "loseShiftRate",
          color = "group", add = "jitter")+ 
  stat_compare_means(label.y = 1)  +
  theme(legend.position = "none") + labs(title="   ")  # Add global p-value

# p123<-ggarrange(p31, p32, p33, nrow = 1) + theme(text=element_text(size=16,  family="Arial"))
# ggsave('simBehresult.pdf', p123, width=17, height=7)


ggarrange(p1, p2, p3, p31, p32, p33, nrow = 2, ncol = 3) + theme(text=element_text(size=16,  family="Arial"))



