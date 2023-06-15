# install.packages("ggpubr")
# install.packages("reshape2")
library(ggpubr)
library(reshape2)
library(gdata)
library(ggplot2)
library(data.table)
library(xlsx)

# test the association between the individual parameters and the behavioural indeces

data.beh <- fulldata  # read the behavioural data


data.subject <- data.frame()
NINDS <- length(unique(data.beh$ID))

perseveration <- as.vector(rep(0,NINDS))
ID <- as.vector(unique(data.beh$ID))

winStayRate <- as.vector(rep(0,NINDS))
loseShiftRate <- as.vector(rep(0,NINDS))
winStay <- as.vector(rep(0,NINDS))
loseShift  <- as.vector(rep(0,NINDS))
wins <- as.vector(rep(0,NINDS))
losses <- as.vector(rep(0,NINDS))

winStayRate.acquisation <- as.vector(rep(0,NINDS))
loseShiftRate.acquisation <- as.vector(rep(0,NINDS))
winStay.acquisation<- as.vector(rep(0,NINDS))
loseShift.acquisation  <- as.vector(rep(0,NINDS))
wins.acquisation<- as.vector(rep(0,NINDS))
losses.acquisation <- as.vector(rep(0,NINDS))


winStayRate.reversal1 <- as.vector(rep(0,NINDS))
loseShiftRate.reversal1 <- as.vector(rep(0,NINDS))
winStay.reversal1 <- as.vector(rep(0,NINDS))
loseShift.reversal1  <- as.vector(rep(0,NINDS))
wins.reversal1 <- as.vector(rep(0,NINDS))
losses.reversal1 <- as.vector(rep(0,NINDS))

group <- as.vector(rep(0,NINDS))

first.reversal <- 40
second.reversal <- 80
# calculateperseveration <- function(choiceafterreversal)
# {
#   # two adjecent going back to the previous optimal choice
#   # in this Jon experiment, the optimal choice at the aquisation phase was always option 1 
#   n <- length(choiceafterreversal)
#   d <- as.vector(rep(0,n))
#   if (choiceafterreversal[1]==1) {
#     d[1] <- 1
#   }
#   for(i in c(2:n)){
#     if (choiceafterreversal[i]==1) {
#       d[i] <- d[i-1] + 1
#     }else if (choiceafterreversal[i]==2) {
#       d[i] <- 0
#     }
#   }
#   
#   return(sum(d>1))
# }

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

# 2023.5.29 by Qiang for Review 2 comment 1

prob.chose.optimal.grp1 <- as.vector(rep(0,80))  # a probability of choosing the optimal option in group 1
prob.chose.optimal.grp2 <- as.vector(rep(0,80))  # in group 2


seriel.number <- 0 # an ID for participant
for (ind in unique(data.beh$ID)){
  seriel.number <- seriel.number+1
  
  data.subject <- data.frame()
  data.subject <- data.beh[which(data.beh$ID==ind),]
  
  group[seriel.number] <- data.subject$group[1]
  
  
  for (sess in unique(data.subject$sess)){
    
    # data of the current sess
    data.subject.sess <- data.subject[which(data.subject$sess==sess),]
      
    outcome <- data.subject.sess$reinforced
    choice <- data.subject.sess$stim_chosen
    optimal <- data.subject.sess$choice_was_optimal
    trial_number <- data.subject.sess$trial_number
    NTRIAL <- length(trial_number)
  
    # Number of reversals completed
    perseveration[seriel.number] <- calculateperseveration(choice[c((NTRIAL/2+1):NTRIAL)])  
    
    # Win-Stay and Lose-Shift
    wins[seriel.number] <- wins[seriel.number] + sum(outcome[1:(NTRIAL-1)])
    winStay[seriel.number] <- winStay[seriel.number] + sum(outcome[1:(NTRIAL-1)]==1 & choice[1:(NTRIAL-1)]==choice[2:NTRIAL])
    loseShift[seriel.number] <- loseShift[seriel.number] + sum(outcome[1:(NTRIAL-1)]==0 & choice[1:(NTRIAL-1)]!=choice[2:NTRIAL])
    losses[seriel.number] <- losses[seriel.number] + sum(outcome[1:(NTRIAL-1)]==0)
    
    # acquisation 
    wins.acquisation[seriel.number] <- sum(outcome[1:(first.reversal-1)])
    winStay.acquisation[seriel.number] <- sum(outcome[1:(first.reversal-1)]==1 & choice[1:(first.reversal-1)]==choice[2:first.reversal])
    loseShift.acquisation[seriel.number] <- sum(outcome[1:(first.reversal-1)]==0 & choice[1:(first.reversal-1)]!=choice[2:first.reversal])
    losses.acquisation[seriel.number] <- sum(outcome[1:(first.reversal-1)]==0 & choice[1:(first.reversal-1)]>0)
    
    
    
    # reversal 1
    wins.reversal1[seriel.number] <- sum(outcome[(first.reversal+1):(second.reversal-1)])
    winStay.reversal1[seriel.number] <- sum(outcome[(first.reversal+1):(second.reversal-1)]==1 & choice[(first.reversal+1):(second.reversal-1)]==choice[(first.reversal+2):second.reversal])
    loseShift.reversal1[seriel.number] <- sum(outcome[(first.reversal+1):(second.reversal-1)]==0 & choice[(first.reversal+1):(second.reversal-1)]!=choice[(first.reversal+2):second.reversal])
    losses.reversal1[seriel.number] <- sum(outcome[(first.reversal+1):(second.reversal-1)]==0 & choice[(first.reversal+1):(second.reversal-1)]>0)
    
    # probability 2023.5.29
    if (group[seriel.number] == 1 & length(optimal) == 80) {
      prob.chose.optimal.grp1 <- prob.chose.optimal.grp1 + optimal
    } else if (group[seriel.number] == 2 & length(optimal) == 80) {
      prob.chose.optimal.grp2 <- prob.chose.optimal.grp2 + optimal
    } else {
      print(optimal)
      print(group[seriel.number])
      print(seriel.number)
    }
    
  }
  # overall rate among sesss
  winStayRate[seriel.number] <- winStay[seriel.number] / wins[seriel.number]
  if (losses[seriel.number] > 0){
    loseShiftRate[seriel.number] <- loseShift[seriel.number] / losses[seriel.number]
  }else{
    loseShiftRate[seriel.number] <- NA
  }
  perseveration[seriel.number]  <- perseveration[seriel.number]  / length(unique(data.subject$sess))
  
  winStayRate.acquisation[seriel.number] <- winStay.acquisation[seriel.number] / wins.acquisation[seriel.number]
  loseShiftRate.acquisation[seriel.number] <- loseShift.acquisation[seriel.number] / losses.acquisation[seriel.number]
  
  winStayRate.reversal1[seriel.number] <- winStay.reversal1[seriel.number] / wins.reversal1[seriel.number]
  loseShiftRate.reversal1[seriel.number] <- loseShift.reversal1[seriel.number] / losses.reversal1[seriel.number]
  
  
}

# 22023.5.30 by Qiang
# group = 2, seriel.number = 39
prob.chose.optimal.grp1 <- prob.chose.optimal.grp1 / sum(group == 1)
prob.chose.optimal.grp2 <- prob.chose.optimal.grp2 / (sum(group == 2)-1)
df1 =data.frame( trial = trial_number, p_chose_correctly = prob.chose.optimal.grp1)
p1 = ggplot(data=df1, aes(x=trial, y=p_chose_correctly)) +
  geom_step()+
  geom_point()
df2 =data.frame( trial = trial_number, p_chose_correctly = prob.chose.optimal.grp2)
p2 = ggplot(data=df2, aes(x=trial, y=p_chose_correctly)) +
  geom_step()+
  geom_point()
ggarrange(p1, p2, nrow = 1) + theme(text=element_text(size=16,  family="Arial"))
# ########


behaviour <- data.frame(ID, group, perseveration, winStayRate, loseShiftRate, wins, losses, 
                        winStayRate.acquisation, loseShiftRate.acquisation, 
                        winStayRate.reversal1, loseShiftRate.reversal1, losses.acquisation, losses.reversal1)
behaviour <- data.table(behaviour)
behaviour$group <- as.factor(behaviour$group)
levels(behaviour$group) <- c("Placebo", "Escitalopram")


p1 <- ggboxplot(behaviour, x = "group", y = "losses",
                color = "group", add = "jitter", outlier.shape = NA)+
  stat_compare_means() 
p2 <- ggboxplot(behaviour, x = "group", y = "winStayRate",
                color = "group", add = "jitter", outlier.shape = NA)+
  stat_compare_means()
p3 <- ggboxplot(behaviour, x = "group", y = "loseShiftRate",
                color = "group", add = "jitter", outlier.shape = NA)+
  stat_compare_means()
ggarrange(p1, p2, p3, nrow = 1) + theme(text=element_text(size=16,  family="Arial"))


p1 <- ggboxplot(behaviour, x = "group", y = "losses.acquisation",
                color = "group", add = "jitter", outlier.shape = NA)+
  stat_compare_means() + theme(legend.position = "none") + labs(title="Empirical results")
p2 <- ggboxplot(behaviour, x = "group", y = "winStayRate.acquisation",
                color = "group", add = "jitter", outlier.shape = NA)+
  stat_compare_means() + theme(legend.position = "none") + labs(title=" ")
p3 <- ggboxplot(behaviour, x = "group", y = "loseShiftRate.acquisation",
                color = "group", add = "jitter", outlier.shape = NA)+
  stat_compare_means() + theme(legend.position = "none") + labs(title=" ")
ggarrange(p1, p2, p3, nrow = 1) + theme(text=element_text(size=16,  family="Arial"))

p1 <- ggboxplot(behaviour, x = "group", y = "losses.reversal1",
                color = "group", add = "jitter", outlier.shape = NA)+
  stat_compare_means() 
p2 <- ggboxplot(behaviour, x = "group", y = "winStayRate.reversal1",
                color = "group", add = "jitter", outlier.shape = NA)+
  stat_compare_means()
p3 <- ggboxplot(behaviour, x = "group", y = "loseShiftRate.reversal1",
                color = "group", add = "jitter", outlier.shape = NA)+
  stat_compare_means()
ggarrange(p1, p2, p3, nrow = 1) + theme(text=element_text(size=16,  family="Arial"))

############################
# individual parameters
############################

run_models("3s")
para.indiviudal <- extract_per_subject_params()
groupname <- as.factor(para.indiviudal$groupname)
print(groupname) # see if necessary to reorder the levels
groupname <- relevel(groupname, "Placebo")
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
      
      cor.result <- cor.test(para.now, beh.now, method ="spearman")
      p[paras,beh,grp] <- cor.result$p.value
      r[paras,beh,grp] <- cor.result$estimate
    }
  }
}


write.csv(r, "indiviudalBehParaCorr_R_ssri.csv")
write.csv(p, "indiviudalBehParaCorr_p_ssri.csv")

q <- array(p.adjust(p, method = "BH"), dim = dim(p), dimnames = dimnames(p))

write.csv(q, "indiviudalBehParaCorr_fdr.csv")


dataplot <- behaviour
colnames(dataplot)[c(1,2)] <- c("subject","groupname")
for (paras in unique(para.indiviudal$baseparam)){
    para.ind.reformate <- para.indiviudal[which(para.indiviudal$baseparam==paras), c("mean", "groupname", "subject")]
    colnames(para.ind.reformate)[1] <- paras
    dataplot <- merge(dataplot, para.ind.reformate, by  = c("subject","groupname"))
}
    


p1 <- ggboxplot(dataplot[!is_in(dataplot$subject,behaviour.outliers.id),], x = "groupname", y = "stim_stickiness_final",
                color = "groupname", add = "jitter")+
  stat_compare_means()


p1 <- ggplot(
  dataplot[!is_in(dataplot$subject,behaviour.outliers.id),], aes(x = stim_stickiness_final, y = winStayRate, facet = groupname, color = groupname)
) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) 

p2 <- ggplot(
  dataplot[!is_in(dataplot$subject,behaviour.outliers.id),], aes(x = stim_stickiness_final, y = loseShiftRate, facet = groupname, color = groupname)
) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) 

ggarrange(p1, p2, nrow = 2) + theme(text=element_text(size=16,  family="Arial"))
ggsave("plot1.pdf", width=4, height=4)

#########################
# correlations with OCI
########################

data.scores <- read.csv("ForQiang2.csv", header = T)

oci.subscales <- read.csv("OCI-R subscales escitalopram 4Qiang.csv", header = T)
colnames(oci.subscales) <- c("ID", "OCI.total", "Hoarding", "Checking", "Ordering", "Neutralizing", "Washing", "Obsessing")

data.scores.new <- merge(data.scores, oci.subscales, by = "ID", all.x = T)



NSCO = 10 # BDI, STAI_state, STAI_trait, OCI.total, Hoarding, Checking, Ordering, Neutralizing, Washing, Obsessing

p.score <- array(1, c(NPARA,NSCO, NGRUP), dimnames=list(c(unique(para.indiviudal$baseparam)), 
                                                  c(colnames(data.scores.new)[c(2,4:12)]),
                                                  c(levels(groupname))))
r.score <- p.score


for (paras in unique(para.indiviudal$baseparam)){
  for (score in colnames(data.scores.new)[c(2,4:12)]){
    
    # within group 
    for (grp in levels(groupname)){
      para.now <- para.indiviudal$mean[which(para.indiviudal$baseparam==paras & groupname==grp & !is_in(para.indiviudal$subject, behaviour.outliers.id))]
      row.id <- which(behaviour$group == grp & !is_in(behaviour$ID, behaviour.outliers.id)) 
      
      score.now <- data.scores.new[[which(colnames(data.scores.new) == score)]][row.id]
      
      cor.result <- cor.test(para.now, score.now, method ="pearson")
      p.score[paras,score,grp] <- cor.result$p.value
      r.score[paras,score,grp] <- cor.result$estimate
    }
  }
}

p.score.toadj <- p.score[,c(5:10),]
q.score <- array(p.adjust(p.score.toadj, method = "BH"), 
                  dim = dim(p.score.toadj), 
                  dimnames = dimnames(p.score.toadj))


colnames(data.scores.new)[1] <- "subject"
dataplot <- merge(dataplot, data.scores.new, by  = c("subject"))

p1 <- ggplot(
  dataplot, aes(x = stim_stickiness_final, y = OCI.total, facet = groupname, color = groupname)
) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) 


t <- array(1, c(NPARA,NSCO), dimnames=list(c(unique(para.indiviudal$baseparam)), 
                                                  c(colnames(data.scores.new)[c(2,4:12)])))
t.p <- t
f <- t
f.p <- t
shapitestp <- t

# between group: test the modulation effect of the escitalopram
for (paras in unique(para.indiviudal$baseparam)){
  for (score in colnames(data.scores.new)[c(2,4:12)]){
    
    para.now.between <- para.indiviudal$mean[which(para.indiviudal$baseparam==paras & !is_in(para.indiviudal$subject, behaviour.outliers.id))]
    row.id.between <- which(!is_in(behaviour$ID, behaviour.outliers.id))
    score.now.between <- data.scores.new[[which(colnames(data.scores.new) == score)]][row.id.between]
    groupname.now.between <- groupname[row.id.between]
    
    
    fit <- lm(score.now.between ~ para.now.between * groupname.now.between)
    shapitestp[paras, score] <- shapiro.test(residuals(fit))$p.value
    t[paras, score] <- summary(fit)$coefficients["para.now.between:groupname.now.betweenEscitalopram","t value"]
    t.p[paras, score] <- summary(fit)$coefficients["para.now.between:groupname.now.betweenEscitalopram","Pr(>|t|)"]
    ftests <- anova(fit)
    f[paras, score] <- ftests["para.now.between:groupname.now.between","F value"]
    f.p[paras, score] <- ftests["para.now.between:groupname.now.between","Pr(>F)"]
  }
}

inter.effect <- data.frame(t=t,t.p=t.p,f=f, f.p=f.p, shapitestp = shapitestp)




# permutation
set.seed(300)
perm.count <- t * 0
perm.total <- 3000

for (nperm in c(1:perm.total)){
  perm.sample <- sample.int(length(score.now.between))
  t.perm <- t * 0
  
  for (paras in unique(para.indiviudal$baseparam)){
    para.now.between <- para.indiviudal$mean[which(para.indiviudal$baseparam==paras & !is_in(para.indiviudal$subject, behaviour.outliers.id))]
    perm.para.now.between <- para.now.between[perm.sample]
    
    for (score in colnames(data.scores.new)[c(2,4:12)]){
      row.id.between <- which(!is_in(behaviour$ID, behaviour.outliers.id))
      score.now.between <- data.scores.new[[which(colnames(data.scores.new) == score)]][row.id.between]
      fit <- lm(score.now.between ~ perm.para.now.between * groupname.now.between)
      t.perm[paras, score] <- summary(fit)$coefficients["perm.para.now.between:groupname.now.betweenEscitalopram","t value"]
    }
  }
  # the first 4 summary scores
  perm.count[, c(1:4)] <- perm.count[, c(1:4)] + (max(abs(t.perm[, c(1:4)])) > abs(t[, c(1:4)]))
  # the remaining 6 subscales of OCI
  perm.count[, c(5:10)] <- perm.count[, c(5:10)] + (max(abs(t.perm[, c(5:10)])) > abs(t[, c(5:10)]))
  
}
perm.count / perm.total



p1 <- ggplot(
  dataplot, aes(x = side_stickiness_final, y = Ordering, facet = groupname, color = groupname)
) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) 
p2 <- ggplot(
  dataplot, aes(x = side_stickiness_final, y = Neutralizing, facet = groupname, color = groupname)
) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) 
pOCI <- ggarrange(p1, p2, nrow = 2) + theme(text=element_text(size=16,  family="Arial"))




#############################
# simulations
#############################


x <- stanfunc$annotated_parameters(fit2, par_exclude_regex = c("p_choose_stim1", "subject_effect"))

source('simulate_depletion_ssri.R')

# placebo
x.mean <- as.data.frame(t(x[c(1,3,5,7), mean])) 
colnames(x.mean) <- c('alpha_g', 'alpha_l', 'beta', 'kappa')
x.sd <- as.data.frame(t(x[c(1,3,5,7), sd]), nrow=1, ncol=4) 
colnames(x.sd) <- c('alpha_g', 'alpha_l', 'beta', 'kappa')
x.file<- 'test_ql_placeboV4.csv'  # perviously V3, for review comments, V4 2023.05.30
simulatewithstats(x.mean, x.sd, x.file)

# ssri
x.mean <- as.data.frame(t(x[c(2,4,6,8), mean]), nrow=1, ncol=4) 
colnames(x.mean) <- c('alpha_g', 'alpha_l', 'beta', 'kappa')
x.sd <- as.data.frame(t(x[c(2,4,6,8), sd]), nrow=1, ncol=4) 
colnames(x.sd) <- c('alpha_g', 'alpha_l', 'beta', 'kappa')
x.file<- 'test_ql_ssriV4.csv'
simulatewithstats(x.mean, x.sd, x.file)

# 2023.05.30
prob.chose.optimal.grp1 <- get(load('plac.RData'))
prob.chose.optimal.grp2 <- get(load('ssri.RData'))

df1 =data.frame( trial = trial_number, p_chose_correctly = prob.chose.optimal.grp1)
p1 = ggplot(data=df1, aes(x=trial, y=p_chose_correctly)) +
  geom_step()+
  geom_point()
df2 =data.frame( trial = trial_number, p_chose_correctly = prob.chose.optimal.grp2)
p2 = ggplot(data=df2, aes(x=trial, y=p_chose_correctly)) +
  geom_step()+
  geom_point()
ggarrange(p1, p2, nrow = 1) + theme(text=element_text(size=16,  family="Arial"))

###############



data0 <- read.csv("test_ql_placeboV3.csv", header = T)
data1 <- read.csv("test_ql_ssriV3.csv", header = T)



data0 <- cbind(data0,rep(1,80)) 
colnames(data0)[20] <- 'group'
data1 <- cbind(data1,rep(2,80)) 
colnames(data1)[20] <- 'group'
data <- rbind(data0, data1)

data$group <- as.factor(data$group)
levels(data$group) <- c("Placebo", "Escitalopram")

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
grp1 <- which(data$group=='Placebo')
grp2 <- which(data$group=='Escitalopram')
simcomp <- matrix(rep(0,19*2), nrow = 19, ncol = 2, dimnames = list(c(colnames(data)[c(1:19)]),
             c('wt', 'p.value')))
for (i in colnames(data)[c(1:19)]) {
     y <- wilcox.test(data[grp1,i], data[grp2,i])
     simcomp[i,"wt"] <- y$statistic
     simcomp[i,"p.value"] <- y$p.value
}

write.csv(simcomp, file = 'simbehresult.csv')

# Visualize: Specify the comparisons you want

p1c = ggboxplot(data, x = "group", y = "losses.ac",
          color = "group", add = "jitter")+ 
  stat_compare_means(label.y = 20)  + theme(legend.position = "none") + labs(title="Computational results")  # Add global p-value

p2c = ggboxplot(data, x = "group", y = "winStayRate.ac",
          color = "group", add = "jitter") + 
  stat_compare_means(label.y = 1)  + theme(legend.position = "none") + labs(title="   ") # Add global p-value

p3c = ggboxplot(data, x = "group", y = "loseShiftRate.ac",
          color = "group", add = "jitter") + 
  stat_compare_means(label.y = 1) + theme(legend.position = "none") + labs(title="   ")  # Add global p-value

# p123 <- ggarrange(p1, p2, p3, nrow = 1) + theme(text=element_text(size=16,  family="Arial"))
# ggsave('simbheresult.pdf', p123, width=4, height=4)
ggarrange(p1,p2,p3,p1c,p2c,p3c, nrow=2, ncol = 3) + theme(text=element_text(size=16,  family="Arial"))

wilcox.test(data$losses[which(data$group=='Placebo')], data$losses[which(data$group=='Escitalopram')])
wilcox.test(data$winStayRate[which(data$group=='Placebo')], data$winStayRate[which(data$group=='Escitalopram')])
wilcox.test(data$loseShiftRate[which(data$group=='Placebo')], data$loseShiftRate[which(data$group=='Escitalopram')])


