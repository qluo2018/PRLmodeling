# prepare Bari  data for stan
# created by Qiang Luo on 11 Sep 2019



library(xlsx)
library(stringr)

datafile <- "ProbLearning4Qiang.xlsx"

# read the ID and the group
group.info <- read.xlsx(datafile, sheetName = "Drug ID codes")
group.info <- group.info[c(1:33),c(2,3)]
group.info$Escitalopram <- droplevels(group.info$Escitalopram)
Escitalopram <- group.info$Escitalopram[1:32]
Escitalopram <- as.numeric(levels(Escitalopram))[Escitalopram]
Placebo <- group.info$Placebo
nEsci <- length(Escitalopram)
nPlac <- length(Placebo)
id.group  <- data.frame(c(Placebo,Escitalopram), c(rep(1,nPlac), rep(2,nEsci))) # Group (1 = placebo; 2 = SSRI) 
colnames(id.group)  <- c("ID", "group")
id.group <- id.group[order(id.group$ID),]

rm(group.info)
rm(Escitalopram)
rm(Placebo)

# read the data for each ID

nsubject <- dim(id.group)[1]
data.table <- list()
need.further.investigation <- rep(0, dim(id.group)[1])
t <- 1
for (i in id.group$ID){
  data.table[[i]] <- read.xlsx(datafile, sheetName = as.character(i), rowIndex=c(2:82), colIndex=c(1:10))
  if (dim(data.table[[i]])[1]<80) {
    t <- t + 1
    need.further.investigation[t] <- 1
  }
  #data.table <- droplevels.data.frame(data.subject[,])
  #indx <- sapply(data.table, is.factor)
  #data.table[indx] <- lapply(data.table[indx], function(x) as.numeric(levels(x))[x])
  #column.names <- droplevels.data.frame(data.subject[1,c(1:10)])
  #colnames(data.table) <- paste0(lapply(column.names, as.character))
  #rownames(data.table) <- c(1:80)
  gc() # flash memory
}


y <- data.frame()
t <- 1
for(i in id.group$ID){
  x <- data.table[[i]]
  n <- dim(x)[1]
  ID <- rep(i, n) 
  Group <- rep(id.group$group[t], n)
  trial_number <- x$trial
  StimChosen <- x$choice  #stimulus chosen 1/2
  MsGiven <- rep(0,n)  # whether this trial was rewarded 0/1
  if(colnames(x)[8] == "stimAfeed"){
    colnames(x)[8]  <- "stim1feed"
  }  
  MsGiven[which(x$choice==1)] <- as.numeric(x$stim1feed[which(x$choice==1)] == 1) 
  MsGiven[which(x$choice==2)] <- as.numeric(x$stim2feed[which(x$choice==2)] == 1)
  Opt  <- as.numeric(c(rep(1,n/2),rep(2,n/2)) == x$choice)  # whether the choice was optimal 0/1
  y <- rbind.data.frame(y, data.frame(ID=ID, Group=Group, trial_number = trial_number, MsGiven = MsGiven, Opt = Opt, StimChosen = StimChosen)) 
  t <- t + 1
}
sort.y <- y[order(y$ID, y$trial_number),]

difference <- matrix(rep(0,nsubject^2), nrow=nsubject, ncol= nsubject, dimnames = list(c(id.group$ID), c(id.group$ID)))
for (i in id.group$ID){
  for (j in id.group$ID){
    difference[paste0(i),paste0(j)] <- with(sort.y, sum(StimChosen[which(ID==i)] - StimChosen[which(ID==j)]))
  }
}

write.csv(sort.y, file  = "cleaned_ssri_nikolina.csv", row.names = FALSE)



