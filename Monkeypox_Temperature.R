options("scipen"=10000, "digits"=4)
library(tidyverse)
library(pscl)
setwd("E:/Desktop/monkeypox")
dir1 = "E:/Desktop/monkeypox/data"  
dir2 = "E:/Desktop/monkeypox/meteo_state" 
file_list1 = list.files(path = dir1, pattern = "*.xlsx$",recursive = TRUE,full.names = TRUE)
file_list2 = list.files(path = dir2, pattern = "*.xlsx$",recursive = TRUE,full.names = TRUE)  
df1 <- list()
df2 <- list()
pop <- read.csv('states_population.csv')
b <- readxl::read_xlsx("dow.xlsx")
b$date <- as.Date(b$date)
for (i in 1:length(file_list1)) {
  df1[[i]] = readxl::read_xlsx(file_list1[i]) 
  df1[[i]] <- df1[[i]][,c("location", "cases", "asof", "state_ap", "state", "X", "case_increase","date")]
  df1[[i]]$date <- as.Date(df1[[i]]$date)
  df1[[i]] <- merge(df1[[i]],b,by = "date")
  df1[[i]]$state <- df1[[i]]$state/1000000
  df1[[i]]$case_increase <- as.integer((df1[[i]]$case_increase/df1[[i]]$state)*100)
  df1[[i]]$cases <- (df1[[i]]$cases/df1[[i]]$state)
}
for (i in 1:length(file_list2)) {
  df2[[i]] = readxl::read_xlsx(file_list2[i]) 
  df2[[i]]$dtr <- df2[[i]]$temp_max-df2[[i]]$temp_min
  df2[[i]]$date <- as.Date(df2[[i]]$date)
  df2[[i]] <- df2[[i]][order(df2[[i]]$date),]
}
single <- list()
ls <- list()
# lag
D <- df1[[1]]$date
for (m in 1:52) {
  for (n in 1:15){
    df1[[m]]$date <- D-n+1
    ls[[n]] <- merge(df1[[m]],df2[[m]],by = "date",all.x = TRUE)
    ls[[n]]$date <- D
  }
  single[[m]] <- ls
}
moving <- list()
# moving average
ls2 <- list()
ls3 <- list()
for (m in 1:52) {
  for (n in 1:15) {
    ls3[[n]] <- single[[m]][[n]][,12:18] 
  }
  sum <- ls3[[1]]
  for (n in 1:14) {
    sum <- sum + ls3[[n+1]]  
    ls2[[n]] <- cbind(single[[m]][[1]][,1:11],(sum/(n+1)))
  } 
  moving[[m]] <- ls2
}
all <- list()
for (n in 1:52) {
  all[[n]] <- single[[n]]
  for (m in 1:14) {
    all[[n]][[15+m]] <- moving[[n]][[m]]
  }
}
# # spearman ----------------------------------------------------------------
# # install.packages("Hmisc")
# library(Hmisc)
# dput(names(all[[1]][[10]]))
# a <- as.matrix(all[[1]][[10]][c("case_increase","dow", "holiday", "day", "dewp", "prcp", "temp",
#                                 "temp_max", "temp_min", "wdsp", "dtr")])
# data.spearman<-rcorr(a, type ="spearman")
# spear <- as.data.frame(rbind(data.spearman[[1]],data.spearman[[3]]))
# writexl::write_xlsx(spear,"data.spearman.xlsx")
# new all --------------------------------------------------------------------
for (n in 1:52) {
  for (m in 1:29) {
    all[[n]][[m]]$prcp <- all[[n]][[29]]$prcp
    all[[n]][[m]]$wdsp <- all[[n]][[29]]$wdsp
  }
}
# best-lag-day filter-------------------------------------------------
###########################【temp】############################################
res.temp <- list()
for (n in 1:52) {
  res.temp[[n]] <- data.frame()
}
for (j in 1:52) {
  for (i in 1:29) {
    res.temp_zinb <- zeroinfl(case_increase ~ temp+prcp+wdsp|dow+holiday,
                              data = all[[j]][[i]],
                              dist = "negbin")
    res.temp[[j]][i,1] <- BIC(res.temp_zinb)
    res.temp[[j]][i,2] <- i
    res.temp[[j]][i,3] <- all[[j]][[i]]$location[1]
    res.temp[[j]][i,4:7] <- summary(res.temp_zinb)$coefficients$count[2,]
    names(res.temp[[j]]) <- c("BIC","lagday","location","Estimate","Std.Error","z.value","P")
  }
}
state.best.lag.temp <- data.frame()
for (i in 1:52) {
  state.best.lag.temp[i,1] <- res.temp[[i]]$location[1]
  j <- which(res.temp[[i]]$BIC==min(res.temp[[i]]$BIC))
  state.best.lag.temp[i,2] <- j
  state.best.lag.temp[i,3:4] <- cbind(res.temp[[i]]$Estimate[j],res.temp[[i]]$P[j])
  state.best.lag.temp[i,5] <- res.temp[[i]]$BIC[j]
  names(state.best.lag.temp) <- c("location","lagday","Estimate","P","BIC")
}
writexl::write_xlsx(state.best.lag.temp,"state.best.lag.temp.xlsx")
###########################【temp_min】############################################
res.temp_min <- list()
for (n in 1:52) {
  res.temp_min[[n]] <- data.frame()
}
for (j in 1:52) {
  for (i in 1:29) {
    res.temp_min_zinb <- zeroinfl(case_increase ~ temp_min+prcp+wdsp|dow+holiday,
                                  data = all[[j]][[i]], 
                                  dist = "negbin")
    res.temp_min[[j]][i,1] <- BIC(res.temp_min_zinb)
    res.temp_min[[j]][i,2] <- i
    res.temp_min[[j]][i,3] <- all[[j]][[i]]$location[1]
    res.temp_min[[j]][i,4:7] <- summary(res.temp_min_zinb)$coefficients$count[2,]
    names(res.temp_min[[j]]) <- c("BIC","lagday","location","Estimate","Std.Error","z.value","P")
  }
}
state.best.lag.temp_min <- data.frame()
for (i in 1:52) {
  state.best.lag.temp_min[i,1] <- res.temp_min[[i]]$location[1]
  j <- which(res.temp_min[[i]]$BIC==min(res.temp_min[[i]]$BIC))
  state.best.lag.temp_min[i,2] <- j
  state.best.lag.temp_min[i,3:4] <- cbind(res.temp_min[[i]]$Estimate[j],res.temp_min[[i]]$P[j])
  state.best.lag.temp_min[i,5] <- res.temp_min[[i]]$BIC[j]
  names(state.best.lag.temp_min) <- c("location","lagday","Estimate","P","BIC")
}
writexl::write_xlsx(state.best.lag.temp_min,"state.best.lag.temp_min.xlsx")
###########################【temp_max】############################################
res.temp_max <- list()
for (n in 1:52) {
  res.temp_max[[n]] <- data.frame()
}
for (j in 1:52) {
  for (i in 1:29) {
    res.temp_max_zinb <- zeroinfl(case_increase ~ temp_max+prcp+wdsp|dow+holiday,
                                  data = all[[j]][[i]],
                                  dist = "negbin")
    res.temp_max[[j]][i,1] <- BIC(res.temp_max_zinb)
    res.temp_max[[j]][i,2] <- i
    res.temp_max[[j]][i,3] <- all[[j]][[i]]$location[1]
    res.temp_max[[j]][i,4:7] <- summary(res.temp_max_zinb)$coefficients$count[2,]
    names(res.temp_max[[j]]) <- c("BIC","lagday","location","Estimate","Std.Error","z.value","P")
  }
}
state.best.lag.temp_max <- data.frame()
for (i in 1:52) {
  state.best.lag.temp_max[i,1] <- res.temp_max[[i]]$location[1]
  j <- which(res.temp_max[[i]]$BIC==min(res.temp_max[[i]]$BIC))
  state.best.lag.temp_max[i,2] <- j
  state.best.lag.temp_max[i,3:4] <- cbind(res.temp_max[[i]]$Estimate[j],res.temp_max[[i]]$P[j])
  state.best.lag.temp_max[i,5] <- res.temp_max[[i]]$BIC[j]
  names(state.best.lag.temp_max) <- c("location","lagday","Estimate","P","BIC")
}
writexl::write_xlsx(state.best.lag.temp_max,"state.best.lag.temp_max.xlsx")
###########################【dewp】############################################
res.dewp <- list()
for (n in 1:52) {
  res.dewp[[n]] <- data.frame()
}
for (j in 1:52) {
  for (i in 1:29) {
    res.dewp_zinb <- zeroinfl(case_increase ~ dewp+prcp+wdsp|dow+holiday,
                              data = all[[j]][[i]],
                              dist = "negbin")
    res.dewp[[j]][i,1] <- BIC(res.dewp_zinb)
    res.dewp[[j]][i,2] <- i
    res.dewp[[j]][i,3] <- all[[j]][[i]]$location[1]
    res.dewp[[j]][i,4:7] <- summary(res.dewp_zinb)$coefficients$count[2,]
    names(res.dewp[[j]]) <- c("BIC","lagday","location","Estimate","Std.Error","z.value","P")
  }
}
state.best.lag.dewp <- data.frame()
for (i in 1:52) {
  state.best.lag.dewp[i,1] <- res.dewp[[i]]$location[1]
  j <- which(res.dewp[[i]]$BIC==min(res.dewp[[i]]$BIC))
  state.best.lag.dewp[i,2] <- j
  state.best.lag.dewp[i,3:4] <- cbind(res.dewp[[i]]$Estimate[j],res.dewp[[i]]$P[j])
  state.best.lag.dewp[i,5] <- res.dewp[[i]]$BIC[j]
  names(state.best.lag.dewp) <- c("location","lagday","Estimate","P","BIC")
}
writexl::write_xlsx(state.best.lag.dewp,"state.best.lag.dewp.xlsx")
###########################【dtr】############################################
res.dtr <- list()
for (n in 1:52) {
  res.dtr[[n]] <- data.frame()
}
for (j in 1:52) {
  for (i in 1:29) {
    res.dtr_zinb <- zeroinfl(case_increase ~ dtr+prcp+wdsp|dow+holiday,
                             data = all[[j]][[i]],
                             dist = "negbin")
    res.dtr[[j]][i,1] <- BIC(res.dtr_zinb)
    res.dtr[[j]][i,2] <- i
    res.dtr[[j]][i,3] <- all[[j]][[i]]$location[1]
    res.dtr[[j]][i,4:7] <- summary(res.dtr_zinb)$coefficients$count[2,]
    names(res.dtr[[j]]) <- c("BIC","lagday","location","Estimate","Std.Error","z.value","P")
  }
}
state.best.lag.dtr <- data.frame()
for (i in 1:52) {
  state.best.lag.dtr[i,1] <- res.dtr[[i]]$location[1]
  j <- which(res.dtr[[i]]$BIC==min(res.dtr[[i]]$BIC))
  state.best.lag.dtr[i,2] <- j
  state.best.lag.dtr[i,3:4] <- cbind(res.dtr[[i]]$Estimate[j],res.dtr[[i]]$P[j])
  state.best.lag.dtr[i,5] <- res.dtr[[i]]$BIC[j]
  names(state.best.lag.dtr) <- c("location","lagday","Estimate","P","BIC")
}
writexl::write_xlsx(state.best.lag.dtr,"state.best.lag.dtr.xlsx")
########################################################################################
# Final data ----------------------------------------------------------------
# best temp ---------------------------------------------------------------------
final <- list()
for (n in 1:52) {
  final[[n]] <- data.frame()
  final[[n]] <- all[[n]][[1]][1:11]
  final[[n]]$state <- pop[n,2]
  final[[n]]$prcp <- all[[n]][[29]]$prcp
  final[[n]]$wdsp <- all[[n]][[29]]$wdsp 
  final[[n]]$temp <- all[[n]][[state.best.lag.temp$lagday[n]]]$temp
  final[[n]]$temp_min <- all[[n]][[state.best.lag.temp_min$lagday[n]]]$temp_min
  final[[n]]$temp_max <- all[[n]][[state.best.lag.temp_max$lagday[n]]]$temp_max
  final[[n]]$dewp <- all[[n]][[state.best.lag.dewp$lagday[n]]]$dewp
  final[[n]]$dtr <- all[[n]][[state.best.lag.dtr$lagday[n]]]$dtr
}
# fix_lag0----------------------------------------------------------------
final <- list()
for (n in 1:52) {
  final[[n]] <- data.frame()
  final[[n]] <- all[[n]][[1]][1:11]
  final[[n]]$state <- pop[n,2]
  final[[n]]$prcp <- all[[n]][[29]]$prcp
  final[[n]]$wdsp <- all[[n]][[29]]$wdsp 
  final[[n]]$temp <- all[[n]][[1]]$temp
  final[[n]]$temp_min <- all[[n]][[1]]$temp_min
  final[[n]]$temp_max <- all[[n]][[1]]$temp_max
  final[[n]]$dewp <- all[[n]][[1]]$dewp
  final[[n]]$dtr <- all[[n]][[1]]$dtr
}
# fix_lag03----------------------------------------------------------------
final <- list()
for (n in 1:52) {
  final[[n]] <- data.frame()
  final[[n]] <- all[[n]][[1]][1:11]
  final[[n]]$state <- pop[n,2]
  final[[n]]$prcp <- all[[n]][[29]]$prcp
  final[[n]]$wdsp <- all[[n]][[29]]$wdsp 
  final[[n]]$temp <- all[[n]][[18]]$temp
  final[[n]]$temp_min <- all[[n]][[18]]$temp_min
  final[[n]]$temp_max <- all[[n]][[18]]$temp_max
  final[[n]]$dewp <- all[[n]][[18]]$dewp
  final[[n]]$dtr <- all[[n]][[18]]$dtr
}
# fix_lag07----------------------------------------------------------------
final <- list()
for (n in 1:52) {
  final[[n]] <- data.frame()
  final[[n]] <- all[[n]][[1]][1:11]
  final[[n]]$state <- pop[n,2]
  final[[n]]$prcp <- all[[n]][[29]]$prcp
  final[[n]]$wdsp <- all[[n]][[29]]$wdsp 
  final[[n]]$temp <- all[[n]][[22]]$temp
  final[[n]]$temp_min <- all[[n]][[22]]$temp_min
  final[[n]]$temp_max <- all[[n]][[22]]$temp_max
  final[[n]]$dewp <- all[[n]][[22]]$dewp
  final[[n]]$dtr <- all[[n]][[22]]$dtr
}
# fix_lag014----------------------------------------------------------------
final <- list()
for (n in 1:52) {
  final[[n]] <- data.frame()
  final[[n]] <- all[[n]][[1]][1:11]
  final[[n]]$state <- pop[n,2]
  final[[n]]$prcp <- all[[n]][[29]]$prcp
  final[[n]]$wdsp <- all[[n]][[29]]$wdsp 
  final[[n]]$temp <- all[[n]][[29]]$temp
  final[[n]]$temp_min <- all[[n]][[29]]$temp_min
  final[[n]]$temp_max <- all[[n]][[29]]$temp_max
  final[[n]]$dewp <- all[[n]][[29]]$dewp
  final[[n]]$dtr <- all[[n]][[29]]$dtr
}
# read geographic area data --------------------------------------------------------------
direct <- readxl::read_xlsx("direction.xlsx")
# ZINB --------------------------------------------------------------
###############【temp】################
last_temp <- data.frame()
res_temp <- list()
for (n in 1:52) {
  res_temp[[n]] <- zeroinfl(case_increase ~ temp+prcp+wdsp|dow+holiday,
                            data = final[[n]], 
                            dist = "negbin")
  last_temp[n,1] <- final[[n]]$location[1]#州名-1列
  last_temp[n,2] <- rownames(summary(res_temp[[n]])$coefficients$count)[2]#变量名-1列
  last_temp[n,3:6] <- summary(res_temp[[n]])$coefficients$count[2,]#估计值-4列
  last_temp[n,7:8] <- confint(res_temp[[n]])[2,]#置信区间-2列
  last_temp[n,9] <- pop[n,2]#样本量,州总人口-1列
  last_temp[n,10] <- final[[n]][nrow(final[[n]]),which(names(final[[n]])=="cases")]#州总病例数-1列
}
names(last_temp) <- c("location","variable","Estimate","Std.Error","z","P","2.5%","97.5%","population","cases")
last_temp <- merge(last_temp,direct,by="location")
last_temp <- filter(last_temp,cases>=10)
writexl::write_xlsx(last_temp,"last_temp.xlsx")
###############【temp_min】################
last_temp_min <- data.frame()
res_temp_min <- list()
for (n in 1:52) {
  res_temp_min[[n]] <- zeroinfl(case_increase ~ temp_min+prcp+wdsp|dow+holiday,
                                data = final[[n]], 
                                dist = "negbin")
  last_temp_min[n,1] <- final[[n]]$location[1]#州名-1列
  last_temp_min[n,2] <- rownames(summary(res_temp_min[[n]])$coefficients$count)[2]#变量名-1列
  last_temp_min[n,3:6] <- summary(res_temp_min[[n]])$coefficients$count[2,]#估计值-4列
  last_temp_min[n,7:8] <- confint(res_temp_min[[n]])[2,]#置信区间-2列
  last_temp_min[n,9] <- pop[n,2]#样本量,州总人口-1列
  last_temp_min[n,10] <- final[[n]][nrow(final[[n]]),which(names(final[[n]])=="cases")]#州总病例数-1列
}
names(last_temp_min) <- c("location","variable","Estimate","Std.Error","z","P","2.5%","97.5%","population","cases")
last_temp_min <- merge(last_temp_min,direct,by="location")
last_temp_min <- filter(last_temp_min,cases>=10)
# writexl::write_xlsx(last_temp_min,"last_temp_min.xlsx")
###############【temp_max】################
last_temp_max <- data.frame()
res_temp_max <- list()
for (n in 1:52) {
  res_temp_max[[n]] <- zeroinfl(case_increase ~ temp_max+prcp+wdsp|dow+holiday,
                                data = final[[n]], 
                                dist = "negbin")
  last_temp_max[n,1] <- final[[n]]$location[1]#州名-1列
  last_temp_max[n,2] <- rownames(summary(res_temp_max[[n]])$coefficients$count)[2]#变量名-1列
  last_temp_max[n,3:6] <- summary(res_temp_max[[n]])$coefficients$count[2,]#估计值-4列
  last_temp_max[n,7:8] <- confint(res_temp_max[[n]])[2,]#置信区间-2列
  last_temp_max[n,9] <- pop[n,2]#样本量,州总人口-1列
  last_temp_max[n,10] <- final[[n]][nrow(final[[n]]),which(names(final[[n]])=="cases")]#州总病例数-1列
}
names(last_temp_max) <- c("location","variable","Estimate","Std.Error","z","P","2.5%","97.5%","population","cases")
last_temp_max <- merge(last_temp_max,direct,by="location")
last_temp_max <- filter(last_temp_max,cases>=10)
# writexl::write_xlsx(last_temp_max,"last_temp_max.xlsx")
###############【dewp】################
last_dewp <- data.frame()
res_dewp <- list()
for (n in 1:52) {
  res_dewp[[n]] <- zeroinfl(case_increase ~ dewp+prcp+wdsp|dow+holiday,
                            data = final[[n]], 
                            dist = "negbin")
  last_dewp[n,1] <- final[[n]]$location[1]#州名-1列
  last_dewp[n,2] <- rownames(summary(res_dewp[[n]])$coefficients$count)[2]#变量名-1列
  last_dewp[n,3:6] <- summary(res_dewp[[n]])$coefficients$count[2,]#估计值-4列
  last_dewp[n,7:8] <- confint(res_dewp[[n]])[2,]#置信区间-2列
  last_dewp[n,9] <- pop[n,2]#样本量,州总人口-1列
  last_dewp[n,10] <- final[[n]][nrow(final[[n]]),which(names(final[[n]])=="cases")]#州总病例数-1列
}
names(last_dewp) <- c("location","variable","Estimate","Std.Error","z","P","2.5%","97.5%","population","cases")
last_dewp <- merge(last_dewp,direct,by="location")
last_dewp <- filter(last_dewp,cases>=10)
# writexl::write_xlsx(last_dewp,"last_dewp.xlsx")
###############【dtr】################
last_dtr <- data.frame()
res_dtr <- list()
for (n in 1:52) {
  res_dtr[[n]] <- zeroinfl(case_increase ~ dtr+prcp+wdsp|dow+holiday,
                           data = final[[n]], 
                           dist = "negbin")
  last_dtr[n,1] <- final[[n]]$location[1]#州名-1列
  last_dtr[n,2] <- rownames(summary(res_dtr[[n]])$coefficients$count)[2]#变量名-1列
  last_dtr[n,3:6] <- summary(res_dtr[[n]])$coefficients$count[2,]#估计值-4列
  last_dtr[n,7:8] <- confint(res_dtr[[n]])[2,]#置信区间-2列
  last_dtr[n,9] <- pop[n,2]#样本量,州总人口-1列
  last_dtr[n,10] <- final[[n]][nrow(final[[n]]),which(names(final[[n]])=="cases")]#州总病例数-1列
}
names(last_dtr) <- c("location","variable","Estimate","Std.Error","z","P","2.5%","97.5%","population","cases")
last_dtr <- merge(last_dtr,direct,by="location")
last_dtr <- filter(last_dtr,cases>=10)
# writexl::write_xlsx(last_dtr,"last_dtr.xlsx")
# Forestplot --------------------------------------------------------------------
# install.packages("bitops")
# install.packages("metafor")
# install.packages("Formula")
library(bitops)
library(metafor)
library(Formula)
###############【temp】################
EffectSize=last_temp$Estimate
se=last_temp$Std.Error
slab=last_temp$location
#Run ramdom effects model
my_model <- rma.uni(measure="RR", yi=EffectSize, sei=se, slab=slab)
pdf("temp.pdf",width=8,height=10)
forest(my_model, 
       xlab="Risk Ratio (log scale)", 
       header="state", 
       xlim = c(-4, 5), 
       at = log(c(0.25, 1,2,4)),
       atransf=exp,
       showweights = T,
       fontsize=8)
dev.off()
summary(my_model)

###############【temp_min】################
EffectSize=last_temp_min$Estimate
se=last_temp_min$Std.Error
slab=last_temp_min$location
#Run ramdom effects model
my_model <- rma.uni(measure="RR", yi=EffectSize, sei=se, slab=slab)
#my_model <- rma.uni(yi=yi, sei=sei, data = effects)
pdf("temp_min.pdf",width=8,height=10)
forest(my_model, 
       xlab="Risk Ratio (log scale)", 
       header="state", 
       xlim = c(-4, 6), 
       at = log(c(0.25, 1,2,4)),
       atransf=exp,
       showweights = T,
       fontsize=8)
dev.off()
summary(my_model)

###############【temp_max】################
EffectSize=last_temp_max$Estimate
se=last_temp_max$Std.Error
slab=last_temp_max$location
#Run ramdom effects model
my_model <- rma.uni(measure="RR", yi=EffectSize, sei=se, slab=slab)
#my_model <- rma.uni(yi=yi, sei=sei, data = effects)
pdf("temp_max.pdf",width=8,height=10)
forest(my_model, 
       xlab="Risk Ratio (log scale)", 
       header="state", 
       xlim = c(-4, 6), 
       at = log(c(0.25, 1,2,4)),
       atransf=exp,
       showweights = T,
       fontsize=8)
dev.off()
summary(my_model)

###############【dewp】################
EffectSize=last_dewp$Estimate
se=last_dewp$Std.Error
slab=last_dewp$location
#Run ramdom effects model
my_model <- rma.uni(measure="RR", yi=EffectSize, sei=se, slab=slab)
#my_model <- rma.uni(yi=yi, sei=sei, data = effects)
pdf("dewp.pdf",width=8,height=10)
forest(my_model,
       xlab="Risk Ratio (log scale)", 
       header="state", 
       xlim = c(-4, 6), 
       at = log(c(0.25, 1,2,4)),
       atransf=exp,
       showweights = T,
       fontsize=8)
dev.off()
summary(my_model)

###############【dtr】################
EffectSize=last_dtr$Estimate
se=last_dtr$Std.Error
slab=last_dtr$location
#Run ramdom effects model
my_model <- rma.uni(measure="RR", yi=EffectSize, sei=se, slab=slab)
#my_model <- rma.uni(yi=yi, sei=sei, data = effects)
pdf("dtr.pdf",width=8,height=10)
forest(my_model,
       xlab="Risk Ratio (log scale)", 
       header="state", 
       xlim = c(-4, 6), 
       at = log(c(0.25, 1,2,4)),
       atransf=exp,
       showweights = T,
       fontsize=8)
dev.off()
summary(my_model)

# errorbar plot -------------------------------------------------------------------
lags <- readxl::read_xlsx("lag.xlsx",sheet = 1)
lags$lag <- factor(lags$lag,levels=c("lag0","lag03","lag07","lag014"))
lags$slab <- factor(lags$slab,levels=c("dtr","dewp","temp","temp_max","temp_min"))

dodge <- position_dodge(width = 0.5)

p<-ggplot(data=lags, mapping=aes(x=lag, y=EffectSize,color=slab, group=slab,ymin=low,ymax=up)) +
  geom_line(aes(color=slab),linetype="dashed",position = dodge,size=1,alpha=0.4)+
  geom_point(aes(color=slab),position = dodge,size=3)+
  geom_errorbar(position = dodge,width=0.5,size=1)+
  geom_hline(aes(yintercept = 1,color="black"),lty=2,size=1)+
  scale_color_manual(values=c(dtr = "#C497B2",dewp = "#0c8e92",  temp = "#3a8cd2" , temp_max = "#f47876", temp_min = "#faca43"))+
  labs(x="Lag",y = "Risk Ratio")+
  theme_classic() +
  theme(legend.position="top",
        legend.title=element_blank(),
        legend.text = element_text(size = 12,face = "bold"),
        legend.key.size = unit(2,"line"),
        axis.line=element_line(linetype=1,color="black",size=1.2),
        axis.title.x=element_text(size=15,face = "bold"),
        axis.title.y=element_text(size=15,face = "bold"),
        axis.text.x=element_text(size = 12,face = "bold"),
        axis.text.y=element_text(size = 12,face = "bold"))

p

ggsave("合并RR-lag.pdf",plot = p,width = 10,height = 6,dpi = 500)


# ################supplement wind+prcp############################# -----------------------------------------------------------
# best-lag-day -------------------------------------------------
###########################【prcp】############################################
res.prcp <- list()
for (n in 1:52) {
  res.prcp[[n]] <- data.frame()
}
for (j in 1:52) {
  for (i in 1:29) {
    res.prcp_zinb <- zeroinfl(case_increase ~ prcp+dewp+wdsp|dow+holiday,
                              data = all[[j]][[i]],
                              dist = "negbin")
    res.prcp[[j]][i,1] <- BIC(res.prcp_zinb)
    res.prcp[[j]][i,2] <- i
    res.prcp[[j]][i,3] <- all[[j]][[i]]$location[1]
    res.prcp[[j]][i,4:7] <- summary(res.prcp_zinb)$coefficients$count[2,]#估计值-4列
    names(res.prcp[[j]]) <- c("BIC","lagday","location","Estimate","Std.Error","z.value","P")
  }
}
state.best.lag.prcp <- data.frame()
for (i in 1:52) {
  state.best.lag.prcp[i,1] <- res.prcp[[i]]$location[1]
  j <- which(res.prcp[[i]]$BIC==min(res.prcp[[i]]$BIC))
  state.best.lag.prcp[i,2] <- j
  state.best.lag.prcp[i,3:4] <- cbind(res.prcp[[i]]$Estimate[j],res.prcp[[i]]$P[j])
  state.best.lag.prcp[i,5] <- res.prcp[[i]]$BIC[j]
  names(state.best.lag.prcp) <- c("location","lagday","Estimate","P","BIC")
}
writexl::write_xlsx(state.best.lag.prcp,"state.best.lag.prcp.xlsx")
###########################【wdsp】############################################
res.wdsp <- list()
for (n in 1:52) {
  res.wdsp[[n]] <- data.frame()
}
for (j in 1:52) {
  for (i in 1:29) {
    res.wdsp_zinb <- zeroinfl(case_increase ~ wdsp+temp|dow+holiday,
                              data = all[[j]][[i]],
                              dist = "negbin")
    res.wdsp[[j]][i,1] <- BIC(res.wdsp_zinb)
    res.wdsp[[j]][i,2] <- i
    res.wdsp[[j]][i,3] <- all[[j]][[i]]$location[1]
    res.wdsp[[j]][i,4:7] <- summary(res.wdsp_zinb)$coefficients$count[2,]#估计值-4列
    names(res.wdsp[[j]]) <- c("BIC","lagday","location","Estimate","Std.Error","z.value","P")
  }
}
state.best.lag.wdsp <- data.frame()
for (i in 1:52) {
  state.best.lag.wdsp[i,1] <- res.wdsp[[i]]$location[1]
  j <- which(res.wdsp[[i]]$BIC==min(res.wdsp[[i]]$BIC))
  state.best.lag.wdsp[i,2] <- j
  state.best.lag.wdsp[i,3:4] <- cbind(res.wdsp[[i]]$Estimate[j],res.wdsp[[i]]$P[j])
  state.best.lag.wdsp[i,5] <- res.wdsp[[i]]$BIC[j]
  names(state.best.lag.wdsp) <- c("location","lagday","Estimate","P","BIC")
}
# writexl::write_xlsx(state.best.lag.wdsp,"state.best.lag.wdsp.xlsx")

# best_lag----------------------------------------------------------------
final <- list()
for (n in 1:52) {
  final[[n]] <- data.frame()
  final[[n]] <- all[[n]][[1]][1:11]
  final[[n]]$state <- pop[n,2]
  final[[n]]$prcp <- all[[n]][[state.best.lag.prcp$lagday[n]]]$prcp
  final[[n]]$wdsp <- all[[n]][[state.best.lag.wdsp$lagday[n]]]$wdsp
}
direct <- readxl::read_xlsx("direction.xlsx")
# ZINB --------------------------------------------------------------
###############【prcp】################
last_prcp <- data.frame()
res_prcp <- list()
for (n in 1:52) {
  res_prcp[[n]] <- zeroinfl(case_increase ~ prcp|dow+holiday,
                     data = final[[n]],
                     dist = "negbin")
  last_prcp[n,1] <- final[[n]]$location[1]#州名-1列
  last_prcp[n,2] <- rownames(summary(res_prcp[[n]])$coefficients$count)[2]#变量名-1列
  last_prcp[n,3:6] <- summary(res_prcp[[n]])$coefficients$count[2,]#估计值-4列
  last_prcp[n,7:8] <- confint(res_prcp[[n]])[2,]#置信区间-2列
  last_prcp[n,9] <- pop[n,2]#样本量,州总人口-1列
  last_prcp[n,10] <- final[[n]][nrow(final[[n]]),which(names(final[[n]])=="cases")]#州总病例数-1列
    }
names(last_prcp) <- c("location","variable","Estimate","Std.Error","z","P","2.5%","97.5%","population","cases")
last_prcp <- merge(last_prcp,direct,by="location")
last_prcp <- filter(last_prcp,cases>=10)
writexl::write_xlsx(last_prcp,"last_prcp.xlsx")
###############【wdsp】################
last_wdsp <- data.frame()
res_wdsp <- list()
for (n in 1:52) {
  res_wdsp[[n]] <- zeroinfl(case_increase ~ wdsp|dow+holiday,
                            data = final[[n]],
                            dist = "negbin")
  last_wdsp[n,1] <- final[[n]]$location[1]#州名-1列
  last_wdsp[n,2] <- rownames(summary(res_wdsp[[n]])$coefficients$count)[2]#变量名-1列
  last_wdsp[n,3:6] <- summary(res_wdsp[[n]])$coefficients$count[2,]#估计值-4列
  last_wdsp[n,7:8] <- confint(res_wdsp[[n]])[2,]#置信区间-2列
  last_wdsp[n,9] <- pop[n,2]#样本量,州总人口-1列
  last_wdsp[n,10] <- final[[n]][nrow(final[[n]]),which(names(final[[n]])=="cases")]#州总病例数-1列
}
names(last_wdsp) <- c("location","variable","Estimate","Std.Error","z","P","2.5%","97.5%","population","cases")
last_wdsp <- merge(last_wdsp,direct,by="location")
last_wdsp <- filter(last_wdsp,cases>=10)
writexl::write_xlsx(last_wdsp,"last_wdsp.xlsx")

# Forestplot --------------------------------------------------------------------
# install.packages("bitops")
# install.packages("metafor")
# install.packages("Formula")
library(bitops)
library(metafor)
library(Formula)  
###############【prcp】################
EffectSize=last_prcp$Estimate
se=last_prcp$Std.Error
slab=last_prcp$location

#Run ramdom effects model
my_model <- rma.uni(measure="RR", yi=EffectSize, sei=se, slab=slab)
#my_model <- rma.uni(yi=yi, sei=sei, data = effects)
pdf("prcp.pdf",width=10,height=12)
forest(my_model, xlab="New infection Rate Ratio", header="state", atransf=exp,
       showweights = T,fontsize=8)
dev.off()
###############【wdsp】################
EffectSize=last_wdsp$Estimate
se=last_wdsp$Std.Error
slab=last_wdsp$location

#Run ramdom effects model
my_model <- rma.uni(measure="RR", yi=EffectSize, sei=se, slab=slab)
#my_model <- rma.uni(yi=yi, sei=sei, data = effects)
pdf("wdsp.pdf",width=10,height=12)
forest(my_model, xlab="New infection Rate Ratio", header="state", atransf=exp,
       showweights = T,fontsize=8)
dev.off()




# matching result -----------------------------------------------------------
aaa <- readxl::read_xlsx("state.best.lag.temp.xlsx")
bbb <- readxl::read_xlsx("last_temp.xlsx")
ccc <- merge(aaa,bbb,by = "location",all.y = TRUE)
writexl::write_xlsx(ccc,"all_temp.xlsx")

aaa <- readxl::read_xlsx("state.best.lag.temp_min.xlsx")
bbb <- readxl::read_xlsx("last_temp_min.xlsx")
ccc <- merge(aaa,bbb,by = "location",all.y = TRUE)
writexl::write_xlsx(ccc,"all_temp_min.xlsx")

aaa <- readxl::read_xlsx("state.best.lag.temp_max.xlsx")
bbb <- readxl::read_xlsx("last_temp_max.xlsx")
ccc <- merge(aaa,bbb,by = "location",all.y = TRUE)
writexl::write_xlsx(ccc,"all_temp_max.xlsx")

aaa <- readxl::read_xlsx("state.best.lag.dewp.xlsx")
bbb <- readxl::read_xlsx("last_dewp.xlsx")
ccc <- merge(aaa,bbb,by = "location",all.y = TRUE)
writexl::write_xlsx(ccc,"all_dewp.xlsx")

aaa <- readxl::read_xlsx("state.best.lag.dtr.xlsx")
bbb <- readxl::read_xlsx("last_dtr.xlsx")
ccc <- merge(aaa,bbb,by = "location",all.y = TRUE)
writexl::write_xlsx(ccc,"all_dtr.xlsx")










# sensitive Analysis.TEMP.location --------------------------------------------------------------------
# install.packages("bitops")
# install.packages("metafor")
# install.packages("Formula")
library(bitops)
library(metafor)
library(Formula)
data <- readxl::read_xlsx("all_temp.xlsx")
G <- readxl::read_xlsx("GDP_rank.xlsx")
M <- merge(data,G,by = "location")

test <- M %>% filter(direction=="midwest")
EffectSize=test$Estimate
se=test$Std.Error
slab=test$location
#Run ramdom effects model
my_model <- rma.uni(measure="RR", yi=EffectSize, sei=se, slab=slab)
pdf("midwest.pdf",width=6,height=4)
forest(my_model, 
       xlab="Risk Ratio (log scale)", 
       header="State", 
       xlim = c(-2.5, 4), 
       at = log(c(0.25, 1,2,4)),
       atransf=exp,
       showweights = T,
       fontsize=6)
dev.off()
summary(my_model)

test <- M %>% filter(direction=="south")
EffectSize=test$Estimate
se=test$Std.Error
slab=test$location
#Run ramdom effects model
my_model <- rma.uni(measure="RR", yi=EffectSize, sei=se, slab=slab)
pdf("south.pdf",width=6,height=6)
forest(my_model, 
       xlab="Risk Ratio (log scale)", 
       header="State", 
       xlim = c(-2.5, 4), 
       at = log(c(0.25, 1,2,4)),
       atransf=exp,
       showweights = T,
       fontsize=6)
dev.off()
summary(my_model)

test <- M %>% filter(direction=="west")
EffectSize=test$Estimate
se=test$Std.Error
slab=test$location
#Run ramdom effects model
my_model <- rma.uni(measure="RR", yi=EffectSize, sei=se, slab=slab)
pdf("west.pdf",width=6,height=4)
forest(my_model, 
       xlab="Risk Ratio (log scale)", 
       header="State", 
       xlim = c(-2.5, 4), 
       at = log(c(0.25, 1,2,4)),
       atransf=exp,
       showweights = T,
       fontsize=6)
dev.off()
summary(my_model)

test <- M %>% filter(direction=="northeast")
EffectSize=test$Estimate
se=test$Std.Error
slab=test$location
#Run ramdom effects model
my_model <- rma.uni(measure="RR", yi=EffectSize, sei=se, slab=slab)
pdf("northeast.pdf",width=6,height=4)
forest(my_model, 
       xlab="Risk Ratio (log scale)", 
       header="State", 
       xlim = c(-2.5, 4), 
       at = log(c(0.25, 1,2,4)),
       atransf=exp,
       showweights = T,
       fontsize=6)
dev.off()
summary(my_model)
# sensitive Analysis.TEMP.GDP --------------------------------------------------------------------
data <- readxl::read_xlsx("all_temp.xlsx")
G <- readxl::read_xlsx("GDP_rank.xlsx")
M <- merge(data,G,by = "location")

test <- M %>% filter(GDPrank<=10)
EffectSize=test$Estimate
se=test$Std.Error
slab=test$location
#Run ramdom effects model
my_model <- rma.uni(measure="RR", yi=EffectSize, sei=se, slab=slab)
pdf("GDP.top10.pdf",width=6,height=4)
forest(my_model, 
       xlab="Risk Ratio (log scale)", 
       header="State", 
       xlim = c(-2.5, 4), 
       at = log(c(0.25, 1,2,4)),
       atransf=exp,
       showweights = T,
       fontsize=6)
dev.off()
summary(my_model)

test <- M %>% filter(GDPrank>=40)
EffectSize=test$Estimate
se=test$Std.Error
slab=test$location
#Run ramdom effects model
my_model <- rma.uni(measure="RR", yi=EffectSize, sei=se, slab=slab)
pdf("GDP.last10.pdf",width=6,height=4)
forest(my_model, 
       xlab="Risk Ratio (log scale)", 
       header="State", 
       xlim = c(-2.5, 4), 
       at = log(c(0.25, 1,2,4)),
       atransf=exp,
       showweights = T,
       fontsize=6)
dev.off()
summary(my_model)

# sensitive Analysis.TEMP.personal income --------------------------------------------------------------------
data <- readxl::read_xlsx("all_temp.xlsx")
G <- readxl::read_xlsx("GDP_rank.xlsx")
M <- merge(data,G,by = "location")

test <- M %>% filter(Personrank<=10)
EffectSize=test$Estimate
se=test$Std.Error
slab=test$location
#Run ramdom effects model
my_model <- rma.uni(measure="RR", yi=EffectSize, sei=se, slab=slab)
pdf("Personrank.top10.pdf",width=6,height=4)
forest(my_model, 
       xlab="Risk Ratio (log scale)", 
       header="State", 
       xlim = c(-2.5, 4), 
       at = log(c(0.25, 1,2,4)),
       atransf=exp,
       showweights = T,
       fontsize=6)
dev.off()
summary(my_model)

test <- M %>% filter(Personrank>=40)
EffectSize=test$Estimate
se=test$Std.Error
slab=test$location
#Run ramdom effects model
my_model <- rma.uni(measure="RR", yi=EffectSize, sei=se, slab=slab)
pdf("Personrank.last10.pdf",width=6,height=4)
forest(my_model, 
       xlab="Risk Ratio (log scale)", 
       header="State", 
       xlim = c(-2.5, 4), 
       at = log(c(0.25, 1,2,4)),
       atransf=exp,
       showweights = T,
       fontsize=6)
dev.off()
summary(my_model)







