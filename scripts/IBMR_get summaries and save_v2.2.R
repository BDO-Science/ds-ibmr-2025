################################################################
### Delta smelt compile IBMR runs, get summaries, save results #
### William Smith (USFWS; BDFWO); 21 June 2022 #################
################################################################

FWS.abundance<-read.table(file.path(input_path,'FWS.abundance_LCME.txt'),header=F)
FWS.abundance<-cbind(FWS.abundance[,2],FWS.abundance[,3],FWS.abundance[,4],FWS.abundance[,5])
super<-median(c(apply(outz[1:20,1,],1,median,na.rm=T)/FWS.abundance[1:20,1], # get ratio of simulated abundance to LCME-estimated abundance
 apply(outz[1:20,2,],1,median,na.rm=T)/FWS.abundance[1:20,2],
 apply(outz[1:20,3,],1,median,na.rm=T)/FWS.abundance[1:20,3],
 apply(outz[1:20,4,],1,median,na.rm=T)/FWS.abundance[1:20,4]))
print(super)
median(outz[n.years,4,]/outz[5,4,]) # mean change in subadult abundance from 1999 to 2014
obs.Febln<-c(63.55,64.60,62.55,65.31,64.24,67.72,61.56,64.37,72.06,62.87,68.44,66.57,62.29,65.94,65.13,66.67,67.64,63.29,69.30,68.22,66.53,68.67,68.88)
sum((obs.Febln[8:20]-apply(outz[(8:20),5,],1,median,na.rm=T))^2)

lamAB<-matrix(NA,19,11)
lam.mn<-vector()
for (t in 1:19) { 
 lamAB[t,1] <- mean(outz[t+1,1,]/outz[t,1,],na.rm=T)
 lamAB[t,2] <- min(outz[t+1,1,]/outz[t,1,],na.rm=T)
 lamAB[t,3] <- max(outz[t+1,1,]/outz[t,1,],na.rm=T)
 lamAB[t,4] <- quantile(outz[t+1,1,]/outz[t,1,],0.025,na.rm=T)
 lamAB[t,5] <- quantile(outz[t+1,1,]/outz[t,1,],0.05,na.rm=T)
 lamAB[t,6] <- quantile(outz[t+1,1,]/outz[t,1,],0.25,na.rm=T)
 lamAB[t,7] <- quantile(outz[t+1,1,]/outz[t,1,],0.5,na.rm=T)
 lamAB[t,8] <- quantile(outz[t+1,1,]/outz[t,1,],0.75,na.rm=T)
 lamAB[t,9] <- quantile(outz[t+1,1,]/outz[t,1,],0.90,na.rm=T)
 lamAB[t,10] <- quantile(outz[t+1,1,]/outz[t,1,],0.975,na.rm=T)
 lamAB[t,11] <- (sd(outz[t+1,1,]/outz[t,1,],na.rm=T)/mean(outz[t+1,1,]/outz[t,1,],na.rm=T))
 }
colnames(lamAB) <- c("mean","min","max","2.5%","5%","25%","50%","75%","90%","97.5%","CV")

lam.mn[1] <- exp(mean(log(lamAB[,7]))) # print geometric mean pop. growth rate 1995-2014
lam.mn[2] <- exp(mean(log(lamAB[12:19,7]))) # print geometric mean pop. growth rate 2007-2014
lam.mn[3] <- exp(mean(log(lamAB[10:19,7]))) # print geometric mean pop. growth rate 2005-2014
lam.mn[4] <- exp(mean(log(lamAB[1:11,7]))) # print geometric mean pop. growth rate 1995-2006
lam.mn[5] <- exp(mean(log(lamAB[which(wtr.yr[1:19]<=2),7]),na.rm=T)) # print geometric mean pop. growth rate AN and wet yrs
lam.mn[6] <- exp(mean(log(lamAB[which(wtr.yr[1:19]>=4),7]),na.rm=T)) # print geometric mean pop. growth rate dry and critical yrs
lam.mn[7] <- exp(mean(log(lamAB[3:19,7]))) # print geometric mean pop. growth rate 1997-2014
lam.mn[8] <- exp(quantile(log(lamAB[3:19,7]),0.025)) # 95% CI
lam.mn[9] <- exp(quantile(log(lamAB[3:19,7]),0.975))

### 2. save results ###
file.save.spot<-c(here('output/model_outputs/'))
action.name=c('FirstFlush')

#write.table(lamAB,file=paste0(file.save.spot,'lamAB_',action.name,'.txt'))
#write.table(lam.mn,file=paste0(file.save.spot,'lamABmn_',action.name,'.txt'))
#saveRDS(outz,file=paste0(file.save.spot,'outz_',action.name,'.rds'))
#saveRDS(outz2,file=paste0(file.save.spot,'outz2_',action.name,'.rds'))
