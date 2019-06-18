for (year in c(2015,2016,2017)){
load(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/twoStateModel_Pitchers_",year,"_afterArticle_UniformPriorsOneSigma_NoS_10000draws.RData"))

#Centering
for (j in 1:10){print(j)
  for (i in 1:10000){
    test[[j]]$beta[1,i,]<-test[[j]]$beta[1,i,]+mean(test[[j]]$b[,1,i,])
    test[[j]]$b[,1,i,]<-test[[j]]$b[,1,i,]-mean(test[[j]]$b[,1,i,])
    
    test[[j]]$beta[2,i,]<-test[[j]]$beta[2,i,]+mean(test[[j]]$b[,2,i,])
    test[[j]]$b[,2,i,]<-test[[j]]$b[,2,i,]-mean(test[[j]]$b[,2,i,])
  }
}

for (j in 1:10){
  for (i in 1:10000){
    test[[j]]$gamma[1,i,]<-test[[j]]$gamma[1,i,]+mean(test[[j]]$g[,1,i,])
    test[[j]]$g[,1,i,]<-test[[j]]$g[,1,i,]-mean(test[[j]]$g[,1,i,])
    
    test[[j]]$gamma[2,i,]<-test[[j]]$gamma[2,i,]+mean(test[[j]]$g[,2,i,])
    test[[j]]$g[,2,i,]<-test[[j]]$g[,2,i,]-mean(test[[j]]$g[,2,i,])
  }
}

expit<-function(x){exp(x)/(1+exp(x))}
for (ind in 1:100){print(ind)

epsList<-list()
for (q in 1:100){
##Posterior predictive checks.  
draw<-sample(9001:10000,1)
chain<-sample(1:10,1)
gamma<-c((test[[chain]]$gamma[1,draw,]),(test[[chain]]$gamma[2,draw,]))
beta<-c((test[[chain]]$beta[1,draw,]),(test[[chain]]$beta[2,draw,]))

g<-c((test[[chain]]$g[ind,1,draw,1]),(test[[chain]]$g[ind,2,draw,1]))
b<-c((test[[chain]]$b[ind,1,draw,1]),(test[[chain]]$b[ind,2,draw,1]))

tau<-test[[1]]$tau[draw]

nsim<-length(data$last[data$mlbid==mlbkey$mlbid[mlbkey$ind==ind]])
S<-rep(NA,nsim)
S[1]<-0

eps<-rep(NA,nsim)

for (t in 2:nsim){
p<-expit((gamma[1]+g[1])*(S[t-1]==1) + (gamma[2]+g[2])*(S[t-1]==0))
S[t]<-rbinom(1,1,p)
mu <- (beta[2]+b[2])*(S[t-1]==1) + (beta[1]+b[1])*(S[t-1]==0)
eps[t]<-rnorm(1,mu,sqrt(1/tau))
}

epsList[[q]]<-data.frame(eps=eps,sim=q)
}


library(ggplot2)
dat <- data.frame(residual=data$resid[data$mlbid==mlbkey$mlbid[mlbkey$ind==ind]])
epsDF <- do.call(rbind,epsList)
last<-data$last[data$mlbid==mlbkey$mlbid[mlbkey$ind==ind]][1]
png(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/posteriorPredictiveDensity_",last,year,".png"))
p<-ggplot(data=dat,aes(x=residual)) + geom_histogram(aes(y=..density..)) + geom_density(colour="black",lwd=2) +geom_density(aes(x=eps,group=sim),data=epsDF,colour=rgb(1,0,0,0.1)) + ggtitle(paste(last," - ",year))
print(p)
dev.off()

#volquez and jimenez gallardo are interesting examples
acfList<-lapply(epsList,function(x){acf(x$eps[-1],plot=FALSE)$acf[,,1]})
acfMat<-do.call(rbind,acfList)
acfDF<-data.frame(acf=c(t(acfMat)),lag=rep(1:dim(acfMat)[2],dim(acfMat)[1]))
realacfDF<-data.frame(acf=acf(dat$resid,plot=FALSE)$acf[1:20,,1],lag=1:20)
png(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/posteriorPredictiveACF_",last,year,".png"))
q<-ggplot(data=subset(acfDF,lag<=20),aes(x=lag,y=acf)) + geom_point(colour=rgb(0,0,0,0.5)) + geom_point(data=realacfDF,aes(x=lag,y=acf),col="red") + ggtitle(paste(last," - ",year))
print(q)
dev.off()
}

}
# plot(rep(1,nrow(acfMat)),acfMat[,1],xlim=c(0,20),ylim=c(-0.2,1),main=data$last[data$mlbid==mlbkey$mlbid[mlbkey$ind==ind]][1])
# for (s in 2:20){
# points(rep(s,nrow(acfMat)),acfMat[,s],pch=16,col=rgb(0,0,0,0.1))
# }
# points(1:20,acf(dat$resid,plot=FALSE)$acf[1:20,,1],pch=16,col="red")
# }
# 
# hist(eps,freq = FALSE)
# hist(data$resid[data$mlbid==mlbkey$mlbid[mlbkey$ind==ind]],add=TRUE,freq=FALSE,col='red')
# plot(eps,type="l")
# plot(data$resid[data$mlbid==mlbkey$mlbid[mlbkey$ind==ind]],type="l")
# S
# 
# 
