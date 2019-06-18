year<-2016
load(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/twoStateModel_Pitchers_",year,"_afterArticle_UniformPriorsOneSigma_OnlyS_5000burnin_50draws_4chains.RData"))
Spost<-test
S<-apply(do.call(rbind,lapply(Spost,function(x){apply(x[[1]],1,mean)})),2,mean)
rm(test)
load(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/twoStateModel_Pitchers_",year,"_afterArticle_UniformPriorsOneSigma_NoS_10000draws.RData"))

##################################################
#Centering
##################################################
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

#Posterior estimates of betas
library(coda)
beta<-mcmc.list(list(
  mcmc(t(test[[1]]$beta[1:2,1:10000,1])),
  mcmc(t(test[[2]]$beta[1:2,1:10000,1])),
  mcmc(t(test[[3]]$beta[1:2,1:10000,1])),
  mcmc(t(test[[4]]$beta[1:2,1:10000,1])),
  mcmc(t(test[[5]]$beta[1:2,1:10000,1])),
  mcmc(t(test[[6]]$beta[1:2,1:10000,1])),
  mcmc(t(test[[7]]$beta[1:2,1:10000,1])),
  mcmc(t(test[[8]]$beta[1:2,1:10000,1])),
  mcmc(t(test[[9]]$beta[1:2,1:10000,1])),
  mcmc(t(test[[10]]$beta[1:2,1:10000,1]))
)
)
jpeg(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/Manuscript/betaPosterior10000_",year,".jpg"))
plot(beta)
dev.off()

#Posterior estimates of tau
library(coda)
tau<-mcmc.list(list(
  mcmc(test[[1]]$tau[1,1:10000,1]),
  mcmc(test[[2]]$tau[1,1:10000,1]),
  mcmc(test[[3]]$tau[1,1:10000,1]),
  mcmc(test[[4]]$tau[1,1:10000,1]),
  mcmc(test[[5]]$tau[1,1:10000,1]),
  mcmc(test[[6]]$tau[1,1:10000,1]),
  mcmc(test[[7]]$tau[1,1:10000,1]),
  mcmc(test[[8]]$tau[1,1:10000,1]),
  mcmc(test[[9]]$tau[1,1:10000,1]),
  mcmc(test[[10]]$tau[1,1:10000,1])
))
jpeg(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/Manuscript/tauPosterior10000_",year,".jpg"))
plot(tau)
dev.off()


#Posterior estimates of gamma
library(coda)
gamma<-mcmc.list(list(
  mcmc(t(test[[1]]$gamma[1:2,1:10000,1])),
  mcmc(t(test[[2]]$gamma[1:2,1:10000,1])),
  mcmc(t(test[[3]]$gamma[1:2,1:10000,1])),
  mcmc(t(test[[4]]$gamma[1:2,1:10000,1])),
  mcmc(t(test[[5]]$gamma[1:2,1:10000,1])),
  mcmc(t(test[[6]]$gamma[1:2,1:10000,1])),
  mcmc(t(test[[7]]$gamma[1:2,1:10000,1])),
  mcmc(t(test[[8]]$gamma[1:2,1:10000,1])),
  mcmc(t(test[[9]]$gamma[1:2,1:10000,1])),
  mcmc(t(test[[10]]$gamma[1:2,1:10000,1]))
)
)

jpeg(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/Manuscript/gammaPosterior10000_",year,".jpg"))
plot(gamma)
dev.off()


#Posterior estimates of b
i<-5 #for example
library(coda)
b<-mcmc.list(list(
  mcmc(t(test[[1]]$b[i,1:2,1:10000,1])),
  mcmc(t(test[[2]]$b[i,1:2,1:10000,1])),
  mcmc(t(test[[3]]$b[i,1:2,1:10000,1])),
  mcmc(t(test[[4]]$b[i,1:2,1:10000,1])),
  mcmc(t(test[[5]]$b[i,1:2,1:10000,1])),
  mcmc(t(test[[6]]$b[i,1:2,1:10000,1])),
  mcmc(t(test[[7]]$b[i,1:2,1:10000,1])),
  mcmc(t(test[[8]]$b[i,1:2,1:10000,1])),
  mcmc(t(test[[9]]$b[i,1:2,1:10000,1])),
  mcmc(t(test[[10]]$b[i,1:2,1:10000,1]))
)
)


#Posterior estimates of g
i<-5 #for example
library(coda)
b<-mcmc.list(list(
  mcmc(t(test[[1]]$g[i,1:2,1:10000,1])),
  mcmc(t(test[[2]]$g[i,1:2,1:10000,1])),
  mcmc(t(test[[3]]$g[i,1:2,1:10000,1])),
  mcmc(t(test[[4]]$g[i,1:2,1:10000,1])),
  mcmc(t(test[[5]]$g[i,1:2,1:10000,1])),
  mcmc(t(test[[6]]$g[i,1:2,1:10000,1])),
  mcmc(t(test[[7]]$g[i,1:2,1:10000,1])),
  mcmc(t(test[[8]]$g[i,1:2,1:10000,1])),
  mcmc(t(test[[9]]$g[i,1:2,1:10000,1])),
  mcmc(t(test[[10]]$g[i,1:2,1:10000,1]))
)
)

###############################################################
#Throw out the first 9000.  
#Some chains are removed because of non-convergence
#In 2015 chains 1, 3, 5, 6, 9, 10 need to be removed. 
#In 2016 chains 3, 4, 5, and 10 need to be removed. 
#In 2017 chains 7 need to be removed. 
###############################################################
keep<-9001:10000
beta<-mcmc.list(list(
  mcmc(t(test[[1]]$beta[1:2,keep,1])),
  mcmc(t(test[[2]]$beta[1:2,keep,1])),
  mcmc(t(test[[3]]$beta[1:2,keep,1])),
  mcmc(t(test[[4]]$beta[1:2,keep,1])),
  mcmc(t(test[[5]]$beta[1:2,keep,1])),
  mcmc(t(test[[6]]$beta[1:2,keep,1])),
  mcmc(t(test[[7]]$beta[1:2,keep,1])),
  mcmc(t(test[[9]]$beta[1:2,keep,1])),
  mcmc(t(test[[8]]$beta[1:2,keep,1])),
  mcmc(t(test[[10]]$beta[1:2,keep,1]))
))

rem<-list()
rem[[2015]]<-c(10,8,6,5,3,1)
rem[[2016]]<-c(10,5,4,3)
rem[[2017]]<-c(9,8,7)
for (i in rem[[year]]){beta[[i]]<-NULL}

jpeg(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/Manuscript/betaPosterior",year,".jpg"))
plot(beta)
dev.off()

#diagnostic plot for convergence.  
jpeg(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/Manuscript/gelmanPlotBetaPosterior",year,".jpg"))
gelman.plot(beta,ylim=c(1,5))
dev.off()

gelman.diag(beta)
summary(beta)

#tau
library(coda)
tau<-mcmc.list(list(
  mcmc(test[[1]]$tau[1,keep,1]),
  mcmc(test[[2]]$tau[1,keep,1]),
  mcmc(test[[3]]$tau[1,keep,1]),
  mcmc(test[[4]]$tau[1,keep,1]),
  mcmc(test[[5]]$tau[1,keep,1]),
  mcmc(test[[6]]$tau[1,keep,1]),
  mcmc(test[[7]]$tau[1,keep,1]),
  mcmc(test[[8]]$tau[1,keep,1]),
  mcmc(test[[9]]$tau[1,keep,1]),
  mcmc(test[[10]]$tau[1,keep,1])
))


rem<-list()
rem[[2015]]<-c(10,8,6,5,3,1)
rem[[2016]]<-c(10,5,4,3)
rem[[2017]]<-c(9,8,7)
for (i in rem[[year]]){tau[[i]]<-NULL}

jpeg(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/Manuscript/tauPosterior",year,".jpg"))
plot(tau)
dev.off()

jpeg(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/Manuscript/gelmanPlotTauPosterior",year,".jpg"))
gelman.plot(tau,ylim=c(1,5))
dev.off()

gelman.diag(tau)
summary(tau)

gamma<-mcmc.list(list(
  mcmc(t(test[[1]]$gamma[1:2,9001:10000,1])),
  mcmc(t(test[[2]]$gamma[1:2,9001:10000,1])),
  mcmc(t(test[[3]]$gamma[1:2,9001:10000,1])),
  mcmc(t(test[[4]]$gamma[1:2,9001:10000,1])),
  mcmc(t(test[[5]]$gamma[1:2,9001:10000,1])),
  mcmc(t(test[[6]]$gamma[1:2,9001:10000,1])),
  mcmc(t(test[[7]]$gamma[1:2,9001:10000,1])),
  mcmc(t(test[[8]]$gamma[1:2,9001:10000,1])),
  mcmc(t(test[[9]]$gamma[1:2,9001:10000,1])),
  mcmc(t(test[[10]]$gamma[1:2,9001:10000,1])))
)


rem<-list()
rem<-list()
rem[[2015]]<-c(10,8,6,5,3,1)
rem[[2016]]<-c(10,5,4,3)
rem[[2017]]<-c(9,8,7)
for (i in rem[[year]]){gamma[[i]]<-NULL}

jpeg(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/Manuscript/gammaPosterior",year,".jpg"))
plot(gamma)
dev.off()

jpeg(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/Manuscript/gelmanPlotGammaPosterior",year,".jpg"))
gelman.plot(gamma,ylim=c(1,5))
dev.off()

gelman.diag(gamma)
summary(gamma)


#Calculating the estimated league wide average transition matrix.  
gamma1<-summary(gamma)$statistics[1,1]
gamma2<-summary(gamma)$statistics[2,1]

c(exp(gamma1)/(1+exp(gamma1)),1-exp(gamma1)/(1+exp(gamma1)))
c(exp(gamma2)/(1+exp(gamma2)),1-exp(gamma2)/(1+exp(gamma2)))


############Summary Data
#Posterior means.  
betaPost<-summary(beta)$stat[,1]
sigmaPost<-1/summary(tau)$stat[1]
gammaPost<-summary(gamma)$stat[,1]


for (y in rem[[year]]){
  test[[y]]<-NULL
}

#Posterior means for transition matrix
g1<-apply(do.call(cbind,lapply(test,function(x){apply(x$g[,,9001:10000,1],c(1,2),mean)}))[,c(1,3,5,7)],1,mean)
g2<-apply(do.call(cbind,lapply(test,function(x){apply(x$g[,,9001:10000,1],c(1,2),mean)}))[,c(1,3,5,7)+1],1,mean)
gPost<-cbind(g1,g2)

#Posterior means for means of emission distributions 
b1<-apply(do.call(cbind,lapply(test,function(x){apply(x$b[,,9001:10000,1],c(1,2),mean)}))[,c(1,3,5,7)],1,mean)
b2<-apply(do.call(cbind,lapply(test,function(x){apply(x$b[,,9001:10000,1],c(1,2),mean)}))[,c(1,3,5,7)+1],1,mean)
bPost<-cbind(b1,b2)

#Merge on ids for bPost and gPost
bPost<-cbind(mlbkey,bPost)
gPost<-cbind(mlbkey,gPost)

#################
#Plots for paper.  
#################
#################
#Single player over a season
#################
small$start_speed_corr<-data$start_speed_corr
last<-"Kershaw"
q<-small$mlbid[small$last==last][1]
indPlayer<-which(ids==q)
temp<-subset(small,mlbid==q)
temp$S<-S[(ind[indPlayer]+1):ind[indPlayer+1]]
temp$num<-1:nrow(temp)

vv<-data.frame(vv=temp$num[temp$newGame==1])
library(chron)
temp$UTC
dtimes = c("2002-06-09 12:45:40","2002-06-09 09:30:40",
           "2002-09-04 16:45:40","2002-11-13 20:00:40","2002-07-07 17:30:40")
dtparts = t(as.data.frame(strsplit(as.character(temp$UTC),' ')))
row.names(dtparts) = NULL
temp$UTCchron = chron(dates=dtparts[,1],times=dtparts[,2],format=c('y-m-d','h:m:s'))

library(ggplot2)
png(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/Manuscript/",last,year,".png"),units="in",res=300,h=5,w=10)
p<-ggplot(data=temp) + geom_vline(aes(xintercept=num,col=S),cex=0.1) + geom_vline(data=vv,aes(xintercept=vv)) + scale_colour_gradient(low = "blue",high="red")   + geom_point(data=temp,aes(x=num,y=resid),cex=1)  + scale_x_continuous(breaks=temp$num[seq(1,nrow(temp),400)],labels=temp$UTC[seq(1,nrow(temp),400)])   + ggtitle(paste(temp$first,temp$last,sep=" ")) + xlab("date") +ylab("residual")
p
dev.off()
#################
#boxplots
#################
beta1<-c(t(do.call(cbind,lapply(test,function(x){x$beta[1,9001:10000,1]}))))
beta2<-c(t(do.call(cbind,lapply(test,function(x){x$beta[2,9001:10000,1]}))))

b1<-c(t(do.call(cbind,lapply(test,function(x){x$b[,1,9001:10000,1]}))))
b2<-c(t(do.call(cbind,lapply(test,function(x){x$b[,2,9001:10000,1]}))))

b<-data.frame(b1,b2)
#tweak these numbers
b$ind<-rep(1:nrow(bPost),each=1000*length(test))

mlbkey2 <- data[!duplicated(data$mlbid),c("mlbid","first","last")]
b<-merge(b,mlbkey,by.x="ind",by.y="ind",all.x=TRUE)
b<-merge(b,mlbkey2,by.x="mlbid",by.y="mlbid",all.x=TRUE)
b$last<-as.factor(b$last)

b$name<-paste0(substring(b$first,1,1),".",b$last)

b<-b[order(b$last),]
unique(b$last)[1:25]

png(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/Manuscript/Boxplots",year,".png"),units="in",res=300,h=5,w=10)
p2<-ggplot(data=b[b$last%in%unique(b$last)[1:25],]) + 
  geom_boxplot(aes(x=factor(name),y=beta1+b1),col="blue") +
  geom_boxplot(aes(x=factor(name),y=beta2+b2),col="red") +
  geom_hline(yintercept = betaPost[1],col=rgb(0,0,1,0.5)) +
  geom_hline(yintercept = betaPost[2],col=rgb(1,0,0,0.5)) +coord_flip()

p2
dev.off()


# #Another plot looking at posterior means of states. 
# betaPost
# bPost<-merge(bPost,mlbkey2,by.x="mlbid",by.y="mlbid",all.x=TRUE)
# 
# ggplot(data=bPost) + geom_point(aes(y=b1+betaPost[1]),col=rgb(0,0,1,0.1))+ geom_point(aes(x=2,y=b2+betaPost[2]),col=rgb(1,0,0,0.1)) 

