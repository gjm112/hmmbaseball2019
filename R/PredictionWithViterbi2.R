#I need to fit the models again with data only from april and May. 
#So get version of the posterior that are based only on the first two months of the season.  
year<-2016
#Need the full data from here:
load(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/twoStateModel_Pitchers_",year,"_afterArticle_UniformPriorsOneSigma_NoS_10000draws.RData"))
data2<-data
#Need posterior estimates from here:
load(paste0("/Users/gregorymatthews/Dropbox/HMMbaseball/twoStateModel_Pitchers_",year,"_afterArticle_UniformPriorsOneSigma_NoS_OneSigma_10000draws_AprilMayOnly.RData"))
data<-data2
rm(data2)



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

###############################################################
#Throw out the first 9000.  
###############################################################
library(coda)
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


#rem<-list()
#rem[[2015]]<-c(10,8,6,5,3,1)
#rem[[2016]]<-c(10,5,4,3)
#rem[[2017]]<-c(7)
#for (i in rem[[year]]){beta[[i]]<-NULL}

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


# rem<-list()
# rem[[2015]]<-c(10,8,6,5,3,1)
# rem[[2016]]<-c(10,5,4,3)
# rem[[2017]]<-c(7)
# for (i in rem[[year]]){tau[[i]]<-NULL}


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
# rem<-list()
# rem<-list()
# rem[[2015]]<-c(10,8,6,5,3,1)
# rem[[2016]]<-c(10,5,4,3)
# rem[[2017]]<-c(7)
# for (i in rem[[year]]){gamma[[i]]<-NULL}



################
#For Prediction
################
library(HiddenMarkov)
predsList<-list()

#j is the individual
betaPost<-summary(beta)$stat[,1]
sigmaPost<-1/summary(tau)$stat[1]
gammaPost<-summary(gamma)$stat[,1]


# for (y in rem[[year]]){
#   test[[y]]<-NULL
# }

g1<-apply(do.call(cbind,lapply(test,function(x){apply(x$g[,,1:1000,1],c(1,2),mean)}))[,c(1,3,5,7)],1,mean)
g2<-apply(do.call(cbind,lapply(test,function(x){apply(x$g[,,1:1000,1],c(1,2),mean)}))[,c(1,3,5,7)+1],1,mean)
gPost<-cbind(g1,g2)

b1<-apply(do.call(cbind,lapply(test,function(x){apply(x$b[,,1:1000,1],c(1,2),mean)}))[,c(1,3,5,7)],1,mean)
b2<-apply(do.call(cbind,lapply(test,function(x){apply(x$b[,,1:1000,1],c(1,2),mean)}))[,c(1,3,5,7)+1],1,mean)
bPost<-cbind(b1,b2)

xb<-cbind(betaPost[1]+b1,betaPost[2]+b2)
xg<-cbind(gammaPost[1]+g1,gammaPost[2]+g2)

expit<-function(x){exp(x)/(1+exp(x))}


for (j in 1:nrow(xb)){print(j)
  Pi<-matrix(NA,nrow=2,ncol=2)
  Pi[2,2]<-expit(xg[j,1])
  Pi[2,1]<-1-expit(xg[j,1])
  
  Pi[1,1]<-1-expit(xg[j,2])
  Pi[1,2]<-expit(xg[j,2])
  
  #distribution of state at time 1
  delta<-c(0.5,0.5)
  
  dataSmall<-data[data$UTC>="2016-05-31 00:00:00" & data$mlbid == ids[j],]
  
  preds<-c()
  npmax<-nrow(dataSmall)
  for (np in 2:npmax){print(np)
    hmm<-dthmm(x=dataSmall$resid[1:np],Pi,delta,distn="norm",list(mean=xb[j,],sd=sigmaPost))
    vit<-Viterbi(hmm)
    preds[np+1]<-vit[length(vit)]
  }
  
  predsList[[j]]<-data.frame(predState=preds,resid=c(dataSmall$resid,NA),pitch_id=c(dataSmall$pitch_id,NA),UTC = c(dataSmall$UTC,NA),startSpeed = c(dataSmall$start_speed_corr,NA),first=c(dataSmall$first,NA),last=c(dataSmall$last,NA)) 
}

for(j in 1:length(predsList))
{
  predsList[[j]]$id <- ids[j]
}

#save(predsList, file = "/Users/gregorymatthews/Dropbox/HMMbaseball/predsList2016.RData")
#load("/Users/gregorymatthews/Dropbox/HMMbaseball/predsList2016.RData")

# library(RMySQL)
# con1 <- dbConnect(MySQL(), host="50.19.97.46", user="gmatthews", password="wildandcrazystats", dbname="bp_pitchinfo")
# q <-  paste ("SELECT * FROM pitches_faster WHERE season = '2016' AND level = 'mlb' AND gametype='R' AND player_position='P'")
# pitches <- dbGetQuery(con1,q)
# save(pitches,file="/Users/gregorymatthews/Dropbox/HMMbaseball/data2016pitchersPitchInfo_ALLPITCHES.RData")

load("/Users/gregorymatthews/Dropbox/HMMbaseball/data2016pitchersPitchInfo_ALLPITCHES.RData")

#######################################
#Now merge.  
#######################################
preds<-do.call(rbind,predsList)

#For a single player
#pitches <- merge(pitches, predsList[[j]][,c("pitch_id","predState","resid")],by.x="pitch_id",by.y="pitch_id",all.x=TRUE)

#For all players
pitches <- merge(pitches, preds[,c("pitch_id","predState","resid")],by.x="pitch_id",by.y="pitch_id",all.x=TRUE)

# #First check fastball velocity. 
# boxplot(pitches$resid~pitches$predState)
# 
# #quick test:
# sub<-subset(pitches,last=="Colon")
# sub2<-subset(sub,!is.na(predState))
# boxplot(sub2$start_speed_corr~sub2$predState)

#Now check curve ball stuff.  
pitches$predStateLag1<-lag(pitches$predState)
curve<-subset(pitches,pi_pitch_type=="CU")

boxplot(curve$pfx_z~curve$predStateLag1)


##matching pitches up to state predictions
##using the id's and the residuals.
totpred <- do.call(rbind.data.frame, predsList)
totpred$sid <- paste(totpred$id, totpred$res, sep="-")

data$sid <- paste(data$mlbid, data$resid, sep="-")
data$predstate <- totpred[match(data$sid, totpred$sid),1]

pitches <- pitches[order(pitches$last,pitches$UTC),]

#Add the predicted state to every pitch
pitches$predState2 <- pitches$predState
for (i in 2:nrow(pitches)){print(i)
  if (is.na(pitches$predState2[i]) & !is.na(pitches$predState[i-1]) & pitches$last[i-1] == pitches$last[i]){
    pitches$predState2[i] <- pitches$predState[i-1]
  }
    
}

save(pitches,file = "/Users/gregorymatthews/Dropbox/hmmbaseballgit/pitches.RData")

table(pitches$predState2)


pitches$mlbid <- as.factor(pitches$mlbid)
library(lme4)
#Curve balls 
summary(lmer(pfx_z~(1|mlbid)+factor(predState2), data=subset(pitches, pi_pitch_type=="CU")))

#sliders
summary(lmer(pfx_x~(1|mlbid)+factor(predState2)*throws, data=subset(pitches, pi_pitch_type=="SL")))

#FC
summary(lmer(pfx_x~(1|mlbid)+factor(predState2)*throws, data=subset(pitches, pi_pitch_type=="FC")))


summary(glm((result=="S")~cs_prob+
              start_speed_corr, 
            data=subset(pitches, swing=="take"),
            family=binomial(logit)))















################################################################
#Stuff below here is Rob Arthur
################################################################





##read in Rudy's data.
st <- read.csv("~/Dropbox/HMMbaseball/data/razz_pitchingstart_projvactual_apr2016_aug142017.csv")
library(lubridate)
##joining on date+MLBID
data$std <- paste(data$mlbid, ymd(data$date))
st$std <- paste(st$MLBAMID, mdy(st$Date))
##finding the sequence of 5 pitches immediately preceding
##each start
v <- vector()
for(i in 1:nrow(st))
{
  ind <- min(which(((data$std)==(st$std[i]))))
  if(((ind+4)<0)|is.infinite(ind))
  {next}
  v[i] <- sum(data[(ind+4):ind,]$predstate==2)
}
#Where does 8252 come from?  
v[nrow(st)] <- NA
st$predhot <- v
##creating differences between actual and projected
st$erd <- st$ER_ACT-st$ER_PROJ
st$kd <- st$K_ACT-st$K_PROJ
st$wd <- st$W_ACT-st$W_PROJ
st$whd <- st$WHIP_ACT-st$WHIP_PROJ
st$bbd <- st$BB_ACT-st$BB_PROJ
st$ipd <- st$IP_ACT-st$IP_PROJ
st$erad <- st$ERA_ACT-st$ERA_PROJ
st$hrd <- st$HR_ACT-st$HR_PROJ

##reading in postseason data.
post <- read.csv("../../Downloads/playoff-pfx (2)/playoff-pfx.csv")
post$dt <- ymd(unlist(lapply(strsplit(as.character(post$gameDate), split=" "), "[[", 1)))
pred <- (subset(st, year(mdy(Date))=="2016"&month(mdy(Date))>8, 
                select=c("Name", "MLBAMID", "predhot","Date")))
#ordering it
library(plyr)
pred <- ddply(pred, "MLBAMID", function(X)
  data.frame(dt=X[order(mdy(X$Date)),][nrow(X),]))
#getting 99th percentile velocities (to correspond to fastball speeds)
#N.B.: no pitch tags in the postseason, so using 99th to limit to fastballs
z <- aggregate(releaseVelocity~pitcherId, 
               data=subset(post, year(dt)=="2016"), quantile, probs=.99)
#to get corresponding speeds, using the 95th percentile of fastballs
v <- aggregate(start_speed~mlbid, data=data, quantile, probs=.95,
               na.rm=T)
z$reg <- v[match(z$pitcherId, v$mlbid),2]
z$predhot <- pred[match(z$pitcherId, pred$MLBAMID),]$dt.predhot
z$name <- pred[match(z$pitcherId, pred$MLBAMID),]$dt.Name
z$dif <- z$releaseVelocity-z$reg
#plotting predicted states versus fastball velocity change
plot(z$predhot, z$dif, xlab="Predicted Hotness", 
     ylab="Difference From Regular Season Velocity", 
     pch=16, col=rgb(0,0,0,.5))
z$pp <- z$predhot-6
abline(lm(z$dif~(z$pp)), lwd=3, col="red")
z16 <- z
##this gets repeated for 2014-2015

##on to win probability analysis...

##playoff stuff

##now all years
pl <- read.csv("../../Downloads/elogames_for_rob.csv")
pl <- subset(pl, season>2013)
pl$date <- unlist(lapply(strsplit(
  as.character(pl$dt), split="T"), "[[", 1))
pl$std1 <- paste(pl$mlb1, pl$date)
pl$std2 <- paste(pl$mlb2, pl$date)

chad <- read.csv("../../Desktop/baseball_dbs/chadwick_4\'2\'2016.csv")
pl$mlb1 <- chad[match(pl$pitcher1_id, chad$key_retro),]$key_mlbam
pl$mlb2 <- chad[match(pl$pitcher2_id, chad$key_retro),]$key_mlbam

v <- aggregate(predstate~std, data=subset(data15,
                                          std%in%subset(data.frame(table(data15$std)), Freq>5)$Var1), 
               function(x) 
               {sum((x[(length(x)-4):length(x)])=="2")})

pl$hot1[pl$season=="2015"] <- 
  v[match(pl$std1, v$std),2][pl$season=="2015"]

pl$hot2[pl$season=="2015"] <- 
  v[match(pl$std2, v$std),2][pl$season=="2015"]
pl$hd <- pl$hot1-pl$hot2

##
v <- aggregate(predstate~std, data=subset(data,
                                          std%in%subset(data.frame(table(data$std)), Freq>5)$Var1), 
               function(x) 
               {sum((x[(length(x)-4):length(x)])=="2")})

pl$hot1[pl$season=="2016"] <- 
  v[match(pl$std1, v$std),2][pl$season=="2016"]

pl$hot2[pl$season=="2016"] <- 
  v[match(pl$std2, v$std),2][pl$season=="2016"]
pl$hd <- pl$hot1-pl$hot2

##
v <- aggregate(predstate~std, data=subset(data14,
                                          month(ymd(date))>5&
                                            std%in%subset(data.frame(table(data14$std)), Freq>5)$Var1), 
               function(x) 
               {sum((x[(length(x)-4):length(x)])=="2")})

pl$hot1[pl$season=="2014"] <- 
  v[match(pl$std1, v$std),2][pl$season=="2014"]

pl$hot2[pl$season=="2014"] <- 
  v[match(pl$std2, v$std),2][pl$season=="2014"]
pl$hd <- pl$hot1-pl$hot2


pl$hd <- pl$hot1-pl$hot2

mod <- (glm(wp~rating_prob+hd, data=pl, family=binomial(logit)))
##and that model has the 8% difference.

predict(mod, data.frame(rating_prob=rep(.5, 11),
                        hd=-5:5), type="response")



##miscellaneous models
summary(glm((result=="S")~cs_prob+
              start_speed_corr+S, 
            data=subset(data, swing=="take"),
            family=binomial(logit)))


pi16$mlbid <- as.factor(pi16$mlbid)
summary(glmer(pfx_z~(1|mlbid)+pre, 
              data=subset(pi16, pi_pitch_type=="CU")))



pi16$mlbid <- as.factor(pi16$mlbid)
summary(glmer(pfx_x~(1|mlbid)+pre*throws, 
              data=subset(pi16, pi_pitch_type=="SL")))

pi16$mlbid <- as.factor(pi16$mlbid)
summary(glmer(pfx_x~(1|mlbid)+pre*throws, 
              data=subset(pi16, pi_pitch_type=="FC")))









