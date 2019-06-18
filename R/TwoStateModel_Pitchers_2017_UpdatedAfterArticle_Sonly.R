
#nohup R CMD BATCH  TwoStateModel_Pitchers_2017_UpdatedAfterArticle.R /home/gmatthews1/HMM/twoStateModel_Pitchers_2017_afterArticle_UniformPriorsOneSigma_NoS_OneSigma_10000draws.Rout
#pitch GROUP GROUP GROUP you moron.  NOT TYPE!!!!!!

# library(RMySQL)
# con1 <- dbConnect(MySQL(), host="50.19.97.46", user="gmatthews", password="wildandcrazystats", dbname="bp_pitchinfo")
# q <-  paste("SELECT * FROM pitches_faster LEFT JOIN cutter_types on (pitches_faster.mlbid=cutter_types.mlbid) WHERE season = '2017' AND (pi_pitch_group = 'FA' or (pi_pitch_type ='FC' and hard=1)) AND level = 'mlb' AND gametype='R' AND player_position='P'")
# data <- dbGetQuery(con1,q)
# save(data,file="/Users/gregorymatthews/Dropbox/HMMbaseball/data2017pitchersPitchInfo.RData")



library(rjags)
library(lme4)
#load("/Users/gregorymatthews/Dropbox/HMMbaseball/data2017pitchersPitchInfo.RData")
load("/home/gmatthews1/HMM/data2017pitchersPitchInfo.RData")
#244057 observations.  With THREE pitch types.  




#Remove missing data 
data<-data[!is.na(data$elevation),] 
data<-data[!is.na(data$gtemp),]
data<-data[!is.na(data$start_speed_corr),]
data<-data[!is.na(data$batter_count),]
data<-data[!is.na(data$order_count),]
data<-data[!is.na(data$pitch_count),]
data<-data[!is.na(data$strike),]
data<-data[!is.na(data$park_id),]
data<-data[!is.na(data$on_1b),]
data<-data[!is.na(data$on_2b),]
data<-data[!is.na(data$on_3b),]
data<-data[!is.na(data$outs),]
data<-data[!is.na(data$inning),]
data<-data[!is.na(data$home_runs),]
data<-data[!is.na(data$away_runs),]
data<-data[!is.na(data$outs),]

#Order the pitches chronologically
data<-data[order(data$mlbid,data$UTC),]

#Add an indicator for new games.  
data$newGame<-c(1,diff(data$pitch_count)<0+0)

#Remove pitchers with fewer than 800 pitches.
#Originally we fit the models first and THEN removed the pitchers with only 800 pitches.  
#I think we should filter first, then model.  
keep<-rownames(table(data$mlbid))[(table(data$mlbid)>=800)]
data<-data[as.character(data$mlbid)%in%keep,] 


data$baserunners<-(!data$on_1b==0 | !data$on_2b==0 | !data$on_3b==0)
modPicher<-lmer(start_speed_corr ~ pi_pitch_type*pitch_count + pi_pitch_type*baserunners  + elevation*gtemp  + (1|mlbid) ,data=data)
data$resid<-resid(modPicher)


#Remove pitcher effect
#modPicher<-lmer(start_speed_corr ~ pitch_count+(1|mlbid),data=data)
#data$resid1<-resid(modPicher)
#Remove Context effect
#modContext<-lm(resid1 ~ elevation*gtemp + (!on_1b==0 | !on_2b==0 | !on_3b==0) ,data=data)
#modContext<-lm(resid1 ~ pi_pitch_type + elevation*gtemp + (!on_1b==0 | !on_2b==0 | !on_3b==0) ,data=data)
#data$resid2<-resid(modContext)

#Pull out all the ids
ids<-sort(unique(data$mlbid)) #176 unique ids 
small<-data[data$mlbid%in%ids,c("mlbid","resid","pitch_id","UTC","newGame","first","last")]

un<-sort(unique(small$mlbid))
small$ind <- NA
for (i in 1:length(un)){print(i)
  small$ind[small$mlbid==un[i]] <- i
}

small$UTC<-as.POSIXct(small$UTC)


#Get JAGS 4.x.y!  That's what makes it work!
small<-small[order(small$ind,small$UTC),]
n<-length(ids)
small<-small[small$ind<=n,]
mlbkey<-small[!duplicated(small$mlbid),c("mlbid","ind")]
ind<-c(0,cumsum(as.vector(table(small$ind))))
dat <- small$resid

Sinit <- 0 

#save.image(file="/Users/gregorymatthews/Dropbox/HMMbaseball/data2017pitchersPitchInfo_PreparedData.RData")
#load("/Users/gregorymatthews/Dropbox/HMMbaseball/data2017pitchersPitchInfo_PreparedData.RData")
# set.seed(1234)
# dataPermute<-data
# for (i in ids){print(i)
#   dataPermute$resid[dataPermute$mlbid==i]<-sample(dataPermute$resid[dataPermute$mlbid==i],length(dataPermute$resid[dataPermute$mlbid==i]),replace=FALSE)
# }


#Two state HMM model
model.str<-"model { 
for (j in 1:n){
dat[ind[j]+1] ~ dnorm(mu[ind[j]+1], tau)
mu[ind[j]+1] <- (beta[2]+b[j,2])*(S[ind[j]+1]==1) + (beta[1]+b[j,1])*(S[ind[j]+1]==0)
S[ind[j]+1] ~ dbern(p[ind[j]+1])
logit(p[ind[j]+1]) <- (gamma[1]+g[j,1])*(Sinit==1) + (gamma[2]+g[j,2])*(Sinit==0)


for (t in (ind[j]+2):ind[j+1])
{
  dat[t] ~ dnorm(mu[t], tau)
  mu[t] <- (beta[2]+b[j,2])*(S[t]==1) + (beta[1]+b[j,1])*(S[t]==0)
  S[t] ~ dbern(p[t])
  logit(p[t]) <- (gamma[1]+g[j,1])*(S[t-1]==1) + (gamma[2]+g[j,2])*(S[t-1]==0)
}

}

beta[1] ~ dunif(-20,20)
beta[2] ~ dunif(beta[1],20)


for (j in 1:n){
b[j,1] ~ dunif(-20,20)
b[j,2] ~ dunif(b[j,1]+beta[1]-beta[2],20)
g[j,1] ~ dunif(-7,7)
g[j,2] ~ dunif(-7,7)
}



for (i in 1:2){
gamma[i] ~ dunif(-7,7)
}

tau ~ dgamma(0.001,0.001)



} "


# for (j in 1:n){
#   b[j,1]<-bstar[j,1] - mean(bstar[,1])
#   b[j,2]<-bstar[j,2] - mean(bstar[,2])
#   g[j,1]<-gstar[j,1] - mean(gstar[,1])
#   g[j,2]<-gstar[j,2] - mean(gstar[,2])
# }

#setwd("/Users/gregorymatthews/Dropbox/HMMbaseball/")
#write(model.str,"jags_model_twoStateModel_pitchers.bug")

setwd("/home/gmatthews1/HMM/")
write(model.str,"jags_model_twoStateModel_pitchers.bug")



greg<-function(seed){
  #jags<-jags.model('/Users/gregorymatthews/Dropbox/HMMbaseball/jags_model_twoStateModel_pitchers.bug',data=list('dat'=dat, 'ind' = ind, 'Sinit' = Sinit, 'n' = n),n.chains=1,n.adapt=100, inits=list(.RNG.name="base::Wichmann-Hill", .RNG.seed=seed))
  jags<-jags.model('/home/gmatthews1/HMM/jags_model_twoStateModel_pitchers.bug',data=list('dat'=dat, 'ind' = ind, 'Sinit' = Sinit, 'n' = n),n.chains=1,n.adapt=100, inits=list(.RNG.name="base::Wichmann-Hill", .RNG.seed=seed))
  print("adapted")
  print(seed)
  #update(jags,5000)
  out<-jags.samples(jags,c('b','beta','g','gamma','tau'),10000)
  out
}
library(parallel)
start<-proc.time()
test<-mclapply(as.list(c(1:10)),greg,mc.cores = 10)
end<-proc.time()
end-start
save.image("/home/gmatthews1/HMM/twoStateModel_Pitchers_2017_afterArticle_UniformPriorsOneSigma_NoS_OneSigma_10000draws.RData")
#save.image("/home/gmatthews1/HMM/twoStateModel_Pitchers_2017_afterArticle_UniformPriorsOneSigma_OnlyS_5000burnin_500draws_4chains.RData")
