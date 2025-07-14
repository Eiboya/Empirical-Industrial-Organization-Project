#######################################################
########            Zhihan ZHANG             ##########
########             EIO PROJECT             ##########
########          (UPDATED VERSION)          ##########
#######################################################

data <- read.table(header = TRUE, sep = ";", dec=".","D:/zhdata/tatat/Data_US.2014_2.txt")
unique(data$AIRLINE)
library(dplyr)

################################
# PART 1. DESCRIPTIVE ANALYSIS #
################################

### Treatment of the data

data$LEG <- ifelse(data$AIRLINE == 'AA'|data$AIRLINE =='CO'|data$AIRLINE =='DL'|data$AIRLINE =='NW'|data$AIRLINE =='UA'|data$AIRLINE =='US', 1, 0)
data$lcc <- ifelse(data$AIRLINE != 'AA'& data$AIRLINE !='CO'& data$AIRLINE !='DL'& data$AIRLINE !='NW'& data$AIRLINE !='UA'& data$AIRLINE !='US', 1, 0)
msize=data$MSIZE
aa <- ifelse(data$AIRLINE == 'AA',1, 0)
dl <- ifelse(data$AIRLINE == 'DL',1, 0)
wn <- ifelse(data$AIRLINE == 'WN',1, 0)
al <- ifelse(data$AIRLINE =='NW'|data$AIRLINE =='UA'|data$AIRLINE =='US'|data$AIRLINE =='CO' & data$AIRLINE != 'AA'& data$AIRLINE !='DL'& data$AIRLINE !='WN',1, 0)
os <- ifelse(data$AIRLINE != 'AA'& data$AIRLINE !='CO'& data$AIRLINE !='DL'& data$AIRLINE !='NW'& data$AIRLINE !='UA'& data$AIRLINE !='US'& data$AIRLINE !='WN', 1, 0)
pop <- log(data$POPDEP)+log(data$POPARR)


Data <- data.frame(market=data$MARKET,
                   leg=data$LEG,
                   lcc=data$lcc,
                   AA=aa,
                   DL=dl,
                   WN=wn,
                   AL=al,
                   OS=os,
                   POP=pop,
                   distance=log(data$D),
                   msize=data$MSIZE
)

nleg=aggregate(x = data$LEG,list(data[,3],data[,5]),function(x){max(x)})
nlcc=aggregate(x = data$lcc,list(data[,3],data[,5]),function(x){max(x)})
df1<-aggregate(list(Nleg = nleg[,3]),list(MARKET=nleg[,1]),function(Nleg){sum(Nleg)})
df2<-aggregate(list(Nlcc = nlcc[,3]),list(MARKET=nlcc[,1]),function(Nlcc){sum(Nlcc)})
df3<- aggregate(list(AA = Data[,4]), list(MARKET = Data[,1]), function(x){max(x)})
df4<- aggregate(list(DL = Data[,5]), list(MARKET = Data[,1]), function(x){max(x)})
df5<- aggregate(list(WN = Data[,6]), list(MARKET = Data[,1]), function(x){max(x)})
df6<- aggregate(list(AL = Data[,7]), list(MARKET = Data[,1]), function(x){max(x)})
df7<- aggregate(list(OS = Data[,8]), list(MARKET = Data[,1]), function(x){max(x)})
df8<- aggregate(list(POP = Data[,9]), list(MARKET = Data[,1]), function(x){mean(x)})
df9<- aggregate(list(distance = Data[,10]), list(MARKET = Data[,1]), function(x){mean(x)})
df10<- aggregate(list(msize = Data[,11]), list(MARKET = Data[,1]), function(x){mean(x)})

DF1 <- merge(df1, df2, by=c("MARKET"))
DF2 <- merge(DF1, df3, by=c("MARKET"))
DF3 <- merge(DF2, df4, by=c("MARKET"))
DF4 <- merge(DF3, df5, by=c("MARKET"))
DF5 <- merge(DF4, df6, by=c("MARKET"))
DF6 <- merge(DF5, df7, by=c("MARKET"))
DF7 <- merge(DF6, df8, by=c("MARKET"))
DF8 <- merge(DF7, df9, by=c("MARKET"))
DATA <- merge(DF8, df10, by=c("MARKET"))

### Descriptive analysis

#airline %
pAA<-sum(DATA$AA==1)/length(DATA$MARKET)
pDL<-sum(DATA$DL==1)/length(DATA$MARKET)
pWN<-sum(DATA$WN==1)/length(DATA$MARKET)
pAL<-sum(DATA$AL==1)/length(DATA$MARKET)
pOS<-sum(DATA$OS==1)/length(DATA$MARKET)

#Table 1
library(psych)
describeBy(DATA)

#Table 2
Ctable <- table(DATA$Nleg, DATA$Nlcc)
ContiTable <- addmargins(Ctable)
ContiTable

#Table 3
#According to the msize's percentile, we divided the market size into 4 levels,
#small<=4.16, 4.16<medium small<=7.47, 7.47<medium large<=13.76, 13.76<large
quantile(DATA$msize)
Size<-cut(DATA$msize, breaks = c(0,4.16, 7.47, 13.76,200),
          labels = c("Small", "Medium-Small", "Medium-Large", "Large"))
DATA$Size <- Size


DATA$N <- DATA$Nleg + DATA$Nlcc
#describe(DATA)
describeBy(DATA,group=DATA$Size)

#Table 4
C2table <- table(DATA$Nleg, DATA$Size)
ContiTable2 <- addmargins(C2table)
ContiTable2

#Table 5
C3table <- table(DATA$Nlcc, DATA$Size)
ContiTable3 <- addmargins(C3table)
ContiTable3

###################################
# PART 2. MLE without & with corr #
###################################


#unique(data$AIRLINE)
#LEG: DL, WN, US, AA, UA
#LCC: FL, F9, NK, AS, B6, VX, G4, SY, QR

# First, write the profit functions of the legacy carriers

#### Nleg & Nlcc ########
N_LEG=as.matrix(DATA$Nleg)
N_LCC=as.matrix(DATA$Nlcc)

#### Matrix #############
n=nrow(DATA)
Xm=as.matrix(cbind(DATA$distance, DATA$msize, DATA$OS))



### profit functions LEG ####
library(mvtnorm)

profit_legacy <- function(dist, msize, os, N_leg, N_lcc, param_leg){
  
  beta_dist     <- param_leg[1]
  beta_pop      <- param_leg[2]
  beta_os  <- param_leg[3]
  
  lambda_1 <- param_leg[4]
  lambda_2 <- param_leg[4] + exp(param_leg[5])
  lambda_3 <- param_leg[4] + exp(param_leg[5]) + exp(param_leg[6])
  lambda_4 <- param_leg[4] + exp(param_leg[5]) + exp(param_leg[6]) + exp(param_leg[7])
  
  lambda_legacy <- c(lambda_1,lambda_2,lambda_3,lambda_4)
  
  gamma_1 <- exp(param_leg[8])
  gamma_2 <- exp(param_leg[8]) + exp(param_leg[9])
  gamma_3 <- exp(param_leg[8]) + exp(param_leg[9]) + exp(param_leg[10])
  gamma_4 <- exp(param_leg[8]) + exp(param_leg[9]) + exp(param_leg[10]) + exp(param_leg[11])
  
  gamma_legacy  <-c(gamma_1,gamma_2,gamma_3,gamma_4)
  
  if(N_leg > 0 & N_lcc > 0){
    
    f <- beta_dist*dist + beta_pop*msize + beta_os*os - lambda_legacy[N_leg] - gamma_legacy[N_lcc]/N_leg
    
  }else if(N_leg > 0 & N_lcc == 0){
    
    f <- beta_dist*dist + beta_pop*msize + beta_os*os - lambda_legacy[N_leg]
    
  }else{
    
    f <- 0
    
  }
  return(f)
}

### profit functions LCC ####


profit_lowcost <- function(dist, msize, os, N_leg, N_lcc, param_lc){
  
  beta_dist     <- param_lc[1]
  beta_pop      <- param_lc[2]
  beta_os  <- param_lc[3]
  
  lambda_1 <- param_lc[4]
  lambda_2 <- param_lc[4] + exp(param_lc[5])
  lambda_3 <- param_lc[4] + exp(param_lc[5]) + exp(param_lc[6])
  lambda_4 <- param_lc[4] + exp(param_lc[5]) + exp(param_lc[6]) + exp(param_lc[7])
  
  lambda_lc <- c(lambda_1,lambda_2,lambda_3,lambda_4)
  
  gamma_1 <- exp(param_lc[8])
  gamma_2 <- exp(param_lc[8]) + exp(param_lc[9])
  gamma_3 <- exp(param_lc[8]) + exp(param_lc[9]) + exp(param_lc[10])
  gamma_4 <- exp(param_lc[8]) + exp(param_lc[9]) + exp(param_lc[10]) + exp(param_lc[11])
  
  gamma_lc  <-  c(gamma_1,gamma_2,gamma_3, gamma_4)
  
  if(N_leg > 0 & N_lcc > 0){
    
    f <- beta_dist*dist + beta_pop*msize  + beta_os*os - lambda_lc[N_lcc] - gamma_lc[N_leg]/N_lcc
    
  }else if(N_leg == 0 & N_lcc > 0){
    
    f <- beta_dist*dist + beta_pop*msize  + beta_os*os - lambda_lc[N_lcc]
    
  }else{
    
    f <- 0
    
  }
  return(f)
}


##### Initial values #####
library(MASS)

#Apply ordered probit for LEGs
oprobit_legacy <- polr(as.factor(N_LEG) ~ Xm, method = c("probit"))
beta_legacy <- c(oprobit_legacy[["coefficients"]][["Xm1"]],oprobit_legacy[["coefficients"]][["Xm2"]],oprobit_legacy[["coefficients"]][["Xm3"]])
lambda_legacy <- oprobit_legacy[["zeta"]]
lambda_legacy_1 <- lambda_legacy[1]
lambda_legacy_2 <- log(lambda_legacy[2] - lambda_legacy[1])
lambda_legacy_3 <- log(lambda_legacy[3] - lambda_legacy[2])
lambda_legacy_4 <- log(lambda_legacy[4] - lambda_legacy[3])

#Apply ordered probit for LCCs
oprobit_lowcost <- polr(as.factor(N_LCC) ~ Xm, method = c("probit"))
beta_low_cost <- c(oprobit_lowcost[["coefficients"]][["Xm1"]],oprobit_lowcost[["coefficients"]][["Xm2"]],oprobit_lowcost[["coefficients"]][["Xm3"]])
lambda_low_cost <- oprobit_lowcost[["zeta"]]
lambda_low_cost_1 <- lambda_low_cost[1]
lambda_low_cost_2 <- log(lambda_low_cost[2] - lambda_low_cost[1])
lambda_low_cost_3 <- log(lambda_low_cost[3] - lambda_low_cost[2])
lambda_low_cost_4 <- log(lambda_low_cost[4] - lambda_low_cost[3])

#GammaCleeren (0.11,0.33,0.54.0.72)
gamma_cleeren <- c(log(0.11), log(0.33-0.11), log(0.54 - 0.33), log(0.72-0.54))
rho_cleeren <- 0.18


#theta_init
theta_init <- c(beta_legacy,lambda_legacy_1,lambda_legacy_2,lambda_legacy_3,lambda_legacy_4,gamma_cleeren,beta_low_cost,lambda_low_cost_1,lambda_low_cost_2,lambda_low_cost_3,lambda_low_cost_4,gamma_cleeren,rho_cleeren)
theta_init


LL_o <- function(i,theta){
  rho <- theta[23]
  corr <- diag(2)
  corr[lower.tri(corr)] <- rho
  corr[upper.tri(corr)] <- rho
  
  if(N_LEG[i] > 0 & N_LCC[i] > 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = N_LEG[i], N_lcc = N_LCC[i], theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = N_LEG[i], N_lcc = N_LCC[i], theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = N_LEG[i]+1, N_lcc = N_LCC[i], theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = N_LEG[i], N_lcc = N_LCC[i]+1, theta[12:22])), mean = c(0,0), corr) -
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3],  N_leg = N_LEG[i]+1, N_lcc = N_LCC[i]-1,  theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3],  N_leg = N_LEG[i], N_lcc = N_LCC[i], theta[12:22])), 
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3],  N_leg = N_LEG[i]+1, N_lcc = N_LCC[i], theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3],  N_leg = N_LEG[i]+1, N_lcc = N_LCC[i], theta[12:22])), mean = c(0,0), corr)
    if ( prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] > 0 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = N_LEG[i], N_lcc = 0, theta[1:11]),-Inf),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = N_LEG[i]+1, N_lcc = 0, theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = N_LEG[i], N_lcc = 1, theta[12:22])), mean = c(0,0), corr) 
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 0 & N_LCC[i] >0){
    prob <- pmvnorm(lower = c(-Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = N_LCC[i], theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = N_LCC[i], theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = N_LCC[i]+1, theta[12:22])), mean = c(0,0), corr) -
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3],  N_leg = 1, N_lcc = N_LCC[i]-1,  theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3],  N_leg = 0, N_lcc = N_LCC[i], theta[12:22])), 
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3],  N_leg = 1, N_lcc = N_LCC[i], theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3],  N_leg = 1, N_lcc = N_LCC[i], theta[12:22])), mean = c(0,0), corr)
    if( prob <= 0){
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else{
    prob <- pmvnorm(lower = c(-Inf ,-Inf),upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 0, theta[1:11]), 
                                                    profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 1, theta[12:22])), mean = c(0,0), corr) 
    
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3],  N_leg = 1, N_lcc = 0,  theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3],  N_leg = 0, N_lcc = 1,  theta[12:22])), 
                    upper = rep(Inf, 2), mean = c(0,0), corr)
    if ( prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
  }
  (ln_prob)
}
LL_o(5,theta_init)
LLo <- function(theta){
  
  ind_lik <- numeric(length(N_LEG))
  
  for (j in 1:length(N_LEG)){
    ind_lik[j] <- LL_o(j,theta)
  }
  
  LL <- sum(ind_lik)
  
  return(-LL)
}
LLo(theta_init)



#############Legacy carriers are leaders###########

####Without corr and With Corr LL_one#####
LL_onenocorr <- function(i,theta){
  
  if(N_LEG[i] == 4 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(-Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 0,theta[1:11]), 
                              Inf), mean = c(0,0))
    if ( prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 0,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 0,theta[1:11]), 
                              Inf), mean = c(0,0))
    if ( prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 0,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 0,theta[1:11]), 
                              Inf), mean = c(0,0))
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 0,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 0,theta[1:11]), 
                              Inf), mean = c(0,0))
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 3,theta[1:11]), 
                              -Inf),
                    upper = c(Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 3,theta[12:22])), mean = c(0,0)) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 3,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 3,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 3,theta[12:22])), mean = c(0,0))
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 3,theta[12:22])),
                    upper = c(Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 2,theta[12:22])), mean = c(0,0)) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 2,theta[12:22])), mean = c(0,0))
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 2,theta[12:22])),
                    upper = c(Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 1,theta[12:22])), mean = c(0,0)) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 0,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 1,theta[12:22])), mean = c(0,0))
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 4 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(-Inf, 
                              -Inf),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 2,theta[12:22])), mean = c(0,0))
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 4 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(-Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 1,theta[12:22])), mean = c(0,0))
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 4){
    prob <- pmvnorm(lower = c(-Inf, 
                              -Inf),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 4,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 4,theta[12:22])), mean = c(0,0))
    
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(-Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 4,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 3,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 3,theta[12:22])), mean = c(0,0))
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 3,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 2,theta[12:22])), mean = c(0,0))
    
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 1,theta[12:22])), mean = c(0,0))
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 0,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 1,theta[12:22])),
                    upper = c(Inf, 
                              Inf), mean = c(0,0))
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[12:22])), mean = c(0,0)) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 0,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[12:22])), mean = c(0,0))
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 3,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[12:22])), mean = c(0,0)) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[12:22])), mean = c(0,0))
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 3,theta[1:11]), 
                              -Inf),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 3,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 3,theta[12:22])), mean = c(0,0)) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 3,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 3,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 3,theta[12:22])), mean = c(0,0))
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[12:22])), mean = c(0,0)) - 
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 1,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 0,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[12:22])), mean = c(0,0))
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 3,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[12:22])), mean = c(0,0)) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 2,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[12:22])), mean = c(0,0))
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 3,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 4,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 3,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 3,theta[12:22])), mean = c(0,0)) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 3,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 3,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 3,theta[12:22])), mean = c(0,0))
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 4){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 4,theta[1:11]), 
                              -Inf),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 4,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 4,theta[12:22])), mean = c(0,0)) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 4,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 4,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 3,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 4,theta[12:22])), mean = c(0,0))
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }
  return(ln_prob)
}
LL_one <- function(i,theta){
  rho <- theta[23]
  corr <- diag(2)
  corr[lower.tri(corr)] <- rho
  corr[upper.tri(corr)] <- rho
  
  if(N_LEG[i] == 4 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(-Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 0,theta[1:11]), 
                              Inf), mean = c(0,0), corr)
    if ( prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 0,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 0,theta[1:11]), 
                              Inf), mean = c(0,0), corr)
    if ( prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 0,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 0,theta[1:11]), 
                              Inf), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 0,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 0,theta[1:11]), 
                              Inf), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 3,theta[1:11]), 
                              -Inf),
                    upper = c(Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 3,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 3,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 3,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 3,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 3,theta[12:22])),
                    upper = c(Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 2,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 2,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 2,theta[12:22])),
                    upper = c(Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 1,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 0,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 1,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 4 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(-Inf, 
                              -Inf),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 2,theta[12:22])), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 4 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(-Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 1,theta[12:22])), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 4){
    prob <- pmvnorm(lower = c(-Inf, 
                              -Inf),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 4,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 4,theta[12:22])), mean = c(0,0), corr)
    
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(-Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 4,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 3,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 3,theta[12:22])), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 3,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 2,theta[12:22])), mean = c(0,0), corr)
    
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 1,theta[12:22])), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 0,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lcc = 1,theta[12:22])),
                    upper = c(Inf, 
                              Inf), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 0,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 1,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 3,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 2,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 3,theta[1:11]), 
                              -Inf),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 3,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 3,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 3,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 3,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lcc = 3,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[12:22])), mean = c(0,0), corr) - 
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 1,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 0,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 1,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 3,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 2,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 2,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 3,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 4,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 3,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 3,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 3,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 3,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 3,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 4){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 4,theta[1:11]), 
                              -Inf),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 4,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 4,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 4,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 4,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lcc = 3,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lcc = 4,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }
  return(ln_prob)
}


####Without corr and With Corr LL function#####
LLnocorr <- function(theta){
  
  ind_liknocorr <- numeric(length(N_LEG))
  
  for (j in 1:length(N_LEG)){
    ind_liknocorr[j] <- LL_onenocorr(j,theta)
  }
  
  LL <- sum(ind_liknocorr)
  
  return(-LL)
}
LL <- function(theta){
  
  ind_lik <- numeric(length(N_LEG))
  
  for (j in 1:length(N_LEG)){
    ind_lik[j] <- LL_one(j,theta)
  }
  
  LL <- sum(ind_lik)
  
  return(-LL)
}

LLnocorr(theta_init)
LL(theta_init)

lower_bound <- c(rep(-Inf,22),-1)
upper_bound <- c(rep(Inf,22),1)

# Without correlation
opts <-list("algorithm"="NLOPT_LN_COBYLA","xtol_rel"=1.0e-4,maxeval=10000,"print_level"=2)
res <- nloptr(x0=theta_init,eval_f=LLnocorr,opts=opts,lb=lower_bound, ub=upper_bound) 
res$solution
#> res$solution
#[1] -0.4173655718  0.0446345899  0.5363285626 -3.3460083685  0.5489332167
#[6]  0.1387373676  0.0001314538 -0.7048994330 -2.5443264456 -2.5518693357
#[11] -2.1466951974  0.2631035153  0.0018709836  5.3261992243  0.2143733095
#[16]  1.7095476655  0.6225784588  0.0400404777  0.3827214928 -2.7638839028
#[21] -2.3444226428 -1.6151581713  0.1831293991


# Results without correlation 
betaleg_distance <- res$solution[1]
alphaleg <- res$solution[2]
betaleg_os <- res$solution[3]
lambdaleg <- c(res$solution[4],res$solution[4]+exp(res$solution[5]),res$solution[4]+exp(res$solution[5])+exp(res$solution[6]),res$solution[4]+exp(res$solution[5])+exp(res$solution[6])+exp(res$solution[7]))
gammaleg <- c(exp(res$solution[8]),exp(res$solution[8])+exp(res$solution[9]),exp(res$solution[8])+exp(res$solution[9])+exp(res$solution[10]),exp(res$solution[8])+exp(res$solution[9])+exp(res$solution[10])+exp(res$solution[11]))
betalcc_distance <- res$solution[12]
alphalcc <- res$solution[13]
betalcc_os <- res$solution[14]
lambdalcc <- c(res$solution[15],res$solution[15]+exp(res$solution[16]),res$solution[15]+exp(res$solution[16])+exp(res$solution[17]),res$solution[15]+exp(res$solution[16])+exp(res$solution[17])+exp(res$solution[18]))
gammalcc <- c(exp(res$solution[19]),exp(res$solution[19])+exp(res$solution[20]),exp(res$solution[19])+exp(res$solution[20])+exp(res$solution[21]),exp(res$solution[19])+exp(res$solution[20])+exp(res$solution[21])+exp(res$solution[22]))

# With correlation
res1 <- nloptr(x0=theta_init,eval_f=LL,opts=opts,lb=lower_bound, ub=upper_bound) 
res1$solution
#> res1$solution
#[1] -0.3571362068  0.0417198622  0.5924131416 -3.0754059110  0.5672172591
#[6]  0.1184864421 -0.0004102722 -0.3067384774 -2.3077637198 -2.0659049987
#[11] -1.5190450266  0.2702044153  0.0016858109  5.4851418711 -0.1251338299
#[16]  1.7717378155  0.6387982344  0.2096140625  0.6428428528 -2.3703941470
#[21] -2.0482381865  0.0450998902  0.3403157938


# Results with correlation
betaleg1_distance <- res1$solution[1]
alphaleg1 <- res1$solution[2]
betaleg1_os <- res1$solution[3]
lambdaleg1 <- c(res1$solution[4],res1$solution[4]+exp(res1$solution[5]),res1$solution[4]+exp(res1$solution[5])+exp(res1$solution[6]),res1$solution[4]+exp(res1$solution[5])+exp(res1$solution[6])+exp(res1$solution[7]))
gammaleg1 <- c(exp(res1$solution[8]),exp(res1$solution[8])+exp(res1$solution[9]),exp(res1$solution[8])+exp(res1$solution[9])+exp(res1$solution[10]),exp(res1$solution[8])+exp(res1$solution[9])+exp(res1$solution[10])+exp(res1$solution[11]))
betalcc1_distance <- res1$solution[12]
alphalcc1 <- res1$solution[13]
betalcc1_os <- res1$solution[14]
lambdalcc1 <- c(res1$solution[15],res1$solution[15]+exp(res1$solution[16]),res1$solution[15]+exp(res1$solution[16])+exp(res1$solution[17]),res1$solution[15]+exp(res1$solution[16])+exp(res1$solution[17])+exp(res1$solution[18]))
gammalcc1 <- c(exp(res1$solution[19]),exp(res1$solution[19])+exp(res1$solution[20]),exp(res1$solution[19])+exp(res1$solution[20])+exp(res1$solution[21]),exp(res1$solution[19])+exp(res1$solution[20])+exp(res1$solution[21])+exp(res1$solution[22]))
rho <- res1$solution[23]

hessian_res1 <- hessian(LL,res1$solution,method = "Richardson")
hessianinvres1 <- spdinv(hessian_res1)
transformation <- list(~x1,~x2,~x3,
                       ~x4,~x4+exp(x5),~x4+exp(x5)+exp(x6),~x4+exp(x5)+exp(x6)+exp(x7),
                       ~exp(x8),~exp(x8)+exp(x9),~exp(x8)+exp(x9)+exp(x10),
                       ~exp(x8)+exp(x9)+exp(x10)+exp(x11),
                       ~x12,~x13,~x14,
                       ~x15,~x15+exp(x16),~x15+exp(x16)+exp(x17),~x15+exp(x16)+exp(x17)+exp(x18),
                       ~exp(x19),~exp(x19)+exp(x20),~exp(x19)+exp(x20)+exp(x21),
                       ~exp(x19)+exp(x20)+exp(x21)+exp(x22),
                       ~x23)
library(msm)
hessianinvres1_corrected <- deltamethod(g = transformation, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
stderrors1 <- sqrt(diag(hessianinvres1_corrected))
#> stderrors1
#[1] 0.076960922 0.003122173 0.232753999 0.447250417 0.387756337 0.435272547
#[7] 0.471891988 0.880047900 0.873768556 0.898250979 1.005479893 0.102763323
#[13] 0.004182063 1.850720587 0.904850436 2.047813810 2.023094349 2.012716387
#[19] 0.553222391 0.557532894 0.599816088 0.693949285 0.081642884


######################################################################
# PART 3. Entry Threshold, counterfactual and inverse order of entry #
######################################################################

############### Entry Threshold Calculation ###################
# Note that we use msize (similar to log(pop)) as our outcome, since without logarithm some numbers will be too large.

# Threshold for LEG
Thresholdleg10 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4])/parares[2]
}

Thresholdleg11 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[8])/1)/parares[2]
}

Thresholdleg12 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+(exp(parares[8])+exp(parares[9]))/1)/parares[2]
}

Thresholdleg13 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+(exp(parares[8])+exp(parares[9])+exp(parares[10]))/1)/parares[2]
}

Thresholdleg14 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+(exp(parares[8])+exp(parares[9])+exp(parares[10])+exp(parares[11]))/1)/parares[2]
}

Thresholdleg20 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[5]))/parares[2]
}

Thresholdleg21 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[5])+exp(parares[8])/2)/parares[2]
}

Thresholdleg22 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[5])+(exp(parares[8])+exp(parares[9]))/2)/parares[2]
}

Thresholdleg23 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[5])+(exp(parares[8])+exp(parares[9])+exp(parares[10]))/2)/parares[2]
}

Thresholdleg24 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[5])+(exp(parares[8])+exp(parares[9])+exp(parares[10])+exp(parares[11]))/2)/parares[2]
}

Thresholdleg30 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[5])+exp(parares[6]))/parares[2]
}

Thresholdleg31 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[5])+exp(parares[6])+exp(parares[8])/3)/parares[2]
}

Thresholdleg32 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[5])+exp(parares[6])+(exp(parares[8])+exp(parares[9]))/3)/parares[2]
}

Thresholdleg33 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[5])+exp(parares[6])+(exp(parares[8])+exp(parares[9])+exp(parares[10]))/3)/parares[2]
}

Thresholdleg34 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[5])+exp(parares[6])+(exp(parares[8])+exp(parares[9])+exp(parares[10])+exp(parares[11]))/3)/parares[2]
}

Thresholdleg40 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[5])+exp(parares[6])+exp(parares[7]))/parares[2]
}

Thresholdleg41 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[5])+exp(parares[6])+exp(parares[7])+exp(parares[8])/4)/parares[2]
}

Thresholdleg42 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[5])+exp(parares[6])+exp(parares[7])+(exp(parares[8])+exp(parares[9]))/4)/parares[2]
}

Thresholdleg43 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[5])+exp(parares[6])+exp(parares[7])+(exp(parares[8])+exp(parares[9])+exp(parares[10]))/4)/parares[2]
}

Thresholdleg44 <- function(parares){
  (-parares[1]*mean(Xm[,1])-parares[3]*mean(Xm[,3])+parares[4]+exp(parares[5])+exp(parares[6])+exp(parares[7])+(exp(parares[8])+exp(parares[9])+exp(parares[10])+exp(parares[11]))/4)/parares[2]
}

pararesleg <- res1$solution

Thresholdleg10(pararesleg)
Thresholdleg11(pararesleg)
Thresholdleg12(pararesleg)
Thresholdleg13(pararesleg)
Thresholdleg14(pararesleg)
Thresholdleg20(pararesleg)
Thresholdleg21(pararesleg)
Thresholdleg22(pararesleg)
Thresholdleg23(pararesleg)
Thresholdleg24(pararesleg)
Thresholdleg30(pararesleg)
Thresholdleg31(pararesleg)
Thresholdleg32(pararesleg)
Thresholdleg33(pararesleg)
Thresholdleg34(pararesleg)
Thresholdleg40(pararesleg)
Thresholdleg41(pararesleg)
Thresholdleg42(pararesleg)
Thresholdleg43(pararesleg)
Thresholdleg44(pararesleg)

mx1 <- mean(Xm[,1])
mx2 <- mean(Xm[,2])
mx3 <- mean(Xm[,3])

transformation_Thresholdleg10 <- ~(-(x1)*mx1-(x3)*mx3+(x4))/(x2)
transformation_Thresholdleg11 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x8)/1)/(x2)
transformation_Thresholdleg12 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+(exp(x8)+exp(x9))/1)/(x2)
transformation_Thresholdleg13 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+(exp(x8)+exp(x9)+exp(x10))/1)/(x2)
transformation_Thresholdleg14 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+(exp(x8)+exp(x9)+exp(x10)+exp(x11))/1)/(x2)
transformation_Thresholdleg20 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x5))/(x2)
transformation_Thresholdleg21 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x5)+exp(x8)/2)/(x2)
transformation_Thresholdleg22 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x5)+(exp(x8)+exp(x9))/2)/(x2)
transformation_Thresholdleg23 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x5)+(exp(x8)+exp(x9)+exp(x10))/2)/(x2)
transformation_Thresholdleg24 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x5)+(exp(x8)+exp(x9)+exp(x10)+exp(x11))/2)/(x2)
transformation_Thresholdleg30 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x5)+exp(x6))/(x2)
transformation_Thresholdleg31 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x5)+exp(x6)+exp(x8)/3)/(x2)
transformation_Thresholdleg32 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x5)+exp(x6)+(exp(x8)+exp(x9))/3)/(x2)
transformation_Thresholdleg33 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x5)+exp(x6)+(exp(x8)+exp(x9)+exp(x10))/3)/(x2)
transformation_Thresholdleg34 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x5)+exp(x6)+(exp(x8)+exp(x9)+exp(x10)+exp(x11))/3)/(x2)
transformation_Thresholdleg40 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x5)+exp(x6)+exp(x7))/(x2)
transformation_Thresholdleg41 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x5)+exp(x6)+exp(x7)+exp(x8)/4)/(x2)
transformation_Thresholdleg42 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x5)+exp(x6)+exp(x7)+(exp(x8)+exp(x9))/4)/(x2)
transformation_Thresholdleg43 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x5)+exp(x6)+exp(x7)+(exp(x8)+exp(x9)+exp(x10))/4)/(x2)
transformation_Thresholdleg44 <- ~(-(x1)*mx1-(x3)*mx3+(x4)+exp(x5)+exp(x6)+exp(x7)+(exp(x8)+exp(x9)+exp(x10)+exp(x11))/4)/(x2)


hessianinv_Thresholdleg10corrected <- deltamethod(g = transformation_Thresholdleg10, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg11corrected <- deltamethod(g = transformation_Thresholdleg11, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg12corrected <- deltamethod(g = transformation_Thresholdleg12, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg13corrected <- deltamethod(g = transformation_Thresholdleg13, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg14corrected <- deltamethod(g = transformation_Thresholdleg14, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg20corrected <- deltamethod(g = transformation_Thresholdleg20, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg21corrected <- deltamethod(g = transformation_Thresholdleg21, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg22corrected <- deltamethod(g = transformation_Thresholdleg22, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg23corrected <- deltamethod(g = transformation_Thresholdleg23, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg24corrected <- deltamethod(g = transformation_Thresholdleg24, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg30corrected <- deltamethod(g = transformation_Thresholdleg30, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg31corrected <- deltamethod(g = transformation_Thresholdleg31, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg32corrected <- deltamethod(g = transformation_Thresholdleg32, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg33corrected <- deltamethod(g = transformation_Thresholdleg33, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg34corrected <- deltamethod(g = transformation_Thresholdleg34, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg40corrected <- deltamethod(g = transformation_Thresholdleg40, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg41corrected <- deltamethod(g = transformation_Thresholdleg41, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg42corrected <- deltamethod(g = transformation_Thresholdleg42, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg43corrected <- deltamethod(g = transformation_Thresholdleg43, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdleg44corrected <- deltamethod(g = transformation_Thresholdleg44, mean = res1$solution, cov = hessianinvres1, ses = FALSE )


std_errors_Thresholdleg10 <- sqrt(diag(hessianinv_Thresholdleg10corrected))
std_errors_Thresholdleg11 <- sqrt(diag(hessianinv_Thresholdleg11corrected))
std_errors_Thresholdleg12 <- sqrt(diag(hessianinv_Thresholdleg12corrected))
std_errors_Thresholdleg13 <- sqrt(diag(hessianinv_Thresholdleg13corrected))
std_errors_Thresholdleg14 <- sqrt(diag(hessianinv_Thresholdleg14corrected))
std_errors_Thresholdleg20 <- sqrt(diag(hessianinv_Thresholdleg20corrected))
std_errors_Thresholdleg21 <- sqrt(diag(hessianinv_Thresholdleg21corrected))
std_errors_Thresholdleg22 <- sqrt(diag(hessianinv_Thresholdleg22corrected))
std_errors_Thresholdleg23 <- sqrt(diag(hessianinv_Thresholdleg23corrected))
std_errors_Thresholdleg24 <- sqrt(diag(hessianinv_Thresholdleg24corrected))
std_errors_Thresholdleg30 <- sqrt(diag(hessianinv_Thresholdleg30corrected))
std_errors_Thresholdleg31 <- sqrt(diag(hessianinv_Thresholdleg31corrected))
std_errors_Thresholdleg32 <- sqrt(diag(hessianinv_Thresholdleg32corrected))
std_errors_Thresholdleg33 <- sqrt(diag(hessianinv_Thresholdleg33corrected))
std_errors_Thresholdleg34 <- sqrt(diag(hessianinv_Thresholdleg34corrected))
std_errors_Thresholdleg40 <- sqrt(diag(hessianinv_Thresholdleg40corrected))
std_errors_Thresholdleg41 <- sqrt(diag(hessianinv_Thresholdleg41corrected))
std_errors_Thresholdleg42 <- sqrt(diag(hessianinv_Thresholdleg42corrected))
std_errors_Thresholdleg43 <- sqrt(diag(hessianinv_Thresholdleg43corrected))
std_errors_Thresholdleg44 <- sqrt(diag(hessianinv_Thresholdleg44corrected))

#Confindence Interval for legacy carriers' entry threshold
c(Thresholdleg10(pararesleg)-1.96*std_errors_Thresholdleg10,Thresholdleg10(pararesleg)+1.96*std_errors_Thresholdleg10)
c(Thresholdleg11(pararesleg)-1.96*std_errors_Thresholdleg11,Thresholdleg11(pararesleg)+1.96*std_errors_Thresholdleg11)
c(Thresholdleg12(pararesleg)-1.96*std_errors_Thresholdleg12,Thresholdleg12(pararesleg)+1.96*std_errors_Thresholdleg12)
c(Thresholdleg13(pararesleg)-1.96*std_errors_Thresholdleg13,Thresholdleg13(pararesleg)+1.96*std_errors_Thresholdleg13)
c(Thresholdleg14(pararesleg)-1.96*std_errors_Thresholdleg14,Thresholdleg14(pararesleg)+1.96*std_errors_Thresholdleg14)
c(Thresholdleg20(pararesleg)-1.96*std_errors_Thresholdleg20,Thresholdleg20(pararesleg)+1.96*std_errors_Thresholdleg20)
c(Thresholdleg21(pararesleg)-1.96*std_errors_Thresholdleg21,Thresholdleg21(pararesleg)+1.96*std_errors_Thresholdleg21)
c(Thresholdleg22(pararesleg)-1.96*std_errors_Thresholdleg22,Thresholdleg22(pararesleg)+1.96*std_errors_Thresholdleg22)
c(Thresholdleg23(pararesleg)-1.96*std_errors_Thresholdleg23,Thresholdleg23(pararesleg)+1.96*std_errors_Thresholdleg23)
c(Thresholdleg24(pararesleg)-1.96*std_errors_Thresholdleg24,Thresholdleg24(pararesleg)+1.96*std_errors_Thresholdleg24)
c(Thresholdleg30(pararesleg)-1.96*std_errors_Thresholdleg30,Thresholdleg30(pararesleg)+1.96*std_errors_Thresholdleg30)
c(Thresholdleg31(pararesleg)-1.96*std_errors_Thresholdleg31,Thresholdleg31(pararesleg)+1.96*std_errors_Thresholdleg31)
c(Thresholdleg32(pararesleg)-1.96*std_errors_Thresholdleg32,Thresholdleg32(pararesleg)+1.96*std_errors_Thresholdleg32)
c(Thresholdleg33(pararesleg)-1.96*std_errors_Thresholdleg33,Thresholdleg33(pararesleg)+1.96*std_errors_Thresholdleg33)
c(Thresholdleg34(pararesleg)-1.96*std_errors_Thresholdleg34,Thresholdleg34(pararesleg)+1.96*std_errors_Thresholdleg34)
c(Thresholdleg40(pararesleg)-1.96*std_errors_Thresholdleg40,Thresholdleg40(pararesleg)+1.96*std_errors_Thresholdleg40)
c(Thresholdleg41(pararesleg)-1.96*std_errors_Thresholdleg41,Thresholdleg41(pararesleg)+1.96*std_errors_Thresholdleg41)
c(Thresholdleg42(pararesleg)-1.96*std_errors_Thresholdleg42,Thresholdleg42(pararesleg)+1.96*std_errors_Thresholdleg42)
c(Thresholdleg43(pararesleg)-1.96*std_errors_Thresholdleg43,Thresholdleg43(pararesleg)+1.96*std_errors_Thresholdleg43)
c(Thresholdleg44(pararesleg)-1.96*std_errors_Thresholdleg44,Thresholdleg44(pararesleg)+1.96*std_errors_Thresholdleg44)

# Threshold for LCC
Thresholdlcc10 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15])/parares[13]
}

Thresholdlcc11 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[19])/1)/parares[13]
}

Thresholdlcc12 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+(exp(parares[19])+exp(parares[20]))/1)/parares[13]
}

Thresholdlcc13 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+(exp(parares[19])+exp(parares[20])+exp(parares[21]))/1)/parares[13]
}

Thresholdlcc14 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+(exp(parares[19])+exp(parares[20])+exp(parares[21])+exp(parares[22]))/1)/parares[13]
}

Thresholdlcc20 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[16]))/parares[13]
}

Thresholdlcc21 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[16])+exp(parares[19])/2)/parares[13]
}

Thresholdlcc22 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[16])+(exp(parares[19])+exp(parares[20]))/2)/parares[13]
}

Thresholdlcc23 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[16])+(exp(parares[19])+exp(parares[20])+exp(parares[21]))/2)/parares[13]
}

Thresholdlcc24 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[16])+(exp(parares[19])+exp(parares[20])+exp(parares[21])+exp(parares[22]))/2)/parares[13]
}

Thresholdlcc30 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[16])+exp(parares[17]))/parares[13]
}

Thresholdlcc31 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[16])+exp(parares[17])+exp(parares[19])/3)/parares[13]
}

Thresholdlcc32 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[16])+exp(parares[17])+(exp(parares[19])+exp(parares[20]))/3)/parares[13]
}

Thresholdlcc33 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[16])+exp(parares[17])+(exp(parares[19])+exp(parares[20])+exp(parares[21]))/3)/parares[13]
}

Thresholdlcc34 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[16])+exp(parares[17])+(exp(parares[19])+exp(parares[20])+exp(parares[21])+exp(parares[22]))/3)/parares[13]
}

Thresholdlcc40 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[16])+exp(parares[17])+exp(parares[18]))/parares[13]
}

Thresholdlcc41 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[16])+exp(parares[17])+exp(parares[18])+exp(parares[19])/4)/parares[13]
}

Thresholdlcc42 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[16])+exp(parares[17])+exp(parares[18])+(exp(parares[19])+exp(parares[20]))/4)/parares[13]
}

Thresholdlcc43 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[16])+exp(parares[17])+exp(parares[18])+(exp(parares[19])+exp(parares[20])+exp(parares[21]))/4)/parares[13]
}

Thresholdlcc44 <- function(parares){
  (-parares[12]*mean(Xm[,1])-parares[14]*mean(Xm[,3])+parares[15]+exp(parares[16])+exp(parares[17])+exp(parares[18])+(exp(parares[19])+exp(parares[20])+exp(parares[21])+exp(parares[22]))/4)/parares[13]
}

parareslcc <- res1$solution

Thresholdlcc10(parareslcc)
Thresholdlcc11(parareslcc)
Thresholdlcc12(parareslcc)
Thresholdlcc13(parareslcc)
Thresholdlcc14(parareslcc)
Thresholdlcc20(parareslcc)
Thresholdlcc21(parareslcc)
Thresholdlcc22(parareslcc)
Thresholdlcc23(parareslcc)
Thresholdlcc24(parareslcc)
Thresholdlcc30(parareslcc)
Thresholdlcc31(parareslcc)
Thresholdlcc32(parareslcc)
Thresholdlcc33(parareslcc)
Thresholdlcc34(parareslcc)
Thresholdlcc40(parareslcc)
Thresholdlcc41(parareslcc)
Thresholdlcc42(parareslcc)
Thresholdlcc43(parareslcc)
Thresholdlcc44(parareslcc)

transformation_Thresholdlcc10 <- ~(-(x12)*mx1-(x14)*mx3+(x15))/(x13)
transformation_Thresholdlcc11 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x19)/1)/(x13)
transformation_Thresholdlcc12 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+(exp(x19)+exp(x20))/1)/(x13)
transformation_Thresholdlcc13 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+(exp(x19)+exp(x20)+exp(x21))/1)/(x13)
transformation_Thresholdlcc14 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+(exp(x19)+exp(x20)+exp(x21)+exp(x22))/1)/(x13)
transformation_Thresholdlcc20 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x16))/(x13)
transformation_Thresholdlcc21 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x16)+exp(x19)/2)/(x13)
transformation_Thresholdlcc22 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x16)+(exp(x19)+exp(x20))/2)/(x13)
transformation_Thresholdlcc23 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x16)+(exp(x19)+exp(x20)+exp(x21))/2)/(x13)
transformation_Thresholdlcc24 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x16)+(exp(x19)+exp(x20)+exp(x21)+exp(x22))/2)/(x13)
transformation_Thresholdlcc30 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x16)+exp(x17))/(x13)
transformation_Thresholdlcc31 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x16)+exp(x17)+exp(x19)/3)/(x13)
transformation_Thresholdlcc32 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x16)+exp(x17)+(exp(x19)+exp(x20))/3)/(x13)
transformation_Thresholdlcc33 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x16)+exp(x17)+(exp(x19)+exp(x20)+exp(x21))/3)/(x13)
transformation_Thresholdlcc34 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x16)+exp(x17)+(exp(x19)+exp(x20)+exp(x21)+exp(x22))/3)/(x13)
transformation_Thresholdlcc40 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x16)+exp(x17)+exp(x18))/(x13)
transformation_Thresholdlcc41 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x16)+exp(x17)+exp(x18)+exp(x19)/4)/(x13)
transformation_Thresholdlcc42 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x16)+exp(x17)+exp(x18)+(exp(x19)+exp(x20))/4)/(x13)
transformation_Thresholdlcc43 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x16)+exp(x17)+exp(x18)+(exp(x19)+exp(x20)+exp(x21))/4)/(x13)
transformation_Thresholdlcc44 <- ~(-(x12)*mx1-(x14)*mx3+(x15)+exp(x16)+exp(x17)+exp(x18)+(exp(x19)+exp(x20)+exp(x21)+exp(x22))/4)/(x13)


hessianinv_Thresholdlcc10corrected <- deltamethod(g = transformation_Thresholdlcc10, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc11corrected <- deltamethod(g = transformation_Thresholdlcc11, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc12corrected <- deltamethod(g = transformation_Thresholdlcc12, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc13corrected <- deltamethod(g = transformation_Thresholdlcc13, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc14corrected <- deltamethod(g = transformation_Thresholdlcc14, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc20corrected <- deltamethod(g = transformation_Thresholdlcc20, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc21corrected <- deltamethod(g = transformation_Thresholdlcc21, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc22corrected <- deltamethod(g = transformation_Thresholdlcc22, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc23corrected <- deltamethod(g = transformation_Thresholdlcc23, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc24corrected <- deltamethod(g = transformation_Thresholdlcc24, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc30corrected <- deltamethod(g = transformation_Thresholdlcc30, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc31corrected <- deltamethod(g = transformation_Thresholdlcc31, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc32corrected <- deltamethod(g = transformation_Thresholdlcc32, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc33corrected <- deltamethod(g = transformation_Thresholdlcc33, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc34corrected <- deltamethod(g = transformation_Thresholdlcc34, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc40corrected <- deltamethod(g = transformation_Thresholdlcc40, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc41corrected <- deltamethod(g = transformation_Thresholdlcc41, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc42corrected <- deltamethod(g = transformation_Thresholdlcc42, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc43corrected <- deltamethod(g = transformation_Thresholdlcc43, mean = res1$solution, cov = hessianinvres1, ses = FALSE )
hessianinv_Thresholdlcc44corrected <- deltamethod(g = transformation_Thresholdlcc44, mean = res1$solution, cov = hessianinvres1, ses = FALSE )


std_errors_Thresholdlcc10 <- sqrt(diag(hessianinv_Thresholdlcc10corrected))
std_errors_Thresholdlcc11 <- sqrt(diag(hessianinv_Thresholdlcc11corrected))
std_errors_Thresholdlcc12 <- sqrt(diag(hessianinv_Thresholdlcc12corrected))
std_errors_Thresholdlcc13 <- sqrt(diag(hessianinv_Thresholdlcc13corrected))
std_errors_Thresholdlcc14 <- sqrt(diag(hessianinv_Thresholdlcc14corrected))
std_errors_Thresholdlcc20 <- sqrt(diag(hessianinv_Thresholdlcc20corrected))
std_errors_Thresholdlcc21 <- sqrt(diag(hessianinv_Thresholdlcc21corrected))
std_errors_Thresholdlcc22 <- sqrt(diag(hessianinv_Thresholdlcc22corrected))
std_errors_Thresholdlcc23 <- sqrt(diag(hessianinv_Thresholdlcc23corrected))
std_errors_Thresholdlcc24 <- sqrt(diag(hessianinv_Thresholdlcc24corrected))
std_errors_Thresholdlcc30 <- sqrt(diag(hessianinv_Thresholdlcc30corrected))
std_errors_Thresholdlcc31 <- sqrt(diag(hessianinv_Thresholdlcc31corrected))
std_errors_Thresholdlcc32 <- sqrt(diag(hessianinv_Thresholdlcc32corrected))
std_errors_Thresholdlcc33 <- sqrt(diag(hessianinv_Thresholdlcc33corrected))
std_errors_Thresholdlcc34 <- sqrt(diag(hessianinv_Thresholdlcc34corrected))
std_errors_Thresholdlcc40 <- sqrt(diag(hessianinv_Thresholdlcc40corrected))
std_errors_Thresholdlcc41 <- sqrt(diag(hessianinv_Thresholdlcc41corrected))
std_errors_Thresholdlcc42 <- sqrt(diag(hessianinv_Thresholdlcc42corrected))
std_errors_Thresholdlcc43 <- sqrt(diag(hessianinv_Thresholdlcc43corrected))
std_errors_Thresholdlcc44 <- sqrt(diag(hessianinv_Thresholdlcc44corrected))

#Confindence Interval for lcc carriers' entry threshold
c(Thresholdlcc10(parareslcc)-1.96*std_errors_Thresholdlcc10,Thresholdlcc10(parareslcc)+1.96*std_errors_Thresholdlcc10)
c(Thresholdlcc11(parareslcc)-1.96*std_errors_Thresholdlcc11,Thresholdlcc11(parareslcc)+1.96*std_errors_Thresholdlcc11)
c(Thresholdlcc12(parareslcc)-1.96*std_errors_Thresholdlcc12,Thresholdlcc12(parareslcc)+1.96*std_errors_Thresholdlcc12)
c(Thresholdlcc13(parareslcc)-1.96*std_errors_Thresholdlcc13,Thresholdlcc13(parareslcc)+1.96*std_errors_Thresholdlcc13)
c(Thresholdlcc14(parareslcc)-1.96*std_errors_Thresholdlcc14,Thresholdlcc14(parareslcc)+1.96*std_errors_Thresholdlcc14)
c(Thresholdlcc20(parareslcc)-1.96*std_errors_Thresholdlcc20,Thresholdlcc20(parareslcc)+1.96*std_errors_Thresholdlcc20)
c(Thresholdlcc21(parareslcc)-1.96*std_errors_Thresholdlcc21,Thresholdlcc21(parareslcc)+1.96*std_errors_Thresholdlcc21)
c(Thresholdlcc22(parareslcc)-1.96*std_errors_Thresholdlcc22,Thresholdlcc22(parareslcc)+1.96*std_errors_Thresholdlcc22)
c(Thresholdlcc23(parareslcc)-1.96*std_errors_Thresholdlcc23,Thresholdlcc23(parareslcc)+1.96*std_errors_Thresholdlcc23)
c(Thresholdlcc24(parareslcc)-1.96*std_errors_Thresholdlcc24,Thresholdlcc24(parareslcc)+1.96*std_errors_Thresholdlcc24)
c(Thresholdlcc30(parareslcc)-1.96*std_errors_Thresholdlcc30,Thresholdlcc30(parareslcc)+1.96*std_errors_Thresholdlcc30)
c(Thresholdlcc31(parareslcc)-1.96*std_errors_Thresholdlcc31,Thresholdlcc31(parareslcc)+1.96*std_errors_Thresholdlcc31)
c(Thresholdlcc32(parareslcc)-1.96*std_errors_Thresholdlcc32,Thresholdlcc32(parareslcc)+1.96*std_errors_Thresholdlcc32)
c(Thresholdlcc33(parareslcc)-1.96*std_errors_Thresholdlcc33,Thresholdlcc33(parareslcc)+1.96*std_errors_Thresholdlcc33)
c(Thresholdlcc34(parareslcc)-1.96*std_errors_Thresholdlcc34,Thresholdlcc34(parareslcc)+1.96*std_errors_Thresholdlcc34)
c(Thresholdlcc40(parareslcc)-1.96*std_errors_Thresholdlcc40,Thresholdlcc40(parareslcc)+1.96*std_errors_Thresholdlcc40)
c(Thresholdlcc41(parareslcc)-1.96*std_errors_Thresholdlcc41,Thresholdlcc41(parareslcc)+1.96*std_errors_Thresholdlcc41)
c(Thresholdlcc42(parareslcc)-1.96*std_errors_Thresholdlcc42,Thresholdlcc42(parareslcc)+1.96*std_errors_Thresholdlcc42)
c(Thresholdlcc43(parareslcc)-1.96*std_errors_Thresholdlcc43,Thresholdlcc43(parareslcc)+1.96*std_errors_Thresholdlcc43)
c(Thresholdlcc44(parareslcc)-1.96*std_errors_Thresholdlcc44,Thresholdlcc44(parareslcc)+1.96*std_errors_Thresholdlcc44)

#################Counterfactual Analysis###################

#Assume all the distance are reduced by 20%
Xm2=as.matrix(cbind(DATA$distance*0.8, DATA$msize, DATA$OS))
LL_onecounter <- function(i,theta){
  rho <- theta[23]
  corr <- diag(2)
  corr[lower.tri(corr)] <- rho
  corr[upper.tri(corr)] <- rho
  
  if(N_LEG[i] == 4 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(-Inf, 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 4, N_lcc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 4, N_lcc = 0,theta[1:11]), 
                              Inf), mean = c(0,0), corr)
    if ( prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 4, N_lcc = 0,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 0,theta[1:11]), 
                              Inf), mean = c(0,0), corr)
    if ( prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 0,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 0,theta[1:11]), 
                              Inf), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 0,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 0,theta[1:11]), 
                              Inf), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 3,theta[1:11]), 
                              -Inf),
                    upper = c(Inf, 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 0, N_lcc = 3,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 3,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 3,theta[12:22])),
              upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 0, N_lcc = 3,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 0, N_lcc = 3,theta[12:22])),
                    upper = c(Inf, 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 0, N_lcc = 2,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 2,theta[12:22])),
              upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 0, N_lcc = 2,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 0, N_lcc = 2,theta[12:22])),
                    upper = c(Inf, 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 0, N_lcc = 1,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 1,theta[12:22])),
              upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 0,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 0, N_lcc = 1,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 4 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(-Inf, 
                              -Inf),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 4, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 4, N_lcc = 2,theta[12:22])), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 4 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(-Inf, 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 4, N_lcc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 4, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 4, N_lcc = 1,theta[12:22])), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 4){
    prob <- pmvnorm(lower = c(-Inf, 
                              -Inf),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 4,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 4,theta[12:22])), mean = c(0,0), corr)
    
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(-Inf, 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 4,theta[12:22])),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 3,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 3,theta[12:22])), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 4, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 3,theta[12:22])),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 2,theta[12:22])), mean = c(0,0), corr)
    
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 4, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 1,theta[12:22])), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 0,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 0, N_lcc = 1,theta[12:22])),
                    upper = c(Inf, 
                              Inf), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 1,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 1,theta[12:22])),
              upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 0,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 1,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 3,theta[12:22])),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 2,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 2,theta[12:22])),
              upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 2,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 3,theta[1:11]), 
                              -Inf),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 3,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 3,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 3,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 3,theta[12:22])),
              upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 1, N_lcc = 3,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 1,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 1,theta[12:22])), mean = c(0,0), corr) - 
      
      pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 1,theta[12:22])),
              upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 0,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 1,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 3,theta[12:22])),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 2,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 2,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 2,theta[12:22])),
              upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 1,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 2,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 3,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 4,theta[12:22])),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 3,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 3,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 3,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 3,theta[12:22])),
              upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 2,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 3,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 4){
    prob <- pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 4,theta[1:11]), 
                              -Inf),
                    upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 4,theta[1:11]), 
                              profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 4,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 4,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 4,theta[12:22])),
              upper = c(profit_legacy(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 3, N_lcc = 3,theta[1:11]), 
                        profit_lowcost(Xm2[i,1],Xm2[i,2],Xm2[i,3], N_leg = 2, N_lcc = 4,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }
  return(ln_prob)
}
LLcounter <- function(theta){
  
  ind_likc <- numeric(length(N_LEG))
  
  for (j in 1:length(N_LEG)){
    ind_likc[j] <- LL_onecounter(j,theta)
  }
  
  LLc <- sum(ind_likc)
  
  return(-LLc)
}
LLcounter(theta_init)

rescounter <- nloptr(x0=theta_init,eval_f=LLcounter,opts=opts,lb=lower_bound, ub=upper_bound) 
rescounter$solution
#> rescounter$solution
#[1] -0.4695898796  0.0447939481  0.4731085713 -3.6554515838  0.7179192728  0.1309541781
#[7] -0.0006179758  0.2180348600 -2.5723783217 -2.1519977803 -1.7323047575  0.3518157027
#[13]  0.0021811854  8.9141312582  0.9357216400  2.1853875875  0.5404763922  0.0323287308
#[19] -0.3592379972 -2.1297567977 -2.0576791816 -1.6126948199  0.2019319126

# Results with correlation of counterfactual
betalegcounter_distance <- rescounter$solution[1]
alphalegcounter <- rescounter$solution[2]
betalegcounter_os <- rescounter$solution[3]
lambdalegcounter <- c(rescounter$solution[4],rescounter$solution[4]+exp(rescounter$solution[5]),rescounter$solution[4]+exp(rescounter$solution[5])+exp(rescounter$solution[6]),rescounter$solution[4]+exp(rescounter$solution[5])+exp(rescounter$solution[6])+exp(rescounter$solution[7]))
gammalegcounter <- c(exp(rescounter$solution[8]),exp(rescounter$solution[8])+exp(rescounter$solution[9]),exp(rescounter$solution[8])+exp(rescounter$solution[9])+exp(rescounter$solution[10]),exp(rescounter$solution[8])+exp(rescounter$solution[9])+exp(rescounter$solution[10])+exp(rescounter$solution[11]))
betalcccounter_distance <- rescounter$solution[12]
alphalcccounter <- rescounter$solution[13]
betalcccounter_os <- rescounter$solution[14]
lambdalcccounter <- c(rescounter$solution[15],rescounter$solution[15]+exp(rescounter$solution[16]),rescounter$solution[15]+exp(rescounter$solution[16])+exp(rescounter$solution[17]),rescounter$solution[15]+exp(rescounter$solution[16])+exp(rescounter$solution[17])+exp(rescounter$solution[18]))
gammalcccounter <- c(exp(rescounter$solution[19]),exp(rescounter$solution[19])+exp(rescounter$solution[20]),exp(rescounter$solution[19])+exp(rescounter$solution[20])+exp(rescounter$solution[21]),exp(rescounter$solution[19])+exp(rescounter$solution[20])+exp(rescounter$solution[21])+exp(rescounter$solution[22]))
rhocounter <- rescounter$solution[23]

# se of counterfactual
hessian_rescounter <- hessian(LLcounter,rescounter$solution,method = "Richardson")
hessianinvrescounter <- solve(hessian_rescounter)
hessianinvrescounter_corrected <- deltamethod(g = transformation, mean = rescounter$solution, cov = hessianinvrescounter, ses = FALSE )
stderrorscounter <- sqrt(diag(hessianinvrescounter_corrected))
#[1] 0.072215660 0.003041122 0.095135459 0.385607711 0.380037720 0.382491381 0.394866139
#[8] 0.058542839 0.072124524 0.127018001 0.335009517 0.075559731 0.002584861 2.207641327
#[15] 0.321885076 2.229670253 2.236486705 2.243129202         NaN         NaN         NaN
#[22]         NaN 0.079502191



#################Inverse the order of enter###################
# Change the order of entry
LL_onechange <- function(i,theta){
  rho <- theta[23]
  corr <- diag(2)
  corr[lower.tri(corr)] <- rho
  corr[upper.tri(corr)] <- rho
  
  if(N_LEG[i] == 4 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(-Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 0,theta[1:11]), 
                              Inf), mean = c(0,0), corr) -
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 1,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 0,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 1,theta[12:22])), mean = c(0,0), corr)
    if ( prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 0,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 0,theta[1:11]), 
                              Inf), mean = c(0,0), corr) - 
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 1,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 0,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 1,theta[12:22])), mean = c(0,0), corr)
    if ( prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 0,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 0,theta[1:11]), 
                              Inf), mean = c(0,0), corr)- 
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 1,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 0,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 1,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 0,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 1,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 0,theta[1:11]), 
                              Inf), mean = c(0,0), corr)- 
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 1,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 0,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lc = 1,theta[12:22])), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 3,theta[1:11]), 
                              -Inf),
                    upper = c(Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lc = 3,theta[12:22])), mean = c(0,0), corr) 
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lc = 3,theta[12:22])),
                    upper = c(Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lc = 2,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lc = 2,theta[12:22])),
                    upper = c(Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lc = 1,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 4 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(-Inf, 
                              -Inf),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 2,theta[12:22])), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 4 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(-Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 1,theta[12:22])), mean = c(0,0), corr)-
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 2,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 2,theta[12:22])), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 4){
    prob <- pmvnorm(lower = c(-Inf, 
                              -Inf),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 4,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 4,theta[12:22])), mean = c(0,0), corr)
    
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(-Inf, 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 4,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 3,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 3,theta[12:22])), mean = c(0,0), corr)-
      
      pmvnorm(lower = c(profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 4,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 4,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 3,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 4,theta[12:22])), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 3,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 2,theta[12:22])), mean = c(0,0), corr)-
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 3,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 3,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 3,theta[12:22])), mean = c(0,0), corr)
    
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 3 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 4, N_lc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 1,theta[12:22])), mean = c(0,0), corr)-
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 2,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 2,theta[12:22])), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 0 & N_LCC[i] == 0){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 0,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lc = 1,theta[12:22])),
                    upper = c(Inf, 
                              Inf), mean = c(0,0), corr)
    
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 1,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 2,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lc = 2,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 3,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 2,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 3,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 3,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 0, N_lc = 3,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 1 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 3,theta[1:11]), 
                              -Inf),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 3,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 3,theta[12:22])), mean = c(0,0), corr) 
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 1){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 2,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 1,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 1,theta[12:22])), mean = c(0,0), corr) - 
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 2,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 1,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 2,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 2){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 3,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 2,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 2,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 3,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 3,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 2,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 1, N_lc = 3,theta[12:22])), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 3){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 3,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 4,theta[12:22])),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 3,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 3,theta[12:22])), mean = c(0,0), corr) -
      
      pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 4,theta[1:11]), 
                        profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 4,theta[12:22])),
              upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 3,theta[1:11]), 
                        Inf), mean = c(0,0), corr)
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }else if(N_LEG[i] == 2 & N_LCC[i] == 4){
    prob <- pmvnorm(lower = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 3, N_lc = 4,theta[1:11]), 
                              -Inf),
                    upper = c(profit_legacy(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 4,theta[1:11]), 
                              profit_lowcost(Xm[i,1],Xm[i,2],Xm[i,3], N_leg = 2, N_lc = 4,theta[12:22])), mean = c(0,0), corr) 
    if (prob <= 0) {
      prob <- 1e-40
    }
    ln_prob <- log(prob)
    
  }
  return(ln_prob)
}
LLchange <- function(theta){
  
  ind_likch <- numeric(length(N_LEG))
  
  for (j in 1:length(N_LEG)){
    ind_likch[j] <- LL_onechange(j,theta)
  }
  
  LLch <- sum(ind_likch)
  
  return(-LLch)
}
LLchange(theta_init)
reschange <- nloptr(x0=theta_init,eval_f=LLchange,opts=opts,lb=lower_bound, ub=upper_bound) 
reschange$solution
#> reschange$solution
#[1] -0.3882622487  0.0423603384  0.4922178318 -3.3049200103  0.6191020098
#[6]  0.1296158211  0.0006600105 -0.8417342822 -2.2126868419 -2.7159166416
#[11] -3.5489292683  0.2594180174  0.0017652688  5.2673188025 -0.0402397345
#[16]  1.7002690652  0.6314677127  0.2529137003  0.6727483323 -2.5739486337
#[21] -2.0183306210 -0.2240544546  0.3125452887

# Results with correlation of changing order
betalegchange_distance <- reschange$solution[1]
alphalegchange <- reschange$solution[2]
betalegchange_os <- reschange$solution[3]
lambdalegchange <- c(reschange$solution[4],reschange$solution[4]+exp(reschange$solution[5]),reschange$solution[4]+exp(reschange$solution[5])+exp(reschange$solution[6]),reschange$solution[4]+exp(reschange$solution[5])+exp(reschange$solution[6])+exp(reschange$solution[7]))
gammalegchange <- c(exp(reschange$solution[8]),exp(reschange$solution[8])+exp(reschange$solution[9]),exp(reschange$solution[8])+exp(reschange$solution[9])+exp(reschange$solution[10]),exp(reschange$solution[8])+exp(reschange$solution[9])+exp(reschange$solution[10])+exp(reschange$solution[11]))
betalccchange_distance <- reschange$solution[12]
alphalccchange <- reschange$solution[13]
betalccchange_os <- reschange$solution[14]
lambdalccchange <- c(reschange$solution[15],reschange$solution[15]+exp(reschange$solution[16]),reschange$solution[15]+exp(reschange$solution[16])+exp(reschange$solution[17]),reschange$solution[15]+exp(reschange$solution[16])+exp(reschange$solution[17])+exp(reschange$solution[18]))
gammalccchange <- c(exp(reschange$solution[19]),exp(reschange$solution[19])+exp(reschange$solution[20]),exp(reschange$solution[19])+exp(reschange$solution[20])+exp(reschange$solution[21]),exp(reschange$solution[19])+exp(reschange$solution[20])+exp(reschange$solution[21])+exp(reschange$solution[22]))
rhochange <- reschange$solution[23]


# se of changing order
hessian_reschange <- hessian(LLchange,reschange$solution,method = "Richardson")
hessianinvreschange <- spdinv(hessian_reschange)
hessianinvreschange_corrected <- deltamethod(g = transformation, mean = reschange$solution, cov = hessianinvreschange, ses = FALSE )
stderrorschange <- sqrt(diag(hessianinvreschange_corrected))
# [1] 0.058630922 0.003129096 0.136320877 0.479268336 0.372984856 0.375840740 0.391268186
#[8] 0.422288820 0.476865057 0.492971063 0.493625161 0.071993682 0.002979982 2.527747102
#[15] 0.647031647 2.583355978 2.586231105 2.589493317 0.331324622 0.348096507 0.376360184
#[22] 0.685192881 0.083393171