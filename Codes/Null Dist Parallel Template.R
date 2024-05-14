library(tidyverse)
library(tictoc)
library(httr)
library(parallel)
library(foreach)
library(doParallel)

# Code notification with Alertzy to alert when simulations finish
url <- "https://alertzy.app/send"
res <- httr::handle(url)
config(res, ssl_verifypeer = TRUE)
key<-# Insert your user key on your preferable device to get direct notifications
  # when the null simulations finish

## Replace AixBjxK for settings and NumReps for the number of replications per cell ##

# Null Distribution Mean & Variance ----
## 250k Sim ----
tic("Null AixBjxK_250k Sim")  # This is to record how long the code runs
#### Null Matrices ----
cl <- parallel::makeCluster(detectCores()-1)
registerDoParallel(cl)
# Parallelized foreach loop
I <- Ai
J <- Bj
K <- NumReps
numTrial <- 250000
APCnull<- NULL
APCnull <- foreach(i = 1:numTrial) %dopar% {
  nullData <- data.frame(value = rnorm(I * J * K),
                         A = rep(1:I, each = K, times = J),
                         B = rep(1:J, each = I * K)
  )
  APCnull[[i]] <- nullData
}
# Stop the cluster
stopCluster(cl)

#### Null Mean & Variance ----
cl <- parallel::makeCluster(detectCores()-1)
registerDoParallel(cl)
nullDistCRA <- unlist(parLapply(cl, APCnull,APCCRADts), use.names = FALSE) 
nullDistRCA <- unlist(parLapply(cl, APCnull,APCRCADts), use.names = FALSE)
nullDistCRM <- unlist(parLapply(cl, APCnull,APCCRMDts), use.names = FALSE)
nullDistRCM <- unlist(parLapply(cl, APCnull,APCRCMDts), use.names = FALSE)
stopCluster(cl) 

summary<- c(mean(nullDistCRA), sd(nullDistCRA), mean(nullDistRCA), sd(nullDistRCA),
            mean(nullDistCRM), sd(nullDistCRM), mean(nullDistRCM), sd(nullDistRCM))

## Means and standard deviations of CRA, RCA, CRM, RCM
APCSSnullDist_AixBjxK<-list(nullDistCRA,nullDistRCA,nullDistCRM,nullDistRCM,summary)
save(APCSSnullDist_AixBjxK,file="APCSSnullDist_AixBjxK_250kSim.RData")

Null1_250k<-toc()
Time_Null1_250k<-Null1_250k$toc - Null1_250k$tic
save(Time_Null1_250k, file="Time Elapsed for Null 1_AixBjxK_250k Sim.RData")

POST(url, body = list(accountKey = key, 
                      title = "Null Mean & Var for AixBjxK is Done", 
                      message = "Comp, 250k"), encode = "form", handle = res)
# The POST function notify the chunk above has finished running

APCSSnullDist_AixBjxK<-NULL

## 100k Sim ----
tic("Null AixBjxK_100k Sim")  
#### Null Matrices ----
cl <- parallel::makeCluster(detectCores()-1)
registerDoParallel(cl)
# Parallelized foreach loop
I <- Ai
J <- Bj
K <- NumReps
numTrial <- 100000
APCnull<- NULL
APCnull <- foreach(i = 1:numTrial) %dopar% {
  nullData <- data.frame(value = rnorm(I * J * K),
                         A = rep(1:I, each = K, times = J),
                         B = rep(1:J, each = I * K)
  )
  APCnull[[i]] <- nullData
}
# Stop the cluster
stopCluster(cl)

#### Null Mean & Variance ----
cl <- parallel::makeCluster(detectCores()-1)
registerDoParallel(cl)
nullDistCRA <- unlist(parLapply(cl, APCnull,APCCRADts), use.names = FALSE) 
nullDistRCA <- unlist(parLapply(cl, APCnull,APCRCADts), use.names = FALSE)
nullDistCRM <- unlist(parLapply(cl, APCnull,APCCRMDts), use.names = FALSE)
nullDistRCM <- unlist(parLapply(cl, APCnull,APCRCMDts), use.names = FALSE)
stopCluster(cl) 

summary<- c(mean(nullDistCRA), sd(nullDistCRA), mean(nullDistRCA), sd(nullDistRCA),
            mean(nullDistCRM), sd(nullDistCRM), mean(nullDistRCM), sd(nullDistRCM))

## Means and standard deviations of CRA, RCA, CRM, RCM
APCSSnullDist_AixBjxK<-list(nullDistCRA,nullDistRCA,nullDistCRM,nullDistRCM,summary)
save(APCSSnullDist_AixBjxK,file="APCSSnullDist_AixBjxK_100kSim.RData")

Null1_100k<-toc()
Time_Null1_100k<-Null1_100k$toc - Null1_100k$tic
save(Time_Null1_100k, file="Time Elapsed for Null 1_AixBjxK_100k Sim.RData")

POST(url, body = list(accountKey = key, 
                      title = "Null Mean & Var for AixBjxK is Done", 
                      message = "Comp, 100k"), encode = "form", handle = res)

APCSSnullDist_AixBjxK<-NULL

## 50k Sim ----
tic("Null AixBjxK_50k Sim")  
#### Null Matrices ----
cl <- parallel::makeCluster(detectCores()-1)
registerDoParallel(cl)
# Parallelized foreach loop
I <- Ai
J <- Bj
K <- NumReps
numTrial <- 50000
APCnull<- NULL
APCnull <- foreach(i = 1:numTrial) %dopar% {
  nullData <- data.frame(value = rnorm(I * J * K),
                         A = rep(1:I, each = K, times = J),
                         B = rep(1:J, each = I * K)
  )
  APCnull[[i]] <- nullData
}
# Stop the cluster
stopCluster(cl)

##### Null Mean & Variance ----
cl <- parallel::makeCluster(detectCores()-1)
registerDoParallel(cl)
nullDistCRA <- unlist(parLapply(cl, APCnull,APCCRADts), use.names = FALSE) 
nullDistRCA <- unlist(parLapply(cl, APCnull,APCRCADts), use.names = FALSE)
nullDistCRM <- unlist(parLapply(cl, APCnull,APCCRMDts), use.names = FALSE)
nullDistRCM <- unlist(parLapply(cl, APCnull,APCRCMDts), use.names = FALSE)
stopCluster(cl) 

summary<- c(mean(nullDistCRA), sd(nullDistCRA), mean(nullDistRCA), sd(nullDistRCA),
            mean(nullDistCRM), sd(nullDistCRM), mean(nullDistRCM), sd(nullDistRCM))

## Means and standard deviations of CRA, RCA, CRM, RCM
APCSSnullDist_AixBjxK<-list(nullDistCRA,nullDistRCA,nullDistCRM,nullDistRCM,summary)
save(APCSSnullDist_AixBjxK,file="APCSSnullDist_AixBjxK_50kSim.RData")

Null1_50k<-toc()
Time_Null1_50k<-Null1_50k$toc - Null1_50k$tic
save(Time_Null1_50k, file="Time Elapsed for Null 1_AixBjxK_50k Sim.RData")

POST(url, body = list(accountKey = key, 
                      title = "Null Mean & Var for AixBjxK is Done", 
                      message = "Comp, 50k"), encode = "form", handle = res)

APCSSnullDist_AixBjxK<-NULL

# Summary Stats ----
APCSSNull_results_AixBjxK<-tibble(Stats=c("CRA Mean", "CRA sd","RCA Mean", "RCA sd","CRM Mean","CRM sd","RCM Mean","RCM sd"))%>% 
  mutate(Value=APCSSnullDist_AixBjxK[[5]]) %>%
  mutate(SquaredValue=Value^2)
APCSSNull_results_AixBjxK

# Null Distribution Critical Values for AixBjxK ----
## 250k Sim ----
nullDistAPCSSA <-NULL
nullDistAPCSSM <-NULL
APCSSnullDist_AixBjxK[[5]]
APCSSnullDist<- c()

tic("Null Critical Values for AixBjxK for 250k Sim")
cl <- parallel::makeCluster(detectCores()-1)
registerDoParallel(cl)
I <- Ai
J <- Bj
K <- NumReps
numTrial <- 250000
APCSSAnull <- NULL
APCSSMnull <- NULL
clusterExport(cl,list("APCCRADts","APCRCADts","APCSSAts","APCCRMDts",
                      "APCRCMDts","APCSSMts","APCSSnullDist"))

# Second null for Average
APCSSAnull <- foreach(i = 1:numTrial) %dopar% {
  nullData <- data.frame(value = rnorm(I * J * K),
                         A = rep(1:I, each = K, times = J),
                         B = rep(1:J, each = I * K)
  )
  APCSSAnull[[i]] <- nullData
}

nullDistAPCSSA <- unlist(parLapply(cl, APCSSAnull,APCSSAts), use.names = FALSE) 
save(nullDistAPCSSA,file="APCSSA Null Distribution AixBjxK_250kSim.RData")
stopCluster(cl)

# Second null for Median
cl <- parallel::makeCluster(detectCores()-1)
registerDoParallel(cl)
I <- Ai
J <- Bj
K <- NumReps
numTrial <- 250000
APCSSAnull <- NULL
APCSSMnull <- NULL
clusterExport(cl,list("APCCRADts","APCRCADts","APCSSAts","APCCRMDts",
                      "APCRCMDts","APCSSMts","APCSSnullDist"))


APCSSMnull <- foreach(i = 1:numTrial) %dopar% {
  nullData <- data.frame(value = rnorm(I * J * K),
                         A = rep(1:I, each = K, times = J),
                         B = rep(1:J, each = I * K)
  )
  APCSSMnull[[i]] <- nullData
}

nullDistAPCSSM <- unlist(parLapply(cl, APCSSMnull,APCSSMts), use.names = FALSE) 
save(nullDistAPCSSM,file="APCSSM Null Distribution AixBjxK_250kSim.RData")
stopCluster(cl)

Null2_250k<-toc()
Time_Null2_250k<-Null2_250k$toc-Null2_250k$tic
save(Time_Null2_250k, file="Time elapsed for Critical Values AixBjxK_250kSim.RData")

POST(url, body = list(accountKey = key, 
                      title = "Critical Values for AixBjxK is Done", 
                      message = "Comp, 250k Sim"), encode = "form", handle = res)

## 100k Sim ----
nullDistAPCSSA <-NULL
nullDistAPCSSM <-NULL
APCSSnullDist <- NULL
APCSSnullDist_AixBjxK[[5]]
APCSSnullDist<- c()

tic("Null Critical Values for AixBjxK for 100k Sim")
cl <- parallel::makeCluster(detectCores()-1)
registerDoParallel(cl)
I <- Ai
J <- Bj
K <- NumReps
numTrial <- 100000
APCSSAnull <- NULL
APCSSMnull <- NULL
clusterExport(cl,list("APCCRADts","APCRCADts","APCSSAts","APCCRMDts",
                      "APCRCMDts","APCSSMts","APCSSnullDist"))

# Second null for Average
APCSSAnull <- foreach(i = 1:numTrial) %dopar% {
  nullData <- data.frame(value = rnorm(I * J * K),
                         A = rep(1:I, each = K, times = J),
                         B = rep(1:J, each = I * K)
  )
  APCSSAnull[[i]] <- nullData
}

nullDistAPCSSA <- unlist(parLapply(cl, APCSSAnull,APCSSAts), use.names = FALSE) 
save(nullDistAPCSSA,file="APCSSA Null Distribution AixBjxK_100kSim.RData")
stopCluster(cl)

# Second null for Median
cl <- parallel::makeCluster(detectCores()-1)
registerDoParallel(cl)
I <- Ai
J <- Bj
K <- NumReps
numTrial <- 100000
APCSSAnull <- NULL
APCSSMnull <- NULL
clusterExport(cl,list("APCCRADts","APCRCADts","APCSSAts","APCCRMDts",
                      "APCRCMDts","APCSSMts","APCSSnullDist"))

APCSSMnull <- foreach(i = 1:numTrial) %dopar% {
  nullData <- data.frame(value = rnorm(I * J * K),
                         A = rep(1:I, each = K, times = J),
                         B = rep(1:J, each = I * K)
  )
  APCSSMnull[[i]] <- nullData
}

nullDistAPCSSM <- unlist(parLapply(cl, APCSSMnull,APCSSMts), use.names = FALSE) 
save(nullDistAPCSSM,file="APCSSM Null Distribution AixBjxK_100kSim.RData")
stopCluster(cl)

Null2_100k<-toc()
Time_Null2_100k<-Null2_100k$toc-Null2_100k$tic
save(Time_Null2_100k, file="Time elapsed for Critical Values AixBjxK_100kSim.RData")

POST(url, body = list(accountKey = key, 
                      title = "Critical Values for AixBjxK is Done", 
                      message = "Comp, 100k Sim"), encode = "form", handle = res)

## 50k Sim ----
nullDistAPCSSA <-NULL
nullDistAPCSSM <-NULL
APCSSnullDist <- NULL
APCSSnullDist_AixBjxK[[5]]
APCSSnullDist<- c()

tic("Null Critical Values for AixBjxK for 50k Sim")
cl <- parallel::makeCluster(detectCores()-1)
registerDoParallel(cl)
I <- Ai
J <- Bj
K <- NumReps
numTrial <- 50000
APCSSAnull <- NULL
APCSSMnull <- NULL
clusterExport(cl,list("APCCRADts","APCRCADts","APCSSAts","APCCRMDts",
                      "APCRCMDts","APCSSMts","APCSSnullDist"))

# Second null for Average
APCSSAnull <- foreach(i = 1:numTrial) %dopar% {
  nullData <- data.frame(value = rnorm(I * J * K),
                         A = rep(1:I, each = K, times = J),
                         B = rep(1:J, each = I * K)
  )
  APCSSAnull[[i]] <- nullData
}

nullDistAPCSSA <- unlist(parLapply(cl, APCSSAnull,APCSSAts), use.names = FALSE) 
save(nullDistAPCSSA,file="APCSSA Null Distribution AixBjxK_50kSim.RData")
stopCluster(cl)

# Second null for Median
cl <- parallel::makeCluster(detectCores()-1)
registerDoParallel(cl)
I <- Ai
J <- Bj
K <- NumReps
numTrial <- 50000
APCSSAnull <- NULL
APCSSMnull <- NULL
clusterExport(cl,list("APCCRADts","APCRCADts","APCSSAts","APCCRMDts",
                      "APCRCMDts","APCSSMts","APCSSnullDist"))
APCSSMnull <- foreach(i = 1:numTrial) %dopar% {
  nullData <- data.frame(value = rnorm(I * J * K),
                         A = rep(1:I, each = K, times = J),
                         B = rep(1:J, each = I * K)
  )
  APCSSMnull[[i]] <- nullData
}

nullDistAPCSSM <- unlist(parLapply(cl, APCSSMnull,APCSSMts), use.names = FALSE) 
save(nullDistAPCSSM,file="APCSSM Null Distribution AixBjxK_50kSim.RData")
stopCluster(cl)

Null2_50k<-toc()
Time_Null2_50k<-Null2_50k$toc-Null2_50k$tic
save(Time_Null2_50k, file="Time elapsed for Critical Values AixBjxK_50kSim.RData")

POST(url, body = list(accountKey = key, 
                      title = "Critical Values for AixBjxK is Done", 
                      message = "Comp, 50k Sim"), encode = "form", handle = res)

# Critical Values ----
## If the critical values attained has a higher p-value than alpha level,
## we will increase a till the attained significance is less than alpha.

a <-quantile(nullDistAPCSSA, 0.95)
sum(nullDistAPCSSA>= a )/length(nullDistAPCSSA)
a
b <- quantile(nullDistAPCSSA,0.99)
sum(nullDistAPCSSA>= b )/length(nullDistAPCSSA)
b
c <- quantile(nullDistAPCSSM,0.95)
sum(nullDistAPCSSM>= c )/length(nullDistAPCSSM)
c
d <- quantile(nullDistAPCSSM,0.99)
sum(nullDistAPCSSM>= d )/length(nullDistAPCSSM)
d

