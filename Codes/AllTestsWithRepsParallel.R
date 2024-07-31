## First column contains the values of the observations; 
## second contains the break-down of factor A (i of them);
## third contains the break-down of factor B (j of them); 

# This file contains the core codes to run different tests for interaction
# in a balanced two-way ANOVA

######## APCSSA/APCSSM ########
## APC CRA ----
APCCRADts <- function(dataFrame) {
  ## Number of levels of factors A (I) and B (J), and number of replications per cell (K)
  I <- nlevels(as.factor(dataFrame[, 2]))
  J <- nlevels(as.factor(dataFrame[, 3]))
  K <- nrow(dataFrame)/(I*J)
  
  ## Split dataframe into smaller dataframes by factor B (by columns) for aligning
  dataList <- split(dataFrame, as.factor(dataFrame[, 3]))
  
  ## Align by column average
  for (j in 1:J) { ## looping over the number of levels for factor B (J)
    dataList[[j]][, 1] <- dataList[[j]][, 1] - mean(dataList[[j]][, 1])
  }
  
  ## Rebind the (new) dataframe by row
  dataFrame <- do.call("rbind", dataList)
  
  ## Split dataframe into smaller dataframes by factor A (by rows) for ranking
  dataList <- split(dataFrame, as.factor(dataFrame[, 2]))
  
  ## Rank by row
  for (i in 1:I) { ## looping over the number of levels for factor A (I)
    dataList[[i]][, 1] <- rank(dataList[[i]][, 1], ties.method = "average")
  }
  
  ## Rebind the (new) dataframe by row
  dataFrame <- do.call("rbind", dataList)
  
  ## Split dataframe into smaller dataframes by factor B (by column) and then by factor A (by row) 
  ## for calculation
  dataList <- split(dataFrame, as.factor(dataFrame[, 3]))
  for (j in 1:J) {
    dataList[[j]] <- split(dataList[[j]], as.factor(dataList[[j]][, 2]))
  }
  
  ## Calculate all the J*(J-1)/2 values of V_jj'
  APCCRA <- c()
  for (j1 in (1:(J - 1))) {
    for (j2 in ((j1 + 1):J)) {
      Vjjp <- 0
      ## Calculate each cross-comparison
      for (i1 in (1:(I - 1))) {
        for (i2 in ((i1 + 1):I)) {
          for (k1 in 1:K) {
            for (k2 in 1:K) {
              for (k3 in 1:K) {
                for (k4 in 1:K) {
                  Vjjp <- Vjjp + (dataList[[j1]][[i1]][, 1][k1] + dataList[[j2]][[i2]][, 1][k2] - dataList[[j1]][[i2]][, 1][k3] - dataList[[j2]][[i1]][, 1][k4])^2
                }
              }
            }
          }
        }
      }
      APCCRA <- c(APCCRA, Vjjp)
    }
  }
  APCCRA <- max(APCCRA)
  
  ## Scale
  APCCRAD <- 2*APCCRA/(K^4*I*(I - 1))
  return(APCCRAD)
}

## APC RCA ----
APCRCADts <- function(dataFrame) {
  ## Number of levels of factors A (I) and B (J), and number of replication per cell (K)
  I <- nlevels(as.factor(dataFrame[, 2]))
  J <- nlevels(as.factor(dataFrame[, 3]))
  K <- nrow(dataFrame)/(I*J) 
  
  ## Split dataframe into smaller dataframes by factor A (by rows) for aligning
  dataList <- split(dataFrame, as.factor(dataFrame[, 2]))
  
  ## Aligning by row average
  for (i in 1:I) { ## looping over the number of levels for factor A (I)
    dataList[[i]][, 1] <- dataList[[i]][, 1] - mean(dataList[[i]][, 1])
  }
  
  ## Rebind the (new) dataframe by row
  dataFrame <- do.call("rbind", dataList)
  
  ## Split dataframe into smaller dataframes by factor B (by columns) for ranking
  dataList <- split(dataFrame, as.factor(dataFrame[, 3]))
  
  ## Rank by column
  for (j in 1:J) { ## looping over the number of levels for factor B (J)
    dataList[[j]][, 1] <- rank(dataList[[j]][, 1], ties.method = "average")
  }
  
  ## Rebind the (new) dataframe by row
  dataFrame <- do.call("rbind", dataList)
  
  ## Split dataframe into smaller dataframes by factor A (by row) and then by factor B (by column) 
  ## for calculation
  dataList <- split(dataFrame, as.factor(dataFrame[, 2]))
  for (i in 1:I) {
    dataList[[i]] <- split(dataList[[i]], as.factor(dataList[[i]][, 3]))
  }
  
  ## Calculate all the I*(I-1)/2 values of V_ii'
  APCRCA <- c()
  for (i1 in (1:(I - 1))) {
    for (i2 in ((i1 + 1):I)) {
      Viip <- 0
      ## Calculate each cross-comparison
      for (j1 in (1:(J - 1))) {
        for (j2 in ((j1 + 1):J)) {
          for (k1 in 1:K) {
            for (k2 in 1:K) {
              for (k3 in 1:K) {
                for (k4 in 1:K) {
                  Viip <- Viip + (dataList[[i1]][[j1]][, 1][k1] + dataList[[i2]][[j2]][, 1][k2] - dataList[[i1]][[j2]][, 1][k3] - dataList[[i2]][[j1]][, 1][k4])^2
                }
              }
            }
          }
        }
      }
      APCRCA <- c(APCRCA, Viip)
    }
  }
  APCRCA <- max(APCRCA)
  
  ## Scale
  APCRCAD <- 2*APCRCA/(K^4*J*(J - 1))
  return(APCRCAD)
}


## APC CRM ----
APCCRMDts <- function(dataFrame) {
  ## Number of levels of factors A (I) and B (J), and number of replication per cell (K)
  I <- nlevels(as.factor(dataFrame[, 2]))
  J <- nlevels(as.factor(dataFrame[, 3]))
  K <- nrow(dataFrame)/(I*J)
  
  ## Split dataframe into smaller dataframes by factor B (by columns) for aligning
  dataList <- split(dataFrame, as.factor(dataFrame[, 3]))
  
  ## Align by column median
  for (j in 1:J) { ## looping over the number of levels for factor B (J)
    dataList[[j]][, 1] <- dataList[[j]][, 1] - median(dataList[[j]][, 1])
  }
  
  ## Rebind the (new) dataframe by row
  dataFrame <- do.call("rbind", dataList)
  
  ## Split dataframe into smaller dataframes by factor A (by rows) for ranking
  dataList <- split(dataFrame, as.factor(dataFrame[, 2]))
  
  ## Rank by row
  for (i in 1:I) { ## looping over the number of levels for factor A (I)
    dataList[[i]][, 1] <- rank(dataList[[i]][, 1], ties.method = "average")
  }
  
  ## Rebind the (new) dataframe by row
  dataFrame <- do.call("rbind", dataList)
  
  ## Split dataframe into smaller dataframes by factor B (by column) and then by factor A (by row) 
  ## for calculation
  dataList <- split(dataFrame, as.factor(dataFrame[, 3]))
  for (j in 1:J) {
    dataList[[j]] <- split(dataList[[j]], as.factor(dataList[[j]][, 2]))
  }
  
  ## Calculate all the J*(J-1)/2 values of V_jj'
  APCCRM <- c()
  for (j1 in (1:(J - 1))) {
    for (j2 in ((j1 + 1):J)) {
      Vjjp <- 0
      ## Calculate each cross-comparison
      for (i1 in (1:(I - 1))) {
        for (i2 in ((i1 + 1):I)) {
          for (k1 in 1:K) {
            for (k2 in 1:K) {
              for (k3 in 1:K) {
                for (k4 in 1:K) {
                  Vjjp <- Vjjp + (dataList[[j1]][[i1]][, 1][k1] + dataList[[j2]][[i2]][, 1][k2] - dataList[[j1]][[i2]][, 1][k3] - dataList[[j2]][[i1]][, 1][k4])^2
                }
              }
            }
          }
        }
      }
      APCCRM <- c(APCCRM, Vjjp)
    }
  }
  APCCRM <- max(APCCRM)
  ## Scale
  APCCRMD <- 2*APCCRM/(K^4*I*(I - 1))
  return(APCCRMD)
}


## APC RCM ----
APCRCMDts <- function(dataFrame) {
  ## Number of levels of factors A (I) and B (J), and number of replication per cell (K)
  I <- nlevels(as.factor(dataFrame[, 2]))
  J <- nlevels(as.factor(dataFrame[, 3]))
  K <- nrow(dataFrame)/(I*J)
  
  ## Split dataframe into smaller dataframes by factor A (by rows) for aligning
  dataList <- split(dataFrame, as.factor(dataFrame[, 2]))
  
  ## Aligning by row median
  for (i in 1:I) { ## looping over the number of levels for factor A (I)
    dataList[[i]][, 1] <- dataList[[i]][, 1] - median(dataList[[i]][, 1])
  }
  
  ## Rebind the (new) dataframe by row
  dataFrame <- do.call("rbind", dataList)
  
  ## Split dataframe into smaller dataframes by factor B (by columns) for ranking
  dataList <- split(dataFrame, as.factor(dataFrame[, 3]))
  
  ## Rank by column
  for (j in 1:J) { ## looping over the number of levels for factor B (J)
    dataList[[j]][, 1] <- rank(dataList[[j]][, 1], ties.method = "average")
  }
  
  ## Rebind the (new) dataframe by row
  dataFrame <- do.call("rbind", dataList)
  
  ## Split dataframe into smaller dataframes by factor A (by row) and then by factor B (by column) 
  ## for calculation
  dataList <- split(dataFrame, as.factor(dataFrame[, 2]))
  for (i in 1:I) {
    dataList[[i]] <- split(dataList[[i]], as.factor(dataList[[i]][, 3]))
  }
  
  ## Calculate all the I*(I-1)/2 values of V_ii'
  APCRCM <- c()
  for (i1 in (1:(I - 1))) {
    for (i2 in ((i1 + 1):I)) {
      Viip <- 0
      ## Calculate each cross-comparison
      for (j1 in (1:(J - 1))) {
        for (j2 in ((j1 + 1):J)) {
          for (k1 in 1:K) {
            for (k2 in 1:K) {
              for (k3 in 1:K) {
                for (k4 in 1:K) {
                  Viip <- Viip + (dataList[[i1]][[j1]][, 1][k1] + dataList[[i2]][[j2]][, 1][k2] - dataList[[i1]][[j2]][, 1][k3] - dataList[[i2]][[j1]][, 1][k4])^2
                }
              }
            }
          }
        }
      }
      APCRCM <- c(APCRCM, Viip)
    }
  }
  APCRCM <- max(APCRCM)
  ## Scale
  APCRCMD <- 2*APCRCM/(K^4*J*(J - 1))
  return(APCRCMD)
}

## Test statistic for APCSSA ----
APCSSAts <- function(dataFrame, numTrial) {
  ## Get the scaled, unstandardized test statistics
  APCCRAD <- APCCRADts(dataFrame)
  APCRCAD <- APCRCADts(dataFrame)
  
  ## Standardize and pick the max of the two
  APCCRADstar <- (APCCRAD - APCSSnullDist[1])/APCSSnullDist[2]
  APCRCADstar <- (APCRCAD - APCSSnullDist[3])/APCSSnullDist[4]
  
  APCSSA <- max(APCCRADstar, APCRCADstar)
  return(APCSSA)
}

## Test statistic for APCSSM ----
APCSSMts <- function(dataFrame, numTrial) {
  ## Get the scaled, unstandardized test statistics
  APCCRMD <- APCCRMDts(dataFrame)
  APCRCMD <- APCRCMDts(dataFrame)
  
  ## Standardize and pick the max of the two
  APCCRMDstar <- (APCCRMD - APCSSnullDist[5])/APCSSnullDist[6]
  APCRCMDstar <- (APCRCMD - APCSSnullDist[7])/APCSSnullDist[8]
  
  APCSSM <- max(APCCRMDstar, APCRCMDstar)
  return(APCSSM)
}

# APCSSA and APCSSM Test ----
# Codes to be used in power study, but for this to work, the previous codes need to be run
APCSSAtest <- function(dataFrame) {
  APCSSAtestStat <- APCSSAts(dataFrame)
  if (APCSSAtestStat >= cutOffAPCSSA) {return(1)} else {return(0)}
}

APCSSMtest <- function(dataFrame) {
  APCSSMtestStat <- APCSSMts(dataFrame)
  if (APCSSMtestStat >= cutOffAPCSSM) {return(1)} else {return(0)}
}

# DeKroon Test ----
### Test Statistics ----
DEKRts <- function(dataFrame) {
  ## Number of levels of factors A (I) and B (J), and number of replication per cell (K)
  I <- nlevels(as.factor(dataFrame[, 2]))
  J <- nlevels(as.factor(dataFrame[, 3]))
  K <- nrow(dataFrame)/(I*J)
  
  ## Split dataframe into smaller dataframes by factor B
  dataList <- split(dataFrame, as.factor(dataFrame[, 3]))
  
  grandSumRank <- rep(0, I)
  SOS <- 0
  
  for (j in 1:J) { ## looping over the number of levels for factor B (J)
    dataList[[j]][, 1] <- rank(dataList[[j]][, 1], ties.method = "average")
    sumRankWithinBlock <- rep(0, I)
    
    for (i in 1:I) {
      sumRankWithinBlock[i] <- sum(dataList[[j]][which(dataList[[j]][, 2] == i), 1])
    }
    
    SOS <- SOS + sum(sumRankWithinBlock^2)
    grandSumRank <- grandSumRank + sumRankWithinBlock
  }
  
  TS <- 12*(SOS - sum(grandSumRank^2)/J)/(K^2*I*(K*I + 1))
  return(TS)
}

DEKRcutoff <- function(I, J, K, sigLev, numTrial) {
  cutOff <- rep(0, numTrial)
  
  for (i in 1:numTrial) {
    DEKRnull <- t(matrix(rnorm(I*J*K), nrow = length(I*J*K)))
    DEKRnull <- cbind(DEKRnull, rep(1:I, each = K, times = J))
    DEKRnull <- cbind(DEKRnull, rep(1:J, each = I*K))
    
    DEKRnull <- as.data.frame(DEKRnull)
    
    cutOff[i] <- DEKRts(DEKRnull)
  }
  DEKRcutOff <- unname(quantile(cutOff, 1 - sigLev))
  
  # Return the cutoff point
  return(DEKRcutOff)
}

### Function to find the test statistics given data -----
DEKRtest <- function(dataFrame) {
  testStat <- DEKRts(dataFrame)
  if (testStat >= cutOff_DEKR) {return(1)} else {return(0)}
}

# F and RT for Interaction ##########

fTest <- function(dataFrame, sigLev) {
  pVal <- summary(aov(dataFrame[, 1]~as.factor(dataFrame[, 2])*as.factor(dataFrame[, 3])))[[1]][["Pr(>F)"]][3]
  if(pVal <= sigLev) {return(1)} else {return(0)}
}

rankTransformTest <- function(dataFrame, sigLev) {
  pVal <- summary(aov(rank(dataFrame[, 1], ties.method = "average")~as.factor(dataFrame[, 2])*as.factor(dataFrame[, 3])))[[1]][["Pr(>F)"]][3]
  if(pVal <= sigLev) {return(1)} else {return(0)}
}


# ART, RAOV, and Permutation Test -----
artTest <- function(dataFrame, sigLev){
  pVal <- anova(art(unlist(dataFrame[, 1])~as.factor(unlist(dataFrame[, 2]))*as.factor(unlist(dataFrame[, 3]))))[3, 7]
  if(pVal <= sigLev) {return(1)} else {return(0)}
}

raovTest <- function(dataFrame, sigLev){
  pVal <- raov(unlist(dataFrame[,1])~as.factor(unlist(dataFrame[, 2]))*as.factor(unlist(dataFrame[, 3])))[[1]][3,5]
  if(pVal <= sigLev) {return(1)} else {return(0)}
}

permTest <- function(dataFrame, sigLev, permReps){
  pVal <- permuANOVA(unlist(dataFrame[,1]), as.factor(unlist(dataFrame[, 2])), as.factor(unlist(dataFrame[, 3])),  
                     perm.type="unrestricted", reps= permReps)[3,1]
  if(pVal <= sigLev) {return(1)} else {return(0)}
}
