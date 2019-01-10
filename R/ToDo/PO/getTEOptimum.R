library(quadprog)
library(plyr)
library(PerformanceAnalytics)

getTEEF = function (stdPL_stats, vecMIN_WEIGHT, vecMAX_WEIGHT) {
  nSec = stdPL_stats$MetaData$nSec
  nYrs = stdPL_stats$MetaData$nYrs
  matER = as.matrix(stdPL_stats$Stats$x$data$ret_pa, ncol = 1)                                                   # annualized returns
  matCov = as.matrix(cov(stdPL_stats$FD[,-1]) * (stdPL_stats$MetaData$annFactorVola)^2, nrow = nSec, ncol = nSec)      # annualized covariance matrix
  
  if(missing(vecMIN_WEIGHT)) vecMIN_WEIGHT = rep(-1, nSec)
  if(missing(vecMAX_WEIGHT)) vecMAX_WEIGHT = rep(1, nSec)
  
  vecMIN_WEIGHT = c(-1,rep(0, nSec-1))
  vecMAX_WEIGHT = -vecMAX_WEIGHT

  
  # solve.QP {quadprog}
  # min(-d^T b + 1/2 b^T D b) with the constraints A^T b >= b_0
  # solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE)
  
  matA = matrix(cbind(matrix(c(1, rep(0, nSec-1))),    # EQUALITY - vector of -1 , and the rest zeros for - FIRST WEIGHT (BENCHMARK) = -1
                      matrix(1,nSec),        # EQUALITY - vector of ones for the SUM OF WEIGHTS
                      diag(1, nSec),         # INEQUALITY - diagonal ones for the MINIMUM WEIGHT constraint
                      diag(-1, nSec)         # INEQUALITY - diagonal of minus ones for the MAXIMUM WEIGHT constraint
                      ), ncol = nSec*2+2, nrow = nSec)
  
  vecb = matrix(c(-1,                         # EQUALITY - the first element is -1, this is the weight of the Benchmark target
                  0,                          # EQUALITY - the SUM OF WEIGHTS
                  vecMIN_WEIGHT,             # INEQUALITY - the MINIMUM WEIGHT
                  vecMAX_WEIGHT              # INEQUALITY - the MAXIMUM WEIGHT
                  ), ncol = 1)
  
  sol = solve.QP(Dmat = Matrix::nearPD(matCov)$mat, dvec = rep(0,nSec), Amat = matA, bvec = vecb, meq = 2)  # get minimum variance portfolio
  ptfWeights = data.frame(t(sol$solution))
  colnames(ptfWeights) = colnames(stdPL_stats$TS[,-1])
  
  maxPossibleRet = max(matER[-1])
  minPossibleRet = sol$solution[-1] %*% matER[-1]
  
  seqTargetRet = rev(-seq(-maxPossibleRet, -minPossibleRet, 0.001))
  if(abs(seqTargetRet[1] - minPossibleRet) < 10^-6 ) seqTargetRet = seqTargetRet[-1]
  
  
  
  matA = matrix(cbind(matrix(c(1, rep(0, nSec-1))),    # EQUALITY - vector of -1 , and the rest zeros for - FIRST WEIGHT (BENCHMARK) = -1
                      matrix(1,nSec),        # EQUALITY - vector of ones for the SUM OF WEIGHTS
                      c(0, matER[-1]),                 # EQUILITY - vector of mean expected returns for the TARGET RETURN
                      diag(1, nSec),         # INEQUALITY - diagonal ones for the MINIMUM WEIGHT constraint
                      diag(-1, nSec)         # INEQUALITY - diagonal of minus ones for the MAXIMUM WEIGHT constraint
                      ), ncol = nSec*2+3, nrow = nSec)
  
  vecb = matrix(c(-1,
                  0,                         # EQUALITY - the SUM OF WEIGHTS
                  seqTargetRet[1],           # EQUALITY - the TARGET RETURN
                  vecMIN_WEIGHT,             # INEQUALITY - the MINIMUM WEIGHT
                  vecMAX_WEIGHT              # INEQUALITY - the MAXIMUM WEIGHT
                  ), ncol = 1)
  
  
  
  for (targetRet in seqTargetRet){
    vecb[3] = targetRet
    sol = try(solve.QP(Dmat = matCov, dvec = rep(0,nSec), Amat = matA, bvec = vecb, meq = 2), silent = T)  # get min var portfolio at target return
    
    if ('try-error' %in% class(sol)) break
    else ptfWeights[nrow(ptfWeights)+1, ] = t(sol$solution)
  }
  
  ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(x<vecMIN_WEIGHT, vecMIN_WEIGHT, x))))
  ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(x>-vecMAX_WEIGHT, -vecMAX_WEIGHT, x))))
  ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(abs(x-vecMIN_WEIGHT)<10^-4, vecMIN_WEIGHT, x))))
  ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(abs(x+vecMAX_WEIGHT)<10^-4, -vecMAX_WEIGHT, x))))
  
  ptfWeights$pa_ret = as.numeric(t(t(ptfWeights[,2:nSec])) %*% matER[-1])
  ptfWeights$pa_active_ret = as.numeric(t(t(ptfWeights[,1:nSec])) %*% matER)
  ptfWeights$pa_te = as.numeric(t(t(apply(ptfWeights[,1:nSec], 1, function(x) sqrt( t(x) %*% matCov %*% t(t(x)) ) ))))
  ptfWeights$IR = as.numeric(ptfWeights$pa_active_ret / ptfWeights$pa_te)
  
  dfMDD = data.frame(apply(ptfWeights[,-1], 1, function(x) t(t(exp(stdPL_stats$FD[,-c(1:2)])-1)) %*% t(t(x[1:nSec-1]))))
  row.names(dfMDD) = stdPL_stats$FD$DATE
  ptfWeights$mDD = (as.numeric(t(maxDrawdown(dfMDD))))
  
  return(ptfWeights)
}

# BMK_vol = sd(as.matrix(FD[,2]))
# numVolScaleToBMK = apply(FD[, -(1:2)], 2, function(x) BMK_vol / sd(as.matrix(x)))
# for (i in 3:ncol(FD)) FD[,i] = FD[,i, with =F] * numVolScaleToBMK[i-2]

getMINTE_FAST = function (FD, strPeriod) {
  nSec = ncol(FD)-1
  nYrs =  as.numeric(FD$DATE[nrow(FD)] - FD$DATE[1]) / 365
  
  if(strPeriod == "DLY") numVarAnnFactor = 260
  if(strPeriod == "WKL") numVarAnnFactor = 52
  if(strPeriod == "YRL") numVarAnnFactor = 12
  
  matER = as.matrix(apply(FD[,-1], 2, function(x) prod(exp(x)))^(1/nYrs)-1, ncol = 1)                                                   # annualized returns
  matCov = as.matrix(cov(FD[,-1]) * numVarAnnFactor , nrow = nSec, ncol = nSec)      # annualized covariance matrix
  
  # vecMIN_WEIGHT = c(-1,rep(0, nSec-1))
  # vecMAX_WEIGHT = -rep(1, nSec)
  
  # solve.QP {quadprog}
  # min(-d^T b + 1/2 b^T D b) with the constraints A^T b >= b_0
  # solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE)
  
  matA = matrix(c(1, rep(0, nSec-1)), ncol = 1, nrow = nSec)
  vecb = matrix(-1, ncol = 1)
  
  sol = solve.QP(Dmat = Matrix::nearPD(matCov)$mat, dvec = rep(0,nSec), Amat = matA, bvec = vecb, meq = 1)  # get minimum variance portfolio
  ptfWeights = data.frame(t(sol$solution))
  colnames(ptfWeights) = colnames(FD[,-1])
  
  # ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(x<vecMIN_WEIGHT, vecMIN_WEIGHT, x))))
  # ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(x>-vecMAX_WEIGHT, -vecMAX_WEIGHT, x))))
  # ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(abs(x-vecMIN_WEIGHT)<10^-4, vecMIN_WEIGHT, x))))
  # ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(abs(x+vecMAX_WEIGHT)<10^-4, -vecMAX_WEIGHT, x))))
  
  ptfWeights$pa_ret = as.numeric(t(t(ptfWeights[,2:nSec])) %*% matER[-1])
  ptfWeights$pa_active_ret = as.numeric(t(t(ptfWeights[,1:nSec])) %*% matER)
  ptfWeights$pa_te = as.numeric(t(t(apply(ptfWeights[,1:nSec], 1, function(x) sqrt( t(x) %*% matCov %*% t(t(x)) ) ))))
  ptfWeights$IR = as.numeric(ptfWeights$pa_active_ret / ptfWeights$pa_te)
  
  return(ptfWeights)
}

# this MUST SUM to 0!!

getMINTE_CONSTRAINED = function (FD, strPeriod, vecMAX_WEIGHT) {
  nSec = ncol(FD)-1
  nYrs =  as.numeric(FD$DATE[nrow(FD)] - FD$DATE[1]) / 365
  
  if(strPeriod == "DLY") numVarAnnFactor = 260
  if(strPeriod == "WKL") numVarAnnFactor = 52
  if(strPeriod == "YRL") numVarAnnFactor = 12
  
  matER = as.matrix(apply(FD[,-1], 2, function(x) prod(exp(x)))^(1/nYrs)-1, ncol = 1)                                                   # annualized returns
  matCov = as.matrix(cov(FD[,-1]) * numVarAnnFactor , nrow = nSec, ncol = nSec)      # annualized covariance matrix
  
  # vecMIN_WEIGHT = c(-1,rep(0, nSec-1))
  # vecMAX_WEIGHT = -rep(1, nSec)
  
  # solve.QP {quadprog}
  # min(-d^T b + 1/2 b^T D b) with the constraints A^T b >= b_0
  # solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE)
  
  matA = matrix(cbind(c(1, rep(0, nSec-1)),
                      c(0, rep(1, nSec-1)),
                      diag(1, nSec),          # INEQUALITY - diagonal ones for the MINIMUM WEIGHT constraint
                      diag(-1,nSec)),         # INEQUALITY - diagonal ones for the MAXIMUM WEIGHT constraint
                ncol = 2 + nSec * 2, nrow = nSec)
  vecb = matrix(c(-1, 1,
                  c(-10,rep(0, nSec-1)),       # INEQUALITY - MINIMUM WEIGHT constraints
                  c(-10, -vecMAX_WEIGHT)        # INEQUALITY - MAXIMUM WEIGHT constraINTS
                  ), ncol = 1)
  
  sol = try(solve.QP(Dmat = Matrix::nearPD(matCov)$mat, dvec = rep(0,nSec), Amat = matA, bvec = vecb, meq = 2), silent =T)  # get minimum variance portfolio
  if ('try-error' %in% class(sol)){
    return(sol)
    # browser()
  } 
  else ptfWeights = data.frame(t(sol$solution))
  colnames(ptfWeights) = colnames(FD[,-1])
  
  # ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(x<vecMIN_WEIGHT, vecMIN_WEIGHT, x))))
  # ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(x>-vecMAX_WEIGHT, -vecMAX_WEIGHT, x))))
  # ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(abs(x-vecMIN_WEIGHT)<10^-4, vecMIN_WEIGHT, x))))
  # ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(abs(x+vecMAX_WEIGHT)<10^-4, -vecMAX_WEIGHT, x))))
  
  ptfWeights$pa_ret = as.numeric(t(t(ptfWeights[,2:nSec])) %*% matER[-1])
  ptfWeights$pa_active_ret = as.numeric(t(t(ptfWeights[,1:nSec])) %*% matER)
  ptfWeights$pa_te = as.numeric(t(t(apply(ptfWeights[,1:nSec], 1, function(x) sqrt( t(x) %*% matCov %*% t(t(x)) ) ))))
  ptfWeights$IR = as.numeric(ptfWeights$pa_active_ret / ptfWeights$pa_te)
  
  return(ptfWeights)
}
