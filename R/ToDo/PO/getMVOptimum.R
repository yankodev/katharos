getMVEF = function (stdPL_stats, vecMIN_WEIGHT, vecMAX_WEIGHT) {
  library(quadprog)
  library(plyr)
  library(PerformanceAnalytics)
  
  nSec = stdPL_stats$MetaData$nSec
  nYrs = stdPL_stats$MetaData$nYrs
  matER = as.matrix(stdPL_stats$Stats$x$data$ret_pa, ncol = 1)                                                   # annualized returns
  matCov = as.matrix(cov(stdPL_stats$FD[,-1]) * (stdPL_stats$MetaData$annFactorVola)^2, nrow = nSec, ncol = nSec)      # annualized covariance matrix
  
  if(missing(vecMIN_WEIGHT)) vecMIN_WEIGHT = rep(-1, nSec)
  if(missing(vecMAX_WEIGHT)) vecMAX_WEIGHT = rep(1, nSec)
  
  vecMAX_WEIGHT = -vecMAX_WEIGHT
  
  # if(missing(MIN_WEIGHT)) MIN_WEIGHT = -1
  # if(missing(MAX_WEIGHT)) MAX_WEIGHT = 1
  # 
  # vecMIN_WEIGHT = rep(MIN_WEIGHT, nSec)
  # vecMAX_WEIGHT = rep(-MAX_WEIGHT, nSec)
  # 
  # if(!missing(dfFixedWeights)){
  #   for (i in 1:nrow(dfFixedWeights)){
  #     vecMIN_WEIGHT[dfFixedWeights$INDEX_NUMBER[i]] = dfFixedWeights$WEIGHT[i]
  #     vecMAX_WEIGHT[dfFixedWeights$INDEX_NUMBER[i]] = -dfFixedWeights$WEIGHT[i]
  #   }
  # }
  
  # solve.QP {quadprog}
  # min(-d^T b + 1/2 b^T D b) with the constraints A^T b >= b_0
  # solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE)
  
  matA = matrix(cbind(matrix(1,nSec),        # EQUALITY - vector of ones for the SUM OF WEIGHTS
                      diag(1, nSec),         # INEQUALITY - diagonal ones for the MINIMUM WEIGHT constraint
                      diag(-1, nSec)         # INEQUALITY - diagonal of minus ones for the MAXIMUM WEIGHT constraint
                      ), ncol = nSec*2+1, nrow = nSec)
  
  vecb = matrix(c(1,                         # EQUALITY - the SUM OF WEIGHTS
                  vecMIN_WEIGHT,             # INEQUALITY - the MINIMUM WEIGHT
                  vecMAX_WEIGHT              # INEQUALITY - the MAXIMUM WEIGHT
                  ), ncol = 1)
  
  sol = solve.QP(Dmat = matCov, dvec = rep(0,nSec), Amat = matA, bvec = vecb, meq = 1)  # get minimum variance portfolio
  ptfWeights = data.frame(t(sol$solution))
  colnames(ptfWeights) = colnames(stdPL_stats$TS[,-1])
  
  maxPossibleRet = max(matER)
  minPossibleRet = sol$solution %*% matER
  
  seqTargetRet = rev(-seq(-maxPossibleRet, -minPossibleRet, 0.001))
  if(abs(seqTargetRet[1] - minPossibleRet) < 10^-6 ) seqTargetRet = seqTargetRet[-1]
  
  matA = matrix(cbind(matrix(1,nSec),        # EQUALITY - vector of ones for the SUM OF WEIGHTS
                      matER,                 # EQUILITY - vector of mean expected returns for the TARGET RETURN
                      diag(1, nSec),         # INEQUALITY - diagonal ones for the MINIMUM WEIGHT constraint
                      diag(-1, nSec)         # INEQUALITY - diagonal of minus ones for the MAXIMUM WEIGHT constraint
                      ), ncol = nSec*2+2, nrow = nSec)
  
  vecb = matrix(c(1,                         # EQUALITY - the SUM OF WEIGHTS
                  seqTargetRet[1],           # EQUALITY - the TARGET RETURN
                  vecMIN_WEIGHT,             # INEQUALITY - the MINIMUM WEIGHT
                  vecMAX_WEIGHT              # INEQUALITY - the MAXIMUM WEIGHT
                  ), ncol = 1)
  
  for (targetRet in seqTargetRet){
    vecb[2] = targetRet
    sol = try(solve.QP(Dmat = matCov, dvec = rep(0,nSec), Amat = matA, bvec = vecb, meq = 2), silent = T)  # get min var portfolio at target return
    
    if ('try-error' %in% class(sol)) break
    else ptfWeights[nrow(ptfWeights)+1, ] = t(sol$solution)
  }
  
  ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(x<vecMIN_WEIGHT, vecMIN_WEIGHT, x))))
  ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(x>-vecMAX_WEIGHT, -vecMAX_WEIGHT, x))))
  ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(abs(x-vecMIN_WEIGHT)<10^-4, vecMIN_WEIGHT, x))))
  ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(abs(x+vecMAX_WEIGHT)<10^-4, -vecMAX_WEIGHT, x))))
  
  ptfWeights$pa_ret = as.numeric(t(t(ptfWeights[,1:nSec])) %*% matER)
  ptfWeights$pa_vola = as.numeric(t(t(apply(ptfWeights[,1:nSec], 1, function(x) sqrt( t(x) %*% matCov %*% t(t(x)) ) ))))
  ptfWeights$Sharpe = as.numeric(ptfWeights$pa_ret / ptfWeights$pa_vola)
  
  dfMDD = data.frame(apply(ptfWeights, 1, function(x) t(t(exp(stdPL_stats$FD[,-1])-1)) %*% t(t(x[1:nSec]))))
  row.names(dfMDD) = stdPL_stats$FD$DATE
  # ptfWeights$mDD = (as.numeric(t(maxDrawdown(dfMDD))))
  
  return(ptfWeights)
}


getMVEF_WithDD = function (stdPL_stats, vecMIN_WEIGHT, vecMAX_WEIGHT) {
  library(quadprog)
  library(plyr)
  library(PerformanceAnalytics)
  
  nSec = stdPL_stats$MetaData$nSec
  nYrs = stdPL_stats$MetaData$nYrs
  matER = as.matrix(stdPL_stats$Stats$x$data$ret_pa, ncol = 1)                                                   # annualized returns
  matCov = as.matrix(cov(stdPL_stats$FD[,-1]) * (stdPL_stats$MetaData$annFactorVola)^2, nrow = nSec, ncol = nSec)      # annualized covariance matrix
  
  if(missing(vecMIN_WEIGHT)) vecMIN_WEIGHT = rep(-1, nSec)
  if(missing(vecMAX_WEIGHT)) vecMAX_WEIGHT = rep(1, nSec)
  
  vecMAX_WEIGHT = -vecMAX_WEIGHT
  
  # if(missing(MIN_WEIGHT)) MIN_WEIGHT = -1
  # if(missing(MAX_WEIGHT)) MAX_WEIGHT = 1
  # 
  # vecMIN_WEIGHT = rep(MIN_WEIGHT, nSec)
  # vecMAX_WEIGHT = rep(-MAX_WEIGHT, nSec)
  # 
  # if(!missing(dfFixedWeights)){
  #   for (i in 1:nrow(dfFixedWeights)){
  #     vecMIN_WEIGHT[dfFixedWeights$INDEX_NUMBER[i]] = dfFixedWeights$WEIGHT[i]
  #     vecMAX_WEIGHT[dfFixedWeights$INDEX_NUMBER[i]] = -dfFixedWeights$WEIGHT[i]
  #   }
  # }
  
  # solve.QP {quadprog}
  # min(-d^T b + 1/2 b^T D b) with the constraints A^T b >= b_0
  # solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE)
  
  matA = matrix(cbind(matrix(1,nSec),        # EQUALITY - vector of ones for the SUM OF WEIGHTS
                      diag(1, nSec),         # INEQUALITY - diagonal ones for the MINIMUM WEIGHT constraint
                      diag(-1, nSec)         # INEQUALITY - diagonal of minus ones for the MAXIMUM WEIGHT constraint
  ), ncol = nSec*2+1, nrow = nSec)
  
  vecb = matrix(c(1,                         # EQUALITY - the SUM OF WEIGHTS
                  vecMIN_WEIGHT,             # INEQUALITY - the MINIMUM WEIGHT
                  vecMAX_WEIGHT              # INEQUALITY - the MAXIMUM WEIGHT
  ), ncol = 1)
  
  sol = solve.QP(Dmat = matCov, dvec = rep(0,nSec), Amat = matA, bvec = vecb, meq = 1)  # get minimum variance portfolio
  ptfWeights = data.frame(t(sol$solution))
  colnames(ptfWeights) = colnames(stdPL_stats$TS[,-1])
  
  maxPossibleRet = max(matER)
  minPossibleRet = sol$solution %*% matER
  
  seqTargetRet = rev(-seq(-maxPossibleRet, -minPossibleRet, 0.001))
  if(abs(seqTargetRet[1] - minPossibleRet) < 10^-6 ) seqTargetRet = seqTargetRet[-1]
  
  matA = matrix(cbind(matrix(1,nSec),        # EQUALITY - vector of ones for the SUM OF WEIGHTS
                      matER,                 # EQUILITY - vector of mean expected returns for the TARGET RETURN
                      diag(1, nSec),         # INEQUALITY - diagonal ones for the MINIMUM WEIGHT constraint
                      diag(-1, nSec)         # INEQUALITY - diagonal of minus ones for the MAXIMUM WEIGHT constraint
  ), ncol = nSec*2+2, nrow = nSec)
  
  vecb = matrix(c(1,                         # EQUALITY - the SUM OF WEIGHTS
                  seqTargetRet[1],           # EQUALITY - the TARGET RETURN
                  vecMIN_WEIGHT,             # INEQUALITY - the MINIMUM WEIGHT
                  vecMAX_WEIGHT              # INEQUALITY - the MAXIMUM WEIGHT
  ), ncol = 1)
  
  for (targetRet in seqTargetRet){
    vecb[2] = targetRet
    sol = try(solve.QP(Dmat = matCov, dvec = rep(0,nSec), Amat = matA, bvec = vecb, meq = 2), silent = T)  # get min var portfolio at target return
    
    if ('try-error' %in% class(sol)) break
    else ptfWeights[nrow(ptfWeights)+1, ] = t(sol$solution)
  }
  
  ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(x<vecMIN_WEIGHT, vecMIN_WEIGHT, x))))
  ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(x>-vecMAX_WEIGHT, -vecMAX_WEIGHT, x))))
  ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(abs(x-vecMIN_WEIGHT)<10^-4, vecMIN_WEIGHT, x))))
  ptfWeights = as.data.frame(t(apply(ptfWeights,1, function(x) ifelse(abs(x+vecMAX_WEIGHT)<10^-4, -vecMAX_WEIGHT, x))))
  
  ptfWeights$pa_ret = as.numeric(t(t(ptfWeights[,1:nSec])) %*% matER)
  ptfWeights$pa_vola = as.numeric(t(t(apply(ptfWeights[,1:nSec], 1, function(x) sqrt( t(x) %*% matCov %*% t(t(x)) ) ))))
  ptfWeights$Sharpe = as.numeric(ptfWeights$pa_ret / ptfWeights$pa_vola)
  
  dfMDD = data.frame(apply(ptfWeights, 1, function(x) t(t(exp(stdPL_stats$FD[,-1])-1)) %*% t(t(x[1:nSec]))))
  row.names(dfMDD) = stdPL_stats$FD$DATE
  ptfWeights$mDD = (as.numeric(t(maxDrawdown(dfMDD))))
  
  return(ptfWeights)
}