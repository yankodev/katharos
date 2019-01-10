library(nloptr)

#### -------- EQUAL RISK CONTRIBUION  -----------------
getERC = function (FD, vecMIN_WEIGHT, vecMAX_WEIGHT){
  matCov = cov(FD)
  strColNames = colnames(FD)
  nSec = ncol(FD)
  
  if(missing(vecMIN_WEIGHT)) vecMIN_WEIGHT = rep(-0.8, nSec)
  if(missing(vecMAX_WEIGHT)) vecMAX_WEIGHT = rep(1, nSec)
  x0 = (vecMAX_WEIGHT + vecMIN_WEIGHT) / 2

  # min f(x) x in R^n
  # g(x) <= 0
  # h(x) = 0
  # lb <= x <= ub
  
  res = nloptr(x0 = x0,                                                                       # x_0
               eval_f = ercTargetFun,                                                          # f(x)
               eval_grad_f = ercGradientFun,                                                   # gradient of f(x)
               eval_g_eq = function(vecWeights, matCov) { sum(vecWeights) - 1 },              # h(x)
               eval_jac_g_eq = function(vecWeights, matCov) { rep(1,length(vecWeights)) },    # Jacobian of h(x)
               lb = vecMIN_WEIGHT,
               ub = vecMAX_WEIGHT,
               opts = list("algorithm"="NLOPT_LD_SLSQP","print_level" = 0,"xtol_rel"=1.0e-8,"maxeval" = 1000),
               matCov = matCov                                                                # optional argument to be passed to f, g, ...
               )
  
  sol = data.frame(t(res$solution))
  colnames(sol) = strColNames
  
  # var99 = 2.33 * sqrt(as.matrix(sol) %*% matCov %*% t(sol))
  # apply((2.33^2) * matCov %*% t(sol), 1, '/', var99) %*% t(sol) -var99      # check 1
  # sum(apply((2.33^2) * t(sol) * matCov %*% t(sol), 1, '/', var99))-var99    # check 2
  # round((apply((2.33^2) * t(sol) * matCov %*% t(sol), 1, '/', var99^2)),4)   # check 3
  
  sol$PtfVar99 = 2.33 * sqrt(as.matrix(sol) %*% matCov %*% t(sol))
  sol$TR = apply(sol[,strColNames], 1, function(x) prod(1+as.matrix(FD) %*% as.matrix(x, ncol=1))-1)
  return(sol)
}

# Equal Risk Contribution Target Function
ercTargetFun = function(vecWeights, matCov) {
  ptfVol = sqrt(as.numeric(t(vecWeights) %*% matCov %*% vecWeights))
  vecMarginalContributions = vecWeights * matCov %*% vecWeights / ptfVol
  return( (max(vecMarginalContributions)-min(vecMarginalContributions))^2 )
}

# Numerical Gradient Approximation of the Equial Risk Contribution Target Function
ercGradientFun = function(vecWeights, matCov) {
  h = 0.0001
  vecFunValUp = vecFunValDown = vecGradient = vecWeights
  for (i in 1:length(vecWeights)){
    vecFunValUp[i] = vecWeights[i] + h
    vecFunValDown[i] = vecWeights[i] - h
    vecGradient[i] = (ercTargetFun(vecFunValUp, matCov) - ercTargetFun(vecFunValDown, matCov)) / (2 * h)
    vecFunValUp = vecFunValDown = vecWeights
  }
  return(vecGradient)
}




#### -------- FIXED RISK CONTRIBUTION!  -----------------
getFRC = function (FD, vecMIN_WEIGHT, vecMAX_WEIGHT, vecRP_CONTRIB){
  matCov = cov(FD)
  strColNames = colnames(FD)
  nSec = ncol(FD)
  
  if(missing(vecMIN_WEIGHT)) vecMIN_WEIGHT = rep(-0.8, nSec)
  if(missing(vecMAX_WEIGHT)) vecMAX_WEIGHT = rep(1, nSec)
  if(missing(vecRP_CONTRIB)) vecRP_CONTRIB = rep(1 / nSec, nSec)
  
  x0 = rep(1 / nSec, nSec)
  
  # min f(x) x in R^n
  # g(x) <= 0
  # h(x) = 0
  # lb <= x <= ub
  
  res = nloptr(x0 = x0,                                                                       # x_0
               eval_f = frcTargetFun,                                                   # f(x)
               eval_grad_f = frcGradientFun,                                            # gradient of f(x)
               eval_g_eq = function(vecWeights, matCov, vecRP_CONTRIB) { sum(vecWeights) - 1 },              # h(x)
               eval_jac_g_eq = function(vecWeights, matCov, vecRP_CONTRIB) { rep(1,length(vecWeights)) },    # Jacobian of h(x)
               lb = vecMIN_WEIGHT,
               ub = vecMAX_WEIGHT,
               opts = list("algorithm"="NLOPT_LD_SLSQP","print_level" = 0,"xtol_rel"=1.0e-4,"maxeval" = 1000),
               matCov = matCov,                                                             #  optional argument to be passed to f, g, ...
               vecRP_CONTRIB = vecRP_CONTRIB                                                # optional argument to be passed to f, g, ...
  )
  
  sol = data.frame(t(res$solution))
  colnames(sol) = strColNames
  
  # var99 = 2.33 * sqrt(as.matrix(sol) %*% matCov %*% t(sol))
  # apply((2.33^2) * matCov %*% t(sol), 1, '/', var99) %*% t(sol) -var99      # check 1
  # sum(apply((2.33^2) * t(sol) * matCov %*% t(sol), 1, '/', var99))-var99    # check 2
  # round((apply((2.33^2) * t(sol) * matCov %*% t(sol), 1, '/', var99^2)) - vecRP_CONTRIB,4)   # check 3
  
  sol$PtfVar99 = 2.33 * sqrt(as.matrix(sol) %*% matCov %*% t(sol))
  sol$TR = apply(sol[,strColNames], 1, function(x) prod(1+as.matrix(FD) %*% as.matrix(x, ncol=1))-1)
  return(sol)
}

# FIXED Risk Contribution Target Function
frcTargetFun = function(vecWeights, matCov, vecRP_CONTRIB) {
  ptfVol = sqrt(as.numeric(t(vecWeights) %*% matCov %*% vecWeights))
  vecMarginalContributions = vecWeights * matCov %*% vecWeights / ptfVol / ptfVol
  return( sum((vecMarginalContributions-vecRP_CONTRIB)^2) )   # SKALIERUNG überprüfen!
}


# Numerical Gradient Approximation of the FIXED Risk Contribution Target Function
frcGradientFun = function(vecWeights, matCov, vecRP_CONTRIB) {
  h = 0.0001
  vecFunValUp = vecFunValDown = vecGradient = vecWeights
  for (i in 1:length(vecWeights)){
    vecFunValUp[i] = vecWeights[i] + h
    vecFunValDown[i] = vecWeights[i] - h
    vecGradient[i] = (frcTargetFun(vecFunValUp, matCov, vecRP_CONTRIB) - frcTargetFun(vecFunValDown, matCov, vecRP_CONTRIB)) / (2 * h)
    vecFunValUp = vecFunValDown = vecWeights
  }
  return(vecGradient)
}
