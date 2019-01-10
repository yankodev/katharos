#### -------- LARGEST SKEW at max VARIANCE  -----------------
getSkewOptimum_AT_maxVar = function (FD, vecMIN_WEIGHT, vecMAX_WEIGHT, maxVar, T){
  matCov = as.matrix(cov(FD))
  strColNames = colnames(FD)
  nSec = ncol(FD)
  FD = as.matrix(FD)
  ret = as.matrix(apply(FD, 2, function(x) prod(exp(x))^(1/T)-1 ), ncol = 1)
  
  if(missing(vecMIN_WEIGHT)) vecMIN_WEIGHT = rep(-0.8*0, nSec)
  if(missing(vecMAX_WEIGHT)) vecMAX_WEIGHT = rep(1, nSec)
  if(missing(maxVar)) maxVar = 8.6e-04
  
  x0 = rep(1 / nSec, nSec)
  
  
  # min f(x) x in R^n
  # g(x) <= 0
  # h(x) = 0
  # lb <= x <= ub
  
  res = nloptr(x0 = x0,                                                                       # x_0
               eval_f = skewOptTargetFun,                                                   # f(x)
               eval_grad_f = skewOptGradientFun,                                            # gradient of f(x)
               eval_g_eq = function(vecWeights, FD, matCov) {sum(vecWeights) - 1},              # h(x)
               eval_jac_g_eq = function(vecWeights, FD, matCov) {rep(1,length(vecWeights))},    # Jacobian of h(x)
               eval_g_ineq = function(vecWeights, FD, matCov) {t(vecWeights) %*% matCov %*% t(t(vecWeights)) - maxVar},
               eval_jac_g_ineq = function(vecWeights, FD, matCov) {2 * t(vecWeights) %*% matCov},
               lb = vecMIN_WEIGHT,
               ub = vecMAX_WEIGHT,
               opts = list("algorithm"="NLOPT_LD_SLSQP","print_level" = 0,"xtol_rel"=1.0e-4,"maxeval" = 1000),
               FD = FD,
               matCov = matCov
  )
  
  sol = data.frame(t(res$solution))
  colnames(sol) = strColNames
  
  # var99 = 2.33 * sqrt(as.matrix(sol) %*% matCov %*% t(sol))
  # apply((2.33^2) * matCov %*% t(sol), 1, '/', var99) %*% t(sol) -var99      # check 1
  # sum(apply((2.33^2) * t(sol) * matCov %*% t(sol), 1, '/', var99))-var99    # check 2
  # round((apply((2.33^2) * t(sol) * matCov %*% t(sol), 1, '/', var99^2)) - vecRP_CONTRIB,4)   # check 3
  
  # sol$PtfVar99 = 2.33 * sqrt(as.matrix(sol) %*% matCov %*% t(sol))
  # sol$TR = apply(sol[,strColNames], 1, function(x) prod(1+as.matrix(FD) %*% as.matrix(x, ncol=1))-1)
  
  sol$Vola_pa = sqrt(as.matrix(sol) %*% matCov %*% (t(as.matrix(sol)))*52)
  sol$Vola_pa_target = sqrt(maxVar*52)
  
  sol$ret_pa = as.matrix(sol[1:nSec]) %*% ret
  sol$min_ret_pa_target = NA
  
  sol$Res_Solution = -res$objective
  sol$SkewNess = skewness(FD %*% (res$solution))
  return(sol)
}




#### -------- LARGEST SKEW at min TE -----------------
getSkewOptimum_AT_maxTE = function (FD, vecMIN_WEIGHT, vecMAX_WEIGHT, maxTE, T, FD_bmk, annFactor){
  matCov = as.matrix(cov(cbind(FD_bmk, FD)))
  strColNames = colnames(FD)
  nSec = ncol(FD)
  FD = as.matrix(FD)
  ret = as.matrix(apply(FD, 2, function(x) prod(exp(x))^(1/T)-1 ), ncol = 1)
  
  if(missing(vecMIN_WEIGHT)) vecMIN_WEIGHT = rep(-0.8*0, nSec)
  if(missing(vecMAX_WEIGHT)) vecMAX_WEIGHT = rep(1, nSec)
  if(missing(maxTE)) maxTE = 1
  # 
  # maxTE = 0.09
  maxTE = (maxTE / annFactor)^2
  
  x0 = rep(1 / nSec, nSec)
  
  
  # min f(x) x in R^n
  # g(x) <= 0
  # h(x) = 0
  # lb <= x <= ub
  
  res = nloptr(x0 = x0,                                                                       # x_0
               eval_f = skewOptTargetFun,                                                   # f(x)
               eval_grad_f = skewOptGradientFun,                                            # gradient of f(x)
               eval_g_eq = function(vecWeights, FD, matCov) {sum(vecWeights) - 1},              # h(x)
               eval_jac_g_eq = function(vecWeights, FD, matCov) {rep(1,length(vecWeights))},    # Jacobian of h(x)
               eval_g_ineq = function(vecWeights, FD, matCov) {t(c(-1,vecWeights)) %*% matCov %*% t(t(c(-1,vecWeights))) - maxTE},
               eval_jac_g_ineq = function(vecWeights, FD, matCov) {2 * t(vecWeights) %*% matCov[-1,-1]},
               lb = vecMIN_WEIGHT,
               ub = vecMAX_WEIGHT,
               opts = list("algorithm"="NLOPT_LD_SLSQP","print_level" = 0,"xtol_rel"=1.0e-4,"maxeval" = 1000),
               FD = FD,
               matCov = matCov
  )
  
  sol = data.frame(t(res$solution))
  colnames(sol) = strColNames
  
  # var99 = 2.33 * sqrt(as.matrix(sol) %*% matCov %*% t(sol))
  # apply((2.33^2) * matCov %*% t(sol), 1, '/', var99) %*% t(sol) -var99      # check 1
  # sum(apply((2.33^2) * t(sol) * matCov %*% t(sol), 1, '/', var99))-var99    # check 2
  # round((apply((2.33^2) * t(sol) * matCov %*% t(sol), 1, '/', var99^2)) - vecRP_CONTRIB,4)   # check 3
  
  # sol$PtfVar99 = 2.33 * sqrt(as.matrix(sol) %*% matCov %*% t(sol))
  # sol$TR = apply(sol[,strColNames], 1, function(x) prod(1+as.matrix(FD) %*% as.matrix(x, ncol=1))-1)
  
  sol$Vola_pa = sqrt(as.matrix(sol) %*% matCov[-1,-1] %*% (t(as.matrix(sol))))*annFactor
  sol$Vola_pa_target = NA
  
  sol$ret_pa = as.matrix(sol[1:nSec]) %*% ret
  sol$min_ret_pa_target = NA
  sol$act_ret_pa = sol$ret_pa - (prod(exp(FD_bmk))^(1/T)-1)
  
  sol$Res_Solution = -res$objective
  sol$SkewNess = skewness(FD %*% (res$solution))
  sol$TE_pa = annFactor * sqrt(t(c(-1, res$solution)) %*% matCov  %*% t(t(c(-1, res$solution))))
  sol$TE_pa_max_target = sqrt(maxTE) * annFactor
  return(sol)
}




#### -------- LARGEST SKEW at max VARIANCE  -----------------
getSkewOptimum_AT_minVar = function (FD, vecMIN_WEIGHT, vecMAX_WEIGHT, minVar, T){
  matCov = as.matrix(cov(FD))
  strColNames = colnames(FD)
  nSec = ncol(FD)
  FD = as.matrix(FD)
  ret = as.matrix(apply(FD, 2, function(x) prod(exp(x))^(1/T)-1 ), ncol = 1)
  
  if(missing(vecMIN_WEIGHT)) vecMIN_WEIGHT = rep(-0.8*0, nSec)
  if(missing(vecMAX_WEIGHT)) vecMAX_WEIGHT = rep(1, nSec)
  if(missing(minVar)) minVar = 0
  
  x0 = rep(1 / nSec, nSec)
  
  
  # min f(x) x in R^n
  # g(x) <= 0
  # h(x) = 0
  # lb <= x <= ub
  
  res = nloptr(x0 = x0,                                                                       # x_0
               eval_f = skewOptTargetFun,                                                   # f(x)
               eval_grad_f = skewOptGradientFun,                                            # gradient of f(x)
               # eval_g_eq = function(vecWeights, FD, matCov) {sum(vecWeights) - 1},              # h(x)
               # eval_jac_g_eq = function(vecWeights, FD, matCov) {rep(1,length(vecWeights))},    # Jacobian of h(x)
               # eval_g_ineq = function(vecWeights, FD, matCov) {-t(vecWeights) %*% matCov %*% t(t(vecWeights)) + minVar},
               # eval_jac_g_ineq = function(vecWeights, FD, matCov) {-2 * t(vecWeights) %*% matCov},
               eval_g_eq = function(vecWeights, FD, matCov) {-t(vecWeights) %*% matCov %*% t(t(vecWeights)) + minVar},
               eval_jac_g_eq = function(vecWeights, FD, matCov) {-2 * t(vecWeights) %*% matCov},
               lb = vecMIN_WEIGHT,
               ub = vecMAX_WEIGHT,
               opts = list("algorithm"="NLOPT_LD_SLSQP","print_level" = 0,"xtol_rel"=1.0e-4,"maxeval" = 1000),
               FD = FD,
               matCov = matCov
  )
  
  sol = data.frame(t(res$solution))
  colnames(sol) = strColNames
  
  # var99 = 2.33 * sqrt(as.matrix(sol) %*% matCov %*% t(sol))
  # apply((2.33^2) * matCov %*% t(sol), 1, '/', var99) %*% t(sol) -var99      # check 1
  # sum(apply((2.33^2) * t(sol) * matCov %*% t(sol), 1, '/', var99))-var99    # check 2
  # round((apply((2.33^2) * t(sol) * matCov %*% t(sol), 1, '/', var99^2)) - vecRP_CONTRIB,4)   # check 3
  
  # sol$PtfVar99 = 2.33 * sqrt(as.matrix(sol) %*% matCov %*% t(sol))
  # sol$TR = apply(sol[,strColNames], 1, function(x) prod(1+as.matrix(FD) %*% as.matrix(x, ncol=1))-1)
  
  sol$Vola_pa = sqrt(as.matrix(sol) %*% matCov %*% (t(as.matrix(sol)))*52)
  sol$Vola_pa_target = sqrt(minVar*52)
  
  sol$ret_pa = as.matrix(sol[1:nSec]) %*% ret
  sol$min_ret_pa_target = NA
  
  sol$Res_Solution = -res$objective
  sol$SkewNess = skewness(FD %*% (res$solution))
  return(sol)
}


#### -------- LARGEST SKEW at min RETURN  -----------------
getSkewOptimum_AT_minRet = function (FD, vecMIN_WEIGHT, vecMAX_WEIGHT, minRet, T){
  matCov = as.matrix(cov(FD))
  strColNames = colnames(FD)
  nSec = ncol(FD)
  FD = as.matrix(FD)
  ret = as.matrix(apply(FD, 2, function(x) prod(exp(x))^(1/T)-1 ), ncol = 1)
  
  if(missing(vecMIN_WEIGHT)) vecMIN_WEIGHT = rep(-0.8*0, nSec)
  if(missing(vecMAX_WEIGHT)) vecMAX_WEIGHT = rep(1, nSec)
  if(missing(minRet)) minRet = 0
  
  x0 = rep(1 / nSec, nSec)
  
  
  # min f(x) x in R^n
  # g(x) <= 0
  # h(x) = 0
  # lb <= x <= ub
  
  res = nloptr(x0 = x0,                                                                       # x_0
               eval_f = skewOptTargetFun,                                                   # f(x)
               eval_grad_f = skewOptGradientFun,                                            # gradient of f(x)
               eval_g_eq = function(vecWeights, FD, matCov) {sum(vecWeights) - 1},              # h(x)
               eval_jac_g_eq = function(vecWeights, FD, matCov) {rep(1,length(vecWeights))},    # Jacobian of h(x)
               eval_g_ineq = function(vecWeights, FD, matCov) {-t(vecWeights) %*% ret + minRet},
               eval_jac_g_ineq = function(vecWeights, FD, matCov) {-ret},
               lb = vecMIN_WEIGHT,
               ub = vecMAX_WEIGHT,
               opts = list("algorithm"="NLOPT_LD_SLSQP","print_level" = 0,"xtol_rel"=1.0e-4,"maxeval" = 1000),
               FD = FD,
               matCov = matCov
  )
  
  sol = data.frame(t(res$solution))
  colnames(sol) = strColNames
  
  # var99 = 2.33 * sqrt(as.matrix(sol) %*% matCov %*% t(sol))
  # apply((2.33^2) * matCov %*% t(sol), 1, '/', var99) %*% t(sol) -var99      # check 1
  # sum(apply((2.33^2) * t(sol) * matCov %*% t(sol), 1, '/', var99))-var99    # check 2
  # round((apply((2.33^2) * t(sol) * matCov %*% t(sol), 1, '/', var99^2)) - vecRP_CONTRIB,4)   # check 3
  
  # sol$PtfVar99 = 2.33 * sqrt(as.matrix(sol) %*% matCov %*% t(sol))
  # sol$TR = apply(sol[,strColNames], 1, function(x) prod(1+as.matrix(FD) %*% as.matrix(x, ncol=1))-1)
  
  sol$Vola_pa = sqrt(as.matrix(sol) %*% matCov %*% (t(as.matrix(sol)))*52)
  sol$Vola_pa_target = NA
  
  sol$ret_pa = as.matrix(sol[1:nSec]) %*% ret
  sol$min_ret_pa_target = minRet
  
  sol$Res_Solution = -res$objective
  sol$SkewNess = skewness(FD %*% (res$solution))
  return(sol)
}

# SKEW target function
skewOptTargetFun = function(vecWeights, FD, matCov) {
  ptfRet = FD %*% vecWeights
  # return(skewness(ptfRet)^2)
  return(-skewness(ptfRet))
}


# Numerical Gradient Approximation of the SKEW Target Function
skewOptGradientFun = function(vecWeights, FD, matCov) {
  h = 0.0001
  vecFunValUp = vecFunValDown = vecGradient = vecWeights
  for (i in 1:length(vecWeights)){
    vecFunValUp[i] = vecWeights[i] + h
    vecFunValDown[i] = vecWeights[i] - h
    vecGradient[i] = (skewOptTargetFun(vecFunValUp, FD, matCov) - skewOptTargetFun(vecFunValDown, FD, matCov)) / (2 * h)
    vecFunValUp = vecFunValDown = vecWeights
  }
  return(vecGradient)
}