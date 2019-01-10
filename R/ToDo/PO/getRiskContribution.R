getRiskMetrics = function (FD, vecWeights, annFactor){
  library(data.table)
  library(tidyr)
  library(DT)
  
  matCov = cov(FD)
  strColNames = colnames(FD)
  nSec = ncol(FD)
  
  ptfVol_per_freq = sqrt(as.matrix(t(vecWeights)) %*% matCov %*% (vecWeights))
  ptfVol_annualized = ptfVol_per_freq * annFactor
  var99_per_freq = 2.33 * ptfVol_per_freq
  var_contribution = data.frame(Factor = colnames(FD), VaR_Contribution = apply((2.33^2) * (vecWeights) * matCov %*% (vecWeights), 1, '/', var99_per_freq^2))
  
  dtSummary = data.table(Days = round(365/annFactor^2,0), Vola = ptfVol_per_freq, Vola_pa = ptfVol_annualized, VaR_99 = var99_per_freq)
  colnames(dtSummary) = c("Days", "Vola", "Vola_pa", "VaR_99")
  
  return(list(dtSummary = dtSummary, var_contribution = var_contribution))
}


getRiskContributionMatrix = function (FD, vecWeights, annFactor){
  library(data.table)
  library(tidyr)
  library(DT)
  
  matCov = cov(FD)
  
  ptfVol_per_freq = apply(as.matrix(vecWeights), 1, function(x) sqrt(t(x) %*% matCov %*% x))
  ptfVol_annualized = ptfVol_per_freq * annFactor
  var99_per_freq = 2.33 * ptfVol_per_freq
  dfRiskContrib = t(apply(vecWeights, 1, function(x) (2.33^2) * x * matCov %*% x) ) / (var99_per_freq^2)
  colnames(dfRiskContrib) = colnames(FD)
  
  return(dfRiskContrib)
}