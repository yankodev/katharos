getTE_Cleaned_Weights = function (stdPL_IS_BMK, weights_MAX_SHARPE, dfGlobalWeightRestr, lHyperParam){
  nFactors = (ncol(stdPL_IS_BMK$TS)-1)
  weights_MAX_SHARPE_TE = weights_MAX_SHARPE
  if (nFactors > 5){
    for (i in 1:(nFactors-5)){
      MS_Zero_Weights = which(round(weights_MAX_SHARPE_TE*100,0)==0)
      MS_Min_Weight = which(abs(weights_MAX_SHARPE_TE) == abs(min(weights_MAX_SHARPE_TE[round(weights_MAX_SHARPE_TE*100,0)>0])))
      tmp_weights_MAX_SHARPE_TE = weights_MAX_SHARPE_TE
      
      weights_MAX_SHARPE_TE[c(MS_Min_Weight, MS_Zero_Weights)] = 0
      
      vecMAX_WEIGHT = dfGlobalWeightRestr[dfGlobalWeightRestr$index_id %in% colnames(stdPL_IS_BMK$FD[,-c(MS_Min_Weight+2, MS_Zero_Weights+2),with=F]), "vecMAX_WEIGHT"]
      vecMAX_WEIGHT = ifelse(vecMAX_WEIGHT >= 1, 100, vecMAX_WEIGHT)
      
      if (sum(vecMAX_WEIGHT)<1.1){ break}
      ptfWeights_MV_EF_IS_TE = try(getMINTE_CONSTRAINED(stdPL_IS_BMK$FD[,-c(MS_Min_Weight+2, MS_Zero_Weights+2),with=F], "WKL", vecMAX_WEIGHT))
      if ('try-error' %in% class(ptfWeights_MV_EF_IS_TE)){ break}
      
      weights_tmp = ptfWeights_MV_EF_IS_TE[1,2:(ncol(ptfWeights_MV_EF_IS_TE)-4)]
      if(ptfWeights_MV_EF_IS_TE$pa_te>lHyperParam$numCleanWeight_TE_tol || ptfWeights_MV_EF_IS_TE$pa_active_ret < lHyperParam$numCleanWeight_AR_pa_tol) break
      for (idx in colnames(weights_tmp)) weights_MAX_SHARPE_TE[,idx] = weights_tmp[idx]
      # print(ptfWeights_MV_EF_IS_TE)
    }
    weights_MAX_SHARPE_TE = tmp_weights_MAX_SHARPE_TE
  }
  return(weights_MAX_SHARPE_TE)
}

getTE_Cleaned_Weights_UNCONSTRAINED = function (stdPL_IS_BMK, weights_MAX_SHARPE, dfGlobalWeightRestr, lHyperParam){
  nFactors = (ncol(stdPL_IS_BMK$TS)-1)
  weights_MAX_SHARPE_TE = weights_MAX_SHARPE
  if (nFactors > 5){
    for (i in 1:(nFactors-5)){
      MS_Zero_Weights = which(round(weights_MAX_SHARPE_TE*100,0)==0)
      MS_Min_Weight = which(abs(weights_MAX_SHARPE_TE) == abs(min(weights_MAX_SHARPE_TE[round(weights_MAX_SHARPE_TE*100,0)>0])))
      tmp_weights_MAX_SHARPE_TE = weights_MAX_SHARPE_TE
      
      weights_MAX_SHARPE_TE[c(MS_Min_Weight, MS_Zero_Weights)] = 0
      
      vecMAX_WEIGHT = dfGlobalWeightRestr[dfGlobalWeightRestr$index_id %in% colnames(stdPL_IS_BMK$FD[,-c(MS_Min_Weight+2, MS_Zero_Weights+2),with=F]), "vecMAX_WEIGHT"]
      vecMAX_WEIGHT = ifelse(vecMAX_WEIGHT >= 1, 100, vecMAX_WEIGHT)
      
      ptfWeights_MV_EF_IS_TE = try(getMINTE_FAST(stdPL_IS_BMK$FD[,-c(MS_Min_Weight+2, MS_Zero_Weights+2),with=F], "WKL"))
      if ('try-error' %in% class(ptfWeights_MV_EF_IS_TE)) break
      
      weights_tmp = ptfWeights_MV_EF_IS_TE[1,2:(ncol(ptfWeights_MV_EF_IS_TE)-4)]
      if(ptfWeights_MV_EF_IS_TE$pa_te>lHyperParam$numCleanWeight_TE_tol || ptfWeights_MV_EF_IS_TE$pa_active_ret < lHyperParam$numCleanWeight_AR_pa_tol) break
      for (idx in colnames(weights_tmp)) weights_MAX_SHARPE_TE[,idx] = weights_tmp[idx]
      # print(ptfWeights_MV_EF_IS_TE)
    }
    weights_MAX_SHARPE_TE = tmp_weights_MAX_SHARPE_TE
  }
  return(weights_MAX_SHARPE_TE)
}