getMaxSharpe_IS_OS_Vol_Scaled_Iter = function(stdPL_IS, stdPL_OS, dfGlobalWeightRestr, targetVola_pa, lHyperParam){
  if(!missing(targetVola_pa)){
    targetVola_wkl = targetVola_pa / sqrt(52)
    volScale_InSample = apply(as.matrix(stdPL_IS$FD[,-1]), 2, function(x) targetVola_wkl / sd(x))
    stdPL_IS_VolScaled = list(FD=data.table(DATE = stdPL_IS$FD$DATE, data.table(sweep(stdPL_IS$FD[,-1], 2, volScale_InSample, `*`))))
    stdPL_IS_VolScaled$TS = data.table(DATE = stdPL_IS$TS$DATE,
                                       data.table(apply(as.matrix(stdPL_IS_VolScaled$FD[,-1]), 2, function(x) c(1,cumprod(exp(x))))))
    
    stdPL_OS_VolScaled = list(FD=data.table(DATE = stdPL_OS$FD$DATE, data.table(sweep(stdPL_OS$FD[,-1], 2, volScale_InSample, `*`))))
    stdPL_OS_VolScaled$TS = data.table(DATE = stdPL_OS$TS$DATE,
                                       data.table(apply(as.matrix(stdPL_OS_VolScaled$FD[,-1]), 2, function(x) c(1,cumprod(exp(x))))))
    
    stdPL_IS = stdPL_IS_VolScaled
    stdPL_OS = stdPL_OS_VolScaled
  }
  
  ####     IN-SAMPLE      ####
  nFactors = (ncol(stdPL_IS$TS)-1)
  ptfWeights_MV_EF_IS = getMVEF(getSecStats(stdPL_IS), dfGlobalWeightRestr$vecMIN_WEIGHT, dfGlobalWeightRestr$vecMAX_WEIGHT)
  weights_MIN_VAR_IS = ptfWeights_MV_EF_IS[1, 1:nFactors]
  weights_MAX_SHARPE_IS = ptfWeights_MV_EF_IS[which(ptfWeights_MV_EF_IS$Sharpe == max(ptfWeights_MV_EF_IS$Sharpe)), 1:nFactors]
  
  FD_MIN_VAR_IS = as.matrix(stdPL_IS$FD[,colnames(weights_MIN_VAR_IS), with = F]) %*% t(as.matrix(weights_MIN_VAR_IS))
  FD_MAX_SHARPE_IS = as.matrix(stdPL_IS$FD[,colnames(weights_MAX_SHARPE_IS), with = F]) %*% t(as.matrix(weights_MAX_SHARPE_IS))
  
  stdPL_IS_BMK =list(FD = data.table(DATE=stdPL_IS$FD$DATE, BMK = as.numeric(FD_MAX_SHARPE_IS),stdPL_IS$FD[,-1]))
  stdPL_IS_BMK$TS = data.table(DATE=stdPL_IS$TS$DATE, BMK = c(1, cumprod(exp(FD_MAX_SHARPE_IS))),stdPL_IS$TS[,-1])
  # colnames(stdPL_IS_BMK$FD)[2]="BMK"
  
  weights_MAX_SHARPE_IS_TE_Clean = getTE_Cleaned_Weights (stdPL_IS_BMK, weights_MAX_SHARPE_IS, dfGlobalWeightRestr, lHyperParam) 
  FD_MAX_SHARPE_IS = data.table(DATE = stdPL_IS$FD$DATE, MAX_SHARPE_IS_TE = as.matrix(stdPL_IS$FD[,colnames(weights_MAX_SHARPE_IS_TE_Clean), with = F]) %*% t(as.matrix(weights_MAX_SHARPE_IS_TE_Clean)))
  stdPL_SecStats_IS = getSecStats_FromFD(FD_MAX_SHARPE_IS)
  # stdPL_SecStats_IS$Stats
  
  ####     OUT-OF-SAMPLE      ####
  FD_MAX_SHARPE_OS = data.table(DATE = stdPL_OS$FD$DATE, MAX_SHARPE_OS_TE = as.matrix(stdPL_OS$FD[,colnames(weights_MAX_SHARPE_IS_TE_Clean), with = F]) %*% t(as.matrix(weights_MAX_SHARPE_IS_TE_Clean)))
  stdPL_SecStats_OS = getSecStats_FromFD(FD_MAX_SHARPE_OS)
  
  # stdPL_SecStats_OS$Stats
  # plot(stdPL_SecStats_IS$TS, type ="l")
  # plot(stdPL_SecStats_OS$TS, type ="l")
  
  FD_MAX_SHARPE_OS_Attrib = sweep(as.matrix(stdPL_OS$FD[,colnames(weights_MAX_SHARPE_IS_TE_Clean), with = F]), 2, as.matrix(weights_MAX_SHARPE_IS_TE_Clean), `*`)
  FD_MAX_SHARPE_OS_Attrib = FD_MAX_SHARPE_OS_Attrib[, apply(abs(FD_MAX_SHARPE_OS_Attrib), 2, max)>0]
  TS_MAX_SHARPE_OS_Attrib = data.table(DATE =stdPL_OS$TS$DATE, apply(FD_MAX_SHARPE_OS_Attrib, 2, function(x) c(1, cumprod(exp(x)))))
  
  dtTS = data.table(DATE = stdPL_OS$TS$DATE, "MaxSharpe_OS" = c(1, cumprod(exp(as.matrix(FD_MAX_SHARPE_OS[,-1])))))
  
  # plot_ly(gather(dtTS, TS_ID, INDEX, -DATE), x=~DATE, y=~INDEX, color=~TS_ID, type ="scatter", mode = "lines") %>%
  #   layout(xaxis = list(title = ""))  %>%
  #   layout(yaxis = list(title = "", tickformat = ".0%")) %>%
  #   layout(title = "MAX Sharpe (OUT-OF-sample, IS-Vol SCALED to 5%)", legend = list(x = 0, y = 1))
  # 
  # plot_ly(gather(TS_MAX_SHARPE_OS_Attrib, TS_ID, INDEX, -DATE), x=~DATE, y=~INDEX, color=~TS_ID, type ="scatter", mode = "lines") %>%
  #   layout(xaxis = list(title = ""))  %>%
  #   layout(yaxis = list(title = "", tickformat = ".0%")) %>%
  #   layout(title = "MAX Sharpe Attribution (OUT-OF-sample, IS-Vol SCALED to 5%)", legend = list(x = 0, y = 1))
  
  stdPL_SecStats_OS_Attrib = getSecStats_FromFD(data.table(DATE = stdPL_OS$FD$DATE, FD_MAX_SHARPE_OS_Attrib))
  strFactorReduce_Ret = rownames(stdPL_SecStats_OS_Attrib$Stats$x$data)[min(which(stdPL_SecStats_OS_Attrib$Stats$x$data$ret_pa == min(stdPL_SecStats_OS_Attrib$Stats$x$data$ret_pa)))]
  strFactorReduce_MDD = rownames(stdPL_SecStats_OS_Attrib$Stats$x$data)[min(which(stdPL_SecStats_OS_Attrib$Stats$x$data$MDD == max(stdPL_SecStats_OS_Attrib$Stats$x$data$MDD)))]
  strFactorReduce_Skew = rownames(stdPL_SecStats_OS_Attrib$Stats$x$data)[min(which(stdPL_SecStats_OS_Attrib$Stats$x$data$Skewness == min(stdPL_SecStats_OS_Attrib$Stats$x$data$Skewness)))]
  
  weights_MAX_SHARPE_IS_TE_Clean[strFactorReduce_Ret] = weights_MAX_SHARPE_IS_TE_Clean[strFactorReduce_Ret] * (1-lHyperParam$numReduceMaxWeight_dueto_Ret)
  weights_MAX_SHARPE_IS_TE_Clean[strFactorReduce_MDD] = weights_MAX_SHARPE_IS_TE_Clean[strFactorReduce_MDD] * (1-lHyperParam$numReduceMaxWeight_dueto_MDD)
  weights_MAX_SHARPE_IS_TE_Clean[strFactorReduce_Skew] = weights_MAX_SHARPE_IS_TE_Clean[strFactorReduce_Skew] * (1-lHyperParam$numReduceMaxWeight_dueto_Skew)
  
  dfGlobalWeightRestr[dfGlobalWeightRestr$index_id == strFactorReduce_Ret, "vecMAX_WEIGHT"] = weights_MAX_SHARPE_IS_TE_Clean[strFactorReduce_Ret]
  dfGlobalWeightRestr[dfGlobalWeightRestr$index_id == strFactorReduce_MDD, "vecMAX_WEIGHT"] = weights_MAX_SHARPE_IS_TE_Clean[strFactorReduce_MDD]
  dfGlobalWeightRestr[dfGlobalWeightRestr$index_id == strFactorReduce_Skew, "vecMAX_WEIGHT"] = weights_MAX_SHARPE_IS_TE_Clean[strFactorReduce_Skew]
  
  
  # dfGlobalWeightRestr[dfGlobalWeightRestr$index_id == strFactorReduce_Ret, "vecMAX_WEIGHT"] = weights_MAX_SHARPE_IS_TE_Clean[strFactorReduce_Ret] * (1-lHyperParam$numReduceMaxWeight_dueto_Ret)
  # dfGlobalWeightRestr[dfGlobalWeightRestr$index_id == strFactorReduce_MDD, "vecMAX_WEIGHT"] = weights_MAX_SHARPE_IS_TE_Clean[strFactorReduce_MDD] * (1-lHyperParam$numReduceMaxWeight_dueto_MDD)
  # dfGlobalWeightRestr[dfGlobalWeightRestr$index_id == strFactorReduce_Skew, "vecMAX_WEIGHT"] = weights_MAX_SHARPE_IS_TE_Clean[strFactorReduce_Skew] * (1-lHyperParam$numReduceMaxWeight_dueto_Skew)

  return(list(stdPL_SecStats_IS = stdPL_SecStats_IS, stdPL_SecStats_OS = stdPL_SecStats_OS, stdPL_SecStats_OS_Attrib = stdPL_SecStats_OS_Attrib, dfGlobalWeightRestr = dfGlobalWeightRestr, MS_Weights = weights_MAX_SHARPE_IS_TE_Clean))
}

getIS_OS_cuts = function(stdPL_FullSample, dtOOB_FROM, dtOOB_TO, nCuts){
  stdPL_InSample = list(TS = data.table(), FD = rbind(stdPL_FullSample$FD[DATE<dtOOB_FROM], stdPL_FullSample$FD[DATE>dtOOB_TO]))
  for (i in 2:nrow(stdPL_InSample$FD)) stdPL_InSample$FD$DATE[i] = stdPL_InSample$FD$DATE[i-1] + 7
  stdPL_InSample$TS = data.table(DATE = c(stdPL_InSample$FD$DATE[1] - (stdPL_InSample$FD$DATE[2]-stdPL_InSample$FD$DATE[1]), stdPL_InSample$FD$DATE),
                                 apply(stdPL_InSample$FD[,-1], 2, function(x) c(1,cumprod(exp(x)))))
  
  rndCutLength =  round(length(stdPL_InSample$TS$DATE) / nCuts / 2, 0)
  idxCuts = data.frame(CUT_ID = seq(1:(nCuts*2)))
  idxCuts$IDX_FROM =  rndCutLength * (idxCuts$CUT_ID-1) + 1
  idxCuts$IDX_TO =  rndCutLength * idxCuts$CUT_ID
  idxCuts$IDX_TO[nCuts*2] = length(stdPL_InSample$TS$DATE)
  
  IS_OS_cut_stdPL = list()
  for (i in 1:nCuts){
    IS_OS_cut_stdPL[[paste0("IS_", i)]] = list(TS = stdPL_InSample$TS[idxCuts$IDX_FROM[(i*2-1)]:idxCuts$IDX_TO[(i*2-1)]],
                                               FD = stdPL_InSample$FD[idxCuts$IDX_FROM[(i*2-1)]:(idxCuts$IDX_TO[(i*2-1)]-1)])
    
    IS_OS_cut_stdPL[[paste0("OS_", i)]] = list(TS = stdPL_InSample$TS[idxCuts$IDX_FROM[(i*2)]:idxCuts$IDX_TO[(i*2)]],
                                               FD = stdPL_InSample$FD[idxCuts$IDX_FROM[(i*2)]:(idxCuts$IDX_TO[(i*2)]-1)])
  }
  
  OOB_stdPL = list(TS = data.table(), FD = stdPL_FullSample$FD[DATE>=dtOOB_FROM & DATE<=dtOOB_TO])
  OOB_stdPL$TS = data.table(DATE = c(OOB_stdPL$FD$DATE[1] - (OOB_stdPL$FD$DATE[2]-OOB_stdPL$FD$DATE[1]), OOB_stdPL$FD$DATE),
                                 apply(OOB_stdPL$FD[,-1], 2, function(x) c(1,cumprod(exp(x)))))
  
  return(list(IS_OS_cut_stdPL = IS_OS_cut_stdPL, OOB_stdPL = OOB_stdPL, FullInSample_stdPL = stdPL_InSample))
}


# getIS_OS_cuts = function(stdPL_FullSample, dtOOB, nCuts){
#   stdPL_InSample = list(TS = stdPL_FullSample$TS[DATE < dtOOB], FD = stdPL_FullSample$FD[DATE < dtOOB])
#   rndCutLength =  round(length(stdPL_InSample$TS$DATE) / nCuts / 2, 0)
#   idxCuts = data.frame(CUT_ID = seq(1:(nCuts*2)))
#   idxCuts$IDX_FROM =  rndCutLength * (idxCuts$CUT_ID-1) + 1
#   idxCuts$IDX_TO =  rndCutLength * idxCuts$CUT_ID
#   idxCuts$IDX_TO[nCuts*2] = length(stdPL_InSample$TS$DATE)
#   
#   IS_OS_cut_stdPL = list()
#   for (i in 1:nCuts){
#     IS_OS_cut_stdPL[[paste0("IS_", i)]] = list(TS = stdPL_InSample$TS[idxCuts$IDX_FROM[(i*2-1)]:idxCuts$IDX_TO[(i*2-1)]],
#                                                FD = stdPL_InSample$FD[idxCuts$IDX_FROM[(i*2-1)]:(idxCuts$IDX_TO[(i*2-1)]-1)])
#     
#     IS_OS_cut_stdPL[[paste0("OS_", i)]] = list(TS = stdPL_InSample$TS[idxCuts$IDX_FROM[(i*2)]:idxCuts$IDX_TO[(i*2)]],
#                                                FD = stdPL_InSample$FD[idxCuts$IDX_FROM[(i*2)]:(idxCuts$IDX_TO[(i*2)]-1)])
#   }
#   
#   OOB_stdPL = list(TS = stdPL_FullSample$TS[DATE > dtOOB])
#   OOB_stdPL$FD = stdPL_FullSample$FD[stdPL_FullSample$FD$DATE > min(OOB_stdPL$TS$DATE), ]
#   OOB_stdPL$TS = data.table(DATE = OOB_stdPL$TS$DATE, sweep(as.matrix(OOB_stdPL$TS[,-1]), 2, as.matrix(OOB_stdPL$TS[1,-1]), `/`))
#   colnames(OOB_stdPL$TS) = colnames(OOB_stdPL$FD)
#   
#   return(list(IS_OS_cut_stdPL = IS_OS_cut_stdPL, OOB_stdPL = OOB_stdPL, FullInSample_stdPL = stdPL_InSample))
# }