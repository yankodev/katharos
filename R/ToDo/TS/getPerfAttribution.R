getPerfAttribution = function(FD, dtWeights){
  # FD[,-1] = apply(FD[,-1], 2, function(x) as.numeric(x))
  FD = FD[FD$DATE >= min(dtWeights$DATE), ]
  gFD = data.table(gather(FD, key = "asset", value = "log_fd", -DATE), key = c("DATE", "asset"))
  gWeights = data.table(gather(dtWeights, key = "asset", value = "weight", -DATE), key = c("DATE", "asset"))
  
  dtWeightsFD = gWeights[gFD]
  dtWeightsFD = dtWeightsFD[order(asset, DATE)]
  for(i in 2:nrow(dtWeightsFD)){
    if(dtWeightsFD[i,asset] == dtWeightsFD[i-1,asset]) dtWeightsFD[i,weight := ifelse(is.na(weight), dtWeightsFD[i-1,weight]*(1+log_fd), weight)]
  }
  
  dtWeightsFD[, weight := weight / sum(weight), by = DATE]
  dtWeightsFD[, weight_lagged:=shift(weight, 1, type = "lag"), by = asset]
  dtWeightsFD[, ptf_r := ifelse(is.na(sum(weight_lagged * log_fd)), 0, sum(weight_lagged * log_fd)), by = DATE]
  dtWeightsFD[, ptf_tr := cumprod(1+ptf_r), by = asset]
  dtWeightsFD[, notionals := weight * ptf_tr, by = asset]
  dtWeightsFD[, ccy := shift(notionals, 1, type ="lag") * log_fd, by = asset]
  dtWeightsFD[, ccy := ifelse(is.na(ccy), 0, ccy), by = asset]
  dtWeightsFD[, attrib := cumsum(ccy), by = asset]
  dtWeightsFD[, attrib_ptf_check := sum(attrib)-ptf_tr+1, by = DATE]
  
  dtAttribOut = dtWeightsFD[, c("DATE", "asset", "weight", "log_fd", "attrib")]
  # dtAttribOut$attrib = dtAttribOut$attrib+1
  dtPtf = unique(dtWeightsFD[, c("DATE", "ptf_r", "ptf_tr")])
  dtPtf[, c("asset", "weight", "ptf_tr") := list("PTF", 1, ptf_tr -1)]
  colnames(dtPtf) = c("DATE", "log_fd", "attrib", "asset", "weight")
  dtAttribOut = rbind(dtAttribOut, dtPtf[, c("DATE", "asset", "weight", "log_fd", "attrib")])
  dtAttribOut = dtAttribOut[complete.cases(dtAttribOut), ]
  return(dtAttribOut)
}
