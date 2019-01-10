# now - build wrapper for multiple secs

# strSecId = 'ECCTAZE ID Equity'
# strModelVersion = 'MAIN'
library(RODBC)
library(data.table)
library(plyr)

getModel = function (strSecId, strModelVersion, FDS){
  dbConn = odbcConnect("TEC103", "TEC103", "Marep_tec103")
  dtModelData = data.table(sqlQuery(dbConn, paste0("select * from (select t.*, max(model_date) over (partition by sec_id, model_version) md from ts_model t where sec_id = '", strSecId, "' and model_version = '", strModelVersion,"') where model_date = md"), as.is = T))
  odbcClose(dbConn)
  dtModelData = data.table(index_id = dtModelData$INDEX_ID, beta = as.numeric(gsub(",", ".", dtModelData$INDEX_BETA)))
  if (sum(ifelse(FDS$dtMasterData$TS_ID %in% dtModelData$index_id,1,0)) != nrow(dtModelData)) {return()}
  if (nrow(dtModelData) == 0) {
    if(strSecId %in% FDS$dtMasterData$TS_ID){
      stdPL = getCommonTS_WKL(FDS$pxList[strSecId])
      stdPL$FD$MODEL = stdPL$FD[,2]
      stdPL$TS$MODEL = stdPL$TS[,2]
      return(list(stdPL = stdPL, dtModelData = data.table(index_id = strSecId, beta = 1)))
    } else {return()}
  }
  
  stdPL = getCommonTS_WKL(FDS$pxList[as.character(dtModelData$index_id)])
  stdPL$FD$MODEL = as.matrix(stdPL$FD[,-1]) %*% matrix(dtModelData$beta, ncol = 1)
  stdPL$TS$MODEL = c(1, cumprod(exp(stdPL$FD$MODEL)))
  
  stdPL_Sec = getCommonTS_WKL(FDS$pxList[strSecId])
  if (min(stdPL$TS$DATE) < stdPL_Sec$TS$DATE[1]) stdPL_Sec$TS[,2] = stdPL_Sec$TS[,2] * stdPL$TS[stdPL$TS$DATE == stdPL_Sec$TS$DATE[1], ]$MODEL
  stdPL$TS = merge(stdPL$TS, stdPL_Sec$TS, by = "DATE", all.x = T)
  stdPL$FD = merge(stdPL$FD, stdPL_Sec$FD, by = "DATE", all.x = T)
  
  return(list(stdPL = stdPL, dtModelData = dtModelData))
  
}

# dtPtf = data.table(sec_id = c("ALLVOSI LX Equity", "BERFEMI GR Equity", "LRIOFIE LX Equity",
#                               "ECCTAZE ID Equity", "DBPI5CE LX Equity", "CRMAFLE ID Equity",
#                               "BSSAI2E LX Equity", "UQGRPEI LX Equity", "AQRSPBE LX Equity"),
#                    weights = c(0.2*0.15, 0.4*0.15, 0.4*0.15,
#                                0.4*0.15, 0.4*0.15, 0.2*0.15,
#                                0.45*0.7, 0.35*0.7, 0.2*0.7))

# dtPtf = data.table(sec_id = as.character(colnames(dfWeights)), weights = as.numeric(dfWeights["Custom_QGR",]))

getRiskModel = function(dtPtf, FDS){
  dtModels = data.table()
  pxList_Model = list()
  for (strSecId in dtPtf$sec_id){
    tmpModelList = getModel(strSecId, "MAIN", FDS)
    dtModels = rbind(data.table(sec_id = strSecId, tmpModelList$dtModelData), dtModels)
    pxList_Model[[(length(pxList_Model)+1)]]=data.table(BBG_TICKER = strSecId, DATE = tmpModelList$stdPL$TS$DATE, INDEX = tmpModelList$stdPL$TS$MODEL)
    names(pxList_Model)[length(pxList_Model)] = strSecId
  }
  
  setkey(dtPtf, sec_id)
  setkey(dtModels, sec_id, index_id)
  dtPtfSecIndexRV = dtPtf[dtModels]
  dtPtfSecIndexRV[,index_exp:=beta*weights]
  
  dtPtfIndexRV = as.data.table(ddply(dtPtfSecIndexRV, ~index_id, summarise, ptf_index_exp = sum(index_exp)))
  dtPtfIndexRV = dtPtfIndexRV[dtPtfIndexRV$ptf_index_exp!=0]
  
  FD = getCommonTS_WKL(FDS$pxList[dtPtfIndexRV$index_id])$FD[,-1]
  mCov = Matrix::nearPD(cov(FD))$mat
  
  ptfVaR_99_1w = as.numeric(2.33*sqrt(matrix(dtPtfIndexRV$ptf_index_exp, nrow = 1) %*% as.matrix(mCov) %*% matrix(dtPtfIndexRV$ptf_index_exp, ncol = 1)))
  dtPtfIndexRV$MV = t(t(as.matrix(dtPtfIndexRV$ptf_index_exp)) %*% as.matrix(mCov) * (2.33^2)/ptfVaR_99_1w)
  dtPtfIndexRV$index_cVaR = dtPtfIndexRV$MV * dtPtfIndexRV$ptf_index_exp
  dtPtfIndexRV$index_cVaR_pct = dtPtfIndexRV$index_cVaR / ptfVaR_99_1w
  
  setkey(dtPtfIndexRV, index_id)
  setkey(dtPtfSecIndexRV, index_id)
  
  dtPtfSecRV = as.data.table(ddply(dtPtfSecIndexRV[dtPtfIndexRV], ~sec_id, summarise, sec_cVaR = sum(index_cVaR*index_exp/ptf_index_exp)))
  dtPtfSecRV$sec_cVaR[is.na(dtPtfSecRV$sec_cVaR)] = 0
  dtPtfSecRV$sec_cVaR_pct = dtPtfSecRV$sec_cVaR / ptfVaR_99_1w
  
  return(list(ptfVaR_99_1w = ptfVaR_99_1w, dtPtfSecIndexRV = dtPtfSecIndexRV, dtPtfIndexRV = dtPtfIndexRV, dtPtfSecRV = dtPtfSecRV))
}

# plot_ly(gather(stdPL$TS, key = "TS_ID", value = "INDEX", -DATE), x = ~DATE, y=~INDEX, color = ~TS_ID, type ="scatter", mode = "lines")
