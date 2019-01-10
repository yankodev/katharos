# ADD matCov as standard outptu

getSecStats_ClearName = function(stdPL){
  library(data.table)
  library(tidyr)
  library(PerformanceAnalytics)
  library(DT)
  
  nYrs = as.numeric(tail(stdPL$TS$DATE, 1) - head(stdPL$TS$DATE,1)) / 365
  nSec = ncol(stdPL$TS) - 1
  daysFreq = as.numeric(quantile(as.numeric(diff(stdPL$TS$DATE)), seq(0,1,0.1))[3])
  annFactorVola = sqrt(ifelse(daysFreq == 7, 52, ifelse(daysFreq == 1, 260, 12)))
  TR = tail(stdPL$TS[,-1], 1) / head(stdPL$TS[,-1],1) - 1
  paRet = (TR + 1)^(1/nYrs)-1
  Vola = data.table(t(apply(as.matrix(stdPL$FD[,-1], ncol = nSec), 2, function(x) sd(x) * annFactorVola)))
  
  dfMDD = data.frame(as.matrix(exp(stdPL$FD[,-1])-1, ncol = nSec), row.names = stdPL$FD$DATE)
  mdd = as.data.table(maxDrawdown(dfMDD))
  colnames(mdd) = colnames(stdPL$FD[,-1])
  
  Sharpe = as.data.table(paRet / as.numeric(Vola))
  Calmar = as.data.table(paRet / as.numeric(mdd))
  if (nSec > 1) {
    matCov = cov(stdPL$FD[,-1])
    matCorr = cor(stdPL$FD[,-1])
    matBeta = matCorr
    for (i in 1:nrow(matCorr)) for (j in 1:ncol(matCorr)) matBeta[i,j] = matCorr[i,j] * sd(unlist(stdPL$FD[,i+1, with = FALSE])) / sd(unlist(stdPL$FD[,j+1, with = FALSE]))
  } else {matCov = matCorr = matBeta = NULL}
  
  
  dtSummary = data.frame(t(rbind(rbind(rbind(rbind(rbind(TR, paRet), Vola), mdd), Sharpe), Calmar)))
  dtSummary$From = stdPL$TS$DATE[1]
  dtSummary$To = stdPL$TS$DATE[nrow(stdPL$TS)]
  colnames(dtSummary) = c("Gesamtrendite", "Rendite p.a.", "Volatilität", "Max Draw-Down", "Sharpe", "Calmar", "From", "To")
  dtSummary = dtSummary[c("Gesamtrendite", "Rendite p.a.", "Volatilität", "Max Draw-Down", "Sharpe", "Calmar")]
  Stats = DT::datatable(dtSummary, options = list(paging = F, searching = F, bInfo = F)) %>% 
    formatPercentage(c('Gesamtrendite', 'Rendite p.a.', 'Volatilität', 'Max Draw-Down'), 2) %>%
    formatRound(c('Sharpe', 'Calmar'), 2)
  # %>%formatDate(c('From', 'To'), method = "toLocaleDateString")
  
  
  stdPL_stats = list(TS=stdPL$TS,
                     FD=stdPL$FD,
                     MetaData = data.frame(nYrs=nYrs, nSec=nSec, daysFreq = daysFreq, annFactorVola=annFactorVola),
                     Stats = Stats,
                     matCov = matCov,
                     matCorr = matCorr,
                     matBeta = matBeta,
                     dtSummary = dtSummary
                     ) 
}
