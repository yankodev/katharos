getPXListBBG = function (dfTickerFromTo){
  pxList = list()
  library(Rblpapi)
  BBGConn = blpConnect()
  
  for (i in 1:nrow(dfTickerFromTo)) {
    if (dfTickerFromTo$TS_TYPE[i] %in% c("Index", "Security"))
      {pxList[[dfTickerFromTo$BBG_TICKER[i]]] = getBBG_TRIndex(dfTickerFromTo$BBG_TICKER[i], dfTickerFromTo$FROM[i], dfTickerFromTo$TO[i])}
    else
      {pxList[[dfTickerFromTo$BBG_TICKER[i]]] = getBBG_PXLAST(dfTickerFromTo$BBG_TICKER[i], dfTickerFromTo$FROM[i], dfTickerFromTo$TO[i])}
  }
  
  blpDisconnect(BBGConn)
  return(pxList)
}

getBBG_TRIndex = function (strBBGTicker, dtFrom, dtTo){
  dfBBGRawData = bdh(strBBGTicker, "TOT_RETURN_INDEX_GROSS_DVDS", start.date = dtFrom, end.date=dtTo)
  return( data.table(DATE = as.Date(dfBBGRawData$date), INDEX = as.numeric(dfBBGRawData$TOT_RETURN_INDEX_GROSS_DVDS)) )
}

getBBG_PXLAST = function (strBBGTicker, dtFrom, dtTo){
  dfBBGRawData = bdh(strBBGTicker, "PX_LAST", start.date = dtFrom, end.date=dtTo)
  return( data.table(DATE = as.Date(dfBBGRawData$date), INDEX = as.numeric(dfBBGRawData$PX_LAST)) )
}