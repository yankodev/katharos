getPXListBBG_FX = function (dfTickerFromTo){
  pxList = list()
  library(Rblpapi)
  BBGConn = blpConnect()
  
  for (i in 1:nrow(dfTickerFromTo)) {
    if (dfTickerFromTo$TS_TYPE[i] %in% c("Index", "Security"))
      {pxList[[dfTickerFromTo$TS_ID[i]]] = getBBG_TRIndex_FX(dfTickerFromTo$BBG_TICKER[i], dfTickerFromTo$FROM[i], dfTickerFromTo$TO[i], dfTickerFromTo$FX_ID[i])}
    else if (!(dfTickerFromTo$TS_TYPE[i] %in% c("IR")))
      {pxList[[dfTickerFromTo$TS_ID[i]]] = getBBG_PXLAST_FX(dfTickerFromTo$BBG_TICKER[i], dfTickerFromTo$FROM[i], dfTickerFromTo$TO[i], dfTickerFromTo$FX_ID[i])}
    else
      {pxList[[dfTickerFromTo$TS_ID[i]]] = getBBG_PXLAST(as.character(dfTickerFromTo$BBG_TICKER[i]), dfTickerFromTo$FROM[i], dfTickerFromTo$TO[i])}
  }
  
  blpDisconnect(BBGConn)
  return(pxList)
}

# options = c("nonTradingDayFillOption" = "ALL_CALENDAR_DAYS", "nonTradingDayFillMethod" = "PREVIOUS_VALUE")
# 
# strBBGTicker = dfTickerFromTo$BBG_TICKER[i]
# dtFrom = dfTickerFromTo$FROM[i]
# dtTo = dfTickerFromTo$TO[i]
# FX_ID = dfTickerFromTo$FX_ID[i]

getBBG_TRIndex_FX = function (strBBGTicker, dtFrom, dtTo, FX_ID){
  dfBBGRawData = bdh(strBBGTicker, "TOT_RETURN_INDEX_GROSS_DVDS", start.date = dtFrom, end.date=dtTo, options = c("currency" = FX_ID))
  return(
    if (nrow(dfBBGRawData)==0){
      data.table()
      } else {data.table(BBG_TICKER = strBBGTicker, DATE = as.Date(dfBBGRawData$date), INDEX = as.numeric(dfBBGRawData$TOT_RETURN_INDEX_GROSS_DVDS))}
    )
}

getBBG_PXLAST_FX = function (strBBGTicker, dtFrom, dtTo, FX_ID){
  dfBBGRawData = bdh(strBBGTicker, "PX_LAST", start.date = dtFrom, end.date=dtTo, options = c("currency" = FX_ID))
  return(
    if (nrow(dfBBGRawData)==0){
      data.table()
      }  else {data.table(BBG_TICKER = strBBGTicker, DATE = as.Date(dfBBGRawData$date), INDEX = as.numeric(dfBBGRawData$PX_LAST))}
    )
}

getBBG_PXLAST = function (strBBGTicker, dtFrom, dtTo){
  dfBBGRawData = bdh(as.character(strBBGTicker), "PX_LAST", start.date = dtFrom, end.date=dtTo)
  return(
    if (nrow(dfBBGRawData)==0){
      data.table()
    }  else {data.table(BBG_TICKER = strBBGTicker, DATE = as.Date(dfBBGRawData$date), INDEX = as.numeric(dfBBGRawData$PX_LAST))}
  )
}