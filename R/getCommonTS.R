#' Get Common Time series
#'
#' The function provides the longest possible common time series for all
#' securities/indices in the price list lPX.
#' The function output is a list with two data.frame's. The TS data.frame holds the indexed time series
#' and the FD data.frame the first log differences.
#' getCommonTS()

getCommonTS = function (lPX, strFreq = "WKL", boolInterpol = T) {
  minDate = max(as.Date(unlist(lapply(lPX, function(x) min(as.Date(x$DATE)))), origin = "1970-01-01"))     # first possible common date
  maxDate = min(as.Date(unlist(lapply(lPX, function(x) max(as.Date(x$DATE)))), origin = "1970-01-01"))     # last possible common date

  if (strFreq == "DLY") {
    dtVec_temp = data.table()
    dtVec_temp = lapply(lPX, function(x) dtVec_temp = rbind(data.frame(DATE = x$DATE), dtVec_temp))
    dtVec_temp = data.table(DATE = as.Date(t(t(unlist(dtVec_temp))), origin = "1970-01-01"))
    colnames(dtVec_temp) = "DATE"

    dtVec_temp = dtVec_temp[, .(count = .N), by = DATE]
    dtVec_temp = dtVec_temp[count >= length(lPX) / 3, ]   # only choose date points, for which at leat 1/3 of the TS have an observation
    dtVec = data.table(DATE = as.Date(dtVec_temp$DATE, origin = "1970-01-01"))
  } else if (strFreq == "WKL") {
    dtVec = seq.Date(minDate, maxDate, 1)
    dtVec = data.table(DATE = dtVec[as.POSIXlt(dtVec)$wday == 3])    # vector of wednesdays within possible common date range
  } else if (strFreq == "MTL") {
    dtVec = seq.Date(as.Date(paste(year(minDate), month(minDate), 1, sep ="-")),
                     length = ceiling(as.numeric(maxDate - minDate) / 30) + 2, by="1 month")-1
    dtVec = data.table(DATE = dtVec)[DATE >= minDate & DATE <= maxDate]    # vector of EOM's within possible common date range
  } else (stop("Error: incorrect frequency string supplied. Choose from DLY, WKL, MTL!"))

  setkey(dtVec, DATE)
  dtVec = dtVec[order(DATE)]
  dtCommonTS = dtVec[DATE >= minDate & DATE <= maxDate]

  for (j in 1:(length(lPX))){
    dtRawPriceTS = data.table(lPX[[j]][, c('DATE','INDEX')])
    setkey(dtRawPriceTS, DATE)
    dtRawPriceTS[, DATE_BackWard := DATE]

    colnames(dtRawPriceTS) = c("DATE", "INDEX_BackWard", "DATE_BackWard")
    dtMergeTS = dtRawPriceTS[dtVec, roll = +Inf]        # take the LAST available value, if missing

    colnames(dtRawPriceTS) = c("DATE", "INDEX_ForWard", "DATE_ForWard")
    dtMergeTS = dtRawPriceTS[dtMergeTS, roll = -Inf]        # take the NEXT available value, if missing

    if (boolInterpol){
      dtCommonTS[, eval(names(lPX)[j])] = ifelse(   # interpolate, if necessary
        dtMergeTS$DATE_ForWard == dtMergeTS$DATE_BackWard, dtMergeTS$INDEX_BackWard,
        round(dtMergeTS$INDEX_BackWard + (dtMergeTS$INDEX_ForWard - dtMergeTS$INDEX_BackWard)
              * as.numeric(dtMergeTS$DATE - dtMergeTS$DATE_BackWard) / as.numeric(dtMergeTS$DATE_ForWard - dtMergeTS$DATE_BackWard), 6))
    } else {
      dtCommonTS[, eval(names(lPX)[j])] = dtMergeTS$INDEX_BackWard  #  do NOT interpolate under any circumstances, take last available value
    }
  }

  setkeyv(dtCommonTS, colnames(dtCommonTS)[2:ncol(dtCommonTS)])
  dtCommonTS = unique(dtCommonTS)
  dtCommonTS = dtCommonTS[order(DATE)]

  dtCommonFD = dtCommonTS[-1,]    # remove first date from the first difference TS

  for (j in 2:ncol(dtCommonTS)){  # assume, that if min(TS) < 0.05 we're handling interest rates
                                  # additionally - if an override column exitsts - use first diffs, else log fd
    if (min(dtCommonTS[, get(colnames(dtCommonTS)[j])]) < 0.05 ||  !is.null(lPX[[j-1]]$OR)){
      dtCommonFD[, j] = diff(dtCommonTS[, get(colnames(dtCommonTS)[j])])
    } else {
      dtCommonTS[, j] = dtCommonTS[, get(colnames(dtCommonTS)[j])] / dtCommonTS[1, get(colnames(dtCommonTS)[j])]  # first difference base on LOG(TS)
      dtCommonFD[, j] = diff(log(dtCommonTS[, get(colnames(dtCommonTS)[j])]))
    }
  }
  return(stdPL = list(TS = dtCommonTS,  FD = dtCommonFD))
}
