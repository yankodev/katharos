# FD = stdPL$FD
# dtFD = data.table(FD1=c(1,FD$`ALLVOLI LX Equity`+1))
getMDD_FAST = function(dtFD){
  for (i in 2:nrow(dtFD)){
    dd = dtFD$FD1[i-1] * dtFD$FD1[i]
    dtFD$FD1[i] = ifelse(dd >= 1, 1, dd)
  }
  return(min(dtFD$FD1)-1)
}

# dtFD = data.table(FD=FD$`ALLVOLI LX Equity`)
library(data.table)
getMDD = function(dtFD){
  dtFD$FD1 = exp(dtFD$FD)
  dtFD$DD = 1
  for (i in 2:nrow(dtFD)){
    dd = dtFD$DD[i-1] * dtFD$FD1[i]
    dtFD$DD[i] = ifelse(dd >= 1, 1, dd)
  }
  return(min(dtFD$DD)-1)
}


library(data.table)
getMDD_nonLog = function(dtFD){
  dtFD$FD1 = dtFD$FD+1
  dtFD$DD = 1
  for (i in 2:nrow(dtFD)){
    dd = dtFD$DD[i-1] * dtFD$FD1[i]
    dtFD$DD[i] = ifelse(dd >= 1, 1, dd)
  }
  return(min(dtFD$DD)-1)
}

# n secs

# data.frame(1:11, colnames(FD))
# dtFD = data.table(FD[, 2:(n+1), with =F])
# dtFD = data.table(FD[, c(3,5,7,8,9,10), with =F])
# n = 6
# perm = data.table(expand.grid(rep(list(0:10), n-1)) )
# colOrders = permute::allPerms(n-1)
# perm_temp = perm
# 
# for (i in 1:nrow(colOrders)) perm = rbind(perm, perm_temp[, colOrders[i,], with = F])
# perm$Y = 10 - apply(perm, 1, sum)
# perm = perm[Y>= 0, ]
# setkey(perm)
# 
# perm = unique(perm/10)
# dtMDDSearch = data.table(perm)
# dtMDDSearch$MDD = -1
# dtMDDSearch$ret = -1
# 
# # save(dtMDDSearch, file="dtMDDSearch_6_Secs.rda")
# 
# 
# 
# for(i in 1:nrow(dtMDDSearch)){
#   dtFDPtf = data.table(FD1 = c(1,1+as.matrix(dtFD[,1:n, with=F], ncol = n) %*% t(dtMDDSearch[i,1:n])))
#   dtMDDSearch$mDD[i] = getMDD_FAST(dtFDPtf)
#   dtMDDSearch$ret[i] = prod(dtFDPtf$FD1)-1
# }
# 
# dtMDDSearch$ret = (1+dtMDDSearch$ret)^(12/nrow(FD))-1
# colnames(dtMDDSearch) = c(colnames(dtFD), "mDD", "ret")
# 
# if (n==3){
#   tmpDT = dtMDDSearch
#   colnames(tmpDT) = c(paste("w", 1:3, sep =""), "mDD", "ret")
#   p = ggplot(tmpDT, aes(x=w1, y = w2, z = w3, fill = -mDD)) + geom_tile(show.legend = T) + geom_point(aes(size =w3), show.legend = T) + coord_equal() + scale_fill_distiller(palette = "Spectral", na.value = "white", labels = percent) + scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::percent) + theme_economist() + scale_colour_economist() + theme(legend.position = "right")
#   p = ggplotly(p)
#   p = p %>% layout(legend = list(x = 1, y = 1, font = f, borderwidth = 0))
#   p = p %>% layout(font =f, title = "Max-Draw-Down Analysis", xaxis = list(title = colnames(dtMDDSearch)[1] , titlefont =f, tickfont = f), yaxis = list(title = colnames(dtMDDSearch)[2], titlefont = f, tickfont = f))
#   p
# }
# 
# labels = colnames(dtFD)
# par(mar=c(8, 4, 1, 1))
# # boxplot(dtMDDSearch[mDD >= -0.04 & ret >= 0.04, 1:n, with=F], xlab = "", xaxt = "n")
# boxplot(dtMDDSearch[ret / -mDD >= 1.5 & ret >= 0.05, 1:n, with=F], xlab = "", xaxt = "n")
# axis(1, labels = FALSE)
# text(x =  seq_along(labels), y = par("usr")[3] - 0.1, srt = 45, adj = 1, labels = labels, xpd = TRUE, cex = 0.8)
# 
# xy = dtMDDSearch[mDD>= -0.03]
# xy = dtMDDSearch[ret / -mDD >= 1.5]
# plot(-xy$mDD, xy$ret)


# dtMDDSearch_RPVEga = dtMDDSearch
# dtMDDSearch_3_5_9 = dtMDDSearch
# dtMDDSearch_2_7 = dtMDDSearch
# save.image()
