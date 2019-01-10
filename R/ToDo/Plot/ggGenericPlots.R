library(ggplot2)
library(ggthemes)
library(scales)
library(plotly)
library(data.table)

f = list(family = "sans-serif", size = 12, color = "#000")

ggGenericDateValueTwoTypeLine = function(dtStdTSLong, strPlotTitle, strXTitle, strYTitle){
  gpl = ggplot(dtStdTSLong) + aes(x= DATE, y = INDEX_TS, colour = paste(SEC_ID, POINT_TYPE)) + theme_stata(base_size = 12) + scale_colour_stata()+ geom_line(size = 0.7)
  gpl = gpl + scale_y_continuous(labels = scales::percent) + scale_x_date(labels = date_format("%m-%Y"))
  gpl = gpl + theme(legend.direction = "vertical", legend.title=element_blank())
  gpl = ggplotly(gpl)
  # gpl = gpl %>% layout(legend = list(x = 0, y = 1, font = list(family = "sans-serif", size = 9, color = "#000"), borderwidth = 0))
  gpl = gpl %>% layout(legend = list(x = 100, y = 1, font = list(family = "sans-serif", size = 9, color = "#000"), borderwidth = 0))
  gpl = gpl %>% layout(font =f, title = strPlotTitle, xaxis = list(title = strXTitle, titlefont =f, tickfont = f), yaxis = list(title = strYTitle, titlefont = f, tickfont = f))
  return(gpl)
}


ggGenericSingleStackedWeightBar = function(df, strPlotTitle, strXTitle, strYTitle){
  gpl = ggplot(df) + theme_economist() + scale_fill_economist() + geom_bar(aes(y=Weight, fill = ID, x = "Security Weights"), stat="identity")
  gpl = gpl + scale_y_continuous(labels = scales::percent)
  gpl = gpl + theme(legend.direction = "vertical", legend.title=element_blank())
  gpl = ggplotly(gpl)
  gpl = gpl %>% layout(legend = list(x = 0, y = 1, font = list(family = "sans-serif", size = 9, color = "#000"), borderwidth = 0))
  gpl = gpl %>% layout(font =f, title = strPlotTitle, xaxis = list(title = strXTitle, titlefont =f, tickfont = f), yaxis = list(title = strYTitle, titlefont = f, tickfont = f))
  return(gpl)
}

##
##  TO DO:
##

getGenericDateYLinePlot = function(dfGather, strPlotTitle, strXTitle, strYTitle){
  gpl = ggplot(dfGather) + aes(x = DATE, y = Y, colour = TS_ID)+ theme_economist(base_size = 16) + scale_colour_economist()+ geom_line(size = 0.7)
  # gpl = ggplot(dfGather) + aes(x = DATE, y = Y, colour = TS_ID)+ theme_stata(base_size = 16) + scale_colour_stata()+ geom_line(size = 0.7)  
  gpl = gpl + scale_y_continuous(labels = scales::percent) + scale_x_date(labels = date_format("%m-%Y"))
  gpl = gpl + theme(legend.direction = "vertical", legend.title=element_blank())
  gpl = ggplotly(gpl)
  gpl = gpl %>% layout(legend = list(x = 1, y = 1, font = f, borderwidth = 0))
  gpl = gpl %>% layout(font =f, title = strPlotTitle, xaxis = list(title = strXTitle, titlefont =f, tickfont = f), yaxis = list(title =strYTitle, titlefont = f, tickfont = f))  
  return(gpl)
}

getGenericVolaYLinePlot = function(dfGather, strPlotTitle, strXTitle, strYTitle){
  gpl = ggplot(dfGather) + aes(x = pa_vola, y = Y, colour = TS_ID)+ theme_economist(base_size = 16) + scale_colour_economist()+ geom_line(size = 0.7)  
  gpl = gpl + scale_y_continuous(labels = function(x) round(as.numeric(x), digits=2)) + scale_x_continuous(labels = scales::percent)
  gpl = gpl + theme(legend.direction = "vertical", legend.title=element_blank())
  gpl = ggplotly(gpl)
  gpl = gpl %>% layout(legend = list(x = 1, y = 1, font = f, borderwidth = 0))
  gpl = gpl %>% layout(font =f, title = strPlotTitle, xaxis = list(title = strXTitle, titlefont =f, tickfont = f), yaxis = list(title =strYTitle, titlefont = f, tickfont = f))  
  return(gpl)
}

# getGenericDateYLinePlot(attrib, "Performance Attribution of rolling MAX Sharpe Allocation", "", "")

getGenericDateYAreaPlot = function (dfGather, strPlotTitle, strXTitle, strYTitle){
  gpl = ggplot(dfGather) + aes(x = DATE, y = Y, fill = TS_ID) + theme_economist() + scale_fill_economist() + geom_area(position = "stack")
  gpl = gpl + scale_y_continuous(labels = scales::percent) + scale_x_date(labels = date_format("%m-%Y"))
  gpl = gpl + theme(legend.direction = "vertical", legend.title=element_blank())
  gpl = ggplotly(gpl)
  gpl = gpl %>% layout(legend = list(x = 1, y = 1, font = f, borderwidth = 0))
  gpl = gpl %>% layout(font =f, title = strPlotTitle, xaxis = list(title = strXTitle, titlefont =f, tickfont = f), yaxis = list(title = strYTitle, titlefont = f, tickfont = f))
  return(gpl)
}

getGenericVolaYAreaPlot = function (dfGather, strPlotTitle, strXTitle, strYTitle){
  # gpl = ggplot(dfGather) + aes(x = pa_vola, y = Y, fill = TS_ID) + theme_economist() + scale_fill_economist() + geom_area(position = "stack")
  gpl = ggplot(dfGather) + aes(x = pa_vola, y = Y, fill = TS_ID) + theme_stata() + scale_fill_stata() + geom_area(position = "stack")
  gpl = gpl + scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::percent)
  gpl = gpl + theme(legend.direction = "vertical", legend.title=element_blank())
  gpl = ggplotly(gpl)
  gpl = gpl %>% layout(legend = list(x = 1, y = 1, font = f, borderwidth = 0))
  gpl = gpl %>% layout(font =f, title = strPlotTitle, xaxis = list(title = strXTitle, titlefont =f, tickfont = f), yaxis = list(title = strYTitle, titlefont = f, tickfont = f))
  return(gpl)
}


getGenericDateYLineAbsolutePlot = function(dfGather, strPlotTitle, strXTitle, strYTitle){
  # gpl = ggplot(dfGather) + aes(x = DATE, y = Y, colour = TS_ID)+ theme_economist(base_size = 16) + scale_colour_economist()+ geom_line(size = 0.7)
  gpl = ggplot(dfGather) + aes(x = DATE, y = Y, colour = TS_ID)+ theme_stata(base_size = 16) + scale_colour_stata()+ geom_line(size = 0.7)  
  gpl = gpl + scale_y_continuous() + scale_x_date(labels = date_format("%m-%Y"))
  gpl = gpl + theme(legend.direction = "vertical", legend.title=element_blank())
  gpl = ggplotly(gpl)
  gpl = gpl %>% layout(legend = list(x = 1, y = 1, font = f, borderwidth = 0))
  gpl = gpl %>% layout(font =f, title = strPlotTitle, xaxis = list(title = strXTitle, titlefont =f, tickfont = f), yaxis = list(title =strYTitle, titlefont = f, tickfont = f))  
  return(gpl)
}