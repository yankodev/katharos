dfSQLLoader = function(strSchema, strTable, dfUpload){
  library(dplyr)
  strControlsPath = "//VKBADS.DE/ORG/VKB/TECTA/TEC103/ALLE/20_DataAdministration/UploadControls/"
  
  if (strSchema == "TECRAW"){
    rodbcConnSqlLdr = RODBC::odbcConnect("TECRAW", "TECRAW", "Marep_tecraw0")
    strCredentials = "sqlldr.exe TECRAW/Marep_tecraw0@wmvkpr10"
  } else if (strSchema == "TEC103"){
    rodbcConnSqlLdr = RODBC::odbcConnect("TEC103", "TEC103", "Marep_tec103")
    strCredentials = "sqlldr.exe TEC103/Marep_tec103@wmvkpr10"
  } else{
    strCredentials = "//VKBADS.DE/ORG/VKB/TECTA/TECTA_ALLE/DataAdministration/ORACLE/64/instantclient_12_2/sqlldr.exe TEC103/Marep_tec103@wmvkpr10"
  }
  
  strControlFile = paste(strControlsPath, "Controls/", strTable, ".ctl", sep ="")
  if (!file.exists(strControlFile)){
    dfColumns = RODBC::sqlQuery(rodbcConnSqlLdr, paste("select listagg(column_name || ' ' || decode(data_type, 'NUMBER', 'CHAR \"TO_NUMBER(replace(:' || column_name ||', ''.'', '',''))\"', 'DATE', 'CHAR \"TO_DATE(:' || column_name ||', ''YYYY-MM-DD'')\"', ''), ',\n') within group (order by column_id)  from user_tab_cols where table_name = '", strTable, "' and USER_GENERATED = 'YES' order by column_id;", sep=""))
    strCtl = "OPTIONS (BINDSIZE = 100000, SKIP = 1)\nLOAD DATA\nINFILE '//VKBADS.DE/ORG/VKB/TECTA/TEC103/ALLE/20_DataAdministration/UploadControls/InFile/$table$.csv'\n"
    strCtl = paste(strCtl, "BADFILE '//VKBADS.DE/ORG/VKB/TECTA/TEC103/ALLE/20_DataAdministration/UploadControls/BadFile/$table$.bad'\n", sep ="")
    strCtl = paste(strCtl, "DISCARDFILE '//VKBADS.DE/ORG/VKB/TECTA/TEC103/ALLE/20_DataAdministration/UploadControls/DiscardFile/$table$.dsc'\n", sep ="")
    strCtl = paste(strCtl, "APPEND\nINTO TABLE $table$\nFIELDS TERMINATED BY \";\"\nOPTIONALLY ENCLOSED BY '\"'\n", sep ="")
    strCtl = paste(strCtl, "(\n", levels(dfColumns[1,1]), "\n)\n", sep ="")
    strCtl = gsub("\\$table\\$", strTable, strCtl)
    
    fileConn = file(strControlFile)
    writeLines(strCtl, fileConn)
    close(fileConn)
  }
  
  strLog = paste(strControlsPath, "LogFile/", strTable, ".log", sep ="")
  strCmd = paste(strCredentials, strControlFile, strLog, sep =" ")
  
  dfUpload = dfUpload %>% mutate_all(as.character)
  dfUpload[is.na(dfUpload)]=""
  write.table(dfUpload, paste("//VKBADS.DE/ORG/VKB/TECTA/TEC103/ALLE/20_DataAdministration/UploadControls/InFile/", strTable, ".csv", sep=""), dec=".", row.names = F, sep=";")
  
  catch = try(system(strCmd, ignore.stdout = T, ignore.stderr = T, wait = T, input = NULL, show.output.on.console = F, minimized = F, invisible = T), silent = T)
  
  RODBC::odbcClose(rodbcConnSqlLdr)
  options(warn=-1)
  rm(rodbcConnSqlLdr, strControlsPath, strCredentials, dfColumns, strCtl, fileConn, strLog, strControlFile, strCmd)
  options(warn=0)
}