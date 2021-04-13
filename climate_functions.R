convertF2C <- function(F){return((F-32)*(5/9))
}


setDat <- function(minTemp, maxTemp, idSet){
  nRecords = nrow(minTemp)
  idyearStart = minTemp[1,1]
  idyearEnd = minTemp[nRecords,1]
  Nyears = idyearEnd - idyearStart + 1
  dateVec = as.Date(paste(minTemp[,1],minTemp[,2],minTemp[,3],sep = "-" ))
  avgTemp = (minTemp[,idSet] + maxTemp[,idSet])/2
  tempRcP45 = data.frame(date=dateVec, min=minTemp[,idSet], max=maxTemp[,idSet], avg=avgTemp)
  setLists = list(set = tempRcP45, idyearStart = idyearStart, Nyears = Nyears)
  return(setLists)
}

list2mat <- function(inlist){
  ndim = ncol(inlist)
  ndata = nrow(inlist)
  matlist = matrix(0, nrow = ndata, ncol = ndim)
  for(i in 1:ndim){
    unlistI = unlist(inlist[,i])
    for(j in 1:ndata){
      matlist[j, i] = unlistI[j]
    }
  }
  return(matlist)
} 

avgData <- function(dataX, tau){
  ndata = nrow(dataX)
  X = dataX$X
  Y = dataX$Y
  avgData = matrix(NaN, nrow = (ndata-tau+1), ncol = 2)
  for(i in 1:(ndata-tau+1)){
    Y1 = Y[i:(i+tau-1)]
    avgData[i,1] = X[i]
    avgData[i,2] = round(mean(Y1, na.rm = TRUE))
  }
  return(avgData)
}

dirName <- function(parent_drive, data_type){
  if (dir.exists(parent_drive)==FALSE){dir.create(parent_drive)}
  dir2Save = paste(parent_drive, '/', data_type, sep = '')
  if (dir.exists(dir2Save)==FALSE){dir.create(dir2Save)}
  return(dir2Save)
}

day2month<-function(data){
  ndata = length(data)
  if(ndata%%4 == 0){
    ndaysVec = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  } else{
    ndaysVec = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  }
  cumDays = cumsum(ndaysVec)
  cumData = cumsum(data)
  cumDataMonth = cumData[cumDays]
  monthData = matrix(NaN, nrow = 1, ncol = 12)
  monthData[1] = cumDataMonth[1]
  for(i in 2:12){
    monthData[i] = cumDataMonth[i]-cumDataMonth[i-1]
  }
  return(monthData/ndaysVec)
  
}
readyData <-function(listData, modelList, idModel){
  minDate = listData$min
  avgData = listData$avg
  maxData = listData$max
  tempSet = data.frame(date = minDate$Date, min = minDate[modelList[idModel]],
                       max = maxData[modelList[idModel]], avg = avgData[modelList[idModel]])  
  colnames(tempSet) = c('date', 'min', 'max', 'avg')
  return(tempSet)
}


climate_data <-function(siteName, locName, model_name, idScene){
  if(idScene==1){
    idCol = 4
  } else{idCol = 5}
  ## reading data
  print(paste('Gathering for ', locName, ' from model ',model_name, sep = ''))
  sub_folder = paste(locName, model_name, sep = '')
  fileMin = paste('Data', siteName, 'tasmin.csv', sep = '/')
  fileMax = paste('Data', sub_folder, 'tasmax.csv', sep = '/')
  minTemp = read.csv(fileMin, header = F, sep = ",")
  maxTemp = read.csv(fileMax, header = F, sep = ",")
  dateVec = as.Date(paste(minTemp[,1],minTemp[,2],minTemp[,3],sep = "-" ))
  dfTemp = minTemp[,1:3]
  dfTemp['Minimum'] = minTemp[, idCol]
  dfTemp['Mean'] = (minTemp[, idCol]+maxTemp[, idCol])/2
  dfTemp['Maximum'] = maxTemp[, idCol]
  dfTemp['Date'] = dateVec
  dfTemp  = dfTemp[, c(7, 1:6)]
  colnames(dfTemp) =c('Date', 'Year', 'Month', 'Day', 'Minimum', 'Mean', 'Maximum')
  return(dfTemp)
}

processData <- function(siteName, locName, modelList, idScene, caseStat){
  set2Analyze.RCP45 <-gatherData(siteName, locName, modelList, idScene)
  minData = set2Analyze.RCP45$min
  maxData = set2Analyze.RCP45$max
  idyearStart = year(minData$Date[1]) 
  idyearEnd = year(minData$Date[nrow(minData)])
  Nyears = idyearEnd- idyearStart +1 
  idHist = which(minData$Date<=2005)
  
  if(caseStat=='MEDIAN'){
    minData = minData[, c(1:11, 13)]
    maxData = maxData[, c(1:11, 13)]
  } else if(caseStat=='MEAN') {
    minData = minData[, c(1:12)]
    maxData = maxData[, c(1:12)]
  }
  minData[idHist,2:11] = minData[idHist, 12]
  maxData[idHist,2:11] = maxData[idHist, 12]
  
  avgData = minData
  avgData[,2:12] = (minData[,2:12]+maxData[,2:12])/2
  return(list(min = minData, max = maxData, avg = avgData))
}


plotConfide.mean<-function(locName, cropName, ModelType, genName){
  listFrame = read.csv('ListLocations.csv')
  nameLoc = as.character(listFrame$County)
  modelList = c('ACCESS1',	'CANESM21',	'CAESM1BGC', 'CCSM4',	'CMCC_CMS',	'CNRCCM5',
                'GFDL_CM3',	'HADGECC',	'HADGEES',	'MICRO5')
  numGCM = length(modelList)
  yearHist = '2005'
  vecHist = matrix(NaN, nrow = 150, ncol = 1)
  matrixPorjection = matrix(NaN, nrow = 150, ncol = numGCM)
  for(idGCM in 1:numGCM){
    dir2read = paste('Output/', cropName, '/', locName, modelList[idGCM], sep = '')
    file2read = paste(dir2read, '/', ModelType, 'Year.csv', sep = '')
    dataRead = read.table(file2read, header = TRUE, sep = ',')
    idHistYear = which( dataRead$Year<=yearHist)
    vecHist[1:length(idHistYear)] = dataRead[, genName][1:length(idHistYear)]
    matrixPorjection[(length(idHistYear)+1):150, idGCM] = dataRead[, genName][(length(idHistYear)+1):150]
  }
  colnames(matrixPorjection) <- modelList
  meanVec = matrix(NaN, nrow = 150, ncol = 1)
  sdVec = matrix(NaN, nrow = 150, ncol = 1)
  for(i in 1:150){
    meanVec[i] <- mean(matrixPorjection[i,], na.rm = TRUE)
    sdVec[i] <- sd(matrixPorjection[i,], na.rm = TRUE)
  }
  
  plotData <- data.frame(year = dataRead$Year, Hist = vecHist, mean=meanVec, sd=sdVec)
  eb <- aes(ymax = mean + sd, ymin = mean - sd) #aesthetic
  
  ggplot(data = plotData, aes(x = year)) + 
    geom_line(aes(y = Hist, color='black'), size = 1.5) + 
    geom_line(aes(y = mean, color='red'), linetype = 'solid', size = 1.5) + 
    geom_line(aes(y = mean - sd, color='slateblue'))+ # adding boundarline
    geom_line(aes(y = mean + sd, color='slateblue'))+
    geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd), alpha = 0.2)+
    xlab('Year') + ylab('DOY') +
    ggtitle(genName) + 
    scale_color_manual(name='Legend',
                       values = c('black' = 'black', 'red' = 'red', 'slateblue' = 'slateblue'),
                       labels=c('Observed', 'Average Projected', '95% CI')) + 
    scale_x_continuous(limits = c(min(plotData$year), max(plotData$year)), expand = c(0,0))+
    theme_bw() + 
    theme(panel.grid = element_blank(),
          axis.ticks =  element_blank()) #not to show grid 
  
}

plotConfide.median<-function(locName, cropName, ModelType, genName){
  listFrame = read.csv('ListLocations.csv')
  nameLoc = as.character(listFrame$County)
  modelList = c('ACCESS1',	'CANESM21',	'CAESM1BGC', 'CCSM4',	'CMCC_CMS',	'CNRCCM5',
                'GFDL_CM3',	'HADGECC',	'HADGEES',	'MICRO5')
  numGCM = length(modelList)
  yearHist = '2005'
  vecHist = matrix(NaN, nrow = 150, ncol = 1)
  matrixPorjection = matrix(NaN, nrow = 150, ncol = numGCM)
  for(idGCM in 1:numGCM){
    dir2read = paste('Output/', cropName, '/', locName, modelList[idGCM], sep = '')
    file2read = paste(dir2read, '/', ModelType, 'Year.csv', sep = '')
    dataRead = read.table(file2read, header = TRUE, sep = ',')
    idHistYear = which( dataRead$Year<=yearHist)
    vecHist[1:length(idHistYear)] = dataRead[, genName][1:length(idHistYear)]
    matrixPorjection[(length(idHistYear)+1):150, idGCM] = dataRead[, genName][(length(idHistYear)+1):150]
  }
  colnames(matrixPorjection) <- modelList
  medianVec = matrix(NaN, nrow = 150, ncol = 1)
  meanVec = matrix(NaN, nrow = 150, ncol = 1)
  sdVec = matrix(NaN, nrow = 150, ncol = 1)
  for(i in 1:150){
    medianVec[i] <- median(matrixPorjection[i,], na.rm = TRUE)
    meanVec[i] <- mean(matrixPorjection[i,], na.rm = TRUE)
    sdVec[i] <- sd(matrixPorjection[i,], na.rm = TRUE)
  }
  
  plotData <- data.frame(year = dataRead$Year, Hist = vecHist, mean=meanVec, median=medianVec, sd=sdVec)
  eb <- aes(ymax = mean + sd, ymin = mean - sd) #aesthetic
  
  ggplot(data = plotData, aes(x = year)) + 
    geom_line(aes(y = Hist, color='black'), size = 1.5) + 
    geom_line(aes(y = median, color='red'), linetype = 'solid', size = 1.5) + 
    geom_line(aes(y = mean - sd, color='slateblue'))+ # adding boundarline
    geom_line(aes(y = mean + sd, color='slateblue'))+
    geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd), alpha = 0.2)+
    xlab('Year') + ylab('DOY') +
    ggtitle(genName) + 
    scale_color_manual(name='Legend',
                       values = c('black' = 'black', 'red' = 'red', 'slateblue' = 'slateblue'),
                       labels=c('Observed', 'Median Projected', '95% CI')) + 
    scale_x_continuous(limits = c(min(plotData$year), max(plotData$year)), expand = c(0,0))+
    theme_bw() + 
    theme(panel.grid = element_blank(),
          axis.ticks =  element_blank()) #not to show grid 
  
}

rowMedians<-function(dataSet){
  nRecords = nrow(dataSet)
  dataSet = as.matrix(dataSet)
  medianVec = vector(length = nRecords)
  for (i in 1:nRecords) {
    medianVec[i] = median(dataSet[i,], na.rm = TRUE)
  }
  return(medianVec)
}
plotSeries<-function(dataSet, cycleName){
  dd = melt(dataSet, id=c("Year"))
  fig<-ggplot(dd) + geom_line(aes(x=Year, y=value, colour=variable)) +
    labs(title = cycleName,x='Year', y = 'DOY', color = 'Legend\n')+
    scale_colour_manual(name='Legend', values=c('black', 'cyan', 'blue', 'orange', 'yellow', 'green', 
                                                'red', 'brown', 'gray', 
                                                'tomato', 'salmon', 'black', 'magenta')) +
    theme_bw() 
  return(fig)
}

ggSet <- function(dataSet){
  newDataSet = data.frame(Year = dataSet$Year, HISTORICAL = dataSet$avgHist, 
                          ACCESS1 = dataSet$ACCESS1, CANESM21 = dataSet$CANESM21,
                          CAESM1BGC = dataSet$CAESM1BGC, CCSM4 = dataSet$CCSM4,   
                          CMCC_CMS = dataSet$CMCC_CMS,  CNRCCM5 = dataSet$CNRCCM5,  
                          GFDL_CM3 = dataSet$GFDL_CM3, HADGECC = dataSet$HADGECC,  
                          HADGEES = dataSet$HADGEES,  MICRO5 = dataSet$MICRO5, 
                          MEDIAN = dataSet$medProj, AVERAGE = dataSet$avgProj)
  return(newDataSet)
}

getStats <- function(X, Y){
  if(length(which(is.nan(Y))) ==length(Y)){
    statVal = c(NaN, NaN, NaN, NaN, NaN, NaN, NaN)
  } else{
    miN = min(Y, na.rm = TRUE)
    maX = max(Y, na.rm = TRUE)
    mu = mean(Y, na.rm = TRUE)
    std= sd(Y, na.rm = TRUE)
    med = round(median(Y, na.rm = TRUE))
    set.na = detectNA(X, Y)
    X.na = set.na$X
    Y.na = set.na$Y
    if (length(Y.na)!=0){
      modfit = lm(Y.na~X.na)
      slope = modfit$coefficients[2]
      intercept = modfit$coefficients[1]
    } else{
      modfit = NaN
      slope = NaN
      intercept = NaN
    }
    
    statVal = c(miN, maX, mu, std, med, slope, intercept)
    
  }
  
  return(statVal)
}
detectNA <- function(X, Y){
  idNNA <- which(is.na(Y)==FALSE & is.nan(Y)==FALSE )
  X = X[idNNA]
  Y = Y[idNNA]
  return(list(X=X, Y=Y))
}
getStatSummary <- function(df.Input){
  statsArray = data.frame(matrix(NaN, nrow = 7, ncol = (ncol(df.Input)-1) ))
  colnames(statsArray) = colnames(df.Input)[2:ncol(df.Input)]
  rownames(statsArray) = c('Minimum', 'Maximum', 'Mean', 'Std', 'Median', 'Slope', 'Intercept')
  statsArray[, 1] = getStats(df.Input$Year, df.Input$HISTORICAL)
  statsArray[, 2] = getStats(df.Input$Year, df.Input$ACCESS1)
  statsArray[, 3] = getStats(df.Input$Year, df.Input$CANESM21)
  statsArray[, 4] = getStats(df.Input$Year, df.Input$CAESM1BGC)
  statsArray[, 5] = getStats(df.Input$Year, df.Input$CCSM4)
  statsArray[, 6] = getStats(df.Input$Year, df.Input$CMCC_CMS)
  statsArray[, 7] = getStats(df.Input$Year, df.Input$CNRCCM5)
  statsArray[, 8] = getStats(df.Input$Year, df.Input$GFDL_CM3)
  statsArray[, 9] = getStats(df.Input$Year, df.Input$HADGECC)
  statsArray[, 10] = getStats(df.Input$Year, df.Input$HADGEES)
  statsArray[, 11] = getStats(df.Input$Year, df.Input$MICRO5)
  statsArray[, 12] = getStats(df.Input$Year, df.Input$MEDIAN)
  statsArray[, 13] = getStats(df.Input$Year, df.Input$AVERAGE)
  return(statsArray)
}

rowStat <- function(df, stat){
  if (stat=='min'){
    vec_stat = rowMins(as.matrix(df), na.rm = TRUE)     
  }
  if (stat=='mean'){
    vec_stat = rowMeans(as.matrix(df), na.rm = TRUE)      
  }
  if (stat=='max'){
    vec_stat = rowMaxs(as.matrix(df), na.rm = TRUE)    
  }  
  return(vec_stat)
}

process_data <- function(listFrame, modelList, dir_save){
  location = listFrame$Points
  for (locName in location){
    for(idModel in 1:length(modelList)){
      model_name = modelList[idModel]
      siteName = paste(locName, model_name, sep = '')
      data.temp.RCP45 <-climate_data(siteName, locName, model_name, 1)
      fileRCP45 = paste(dirName(dir_save,'RCP45'), '/', siteName, '.csv', sep = '')
      write.csv(data.temp.RCP45, file = fileRCP45)
      data.temp.RCP85 <-climate_data(siteName, locName, model_name, 2)
      fileRCP85 = paste(dirName(dir_save,'RCP85'), '/', siteName, '.csv', sep = '')
      write.csv(data.temp.RCP85, file = fileRCP85)
    }
  }
}

process_data_V1 <- function(listFrame, modelList, dir_save, id_scene){
  location = listFrame$Points
  for (locName in location) {
    for(idModel in 1:length(modelList)){
      model_name = modelList[idModel]
      siteName = paste(locName, model_name, sep = '')
      data_model <-climate_data(siteName, locName, model_name, id_scene)
      if (idModel == 1){
        df_model.min = data_model[, 2:5]
        df_model.min[model_name] = data_model$Minimum
        df_model.mean = data_model[, 2:5]
        df_model.mean[model_name] = data_model$Mean
        df_model.max = data_model[, 2:5]
        df_model.max[model_name] = data_model$Maximum    
      }
      else{
        df_model.min[model_name] = data_model$Minimum
        df_model.mean[model_name] = data_model$Mean
        df_model.max[model_name] = data_model$Maximum
      }
      if(id_scene==1){
        fileRCP = paste(dirName(dir_save,'RCP45'), '/', siteName, '.csv', sep = '')
      }
      else{
        fileRCP = paste(dirName(dir_save,'RCP85'), '/', siteName, '.csv', sep = '')
      }
      write.csv(data_model, file = fileRCP)
    }
    ensemble_df.min = df_model.min[, -c(1:4)] 
    ensemble_df.min$ENSEMBLE = rowStat(ensemble_df.min, stat='min')
    ensemble_df.mean = df_model.mean[, -c(1:4)] 
    ensemble_df.mean$ENSEMBLE <- rowStat(ensemble_df.mean, stat='mean')
    ensemble_df.max = df_model.max[, -c(1:4)] 
    ensemble_df.max$ENSEMBLE <- rowStat(ensemble_df.max, stat='max')
    if(id_scene==1){
      file_min = paste(dirName(dir_save,'Site_RCP45'), '/Minimum_', locName, '.csv', sep = '')
      file_mean = paste(dirName(dir_save,'Site_RCP45'), '/Mean_', locName, '.csv', sep = '')
      file_max = paste(dirName(dir_save,'Site_RCP45'), '/Maximum', locName, '.csv', sep = '')
    }
    else{
      file_min = paste(dirName(dir_save,'Site_RCP85'), '/Minimum_', locName, '.csv', sep = '')
      file_mean = paste(dirName(dir_save,'Site_RCP85'), '/Mean_', locName, '.csv', sep = '')
      file_max = paste(dirName(dir_save,'Site_RCP85'), '/Maximum', locName, '.csv', sep = '')
    }
    write.csv(ensemble_df.min, file = file_min)
    write.csv(ensemble_df.mean, file = file_mean)
    write.csv(ensemble_df.max, file = file_max)
  }
}

get_aggregate <-function(df, time_scale, stat){
  ncols = ncol(df)-3
  head_names = colnames(df)[-c(12:14)]
  if (time_scale == 'year'){
    head_names = c('Year', head_names)
    yearVec = levels(as.factor(df$year))
    nyear = length(yearVec)
    data_matrix = matrix(NaN, nrow = nyear, ncol = ncols+1)
    for (i in 1:nyear) {
      yearData = subset(df, year==yearVec[i])
      yearData = yearData[, -c(12:14)]
      if(stat=='min'){
        stat_vec = colMins(as.matrix(yearData), na.rm = TRUE)
      }
      else if (stat=='mean'){
        stat_vec = colMeans2(as.matrix(yearData), na.rm = TRUE)
      }
      else if (stat=='max'){
        stat_vec = colMaxs(as.matrix(yearData), na.rm = TRUE)
      }
      else if(stat=='sum'){
        stat_vec = colSums2(as.matrix(yearData), na.rm = TRUE)
      }
      else{
        print ('No key word')
      }
      data_matrix[i, 1] = as.integer(yearVec[i])
      data_matrix[i, 2:(ncols+1)] = stat_vec
    }
  }
  else if (time_scale=='month'){
    head_names = c('Year', 'Month', head_names)
    yearVec = levels(as.factor(df$year))
    nyear = length(yearVec)
    month_vec = levels(as.factor(df$month))
    nmonth = length(month_vec)
    data_matrix = matrix(NaN, nrow = nyear*nmonth, ncol = ncols+2)
    k = 1
    for (i in 1:nyear) {
      yearData = subset(df, year==yearVec[i])
      for (j in 1:nmonth) {
        month_data = subset(yearData, month==month_vec[j])
        month_data = month_data[, -c(12:14)]
        if(stat=='min'){
          stat_vec = colMins(as.matrix(month_data), na.rm = TRUE)
        }
        else if (stat=='mean'){
          stat_vec = colMeans2(as.matrix(month_data), na.rm = TRUE)
        }
        else if (stat=='max'){
          stat_vec = colMaxs(as.matrix(month_data), na.rm = TRUE)
        }
        else if(stat=='sum'){
          stat_vec = colSums2(as.matrix(month_data), na.rm = TRUE)
        }
        else{
          print ('No key word')
        }
        data_matrix[k, 1] = as.integer(yearVec[i])
        data_matrix[k, 2] = as.integer(month_vec[j])
        data_matrix[k, 3:(ncols+2)] = stat_vec
        k = k+1
      }
    }
  }
  df_aggregate = data.frame(data_matrix)
  colnames(df_aggregate) = head_names
  return(df_aggregate)
  
}

aggregate_climate_data <- function(listFrame, model, dir_save, id_scene){
  location = listFrame$Points
  for (locName in location){
    print(paste('Processing ', locName, sep = ''))
    if (id_scene==1){
      file_mean = paste(dirName(dir_save,'Site_RCP45'), '/Mean_', locName, '.csv', sep = '')
      file_model = paste(dirName(dir_save,'RCP45'), '/', locName, model, '.csv', sep = '')
    } else{
      file_mean = paste(dirName(dir_save,'Site_RCP85'), '/Mean_', locName, '.csv', sep = '')
      file_model = paste(dirName(dir_save,'RCP85'), '/', locName, model, '.csv', sep = '')
    }
    ensemble_df = read.csv(file_mean, header = T, sep = ",")
    ensemble_df = ensemble_df[, -1]
    data.model = read.csv(file_model, header = T, sep = ",")
    ensemble_df_YYmm = ensemble_df
    ensemble_df_YYmm$Date =data.model$Date
    ensemble_df_YYmm$month = month(data.model$Date)
    ensemble_df_YYmm$year = year(data.model$Date)
    print('Aggregating Annually')
    year_data_min = get_aggregate(ensemble_df_YYmm, time_scale = 'year', stat = 'min')
    year_data_mean = get_aggregate(ensemble_df_YYmm, time_scale = 'year', stat = 'mean')
    year_data_max = get_aggregate(ensemble_df_YYmm, time_scale = 'year', stat = 'max')
    print('Aggregating monthly')
    month_data_min = get_aggregate(ensemble_df_YYmm, time_scale = 'month', stat = 'min')
    month_data_mean = get_aggregate(ensemble_df_YYmm, time_scale = 'month', stat = 'mean')
    month_data_max = get_aggregate(ensemble_df_YYmm, time_scale = 'month', stat = 'max')
    
    if (id_scene==1){
      file_year_min = paste(dirName(dir_save,'Summary_RCP45'), '/Annual_min_', locName, '.csv', sep = '')
      file_year_mean = paste(dirName(dir_save,'Summary_RCP45'), '/Annual_mean_', locName, '.csv', sep = '')
      file_year_max = paste(dirName(dir_save,'Summary_RCP45'), '/Annual_max_', locName, '.csv', sep = '')
      file_month_min = paste(dirName(dir_save,'Summary_RCP45'), '/Month_min_', locName, '.csv', sep = '')
      file_month_mean = paste(dirName(dir_save,'Summary_RCP45'), '/Month_mean_', locName, '.csv', sep = '')
      file_month_max = paste(dirName(dir_save,'Summary_RCP45'), '/Month_max_', locName, '.csv', sep = '')
    }
    if(id_scene==2){
      file_year_min = paste(dirName(dir_save,'Summary_RCP85'), '/Annual_min_', locName, '.csv', sep = '')
      file_year_mean = paste(dirName(dir_save,'Summary_RCP85'), '/Annual_mean_', locName, '.csv', sep = '')
      file_year_max = paste(dirName(dir_save,'Summary_RCP85'), '/Annual_max_', locName, '.csv', sep = '')
      file_month_min = paste(dirName(dir_save,'Summary_RCP85'), '/Month_min_', locName, '.csv', sep = '')
      file_month_mean = paste(dirName(dir_save,'Summary_RCP85'), '/Month_mean_', locName, '.csv', sep = '')
      file_month_max = paste(dirName(dir_save,'Summary_RCP85'), '/Month_max_', locName, '.csv', sep = '')
    }
    
    write.csv(year_data_min, file = file_year_min)
    write.csv(year_data_mean, file = file_year_mean)
    write.csv(year_data_max, file = file_year_max)
    write.csv(month_data_min, file = file_month_min)
    write.csv(month_data_mean, file = file_month_mean)
    write.csv(month_data_max, file = file_month_max)
    print('-------------------------------')
  }
  
}

get_daily_spatial_data<-function(model_name, stat, id_scene, listFrame){
  data_spatial = data.frame()
  siteName = paste('SHASTA0', 'ACCESS1', sep = '')
  data_temp <-climate_data(siteName, 'SHASTA0', 'ACCESS1', idScene=id_scene)
  date_vec = as.character(data_temp$Date)
  for(locName in listFrame$Points){
    print(paste('Gathering data at ', locName, ' from model ', model_name, sep = '' ))
    if(id_scene==1){
      data_dir = dirName('PROCESS', 'Site_RCP45')
    }
    else{
      data_dir = dirName('PROCESS', 'Site_RCP85')
    }
    if(stat=='min'){
      data.File = paste(data_dir, '/Minimum_', locName, '.csv', sep='')
    }
    if (stat=='mean'){
      data.File = paste(data_dir, '/Mean_', locName, '.csv', sep='')
    }
    if (stat=='max'){
      data.File = paste(data_dir, '/Maximum', locName, '.csv', sep='')
    }
    data.read = read.csv(data.File)
    if(is.na(model_name)){
      model_name = 'ENSEMBLE'
    }
    data_val = as.matrix(data.read[model_name])
    data_matrix = t(as.data.frame(data_val))
    data_matrix = as.data.frame(data_matrix)
    colnames(data_matrix) <- date_vec
    rownames(data_matrix) = locName
    data_spatial <- rbind(data_spatial, data_matrix)
    rm(data_matrix, data_val)
  }
  return(data_spatial)
}


get_spatial_data <- function(listFrame, dir_read, id_scene, scale, stat, model){
  location = listFrame$Points
  if (scale=='Daily'){
    data_matrix<-get_daily_spatial_data(model, stat, id_scene, listFrame)
    df_out = listFrame[, 1:3]
    df_out = cbind(df_out, data_matrix)
    if(is.na(model)){
      model = 'ENSEMBLE'
    }
  } 
  else{
    if (scale=='Month'){
      data_matrix = matrix(NaN, nrow = length(location), ncol = 1800)
    }
    if (scale=='Annual'){
      data_matrix = matrix(NaN, nrow = length(location), ncol = 150)
    }
    for (i in 1:length(location)) {
      if (id_scene==1){
        file_data = paste(dirName(dir_read,'Summary_RCP45'), '/', scale, '_', stat, '_', location[i], '.csv', sep = '')
      }
      else{
        file_data = paste(dirName(dir_read,'Summary_RCP85'), '/', scale, '_', stat, '_', location[i], '.csv', sep = '')
      }
      data_climate = read.csv(file_data)
      if(is.na(model)){
        data_matrix[i, ]= data_climate$ENSEMBLE
        model = 'ENSEMBLE'
      }
      else{
        data_matrix[i, ]= as.numeric(as.matrix(data_climate[model]))
      }
    }
    data_matrix = data.frame(data_matrix)
    if (scale=='Month'){
      date_df = data_climate[, c('Year', 'Month')]
      date_vec = c()
      for (i in 1:nrow(date_df)) {
        date_string =as.yearmon(paste(date_df$Year[i], date_df$Month[i], sep = '-'))
        date_vec = c(date_vec, as.character(date_string))
      }
      colnames(data_matrix) = date_vec
    }
    else {
      colnames(data_matrix) = data_climate$Year
    }
    df_out = listFrame[, 1:3]
    df_out = cbind(df_out, data_matrix)
  }
  if (id_scene==1){
    emm_scene = 'RCP45'
  }
  else{
    emm_scene = 'RCP85'
  }
  dir2save = dirName(dir_read, 'Spatial_data')
  file_out = paste(dir2save, '/', model, '_', scale, '_', stat, '_', emm_scene, '.csv', sep = '')
  write.csv(df_out, file = file_out, row.names = FALSE)
  return(df_out)
  
}

export_data <- function(listFrame, dir_read, id_scene, scale, stat, modelList){
  for (model in modelList) {
    spatial_data <- get_spatial_data(listFrame, dir_read, id_scene, scale, stat, model)
  }
}

setProjection<- function(dataSet){
  colnames(dataSet) = c('Name', 'Latitude', 'Longitude', 'Temperature')
  coordinates(dataSet) <- ~ Longitude + Latitude #longitude first
  proj4string(dataSet) =  "+proj=longlat +datum=WGS84"
  return(dataSet)
}
getGrid<- function(mapSite, gridSize){
  r <- raster(mapSite)
  res(r) <- gridSize  # 10 km if your CRS's units are in km
  g <- as(r, 'SpatialGrid')
  return(g)
}
getVariogram<-function(data){
  UTM <- spTransform(data, CRS=CRS("+proj=utm +zone=35+north+ellps=WGS84+datum=WGS84"))
  vargram<-variogram(Temperature~1, UTM)
  np <- round(min(vargram$np))
  pSill <- round(max(vargram$gamma))
  fve <- fit.variogram(vargram, vgm(psill = pSill, "Sph", NA, np))
  plt<-plot(vargram, fve)
  return(list(variogram = vargram, sill=pSill, fitted=fve, plt=plt))
  
}

do.OK <- function(data, g, fve, caAOI) {
  k <- gstat(formula=Temperature~1, locations=data, model=fve)
  kp <- predict(k, g)
  ok <- brick(kp)
  ok <- mask(ok, caAOI)
  return(ok)
}

get_ordinary_kriging <- function(model, dir2Read, fileStudy, scale, stat, attribute_name, emm_scene){
  file_name = paste(model, scale, stat, emm_scene, sep = '_')
  file2Read = paste(dir2Read, '/', file_name, '.csv', sep = '')
  data_set <- read.csv(file2Read, sep=",", header=T)
  df_info = data_set[, 1:3]
  data4Krig = data_set[attribute_name]
  data4Krig = cbind(df_info, data4Krig)
  data2Analyze = na.omit(data4Krig)
  data2Analyze <- setProjection(data2Analyze)
  TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +ellps=GRS80")
  sfAOI <- st_read(fileStudy)
  sfAOI1 = as_Spatial(sfAOI)
  caAOI <- spTransform(sfAOI1, TA)
  UTM <- spTransform(data2Analyze, CRS=CRS("+proj=utm +zone=35+north+ellps=WGS84+datum=WGS84"))
  data.transform = spTransform(data2Analyze, TA)
  # Kriging----
  g <- getGrid(mapSite = caAOI, gridSize = 10)
  varResult<-getVariogram(data2Analyze)
  ok <- do.OK(data = data.transform, g, varResult$fitted, caAOI) 
  return(list(Variogram=varResult, krig=ok))
}

