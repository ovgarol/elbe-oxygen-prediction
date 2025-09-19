# SPDX-FileCopyrightText: 2025 Helmholtz-Zentrum hereon GmbH
# SPDX-License-Identifier: CC-BY-4.0
# SPDX-FileContributor Ovidio Garcia-Oliva <ovidio.garcia@hereon.de>

# Load necessary libraries
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(ggplot2)
library(caret)
library(lubridate)
library(TeachingDemos)

setwd(system('pwd',intern=T))
sink("output.txt")
col.scale = 'Berlin'
#####

get.data.WSV = function(name, is.log = F){
  
  data = read.csv(name,skip=22,sep='\t')
  data$date = as.POSIXct(data[,1],format='%Y-%m-%d %H:%M:%S') 
  data$date = as.Date(data$date)
  
  data$x = data[,2]
  if(is.log) data$x = log(data$x)
  data = subset(data, select=c(date,x))
  data = na.omit(data)
  data = aggregate(data, list(data$date), mean)
  
  return(data)
}

get.data.DWD = function(name){
  data = read.delim(system(name, intern = TRUE), 
                    sep = ";",
                    header = TRUE, 
                    comment.char = "#"
  )
  
  data$date = as.Date(as.POSIXct(as.character(data$MESS_DATUM),format="%Y%m%d%H"))
  data = data[data$TT_TU!=-999,]
  data$x = data$TT_TU
  
  data = subset(data, select=c(date,x))
  data = aggregate(data, list(data$date), mean)
  
  
  return(subset(data,select=c(date,x)))
}

.formater = function(xs)sprintf("%.2f",xs)

color.bar.h = function(lut, min, max=-min, nticks=2, title='',...){
  scale = (length(lut)-0)/(max-min)
  ticks = seq(min, max, len=nticks)
  plot( c(min,max*1.),c(-.1,.1), type='n', bty='n', xaxt='n', ylab='', yaxt='n', xlab='', main=NA,...)
  text(sum(c(min,max*1.))/2,0.,title,adj=c(0.5,1.25),col='#444444')
  axis(3, ticks, las=1,cex=0.95,cex.axis=0.95,line=-1,tick=F,col.axis='#444444')
  
  rect(min,.1,max,0, col=NA, border='#444444', lwd=1, useRaster = TRUE)
  for (i in 1:(length(lut)-0)) {
    y = (i-1)/scale + min
    rect(y,.1,y+1/scale,0, col=lut[i], border=NA, useRaster = TRUE)
  }
}


lagged_model = function(df, N.MAX, N.MIN, stepit = F){
  # Create lagged features
  df_lagged = create_lagged_features(df, lags = N.MAX, init = N.MIN)
  
  # Remove rows with NA (due to lagging)
  df_model = df_lagged %>% drop_na()
  
  # Define the formula for regression
  # Predicting z using lagged values of x, y, z
  formula = as.formula(paste(
    "z ~", 
    paste(c(
      paste0("x_lag", N.MIN:N.MAX),
      paste0("y_lag", N.MIN:N.MAX),
      paste0("z_lag", N.MIN:N.MAX)
    ), collapse = " + ")
  ))
  
  # Fit a linear regression model for the value
  model = glm(formula, data = df_model)
  if(stepit) model = step(model, direction = "both", trace = 0)
  
  # Predicting hypoxia using lagged values of x, y, z
  formula_glm = as.formula(paste(
    "hypoxia ~", 
    paste(c(
      paste0("x_lag", N.MIN:N.MAX),
      paste0("y_lag", N.MIN:N.MAX),
      paste0("z_lag", N.MIN:N.MAX)
    ), collapse = " + ")
  ))
  # Fit a logit regression model for hypoxia
  glm_model = glm(formula_glm, data = df_model, family = binomial(link = "logit"))
  if(stepit) glm_model = step(glm_model, direction = "both", trace = 0)
  
  return(list(model,glm_model,df_model))
}

########

temp.air = get.data.DWD('ls ./data/produkt_tu_stunde_1*.txt')

for(KK in 1:3){
  
  if(KK==1){
    temp.water = get.data.WSV("./data/Bunthaus_WGMN!Wassertemperatur.txt")
    oxygen = get.data.WSV("./data/Bunthaus_WGMN!Sauerstoffgehalt_(Einzelmessung).txt")
    name = 'Bunthaus'
  }
  
  if(KK==2){
    temp.water = get.data.WSV("./data/Seemannshoeft_WGMN!Wassertemperatur.txt")
    oxygen = get.data.WSV("./data/Seemannshoeft_WGMN!Sauerstoffgehalt_(Einzelmessung).txt")
    name = 'Seemannshöft'
  }
  
  if(KK==3){
    temp.water = get.data.WSV("./data/Blankenese_WGMN!Wassertemperatur.txt")
    oxygen = get.data.WSV("./data/Blankenese_WGMN!Sauerstoffgehalt_(Einzelmessung).txt")
    name = 'Blankenese'
  }
  
  station.data = merge(temp.air,temp.water,by='date',suffixes = c('.air','.temp'))
  station.data = merge(station.data,oxygen,by='date',suffixes = c('','.oxy'))
  
  #####
  par(las=1)
  
  ts_data = NULL
  ts_data$t = station.data$date
  ts_data$x = station.data$x.temp
  ts_data$y = station.data$x.air
  ts_data$z = station.data$x
  ts_data$hypoxia = ts_data$z < 4
  
  ts_data = as.data.frame(ts_data)
  ts_data = ts_data[year(ts_data$t)>1995,]
  
  all_dates = data.frame(t=seq.Date(min(station.data$date),max(station.data$date),by='day'))
  all_dates = data.frame(t=seq.Date(as.Date('2000-01-01'),as.Date('2010-01-01'),by='day'))
  
  ts_all = ts_data
  ts_data = merge(all_dates,ts_data,by='t',all.x=T)
  
  # Create lag features for x, y, z
  create_lagged_features = function(data, lags = N.MAX, init = N.MIN) {
    for (var in c("x", "y", "z")) {
      for (i in init:lags) {
        data[[paste0(var, "_lag", i)]] <- dplyr::lag(data[[var]], n = i)
      }
    }
    return(data)
  }
  
  # Assume df is your original time series data
  # Make sure it is sorted by time
  df = ts_data %>% arrange(t)
  
  ##########
  
  span = 7 # maximum lag (no larger than a week)
  
  rr = NULL
  rr$min = rep(1:span,each=span)
  rr$max = rr$min + rep(1:span)-1
  rr$n = 1:length(rr$min)
  rr = as.data.frame(rr)
  rr$r1 = 0
  rr$r2 = 0
  rr$accu = 0
  rr$reca = 0
  rr$spec = 0
  rr$prec = 0

  for(i in 1:length(rr$n)){
    
    N.MAX = rr$max[i]
    N.MIN = rr$min[i]
    
    results = lagged_model(df, N.MAX, N.MIN)
    model = results[[1]]
    glm_model = results[[2]]
    df_model = results[[3]]
    
    # Predict z at time t 
    predicted_z = predict(model)
    predicted_hypoxia = predict(glm_model,type = "response")
    
    # Output the prediction
    if(F){
      par(mfrow=c(2,2))
      plot(df_model$z, predicted_z)
      abline(b=1,a=0)
      plot(df_model$hypoxia, predicted_hypoxia )
      abline(b=1,a=0) 
      plot(df_model$z,predicted_hypoxia)
    }
    
    # storing AIC values
    r1 = summary(model)
    rr$r1[i] = r1$aic
    
    r2 = summary(glm_model)
    rr$r2[i] = r2$aic
    
    # creating confusion matrix accuracy, sensibility and specificity
    is.hypoxic = as.factor(predicted_hypoxia>0.5)
    levels(is.hypoxic) = c(levels(is.hypoxic), "TRUE", "FALSE")
    
    cm = confusionMatrix(as.factor(df_model$hypoxia),
                         is.hypoxic,
                         positive = 'TRUE'
                         )
    rr$accu[i] = cm$overall[1]
    rr$reca[i] = cm$byClass[1]
    rr$spec[i] = cm$byClass[2]
    rr$prec[i] = cm$byClass[5]
  }
  
  # Output the prediction with the better F1 or AIC
  rr$F1 = 2*rr$prec*rr$reca/(rr$prec+rr$reca)
  
  plot(rr)
  
  N.MAX = rr$max[which.min(rr$r2)]
  N.MIN = rr$min[which.min(rr$r2)]
  # N.MAX = rr$max[which.max(rr$F1)]
  # N.MIN = rr$min[which.max(rr$F1)]
  
  results = lagged_model(df, N.MAX, N.MIN,stepit=T)
  model = results[[1]]
  glm_model = results[[2]]
  df_model = results[[3]]
  
  print(name)
  print(summary(model))
  print(summary(glm_model))
  
  # Predict z and hypoxia
  predicted_z = predict(model)
  predicted_hypoxia = predict(glm_model,type = "response")
  
  if(F){
    par(mfrow=c(2,2))
    plot(df_model$z, predicted_z)
    abline(b=1,a=0)
    
    plot(df_model$hypoxia, predicted_hypoxia,ylim=c(0,1) )
    abline(b=1,a=0) 
    
    plot(df_model$z,predicted_hypoxia)
    
    plot(rr$n,rr$r1 - rr$r1[which.min(rr$r1)],ylim=c(0,40))
    abline(h=2)
  }
  
  summary(model)
  summary(glm_model)
  
  
  ####
  # performance
  ####

  pdf(paste0('./fig/',name,'_performance.pdf'), width = 8, height = 4)
  par(mfrow=c(1,2),oma=0.1*c(1,1,1,1),mai=0.5*c(1.75,1.5,1,1), las=1)
  
  plot(rr$min,rr$reca,
       ylim=c(.30,1),
       xlab='forecast horizon, n',
       ylab='performance',
       type='n')
  points(rr$min,rr$F1,cex=1,col=alpha('gray',0.5),pch=20)
  abline(h=0.5,lty=2)
  lines(supsmu(rr$min,rr$reca,span=0.01),col='tan',lwd=1)
  lines(supsmu(rr$min,rr$prec,span=0.01),col='gray',lwd=2)
  lines(supsmu(rr$min,rr$F1),col='black',lwd=3)
  abline(h=0.5,lty=2)
  title(main=name,adj=0,font.main=1,line=0.5)
  if(KK==1)legend('topright',
         bty='n',
         legend = c('Recall','Precision','F1-Score'),
         col=c('tan','gray','black'),
         lwd=c(1,2,3)
         )
  #plot(rr$min,rr$accu,ylim=c(.90,1))
  #plot(rr$min,rr$spec,ylim=c(.90,1))
  text(1,0.3,letters[KK],adj=c(0,0),font=2)
  image(unique(rr$min),
        1:span, 
        t(matrix(round(100*rr$F1), nrow = span)),
        xlab='forecast horizon, n',
        ylab='observation window, m',
        col=hcl.colors(length(unique(round(100*rr$F1))),'Zi',rev=T)
  )
  text(7,7,letters[KK+3],adj=c(0,0),font=2)
  title(main=paste('F1-Score at',name),adj=0,font.main=1,line=0.5)
  
  for(i in 1:7)for(j in 1:7)if(!(i==7 & j==7))text(i,j,paste0(t(matrix(rr$F1%>%.formater(), nrow = span))[i,j]),col='white')
  #text(span,1,round(min(rr$F1),digits=2),col='black')
  dev.off()
  ####
  # validation
  ####
  
  # Predict z and hypoxia
  df_all = create_lagged_features(ts_all, N.MAX, N.MIN)
  predicted_z = predict(model ,newdata = df_all)
  predicted_hypoxia = predict(glm_model,type = "response",newdata = df_all)
  
  # creating confusion matrix accuracy, sensibility and specificity
  is.hypoxic = as.factor(predicted_hypoxia>0.5)
  levels(is.hypoxic) = c(levels(is.hypoxic), "TRUE", "FALSE")
  
  cm = confusionMatrix(as.factor(df_all$hypoxia),
                       is.hypoxic,
                       positive = 'TRUE'
  )
  
  print(cm)
  
  accu = cm$overall[1] %>% round(digits = 2) 
  reca = cm$byClass[1] %>% round(digits = 2)
  spec = cm$byClass[2] %>% round(digits = 2)
  prec = cm$byClass[5] %>% round(digits = 2)
  F1 = 2*prec*reca/(prec+reca) 
  F1 = F1 %>% round(digits = 2)
  
  pdf(paste0('./fig/',name,'_comp.pdf'), width = 8, height = 3)
  par(mfrow=c(1,1),oma=0.1*c(1,1,1,1),mai=0.5*c(1,1.5,1,0), las=1)
  plot(ts_all$t,
       ts_all$z,
       ylim=c(0,16),
       xlim=as.Date(c('1995-01-01','2025-01-01')),
       type='n',
       xlab='',
       ylab='dissolved oxygen mg/L'
  )
  polygon(as.Date(c('2000-01-01','2000-01-01','2010-01-01','2010-01-01')),
          c(0,16,16,0),
          border=NA,
          col=alpha('tan',0.15)
  )
  text(as.Date('2000-01-01'), 16,
       'training period', 
       col='tan',
       font=2,
       adj=c(-0.1,1.1)
  )
  text(as.Date('1995-01-01'), 16,
       letters[KK], 
       col='black',
       font=2,
       adj=c(0,1)
  )
  points(df_all$t,
        predicted_z,
        col=c('slategray','tan2')[df_all$t%in%ts_data$t +  1 ],
        pch=20,
        lwd=3
  )
  lines(ts_all$t,
        ts_all$z,
        type='l',
        lwd=0.5
  )
  abline(h=4,lty=2)
  title(main= paste0(name,', lags from ',N.MIN,' to ',N.MAX,' (n = ', N.MIN,', m = ',N.MAX-N.MIN,')'),adj=0,font.main=1,line=0.5)
  title(sub= paste0('Precision = ', prec, ', Recall = ', reca, ', F1-Score = ', F1),adj=1,font.main=1,line=-1,cex.sub=0.8)
  dev.off()
  
  ##########
  ## curves for hypoxia
  ##########
  
  pdf(paste0('./fig/',name,'_hypox.pdf'), width = 4, height = 4)
  par(mfrow=c(1,1),oma=0.1*c(1,1,1,1),mai=0.5*c(1.75,1.75,1,1), las=1)
  plot(0,
       xlab='dissolved oxygen, mg/L',
       ylab='hypoxia probability',
       ylim=c(0,1),
       xlim=c(1,8),
       type='n'
  )
  text(8, 1,
       letters[KK], 
       col='black',
       font=2,
       adj=c(0,1)
  )
  
  title(main=name,adj=0,font.main=1,line=0.5)
  
  ALL.DAYS = 12
  
  palette(alpha(hcl.colors(2*ALL.DAYS + span,col.scale,rev=F),0.15))
  
  for(N.MIN in 1:ALL.DAYS){
    for(N.MAX in N.MIN:(N.MIN+span)){
      df_all = create_lagged_features(ts_all, N.MAX, N.MIN)
      results = lagged_model(df_all, N.MAX, N.MIN)
      model = results[[1]]
      glm_model = results[[2]]
      df_model = results[[3]]
      
      # Predict z at time t + 1
      predicted_z = predict(model)
      predicted_hypoxia = predict(glm_model,type = "response")
      
      lines(supsmu(df_model$z,predicted_hypoxia,span=.001),col=(N.MAX+N.MIN))
      #points(df_model$z,predicted_hypoxia,col=(N.MAX+N.MIN),pch=20)
      
    }
  }
  abline(v=4,h=0.5,lty=2)
  dev.off()
  
  ##########
  ## curves for oxygen
  ##########
  
  pdf(paste0('./fig/',name,'_oxy.pdf'), width = 4, height = 4)
  par(mfrow=c(1,1),oma=0.1*c(1,1,1,1),mai=0.5*c(1.75,1.5,1,0), las=1)
  plot(0,
       xlab='observed dissolved oxygen, mg/L',
       ylab='modelled dissolved oxygen, mg/L',
       ylim=c(1,15),
       xlim=c(1,15),
       type='n'
  )
  text(1, 15,
       letters[KK], 
       col='black',
       font=2,
       adj=c(0,1)
  )
  
  title(main=name,adj=0,font.main=1,line=0.5)
  
  ALL.DAYS = 12
  
  palette(alpha(hcl.colors(2*ALL.DAYS + span,col.scale,rev=F) -> pc,0.15))
  
  for(N.MIN in 1:ALL.DAYS){
    for(N.MAX in N.MIN:(N.MIN+span)){
      if(N.MIN%%2 == 0 ) next
      df_all = create_lagged_features(ts_all, N.MAX, N.MIN)
      results = lagged_model(df_all, N.MAX, N.MIN)
      model = results[[1]]
      glm_model = results[[2]]
      df_model = results[[3]]
      
      # Predict z at time t + 1
      predicted_z = predict(model)
      predicted_hypoxia = predict(glm_model,type = "response")
      
      lines(supsmu(df_model$z,predicted_z,span=.001),col=(N.MAX+N.MIN))
      #points(df_model$z,predicted_z,col=(N.MAX+N.MIN),pch=20)
    }
  }
  abline(v=4,h=4,lty=2)
  abline(a=0,b=1,lty=2)
  
  subplot(color.bar.h(pc,
                      1,ALL.DAYS + 4,nticks = 3,
                      title='lagged days',cex.axis=0.75),
          12,4,
          size=c(1.0,0.5)
  )
  
  dev.off()
  
}

sink() 



