# SPDX-FileCopyrightText: 2025 Helmholtz-Zentrum hereon GmbH
# SPDX-License-Identifier: Apache-2.0
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
library(latex2exp)

setwd(system('pwd',intern=T))
Sys.setlocale("LC_TIME", "en_US.UTF-8")

#sink("output.txt")
col.scale = 'Viridis'
threshold.value = 4

simplest.model.only = T ## T: main figures for manuscript, F: test mode
test.wind = T ## T: test influence of wind, F: test the influence of chla and dis

use_fut = F ## use future values of air temperature
use.weights = F ## use increased weighting for low oxygen 
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

get.data.DWD = function(name,var.name='TT_TU'){
  data = read.delim(system(name, intern = TRUE), 
                    sep = ";",
                    header = TRUE, 
                    comment.char = "#"
  )
  
  data$date = as.Date(as.character(data$MESS_DATUM),format="%Y%m%d")
  data$x = data[[var.name]]
  data = data[data$x!=-999,]
  
  data = subset(data, select=c(date,x))
  data = aggregate(data, list(data$date), mean)
  
  
  return(subset(data,select=c(date,x)))
}

.formater = function(xs,format="%.2f")sprintf(format,xs)

plot = function(...) graphics::plot(...,mgp=c(1.75,0.2,0),tck=0.02)

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

create_lagged_features = function(data, lags, init = 1, vars) {
  missing_vars = setdiff(vars, names(data))
  if(length(missing_vars) > 0) {
    stop(paste("Missing columns for lag generation:", paste(missing_vars, collapse = ", ")))
  }
  for (var in vars) {
    for (i in init:lags) {
      data[[paste0(var, "_lag", i)]] <- dplyr::lag(data[[var]], n = i)
    }
  if(use_fut && var == "y") for (i in init:lags) {
    data[[paste0('y', "_fut", i)]] <- rev(dplyr::lag(rev(data[['y']]), n = i))
  }
  }
  return(data)
}

lagged_model = function(df, N.MAX, N.MIN, stepit = F,
                        vars = c("x", "y", "z"),
                        target = "z",
                        binary_target = "hypoxia"){
  if(!target %in% names(df)) {
    stop(paste("Target column not found:", target))
  }

  # Create lagged features
  df_lagged = create_lagged_features(df, lags = N.MAX, init = N.MIN, vars = vars)

  # Remove rows with NA (due to lagging)
  df_model = df_lagged %>% drop_na()

  # Weight by target if it's continuous
  if(use.weights) {
    #w_lo = 1/(1+as.numeric(df_model[[target]]>4))
    w_lo = 1/(1+df_model[[target]])
    
  } else {
    w_lo = 1 + 0*df_model[[target]]
  }

  # Build lagged term list for all predictor variables
  lagged_terms = c()
  for(var in vars) {
    lagged_terms = c(lagged_terms, paste0(var, "_lag", N.MIN:N.MAX))
  }
  
  if(use_fut) lagged_terms = c(lagged_terms,paste0("y_fut", N.MIN:N.MAX))

  # Define the formula for regression on continuous target
  formula = as.formula(paste(target, "~", paste(lagged_terms, collapse = " + ")))

  # Fit a linear regression model for the value
  model = glm(formula, data = df_model, weights=w_lo)
  if(stepit) model = step(model, direction = "both", trace = 0)

  # Fit a logit regression model for binary target (if it exists)
  glm_model = NULL
  if(!is.null(binary_target) && binary_target %in% names(df_model)) {
    formula_glm = as.formula(paste(binary_target, "~", paste(lagged_terms, collapse = " + ")))
    glm_model = glm(formula_glm, data = df_model, family = binomial(link = "logit"), weights=w_lo)
    if(stepit) glm_model = step(glm_model, direction = "both", trace = 0)
  }

  return(list(model, glm_model, df_model))
}

########

# this is always on, except for the exceptions
use.all.data = T ## test influence of other variables on predictability

# For the figures in the main manuscript
if(simplest.model.only){
  test.wind = F
  use.all.data = F
} 

# for wind analysis
if(test.wind){
  use.all.data = F
  wind.speed = get.data.DWD('ls ./data/produkt_ff_stunde_1*.txt',var.name = 'F')
  wind.dir = get.data.DWD('ls ./data/produkt_ff_stunde_1*.txt',var.name = 'D')
  wind = merge(wind.speed,wind.dir,by='date',suffixes = c('.s','.d'))
  
  wind.U10 = NULL
  wind.U10$date = wind$date
  wind.U10$x = - wind$x.s * sin(pi * wind$x.d/180)
  wind.U10 = as.data.frame(wind.U10)
  
  wind.V10 = NULL
  wind.V10$date = wind$date
  wind.V10$x = - wind$x.s * cos(pi * wind$x.d/180)
  wind.V10 = as.data.frame(wind.V10)
}

temp.air = get.data.DWD('ls ./data/produkt_tu_stunde_1*.txt')
discharge = get.data.WSV("./data/ab_darchau!Abfluss.txt")

target_var = "z"
binary_target_var = "hypoxia"
baseline_lag = 5

predictor_map = c(
  p = "x.u10",
  q = "x.v10",
  v = "x.chla",
  w = "x.dis",
  x = "x.temp",
  y = "x.air",
  z = "x"
)

predictor_maps = list(c(  v = "x.chla", ## 1. All variables
                          w = "x.dis",
                          x = "x.temp",
                          y = "x.air",
                          z = "x"),
                      c(  w = "x.dis", ## 2. No chla
                          x = "x.temp",
                          y = "x.air",
                          z = "x"),
                      c(  v = "x.chla",## 3. No discharge
                          x = "x.temp",
                          y = "x.air",
                          z = "x"),
                      c(  x = "x.temp", ## 4. No discharge no chla
                          y = "x.air",
                          z = "x"),
                      c(  p = "x.u10",  ## 5. wind effect
                          q = "x.v10",
                          x = "x.temp", 
                          y = "x.air",
                          z = "x")
                      )

performance_metrics = list(list(1,2,3,4,5),list(1,2,3,4,5),list(1,2,3,4,5))

for(MM in 1:5){
  if(test.wind) if(!MM%in%c(4,5)) next
  if(!test.wind & MM==5) next  
  if(simplest.model.only) if(MM!=4) next
  
  predictor_map = predictor_maps[[MM]] 
  prefix = paste0('_',MM)
  if(simplest.model.only) prefix = '_0'
  
for(KK in 1:3){
  # the comparison is just calculated for seemanshoeft the other stations have not enough data
  if(!simplest.model.only & !test.wind & (KK%in%c(1,3))) next  
  
  if(KK==1){
    temp.water = get.data.WSV("./data/Bunthaus_WGMN!Wassertemperatur.txt")
    oxygen = get.data.WSV("./data/Bunthaus_WGMN!Sauerstoffgehalt_(Einzelmessung).txt")
    chla = get.data.WSV("./data/Bunthaus_WGMN!Chlorophyll.txt",is.log = T)
    name = 'Bunthaus'
  }

  if(KK==2){
    temp.water = get.data.WSV("./data/Seemannshoeft_WGMN!Wassertemperatur.txt")
    oxygen = get.data.WSV("./data/Seemannshoeft_WGMN!Sauerstoffgehalt_(Einzelmessung).txt")
    chla = get.data.WSV("./data/Seemannshoeft_WGMN!Chlorophyll.txt",is.log = F)
    name = 'Seemannshöft'
  }

  if(KK==3){
    temp.water = get.data.WSV("./data/Blankenese_WGMN!Wassertemperatur.txt")
    oxygen = get.data.WSV("./data/Blankenese_WGMN!Sauerstoffgehalt_(Einzelmessung).txt")
    chla = get.data.WSV("./data/Seemannshoeft_WGMN!Chlorophyll.txt",is.log = F) # CHla is not available for Blankenese
    name = 'Blankenese'
  }

  station.data = merge(temp.air,temp.water,by='date',suffixes = c('.air','.temp'))
  station.data = merge(station.data,oxygen,by='date',suffixes = c('','.oxy'))
  if(!simplest.model.only){
    if('x.dis'%in%predictor_map | use.all.data) station.data = merge(station.data,discharge,by='date',suffixes = c('','.dis'))
    if('x.chla'%in%predictor_map | use.all.data) station.data = merge(station.data,chla,by='date',suffixes = c('','.chla'))
    if(test.wind){
      station.data = merge(station.data,wind.U10,by='date',suffixes = c('','.u10'))
      station.data = merge(station.data,wind.V10,by='date',suffixes = c('','.v10'))
    }
  }
      
  station.data = subset(station.data,select=-grep('Group*',colnames(station.data)))
  
  #####
  par(las=1)

  missing_station_vars = setdiff(unname(predictor_map), names(station.data))
  if(length(missing_station_vars) > 0) {
    stop(paste("Missing station columns:", paste(missing_station_vars, collapse = ", ")))
  }

  ts_data = data.frame(t = station.data$date)
  for(var in names(predictor_map)){
    ts_data[[var]] = station.data[[predictor_map[[var]]]]
  }
  ts_data[[binary_target_var]] = ts_data[[target_var]] < threshold.value

  predictor_vars = names(predictor_map)

  ts_data = as.data.frame(ts_data)
  ts_data = ts_data[year(ts_data$t)>1995,]

  # this is a naive model assuming that hypoxia just depend of the year of the day
  if(T){
    ts_data$doy = yday(ts_data$t)
    ts_doy = aggregate(ts_data,by=list(ts_data$doy),function(...)mean(...,na.rm=T))
    ts_doy = subset(ts_doy,select=c('doy','hypoxia'))
    plot(ts_doy$doy,ts_doy$hypoxia,main=name) 
    ts_doy = merge(ts_data,ts_doy,by='doy',suffixes = c('','.naive'))
    plot(ts_doy$hypoxia,ts_doy$hypoxia.naive)
    naive_model = glm(hypoxia~hypoxia.naive,data=ts_doy,family = binomial(link = "logit"))
    summary(naive_model)

    probs = predict(naive_model, newdata=ts_doy, type='response')
    
    thresholds = seq(0, 1, by=0.01)
    f1_scores = sapply(thresholds, function(th) {
      preds = as.factor(probs > th)
      levels(preds) = c(levels(preds), "TRUE", "FALSE")
      
      cm = confusionMatrix(as.factor(ts_doy$hypoxia), preds, positive='TRUE')
      p = cm$byClass['Precision']
      r = cm$byClass['Recall']
      if (is.na(p) | is.na(r) | (p+r)==0) return(NA)
      2*p*r/(p+r)
    })
    
    best_th = thresholds[which.max(f1_scores)]
    
    is.hypoxic = as.factor(predict(naive_model,newdata = ts_doy,type='response')>best_th)
    levels(is.hypoxic) = c(levels(is.hypoxic), "TRUE", "FALSE")
    
    plot(ts_doy$hypoxia, predict(naive_model,newdata = ts_doy,type='response'))
    cm = confusionMatrix(as.factor(ts_doy$hypoxia),
                         is.hypoxic,
                         positive = 'TRUE'
    )
    # this is the naive F1-Score and precision assuming that only the climatology 
    naive_prec = cm$byClass['Precision']
    naive_F1 = 2*cm$byClass['Precision']*cm$byClass['Recall']/(cm$byClass['Precision']+cm$byClass['Recall'])
    print(naive_prec)
    print(naive_F1)
  }  
  
  all_dates = data.frame(t=seq.Date(min(station.data$date),max(station.data$date),by='day'))
  all_dates = data.frame(t=seq.Date(as.Date('2000-01-01'),as.Date('2010-01-01'),by='day'))
  if('x.chla'%in%predictor_map | use.all.data) all_dates = data.frame(t=seq.Date(as.Date('2014-01-01'),as.Date('2024-01-01'),by='day'))
  training_dates = as.Date(range(all_dates$t))
  
  ts_all = ts_data
  ts_data = merge(all_dates,ts_data,by='t',all.x=T)
  
  # Make sure it is sorted by time
  df = ts_data %>% arrange(t)

  ##########

  span = 7 # maximum lag (no larger than a week)

  rr = NULL
  rr$min = rep(1:span,each=span)
  rr$max = rr$min + rep(1:span)-1
  rr$n = 1:length(rr$min)
  rr = as.data.frame(rr)
  rr$r1 = NA
  rr$r2 = NA
  rr$accu = NA
  rr$reca = NA
  rr$spec = NA
  rr$prec = NA

  for(i in 1:length(rr$n)){
    
    N.MAX = rr$max[i]
    N.MIN = rr$min[i]

    if(N.MIN<baseline_lag) next
    
    results = lagged_model(df, N.MAX, N.MIN,
                 vars = predictor_vars,
                 target = target_var,
                 binary_target = binary_target_var)
    model = results[[1]]
    glm_model = results[[2]]
    df_model = results[[3]]

    # Predict z at time t
    predicted_z = predict(model)
    predicted_hypoxia = predict(glm_model,type = "response")

    # Output the prediction
    if(F){
      par(mfrow=c(2,2))
      plot(df_model[[target_var]], predicted_z)
      abline(b=1,a=0)
      plot(df_model[[binary_target_var]], predicted_hypoxia )
      abline(b=1,a=0)
      plot(df_model[[target_var]],predicted_hypoxia)
    }

    # storing AIC values
    r1 = summary(model)
    rr$r1[i] = r1$aic

    r2 = summary(glm_model)
    rr$r2[i] = r2$aic

    # creating confusion matrix accuracy, sensibility and specificity
    is.hypoxic = as.factor(predicted_hypoxia>0.5)
    levels(is.hypoxic) = c(levels(is.hypoxic), "TRUE", "FALSE")

    cm = confusionMatrix(as.factor(df_model[[binary_target_var]]),
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
  performance_metrics[[KK]][[MM]] = rr
  
  use.aic = T
  if(use.aic){
    N.MAX = rr$max[which.min(rr$r1)]
    N.MIN = rr$min[which.min(rr$r1)]
  }else{
    N.MAX = rr$max[which.max(rr$F1)]
    N.MIN = rr$min[which.max(rr$F1)]
  }

  results = lagged_model(df, N.MAX, N.MIN,
                         stepit=T,
                         vars = predictor_vars,
                         target = target_var,
                         binary_target = binary_target_var)
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
    plot(df_model[[target_var]], predicted_z)
    abline(b=1,a=0)

    plot(df_model[[binary_target_var]], predicted_hypoxia,ylim=c(0,1) )
    abline(b=1,a=0)

    plot(df_model[[target_var]],predicted_hypoxia)

    plot(rr$n,rr$r1 - rr$r1[which.min(rr$r1)],ylim=c(0,40))
    abline(h=2)
  }

  summary(model)
  summary(glm_model)

  ####
  # performance
  ####

  pdf(paste0('./fig/',name,prefix,'_performance.pdf'), width = 8, height = 4)
  par(mfrow=c(1,2),oma=0.1*c(1,1,1,1),mai=0.5*c(1.75,1.5,1,1), las=1)

  plot(rr$min,rr$reca,
       ylim=c(.30,1),
       xlab='forecast horizon, n',
       ylab='performance',
       type='n')
  points(rr$min,rr$F1,cex=1,col=alpha('gray',0.5),pch=20)
  #abline(h=0.5,lty=2)
  lines(supsmu(rr$min,rr$reca,span=0.01),col='tan',lwd=1)
  lines(supsmu(rr$min,rr$prec,span=0.01),col='gray',lwd=2)
  lines(supsmu(rr$min,rr$F1),col='black',lwd=3)
  abline(h=mean(ts_doy$hypoxia),lty=2)
  text(1,mean(ts_doy$hypoxia),'random prediction',cex=0.75,adj=c(0,1))
  abline(h=naive_F1,lty='22',col='#444444',lwd=0.5)
  text(1,naive_F1,'climatological forecast',col='#444444',cex=0.75,adj=c(0,1))
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
        col=hcl.colors(length(unique(round(100*rr$F1))),'Zi',rev=T),
        mgp=c(1.75,0.2,0),tck=0.02
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
  df_all = create_lagged_features(ts_all, N.MAX, N.MIN, vars = predictor_vars)
  predicted_z = predict(model ,newdata = df_all)
  predicted_hypoxia = predict(glm_model,type = "response",newdata = df_all)

  # creating confusion matrix accuracy, sensibility and specificity
  is.hypoxic = as.factor(predicted_hypoxia>0.5)
  levels(is.hypoxic) = c(levels(is.hypoxic), "TRUE", "FALSE")

  cm = confusionMatrix(as.factor(df_all[[binary_target_var]]),
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

  all_dates = NULL
  all_dates$t = seq.Date(as.Date('1995-01-01'),as.Date('2025-01-01'),by='day')
  ts_all = merge(ts_all,as.data.frame(all_dates),by='t',all.y=T)

  df_all$predicted_z = predicted_z
  df_all = merge(df_all,as.data.frame(all_dates),by='t',all.y=T)

  pdf(paste0('./fig/',name,prefix,'_comp.pdf'), width = 8, height = 3)
  par(mfrow=c(1,1),oma=0.1*c(1,1,1,1),mai=0.5*c(1,1.5,1,0.5), las=1)
  plot(ts_all$t,
      ts_all[[target_var]],
       ylim=c(0,16),
       xlim=as.Date(range(ts_all$t[!is.na(ts_all$z)])),
       type='n',
       xlab='',
       ylab=TeX('dissolved oxygen, mg $L^{-1}$')
  )
  polygon(as.Date(c(training_dates[1],training_dates[1],training_dates[2],training_dates[2])),
          c(0,16,16,0),
          border=NA,
          col=alpha('tan',0.15)
  )
  text(training_dates[1], 16.2,
       'training period',
       col='tan',
       cex=0.66,
       font=1,
       adj=c(-0.1,1.1)
  )
  text(as.Date('1995-01-01'), 16,
       letters[KK],
       col='black',
       font=2,
       adj=c(0,1)
  )
  lines(df_all$t,
        df_all$predicted_z,
        col='tan2',
        pch=20,
        lwd=1
  )
  lines(ts_all$t,
      ts_all[[target_var]],
        type='l',
        lwd=0.33
  )

  legend('bottomleft',
         legend=c('best ARM','observation'),
         fill = alpha(c('tan2','black'),0.99),
         bty='n',
         cex=0.75,
         horiz=T
  )

  abline(h=4,lty=2)
  title(main= paste0(name,', lags from ',N.MIN,' to ',N.MAX,' (n = ', N.MIN,', m = ',N.MAX-N.MIN,')'),adj=0,font.main=1,line=0.5)
  title(sub= paste0('Precision = ', prec, ', Recall = ', reca, ', F1-Score = ', F1),adj=0.95,font.main=1,line=-1,cex.sub=0.8)
  dev.off()

  ##########
  ## curves for hypoxia detail
  ##########

  year.comp = 2021
  y.filt = year(df_all$t)==year.comp

  pdf(paste0('./fig/',name,prefix,'_comp_detail.pdf'), width = 8, height = 3)
  par(mfrow=c(1,1),oma=0.1*c(1,1,1,1),mai=0.5*c(1,1.5,0.5,0.5), las=1)
  plot(ts_all$t,
      ts_all[[target_var]],
       ylim=c(0,16),
       xlim=as.Date(paste0(year.comp,c('-01-01','-12-31'))),
       type='n',
       xlab='',
       ylab=TeX('dissolved oxygen, mg $L^{-1}$')
  )
  text(min(ts_all$t[y.filt]), 16,
       letters[KK],
       col='black',
       font=2,
       adj=c(0,1)
  )
  lines(df_all$t[y.filt],
        df_all$predicted_z[y.filt],
        col='tan2',
        pch=20,
        lwd=1.5
  )
  lines(ts_all$t[y.filt],
      ts_all[[target_var]][y.filt],
        type='l',
        lwd=1
  )
  climatology = aggregate(ts_all,by=list(yday(ts_all$t)),function(...)mean(...,na.rm=T))
    lines(as.Date(paste0(year.comp,'-01-01'))+climatology$Group.1,
      climatology[[target_var]],
        col='steelblue',
        type='l',
        lwd=1
  )
  abline(h=4,lty=2)
  title(main= paste0(name," (",year.comp,")"),adj=0,font.main=1,line=0.5)

  legend('topright',
         legend=c('ARM','climatology','observation'),
         fill = alpha(c('tan2','steelblue','black'),0.75),
         bty='n',
         cex=0.75,
         horiz=T
  )


  df_all$Group.1 = yday(df_all$t)
  df_all = merge(df_all,subset(climatology,select = -c(t)),by='Group.1',all=T,suffixes = c('','.clima'))
  clima_target_col = paste0(target_var, ".clima")
  baseline_lag_col = paste0(target_var, "_lag", baseline_lag)
  if(!baseline_lag_col %in% names(df_all)) {
    df_all[[baseline_lag_col]] = NA
  }

  par(mfrow=c(1,2),oma=0.1*c(1,1,1,1),mai=0.5*c(1.5,1.5,0.5,0.5), las=1)
  
  err_model = df_all[[target_var]] - df_all$predicted_z
  err_clima = df_all[[target_var]] - df_all[[clima_target_col]]
  err_lag = df_all[[target_var]] - df_all[[baseline_lag_col]]

  the.breaks = seq(min(c(err_clima, err_model, err_lag),na.rm=T)-.5,
                   max(c(err_clima, err_model, err_lag),na.rm=T)+.5,
                   by=0.2
  )
  the.breaks = pretty(the.breaks,n=30)
  hh = hist(err_model,breaks=the.breaks,plot=F)
  plot(NA,xlim=range(hh$breaks),ylim=range(hh$counts,na.rm=T),
       xaxt='l',yaxt='l',
       main='',
       xlab='observed - predicted DO',
       ylab='',
  )
  hist(err_model,
       col=alpha('tan2',0.33),
       breaks=the.breaks,
       main='',
       xlab='observed - predicted DO',
       ylab='',
       border=NA,mgp=c(1.75,0.2,0),tck=0.02,add=T
  )
  mtext(side=2,line=2.5,'frequency',las=0,outer=F)
  hist(err_clima,
       col=alpha('steelblue',0.33),
       breaks=the.breaks,
       main='',
       xlab='',
       ylab='',
       xaxt='n',
       yaxt='n',
       border=NA,
       add=T
  )
  hist(err_lag,
       col=alpha('magenta2',0.33),
       breaks=the.breaks,
       main='',
       xlab='',
       ylab='',
       xaxt='n',
       yaxt='n',
       border=NA,
       add=T
  )
  title(main='model error',font.main=1,adj=0,line=0.5)

  rmsd.model = sqrt(sum(err_model^2,na.rm=T)/sum(!is.na(err_model))) %>% round(digits=2)
  rmsd.clima = sqrt(sum(err_clima^2,na.rm=T)/sum(!is.na(err_clima))) %>% round(digits=2)
  rmsd.lag1 = sqrt(sum(err_lag^2,na.rm=T)/sum(!is.na(err_lag))) %>% round(digits=2)

  legend('topleft',
         title = 'RMSD',
            legend=c(paste('ARM:',rmsd.model),paste('clima:',rmsd.clima),paste('lag', baseline_lag, ':',rmsd.lag1)),
         fill = alpha(c('tan2','steelblue','magenta2'),0.33),
         bty='n',
         cex=0.66
  )

        hypoxia_filt = as.logical(df_all[[binary_target_var]])
        the.breaks = seq(min(c(err_clima[hypoxia_filt], err_model[hypoxia_filt], err_lag[hypoxia_filt]),na.rm=T)-.5,
                max(c(err_clima[hypoxia_filt], err_model[hypoxia_filt], err_lag[hypoxia_filt]),na.rm=T)+.5,
                   by=0.2
  )
  the.breaks = pretty(the.breaks,n=30)
        hh = hist(err_model[hypoxia_filt],breaks=the.breaks,plot=F)
  plot(NA,xlim=range(hh$breaks),ylim=range(hh$counts,na.rm=T),
       xaxt='l',yaxt='l',
       main='',
       xlab='observed - predicted DO',
       ylab='',
  )
        hist(err_model[hypoxia_filt],
       col=alpha('tan2',0.33),
       breaks=the.breaks,
       add=T,
       border=NA,mgp=c(1.75,0.2,0),tck=0.02
  )
        hist(err_clima[hypoxia_filt],
       col=alpha('steelblue',0.33),
       breaks=the.breaks,
       main='',
       xlab='',
       ylab='',
       xaxt='n',
       yaxt='n',
       border=NA,
       add=T
  )
  hist(err_lag[hypoxia_filt],
       col=alpha('magenta2',0.33),
       breaks=the.breaks,
       main='',
       xlab='',
       ylab='',
       xaxt='n',
       yaxt='n',
       border=NA,
       add=T
  )
  title(main='model error (hypoxia)',font.main=1,adj=0,line=0.5)
  abline(v=mean(err_model[hypoxia_filt],na.rm = T),lty=2,col='tan2')
  abline(v=mean(err_clima[hypoxia_filt],na.rm = T),lty=2,col='steelblue')
  abline(v=mean(err_lag[hypoxia_filt],na.rm = T),lty=2,col='magenta2')
  text(mean(err_model[hypoxia_filt],na.rm = T),0.9*max(hh$counts),mean(err_model[hypoxia_filt],na.rm = T)%>%.formater(),adj=c(0,0.5),lty=2,col='tan2')
  text(mean(err_clima[hypoxia_filt],na.rm = T),0.6*max(hh$counts),mean(err_clima[hypoxia_filt],na.rm = T)%>%.formater(),adj=c(0,0.5),lty=2,col='steelblue')
  text(mean(err_lag[hypoxia_filt],na.rm = T),0.3*max(hh$counts),mean(err_lag[hypoxia_filt],na.rm = T)%>%.formater(),adj=c(0,0.5),lty=2,col='magenta2')
  
  rmsd.model = sqrt(sum((err_model[hypoxia_filt])^2,na.rm=T)/sum(!is.na(err_model[hypoxia_filt]))) %>% round(digits=2)
  rmsd.clima = sqrt(sum((err_clima[hypoxia_filt])^2,na.rm=T)/sum(!is.na(err_clima[hypoxia_filt]))) %>% round(digits=2)
  rmsd.lag1 = sqrt(sum((err_lag[hypoxia_filt])^2,na.rm=T)/sum(!is.na(err_lag[hypoxia_filt]))) %>% round(digits=2)

  legend('topleft',
         title = 'RMSD',
         legend=c(paste('ARM:',rmsd.model),paste('clima:',rmsd.clima),paste('lag', baseline_lag, ':',rmsd.lag1)),
         fill = alpha(c('tan2','steelblue','magenta2'),0.33),
         bty='n',
         cex=0.66
  )

  dev.off()

  ##########
  ## curves for hypoxia
  ##########

  pdf(paste0('./fig/',name,prefix,'_hypox.pdf'), width = 4, height = 4)
  par(mfrow=c(1,1),oma=0.1*c(1,1,1,1),mai=0.5*c(1.75,1.5,1,0), las=1)
  plot(0,
       xlab=TeX('dissolved oxygen, mg $L^{-1}$'),
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

  ALL.DAYS = 7

  #palette(alpha(hcl.colors(2*ALL.DAYS + span,col.scale,rev=F),0.15))
  palette(alpha(hcl.colors(ALL.DAYS+3,col.scale,rev=F),0.25))
  
  for(N.MIN in 1:ALL.DAYS){
    for(N.MAX in N.MIN:(N.MIN+span)){
      df_all = create_lagged_features(ts_all, N.MAX, N.MIN, vars = predictor_vars)
      results = lagged_model(df_all, N.MAX, N.MIN,
                 vars = predictor_vars,
                 target = target_var,
                 binary_target = binary_target_var)
      model = results[[1]]
      glm_model = results[[2]]
      df_model = results[[3]]

      # Predict z at time t + 1
      predicted_z = predict(model)
      predicted_hypoxia = predict(glm_model,type = "response")

      lines(supsmu(df_model[[target_var]],predicted_hypoxia,span=.001),col=N.MIN)
      #points(df_model$z,predicted_hypoxia,col=(N.MAX+N.MIN),pch=20)

    }
  }
  abline(v=4,h=0.5,lty=2)
  dev.off()

  ##########
  ## curves for oxygen
  ##########

  pdf(paste0('./fig/',name,prefix,'_oxy.pdf'), width = 4, height = 4)
  par(mfrow=c(1,1),oma=0.1*c(1,1,1,1),mai=0.5*c(1.75,1.5,1,0), las=1)
  plot(0,
       xlab=TeX('observed dissolved oxygen, mg $L^{-1}$'),
       ylab=TeX('modelled dissolved oxygen, mg $L^{-1}$'),
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

  ALL.DAYS = 7

  #palette(alpha(hcl.colors(2*ALL.DAYS + span,col.scale,rev=F) -> pc,0.15))
  palette(alpha(hcl.colors(ALL.DAYS+3,col.scale,rev=F)[1:ALL.DAYS] -> pc,0.25))
  
  for(N.MIN in 1:ALL.DAYS){
    for(N.MAX in N.MIN:(N.MIN+span)){
      if(N.MIN%%2 == 0 ) next
      df_all = create_lagged_features(ts_all, N.MAX, N.MIN, vars = predictor_vars)
      results = lagged_model(df_all, N.MAX, N.MIN,
                 vars = predictor_vars,
                 target = target_var,
                 binary_target = binary_target_var)
      model = results[[1]]
      glm_model = results[[2]]
      df_model = results[[3]]

      # Predict z at time t + 1
      predicted_z = predict(model)
      predicted_hypoxia = predict(glm_model,type = "response")

      lines(supsmu(df_model[[target_var]],predicted_z,span=.001),col=N.MIN)
      #points(df_model$z,predicted_z,col=N.MIN,pch=20)
    }
  }
  abline(v=4,h=4,lty=2)
  abline(a=0,b=1,lty=2)

  subplot(color.bar.h(pc,
                      1,ALL.DAYS,nticks = 3,
                      title='n',cex.axis=0.75),
          12,4,
          size=c(1.0,0.5)
  )
  dev.off()
}

}

if(!simplest.model.only){
  
  if(!test.wind){
    pdf('./fig/F1_comparison.pdf',width = 8,height = 4)
    par(mfrow=c(1,2),oma=0.1*c(1,1,1,1),mai=0.5*c(1.75,1.5,1,0), las=1)
    my.palette = c('gray','steelblue','green3','black','red')
    plot(rr$min,rr$F1,
         ylim=c(.30,1),
         xlab='forecast horizon, n',
         ylab='F1-Score',
         type='n'
    )
    abline(h=0.5,lty=2)
    text(7,1,'a',font=2,adj=c(1,1))
    title(main='performance-horizon trade-off',adj=0,font.main=1,line=0.5)
    for(MM in 1:4){
      rr_model = performance_metrics[[2]][[MM]]
      lines(supsmu(rr_model$min,rr_model$F1),col=my.palette[MM],lwd=1)
      points(rr_model$min,rr_model$F1,col=alpha(my.palette[MM],0.33),lwd=1,pch=20,cex=0.5)
    }
    legend('bottomleft',
           legend=c('Temp + Disc + Chla','Temp + Disc','Temp + Chla','Temp'),
           fill = my.palette,
           cex=0.75,
           bty='n'
    )
    
    plot(rr$F1,rr$r2,
         ylim=c(100,1000),
         xlim=c(0.55,1.0),
         xlab='F1-Score',
         ylab='AIC for hypoxia',
         log='',
         type='n'
    )
    abline(v=0.5,lty=2)
    text(0.55,100,'b',font=2,adj=c(0,0))
    title(main='complexity-performance trade-off',adj=0,font.main=1,line=0.5)
    for(MM in 1:4){
      rr_model = performance_metrics[[2]][[MM]]
      lines(supsmu(rr_model$F1,rr_model$r2),col=my.palette[MM],lwd=1)
      points(rr_model$F1,rr_model$r2,col=alpha(my.palette[MM],0.33),lwd=1,pch=20,cex=0.5)
    }
    polygon(c(0.91,0.96,0.96,0.91),c(150,150,190,190),border='#444444',lty=1,lwd=1)
    subplot({
      plot(rr$F1,rr$r2,
           ylim=c(150,190),
           xlim=c(0.91,0.96),
           cex.axis=.75,
           mgp=c(1,0,0),
           xlab='',
           ylab='',
           log='',
           type='n'
      )
      for(MM in 1:4){
        rr_model = performance_metrics[[2]][[MM]]
        rr_model = rr_model[rr_model$F1>0.9,]
        points(rr_model$F1,rr_model$r2,col=alpha(my.palette[MM],0.33),lwd=1,pch=19,cex=0.5)
        #if(MM%in%c(1,4))text(rr_model$F1,rr_model$r2,rr_model$max,adj=c(0.5,1.5),col=alpha(my.palette[MM],1),cex=0.75)
      }
    },
    1,1000,
    vadj=1,hadj=1,
    size=c(1,1)
    )
    
    dev.off()
    
    rr_ref = performance_metrics[[2]][[4]]  
    pdf('./fig/F1_comparison_matrix.pdf',width = 9,height = 3)
    par(mfrow=c(1,3),las=1)
    for(MM in 1:3){
      rr_model = performance_metrics[[2]][[MM]]
      image(unique(rr$min),
            1:span,
            t(matrix(round(100*(rr_model$F1-rr_ref$F1)), nrow = span)),
            xlab='forecast horizon, n',
            ylab='observation window, m',
            zlim = c(-1,1)*max(100*abs((rr_model$F1-rr_ref$F1))),
            col=colorRampPalette(c('steelblue','gray','orangered'))(100),
            mgp=c(1.75,0.2,0),tck=0.02
      )
      text(7,7,letters[MM],adj=c(0,0),font=2)
      title(main=c('Temp + Disc + Chla','Temp + Disc','Temp + Chla','Temp')[MM],adj=0,font.main=1,line=0.5)
      
      for(i in 1:7)for(j in 1:7)if(!(i==7 & j==7))text(i,j,paste0(t(matrix(((rr_model$F1-rr_ref$F1))%>%.formater('%.2f'), nrow = span))[i,j]),col='white')
    }
    mtext(side=3,'Model variables change of F1 in respect to only Temp models',outer=T,line=-1.5)
    dev.off()
  }
  
  if(test.wind){
    pdf('./fig/F1_wind_comparison.pdf',width = 6,height = 9)
    par(mfrow=c(3,2),oma=0.1*c(1,1,1,1),mai=0.5*c(1.75,1.5,1,0), las=1)
    my.palette = c('gray','steelblue','green3','black','red')
    for(KK in 1:3){
      plot(rr$min,rr$F1,
           ylim=c(.30,1),
           xlab='forecast horizon, n',
           ylab='F1-Score',
           type='n'
      )
      abline(h=0.5,lty=2)
      text(7,1,'a',font=2,adj=c(1,1))
      title(main='performance',adj=0,font.main=1,line=0.5)
      for(MM in 4:5){
        rr_model = performance_metrics[[KK]][[MM]]
        lines(supsmu(rr_model$min,rr_model$F1),col=my.palette[MM],lwd=1)
        points(rr_model$min,rr_model$F1,col=alpha(my.palette[MM],0.33),lwd=1,pch=20,cex=0.5)
      }
      legend('bottomleft',
             legend=c('Temp','Temp + Wind'),
             fill = my.palette[4:5],
             cex=0.75,
             bty='n'
      )
      
      plot(rr$F1,rr$r2,
           ylim=c(00,600),
           xlim=c(0.55,1.0),
           xlab='F1-Score',
           ylab='AIC for hypoxia',
           log='',
           type='n'
      )
      abline(v=0.5,lty=2)
      text(0.55,0,'b',font=2,adj=c(0,0))
      title(main='complexity-performance trade-off',adj=0,font.main=1,line=0.5)
      for(MM in 4:5){
        rr_model = performance_metrics[[KK]][[MM]]
        lines(supsmu(rr_model$F1,rr_model$r2),col=my.palette[MM],lwd=1)
        points(rr_model$F1,rr_model$r2,col=alpha(my.palette[MM],0.33),lwd=1,pch=20,cex=0.5)
      }
    }
  
    dev.off()
    
  }
}

