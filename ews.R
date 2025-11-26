# SPDX-FileCopyrightText: 2025 Helmholtz-Zentrum hereon GmbH
# SPDX-License-Identifier: CC-BY-4.0
# SPDX-FileContributor Ovidio Garcia-Oliva <ovidio.garcia@hereon.de>

mask.far <- function(M, N, off) {
  idx = abs(row(M) - col(M) + off) >= N
  M[idx] = NA
  return(M)
}

wgmn_data = read.csv("./wgmn/wgmn_data.csv")

oxy_data = subset(wgmn_data, Station == 'Blankenese' &
                    Messgrösse == 'Sauerstoffkonzentration',
                  select=c(Measurement,Messwert))

tem_data = subset(wgmn_data, Station == 'Blankenese' &
                    Messgrösse == 'Wassertemperatur',
                  select=c(Measurement,Messwert))

wgmn_data = merge(oxy_data,tem_data,by='Measurement')

wgmn_data$t = as.Date(wgmn_data$Measurement, format='%d.%m.%Y %H:%M') 
wgmn_data$x = wgmn_data$Messwert.y
wgmn_data$z = wgmn_data$Messwert.x
wgmn_data$y = predict(lm(y~x*z*t,data=ts_all),newdata =  wgmn_data)

wgmn_data = aggregate(wgmn_data,by=list(wgmn_data$t),mean)

wgmn_data = subset(wgmn_data,select = c(t,x,y,z))
wgmn_data$hypoxia = wgmn_data$z<4

max.date = max(wgmn_data$t)

NN.max = 7*2

for(i in 1:NN.max)wgmn_data[nrow(wgmn_data) + 1,] = c(wgmn_data$t[nrow(wgmn_data)]+1 ,NA,NA,NA,NA)

predictions.DO = matrix(nrow=length(wgmn_data$t),ncol=NN.max)
predictions.hypox = matrix(nrow=length(wgmn_data$t),ncol=NN.max)

observation.window=5

for(i in 1:NN.max){
  N.MIN = i # forecast horizon
  N.MAX = observation.window+N.MIN
  
  results = lagged_model(df, N.MAX, N.MIN)
  model = results[[1]]
  glm_model = results[[2]]
  df_model = results[[3]]
  
  # Predict z at time t 
  predicted_z = predict(model,newdata =  create_lagged_features(wgmn_data,N.MAX,N.MIN))
  predicted_hypoxia = predict(glm_model,type = "response",newdata =  create_lagged_features(wgmn_data,N.MAX,N.MIN))
  
  predictions.DO[,i] = predicted_z
  predictions.hypox[,i] = 100*predicted_hypoxia
}



predictions.DO = mask.far(predictions.DO,2*observation.window,-length(wgmn_data$t)+NN.max)
predictions.hypox = mask.far(predictions.hypox,2*observation.window,-length(wgmn_data$t)+NN.max)



wgmn_data$pred.DO = rowMeans(predictions.DO,na.rm = T)
wgmn_data$pred.hypox = rowMeans(predictions.hypox,na.rm = T)


pdf('prediction.pdf',width=8,height = 4)
par(mfrow=c(1,2),las=1)

plot(wgmn_data$t,wgmn_data$z,
     xlim=c(max.date-14,max(wgmn_data$t)),
     ylim=range(c(wgmn_data$z,wgmn_data$pred.DO),na.rm=T),
     xlab='',
     ylab='mg/L',
     type='l',lwd=2
     )
title(main='dissolved oxygen',adj=0)
abline(v=max.date+(0:4)*7,col='lightgray')
lines(wgmn_data$t,wgmn_data$pred.DO,col='red')
lines(wgmn_data$t,apply(predictions.DO, 1, function(...)min(...,na.rm=T)),col='red',lty=3)
lines(wgmn_data$t,apply(predictions.DO, 1, function(...)max(...,na.rm=T)),col='red',lty=3)
lines(supsmu(as.Date('2025-01-01')+yday(df$t)-1,df$z,span = 0.1/365),col='orange')

plot(wgmn_data$t,100*wgmn_data$hypoxia,
     xlim=c(max.date-14,max(wgmn_data$t)),
     ylim=c(0,15),
     xlab='',
     ylab='%',
     type='l',lwd=2
)
title(main='hypoxia probability',adj=0)
abline(v=max.date+(0:4)*7,col='lightgray')
lines(wgmn_data$t,wgmn_data$pred.hypox,col='red')
lines(wgmn_data$t,apply(predictions.hypox, 1, function(...)max(...,na.rm=T)),col='red',lty=3)
lines(wgmn_data$t,apply(predictions.hypox, 1, function(...)min(...,na.rm=T)),col='red',lty=3)
lines(supsmu(as.Date('2025-01-01')+yday(df$t)-1,as.numeric(df$hypoxia),span = 0.1/365),col='orange')

## matrix plot
image(wgmn_data$t,1:NN.max,predictions.DO,
      xaxt='n',
      xlab='',
      ylab='forecast horizon',
      xlim=c(max.date-7,max(wgmn_data$t))
)
xlabs= max.date+(-1:4)*7
axis(side=1,at=xlabs,label=paste0(month(xlabs),'-',day(xlabs)),las=1)
abline(v=max.date+(0:4)*7)

image(wgmn_data$t,1:NN.max,predictions.hypox,
      xaxt='n',
      xlab='',
      ylab='forecast horizon',
      xlim=c(max.date-7,max(wgmn_data$t))
)
xlabs= max.date+(-1:4)*7
axis(side=1,at=xlabs,label=paste0(month(xlabs),'-',day(xlabs)),las=1)
abline(v=max.date+(0:4)*7)

dev.off()