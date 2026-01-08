# SPDX-FileCopyrightText: 2025 Helmholtz-Zentrum hereon GmbH
# SPDX-License-Identifier: CC-BY-4.0
# SPDX-FileContributor Ovidio Garcia-Oliva <ovidio.garcia@hereon.de>

library(matrixStats)

mask.far = function(M, N, off) {
  idx = abs(row(M) - col(M) + off) >= N
  idx[,1:N] = F 
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

predictions.DO = mask.far(predictions.DO,1*observation.window,-length(wgmn_data$t)+NN.max)
predictions.hypox = mask.far(predictions.hypox,1*observation.window,-length(wgmn_data$t)+NN.max)

wgmn_data$pred.DO = rowMeans(predictions.DO,na.rm = T)
wgmn_data$pred.hypox = rowMeans(predictions.hypox,na.rm = T)

sigma.level = 3 # amplification factor for 1-2-3-sigma levels following the 68-95-99.7 rule of the sample mean
wgmn_data$pred.DO.error = sigma.level*apply(predictions.DO, 1, function(...)sd(...,na.rm=T)*(1/(sum(!is.na(...))))**0.5)
wgmn_data$pred.hypox.error = sigma.level*apply(predictions.hypox, 1, function(...)sd(...,na.rm=T)*(1/(sum(!is.na(...))-1))**0.5)

wgmn_data$pred.DO.error[is.na(wgmn_data$pred.DO.error)] = 1.5*max(wgmn_data$pred.DO.error,na.rm = T) 
wgmn_data$pred.hypox.error[is.na(wgmn_data$pred.hypox.error)] = 1.5*max(wgmn_data$pred.hypox.error,na.rm = T)  

pdf('prediction.pdf',width=8,height = 4)
par(mfrow=c(1,2),las=1)

plot(wgmn_data$t,wgmn_data$z,
     xlim=c(max.date-14,max(wgmn_data$t)),
     ylim=range(predictions.DO[(length(wgmn_data$t)-2*NN.max):nrow(predictions.DO),],na.rm=T),
     #ylim=range(c(wgmn_data$z,wgmn_data$pred.DO+2),na.rm=T),
     xlab='',
     ylab='mg/L',
     pch=20,
     cex=0.5,
     type='o',lwd=1
     )
title(main='dissolved oxygen',adj=0)
abline(v=max.date+(0:4)*7,col='lightgray')
lines(wgmn_data$t,wgmn_data$pred.DO,col='red')

polygon(c(wgmn_data$t,rev(wgmn_data$t))[c(!is.na(wgmn_data$pred.DO.error),rev(!is.na(wgmn_data$pred.DO.error)))],
        c(wgmn_data$pred.DO-wgmn_data$pred.DO.error,rev(wgmn_data$pred.DO+wgmn_data$pred.DO.error))[c(!is.na(wgmn_data$pred.DO.error),rev(!is.na(wgmn_data$pred.DO.error)))],
        col=alpha('tomato',0.25),
        border=F
        )

lines(wgmn_data$t,apply(predictions.DO, 1, function(...)min(...,na.rm=T)),col='red',lty=3)
lines(wgmn_data$t,apply(predictions.DO, 1, function(...)max(...,na.rm=T)),col='red',lty=3)
#lines(supsmu(as.Date('2025-01-01')+yday(df$t)-1,df$z,span = 0.01/365),col='orange')

y.pos = range(predictions.DO[(length(wgmn_data$t)-2*NN.max):nrow(predictions.DO),],na.rm=T)[2]

points(wgmn_data$t,
       0*wgmn_data$pred.DO + y.pos,
       col=alpha(hcl.colors(10,'Zi',rev=T),0.995)[1+as.integer(pmin(10,pmax(0,10-10*(wgmn_data$pred.DO.error))))],
       cex=1.25,#3-2*(wgmn_data$pred.DO.error),
       type='p',
       pch=15
       )

if(T)for(i in 1:length(wgmn_data$t))if(i%%2==0){
  text(wgmn_data$t[i],
       y.pos,
       9-round(10*(wgmn_data$pred.DO.error)[i],digits = 0),
       las=2,
       cex=0.5
       )
}

plot(wgmn_data$t,100*wgmn_data$hypoxia,
     xlim=c(max.date-14,max(wgmn_data$t)),
     ylim=c(0.01,max(wgmn_data$pred.hypox,na.rm = T)),
     xlab='',
     log='y',
     ylab='%',
     type='l',lwd=2
)
title(main='hypoxia probability',adj=0)
abline(v=max.date+(0:4)*7,col='lightgray')
lines(wgmn_data$t,wgmn_data$pred.hypox,col='red')

polygon(c(wgmn_data$t,rev(wgmn_data$t))[c(!is.na(wgmn_data$pred.DO.error),rev(!is.na(wgmn_data$pred.DO.error)))],
        pmax(0.001, c(wgmn_data$pred.hypox-wgmn_data$pred.hypox.error,rev(wgmn_data$pred.hypox+wgmn_data$pred.hypox.error))[c(!is.na(wgmn_data$pred.DO.error),rev(!is.na(wgmn_data$pred.DO.error)))]),
        col=alpha('tomato',0.25),
        border=F
        )

lines(wgmn_data$t,apply(predictions.hypox, 1, function(...)max(...,na.rm=T)),col='red',lty=3)
lines(wgmn_data$t,apply(predictions.hypox, 1, function(...)min(...,na.rm=T)),col='red',lty=3)

#lines(supsmu(as.Date('2025-01-01')+yday(df$t)-1,as.numeric(df$hypoxia),span = 0.1/365),col='orange')

y.pos = max(wgmn_data$pred.hypox,na.rm = T)

points(wgmn_data$t,
       0*wgmn_data$pred.DO + y.pos,
       col=alpha(hcl.colors(10,'Zi',rev=T),0.995)[1+as.integer(pmin(10,pmax(0,10-10*(wgmn_data$pred.hypox.error))))],
       cex=1.25,#3-2*(wgmn_data$pred.DO.error),
       type='p',
       pch=15
)

if(T)for(i in 1:length(wgmn_data$t))if(i%%2==0){
  text(wgmn_data$t[i],
       y.pos,
       9-round(10*(wgmn_data$pred.hypox.error)[i],digits = 0),
       las=2,
       cex=0.5
  )
}


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


