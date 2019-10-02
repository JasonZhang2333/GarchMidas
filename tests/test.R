#.rs.restartR()
library(GarchMidas)
numLags <-24
numLags2 <-48
begin <- 1845
tail <- 3545
Forecast_RV = numeric(0)
for(t in 1:(tail-begin)){
  out <- fit_GarchMidas(rv[t:(begin+t-1),],'return','rv',numLags)
  tau<-out$tau[length(out$tau)]
  g<-out$g[length(out$g)]
  mu<-out$parameters["mu"]
  alpha<-out$parameters["alpha"]
  beta<-out$parameters["beta"]
  r<-rv[begin+t-1,"return"]
  Forecast_RV[t] <-tau*(1-alpha-beta+alpha*(r-mu)^2/tau+beta*g)
}

Forecast_FU = numeric(0)
for(t in 1:(tail-begin)){
  out <- fit_GarchMidas(fu[t:(begin+t-1),],'return','fu',numLags)
  tau<-out$tau[length(out$tau)]
  g<-out$g[length(out$g)]
  mu<-out$parameters["mu"]
  alpha<-out$parameters["alpha"]
  beta<-out$parameters["beta"]
  r<-fu[begin+t-1,"return"]
  Forecast_FU[t] <-tau*(1-alpha-beta+alpha*(r-mu)^2/tau+beta*g)
}

Forecast_MU = numeric(0)
for(t in 1:(tail-begin)){
  out <- fit_GarchMidas(mu[t:(begin+t-1),],'return','mu',numLags)
  tau<-out$tau[length(out$tau)]
  g<-out$g[length(out$g)]
  mu<-out$parameters["mu"]
  alpha<-out$parameters["alpha"]
  beta<-out$parameters["beta"]
  r<-mu[begin+t-1,"return"]
  Forecast_MU[t] <-tau*(1-alpha-beta+alpha*(r-mu)^2/tau+beta*g)
}

Forecast_EPU = numeric(0)
for(t in 1:(tail-begin)){
  out <- fit_GarchMidas(epu[t:(begin+t-1),],'return','epu',numLags2)
  tau<-out$tau[length(out$tau)]
  g<-out$g[length(out$g)]
  mu<-out$parameters["mu"]
  alpha<-out$parameters["alpha"]
  beta<-out$parameters["beta"]
  r<-epu[begin+t-1,"return"]
  Forecast_EPU[t] <-tau*(1-alpha-beta+alpha*(r-mu)^2/tau+beta*g)
}

Forecast_CNEPU = numeric(0)
for(t in 1:(tail-begin)){
  out <- fit_GarchMidas(cnepu[t:(begin+t-1),],'return','cnepu',numLags2)
  tau<-out$tau[length(out$tau)]
  g<-out$g[length(out$g)]
  mu<-out$parameters["mu"]
  alpha<-out$parameters["alpha"]
  beta<-out$parameters["beta"]
  r<-cnepu[begin+t-1,"return"]
  Forecast_CNEPU[t] <-tau*(1-alpha-beta+alpha*(r-mu)^2/tau+beta*g)
}