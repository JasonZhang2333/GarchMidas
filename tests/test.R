# .rs.restartR()
library(GarchMidas)
long_epu_result1<-fit_GarchMidas(long_epu,'return','logdiff_epu',36)
long_epu_result1
long_epu_result1$variance.ratio
long_epu_result2<-fit_GarchMidas(long_epu,'return','epu',36)
long_epu_result2
long_epu_result2$variance.ratio

mu_result1<-fit_GarchMidas(mu,'hundredfold_return','logdiff_mu',24)
mu_result1
mu_result1$variance.ratio
mu_result2<-fit_GarchMidas(mu,'hundredfold_return','mu',24)
mu_result2
mu_result2$variance.ratio

fu_result1<-fit_GarchMidas(fu,'hundredfold_return','logdiff_fu',24)
fu_result1
fu_result1$variance.ratio
fu_result2<-fit_GarchMidas(fu,'hundredfold_return','fu',24)
fu_result2
fu_result2$variance.ratio

epu_result1<-fit_GarchMidas(epu,'hundredfold_return','logdiff_epu',24)
epu_result1
epu_result1$variance.ratio
epu_result2<-fit_GarchMidas(epu,'hundredfold_return','epu',24)
epu_result2
epu_result2$variance.ratio