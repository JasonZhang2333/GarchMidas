# .rs.restartR()
#library(GarchMidas)
# long_epu_result1<-fit_GarchMidas(long_epu,'return','logdiff_epu',36)
# long_epu_result1
# long_epu_result1$variance.ratio
# long_epu_result2<-fit_GarchMidas(long_epu,'return','epu',36)
# long_epu_result2
# long_epu_result2$variance.ratio

library(GarchMidas)
mu_result<-fit_GarchMidas(mu,'hundredfold_return','mu',24)
mu_result
mu_result$variance.ratio

fu_result<-fit_GarchMidas(fu,'hundredfold_return','fu',24)
fu_result
fu_result$variance.ratio

epu_result<-fit_GarchMidas(epu,'hundredfold_return','epu',24)
epu_result
epu_result$variance.ratio

result <- caculate_llh(fu,'hundredfold_return','fu','month', K.seq = c(12,18,24,30,36,42,48))
