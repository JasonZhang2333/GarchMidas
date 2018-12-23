#.rs.restartR()
library(GarchMidas)
rv<- mu[which(mu$date>='2009-01-01'),'rv']
mu_result<-fit_GarchMidas(mu,'hundredfold_return','mu',48)
fu_result<-fit_GarchMidas(fu,'hundredfold_return','fu',48)
epu_result<-fit_GarchMidas(epu,'hundredfold_return','epu',48)
pre_mu <- predict(mu_result,'2009-01-01')
pre_fu <- predict(fu_result,'2009-01-01')
pre_epu <- predict(epu_result,'2009-01-01')
error_mu <- rv-pre_mu
error_fu <- rv-pre_fu
error_epu <- rv-pre_epu
mse_mu <- mean((rv-pre_mu)^2,na.rm = TRUE)
mse_fu <- mean((rv-pre_fu)^2,na.rm = TRUE)
mse_epu <- mean((rv-pre_epu)^2,na.rm = TRUE)
pdf("prediction_error.pdf",width = 6,height = 4)
plot(rv)
plot(pre_mu)
plot(pre_fu)
plot(pre_epu)
plot(error_mu)
plot(error_fu)
plot(error_epu)
dev.off()