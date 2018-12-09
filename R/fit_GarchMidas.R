#' This function estimates a multiplicative mixed-frequency GARCH model. For the sake of numerical stability, it is best to multiply log returns by 100.
#' @param data data frame containing a column named date of type 'Date'.
#' @param y name of high frequency dependent variable in df.
#' @param x covariate employed in GarchMidas.
#' @param K an integer specifying lag length K in the long-term component.
#' @param low.freq a string of the low frequency variable in the df.
#' @param var.ratio.freq specify a frequency column on which the variance ratio should be calculated.
#' @keywords fit_GarchMidas
#' @export
#' @importFrom pracma jacobian
#' @importFrom stats nlminb
#' @importFrom pracma hessian
#' @importFrom stats constrOptim
#' @importFrom stats na.exclude
#' @importFrom stats optim
#' @importFrom stats pnorm
#' @importFrom stats var
#' @importFrom stats aggregate
#' @importFrom utils tail
#' @examples
#' \dontrun{
#' fit_GarchMidas(data = mu, y = "return", x = "mu", low.freq = "month", K = 24)
#' }

fit_GarchMidas <- function(data, y, x, K, low.freq = "month", var.ratio.freq = NULL) {
  # Order by high frequency variable
  data <- data[order(data$date), ]
  # We store date in new variable because computation on integerized date seemed to be faster
  date_backup <- data[["date"]]
  data["date"] <- as.numeric(unlist(data["date"]))
  if (is.null(var.ratio.freq) == TRUE) {
    var.ratio.freq <- low.freq
  }

  low_freq_backup <- data[, low.freq]
  if (x != "date") {
    df_llh <- data[, c(y, x, low.freq)]
    df_llh[, low.freq] <- as.integer(unlist(df_llh[ , low.freq]))
  }
  g_zero <- var(unlist(data[[y]]))
  ret <- data[[y]]

  # Parameter estimation
  covariate <- unlist(unique(data[c(low.freq, x)])[x])
  lf <- function(p) {
    llh(df = df_llh,
           y = ret,
           x = covariate,
           low.freq = low.freq,
           mu = p["mu"],
           omega = 1 - p["alpha"] - p["beta"],
           alpha  = p["alpha"],
           beta = p["beta"],
           gamma = 0,
           m = p["m"],
           theta = p["theta"],
           w1 = p["w1"],
           w2 = p["w2"],
           g_zero = g_zero,
           K = K)
  }
  par.start <- c(mu = 0, alpha = 0.02, beta = 0.85, m = 0, theta = 0, w1 = 1.00000001, w2 = 3)
  ui.opt <- rbind(c(0, -1, -1, 0, 0, 0, 0),
                  c(0,  0,  0, 0, 0, 1, 0),
                  c(0,  0,  0, 0, 0, 0, 1),
                  c(0,  1,  0, 0, 0, 0, 0),
                  c(0,  0,  1, 0, 0, 0, 0))
  ci.opt <- c(-0.99999999, 1, 1, 0, 0)
  p.e.nlminb <- constrOptim(theta = par.start, f = function(theta) { sum(lf(theta)) },
                            grad = NULL, ui = ui.opt, ci = ci.opt, hessian = FALSE)
  par <- p.e.nlminb$par
  tau <- calculate_tau(df = data, x = covariate, low.freq = low.freq,
                          w1 = par["w1"], w2 = par["w2"],
                          theta = par["theta"],
                          m = par["m"], K = K)$tau
  tau_forecast <-
    exp(sum_tau_fcts(m = par["m"],
                     i = K + 1,
                     theta = par["theta"],
                     phivar = calculate_phi(w1 = par["w1"], w2 = par["w2"], K = K),
                     covariate = c(tail(unlist(unique(data[c(x, low.freq)])[x]), K), NA),
                     K = K))
  returns <- unlist(data[y])
  g <- c(rep(NA, times = sum(is.na((returns - par["mu"])/sqrt(tau)))),
         calculate_g(omega = 1 - par["alpha"] - par["beta"],
                     alpha = par["alpha"],
                     beta = par["beta"],
                     gamma = 0,
                     as.numeric(na.exclude((returns - par["mu"])/sqrt(tau))),
                     g0 = g_zero))

  if ((var.ratio.freq %in% c("date", low.freq)) == FALSE) {
    df.fitted <- cbind(data[c("date", y, low.freq, x, var.ratio.freq)], g = g, tau = tau)
  } else {
    df.fitted <- cbind(data[c("date", y, low.freq, x)], g = g, tau = tau)
  }

  df.fitted$residuals <- unlist((df.fitted[y] - par["mu"]) / sqrt(df.fitted$g * df.fitted$tau))

  df.fitted$date <- as.Date(date_backup)
  
  # Standard errors --------------------------------------------------------------------------------
  inv_hessian <- try({solve(-hessian(function (theta) {sum(lf(theta))},par,h=1e-6))})
  if (class(inv_hessian) == "try-error") {
    stop("Inverting the Hessian matrix failed. Possible workaround: Multiply returns by 100.")
  }
  rob.std.err <- sqrt(diag(inv_hessian %*% crossprod(jacobian(lf, par)) %*% inv_hessian))

  # Output -----------------------------------------------------------------------------------------
  output <-
    list(par = par,
         std.err = rob.std.err,
         estimation = data.frame(estimate = round(par,3),
                                 rob.std.err = round(rob.std.err,3),
                                 p.value = round(2 * (1 - pnorm(unlist(abs(par/rob.std.err)))),3)),
         tau = tau,
         g = g,
         df.fitted = df.fitted,
         K = K,
         llh = -p.e.nlminb$value,
         bic = log(sum(!is.na(tau))) * length(par) - 2 * (-p.e.nlminb$value),
         optim = p.e.nlminb)

  # Additional output -------------------------------------
  output$variance.ratio <- 100 *
                   var(log(aggregate(df.fitted$tau, by = df.fitted[var.ratio.freq],
                                     FUN = mean)[,2]),
                       na.rm = TRUE) /
                   var(log(aggregate(df.fitted$tau * df.fitted$g, by = df.fitted[var.ratio.freq],
                                     FUN = mean)[,2]),
                       na.rm = TRUE)
  output$tau.forecast <- tau_forecast
  output$est.weighting <- calculate_phi(w1 = par["w1"], w2 = par["w2"], K = K)
  class(output) <- "GarchMidas"
  output
}
