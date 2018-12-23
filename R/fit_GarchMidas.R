#' This function estimates a multiplicative mixed-frequency GARCH model. For the sake of numerical stability, it is best to multiply log returns by 100.
#' @param data data frame containing a column named date of type 'Date'.
#' @param y name of high frequency dependent variable in df.
#' @param x covariate employed in GarchMidas.
#' @param K an integer specifying lag length K in the long-term component.
#' @param freq a string of the low frequency variable in the df.
#' @keywords fit_GarchMidas
#' @export
#' @importFrom pracma jacobian
#' @importFrom stats nlminb
#' @importFrom pracma hessian
#' @importFrom pracma zeros
#' @importFrom stats constrOptim
#' @importFrom stats na.exclude
#' @importFrom stats optim
#' @importFrom stats pnorm
#' @importFrom stats var
#' @importFrom stats aggregate
#' @importFrom utils tail
#' @examples
#' \dontrun{
#' fit_GarchMidas(data = mu, y = "return", x = "epu", K = 24, freq = "month")
#' }

fit_GarchMidas <- function(data, y, x, K, freq = "month") {
  data <- data[order(data$date), ]
  date_backup <- data[["date"]]
  # We store date in new variable because computation on integerized date seemed to be faster
  data["date"] <- as.numeric(unlist(data["date"]))
  df.llh <- data[, c(y, x, freq)]
  df.llh[, freq] <- as.integer(unlist(df.llh[ , freq]))
  g0 <- var(unlist(data[[y]]))
  covariate <- unlist(unique(data[c(freq, x)])[x])
  
  # Parameter estimation ----------------------------------------------------------------------------
  lf <- function(p) {
    llh(df = df.llh, y = data[[y]], x = covariate, freq = freq, mu = p["mu"], 
        omega = 1 - p["alpha"] - p["beta"], alpha  = p["alpha"], beta = p["beta"], 
        m = p["m"], theta = p["theta"], w1 = p["w1"], w2 = p["w2"], g0 = g0, K = K)
  }
  par.start <- c(mu = 0, alpha = 0.02, beta = 0.85, m = 0, theta = 0, w1 = 1.00000001, w2 = 3)
  ui.opt <- rbind(c(0, -1, -1, 0, 0, 0, 0),
                  c(0,  0,  0, 0, 0, 1, 0),
                  c(0,  0,  0, 0, 0, 0, 1),
                  c(0,  1,  0, 0, 0, 0, 0),
                  c(0,  0,  1, 0, 0, 0, 0))
  ci.opt <- c(-0.99999999, 1, 1, 0, 0)
  optim.par <- constrOptim(theta = par.start, f = function(theta) { sum(lf(theta)) },
                            grad = NULL, ui = ui.opt, ci = ci.opt, hessian = FALSE)
  par <- optim.par$par
  tau <- calculate_tau(df = data, x = covariate, freq = freq,
                          w1 = par["w1"], w2 = par["w2"],
                          theta = par["theta"],
                          m = par["m"], K = K)$tau
  returns <- unlist(data[y])
  g <- c(rep(NA, times = sum(is.na((returns - par["mu"])/sqrt(tau)))),
         calculate_g(omega = 1 - par["alpha"] - par["beta"],
                     alpha = par["alpha"],
                     beta = par["beta"],
                     as.numeric(na.exclude((returns - par["mu"])/sqrt(tau))),
                     g0 = g0))
  df.fitted <- cbind(data[c("date", y, freq, x)], g = g, tau = tau)
  df.fitted$residuals <- unlist((df.fitted[y] - par["mu"]) / sqrt(df.fitted$g * df.fitted$tau))
  df.fitted$date <- as.Date(date_backup)
  
  # Standard errors --------------------------------------------------------------------------------
  inv_hessian <- try({solve(-hessian(function (theta) {sum(lf(theta))},par,h=1e-6))})
  if (class(inv_hessian) == "try-error") {
    warning("Inverting the Hessian matrix failed. Possible workaround: Multiply returns by 100.")
    inv_hessian<-zeros(length(par))
  }
  rob.std.err <- sqrt(diag(inv_hessian %*% crossprod(jacobian(lf, par)) %*% inv_hessian))
  # Output -----------------------------------------------------------------------------------------
  output <-
    list(parameters = par,
         std.err = rob.std.err,
         residuals=df.fitted$residuals,
         estimation = data.frame(estimate = round(par,3),
                                 std.err = round(rob.std.err,3),
                                 p.value = round(2 * (1 - pnorm(unlist(abs(par/rob.std.err)))),3)),
         phi=calculate_phi(w1 = par["w1"], w2 = par["w2"], K = K),
         tau = tau,
         g = g,
         df.fitted = df.fitted,
         K = K,
         llh = -optim.par$value,
         bic = log(sum(!is.na(tau))) * length(par) - 2 * (-optim.par$value),
         optim = optim.par)
  output$variance.ratio <- 100 *
                   var(log(aggregate(df.fitted$tau, by = df.fitted[freq],
                                     FUN = mean)[,2]),
                       na.rm = TRUE) /
                   var(log(aggregate(df.fitted$tau * df.fitted$g, by = df.fitted[freq],
                                     FUN = mean)[,2]),
                       na.rm = TRUE)
  class(output) <- "GarchMidas"
  output
}
