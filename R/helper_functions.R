#' @keywords internal
forecast_garch <- function(omega, alpha, beta, g, ret, steps.ahead) {
  omega / (1 - alpha - beta) + (alpha + beta)^(steps.ahead - 1) * (omega + alpha * ret^2 + beta * g - omega / (1 - alpha - beta))
}

#' @keywords internal
calculate_phi <- function(w1, w2, K) {
    weights <- sapply(c(1:K), FUN = function(j) (j / K)^(w1 - 1) * (1 - j / K)^(w2 - 1))
    weights <- weights/sum(weights)
    weights
}

#' @keywords internal
calculate_tau <- function(df, x, freq, w1, w2, theta, m, K) {
  phi.var <- calculate_phi(w1, w2, K)
  tau <- c(rep(NA, times = K),
           exp(sum_tau(m = m, theta = theta, phivar = phi.var, covariate = x, K = K)))
  result <- merge(df, cbind(unique(df[freq]), tau), by = freq)
  result
}

#' @keywords internal
llh <-function(df, x, y, freq, mu, omega, alpha, beta,
           m, theta, w1 = 1, w2 = 1, g0, K) {

    tau <- calculate_tau(df = df, x = x, freq = freq,
                            w1 = w1, w2 = w2, theta = theta, m = m, K = K)$tau
    ret <- y[which.min(is.na(tau)):length(y)]  # lags can't be used for likelihood
    tau <- tau[which.min(is.na(tau)):length(tau)]
    g <- calculate_g(omega = omega, alpha = alpha, beta = beta,
                     returns = ((ret - mu)/sqrt(tau)), g0 = g0)
    if (sum(g <= 0) > 0) {
      print(sum(g <= 0))
      stop("g_t seems to be negative for at least one point in time?")
    } else {
      1/2 * log(2 * pi) + 1/2 * log(g * tau) + 1/2 * (ret - mu)^2/(g * tau)
    }
  }
