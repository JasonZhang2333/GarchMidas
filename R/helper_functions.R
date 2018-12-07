#' @keywords internal
forecast_garch <- function(omega, alpha, beta, gamma, g, ret, steps.ahead) {
  omega / (1 - alpha - gamma/2 - beta) + (alpha + beta + gamma/2)^(steps.ahead - 1) * (omega + (alpha + gamma/2 * as.numeric(ret < 0)) * ret^2 + beta * g - omega / (1 - alpha - gamma/2 - beta))
}

#' @keywords internal
calculate_phi <- function(w1, w2, K) {
    weights <- sapply(c(1:K),
                      FUN = function(j) (j / K)^(w1 - 1) * (1 - j / K)^(w2 - 1))
    weights <- weights/sum(weights)
    weights
}

#' @keywords internal
calculate_tau <- function(df, x, low.freq, w1, w2, theta, m, K) {
  phi.var <- calculate_phi(w1, w2, K)
  tau <- c(rep(NA, times = K),
           exp(sum_tau(m = m, theta = theta, phivar = phi.var, covariate = x, K = K)))
  result <- merge(df, cbind(unique(df[low.freq]), tau), by = low.freq)
  result
}

#' @keywords internal
llh <-function(df, x, y, low.freq, mu, omega, alpha, beta, gamma,
           m, theta, w1 = 1, w2 = 1, g_zero, K) {

    tau <- calculate_tau(df = df, x = x, low.freq = low.freq,
                            w1 = w1, w2 = w2, theta = theta, m = m, K = K)$tau
    ret <- y
    ret <- ret[which.min(is.na(tau)):length(ret)]  # lags can't be used for likelihood
    tau <- tau[which.min(is.na(tau)):length(tau)]
    g <- calculate_g(omega = omega, alpha = alpha, beta = beta, gamma = gamma,
                     returns = ((ret - mu)/sqrt(tau)), g0 = g_zero)
    if (sum(g <= 0) > 0) {
      stop("g_t seems to be negative for at least one point in time?")
      rep(NA, times = length(g))
    } else {
      1/2 * log(2 * pi) + 1/2 * log(g * tau) + 1/2 * (ret - mu)^2/(g * tau)
    }
  }
