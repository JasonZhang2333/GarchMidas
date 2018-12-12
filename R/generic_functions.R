#' @export
print.GarchMidas <- function(x, ...) {
  if (class(x) != "GarchMidas") {
      stop("Obejct is not in class GarchMidas")
  }
  print(x$estimation)
}

#' @importFrom graphics lines
#' @export
plot.GarchMidas <- function(x, ...) {
  if (class(x) != "GarchMidas") {
    stop("Obejct is not in class GarchMidas")
  }

  df_plot <- aggregate(x$df.fitted, by = list(x$df.fitted[, 3]), FUN = mean)
  plot(x = df_plot[, 1], y = sqrt(df_plot$g),
       type = "l",
       xlab = colnames(x$df.fitted)[3], ylab = "vol",
       main = "sqrt(tau * g) and sqrt(tau) in red", sub = "")
  lines(x = df_plot[, 1],
        y = sqrt(df_plot$tau),
        col = "red")
}

#' @export
predict.GarchMidas <- function(object, horizon = c(1:10), fcts.tau = NULL, return = NULL, cond.var = NULL, cond.tau = NULL, ...) {
  if (class(object) != "GarchMidas") {
    stop("Obejct is not in class GarchMidas")
  }

  if (is.null(cond.var) == TRUE) {
    cond.var <- tail(object$g, 1)
  }

  if (is.null(cond.tau) == TRUE) {
    cond.tau <- tail(object$tau, 1)
  }

  if (is.null(fcts.tau) == TRUE) {
    fcts.tau <- object$tau.forecast
  }

  if (is.null(return) == TRUE) {
    return <- tail(object$df.fitted$return, 1)
  }

  fcts.tau * as.numeric(sapply(horizon, forecast_garch,
                               omega = 1 - object$par["alpha"] - object$par["beta"],
                               alpha = object$par["alpha"],
                               beta = object$par["beta"],
                               gamma = 0,
                               ret = (return - object$par["mu"])/ sqrt(cond.tau),
                               g = cond.var))

}

#' This function plots the weighting scheme of an estimated GARCH-MIDAS model
#' @param x GarchMidas object obtained by fit_GarchMidas
#' @importFrom graphics plot
#' @export
plot_weighting_scheme <- function(x) {
  if (class(x) != "GarchMidas") {
    stop("Obejct is not in class GarchMidas")
  }
  plot(c(1:x$K), x$phi)
}

#' This function finds the best K of an estimated GARCH-MIDAS model
#' @param data data frame containing a column named date of type 'Date'.
#' @param y name of high frequency dependent variable in df.
#' @param x covariate employed in GarchMidas.
#' @param freq a string of the low frequency variable in the df.
#' @param K.seq an integer sequence specifying lag length K in the long-term component.
#' @export
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterEvalQ
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
caculate_llh <- function(data, y, x, freq = "month", K.seq = c(6:48)) {
  fun <- function(k){
    result <-fit_GarchMidas(data,y,x,k,freq)
    result$llh
  }
  clnum<-detectCores(logical = FALSE)
  cl<-makeCluster(getOption('cl.cores',clnum))
  clusterEvalQ(cl, library(GarchMidas))
  llhs<-parLapply(cl,K.seq,fun)
  stopCluster(cl)
  llhs
}
