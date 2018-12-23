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

  df_plot <- x$df.fitted[which(!is.na(x$df.fitted$g)),]
  plot(x = df_plot[, 1], y = sqrt(252*df_plot$g*df_plot$tau),
       type = "l",
       xlab = colnames(x$df.fitted)[3], ylab = "vol",
       main = "sqrt(tau * g) and sqrt(tau) in red", sub = "")
  lines(x = df_plot[, 1],
        y = sqrt(252*df_plot$tau),
        col = "red")
}

#' @export
predict.GarchMidas <- function(x,date, ...) {
  if (class(x) != "GarchMidas") {
    stop("Obejct is not in class GarchMidas")
  }
  index <- which(x$df.fitted$date>=as.Date(date))[1]
  fitted <- x$df.fitted[(index-1):dim(x$df.fitted)[1],]
  alpha <- x$par["alpha"]
  beta <- x$par["beta"]

  result<-fitted$tau*(1-alpha-beta+beta*fitted$g)+alpha*(fitted[,2]-x$par["mu"])^2
  result[-length(result)]/10000
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
