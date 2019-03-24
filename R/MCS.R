setClass("SSM", representation(Info = "list", Bootstrap = "list",
                               show = "matrix"))
setMethod("show", "SSM", function(object) {
  cat(paste("\n------------------------------------------"))
  cat(paste("\n-          Superior Set of Models        -"))
  cat(paste("\n------------------------------------------\n"))
  show(object@show)
  cat(paste("\nDetails"))
  cat(paste("\n------------------------------------------\n"))
  cat(paste("\nNumber of eliminated models\t:\t"))
  cat(object@Info$n_elim)
  cat(paste("\nStatistic\t:\t"))
  cat(object@Info$statistic)
  cat(paste("\nElapsed Time\t:\t"))
  print(object@Info$elapsed.time)
})
#' This function finds the Model Confidence Set
#' @param Loss data frame of loss function
#' @param alpha confidence level
#' @param B bootstrap times
#' @param statistic "Tmax" or "TR"
#' @export
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterEvalQ
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
MCS<-function(Loss, alpha = 0.1, B = 5000, statistic = "Tmax", verbose = TRUE) {
  time.start = Sys.time()
  Loss = as.matrix(Loss)
  M_start = ncol(Loss)
  N <- nrow(Loss)
  clnum<-detectCores(logical = FALSE)
  cl<-makeCluster(getOption('cl.cores',clnum))
  clusterEvalQ(cl, library(GarchMidas))
  repeat{
    M <- ncol(Loss)
    model.names <- colnames(Loss)
    col.names <- do.call(c, lapply(1:M, function(x){
      paste(model.names[x], model.names[-x], sep = '.')
    }))
    d <- do.call(cbind, lapply(1:M, function(x){
      Loss[,x] - Loss[,-x]
    }))
    colnames(d) <- col.names
    d_ij_mean <- colMeans(d)
    d_i_mean <- sapply(1:M, function(x){
      mean(d_ij_mean[paste(model.names[x], model.names[-x], sep = '.')])
    })
    names(d_i_mean) <- model.names
    foo <- expand.grid(1:M, 1:M)[, 2:1]
    foo <- foo[foo[, 1] != foo[, 2],]
    index <- col.names[foo[, 1] < foo[, 2]]
    d_cut = as.list(as.data.frame(d[, index]))
    k = max(na.omit(as.numeric(parSapply(cl, d_cut, function(x) {
      try(ar(x)$order, silent = TRUE)
    }))))
    V <- ceiling(N/k)
    indexs <- parLapply(cl, 1:B, boot.block, v = V, n = N, k = k)
    d <- as.data.frame(d)
    d_b_ij_mean <- parLapply(cl, indexs, function(x, d){
      colMeans(d[x,,drop=FALSE])
    }, d = d)
    d_b_i_mean <- do.call(rbind, parLapply(cl, d_b_ij_mean, function(x, M, model.names) {
      sapply(1:M, function(l) mean(x[paste(model.names[l],
                                           model.names[-l], sep = ".")]))
    }, M = M, model.names = model.names))
    d_b_i_var <- colMeans(t(apply(d_b_i_mean, 1, function(x) (x - d_i_mean)^2)))
    names(d_b_i_var) <- model.names
    d_b_ij_mean <- do.call(rbind, d_b_ij_mean)
    d_b_ij_var <- matrix(colMeans(t(apply(d_b_ij_mean, 1, function(x) (x - d_ij_mean)^2))), nrow = 1)
    colnames(d_b_ij_var) <- names(d_ij_mean)
    TR <- max(abs(d_ij_mean)/(d_b_ij_var^0.5))
    TM <- max(d_i_mean/(d_b_i_var^0.5))
    TR_b <- sapply(1:B, function(x){
      max(abs(d_b_ij_mean[x,] - d_ij_mean)/(d_b_ij_var^0.5))
    })
    TM_b <- sapply(1:B, function(x){
      max((d_b_i_mean[x,] - d_i_mean)/(d_b_i_var^0.5))
    })
    Pr <- length(which(TR < TR_b))/B
    Pm <- length(which(TM < TM_b))/B
    
    #elimination rule
    V_i_M <- d_i_mean/(d_b_i_var^0.5)
    V_i_M_order <- order(V_i_M, decreasing = TRUE)
    TR_all <- d_ij_mean/(d_b_ij_var^0.5)
    V_i_R <- sapply(1:M, function(x) {
      max(TR_all[1, paste(model.names[x], model.names[-x], sep = ".")])
    })
    names(V_i_R) <- model.names
    V_i_R_order <- order(V_i_R, decreasing = TRUE)
    
    Pm_H0 <- NULL
    for(i in 1:M){
      if(i == 1){
        model_temp <- model.names
      } else {
        model_temp <- model.names[-V_i_M_order[1:(i-1)]]
      }
      TM_temp <- max(d_i_mean[model_temp]/(d_b_i_var[model_temp]^0.5))
      Pm_H0 <- c(Pm_H0, length(which(TM_temp < TM_b))/B)
    }
    mcs_Pm <- sapply(1:M, function(x) max(Pm_H0[1:x]))
    
    Pr_H0 <- NULL
    combine.names <- names(d_ij_mean)
    for(i in 1:M){
      if(i == 1){
        model_temp <- combine.names
      } else {
        remove <- do.call(c, lapply(1:(i - 1), function(x) {
          which(gsub(model.names[V_i_R_order[x]], "", combine.names) != combine.names)
        }))
        model_temp <- combine.names[-remove]
      }
      if(i < M) {
        TR_temp <- max(abs(d_ij_mean[model_temp])/(d_b_ij_var[1,model_temp]^0.5))
        Pr_H0 <- c(Pr_H0, length(which(TR_temp < TR_b))/B)
      } else {
        Pr_H0 <- c(Pr_H0, 1)
      }
    }
    mcs_Pr <- sapply(1:M, function(x) max(Pr_H0[1:x]))
    matrix_result <- matrix(NA, M, 7, dimnames = list(model.names, c('Rank_M','V_M','MCS_M','Rank_R','V_R','MCS_R','Loss')))
    matrix_result[,'V_M'] <- V_i_M
    matrix_result[names(sort(V_i_M)),'Rank_M'] <- 1:M
    matrix_result[model.names[V_i_M_order], "MCS_M"] = mcs_Pm
    matrix_result[, "V_R"] = V_i_R
    matrix_result[names(sort(V_i_R)), "Rank_R"] = 1:M
    matrix_result[model.names[V_i_R_order], "MCS_R"] = mcs_Pr
    matrix_result[, "Loss"] = colMeans(Loss)
    if (statistic == "Tmax") {
      p2test = Pm
    }
    if (statistic == "TR") {
      p2test = Pr
    }
    if (p2test > alpha | all(d_b_ij_var == 0)) {
      if (verbose) {
        cat(paste("\n###########################################################################################################################\n"))
        cat(paste("Superior Set Model created\t:\n"))
        show(matrix_result)
        cat(paste("p-value\t:\n"))
        print(p2test)
        cat(paste("\n###########################################################################################################################"))
      }
      break
    } else {
      if (statistic == "Tmax")
        eliminate = which(V_i_M == max(V_i_M))
      if (statistic == "TR")
        eliminate = which(V_i_R == max(V_i_R))
      
      if (verbose)
        cat(paste("\nModel", model.names[eliminate],
                  "eliminated", Sys.time()))
      Loss = as.matrix(Loss[, -eliminate])
      colnames(Loss) = model.names[-eliminate]
      if (ncol(Loss) == 1) {
        if (verbose) {
          cat(paste("\n###########################################################################################################################\n"))
          cat(paste("Superior Set Model created\t:\n"))
          matrix_result = matrix(matrix_result[-eliminate, ], nrow = 1, dimnames = list(colnames(Loss),colnames(matrix_result)))
          show(matrix_result)
          cat(paste("p-value\t:\n"))
          show(p2test)
          cat(paste("\n###########################################################################################################################"))
        }
        break
      }
    }
  }
  stopCluster(cl)
  elapsed.time = Sys.time() - time.start
  n_elim = M_start - nrow(matrix_result)
  result = new("SSM", show = matrix_result, Info = list(model.names = rownames(matrix_result),
                                                        elapsed.time = elapsed.time, statistic = statistic, n_elim = n_elim,
                                                        mcs_pvalue = p2test, alpha = alpha, B = B, k = k), Bootstrap = list(TR = list(Stat = TR,
                                                                                                                                      BootDist = TR_b), Tmax = list(Stat = TM, BootDist = TM_b)))
  return(result)
}