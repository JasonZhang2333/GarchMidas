#' Stock returns and uncertainty index.
#'
#' A dataset containing SSE (Shanghai Stock Exchange) Composite Index returns and the China Economic Policy Uncertainty Index
#'
#' @format A data frame with 3,789 rows and 7 variables:
#' \describe{
#'   \item{date}{date}
#'   \item{month}{a dummy for each year/month combination}
#'   \item{return}{daily SSE log returns}
#'   \item{hundredfold_return}{daily SSE log returns times 100}
#'   \item{rv}{5-minute realized variances}
#'   \item{epu}{Economic Policy Uncertainty Index divided by 100}
#'   \item{hundredfold_epu}{Economic Policy Uncertainty Index}
#' }
"epu"

#' Stock returns and uncertainty index.
#'
#' A dataset containing SSE (Shanghai Stock Exchange) Composite Index returns and the China Macroeconomic Uncertainty Index
#'
#' @format A data frame with 3,789 rows and 7 variables:
#' \describe{
#'   \item{date}{date}
#'   \item{return}{daily SSE log returns}
#'   \item{hundredfold_return}{daily SSE log returns times 100}
#'   \item{rv}{5-minute realized variances}
#'   \item{month}{a dummy for each year/month combination}
#'   \item{mu}{Macroeconomic Uncertainty Index}
#'   \item{hundredfold_mu}{Macroeconomic Uncertainty Index times 100}
#' }
"mu"

#' Stock returns and uncertainty index.
#'
#' A dataset containing SSE (Shanghai Stock Exchange) Composite Index returns and the China Financial Uncertainty Index
#'
#' @format A data frame with 3,789 rows and 7 variables:
#' \describe{
#'   \item{date}{date}
#'   \item{return}{daily SSE log returns}
#'   \item{hundredfold_return}{daily SSE log returns times 100}
#'   \item{rv}{5-minute realized variances}
#'   \item{month}{a dummy for each year/month combination}
#'   \item{fu}{Financial Uncertainty Index}
#'   \item{hundredfold_fu}{Financial Uncertainty Index times 100}
#' }
"fu"
