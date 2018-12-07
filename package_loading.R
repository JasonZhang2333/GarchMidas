library(devtools)
library(roxygen2)

usethis::use_rcpp()
Rcpp::compileAttributes()

devtools::document()
devtools::load_all()


