library(devtools)
library(roxygen2)

usethis::use_rcpp()
Rcpp::compileAttributes()

devtools::document()
devtools::load_all()
devtools::build()
install.packages("D:\\Code\\R\\GarchMidas_1.0.tar.gz", repos = NULL)
