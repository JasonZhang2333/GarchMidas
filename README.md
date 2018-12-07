[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
# GarchMidas

An R package for estimating GARCH-MIDAS models. The GARCH-MIDAS model decomposes the conditional variance of (daily) stock returns into a short- and long-term component, where the latter may depend on an exogenous covariate sampled at a lower frequency.

It's modified from [mfGARCH](https://github.com/onnokleen/mfGARCH).

## Installation

```r
# Install package via devtools
# install.packages("devtools")
library(devtools)
install_github("JasonZhang2333/GarchMidas")
```

## Example
```r
library(GarchMidas)
# epu
result <- fit_GarchMidas(data = epu, y = "return", x = "epu", low.freq = "month", K = 24)
print(result)
plot(result)
```