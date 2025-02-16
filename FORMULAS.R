install.packages("urca")  # For ADF test with trend and intercept options
install.packages("tseries")  # Another option for ADF test

library(urca)
library(tseries)

set.seed(123)
MXBR0FN_INDEX <- ts(rnorm(100)) 

D_MXBR0FN_INDEX <- diff(MXBR0FN_INDEX)

adf_test <- ur.df(D_MXBR0FN_INDEX, type = "trend", lags = 0, selectlags = "BIC")

summary(adf_test)