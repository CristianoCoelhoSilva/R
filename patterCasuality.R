# Cross-validation with bootstrap
cv_result_boot <- pcCrossValidation(
  X, Y,
  E = 2, 
  tau = 2,
  metric = "euclidean",
  h = 1,
  weighted = FALSE,
  numberset = c(100, 200, 300),
  random = TRUE,
  bootstrap = 100
)
print(cv_result_boot, type = "positive")
plot(cv_result_boot, status = "positive")

