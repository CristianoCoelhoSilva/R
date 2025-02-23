# Basic cross-validation
cv_result <- pcCrossValidation(
  X, Y,
  E = 3, tau = 1,
  metric = "euclidean",
  h = 1,
  weighted = FALSE,
  random = TRUE,
  numberset = c(100, 200, 300),
  bootstrap = 100
)

print(cv_result, type = "positive")
plot(cv_result, status = "positive")

