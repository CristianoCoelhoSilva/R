library(patterncausality)

optimalParams <- optimalParametersSearch(
  Emax = 3,
  tauMax = 3,
  metric = "euclidean",
  dataset = dataset,
  h = 1,
  weighted = FALSE
)
print(optimalParams)
