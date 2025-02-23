pcmatrix <- pcMatrix(dataset, E = 3, tau = 1,
                     metric = "euclidean", h = 1,
                     weighted = TRUE)
effects <- pcEffect(pcmatrix)
print(effects)
plot(effects)
