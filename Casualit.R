data(climate_indices)

X <- climate_indices$PNA
Y <- climate_indices$NAO
numberset <- c(100,200,300,400,500)
pc_cv <- pcCrossValidation(X,Y,3,2,"manhattan",1,TRUE,numberset = c(100,200,300,400,500))
print(pc_cv)

plot(pc_cv)

data(climate_indices)
X <- climate_indices$AO
Y <- climate_indices$AAO
pc_result <- pcLightweight(X, Y, E = 3, tau = 2, metric = "euclidean", h = 1, weighted = TRUE)
plot_total(pc_result)
