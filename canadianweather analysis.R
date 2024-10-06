library(devtools)
load_all("C:/Users/Evidya/Downloads/penFoFPLS-master/penFoFPLS-master") # à modifier selon la localisation de votre dossier penFo
library(fda)
library(ggplot2)
library(caret)

data("CanadianWeather")
# data("daily")
# summary(CanadianWeather)

# visualisation des données pour voir la répartition des stations métorologiques à l'aide de leurs longitudes et latitudes

with(CanadianWeather, plot(-coordinates[, 2], coordinates[, 1], type='n',
                           xlab="West Longitude", ylab="North Latitude",
                           axes=FALSE) )
Wlon <- pretty(CanadianWeather$coordinates[, 2])
axis(1, -Wlon, Wlon)
axis(2)


rgns <- 1:4
names(rgns) <- c('Arctic', 'Atlantic', 'Continental', 'Pacific')
Rgns <- rgns[CanadianWeather$region]
with(CanadianWeather, points(-coordinates[, 2], coordinates[, 1],
                             col=Rgns, pch=Rgns) )
legend('topright', legend=names(rgns), col=rgns, pch=rgns)


op <- par(mar=c(5, 4, 4, 5)+.1)

# vosualisation de la moyenne de la température par mois sur les stations Pr. Rupert, Montréal, Edmond et Resolute
# Plot
stations <- c("Pr. Rupert", "Montreal", "Edmonton", "Resolute")
matplot(day.5, CanadianWeather$dailyAv[, stations, "Temperature.C"],
        type="l", axes=FALSE, xlab="", ylab="Mean Temperature (deg C)")
axis(2, las=1)
# Label the horizontal axis with the month names
axis(1, monthBegin.5, labels=FALSE)
axis(1, monthEnd.5, labels=FALSE)
axis(1, monthMid, monthLetters, tick=FALSE)
# Add the monthly averages
matpoints(monthMid, CanadianWeather$monthlyTemp[, stations])
# Add the names of the weather stations
mtext(stations, side=4,
      at=CanadianWeather$dailyAv[365, stations, "Temperature.C"],
      las=1)

par(op)

# Visualisation de la température quotidienne moyenne pour une station spécifique, ici la station 1 qui correspond à St. Johns
station_idx <- 1
plot(1:365, CanadianWeather$dailyAv[, station_idx, "Temperature.C"],
     type = 'l', col = 'blue', lwd = 2,
     xlab = 'Jour de l\'année', ylab = 'Température (°C)',
     main = paste("Température Quotidienne Moyenne -", CanadianWeather$place[station_idx]))


# analyse avec fda


daybasis65 <- create.fourier.basis(rangeval=c(0, 365), nbasis=65)
daytempfd <- with(CanadianWeather, smooth.basis(day.5,
                                                dailyAv[,1:35,"Temperature.C"],
                                                daybasis65, fdnames=list("Day", "Station", "Deg C"))$fd )
plot(daytempfd, axes=FALSE)

axisIntervals(1)


# preparation des données de test et de'entrainement
# stations_with_data <- colnames(CanadianWeather$dailyAv[, , "Temperature.C"])

# Sélectionnez les variables Precipitation.mm et Temperature.C pour ces stations
X <- CanadianWeather$dailyAv[, , "Precipitation.mm"]
Y <- CanadianWeather$dailyAv[, , "Temperature.C"]

set.seed(123)
indices_train <- sample(1:nrow(Y), 0.8 * nrow(Y))  # 80% pour l'entraînement
indices_test <- setdiff(1:nrow(Y), indices_train)   # 20% pour les tests

# ensembles d'entraînement
X_train <- X[indices_train, ]
Y_train <- Y[indices_train, ]

# ensembles de test
X_test <- X[indices_test, ]
Y_test <- Y[indices_test, ]


# 
# fdobj_temp <- fd(X_train)
# 
# # plot(fdobj_temp, main = "Modèle Ajusté sur l'Ensemble d'Entraînement")
# 
# 
# # la fonction predict pour faire des prédictions
# predicted_fda <- predict.fd(fdobj_temp, newdata = X_test)
# 
# # Visualisation les prédictions
# matplot(Y_test , type = 'l', col = 'blue', 
#         xlab = 'Jour de l\'année', ylab = 'Température (°C)',
#         main = 'Températures réelles vs prédictes par penFoFPLS')
# 
# # Ajouter les données réelles de test
# matlines(predicted_fda, type = 'l', col = 'red')
# legend('topright', legend = c('Réelles', 'Prédictions'), col = c('blue', 'red'), lty = 1)
# 
# 
# epsilon <- 1e-10
# mape <- mean(abs((Y_test - predicted_fda) / (Y_test + epsilon))) * 100


# ===== test avec penFoFPLS =========================



# Ajustement avec le modèle ffpls_bs

argvals_X <- seq(0, 1, length.out = ncol(X_train))
argvals_Y <- seq(0, 1, length.out = ncol(Y_train))

basisobj_X <- fda::create.bspline.basis(rangeval = range(argvals_X), nbasis = 20)
basisobj_Y <- fda::create.bspline.basis(rangeval = range(argvals_Y), nbasis = 20)

ncomp <- 3
penalty_X <- 0
penalty_Y <- 0


model <- penFoFPLS::ffpls_bs(X = X_train,
                             Y = Y_train,
                             center = TRUE,
                             argvals_X = argvals_X,
                             argvals_Y = argvals_Y,
                             ncomp = ncomp,
                             basisobj_X = basisobj_X,
                             basisobj_Y = basisobj_Y,
                             penalty_X = penalty_X,
                             penalty_Y = penalty_Y)


# Faire des prédictions sur l'ensemble de test
prediction_penFo <- predict.ffpls_bs(model, newdata = X_test)

# Visualiser les résultats
matplot(Y_test , type = 'l', col = 'blue', 
        xlab = 'Jour de l\'année', ylab = 'Température (°C)',
        main = 'Températures réelles vs prédictes par penFoFPLS')

# Ajouter les données réelles de test
matlines(prediction_penFo[, , 1], type = 'l', col = 'red')
legend('topright', legend = c('Réelles', 'Prédictions'), col = c('blue', 'red'), lty = 1)

# mape_penFo <- mean(abs((Y_test - prediction_penFo[, , 1]) / (Y_test + epsilon))) * 100


# les valeurs propres
model_cv <- penFoFPLS::cv_unique_fof_par(X = X_train,
                             Y = Y_train,
                             center = TRUE,
                             argvals_X = argvals_X,
                             argvals_Y = argvals_Y,
                             ncomp = ncomp,
                             basisobj_X = basisobj_X,
                             basisobj_Y = basisobj_Y)

penFoFPLS::means_plot(model_cv)
plot.cv_fofr(model_cv)


data.frame( RMSE = RMSE(prediction_penFo[, , 1], Y_test),
            MAE = MAE(prediction_penFo[, , 1], Y_test)) 


# ===== validation croisée avec penFoFPLS ====

num_folds <- 5

folds <- createFolds(seq_len(nrow(CanadianWeather$dailyAv[, , "Temperature.C"])), k = num_folds, list = TRUE)

accuracy_scores <- numeric(num_folds)

for (fold in seq_along(folds)) {
  # Extract training and testing indices for the fold
  train_indices <- unlist(folds[-fold])
  test_indices <- unlist(folds[fold])
  
  # Extract the functional data for training and testing
  X_train_cv <- CanadianWeather$dailyAv[, , "Precipitation.mm"][train_indices, ]
  Y_train_cv <- CanadianWeather$dailyAv[, , "Temperature.C"][train_indices, ]
  
  X_test_cv <- CanadianWeather$dailyAv[, , "Precipitation.mm"][test_indices, ]
  Y_test_cv <- CanadianWeather$dailyAv[, , "Temperature.C"][test_indices, ]
  
  fdobj_cv <- penFoFPLS::ffpls_bs(X = X_train_cv,
                                  Y = Y_train_cv,
                                  center = TRUE,
                                  argvals_X = argvals_X,
                                  argvals_Y = argvals_Y,
                                  ncomp = ncomp,
                                  basisobj_X = basisobj_X,
                                  basisobj_Y = basisobj_Y,
                                  penalty_X = penalty_X,
                                  penalty_Y = penalty_Y)
  
  predictions <- predict.ffpls_bs(fdobj_cv, X_test_cv)

  accuracy <- RMSE(predictions[, , 1], Y_test_cv)
  
  # Store the accuracy for this fold
  accuracy_scores[fold] <- accuracy
}


print(accuracy_scores)

