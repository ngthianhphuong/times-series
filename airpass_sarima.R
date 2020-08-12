install.packages("latex2exp")
install.packages("caschrono")

library(latex2exp)
library(caschrono)
library(forecast)
nlag = 36 # nombre de retard dans les sorties PACF, ACF
sais = 12

# modèle multiplicatif
x = AirPassengers
plot(AirPassengers, xlab="t", ylab="", main=TeX("$X_t$"))

# modèle additif
y = log(x)
plot(y, xlab="t", ylab="", main=TeX("$\\ln\\left(X_t\\right)$"))

# obtenir ACF et vérifier sa décroissance vers 0
acf = acf(y, lag.max=nlag, plot=FALSE)
acf$lag = acf$lag*sais

plot(acf, ci=0.05, xlim=c(2, nlag-1), ylim=c(-1,1), xaxp=c(1, nlag, nlag-1),
     type="h", lend="butt", lwd=5, xlab="h", ylab="ACF",
     main=TeX("$\\ln\\left(X_t\\right)$"))
# décroissance lente, ce ACF n'est pas un autocorrélogramme simple

# différencier une première fois
y_dif_1 = diff(y, lag=1, differences=1) # retard 1, différencier une seule fois
acf = acf(y_dif_1, lag.max=nlag, plot=FALSE)
acf$lag = acf$lag*sais
plot(acf, ci=0.05, xlim=c(2, nlag-1), ylim=c(-1, 1), xaxp=c(1, nlag, nlag-1),
     type="h", lend="butt", lwd=5, xlab="h", ylab="ACF",
     main=TeX("$(I-B)\\ln\\left(X_t\\right)$"))
# décroissance rapide au début mais il y a des pics toutes les 12 périodes

# différencier ordre 12
y_dif_1_12 = diff(y_dif_1, lag=12, differences=1)
acf = acf(y_dif_1_12, lag.max=nlag, plot=FALSE)
acf$lag = acf$lag*sais
plot(acf, ci=0.95, xlim=c(2, nlag-1), ylim=c(-1, 1), xaxp=c(1, nlag, nlag-1),
     type="h", lend="butt", lwd=5, xlab="h", ylab=TeX("$\\widehat{\\rho}(h)$"),
     main=TeX("$(I-B)\\left(I-B^{12}\\right)\\ln\\left(X_t\\right)$"))
# peut être considérée comme stationnaire

# calculer le PACF
pacf = pacf(y_dif_1_12, lag.max = nlag, plot=FALSE)
pacf$lag = pacf$lag*sais
plot(pacf, ci=0.95, xlim=c(2, nlag-1), ylim=c(-1, 1), xaxp=c(1, nlag, nlag-1),
     type="h", lend="butt", lwd=5, xlab="h", ylab=TeX("$\\widehat{r}(h)$"),
     main=TeX("$(I-B)\\left(I-B^{12}\\right)\\ln\\left(X_t\\right)$"))

# identification de modèle
# pic plus clair: h=1 ou h=12

model1 = Arima(y, order=c(1,1,1), list(order=c(1,1,1), period=12),
               include.mean=FALSE, method="CSS-ML")
summary(model1)

# tester la significativité des paramètres du modèle
t_stat(model1) # ar1 et sar1 ne sont pas significatifs

# test de blancheur
Box.test.2(model1$residuals, nlag=c(6, 12, 18, 24, 30, 36),
           type="Ljung-Box", decim=5) # le résidu suit un bruit blanc

# enlever le paramètre le moins significatif (sar1)
model2 = Arima(y, order=c(1,1,1), list(order=c(0,1,1), period=12),
               include.mean=FALSE, method="CSS-ML")
summary(model2)
t_stat(model2)
Box.test.2(model2$residuals, nlag=c(6, 12, 18, 24, 30, 36),
           type="Ljung-Box", decim=5) # test de blancheur ok

# enlever le paramètre ar1
model3 = Arima(y, order=c(0,1,1), list(order=c(0,1,1), period=12),
               include.mean=FALSE, method="CSS-ML")
summary(model3)
t_stat(model3)
Box.test.2(model3$residuals, nlag=c(6, 12, 18, 24, 30, 36),
           type="Ljung-Box", decim=5) # test de blancheur ok

# vérifier la normalité du résidu
shapiro.test(model3$residuals) # le test de normalité n'est pas rejeté

# prévision
pred_model3 = forecast(model3, h=12, level=95)
pred = ts(c(x[144], exp(pred_model3$mean)), start=c(1960,12), frequency=12)
pred_l=ts(c(x[144], exp(pred_model3$lower)), start=c(1960,12), frequency=12)
pred_u=ts(c(x[144], exp(pred_model3$upper)), start=c(1960,12), frequency = 12)
ts.plot(x, pred, pred_l, pred_u, xlab="t", ylab=" ",
        main="Air passengers", col=c(1, 2, 3, 3),
        lty=c(1,1,2,2), lwd=c(1,3,2,2))

ts.plot(window(x, start=c(1960,1)), pred, pred_l, pred_u, xlab="t",
        ylab=" ", main="Air passengers", col=c(1,2,3,3),
        lty=c(1,1,2,2), lwd=c(1,3,2,2))

# tronquer la série
x_tronc = window(x, end=c(1959,12))
y_tronc = log(x_tronc)
x_a_prevoir = window(x, start=c(1960,1))

# vérifier si le modèle 3 est toujours convenable
model3tronc = Arima(y_tronc, order=c(0,1,1), list(order=c(0,1,1), period=12),
               include.mean=FALSE, method="CSS-ML")
summary(model3tronc)
t_stat(model3tronc)
Box.test.2(model3tronc$residuals, nlag=c(6, 12, 18, 24, 30, 36),
           type="Ljung-Box", decim=5) # test de blancheur ok
shapiro.test(model3tronc$residuals)

# prédiction
pred_model3tronc = forecast(model3tronc, h=12, level=95)
pred_tronc = ts(exp(pred_model3tronc$mean), start=c(1960,1), frequency=12)
pred_l_tronc=ts(exp(pred_model3tronc$lower), start=c(1960,1), frequency=12)
pred_u_tronc=ts(exp(pred_model3tronc$upper), start=c(1960,1), frequency = 12)
ts.plot(x_a_prevoir, pred_tronc, pred_l_tronc, pred_u_tronc, xlab="t", ylab=" ",
        main="Air passengers", col=c(1, 2, 3, 3),
        lty=c(1,1,2,2), lwd=c(3,3,2,2))
legend("topleft", legend=c("X", "X_prev"), col=c(1,2,3,3),
       lty=c(1,1), lwd=c(3,3))
legend("topright", legend=c("int95%_inf", "int95%_sup"), col=c(3,3),
       lty=c(2,2), lwd=c(2,2))


  