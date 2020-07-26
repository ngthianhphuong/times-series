# sunplot series
plot(sunspot.year, xlab='t', ylab='Sunsplots')

# bruit blanc gaussien de loi N(0, 3^2)
set.seed(1789)
plot(ts(rnorm(100, sd=3), start=1, end=100), xlab='t', ylab='Bruit blanc gaussien de variance 9')
abline(h=0)

# série uspop
plot(uspop, xlab='t', ylab='Uspop')

# série airpass
plot(AirPassengers, xlab='t', ylab='Airpass')
plot(log(AirPassengers), xlab='t', ylab='Airpass')


# série beer
beer = read.csv("datasets/beer.csv", header=F, dec=".", sep=",")
beer = ts(beer[, 2], start=1956, freq=12)
plot(beer, xlab = "t", ylab = "Beer")


# série lynx
plot(lynx, xlab="t", ylab="Lynx")

x = AirPassengers
y = log(x)

# base tendencielle: 12 ans * 12 mois
t = 1:144

# base saisonnière (une indicatrice s pour chaque moi)
for (i in 1:12)
{
  su = rep(0, times=12) # un vecteur de 12 éléments
  su[i] = 1 # l'élément correspondant au mois reçoit 1
  s = rep(su, times=12) # 12 vecteurs pour 12 mois
  assign(paste("s", i, sep=""), s)
}

# régression linéaire Y(t) = a + bt + sigma(ci*l)
# ==> a, b et ci sont à trouver
reg = lm(y~t+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12-1)
summary(reg)

# obtenir les coefficients
reg$coefficients

a = mean(reg$coefficients[2:13])
b = reg$coefficients[1]
c = reg$coefficients[2:13] - mean(reg$coefficients[2:13])

# obtenir la série (logarithmique) corrigée de variations saisonnières
y_cvs = y - (c[1]*s1 + c[2]*s2+c[3]*s3+c[4]*s4+c[5]*s5+c[6]*s6+c[7]*s7+c[8]*s8+c[9]*s9+c[10]*s10+c[11]*s11+c[12]*s12)

# série initiale
x_cvs = exp(y_cvs)

# plot
ts.plot(x, x_cvs, xlab="t", ylab="Airpass", col=c(1,2), lwd=c(1,2))
legend("topleft", legend=c("X", "X_CVS"), col=c(1,2), lwd = c(1,2))


decomp_x = decompose(x, type="multiplicative")
plot(decomp_x)

decomp_y = decompose(y, type="additive")
plot(decomp_y)

x_cvs = exp(y-decomp_y$seasonal)
ts.plot(x, x_cvs, xlab="t", ylab="Airpass", col=c(1,2), lwd=c(1,2))
legend("topleft", legend=c("X", "X_CVS"), col=c(1,2), lwd=c(1,2))

# prédiction
install.packages("forecast")
library(forecast)

# lissage exponentiel simple
les = ets(y, model="ANN") # A: additive error. Null trend. Null seasonality
les.pred = predict(les, 12)
plot(les.pred)

les_alt1 = ets(y, alpha=0.1, model="ANN") # coefficient de lissage = 0.9
plot(les_alt1)

les_alt2 = ets(y, alpha=0.9, model="ANN") # coef = 0.1
plot(les_alt2)

# lissage exponentiel double
led = ets(y, model="AAN")
led.pred = predict(led, 12)
plot(led.pred)

# méthode Holt-Winters
hw = ets(x, model="MMM")
hw.pred = predict(hw, 12)
plot(hw.pred)

