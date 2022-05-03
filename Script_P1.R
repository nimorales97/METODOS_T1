library("R.matlab")
library("fcuk")
library("tidyverse")

# 1 ----
data <- readMat("represa-1.mat")
df <- data.frame(
  x = c(data$X,data$Xtest),
  y = c(data$y,data$ytest)
)

train <- 1:12
test <- 13:33

mod_1 <- lm(y ~ x, data = df[train,])
plot(df$x[train], df$y[train])
lines(mod_1$fitted.values ~ df$x[train])
# 2 ----
.error_estandar <- function(model){
  k=length(model$coefficients)-1
  
  #calculate sum of squared residuals
  SSE=sum(model$residuals**2)
  
  #calculate total observations in dataset
  n=length(model$residuals)
  
  #calculate residual standard error
  sqrt(SSE/(n-(1+k))) %>% return
}
# a)
mods <- list()
for (i in train){
  mods[[i]] <- lm(df$y[1:i] ~ df$x[1:i])
}
# b)
errores <- lapply(mods,.error_estandar)
# c)
mod_1c <- lm(y~x,data=df)
.res <- mod_1c$residuals[train]**2
.k <- length(mod_1c$coef) - 1
.n <- length(train)
sum(.res)/(.n-(1+.k)) #55.99843
# d)
errores <- c(errores, sum(.res)/(.n-(1+.k)))
plot(c(train,33), errores)

# 3 ----
.normaliza <- function(X){
  list(
    mu = mean(X),
    sd = sd(X),
    x = (X - mean(X))/sd(X)
  ) %>% return
}
x_1 <- .normaliza(df$x   )$x
x_2 <- .normaliza(df$x**2)$x
x_3 <- .normaliza(df$x**3)$x
x_4 <- .normaliza(df$x**4)$x
x_5 <- .normaliza(df$x**5)$x
x_6 <- .normaliza(df$x**6)$x
x_7 <- .normaliza(df$x**7)$x
x_8 <- .normaliza(df$x**8)$x
# 4 ----
mod_3 <- lm(data$y[train] ~ x_1[train] + x_2[train] + x_3[train] + x_4[train] + x_5[train] + x_6[train] + x_7[train] + x_8[train])
plot(x_1[train],data$y[train])
lines(x_1[train][order(x_1[train])], mod_3$fitted.values[order(x_1[train])])
.error_estandar(mod_3) #0.4807514
# 5 ----
mods.poly <- list()
for (i in train){
  mods.poly[[i]] <- lm(df$y[1:i] ~ df$x[1:i])
}
e.poly <- lapply(mods.poly,.error_estandar)
mod_5 <- lm(df$y ~ x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7 + x_8)
.res <- mod_5$residuals**2
.k <- length(mod_5$coef) - 1
.n <- length(data$y)
sum(.res)/(.n-(1+.k)) #35.97341

plot(1:13, unlist(errores), col = 2)
points(c(unlist(e.poly),55.99843), col = 3)

# 6 ----
lambda <- 10
theta <- lambda*(sum(mod_5$coef**2)/(2*33))
mod_6 <- lm(df$y ~ x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7 + x_8 - rep(theta, 33))
.error_estandar(mod_6) #2.120537
plot(x_1,df$y)
lines(x_1[order(x_1)], mod_6$fitted.values[order(x_1)])

lambda <- 100
theta <- function(x){x*(sum(mod_5$coef**2)/(2*33))}
mod_6 <- lm(df$y ~ x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7 + x_8 - rep(theta, 33))
.error_estandar(mod_6) #2.120537
plot(x_1,df$y)
lines(x_1[order(x_1)], mod_6$fitted.values[order(x_1)])

# 7 ----
message("--SOLO DATOS TRAIN--")
lm(y~x,data=df[train,]) %>% .error_estandar
message("Lineal simple: 7.327849")
lm(df$y[train]~x_1[train] + x_2[train] + x_3[train] + x_4[train] + x_5[train] + x_6[train] + x_7[train] + x_8[train]) %>% .error_estandar
message("Poly sin penalizar: 0.4807514")
lm(df$y[train]~x_1[train] + x_2[train] + x_3[train] + x_4[train] + x_5[train] + x_6[train] + x_7[train] + x_8[train] - rep(theta(10), 12)) %>% .error_estandar
message("Poly penalizado (lambda 10): 0.4807514")
lm(df$y[train]~x_1[train] + x_2[train] + x_3[train] + x_4[train] + x_5[train] + x_6[train] + x_7[train] + x_8[train] - rep(theta(100), 12)) %>% .error_estandar
message("Poly penalizado (lambda 100): 0.4807514")

