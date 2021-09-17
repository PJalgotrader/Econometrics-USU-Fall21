

starting <- 0
ending   <- 10

x<- seq(starting,ending,0.1)
y<- x^2 +rnorm(x)*10
plot(x,y)
curve(x^2, starting,ending, add=T, col="black", lty=3, lwd=3)

lin_fit <- lm(y~x)
abline(lin_fit, col="blue")


nonlin_fit <- lm(y~I(x^2))
lines(x, predict(nonlin_fit), col="red", lwd= 3)


summary(lin_fit)
summary(nonlin_fit)




