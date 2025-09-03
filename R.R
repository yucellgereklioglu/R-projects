if(!require(gld)) install.packages("gld")
if(!require(VGAM)) install.packages("VGAM")      
if(!require(evd)) install.packages("evd")       
library(gld)
library(VGAM)
library(evd)
x <- seq(0.01, 20, length.out=1000)  
lambda <- c(0, 1, 0.14, 0.14)
gld_quantile <- function(u, l1, l2, l3, l4) {
  l1 + (1/l2) * (u^l3 - (1 - u)^l4)
}
u_grid <- seq(0, 1, length.out=100000)
q_grid <- gld_quantile(u_grid, lambda[1], lambda[2], lambda[3], lambda[4])
cdf_gld <- approx(q_grid, u_grid, xout=x, yleft=0, yright=1)$y
cdf_pareto <- pparetoII(x, scale=1, shape=2)
cdf_inv_exp <- 1 - exp(-1/x)
cdf_weibull <- pweibull(x, shape=2, scale=1)
cdf_gumbel <- pgumbel(x, loc=0, scale=1)
plot(x, cdf_gld, type="l", col="purple", lwd=2, ylim=c(0,1), 
     ylab="CDF", xlab="x", main="GLD ve diğer dağılımların kümülatif dağılım fonksiyon grafiği ")

lines(x, cdf_pareto, col="red", lwd=2, lty=2)
lines(x, cdf_inv_exp, col="blue", lwd=2, lty=3)
lines(x, cdf_weibull, col="green", lwd=2, lty=4)
lines(x, cdf_gumbel, col="orange", lwd=2, lty=5)

legend("bottomright", legend=c("GLD", "Pareto", "Ters Üstel", "Weibull", "Gumbel"),
       col=c("purple", "red", "blue", "green", "orange" ),
       lty=1:6, lwd=2)


