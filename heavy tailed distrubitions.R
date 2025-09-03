library(gld)
install.packages("fitdistrplus")
library(fitdistrplus)
install.packages("ggplot2") 
library(ggplot2)
set.seed(123) 
veri <- runif(35000, min = 12500, max = 98000)
install.packages("gld")
gld_model <- fit.gld(veri)
install.packages("fitdistrplus")
weibull_model <- fitdist(veri, "weibull")
lognormal_model <- fitdist(veri, "lnorm")
ggplot(data.frame(x = veri), aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1000, fill = "lightblue", color = "black") +
  stat_function(fun = dweibull, args = list(shape = weibull_model$estimate["shape"], scale = weibull_model$estimate["scale"]), color = "red", size = 1.5) +
  ggtitle("Weibull Histogram Grafiği ")
weibull_var_95 <- qweibull(0.05, shape = weibull_model$estimate["shape"], scale = weibull_model$estimate["scale"])

veri <- rweibull(n = 1000, shape = 1.5, scale = 2)
weibull_model <- fitdist(veri, "weibull")
plot(weibull_model)