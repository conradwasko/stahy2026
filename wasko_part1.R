file.name <- "https://raw.githubusercontent.com/conradwasko/stahy2026/refs/heads/main/086071.csv"

# Set up data
data           <- read.csv(file.name)
colnames(data) <- c("code", "id", "year", "month", "day", "rain_mm", "acc", "quality")
data <- data[data$year != min(data$year) & data$year != max(data$year), ]
head(data)

# A demonstration of the CLT (step by step)
hist(data$rain_mm) 

annual_mean  <- aggregate(rain_mm ~ year, data = data, FUN = mean)

head(annual_mean)
hist(annual_mean$rain_mm)

rain_ordered <- sort(annual_mean$rain_mm)
n            <- length(rain_ordered)
pp           <- (1:n)/(n+1)
print(pp)

sample_mean  <- mean(annual_mean$rain)
sample_sd    <- sd(annual_mean$rain)

th_quantiles <- qnorm(pp, mean = sample_mean, sd = sample_sd)

plot(th_quantiles, rain_ordered,
     main = "Q-Q Plot (Manual)",
     xlab = "Theoretical Quantiles",
     ylab = "Sample Quantiles",
     pch  = 16, col = "steelblue")
abline(a = 0, b = 1, lwd = 2)

# A demonstration of the CLT (using in built functions)
annual_mean <- aggregate(rain_mm ~ year, data = data, FUN = mean)
stats::qqnorm(annual_mean$rain)
stats::qqline(annual_mean$rain, col = "red3")

# A demonstration of the EVT (step by step)
annual_maxima <- aggregate(rain_mm ~ year, data = data, FUN = max)
hist(annual_maxima$rain_mm)
stats::qqnorm(annual_maxima$rain)
stats::qqline(annual_maxima$rain, col = "red3")

library(extRemes)
am.sorted <- sort(annual_maxima$rain_mm)
n         <- length(am.sorted)
pp        <- (1:n)/(n+1)
gev.fit   <- fevd(annual_maxima$rain_mm)
print(gev.fit)
th        <- qevd(pp,
                  gev.fit$results$par[1],
                  gev.fit$results$par[2],
                  gev.fit$results$par[3])

plot(th, am.sorted,
     xlab = "Theoretical GEV quantiles",
     ylab = "Empirical quantiles",
     main = "GEV Q-Q plot",
     pch = 16, col = "steelblue")

abline(a = 0, b = 1, lwd = 2)

# A demonstration of the EVT (using in built functions)
library(extRemes)
annual_maxima <- aggregate(rain_mm ~ year, data = data, FUN = max)
fit <- fevd(annual_maxima$rain_mm, type = "GEV")
plot(fit, type = "qq")

# GEV DEMO
x = ppoints(100)
print(x)

y.1 = qevd(x, 0, 1, 0)
y.2 = qevd(x, 1, 1, 0)
y.3 = qevd(x, 2, 1, 0)

plot(x,  y.1, type = "l", mgp = c(2, 0.6, 0), xlab = "Gumbel PP", ylab = "Quantile", lwd = 3)
plot(-log(-log(x)),  y.1, type = "l", mgp = c(2, 0.6, 0), xlab = "Gumbel PP", ylab = "Quantile", lwd = 3)
lines(-log(-log(x)), y.2, type = "l", mgp = c(2, 0.6, 0), lwd = 3, col = adjustcolor("black", alpha = 0.5))
lines(-log(-log(x)), y.3, type = "l", mgp = c(2, 0.6, 0), lwd = 3, col = adjustcolor("black", alpha = 0.3))

y.1 = qevd(x, 0, 1.0, 0)
y.2 = qevd(x, 0, 1.2, 0)
y.3 = qevd(x, 0, 2.0, 0)

plot(-log(-log(x)),  y.1, type = "l", mgp = c(2, 0.6, 0),  xlab = "Gumbel PP", ylab = "Quantile", lwd = 3)
lines(-log(-log(x)), y.2, type = "l", mgp = c(2, 0.6, 0), lwd = 3, col = adjustcolor("black", alpha = 0.5))
lines(-log(-log(x)), y.3, type = "l", mgp = c(2, 0.6, 0), lwd = 3, col = adjustcolor("black", alpha = 0.3))
abline(h = 0, v = 0)

y.1 = qevd(x, 0, 1, 0)
y.2 = qevd(x, 0, 1, -0.2)
y.3 = qevd(x, 0, 1, 0.2)
y.4 = qevd(x, 0, 1, 0.4)

plot(-log(-log(x)),  y.1, type = "l", mgp = c(2, 0.6, 0),  xlab = "Gumbel PP", ylab = "Quantile", lwd = 3)
lines(-log(-log(x)), y.2, type = "l", mgp = c(2, 0.6, 0), lwd = 3, col = adjustcolor("black", alpha = 0.5))
lines(-log(-log(x)), y.3, type = "l", mgp = c(2, 0.6, 0), lwd = 3, col = adjustcolor("black", alpha = 0.5))
lines(-log(-log(x)), y.4, type = "l", mgp = c(2, 0.6, 0), lwd = 3, col = adjustcolor("black", alpha = 0.3))
abline(h = 0, v = 0)

plot(density(y.1), type = "l", lwd = 2)
lines(density(y.2), type = "l", col = "steelblue", lwd = 2)
lines(density(y.3), type = "l", col = "red3", lwd = 2)
lines(density(y.4), type = "l", col = "red3", lwd = 2)

