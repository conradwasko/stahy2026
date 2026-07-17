library(extRemes)

file.name <- "https://raw.githubusercontent.com/conradwasko/stahy2026/refs/heads/main/089002.txt"
data      <- read.table(file.name, header = TRUE)
head(data)
rain <- data$min_12
year <- data$year

n           <- length(rain)
rain.sorted <- sort(rain)
pp          <- (1:n-0.4)/(n+0.2)

# Fit stationary model
g <- fevd(rain, type = "GEV", method = "MLE")
summary(g)

plot(-log(-log(pp)), rain.sorted, pch = 17, mgp = c(2, 0.6, 0), ylab = "Quantile (mm)")
axis(side = 1, at = -log(-log(1-1/c(2, 5, 10, 20, 50, 100))), labels = c(2, 5, 10, 20, 50, 100), line = -2.5, mgp = c(2, 0.6, 0))
G = qevd(pp, loc = g$results$par[1], scale = g$results$par[2], shape = g$results$par[3], type = "GEV")
lines(-log(-log(pp)), G)

# Fit non-stationary model (location only)
g_mu <- fevd(rain, type = "GEV", location.fun = ~year)
summary(g_mu)

plot(-log(-log(pp)), rain.sorted, pch = 17, mgp = c(2, 0.6, 0), ylab = "Quantile (mm)")
axis(side = 1, at = -log(-log(1-1/c(2, 5, 10, 20, 50, 100))), labels = c(2, 5, 10, 20, 50, 100), line = -2.5, mgp = c(2, 0.6, 0))
G = qevd(pp, loc = g$results$par[1], scale = g$results$par[2], shape = g$results$par[3], type = "GEV")
lines(-log(-log(pp)), G)
for (i in 1:n) {
  t.year <- year[i]
  G_mu   <- qevd(pp, loc = g_mu$results$par[1] + t.year*g_mu$results$par[2], scale = g_mu$results$par[3], shape = g_mu$results$par[4], type = "GEV")
  lines(-log(-log(pp)), G_mu, col = hcl.colors(n, palette = "Teal")[i], lwd = 2)
}

# Fit non-stationary model (scale only)
g_scale <- fevd(rain, type = "GEV", scale.fun = ~year)
summary(g_scale)

plot(-log(-log(pp)), rain.sorted, pch = 17, mgp = c(2, 0.6, 0), ylab = "Quantile (mm)")
axis(side = 1, at = -log(-log(1-1/c(2, 5, 10, 20, 50, 100))), labels = c(2, 5, 10, 20, 50, 100), line = -2.5, mgp = c(2, 0.6, 0))
G = qevd(pp, loc = g$results$par[1], scale = g$results$par[2], shape = g$results$par[3], type = "GEV")
lines(-log(-log(pp)), G)
for (i in 1:n) {
  t.year  <- year[i]
  G_scale <- qevd(pp, loc = g_scale$results$par[1], 
                  scale = g_scale$results$par[2]+t.year*g_scale$results$par[3], 
                  shape = g_scale$results$par[4], type = "GEV")
  lines(-log(-log(pp)), G_scale, col = hcl.colors(n, palette = "Teal")[i])
}

# Fit non-stationary model (location and scale)
g_loc_scale <- fevd(rain, type = "GEV", location.fun =~year, scale.fun = ~year)

plot(-log(-log(pp)), rain.sorted, pch = 17, mgp = c(2, 0.6, 0), ylab = "Quantile (mm)")
axis(side = 1, at = -log(-log(1-1/c(2, 5, 10, 20, 50, 100))), labels = c(2, 5, 10, 20, 50, 100), line = -2.5, mgp = c(2, 0.6, 0))
G = qevd(pp, loc = g$results$par[1], scale = g$results$par[2], shape = g$results$par[3], type = "GEV")
lines(-log(-log(pp)), G)
for (i in 1:n) {
  t.year  <- year[i]
  G_loc_scale <- qevd(pp, 
                      loc = g_loc_scale$results$par[1] + g_loc_scale$results$par[2]*t.year, 
                      scale = g_loc_scale$results$par[3] + g_loc_scale$results$par[4]*t.year, 
                      shape = g_loc_scale$results$par[5], type = "GEV")
  lines(-log(-log(pp)), G_loc_scale, col = hcl.colors(n, palette = "Teal")[i])
}

# Statistical tests
lr.test(g, g_mu)   
lr.test(g, g_scale)  
lr.test(g, g_loc_scale)

# One final comparison
#remotes::install_github("ilapros/ilaprosUtils")
library(ilaprosUtils)

# Fit non-stationary model (location and scale)
g_cv <- gevcvd.fit(rain, ydat = cbind(year), mul = 1, show = FALSE)

plot(-log(-log(pp)), rain.sorted, pch = 17, mgp = c(2, 0.6, 0), ylab = "Quantile (mm)")
axis(side = 1, at = -log(-log(1-1/c(2, 5, 10, 20, 50))), labels = c(2, 5, 10, 20, 50), line = -2.5, mgp = c(2, 0.6, 0))
G = qevd(pp, loc = g$results$par[1], scale = g$results$par[2], shape = g$results$par[3], type = "GEV")
lines(-log(-log(pp)), G)
for (i in 1:n) {
  t.year  <- year[i]
  G_loc_scale <- qevd(pp, 
                      loc   = g_cv$vals[i,1], 
                      scale = g_cv$vals[i,2], 
                      shape = g_cv$vals[i,3], 
                      type = "GEV")
  lines(-log(-log(pp)), G_loc_scale, col = hcl.colors(n, palette = "Teal")[i])
}

2*(g$results$value - g_cv$nllh)
pchisq(2*(g$results$value - g_cv$nllh), 1, lower.tail = FALSE)
