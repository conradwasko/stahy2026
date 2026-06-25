# Set up data
file.name <- "https://raw.githubusercontent.com/conradwasko/stahy2026/refs/heads/main/086071.csv"

data           <- read.csv(file.name)
colnames(data) <- c("code", "id", "year", "month", "day", "rain_mm", "acc", "quality")
data <- data[data$year != min(data$year) & data$year != max(data$year), ]
annual_maxima <- aggregate(rain_mm ~ year, data = data, FUN = max)
head(annual_maxima)

rain <- annual_maxima$rain_mm
year <- annual_maxima$year

# For you to do
file.name <- "https://raw.githubusercontent.com/conradwasko/stahy2026/refs/heads/main/089002.txt"
data      <- read.table(file.name, header = TRUE)
head(data)
rain <- data$min_12
year <- data$year

# Let's fit a non-stationary model
library(extRemes)

plot(year, rain, mgp = c(2, 0.6, 0), xlab = "Year", ylab = "Rainfall (mm)", pch = 16)
lm.fit <- lm(rain ~ year)
summary(lm.fit)
abline(lm.fit, col = "red3", lwd = 3)

g <- fevd(rain, type = "GEV")
summary(g)
plot(g)

g_mu <- fevd(rain, type = "GEV", location.fun = ~year)
summary(g_mu)

g_scale <- fevd(rain, type = "GEV", scale.fun = ~year)
g_loc_scale <- fevd(rain, type = "GEV", location.fun =~year, scale.fun = ~year)
plot(g_loc_scale)
plot(g_loc_scale, type = "rl")

lr.test(g, g_mu)   
lr.test(g, g_scale)  
lr.test(g, g_loc_scale)

