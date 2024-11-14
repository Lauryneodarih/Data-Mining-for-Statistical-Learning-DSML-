x2 <- read.table(file = "HW04part2-1.x.csv", header = TRUE)
# Part #1 deterministic equidistant design
m <- 1000
n <- 101
x <- seq(-2 * pi, 2 * pi, length.out = n)
yi <- (1 - x^2) * exp(-0.5 * x^2)
df <- data.frame(x = x, y = yi)

# Initialize matrices for fitted values
fvlp <- fvnw <- fvss <- matrix(0, nrow = n, ncol = m)

for (j in 1:m) {
  y <- yi + rnorm(length(x), sd = 0.2)
  fvlp[, j] <- predict(loess(y ~ x, span = 0.75), newdata = data.frame(x = x))
  fvnw[, j] <- ksmooth(x, y, kernel = "normal", bandwidth = 0.2, x.points = x)$y
  fvss[, j] <- predict(smooth.spline(y ~ x), x = x)$y
}

# Calculate the means
meanlp <- apply(fvlp, 1, mean)
meannw <- apply(fvnw, 1, mean)
meanss <- apply(fvss, 1, mean)
dmin <- min(meanlp, meannw, meanss)
dmax <- max(meanlp, meannw, meanss)

# Plot means
matplot(x, meanlp, "l", ylim = c(dmin, dmax), ylab = "Response")
matlines(x, meannw, col = "red")
matlines(x, meanss, col = "blue")

# Define a function to calculate bias
calculate_bias <- function(fv, yi) {
  apply(fv, 1, mean) - yi
}

# Plot bias for each method
lo_bias <- calculate_bias(fvlp, yi)
nw_bias <- calculate_bias(fvnw, yi)
ss_bias <- calculate_bias(fvss, yi)
bias_min <- min(lo_bias, nw_bias, ss_bias)
bias_max <- max(lo_bias, nw_bias, ss_bias)
matplot(x, lo_bias, "l", ylim = c(bias_min, bias_max), ylab = "Empirical Bias")
matlines(x, nw_bias, col = "red")
matlines(x, ss_bias, col = "blue")

# Define a function to calculate variance (MSE)
calculate_variance <- function(fv, yi, m) {
  apply((fv - yi)^2, 1, sum) / (m - 1)
}

# Plot variance for each method
lo_var <- calculate_variance(fvlp, yi, m)
nw_var <- calculate_variance(fvnw, yi, m)
ss_var <- calculate_variance(fvss, yi, m)
var_min <- min(lo_var, nw_var, ss_var)
var_max <- max(lo_var, nw_var, ss_var)
matplot(x, lo_var, "l", ylim = c(var_min, var_max), ylab = "Empirical Variance (MSE)")
matlines(x, nw_var, col = "red")
matlines(x, ss_var, col = "blue")

# Part #2 non-equidistant design
x2 <- read.table(file = "HW04part2-1.x.csv", header = TRUE)$x
fvlp2 <- fvnw2 <- fvss2 <- list()

for (j in 1:m) {
  y2 <- (1 - x2^2) * exp(-0.5 * x2^2) + rnorm(length(x2), sd = 0.2)
  fvlp2[[j]] <- predict(loess(y2 ~ x2, span = 0.3365), newdata = data.frame(x2 = x2))
  fvnw2[[j]] <- ksmooth(x2, y2, kernel = "normal", bandwidth = 0.2, x.points = x2)$y
  fvss2[[j]] <- predict(smooth.spline(y2 ~ x2, spar = 0.7163), x = x2)$y
}

# Convert list of results into matrices
fvlp2 <- do.call(cbind, fvlp2)
fvnw2 <- do.call(cbind, fvnw2)
fvss2 <- do.call(cbind, fvss2)

# Calculate the means
meanlp2 <- apply(fvlp2, 1, mean)
meannw2 <- apply(fvnw2, 1, mean)
meanss2 <- apply(fvss2, 1, mean)
dmin2 <- min(meanlp2, meannw2, meanss2)
dmax2 <- max(meanlp2, meannw2, meanss2)

# Plot the fitted values for each method
matplot(x2, meanlp2, "l", ylim = c(dmin2, dmax2), ylab = "Response (Non-Equidistant Design)")
matlines(x2, meannw2, col = "red")
matlines(x2, meanss2, col = "blue")

# Plot bias for each method
lo_bias2 <- calculate_bias(fvlp2, y2i)
nw_bias2 <- calculate_bias(fvnw2, y2i)
ss_bias2 <- calculate_bias(fvss2, y2i)
bias_min2 <- min(lo_bias2, nw_bias2, ss_bias2)
bias_max2 <- max(lo_bias2, nw_bias2, ss_bias2)
plot(x2, lo_bias2, type = "l", ylim = c(bias_min2, bias_max2), ylab = "Empirical Bias (Non-Equidistant Design)")
lines(x2, nw_bias2, col = "red")

# Plot variance for each method
lo_var2 <- calculate_variance(fvlp2, y2i, m)
nw_var2 <- calculate_variance(fvnw2, y2i, m)
ss_var2 <- calculate_variance(fvss2, y2i, m)
var_min2 <- min(lo_var2, nw_var2, ss_var2)
var_max2 <- max(lo_var2, nw_var2, ss_var2)
plot(x2, lo_var2, type = "l", ylim = c(var_min2, var_max2), ylab = "Empirical Variance (Non-Equidistant Design)")
lines(x2, nw_var2, col = "red")
lines(x2, ss_var2, col = "blue")

# Plot MSE for each method
mse_min2 <- min(calculate_variance(fvlp2, y2i, m), calculate_variance(fvnw2, y2i, m), calculate_variance(fvss2, y2i, m))
mse_max2 <- max(calculate_variance(fvlp2, y2i, m), calculate_variance(fvnw2, y2i, m), calculate_variance(fvss2, y2i, m))
plot(x2, lo_var2, type = "l", ylim = c(mse_min2, mse_max2), ylab = "Empirical MSE (Non-Equidistant Design)")
lines(x2, nw_var2, col = "red")
lines(x2, ss_var2, col = "blue")
