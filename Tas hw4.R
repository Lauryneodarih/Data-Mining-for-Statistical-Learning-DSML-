x2 <- read.table(file = "HW04part2-1.x.csv", header = TRUE)
## Generate n=101 equidistant points in [-2\pi, 2\pi]
m <- 1000
n <- 101
x <- 2*pi*seq(-1, 1, length=n)
## Initialize the matrix of fitted values for three method##
fvlp <- fvnw <- fvss <- matrix(0, nrow= n, ncol= m)

##Generate data, fit the data and store the fitted values
for (j in 1:m){
  ## simulate y-values
  ## You need to change the definition of the f(x) function
  y <- (1-x^2) * exp(-0.5 * x^2) + rnorm(length(x), mean =0, sd = 0.2);
  
  ## Get the estimates and store them
  fvlp[,j] <- predict(loess(y ~ x, span = 0.75), newdata = x);
  fvnw[,j] <- ksmooth(x, y, kernel="normal", bandwidth= 0.2, x.points=x)$y;
  fvss[,j] <- predict(smooth.spline(y ~ x), x=x)$y;
}

## Can you plot the empirical bias or empirical variance?
## Below is the sample R code to plot the mean of three estimators in a single plot.
dmin = min(apply(fvlp,1,mean), apply(fvnw,1,mean), apply(fvss,1,mean));
dmax = max(apply(fvlp,1,mean), apply(fvnw,1,mean), apply(fvss,1,mean));
matplot(x, apply(fvlp,1,mean), "l", ylim=c(dmin, dmax))
matlines(x, apply(fvnw,1, mean), col="red")
matlines(x, apply(fvss,1, mean), col="blue")

## Plot empirical Bias
ytrue = (1-x^2) * exp(-0.5 * x^2);
biaslp= apply(fvlp,1,mean) - ytrue;
biasnw= apply(fvnw,1, mean) - ytrue;
biasss= apply(fvss,1, mean) - ytrue;

dmin = min(biaslp, biasnw, biasss);
dmax = max(biaslp, biasnw, biasss);
matplot(x, biaslp, "l", ylim=c(dmin, dmax), ylab="Empirical Bias");
matlines(x, biasnw, col="red");
matlines(x, biasss, col="blue");

## Plot empirical Variance (or actually empirical MSE)
MSElp= apply( (fvlp - ytrue)^2, 1,sum) / (m-1);
MSEnw= apply( (fvnw - ytrue)^2, 1,sum) / (m-1);
MSEss= apply( (fvss - ytrue)^2, 1,sum) / (m-1);

dmin = min(MSElp, MSEnw, MSEss);
dmax = max(MSElp, MSEnw, MSEss);
matplot(x, MSElp, "l", ylim=c(dmin, dmax), ylab="Empirical MSE");
matlines(x, MSEnw, col="red");
matlines(x, MSEss, col="blue");

## Non-equidistant points
set.seed(79)
x <- 2*pi*sort(c(0.5, -1 + rbeta(50,2,2), rbeta(50,2,2)))
## Initialize the matrix of fitted values for three method##

fvlp <- fvnw <- fvss <- matrix(0, nrow= n, ncol= m)
##Generate data, fit the data and store the fitted values
for (j in 1:m){
  ## simulate y-values
  ## You need to change the definition of the f(x) function
  y <- (1-x^2) * exp(-0.5 * x^2) + rnorm(length(x), sd=0.2);
  
  ## Get the estimates and store them
  fvlp[,j] <- predict(loess(y ~ x, span = 0.3365), newdata = x);
  fvnw[,j] <- ksmooth(x, y, kernel="normal", bandwidth= 0.2, x.points=x)$y;
  fvss[,j] <- predict(smooth.spline(y ~ x, spar= 0.7163), x=x)$y;
}
## Plot the means
dmin = min(apply(fvlp,1,mean), apply(fvnw,1,mean), apply(fvss,1,mean));
dmax = max(apply(fvlp,1,mean), apply(fvnw,1,mean), apply(fvss,1,mean));
matplot(x, apply(fvlp,1,mean), "l", ylim=c(dmin, dmax))
matlines(x, apply(fvnw,1, mean), col="red")
matlines(x, apply(fvss,1, mean), col="blue")

## Plot empirical Bias
ytrue = (1-x^2) * exp(-0.5 * x^2);
biaslp= apply(fvlp,1,mean) - ytrue;
biasnw= apply(fvnw,1, mean) - ytrue;
biasss= apply(fvss,1, mean) - ytrue;

dmin = min(biaslp, biasnw, biasss);
dmax = max(biaslp, biasnw, biasss);
matplot(x, biaslp, "l", ylim=c(dmin, dmax), ylab="Empirical Bias");
matlines(x, biasnw, col="red");
matlines(x, biasss, col="blue");

## Plot empirical Variance (or actually empirical MSE)
MSElp= apply( (fvlp - ytrue)^2, 1,sum) / (m-1);
MSEnw= apply( (fvnw - ytrue)^2, 1,sum) / (m-1);
MSEss= apply( (fvss - ytrue)^2, 1,sum) / (m-1);

dmin = min(MSElp, MSEnw, MSEss);
dmax = max(MSElp, MSEnw, MSEss);
matplot(x, MSElp, "l", ylim=c(dmin, dmax), ylab="Empirical MSE");
matlines(x, MSEnw, col="red");
matlines(x, MSEss, col="blue");


