args<-commandArgs(TRUE)

library(MASS) #For kde2d

#Function BVGEF Computes Value F(X)
BVGEF <- function(X, lambda, alpha) {
	return ((1 - exp(-lambda * X[1]))^(alpha[2]) * (1 - exp(-lambda * X[2]))^(alpha[3]) * (1 - exp(-lambda * min(X)))^(alpha[1]));
}

#Function GE generates n numbers from generalised exponential
GE <- function(n, lambda, alpha) {
	U <- runif(n,0,1);
	return (- log(1 - U^(1 / alpha)) / lambda);
}

#Function BVGE generates n numbers from Bivariate generalised exponential
BVGE <- function(n, lambda, alpha) {
	bvge <- matrix(0, nrow = n, ncol = 2);

	for(i in 1:n) {
		U <- vector(length = 3);
		U[1]<-GE(1,lambda,alpha[1]);
		U[2]<-GE(1,lambda,alpha[2]);
		U[3]<-GE(1,lambda,alpha[3]);		

		bvge[i,] <- c(max(U[1], U[2]), max(U[1], U[3]));
#		bvge[i,1] <- max(U[1], U[2]);
#		bvge[i,2] <- max(U[1], U[3]);
	}
	return (bvge);
}

#Function TRUNC_BVGE generates n numbers from Truncated Bivariate generalised exponential
TRUNC_BVGE <- function(n, lambda, alpha) {
	trunc_bvge<-matrix(0, nrow = n, ncol = 2);

	for (i in 1:n) {
		repeat {
			u<-runif(1, 0, 1);
			X <- BVGE(1, lambda, alpha);
			F <- BVGEF(X, lambda, alpha);

			if (u <= (1 - (4 * lambda * F)/(1 + lambda))) {
				trunc_bvge[i,] <- X;
				break;
			}
		}
	}

	return(trunc_bvge);
}  

n = as.integer(args[1]);
lambda = as.numeric(args[2]);
#alpha <- vector(length = 3);
alpha <- c(as.numeric(args[3]), as.numeric(args[4]), as.numeric(args[5]));

set.seed(1);

#X <- TRUNC_BVGE(1000,1,0.5,0.6,0.7);
X <- TRUNC_BVGE(n, lambda, alpha);
f <- kde2d(X[,1], X[,2], n = n); # Two dimensional kernel density approximation

cat("The sample means, variances, and covariance, given lambda", lambda, ", alpha", alpha, ", and sample size ", n, ", are estimated to be :","\nmean(X1) =", mean(X[,1]), "\nmean(X2) =", mean(X[,2]), "\nvariance(X1) =", var(X[,1]), "\nvariance(X2) =", var(X[,2]), "\ncorrelation(X1, X2) =", cor(X[,1], X[,2]), "\n");

pdf("2.pdf");
#contour(f, xlab = "X1", ylab = "X2", main = "")
persp(f, xlab = "X1", ylab = "X2", zlab = "Density", main = "", box = TRUE)
legend('topright', legend = c(paste("lambda =", lambda), paste("alpha =", alpha[1], ",", alpha[2], ",", alpha[3]), paste("sample size =", n)), lty = 0, col = "white", bty = 'n');

#The sample means, variances, and covariance, given lambda 1 , alpha 0.5 0.6 0.7 , and sample size  100 , are estimated to be : 
#mean(X1) = 0.593018 
#mean(X2) = 0.6326948 
#variance(X1) = 0.5792707 
#variance(X2) = 0.5200241 
#correlation(X1, X2) = -0.137354