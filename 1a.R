args<-commandArgs(TRUE)

gen_inv <- function(lambda, alpha, n) {
	U <- runif(n, 0, 1);
	Q <- vector(length = n);
	X <- vector(length = n);
	Q <- ((1 + lambda) - sqrt((1 + lambda)^2 - (4 * lambda * U))) / (2 * lambda);
	X <- - log(1 - Q^(1 / alpha)) / lambda;

	pdf("1a.pdf");
	hist(X, breaks = 50, main = "");
	legend('topright', legend = c(paste("lambda =", lambda), paste("alpha =", alpha), paste("sample size =", n)), lty = 0, col = "white", bty = 'n');

	cat("The sample mean and variance, given lambda", lambda, ", alpha", alpha, ", and sample size ", n, ", are estimated to be", mean(X), ", and", var(X), ", respectively.\n")
}

lambda = as.numeric(args[1]);
alpha = as.numeric(args[2]);
n = as.integer(args[3]);

set.seed(1);

gen_inv(lambda, alpha, n);

#The sample mean and variance, given lambda = 0.5 , alpha = 2 , and sample size  1000 , are estimated to be 2.441197 , and 3.944559 , respectively.