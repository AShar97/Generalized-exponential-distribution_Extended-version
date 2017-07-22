args<-commandArgs(TRUE)

gen_ar <- function(lambda, alpha, n) {
	X <- vector(length = n);

	for (i in 1:n) {
		repeat {
			u<-runif(2, 0, 1);
			if (u[2] <= ((1+lambda) - (2*lambda*u[1]))/(1 + abs(lambda))) {
				X[i] = - (log(1 - u[1]^(1 / alpha)) / lambda);
				break;
			}
		}
	}

	pdf("1b.pdf");
	hist(X, breaks = 50, main = "");
	legend('topright', legend = c(paste("lambda =", lambda), paste("alpha =", alpha), paste("sample size =", n)), lty = 0, col = "white", bty = 'n');

	cat("The sample mean and variance, given lambda", lambda, ", alpha", alpha, ", and sample size ", n, ", are estimated to be", mean(X), ", and", var(X), ", respectively.\n")
}

lambda = as.numeric(args[1]);
alpha = as.numeric(args[2]);
n = as.integer(args[3]);

set.seed(1);

gen_ar(lambda, alpha, n);

#The sample mean and variance, given lambda 0.5 , alpha 2 , and sample size  1000 , are estimated to be 2.43525 , and 3.771058 , respectively.