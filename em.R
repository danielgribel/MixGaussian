set.seed(1607)

nbPoints <- 1000

nbClusters <- 3

alphas <- runif(nbClusters)

alphas <- alphas/sum(alphas)

means <- sample.int(10, nbClusters, replace = FALSE)

x <- rep(0, nbPoints)

s <- sample(seq(1:nbClusters), size = nbPoints, replace = TRUE, prob = alphas)

for( i in 1:nbPoints ) {
	x[i] <- rnorm(1, mean = means[s[i]])
}

# simulating an outlier
x[1] <- 50.0

# initial guess for distributions means
mu <- sample.int(10, nbClusters, replace = FALSE)

# initial guess for proportion of distributions (guess for alphas)
tau <- rep(1.0/nbClusters, nbClusters)

T <- array(dim = c(nbClusters, nbPoints))
P <- array(dim = c(nbClusters, nbPoints))

for(i in 1:100) {
	
	# Given the observed data, as well as the distribution parameters, what are the latent variables?
	for(j in 1:nbClusters) {
		T[j,] <- tau[j] * dnorm( x, mu[j] )
	}

	for(j in 1:nbClusters) {
		P[j,] <- T[j,] / colSums (T, dims = 1)
		tau[j] <- mean(P[j,], na.rm=TRUE)

		# Given the observed data, as well as the latent variables, what are the population parameters?
		mu[j] <- sum((P[j,] * x), na.rm=TRUE) / sum(P[j,], na.rm=TRUE)
	}

	# print(mu)
}

f_points <- colSums(T, dims=1)
print(f_points)