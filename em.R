set.seed(1607)

nbPoints <- 1000

nbClusters <- 3

y_alphas <- runif(nbClusters)

y_alphas <- y_alphas/sum(y_alphas)

y_means <- sample.int(10, nbClusters, replace = FALSE)

x <- rep(0, nbPoints)

s <- sample(seq(1:nbClusters), size = nbPoints, replace = TRUE, prob = y_alphas)

for( i in 1:nbPoints ) {
	x[i] <- rnorm(1, mean = y_means[s[i]])
}

# simulating an outlier
x[1] <- 50.0

# initial guess for distributions means
means <- sample.int(10, nbClusters, replace = FALSE)

# initial guess for proportion of distributions (guess for alphas)
alphas <- rep(1.0/nbClusters, nbClusters)

T <- array(dim = c(nbClusters, nbPoints))
P <- array(dim = c(nbClusters, nbPoints))

for(i in 1:100) {
	
	# Given the observed data, as well as the distribution parameters, what are the latent variables?
	for(j in 1:nbClusters) {
		T[j,] <- alphas[j] * dnorm( x, means[j] )
	}

	for(j in 1:nbClusters) {
		P[j,] <- T[j,] / colSums (T, dims = 1)
		alphas[j] <- mean(P[j,], na.rm=TRUE)

		# Given the observed data, as well as the latent variables, what are the population parameters?
		means[j] <- sum((P[j,] * x), na.rm=TRUE) / sum(P[j,], na.rm=TRUE)
	}

	# print(means)
}

f_points <- colSums(T, dims=1)
print(f_points)