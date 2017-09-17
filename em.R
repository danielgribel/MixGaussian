set.seed(1607)

nbPoints <- 1000

nbClusters <- 3

# real alphas (proportion of each distribution in the entire population)
y_alphas <- runif(nbClusters)
y_alphas <- y_alphas/sum(y_alphas)

# real means of the distributions -- means are random in the interval 1..10
y_means <- sample.int(10, nbClusters, replace = FALSE)

# data points
x <- rep(0, nbPoints)

# choose the distribution from which the point came according to real alphas
s <- sample(seq(1:nbClusters), size = nbPoints, replace = TRUE, prob = y_alphas)

# generates data points according to the chosen distribution
for( i in 1:nbPoints ) {
	x[i] <- rnorm(1, mean = y_means[s[i]])
}

# simulating an outlier -- out of interval 0..10
x[1] <- 50.0

# initial guess for distributions means
means <- sample.int(10, nbClusters, replace = FALSE)

# initial guess for proportion of distributions (guess for alphas)
alphas <- rep(1.0/nbClusters, nbClusters)

T <- array(dim = c(nbClusters, nbPoints))
P <- array(dim = c(nbClusters, nbPoints))

for(i in 1:100) {
	
	# given the observed data and the distribution parameters, estimate the prob of data point coming from each distribution
	for(j in 1:nbClusters) {
		T[j,] <- alphas[j] * dnorm( x, means[j] )
	}

	for(j in 1:nbClusters) {
		# normalize the density prob
		P[j,] <- T[j,] / colSums (T, dims = 1)

		# given the observed data and the density prob, estimate the population parameters (alphas and means)
		alphas[j] <- mean(P[j,], na.rm=TRUE)
		means[j] <- sum((P[j,] * x), na.rm=TRUE) / sum(P[j,], na.rm=TRUE)
	}

	# print(means)
}

f_points <- colSums(T, dims=1)
print(f_points)