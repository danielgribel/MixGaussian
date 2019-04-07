# Generate a Mixture of Gaussians distributions with fixed standard deviation in each dimension (spherical clusters)
# and calculate the C-separability index according to 'Learning mixtures of Gaussians' (S Dasgupta, 1999)
require(mvtnorm)

set.seed(4450)

# The range for components means
minMean <- 0.0
maxMean <- 5.0

# The range for components standard deviations
minCov <- 0.0
maxCov <- 1.0

# Number of Gaussian components (clusters)
m <- 5

# Number of features (dimensions)
d <- 2

# Number of data points
n <- 1000

# C-separability parameter
c <- 1.0

data <- matrix(nrow = n, ncol = d)

# Randomly generate the means for each component
means <- replicate(m, sample(seq(from = minMean, to = maxMean, by = 0.0001), size = d, replace = TRUE))

# Randomly generate the standard deviation for each component
stdev <- sample(seq(from = minCov, to = maxCov, by = 0.0001), size = m, replace = TRUE)

# Randomly choose the component that generates a data point 
label <- sample(seq(from = 1, to = m, by = 1), size = n, replace = TRUE)

for(i in 1:n) {
	# Generate a data point from corresponding component
	data[i,] <- rmvnorm(1, means[,label[i]], diag(stdev[label[i]], d))
}

# Generate all pairs for components indices
pairs <- combn(m, 2)

cSeparability <- 0

# For each pair of components check if they are C-separable according to (S Dasgupta, 1999)
for(j in 1:ncol(pairs)) {
	
    # Get index of the 1st component
	c1 <- pairs[,j][1]

    # Get index of the 2nd component
	c2 <- pairs[,j][2]
	
	# Get the max eigenvalue of the 1st co-variance
	lambda1 <- max(eigen(diag(stdev[c1], d))$values)

	# Get the max eigenvalue of the 2nd co-variance
	lambda2 <- max(eigen(diag(stdev[c2], d))$values)
	
	# Left side of the C-separability inequality
	dist <- sqrt(sum((means[,c1] - means[,c2]) ^ 2))
		
	# Right side of the C-separability inequality
	sepThreshold <- c * sqrt(d * max(lambda1, lambda2))
		
	# Check if the pair of components is C-separable
	if(dist >= sepThreshold) {
		cSeparability = cSeparability + 1
	}
}

# Percentage of pair of components satisfying the C-separability threshold
# 1.0 means that the mixture is completly separable regarding C
cSeparability <- cSeparability/ncol(pairs)

print(cSeparability)

plot(data, col = label, pch = 19)