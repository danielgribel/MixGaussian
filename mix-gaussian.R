require(mvtnorm)

set.seed(1607)

# number of runs (number of instances generated)
nbRuns <- 1

# the range for the means: from $minMean to $maxMean
minMean <- 0
maxMean <- 25

# the range for the stdev: from $minCov to $maxCov
minCov <- 1
maxCov <- 10

# degree of separability
c <- 1.0

# number of clusters (distributions)
m <- 10

# number of features
d <- 2

# number of data points
nbPoints <- 1000

# number of points per cluster (distribution) -- or cardinality of each group
Q <- nbPoints/m

eps <- 0.02

it <- 1
nbValild <- 0

labels <- c()

stt <- 1
end <- Q

for(i in 1:m) {
	labels[stt:end] <- i
	stt <- end + 1
	end <- end + Q
}

# while(it <= nbRuns && nbValild < 10) {
while(it <= nbRuns) {
	means = matrix(nrow=d, ncol=m)
	stdev = matrix(nrow=d, ncol=m)
	data <- matrix(nrow=m*Q, ncol=d)

	# randomly generate the standard deviation for each cluster (gaussian distribution)
	st <- sample(seq(from = minCov, to = maxCov), size = m, replace = TRUE)
	
	q <- 1

	for (i in 1:m) {
		# randomly generate the means for each cluster (gaussian distribution) and each feature inside the cluster
		means[,i] <- sample(seq(from = minMean, to = maxMean), size = d, replace = TRUE)
		
		# generate the same standard dev for each feature of the gaussian distribution
		stdev[,i] <- runif(d, st[i], st[i])
		
		# stdev[,i] <- sample(seq(from = maxCov, to = maxCov), size = d, replace = TRUE) # standard dev for gaussian distributions
		
		# generate the gaussian distribution with means and co-variance matrix
		P <- rmvnorm(Q, means[,i], diag(stdev[,i], d))
		
		# append the points generated in each gaussian to the entire dataset
		for(q in 1:Q) {
			data[(i-1)*Q + q,] <- P[q,]	
		}
	}

	# get min and max for 1st and 2nd features to generate the plot
	xmin <- min(data[,1])
	xmax <- max(data[,1])
	ymin <- min(data[,2])
	ymax <- max(data[,2])

	plot(data, xlim = c(xmin, xmax), ylim = c(ymin, ymax), pch = 19, col = "gray40")

	# generate all pairs by indeces
	pairs <- combn(m, 2)
	
	cont <- 0
	contNext <- 0

	# for each pair check if they are c-separable
	for(j in 1:ncol(pairs)) {
		
		a <- pairs[,j][1]
		b <- pairs[,j][2]

		# get the max eigenvalue of the co-variance matrix of cluster 1
		lambda1 <- max(eigen(diag(stdev[,a], d))$values)

		# get the max eigenvalue of the co-variance matrix of cluster 2
		lambda2 <- max(eigen(diag(stdev[,b], d))$values)
		
		maxLambda <- max(lambda1, lambda2)

		# left side of the c-separability inequality
		leftSide <- sqrt(sum((means[,a] - means[,b]) ^ 2))
		
		# right side of the c-separability inequality
		rigthSide <- c * sqrt(d * maxLambda)
		
		# right side of the next c-separability inequality
		nextRigthSize <- (c+0.5) * sqrt(d * maxLambda)

		# check if pairs are c-separable
		if(leftSide >= rigthSide) {
			cont = cont+1
			# check if the pairs are c-separable also for the next c
			if(leftSide >= nextRigthSize) {
				contNext = contNext + 1
			}
		}
	}

	# get the proportion of pairs accomplishing the c-separability for current and next degree
	result <- c(1.0*cont/ncol(pairs), 1.0*contNext/ncol(pairs))

	if(result[1] + eps > 1 && result[2] < 1) {
		# save file
		nbValild <- nbValild + 1
		
		outputFile <- "~/src/hg/hg-means/data/gaussian/G"
		labelsFile <- "~/src/hg/hg-means/labels/gaussian/G"

		cStr <- toString(c)
		cStr <- gsub("\\.", "", cStr)
		
		fileDesc <- ""
		fileDesc <- paste(fileDesc, toString(m), sep="-")
		fileDesc <- paste(fileDesc, toString(d), sep="-")
		fileDesc <- paste(fileDesc, toString(nbValild), sep="-")
		fileDesc <- paste(fileDesc, ".txt", sep="")

		outputFile <- paste(outputFile, fileDesc, sep="")
		labelsFile <- paste(labelsFile, fileDesc, sep="")

		# Write data to file
		# write.table(paste(toString(nbPoints), toString(d)), file = outputFile, sep = " ", quote = FALSE, row.names = FALSE, col.names = FALSE)
		# write.table(data, file = outputFile, sep = " ", row.names = FALSE, col.names = FALSE, append=TRUE)

		# Write labels to file
		# write.table(labels, file = labelsFile, sep = " ", row.names = FALSE, col.names = FALSE)
		# print(paste("Saving", outputFile , sep=" "))
	}

	it <- it+1
	# print the proportion of pairs of clusters that are c-separable and (c+0.5)-separable
	print(result)
}