# mix-gaussian
Mixture of gaussians model

mix-gaussian.R script creates mixture of gaussians instances. We can specify the following parameters:

-- nbRuns: number of instances (datasets) to be generated
-- minMean: minimum value for the mean range of the gaussians
-- maxMean: maximum value for the mean range of the gaussians
-- minCov: minimum value for the covariance range of the gaussians
-- maxCov: maximum value for the covariance range of the gaussians
-- c: degree of separability of the gaussians -- specify how overllaping the clusters are
-- m: number of clusters (distributions)
-- d: number of features (dimensionality of data)
-- nbPoints: number of data points
-- Q: number of points per cluster (cardinality of each group)

OBS 1: Requires mvtnorm package
OBS 2: Uncomment lines in the very end of the script for file writing 
