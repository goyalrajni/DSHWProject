

# Function: bootMeanFunction()
# Inputs:   n = number of samples collect
#           nSimulations = number of bootstrap sample simulations
#           distType = specifies distribution type 
# Outputs:  xbar = norm distribution sample mean
#           sdSample = norm distribution sample standard deviation
#           bootmean = vector of bootstrap sample means
#           print - prints random sample summary statistics
#           plots - plots histogram of sample data
# Descrip: Receives number and type of samples to be generated and number of bootstrap samples to
#          be collected, produces bootstrap samples, and returns the sample mean, sample standard 
#          deviation, and bootstrap sample means.


bootMeanFunction <- function(n, nSimulations, distType) {
  
  # We use conditional statement here to collect samples based on normal or exponential distribution
  # and generate histograms of sample data
  if(distType == "r") {
    sample <- rnorm(n, 50, 4)
    hist(sample, main = paste("RNORM Sample Size of", n), col = "blue")
  }
  else if(distType == "e") {
    sample <- rexp(n)
    hist(sample, main = paste("REXP Sample Size of", n), col = "blue")
  }
  
  # We assign sample standard deviation and mean to variables here
  sdSample <- sd(sample)
  xbar <- mean(sample)
  
  # Prints random sample summary results 
  cat("Random Normal sample summary statistics:\n")
  print(summary(sample))
  
  # Initializes bootmean vector as numeric type of size 'nSimulations'
  bootmean <- numeric(nSimulations)
  
  # Run bootstrap sequence and save bootsample means to bootmean vector
  for (i in 1:nSimulations){
    temp <- sample(sample, size = length(sample), replace = TRUE)
    bootmean[i] <- mean(temp)
  }
  
  # Return sample mean, standard deviation, and vector of bootsrap means here using c vector
  return(c(xbar,sdSample,bootmean))
}


# Function: analyze()
# Inputs:   N = number of sample data to collect
#           repeats = number of bootstrap sample repeats
#           DistTYPE = distribution type specifier
# Outputs:  print(summary(BOOTMean)) - prints random sample's summary statistics
#           print - prints bootstrap sample's summary statistics
#           print - prints bootstrap sample's standard deviation
#           print - prints standard error of the sample mean
#           plots - plots histogram of bootstrap means
#
# Descrip: Receives number and type of samples to be generated and number of bootstrap samples to be
#          collected, and passes these values to the bootMeanFunction() function.Processes bootMeanFunction()
#          returned values and produces bootstrap mean summary statistics, and standard error of
#          the sample mean. Additionally, bootstrap mean histograms are plotted for comparison to sample
#          histograms and random sample's and bootstrap sample's standard deviations are displayed.

analyze <- function(N, repeats, DistType) {
  
  # Obtain sample mean, sample sd, and bootstrap mean from random distribution
  funcReturn <- bootMeanFunction(N, repeats, DistType)
  sampleMean <- funcReturn[1]
  sampleSD <- funcReturn[2]
  BootNorm <- funcReturn[-1:-2]
  
  # Prints bootstrap mean summary results here
  cat("Bootstrap sample's summary statistics:\n")
  print(summary(BootNorm))
  
  # Run conditional statement to print correct histogram here
  if(DistType == distNorm) {
    hist(BootNorm, main = paste("Bootstrapped RNORM Sample Size of", N),
         xlab = "Bootstrap sample", col = "yellow")
  }
  else if(DistType == distExp) {
    hist(BootNorm, main = paste("Bootstrapped REXP Sample Size of", N),
         xlab = "Bootstrap sample", col = "yellow")
  }
  # Add sample mean and bootstrap mean of means lines for comparison in bootstrap histogram
  abline(v = sampleMean, col = "blue", lwd = 2)
  abline(v = mean(BootNorm), col = "green", lwd = 2, lty = 3)
  
  # Print random sample and bootstrap standard deviations and standard error
  print(paste("Standard Deviation of Random Normal Samples  =",round(sampleSD,4)))
  print(paste("Standard Deviation of Bootstrap Means =",round(sd(BootNorm),4)))
}

  ### Bootstrapping Setup:
  ##### I'm initializing my variables to collect sample sizes of 50 and 500 for both random normal and random exponential sampling to see how different sample sizes perform when they're bootstrapped. For bootsrap sample collection, I've chosen my repeats to be 2000 bootstrap samples.

# Initializing the variables
repeats <- 2000 # Number of bootstrap samples to collect
distNorm <- "r" # Random Normal distribution selection
distExp <- "e" # Exponential distribution selection
sample1 <- 50
sample2 <- 500

  
  ### Normal Distribution Outputs - Sample Size of 50

## Set graphical parameters for histograms
par(mfrow = c(1, 2), cex.main = 0.8)  # 2 rows and 2 columns with title text shrink by 20%

## Normal Distribution 50 Samples - Demonstration of Central Limit Theorem
analyze(sample1, repeats, distNorm)

  
  ### Normal Distribution Outputs - Sample Size of 500

## Set graphical parameters for histograms
par(mfrow = c(1, 2), cex.main = 0.8)  # 2 rows and 2 columns with title text shrink by 20%

## Normal Distribution 500 Samples - Demonstration of Central Limit Theorem
analyze(sample2, repeats, distNorm)

  
  ### Exponential Distribution Outputs - Sample Size of 50

## Set graphical parameters for histograms
par(mfrow = c(1, 2), cex.main = 0.8)  # 2 rows and 2 columns with title text shrink by 20%

## Exponential Distribution 50 Samples - Demonstration of Central Limit Theorem
analyze(sample1, repeats, distExp)

  
  ### Exponential Distribution Outputs - Sample Size of 500

## Set graphical parameters for histograms
par(mfrow = c(1, 2), cex.main = 0.8)  # 2 rows and 2 columns with title text shrink by 20%

## Exponential Distribution 500 Samples - Demonstration of Central Limit Theorem
analyze(sample2, repeats, distExp)

