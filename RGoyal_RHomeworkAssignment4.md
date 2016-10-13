# MSDS 6306: Introduction to Data Science (402) Homework-4
Rajni Goyal  
October 13, 2016  




```r
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
#              be collected, produces bootstrap samples, and returns the sample mean, sample standard 
#              deviation, and bootstrap sample means.


bootMeanFunction <- function(n, nSimulations, distType) {
    
# We use conditional statement here to collect samples based on normal or exponential distribution
# and generate histograms of sample data
    if(distType == "n") {
        sample <- rnorm(n, 50, 4)
        hist(sample, main = paste("RNORM Sample Size of", n), col = "blue")
    }
    else if(distType == "e") {
        sample <- rexp(n)
        hist(sample, main = paste("REXP Sample Size of", n), col = "blue")
    }
    
# WE assign sample standard deviation and mean to variables here
    sdSample <- sd(sample)
    xbar <- mean(sample)
    
# Prints random sample summary results 
    cat("Random sample summary statistics:\n")
    print(summary(sample))
    
# Initializes bootmean vector as numeric type of size 'nSimulations'
    bootmean <- numeric(nSimulations)
    
# Run bootstrap sequence and save bootsample means to bootmean vector
    for (i in 1:nSimulations){
        bootsample <- sample(sample, size = length(sample), replace = TRUE)
        bootmean[i] <- mean(bootsample)
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
#              collected, and passes these values to the bootMeanFunction() function. Processes bootMeanFunction()
#              returned values and produces bootstrap mean summary statistics, and standard error of
#              the sample mean. Additionally, bootstrap mean histograms are plotted for comparison to sample
#              histograms and random sample's and bootstrap sample's standard deviations are displayed.

analyze <- function(N, repeats, DistTYPE) {
    
# Obtain sample mean, sample sd, and bootstrap mean from random distribution
    funcReturn <- bootMeanFunction(N, repeats, DistTYPE)
    sampleMean <- funcReturn[1]
    sampleSD <- funcReturn[2]
    BOOTMean <- funcReturn[-1:-2]
    
# Prints bootstrap mean summary results here
    cat("Bootstrap sample's summary statistics:\n")
    print(summary(BOOTMean))
    
# Run conditional statement to print correct histogram here
    if(DistTYPE == distNorm) {
        hist(BOOTMean, main = paste("Bootstrapped RNORM Sample Size of", N),
             xlab = "Bootstrap sample", col = "yellow")
    }
    else if(DistTYPE == distExp) {
        hist(BOOTMean, main = paste("Bootstrapped REXP Sample Size of", N),
             xlab = "Bootstrap sample", col = "yellow")
    }
# Add sample mean and bootstrap mean of means lines for comparison in bootstrap histogram
    abline(v = sampleMean, col = "indianred1", lwd = 2)
    abline(v = mean(BOOTMean), col = "darkgreen", lwd = 2, lty = 3)
    
# Print random sample and bootstrap standard deviations and standard error
    print(paste("Random sample's standard deviation =",round(sampleSD,4)))
    print(paste("Bootstrap means' standard deviation =",round(sd(BOOTMean),4)))
    print(paste("Standard error of the sample mean =",round((sampleSD/sqrt(N)),4)))
}
```

<br>

### Bootstrapping Setup:
#### I chose to collect sample sizes of 50 and 500 for both random normal and random exponential sampling to see how different sample sizes perform when bootstrapped. For bootsrap sample collection, I opted to run 2000 bootstrap samples.

```r
# Initialize variables
repeats <- 2000 # Number of bootstrap samples to collect
distNorm <- "n" # Normal distribution selection
distExp <- "e" # Exponential distribution selection
sample1 <- 50
sample2 <- 500
```

<br>

### Normal Distribution Outputs - Sample Size of 50

```r
## Set graphical parameters for histograms
par(mfrow = c(1, 2), cex.main = 0.8)  # 2 rows and 2 columns with title text shrink by 20%

## Normal Distribution 100 Samples - Demonstration of Central Limit Theorem
analyze(sample1, repeats, distNorm)
```

```
## Random sample summary statistics:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   40.46   47.13   49.12   49.70   51.83   58.36 
## Bootstrap sample's summary statistics:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   48.01   49.30   49.69   49.69   50.08   51.67
```

![](RGoyal_RHomeworkAssignment4_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```
## [1] "Random sample's standard deviation = 4.0548"
## [1] "Bootstrap means' standard deviation = 0.5626"
## [1] "Standard error of the sample mean = 0.5734"
```

<br>

### Normal Distribution Outputs - Sample Size of 500

```r
## Set graphical parameters for histograms
par(mfrow = c(1, 2), cex.main = 0.8)  # 2 rows and 2 columns with title text shrink by 20%

## Normal Distribution 10 Samples - Demonstration of Central Limit Theorem
analyze(sample2, repeats, distNorm)
```

```
## Random sample summary statistics:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   37.19   47.48   49.98   50.06   52.94   61.90 
## Bootstrap sample's summary statistics:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   49.39   49.93   50.06   50.06   50.18   50.65
```

![](RGoyal_RHomeworkAssignment4_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```
## [1] "Random sample's standard deviation = 4.0332"
## [1] "Bootstrap means' standard deviation = 0.1777"
## [1] "Standard error of the sample mean = 0.1804"
```

<br>

### Exponential Distribution Outputs - Sample Size of 50

```r
## Set graphical parameters for histograms
par(mfrow = c(1, 2), cex.main = 0.8)  # 2 rows and 2 columns with title text shrink by 20%

## Exponential Distribution 100 Samples - Demonstration of Central Limit Theorem
analyze(sample1, repeats, distExp)
```

```
## Random sample summary statistics:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.01144 0.43770 0.95460 1.22700 1.72100 6.59400 
## Bootstrap sample's summary statistics:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.7606  1.1160  1.2220  1.2290  1.3260  1.9160
```

![](RGoyal_RHomeworkAssignment4_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```
## [1] "Random sample's standard deviation = 1.1653"
## [1] "Bootstrap means' standard deviation = 0.1609"
## [1] "Standard error of the sample mean = 0.1648"
```

<br>

### Exponential Distribution Outputs - Sample Size of 500

```r
## Set graphical parameters for histograms
par(mfrow = c(1, 2), cex.main = 0.8)  # 2 rows and 2 columns with title text shrink by 20%

## Exponential Distribution 10 Samples - Demonstration of Central Limit Theorem
analyze(sample2, repeats, distExp)
```

```
## Random sample summary statistics:
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000963 0.295600 0.644800 1.002000 1.329000 7.974000 
## Bootstrap sample's summary statistics:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.8448  0.9685  1.0010  1.0020  1.0350  1.1590
```

![](RGoyal_RHomeworkAssignment4_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```
## [1] "Random sample's standard deviation = 1.0751"
## [1] "Bootstrap means' standard deviation = 0.0489"
## [1] "Standard error of the sample mean = 0.0481"
```

<br>

