#funciones a implementar


#' Variance of a sample
#' @param x dataset
#' @export
variance <- function(x){
  sum <- 0
  for(xi in x){
    sum <- sum + xi^2
  }
  sum <- sum/length(x)
  sum - mean(x)^2
}



#' Sample mean with infinite population, known variance
#'
#' @param m media
#' @param var variance of the full population
#' @param n size of the sample.
#' @param val value that the mean has to be greater/lower than
#' @param lower TRUE(lower than val) or FALSE(greater than val)
#' @return probability that the mean of a sample is greater than `val`
#' @examples
#' sampleMean_ifpop_var(6, 4, 25, 6.3, TRUE)
#' @export
sampleMean_ifpop_var <- function(m, var, n, val, lower){
  std <- sqrt(var)
  normalstd <- std/sqrt(n)
  pnorm(val, m, normalstd, lower.tail = lower)
}


#' Sample mean with infinite population, unknown variance
#'
#' @param m media
#' @param S standard deviation of the sample
#' @param n size of the sample.
#' @param val value that the mean has to be greater/lower than
#' @param lower TRUE(lower than val) or FALSE(greater than val)
#' @return probability that the mean of a sample is greater/lower than `val`
#' @examples
#' sampleMean_ifpop_novar(6, 4, 25, 6.3, TRUE)
#'
#' exercise example
#' sampleMean_ifpop_novar(4, 25, 8, TRUE), probability that the mean of
#' the sample is lower than 8 with std = 4 n = 25
#'
#' Calculate the probability that the mean of the sample does not differ from
#' the population mean by more than 8 units, with std = 10.85 and n = 16
#'
#' probMoreThanMinus8 <- sampleMean_ifpop_novar(10.85, 16, -8, FALSE)
#' probMoreThan8 <- sampleMean_ifpop_novar(10.85, 16, 8, FALSE)
#' probInBetween <- probMoreThan8 - probMoreThanMinus8
#' @export
sampleMean_ifpop_novar <- function(m, S, n, val, lower){
  if(n < 30){
    #t = (sampleMean - mean)/(sampleSigma/sqrt(n))

    #degrees of freedom
    df <- n-1
    t <- val/(S/sqrt(n))
    #cumulative probability
    pt(t, df, lower.tail = lower)
  }else{
    normalstd <- S/sqrt(n)
    pnorm(val, m, normalstd, lower.tail = lower)
  }

}

#sample mean with finite population, known variance
#sample mean with finite population, unkown variance, n > 30

#sample quasivariance

#sample proportion, n > 30

#' Sample mean with finite population, known variance
#' @param x is the value that we suposse the mean sample takes (or greater)
#' @param n size of the sample
#' @param m is the mean of the population (we want to know the mean of the sample)
#' @param st is the standart deviation of the population
#' @param p is the size of the population
#' @export
sampleMean_finpop_var <- function(x,m, st, n, p){
  dnorm(x,m,((st/(sqrt(n)))*(sqrt((p-n)/(p-1)))))
}

#' Sample mean with finite population, unknown variance and large n
#' @param x is the value that we suposse the mean sample takes (or greater)
#' @param n size of the sample
#' @param m is the mean of the population (we want to know the mean of the sample)
#' @param s is the desviacion muestral (ni idea de como era eso en inglés)
#' @param p is the size of the population
#' @export
sampleMean_finpop_novar <- function(x,m,s,n,p){
  dnorm(x,m,((s/sqrt(n))*(sqrt(p-n/p-1))))
}


#'Sample Quasivariance(S^2) distribution
#density
#' @param n are the grades of liberty for all sample quasivariance
#' @param x is the value we want to knoe it's density
#' @param type "density"=dquisq, "acumulative"=pshisq, "cuantil"= qchisq
#' @export
density_chisq<- function(x,n,type = c("density", "acumulative","cuantil")){
  if(type == "density"){
    return(dchisq(x,n))
  } else if(type == "cdf"){
    return(pchisq(x,n))
  } else { # cuantil
    return(qchisq(x,n))
  }

}

#'Sample proportion if n>30
#' @param is the number of sucesses
#' @param p proporcional population
#' @param n is the size of the sample(always over 30)
#' @param type  "density" = dnorm, "cdf" = pnorm, "upper" = 1 - pnorm
#' @export
sample_proportion_big_n <- function(x,n,p,type = c("density", "cdf", "upper")){
  se<- sqrt(p*(1-p)/n)

  if(type == "density"){
    return(dnorm(x, mean = p, sd = se))
  } else if(type == "cdf"){
    return(pnorm(x, mean = p, sd = se))
  } else { # upper tail
    return(1 - pnorm(x, mean = p, sd = se))
  }
}








#' confidence interval for mean, known variance, normal distribution
#' @param n size of the sample
#' @param mean mean of the sample
#' @param var variance of the ENTIRE POPULATION (NOT SAMPLE)
#' @param a confidence interval we want, for 95% confidence, a = 0.05, for 90% confidence, a = 0.1
#' @return print confidence interval for the ACTUAL mean in the distribution
#' @export
confidence_normalMean_var <- function(n, mean, var, a){
  z <- qnorm(a/2, lower.tail = FALSE)
  e <- z*sqrt(var/n)
  print(mean - e)
  print(mean + e)
}
#' confidence interval for mean, unknown variance
#' @param n size of the sample
#' @param mean mean of the sample
#' @param S standard deviation of the sample
#' @param a confidence interval we want, for 95% confidence, a = 0.05, for 90% confidence, a = 0.1
#' @return print confidence interval for the ACTUAL mean in the distribution
#' @export
confidence_normalMean_novar <- function(n, mean, S, a){
  if(n > 30){
    z <- qnorm(a/2, lower.tail = FALSE)
    e <- z*S/sqrt(n)
    print(mean - e)
    print(mean + e)
  }else{
    t <- qt(a/2, n-1, lower.tail = FALSE)
    e <- t*S/sqrt(n)
    print(mean - e)
    print(mean + e)
  }

}

#' confidence interval for variance, normal distribution
#' @param n size of the sample
#' @param S standard deviation of the sample
#' @param a confidence interval we want, for 95% confidence, a = 0.05, for 90% confidence, a = 0.1
#' @return print confidence interval for the ACTUAL variance in the distribution
#' @export
confidence_normalVariance <- function(n, S, a){
  df <- n-1
  chiLow <- qchisq(1- a/2, df)
  chiHigh <- qchisq(a/2, df)
  e <- df*S^2
  print(e/chiLow)
  print(e/chiHigh)
}

#' Confidence interval for p, binomial distribution
#' @param n size of the sample
#' @param phat probability of success in the sample
#' @param a confidence interval we want, for 95% confidence, a = 0.05, for 90% confidence, a = 0.1
#' @return print confidence interval for the ACTUAL p in the distribution
#' @export
confidence_binomial <- function(n, phat, a){
  z <- qnorm(a/2, lower.tail = FALSE)
  e <- z*sqrt((phat*(1-phat))/n)
  print(phat - e)
  print(phat + e)
}

#' Confidence interval for difference of means, known population variances, normal distributions
#' @param mean1 mean of the first sample
#' @param mean2 mean of the second sample
#' @param var1 variance of the first POPULATION (NOT SAMPLE)
#' @param var2 variance of the second POPULATION (NOT SAMPLE)
#' @param n1 size of the first sample
#' @param n2 size of the second sample
#' @param a confidence interval we want, for 95% confidence, a = 0.05, for 90% confidence, a = 0.1
#' @return print confidence interval for the difference of means in two normal distributions
#' @export
confidence_diferenceMeans_var <- function(mean1, mean2, var1, var2, n1, n2, a){
  z <- qnorm(a/2, lower.tail = FALSE)
  e <- z*sqrt((var1/n1) + (var2/n2))
  print(mean1 - mean2 - e)
  print(mean1 - mean2 + e)
}

#' Confidence interval for difference of means, unknown variance
#' @param x dataset 1
#' @param y dataset 2
#' @param a confidence interval we want, for 95% confidence, a = 0.05, for 90% confidence, a = 0.1
#' @return print confidence interval for the difference of means in two normal distributions
#' @export
confidence_diferenceMeans_novar <- function(x, y, a){
  if(n1 + n2 <= 30){
    t.test(x, y, conf.level = (1-a))
  }else{
    z <- qnorm(a/2, lower.tail = FALSE)
    e <- z*sqrt((variance(x)/length(x)) + (variance(y)/length(y)))
    print((mean(x)-mean(y)) - e)
    print((mean(x)-mean(y)) + e)
  }
}

#confidence interval for ratio of variances of 2 normal distributions

#confidence interval for difference of (p1 - p2) of 2 binomial distributions




#' Binomial probability
#'
#' @param x number of successes we want
#' @param n number of times the experiment was done
#' @param p probability of success of the experiment
#' @return probability that if we do the experiment `n` times we have `x` successes
#' @examples
#' binomial_distribution(3, 20, 0.6)
#' @export
binomial_distribution_exactly <- function(x, n, p){
  (choose(n,x))*(p^x)*((1-p)^(n-x))
}

#' Binomial probability X > x
#' @param x minimum number of successes we want
#' @param n number of times the experiment was done
#' @param p probability of success of the experiment
#' @param equal TRUE for including x (>=) FALSE to not include x (>)
#' @return probability that if we do the experiment `n` times we have `x`or more successes
#' @examples
#' binomial_distribution(3, 20, 0.6, FALSE)
#' @export
binomial_distribution_greater <- function(x, n, p, equal){
  sum <- 0
  include <- 1
  if(equal){
    include <- 0
  }
  for(i in (x+include):(n+1)){
    sum <- sum + (choose(n,i))*(p^i)*((1-p)^(n-i))
  }
  sum
}

#' Binomial probability for X < x
#' @param x maximum number of successes we want
#' @param n number of times the experiment was done
#' @param p probability of success of the experiment
#' @param equal TRUE for including x (<=) FALSE to not include x (<)
#' @return probability that if we do the experiment `n` times we have `x` or less successes
#' @examples
#' binomial_distribution(3, 20, 0.6, FALSE)
#' @export
binomial_distribution_lower <- function(x, n, p, equal){
  sum <- 0
  include <- 0
  if(equal){
    include <- 1
  }
  for(i in 0:(x+include)){
    sum <- sum + (choose(n,i))*(p^i)*((1-p)^(n-i))
  }
  sum
}

