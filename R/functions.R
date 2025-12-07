#funciones a implementar

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

#' confidence interval for mean, known variance, normal distribution
#' @param n size of the sample
#' @param mean mean of the sample
#' @param var variance of the ENTIRE POPULATION (NOT SAMPLE)
#' @param a confidence interval we want, for 95% confidence, a = 0.05, for 90% confidence, a = 0.1
#' @return print confidence interval for the ACTUAL p in the distribution
#'
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
#'
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
#'
confidence_normalVariance <- function(n, S, a){
  df <- n-1
  chiLow <- qchisq(a/2, df, lower.tail = TRUE)
  chiHigh <- qchisq(a/2, df, lower.tail = FALSE)
  e <- df*S^2
  print(e/chiLow)
  print(e/chiHigh)
}

#' Confidence interval for p, binomial distribution
#' @param n size of the sample
#' @param phat probability of succes in the sample
#' @param a confidence interval we want, for 95% confidence, a = 0.05, for 90% confidence, a = 0.1
#' @return print confidence interval for the ACTUAL p in the distribution
#'
confidence_binomial <- function(n, phat, a){
  z <- qnorm(a/2, lower.tail = FALSE)
  e <- z*sqrt((phat*(1-phat))/n)
  print(phat - e)
  print(phat + e)
}

#confidence interval for difference of means, known variance
#confidence interval for difference of means, unknown variance, n1+n2 > 30,
#confidence interval for difference of means, unknown variance, n1+n2 < 30, equal sample var
#confidence interval for difference of means, unknown variance, n1+n2 < 30, diff sample var

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
  (choose(n,x))(p^x)((1-p)^(n-x))
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
  for(i in (x+include):n+1){
    sum <- sum + (choose(n,i))(p^i)((1-p)^(n-i))
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
  for(i in 0:x+include){
    sum <- sum + (choose(n,i))(p^i)((1-p)^(n-i))
  }
  sum
}


