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



#' Sample mean with infinite population, unknown variance, n > 30
#'
#' @param m media
#' @param S standard deviation of the sample
#' @param n size of the sample.
#' @param val value that the mean has to be greater/lower than
#' @param lower TRUE(lower than val) or FALSE(greater than val)
#' @return probability that the mean of a sample is greater than `val`
#' @examples
#' sampleMean_ifpop_nbig(6, 4, 25, 6.3, TRUE)
#' @export
sampleMean_ifpop_nbig <- function(m, var, n, val, lower){
  normalstd <- S/sqrt(n)
  pnorm(val, m, normalstd, lower.tail = lower)
}
#' Sample mean with infinite population, unknown variance, n < 30
#'
#' @param S standard deviation of the sample
#' @param n size of the sample.
#' @param val value that the mean has to be greater/lower than
#' @param lower TRUE(lower than val) or FALSE(greater than val)
#' @return probability that the mean of a sample is greater than `val`
#' @examples
#' sampleMean_ifpop_nsmall(4, 25, 8, TRUE), probability that the mean of
#' the sample is lower than 8 with std = 4 n = 25
#'
#' Calculate the probability that the mean of the sample does not differ from
#' the population mean by more than 8 units, with std = 10.85 and n = 16
#'
#' probMoreThanMinus8 <- sampleMean_if_pop_nsmall(10.85, 16, -8, FALSE)
#' probMoreThan8 <- sampleMean_if_pop_nsmall(10.85, 16, 8, FALSE)
#' probInBetween <- probMoreThan8 - probMoreThanMinus8
#' @export
sampleMean_ifpop_nsmall <- function(S, n, val, lower){
  #t = (sampleMean - mean)/(sampleSigma/sqrt(n))

  #degrees of freedom
  df <- n-1
  t <- val/(S/sqrt(n))
  #cumulative probability
  pt(t, df, lower.tail = lower)
}
#sample mean with finite population, known variance
#sample mean with finite population, unkown variance, n > 30

#sample quasivariance

#sample proportion, n > 30

#confidence interval for mean, known variance, normal distribution
#confidence interval for mean, unknown variance, n > 30, normal distribution
#confidence interval for mean, unknown variance, n <= 30, normal distribution

#confidence interval for variance, normal distribution

#cofnidence interval por p, binomial distribution

#confidence interval for difference of means, known variance
#confidence interval for difference of means, unknown variance, n1+n2 > 30,
#confidence interval for difference of means, unknown variance, n1+n2 < 30, equal sample var
#confidence interval for difference of means, unknown variance, n1+n2 < 30, diff sample var

#confidence interval for ratio of variances of 2 normal distributions

#confidence interval for difference of (p1 - p2) of 2 binomial distributions
