
#funciones a implementar


#' Variance of a sample
#' @param x dataset
#' @export
variance <- function(x){
  n <- length(x)
  var(x)*((n-1)/n)
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

#' Sample mean with finite population, known variance (Marga)

#' @description
#'  This is basically for when we wnat to know the probability of the
#' sample mean being greater/lower than x
#' @param x is the value that we supose the mean sample takes (or lower/greater)
#' @param n size of the sample
#' @param m is the mean of the population (we want to know the mean of the sample)
#' @param st is the standard deviation of the population
#' @param N is the size of the population
#' @param lower TRUE(lower than x) or FALSE(greater than x)
#' @export
sampleMean_finpop_var <- function(x,m, st, n, N, lower){
  pnorm(x,m,((st/(sqrt(n)))*(sqrt((N-n)/(N-1)))), lower.tail = lower)
}


#' This is for when we want the st of the sample mean (Marga)
#' @param n size of the sample
#' @param st is the standart deviation of the population
#' @param is the size of the population
#' @export
mean_sample_what_is_the_st_of_sample <- function(n,st,p){
  ((st/(sqrt(n)))*(sqrt((p-n)/(p-1))))
}


#' Sample mean with finite population, unkown variance, n > 30 (Marga)
#' @description This is when we what to know the probability of the
#' sample mean beign greater than x
#' @param x is the value that we suposse the mean sample takes (or greater)
#' @param n size of the sample
#' @param m is the mean of the population (we want to know the mean of the sample)
#' @param s is the standard deviation of the population
#' @param N is the size of the population
#' @param lower TRUE(lower than x) or FALSE(greater than x)
#' @export
sampleMean_finpop_novar <- function(x,m,s,n,N, lower){
  pnorm(x,m,(s/(sqrt(n))*(sqrt((N-n)/(N-1)))), lower.tail = lower)
}

#sample quasivariance
#sample proportion, n > 30




#'Sample Quasivariance(S^2) distribution
#' @param n size of the sample
#' @param var variance of the population
#' @param val value that we want to be greater/lower than
#' @param lower TRUE(lower than val) or FALSE(greater than val)
#' @export
sample_Quasivariance <- function(n, var, val, lower){
  df <- n-1
  Target <- df*val/var
  pchisq(Target, df, lower.tail = lower)
}



#' Density of sample quasivariance (Marga)
#' @description
#' (Benja): no estoy muy seguro que hace esto la vd pero lo dejo por si ella la quiere para algo
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

#' Sample proportion if n>30 (Marga)
#' @param x the number of sucesses
#' @param p proportional population
#' @param n is the size of the sample(always over 30)
#' @param lower TRUE(lower than x) or FALSE(greater than x)
#' @export
sample_proportion_big_n <- function(x,n,p,lower){

  pnorm(x,p,sqrt(p*(1-p)/n), lower.tail = lower)
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
#' Confidence interval for difference of means, unknown variance
#' @param x dataset 1
#' @param y dataset 2
#' @param a confidence interval we want, for 95% confidence, a = 0.05, for 90% confidence, a = 0.1
#' @return print confidence interval for the difference of means in two normal distributions
#' @export
confidence_ratioVariances_normal <- function(x, y, a){
  var.test(x, y, conf.level = (1-a))
}

#' Confidence interval for difference of (p1 - p2) of 2 binomial distributions
#' @param p1 proportion of success of the first sample
#' @param p2 proportion of success of the second sample
#' @param n1 size of the first sample
#' @param n2 size of the second sample
#' @param a alpha of the confidence interval, for 95% confidence, a = 0.05
#' @export
confidence_differenceProportions_binomial <- function(p1, p2, n1, n2, a){
  z <- qnorm(a/2, lower.tail = FALSE)
  e <- z*sqrt((p1*(1-p1)/n1)*(p2*(1-p2)/n2))
  print(p1-p2-e)
  print(p1-p2+e)
}




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


#Hypothesis testing

#' Hypothesis test for mean of a normal distribution
#' @importFrom BSDA z.test
#' @param x is the dataset
#' @param mean0 is the value we compare the mean to
#' @param type is the alternative hypothesis: "two.sided" if H0 is = and H1 !=, "less" if H0 is > and H1 <, "greater" if H0 is < and H1 >
#' @param a is the confidence level, for confidence 95%, a = 0.05
#' @param knownVar variance of the population, if not known, leave NULL
#' @return the function prints a bunch of shit, here's how to interpret:
#'General rule: If p ≤ α → Reject H₀, If p > α → Do NOT reject H₀
#' @export
hypothesis_mean_normal <- function(x, mean0, type = c("two.sided", "less", "greater"), a = 0.05, knownVar = NULL){

  type <- match.arg(type)
  if(is.null(knownVar)){
    if(length(x) > 30){
      BSDA::z.test(x, alternative=type, mu=mean0, conf.level= (1-a))
    } else {
      t.test(x, alternative=type, mu=mean0, conf.level = (1-a))
    }
  }else{
    BSDA::z.test(x, alternative=type, mu=mean0, sigma.x=sqrt(knownVar), conf.level= (1-a))
  }
}

#' Hypothesis test for variance of a normal distribution
#'
#' @param x dataset (numeric vector)
#' @param var0 hypothesised variance σ0^2
#' @param type is the alternative hypothesis: "two.sided" if H0 is = and H1 !=, "less" if H0 is > and H1 <, "greater" if H0 is < and H1 >
#' @param a is the confidence level, for confidence 95%, a = 0.05
#' @return prints test statistic and p-value (use p vs a to decide)
#' General rule to interpret: If p ≤ a → Reject H0, if p > a → Do NOT reject H0.
#' @export
hypothesis_variance_normal <- function(x, var0,
                                       type = c("two.sided", "less", "greater"),
                                       a = 0.05) {

  type <- match.arg(type)
  n   <- length(x)
  s2  <- var(x)
  df  <- n - 1

  # Test statistic (n-1)s^2 / var0 ~ chi^2_(n-1) under H0
  chi <- df * s2 / var0

  # p-value depending on alternative
  if (type == "less") {
    # H1: σ^2 < σ0^2  → lower tail
    pval <- pchisq(chi, df = df)
  } else if (type == "greater") {
    # H1: σ^2 > σ0^2  → upper tail
    pval <- 1 - pchisq(chi, df = df)
  } else {
    # two-sided: H1: σ^2 ≠ σ0^2
    lower_tail  <- pchisq(chi, df = df)
    upper_tail  <- 1 - lower_tail
    pval <- 2 * min(lower_tail, upper_tail)
  }
}

#' Hypothesis test for a binomial proportion
#' @param x number of successes
#' @param n number of trials
#' @param p0 hypothesized proportion
#' @param type is the alternative hypothesis: "two.sided" if H0 is = and H1 !=, "less" if H0 is > and H1 <, "greater" if H0 is < and H1 >
#' @param a is the confidence level, for confidence 95%, a = 0.05
#' @export
hypothesis_proportion <- function(x, n, p0,
                                  type = c("two.sided", "less", "greater"),
                                  a = 0.05) {

  type <- match.arg(type)
  phat <- x / n
  se <- sqrt(p0 * (1 - p0) / n)
  z <- (phat - p0) / se

  if (type == "less") {
    p <- pnorm(z)
  } else if (type == "greater") {
    p <- 1 - pnorm(z)
  } else {
    p <- 2 * min(pnorm(z), 1 - pnorm(z))
  }

}

#' Hypothesis test for mean of two samples
#' @importFrom BSDA z.test
#' @param x first dataset
#' @param y second dataset
#' @param type is the alternative hypothesis: "two.sided" if H0 is = and H1 !=, "less" if H0 is > and H1 <, "greater" if H0 is < and H1 >
#' @param a is the confidence level, for confidence 95%, a = 0.05
#' @param knownVar1 population variance of x (NULL if unknown)
#' @param knownVar2 population variance of y (NULL if unknown)
#' @param equalVar assume equal variances? (NULL = auto-detect)
#' @return test output
#' @export
hypothesis_TwoMeans_normal <- function(x, y,
                                    type = c("two.sided", "less", "greater"),
                                    a = 0.05,
                                    knownVar1 = NULL,
                                    knownVar2 = NULL) {

  type <- match.arg(type)
  n1 <- length(x); n2 <- length(y)

  # 1) CASE: KNOWN VARIANCES → Z-TEST
  if(!is.null(knownVar1) && !is.null(knownVar2)) {
    return(
      BSDA::z.test(
        x, y,
        alternative = type,
        sigma.x = sqrt(knownVar1),
        sigma.y = sqrt(knownVar2),
        conf.level = 1 - a
      )
    )
  }

  # 2) CASE: UNKNOWN VARIANCES, LARGE SAMPLES → Z-TEST
  if(n1 + n2 > 30) {
    return(
      BSDA::z.test(
        x, y,
        alternative = type,
        sigma.x = sd(x),
        sigma.y = sd(y),
        conf.level = 1 - a
      )
    )
  }

  # 3) SMALL SAMPLES → ALWAYS USE WELCH
  t.test(
    x, y,
    alternative = type,
    var.equal = FALSE,
    conf.level = 1 - a
  )
}

#' Hypothesis test for equality of variances (two normal pops)
#' @param x is the first dataset
#' @param y is the second dataset
#' @param type is the alternative hypothesis: "two.sided" if H0 is = and H1 !=, "less" if H0 is > and H1 <, "greater" if H0 is < and H1 >
#' @param a is the confidence level, for confidence 95%, a = 0.05
#' @export
hypothesis_TwoVariances <- function(x, y,
                                 type = c("two.sided", "less", "greater"),
                                 a = 0.05) {

  type <- match.arg(type)

  var.test(x, y,
           alternative = type,
           conf.level = 1 - a)
}








