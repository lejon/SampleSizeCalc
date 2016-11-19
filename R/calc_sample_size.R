

#' sample_size_logistic_regression
#'
#' Conservative estimate of number of samples needed for logistic regression
#'
#' @param k the number of independent variables
#' @param p the percentage of samples for the lowest occuring class
#'
#' @return the number of samples needed
#' @export
#'
#' @examples
#' sample_size_logistic_regression(20,0.2)
sample_size_logistic_regression <- function(k, p) {
  10 * k / p
}

#' sample_size_multiple_regression
#'
#' Calculate the number of samples needed for mutiple linear regression
#'
#'  p-value, alpha level, or type I error rate. By convention, this value should be less than or equal to 0.05 to claim statistical significance
#' @param nr_predictors the number of independent covariates (i.e not including the target variable)
#' @param effect_size effect sizes (also called Cohens f2) of 0.02, 0.15, and 0.35 are considered small, medium, and large, respectively
#' @param r2 estimated r-squared, give either r2 or effect_size
#' @param power the desired statistical power level
#' @param alpha p-value, alpha level, or type I error rate, usually 0.1, 0.05 or 0.01
#' @param verify should the sample size be 'verified' by simulation using MCMC, this can take a long time (on the order of 15 mins)
#'
#' @return the number of samples needed
#' @export
#'
#' @importFrom MBESS ss.power.R2
#'
#' @examples
#' sample_size_multiple_regression(20,effect_size=0.15,power=.8,alpha=0.05)
#' sample_size_multiple_regression(20,r2=0.1304348,power=.8,alpha=0.05)
sample_size_multiple_regression <- function(nr_predictors, r2=0.2, effect_size=0.15, power=.8, alpha=0.05, verify=FALSE, ...) {
  if (alpha > 1 | alpha < 0)
    stop("'alpha' must lie in 0 < alpha < 1.")
  if (power > 1 | power < 0)
    stop("'power' must lie in 0 < power < 1.")
  if(!missing(r2) & !missing(effect_size))
    stop("Don't specify both 'r2' and 'effect_size' (Cohens f2).")
  if(missing(r2)) {
    MBESS::ss.power.R2(
      alpha.level = alpha,
      desired.power = power,
      Cohen.f2 = effect_size,
      p=nr_predictors,
      verify.ss = verify,...)$Necessary.Sample.Size
  } else {
    MBESS::ss.power.R2(
      alpha.level = alpha,
      desired.power = power,
      Population.R2=r2,
      p=nr_predictors,
      verify.ss = verify,...)$Necessary.Sample.Size
  }
}

#' coenf2_to_r2
#'
#'  Convert between Cohens f2 and r-squared
#'
#' @param cohensf2 value of cohens f2
#'
#' @return corresponding r-squared value
#' @export
#'
#' @examples
#' cohenf2_to_r2(0.15)
cohenf2_to_r2 <- function(cohensf2) {
  cohensf2 / (cohensf2 + 1)
}

#' r2_to_cohenf2
#'
#' Convert between r-squared and Cohens f2
#'
#' @param r2 r-squared value
#'
#' @return corresponding Cohens f2
#' @export
#'
#' @examples
#' r2_to_cohenf2(0.1304348)
r2_to_cohenf2 <- function(r2) {
  r2 / (1 - r2)
}

#' sample_size_2sided_2sample_t_test
#'
#' Calculate required sample size for a two sample two sided test
#'
#' @return required number of samples \strong{per group}
#' @export
#' @importFrom pwr pwr.t.test
#'
#' @examples
#' sample_size_2sided_2sample_t_test(0.5,.8,0.05)
sample_size_2sided_2sample_t_test <- function(cohensd, power, alpha) {
  ceiling(pwr::pwr.t.test(d=cohensd,power=power,sig.level=alpha,type="two.sample",alternative="two.sided")$n)
}

#' sample_size_1sided_2sample_t_test
#'
#' Calculate required sample size for a two sample one sided test
#'
#' @return required number of samples \strong{per group}
#' @export
#' @importFrom pwr pwr.t.test
#'
#' @examples
#' sample_size_1sided_2sample_t_test(0.5,.8,0.05)
sample_size_1sided_2sample_t_test <- function(cohensd, power, alpha) {
  ceiling(pwr::pwr.t.test(d=cohensd,power=power,sig.level=alpha,type="two.sample",alternative="greater")$n)
}
