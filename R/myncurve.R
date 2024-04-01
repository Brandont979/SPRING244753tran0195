#' Generate a normal curve
#'
#' @param a probability limit
#' @param mu mean
#' @param sigma standard deviation
#'
#' @return a graph of the normal curve using the function
#' @export
#'
#' @examples
#' myncurve(1, 0, 1)
#'
myncurve = function(a, mu, sigma){
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3*sigma, mu + 3*sigma))

  xnorm = seq(0, a, length = 1000)
  ynorm = dnorm(xnorm, mu, sigma)

  polygon(c(0, xnorm, a), c(0, ynorm, 0), col = "Blue")

  normprob = round(pnorm(a, mu, sigma) - pnorm(0, mu, sigma), 4)

  list(mu = mu, sigma = sigma, normprob = normprob)
}
