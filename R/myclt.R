#' myclt function
#'
#' @param n size of sample
#' @param iter number of trials
#'
#' @return a histogram of uniformed distribution generated from the parameters
#' @export
#'
#' @examples
#' myclt(3,1000)
#'
myclt = function(n, iter) {
  y = runif(n * iter, 0, 5)
  data = matrix(y, nr = n, nc = iter, byrow = TRUE)
  sm = apply(data, 2, sum)
  hist(sm)
}
