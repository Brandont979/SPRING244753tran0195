#' Fitted line model for quad.lm
#'
#' @param x - BHDiameter value
#'
#' @return predicted height value
#' @export
#'
#' @examples
#' myplot(3)
#'
myplot=function(x){
  0.86089580 +1.46959217*x  -0.02745726*x^2
}
