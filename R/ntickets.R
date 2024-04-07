#' ntickets function
#'
#' @param N number of seats on flight
#' @param gamma probability a plane wille be truly overbooked
#' @param p probability of a "show"
#'
#' @return A named list of nd, nc, N, p, and gamma
#' @export
#'
#' @examples
#' ntickets(200, 0.02, 0.95)
#'
ntickets <- function(N, gamma, p) {
  # Calculate discrete function
  obj_discrete <- function(n) {
    return(1 - gamma - pbinom(N, n, p))
  }

  # Calculate continuous function
  obj_continuous <- function(n) {
    return(1 - gamma - pnorm(N + 0.5, n*p, sqrt(n*p*(1 - p))))
  }

  # Optimal tickets for discrete
  nd <- seq(N, floor(N + 0.1*N), by = 1)
  discrete <- 1 - gamma - pbinom(q = N, size = nd, prob = p)
  nd <- nd[which.min(abs(discrete))]

  # Optimal tickets for continuous
  cont <- function(n) {
    1 - gamma - pnorm(N + 0.5, n*p, sqrt(n*p*(1 - p)))
  }
  continuous <- sapply(nd, cont)
  nc <- nd[which.min(abs(continuous))]

  # Print named list
  print(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))

  # Plot discrete distribution
  plot_discrete <- function() {
    n_range <- seq(N - 20, N + 40, by = 1)
    obj_values_discrete <- sapply(n_range, obj_discrete)

    plot(n_range, obj_values_discrete, type = "b", col = "blue", lty = 1, xlab = "n", ylab = "Objective",
         main = paste("Objective vs. n (nd =", nd, ", gamma =", gamma, ", N =", N, " discrete)"))
    abline(v = nd, h = 0, col = "red")
  }

  # Plot continuous distribution
  plot_continuous <- function() {
    n_range <- seq(N - 20, N + 40, by = 1)
    obj_values_continuous <- sapply(n_range, obj_continuous)

    plot(n_range, obj_values_continuous, type = "l", col = "black", lty = 1, xlab = "n", ylab = "Objective",
         main = paste("Objective vs. n (nc =", nc, ", gamma =", gamma, ", N =", N, " continuous)"))
    abline(v = nc, h = 0, col = "red")
  }

  plot_discrete()
  plot_continuous()
}
