#' myFPC
#'
#' A function which will produce the factor analysis data for principal components and max likelihood methods
#'
#' @param sigma covariance matrix
#' @param m number of factors
#'
#' @return a list of factor loadings and cumulative variance accounted for by each factor
#' @export
#'
#' @examples \dontrun{myFPC(sigma = S, m = 4)}
myFPC <- function(sigma, m) {

  # n & p
  n <- dim(sigma)[1]
  p <- dim(sigma)[2]

  # Eigenvalues / Eigenvectors
  eig <- eigen(sigma)
  lam <- eig$values
  ei <- eig$vectors

  # L
  L <- matrix(NA, nrow = length(ei[,1]), ncol = m)
  for (i in 1:m) {
    L[,i] <- sqrt(lam[i]) * ei[,i]
  }

  # Cumulative Proportion
  plam <- c()
  for (j in 1:p) {
    plam[j] <- lam[j]/p
  }
  cprop <- cumsum(plam)

  # Max Likelihood Method
  ML <- stats::factanal(covmat = sigma, factors = m, rotation = "none")

  # List
  list(L=L, cprop=cprop, ML=ML)
}
