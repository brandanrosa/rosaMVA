#' myPCA
#'
#' A function which calculates the coefficients for the Primary Components, cumulative variance explained by each PC, and produces a scree plot
#'
#' @param df a numeric data frame
#'
#' @return a scree plot, the coefficients for the Primary Components, and cumulative variance explained by each PC
#' @export
#'
#' @importFrom ggplot2 qplot geom_line geom_point xlab ylab ggtitle ylim
#'
#' @examples \dontrun{myPCA(df=logMALE)}
myPCA <- function(df) {
  # n & p
  n <- dim(df)[1]
  p <- dim(df)[2]

  # Create Matrix
  X <- as.matrix(df)

  # Do the things
  xbar <- as.matrix(colMeans(X))
  S <- stats::cov(X)
  eig <- eigen(S)
  lam <- eig$values
  pc <- eig$vectors
  tot_var <- sum(lam)
  sdev <- sqrt(lam)

  plam <- c()
  for (i in 1:p) {
    plam[i] <- lam[i]/tot_var
  }
  cprop <- cumsum(plam)

  # Scree plot
  pca <- stats::prcomp(X, scale = TRUE)
  variance = pca $sdev^2 / sum(pca $sdev^2)
  g <- qplot(c(1:p), variance) +
    geom_line(color = 'navy', cex=1.15) +
    geom_point(cex=3, color = 'hotpink')+
    xlab("Principal Component") +
    ylab("Pct Variance Explained") +
    ggtitle("Scree Plot") +
    ylim(0, 1)
  print(g)

  # List
  list(PCs=round(pc,4), Variance=round(lam,4), CumPctVar=round(cprop,4))
}
