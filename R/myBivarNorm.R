#' myBivarNorm
#'
#' A function to test a bivariate data set for Univariate and Bivariate Normality
#'
#' @param df data frame
#' @param alpha alpha level for Shapiro-Wilk hypothesis test (univariate)
#' @param contour contour ellipse level
#'
#' @return Q-Q plots and histograms of both variables, a Chi-Sq Plot of ordered distances, and potential results of the question of Normality
#' @export
#'
#' @importFrom EnvStats qqPlot
#' @importFrom rcompanion plotNormalHistogram
#'
#' @examples \dontrun{myBivarNorm(df = micro, alpha = 0.05, contour = 0.5)}
myBivarNorm <- function(df, alpha = 0.05, contour = 0.5) {

  # Kill Current Graphics
  grDevices::graphics.off()

  # n & p
  n <- dim(df)[1]
  p <- dim(df)[2]

  # Define X & Y for Function
  x1 <- df[,1]
  x2 <- df[,2]

  # Univariate Test for Normality
  sX1 <- stats::shapiro.test(x1)
  sX2 <- stats::shapiro.test(x2)
  swTab <- data.frame(x1 = c(sX1$statistic, sX1$p.value), x2 = c(sX2$statistic, sX2$p.value))
  row.names(swTab) <- c("W", "p-val")
  round(swTab,4)

  resX1 <- ifelse(sX1$p.value < alpha, "Non-Normal", "Normal")
  resX2 <- ifelse(sX2$p.value < alpha, "Non-Normal", "Normal")
  resUni <- data.frame(x1=resX1, x2=resX2)

  # Multivariate Test for Normality
  X <- as.matrix(df)
  mu <- as.matrix(colMeans(X))
  sigma <- as.matrix(stats::cov(X))

  dsqd <- function(X, df) {
    t(X - mu) %*% solve(sigma) %*% (X - mu)
  }

  v <- apply(df, 1, dsqd, df = df)
  ind <- which(v > stats::qchisq(1-contour, p)) # indices of condition
  out <- df[ind,] # unusual points
  numOut <- dim(out)[1]
  pctOut <- numOut/n

  # ChiSq Check
  j <- 1:n
  dsq <- sort(v)
  qchi <- stats::qchisq((j-0.5)/n, p)
  resM <- ifelse(pctOut != stats::qchisq(1-contour, p), "Likely Non-Normal", "Likely Normal")
  resMult <- data.frame(Test=resM, PctOut=pctOut, Contour=contour, n=n)

  # Plots
  # grDevices::windows()
  graphics::par(mfrow=c(3,2))
  plotNormalHistogram(x1)
  plotNormalHistogram(x2)
  qqPlot(x1, add.line = TRUE, line.col = "blue", line.lwd = 2)
  qqPlot(x2, add.line = TRUE, line.col = "blue", line.lwd = 2)
  graphics::mtext("Univariate/Bivariate Normality Plots", side = 3, line = -2, outer = TRUE, cex = 1.5, col = 'blue')
  plot(qchi, dsq, main = "ChiSq Plot",
       xlab = "ChiSq Quantiles",
       ylab = "Ordered Squared Dist",
       pch=21, bg='hotpink', cex=1.35)

  # List
  list(UnivarResult=resUni, MultivarResult=resMult)
}
