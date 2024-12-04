#' myClusterF
#'
#' A function which produces dendrogram clusterings for Single, Complete, and Average Linkages
#'
#' @param df data frame
#' @param Sim are the values similarities or differences
#'
#' @return a plot of three dendrograms for the three linkages and a list of the heights
#' @export
#'
#' @examples \dontrun{myClusterF(df = LANGUAGES)}
myClusterF <- function(df, Sim = FALSE) {

  # Calc correlation matrix, if needed for Similarity Analyses
  R <- stats::cor(df)

  # Ddist
  ifelse(Sim == FALSE, Ddist <- stats::dist(df), Ddist <- stats::dist(1-R))
  # Ddist <- as.dist(df)

  # Single Linkage
  outS <- stats::hclust(d = Ddist, method = "single")
  hS <- outS$height

  # Complete Linkage
  outC <- stats::hclust(d = Ddist, method = "complete")
  hC <- outC$height

  # Average Linkage
  outA <- stats::hclust(d = Ddist, method = "average")
  hA <- outA$height

  # Plot Dendrograms
  graphics::par(mfrow = c(2,2))
  plot(outS, hang = -1, main = "Single Linkage", col = 'darkblue')
  plot(outC, hang = -1, main = "Complete Linkage", col = 'hotpink')
  plot(outA, hang = -1, main = "Average Linkage", col = 'darkgreen')

  # Compare Heights of Different Methods
  ht <- data.frame(hS, hC, hA)
  names(ht) <- c("SingleHT", "CompleteHT", "AverageHT")

  # List
  list(Heights=round(ht,3))
}
