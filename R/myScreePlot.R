#' myScreePlot
#'
#' @param df data frame
#'
#' @return a scree plot of amount of variance explained by the principal components
#' @export
#'
#' @importFrom ggplot2 qplot geom_line geom_point title xlab ylab
#'
#' @examples \dontrun{myScreePlot(ddt)}
myScreePlot <- function(df) {

  p <- dim(df)[2]

  res <- stats::prcomp(df, scale. = TRUE)

  variance = res$sdev^2 / sum(res$sdev^2)

  # Scree plot
  qplot(c(1:p), variance) +
    geom_line(color = 'darkgreen', cex=1.5) +
    geom_point(size=4, color = 'hotpink') +
    graphics::title(main = "Scree Plot", xlab("Principal Component"), ylab("Variance Explained"))
}
