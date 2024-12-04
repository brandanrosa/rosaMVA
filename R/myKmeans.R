#' myKmeans
#'
#' @param df data frame
#' @param k number of clusters
#' @param x x variable
#' @param y y variable
#' @param z for table information
#'
#' @return a plot of the kmeans clusters and tables of counts for each cluster
#' @export
#'
#' @importFrom dplyr transmute
#' @importFrom magrittr  %>%
#' @importFrom rlang .data
#'
#' @examples \dontrun{myKmeans(ddt, 2, LENGTH, WEIGHT, SPECIES)}
myKmeans <- function(df, k, x, y, z) {

  df1 <- df %>%
    transmute(.data[[x]], .data[[y]])
  q <- stats::kmeans(x = df1, centers = k)

  # Plot
  plot(x = df1, pch=21, cex=2, bg=q$cluster, main = "K-Means Clustering Method")
  graphics::text(x = df1, labels = z, cex=0.5)
  graphics::points(q$centers, col=6:8, pch=8, cex=20)

  # Indices
  cc <- q$cluster
  ind1 <- which(cc == 1)
  ind2 <- which(cc == 2)
  ind3 <- which(cc == 3)

  # tables
  t1 <- table(df[ind1, z])
  round(t1/sum(t1) * 100)
  t2 <- table(df[ind2, z])
  round(t2/sum(t2) * 100)
  t3 <- table(df[ind3, z])
  round(t3/sum(t3) * 100)

  # list
  list(Table1 = t1, Table2 = t2, Table3 = t3)
}
