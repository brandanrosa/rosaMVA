#' myJWdata
#'
#' A function of brevity. It streamlines where to pull the JW Data sets from.
#'
#' @param x In quotes, the name of the data set.
#'
#' @return a data set
#' @export
#'
#' @examples \dontrun{myJWdata(x = "P1-4")}
myJWdata <- function(x) {
  front <- "C:/Users/Legion T5/OneDrive/Desktop/Folder/OU Courses/math5793/Data/"
  end <- ".dat"
  utils::read.delim(paste0(front, x, end), sep = "", header = FALSE)
}
