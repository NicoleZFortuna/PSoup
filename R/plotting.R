#' #' A function to draw arrows when a relationship includes many interacting
#' #' objects (for either the origin or the destination)/
#' #'
#' #' @param x0 a vector containing the x coordinate of originating objects.
#' #'           should have as many x-values as there are originating objects.
#' #' @param x1 a vector containing the x coordinate of destination objects.
#' #'           should have as many x-values as there are destination objects.
#' #' @param y0 a vector containing the y coordinate of originating objects.
#' #'           should have as many y-values as there are originating objects.
#' #' @param y1 a vector containing the y coordinate of destination objects.
#' #'           should have as many y-values as there are destination objects.
#' #' @param arr.type type of arrowhead to draw, one of "none", "simple",
#' #'           "curved", "triangle", "circle", "ellipse" or "T".
#' #' @importFrom graphics curve
#' #' @importFrom graphics arrows
#'
#' arrowPlus <- function(x0, x1, y0, y1, arr.type = 0) {
#'   if (!length(x0)==length(y0)) stop("Unequal origin coordinates!")
#'   if (!length(x1)==length(y1)) stop("Unequal destination coordinates!")
#'
#'   for (i in 1:length(x0)) {
#'     for (j in 1:length(x1)) {
#'       to <- x0[i] + (x1[j]-x0[i])/2
#'       r <- (y1[j] - y0[i])/(to-x0[i])
#'
#'       if (y1[j] > y0[i]) {
#'         curve(y0 - sqrt(r^2 - (x - to)^2), x0, to, arr.type = arr.type)
#'       } else if (y1[j] < y0[i]) {
#'         curve(y0 - sqrt(r^2 - (x - to)^2), x0, to, arr.type = arr.type)
#'       } else {
#'         stop("Origin and destination have the same height. If this is correct, call the arrows function instead.")
#'       }
#'     }
#'   }
#'
#'   arrows(to, y0, x1, y1, arr.type = arr.type)
#' }
#'
#' #' An S3 method for printing diagrams of the inputs and outputs of an
#' #' object of class hormone.
#' #'
#' #' @param x an object of class hormone.
#' #' @importFrom graphics polygon
#' #' @importFrom graphics par
#' #' @importFrom graphics text
#'
#' plot.Hormone <-function(x, ...) {
#'   for (i in 1:nrow(x@inputs)) { # calculating the number of y-axis partitions
#'     if (i == 1) { ObjectNo <- nrow(x@inputs) - 1 }
#'     else { ObjectNo <- ObjectNo + nrow(x@inputs) - 1}
#'   }
#'
#'   par(mar=c (0,0,0,0))
#'
#'   if (ObjectNo < 5) {
#'     plot(NA, xlim=c(-10, 10), ylim=c(-10, 10), ylab="", xlab="")
#'
#'     yOrigin <- seq(-10, 10, 20/(ObjectNo+1))[2:(ObjectNo+1)]
#'   }
#'
#'   xOrigin <- seq(-10, 10, 20/4)[2:4]
#'
#'   polygon(c(-2, -2, 2, 2), c(-1.5, 1.5, 1.5, -1.5), col = "cadetblue1")
#'   text(0,0,labels=x$Name)
#'
#'   pos<- 20/(nrow(x@inputs)) * 1:length(x$Input[[1]][-1])
#'   for (i in 1:length(x$Input[[1]][-1])) {
#'     polygon(c(-8, -8, -4, -4),
#'             c(-11.5+pos[i], -8.5+pos[i], -8.5+pos[i], -11.5+pos[i]), col = "coral")
#'     text(-6, -10+pos[i], x$Input[[1]][i+1])
#'
#'   }
#'   polygon(c(-2, -2, 2, 2), c(-1.5, 1.5, 1.5, -1.5), col = "cadetblue1")
#'   text(0,0,labels=x$Name)
#' }
#'
