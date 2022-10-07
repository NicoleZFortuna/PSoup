#' A function to quickly plot the through time outcome of a simulation
#'
#' @param simulationData the data.frame output of a simulation.
#' @importFrom viridis viridis
#' @importFrom graphics lines

quickPlot <- function(simulationData, ...) {
 plot(NA, ylim = c(0, max(simulationData) + 0.5), xlim = c(0, nrow(simulationData)),
      xlab = "Time", ylab = "Expression", ...)
 cols = viridis(ncol(simulationData))
 for (i in 1:ncol(simulationData)) {
   lines(simulationData[, i], col = cols[i])
 }
}

#' A function to pull the final states from a set of simulations and normalise
#' against the wildtype (baseline).
#'
#' @param simulations a list containing the output of the setupSims function
#' @param nodes a vector containing all the nodes that the user wants to plot
#' @importFrom utils tail
#' @export

finalStates <- function(simulations, nodes = NA) {
  final <- tail(simulations[[1]]$simulation,1)
  final[2:length(simulations), ] <- NA

  for (i in 2:length(simulations)) {
    final[i, ] <- tail(simulations[[i]]$simulation,1)
    final[i, ] <- final[i, ]/final[1, ]
  }
  final[1, ] <- final[1, ]/final[1, ]

  final
}

#barplot(as.matrix(final$BranchOutgrowth), beside = T, ylab = "Expression",
#        cex.names = 0.75, names.arg = c(expression(frac("WT","WT")), expression(frac("RMS1","WT")), expression(frac("RMS1","RMS1")), expression(frac("RMS1","RMS2")), expression(frac("RMS1","RMS3")), expression(frac("RMS1","RMS4")), expression(frac("RMS1","RMS5"))))
#legend("topright", legend = c("WT", "RMS3S = 0", "RMS3S = 0.5"), pch = 15, col = viridis(3))


##' A function to comparatively plot the outcomes of different simulations
##'
##' This function allows you to quickly plot the outcome of different simulations.
##' @param WTdata a data.frame containing the outcome of the wild type (baseline)
##'        simulation. Data should be the value of nodes at the stable state.
##' @param expData a list of data.frames containing the
##' @param toPlot a vector specifying the nodes that you want to plot. The
##'        default is set to NA, in which case all node data will be plotted.
##' @param cols specifies the colours that will be used to plot node values
##'        over time. Can accept colour palettes from the viridis package.
##'        Colours can be individually selected and provided as a vector
##'        of the same length as the nodes that you wish to plot. If no
##'        value is provided will use the viridis palette as default.

# #plot.simData <- function(simData, toPlot = NA)
#
#
# file <- "/Users/uqnfortu/Desktop/equations.R"
#
# simulations <- setupSims(file, randomStart = NA, delay = 2, tmax = 50)
#
# final = finalStates(simulations)
# barplot(as.matrix(final$BranchOutgrowth), beside = T, ylab = "Expression", las=2,
#         names.arg = c("WT/WT", "RMS1/WT", "WT/RMS1", "RMS2/WT", "WT/RMS2", "RMS3/WT", "WT/RMS3", "RMS4/WT", "WT/RMS4", "RMS5/WT", "WT/RMS5"))
#
# plot(NA, ylim=c(0,6), xlim=c(0,6), xaxt = "n", yaxt = "n", xlab = "Rootstock", ylab = "Scion")
# axis(1, seq(0.5, 5.5, 1), c("WT", paste0("RMS", 1:5)))
# axis(2, seq(0.5, 5.5, 1), c("WT", paste0("RMS", 1:5)))
# x=c(1,1,2,1,3,1,4,1,5,1,6,1)-0.5
# y=c(1,2,1,3,1,4,1,5,1,5,1,6)-0.5
#
# points(x=x,y=y,
#        col = generateColours(final$BranchOutgrowth),
#        pch = 15, cex=3)

