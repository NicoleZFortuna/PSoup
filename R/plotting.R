#' A function to quickly plot the through time outcome of a simulation
#'
#' @param simulationData the data.frame output of a simulation.
#' @importFrom viridis viridis

quickPlot <- function(simulationData) {
  plot(NA, ylim = c(0, max(simulationData)+0.5), xlim = c(0, nrow(simulationData)),
       xlab = "Time", ylab = "Expression")
  cols = viridis(ncol(simulationData))
  for (i in 1:ncol(simulationData)) {
    lines(simulationData[, i], col = cols[i])
  }
}



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

#plot.simData <- function(simData, toPlot = NA)
