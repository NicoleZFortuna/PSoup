#' A function to quickly inspect the through time outcome of a simulation
#'
#' @param simulationData the data.frame output of a simulation. A subset of the
#'        data.frame can be passed if the user does not want to plot all of the
#'        nodes.
#' @param col default is set to NA, in which case the viridis colour palette will
#'        be used. Otherwise, the user can pass a vector of colors the same length
#'        as the number of nodes to be plotted
#' @param logTransform whether the simulation values should be log transformed.
#' @param addLabels if you want to add labels next to the
#'        final value of each node.
#' @importFrom viridis viridis
#' @importFrom stats plot.ts
#' @importFrom graphics plot.new
#' @importFrom graphics plot.window
#' @importFrom graphics legend
#'
#' @export

quickPlot <- function(simulationData, col = NA,
                      logTransform = FALSE,
                      addLabels = FALSE,
                      title = NA) {
  if (unique(is.na(col))) {col = viridis(ncol(simulationData))}

  par(mfrow = c(1, 2), mar = c(4,4,1,0), xpd = T)
  if (logTransform == FALSE) {
    plot.ts(simulationData, plot.type = "single",
            col = col, ylab = "Expression")
  } else {
    plot.ts(log(simulationData), plot.type = "single",
            col = col, ylab = "Expression")
    mtext(title)
  }

  if (addLabels == T) {
    text(x = rep(nrow(simulationData) + 5, ncol(simulationData)),
         y = log(tail(simulationData, 1)),
         labels = colnames(simulationData), xpd = NA,
         adj = 0, cex = 0.8)
  }

  plot.new( )
  plot.window(ylim = c(0, 10), xlim = c(0, 10))
  legend(0, 10, legend = colnames(simulationData),
         fill = col, cex = 0.5,
         bty = "n")
}

#' A function to pull the final states from a set of simulations and normalise
#' against the wildtype (baseline).
#'
#' @param simulations a list containing the output of the setupSims function
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


#' A function to comparatively plot the outcomes of different simulations
#'
#' This function allows you to quickly plot the outcome of different simulations.
#' @param data a data.frame containing the outcome of a set of simulations. The
#'        first row should contain the wild type condition.
#' @param node a string showing which node data will be plotted.
#' @param cols specifies the colours that will be used to plot node values
#'        over time. Can accept colour palettes from the viridis package.
#'        Colours can be individually selected and provided as a vector
#'        of the same length as the nodes that you wish to plot. If no
#'        value is provided will use the viridis palette as default.
#' @param bioData a matrix containing the biological data corresponding to the
#'        simulated data.

plot.simData <- function(data, node, bioData = NA, ...) {
  vals = finalStates(data)[, node]
  conds = sapply(data, FUN = function(x) paste(colnames(x$scenario$genotype),
                                               x$scenario$genotype, sep = "."))
  conds[[1]] = "WT"

  barplot(vals, names.arg = conds, ylab = node, ...)
}

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

