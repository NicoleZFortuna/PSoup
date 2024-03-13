#' A plot to quickly inspect the progression of a simulation.
#'
#' This function is not designed to produce high quality plots. It is simply to
#' quickly inspect the progression of a simulation.
#'
#' @param simData the output of a single simulation.
#' @param logTransform defaults to TRUE. Indicates if the data should be log
#'        transformed.
#' @param removeBaseline defualts to TRUE Indicates if nodes that have remained
#'        at baseline throughout the simulation should be removed.
#' @importFrom methods is
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_colour_viridis_d
#' @importFrom ggplot2 theme_bw
#' @export

fastPlot <- function(sim, logTransform = T, removeBaseline = T) {
  if (is(sim, "list") & "simulation" %in% names(sim)) sim <- sim$simulation

  nonBaseline <- apply(sim, 2, function(x) any(x != 1))

  if (removeBaseline == T & sum(nonBaseline) > 0) {
    sim <- sim[nonBaseline]
  }
  dat = cbind(x = 1:nrow(sim), if (logTransform == T) {log(sim)} else {sim})

  dat_long <- melt(dat, id = "x")
  colnames(dat_long)[2] <- "Nodes"

  dat_long$x <- dat_long$x - 1

  if (removeBaseline == T & sum(nonBaseline) > 0 & sum(nonBaseline) < length(nonBaseline)) {
    caption <- paste0("Nodes which never deviated from the baseline condition:\n",
                      paste0(names(nonBaseline[nonBaseline == F]), collapse = ", "))
  } else if (removeBaseline == T & sum(nonBaseline) == length(nonBaseline)) {
    caption <- "All nodes deviated from the baseline condition."
  } else if (removeBaseline == T & sum(nonBaseline) == 0) {
    caption <- "All nodes maintained the baseline condition"
  } else {
    caption <- ggplot2::waiver()
  }

  if (max(dat_long$x) < 30) {
    breaks <- seq(0, max(dat_long$x), 1)
  } else if (max(dat_long$x) >= 10 & max(dat_long$x) < 50) {
    if ((max(dat_long$x) %% 2) == 0) {maxX <- max(dat_long$x)
    } else {maxX <- max(dat_long$x) + 1}

    breaks <- seq(0, maxX, 2)
  } else if (max(dat_long$x) >= 50 & max(dat_long$x) < 100) {
    if ((max(dat_long$x) %% 5) == 0) {maxX <- max(dat_long$x)
    } else {maxX <- max(dat_long$x) + 5 - (max(dat_long$x) %% 5)}

    breaks <- seq(0, maxX, 5)
  } else if (max(dat_long$x) > 100) {
    if ((max(dat_long$x) %% 10) == 0) {maxX <- max(dat_long$x)
    } else {maxX <- max(dat_long$x) + 10 - (max(dat_long$x) %% 10)}

    breaks <- seq(0, maxX, 10)
  }

  ggplot(data = dat_long, aes(x = x, y = value, color = Nodes)) +
    geom_line() +
    labs(x = "Steps",
         y = if (logTransform == T) {"Log Transformed Node Value"} else {"Node Value"},
         caption = caption) +
    scale_colour_viridis_d(option = "H") +
    theme_bw() +
    scale_x_continuous(breaks = breaks)
}

#' A function to pull the final states from a set of simulations.
#'
#' @param simulations a list containing the output of the setupSims function
#' @importFrom utils tail
#' @export

finalStates <- function(simulations) {
  final <- tail(simulations[[1]]$simulation, 1)
  final[2:length(simulations), ] <- NA

  for (i in 2:length(simulations)) {
    final[i, ] <- tail(simulations[[i]]$simulation,1)
  }

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
#' @importFrom graphics barplot

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
# simulations <- setupSims(file, randomStart = NA, delay = 2, maxStep = 50)
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

