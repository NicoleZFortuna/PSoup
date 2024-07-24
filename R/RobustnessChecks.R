#' A function to test how network behavior changes in response to changing the
#' nature of edge types.
#'
#' This function builds alternative network objects and
#' their corresponding nextStep files. Alternate networks are build by one by
#' one modifying the nature of edges. Specifically stimulations and inhibitions
#' (necessary stimulations are left as is). These alternate networks can then
#' be the basis of simulations designed to compare network behaviour, however
#' this feature can be turned off.
#'
#' @param startingNetwork an object of class network describing the system to
#'        to be tested
#' @param folder the directory containing the original nextStep function built
#'        on the original network
#' @param runSim default set to TRUE. Specifies if the user wants to automatically
#'             run simulations across the alternative networks generated to
#'             compare their behaviour. If TRUE, the user must provide objects
#'             defining the simulations to be run (eg. a genotypeDef object).
#'             including the nextStep.R file. This can be done by running the
#'             buildModel function on the original network, and defining the
#'             simulation conditions before running this function. If runSim is
#'             set to FALSE, there is no need to run the buildModel function
#'             as all objects for the starting network will be build by this
#'             function.
#' @param maxStep the maximum number of steps that you want to simulate for. Will
#'             terminate simulation when steady state is reached, unless maxStep is
#'             reached first. If set to NA (the default), will simulate until
#'             stability is reached.
#' @param steadyThreshold the number of decimal places to which node values must
#'             be equivalent to be considered a steady state. This threshold must
#'             be passed for all nodes.
#' @param ruleStyle either "Dun", or "Mike". The Dun style resembles the
#'               original Dun equations normalised such that WT conditions are
#'               always 1. The Mike style creates mirrored stimulatory and
#'               inhibitory effects.
#' @param necStimStyle the multiplicative effect taken on by necessary stimulants.
#'               Can be "linear" (the default) or saturating. If saturating,
#'               they can follow a standard "Michaelis-Menten" form, or a
#'               "switch-like" form.
#' @param necStimFile the file directory containing a function for determining
#'               the from for the multiplicative effect of a necessary stimulant.
#'               This function should only have a single argument representing the
#'               value of the necessary stimulant. If a file path is provided, the
#'               necStimStyle argument will be ignored.
#' @param exogenousSupply specifies if the value of a node (or nodes) is
#'               determined by an outside supply. In this case, the value of the
#'               node is supplied by the user and remains consistent throughout
#'               the course of the simulation. The default value for this
#'               argument is NULL. To specify nodes with an exogenous supply,
#'               provide a named vector containing the values of the nodes, with
#'               each vector member named after their respective node.
#' @param priorScreen logical. Specifies if the function should collect
#'               modifier values generated from generated prior distributions.
#' @param ruleStyle either "Dun", or "Mike". The Dun style resembles the
#'               original Dun equations normalised such that WT conditions are
#'               always 1. The Mike style creates mirrored stimulatory and
#'               inhibitory effects.
#' @param necStimStyle the multiplicative effect taken on by necessary stimulants.
#'               Can be "linear" (the default) or saturating. If saturating,
#'               they can follow a standard "Michaelis-Menten" form, or a
#'               "switch-like" form.
#' @param necStimFile the file directory containing a function for determining
#'               the from for the multiplicative effect of a necessary stimulant.
#'               This function should only have a single argument representing the
#'               value of the necessary stimulant. If a file path is provided, the
#'               necStimStyle argument will be ignored.
#' @param saveNetwork logical. Defaults to TRUE. Indicates if alternative
#'               network objects should be saved in the generated folder.
#' @param saveOutput logical. Defaults to TRUE. Indicates if all the simulated
#'               output should be saved in the provided folder. If set to FALSE,
#'               only the outputs to simulations that have not achieved stability
#'               will be saved.
#' @export

exploreEdges <- function(startingNetwork,
                         folder,
                         runSim = TRUE,
                         maxStep = 100,
                         steadyThreshold = 4,
                         ruleStyle = "Dun",
                         necStimStyle = "Michaelis-Menten",
                         necStimFile = NULL,
                         exogenousSupply = NULL,
                         priorScreen = FALSE,
                         saveNetwork = TRUE,
                         saveOutput = FALSE) {
  if (isTRUE(runSim)) {
    load(paste0(folder, "/nodestartDef.RData"))

    if (priorScreen == FALSE) {
      load(paste0(folder, "/genotypeDef.RData"))
    } else {
      load(paste0(folder, "/priorDef.RData"))
      genotypeDef <- priorDef
    }

    if (nrow(nodestartDef) > 1) stop("Please make sure that your nodestartDef data.frame only contains a single wild-type row (one row only containing 1s).")
  }

  nonWT <- function(x) {
    nWT <- which(x != 1)
    dat <- data.frame(node = attributes(x)$names[nWT], val = as.numeric(x)[nWT])
    model <- paste(apply(dat, 1, paste0, collapse = ""), collapse = "_")
    if (model == "") return("WT")
    else return(model)
  }

  # stating the conditions to be explored
  #nodeModelConditions <- apply(nodestartDef, 1, nonWT)
  geneModelConditions <- apply(genotypeDef, 1, nonWT)

  # a function to generate a list of the network alternatives to be tested
  networkChanges <- function(x) {
    edges <- c("stimulation", "inhibition")
    toChange <- which(x@inputs$Influence %in% edges)

    if (length(toChange) == 0) return(NULL)

    if (nrow(x@inputs) == 0) {
      return(NULL)
    } else {
      newEdge <- sapply(-match(x@inputs$Influence[toChange], edges), function(y) edges[y])

      return(paste0(x@inputs$Node, ".", x@name, ".", newEdge))
    }
  }

  # the list of alternative networks. The syntax being 'origin node.destination node.edge type'
  networkAlternations <- unlist(lapply(startingNetwork@objects$Hormones, networkChanges), use.names = F)

  if (isTRUE(runSim)) {  # object to house the final state information for each topology and model parameter condition
    dat <- array(NA, dim = c(length(geneModelConditions),
                             length(startingNetwork@objects$Hormones),
                             length(networkAlternations) + 1),
                 dimnames = list(geneModelConditions,
                                 names(startingNetwork@objects$Hormones),
                                 c("startingNetwork", unname(networkAlternations))))

    # object and counter to save any condition that did not achieve stability
    noStabilityTracker <- data.frame(model.row = NA, network.depth = NA)
    nST <- 0
  }

  # split networkAlternations vector to make it easy to generate alternative topologies
  newNetInfo <- strsplit(networkAlternations, ".", fixed = T)

  for (n in 1:(length(networkAlternations) + 1)) {
    net <- startingNetwork

    if (n != 1) {
      # if not using the original topology, generate the relevant change in the
      # topology and then generate the corresponding nextStep function.
      net@objects$Hormones[[newNetInfo[[n - 1]][2]]]@inputs$Influence[net@objects$Hormones[[newNetInfo[[n - 1]][2]]]@inputs$Node == newNetInfo[[n - 1]][1]] <- newNetInfo[[n - 1]][3]
      altTopologyName = sprintf("altNetwork_%s-%s.%s",
                                newNetInfo[[n - 1]][1],
                                newNetInfo[[n - 1]][2],
                                newNetInfo[[n - 1]][3])

      buildModel(network = net, folder = folder, forceOverwrite = T, maxStep = maxStep,
                 steadyThreshold = steadyThreshold, ruleStyle = ruleStyle,
                 necStimStyle = necStimStyle, necStimFile = necStimFile,
                 saveNetwork = saveNetwork, robustnessTest = T,
                 altTopologyName = altTopologyName)
    } else {
      altTopologyName = NULL

      if (runSim == FALSE) { # only builds the files associated with the original
        # model if not runing a simulation as will override the
        buildModel(network = net, folder = folder, forceOverwrite = T, maxStep = maxStep,
                   steadyThreshold = steadyThreshold, ruleStyle = ruleStyle,
                   necStimStyle = necStimStyle, necStimFile = necStimFile,
                   saveNetwork = saveNetwork, robustnessTest = F,
                   altTopologyName = altTopologyName)
      }
    }

    if (isTRUE(runSim)) {
      #run simulation, making sure that the correct nextStep function is being used
      simulations <- setupSims(folder, maxStep = maxStep,
                               steadyThreshold = steadyThreshold,
                               exogenousSupply = exogenousSupply,
                               priorScreen = priorScreen,
                               robustnessTest = if (n == 1) {FALSE} else {TRUE},
                               altTopologyName = altTopologyName,
                               saveOutput = saveOutput)

      dat[ , colnames(dat), n] <- as.matrix(finalStates(simulations$screen)[, colnames(dat)])

      tempStabilityVec <- stabilityVector(simulations)
      if (any(tempStabilityVec == FALSE)) {
        isStable <- stabilityVector(simulations)
        noStabilityTracker[(nST + 1):((nST + 1) + sum(isStable == FALSE) - 1), 'model.row'] <- which(isStable == FALSE)
        noStabilityTracker[(nST + 1):((nST + 1) + sum(isStable == FALSE) - 1), 'network.depth'] <- networkAlternations[n - 1]

        nST <- nST + sum(isStable == FALSE)

        if (saveOutput == FALSE) {
          # create object to house any simulations that have not achieved stability
          # if this object has length > 0, save it.
        }
      }
    }
  }

  if (isTRUE(runSim)) {
    return(list(data = dat, stable = noStabilityTracker))
  }
}
