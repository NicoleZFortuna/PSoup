#' A function to test how network behavior changes in response to changing the
#' nature of edge types.
#'
#' @param startingNetwork an object of class network describing the system to
#'        to be tested
#' @param folder the directory containing the original nextStep function built
#'        on the original network
#' @param tmax description
#' @param steadyThreshold description
#' @param ruleStyle description
#' @param nesStimStyle description
#' @param nesStimFile description
#' @param saveNetwork description
#' @param saveOutput description

exploreEdges <- function(startingNetwork,
                           folder,
                           tmax = 100,
                           steadyThreshold = 4,
                           ruleStyle = "Dun",
                           nesStimStyle = "Linear",
                           nesStimFile = NULL,
                           saveNetwork = T,
                           saveOutput = F) {

  load(paste0(folder, "/nodestartDef.RData"))
  load(paste0(folder, "/genotypeDef.RData"))

  if (nrow(nodestartDef) > 1) stop("Please make sure that your nodestartDef data.frame only contains a single wild-type row (one row only containing 1s).")

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

  networkAlternations <- unlist(lapply(startingNetwork@objects$Hormones, networkChanges), use.names = F)


  dat <- array(NA, dim = c(length(geneModelConditions),
                           length(startingNetwork@objects$Hormones),
                           length(networkAlternations) + 1),
               dimnames = list(geneModelConditions,
                               names(startingNetwork@objects$Hormones),
                               c("startingNetwork", networkAlternations)))

  newNetInfo <- strsplit(networkAlternations, ".", fixed = T)

  noStabilityTracker <- data.frame(model.row = NA, network.depth = NA)
  nST <- 0

  for (n in 1:length(networkAlternations)) {
    net <- startingNetwork

    if (n != 1) {
      net@objects$Hormones[[newNetInfo[[n]][2]]]@inputs$Influence[net@objects$Hormones[[newNetInfo[[n]][2]]]@inputs$Node == newNetInfo[[n]][1]] <- newNetInfo[[n]][3]
      buildModel(net, folder, forceOverwrite = T, tmax, steadyThreshold,
                 ruleStyle, nesStimStyle, nesStimFile, saveNetwork = F,
                 robustnessTest = T, forceOverwrite = T)

      if (saveNetwork == T) {
        save(net, file = sprintf("%s/altNetwork_%s->%s.%s", folder,
                                 newNetInfo[[n]][1],
                                 newNetInfo[[n]][2],
                                 newNetInfo[[n]][3]))
      }
      simulations <- setupSims(folder, tmax = tmax,
                               steadyThreshold = steadyThreshold,
                               robustnessTest = T)[, colnames(dat)]
    } else {
      source(paste0(folder, "/nextStep.R"), local = T)

      simulations <- setupSims(folder, tmax = tmax,
                               steadyThreshold = steadyThreshold,
                               robustnessTest = F)[, colnames(dat)]
    }

    dat[, colnames(dat), n] <- simulations$screen$simulations[, colnames(dat)]

    if (any(simulations$screen$stable == F)) {
      noStabilityTracker[(nST + 1):sum(simulations$screen$stable == F), 'model.row'] <- which(simulations$screen$stable == F)
      noStabilityTracker[(nST + 1):sum(simulations$screen$stable == F), 'network.depth'] <- networkAlternations[n]

      nST <- nST + sum(simulations$screen$stable == F)
    }
  }

  return(list(data = dat, stable = noStabilityTracker))
}

measureEdgeRobustness <- function(x) {

}

exploreNodeDeletions <- function(startingNetwork) {

}

measureNodeRobustness <- function() {

}

#exploreNodeAdditions?

