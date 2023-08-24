#' A function to test how network behavior changes in response to changing the
#' nature of edge types.
#'
#' @param startingNetwork an object of class network describing
#' @param nodeStartVals description
#' @param genotypeVals description

checkEdgeTypes <- function(startingNetwork, nodeStartVals, genotypeVals) {
  # finding the dimensionality of the data to be generated
  modelNo <- nrow(nodeStartVals) * nrow(genotypeVals)
  networkNo <- sum(sapply(peaNetwork@objects$Hormones, function(x) nrow(x@inputs)))

  # stating the conditions to be explored
  modelConditions <-


  dat <- array(NA, dim = c(modelNo, length(peaNetwork@objects$Hormones), networkNo),
               dimnames = list("ModelConditions", peaNetwork@objects$Hormones, "NetworkConfiguration"))
}


sapply(peaNetwork@objects$Hormones, function(x) nrow(x@inputs))
