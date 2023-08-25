#' A function to test how network behavior changes in response to changing the
#' nature of edge types.
#'
#' @param startingNetwork an object of class network describing
#' @param nodeStartVals description
#' @param genotypeVals description

checkEdgeTypes <- function(startingNetwork, nodeStartVals, genotypeVals) {
  # finding the dimensionality of the data to be generated

  nonWT <- function(x) {
    nWT <- which(x != 1)
    dat <- data.frame(node = attributes(x)$names[nWT], val = as.numeric(x)[nWT])
    model <- paste(apply(dat, 1, paste0, collapse = ""), collapse = "_")
    if (model == "") return("WT")
    else return(model)
  }

  # stating the conditions to be explored
  #nodeModelConditions <- apply(nodeStartVals, 1, nonWT)
  geneModelConditions <- apply(genotypeVals, 1, nonWT)

  networkChanges <- function(x) {
    edges <- c("stimulation", "inhibition")
    toChange <- which(x@inputs$Influence %in% edges)

    if (nrow(x@inputs) == 0) {
      return(NULL)
    } else {
      newEdge <- sapply(-match(x@inputs$Influence[toChange], edges), function(y) edges[y])

      return(paste0(x@inputs$Node, "->", x@name, ".", newEdge))
    }
  }

  networkAlternations <- unlist(lapply(peaNetwork@objects$Hormones, networkChanges), use.names = F)


  dat <- array(NA, dim = c(length(geneModelConditions),
                           length(peaNetwork@objects$Hormones),
                           length(networkAlternations) + 1),
               dimnames = list(geneModelConditions,
                               names(peaNetwork@objects$Hormones),
                               c("startingNetwork", networkAlternations)))
}


sapply(peaNetwork@objects$Hormones, function(x) nrow(x@inputs))
