#buildEquation

#' A function to generate equations from a network object
#'
#' @param network an object of class network
#' @param upPenalty a penalty applied to hormones traveling upwards. Behaves as
#'                a multiplier.
#' @param containerPenalty a penalty applied to hormones traveling upwards.
#'                Behaves as a multiplier.
#' @param file the name of the file that you want the equations to be saved to.
#'                The default is to create an R script called equations.R in the
#'                current working directory.
buildEquation <- function(network, upPenalty, containerPenalty, file = "./equations.R") {
  # a place to save the equations
  if (file.exists(file)) {
    print("The folder already exists")
  } else {
    file.create(file)
  }

  nodes = peaNetwork@objects$Hormones
  genotypes = peaNetwork@objects$Genotypes

  inhibition = c("inhibition", "sufficient inhibition", "necessary inhibition")
  stimulation = c("stimulation", "sufficient stimulation", "necessary stimulation")

  for (i in 1:length(nodes)) {
    if (any(inhibition %in% nodes[[i]]@inputs$Influence)) {
      if (any(stimulation %in% nodes[[i]]@inputs$Influence)) {
        cat(sprintf("%s = (2 * %s)/", nodes[[i]]@name, paste(nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% stimulation],
                                                            collapse = " * ")),
            sprintf("(1 + %s)", paste(nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% inhibition],
                                  collapse = " * ")), "\n", sep = "", append = T,  file = file)
      } else {
        cat(sprintf("%s = 2/", nodes[[i]]@name),
            sprintf("(1 + %s)", paste(nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% inhibition],
                                  collapse = " * ")), "\n", sep = "", append = T,  file = file)
      }
    } else {
      if (any(stimulation %in% nodes[[i]]@inputs$Influence)) {
        cat(sprintf("%s = %s", nodes[[i]]@name, paste(nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% stimulation],
                                                      collapse = " * ")), "\n",
            sep = "", append = T,  file = file)
      } else {
        cat(sprintf("%s = 1", nodes[[i]]@name), "\n", sep = "", append = T,  file = file)
      }
    }
  }

  # what are the names of

}

#plantSimulator
