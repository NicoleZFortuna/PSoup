
#' A function to generate equations from a network object
#'
#' This function is used to define the script that is needed to simulate the
#' model of interest. It defines the genotypes of the network, and sets them
#' to a default of 1. It also initiates that data.frame which will record the
#' changes to the system over time, including initiating the starting values
#' for the nodes. The default starting values are 1. A nextStep function is
#' also defined, which provides the information needed to calculate the next
#' time step in the simulation. This file can be edited by the user as required.
#' The file will need to be provided to the simulateNetwork function as an input.
#' @param network an object of class network
#' @param upPenalty a penalty applied to hormones traveling upwards. Behaves as
#'                a multiplier.
#' @param containerPenalty a penalty applied to hormones traveling upwards.
#'                Behaves as a multiplier.
#' @param file the name of the file that you want the equations to be saved to.
#'                The default is to create an R script called equations.R in the
#'                current working directory.
buildModel <- function(network, upPenalty = NA, containerPenalty = NA, file = "./equations.R") {
  # a place to save the equations
  if (file.exists(file)) {
    warning("This file already exists and will be overwritten.")
  } else {
    file.create(file)
  }

  nodes = network@objects$Hormones
  genotypes = network@objects$Genotypes

  cat("# defining genotype values\n", file = file)
  cat("gen = list()\n", file = file, append = T)
  for (i in 1:length(genotypes)) {
    cat(paste0("gen$", genotypes[[i]]@name, substr(names(genotypes[[i]]@expression), 1, 1),
               " = ", genotypes[[i]]@expression), sep = "\n", append = T, file = file)
  }

  cat("\n# defining storage data.frame and node initial values\n", file = file, append = T)
  cat("dat <- data.frame(\n'", paste(names(nodes), collapse = "' = 1, \n'"), "' = 1\n)\n",
      sep="", file = file, append = T)

  inhibition = c("inhibition", "sufficient inhibition", "necessary inhibition")
  stimulation = c("stimulation", "sufficient stimulation", "necessary stimulation")

  cat("\n# defining node equations", file = file, append = T)
  cat("\nnextStep <- function(dat, gen, delay = NA) {\n", file = file, append = T)
  cat("\tdat[nrow(dat) + 1, ] = NA\n", file = file, append = T)
  cat("\tt = nrow(dat)\n\n", file = file, append = T)

  for (i in 1:length(nodes)) {
    if (any(inhibition %in% nodes[[i]]@inputs$Influence)) {
      if (any(stimulation %in% nodes[[i]]@inputs$Influence)) {
        cat(sprintf("\tdat$%s[t] = (2 * %s)/",
                    nodes[[i]]@name,
                    paste0("dat$", nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% stimulation],
                           "[t-1]", collapse = " * ")),
            sprintf("(1 + %s)",
                    paste0("dat$", nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% inhibition],
                           "[t-1]", collapse = " * ")),
            if (length(nodes[[i]]@genotypes) == 0) {NULL} else {paste0(" * gen$", nodes[[i]]@genotypes,
                                                                       substr(nodes[[i]]@container, 1, 1))},
            "\n", sep = "", append = T,  file = file)
      } else {
        cat(sprintf("\tdat$%s[t] = 2/", nodes[[i]]@name),
            sprintf("(1 + %s)", paste0("dat$", nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% inhibition],
                                       "[t-1]", collapse = " * ")),
            if (length(nodes[[i]]@genotypes) == 0) {NULL} else {paste0(" * gen$", nodes[[i]]@genotypes,
                                                                       substr(nodes[[i]]@container, 1, 1))},
            "\n", sep = "", append = T,  file = file)
      }
    } else {
      if (any(stimulation %in% nodes[[i]]@inputs$Influence)) {
        cat(sprintf("\tdat$%s[t] = %s",
                    nodes[[i]]@name,
                    paste0("dat$", nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% stimulation],
                           "[t-1]", collapse = " * ")),
            if (length(nodes[[i]]@genotypes) == 0) {NULL} else {paste0(" * gen$", nodes[[i]]@genotypes,
                                                                       substr(nodes[[i]]@container, 1, 1))},
            "\n", sep = "", append = T,  file = file)
      } else {
        cat(sprintf("\tdat$%s[t] = 1", nodes[[i]]@name),
            if (length(nodes[[i]]@genotypes) == 0) {NULL} else {paste0(" * gen$", nodes[[i]]@genotypes,
                                                                       substr(nodes[[i]]@container, 1, 1))},
            "\n", sep = "", append = T,  file = file)
      }
    }
  }
  cat("\tdat[t, ]\n", file = file, append = T)
  cat("}", file = file, append = T)
}
