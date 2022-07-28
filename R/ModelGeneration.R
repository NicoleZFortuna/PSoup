
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
buildEquation <- function(network, upPenalty = NA, containerPenalty = NA, file = "./equations.R") {
  # a place to save the equations
  if (file.exists(file)) {
    print("The folder already exists")
  } else {
    file.create(file)
  }

  nodes = peaNetwork@objects$Hormones
  genotypes = peaNetwork@objects$Genotypes

  cat("# defining genotype values\n", file = file)
  for (i in 1:length(genotypes)) {
    cat(paste0(genotypes[[i]]@name, substr(names(genotypes[[i]]@expression), 1, 1),
               " = ", genotypes[[i]]@expression), sep = "\n", append = T, file = file)
  }

  cat("\n# defining storage data.frame\n", file = file, append = T)
  cat(sprintf("dat <- setNames(data.frame(matrix(ncol = %s, nrow = 0)), ", length(nodes)), file = file, append = T)
  cat("c('", paste(names(nodes), collapse = "', '"), "'))\n", sep="", file = file, append = T)

  inhibition = c("inhibition", "sufficient inhibition", "necessary inhibition")
  stimulation = c("stimulation", "sufficient stimulation", "necessary stimulation")

  cat("\n# defining node equations\n", file = file, append = T)
  for (i in 1:length(nodes)) {
    if (any(inhibition %in% nodes[[i]]@inputs$Influence)) {
      if (any(stimulation %in% nodes[[i]]@inputs$Influence)) {
        cat(sprintf("dat$%s[t] = (2 * %s)/", nodes[[i]]@name, paste0(nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% stimulation],
                                                                 "[t-1]", collapse = " * ")),
            sprintf("(1 + %s)", paste0(nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% inhibition],
                                       "[t-1]", collapse = " * ")),
            if (length(nodes[[i]]@genotypes) == 0) {NULL} else {paste0(" * ", nodes[[i]]@genotypes,
                                                                       substr(nodes[[i]]@container, 1, 1))},
            "\n", sep = "", append = T,  file = file)
      } else {
        cat(sprintf("dat$%s[t] = 2/", nodes[[i]]@name),
            sprintf("(1 + %s)", paste0(nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% inhibition],
                                       "[t-1]", collapse = " * ")),
            if (length(nodes[[i]]@genotypes) == 0) {NULL} else {paste0(" * ", nodes[[i]]@genotypes,
                                                                       substr(nodes[[i]]@container, 1, 1))},
            "\n", sep = "", append = T,  file = file)
      }
    } else {
      if (any(stimulation %in% nodes[[i]]@inputs$Influence)) {
        cat(sprintf("dat$%s[t] = %s", nodes[[i]]@name, paste0(nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% stimulation],
                                                          "[t-1]", collapse = " * ")),
            if (length(nodes[[i]]@genotypes) == 0) {NULL} else {paste0(" * ", nodes[[i]]@genotypes,
                                                                       substr(nodes[[i]]@container, 1, 1))},
            "\n", sep = "", append = T,  file = file)
      } else {
        cat(sprintf("dat$%s[t] = 1", nodes[[i]]@name),
            if (length(nodes[[i]]@genotypes) == 0) {NULL} else {paste0(" * ", nodes[[i]]@genotypes,
                                                                       substr(nodes[[i]]@container, 1, 1))},
            "\n", sep = "", append = T,  file = file)
      }
    }
  }
}

#' A function to simulate the outcome of a network
#'
#'
networkSimulator <- function(equations = "./equations.R") {

}
