
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
  cat("gen = data.frame(\n", file = file, append = T)
  for (i in 1:length(genotypes)) {
    cat(paste0("\tgen$", genotypes[[i]]@name, substr(names(genotypes[[i]]@expression), 1, 1),
               " = ", genotypes[[i]]@expression), sep = ",\n", append = T, file = file)
  }
  cat(")\n", file = file, append = T)

  cat("\n# defining storage data.frame and node initial values\n", file = file, append = T)
  cat("dat <- data.frame(\n\t'", paste(names(nodes), collapse = "' = 1, \n\t'"), "' = 1\n)\n",
      sep="", file = file, append = T)

  inhibition = c("inhibition", "sufficient inhibition", "necessary inhibition")
  stimulation = c("stimulation", "sufficient stimulation", "necessary stimulation")

  cat("\n# defining node equations", file = file, append = T)
  cat("\nnextStep <- function(dat, gen, delay = NA) {\n", file = file, append = T)
  cat("\tdat[nrow(dat) + 1, ] = NA\n", file = file, append = T)
  cat("\tt = nrow(dat)\n\n", file = file, append = T)

  for (i in 1:length(nodes)) {
    # collating all the stimulatory action
    if (any(nodes[[i]]@inputs$Influence == "stimulation")) {
      stimString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% "stimulation" & is.na(nodes[[i]]@inputs$Coregulator)]
      if (length(stimString) > 0) {stimString <- paste0("dat$", stimString, "[t-1]", collapse = " + ")}

      # are there any stimulants that are coregulators
      if (any(!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "stimulation")) {
        coregInput <- nodes[[i]]@inputs[!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "stimulation", -3]

        coreg <- coregulators(coregInput)

        stimString = paste(stimString, coreg, sep = " + ")
      }
    } else {stimString = NA}

    # collating all the inhibitory action
    if (any(nodes[[i]]@inputs$Influence == "inhibition")) {
      inhibString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% "inhibition" & is.na(nodes[[i]]@inputs$Coregulator)]
      numInhib <- length(inhibString)
      if (length(inhibString) > 0) {inhibString <- paste0("dat$", inhibString, "[t-1]", collapse = " + ")}

      # are there any inhibitors that are coregulators
      if (any(!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "inhibition")) {
        coregInput <- nodes[[i]]@inputs[!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "inhibition", -3]

        coreg <- coregulators(coregInput)

        numInhib = coreg$num
        inhibString = paste(inhibString, coreg$coreg, sep = " + ")
      }
    } else {inhibString = NA}

    # combining stimulatory and inhibitory effects
    if (class(stimString) == "character" & is.na(inhibString)) {
      # if there are only stimulatory effects
      allModulations <- stimString
    } else if (class(inhibString) == "character" & is.na(stimString)) {
      # if there are only inhibitory effects
      allModulations <- sprintf("%s/(1 + %s)", numInhib + 1, inhibString)
    } else if (class(stimString) == "character" & class(inhibString) == "character") {
      allModulations <- sprintf("%s*(%s)/(1 + %s)", numInhib + 1, stimString, inhibString)
    } else if (is.na(stimString) & is.na(inhibString)) {
      # if it is constituent wo influence from other nodes
      allModulations <- 1
    }

    # multiplying modulations by necessary stimulants and genotypes
    if (any(nodes[[i]]@inputs$Influence == "necessary stimulation")) {
      necstimString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% "necessary stimulation" & is.na(nodes[[i]]@inputs$Coregulator)]
      if (length(necstimString) > 0) {necstimString <- paste0("dat$", necstimString, "[t-1]", collapse = " + ")}

      # are there any necessary stimulants that are coregulators
      if (any(!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "stimulation")) {
        coregInput <- nodes[[i]]@inputs[!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "necessary stimulation", -3]

        coreg <- coregulators(coregInput)

        necstimString = paste(necstimString, coreg, sep = " + ")
      }
      allModulations <- sprintf("(%s) * (%s)", allModulations, necstimString)
    }

    if (length(nodes[[i]]@genotypes) > 0) {
      genes <- nodes[[i]]@genotypes
      genoString <- rep(NA, length(genes))
      for (g in 1:length(genes)) {
        if (class(genotypes[[genes[g]]]@coregulator) == "character") {
          cogenes <- c(genes[g], genotypes[[genes[g]]]@coregulator)

          genoString[g] <- paste0(cogenes[order(cogenes)], collapse = "*")
        }
      }

      genoString = unique(genoString)
      allModulations <- sprintf("(%s) * (%s)", allModulations, genoString)
    }

    # add if there is a source from another node (will not be influenced by dynamics of current node)
    if (any(nodes[[i]]@inputs$Influence == "altSource")) {
      altString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% "altSource"]
      allModulations <- sprintf("%s + %s", paste0("dat$", altString, "[t-1]", collapse = " + "), allModulations)
    }
    cat(paste0("\tdat$",nodes[[i]]@name, "[t-1] = ", allModulations, "\n"), file = file, append = T)
  }
  cat("\tdat[t, ]\n", file = file, append = T)
  cat("}", file = file, append = T)
}


#' A function that creates a string with coregulators multiplied together
#'
#' Coregulators are sorted so that their multiplications are only represented
#' once. Each modulator is given a temporal modifier. Sets of coregulators are
#' summed.
#'
#' @param coregulators a subsed of a the inputs of a hormone object. This subset
#'        must all have the same modulating effect, and have coregulators
#' @param returnNum return the number of unique coregulator sets. Default is
#'        set to FLASE.
coregulators <- function(coreg, returnNum = FALSE) {
  coreg <- unname(as.matrix(coreg))
  for (r in 1:nrow(coreg)) {
    coreg[r, ] <- sort(coreg[r, ])
    coreg[r, ] <- paste0("dat$", coreg[r, ], "[t-1]")
  }

  coreg <- unique(paste(coreg[,1], coreg[,2], sep = "*"))
  if (returnNum == T) num = length(coreg)
  coreg <- paste0(coreg, collapse = " + ")

  if (returnNum == FALSE) return(coreg)
  else return(list(coreg = coreg, num = num))
}
