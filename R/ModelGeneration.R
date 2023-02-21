
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
#' @param folder the name of the folder that you want the components of the
#'               model to be saved to. The default is to create a directory
#'               called Model in the current working directory. The user can
#'               provide their own directory and folder name. In this folder
#'               will be generated three R scripts: one to define genotypes
#'               (genotypeDef.R), one to define the starting node values
#'               (nodestartDef.R), and one to define the function that
#'               gives the difference equations that are used to simulate the
#'               network (nextStep.R).
#' @param forceOverwrite default set to FALSE. Will stop the function if the
#'               folder already exits. Can set to true if you want to replace
#'               the existing folder
#' @param dataFrame default set to TRUE. This parameter allows you to choose if
#'               you would like for node starting values, and genotype values
#'               to be stored as a data.frame, or as an Rscript.
#' @export

buildModel <- function(network, folder = "./Model", forceOverwrite = FALSE,
                       dataFrame = TRUE) {
  # a place to save the equations
  if (dir.exists(folder) & forceOverwrite == FALSE) {
    stop("This folder already exists. If you want to overwrite this folder,
         set forceOverwrite to TRUE.")
  } else {
    dir.create(folder)
    genfile = paste0(folder, "/genotypeDef")
    nodefile = paste0(folder, "/nodestartDef")
    funcfile = paste0(folder, "/nextStep.R")

    file.create(funcfile)
  }

  nodes = network@objects$Hormones
  genotypes = network@objects$Genotypes

  if (dataFrame == TRUE) {
    genHolder = as.vector(sapply(genotypes,
                                 FUN = function(x) paste(x@name,
                                                         substr(names(x@expression),
                                                                1, 1), sep = ".")))
    genotypeDef = rep(1, length(genHolder))
    names(genotypeDef) = genHolder
    genotypeDef = as.data.frame(t(genotypeDef))

    save(genotypeDef, file = paste0(genfile, ".RData"))

    nodestartDef = rep(1, length(nodes))
    names(nodestartDef) = names(nodes)
    nodestartDef = as.data.frame(t(nodestartDef))

    save(nodestartDef, file = paste0(nodefile, ".RData"))
  } else {
    file.create(genfile)
    file.create(nodefile)

    cat("# defining genotype values\n", file = paste0(genfile, ".R"))
    cat("genotypeDef = data.frame(\n", file = paste0(genfile, ".R"), append = T)
    for (i in 1:length(genotypes)) {
      if (i < length(genotypes)) {
        cat(paste0("\t", genotypes[[i]]@name, substr(names(genotypes[[i]]@expression), 1, 1),
                   " = ", genotypes[[i]]@expression,","), sep = "\n", append = T, file = paste0(genfile, ".R"))
      } else {
        cat(paste0("\t", genotypes[[i]]@name, substr(names(genotypes[[i]]@expression), 1, 1),
                   " = ", genotypes[[i]]@expression, collapse = ",\n"), sep = "\n", append = T, file = paste0(genfile, ".R"))
      }
    }
    cat(")\n", file = paste0(genfile, ".R"), append = T)

    cat("# defining storage data.frame and node initial values\n", file = paste0(nodefile, ".R"))
    cat("nodestartDef <- data.frame(\n\t'", paste(names(nodes), collapse = "' = 1, \n\t'"), "' = 1\n)\n",
        sep="", file = paste0(nodefile, ".R"), append = T)
  }

  inhibition = c("inhibition", "sufficient inhibition", "necessary inhibition")
  stimulation = c("stimulation", "sufficient stimulation", "necessary stimulation")

  cat("# defining node equations", file = funcfile)
  cat("\nnextStep <- function(dat, gen, delay = NA) {\n", file = funcfile, append = T)
  cat("\tdat[nrow(dat) + 1, ] = NA\n", file = funcfile, append = T)
  cat("\tt = nrow(dat)\n", file = funcfile, append = T)
  cat("\tif (is.na(delay)) {delay == 2}\n\n", file = funcfile, append = T)

  for (i in 1:length(nodes)) {
    # collating all the stimulatory action
    if (any(nodes[[i]]@inputs$Influence == "stimulation")) {
      stimString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% "stimulation" & is.na(nodes[[i]]@inputs$Coregulator)]
      if (length(stimString) > 0) {
        stimString <- differenceString(stimString,
                                       nodes[[i]]@inputs$Operator[nodes[[i]]@inputs$Operator == "Delay" & nodes[[i]]@inputs$Influence == "stimulation"])
      }

      # are there any stimulants that are coregulators
      if (any(!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "stimulation")) {
        coregInput <- nodes[[i]]@inputs[!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "stimulation", 1:2]

        coreg <- coregulators(coregInput)

        stimString = paste0(stimString, coreg, collapse = " + ")
      }
    } else {stimString = NA}

    # collating all the inhibitory action
    if (any(nodes[[i]]@inputs$Influence == "inhibition")) {
      inhibString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% "inhibition" & is.na(nodes[[i]]@inputs$Coregulator)]
      numInhib <- length(inhibString)
      if (length(inhibString) > 0) {
        inhibString <- differenceString(inhibString,
                                        nodes[[i]]@inputs$Operator[nodes[[i]]@inputs$Operator == "Delay" & nodes[[i]]@inputs$Influence == "inhibition"])
      }

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
      if (length(necstimString) > 0) {
        necstimString <- differenceString(necstimString,
                                          nodes[[i]]@inputs$Operator[nodes[[i]]@inputs$Operator == "Delay" & nodes[[i]]@inputs$Influence == "necessary stimulation"],
                                          takeProduct = TRUE)
      }

      # are there any necessary stimulants that are coregulators
      if (any(!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "necessary stimulation")) {
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
          cogenes <- paste0("gen$", c(genes[g], genotypes[[genes[g]]]@coregulator), ".", substr(nodes[[i]]@container, 1, 1))

          genoString[g] <- paste0(cogenes[order(cogenes)], collapse = "*")
        }
      }

      genoString = unique(genoString)
      allModulations <- sprintf("(%s) * (%s)", allModulations, genoString)
    }

    # add if there is a source from another node (will not be influenced by dynamics of current node)
    if (any(nodes[[i]]@inputs$Influence == "altSource")) {
      altString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% "altSource"]
      if (length(altString) > 0) {
        altString <- differenceString(altString,
                                      nodes[[i]]@inputs$Operator[nodes[[i]]@inputs$Operator == "Delay" & nodes[[i]]@inputs$Influence == "altSource"])
      }

      allModulations <- sprintf("%s + %s", paste0(altString, collapse = " + "), allModulations)
    }
    cat(paste0("\tdat$",nodes[[i]]@name, "[t] = ", allModulations, "\n"), file = funcfile, append = T)
  }
  cat("\tdat[t, ]\n", file = funcfile, append = T)
  cat("}", file = funcfile, append = T)
}


#' A function that creates a string with coregulators multiplied together
#'
#' Coregulators are sorted so that their multiplications are only represented
#' once. Each modulator is given a temporal modifier. Sets of coregulators are
#' summed.
#'
#' @param coreg a subsed of a the inputs of a hormone object. This subset
#'        must all have the same modulating effect, and have coregulators
#' @param returnNum return the number of unique coregulator sets. Default is
#'        set to FLASE.
coregulators <- function(coreg, returnNum = FALSE) {
  coreg <- unname(as.matrix(coreg))

  split <- strsplit(coreg[,2], ", ") # Splitting apart lists of coregulators

  lengths <- sapply(split, length)

  # in the case that there is ever more than one coregulator, increase the size
  # of the coreg matrix to accomodate
  if (max(lengths) > 1) {
    coreg <- cbind(coreg, matrix(NA, nrow = nrow(coreg), ncol = max(lengths) - 1))
  }

  # separating coregulators into their own cells
  for (i in 1:max(lengths)) {
    index <- which(lengths >= i)
    coreg[index, i + 1] <- sapply(split[index], `[[`, i)
  }


  for (r in 1:nrow(coreg)) {
    coreg[r, !is.na(coreg[r, ])] <- sort(coreg[r, !is.na(coreg[r, ])])
    coreg[r, !is.na(coreg[r, ])] <- paste0("dat$", coreg[r, !is.na(coreg[r, ])], "[t-1]")
  }

  coreg <- unique(apply(coreg, 1, function(x) paste0(x[!is.na(x)], collapse = "*")))
  if (returnNum == T) num = length(coreg)
  coreg <- paste0(coreg, collapse = " + ")

  if (returnNum == FALSE) return(coreg)
  else return(list(coreg = coreg, num = num))
}

#' A function to build the difference equation including delays
#'
#' @param string whatever string is being constructed.
#' @param delays a vector specifying if there are any delays associated with
#'        a particular input.
#' @param takeProduct logical. Should be set to TRUE if this function is being
#'        to collapse necessary stimulants (* instead of)
differenceString <- function(string, delays, takeProduct = FALSE) {
  delays[delays != "delay" | is.na(delays)] = 1

  if (takeProduct == FALSE) {
    fullString <- paste0("dat$", string, "[t-",delays,"]", collapse = " + ")
  } else {
    fullString <- paste0("dat$", string, "[t-",delays,"]", collapse = " * ")
  }

  fullString
}



