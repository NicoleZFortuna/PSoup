
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
#'               the existing folder.
#' @param dataFrame default set to TRUE. This parameter allows you to choose if
#'               you would like for node starting values, and genotype values
#'               to be stored as a data.frame, or as an Rscript.
#' @param altSource whether alternative sources should be additive to other
#'               inputs to a node. If FALSE, should be treated as just another
#'               input.
#' @param returnAs a string which indicates how the user wants the model
#'               to be returned. The default is to create the model for
#'               use in R ("R"). Users can also choose "C", in which case
#'               a C script will be generated.
#' @param splitCompartment determines if the network will be split into
#'               its separate compartments or maintained as a whole.
#'               Separation of compartments should only be done if you
#'               intend to use L-Systems to mediate the communication
#'               nodes of different compartments. As such, the default
#'               has been set to FALSE.
#' @export

buildModel <- function(network, folder = "./Model", forceOverwrite = FALSE,
                       altSource = FALSE, returnAs = "R",
                       splitCompartment = FALSE) {
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

  if (altSource == FALSE) {
    # changing network influences so all altSource inputs are stimulants
    network@objects$Hormones <- altSourceToStimulant(network@objects$Hormones)
  }

  nodes = network@objects$Hormones
  genotypes = network@objects$Genotypes

  # Creating the genotype and node data.frames
  genHolder = unlist(sapply(genotypes,
                               FUN = function(x) paste(x@name,
                                                       substr(names(x@expression[which(x@expression == 1)]),
                                                              1, 1), sep = ".")), use.names = F)

  genotypeDef = rep(1, length(genHolder))
  names(genotypeDef) = genHolder
  genotypeDef = as.data.frame(t(genotypeDef))

  save(genotypeDef, file = paste0(genfile, ".RData"))

  nodestartDef = rep(1, length(nodes))
  names(nodestartDef) = names(nodes)
  nodestartDef = as.data.frame(t(nodestartDef))

  save(nodestartDef, file = paste0(nodefile, ".RData"))

  inhibition = c("inhibition", "sufficient inhibition", "necessary inhibition")
  stimulation = c("stimulation", "sufficient stimulation", "necessary stimulation")

  cat("# defining node equations", file = funcfile)
  cat("\nnextStep <- function(dat, gen, delay = NA) {\n", file = funcfile, append = T)
  cat("\tdat[nrow(dat) + 1, ] = NA\n", file = funcfile, append = T)
  cat("\tt = nrow(dat)\n", file = funcfile, append = T)
  cat("\tdelay = nrow(dat) - 1\n\n", file = funcfile, append = T)

  for (i in 1:length(nodes)) {
    # collating all the stimulatory action
    if (any(nodes[[i]]@inputs$Influence == "stimulation")) {
      stimString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% "stimulation" & is.na(nodes[[i]]@inputs$Coregulator)]
      numStim <- length(stimString)
      if (numStim > 0) {
        stimString <- differenceString(stimString,
                                       nodes[[i]]@inputs$Operator[nodes[[i]]@inputs$Operator == "delay" & nodes[[i]]@inputs$Influence == "stimulation"])
      }

      # are there any stimulants that are coregulators
      if (any(!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "stimulation")) {
        coregInput <- nodes[[i]]@inputs[!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "stimulation", 1:2]

        coreg <- coregulators(coregInput, returnNum = T)

        numStim <- numStim + coreg$num
        stimString <- paste0(stimString, coreg$coreg, collapse = " + ")
      }

      # take the average of the stimulatory effects if there are more than one
      if (numStim > 1) {
        stimString <- sprintf("(%s)/%s", stimString, numStim)
      }
    } else {stimString = NA}

    # collating all the inhibitory action
    if (any(nodes[[i]]@inputs$Influence == "inhibition")) {
      inhibString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% "inhibition" & is.na(nodes[[i]]@inputs$Coregulator)]
      numInhib <- length(inhibString)
      if (numInhib > 0) {
        inhibString <- differenceString(inhibString,
                                        nodes[[i]]@inputs$Operator[nodes[[i]]@inputs$Operator == "delay" & nodes[[i]]@inputs$Influence == "inhibition"])
      }

      # are there any inhibitors that are coregulators
      if (any(!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "inhibition")) {
        coregInput <- nodes[[i]]@inputs[!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "inhibition", 1:2]

        coreg <- coregulators(coregInput, returnNum = T)

        numInhib <- numInhib + coreg$num
        inhibString <- paste(inhibString, coreg$coreg, collapse = " + ")
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
      # if there are both stimulatory and inhibitory effects
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
                                          nodes[[i]]@inputs$Operator[nodes[[i]]@inputs$Operator == "delay" & nodes[[i]]@inputs$Influence == "necessary stimulation"],
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
                                      nodes[[i]]@inputs$Operator[nodes[[i]]@inputs$Operator == "delay" & nodes[[i]]@inputs$Influence == "altSource"])
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
#' @param language which programming language should the equation be generated in?
#'        Can be either "R", or "C".
coregulators <- function(coreg, returnNum = FALSE, language) {
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
    if (language == "R") {
      coreg[r, !is.na(coreg[r, ])] <- paste0("dat$", coreg[r, !is.na(coreg[r, ])], "[t-1]")
    } else if (language == "C") {
      coreg[r, !is.na(coreg[r, ])] <- paste0("old->", coreg[r, !is.na(coreg[r, ])])
    }

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
#' @param language which programming language should the equation be generated in?
#'        Can be either "R", or "C".
differenceString <- function(string, delays = NA, takeProduct = FALSE,
                             language) {
  delays[delays != "delay" | is.na(delays)] = 1

  terms = c(" + ", " * ")

  if (language == "R") {
    fullString <- paste0("dat$", string, "[t-",delays,"]", collapse = terms[takeProduct + 1])
  } else if (language == "C") {
    fullString <- paste0("old->", string, collapse = terms[takeProduct + 1])
  }

  fullString
}

#' A function to generate a C script which will execute a simulation of the
#' network given a starting condition.
#'
#' @param network an object of class network.
#' @param tmax the maximum value that the simulation will be allowed to
#'        proceed. If the midpoint is reached, a warning will be returned.
#'        The default value is set to 0.
#' @param steadyThreshold the number of decimal places to which node values must
#'             be equivalent to be considered a steady state. This threshold must
#'             be passed for all nodes.
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
#'               the existing folder.

generateC <- function(network, tmax = 100, steadyThreshold = 4,
                      folder = "./Model", forceOverwrite = FALSE) {
  insertKeywords <- c("insertTMAX",
                     "insertSTRUCTNODENAMES",
                     "insertSTRUCTGENENAMES",
                     "insertDATVALS",
                     "insertGENEVALS",
                     "insertEQUATIONS",
                     "insertTHRESHOLD",
                     "insertCOMPARISONCHAIN",
                     "insertFINALPRINT")

  # defining constants
  insertTMAX = tmax
  insertTHRESHOLD = 10^(-steadyThreshold)

  # change altSources to stimulations
  network@objects$Hormones <- altSourceToStimulant(network@objects$Hormones)

  # define node and gene names
  insertSTRUCTNODENAMES <- paste0("float ", sub("\\.", "_", names(network@objects$Hormones)), ";", collapse = "\n\t")

  genHolder = unlist(sapply(network@objects$Genotypes,
                            FUN = function(x) paste(x@name, substr(names(x@expression[which(x@expression == 1)]), 1, 1), sep = "_")),
                     use.names = F)
  insertSTRUCTGENENAMES <- paste0("float ", genHolder, ";", collapse = "\n\t")

  # create standard values for nodes and genes
  insertDATVALS <- paste(rep(1, length(network@objects$Hormones)), collapse = ", ")
  insertGENEVALS <- paste(rep(1, length(genHolder)), collapse = ", ")

  # define equations for nodes
  insertEQUATIONS <- rep(NA, length(network@objects$Hormones))
  for (i in 1:length(insertEQUATIONS)) {
    insertEQUATIONS[i] <- generateEquation(network@objects$Hormones[[i]],
                                           network@objects$Genotypes,
                                           "C")
  }

  insertEQUATIONS <- gsub("\\.", "_",
                         paste0("new->", names(network@objects$Hormones), " = ",
                                insertEQUATIONS, collapse = "\n\t"))

  # create chain to check if nodes have changed in the previous timestep
  insertCOMPARISONCHAIN <- paste(sprintf("if (fabs(old->%s - new->%s) > THRESHOLD) {\n\t\treturn 0;\n\t}",
                                         names(network@objects$Hormones),
                                         names(network@objects$Hormones)),
                                 collapse = " else ")
  insertCOMPARISONCHAIN <- gsub("\\.", "_", insertCOMPARISONCHAIN)

  insertFINALPRINT <- paste0('"The final node values at time %d are - ',
                             paste(sprintf("%s: %%.2f", names(network@objects$Hormones)),
                                   collapse = ", "),
                             '", t, ', paste(paste0("new->", names(network@objects$Hormones)),
                                                collapse = ", "))
  insertFINALPRINT <- gsub("\\.", "_", insertFINALPRINT)

  text <- readLines(file("./inst/scaffold.txt"))

  insertReplacements <- list("insertTMAX" = insertTMAX,
                             "insertSTRUCTNODENAMES" = insertSTRUCTNODENAMES,
                             "insertSTRUCTGENENAMES" = insertSTRUCTGENENAMES,
                             "insertDATVALS" = insertDATVALS,
                             "insertGENEVALS" = insertGENEVALS,
                             "insertEQUATIONS" = insertEQUATIONS,
                             "insertTHRESHOLD" = insertTHRESHOLD,
                             "insertCOMPARISONCHAIN" = insertCOMPARISONCHAIN,
                             "insertFINALPRINT" = insertFINALPRINT)

  containsKey <- function(x, key) {
    test <- grep(key, x)
    if (length(test) == 0) {
      test = 0
    }
    test
  }

  for (i in 1:length(insertKeywords)) {
    whichLine <- as.logical(sapply(text, containsKey, key = insertKeywords[i], USE.NAMES = F))
    text[whichLine] <- sub(insertKeywords[i], insertReplacements[i], text[whichLine])
  }

  # a place to save the equations
  if (dir.exists(folder) & forceOverwrite == FALSE) {
    stop("This folder already exists. If you want to overwrite this folder,
         set forceOverwrite to TRUE.")
  } else {
    dir.create(folder)
    file = paste0(folder, "/Cscript.c")

    file.create(file)
  }

  cat(paste0(text[1], "\n"), file = file)

  for (i in 2:length(text)) {
    cat(paste0(text[i], "\n"), file = file, append = T)
  }
}

#' A function to convert altSource inputs to stimulants.
#'
#' @param hormones the list of hormones contained in a network object.
altSourceToStimulant <- function(hormones) {
  for (i in 1:length(hormones)) {
    if ("altSource" %in% hormones[[i]]@inputs$Influence) {
      hormones[[i]]@inputs$Influence[hormones[[i]]@inputs$Influence == "altSource"] <- "stimulation"
    }
  }
  return(hormones)
}

#' A function to generate an equation for a specific node
#'
#' @param node a node from within a network object
#' @param genes the list of genes frome within a network object
#' @param language which programming language should the equation be generated in?
#'        Can be either "R", or "C".
#'
generateEquation <- function(node, genotypes, language) {
  inhibition = c("inhibition", "sufficient inhibition", "necessary inhibition")
  stimulation = c("stimulation", "sufficient stimulation", "necessary stimulation")

  # collating all the stimulatory action
  if (any(node@inputs$Influence == "stimulation")) {
    stimString <- node@inputs$Node[node@inputs$Influence %in% "stimulation" & is.na(node@inputs$Coregulator)]
    numStim <- length(stimString)
    if (numStim > 0) {
      stimString <- differenceString(stimString,
                                     node@inputs$Operator[node@inputs$Operator == "delay" & node@inputs$Influence == "stimulation"],
                                     language = language)
    }

    # are there any stimulants that are coregulators
    if (any(!is.na(node@inputs$Coregulator) & node@inputs$Influence == "stimulation")) {
      coregInput <- node@inputs[!is.na(node@inputs$Coregulator) & node@inputs$Influence == "stimulation", 1:2]

      coreg <- coregulators(coregInput, returnNum = T, language = "C")

      numStim <- numStim + coreg$num
      stimString <- paste0(stimString, coreg$coreg, collapse = " + ")
    }

    # take the average of the stimulatory effects if there are more than one
    if (numStim > 1) {
      stimString <- sprintf("(%s)/%s", stimString, numStim)
    }
  } else {
    stimString = NA
    numStim = 0
  }

  # collating all the inhibitory action
  if (any(node@inputs$Influence == "inhibition")) {
    inhibString <- node@inputs$Node[node@inputs$Influence %in% "inhibition" & is.na(node@inputs$Coregulator)]
    numInhib <- length(inhibString)
    if (numInhib > 0) {
      inhibString <- differenceString(inhibString,
                                      node@inputs$Operator[node@inputs$Operator == "delay" & node@inputs$Influence == "inhibition"],
                                      language = language)
    }

    # are there any inhibitors that are coregulators
    if (any(!is.na(node@inputs$Coregulator) & node@inputs$Influence == "inhibition")) {
      coregInput <- node@inputs[!is.na(node@inputs$Coregulator) & node@inputs$Influence == "inhibition", 1:2]

      coreg <- coregulators(coregInput, returnNum = T, language = "C")

      numInhib <- numInhib + coreg$num
      inhibString <- paste(inhibString, coreg$coreg, collapse = " + ")
    }
  } else {
    inhibString = NA
    numInhib = 0
  }

  # combining stimulatory and inhibitory effects
  if (numStim > 0 & numInhib == 0) {
    # if there are only stimulatory effects
    allModulations <- stimString
  } else if (numInhib > 0 & numStim == 0) {
    # if there are only inhibitory effects
    allModulations <- sprintf("%s/(1 + %s)", numInhib + 1, inhibString)
  } else if (class(stimString) == "character" & class(inhibString) == "character") {
    # if there are both stimulatory and inhibitory effects
    allModulations <- sprintf("%s * (%s)/(1 + %s)", numInhib + 1, stimString, inhibString)
  } else if (is.na(stimString) & is.na(inhibString)) {
    # if it is constituent wo influence from other nodes
    allModulations <- 1
  }

  # multiplying modulations by necessary stimulants and genotypes
  if (any(node@inputs$Influence == "necessary stimulation")) {
    necstimString <- node@inputs$Node[node@inputs$Influence %in% "necessary stimulation" & is.na(node@inputs$Coregulator)]
    if (length(necstimString) > 0) {
      necstimString <- differenceString(necstimString,
                                        node@inputs$Operator[node@inputs$Operator == "delay" & node@inputs$Influence == "necessary stimulation"],
                                        takeProduct = TRUE, language = language)
    }

    # are there any necessary stimulants that are coregulators
    if (any(!is.na(node@inputs$Coregulator) & node@inputs$Influence == "necessary stimulation")) {
      coregInput <- node@inputs[!is.na(node@inputs$Coregulator) & node@inputs$Influence == "necessary stimulation", -3]

      coreg <- coregulators(coregInput, language = "C")

      necstimString = paste(necstimString, coreg, sep = " + ")
    }
    allModulations <- sprintf("(%s) * (%s)", allModulations, necstimString)
  }

  if (length(node@genotypes) > 0) {
    genes <- node@genotypes
    genoString <- rep(NA, length(genes))
    for (g in 1:length(genes)) {
      if (class(genotypes[[genes[g]]]@coregulator) == "character") {
        if (language == "R") {
          cogenes <- paste0("gen$", c(genes[g], genotypes[[genes[g]]]@coregulator), ".", substr(node@container, 1, 1))
        } else if (language == "C") {
          cogenes <- paste0("gen->", c(genes[g], genotypes[[genes[g]]]@coregulator), ".", substr(node@container, 1, 1))
        }

        genoString[g] <- paste0(cogenes[order(cogenes)], collapse = " * ")
      }
    }

    genoString = unique(genoString)
    allModulations <- sprintf("(%s) * (%s)", allModulations, genoString)
  }

  # add if there is a source from another node (will not be influenced by dynamics of current node)
  if (any(node@inputs$Influence == "altSource")) {
    altString <- node@inputs$Node[node@inputs$Influence %in% "altSource"]
    if (length(altString) > 0) {
      altString <- differenceString(altString,
                                    node@inputs$Operator[node@inputs$Operator == "delay" & node@inputs$Influence == "altSource"],
                                    language = language)
    }

    allModulations <- sprintf("%s + %s", paste0(altString, collapse = " + "), allModulations)
  }

  if (language == "C") {
    allModulations <- paste0(allModulations, ";")
  }

  return(allModulations)
}


