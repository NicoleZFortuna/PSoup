
#' A function to generate equations from a network object
#'
#' This function is used to define the script that is needed to simulate the
#' model of interest. It defines the genotypes of the network and sets them
#' to a default of 1. It also initiates default starting node values of 1.
#' A function is also defined, which provides the routine needed to
#' calculate the next time step in the simulation. The folder containing the
#' output of this function can be edited by the user as required. The folder
#' directory will need to be provided to the simulateNetwork or setupSims
#' functions as an input.
#' @param network an object of class network
#' @param folder the name of the folder that you want the components of the
#'               model to be saved to. The default is to create a directory
#'               called Model in the current working directory. The user can
#'               provide their own directory and folder name. If the language
#'               argument is set to "R", two objects: one to define genotypes
#'               (genotypeDef.RData), one to define the starting node values
#'               (nodestartDef.RData) will be saved in the folder. In addition,
#'               a script to define the function that will calculate how the
#'               nodevalues change with each timestep will be saved (nextStep.R).
#'               If language is set to "C", a .h and a .c script will be
#'               generated in the folder. These scripts will need to be
#'               compiled locally on your computer before you will be able to
#'               execute the model.
#' @param forceOverwrite default set to FALSE. Will stop the function if the
#'               folder already exits. Can set to true if you want to replace
#'               the existing folder. In which case a warning will still be thrown.
#' @param altSource whether alternative sources should be additive to other
#'               inputs to a node. If FALSE, should be treated as just another
#'               input.
#' @param language a string which indicates how the user wants the model
#'               to be returned. The default is to create the model for
#'               use in R ("R"). Users can also choose "C", in which case
#'               a C script will be generated.
#' @param splitCompartment determines if the network will be split into
#'               its separate compartments or maintained as a whole.
#'               Separation of compartments should only be done if you
#'               intend to use L-Systems to mediate the communication
#'               nodes of different compartments. As such, the default
#'               has been set to FALSE. NOT YET FINISHED!
#' @param tmax this only needs to be specified in the case that language
#'               has been set to "C". The maximum value that the simulation
#'               will be allowed to proceed. If the midpoint is reached, a
#'               warning will be returned. The default value is set to 100.
#' @param steadyThreshold this only needs to be specified in the case that
#'               language has been set to "C". The number of decimal places
#'               to which node values must be equivalent to be considered
#'               a steady state. This threshold must be passed for all nodes.
#'               The default is set to 4.
#' @param ruleStyle either "Dun", or "Mike". The Dun style resembles the
#'               original Dun equations normalised such that WT conditions are
#'               always 1. The Mike style creates mirrored stimulatory and
#'               inhibitory effects.
#' @param nesStimStyle the multiplicative effect taken on by necessary stimulants.
#'               Can be "linear" (the default) or saturating. If saturating,
#'               they can follow a standard "Michaelis-Menten" form, or a
#'               "switch-like" form.
#' @param nesStimFile the file directory containing a function for determining
#'               the from for the multiplicative effect of a necessary stimulant.
#'               This function should only have a single argument representing the
#'               value of the necessary stimulant. If a file path is provided, the
#'               nesStimStyle argument will be ignored.
#' @param saveNetwork logical. Defaults to TRUE. Indicates if the provided
#'               network object should be saved in the generated folder.
#' @param robustnessTest logical. Defaults to FALSE. Specifies if the nextStep
#'               function being built is part of a network robustness check.
#' @param altTopologyName default to NULL. If robustnessTest = T, this argument
#'               allows the user to keep the generated alternate nextStep function
#'               with a specific name. If no name is provided, the alternate
#'               nextStep functions will be names nextStepAlt.R.
#' @export

buildModel <- function(network,
                       folder = "./Model",
                       forceOverwrite = FALSE,
                       altSource = FALSE,
                       language = "R",
                       splitCompartment = FALSE,
                       tmax = 100,
                       steadyThreshold = 4,
                       ruleStyle = "Dun",
                       nesStimStyle = "Linear",
                       nesStimFile = NULL,
                       saveNetwork = T,
                       robustnessTest = F,
                       altTopologyName = NULL) {

  # determining the form that a necessary stimulant should take.
  if (!is.null(nesStimFile)) {
    # in the case that there is a user provided function
    text <- readLines(file(nesStimFile))
    functionName <- regmatches(text[1], regexpr("^.*?(?=[^A-Za-z0-9_])", text[1], perl = T))
  } else if (!nesStimStyle == "Linear") {
    if (nesStimStyle == "Michaelis-Menten") {
      text <- readLines(file("./inst/nStim_MM"))
    } else if (nesStimStyle == "switch-like") {
      text <- readLines(file("./inst/nStim_sl"))
    } else {
      stop("No valid form for a necessary stimulation has been provided.")
    }
    functionName <- regmatches(text[1], regexpr("^.*?(?=[^A-Za-z0-9_])", text[1], perl = T))
  } else {
    # in the case that the necessary stimulant has a linear form
    functionName = NULL
  }

  # calling other function if language is "C"
  if (language == "C") {
    generateC(network, tmax = tmax, steadyThreshold = steadyThreshold,
                          folder = folder, forceOverwrite = forceOverwrite)
    return(NULL)
  }

  # a place to save the equations
  if (dir.exists(folder) & forceOverwrite == FALSE) {
    stop("This folder already exists. If you want to overwrite this folder,
         set forceOverwrite to TRUE.")
  } else {
    dir.create(folder)
    genfile = paste0(folder, "/genotypeDef")
    nodefile = paste0(folder, "/nodestartDef")

    if (robustnessTest == F) {
      funcfile = paste0(folder, "/nextStep.R")
    } else {
      funcfile = paste0(folder, "/", altTopologyName, "_nextStepAlt.R")
    }

    file.create(funcfile)
  }

  # save the originating network object
  if (saveNetwork == T) {
    save(network, file = paste0(folder, "/",
                                if (robustnessTest == T) {altTopologyName}
                                else {network@name},
                                ".RData"))
  }

  if (altSource == FALSE) {
    # changing network influences so all altSource inputs are stimulants
    network@objects$Hormones <- altSourceToStimulant(network@objects$Hormones)
  }

  nodes = network@objects$Hormones
  genotypes = network@objects$Genotypes

  # if nextStep is being generated as part of a robustness test, the node and
  # modifier data.frmes do not need to be generated
  if (robustnessTest == F) {
    # Creating the genotype and node data.frames
    genHolder = unlist(sapply(genotypes,
                              FUN = function(x) paste(x@name,
                                                      substr(names(x@expression[which(x@expression == 1)]),
                                                             1, 1), sep = "_")), use.names = F)

    genotypeDef = rep(1, length(genHolder))
    names(genotypeDef) = genHolder
    genotypeDef = as.data.frame(t(genotypeDef))

    nodestartDef = rep(1, length(nodes))
    names(nodestartDef) = names(nodes)
    nodestartDef = as.data.frame(t(nodestartDef))

    save(genotypeDef, file = paste0(genfile, ".RData"))
    save(nodestartDef, file = paste0(nodefile, ".RData"))
  }

  inhibition = c("inhibition", "sufficient inhibition", "necessary inhibition")
  stimulation = c("stimulation", "sufficient stimulation", "necessary stimulation")

  cat("# defining node equations", file = funcfile)
  cat("\nnextStep <- function(dat, gen, delay = NA) {\n", file = funcfile, append = T)
  cat("\tdat[nrow(dat) + 1, ] = NA\n", file = funcfile, append = T)
  cat("\tt = nrow(dat)\n", file = funcfile, append = T)
  cat("\tdelay = nrow(dat) - 1\n\n", file = funcfile, append = T)

  # Including a function to calculate the effects of a necessary stimulant in
  # in the case that the style is not linear and/or there is a provided function.
  if (!(nesStimStyle == "Linear" & is.null(nesStimFile))) {
    cat("\t# a function to define the effect of a necessary stimulant.\n", file = funcfile, append = T)
    cat(paste0("\t", text), sep = "\n", file = funcfile, append = T)
    cat("\n", file = funcfile, append = T)
  }

  for (i in 1:length(nodes)) {
    equation <- generateEquation(nodes[[i]],
                                genotypes,
                                language = language,
                                ruleStyle = ruleStyle,
                                necStimFunc = functionName)
    cat(paste0("\tdat$",nodes[[i]]@name, "[t] = ", equation, "\n"),
        file = funcfile, append = T)
  }
  cat("\tdat[t, ]\n", file = funcfile, append = T)
  cat("}", file = funcfile, append = T)
}

#' A function to generate a C script which will execute a simulation of the
#' network given a starting condition.
#'
#' @param network an object of class network.
#' @param tmax the maximum value that the simulation will be allowed ton proceed.
#'             If the midpoint is reached, a warning will be returned. The
#'             default value is set to 0.
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
#' @param ruleStyle either "Dun", or "Mike". The Dun style resembles the
#'               original Dun equations normalised such that WT conditions are
#'               always 1. The Mike style creates mirrored stimulatory and
#'               inhibitory effects.
#' @param necStimFunc the name of the function to be applied to necessary
#'               stimulants. The default is NULL, in which case no function will
#'               be applied, ant therefore the form will be linear.
#' @importFrom stringr str_remove

generateC <- function(network,
                      tmax = 100,
                      steadyThreshold = 4,
                      folder = "./Model",
                      forceOverwrite = FALSE,
                      ruleStyle = "Dun",
                      necStimFunc = NULL) {

  # defining constants
  insertTMAX = tmax
  insertTHRESHOLD = 10^(-steadyThreshold)

  # change altSources to stimulations
  network@objects$Hormones <- altSourceToStimulant(network@objects$Hormones)
  hormonesWcompartment <- sub("\\.", "_", names(network@objects$Hormones))

  # function to append container info to hormone and genotype names
  getGeneContainer <- function(x) {
    paste(x@name, substr(names(x@expression[which(x@expression == 1)]), 1, 1), sep = "_")
  }

  # collect corrected hormone and gene names
  hormoneList <- names(network@objects$Hormones)
  geneList <- sapply(network@objects$Genotypes, getGeneContainer)

  # define node and gene names
  insertSTRUCTNODENAMES <- paste0("float m", hormoneList, ";", collapse = "\n\t")
  insertSTRUCTGENENAMES <- paste0("float m", geneList, ";", collapse = "\n\t")

  # create standard values for nodes and genes
  insertDATVALS <- paste0("\tdat.", hormoneList, " = 1;", collapse = "\n")
  insertGENEVALS <- paste0("\tgen.", geneList, " = 1;", collapse = "\n")

  # define equations for nodes
  insertEQUATIONS <- rep(NA, length(network@objects$Hormones))
  for (i in 1:length(insertEQUATIONS)) {
    insertEQUATIONS[i] <- generateEquation(node = network@objects$Hormones[[i]],
                                           genotypes = network@objects$Genotypes,
                                           language = "C",
                                           ruleStyle = ruleStyle,
                                           necStimFunc = necStimFunc)
  }

  insertEQUATIONS <- paste0("pDat->m", names(network@objects$Hormones), " = ",
                            insertEQUATIONS, collapse = "\n\t")

  # create chain to check if nodes have changed in the previous timestep
  insertCOMPARISONCHAIN <- paste0(sprintf("fabs(oldDat.m->%s - newDat.m->%s) > THRESHOLD",
                                          hormoneList, hormoneList),
                                 collapse = "\n\t\t|| ")

  insertFINALPRINT <- paste0('fprintf(stderr, "', hormoneList,
                             ': %.6f, \\n\\t", dat.m', hormoneList, ')',
                             collapse = ";\n\t")

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

  # a function that will be supplied to sapply to simultaneously find the location
  # of all keywords
  containsKey <- function(x, key) {
    test <- grep(key, x)
    if (length(test) == 0) {
      test = 0
    }
    test
  }

  for (i in 1:length(insertReplacements)) {
    whichLine <- as.logical(sapply(text, containsKey, key = names(insertReplacements)[i], USE.NAMES = F))
    text[whichLine] <- sub(names(insertReplacements)[i], insertReplacements[i], text[whichLine])
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
#' @param ruleStyle either "Dun", or "Mike". The Dun style resembles the original
#'        Dun equations normalised such that WT conditions are always 1. The Mike
#'        style creates mirrored stimulatory and inhibitory effects.
#' @param necStimFunc the name of the function to be applied to necessary
#'        stimulants. The default is NULL, in which case no function will be
#'        applied, ant therefore the form will be linear.

generateEquation <- function(node,
                             genotypes,
                             language,
                             ruleStyle = "Dun",
                             necStimFunc = NULL) {
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

      coreg <- coregulators(coregInput, returnNum = T, language = language,
                            operator = node@inputs[!is.na(node@inputs$Coregulator) & node@inputs$Influence == "stimulation", "Operator"])

      numStim <- numStim + coreg$num
      stimString <- paste0(c(stimString, coreg$coreg), collapse = " + ")
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

      coreg <- coregulators(coregInput, returnNum = T, language = language,
                            operator = node@inputs[!is.na(node@inputs$Coregulator) & node@inputs$Influence == "inhibition", "Operator"])

      numInhib <- numInhib + coreg$num
      inhibString <- paste0(c(inhibString, coreg$coreg), collapse = " + ")
    }

    # take the average of the inhibitory effects if there are more than one
    if (numInhib > 1) {
      inhibString <- sprintf("(%s)/%s", inhibString, numInhib)
    }
  } else {
    inhibString = NA
    numInhib = 0
  }

  if (ruleStyle == "Dun") {
    # combining stimulatory and inhibitory effects
    if (numStim > 0 & numInhib == 0) {
      # if there are only stimulatory effects
      allModulations <- stimString
    } else if (numInhib > 0 & numStim == 0) {
      # if there are only inhibitory effects
      allModulations <- sprintf("2/(1 + %s)" , inhibString)
    } else if (class(stimString) == "character" & class(inhibString) == "character") {
      # if there are both stimulatory and inhibitory effects
      allModulations <- sprintf("%s * (%s)/(1 + %s)", numInhib + 1, stimString, inhibString)
    } else if (is.na(stimString) & is.na(inhibString)) {
      # if it is constituent wo influence from other nodes
      allModulations <- 1
    }
  } else if (ruleStyle == "Mike") {
    # Creating the Mike syntax
    if (is.na(stimString)) {
      stimString = NULL
    } else {
      alpha <- 1
      stimString <- paste0("(", alpha, " + ", stimString, ")/(", alpha + 1, ")")
    }

    if (is.na(inhibString)) {
      inhibString = NULL
    } else {
      inhibString <- paste0("2/(1 + ", inhibString, ")")
    }

    # combining stimulatory and inhibitory effects
    if (numStim > 0 & numInhib == 0) {
      # if there are only stimulatory effects
      allModulations <- stimString
    } else if (numInhib > 0 & numStim == 0) {
      # if there are only inhibitory effects
      allModulations <- inhibString
    } else if (class(stimString) == "character" & class(inhibString) == "character") {
      # if there are both stimulatory and inhibitory effects
      allModulations <- paste(stimString, "*", inhibString)
    } else if (is.null(stimString) & is.null(inhibString)) {
      # if it is constituent wo influence from other nodes
      allModulations <- 1
    }

  } else {
    stop("You have not provided an accepted algorithm rule style.")
  }

  # multiplying modulations by necessary stimulants and genotypes
  if (any(node@inputs$Influence == "necessary stimulation")) {
    necstimString <- node@inputs$Node[node@inputs$Influence %in% "necessary stimulation" & is.na(node@inputs$Coregulator)]
    if (length(necstimString) > 0) {
      necstimString <- differenceString(necstimString,
                                        node@inputs$Operator[node@inputs$Operator == "delay" & node@inputs$Influence == "necessary stimulation"],
                                        takeProduct = NULL, language = language)

      if (!is.null(necStimFunc)) {
        necstimString <- paste0(necStimFunc, "(", necstimString, ")", collapse = " * ")
      }
    }

    # are there any necessary stimulants that are coregulators
    if (any(!is.na(node@inputs$Coregulator) & node@inputs$Influence == "necessary stimulation")) {
      coregInput <- node@inputs[!is.na(node@inputs$Coregulator) & node@inputs$Influence == "necessary stimulation", -3]

      # NEED TO EDIT COREGULATORS FUNCTION SO THAT IT CAN RETURN A NON CONCATENATED VECTOR FOR THE PURPOSE OF ADDING A FUNCTION
      coreg <- coregulators(coregInput, language = language)

      if (!is.null(necStimFunc)) {
        paste0(necStimFunc, "(", coreg, ")", collapse = " * ")
      }

      necstimString = paste(necstimString, coreg, sep = " * ")
    }
    allModulations <- sprintf("(%s) * (%s)", allModulations, necstimString)
  }

  if (length(node@genotypes) > 0) {
    genes <- node@genotypes
    genoString <- rep(NA, length(genes))
    for (g in 1:length(genes)) {
      if (class(genotypes[[genes[g]]]@coregulator) == "character") {
        if (language == "R") {
          cogenes <- paste0("gen$", c(genes[g], genotypes[[genes[g]]]@coregulator), "_", substr(node@container, 1, 1))
        } else if (language == "C") {
          cogenes <- paste0("gen.m", c(genes[g], genotypes[[genes[g]]]@coregulator), "_", substr(node@container, 1, 1))
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

#' A function that creates a string with coregulators multiplied together
#'
#' Coregulators are sorted so that their multiplications are only represented
#' once. Each modulator is given a temporal modifier. Sets of coregulators are
#' summed.
#'
#' @param coreg a subsed of a the inputs of a hormone object. This subset
#'        must all have the same modulating effect, and have coregulators
#' @param returnNum return the number of unique coregulator sets. Default is
#'        set to FALSE.
#' @param language which programming language should the equation be generated in?
#'        Can be either "R", or "C".
#' @param operator which operator defines the coregulator, either "and" or "or".

coregulators <- function(coreg,
                         returnNum = FALSE,
                         language,
                         operator) {
  coreg <- unname(as.matrix(coreg))

  split <- strsplit(coreg[,2], ", ") # Splitting apart lists of coregulators

  lengths <- sapply(split, length)

  # in the case that there is ever more than one coregulator, increase the size
  # of the coreg matrix to accommodate
  if (max(lengths) > 1) {
    coreg <- cbind(coreg, matrix(NA, nrow = nrow(coreg), ncol = max(lengths) - 1))
  }

  # separating coregulators into their own cells
  for (i in 1:max(lengths)) {
    index <- which(lengths >= i)
    coreg[index, i + 1] <- sapply(split[index], `[[`, i)
  }

  coregString = rep(NA, nrow(coreg))
  # providing previous timestep syntax depending on the language to be used
  for (r in 1:nrow(coreg)) {
    coreg[r, !is.na(coreg[r, ])] <- sort(coreg[r, !is.na(coreg[r, ])])
    if (language == "R") {
      coregString[r] <- paste0("dat$", coreg[r, !is.na(coreg[r, ])], "[t-1]", collapse = ", ")
    } else if (language == "C") {
      coregString[r] <- paste0("old->", coreg[r, !is.na(coreg[r, ])], collapse = ", ")
    } else {
      stop("Incorrect language specification. Can only accept 'R', or 'C'.")
    }
  }

  # only keep the first instance of a piece of information
  firstApp <- !duplicated(coregString)

  coregString <- coregString[firstApp]
  operator    <- operator[firstApp]

  # perform the correct calculation depending on operator
  # SO FAR ONLY FOR R!!!!
  for (i in 1:length(coregString)) {
    if (operator[i] == "and") {
      coregString[i] <- paste0("min(", coregString[i], ")")
    } else if (operator[i] == "or") {
      coregString[i] <- paste0("max(", coregString[i], ")")
    } else {
      stop("Incorrect operator specification. Can only accept 'and', or 'or'.")
    }
  }

  if (returnNum == T) num = length(coregString)
  coregString <- paste0(coregString, collapse = " + ")

  if (returnNum == FALSE) return(coreg)
  else return(list(coreg = coregString, num = num))
}

#' A function to build the difference equation including delays
#'
#' @param string whatever string is being constructed.
#' @param delays a vector specifying if there are any delays associated with
#'        a particular input.
#' @param takeProduct logical. Should be set to TRUE if this function is being
#'        used to collapse necessary stimulants (* instead of +). Can be set to
#'        NULL of you do not want to concatenate into a single string.
#' @param language which programming language should the equation be generated in?
#'        Can be either "R", or "C".

differenceString <- function(string,
                             delays = NA,
                             takeProduct = FALSE,
                             language) {
  delays[delays != "delay" | is.na(delays)] = 1

  terms = c(" + ", " * ")

  if (is.null(takeProduct)) {
    collapse = NULL
  } else {
    collapse = terms[takeProduct + 1]
  }

  if (language == "R") {
    fullString <- paste0("dat$", string, "[t-",delays,"]", collapse = collapse)
  } else if (language == "C") {
    fullString <- paste0("oldDat.m", string, collapse = collapse)
  }

  fullString
}
