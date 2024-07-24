
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
#'               use in R ("R"). Users can also choose "C", or "C#".
#' @param splitCompartment determines if the network will be split into
#'               its separate compartments or maintained as a whole.
#'               Separation of compartments should only be done if you
#'               intend to use L-Systems to mediate the communication
#'               nodes of different compartments. As such, the default
#'               has been set to FALSE. NOT YET FINISHED!
#' @param maxStep this only needs to be specified in the case that language
#'               has been set to "C/C#". The maximum value that the simulation
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
#' @param necStimStyle the multiplicative effect taken on by necessary stimulants.
#'               Can be "linear" (the default) or saturating. If saturating,
#'               they can follow a standard "Michaelis-Menten" form, or a
#'               "switch-like" form.
#' @param necStimFile the file directory containing a function for determining
#'               the form for the multiplicative effect of a necessary stimulant.
#'               If more than one form exists, this argument can be a vector of
#'               pathways. If the user wants to use more than a single form, the
#'               nesStimMap argument must be used. Default set to NULL, in which
#'               case the necStimStyle will be applied to all necessary
#'               stimulants.
#' @param necStimMap if more than one form is to be used for necessary
#'               stimulants, the user must indicate which form will be applied
#'               to which necessary stimulant. Only the necessary stimulants
#'               deviate from the necStimStyle need to be indicated. This
#'               indication will be achieve by passing a data.frame with the
#'               following columns: to, from, style, and threshold. to indicates the
#'               downstream node for the necessary stimulant. from the origin
#'               node, and style the name of the functional form for the
#'               necessary stimulant. The threshold column contains boolean
#'               values indicating the presence or absence of a threshold
#'               parameter.
#' @param saveNetwork logical. Defaults to TRUE. Indicates if the provided
#'               network object should be saved in the generated folder.
#' @param robustnessTest logical. Defaults to FALSE. Specifies if the nextStep
#'               function being built is part of a network robustness check.
#' @param altTopologyName default to NULL. If robustnessTest = T, this argument
#'               allows the user to keep the generated alternate nextStep function
#'               with a specific name. If no name is provided, the alternate
#'               nextStep functions will be names nextStepAlt.R.
#' @param exogenous default set to TRUE. This argument allows you to tailor the
#'               construction of a model in either C or C#. If TRUE, the generated
#'               code will allow for an exogenous supply to be added to the model.
#'
#' @export

buildModel <- function(network,
                       folder = "./Model",
                       forceOverwrite = FALSE,
                       altSource = FALSE,
                       language = "R",
                       splitCompartment = FALSE,
                       maxStep = 100,
                       steadyThreshold = 4,
                       ruleStyle = "Dun",
                       necStimStyle = "Linear",
                       necStimFile = NULL,
                       necStimMap = NULL,
                       saveNetwork = T,
                       robustnessTest = F,
                       altTopologyName = NULL,
                       exogenous = TRUE) {

  # determining the forms that a necessary stimulant can take.
  if (necStimStyle == "Michaelis-Menten") { necStimFile <- c(necStimFile, "./inst/nStim_MM")
  } else if (necStimStyle == "switch-like") { necStimFile <- c(necStimFile, "./inst/nStim_sl")
  } else {
    if (!necStimStyle == "Linear") stop("An invalid necStimStyle has been chosen. Only 'linear', 'nStim_MM', or 'nStim_sl' are available as native functional forms.")
  }

  if (is.data.frame(necStimMap) & any(necStimMap$style %in% c("nStim_sl", "nStim_MM"))) {
    if ("nStim_sl" %in% necStimMap$style) { necStimFile <- c(necStimFile, "./inst/nStim_sl")
    } else if ("nStim_MM" %in% necStimMap$style) { necStimFile <- c(necStimFile, "./inst/nStim_MM")}
  }

  necStimFile <- unique(necStimFile)

  # collecting the functional forms and their names
  functionText <- lapply(necStimFile, function(x) {readLines(file(x))})
  functionName <- lapply(functionText, function(x) {regmatches(x[1], regexpr("^.*?(?=[^A-Za-z0-9_])", x[1], perl = T))})

  # calling other function if language is "C"
  if (language == "C" | language == "C#") {
    generateC(network, maxStep = maxStep, steadyThreshold = steadyThreshold,
              folder = folder, forceOverwrite = forceOverwrite,
              sharp = if (language == "C") {FALSE} else {TRUE},
              exogenous = exogenous)
    return(NULL)
  }

  # a place to save the equations
  if (dir.exists(folder) & forceOverwrite == FALSE) {
    stop("This folder already exists. If you want to overwrite folder contents,
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
  # modifier data.frames do not need to be generated
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

    if (file.exists(paste0(genfile, ".RData"))) file.remove(paste0(genfile, ".RData"))
    save(genotypeDef, file = paste0(genfile, ".RData"))

    if (file.exists(paste0(nodefile, ".RData"))) file.remove(paste0(nodefile, ".RData"))
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
  if (!(necStimStyle == "Linear" & is.null(necStimFile))) {
    cat("\t# functions to define the forms of a necessary stimulant.\n", file = funcfile, append = T)
    for (i in 1:length(functionText)) {
      cat(paste0("\t", functionText[[i]]), sep = "\n", file = funcfile, append = T)
      cat("\n", file = funcfile, append = T)
    }
  }

  for (i in 1:length(nodes)) {
    eqNstimMap <- NULL
    if (nodes[[i]]@name %in% necStimMap$to) {
      inNecStim <- nodes[[i]]@inputs$Node[which(nodes[[i]]@inputs$Influence == "necessary stimulation")]
      eqNstimMap <- data.frame(necInput = inNecStim, func = necStimMap$style[which(necStimMap$from %in% inNecStim)])
    }
    equation <- generateEquation(nodes[[i]],
                                genotypes,
                                language = language,
                                ruleStyle = ruleStyle,
                                necStimFunc = eqNstimMap)
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
#' @param maxStep the maximum value that the simulation will be allowed ton proceed.
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
#' @param sharp logical. Indicates if the code is being generated is C# rather
#'               than C.
#' @param exogenous default set to TRUE. This argument allows you to tailor the
#'               construction of a model in either C or C#. If TRUE, the generated
#'               code will allow for an exogenous supply to be added to the model.
#' @importFrom stringr str_remove

generateC <- function(network,
                      maxStep = 100,
                      steadyThreshold = 4,
                      folder = "./Model",
                      forceOverwrite = FALSE,
                      ruleStyle = "Dun",
                      necStimFunc = NULL,
                      sharp = FALSE,
                      exogenous = TRUE) {

  # defining constants
  insertTMAX = maxStep
  insertTHRESHOLD = 10^(-steadyThreshold)

  # change altSources to stimulations
  network@objects$Hormones <- altSourceToStimulant(network@objects$Hormones)
  hormonesWcompartment <- sub("\\.", "_", names(network@objects$Hormones))

  # create exogenous supply object if needed
  if (isFALSE(exogenous)) {
    insertEXOdefinition <- ""
    insertEXOargument   <- ""
    insertEXOobject   <- ""
    insertEXOtype <- ""
    insertEXOmainDef <- "\n"
  } else {
    insertEXOdefinition <- "\npublic struct ExoVals\n{\n\tinsertDATVALSdefinition\n\n\tpublic ExoVals(insertDATVALSarguments)\n\t{\n\t\tinsertDATVALSinternal\n\t}\n}\n"
    insertEXOargument   <- ", ExoVals exo"
    insertEXOobject   <- ", exo"
    insertEXOtype <- ", exoVals"
    insertEXOmainDef <- "\n\t\tExoVals exoVals = new ExoVals(insertEXOVALS);\n"
  }

  # function to append container info to hormone and genotype names
  getGeneContainer <- function(x) {
    paste(x@name, substr(names(x@expression[which(x@expression == 1)]), 1, 1), sep = "_")
  }

  # collect corrected hormone and gene names
  hormoneList <- names(network@objects$Hormones)
  geneList <- unname(unlist(sapply(network@objects$Genotypes, getGeneContainer)))

  # define node and gene names
  if (isFALSE(sharp)) {
    insertSTRUCTNODENAMES <- paste0("float m", hormoneList, ";", collapse = "\n\t")
    insertSTRUCTGENENAMES <- paste0("float m", geneList, ";", collapse = "\n\t")
  } else {
    insertDATVALSdefinition <- paste0("public float m", hormoneList, ";", collapse = "\n\t")
    insertDATVALSarguments <- paste0("float M", hormoneList, collapse = ", ")
    insertDATVALSinternal <- paste0("m", hormoneList, " = M", hormoneList, ";", collapse = "\n\t\t")

    insertGENEVALSdefinition <- paste0("public float m", geneList, ";", collapse = "\n\t")
    insertGENEVALSarguments <- paste0("float M", geneList, collapse = ", ")
    insertGENEVALSinternal <- paste0("m", geneList, " = M", geneList, ";", collapse = "\n\t\t")
  }

  # create standard values for nodes and genes
  if (isFALSE(sharp)) {
    insertDATVALS <- paste0("\tdat.m", hormoneList, " = 1;", collapse = "\n")
    insertGENEVALS <- paste0("\tgen.m", geneList, " = 1;", collapse = "\n")
  } else {
    insertDATVALS <- paste0(rep(1, length(hormoneList)), "f", collapse = ", ")
    insertGENEVALS <- paste0(rep(1, length(geneList)), "f", collapse = ", ")
    insertEXOVALS <- paste0(rep(0, length(hormoneList)), "f", collapse = ", ")
  }


  # define equations for nodes
  insertEQUATIONS <- rep(NA, length(network@objects$Hormones))
  for (i in 1:length(insertEQUATIONS)) {
    insertEQUATIONS[i] <- generateEquation(node = network@objects$Hormones[[i]],
                                           genotypes = network@objects$Genotypes,
                                           language = "C",
                                           ruleStyle = ruleStyle,
                                           necStimFunc = necStimFunc,
                                           sharp = sharp,
                                           exogenous = exogenous)
  }

  insertEQUATIONS <- paste0(if(isFALSE(sharp)) {"pDat->m"} else {"dat.m"},
                            names(network@objects$Hormones), " = ",
                            insertEQUATIONS, collapse = "\n\t\t")

  # create chain to check if nodes have changed in the previous timestep
  insertCOMPARISONCHAIN <- paste0(sprintf("%s(oldDat.m%s - newDat.m%s) > THRESHOLD",
                                          if(isFALSE(sharp)) {"fabs"} else {"Math.Abs"},
                                          hormoneList, hormoneList),
                                 collapse = "\n\t\t\t|| ")

  if (isFALSE(sharp)) {
    insertFINALPRINT <- paste0('fprintf(stderr, "', hormoneList,
                               ': %.6f, \\\\n\\\\t", dat.m', hormoneList, ')',
                               collapse = ";\n\t")
  } else {
    insertFINALPRINT <- paste0(sprintf('Console.WriteLine($"\\\\t%s: {finalVals.m%s:F6} \\\\n\\\\t");',
                                hormoneList, hormoneList), collapse = ";\n\t\t")
  }

  if (isFALSE(sharp)) {
    text_main.c <- readLines(file("./inst/Cscaffold/main.c"))
    text_psoup.c <- readLines(file("./inst/Cscaffold/psoup.c"))
    text_psoup.h <- readLines(file("./inst/Cscaffold/psoup.h"))

    insertReplacements <- list("insertTMAX" = insertTMAX,
                               "insertSTRUCTNODENAMES" = insertSTRUCTNODENAMES,
                               "insertSTRUCTGENENAMES" = insertSTRUCTGENENAMES,
                               "insertDATVALS" = insertDATVALS,
                               "insertGENEVALS" = insertGENEVALS,
                               "insertEQUATIONS" = insertEQUATIONS,
                               "insertTHRESHOLD" = insertTHRESHOLD,
                               "insertCOMPARISONCHAIN" = insertCOMPARISONCHAIN,
                               "insertFINALPRINT" = insertFINALPRINT)
  } else {
    text_program.cs <- readLines(file("./inst/CsharpScaffold/program.cs"))

    insertReplacements <- list("insertTMAX" = insertTMAX,
                               "insertEXOdefinition" = insertEXOdefinition,
                               "insertEXOmainDef" = insertEXOmainDef,
                               "insertDATVALSdefinition" = insertDATVALSdefinition,
                               "insertDATVALSarguments" = insertDATVALSarguments,
                               "insertDATVALSinternal" = insertDATVALSinternal,
                               "insertGENEVALSdefinition" = insertGENEVALSdefinition,
                               "insertGENEVALSarguments" = insertGENEVALSarguments,
                               "insertGENEVALSinternal" = insertGENEVALSinternal,
                               "insertDATVALS" = insertDATVALS,
                               "insertGENEVALS" = insertGENEVALS,
                               "insertEXOVALS" = insertEXOVALS,
                               "insertEXOargument" = insertEXOargument,
                               "insertEXOobject" = insertEXOobject,
                               "insertEXOtype" = insertEXOtype,
                               "insertEQUATIONS" = insertEQUATIONS,
                               "insertTHRESHOLD" = insertTHRESHOLD,
                               "insertCOMPARISONCHAIN" = insertCOMPARISONCHAIN,
                               "insertFINALPRINT" = insertFINALPRINT)
  }


  # a function that will be supplied to sapply to simultaneously find the location
  # of all keywords
  containsKey <- function(x, key) {
    test <- grep(key, x)
    if (length(test) == 0) {
      test = 0
    }
    test
  }

  # a function to replace key words
  replaceKeyWords <- function(text, insertReplacements) {
    for (i in 1:length(insertReplacements)) {
      whichLine <- as.logical(sapply(text, containsKey, key = names(insertReplacements)[i], USE.NAMES = F))
      text[whichLine] <- sub(names(insertReplacements)[i], insertReplacements[i], text[whichLine])
    }
    text
  }

  if (isFALSE(sharp)) {
    text_main.c <- replaceKeyWords(text_main.c, insertReplacements)
    text_psoup.c <- replaceKeyWords(text_psoup.c, insertReplacements)
    text_psoup.h <- replaceKeyWords(text_psoup.h, insertReplacements)
  } else {
    text_program.cs <-replaceKeyWords(text_program.cs, insertReplacements)
  }

  # a place to save the equations
  if (dir.exists(folder) & forceOverwrite == FALSE) {
    stop("This folder already exists. If you want to overwrite this folder,
         set forceOverwrite to TRUE.")
  } else {
    dir.create(folder)
  }

  # a function to write the
  writeCtoFile <- function(text, file) {
    file.create(file)

    cat(paste0(text[1], "\n"), file = file)

    for (i in 2:length(text)) {
      cat(paste0(text[i], "\n"), file = file, append = T)
    }
  }

  if (isFALSE(sharp)) {
    writeCtoFile(text_main.c, paste0(folder, "/main.c"))
    writeCtoFile(text_psoup.c, paste0(folder, "/psoup.c"))
    writeCtoFile(text_psoup.h, paste0(folder, "/psoup.h"))
  } else {
    writeCtoFile(text_program.cs, paste0(folder, "/program.cs"))
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
#' @param genotypes the list of genes frome within a network object
#' @param language which programming language should the equation be generated in?
#'        Can be either "R", or "C".
#' @param ruleStyle either "Dun", or "Mike". The Dun style resembles the original
#'        Dun equations normalised such that WT conditions are always 1. The Mike
#'        style creates mirrored stimulatory and inhibitory effects.
#' @param necStimFunc a data.frame containing the columns: necInput, and function.
#'        The name of the function will be applied to the indicated necessary
#'        stimulant. The default is NULL, in which case no function will be
#'        applied to necessary stimulants, and therefore the form will be linear.
#' @param threshold if the necStimFunc has a threshold, the argument name for
#'        that threshold should be specified here.
#' @param sharp description
#' @importFrom methods is

generateEquation <- function(node,
                             genotypes,
                             language,
                             ruleStyle = "Dun",
                             necStimFunc = NULL,
                             threshold = NULL,
                             sharp = FALSE,
                             exogenous = TRUE) {
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
                            operator = node@inputs[!is.na(node@inputs$Coregulator) & node@inputs$Influence == "stimulation", "Operator"],
                            sharp = sharp)

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
                            operator = node@inputs[!is.na(node@inputs$Coregulator) & node@inputs$Influence == "inhibition", "Operator"],
                            sharp = sharp)

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
    } else if (is(stimString, "character") & is(inhibString, "character")) {
      # if there are both stimulatory and inhibitory effects
      allModulations <- sprintf("2 * (%s)/(1 + %s)", stimString, inhibString)
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
    } else if (is(stimString, "character") & is(inhibString, "character")) {
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
  if (length(node@genotypes) > 0) {
    genes <- node@genotypes
    genoString <- rep(NA, length(genes))
    for (g in 1:length(genes)) {
      if (is(genotypes[[genes[g]]]@coregulator, "character")) {
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

  if (any(node@inputs$Influence == "necessary stimulation")) {
    necstimString <- node@inputs$Node[node@inputs$Influence %in% "necessary stimulation" & is.na(node@inputs$Coregulator)]
    if (!is.null(necStimFunc)) {
      if (length(necstimString) < nrow(necStimFunc)) {
        necStimFuncNoCoreg <- rbind(necStimFunc, data.frame(necInput = necstimString[which(!necstimString %in% eqNstimMap$necInput)], func = NA))
      }
    }

    if (length(necstimString) > 0) {
      necstimString <- differenceString(necstimString,
                                        node@inputs$Operator[node@inputs$Operator == "delay" & node@inputs$Influence == "necessary stimulation"],
                                        takeProduct = if (length(necstimString) == 1) {NULL} else {T},
                                        language = language)


      if (!is.null(necStimFunc)) {
        a <- if (any(!is.na(necStimFunc$func))) {paste0(necStimFunc$func[!is.na(necStimFunc$func)],
                                                        "(", necStimFunc$necInput[!is.na(necStimFunc$func)],
                                                        ")", collapse = "*")} else {NULL}
        b <- if (any(is.na(necStimFunc$func))) {paste0("(", necStimFunc$necInput[is.na(necStimFunc$func)],
                                                       ")", collapse = "*")} else {NULL}
        necstimString <- paste0(a, b, collapse = "*")
      }
    }

    # are there any necessary stimulants that are coregulators
    if (any(!is.na(node@inputs$Coregulator) & node@inputs$Influence == "necessary stimulation")) {
      coregInput <- node@inputs[!is.na(node@inputs$Coregulator) & node@inputs$Influence == "necessary stimulation", 1:2]

      # NEED TO EDIT COREGULATORS FUNCTION SO THAT IT CAN RETURN A NON CONCATENATED VECTOR FOR THE PURPOSE OF ADDING A FUNCTION
      coreg <- coregulators(coregInput, returnNum = T, language = language,
                            operator = node@inputs[!is.na(node@inputs$Coregulator) & node@inputs$Influence == "necessary stimulation", "Operator"],
                            sharp = sharp)

      if (!is.null(necStimFunc)) {
        necstimString = paste0(necStimFunc, "(", coreg$coreg, ")", collapse = " * ")
      }

      necstimString = paste(necstimString, coreg$coreg, collapse = " * ")
    }
    allModulations <- sprintf("(%s) * (%s)", allModulations, necstimString)
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

  if (isTRUE(exogenous)) {
    if (language == "C" | language == "C#") {
      allModulations <- paste0(allModulations, " + exo.m", node@name)
    }
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
#' @param coreg a subset of a the inputs of a hormone object. This subset
#'        must all have the same modulating effect, and have coregulators
#' @param returnNum return the number of unique coregulator sets. Default is
#'        set to FALSE.
#' @param language which programming language should the equation be generated in?
#'        Can be either "R", or "C".
#' @param operator which operator defines the coregulator, either "and" or "or".
#' @param sharp description

coregulators <- function(coreg,
                         returnNum = FALSE,
                         language,
                         operator,
                         sharp = FALSE) {
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
      coregString[r] <- paste0("oldDat.m", coreg[r, !is.na(coreg[r, ])], collapse = ", ")
    } else {
      stop("Incorrect language specification. Can only accept 'R', or 'C'.")
    }
  }

  # only keep the first instance of a piece of information
  firstApp <- !duplicated(coregString)

  coregString <- coregString[firstApp]
  operator    <- operator[firstApp]

  # perform the correct calculation depending on operator
  for (i in 1:length(coregString)) {
    if (operator[i] == "and") {
      if (language == "R") {
        coregString[i] <- paste0("min(", coregString[i], ")")
      } else if (language == "C") {
        if (isFALSE(sharp)) {
          coregString[i] <- paste0("getMin(", coregString[i], ")")
        } else {
          coregVector <- strsplit(coregString[i], ",")[[1]]
          numCoreg <- length(coregVector)
          coregString[i] <- paste0(paste0(rep("Math.Min(", numCoreg - 1),
                                          coregVector[-numCoreg], collapse = ", "),
                                   ", ", coregVector[numCoreg], paste0(rep(")", numCoreg - 1),
                                                                       collapse = ""))

        }
      }
    } else if (operator[i] == "or") {
      if (language == "R") {
        coregString[i] <- paste0("max(", coregString[i], ")")
      } else if (language == "C") {
        coregString[i] <- paste0("getMax(", coregString[i], ")")
      }
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
  if (length(delays) == 0) {
    delays <- NA
  }

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

