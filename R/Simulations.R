#' A function to simulate a network
#'
#' Takes the output folder of the buildNetwork function and uses it to run a
#' network simulation. The simulation will run until a steady state has been
#' achieved unless otherwise specified. A steady state is considered to be
#' reached when all node values are considered to be equivelant to the previous
#' timestep for up to a threshold of decimal places.
#' @note this function can only be used if the output of the buildModel function
#'             was generated with the language argument set to "R".
#' @param folder the directory of the folder generated by the buildModel function.
#' @param delay the amount of delay to apply for delayed action of nodes. The
#'             default is set to 2.
#' @param maxStep the maximum number of steps that you want to simulate for. Will
#'             terminate simulation when steady state is reached, unless maxStep is
#'             reached first. If set to NA (the default), will simulate until
#'             stability is reached.
#' @param genotype default set to NA. Allows the user to provide a different genotypeDef
#'             data.frame from the one specified in the provided file.
#' @param startingValues default set to NA. Allows the user to provide a different
#'             nodestartDef data.frame from the one specified in the provided file.
#' @param steadyThreshold the number of decimal places to which node values must
#'             be equivalent to be considered a steady state. This threshold must
#'             be passed for all nodes.
#' @param exogenousSupply specifies if the value of a node (or nodes) is
#'               determined by an outside supply. In this case, the value of the
#'               node is supplied by the user and remains consistent throughout
#'               the course of the simulation. The default value for this
#'               argument is FALSE. If set to TRUE, there must exist an exogenousDef
#'               object in the model folder. This can be generated with the
#'               exogenousScreen function.
#' @param necStimThreshold if the form of any necessary stimulant contains a
#'               threshold, the user must indicate said threshold and to which
#'               form the threshold will be applied. This
#'               indication will be achieved by passing a data.frame with the
#'               following columns: to, from, style, and threshold. to indicates
#'               the downstream node for the necessary stimulant. from the origin
#'               node, and style the name of the functional form for the
#'               necessary stimulant. The threshold column contains the
#'               values of the threshold parameter. Information only needs to
#'               be provided for cases where a threshold applies.
#' @param robustnessTest logical. Defaults to FALSE. Specifies if the nextStep
#'               function being used is part of a network robustness check.
#' @param altTopologyName default to NULL. If robustnessTest =TRUE, this argument
#'               allows the user to keep the generated alternate nextStep function
#'               with a specific name. If no name is provided, the alternate
#'               nextStep functions will be names nextStepAlt.R.
#' @param reduceSize logical. Default set to FALSE. This argument is used to
#'               minimise the size of simulation output. If set to TRUE, only
#'               the final values of the simulation will be returned if the
#'               simulation reached stability. If the simulation was not able to
#'               reach stability, the full output of the simulation will be returned.
#' @export

simulateNetwork <- function(folder,
                            delay = 2,
                            maxStep = NA,
                            genotype = NA,
                            startingValues = NA,
                            steadyThreshold = 4,
                            exogenousSupply = FALSE,
                            necStimThreshold = NULL,
                            robustnessTest = FALSE,
                            altTopologyName = NULL,
                            reduceSize = FALSE) {
  # Checking if a meaningful delay has been provided
  if (delay == 1) {
    warning("You have selected a delay of 1 which is functionaly equivalent to
            no delay at all.")
  } else if (delay < 1) {
    warning("You have chosen an invalid delay. The delay has been reset to 2.")
    delay = 2
  }

  # sourcing data for simulation
  if (robustnessTest == FALSE) {
    source(paste0(folder, "/nextStep.R"), local = TRUE)
  } else {
    if (is.null(altTopologyName) | altTopologyName == "" | altTopologyName == "nextStep.R") {
      source(paste0(folder, "/nextStep.R"), local = TRUE)
    } else {
      source(paste0(folder, "/", altTopologyName, "_nextStepAlt.R"))
    }
  }


  if (any(is.na(genotype))) { # if a genotype has not been explicitly provided
    load(paste0(folder, "/genotypeDef.RData"))
  } else {
    genotypeDef <- genotype
  }

  if (any(is.na(startingValues))) { # if starting vals have not been explicitly provided
    load(paste0(folder, "/nodestartDef.RData"))
  } else {
    nodestartDef <- startingValues
  }

  if (nrow(genotypeDef) > 1 | nrow(nodestartDef) > 1) {
    stop("You have provided more than one condition. Consider using the setupSims function instead.")
  }

  if (is.null(exogenousSupply)) {
    exogenousDef <- NULL
    exogenousSupply <- FALSE
  } else if (is(exogenousSupply, "data.frame") | is(exogenousSupply, "integer")) {
    exogenousDef <- exogenousSupply
  } else if (exogenousSupply == TRUE) {
    load(paste0(folder, "/exogenousDef.RData")) # should probably remove this condition
  } else {
    exogenousDef <- NULL
  }

  # Run simulations
  if (is.na(maxStep)) {
    rowChunk <- 100
  } else {
    rowChunk <- maxStep
  }

  simDat <- nodestartDef # initiating data.frame
  simDat[2:(rowChunk + delay - 1), ] <- NA
  simDat[1:delay, ] <- nodestartDef

  t <- 2
  while (t <= nrow(simDat)) {
    row = t + delay - 1
    simDat[row, ] <- nextStep(dat = simDat[c((row-delay), (row-1)), ],
                              gen = genotypeDef[1,], delay = delay)

    # If any node has an exogenous supply
    if (!is.null(exogenousDef)) {
      # add the exogenous amount to the value calculated by nextStep
      simDat[row, names(exogenousDef)] <- simDat[row, names(exogenousDef)] + unname(exogenousDef)
    }

    # If reached steady state, return simDat
    if(all(round(simDat[row,], steadyThreshold) == round(simDat[row - 1,], steadyThreshold))) {
      if (reduceSize == TRUE) {
        simDat = simDat[row, ]
      } else {
        simDat = simDat[delay:row, ]
      }

      return(list(simulation = simDat, stable = TRUE))
    }

    # If maxStep has been reached (if there even is one), return simDat
    if (!is.na(maxStep) & t == maxStep) {
      warning("The maximum timestep has been reached without achieving stability.")
      simDat = simDat[delay:row, ]
      return(list(simulation = simDat, stable = FALSE))
    }

    # If any node has reached infinity
    if (any(is.infinite(unlist(simDat[row, ])))) {
      warning(paste0("The simulation was terminated at time ", t, " as the following node/s reached infinity: ",
                    paste(names(simDat)[is.infinite(unlist(simDat[row, ]))], collapse = ", "), "."))
      simDat = simDat[delay:row, ]
      return(list(simulation = simDat, stable = FALSE))
    }

    # If simDat has been filled without reaching steady state or maxStep, add more rows
    if (row == nrow(simDat)) {
      simDat[(row + 1):(row + rowChunk), ] <- NA
    }

    t <- t + 1
  }
}

#' A wrapper function that allows you to run simulations of a network under
#' many different conditions.
#'
#' This function allows you to set up a variety of genotype and starting node
#' conditions to be simulated. It also allows you to test the outcome of
#' simulations under randomly assigned starting values. The function will make
#' sure to include one wild type condition (all genotypes set to 1) regardless
#' of if specified by the user.
#' @param folder the model folder generated by the buildModel function.
#' @param delay the amount of delay for any delayed transport. Default set to 2.
#' @param maxStep the maximum number of timesteps for which to simulate.
#' @param steadyThreshold the number of decimal places to which node values must
#'             be equivalent to be considered a steady state. This threshold must
#'             be passed for all nodes.
#' @param exogenousSupply specifies if the value of a node (or nodes) is
#'               determined by an outside supply. In this case, the value of the
#'               node is supplied by the user and remains consistent throughout
#'               the course of the simulation. The default value for this
#'               argument is FALSE. If set to TRUE, there must exist an exogenousDef
#'               object in the model folder. This can be generated with the
#'               exogenousScreen function.
#' @param priorScreen logical. Specifies if the function should collect
#'               modifier values generated from generated prior distributions.
#'               Default is set to FALSE.
#' @param robustnessTest logical. Defaults to FALSE. Specifies if the nextStep
#'               function being used is part of a network robustness check.
#' @param genotypeBaseline logical. Defaults to FALSE. Specified if only the
#'               first row of the genotype object should be used. If selected,
#'               the output object name will reflect that decision. If
#'               genotypeBaseline = TRUE and priorScreen = TRUE, a warning will
#'               be thrown to the user and the priorScreen argument will take
#'               precedence.
#' @param nodestartBaseline logical. Defaults to FALSE. Specified if only the
#'               first row of the nodestart object should be used. If selected,
#'               the output object name will reflect that decision.
#' @param necStimThreshold if the form of any necessary stimulant contains a
#'               threshold, the user must indicate said threshold and to which
#'               form the threshold will be applied. This
#'               indication will be achieved by passing a data.frame with the
#'               following columns: to, from, style, and threshold. to indicates
#'               the downstream node for the necessary stimulant. from the origin
#'               node, and style the name of the functional form for the
#'               necessary stimulant. The threshold column contains the
#'               values of the threshold parameter. Information only needs to
#'               be provided for cases where a threshold applies.
#' @param altTopologyName default to NULL. If robustnessTest = TRUE, this argument
#'               allows the user to keep the generated alternate nextStep function
#'               with a specific name. If no name is provided, the alternate
#'               nextStep functions will be names nextStepAlt.R.
#' @param saveOutput logical. Default set to TRUE. Indicates if the output of
#'               simulation screen should be automatically saved in the
#'               provided folder location upon completion.
#' @param combinatorial logical. States if setupSims should simulate every
#'               combination of conditions that are being explored. If set to
#'               TRUE, every row from each set of explored conditions will be
#'               simulated against all rows from every other explored conditions
#'               (with the explored conditions relating to each data.frame that
#'               you have specified for setupSims to explore). If set to FALSE,
#'               it will be expected that all of the data.frames of conditions
#'               that have been passed to setupSims will have the same number of
#'               rows. In this case, each row will only be paired with the
#'               equivalent row of each data.frame. When this argument is set to
#'               FALSE, it is up to the user to make sure that all of the
#'               appropriate control conditions exist.
#' @param preventDrop logical. Allows the user to override setupSims dropping
#'               any repeated conditions. This will also prevent reorganisation
#'               of conditions for clarity. Default is set to FALSE.
#' @param reduceSize logical. Default set to FALSE. This argument is used to
#'               minimise the size of simulation output. If set to TRUE, only
#'               the final values of the simulation will be returned if the
#'               simulation reached stability. If the simulation was not able to
#'               reach stability, the full output of the simulation will be returned.
#' @param nCores the number of cores to be used to run simulations. The default
#'               is set to 1, in which case simulations will be run sequentially
#'               on a singe core. If greater than 1, simulations will be run in
#'               parallel across the number of cores indicated. You can check
#'               the number of cores on your system with the function
#'               detectCores() from the parallel package. It is recommended that
#'               the maximum number of cores that you chose is at lease one less
#'               than the number returned by detectCores().
#' @importFrom stats runif
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom parallel makeCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel stopCluster
#'
#' @export

setupSims <- function(folder,
                      delay = 2,
                      maxStep = 100,
                      steadyThreshold = 4,
                      exogenousSupply = FALSE,
                      priorScreen = FALSE,
                      robustnessTest = FALSE,
                      genotypeBaseline = FALSE,
                      nodestartBaseline = FALSE,
                      necStimThreshold = NULL,
                      altTopologyName = NULL,
                      saveOutput = TRUE,
                      combinatorial = TRUE,
                      preventDrop = FALSE,
                      reduceSize = FALSE,
                      nCores = 1) {
  # loading parameter values
  if (priorScreen == TRUE) {
    if (!file.exists(paste0(folder, "/priorDef.RData"))) {
      stop("You have requested to use modifer values generated from a prior distribution, but there is no priorDef.RData file in the folder indicated.")
    }
    load(paste0(folder, "/priorDef.RData"))
    genotypeDef <- priorDef

    if (genotypeBaseline == TRUE) {
      warning("You cannot only run simulations with only the genetic baseline if running a prior screen. The genotypeBaseline argument has been set to FALSE.")
      genotypeBaseline <- FALSE
    }
  } else {
    load(paste0(folder, "/genotypeDef.RData"))

    if (genotypeBaseline == TRUE) {
      genotypeDef <- genotypeDef[1,]
      genotypeDef[1,] <- 1
    }
  }

  if (is.na(maxStep)) {
    maxStep <- 100
    warning("You did not provide a maxStep value. It has been set to 100.")
  }

  load(paste0(folder, "/nodestartDef.RData"))
  if (nodestartBaseline == TRUE) {
    nodestartDef <- nodestartDef[1,]
    nodestartDef[1,] <- 1
  }

  if (is.null(exogenousSupply)) {
    exogenousDef <- NULL
    exogenousSupply <- FALSE
  } else if (exogenousSupply == TRUE) {
    load(paste0(folder, "/exogenousDef.RData"))
  } else {
    exogenousDef <- NULL
  }


  if (isTRUE(combinatorial)) {
    # Ensuring that the first row of any screening dataframes contain a wildtype condition in the first row
    nodestartDef    <- tidyScreen(nodestartDef, "nodestartDef")
    genotypeDef     <- tidyScreen(genotypeDef, "genotypeDef")
    exogenousDef    <- tidyScreen(exogenousDef, "exogenousDef", exogenous = TRUE)
  } else {
    sizeCheck <- c(nrow(nodestartDef), nrow(genotypeDef), nrow(exogenousDef)) # collecting number of conditions

    if (any(!sizeCheck %in% c(1, max(sizeCheck)))) {
      stop("Objects defining the conditions to be explored must have the same number of rows (excluding any conditions that are being maintained at baseline.")
    }

    WT <- c(n = F, g = F, e = F) # To keep track if

    if (nrow(nodestartDef) == 1) nodestartDef[2:max(sizeCheck), ] <- nodestartDef[1, ]; WT['n'] <- T
    if (nrow(genotypeDef) == 1) genotypeDef[2:max(sizeCheck), ]   <- genotypeDef[1, ]; WT['g'] <- T
    if (!is.null(exogenousDef)) {
      if (ncol(exogenousDef) == 1) { #
        exNames <- colnames(exogenousDef)
      }

      if (nrow(exogenousDef) == 1) exogenousDef[2:max(sizeCheck), ]   <- exogenousDef[1, ]; WT['e'] <- T

      if (ncol(exogenousDef) == 1) { #
        exogenousDef  <- as.data.frame(exogenousDef)
        colnames(exogenousDef) <- exNames
      }
    }


    if (isFALSE(preventDrop)) {
      if (is.null(exogenousDef)) { # checking if simulations will be run more than once
        duplications <- duplicated(cbind(nodestartDef, genotypeDef))
      } else {
        duplications <- duplicated(cbind(nodestartDef, genotypeDef, exogenousDef))
      }

      if (any(duplications)) {
        nodestartDef <- nodestartDef[-which(duplications), ]
        genotypeDef  <- genotypeDef[-which(duplications), ]
        if (ncol(exogenousDef) == 1) {
          exogenousDef <- as.data.frame(exogenousDef[-which(duplications), ])
          colnames(exogenousDef) <- exNames
        } else {
          exogenousDef <- exogenousDef[-which(duplications), ]
        }

        if (isTRUE(WT['n'])) save(nodestartDef, file = paste0(folder, "/nodestartDef.RData"))
        if (isTRUE(WT['g'])) save(genotypeDef, file =  paste0(folder, "/genotypeDef.RData"))
        if (isTRUE(WT['e'])) save(exogenousDef, file = paste0(folder, "/exogenousDef.RData"))

        warning("The following conditions have been removed due to repetition: ",
                paste0(which(duplications), collapse = ", "),
                ". These rows have been removed from the objects contained in your model folder.")

      }
    }
  }

  # establishing the indexes for the various
  if (combinatorial == TRUE) {
    INDEX <- expand.grid(d = 1:nrow(nodestartDef), g = 1:nrow(genotypeDef),
                         ex = if (is.null(exogenousDef)) {FALSE} else {1:nrow(exogenousDef)})
  } else {
    INDEX <- data.frame(d = 1:nrow(nodestartDef), g = 1:nrow(genotypeDef),
                         ex = if (is.null(exogenousDef)) {FALSE} else {1:nrow(exogenousDef)})
  }

  # prepare clones to run in parallel
  cl <- makeCluster(nCores)
  registerDoParallel(cl)

  # Run simulations
  sims <- foreach(i = 1:nrow(INDEX), .packages = "PSoup") %dopar% {
    if (is.null(exogenousDef)) {
      exogenousCondition <- FALSE
    } else {
      exogenousCondition <- exogenousDef[INDEX$ex[i], ]
      if (ncol(exogenousDef) == 1) {
        names(exogenousCondition) <- exNames
      }
    }
    simulation = simulateNetwork(folder = folder,
                                 delay = delay,
                                 maxStep = maxStep,
                                 genotype = genotypeDef[INDEX$g[i], ],
                                 startingValues = nodestartDef[INDEX$d[i], ],
                                 exogenousSupply = exogenousCondition,
                                 robustnessTest = robustnessTest,
                                 altTopologyName = altTopologyName,
                                 reduceSize = reduceSize)

    list(scenario = list(genotype = genotypeDef[INDEX$g[i], ],
                                    startingValues = nodestartDef[INDEX$d[i], ],
                                    exogenousSupply = exogenousCondition),
                    simulation = simulation$simulation,
                    stable = simulation$stable)

    #report(i, nrow(nodestartDef))
  }

  stopCluster(cl)

  output <- list("modelFolder" = folder,
                 "parameters" = c("maxStep" = maxStep, "delay" = delay),
                 "screen" = sims)

  # Creating a string to inform which conditions were screened
  outputName <- NULL
  if (priorScreen == T) {
    outputName <- "priorDef"
  } else {
    if (genotypeBaseline == FALSE) {
      outputName <- "genotypeDef"
    }
  }
  if (nodestartBaseline == FALSE) outputName <- paste0(outputName, "_nodestartDef")
  if (exogenousSupply == TRUE) outputName <- paste0(outputName, "_exogenousDef")
  if (is.null(outputName)) {
    outputName <- "baseline"
    warning("You have only simulated the baseline condition.")
  }

  # remove initial underscore if modifiers have been held at baseline
  if (substring(outputName, 1, 1) == "_") {
    outputName <- substring(outputName, 2)
  }

  if (saveOutput == TRUE) {
    if (!is.null(altTopologyName)) altTopologyName <- paste0(altTopologyName, "_")
    save(output, file = paste0(folder, "/", altTopologyName, outputName, "_Sims.RData"))
  }

  return(output)
}

#' A function to generate genotype screens.
#'
#' Will generate genotype screens which considers compartmental information,
#' and allows for the possibility of multiple types of mutation.
#' @param folder the model folder generated by the buildModel function.
#' @param numMutations how many concurrent mutations do you want to test.
#' @param mutationVal for each combination of mutated genes, what value should
#'        the genes take on? Default is set to 0 (knockout).
#' @param returnExcel default set to FALSE. If TRUE, will create an excel file
#'        and save it in the model folder. This file will contain a matrix
#'        showing all of the genotype combinations that have been screened. This
#'        can be used to record any existing experimental outcomes, to be
#'        compared against simulated outcomes later on.
#' @param graft default set to FALSE. Indicates if the user wants to
#'        consider differential expression between compartments.
#' @importFrom utils combn
#' @importFrom utils write.table
#' @export

genotypeScreen <- function(folder,
                           numMutations = 1,
                           mutationVal = 0,
                           returnExcel = FALSE,
                           graft = FALSE) {
  if (file.exists(paste0(folder, "/genotypeDef.RData"))) {
    load(paste0(folder, "/genotypeDef.RData"))
  } else {
    source(paste0(folder, "/genotypeDef.R"))
  }

  if (nrow(genotypeDef) > 1) {
    warning("Overwrote existing extended genotype screen.")
    genotypeDef <- genotypeDef[1, ]
  }

  # collect the compartment extention of nodes
  comp = unlist(strsplit(colnames(genotypeDef), "_"))[c(FALSE, TRUE)]

  compartments <- vector("list", length = length(unique(comp)))
  names(compartments) <- unique(comp)

  for (i in 1:length(compartments)) {
    compartments[[i]] <- colnames(genotypeDef)[which(comp == unique(comp)[i])]
  }

  if (graft == FALSE) {
    noComp <- unlist(strsplit(colnames(genotypeDef), "_"))[c(TRUE, FALSE)]
    genotypeDef[2:((numCombn(length(unique(noComp)), numMutations)*length(mutationVal)) + 1), ] <- 1
  } else {
    genotypeDef[2:(numCombn(ncol(genotypeDef), numMutations)*length(mutationVal) + 1), ] <- 1
  }

  if (graft == FALSE){
    combGen <- combn(unique(noComp), numMutations)
    for (i in 2:nrow(genotypeDef)) {
      genotypeDef[i, which(noComp %in% combGen[, i - 1])] <- mutationVal
    }
  } else {
    combCol <- combn(ncol(genotypeDef), numMutations)
    for (i in 2:nrow(genotypeDef)) {
      genotypeDef[i, combCol[, i - 1]] <- mutationVal
    }
  }

  save(genotypeDef, file = paste0(folder, "/genotypeDef.Rdata"))
  return("File overwritten.")

  if (returnExcel == TRUE) {
    sheets = vector("list", length = length(mutationVal))
    names(sheets) <- paste0("MutationVal=", mutationVal)

    spreadSheet <- matrix(NA, ncol = ncol(nodestartDef), nrow = nrow(genotypeDef))

    colnames(spreadSheet) <- colnames(nodestartDef)
    mutated <- genotypeDef != 1
    rows <- apply(mutated, 1, which)
    rowNames <- lapply(rows, names)
    rownames(spreadSheet) <- c("WT", sapply(rowNames, paste, collapse = ", ")[-1])

    write.table(spreadSheet,
                file = paste0(folder, "/experimentalData_mutationVal=",mutationVal,".csv"),
                sep = ",", row.names =TRUE, col.names = NA)
  }
}

#' A function to generate a number of random starting point reassignments.
#'
#' Starting points will be generated for each node, pulled from a uniform
#' distribution. The existing starting values in the model folder will not
#' be overwritten.
#' @param folder the model folder generated by the buildModel function.
#' @param restarts the number of randomly assigned starting points that you
#'        want to test.
#' @param minVal default set to 0. The minimum value for a node to be.
#' @param maxVal default set to 2. The maximum value for a node to be.
#' @export

randomStartScreen <- function(folder,
                              restarts,
                              minVal = 0,
                              maxVal = 2) {

  if (minVal < 0) {
    minVal = 0
    warning("Expression levels cannot be below 0. Have reassigned the minimum
            value to 0.")
  }

  if (file.exists(paste0(folder, "/nodestartDef.RData"))) {
    load(paste0(folder, "/nodestartDef.RData"))
  } else {
    source(paste0(folder, "/nodestartDef.R"))
  }

  nodestartDef[(nrow(nodestartDef) + 1):(nrow(nodestartDef) + restarts), ] <- round(runif(restarts*ncol(nodestartDef),
                                                                                          minVal, maxVal), 4)
  save(nodestartDef, file = paste0(folder, "/nodestartDef.Rdata"))
}

#' A function to generate all combinations of two vectors containing the values
#' to be screened for two nodes. This function will also include the possibility
#' of no exogenous values being provided in the form of NA.
#'
#' @param nodes a vector containing the names of two nodes of interest
#' @param screen1 a vector containing all the values to be tested for the
#'                first listed node.
#' @param screen2 a vector containing all the values to be tested for the
#'                second listed node.
#' @param folder the directory for the model folder in which to save the output
#'        of this function.
#' @export
exogenousScreen <- function(nodes, screen1, screen2, folder) {
  # making sure that there is a condition where no exogenous hormone is provided
  if (!0 %in% screen1) {screen1 <- c(0, screen1)}
  if (!0 %in% screen1) {screen1 <- c(0, screen1)}

  exogenousDef <- expand.grid(screen1, screen2)
  colnames(exogenousDef) <- nodes

  save(exogenousDef, file = paste0(folder, "/exogenousDef.RData"))
}

#' A function to calculate the number of unique combinations of a vector
#' of length n of sample size r.
#'
#' @param n number of items in the vector.
#' @param r the size of the sample.

numCombn <- function(n, r) {
  if (r > n) {
    stop("Your sample size is greater than the length of the vector.")
  }

  factorial(n)/(factorial(r)*factorial(n-r))
}

#' A function to make sure that data.frames containing conditions to screen
#' are organised correctly. The wild-type condition is made to come first,
#' and any duplicate rows are removed.
#'
#' @param frame a data.frame
#' @param name a string giving the name of the data frame in case a warning is generated.
#' @param exogenous logical. Indicates if the wild type row should be 1s or 0s.
#'                  if checking modifier or node screens, should be set to F.
#'                  If checking exogenous screens, should be T.
#' @importFrom prodlim row.match
tidyScreen <- function(frame, name, exogenous = FALSE) {
  WT <- as.numeric(!exogenous)

  warn <- NULL # initiate vector to store warning messages.

  # remove any row duplication
  if (any(duplicated(frame))) {
    frame <- frame[!duplicated(frame), ]
    warn <- c(warn, "Have removed duplicate rows from the %s object.")
  }

  if (!all(frame[1, ] == WT)) { # if the first row is not WT, search for WT rows
    WTrow <- prodlim::row.match(rep(WT, ncol(frame)), frame)

    if (is.na(WTrow)) { # if there is no WT row, add an initial WT row
      frame <- rbind(rep(WT, ncol(frame)), frame)
      warn <- c(warn, "Have added a row containing the baseline condition to the %s object.")
    } else { # if there is a WT row, move it to the first row
      OTHERrow <- (1:nrow(frame))[-WTrow]
      frame[1:nrow(frame), ] <- frame[c(WTrow, OTHERrow), ]
      warn <- c(warn, "Have moved the row containing the baseline condition to the first row in the %s object.")
    }
  }

  if (!is.null(warn)) { # provide warning messages in case any changes were made
    sapply(sprintf(warn, name), warning, call. = FALSE)
  }

  frame
}

#' A function to generate a screen of modifier conditions based on a set of
#' prior distributions.
#'
#' @param folder a string stating the directory of the folder containing
#'        your generated model.
#' @param priorDistribution states the prior distribution to be used to generate
#'        modifier values. If of length 1, the prior will be applied to all
#'        modifier values. If length is greater than 1, the vector must be
#'        named with the corresponding modifier names. To specify a value
#'        for particular modifiers, provide the value of that modifier
#'        instead of the distribution to be used. Available distributions
#'        and be either 'logNormal', or 'uniform'. Default is set to 'logNormal'.
#'        if 'uniform' is chosen, you must provide values for the minVal and
#'        maxVal arguments.
#' @param n the number of simulations for which a set of priors will be generated.
#' @param returnVals logical. If the output should be returned to the user.
#' @param minVal default set to 0. The minimum starting value for a node.
#' @param maxVal default set to 2. The maximum starting value for a node.
#' @importFrom stats rlnorm
#' @export

modifierPriorScreen <- function(folder,
                                priorDistribution = "logNormal",
                                n,
                                returnVals =FALSE,
                                minVal = 0,
                                maxVal = 2,
                                savePriors = T) {
  # checking user inputs
  if ("uniform" %in% priorDistribution & minVal < 0) {
    minVal = 0
    warning("Expression levels cannot be below 0. Have reassigned the minimum
            value to 0.")
  }

  if ("uniform" %in% priorDistribution & is.null(maxVal)) {
    stop("If you are drawing from a uniform distribution, you must provide a maxVal.")
  }

  load(paste0(folder, "/genotypeDef.RData"))

  # make sure that the priorDistribution has the correct naming convention
  if (length(priorDistribution) == 1) {
    priorDistribution <- rep(priorDistribution, ncol(genotypeDef))
    names(priorDistribution) <- colnames(genotypeDef)
  } else {
    # check if anything has been missnamed
    if (length(setdiff(names(priorDistribution), colnames(genotypeDef))) > 1 |
        length(setdiff(colnames(genotypeDef), names(priorDistribution))) > 1) {
      stop("priorDistribution vector must have the same names as the network nodes.")
    }
  }

  # giving object a different name to distinguish from defined set of
  # experimental conditions
  priorDef <- genotypeDef[1, ]

  # will want to allow for different distributions in the future!!! Maybe even
  # user specified distributions?
  priorDef[2:(n + 1), names(priorDistribution)[priorDistribution == "logNormal"]] <- rlnorm(n * sum(priorDistribution == "logNormal"))
  if (any(priorDistribution == "uniform")) {
    priorDef[2:(n + 1), names(priorDistribution)[priorDistribution == "uniform"]] <- runif(n * sum(priorDistribution == "uniform"),
                                                                                           min = minVal, max = maxVal)
  }


  # replace vals with specific numbers if provided by user
  givenVals <- sapply(priorDistribution, function(x) suppressWarnings(as.numeric(x)))

  if (any(!is.na(givenVals))) {
    for (mod in names(givenVals[!is.na(givenVals)])) {
      priorDef[2:(n + 1), mod] <- givenVals[[mod]]
    }
  }

  if (savePriors == TRUE) save(priorDef, file = paste0(folder, "/priorDef.RData"))

  if (savePriors == FALSE) return(priorDef)
}


#' A function to produce a report of simulation progression
#'
#' @param x the index for the simulation completed
#' @param r the total number of simulations to be completed
#' @importFrom stats quantile

report <- function(x, r) {
  if (r < 20) return(NULL) # not worth reporting on progress

  thresh <- round(quantile(1:r, seq(0, 1, 0.2)))

  if (x %in% thresh) {
    i <- which(x == thresh)
    cat(paste0("\r", names(i), " of simulations are complete."))
  }
}
