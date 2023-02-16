#' A function that converts an SBGN-ML text file into a network object
#'
#' @param file a string. A text file with extension .xml containing the markdown output
#'             of an sbgn diagram.
#' @param networkName a string. The name that you wish to give the network.
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_children
#' @importFrom xml2 as_list
#' @importFrom xml2 xml_name
#' @export

convertSBGNdiagram <- function(file, networkName) {
  text <- read_xml(file)
  lang <- attr(as_list(xml_children(text))[[1]],"language")
  if (lang != "activity flow") {stop("Your diagram is not of type SBGN-AF!")}
  nodes <- xml_children(xml_children(text))
  nodesList <- as_list(nodes)

  nodeTypes <- xml_name(nodes)
  classes <- unlist(lapply(nodesList, attr, which = ".class"))
  ids <- unlist(lapply(nodesList, attr, which = "id"))
  compIndex <- which(classes == "compartment")
  nodeIndex <- which(classes == "biological activity")
  arcIndex <- which(nodeTypes == "arc")
  logicIndex <- which(classes %in% c("delay", "and", "or", "not"))
  submapIndex <- which(classes == "submap")

  compartment <- data.frame(name = rep(NA, length(compIndex)), id = NA)
  for (i in 1:length(compIndex)) {
    compartment[i , ] <- c(attr(nodesList[[compIndex[i]]]$label,"text"), ids[compIndex[i]])
  }

  nodeInfo <- data.frame(name = NA, id = ids[nodeIndex], compartment = NA)
  for (i in 1:nrow(nodeInfo)) {
    nodeInfo$name[i] <- attr(nodesList[[nodeIndex[i]]]$label,"text")
    nodeInfo$compartment[i] <- compartment$name[attr(nodesList[[nodeIndex[i]]],
                                                     "compartmentRef") == compartment$id]
  }

  if (length(submapIndex) > 0) {
    for (i in submapIndex) {
      subMapInfo <- getSubmapNodes(node = nodes[i],
                                   comp = compartment$name[attr(nodesList[[i]],
                                                                "compartmentRef") == compartment$id])
      nodeInfo <- rbind(nodeInfo,
                        subMapInfo$nodeInfo)
      nodesList <- append(nodesList,
                          subMapInfo$newNodes,
                          after = length(nodesList))
    }
    # redefining indexes now that have collected submap nodes
    classes <- unlist(lapply(nodesList, attr, which = ".class"))
    ids <- unlist(lapply(nodesList, attr, which = "id"))
    compIndex <- which(classes == "compartment")
    nodeIndex <- which(classes == "biological activity")
    arcIndex <- which(classes %in% c(langConversion$AF, "logic arc"))
    logicIndex <- which(classes %in% c("delay", "and", "or", "not"))
    #submapIndex <- which(classes == "submap")
  }

  # distinguishing alternative production sites by compartment
  if (length(unique(nodeInfo$name)) < nrow(nodeInfo)) {
    # finding the number of letters necessary to distinguish btw different compartments
    letterNo <- 1
    while(length(unique(substr(compartment$name, 1, letterNo))) < nrow(compartment)) {
      letterNo <- letterNo + 1
    }

    # updating nodeInfo names
    repeats <- names(which(table(nodeInfo$name) > 1))
    nodeInfo$name[nodeInfo$name %in% repeats] <- paste(nodeInfo$name[which(nodeInfo$name %in% repeats)],
                                                       substr(nodeInfo$compartment[which(nodeInfo$name %in% repeats)],
                                                              1, letterNo),
                                                       sep = ".")
  }

  arcInfo <- data.frame(influence = rep(NA, length(arcIndex)), source = NA, target = NA)
  for (i in 1:nrow(arcInfo)) {
    arcInfo[i, ] <- c(attributes(nodesList[[arcIndex[i]]])$.class,
                      attributes(nodesList[[arcIndex[i]]])$source,
                      attributes(nodesList[[arcIndex[i]]])$target)
  }

  # a function to remove the decimal places added to the ids of arcs
  # pointing to and from a submap
  removeIDdots <- function(x) {
    id <- unlist(strsplit(x, "\\."))[rep(c(T,F), length(x))]
    id
  }

  arcInfo$source[grepl("\\.", arcInfo$source)] <- removeIDdots(arcInfo$source[grepl("\\.", arcInfo$source)])
  arcInfo$target[grepl("\\.", arcInfo$target)] <- removeIDdots(arcInfo$target[grepl("\\.", arcInfo$target)])

  hormones <- vector("list",length=length(nodeIndex))

  genotypes <- list()
  genCount = 1
  genTracker = NULL

  for (i in 1:length(hormones)) {
    hormones[[i]] <- buildHormone(nodeInfo = nodeInfo, arcInfo = arcInfo, i = i,
                                 logicIndex = logicIndex, ids = ids,
                                 nodesList = nodesList, lang = lang)

    # append genotype information if available
    if ("glyph" %in% names(nodesList[[nodeIndex[i]]])) {
      hormones[[i]]@genotypes <- strsplit(attr(nodesList[[nodeIndex[i]]]$glyph$label,"text"), ", ")[[1]]

      expression <- rep(NA, nrow(compartment))
      names(expression) <- compartment$name

      for (g in 1:length(hormones[[i]]@genotypes)) {
        expression[names(expression) == hormones[[i]]@container] = 1

        if (!hormones[[i]]@genotypes[g] %in% genTracker) {
          genTracker[genCount] <- hormones[[i]]@genotypes[g]
          genotypes[[genCount]] <- new("Genotype",
                                       name = hormones[[i]]@genotypes[g],
                                       expression = expression,
                                       influence = data.frame(Node = hormones[[i]]@name,
                                                              Influence = "production"))
          if (length(hormones[[i]]@genotypes)>1){
            genotypes[[genCount]]@coregulator <- hormones[[i]]@genotypes[-g]
          }
          genCount = genCount + 1
        } else {
          whichGen <- which(genTracker %in% hormones[[i]]@genotypes[g])
          genotypes[[whichGen]]@expression[names(expression) == hormones[[i]]@container] <- 1
        }
      }
    }
  }

  # build into network
  network <- buildNetwork(hormones = hormones, genotypes = genotypes, name = networkName)

  return(network)
}

#' A function to pull out information from with in submaps
#'
#' This function pulls out the nodes existing within a submap,
#' as well as collects the node information for processing.
#' @param node the parent node containing a submap
#' @param comp the compartment within which the submap sits
#' @param newNodes any carryover nodes in the case that the
#'        the function is being used recursively (if there
#'        are submaps within submaps)
getSubmapNodes <- function(node, comp, newNodes = NA) {
  subNodes <- xml_children(node)
  subNodeList <- as_list(subNodes)
  subNodeTypes <- xml_name(subNodes)
  whichSubNodes <- which(subNodeTypes == "glyph")
  subClasses <- unlist(lapply(subNodeList, attr, which = ".class"))
  subIds <- unlist(lapply(subNodeList, attr, which = "id"))

  nodeIndex <- which(subClasses == "biological activity")
  submapIndex <- which(subClasses == "submap")
  logicIndex <- which(subClasses %in% c("delay", "and", "or", "not")) + 2

  nodeInfo <- data.frame(name = NA, id = subIds[nodeIndex], compartment = comp)
  for (i in 1:nrow(nodeInfo)) {
    nodeInfo$name[i] <- attr(subNodeList[[nodeIndex[i] + 2]]$label,"text")
  }

  if (is.na(newNodes)) {
    newNodes = list()
    count = 0
  } else {
    count = length(newNodes)
  }

  for (i in 1:length(whichSubNodes)) {
    newNodes[[i + count]] <- subNodeList[[whichSubNodes[i]]]
  }

  if (length(submapIndex) > 0) {
    for (i in submapIndex) {
      nodeInfo <- rbind(nodeInfo,
                        getSubmapNodes(node = subNodes[i],
                                       comp = comp,
                                       newNodes = newNodes))
    }
  }

  return(list(nodeInfo = nodeInfo, newNodes = newNodes))
}

#' A function to build a hormone object
#'
#' Organises information into the correct format to describe a hormone
#' @param nodeInfo a data.frame containing summary information for nodes
#' @param arcInfo a data.frame containing arc information, including
#'        origins and destinations
#' @param i external index
#' @param logicIndex a vector consisting of the indexes of logical
#'        operators
#' @param ids a vector of all the ids
#' @param nodesList a list of all the nodes in the diagram
#' @param lang the SBGN language used to construct the original diagram

buildHormone <- function(nodeInfo, arcInfo, i, logicIndex, ids, nodesList, lang) {
  id <- nodeInfo$id[i]

  # if there are operators coming in to a node
  if (any(which(ids %in% arcInfo$source[id == arcInfo$target]) %in% logicIndex)) {
    originID <- arcInfo$source[id == arcInfo$target]
    independentInput <- nodeInfo$name[nodeInfo$id == originID[originID %in% nodeInfo$id]]
    logicInputID <- originID[!originID %in% nodeInfo$id]

    inNodes <- if (length(independentInput) == 0) {NULL} else {independentInput}
    inInfluence <- arcInfo$influence[match(nodeInfo$id[match(independentInput,
                                                             nodeInfo$name)],
                                           arcInfo$source)]
    inCoreg <- if (length(outNames) == 0) {NULL} else {NA}
    inOperator <- if (length(outNames) == 0) {NULL} else {NA}

    for (lN in logicInputID) {
      logicInputNode <- nodeInfo$name[match(arcInfo$source[arcInfo$target == lN],
                                            nodeInfo$id)]
      Influence <- rep(arcInfo$influence[arcInfo$source == lN],
                       length(logicInputNode))
      Coreg <- rep(NA, length(logicInputNode))
      if (length(logicInputNode) > 1) {
        for (j in 1:length(logicInputNode)) {
          Coreg[j] <- paste(logicInputNode[-j], collapse = ", ")
        }
      }
      inNodes <- c(inNodes, logicInputNode)
      inInfluence <- c(inInfluence, Influence)
      inCoreg <- c(inCoreg, Coreg)
      inOperator <- c(inOperator,
                      rep(attributes(nodesList[[which(ids == lN)]])$.class,
                          length(logicInputNode)))
    }
  } else {
    inNames <- nodeInfo$name[match(arcInfo$source[id == arcInfo$target], nodeInfo$id)]

    inNodes <- inNames
    inCoreg <- if (length(inNames) == 0) {NULL} else {NA}
    inInfluence <- arcInfo$influence[id == arcInfo$target]
    inOperator <- if (length(inNames) == 0) {NULL} else {NA}
  }

  # if there are operators going out of the node
  if (any(which(ids %in% arcInfo$target[id == arcInfo$source]) %in% logicIndex)) {
    targetID <- arcInfo$target[id == arcInfo$source]
    independentOutput <- nodeInfo$name[match(targetID[targetID %in% nodeInfo$id], nodeInfo$id)]
    logicOutputID <- targetID[!targetID %in% nodeInfo$id]

    outNodes <- independentOutput
    outCoreg <- if (length(outNodes) == 0) {NULL} else {rep(NA, length(outNodes))}
    outInfluence <- arcInfo$influence[id == arcInfo$source][arcInfo$target[id == arcInfo$source] %in% nodeInfo$id]
    outOperator <- if (length(outNodes) == 0) {NULL} else {rep(NA, length(outNodes))}

    for (lN in logicOutputID) {
      logicOutputNode <- nodeInfo$name[match(arcInfo$target[arcInfo$source == lN],
                                             nodeInfo$id)]
      Influence <- arcInfo$influence[arcInfo$source == lN]
      Coreg <- nodeInfo$name[match(arcInfo$source[arcInfo$target == lN & arcInfo$source != id],
                                   nodeInfo$id)]

      outNodes <- c(outNodes, logicOutputNode)
      outInfluence <- c(outInfluence, Influence)
      outCoreg <- c(outCoreg, Coreg)
      outOperator <- c(outOperator, attributes(nodesList[[which(ids == lN)]])$.class)
    }
  } else {
    outNames <- nodeInfo$name[match(arcInfo$target[id == arcInfo$source], nodeInfo$id)]

    outNodes <- outNames
    outCoreg <- if (length(outNames) == 0) {NULL} else {NA}
    outInfluence <- arcInfo$influence[id == arcInfo$source]
    outOperator <- if (length(outNames) == 0) {NULL} else {NA}
  }

  # build hormone object
  hormone <- new("Hormone",
                 name = nodeInfo$name[i],
                 container =  nodeInfo$compartment[i],
                 inputs = data.frame(Node = inNodes,
                                     Coregulator = inCoreg,
                                     Influence = inInfluence,
                                     Operator = inOperator),
                 outputs = data.frame(Node = outNodes,
                                      Coregulator = outCoreg,
                                      Influence = outInfluence,
                                      Operator = outOperator),
                 travel = 1,
                 degradation = 1)

  # correct for altSources
  if (nrow(hormone@inputs) > 0 & any(grepl("\\.", inNodes))) {
    splitNodes <- which(grepl("\\.", inNodes))
    for (j in splitNodes) {
      if (strsplit(inNodes[j], split = "\\.")[[1]][1] == strsplit(nodeInfo$name[i],
                                                                  split = "\\.")[[1]][1]) {
        hormone@inputs$Influence[j] <- "altSource"
      }
    }
  }

  if (nrow(hormone@outputs) > 0 & any(grepl("\\.", outNodes))) {
    splitNodes <- which(grepl("\\.", outNodes))
    for (j in splitNodes) {
      if (strsplit(outNodes[j], split = "\\.")[[1]][1] == strsplit(nodeInfo$name[i],
                                                                  split = "\\.")[[1]][1]) {
        hormone@outputs$Influence[j] <- "altSource"
      }
    }
  }

  # convert language to my standard
  hormone@inputs$Influence <- languageConversion(hormone@inputs$Influence, lang)
  hormone@outputs$Influence <- languageConversion(hormone@outputs$Influence, lang)

  hormone
}

#' A function to convert SBGN language to peaSoup language
#'
#' @param influenceFrame the influence column of either the input or output
#'                       slot of a hormone object
#' @param language the SBGN language style used to build an initial diagrammatic
#'                network
#'
languageConversion <- function(influenceFrame, language) {
  column <- which(c("activity flow", "entity relationship") %in% language) + 1

  replace <- langConversion$peaSoup[match(influenceFrame, langConversion[,column])]
  replace
}
