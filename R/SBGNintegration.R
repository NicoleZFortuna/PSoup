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
  if (lang != "activity flow") {stop("Your diagram is of type SBGN-AF!")}
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
    id = attr(nodesList[[nodeIndex[i]]],"id")

    #checking that we are not dealing with a hormone that is associated with a logical operator
    if (!any(which(ids %in% c(arcInfo$source[id == arcInfo$target],
                              arcInfo$target[id == arcInfo$source])) %in% logicIndex)) {
      inNames <- nodeInfo$name[match(arcInfo$source[id == arcInfo$target], nodeInfo$id)]
      outNames <- nodeInfo$name[match(arcInfo$target[id == arcInfo$source], nodeInfo$id)]
      hormones[[i]] <- new("Hormone",
                           name = nodeInfo$name[i],
                           container = compartment$name[attr(nodesList[[nodeIndex[i]]],
                                                             "compartmentRef") == compartment$id],
                           inputs = data.frame(Node = inNames,
                                               Coregulator = if (length(inNames) == 0) {NULL} else {NA},
                                               Influence = arcInfo$influence[id == arcInfo$target],
                                               Delay = if (length(inNames) == 0) {NULL} else {NA}),
                           outputs = data.frame(Node = outNames,
                                                Coregulator = if (length(outNames) == 0) {NULL} else {NA},
                                                Influence = arcInfo$influence[id == arcInfo$source],
                                                Delay = if (length(outNames) == 0) {NULL} else {NA}),
                           travel = 1,
                           degradation = 1)

      # Correcting for alternative sources
      if (nrow(hormones[[i]]@inputs) > 0) {
        if (any(strsplit(inNames, split = "\\.")[[1]][1] == strsplit(nodeInfo$name[i], split = "\\.")[[1]][1])) {
          overWrite <- which(strsplit(inNames, split = "\\.")[[1]][1] == strsplit(nodeInfo$name[i], split = "\\.")[[1]][1])
          hormones[[i]]@inputs$Influence[overWrite] <- "altSource"
        }
      }

      if (nrow(hormones[[i]]@outputs) > 0) {
        if (any(strsplit(outNames, split = "\\.")[[1]][1] == strsplit(nodeInfo$name[i], split = "\\.")[[1]][1])) {
          overWrite <- which(strsplit(outNames, split = "\\.")[[1]][1] == strsplit(nodeInfo$name[i], split = "\\.")[[1]][1])
          hormones[[i]]@outputs$Influence[overWrite] <- "altSource"
        }
      }
    } else {
      # if one of the inputs are logical
      if (any(which(ids %in% arcInfo$source[id == arcInfo$target]) %in% logicIndex)) {
        rowNodes <- which(ids %in% arcInfo$source[id == arcInfo$target])

        # collecting input node information including operator info
        N = data.frame(Node = rowNodes, Coregulator = NA, Operator = NA)
        o = 1
        for (r in 1:length(rowNodes)) {
          if (rowNodes[r] %in% logicIndex) {
            operatorOrigin <- nodeInfo$name[nodeInfo$id %in% arcInfo$source[ids[rowNodes[r]] == arcInfo$target]]
            if (length(operatorOrigin) == 1) {
              N[o, ] <- c(operatorOrigin, NA, attr(nodesList[[rowNodes[r]]],".class"))
            } else if (length(operatorOrigin) == 2){
              N[o:(o + 1), "Node"] <- operatorOrigin
              N[o:(o + 1), "Coregulator"] <- rev(operatorOrigin)
              N[o:(o + 1), "Operator"] <- attr(nodesList[[rowNodes[r]]],".class")
            } else {
              stop("Nicole was too lazy to allow for more than two dependent inputs. Go bug her if you would like this feature.")
            }
            o = o + length(operatorOrigin)
          } else {
            N[o, ] <- c(attr(nodesList[[rowNodes[r]]]$label,"text"), NA, NA)
            o = o + 1
          }
        }

        if (!any(N$Operator %in% c("delay", "and"))) {
          stop("peaSoup only accepts the 'delay' and 'and' operators.")
        }

        if ("and" %in% N$Operator) {
          Influence <- rep(NA, nrow(N))
          for (j in 1:nrow(N)) {
            Influence[j] <- arcInfo$influence[id == arcInfo$target][which(unique(N$Operator) %in% N$Operator[j])]
          }
        } else {
          Influence <- arcInfo$influence[id == arcInfo$target]
        }

        hormones[[i]] <- new("Hormone",
                             name = nodeInfo$name[i],
                             container = compartment$name[attr(nodesList[[nodeIndex[i]]],
                                                               "compartmentRef") == compartment$id],
                             inputs = data.frame(Node = N$Node,
                                                 Coregulator = N$Coregulator,
                                                 Influence = Influence,
                                                 Delay = N$Operator=="delay"),
                             outputs = data.frame(Node = nodeInfo$name[match(arcInfo$target[id == arcInfo$source], nodeInfo$id)],
                                                  Coregulator = NA,
                                                  Influence = arcInfo$influence[id == arcInfo$source],
                                                  Delay = NA),
                             travel = 1,
                             degradation = 1)

                             #travel = as.numeric(length(unique(nodeInfo$compartment[nodeInfo$name %in% c(nodeInfo$name[i], nodeInfo$name[nodeInfo$id == arcInfo$target[id == arcInfo$source]])])) > 1))
      }

      # if one of the outputs are logical
      if (any(which(ids %in% arcInfo$target[id == arcInfo$source]) %in% logicIndex)) {
        rowNodes <- which(ids %in% arcInfo$target[id == arcInfo$source])

        # collecting input node information including operator info
        N = data.frame(Node = length(rowNodes), Coregulator = NA, Influence = NA, Operator = NA)
        o=1
        for (r in 1:length(rowNodes)) {
          if (rowNodes[r] %in% logicIndex) {
            operatorDestination <- nodeInfo$name[nodeInfo$id %in% arcInfo$target[ids[rowNodes[r]] == arcInfo$source]]
            if (length(operatorDestination) == 1) {
              N[o, ] <- c(operatorDestination, NA, arcInfo$influence[ids[rowNodes[r]] == arcInfo$source],
                          attr(nodesList[[rowNodes[r]]],".class"))
            } else if (length(operatorDestination) == 2){
              N[o:(o + 1), "Node"] <- operatorDestination
              N[o:(o + 1), "Coregulator"] <- rev(operatorDestination)
              N[o:(o + 1), "Influence"] <- arcInfo$influence[ids[rowNodes[r]] == arcInfo$source]
              N[o:(o + 1), "Operator"] <- attr(nodesList[[rowNodes[r]]],".class")
            } else {
              stop("Nicole was too lazy to allow for more than two dependent inputs. Go bug her if you would like this feature.")
            }
            o = o + length(operatorDestination)
          } else {
            N[o, ] <- c(attr(nodesList[[rowNodes[r]]]$label,"text"), NA,
                        arcInfo$influence[id == arcInfo$source], NA)
            o = o + 1
          }
        }

        hormones[[i]] <- new("Hormone",
                             name = nodeInfo$name[i],
                             container = compartment$name[attr(nodesList[[nodeIndex[i]]],
                                                               "compartmentRef") == compartment$id],
                             inputs = data.frame(Node = nodeInfo$name[match(arcInfo$source[id == arcInfo$target], nodeInfo$id)],
                                                 Coregulator = NA,
                                                 Influence = arcInfo$influence[id == arcInfo$target],
                                                 Delay = NA),
                             outputs = N,
                             travel = 1,
                             degradation = 1)

                             #travel = as.numeric(length(unique(nodeInfo$compartment[nodeInfo$name %in% c(nodeInfo$name[i], nodeInfo$name[nodeInfo$id == arcInfo$target[id == arcInfo$source]])])) > 1))
        # will need to incur a temporal penalty in the case of a delay
      }
    }

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

  # convert language to my standard
  for (i in 1:length(hormones)) {
    hormones[[i]]@inputs$Influence <- languageConversion(hormones[[i]]@inputs$Influence, lang)
    hormones[[i]]@outputs$Influence <- languageConversion(hormones[[i]]@outputs$Influence, lang)
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
