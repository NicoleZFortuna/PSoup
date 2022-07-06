#' A function that converts an SBGN-ML text file into a network object
#'
#' @param file A text file with extension .xml containing the markdown output
#'             of an sbgn diagram.
#' @param path The path directory for where to store
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_children
#' @importFrom xml2 as_list

convertSBGNdiagram <- function(file, path, networkName) {
  text <- read_xml(file)
  lang <- attr(as_list(xml_children(text))[[1]],"language")
  nodes <- xml_children(xml_children(text))
  nodesList <- as_list(nodes)

  nodeTypes <- xml_name(nodes)
  classes <- unlist(lapply(nodesList, attr, which = ".class"))
  ids <- unlist(lapply(nodesList, attr, which = "id"))
  compIndex <- which(classes == "compartment")
  nodeIndex <- which(classes == "biological activity")
  arcIndex <- which(nodeTypes == "arc")
  logicIndex <- which(classes %in% c("delay", "and", "or", "not"))

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

  arcInfo <- data.frame(influence = rep(NA, length(arcIndex)), source = NA, target = NA)
  for (i in 1:nrow(arcInfo)) {
    arcInfo[i, ] <- c(attributes(nodesList[[arcIndex[i]]])$.class,
                      attributes(nodesList[[arcIndex[i]]])$source,
                      attributes(nodesList[[arcIndex[i]]])$target)
  }

  hormones <- vector("list",length=length(nodeIndex))
  for (i in 1:length(nodeIndex)) {
    id = attr(nodesList[[nodeIndex[i]]],"id")

    hormones[[i]] <- new("Hormone",
                         name = attr(nodesList[[nodeIndex[i]]]$label,"text"),
                         container = compartment$name[attr(nodesList[[nodeIndex[i]]],
                                                           "compartmentRef") == compartment$id],
                         inputs = data.frame(Node = c(nodeInfo$name[nodeInfo$id %in% arcInfo$source[id == arcInfo$target]]),
                                             Influence = c(arcInfo$influence[id == arcInfo$target])),
                         outputs = data.frame(Node = nodeInfo$name[nodeInfo$id %in% arcInfo$target[id == arcInfo$source]],
                                              Influence = arcInfo$influence[id == arcInfo$source]),
                         travel = as.numeric(length(unique(nodeInfo$compartment[nodeInfo$name %in% c(nodeInfo$name[i], nodeInfo$name[nodeInfo$id == arcInfo$target[id == arcInfo$source]])])) > 1))

    # append genotype information in available
    if ("glyph" %in% names(nodesList[[nodeIndex[i]]])) {
      hormones[[i]]@genotypes <- strsplit(attr(nodesList[[nodeIndex[i]]]$glyph$label,"text"), ", ")[[1]]
    }
  }

}

#buildEquation

#plantSimulator
