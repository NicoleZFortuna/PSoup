#' A function that converts an SBGN-ML text file into a network object
#'
#' @param file A text file with extension .xml containing the markdown output
#'             of an sbgn diagram.
#' @param path The path directory for where to store the network
#' @param networkName The name that you wish to give the network
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

  genotypes <- list()
  genCount = 1
  genTracker = NULL

  for (i in 1:length(hormones)) {
    id = attr(nodesList[[nodeIndex[i]]],"id")

    if (!any(which(ids %in% c(arcInfo$source[id == arcInfo$target], arcInfo$target[id == arcInfo$source])) %in% logicIndex)) {
      hormones[[i]] <- new("Hormone",
                           name = attr(nodesList[[nodeIndex[i]]]$label,"text"),
                           container = compartment$name[attr(nodesList[[nodeIndex[i]]],
                                                             "compartmentRef") == compartment$id],
                           inputs = data.frame(Node = nodeInfo$name[nodeInfo$id %in% arcInfo$source[id == arcInfo$target]],
                                               Influence = arcInfo$influence[id == arcInfo$target]),
                           outputs = data.frame(Node = nodeInfo$name[nodeInfo$id %in% arcInfo$target[id == arcInfo$source]],
                                                Influence = arcInfo$influence[id == arcInfo$source]),
                           travel = as.numeric(length(unique(nodeInfo$compartment[nodeInfo$name %in% c(nodeInfo$name[i], nodeInfo$name[nodeInfo$id == arcInfo$target[id == arcInfo$source]])])) > 1))
    } else {
      # if one of the inputs are logical
      if (any(which(ids %in% arcInfo$source[id == arcInfo$target]) %in% logicIndex)) {
        rowNodes <- which(ids %in% arcInfo$source[id == arcInfo$target])

        N = vector(length = length(rowNodes))
        for (r in 1:length(rowNodes)) {
          if (rowNodes[r] %in% logicIndex) {
            N[r] <- nodeInfo$name[nodeInfo$id %in% arcInfo$source[ids[rowNodes[r]] == arcInfo$target]]
          } else {
            N[r] <- attr(nodesList[[rowNodes[r]]]$label,"text")
          }
        }

        hormones[[i]] <- new("Hormone",
                             name = attr(nodesList[[nodeIndex[i]]]$label,"text"),
                             container = compartment$name[attr(nodesList[[nodeIndex[i]]],
                                                               "compartmentRef") == compartment$id],
                             inputs = data.frame(Node = N,
                                                 Influence = arcInfo$influence[id == arcInfo$target]),
                             outputs = data.frame(Node = nodeInfo$name[nodeInfo$id %in% arcInfo$target[id == arcInfo$source]],
                                                  Influence = arcInfo$influence[id == arcInfo$source]),
                             travel = as.numeric(length(unique(nodeInfo$compartment[nodeInfo$name %in% c(nodeInfo$name[i], nodeInfo$name[nodeInfo$id == arcInfo$target[id == arcInfo$source]])])) > 1))
      }

      # if one of the outputs are logical
      if (any(which(ids %in% arcInfo$target[id == arcInfo$source]) %in% logicIndex)) {
        rowNodes <- which(ids %in% arcInfo$target[id == arcInfo$source])

        N = vector(length = length(rowNodes))
        I = vector(length = length(rowNodes))
        for (r in 1:length(rowNodes)) {
          if (rowNodes[r] %in% logicIndex) {
            N[r] <- nodeInfo$name[nodeInfo$id %in% arcInfo$target[ids[rowNodes[r]] == arcInfo$source]]
            I[r] <- arcInfo$influence[ids[rowNodes[r]] == arcInfo$source]
          } else {
            N[r] <- attr(nodesList[[rowNodes[r]]]$label,"text")
            I[r] <- arcInfo$influence[id == arcInfo$source]
          }
        }

        hormones[[i]] <- new("Hormone",
                             name = attr(nodesList[[nodeIndex[i]]]$label,"text"),
                             container = compartment$name[attr(nodesList[[nodeIndex[i]]],
                                                               "compartmentRef") == compartment$id],
                             inputs = data.frame(Node = nodeInfo$name[nodeInfo$id %in% arcInfo$source[id == arcInfo$target]],
                                                 Influence = arcInfo$influence[id == arcInfo$target]),
                             outputs = data.frame(Node = N,
                                                  Influence = I),
                             travel = as.numeric(length(unique(nodeInfo$compartment[nodeInfo$name %in% c(nodeInfo$name[i], nodeInfo$name[nodeInfo$id == arcInfo$target[id == arcInfo$source]])])) > 1))
        # will need to incur a temporal penalty in the case of a delay
      }
    }

    # append genotype information if available
    if ("glyph" %in% names(nodesList[[nodeIndex[i]]])) {
      hormones[[i]]@genotypes <- strsplit(attr(nodesList[[nodeIndex[i]]]$glyph$label,"text"), ", ")[[1]]

      for (g in 1:length(hormones[[i]]@genotypes)) {
        if (!hormones[[i]]@genotypes[g] %in% genTracker) {
          genTracker[genCount] <- hormones[[i]]@genotypes[g]
          genotypes[[genCount]] <- new("Genotype",
                                       name = hormones[[i]]@genotypes[g],
                                       expression = data.frame(Container = hormones[[i]]@container,
                                                               Expression = 1),
                                       influence = data.frame(Node = hormones[[i]]@name,
                                                              Influence = "production"))
          if (length(hormones[[i]]@genotypes)>1){
            genotypes[[genCount]]@coregulator <- hormones[[i]]@genotypes[-g]
          }
          genCount = genCount + 1
        } else {
          genotypes[[which(hormones[[i]]@genotypes[g] %in% genTracker)]]@expression[g,] <- c(hormones[[i]]@container, 1)

          genotypes[[which(hormones[[i]]@genotypes[g] %in% genTracker)]]@influence[g,] <- c(hormones[[i]]@name, "production")
        }
      }
    }
  }

  # build into network

  # save output if requested

}

#buildEquation

#plantSimulator
