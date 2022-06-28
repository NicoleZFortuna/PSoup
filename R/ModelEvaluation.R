# A function to check the internal consistency of a Network object.
#
# This function checks the inputs and outputs of all nodes in the
# network to make sure that all inputs and outputs for nodes are
# in agreement with each other. In addition, checks that the
# genotypes are consistent in their coregulators, and that they
# agree with the nodes in terms of regulation.
#
# @param network an object of class network.
consistencyCheck <- function(network) {
  Hnames <- names(network@objects$Hormones)
  Gnames <- names(network@objects$Genotypes)

  errorList = list()
  eL=1

  for (h in 1:length(Hnames)) { #Checking the each hormone against their inputs and outputs
    key = network@objects$Hormones[[h]]@name

    if (nrow(network@objects$Hormones[[key]]@inputs) > 0) {
      for (i in 1:nrow(network@objects$Hormones[[key]]@inputs)) {
        inKey = network@objects$Hormones[[key]]@inputs$Node[i]

        #Checking that the input key refers to a node in the network
        if (inKey %in% Hnames) {
          #Checking that the input and output of associated nodes are matching
          if (network@objects$Hormones[[key]]@inputs$Influence[i] !=
              network@objects$Hormones[[inKey]]@outputs$Influence[network@objects$Hormones[[inKey]]@outputs$Node==key]) {
            errorList[[eL]] <- paste("There is a discrepancy between hormones:", key, "and,", inKey)
            eL = eL + 1
          }
        } else {
          errorList[[eL]] <- paste("The input node", inKey, "of the", key, "node does not exist.")
          eL = eL + 1
        }
      }
    }

    if (nrow(network@objects$Hormones[[key]]@outputs) > 0) {
      for (o in 1:nrow(network@objects$Hormones[[key]]@outputs)) {
        outKey = network@objects$Hormones[[key]]@outputs$Node[o]

        #Checking that the output key refers to a node in the network
        if (!outKey %in% Hnames) {
          errorList[[eL]] <- paste("The output node", outKey, "of the", key, "node does not exist.")
          eL = eL + 1
        } else {
          # Checking that the input node lists the key as an output node
          if (!key %in% network@objects$Hormones[[outKey]]@inputs$Node) {
            errorList[[eL]] <- paste("The nodes", key, "and", outKey, "disagree on their relationship.")
            eL = eL + 1
          }
          # Checking that the stated relationship between the two nodes is consistent
          if (network@objects$Hormones[[key]]@outputs$Influence[o] !=
              network@objects$Hormones[[outKey]]@inputs$Influence[network@objects$Hormones[[outKey]]@inputs$Node==key]) {
            errorList[[eL]] <- paste("There is a discrepancy between hormones:", key, "and,", outKey)
            eL = eL + 1
          }
        }
      }
    }
  }
}

# Need to make sure that the keys point to something
# Need to make sure that the key is listed in the affects of the corresponding node
# Need to make sure that the listed relationship is consistent between affected nodes

  # travel
}

# validityCheck

