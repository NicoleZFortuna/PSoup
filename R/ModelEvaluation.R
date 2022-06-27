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

    for (i in 1:nrow(network@objects$Hormones[[key]]@inputs)) {
      inKey = network@objects$Hormones[[key]]@inputs$Node[i]
      if (network@objects$Hormones[[key]]@inputs$Influence[i] !=
          network@objects$Hormones[[inKey]]@outputs$Influence[network@objects$Hormones[[inKey]]@outputs$Node==key]) {
        errorList[[eL]] <- paste("There is a discrepancy between hormones:", key, "and,", inKey)
        eL = eL + 1
      }
    }

    for (o in 1:nrow(network@objects$Hormones[[key]]@outputs)) {
      outKey = network@objects$Hormones[[key]]@outputs$Node[i]
      if (network@objects$Hormones[[key]]@outputs$Influence[i] !=
          network@objects$Hormones[[outKey]]@inputs$Influence[network@objects$Hormones[[outKey]]@inputs$Node==key]) {
        errorList[[eL]] <- paste("There is a discrepancy between hormones:", key, "and,", inKey)
        eL = eL + 1
      }
    }

  }

  # travel
}

# validityCheck

