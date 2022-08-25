
# collating all the stimulatory action
if (any(nodes[[i]]@inputs$Influence == "stimulation")) {
  stimString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% stimulation & is.na(nodes[[i]]@inputs$Coregulator)]
  if (length(stimString) > 0) {stimString <- paste0("dat$", stimString, "[t-1]", collapse = " + ")}

  # are there any stimulants that are coregulators
  if (any(!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "stimulation")) {
    coreg <- nodes[[i]]@inputs[!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "stimulation", -3]
    coreg <- unname(as.matrix(coreg))
    for (r in 1:nrow(coreg)) {
      coreg[r, ] <- sort(coreg[r, ])
      coreg[r, ] <- paste0("dat$", coreg[r, ], "[t-1]")
    }

    coreg <- unique(paste(coreg[,1], coreg[,2], sep = "*"))
    coreg <- paste0(coreg, collapse = " + ")
  }
  stimString = paste(stimString, coreg, sep = " + ")
} else {stimString = NA}

# collating all the inhibitory action
if (any(nodes[[i]]@inputs$Influence == "inhibition")) {
  inhibString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% inhibition & is.na(nodes[[i]]@inputs$Coregulator)]
  numInhib <- length(inhibString)
  if (length(inhibString) > 0) {inhibString <- paste0("dat$", inhibString, "[t-1]", collapse = " + ")}

  # are there any inhibitors that are coregulators
  if (any(!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "inhibition")) {
    coreg <- nodes[[i]]@inputs[!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "inhibition", -3]
    coreg <- unname(as.matrix(coreg))
    for (r in 1:nrow(coreg)) {
      coreg[r, ] <- sort(coreg[r, ])
      coreg[r, ] <- paste0("dat$", coreg[r, ], "[t-1]")
    }

    coreg <- unique(paste(coreg[,1], coreg[,2], sep = "*"))
    numInhib - numInhib + length(coreg)
    coreg <- paste0(coreg, collapse = " + ")
  }
  inhibString = paste(inhibString, coreg, sep = " + ")
} else {inhibString = NA}

# combining stimulatory and inhibitory effects
if (class(stimString) == "character" & is.na(inhibString)) {
  # if there are only stimulatory effects
  allModulations <- stimString
} else if (class(inhibString) == "character" & is.na(stimString)) {
  # if there are only inhibitory effects
  allModulations <- sprintf("%s/(1 + %s)", numInhib + 1, inhibString)
} else if (class(stimString) == "character" & class(inhibString) == "character") {
  allModulations <- sprintf("(%s*%s)/(1 + %s)", numInhib + 1, stimString, inhibString)
} else if (is.na(stimString) & is.na(inhibString)) {
  allModulations <- 1
}




