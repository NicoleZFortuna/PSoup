
# collating all the stimulatory action
if (any(nodes[[i]]@inputs$Influence == "stimulation")) {
  stimString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% "stimulation" & is.na(nodes[[i]]@inputs$Coregulator)]
  if (length(stimString) > 0) {stimString <- paste0("dat$", stimString, "[t-1]", collapse = " + ")}

  # are there any stimulants that are coregulators
  if (any(!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "stimulation")) {
    coregInput <- nodes[[i]]@inputs[!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "stimulation", -3]

    coreg <- coregulators(coregInput)

    stimString = paste(stimString, coreg, sep = " + ")
  }
} else {stimString = NA}

# collating all the inhibitory action
if (any(nodes[[i]]@inputs$Influence == "inhibition")) {
  inhibString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% "inhibition" & is.na(nodes[[i]]@inputs$Coregulator)]
  numInhib <- length(inhibString)
  if (length(inhibString) > 0) {inhibString <- paste0("dat$", inhibString, "[t-1]", collapse = " + ")}

  # are there any inhibitors that are coregulators
  if (any(!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "inhibition")) {
    coregInput <- nodes[[i]]@inputs[!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "inhibition", -3]

    coreg <- coregulators(coregInput)

    numInhib = coreg$num
    inhibString = paste(inhibString, coreg$coreg, sep = " + ")
  }
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
  # if it is constituent wo influence from other nodes
  allModulations <- 1
}

# multiplying modulations by necessary stimulants and genotypes
if (any(nodes[[i]]@inputs$Influence == "necessary stimulation") | length(nodes[[i]]@genotypes) > 0) {
  necstimString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% "necessary stimulation" & is.na(nodes[[i]]@inputs$Coregulator)]
  if (length(stimString) > 0) {stimString <- paste0("dat$", stimString, "[t-1]", collapse = " + ")}

  # are there any necessary stimulants that are coregulators
  if (any(!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "stimulation")) {
    coregInput <- nodes[[i]]@inputs[!is.na(nodes[[i]]@inputs$Coregulator) & nodes[[i]]@inputs$Influence == "stimulation", -3]

    coreg <- coregulators(coregInput)

    necstimString = paste(necstimString, coreg, sep = " + ")
  }

  genes <- nodes[[i]]@genotypes
  genoString <- rep(NA, length(genes))
  for (g in 1:length(genes)) {
    if (class(genotypes[[genes[g]]]@coregulator) == "character") {
      cogenes <- c(genes[g], genotypes[[genes[g]]]@coregulator)

      genoString[g] <- paste0(cogenes[order(cogenes)], collapse = "*")
    }
  }

  genoString = unique(genoString)

  # conditionals for all possibilities
  sprintf("(%s) * (%s) * (%s)", allModulations, necstimString, genoString)
}

# add if there is a source from another node (will not be influenced by dynamics of current node)
if (any(nodes[[i]]@inputs$Influence == "altSource")) {
  altString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% "altSource"]
  allModulations <- sprintf("%s + %s", paste0("dat$", altString, "[t-1]", collapse = " + "), allModulations)
}






