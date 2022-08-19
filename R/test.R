

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
}

if (any(nodes[[i]]@inputs$Influence == "inhibition")) {
  inhibString <- nodes[[i]]@inputs$Node[nodes[[i]]@inputs$Influence %in% inhibition & is.na(nodes[[i]]@inputs$Coregulator)]
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
    coreg <- paste0(coreg, collapse = " + ")
  }
  inhibString = paste(inhibString, coreg, sep = " + ")
}

if ()


