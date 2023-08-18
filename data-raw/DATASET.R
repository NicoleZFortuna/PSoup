# Generating the inbuilt network provided with the package
peaNetwork <- convertSBGNdiagram(file = "./data-raw/DunAFgenotype.sbgn", networkName = "peaNetwork")
save(peaNetwork, file = "./Data/peaNetwork.RData")

# a key used to maintain consistent language
langConversion <- data.frame(peaSoup = c("stimulation",
                                          "inhibition",
                                          "necessary stimulation",
                                          "necessary inhibition",
                                          "sufficient stimulation",
                                          "sufficient inhibition",
                                          "unknown",
                                          "altSource"),
                             AF = c("positive influence",
                                    "negative influence",
                                    "necessary stimulation",
                                    NA, NA, NA,
                                    "unknown influence",
                                    "altSource"),
                             ER = c("stimulation",
                                    "inhibition",
                                    "necessary stimulation",
                                    NA,
                                    "absolute stimulation",
                                    "absolute inhibition",
                                    "modulation",
                                    "altSource"))
save(langConversion, file = "./Data/langConversion.RData")



