Strigolactone.R <- new("Hormone",
                      name = "Strigolactone.R",
                      container = "Rootstock",
                      inputs = data.frame(Node = c("Feedback.R",
                                                   "Inhibitor",
                                                   "ShootSignal"),
                                          Coregulator = c(NA, NA, NA),
                                          Influence = c("stimulation",
                                                        "inhibition",
                                                        "stimulation"),
                                          Operator = c(NA, NA, NA)),
                      outputs = data.frame(Node = "Strigolactone.S",
                                           Coregulator = NA,
                                           Influence = "altSource",
                                           Operator = NA),
                      travel = 1,
                      degradation = 1,
                      genotypes = c("RMS1", "RMS5"))
save(Strigolactone.R, file = "./Data/Strigolactone.R.RData")


Strigolactone.S <- new("Hormone",
                      name = "Strigolactone.S",
                      container = "Scion",
                      inputs = data.frame(Node = c("Feedback.S",
                                                   "Strigolactone.R",
                                                   "ShootSignal"),
                                          Coregulator = c(NA, NA, NA),
                                          Influence = c("stimulation",
                                                        "altSource",
                                                        "stimulation"),
                                          Operator = c(NA, NA, NA)),
                      outputs = data.frame(Node = "BranchInhibitor",
                                           Coregulator = NA,
                                           Influence = "stimulation",
                                           Operator = NA),
                      travel = 1,
                      degradation = 1,
                      genotypes = c("RMS1", "RMS5"))
save(Strigolactone.S, file = "./Data/Strigolactone.S.RData")

Feedback.R <- new("Hormone",
                 name = "Feedback.R",
                 container = "Rootstock",
                 inputs = data.frame(Node = "Feedback.S",
                                     Coregulator = NA,
                                     Influence = "altSource",
                                     Operator = NA),
                 outputs = data.frame(Node = c("Cytokinin", "Strigolactone.R"),
                                      Coregulator = c(NA, NA),
                                      Influence = c("inhibition", "stimulation"),
                                      Operator = c(NA, NA)),
                 travel = 1,
                 degradation = 1,
                 genotypes = "RMS2")
save(Feedback.R, file = "./Data/Feedback.R.RData")

Feedback.S <- new("Hormone",
                 name = "Feedback.S",
                 container = "Scion",
                 inputs = data.frame(Node = c("BranchInhibitor"),
                                     Coregulator = NA,
                                     Influence = c("inhibition"),
                                     Operator = NA),
                 outputs = data.frame(Node = c("Feedback.R", "Strigolactone.S"),
                                      Coregulator = c(NA, NA),
                                      Influence = c("altSource", "stimulation"),
                                      Operator = c(NA, NA)),
                 travel = 1,
                 degradation = 1,
                 genotypes = "RMS2")
save(Feedback.S, file = "./Data/Feedback.S.RData")

Cytokinin <- new("Hormone",
                 name = "Cytokinin",
                 container = "Rootstock",
                 inputs = data.frame(Node = "Feedback.R",
                                     Coregulator = NA,
                                     Influence = "inhibition",
                                     Operator = NA),
                 outputs = data.frame(Node = "BranchOutgrowth",
                                      Coregulator = NA,
                                      Influence = "stimulation",
                                      Operator = NA),
                 travel = 1,
                 degradation = 1)
save(Cytokinin, file = "./Data/Cytokinin.RData")

BranchInhibitor = new("Hormone",
                      name = "BranchInhibitor",
                      container = "Scion",
                      inputs = data.frame(Node = "Strigolactone.S",
                                          Coregulator = NA,
                                          Influence = "stimulation",
                                          Operator = NA),
                      outputs = data.frame(Node = c("BudRelease", "Feedback.S"),
                                           Coregulator = c(NA, NA),
                                           Influence = c("inhibition", "inhibition"),
                                           Operator = c(NA, NA)),
                      travel = 1,
                      degradation = 1,
                      genotypes = c("RMS3", "RMS4"))
save(BranchInhibitor, file = "./Data/BranchInhibitor.RData")

BudRelease <- new("Hormone",
                  name = "BudRelease",
                  container = "Scion",
                  inputs = data.frame(Node = "BranchInhibitor",
                                      Coregulator = NA,
                                      Influence = "inhibition",
                                      Operator = NA),
                  outputs = data.frame(Node = "BranchOutgrowth",
                                       Coregulator = NA,
                                       Influence = c("necessary stimulation"),
                                       Operator = "Delay"),
                  travel = 1,
                  degradation = 1)
save(BudRelease, file = "./Data/BudRelease.RData")

BranchOutgrowth <- new("Hormone",
                       name = "BranchOutgrowth",
                       container = "Scion",
                       inputs = data.frame(Node = c("BudRelease",
                                                    "Cytokinin"),
                                           Coregulator = c(NA, NA),
                                           Influence = c("necessary stimulation",
                                                         "stimulation"),
                                           Operator = c("Delay", NA)),
                       outputs = data.frame(Node = "ShootSignal",
                                            Coregulator = NA,
                                            Influence = "necessary stimulation",
                                            Operator = NA),
                       travel = 1,
                       degradation = 1)
save(BranchOutgrowth, file = "./Data/BranchOutgrowth.RData")

ShootSignal <- new("Hormone",
                   name = "ShootSignal",
                   container = "Scion",
                   inputs = data.frame(Node = "BranchOutgrowth",
                                       Coregulator = NA,
                                       Influence = "necessary stimulation",
                                       Operator = NA),
                   outputs = data.frame(Node = c("Strigolactone.S",
                                                 "Strigolactone.R"),
                                        Coregulator = c(NA, NA),
                                        Influence = c("stimulation",
                                                      "stimulation"),
                                        Operator = c(NA, NA)),
                   travel = 1,
                   degradation = 1)
save(ShootSignal, file = "./Data/ShootSignal.RData")

Inhibitor <- new("Hormone",
                 name = "Inhibitor",
                 container = "Rootstock",
                 inputs = data.frame(Node = NULL,
                                     Coregulator = NULL,
                                     Influence = NULL,
                                     Operator = NULL),
                 outputs = data.frame(Node = "Strigolactone.R",
                                      Coregulator = NA,
                                      Influence = "inhibition",
                                      Operator = NA),
                 travel = 1,
                 degradation = 1,
                 genotypes = c("RMS3", "RMS4"))
save(Inhibitor, file = "./Data/Inhibitor.RData")

RMS1 <- new("Genotype",
            name = "RMS1",
            expression = c("Scion" = 1, "Rootstock" = 1),
            coregulator = "RMS5",
            influence = data.frame(Node = c("Strigolactone.R", "Strigolactone.S"),
                                   Influence = c("production", "production")))
save(RMS1, file = "./Data/RMS1.RData")

RMS5 <- new("Genotype",
            name = "RMS5",
            expression = c("Scion" = 1, "Rootstock" = 1),
            coregulator = "RMS1",
            influence = data.frame(Node = c("Strigolactone.R", "Strigolactone.S"),
                                   Influence = c("production", "production")))
save(RMS5, file = "./Data/RMS5.RData")

RMS2 <- new("Genotype",
            name = "RMS2",
            expression = c("Scion" = 1, "Rootstock" = 1),
            influence = data.frame(Node = c("Feedback.R", "Feedback.S"),
                                   Influence = c("production", "production")))
save(RMS2, file = "./Data/RMS2.RData")

RMS3 <- new("Genotype",
            name = "RMS3",
            expression = c("Scion" = 1, "Rootstock" = 1),
            coregulator = "RMS4",
            influence = data.frame(Node = c("BranchInhibitor", "Inhibitor"),
                                   Influence = c("production", "production")))
save(RMS3, file = "./Data/RMS3.RData")

RMS4 <- new("Genotype",
            name = "RMS4",
            expression = c("Scion" = 1, "Rootstock" = 1),
            coregulator = "RMS3",
            influence = data.frame(Node = c("BranchInhibitor", "Inhibitor"),
                                   Influence = c("production", "production")))
save(RMS4, file = "./Data/RMS4.RData")

peaNetwork <- buildNetwork(hormones = list(Strigolactone.R,
                                           Strigolactone.S,
                                           Feedback.R,
                                           Feedback.S,
                                           Cytokinin,
                                           BranchInhibitor,
                                           BudRelease,
                                           BranchOutgrowth,
                                           ShootSignal,
                                           Inhibitor),
                           genotypes = list(RMS1,
                                            RMS2,
                                            RMS3,
                                            RMS4,
                                            RMS5),
                           name = "peaNetwork")
save(peaNetwork, file = "./Data/peaNetwork.RData")

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



