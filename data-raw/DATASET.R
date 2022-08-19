StrigolactoneR <- new("Hormone",
                      name = "StrigolactoneR",
                      container = "rootstock",
                      inputs = data.frame(Node = c("FeedbackR",
                                                   "Inhibitor",
                                                   "ShootSignal"),
                                          Coregulator = c(NA, NA, NA),
                                          Influence = c("stimulation",
                                                        "inhibition",
                                                        "stimulation")),
                      outputs = data.frame(Node = "StrigolactoneS",
                                           Coregulator = NA,
                                           Influence = "altSource"),
                      travel = 1,
                      degradation = 1,
                      genotypes = c("RMS1", "RMS5"))
save(StrigolactoneR, file = "./Data/StrigolactoneR.RData")


StrigolactoneS <- new("Hormone",
                      name = "StrigolactoneS",
                      container = "scion",
                      inputs = data.frame(Node = c("FeedbackS",
                                                   "StrigolactoneR",
                                                   "ShootSignal"),
                                          Coregulator = c(NA, NA, NA),
                                          Influence = c("stimulation",
                                                        "altSource",
                                                        "stimulation")),
                      outputs = data.frame(Node = "BranchInhibitor",
                                           Coregulator = NA,
                                           Influence = "stimulation"),
                      travel = 1,
                      degradation = 1,
                      genotypes = c("RMS1", "RMS5"))
save(StrigolactoneS, file = "./Data/StrigolactoneS.RData")

FeedbackR <- new("Hormone",
                 name = "FeedbackR",
                 container = "rootstock",
                 inputs = data.frame(Node = "FeedbackS",
                                     Coregulator = NA,
                                     Influence = "altSource"),
                 outputs = data.frame(Node = c("Cytokinin", "StrigolactoneR"),
                                      Coregulator = c(NA, NA),
                                      Influence = c("inhibition", "stimulation")),
                 travel = 1,
                 degradation = 1,
                 genotypes = "RMS2")
save(FeedbackR, file = "./Data/FeedbackR.RData")

FeedbackS <- new("Hormone",
                 name = "FeedbackS",
                 container = "scion",
                 inputs = data.frame(Node = c("BranchInhibitor"),
                                     Coregulator = NA,
                                     Influence = c("inhibition")),
                 outputs = data.frame(Node = c("FeedbackR", "StrigolactoneS"),
                                      Coregulator = c(NA, NA),
                                      Influence = c("altSource", "stimulation")),
                 travel = 1,
                 degradation = 1,
                 genotypes = "RMS2")
save(FeedbackS, file = "./Data/FeedbackS.RData")

Cytokinin <- new("Hormone",
                 name = "Cytokinin",
                 container = "rootstock",
                 inputs = data.frame(Node = "FeedbackR",
                                     Coregulator = NA,
                                     Influence = "inhibition"),
                 outputs = data.frame(Node = "BranchOutgrowth",
                                      Coregulator = NA,
                                      Influence = "stimulation"),
                 travel = 1,
                 degradation = 1)
save(Cytokinin, file = "./Data/Cytokinin.RData")

BranchInhibitor = new("Hormone",
                      name = "BranchInhibitor",
                      container = "scion",
                      inputs = data.frame(Node = "StrigolactoneS",
                                          Coregulator = NA,
                                          Influence = "stimulation"),
                      outputs = data.frame(Node = c("BudRelease", "FeedbackS"),
                                           Coregulator = c(NA, NA),
                                           Influence = c("inhibition", "inhibition")),
                      travel = 1,
                      degradation = 1,
                      genotypes = c("RMS3", "RMS4"))
save(BranchInhibitor, file = "./Data/BranchInhibitor.RData")

BudRelease <- new("Hormone",
                  name = "BudRelease",
                  container = "scion",
                  inputs = data.frame(Node = "BranchInhibitor",
                                      Coregulator = NA,
                                      Influence = "inhibition"),
                  outputs = data.frame(Node = "BranchOutgrowth",
                                       Coregulator = NA,
                                       Influence = c("necessary stimulation")),
                  travel = 1,
                  degradation = 1)
save(BudRelease, file = "./Data/BudRelease.RData")

BranchOutgrowth <- new("Hormone",
                       name = "BranchOutgrowth",
                       container = "scion",
                       inputs = data.frame(Node = c("BudRelease",
                                                    "Cytokinin"),
                                           Coregulator = c(NA, NA),
                                           Influence = c("necessary stimulation",
                                                         "stimulation")),
                       outputs = data.frame(Node = "ShootSignal",
                                            Coregulator = NA,
                                            Influence = "necessary stimulation"),
                       travel = 1,
                       degradation = 1)
save(BranchOutgrowth, file = "./Data/BranchOutgrowth.RData")

ShootSignal <- new("Hormone",
                   name = "ShootSignal",
                   container = "scion",
                   inputs = data.frame(Node = "BranchOutgrowth",
                                       Coregulator = NA,
                                       Influence = "necessary stimulation"),
                   outputs = data.frame(Node = c("StrigolactoneS",
                                                 "StrigolactoneR"),
                                        Coregulator = c(NA, NA),
                                        Influence = c("stimulation",
                                                      "stimulation")),
                   travel = 1,
                   degradation = 1)
save(ShootSignal, file = "./Data/ShootSignal.RData")

Inhibitor <- new("Hormone",
                 name = "Inhibitor",
                 container = "rootstock",
                 inputs = data.frame(Node = NULL,
                                     Coregulator = NULL,
                                     Influence = NULL),
                 outputs = data.frame(Node = "StrigolactoneR",
                                      Coregulator = NA,
                                      Influence = "inhibition"),
                 travel = 1,
                 degradation = 1,
                 genotypes = c("RMS3", "RMS4"))
save(Inhibitor, file = "./Data/Inhibitor.RData")

RMS1 <- new("Genotype",
            name = "RMS1",
            expression = c("scion" = 1, "rootstock" = 1),
            coregulator = "RMS5",
            influence = data.frame(Node = c("StrigolactoneR", "StrigolactoneS"),
                                   Influence = c("production", "production")))
save(RMS1, file = "./Data/RMS1.RData")

RMS5 <- new("Genotype",
            name = "RMS5",
            expression = c("scion" = 1, "rootstock" = 1),
            coregulator = "RMS1",
            influence = data.frame(Node = c("StrigolactoneR", "StrigolactoneS"),
                                   Influence = c("production", "production")))
save(RMS5, file = "./Data/RMS5.RData")

RMS2 <- new("Genotype",
            name = "RMS2",
            expression = c("scion" = 1, "rootstock" = 1),
            influence = data.frame(Node = c("FeedbackR", "FeedbackS"),
                                   Influence = c("production", "production")))
save(RMS2, file = "./Data/RMS2.RData")

RMS3 <- new("Genotype",
            name = "RMS3",
            expression = c("scion" = 1, "rootstock" = 1),
            coregulator = "RMS4",
            influence = data.frame(Node = c("BranchInhibitor", "Inhibitor"),
                                   Influence = c("production", "production")))
save(RMS3, file = "./Data/RMS3.RData")

RMS4 <- new("Genotype",
            name = "RMS4",
            expression = c("scion" = 1, "rootstock" = 1),
            coregulator = "RMS3",
            influence = data.frame(Node = c("BranchInhibitor", "Inhibitor"),
                                   Influence = c("production", "production")))
save(RMS4, file = "./Data/RMS4.RData")

peaNetwork <- buildNetwork(hormones = list(StrigolactoneR,
                                           StrigolactoneS,
                                           FeedbackR,
                                           FeedbackS,
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
                                           "unknown"),
                             AF = c("positive influence",
                                      "negative influence",
                                      "necessary stimulation",
                                      NA, NA, NA,
                                      "unknown influence"),
                             ER = c("stimulation",
                                    "inhibition",
                                    "necessary stimulation",
                                    NA,
                                    "absolute stimulation",
                                    "absolute inhibition",
                                    "modulation"))
save(langConversion, file = "./Data/langConversion.RData")



