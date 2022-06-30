StrigolactoneR <- new("Hormone",
                      name = "StrigolactoneR",
                      container = "rootstock",
                      inputs = data.frame(Node = c("FeedbackR",
                                                   "Inhibitor",
                                                   "ShootSignal"),
                                          Influence = c("stimulation",
                                                        "inhibition",
                                                        "stimulation")),
                      outputs = data.frame(Node = "StrigolactoneS", Influence = "stimulation"),
                      travel = 0.8,
                      genotypes = c("RMS1", "RMS5"))
save(StrigolactoneR, file = "./Data/StrigolactoneR.RData")


StrigolactoneS <- new("Hormone",
                      name = "StrigolactoneS",
                      container = "scion",
                      inputs = data.frame(Node = c("FeedbackS",
                                                   "StrigolactoneR",
                                                   "ShootSignal"),
                                          Influence = c("stimulation",
                                                        "stimulation",
                                                        "stimulation")),
                      outputs = data.frame(Node = "BranchInhibitor", Influence = "stimulation"),
                      travel = 0,
                      genotypes = c("RMS1", "RMS5"))
save(StrigolactoneS, file = "./Data/StrigolactoneS.RData")

FeedbackR <- new("Hormone",
                 name = "FeedbackR",
                 container = "rootstock",
                 inputs = data.frame(Node = "FeedbackS",
                                     Influence = "stimulation"),
                 outputs = data.frame(Node = c("Cytokinin", "StrigolactoneR"),
                                      Influence = c("inhibition", "stimulation")),
                 travel = 0,
                 genotypes = "RMS2")
save(FeedbackR, file = "./Data/FeedbackR.RData")

FeedbackS <- new("Hormone",
                 name = "FeedbackS",
                 container = "scion",
                 inputs = data.frame(Node = c("BranchInhibitor"),
                                     Influence = c("inhibition")),
                 outputs = data.frame(Node = c("FeedbackR", "StrigolactoneS"),
                                      Influence = c("stimulation", "stimulation")),
                 travel = 1,
                 genotypes = "RMS2")
save(FeedbackS, file = "./Data/FeedbackS.RData")

Cytokinin <- new("Hormone",
                 name = "Cytokinin",
                 container = "rootstock",
                 inputs = data.frame(Node = "FeedbackR",
                                     Influence = "inhibition"),
                 outputs = data.frame(Node = "BranchOutgrowth",
                                     Influence = "stimulation"),
                 travel = 0.8)
save(Cytokinin, file = "./Data/Cytokinin.RData")

BranchInhibitor = new("Hormone",
                      name = "BranchInhibitor",
                      container = "scion",
                      inputs = data.frame(Node = "StrigolactoneS",
                                          Influence = "stimulation"),
                      outputs = data.frame(Node = c("BudRelease", "FeedbackS"),
                                           Influence = c("inhibition", "inhibition")),
                      travel = 1,
                      genotypes = c("RMS3", "RMS4"))
save(BranchInhibitor, file = "./Data/BranchInhibitor.RData")

BudRelease <- new("Hormone",
                  name = "BudRelease",
                  container = "scion",
                  inputs = data.frame(Node = "BranchInhibitor",
                                      Influence = "inhibition"),
                  outputs = data.frame(Node = "BranchOutgrowth",
                                       Influence = c("necessary stimulation")),
                  travel = 0)
save(BudRelease, file = "./Data/BudRelease.RData")

BranchOutgrowth <- new("Hormone",
                       name = "BranchOutgrowth",
                       container = "scion",
                       inputs = data.frame(Node = c("BudRelease",
                                                    "Cytokinin"),
                                           Influence = c("necessary stimulation",
                                                         "stimulation")),
                       outputs = data.frame(Node = "ShootSignal",
                                            Influence = "necessary stimulation"),
                       travel = 0)
save(BranchOutgrowth, file = "./Data/BranchOutgrowth.RData")

ShootSignal <- new("Hormone",
                   name = "ShootSignal",
                   container = "scion",
                   inputs = data.frame(Node = "BranchOutgrowth",
                                       Influence = "necessary stimulation"),
                   outputs = data.frame(Node = c("StrigolactoneS",
                                                 "StrigolactoneR"),
                                        Influence = c("stimulation",
                                                      "stimulation")),
                   travel = 1)
save(ShootSignal, file = "./Data/ShootSignal.RData")

Inhibitor <- new("Hormone",
                   name = "Inhibitor",
                   container = "rootstock",
                   inputs = data.frame(Node = NULL,
                                       Influence = NULL),
                   outputs = data.frame(Node = "StrigolactoneR",
                                        Influence = "inhibition"),
                   travel = 1,
                 genotypes = c("RMS3", "RMS4"))
save(Inhibitor, file = "./Data/Inhibitor.RData")

RMS1 <- new("Genotype",
            name = "RMS1",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)),
            coregulator = "RMS5",
            influence = data.frame(Node = c("StrigolactoneR", "StrigolactoneS"),
                                   Influence = c("production", "production")))
save(RMS1, file = "./Data/RMS1.RData")

RMS5 <- new("Genotype",
            name = "RMS5",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)),
            coregulator = "RMS1",
            influence = data.frame(Node = c("StrigolactoneR", "StrigolactoneS"),
                                   Influence = c("production", "production")))
save(RMS5, file = "./Data/RMS5.RData")

RMS2 <- new("Genotype",
            name = "RMS2",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)),
            influence = data.frame(Node = c("FeedbackR", "FeedbackS"),
                                   Influence = c("production", "production")))
save(RMS2, file = "./Data/RMS2.RData")

RMS3 <- new("Genotype",
            name = "RMS3",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)),
            coregulator = "RMS4",
            influence = data.frame(Node = c("BranchInhibitor", "Inhibitor"),
                                   Influence = c("production", "production")))
save(RMS3, file = "./Data/RMS3.RData")

RMS4 <- new("Genotype",
            name = "RMS4",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)),
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



