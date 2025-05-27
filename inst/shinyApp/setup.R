dirpath <- file.path("../attie_alan/FounderDietStudy/deployLiverNew")
traitData <- readRDS(file.path(dirpath, "liverData.rds")) |>
  dplyr::filter(dataset == dataset[1], trait %in% unique(trait)[1:2])
traits <- unique(traitData$trait)
traitSignal <- readRDS(file.path(dirpath, "liverSignal.rds")) |>
  dplyr::filter(dataset == dataset[1], trait %in% traits)
traitStats <- readRDS(file.path(dirpath, "liverStats.rds")) |>
  dplyr::filter(dataset == dataset[1], trait %in% traits)
#traitModule <- readRDS(file.path(dirpath, "traitModule.rds"))
traitModule <- NULL
datasets <- readRDS(file.path(dirpath, "datasets.rds"))
customSettings <- list(
  help = "help.md",
  condition = "diet",
  group = "module",
  entrykey = "Founder",
  dataset = datasets)
#Founder


