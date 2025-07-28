deploy <- "liver"
deploydir <- "deployLiverNew"
deploy <- "trait"
deploydir <- "deployNew"
small <- FALSE

dirpath <- file.path("../attie_alan/FounderDietStudy", deploydir)
traitData <- readRDS(file.path(dirpath, paste0(deploy, "Data.rds")))
traitSignal <- readRDS(file.path(dirpath, paste0(deploy, "Signal.rds")))
traitStats <- readRDS(file.path(dirpath, paste0(deploy, "Stats.rds")))
#traitModule <- readRDS(file.path(dirpath, "traitModule.rds"))
traitModule <- NULL
datasets <- readRDS(file.path(dirpath, "datasets.rds"))
customSettings <- list(
  help = "help.md",
  condition = "diet",
  group = "module",
#  entrykey = "Founder",
  dataset = datasets)
#Founder

# Small data
if(small) {
  traitData <- traitData |>
    dplyr::filter(dataset == dataset[1], trait %in% unique(trait)[1:2])
  traits <- unique(traitData$trait)
  traitSignal <- traitSignal |>
    dplyr::filter(dataset == dataset[1], trait %in% traits)
  traitStats <- traitStats |>
    dplyr::filter(dataset == dataset[1], trait %in% traits)
}


