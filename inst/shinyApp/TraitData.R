# Source to setup data for foundr app
# local variables optional:
#       data_subset: subset of dataset column entries in traitData
#       custom_settings: add customSettings if `TRUE`
#       data_instance: one of "Trait" or "Liver" (default "Trait")

if(!exists("dirpath")) {
  dirpath <- file.path("~", "founder_diet_study")
  dirpath <- file.path(dirpath, "HarmonizedData")
  datapath <- file.path(dirpath, "AppSetup", dataInstance)
} else {
  datapath <- dirpath
}
cat("dirpath (...) =", dirpath, "\n")

if(!(exists("data_instance") & tolower(data_instance) %in% c("trait","liver"))) {
  data_instance <- "trait"
}
data_instance <- tolower(data_instance)
dataInstance <- stringr::str_to_title(data_instance)
cat("Instance:", data_instance, "\n")
cat("Assumes", 
    paste(data_instance, c("Data", "Signal", "Stats"), sep = "", collapse = ", "),
    "RDS files are in ... folder\n")

if(exists("data_subset") && is.character(data_subset)) {
  cat("Data subset:", paste(data_subset, collapse = ", "), "\n")
  traitData <- dplyr::filter(
    readRDS(file.path(dirpath, paste0(data_instance, "Data.rds"))),
    dataset %in% data_subset)
  traitSignal <- dplyr::filter(
    readRDS(file.path(dirpath, paste0(data_instance, "Signal.rds"))),
    dataset %in% data_subset)
  traitStats <- dplyr::filter(
    readRDS(file.path(dirpath, paste0(data_instance, "Stats.rds"))),
    dataset %in% data_subset)
} else {
  traitData   <- readRDS(file.path(dirpath, paste0(data_instance, "Data.rds")))
  traitSignal <- readRDS(file.path(dirpath, paste0(data_instance, "Signal.rds")))
  traitStats  <- readRDS(file.path(dirpath, paste0(data_instance, "Stats.rds")))
}
cat("Assumes traitModule.rds is in", dirpath, "\n")
traitModule <- readRDS(file.path(dirpath, "traitModule.rds"))

if(exists("custom_settings") && custom_settings) {
  cat("Custom settings\n")
  cat("Assumes", datapath, "has proper files\n")
  if(exists("new_help")) {
    # Set up help.md using datasets in `traitSignal`
    # Need to first put `help.Rmd` in `file.path(dirpath, "AppSetup", dataInstance)`
    foundr::link_datasets(traitSignal, file.path(dirpath, "../RawData/source.csv"),
                          file.path(dirpath, "AppSetup", dataInstance))
  }
  datasets <- readRDS(file.path(datapath, "datasets.rds"))
  
  customSettings <- list(
    help = file.path(datapath, "help.md"),
    condition = "diet",
    group = "module",
    entrykey = "Founder",
    dataset = datasets)
}

