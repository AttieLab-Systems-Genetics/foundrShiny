# Set up help.md using datasets in `traitSignal`
# Need to first put `help.Rmd` in file.path(dirpath, "LiverData")
foundr::link_datasets(traitSignal, file.path(dirpath, "../RawData/source.csv"), file.path(dirpath, "LiverData"))
datasets <- readRDS(file.path(dirpath, "LiverData", "datasets.rds"))

customSettings <- list(
  help = file.path(dirpath, "deploy", "help.md"),
  condition = "diet",
  entrykey = "Founder",
  dataset = datasets)
