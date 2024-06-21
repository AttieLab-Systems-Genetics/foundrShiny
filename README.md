# foundrShiny

Shiny app tools for foundr package. To install:

```
devtools::install_github("byandell/foundr", ref = "foundrBase")
devtools::install_github("byandell/foundrShiny")
```

This package can be used on its own for analysis and visualization of founder data,
and is part of a (planned) collection of packages. See
[Foundr App Developer Guide](https://docs.google.com/presentation/d/171HEopFlSTtf_AbrA28YIAJxJHvkzihB4_lcV6Ct-eI)
for an overview of package(s) use and components.

- [foundr](https://github.com/byandell/foundr): data analysis and visualization
  - See [foundrBase tree of foundr](https://github.com/byandell/foundr/tree/foundrBase) for the revised `foundr` package (in testing phase).
- [foundrShiny](https://github.com/byandell/foundrShiny): interactive shiny app
- foundrHarmony: harmonize data from multiple sources (to be written)
- [modulr](https://github.com/byandell/modulr): harmonize WGCNA module objects



## Apps

Nearly all files in the `R` function directory have built-in apps following
[shiny module naming conventions](https://mastering-shiny.org/scaling-modules.html#naming-conventions). Exceptions are the following:

- foundr_helpers.R: helper functions
- foundrSetup.R: setup data for all apps (requires data access)

- download.R: download module
- contrastTime.R: contrasts over time module
- timePlot.R: time plot module
- timeTable.R: time table module
- timeTraits.R: time traits module

## Improvements Planned

- fix contrast tab module list
- order module list on contrast by p-values
- SD and logp calc and guideline defaults
- add pct eigenvalue from module
- larger points on several plots
- MixMod as only dataset does not display for Contrasts tab. Seems to be related to not showing order.
- Need to add group element to customSettings (done)
- redesign `R/contrastPlot.R`
  + split out volcano,biplot,dotplot as separate modules (done)
  + need to rethink foundr::ggplot_conditionContrasts, which is now bloated
- new panelPar.R is nonstandard, but can simplify panel parameters
  + experiment and add in
- make apps for remaining files (timePlot, timeTable)

## Module Parameters

Parameters are organized into groups

- `main_par` global parameters
- `panel_par` panel-specific parameters
- `plot_par` plot-specific parameters

Shiny server sequence


```
main_par <- mainParServer("main_par", traitSignal)
panel_par <- panelParServer("panel_par", main_par, traitSignal)
plot_par <- 
```

### Global Parameters (`main_par`)

```
mainParInput("main_par"), # Dataset
mainParOutput("main_par"), # Plots/Tables Height
```

### Plot-specific Parameters (`plot_par`)

```
plotParInput("plot_par") # RankOrder Interact
plotParUI("plot_par"), # SD,Vert sliders
plotParOutput("plot_par"), # Row Names (strains/terms)
```

### Panel-specific Parameters (`panel_par`)

```
  panelParUI("panel_par"), # Traits
  panelParInput("panel_par"), # Strains
  panelParOutput("panel_par") # Sexes (B/F/M/C)
```  


```
foundrSetup(data_subset = "Physio",
  dirpath = "~/Documents/Research/attie_alan/FounderDietStudy/deployLiver")
```