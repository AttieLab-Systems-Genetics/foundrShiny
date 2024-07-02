# foundrShiny

Shiny app tools companion package for
[foundr](https://github.com/byandell/foundr) package. To install:

```
install.packages("devtools")
devtools::install_github("byandell/foundr", ref = "foundrBase")
devtools::install_github("byandell/foundrShiny")
```

This package can be used to build shiny apps for analysis and visualization of founder data.
It is part of a (planned) collection of packages. See
[Foundr App Developer Guide](https://docs.google.com/presentation/d/171HEopFlSTtf_AbrA28YIAJxJHvkzihB4_lcV6Ct-eI)
for an overview of package(s) use and components.

- [foundr](https://github.com/byandell/foundr): data analysis and visualization
  - See [foundrBase tree of foundr](https://github.com/byandell/foundr/tree/foundrBase) for the revised `foundr` package (in testing phase).
- [foundrShiny](https://github.com/byandell/foundrShiny): interactive shiny app
- [foundrHarmony](https://github.com/byandell/foundrHarmony): harmonize data from multiple sources (being written)
- [modulr](https://github.com/byandell/modulr): harmonize WGCNA module objects

## Improvements Planned

- order module list on contrast by p-values
- SD and logp calc and guideline defaults
- add pct eigenvalue from module
- larger points on several plots
- Need to add group element to customSettings (done)
- need to rethink foundr::ggplot_conditionContrasts, which is now bloated
- updated foundr app mostly works now except
  + shifting between panels seems to wipe out `key_trait` and maybe others
  + the `bareApp()` is a draft diagnostic tool to investigate

## foundrShiny Modular Organization

The foundrShiny package is completely organized using
[Shiny modules](https://mastering-shiny.org/scaling-modules.html).
All files in the `R` function directory have built-in apps following
[shiny module naming conventions](https://mastering-shiny.org/scaling-modules.html#naming-conventions).
A useful guide is
[Mastering Shiny](https://mastering-shiny.org/).

The app has a primary module `foundr` with secondary modules for the tab panels
`trait`, `contrast`, `stats`, `time` and `about`.

- [foundr.R](https://github.com/byandell/foundrShiny/blob/main/R/foundr.R):
main app; see also
[app.R](https://github.com/byandell/foundrShiny/blob/main/inst/shinyApp/app.R):
for deployment example.
- [trait.R](https://github.com/byandell/foundrShiny/blob/main/R/trait.R):
trait visualization
- [contrast.R](https://github.com/byandell/foundrShiny/blob/main/R/contrast.R):
contrasts of conditions across strains by sex
- [stats.R](https://github.com/byandell/foundrShiny/blob/main/R/stats.R):
stats on design effects
- [time.R](https://github.com/byandell/foundrShiny/blob/main/R/time.R):
traits over time
- [about.R](https://github.com/byandell/foundrShiny/blob/main/R/about.R):
about this app

Specialized modules are called from these modules.
Two additional files that are not modules are the following:

- foundr_helpers.R: helper functions
- foundrSetup.R: setup data for all apps (requires data access)

## foundrShiny Parameters

Parameters are organized into sets

- `main_par` global parameters
- `panel_par` panel-specific parameters
- `plot_par` plot-specific parameters

These sets of parameters are [reactiveValues](https://mastering-shiny.org/reactivity-objects.html)
placed in Shiny modules to simplify code readability through reuse.

The parameter sets are served up via servers, which depend on some data object,

```
main_par <- mainParServer("main_par", traitSignal)
panel_par <- panelParServer("panel_par", main_par, traitSignal)
plot_par <- plotParServer("plot_par", contrast_table)
```

and deployed via three UIs, which input parameter values.
Each set is organized in its own shiny module, complete with app.

- [mainPar.R](https://github.com/byandell/foundrShiny/blob/main/R/mainPar.R)
- [panelPar.R](https://github.com/byandell/foundrShiny/blob/main/R/panelPar.R)
- [plotPar.R](https://github.com/byandell/foundrShiny/blob/main/R/plotPar.R)

### Global Parameters (`main_par`)

The main parameters `main_par` are set in the main app in
[foundr.R](https://github.com/byandell/foundrShiny/blob/main/R/foundr.R)
and flow through to the panels

```
mainParInput("main_par"), # dataset
mainParUI("main_par"), # order
mainParOutput("main_par"), # plot_table, height
```

### Plot-specific Parameters (`plot_par`)

This set is used in the
[contrastPlot.R](https://github.com/byandell/foundrShiny/blob/main/R/contrastPlot.R)
module, which is reused to develop contrast plots across traits.

```
plotParInput("plot_par") # ordername, interact
plotParUI("plot_par"), # volsd, volvert (sliders)
plotParOutput("plot_par"), # rownames (strains/terms)
```

These parameters are used in the three types of plots called via `contrastPlot`

- [volcano.R](https://github.com/byandell/foundrShiny/blob/main/R/volcano.R)
- [biplot.R](https://github.com/byandell/foundrShiny/blob/main/R/biplot.R)
- [dotplot.R](https://github.com/byandell/foundrShiny/blob/main/R/dotplot.R)

which are deployed in the `contrast` and `stats` panels via the following modules

- [contrastSex.R](https://github.com/byandell/foundrShiny/blob/main/R/contrastSex.R):
contrasts of conditions across strains by sex
- [contrastGroup.R](https://github.com/byandell/foundrShiny/blob/main/R/contrastGroup.R):
contrasts for groups of traits (such as WGCNA modules)
- [stats.R](https://github.com/byandell/foundrShiny/blob/main/R/stats.R):
stats on design effects

### Panel-specific Parameters (`panel_par`)

Panel parameters are reused in different panels in slightly different ways.
It made sense to allow these to let the user set these separately within each panel. **This is in development and not yet implemented.** Note that some modules have `input$trait` or `input$traits`. I have begun migrating to panelPar in
`traitTable`.

```
  panelParInput("panel_par"), # Strains, Facet
  panelParUI("panel_par"), # Traits
  panelParOutput("panel_par") # Sexes (B/F/M/C)
```  

Some parameters are passed as lists from one module to another. For instance,
the `traitTable` module returns a 
- trait
  + traitTable: strains
  + traitSolos: facet
  + traitPairs: facet
- stats
  + contrastPlot: sex
- time
  + timePlot: strains, facet
  + timeTable: strains
  + timeTraits: contrast (traits)
- contrast
  + contrastSex: sex 
  + contrastTime: contrast, strains
  + timePlot: strains, facet
  + contrastGroup: sex, group
  + contrastPlot: sex, strain (via volcano?)
  
Careful that contrastPlot and its sub-modules
(volcano,biplot,dotplot)
us rownames from plotPar, which is substitute for strain.
Volcano may have wrong call for strain.


## Panel sub-modules

Panels deploy sub-modules, which in turn might deploy other sub-modules.
By convention, each module has a Server and one or more of Input, UI, Output.
Several sub-modules return reactive objects, which might be used by other
sub-modules.

### Trait module

- traitOrder
- traitNames (used twice)
- corTable
- corPlot
- traitTable
- traitSolos
- traitPairs

### Stats module

- contrastPlot

### Time module

- timeTable
  + traitOrder (reused from trait module)
  + timeTraits
- timePlot

### Contrast module

- contrastTable (used three times)
  + traitOrder (reused from trait module)
- contrastSex
  + contrastPlot
- contrastTime
  + timeTraits
- timePlot (reused from time module)
- contrastGroup
  + contrastPlot

```
foundrSetup(data_subset = "Physio",
  dirpath = "~/Documents/Research/attie_alan/FounderDietStudy/deployLiver")
```