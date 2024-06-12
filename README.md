# foundrShiny
Shiny app tools for foundr package

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

- blank dataseet and trait
- fix contrast tab module list
- order module list on contrast by p-values
- SD and logp calc and guideline defaults
- add pct eigenvalue from module
- larger points on several plots
- MixMod as only dataset does not display for Contrasts tab. Seems to be related to not showing order.

```
foundrSetup(data_subset = "Physio",
  dirpath = "~/Documents/Research/attie_alan/FounderDietStudy/deployLiver")
```