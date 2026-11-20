# OxiDR

R Shiny app from the Chouchani Lab for exploring cysteine oxidation data (OxiDR), with 3D protein structures (Mol*).

## Files
- `app.R` - the Shiny app
- `oxiDR_DATA.tsv` - data read at startup
- `Table_S2.xlsx` - served as the "OxiDR.xlsx" download

## Run locally
```r
install.packages(c("shiny","DT","shinyjs","glue","png","readxl","ggplot2","readr","plotly","bslib","dplyr","remotes"))
remotes::install_github("Appsilon/shiny.molstar")
shiny::runApp()
```