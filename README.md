### Data and R code for: "Mitigating ecosystem service tradeoffs in rangelands by using grazing duration and timing to manage water quality" by Hulvey, KB, CD Mellon and AR Kleinhesselink. Journal of Applied Ecology. 

### Date: 2021-06-18 

### Contents:

1) R code to reproduce the analysis is in __riparian_grazing_analysis.Rmd__
2) Data used in the analysis is in an excel spreadsheet __data/ecoli_data.xlsx__
3) Statistical output, data and code for the Shiny interactive figures are found in the __Grazing-Windows/__ folder 

### To recreat the analysis: 

This is an Rstudio project. Please open the __riparian_grazing.Rproj__ file in Rstudio. Then run the code found in __riparian_grazing_analysis.Rmd__ script. This script fits all models and generates all figures used in the publication. The script also saves models and data for use by the Shiny interactive figure. This is an R markdown document which can be knitted to a PDF or HTML file.  

### Data used in the analysis in "ecoli_data.xlsx"

10 columns by 361 rows. 

Column descriptions: 

* "treatment", character. Cattle rotation Treatment labels for each stream. 
- "stream", character. Abbreviated stream names. 
- "pasture", character. Abbreviated pasture names. 
- "ecoli_MPN", numeric (MPN 100 ml<sup>-1</sup>). Observed concentrations of *E. coli* in units of most probable number (MPN) of colony forming units per 100 ml of water. See text for methods. 
- "grazing_start", date (month/day/year). Date when cattle were moved on to pasture according to rotation scheme. 
- "grazing_end", date (month/day/year). Date when cattle were moved off of the pasture according to rotation scheme. 
- "trailing_start", date (month/day/year). Start date for cattle temporarily occupying pasture while moving through the area.  
- "trailing end", date (month/day/year). End date for cattle temporarily occupying pasture while moving through the area. 
- "sample_date" date (month/day/year).  Date for *E. coli* water sample collection. 
- "cattle_present" binary (0/1). Indicates whether cattle were present in the pasture, either during the grazing period or directly observed.


### Grazing-Windows 

This directory contains data and R code required to create and deploy the R Shiny interactive figures at https://beautiful.shinyapps.io/Grazing-Windows/. 

### Built With
- OS: x86_64-apple-darwin17.0
- R version 4.0.4 (2021-02-15) "Lost Library Book"

#### Required Packages: 

- tidyverse
- scales
- gamm4
- emmeans
- lubridate
- cowplot
- pbkrtest
- stargazer
- ggrepel
- shinyr

### Authors
Andrew R. Kleinhesselink and Kris B. Hulvey
