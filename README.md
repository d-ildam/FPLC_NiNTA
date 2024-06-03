# Introduction to FPLC NiNTA Plots
[![DOI](https://zenodo.org/badge/809977276.svg)](https://zenodo.org/doi/10.5281/zenodo.11458405)

This app shows publication ready graphs of Nickel-NTA protein purification from AKTAprime plus FPLC system. If you wish to view the app online, link is accessible here:

https://ildamapp.shinyapps.io/FPLC_NiNTA/


If you would like to run the app from github, please make sure that you have them installed before proceeding:

```
install.packages(c("shiny", "readxl", "ggplot2", "ggpubr", "tidyverse", "shinythemes", "shinyFiles", "rsconnect"))
```

After all the packages are installed, run the lines below in the RStudio Console or from a script. It will download the app and display it in a appropriate browser window:

```
library(shiny)
runGitHub(rep = "FPLC_NiNTA", username = "d-ildam", ref = "main")
```
