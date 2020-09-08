# Set work directory (N.B. user specific!)
setwd("~/git/molgenis-r-armadillo/vignettes")

# To update the vignette:
knitr::knit("MolgenisArmadillo.Rmd.orig", output = "MolgenisArmadillo.Rmd")

# To create a script that uploads the files:
knitr::purl("MolgenisArmadillo.Rmd.orig", output = "MolgenisArmadillo.R")
