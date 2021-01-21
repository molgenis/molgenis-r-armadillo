# Set work directory (N.B. user specific!)
setwd("~/git/molgenis-r-armadillo/vignettes")

# To update the vignette:
knitr::knit("MolgenisArmadillo.Rmd.orig", output = "MolgenisArmadillo.Rmd")
knitr::knit("creating_data_subsets.Rmd.orig",
            output = "creating_data_subsets.Rmd")

# To create a script that uploads the files:
knitr::purl("MolgenisArmadillo.Rmd.orig", output = "MolgenisArmadillo.R")
knitr::purl("creating_data_subsets.Rmd.orig",
            output = "creating_data_subsets.R")
