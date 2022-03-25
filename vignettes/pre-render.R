# Set work directory (N.B. user specific!)
setwd("~/git/molgenis-r-armadillo/vignettes")

# To update the vignette:
knitr::knit("MolgenisArmadillo.Rmd.orig", output = "MolgenisArmadillo.Rmd")
knitr::knit("creating_data_subsets.Rmd.orig",
            output = "creating_data_subsets.Rmd")
knitr::knit("install_packages.Rmd.orig", output = "install_packages.Rmd")
knitr::knit("create_resources.Rmd.orig", output = "create_resources.Rmd")

# To create a script that uploads the files:
knitr::purl("MolgenisArmadillo.Rmd.orig", output = "MolgenisArmadillo.R")
knitr::purl("creating_data_subsets.Rmd.orig",
            output = "creating_data_subsets.R")
knitr::purl("install_packages.Rmd.orig", output = "install_packages.R")
knitr::purl("create_resources.Rmd.orig", output = "create_resources.Rmd")
