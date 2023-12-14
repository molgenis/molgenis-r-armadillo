# To update the vignette:
knitr::knit("vignettes/MolgenisArmadillo.Rmd.orig", output = "vignettes/MolgenisArmadillo.Rmd")
knitr::knit("vignettes/creating_data_subsets.Rmd.orig", output = "vignettes/creating_data_subsets.Rmd")
knitr::knit("vignettes/install_packages.Rmd.orig", output = "vignettes/install_packages.Rmd")
knitr::knit("vignettes/create_resources.Rmd.orig", output = "vignettes/create_resources.Rmd")

# To create a script that uploads the files:
knitr::purl("vignettes/MolgenisArmadillo.Rmd.orig", output = "vignettes/MolgenisArmadillo.R")
knitr::purl("vignettes/install_packages.Rmd.orig", output = "vignettes/install_packages.R")
knitr::purl("vignettes/create_resources.Rmd.orig", output = "vignettes/create_resources.R")
knitr::purl("vignettes/creating_data_subsets.Rmd.orig", output = "vignettes/creating_data_subsets.R")
