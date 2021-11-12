# To update the vignette:
knitr::knit("vignettes/MolgenisArmadillo.Rmd.orig", output = "vignettes/MolgenisArmadillo.Rmd")
knitr::knit("vignettes/creating_data_subsets.Rmd.orig",
            output = "vignettes/creating_data_subsets.Rmd")

# To create a script that uploads the files:
knitr::purl("vignettes/MolgenisArmadillo.Rmd.orig", output = "vignettes/MolgenisArmadillo.R")
knitr::purl("vignettes/creating_data_subsets.Rmd.orig",
            output = "vignettes/creating_data_subsets.R")
