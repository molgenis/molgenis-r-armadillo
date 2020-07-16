knitr::knit("vignettes/MolgenisArmadillo.Rmd.orig", output = "vignettes/MolgenisArmadillo.Rmd")
armadillo.delete_workspace("shared.folder", "test-data")
armadillo.delete_folder("shared.folder")
