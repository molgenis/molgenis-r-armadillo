knitr::knit("vignettes/MolgenisArmadillo.Rmd.orig", output = "vignettes/MolgenisArmadillo.Rmd")
delete_workspace("shared.folder", "test-data")
delete_folder("shared.folder")