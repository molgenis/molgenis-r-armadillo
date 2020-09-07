knitr::knit("vignettes/MolgenisArmadillo.Rmd.orig", output = "vignettes/MolgenisArmadillo.Rmd")
armadillo.delete_table("project", "folder", "iris")
armadillo.delete_project("project")
