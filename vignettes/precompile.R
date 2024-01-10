# Precompiled vignettes that depend on API key
# borrowed from https://github.com/ropensci/eia/blob/master/vignettes/precompile.R
# and this blog post https://ropensci.org/blog/2019/12/08/precompute-vignettes/

library(knitr)
knit("vignettes/introduction.Rmd.orig", "vignettes/introduction.Rmd")
knit("vignettes/matos-otndo.Rmd.orig", "vignettes/matos-otndo.Rmd")
knit("vignettes/multiple-projects.Rmd.orig", "vignettes/multiple-projects.Rmd")
