# Precompiled vignettes that depend on API key
# borrowed from https://github.com/ropensci/eia/blob/master/vignettes/precompile.R
# and this blog post https://ropensci.org/blog/2019/12/08/precompute-vignettes/

library(knitr)
knit("vignettes/_introduction.Rmd", "vignettes/introduction.Rmd")
knit("vignettes/_matos-otndo.Rmd", "vignettes/matos-otndo.Rmd")
knit("vignettes/_multiple-projects.Rmd", "vignettes/multiple-projects.Rmd")
knit("vignettes/_matos-rvdat.Rmd", "vignettes/matos-rvdat.Rmd")
