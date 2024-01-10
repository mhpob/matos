# Precompiled vignettes that depend on API key
# borrowed from https://github.com/ropensci/eia/blob/master/vignettes/precompile.R
# and this blog post https://ropensci.org/blog/2019/12/08/precompute-vignettes/

library(knitr)

# Need to change workind directory to get the figure paths correct.
#   see Note section in ?knitr::knit
orig_wd <- getwd()
setwd("vignettes")

knit("_introduction.Rmd", "introduction.Rmd")
knit("_matos-otndo.Rmd", "matos-otndo.Rmd")
knit("_multiple-projects.Rmd", "multiple-projects.Rmd")
knit("_matos-rvdat.Rmd", "matos-rvdat.Rmd")

setwd(orig_wd)
