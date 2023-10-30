# Minor fixes
## matos 0.3.005
  - Precompile vignettes to allow [r-universe](https://mhpob.r-universe.dev/matos) to build.

## matos 0.3.004
  - Fix broken link in [`set_matos_credentials`](https://matos.obrien.page/reference/set_matos_credentials.html) documentation.

## matos 0.3.003
  - Fix [`upload_file`](https://matos.obrien.page/reference/upload_file.html) to actually allow multiple uploads

# matos 0.3

  - The meat of what were `make_tag_push_summary` and `make_receiver_push_summary` have been moved over to a new package: [`otndo`](https://otndo.obrien.page). These functions now wrap those in `otndo` and have been renamed [`matos_tag_summary`](https://matos.obrien.page/reference/matos_tag_summary.html) and [`matos_receiver_summary`](https://matos.obrien.page/reference/matos_receiver_summary.html), respectively.
  - [`list_extract_files`](https://matos.obrien.page/reference/list_extract_files.html) and [`list_project_files`](https://matos.obrien.page/reference/list_project_files.html) now accept vectors of different detection/file types to subset the returned data frame.
  - Misc. tests have been added

# matos 0.2.13

  - [`matos_logoff`](https://matos.obrien.page/reference/matos_logoff.html), allowing you to log out of your MATOS session.
  - A draft vignette on how to manage multiple telemetry projects by using `matos` in parallel.
  - [`make_receiver_push_summary`](https://matos.obrien.page/reference/make_receiver_push_summary.html) and [`make_tag_push_summary`](https://matos.obrien.page/reference/make_tag_push_summary.html) now scrape the OTN GeoServer in order to get a correct title for your project summaries!
  - A more-informative error now appears if you try to find information on a project that doesn't exist.

# matos 0.2.1

  - The website has moved to [https://matos.obrien.page/](https://matos.obrien.page/).
  - [`set_matos_credentials`](https://matos.obrien.page/reference/set_matos_credentials.html) 
    - *NEW* function!
    - Allows you to store your MATOS credentials in your [.Renviron](https://rstats.wtf/r-startup.html#renviron) for seamless log in.
  - [`make_tag_push_summary`](https://matos.obrien.page/reference/make_tag_push_summary.html)
    - *NEW* function!
    - Undergoing heavy development, so please file issues with bugs and suggestions.
    - Summarize your your TON returns of tagged fish!
  - [`make_tag_push_summary`](https://matos.obrien.page/reference/make_tag_push_summary.html) and [`make_receiver_push_summary`](https://matos.obrien.page/reference/make_receiver_push_summary.html) now accept zipped folders as input.

# matos 0.2

  - The package has a pkgdown website at [https://mhpob.github.io/matos/](https://mhpob.github.io/matos/)
  - [`act_push_summary`](https://mhpob.github.io/matos/reference/act_push_summary.html) is live! This currently only does receiver summaries, but tag summaries are coming soon.
  - Most functions have been renamed following a LIST-GET workflow.
    - LIST your files to see what you have
    - GET those files
    - and also... UPLOAD. But that didn't fit into the pithy saying.
  - A few functions, namely `list_files` and `get_file` have been split into functions with fewer options and clearer names ([`list_extract_files`](https://matos.obrien.page/reference/list_extract_files.html) and [`list_project_files`](https://matos.obrien.page/reference/list_project_files.html), e.g.). Hopefully this will make things more intuitive.
  
# matos 0.1.1
  - `get_updates`: A new function to download all files updated since a given date. Super useful after a data push!
  - `list_files` now has a `since` argument, allowing you to only list the files that have been updated since a certain date.
* Added a `NEWS.md` file to track changes to the package.
