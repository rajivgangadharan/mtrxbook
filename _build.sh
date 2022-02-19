#!/bin/sh

set -ev
export OPENSSL_CONF=/etc/ssl
Rscript -e 'install.packages(c("dplyr", "lubridate"),dependencies = TRUE, repos = "http://cran.us.r-project.org")'
Rscript -e 'install.packages(c("readxl","knitr","kableExtra", "devtools"),dependencies = TRUE, repos = "http://cran.us.r-project.org")'
Rscript -e 'install.packages(c("blogdown", "bookdown", "downlit", "here", "bslib", "plotly", "shiny"), dependencies = TRUE,repos = "http://cran.us.r-project.org")'
Rscript -e 'devtools::install_github("rajivgangadharan/finmetrics")'
Rscript -e 'install.packages("webshot",dependencies=TRUE, repos="http://cran.us.r-project.org")'
Rscript -e 'webshot::install_phantomjs(force=TRUE)'


Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
#Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::pdf_book')"
#Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::epub_book')"

