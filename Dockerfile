FROM rocker/verse AS R_dev_build 
WORKDIR /usr/src
RUN apt-get update
RUN apt install -y pandoc-citeproc texlive-binaries
RUN Rscript -e 'install.packages(c("readxl","knitr","kableExtra", "devtools", "remotes"), dependencies = TRUE)'

FROM R_dev_build AS R_data_manip_build
RUN Rscript -e 'install.packages(c("dplyr", "lubridate"),dependencies = TRUE)'

FROM R_data_manip_build AS R_palette_build
RUN Rscript -e 'install.packages(c("scales"), dependencies = TRUE,repos = "http://cran.us.r-project.org")'
RUN Rscript -e 'install.packages("webshot",dependencies=TRUE, repos="http://cran.us.r-project.org")'
RUN Rscript -e 'webshot::install_phantomjs(force=TRUE)'

FROM R_palette_build AS R_rmarkdown_build
RUN apt remove -y hugo
RUN Rscript -e 'blogdown::install_hugo()'
RUN Rscript -e 'install.packages(c("blogdown", "bookdown", "downlit", "here", "bslib", "plotly", "shiny"),dependencies = TRUE)'
RUN Rscript -e 'remotes::install_github("d3treeR/d3treeR", upgrade=c("never"))'
RUN Rscript -e 'install.packages(c("treemap"), dependencies = TRUE)'

# All custom libs will go here
FROM R_rmarkdown_build AS R_custom_lib_build
RUN Rscript -e 'devtools::install_github("rajivgangadharan/finmetrics")'

FROM R_custom_lib_build as R_main_build 
RUN mkdir -p /usr/src/www/html
RUN mkdir -p /usr/src/inst/extdata
COPY . .

# Will be run during the run process
CMD  Rscript -e 'bookdown::render_book("index.Rmd",output_dir="book")' \
&& cp -R /usr/src/book/* /usr/src/www/html/  
