FROM rocker/verse AS R_dev_build 
WORKDIR /usr/src
RUN apt-get update
RUN apt install -y pandoc-citeproc texlive-binaries
RUN Rscript -e 'install.packages(c("readxl","knitr","kableExtra", "devtools"),dependencies = TRUE)'

FROM R_dev_build AS R_data_manip_build
RUN Rscript -e 'install.packages(c("dplyr", "lubridate"),dependencies = TRUE)'

FROM R_data_manip_build AS R_rdown_build
RUN apt remove -y hugo
RUN Rscript -e 'blogdown::install_hugo()'
RUN Rscript -e 'install.packages(c("blogdown", "bookdown", "downlit", "here", "bslib", "plotly", "shiny"),dependencies = TRUE)'

FROM R_rdown_build as R_main_build 
RUN mkdir -p /usr/src/www/html
RUN mkdir -p /usr/src/inst/extdata
RUN Rscript -e 'devtools::install_github("rajivgangadharan/finmetrics")'
COPY . .

# Will be run during the run process
CMD  Rscript -e 'bookdown::render_book("index.Rmd",output_dir="book")' \
&& cp -R /usr/src/book/* /usr/src/www/html/  
