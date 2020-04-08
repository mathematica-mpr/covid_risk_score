FROM rocker/tidyverse:3.6.1 as base

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libxml2-dev \
    libgit2-dev \
    libpng-dev \
    libudunits2-dev \
    libgdal-dev \
    ca-certificates

RUN R -e "install.packages(c('rlang','shiny', 'shinythemes', 'shinyjs', 'shinycssloaders', 'shinyBS', 'tidycensus' , 'assertr', 'flexdashboard', 'httr'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('git2r', type='source', configure.vars='autobrew=yes')"
RUN R -e "devtools::install_github('rstudio/renv')"
RUN R -e "install.packages(c('aws.signature', 'aws.ec2metadata', 'aws.s3'), repos = c(cloudyr = 'http://cloudyr.github.io/drat', getOption('repos')))"