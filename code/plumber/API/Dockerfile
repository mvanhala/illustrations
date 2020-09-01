FROM rocker/tidyverse:3.6.3

RUN apt-get update -qq && apt-get install -y \
  libsodium-dev

RUN R -e 'devtools::install_github("rstudio/plumber", ref = "b7ff05a")'
RUN R -e 'install.packages("dplyr", repos = "https://cran.rstudio.com")'
RUN R -e 'install.packages("tidyr", repos = "https://cran.rstudio.com")'

COPY . /plumber

WORKDIR /plumber

EXPOSE 8000
ENTRYPOINT ["Rscript", "entrypoint.R"]

