FROM rocker/r-ver:latest
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libmariadb-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never")'
RUN Rscript -e 'remotes::install_version("jsonlite",upgrade="never")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never")'
RUN Rscript -e 'remotes::install_version("fmsb",upgrade="never")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never")'
RUN Rscript -e 'remotes::install_version("RSQLite",upgrade="never")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never")'
RUN Rscript -e 'remotes::install_version("uuid",upgrade="never")'
RUN Rscript -e 'remotes::install_version("shinyvalidate",upgrade="never")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never")'
RUN Rscript -e 'remotes::install_version("RMariaDB",upgrade="never")'
RUN Rscript -e 'remotes::install_version("pool",upgrade="never")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never")'
# RUN Rscript -e 'remotes::install_version("DT",upgrade="never")' 0.23 causes a bug
RUN Rscript -e 'remotes::install_github("rstudio/DT")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never")'

EXPOSE 3838
COPY . /build_files
WORKDIR /build_files
RUN R -e 'remotes::install_local("dependencies/lcarsM.tar.gz", force = T)'
RUN R -e 'remotes::install_local(upgrade="never", force = TRUE)'

RUN rm -r /build_files
