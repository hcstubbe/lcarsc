FROM rocker/r-ver:4.1.2
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libmariadb-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.5.1")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.2")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.2.4")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.11")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("RSQLite",upgrade="never", version = "2.2.9")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "2.1.1")'
RUN Rscript -e 'remotes::install_version("uuid",upgrade="never", version = "1.0-3")'
RUN Rscript -e 'remotes::install_version("shinyvalidate",upgrade="never", version = "0.1.1")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("RMariaDB",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("pool",upgrade="never", version = "0.1.6")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.7")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.20")'
RUN Rscript -e 'remotes::install_version("fmsb",upgrade="never")'

EXPOSE 3838
COPY . /build_files
WORKDIR /build_files
RUN R -e 'remotes::install_local(upgrade="never", force = TRUE)'
RUN Rscript -e 'remotes::install_version("devtools",upgrade="never")'
RUN R -e 'devtools::install_local("dependencies/lcarsM.tar.gz", force = T)'

RUN rm -r /build_files


CMD  ["R", "-e", "lcarsc::run_app(options = list(host = '0.0.0.0', port = 3838))"]
