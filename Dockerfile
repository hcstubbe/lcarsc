FROM rocker/verse:4.2.1
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libmariadb-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.6.2")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.3")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "0.3.4")'
RUN Rscript -e 'remotes::install_version("jsonlite",upgrade="never", version = "1.8.0")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.39")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.2")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.4")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "NA")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.14")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("fmsb",upgrade="never", version = "0.7.3")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("RSQLite",upgrade="never", version = "2.2.15")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "2.1.2")'
RUN Rscript -e 'remotes::install_version("uuid",upgrade="never", version = "1.1-0")'
RUN Rscript -e 'remotes::install_version("shinyvalidate",upgrade="never", version = "0.1.2")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("RMariaDB",upgrade="never", version = "1.2.2")'
RUN Rscript -e 'remotes::install_version("pool",upgrade="never", version = "0.1.6")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.9")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.3")'
RUN Rscript -e 'remotes::install_github("rstudio/DT@823b781c7e88c2baa847edbb36c606f29fd85c6a")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 3838
