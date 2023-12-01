FROM rocker/verse:4.2.3
# Set the used port, once deployed in CrownLabs this variable will be overrided with the value used by CrownLabs
ENV CROWNLABS_LISTEN_PORT=80
# Set the web-service basepath, once deployed in CrownLabs this variable will be overrided with the value used by CrownLabs
ENV CROWNLABS_BASE_PATH=/
RUN apt-get update && apt-get install -y  cmake libcurl4-openssl-dev libfribidi-dev libharfbuzz-dev libicu-dev libpng-dev libssl-dev libtiff-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("RColorBrewer",upgrade="never", version = "1.1-3")'
RUN Rscript -e 'remotes::install_version("scales",upgrade="never", version = "1.2.1")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.4.3")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.3.2.1")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "1.0.1")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.42")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.5")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.4")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("imputeTS",upgrade="never", version = "3.3")'
RUN Rscript -e 'remotes::install_version("simputation",upgrade="never", version = "0.2.8")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.9.2")'
RUN Rscript -e 'remotes::install_version("pkgdown",upgrade="never", version = "2.0.7")'
RUN Rscript -e 'remotes::install_version("amap",upgrade="never", version = "0.8-19")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinyalert",upgrade="never", version = "3.0.0")'
RUN Rscript -e 'remotes::install_version("NbClust",upgrade="never", version = "3.0.1")'
RUN Rscript -e 'remotes::install_version("shinyBS",upgrade="never", version = "0.61.1")'
RUN Rscript -e 'remotes::install_version("shinyFeedback",upgrade="never", version = "0.4.0")'
RUN Rscript -e 'remotes::install_version("MLmetrics",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("partykit",upgrade="never", version = "1.2-19")'
RUN Rscript -e 'remotes::install_version("plyr",upgrade="never", version = "1.8.8")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.8.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.30")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.1")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
# Create a user with UID 1010
RUN useradd -m myuser -u 1010
# add permission to myuser  to all files
USER root
RUN chown -R myuser:myuser /build_zone
# Use the previously created user to run the container
USER myuser
EXPOSE ${CROWNLABS_LISTEN_PORT}
CMD R -e "options('shiny.port'=${CROWNLABS_LISTEN_PORT},shiny.host='0.0.0.0');library(eDASH);eDASH::run_app()"
