FROM ubuntu:18.04

ENV OCPU_BRANCH 2.1.1
ENV RSTUDIO_VERSION 1.2.1335
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update

RUN apt-get install -y apt-transport-https ca-certificates

# base packages
RUN apt-get update && apt-get install -y \
	build-essential git python3-pip python-setuptools graphviz \
	curl libcurl4-openssl-dev libssl-dev \
	software-properties-common make cmake devscripts \
	apache2 apache2-dev libapreq2-dev libapparmor-dev apparmor-profiles \
	libprotobuf-dev protobuf-compiler language-pack-en-base \
	xvfb xauth libxml2-dev nano \
	libicu-dev pkg-config libssh2-1-dev locales apt-utils wget libstdc++6

RUN add-apt-repository ppa:ubuntu-toolchain-r/test && \
	apt-get update && \
	apt-get upgrade -y libstdc++6
	
RUN apt-get update && apt-get install -y \
	libgeos-dev libgdal-dev libproj-dev \
	libpng-dev libjpeg-dev libjpeg-turbo8-dev libtiff-dev fftw3-dev \
	libopenblas-dev liblapack-dev

# install R 
RUN echo "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/" >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN apt-get update && apt-get install -y r-base r-base-dev

RUN apt-get install -y libx11-dev libcairo2-dev cairo-perf-utils

# R packages
RUN R -e "install.packages(c('devtools', 'ggplot2', 'lubridate', 'formatR', 'assertthat', 'tictoc', 'fst', 'reticulate', 'argparse', 'bit64', 'dplyr', 'testthat'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('roxygen2'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('data.table', type = 'source', repos = 'http://Rdatatable.github.io/data.table')"

# lightgbm
RUN git clone --recursive https://github.com/Microsoft/LightGBM && \
	cd LightGBM && Rscript build_r.R

# xgboost	
RUN git clone --recursive https://github.com/dmlc/xgboost && \
	cd xgboost && cd R-package && R CMD INSTALL .

# R packages
RUN R -e "install.packages(c('plotly', 'aws.s3'), repos='http://cran.rstudio.com/')"

# Plumber
RUN apt-get install -y libcurl4-gnutls-dev libsodium-dev
RUN R -e "devtools::install_github('trestletech/plumber')"
RUN R -e "install.packages(c('caret'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('glmnet'), repos='http://cran.rstudio.com/')"

EXPOSE 8080

# RUN mkdir dotlayer
COPY plumber /dotlayer

WORKDIR /dotlayer

RUN ls

ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/dotlayer/pred_bixi.R'); pr$run(host='0.0.0.0', port=8080)"]

#docker run --rm -p 8000:8000 trestletech/plumber \
#  /usr/local/lib/R/site-library/plumber/examples/04-mean-sum/plumber.R
 
# ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/usr/local/lib/R/site-library/plumber/examples/04-mean-sum/plumber.R'); pr$run(host='0.0.0.0', port=8080)"]
# CMD ["/usr/local/lib/R/site-library/plumber/examples/04-mean-sum/plumber.R"]

