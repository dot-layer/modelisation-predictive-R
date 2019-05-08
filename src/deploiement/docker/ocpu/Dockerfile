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

# rstudio
RUN apt-get install -y gdebi-core git sudo && \
	wget --quiet https://download2.rstudio.org/server/bionic/amd64/rstudio-server-${RSTUDIO_VERSION}-amd64.deb && \
	gdebi --non-interactive rstudio-server-${RSTUDIO_VERSION}-amd64.deb && \
	rm -f rstudio-server-${RSTUDIO_VERSION}-amd64.deb

# opencpu
RUN useradd -ms /bin/bash builder

USER builder

RUN cd ~/ && wget --quiet https://github.com/opencpu/opencpu-server/archive/v${OCPU_BRANCH}.tar.gz && \
	tar xzf v${OCPU_BRANCH}.tar.gz && \
	cd opencpu-server-${OCPU_BRANCH} && \
	dpkg-buildpackage -us -uc

USER root

RUN apt-get install -y libapache2-mod-r-base && \
	dpkg -i /home/builder/opencpu-lib_*.deb && \
	dpkg -i /home/builder/opencpu-server_*.deb

RUN ln -sf /proc/self/fd/1 /var/log/apache2/access.log && \
	ln -sf /proc/self/fd/1 /var/log/apache2/error.log && \
	ln -sf /proc/self/fd/1 /var/log/opencpu/apache_access.log && \
	ln -sf /proc/self/fd/1 /var/log/opencpu/apache_error.log

# Set opencpu password so that we can login
RUN echo "opencpu:opencpu" | chpasswd

# Custom server.conf for pre-loading specific packages in ocpu
#COPY server.conf /etc/opencpu/server.conf

# R packages
RUN R -e "install.packages(c('plotly', 'aws.s3'), repos='http://cran.rstudio.com/')"

EXPOSE 80
EXPOSE 443
EXPOSE 8004
EXPOSE 8787

# Start non-daemonized webserver
CMD /usr/lib/rstudio-server/bin/rserver && apachectl -DFOREGROUND
