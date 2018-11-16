# Copyright (c) Bioinformatics Core Facility of the Max Planck Institute for Biology of Ageing.

FROM rocker/shiny:3.4.4

LABEL maintainer "bioinformatics@age.mpg.de"

RUN apt-get update

RUN apt-get install -yq apt-utils libreadline-dev && \
    apt-get install -yq  liblzma-dev procps psmisc libapparmor1 \
    libedit2 openjdk-8-jdk libbz2-dev libicu-dev

RUN cd / && \
    wget http://ftp.de.debian.org/debian/pool/main/o/openssl/libssl1.0.0_1.0.1t-1+deb8u8_amd64.deb && \
    dpkg -i libssl1.0.0_1.0.1t-1+deb8u8_amd64.deb

RUN R CMD javareconf

RUN cd / && \
    wget https://download2.rstudio.org/rstudio-server-1.1.463-amd64.deb && \
    dpkg -i rstudio-server-1.1.463-amd64.deb && \
    echo "rsession-which-r=$(which R)" >> /etc/rstudio/rserver.conf

# rstudio server port
EXPOSE 8787

ENV NB_USER mpiage
ENV NB_UID 1000

RUN useradd -m -s /bin/bash -N -u $NB_UID $NB_USER ; \
echo "root:bioinf" | chpasswd ; \
echo "mpiage:bioinf" | chpasswd ; \
adduser mpiage sudo

RUN chown -R mpiage: /home/mpiage