#!/bin/bash

apt-get update -qq && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
    apt-transport-https \
    curl \
    fontconfig \
    libbz2-dev \
    libcurl4-openssl-dev \
    libicu-dev \
    liblzma-dev \
    libpcre3-dev \
    locales \
    perl \
    sudo \
    tzdata \
    wget \
    zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

# Install TinyTeX
export TINYTEX_DIR=/opt/TinyTeX
wget -qO- "https://yihui.name/gh/tinytex/tools/install-unx.sh" | sh -s - --admin --no-path && \
    /opt/TinyTeX/bin/*/tlmgr path add && \
    tlmgr update --self && \
    tlmgr install epstopdf-pkg

# Install pandoc
mkdir -p /opt/pandoc && \
    wget -O /opt/pandoc/pandoc.gz https://files.r-hub.io/pandoc/linux-64/pandoc.gz && \
    gzip -d /opt/pandoc/pandoc.gz && \
    chmod +x /opt/pandoc/pandoc && \
    ln -s /opt/pandoc/pandoc /usr/bin/pandoc && \
    wget -O /opt/pandoc/pandoc-citeproc.gz https://files.r-hub.io/pandoc/linux-64/pandoc-citeproc.gz && \
    gzip -d /opt/pandoc/pandoc-citeproc.gz && \
    chmod +x /opt/pandoc/pandoc-citeproc && \
    ln -s /opt/pandoc/pandoc-citeproc /usr/bin/pandoc-citeproc


R_VERSION=3.6.3
OS_IDENTIFIER=ubuntu-1804

# Install R
wget https://cdn.rstudio.com/r/${OS_IDENTIFIER}/pkgs/r-${R_VERSION}_1_amd64.deb && \
    apt-get update -qq && \
    DEBIAN_FRONTEND=noninteractive apt-get install -f -y ./r-${R_VERSION}_1_amd64.deb && \
    ln -s /opt/R/${R_VERSION}/bin/R /usr/bin/R && \
    ln -s /opt/R/${R_VERSION}/bin/Rscript /usr/bin/Rscript && \
    ln -s /opt/R/${R_VERSION}/lib/R /usr/lib/R && \
    rm r-${R_VERSION}_1_amd64.deb && \
    rm -rf /var/lib/apt/lists/*


# Install additional libraries
add-apt-repository -y ppa:cran/v8

apt-get update -qq &&
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
    default-jdk \
    libgdal-dev \
    libgeos-dev \
    libgit2-dev \
    libjq-dev \
    libmagick++-dev \
    libnode-dev \
    libpng-dev \
    libproj-dev \
    libprotobuf-dev \
    libsodium-dev \
    libssl-dev \
    libssh2-1-dev \
    libudunits2-dev \
    libxml2-dev \
    protobuf-compiler

# Install EFS utils
git clone https://github.com/aws/efs-utils
apt-get -y install binutils
cd efs-utils
./build-deb.sh
apt-get -y install ./build/amazon-efs-utils*deb
cd -

mkdir -p /efs

R -e 'install.packages(c("bookdown", "data.table", "devtools", "dtplyr", "pals", "rmarkdown", "statmod", "tidyverse", "tweedie", "yardstick"), repos = "https://cran.rstudio.com")'

