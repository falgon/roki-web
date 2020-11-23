FROM ubuntu:20.04
SHELL ["/bin/bash", "-c"]
USER root
LABEL maintainer="Roki <falgon53@gmail.com>"
ENV DEBIAN_FRONTEND noninteractive
ENV DEBCONF_NOWARNINGS yes
RUN apt-get -qq update \
    && apt-get -y -qq install --no-install-recommends \
        curl \
        ca-certificates \
        language-pack-ja \
        libtinfo-dev \
        git \
    && curl -sSL https://get.haskellstack.org/ | sh \
    && curl -sSL https://deb.nodesource.com/setup_14.x | bash \
    && apt-get -y -qq install --no-install-recommends nodejs \
    && apt-get -y -qq autoremove \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /opt/src
ENV LANG ja_JP.UTF-8
ENV LANGUAGE ja_JP:ja
ENV LC_ALL ja_JP.UTF-8
COPY . /opt/src
WORKDIR /opt/src
RUN stack --no-terminal build --only-dependencies
RUN npm i
