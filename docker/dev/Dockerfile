FROM ubuntu:20.04
SHELL ["/bin/bash", "-c"]
LABEL maintainer="Roki <falgon53@yahoo.co.jp>"
LABEL org.opencontainers.image.authors="Roki <falgon53@yahoo.co.jp>"
LABEL org.opencontainers.image.description="Develop image from the website roki.dev"

ENV DEBIAN_FRONTEND=noninteractive \
    DEBCONF_NOWARNINGS=yes
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
    && rm -rf /var/lib/apt/lists/* \
    && groupadd -g 61000 rw-dev \
    && useradd -g 61000 -l -m -s /bin/bash -u 61000 rw-dev
USER rw-dev
RUN mkdir /home/rw-dev/src
ENV LANG=ja_JP.UTF-8 \
    LANGUAGE=ja_JP:ja \
    LC_ALL=ja_JP.UTF-8
COPY --chown=rw-dev:rw-dev . /home/rw-dev/src
WORKDIR /home/rw-dev/src
RUN stack --no-terminal build --only-dependencies \
    && npm i
