FROM ubuntu:20.04 as bin
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
    && curl -sSL https://get.haskellstack.org/ | sh \
    && apt-get -y -qq autoremove \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /opt/src
ENV LANG ja_JP.UTF-8
ENV LANGUAGE ja_JP:ja
ENV LC_ALL ja_JP.UTF-8
COPY . /opt/src
WORKDIR /opt/src
RUN mkdir -p /root/dist \
    && stack --no-terminal --allow-different-user --local-bin-path=/root/dist install
RUN cp -r ./package.json ./tools /root/dist
USER rw-c

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
        git \
    && curl -sSL https://deb.nodesource.com/setup_14.x | bash \
    && apt-get -y -qq install --no-install-recommends nodejs \
    && apt-get -y -qq autoremove \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
RUN groupadd -g 61000 rw-c \
    && useradd -g 61000 -l -m -s /bin/bash -u 61000 rw-c
ENV LANG ja_JP.UTF-8
ENV LANGUAGE ja_JP:ja
ENV LC_ALL ja_JP.UTF-8
COPY --chown=rw-c:rw-c --from=bin /root/dist /home/rw-c/bin
WORKDIR /home/rw-c/bin
RUN npm i
ENV PATH $PATH:/home/rw-c/bin
