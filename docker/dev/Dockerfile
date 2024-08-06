FROM ubuntu:24.04
SHELL ["/bin/bash", "-c"]
LABEL maintainer="Roki <falgon53@yahoo.co.jp>"
LABEL org.opencontainers.image.authors="Roki <falgon53@yahoo.co.jp>"
LABEL org.opencontainers.image.description="Develop image from the website roki.dev"

ENV DEBIAN_FRONTEND=noninteractive \
    DEBCONF_NOWARNINGS=yes
RUN sed -i 's@archive.ubuntu.com@ftp.jaist.ac.jp/pub/Linux@g' /etc/apt/sources.list \
    && apt-get -qq -o=Dpkg::Use-Pty=0 update \
    && apt-get -y -qq -o=Dpkg::Use-Pty=0 install --no-install-recommends \
        build-essential \
        curl \
        ca-certificates \
        language-pack-ja \
        libtinfo-dev \
        git \
        libffi-dev \
        libffi8ubuntu1 \
        libgmp-dev \
        libgmp10 \
        libncurses-dev \
        pkg-config \
        zlib1g \
        zlib1g-dev \
    && curl -sSLJ https://github.com/mikefarah/yq/releases/latest/download/yq_linux_amd64 -o \
        /usr/local/bin/yq \
    && chmod a+x /usr/local/bin/yq \
    && apt-get -y -qq autoremove \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && groupadd -g 61000 rw-dev \
    && useradd -g 61000 -l -m -s /bin/bash -u 61000 rw-dev
USER rw-dev
RUN mkdir /home/rw-dev/src
ENV LANG=ja_JP.UTF-8 \
    LANGUAGE=ja_JP:ja \
    LC_ALL=ja_JP.UTF-8 \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1
COPY --chown=rw-dev:rw-dev . /home/rw-dev/src
WORKDIR /home/rw-dev/src
RUN curl -sSo- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.0/install.sh | bash \
    && export NVM_DIR="${HOME}/.nvm" \
    && [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" || true \
    && nvm install \
    && nvm use \
    && npm ci \
    && export BOOTSTRAP_HASKELL_GHC_VERSION="$(yq '.compiler' ./stack.yaml | cut -f2 -d-)" \
    && curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh \
    && source "$HOME/.ghcup/env" \
    && stack config set install-ghc false --global \
    && stack config set system-ghc true --global \
    && stack --no-terminal build --only-dependencies 
