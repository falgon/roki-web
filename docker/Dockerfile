# Base stage with common dependencies
FROM ubuntu:24.10 AS base
SHELL ["/bin/bash", "-c"]
LABEL maintainer="Roki <falgon53@yahoo.co.jp>"
LABEL org.opencontainers.image.authors="Roki <falgon53@yahoo.co.jp>"
ENV DEBIAN_FRONTEND=noninteractive \
    DEBCONF_NOWARNINGS=yes \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1

RUN sed -i 's@archive.ubuntu.com@ftp.jaist.ac.jp/pub/Linux@g' /etc/apt/sources.list \
    && apt-get -qq -o=Dpkg::Use-Pty=0 update \
    && apt-get -y -o=Dpkg::Use-Pty=0 -qq install --no-install-recommends \
        curl \
        ca-certificates \
        chromium \
        language-pack-ja \
        build-essential \
        libffi-dev \
        libffi8ubuntu1 \
        libgmp-dev \
        libgmp10 \
        libncurses-dev \
        libtinfo-dev \
        pkg-config \
        zlib1g \
        zlib1g-dev \
        git \
    && apt-get -y -qq autoremove \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && curl -sSLJ https://github.com/mikefarah/yq/releases/latest/download/yq_linux_amd64 -o \
        /usr/local/bin/yq \
    && chmod a+x /usr/local/bin/yq

ENV LANG=ja_JP.UTF-8 \
    LANGUAGE=ja_JP:ja \
    LC_ALL=ja_JP.UTF-8 \
    PUPPETEER_EXECUTABLE_PATH=/usr/bin/chromium

# Development stage
FROM base AS dev
LABEL org.opencontainers.image.description="Development image from the website roki.dev"

RUN groupadd -g 61000 rw-dev \
    && useradd -g 61000 -l -m -s /bin/bash -u 61000 rw-dev

USER rw-dev
RUN mkdir /home/rw-dev/src
COPY --chown=rw-dev:rw-dev . /home/rw-dev/src
WORKDIR /home/rw-dev/src

RUN curl -sSo- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.0/install.sh | bash \
    && export NVM_DIR="${HOME}/.nvm" \
    && [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" || true \
    && export NPM_CONFIG_UPDATE_NOTIFIER=false \
    && nvm install \
    && nvm use \
    && npm ci \
    && export BOOTSTRAP_HASKELL_GHC_VERSION="$(yq '.compiler' ./stack.yaml | cut -f2 -d-)" \
    && curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh \
    && source "$HOME/.ghcup/env" \
    && stack config set install-ghc false --global \
    && stack config set system-ghc true --global \
    && stack --no-terminal build --only-dependencies

# Build stage (builds the apps from source)
FROM base AS build
LABEL org.opencontainers.image.description="Build stage for the website roki.dev"

COPY . /opt/src
WORKDIR /opt/src

# Install GHCup and Stack dependencies
RUN export BOOTSTRAP_HASKELL_GHC_VERSION="$(yq '.compiler' ./stack.yaml | cut -f2 -d-)" \
    && export BOOTSTRAP_HASKELL_ADJUST_BASHRC=P \
    && export BOOTSTRAP_HASKELL_INSTALL_STACK=1 \
    && export BOOTSTRAP_HASKELL_INSTALL_HLS=0 \
    && export BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    && export PUPPETEER_EXECUTABLE_PATH="$(which chromium)" \
    && curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh \
    && source "$HOME/.ghcup/env" \
    && stack config set install-ghc false --global \
    && stack config set system-ghc true --global \
    && mkdir -p /root/dist \
    && stack --no-terminal --allow-different-user --local-bin-path=/root/dist install \
    && cp -r ./package.json ./package-lock.json ./tools ./.nvmrc /root/dist

# Final production stage (apps)
# Use Ubuntu instead of Alpine for better compatibility with Haskell binaries
FROM ubuntu:24.10 AS apps
SHELL ["/bin/bash", "-c"]
LABEL maintainer="Roki <falgon53@gmail.com>"
LABEL org.opencontainers.image.authors="Roki <falgon53@yahoo.co.jp>"
LABEL org.opencontainers.image.description="Production image from the website roki.dev"
ENV DEBIAN_FRONTEND=noninteractive \
    DEBCONF_NOWARNINGS=yes

RUN sed -i 's@archive.ubuntu.com@ftp.jaist.ac.jp/pub/Linux@g' /etc/apt/sources.list \
    && apt-get -qq -o=Dpkg::Use-Pty=0 update \
    && apt-get -y -o=Dpkg::Use-Pty=0 -qq install --no-install-recommends \
        curl \
        ca-certificates \
        language-pack-ja \
        git \
        libgmp10 \
        libffi8ubuntu1 \
        zlib1g \
    && apt-get -y -qq autoremove \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && groupadd -g 61000 rw-c \
    && useradd -g 61000 -l -m -s /bin/bash -u 61000 rw-c

USER rw-c
ENV LANG=ja_JP.UTF-8 \
    LANGUAGE=ja_JP:ja \
    LC_ALL=ja_JP.UTF-8

COPY --chown=rw-c:rw-c --from=build /root/dist /home/rw-c/bin
WORKDIR /home/rw-c/bin

# Install Node.js using nvm and create symlinks
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.0/install.sh | bash \
    && export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")" \
    && [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" || true \
    && export NPM_CONFIG_UPDATE_NOTIFIER=false \
    && nvm install \
    && nvm use \
    && npm ci \
    && NODE_VERSION=$(node -v | sed 's/v//') \
    && ln -s "$NVM_DIR/versions/node/v$NODE_VERSION/bin/node" /home/rw-c/bin/node \
    && ln -s "$NVM_DIR/versions/node/v$NODE_VERSION/bin/npm" /home/rw-c/bin/npm \
    && ln -s "$NVM_DIR/versions/node/v$NODE_VERSION/bin/npx" /home/rw-c/bin/npx

# Set up PATH and nvm environment
ENV NVM_DIR="/home/rw-c/.nvm"
ENV PATH="/home/rw-c/bin:$PATH"
EXPOSE 8888