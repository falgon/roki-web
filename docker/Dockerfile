# Base stage with common dependencies
FROM ubuntu:24.04 AS base
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

RUN --mount=type=cache,target=/home/rw-dev/.stack,uid=61000,gid=61000 \
    --mount=type=cache,target=/home/rw-dev/.ghcup,uid=61000,gid=61000 \
    curl -sSo- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.0/install.sh | bash \
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
    && stack update \
    && export STACK_YAML=stack.yaml \
    && echo "Starting Stack dependency build..." \
    && for i in {1..3}; do \
        echo "Attempt $i of 3..." && \
        timeout 3600 stack --no-terminal build --only-dependencies -j4 && \
        echo "Successfully built dependencies on attempt $i" && break || \
        (echo "Attempt $i failed, waiting 30 seconds before retry..." && sleep 30); \
       done || (echo "All attempts failed" && exit 1)

# Dependencies build stage (builds only dependencies, shared across builds)
FROM base AS deps
LABEL org.opencontainers.image.description="Dependencies stage for the website roki.dev"

# Copy only files needed for dependency resolution
COPY stack.yaml stack.yaml.lock package.yaml /opt/src/
COPY roki-web.cabal /opt/src/
WORKDIR /opt/src

# Install GHCup and build Stack dependencies only
RUN --mount=type=cache,target=/root/.stack \
    --mount=type=cache,target=/root/.ghcup \
    export BOOTSTRAP_HASKELL_GHC_VERSION="$(yq '.compiler' ./stack.yaml | cut -f2 -d-)" \
    && export BOOTSTRAP_HASKELL_ADJUST_BASHRC=P \
    && export BOOTSTRAP_HASKELL_INSTALL_STACK=1 \
    && export BOOTSTRAP_HASKELL_INSTALL_HLS=0 \
    && export BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    && curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh \
    && source "$HOME/.ghcup/env" \
    && stack config set install-ghc false --global \
    && stack config set system-ghc true --global \
    && stack update \
    && echo "Starting Stack dependency build..." \
    && for i in {1..3}; do \
        echo "Attempt $i of 3..." && \
        timeout 3600 stack --no-terminal --allow-different-user build --only-dependencies -j4 && \
        echo "Successfully built dependencies on attempt $i" && break || \
        (echo "Attempt $i failed, waiting 30 seconds before retry..." && sleep 30); \
       done || (echo "All attempts failed" && exit 1)

# Build stage (builds the apps from source, uses deps stage)
FROM deps AS build
LABEL org.opencontainers.image.description="Build stage for the website roki.dev"

# Now copy the full source
COPY . /opt/src
WORKDIR /opt/src

# Build the application (dependencies already built in deps stage)
RUN --mount=type=cache,target=/root/.stack \
    --mount=type=cache,target=/root/.ghcup \
    source "$HOME/.ghcup/env" \
    && mkdir -p /root/dist \
    && stack --no-terminal --allow-different-user --local-bin-path=/root/dist install -j4 \
    && cp -r ./package.json ./package-lock.json ./tools ./.nvmrc /root/dist

# Final production stage (apps)
# Use Ubuntu instead of Alpine for better compatibility with Haskell binaries
FROM ubuntu:24.04 AS apps
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
    && ln -sf "$NVM_DIR/versions/node/v$NODE_VERSION/bin/node" /home/rw-c/bin/node \
    && ln -sf "$NVM_DIR/versions/node/v$NODE_VERSION/bin/npm" /home/rw-c/bin/npm \
    && ln -sf "$NVM_DIR/versions/node/v$NODE_VERSION/bin/npx" /home/rw-c/bin/npx \
    && ln -sf "$NVM_DIR/versions/node/v$NODE_VERSION/bin/tsx" /home/rw-c/bin/tsx 2>/dev/null || true \
    && npm list -g tsx || npm install -g tsx

# Set up PATH and nvm environment
ENV NVM_DIR="/home/rw-c/.nvm"
ENV PATH="/home/rw-c/bin:/home/rw-c/.nvm/versions/node/v*/bin:$PATH"
EXPOSE 8888