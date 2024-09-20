# base stage
FROM ubuntu:24.04 AS base
ARG GROUP_ID
ARG USER_NAME
ARG USER_ID
ARG BOOTSTRAP_HASKELL_STACK_VERSION
ARG YQ_VERSION
SHELL ["/bin/bash", "-c", "-o", "pipefail"]
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
        pkg-config \
        zlib1g \
        zlib1g-dev \
    && apt-get -y -qq autoremove \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && curl -sSLJ "https://github.com/mikefarah/yq/releases/download/v${YQ_VERSION}/yq_linux_amd64" -o \
        /usr/local/bin/yq \
    && chmod a+x /usr/local/bin/yq
RUN groupadd -g "${GROUP_ID}" "${USER_NAME}" \
    && useradd -g "${GROUP_ID}" -l -m -s /bin/bash -u "${USER_ID}" "${USER_NAME}"
USER $USER_NAME
ENV LANG=ja_JP.UTF-8 \
    LANGUAGE=ja_JP:ja \
    LC_ALL=ja_JP.UTF-8 \
    PUPPETEER_EXECUTABLE_PATH=/usr/bin/chromium
RUN --mount=type=bind,source=./stack.yaml,dst=/tmp/stack.yaml \
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
        BOOTSTRAP_HASKELL_GHC_VERSION="$(yq '.compiler' /tmp/stack.yaml | cut -f2 -d-)" sh \
    && mkdir "$HOME/src"
ENV PATH=/home/rw/.ghcup/bin:$PATH \
    NPM_CONFIG_UPDATE_NOTIFIER=false
RUN stack config set install-ghc false --global \
    && stack config set system-ghc true --global

# development stage
FROM base AS dev
ARG NVM_VERSION
ARG USER_NAME
SHELL ["/bin/bash", "-c", "-o", "pipefail"]
LABEL maintainer="Roki <falgon53@gmail.com>"
LABEL org.opencontainers.image.authors="Roki <falgon53@yahoo.co.jp>"
LABEL org.opencontainers.image.description="Development image from the website roki.dev"
USER $USER_NAME
WORKDIR /home/rw/src
RUN --mount=type=bind,source=.,dst=. \
    curl -so- "https://raw.githubusercontent.com/nvm-sh/nvm/v${NVM_VERSION}/install.sh" | bash \
    && \. "$HOME/.nvm/nvm.sh" || true \
    && nvm install \
    && nvm use 
USER root
RUN --mount=type=bind,source=.,dst=.,rw \
    export HOME=/home/rw \
    && \. "$HOME/.nvm/nvm.sh" || true \
    && npm --silent ci \
    && stack --no-terminal --allow-different-user build --only-dependencies

# build app stage
FROM dev AS build
ARG USER_NAME
SHELL ["/bin/bash", "-c", "-o", "pipefail"]
USER root
WORKDIR /home/$USER_NAME/src
RUN --mount=type=bind,source=.,dst=.,rw \
    export DIST_DIR="/home/$USER_NAME/roki-web-apps" \
    && mkdir "$DIST_DIR" \
    && stack --no-terminal --allow-different-user --local-bin-path="$DIST_DIR" install \
    && cp -r ./package.json ./package-lock.json ./tools ./.nvmrc "$DIST_DIR"

# app stage
FROM ubuntu:24.04 AS apps
ARG GROUP_ID
ARG USER_NAME
ARG USER_ID
ARG NVM_VERSION
LABEL maintainer="Roki <falgon53@gmail.com>"
LABEL org.opencontainers.image.authors="Roki <falgon53@yahoo.co.jp>"
LABEL org.opencontainers.image.description="site/spa image from the website roki.dev"
SHELL ["/bin/bash", "-c", "-o", "pipefail"]
RUN groupadd -g "${GROUP_ID}" "${USER_NAME}" \
    && useradd -g "${GROUP_ID}" -l -m -s /bin/bash -u "${USER_ID}" "${USER_NAME}" \
    && mkdir "$HOME/roki-web-apps"
RUN sed -i 's@archive.ubuntu.com@ftp.jaist.ac.jp/pub/Linux@g' /etc/apt/sources.list \
    && apt-get -qq -o=Dpkg::Use-Pty=0 update \
    && apt-get -y -o=Dpkg::Use-Pty=0 -qq install --no-install-recommends \
        curl \
        ca-certificates \
        chromium \
        language-pack-ja \
        build-essential
ENV LANG=ja_JP.UTF-8 \
    LANGUAGE=ja_JP:ja \
    LC_ALL=ja_JP.UTF-8 \
    PUPPETEER_EXECUTABLE_PATH=/usr/bin/chromium
USER $USER_NAME
COPY --from=build /home/$USER_NAME/roki-web-apps /home/$USER_NAME/roki-web-apps
WORKDIR /home/$USER_NAME/roki-web-apps
ENV NPM_CONFIG_UPDATE_NOTIFIER=false
RUN curl -so- "https://raw.githubusercontent.com/nvm-sh/nvm/v${NVM_VERSION}/install.sh" | bash \
    && export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")" \
    && [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" || true \
    && nvm install -s \
    && nvm use \
    && cd "$HOME/roki-web-apps" \
    && npm --silent ci
ENV PATH=/home/rw/roki-web-apps:$PATH
