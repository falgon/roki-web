FROM ubuntu:24.04 as bin
SHELL ["/bin/bash", "-c"]
LABEL maintainer="Roki <falgon53@yahoo.co.jp>"
LABEL org.opencontainers.image.authors="Roki <falgon53@yahoo.co.jp>"
LABEL org.opencontainers.image.description="Preview image from the website roki.dev"
ENV DEBIAN_FRONTEND=noninteractive \
    DEBCONF_NOWARNINGS=yes \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN sed -i 's@archive.ubuntu.com@ftp.jaist.ac.jp/pub/Linux@g' /etc/apt/sources.list \
    && apt-get -qq -o=Dpkg::Use-Pty=0 update \
    && apt-get -y -o=Dpkg::Use-Pty=0 -qq install --no-install-recommends \
        curl \
        ca-certificates \
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
    && mkdir -p /opt/src \
    && curl -sSLJ https://github.com/mikefarah/yq/releases/latest/download/yq_linux_amd64 -o \
        /usr/local/bin/yq \
    && chmod a+x /usr/local/bin/yq
ENV LANG=ja_JP.UTF-8 \
    LANGUAGE=ja_JP:ja \
    LC_ALL=ja_JP.UTF-8
COPY . /opt/src
WORKDIR /opt/src
RUN export BOOTSTRAP_HASKELL_GHC_VERSION="$(yq '.compiler' ./stack.yaml | cut -f2 -d-)" \
    && curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN source "$HOME/.ghcup/env" \
    && stack config set install-ghc false --global \
    && stack config set system-ghc true --global \
    && mkdir -p /root/dist \
    && stack --no-terminal --allow-different-user --local-bin-path=/root/dist install \
    && cp -r ./package.json ./package-lock.json ./tools ./.nvmrc /root/dist

FROM ubuntu:24.04
SHELL ["/bin/bash", "-c"]
LABEL maintainer="Roki <falgon53@gmail.com>"
LABEL org.opencontainers.image.authors="Roki <falgon53@yahoo.co.jp>"
LABEL org.opencontainers.image.description="Preview image from the website roki.dev"
ENV DEBIAN_FRONTEND=noninteractive \
    DEBCONF_NOWARNINGS=yes
RUN sed -i 's@archive.ubuntu.com@ftp.jaist.ac.jp/pub/Linux@g' /etc/apt/sources.list \
    && apt-get -qq -o=Dpkg::Use-Pty=0 update \
    && apt-get -y -o=Dpkg::Use-Pty=0 -qq install --no-install-recommends \
        curl \
        ca-certificates \
        language-pack-ja \
        git \
    && apt-get -y -qq autoremove \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && groupadd -g 61000 rw-c \
    && useradd -g 61000 -l -m -s /bin/bash -u 61000 rw-c
USER rw-c
ENV LANG=ja_JP.UTF-8 \
    LANGUAGE=ja_JP:ja \
    LC_ALL=ja_JP.UTF-8
COPY --chown=rw-c:rw-c --from=bin /root/dist /home/rw-c/bin
WORKDIR /home/rw-c/bin
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.0/install.sh | bash \
    && export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")" \
    && [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" || true \
    && nvm install \
    && nvm use \
    && npm ci
ENV PATH $PATH:/home/rw-c/bin
