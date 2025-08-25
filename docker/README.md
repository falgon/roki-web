<h1 align="center">Docker usage</h1>
<p align="center"><a href="https://github.com/falgon/roki-web/actions/workflows/manually-gh-registry.yml"><img alt="manually-gh-registry" src="https://github.com/falgon/roki-web/actions/workflows/manually-gh-registry.yml/badge.svg?branch=master"></a></p>

## Multi-Architecture Support

Docker images are built for both `linux/amd64` (Intel/AMD) and `linux/arm64` (Apple Silicon) platforms. The appropriate architecture will be automatically selected when pulling images.

### Platform Selection

You can specify the platform using the `DOCKER_DEFAULT_PLATFORM` environment variable:

```sh
# Use ARM64 image on Apple Silicon Mac (default is amd64)
export DOCKER_DEFAULT_PLATFORM=linux/arm64

# Use AMD64 image explicitly
export DOCKER_DEFAULT_PLATFORM=linux/amd64
```

## Usage

Update blog posts using docker container

```sh
# start a preview server
pushd ./docker \
    && docker compose up -d preview \
    && COMPOSE_HTTP_TIMEOUT=86400 docker compose logs -f preview \
    ; popd

# build blog posts
pushd ./docker && docker compose run build; popd

# clean the generated docs
pushd ./docker && docker compose run clean; popd

# Reservation posting
DATE=$(date "+%m-%d-%R") BRANCH_NAME="hoge" pushd ./docker && docker compose run spa; popd
```

When using a pre-built image (Requires PAT with `read:packages` permission)

```sh
# Login to GitHub Package Registry
echo $GITHUB_TOKEN | docker login docker.pkg.github.com -u <username> --password-stdin

# Pull the image (automatically selects appropriate architecture)
docker pull docker.pkg.github.com/falgon/roki-web/roki-web-env:latest

# Set image tag (optional, defaults to 'latest')
export ROKI_WEB_ENV_IMAGE_TAG=latest

# For Apple Silicon Mac, optionally set platform
export DOCKER_DEFAULT_PLATFORM=linux/arm64

# start a preview server
pushd ./docker \
    && docker compose -f docker-compose-ghcr.yml up -d preview \
    && COMPOSE_HTTP_TIMEOUT=86400 docker compose -f docker-compose-ghcr.yml logs -f preview \
    ; popd

# build blog posts
pushd ./docker && docker compose -f docker-compose-ghcr.yml run build; popd

# clean the generated docs
pushd ./docker && docker compose -f docker-compose-ghcr.yml run clean; popd

# Reservation posting
DATE=$(date "+%m-%d-%R") BRANCH_NAME="hoge" pushd ./docker \
    && docker compose -f docker-compose-ghcr.yml run spa \
    ; popd
```

Develop/build inside docker container

```sh
pushd ./docker && docker compose up -d dev && popd
docker exec -it roki-web-dev bash
```

When using a pre-built image (Requires PAT with `read:packages` permission)

```sh
# Login to GitHub Package Registry
echo $GITHUB_TOKEN | docker login docker.pkg.github.com -u <username> --password-stdin

# Pull the dev image
docker pull docker.pkg.github.com/falgon/roki-web/roki-web-dev:latest

# Set image tag and platform (optional)
export ROKI_WEB_DEV_IMAGE_TAG=latest
export DOCKER_DEFAULT_PLATFORM=linux/arm64  # For Apple Silicon Mac

# Start dev container
pushd ./docker && docker compose -f docker-compose-ghcr.yml up -d dev; popd
docker exec -it roki-web-dev-ghcr bash
```
