<h1 align="center">Docker usage</h1>
<p align="center"><a href="https://github.com/falgon/roki-web/actions/workflows/manually-gh-registry.yml"><img alt="manually-gh-registry" src="https://github.com/falgon/roki-web/actions/workflows/manually-gh-registry.yml/badge.svg?branch=master"></a></p>


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
cat ~/.ghcr.txt | docker login ghcr.io -u falgon --password-stdin
docker pull docker.pkg.github.com/falgon/roki-web/roki-web-env:latest

# start a preview server
pushd ./docker \
    && docker compose -f docker-compose-ghpr.yml up -d preview \
    && COMPOSE_HTTP_TIMEOUT=86400 docker compose -f docker-compose-ghpr.yml logs -f preview \
    ; popd

# build blog posts
pushd ./docker && docker compose -f docker-compose-ghpr.yml run build; popd

# clean the generated docs
pushd ./docker && docker compose -f docker-compose-ghpr.yml run clean; popd

# Reservation posting
DATE=$(date "+%m-%d-%R") BRANCH_NAME="hoge" pushd ./docker \
    && docker compose -f docker-compose-ghpr.yml run spa \
    ; popd
```

Develop/build inside docker container

```sh
pushd ./docker && docker compose up -d dev && popd
docker exec -it roki-web-dev bash
```

When using a pre-built image (Requires PAT with `read:packages` permission)

```sh
cat ~/.ghcr.txt | docker login ghcr.io -u falgon --password-stdin
docker pull docker.pkg.github.com/falgon/roki-web/roki-web-dev:latest
pushd ./docker && docker compose -f docker-compose-ghpr.yml up -d dev; popd
docker exec -it roki-web-dev bash
```
