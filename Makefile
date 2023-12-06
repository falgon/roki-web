SHELL=/bin/bash

init:
	@nvm install
	@nvm use
	@npm i
	@stack build

watch:
	@stack build
	@stack exec site -- build --preview
	@stack exec site -- check --internal-links
	@stack exec site -- watch --preview

watch-prebuild:
	@cat ${HOME}/.ghcr.txt | docker login docker.pkg.github.com -u falgon --password-stdin
	@docker pull docker.pkg.github.com/falgon/roki-web/roki-web-env:latest
	@pushd ./docker \
		&& docker-compose --compatibility -f docker-compose-ghpr.yml up -d preview \
		&& docker-compose -f docker-compose-ghpr.yml logs -f preview \
		; popd

stop-watch-prebuild:
	@pushd ./docker \
		&& docker-compose --compatibility -f docker-compose-ghpr.yml stop preview \
		; popd

create-pr-master-develop:
	@gh pr create -t "WIP master <- develop" \
		-a @me \
		-l automerge -l dependencies \
		-B master \
		-b "Merge the develop branch into the master branch and deploy" \
		-H develop

.PHONY: init watch watch-prebuild stop-watch-prebuild create-pr-master-develop
