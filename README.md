# roki-web

![CI](https://github.com/falgon/roki-web/workflows/CI/badge.svg)
[![Known Vulnerabilities](https://snyk.io/test/github/falgon/roki-web/badge.svg?targetFile=package.json)](https://snyk.io/test/github/falgon/roki-web?targetFile=package.json)
[![CodeFactor](https://www.codefactor.io/repository/github/falgon/roki-web/badge?s=e4b1f45b3bb2dc89c42f654d991238ef7771bc9f)](https://www.codefactor.io/repository/github/falgon/roki-web)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/0f3e0d5c5bbe43a78eaeed7485b72c7f)](https://www.codacy.com?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=falgon/roki-web&amp;utm_campaign=Badge_Grade)
[![Maintainability](https://api.codeclimate.com/v1/badges/e6574c3042df63bf6d41/maintainability)](https://codeclimate.com/github/falgon/roki-web/maintainability)
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Ffalgon%2Froki-web.svg?type=shield)](https://app.fossa.com/projects/git%2Bgithub.com%2Ffalgon%2Froki-web?ref=badge_shield)

The roki's website and blog.

* Website: [roki.dev](https://roki.dev)
    * Tech blog: [roki.dev/roki.log](https://roki.dev/roki.log/)
    * Diary: [roki.dev/roki.diary](https://roki.dev/roki.diary/)

## Usage

```sh
$ stack exec site -- --help
Usage: site [--version] [--preview] [-v|--verbose] [--internal-links] COMMAND
  The static site roki.dev compiler version 0.1.0.0 powerted by Hakyll

Available options:
  -h,--help                Show this help text
  --version                Show version
  --preview                Enable a preview flag
  -v,--verbose             Run in verbose mode
  --internal-links         Check internal links only

Available commands:
  build                    Generate the site
  check                    Validate the site output
  clean                    Clean up and remove cache
  deploy                   Upload/deploy roki.dev
  rebuild                  Clean and build again
  server                   Start a preview server
  watch                    Autocompile on changes and start a preview server

$ stack exec site -- build --preview # fast build (This does not render KaTeX)
$ stack exec site -- build # release build
```

## System overview

![system overview](https://user-images.githubusercontent.com/1241783/90969880-d99b8a00-e538-11ea-8f35-684365e14406.png)

* [roki-web](https://github.com/falgon/roki-web) (this repository)
* [roki-web-post](https://github.com/falgon/roki-web-post) (private repository)

[GitHub Actions for GitHub pages](https://github.com/peaceiris/actions-gh-pages)
has been very helpful in building this system. 
Thanks for it.


## History

This is a project for a new website that integrates the following two sites.

* [Roki Profile](https://falgon.github.io/roki/)
    * Repo: <https://github.com/falgon/roki>
* [roki.log](https://falgon.github.io/roki.log/)
    * Repo: <https://bitbucket.org/r0ki/roki.log/> (private)
    * Repo (gh-page): <https://github.com/falgon/roki.log>

These two will be discontinued in the future and replaced by this new website and
have the following characteristics.

* The website [Roki Profile](https://falgon.github.io/roki/) is deployed manually (maintained by snyk-bot)
* The blog [roki.log](https://falgon.github.io/roki.log/) is deployed bitbucket-pipelines 

I used bitbucket, a kind of Git service that I can use private repositories for free, 
because github couldn't use private repositories for free before.
But times have changed and now private repositories are now available for free and 
native CI (GitHub Actions) are also available.
In this project, I use [Hakyll](https://jaspervdj.be/hakyll/), which is a static site generator, 
to manage the frontend library with node.js and have it managed with synk-bot. 
Deployment is done with github actions, drafts of blog posts are managed in another repository, 
and commits to a specific branch in that repository are triggered and pushed to this branch.
This makes it possible to make open source and keep drafts private, 
improving and integrating the previously separated management form.

## License

* [Bulma](https://bulma.io/): [MIT License](https://github.com/jgthms/bulma/blob/master/LICENSE)
* [D3.js](https://d3js.org/): [BSD 3-Clause "New" or "Revised" License](https://github.com/d3/d3/blob/master/LICENSE)
* [FontAwesome Free](https://github.com/FortAwesome/Font-Awesome)
    * Font Awesome Icons: [SIL Open Font Liscense](https://github.com/FortAwesome/Font-Awesome/blob/master/LICENSE.txt)
    * Font Awesome Code: [MIT License](https://github.com/FortAwesome/Font-Awesome/blob/master/LICENSE.txt)
* [KaTeX](https://github.com/Khan/KaTeX): [MIT License](https://github.com/Khan/KaTeX/blob/master/LICENSE.txt)
* [math.js](https://mathjs.org/): [Apache 2.0](https://github.com/josdejong/mathjs/blob/develop/LICENSE)

Licenses of this project is managed by [FOSSA](https://fossa.com/) (you can see above "license scan" badge).
For more details, you can see [the dependency report of FOSSA](https://app.fossa.com/projects/git%2Bgithub.com%2Ffalgon%2Froki-web?utm_source=share_link).
