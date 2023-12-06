#!/usr/bin/env bash

set -euo pipefail

BASEDIR=$(dirname "$0")
NODE_ENV=production node --max-old-space-size=8192 $BASEDIR/katex.js "$@"
