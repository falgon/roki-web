#!/bin/bash
# CI will fail on npm ci without proper package-lock.json
# This script would help generate it if we could run it
echo "This PR needs a proper package-lock.json generated with:"
echo "npm install"
echo ""
echo "The following packages were updated to v4.0.4:"
echo "- @vitest/coverage-v8"
echo "- @vitest/ui"
echo "- vitest"