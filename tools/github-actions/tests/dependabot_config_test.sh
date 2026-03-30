#!/usr/bin/env bash

set -euo pipefail

script_dir="$(
    cd "$(dirname "${BASH_SOURCE[0]}")" && pwd
)"
readonly script_dir
readonly config_path="${script_dir}/../../../.github/dependabot.yml"

ruby - "${config_path}" <<'RUBY'
require "yaml"

config_path = ARGV.fetch(0)
config = YAML.load_file(config_path)
updates = config.fetch("updates")

find_update = lambda do |ecosystem|
  updates.find { |update| update["package-ecosystem"] == ecosystem } ||
    raise("missing #{ecosystem} config")
end

assert_common_settings = lambda do |update, expected_directory:, expected_labels:|
  raise "#{update['package-ecosystem']} directory mismatch" unless update["directory"] == expected_directory
  raise "#{update['package-ecosystem']} target branch mismatch" unless update["target-branch"] == "develop"
  raise "#{update['package-ecosystem']} assignees mismatch" unless update["assignees"] == ["falgon"]
  raise "#{update['package-ecosystem']} labels mismatch" unless update["labels"] == expected_labels

  schedule = update.fetch("schedule")
  raise "#{update['package-ecosystem']} schedule interval mismatch" unless schedule["interval"] == "daily"
  raise "#{update['package-ecosystem']} schedule time mismatch" unless schedule["time"] == "09:00"
  raise "#{update['package-ecosystem']} schedule timezone mismatch" unless schedule["timezone"] == "Asia/Tokyo"

  separator = update.dig("pull-request-branch-name", "separator")
  raise "#{update['package-ecosystem']} branch separator mismatch" unless separator == "/"
end

npm = find_update.call("npm")
assert_common_settings.call(
  npm,
  expected_directory: "/",
  expected_labels: ["dependabot/npm", "dependencies", "automerge"],
)

github_actions = find_update.call("github-actions")
assert_common_settings.call(
  github_actions,
  expected_directory: "/",
  expected_labels: ["dependabot/github-actions", "dependencies", "automerge"],
)

docker = find_update.call("docker")
assert_common_settings.call(
  docker,
  expected_directory: "/docker",
  expected_labels: ["dependabot/docker", "dependencies", "automerge"],
)

puts "dependabot_config tests passed"
RUBY
