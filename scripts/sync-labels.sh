#!/usr/bin/env bash
# Create or update the labels in .github/labels.yml. Needs gh and python3 with PyYAML.
set -euo pipefail
rows=$(mktemp)
trap 'rm -f "$rows"' EXIT
python3 - <<'PY' >"$rows"
import yaml
for label in yaml.safe_load(open(".github/labels.yml", encoding="utf-8")):
    print(f"{label['name']}\t{label['color']}\t{label['description']}")
PY
while IFS=$'\t' read -r name color description; do
  gh label create "$name" --color "$color" --description "$description" --force
done <"$rows"
