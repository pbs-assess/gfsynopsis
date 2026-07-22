#!/usr/bin/env bash

set -Eeuo pipefail

project_name="${CLOUDFLARE_PAGES_PROJECT:-gfsynopsis}"
deploy_branch="${CLOUDFLARE_PAGES_BRANCH:-main}"
wrangler_version="${WRANGLER_VERSION:-4.113.0}"
assume_yes=false
dry_run=false

usage() {
  cat <<'EOF'
Usage: report/web/deploy.sh [--dry-run] [--yes]

Build, validate, and deploy the static species website to Cloudflare Pages.

  --dry-run  Build and validate without contacting Cloudflare or deploying.
  --yes      Skip the production deployment confirmation.
  -h, --help Show this help.

Optional environment variables:
  CLOUDFLARE_PAGES_PROJECT  Pages project name (default: gfsynopsis)
  CLOUDFLARE_PAGES_BRANCH   Deployment branch (default: main)
  CLOUDFLARE_ACCOUNT_ID     Cloudflare account to use, if you have more than one
  WRANGLER_VERSION          Wrangler version (default: 4.113.0)
EOF
}

while (($#)); do
  case "$1" in
    --dry-run)
      dry_run=true
      ;;
    --yes)
      assume_yes=true
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      printf 'Unknown option: %s\n\n' "$1" >&2
      usage >&2
      exit 2
      ;;
  esac
  shift
done

for command_name in Rscript node npx; do
  if ! command -v "$command_name" >/dev/null 2>&1; then
    printf 'Required command not found: %s\n' "$command_name" >&2
    exit 1
  fi
done

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/../.." && pwd)"
generated_dir="$script_dir/generated"

cd "$repo_root"

printf 'Building website...\n'
Rscript report/R/10-build-web.R

printf 'Validating generated JavaScript...\n'
node --check "$generated_dir/app.js"

if [[ ! -f "$generated_dir/index.html" || ! -f "$generated_dir/species.json" ]]; then
  printf 'Generated website is incomplete: %s\n' "$generated_dir" >&2
  exit 1
fi

if [[ "$dry_run" == true ]]; then
  printf 'Dry run complete; nothing was deployed.\n'
  exit 0
fi

printf 'Checking Cloudflare authentication...\n'
npx --yes "wrangler@${wrangler_version}" whoami

if [[ "$assume_yes" != true ]]; then
  if [[ ! -t 0 ]]; then
    printf 'Refusing an unconfirmed production deployment. Re-run with --yes.\n' >&2
    exit 1
  fi

  printf 'Deploy %s to Cloudflare Pages project %s (branch %s)? [y/N] ' \
    "$generated_dir" "$project_name" "$deploy_branch"
  read -r response
  case "$response" in
    y|Y|yes|YES|Yes)
      ;;
    *)
      printf 'Deployment cancelled.\n'
      exit 0
      ;;
  esac
fi

npx --yes "wrangler@${wrangler_version}" pages deploy "$generated_dir" \
  --project-name "$project_name" \
  --branch "$deploy_branch" \
  --commit-dirty=true

