# Interactive species pages

The files in this directory are the tracked source files for the static species
website. The deployable site is created in the ignored `generated/` directory.

## Production site

The Cloudflare Pages Direct Upload project is available at
<https://gfsynopsis.pages.dev/>. For example, the Pacific Cod page is
<https://gfsynopsis.pages.dev/?species=pacific-cod>.

The project is temporarily owned through a personal Cloudflare account. Move
or recreate it under an organizational account and add another maintainer when
that account is available.

## Deploying to Cloudflare Pages

The tracked deployment script contains no credentials or Cloudflare account
identifier, so it is safe to commit and push to GitHub. Wrangler stores its
OAuth login outside this repository. Never add an API token or password to the
script.

Sign in once on a new computer with the pinned Wrangler version:

```sh
npx --yes wrangler@4.113.0 login
```

Then build, validate, review the account shown by Wrangler, and confirm the
production deployment with:

```sh
./report/web/deploy.sh
```

To build and validate without contacting Cloudflare or deploying anything:

```sh
./report/web/deploy.sh --dry-run
```

For a non-interactive deployment, use `--yes`. If Wrangler has access to more
than one account, set `CLOUDFLARE_ACCOUNT_ID` for that command; an account ID is
not a secret, but it does not need to be committed.

The script deploys to the `gfsynopsis` Pages project on the `main` branch. Its
project, branch, and pinned Wrangler version can be overridden with the
environment variables shown by `./report/web/deploy.sh --help`.

## Building and previewing locally

From the repository root, build the web data and copy the current synopsis
PNGs with:

```sh
Rscript report/R/10-build-web.R
```

The build validates the species metadata and images before replacing
`report/web/generated/`, then copies the HTML, CSS, JavaScript, and Cloudflare
headers into that directory.

Preview the generated site locally with:

```sh
python3 -m http.server 8000 --directory report/web/generated
```

Then open `http://localhost:8000/`. A direct species link looks like
`http://localhost:8000/?species=pacific-cod`.

## Local verification

After each metadata, frontend, or figure update, verify at least one ordinary
species and the exceptional species links below:

- `?species=pacific-cod`
- `?species=north-pacific-spiny-dogfish`
- `?species=rougheye-blackspotted-rockfish-complex`
- `?species=pacific-hake`
- `?species=sablefish`

Confirm that refresh and browser back/forward navigation preserve the selected
species, only its two figure files are requested, and the browser console has
no errors. Check a wide desktop viewport as well as widths near 768 px and
360 px; neither the controls, metadata, nor figures should cause horizontal
scrolling. Test filtering and selection in the labelled species combobox with
both the keyboard and a pointer.

Do not commit `generated/`; it contains approximately 109 MB of derived PNGs.
