# Upload a File to the AMRZNN Dashboard Data Repository

Uploads a local file to the [AMRZNN dashboard data
repository](https://github.com/AMRZNN/dashboard_data) via the GitHub
Contents API. Creates the file if it does not yet exist, or overwrites
it if it does. No local git installation or repository clone is
required.

## Usage

``` r
amrznn_upload(
  local_file,
  repo_path,
  pat = gh::gh_token(),
  commit_message = paste("Automated upload:", Sys.Date()),
  repo = "AMRZNN/dashboard_data",
  verbose = TRUE
)
```

## Arguments

- local_file:

  `character` - Path to the local file to upload.

- repo_path:

  `character` - Target path within the repository, e.g.
  `"umcg/weekly_resistance.csv"`. Forward slashes only.

- pat:

  `character` - A GitHub Personal Access Token (PAT) with
  `Contents: Write` permission on the `AMRZNN/dashboard_data`
  repository. Defaults to the token returned by
  [`gh::gh_token()`](https://gh.r-lib.org/reference/gh_token.html).

- commit_message:

  `character` - Commit message to use. Defaults to
  `"Automated upload: <Sys.Date()>"`.

- repo:

  `character` - Target repository in `owner/repo` format. Defaults to
  `"AMRZNN/dashboard_data"` and should not normally be changed.

- verbose:

  `logical` - If `TRUE`, prints a confirmation message on success.
  Defaults to `TRUE`.

## Value

Invisibly returns the HTTP status code of the `PUT` response (`200L` for
update, `201L` for new file).

## Examples

``` r
if (FALSE) { # \dontrun{
amrznn_upload(
  local_file = "output/weekly_resistance.csv",
  repo_path  = "umcg/weekly_resistance.csv"
)
} # }
```
