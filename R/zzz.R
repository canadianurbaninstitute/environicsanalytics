# Package Load Hooks

#' Load Project-Level .Renviron on Package Load
#'
#' Runs automatically when the package is loaded (via `library()` or `::`).
#' R's own startup sequence only reads a project-level `.Renviron` if the
#' working directory was already the project root when the R process itself
#' started - it won't pick up a `.Renviron` created after startup, or one in
#' a directory reached only via a later `setwd()`. Re-reading it here ties
#' credential loading to "the package is in use" rather than to how or where
#' R happened to be launched.
#'
#' Silent if no `.Renviron` is present in the working directory at load time
#' - that's the normal case for a `~/.Renviron`, explicit `Sys.setenv()`, or
#' credentials passed directly to `init_credentials()`. Never errors: a
#' broken `.Renviron` would otherwise break loading the package entirely.
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  renviron_path <- ".Renviron"

  if (file.exists(renviron_path)) {
    tryCatch(
      {
        readRenviron(renviron_path)
        packageStartupMessage(
          "environicsanalytics: loaded ", normalizePath(renviron_path)
        )
      },
      error = function(e) {
        warning(
          "environicsanalytics: found .Renviron but failed to load it: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  invisible(NULL)
}
