# Authentication: Environics API Token Management System

# Create a private package environment to store credentials and tokens
.pkg_env <- new.env(parent = emptyenv())


#' Initialize Package Credentials
#'
#' Sets up OAuth credentials for the package. Call this once at the start
#' of your session or in your app initialization.
#'
#' @param client_id Character. OAuth client ID
#' @param client_secret Character. OAuth client secret
#' @param scope Character. OAuth scope (default: "mobilescapes")
#' @param token_url Character. Token endpoint URL
#'
#' @return Invisibly TRUE if credentials are valid, stops with error otherwise
#'
#' @examples
#' \dontrun{
#' init_credentials(
#'   client_id = Sys.getenv("CLIENT_ID"),
#'   client_secret = Sys.getenv("CLIENT_SECRET")
#' )
#' }
#'
#' @export
init_credentials <- function(
    client_id = Sys.getenv("CLIENT_ID"),
    client_secret = Sys.getenv("CLIENT_SECRET"),
    scope = Sys.getenv("SCOPE", "mobilescapes"),
    token_url = "https://login.environicsanalytics.com/connect/token"
    ) {

  # Validate credentials are loaded
  if (client_id == "" || client_secret == "") {
    stop(
      "ERROR: CLIENT_ID CLIENT_SECRET, and SCOPE should be loaded as environment
       variables, or provided as arguments to the function"
    )
  }

  # Verify credentials are valid by attempting to get a token
  cat("Verifying credentials...\n")
  tryCatch(
    {
      token <- .get_bearer_token_internal(
        client_id = client_id,
        client_secret = client_secret,
        scope = scope,
        token_url = token_url
      )

      # Store credentials and settings in package environment
      .pkg_env$client_id <- client_id
      .pkg_env$client_secret <- client_secret
      .pkg_env$scope <- scope
      .pkg_env$token_url <- token_url
      .pkg_env$token <- token
      .pkg_env$token_obtained_at <- Sys.time()
      .pkg_env$token_expires_in <- NULL  # Will be set by get_bearer_token

      cat("Credentials verified and initialized successfully!\n")
      invisible(TRUE)
    },
    error = function(e) {
      stop("Failed to verify credentials: ", conditionMessage(e))
    }
  )
}

#' Check if Credentials are Initialized
#'
#' @return Logical. TRUE if credentials are set up and valid
#'
#' @keywords internal
.is_initialized <- function() {
  !is.null(.pkg_env$client_id) && !is.null(.pkg_env$client_secret)
}

#' Get Credentials with Validation
#'
#' Internal function to retrieve stored credentials. Throws error if not
#' initialized.
#'
#' @return List with client_id, client_secret, scope, token_url, api_base_url
#'
#' @keywords internal
.get_credentials <- function() {
  if (!.is_initialized()) {
    stop(
      "Credentials not initialized. Call init_credentials() first.",
      call. = FALSE
    )
  }

  list(
    client_id = .pkg_env$client_id,
    client_secret = .pkg_env$client_secret,
    scope = .pkg_env$scope,
    token_url = .pkg_env$token_url
  )
}


#' Get Bearer Token (Internal)
#'
#' Internal function that performs the actual token request.
#'
#' @param client_id Character. OAuth client ID
#' @param client_secret Character. OAuth client secret
#' @param scope Character. OAuth scope
#' @param token_url Character. Token endpoint URL
#'
#' @return Character. Bearer token string
#'
#' @keywords internal
.get_bearer_token_internal <- function(client_id, client_secret, scope, token_url) {

  req <- httr2::request(token_url) |>
    httr2::req_body_form(
      grant_type = "client_credentials",
      client_id = client_id,
      client_secret = client_secret,
      scope = scope
    ) |>
    httr2::req_error(body = function(resp) {
      paste("Authentication failed:", httr2::resp_body_string(resp))
    })

  resp <- httr2::req_perform(req)
  result <- httr2::resp_body_json(resp)

  return(result$access_token)
}

#' Get Valid Bearer Token Quietly
#'
#' Returns a valid bearer token, refreshing if necessary, without verbosity.
#'
#' @return Character. Valid bearer token string
#'
#' @examples
#' \dontrun{
#' init_credentials(
#'   client_id = Sys.getenv("CLIENT_ID"),
#'   client_secret = Sys.getenv("CLIENT_SECRET")
#' )
#' token <- get_bearer_token()
#' }
#'
#' @export
.quietly_get_bearer_token <- function() {
  creds <- .get_credentials()

  # Fetch new token
  token <- .get_bearer_token_internal(
    client_id = creds$client_id,
    client_secret = creds$client_secret,
    scope = creds$scope,
    token_url = creds$token_url
  )

  .pkg_env$token <- token
  return(token)
}

#' Get Valid Bearer Token
#'
#' Returns a valid bearer token, refreshing if necessary. This is the main
#' function to use when making API calls.
#'
#' @return Character. Valid bearer token string
#'
#' @examples
#' \dontrun{
#' init_credentials(
#'   client_id = Sys.getenv("CLIENT_ID"),
#'   client_secret = Sys.getenv("CLIENT_SECRET")
#' )
#' token <- get_bearer_token()
#' }
#'
#' @export
get_bearer_token <- function() {
  creds <- .get_credentials()

  cat("Requesting Bearer token...\n")

  # Fetch new token
  token <- .get_bearer_token_internal(
    client_id = creds$client_id,
    client_secret = creds$client_secret,
    scope = creds$scope,
    token_url = creds$token_url
  )

  .pkg_env$token <- token
  return(token)
}

#' Clear Stored Credentials
#'
#' Removes all stored credentials and tokens from memory. Useful for
#' testing or switching credentials.
#'
#' @return Invisibly TRUE
#'
#' @examples
#' \dontrun{
#' clear_credentials()
#' }
#'
#' @export
clear_credentials <- function() {
  rm(list = ls(envir = .pkg_env), envir = .pkg_env)
  cat("Credentials cleared.\n")
  invisible(TRUE)
}
