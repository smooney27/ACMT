refresh_api_key <- function() {
  cat(file=stderr(), "Checking API key...")
  readRenviron("~/.Renviron")
  if (Sys.getenv("CENSUS_API_KEY") == "") {
    cat(file=stderr(), "Installing Census API key...")
    census_api_key('<Your key here>', install=T)
    cat(file=stderr(), "Installed Census API key")
  } else {
    cat(file=stderr(), "Census API key already installed")
  }
}
refresh_api_key()
