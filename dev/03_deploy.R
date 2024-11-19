######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

## Build documentation ----
devtools::document()

## Run checks ----
devtools::check()

## Docker ----
## create renv.lock file and Dockerfile
golem::add_dockerfile_with_renv(output_dir = "deploy", document = FALSE, open = FALSE)

# Deploy to Posit Connect
# In command line.
options(rsconnect.packrat = TRUE) # https://github.com/ThinkR-open/golem/issues/1098

rsconnect::deployApp(
  account = Sys.getenv("POSIT_CONNECT_ACCOUNT"),
  appFiles = c(
    # Add any additional files unique to your app here.
    "R/",
    "inst/",
    "data/",
    "NAMESPACE",
    "DESCRIPTION",
    "app.R"
  ),
  appId = rsconnect::deployments(".")$appID,
  lint = FALSE,
  forceUpdate = TRUE
)

# Deploy to ShinyApps.io
# In command line.
options(rsconnect.packrat = TRUE) # https://github.com/ThinkR-open/golem/issues/1098

rsconnect::deployApp(
  account = Sys.getenv("SHINYAPPS_IO_ACCOUNT"),
  appFiles = c(
    # Add any additional files unique to your app here.
    "R/",
    "inst/",
    "data/",
    "NAMESPACE",
    "DESCRIPTION",
    "app.R"
  ),
  appId = rsconnect::deployments(".")$appID,
  lint = FALSE,
  forceUpdate = TRUE
)
