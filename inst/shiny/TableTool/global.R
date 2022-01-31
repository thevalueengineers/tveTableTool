library(AzureAuth)

tenant <- Sys.getenv("TENANT", "")
app <- Sys.getenv("APP_ID", "")
secret <- Sys.getenv("APP_SECRET", "")
if(secret == "") secret <- NULL


# the Azure resource permissions needed
# if your app doesn't use any Azure resources (you only want to do authentication),
# set the resource to "openid" only
resource <- c(
  # "https://management.azure.com/.default",
  "openid"
)

# set this to the site URL of your app once it is deployed
# this must also be the redirect for your registered app in Azure Active Directory
redirect <- "https://thevalueengineers.shinyapps.io/TableTool/"
# redirect <- "http://localhost:8100"

port <- httr::parse_url(redirect)$port
options(shiny.port=if(is.null(port)) 80 else as.numeric(port))


# code for cleaning url after authentication
clean_url_js <- sprintf(
  "
    $(document).ready(function(event) {
      const nextURL = '%s';
      const nextTitle = 'My new page title';
      const nextState = { additionalInformation: 'Updated the URL with JS' };
      // This will create a new entry in the browser's history, without reloading
      window.history.pushState(nextState, nextTitle, nextURL);
    });
    ", redirect
)
