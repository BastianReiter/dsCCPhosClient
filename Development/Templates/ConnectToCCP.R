

# Load needed packages
library(dsBaseClient)
library(dsCCPhosClient)
library(DSI)
library(DSOpal)
library(tibble)


# Initiate tibble that holds credentials of participating CCP sites
CCPSiteCredentials <- tibble(SiteName = character(),
                             URL = character(),
                             Token = character())

# Add site "Sissy"
CCPSiteCredentials <- add_row(CCPSiteCredentials,
                              SiteName = "Sissy",
                              URL = "https://dktk-datashield-test/opal/",
                              Token = "625732d7-36d5-44d1-9550-7e7899e594cc")

# Add site "Franz"
CCPSiteCredentials <- add_row(CCPSiteCredentials,
                              SiteName = "Franz",
                              URL = "https://dktk-test/opal/",
                              Token = "73a4e20f-3474-4aa0-8696-6a485bda2242")


CCPConnections <- ds.ConnectToCCP(CCPSiteCredentials)

