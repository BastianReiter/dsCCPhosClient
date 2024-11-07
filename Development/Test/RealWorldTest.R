
install.packages("devtools")

# Install CCPhos R packages
devtools::install_github(repo = "BastianReiter/dsCCPhos")
devtools::install_github(repo = "BastianReiter/dsCCPhosClient")
#devtools::install_github(repo = "BastianReiter/CCPhosApp")

library(dplyr)
library(dsCCPhosClient)
library(DSI)
library(CCPhosApp)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# USING CCPHOS APP (look for manual approach below)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

StartCCPhosApp()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MANUAL APPROACH (without app)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Read in CCP site specifications from uploaded file (first upload 'SiteSpecs.csv' using RStudio) ...
Credentials <- read.csv(file = "SiteSpecs.csv")

# ... or enter specifications manually
# Credentials <- data.frame(SiteName = c("Sissy", "Franz", "Mannheim"),
#                           URL = c("https://dktk-datashield-test/opal/", "https://dktk-test/opal/", "https://mannheim/opal/"),
#                           Token = c("xxx", "xxx", "xxx"))

# Filtering for sites that work
Credentials <- Credentials %>%
  filter(SiteName %in% c(# "Sissi",      # Altes Datenmodell
    # "Franz"      # Not available
    # "Berlin",      # Altes Datenmodell
    # "Dresden",      # Altes Datenmodell
    # "Mainz",      # Altes Datenmodell
    # "Mannheim",      # Instabil, Connection möglich, danach nicht testbar
    # "MunichLMU"      # Altes Datenmodell
    # "MunichTU"      # Instabil, Connection möglich, danach nicht testbar
    # "Essen",      # Altes Datenmodell
    # "Freiburg",      # Kein Export?
    # "Ulm",      # Not available
    "Wuerzburg",
    "Hannover"
  ))

# Establish connection to servers using convenience funtion 'ConnectToCCP'
CCPConnections <- ConnectToCCP(CCPSiteSpecifications = Credentials)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check server requirements using dsCCPhosClient::CheckServerRequirements()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ServerRequirements <- CheckServerRequirements(DataSources = CCPConnections)


Test <- GetServerOpalInfo(CCPSiteSpecifications = Credentials,
                          DataSources = CCPConnections)

# Get manual overview over available Opal tables
DSI::datashield.tables(conns = CCPConnections)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Raw Data Set (RDS) from Opal data base to R sessions on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Messages <- LoadRawDataSet(CCPSiteSpecifications = Credentials,
                           DataSources = CCPConnections)


# Collect comprehensive information about all workspace objects
ServerWorkspaceInfo <- GetServerWorkspaceInfo(DataSources = CCPConnections)

ds.names(xname = "RDS_Diagnosis", datasources = CCPConnections)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check RDS tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSTableCheck <- ds.CheckRDSTables(DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get random samples from Raw Data Set on servers for easier testing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ds.DrawSample(RawDataSetName = "RawDataSet",
#               SampleSize = "2000")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use dsCCPhos functionality to process data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ds.CurateData(DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Log out from all servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DSI::datashield.logout(CCPConnections)
