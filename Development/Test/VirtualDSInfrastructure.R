

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Virtual dataSHIELD infrastructure for testing purposes -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Install own dataSHIELD packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#devtools::install_github(repo = "BastianReiter/dsCCPhos")
#devtools::install_github(repo = "BastianReiter/dsCCPhosClient")
#devtools::install_github(repo = "BastianReiter/CCPhosApp")

# Install additional datashield-packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#devtools::install_github("tombisho/dsSynthetic", dependencies = TRUE)
#devtools::install_github("tombisho/dsSyntheticClient", dependencies = TRUE)

#devtools::install_github("neelsoumya/dsSurvival")
#devtools::install_github("neelsoumya/dsSurvivalClient")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load required packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library(dsBaseClient)
library(dsCCPhosClient)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Establish Connections to virtual servers using dsCCPhosClient::ConnectToVirtualCCP()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TestData <- readRDS("../dsCCPhos/Development/Data/RealData/CCPRealData_Frankfurt.rds")

CCPConnections <- ConnectToVirtualCCP(CCPTestData = TestData,
                                      NumberOfSites = 3,
                                      NumberOfPatientsPerSite = 1000)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check server requirements using dsCCPhosClient::CheckServerRequirements()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Messages_ServerRequirements <- CheckServerRequirements(DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Raw Data Set (RDS) from Opal data base to R sessions on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Messages_Loading <- LoadRawDataSet(DataSources = CCPConnections,
                                   ProjectName = "Virtual")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check out objects in server workspaces
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GetServerWorkspaceInfo(DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Applying dsCCPhos functionality
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Get validation report of Raw Data Set (RDS)
# ValidationReportRDS <- ds.GetValidationReport_RDS(Name_RawDataSet = "RawDataSet",
#                                                       DataSources = CCPConnections)


#CCPhosApp::StartCCPhosApp(CCPConnections = CCPConnections)


# Transform Raw Data Set (RDS) into Curated Data Set (CDS)
Messages_DataCuration <- dsCCPhosClient::ds.CurateData(RawDataSetName = "RawDataSet",
                                                       OutputName = "CurationOutput",
                                                       DataSources = CCPConnections)

# Get Curation reports
CurationReports <- dsCCPhosClient::ds.GetCurationReport(DataSources = CCPConnections)

# Exemplary look at a curation report table
View(CurationReports$All$Transformation$Staging)

# Make html file displaying tables from curation report
# dsCCPhosClient::MakeCurationReport(CurationReportData = CurationReports$All,
#                                    PathToReportTemplate = "./Development/Reporting/CurationReport.qmd")


# Make tables from Curated Data Set directly addressable by unpacking them into R server session
Messages_UnpackingCDS <- dsCCPhosClient::ds.UnpackCuratedDataSet(CuratedDataSetName = "CuratedDataSet",
                                                                 DataSources = CCPConnections)



# Get validation report of Curated Data Set (CDS)
# ValidationReportCDS <- ds.GetValidationReport_CDS(Name_CurationOutput = "CurationOutput",
#                                                   DataSources = CCPConnections)



# Try out data augmentation method
dsCCPhosClient::ds.AugmentData(CuratedDataSetName = "CuratedDataSet",
                               OutputName = "AugmentationOutput",
                               DataSources = CCPConnections)


# Make tables from Augmented Data Set directly addressable by unpacking them into R server session
Messages_UnpackingADS <- dsCCPhosClient::ds.UnpackAugmentedDataSet(AugmentedDataSetName = "AugmentedDataSet",
                                                                   DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform exemplary analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ds.colnames(x = "ADS_Patients",
            datasources = CCPConnections)


SampleStatistics <- ds.GetSampleStatistics(TableName = "ADS_Patients",
                                           MetricFeatureName = "PatientAgeAtDiagnosis",
                                           DataSources = CCPConnections)


TestPlot <- MakeBoxPlot(SampleStatistics = SampleStatistics,
                        AxisTitle_y = "Patient age at diagnosis",
                        FillPalette = c("All" = CCPhosColors$MediumGrey,
                                        "SiteA" = CCPhosColors$Primary,
                                        "SiteB" = CCPhosColors$Secondary,
                                        "SiteC" = CCPhosColors$Tertiary))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Log out from all servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DSI::datashield.logout(CCPConnections)






# Generate synthetic data using package dsSynthetic (which in turn makes use of packages synthpop and simstudy)

# library(dsSyntheticClient)
#
# SyntheticData <- ds.syn(data = "Diagnosis")
#
#                         method = "cart",
#                         m = 1,
#                         seed = 123)
#
# SyntheticData <- SyntheticData$SiteTotal$Warning
#
#
# SyntheticData



#OpalDB_A <- dsCCPhos::MakeTestDB(CCPTestData_A)
#res_CCPTestData_A <- resourcer::newResource(name = "CCPTest")
#resourcer::PostgresResourceConnector$new()

# res_CCPTestData_A <- resourcer::newResource(name = "CCPTestData",
#                                             url = "file://./Development/Data/RealData/CCPTestData_A.RData",
#                                             format = "list")

#Test <- resourcer::FileResourceGetter$new()
