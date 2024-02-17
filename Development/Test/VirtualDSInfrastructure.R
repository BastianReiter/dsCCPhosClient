

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

library(dsBase)
library(dsBaseClient)
library(dsCCPhos)
library(dsCCPhosClient)
library(DSLite)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Establish Connections to virtual servers using dsCCPhosClient::ConnectToVirtualCCP()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("../dsCCPhos/Development/Data/TestData/CCPTestData_Total.RData")

CCPConnections <- ConnectToVirtualCCP(CCPTestData = CCPTestData_Total,
                                      NumberOfSites = 3,
                                      NumberOfPatientsPerSite = 300)


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
# Applying dsCCPhos functionality
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Get validation report of Raw Data Set (RDS)
# ValidationReportRDS <- ds.GetValidationReport_RDS(Name_RawDataSet = "RawDataSet",
#                                                       DataSources = CCPConnections)


CCPhosApp::StartCCPhosApp(CCPConnections = CCPConnections,
                          CCPhosData = NULL)


# Transform Raw Data Set (RDS) into Curated Data Set (CDS)
dsCCPhosClient::ds.CurateData(Name_RawDataSet = "RawDataSet",
                              Name_Output = "CurationOutput",
                              DataSources = CCPConnections)


# Get Curation reports
CurationReport <- dsCCPhosClient::ds.GetCurationReport(Name_CurationOutput = "CurationOutput",
                                                       DataSources = CCPConnections)

# Exemplary look at a curation report table
View(CurationReports$SiteA$Monitor_Diagnosis)

# Make html file displaying tables from curation report
dsCCPhosClient::MakeCurationReport(CurationReportData = CurationReport,
                                   PathToReportTemplate = "./Development/Reporting/CurationReport.qmd")

# # Save for easier testing of CCPhosApp
# saveRDS(object = CurationReport,
#         file = "CurationReport.rds")


# Use CCPhosApp to display curation reports
CCPhosApp::StartCCPhosApp(CCPhosData = CurationReport)



# Make tables from Curated Data Set directly addressable by unpacking them into R server session
dsCCPhosClient::ds.UnpackCuratedDataSet(Name_CurationOutput = "CurationOutput",
                                        DataSources = CCPConnections)


# List all objects in server-sided R sessions
DSI::datashield.symbols(conns = CCPConnections)




# Get validation report of Curated Data Set (CDS)
# ValidationReportCDS <- ds.GetValidationReport_CDS(Name_CurationOutput = "CurationOutput",
#                                                   DataSources = CCPConnections)



# Try out data augmentation method
dsCCPhosClient::ds.AugmentData(Name_CurationOutput = "CurationOutput",
                               Name_Output = "AugmentationOutput",
                               DataSources = CCPConnections)


# Make tables from Augmented Data Set directly addressable by unpacking them into R server session
dsCCPhosClient::ds.UnpackAugmentedDataSet(Name_AugmentationOutput = "AugmentationOutput",
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
                        FillPalette = c("All" = Colors$MediumGrey,
                                        "SiteA" = Colors$Primary,
                                        "SiteB" = Colors$Secondary,
                                        "SiteC" = Colors$Tertiary))









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
