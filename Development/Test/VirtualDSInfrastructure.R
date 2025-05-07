
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

library(dsBaseClient)
library(dsCCPhosClient)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Establish Connections to virtual servers using dsCCPhosClient::ConnectToVirtualCCP()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#TestData <- readRDS("../dsCCPhos/Development/Data/RealData/CCPRealData_Frankfurt.rds")
TestData <- readRDS("../dsCCPhos/Development/Data/TestData/CCPTestData.rds")
#TestData_WithEmptyTables <- readRDS("../dsCCPhos/Development/Data/TestData/CCPTestData_WithEmptyTables.rds")


# TestData$sample <- data.frame()
# TestData$`molecular-marker` <- data.frame()
#
# saveRDS(TestData, file = "../dsCCPhos/Development/Data/TestData/CCPTestData_WithEmptyTables.rds")


CCPConnections <- ConnectToVirtualCCP(CCPTestData = TestData,
                                      NumberOfSites = 3,
                                      NumberOfPatientsPerSite = 5000)
                                      #AddedDsPackages = "dsSurvivalFix")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check server requirements using dsCCPhosClient::CheckServerRequirements()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Requirements <- CheckServerRequirements(DataSources = CCPConnections)


# datashield.pkg_status(conns = CCPConnections)
# datashield.method_status(conns = CCPConnections)
# datashield.methods(conns = CCPConnections, type = "assign")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Raw Data Set (RDS) from Opal data base to R sessions on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Messages <- LoadRawDataSet(CCPSiteSpecifications = NULL,
                           DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check RDS tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSTableCheck <- ds.CheckRDSTables(DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validate RDS data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSValidationReport <- ds.GetRDSValidationReport(DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Optionally: Draw random sample from Raw Data Set on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ds.DrawSample(RawDataSetName = "RawDataSet",
              SampleSize = "1000")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform Raw Data Set (RDS) into Curated Data Set (CDS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Transform Raw Data Set (RDS) into Curated Data Set (CDS) (using default settings)
Curation <- ds.CurateData(RawDataSetName = "RawDataSet",
                          OutputName = "CurationOutput",
                          DataSources = CCPConnections)

# Make tables from Curated Data Set directly addressable by unpacking them into R server session
Messages <- ds.UnpackCuratedDataSet(CuratedDataSetName = "CuratedDataSet",
                                    DataSources = CCPConnections)

# Get curation reports
CurationReport <- dsCCPhosClient::ds.GetCurationReport(DataSources = CCPConnections)

View(CurationReport$EntryCounts$BioSampling)

# Exemplary look at a curation report table
#View(CurationReport$Transformation$All$Monitors$Staging)
#View(CurationReport$Transformation$All$EligibilityOverviews$Staging)
#View(CurationReport$Transformation$All$ValueSetOverviews$Raw)

# Get validation report of Curated Data Set (CDS)
# ValidationReportCDS <- ds.GetValidationReport_CDS(Name_CurationOutput = "CurationOutput",
#                                                   DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot data on value eligibility for exemplary table in CDS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Restructure eligibility overview table to meet requirements of plot function
# - Create separate data frames for each 'Feature' value
# - Columns in final object:
#   - 'Feature': contains names of features
#   - 'data': plot data for feature-specific plot
#-------------------------------------------------------------------------------

PlotData <- CurationReports$All$Transformation$EligibilityOverviews$Staging %>%
                select(-ends_with("_Proportional")) %>%
                pivot_longer(cols = c(Raw, Harmonized, Recoded, Final),
                             names_to = "Stage",
                             values_to = "Count") %>%
                pivot_wider(names_from = "Eligibility",
                            values_from = "Count") %>%
                nest(.by = Feature)      # 'Split' the whole table into smaller data frames for each 'Feature' value


library(plotly)

plot_ly(data = filter(PlotData, Feature == "UICCStage")$data[[1]],
        x = ~Stage,
        y = ~Eligible,
        type = "bar",
        name = "Eligible",
        color = I(dsCCPhosClient::CCPhosColors$Green)) %>%
    add_trace(y = ~Ineligible,
              name = "Ineligible",
              color = I(dsCCPhosClient::CCPhosColors$Red)) %>%
    add_trace(y = ~Missing,
              name = "Missing",
              color = I(dsCCPhosClient::CCPhosColors$MediumGrey)) %>%
    layout(xaxis = list(categoryorder = "array",
                        categoryarray = c("Raw", "Harmonized", "Recoded", "Final")),
           yaxis = list(title = "Count"),
           barmode = "stack")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform Curated Data Set (CDS) into Augmented Data Set (ADS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Run ds.AugmentData
Messages <- ds.AugmentData(CuratedDataSetName = "CuratedDataSet",
                           OutputName = "AugmentationOutput",
                           DataSources = CCPConnections)


# Make tables from Augmented Data Set directly addressable by unpacking them into R server session
Messages <- ds.UnpackAugmentedDataSet(AugmentedDataSetName = "AugmentedDataSet",
                                      DataSources = CCPConnections)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get overview of objects in server workspaces
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Using dsCCPhosClient::GetServerWorkspaceInfo() and dsCCPhosClient::ds.GetObjectMetaData()
#-------------------------------------------------------------------------------

# Collect comprehensive information about all workspace objects
ServerWorkspaceInfo <- GetServerWorkspaceInfo(DataSources = CCPConnections)

# Overview of all objects in server R sessions
View(ServerWorkspaceInfo$Overview)

# Detailed meta data of a particular object (also part of ServerWorkspaceInfo)
ObjectMetaData <- ds.GetObjectMetaData(ObjectName = "ADS_Patients",
                                       DataSources = CCPConnections)

# Explore Object meta data: Structural overview
View(ObjectMetaData$FirstEligible$Structure)

# Get type of feature 'PatientID'
ObjectMetaData$FirstEligible$DataTypes["PatientID"]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform exemplary analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Test <- ds.GetTTEModel(DataSources = CCPConnections,
                       TableName = "ADS_Patients",
                       TimeFeature = "TimeFollowUp",
                       EventFeature = "IsDocumentedDeceased",
                       ModelType = "coxph",
                       CovariateA = "UICCStageCategory",
                       #CovariateB = "UICCStageCategory",
                       MinFollowUpTime = 20)




Test <- ds.GetFeatureInfo(DataSources = CCPConnections,
                          TableName = "ADS_Patients",
                          FeatureName = "TNM_T")

Test <- ds.GetSampleStatistics(DataSources = CCPConnections,
                               TableName = "ADS_Patients",
                               MetricFeatureName = "PatientAgeAtDiagnosis")

Test <- ds.GetFrequencyTable(DataSources = CCPConnections,
                             TableName = "ADS_Patients",
                             FeatureName = "TNM_T",
                             MaxNumberCategories = 5)

RelativeFrequencies <- Test$RelativeFrequencies %>%
                            mutate(across(-Site, ~ paste0("(", round(.x * 100, 0), "%)")))

TableData <- Test$AbsoluteFrequencies %>%
                  mutate(across(everything(), as.character)) %>%
                  bind_rows(RelativeFrequencies) %>%
                  group_by(Site) %>%
                      summarize(across(everything(), ~ paste0(.x, collapse = "  ")))


library(ggplot2)

PlotData <- Test$AbsoluteFrequencies %>%
                pivot_longer(cols = -Site,
                             names_to = "Value",
                             values_to = "AbsoluteFrequency") %>%
                filter(Site != "All")

Plot <- ggplot(data = as.data.frame(PlotData),
               mapping = aes(fill = Site,
                             x = Value,
                             y = AbsoluteFrequency)) +
            geom_bar(position = "stack",
                     stat = "identity")

Plot <- MakeColumnPlot(DataFrame = PlotData,
                       XFeature = Value,
                       YFeature = AbsoluteFrequency,
                       GroupingFeature = Site)



Test <- ExploreFeature(DataSources = CCPConnections,
                       TableName = "ADS_Patients",
                       FeatureName = "TimeDiagnosisToDeath")




ds.GetObjectMetaData(ObjectName = "AugmentationOutput",
                     DataSources = CCPConnections)


ds.mean(x = "ADS_Patients$PatientAgeAtDiagnosis",
        datasources = CCPConnections)


MetaData_ADS_Patients <- ds.GetObjectMetaData(ObjectName = "ADS_Patients",
                                              DataSources = CCPConnections)

View(MetaData_ADS_Patients$SiteA$ContentOverview)


SampleStatistics <- ds.GetSampleStatistics(TableName = "ADS_Patients",
                                           MetricFeatureName = "PatientAgeAtDiagnosis",
                                           DataSources = CCPConnections)


TestPlot <- MakeBoxPlot(SampleStatistics = SampleStatistics,
                        AxisTitle_y = "Patient age at diagnosis",
                        FillPalette = c("All" = CCPhosColors$MediumGrey,
                                        "SiteA" = CCPhosColors$Primary,
                                        "SiteB" = CCPhosColors$Secondary,
                                        "SiteC" = CCPhosColors$Tertiary))

TestPlot


# dsSurvivalClient::ds.Surv(time = "ADS_Patients$TimeFollowUp",
#                           event = "ADS_Patients$IsDocumentedDeceased",
#                           objectname = "TestSurv",
#                           datasources = CCPConnections)
#
# dsSurvivalClient::ds.survfit(formula = 'TestSurv',
#                              object = "TestSurvfit")



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
