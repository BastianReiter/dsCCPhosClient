
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Virtual DataSHIELD infrastructure for testing purposes -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Install newest base DataSHIELD packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# devtools::install_github(repo = "datashield/dsBase")
# devtools::install_github(repo = "datashield/dsBaseClient")

# Install own DataSHIELD packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# devtools::install_github(repo = "BastianReiter/dsFreda")
# devtools::install_github(repo = "BastianReiter/dsFredaClient")
# devtools::install_github(repo = "BastianReiter/dsCCPhos")
# devtools::install_github(repo = "BastianReiter/dsCCPhosClient")
# devtools::install_github(repo = "BastianReiter/CCPhosApp")

# Install additional Datashield-packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# install.packages("dsTidyverse")
# install.packages("dsTidyverseClient")
#
# devtools::install_github("tombisho/dsSynthetic", dependencies = TRUE)
# devtools::install_github("tombisho/dsSyntheticClient", dependencies = TRUE)
#
# devtools::install_github("neelsoumya/dsSurvival")
# devtools::install_github("neelsoumya/dsSurvivalClient")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load required packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dsBaseClient)
library(dsFredaClient)
library(dsTidyverseClient)
library(resourcer)

# Print DataSHIELD errors right away
options(datashield.errors.print = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Establish Connections to virtual servers using dsCCPhosClient::ConnectToVirtualCCP()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#TestData <- readRDS("../dsCCPhos/Development/Data/RealData/CCPRealData_Frankfurt.rds")
TestData <- readRDS("../dsCCPhos/Development/Data/TestData/CCPTestData.rds")

# Definition of test resource, exemplary with local csv-file
# TestResource <- resourcer::newResource(name = "TestResource",
#                                        #url = "file://./Development/Test/DummyData.csv",
#                                        url = "file://localhost/C:/Users/Basti/ARBEIT Lokal/dsCCPhosClient/Development/Test/DummyData.csv",
#                                        format = "csv")


CCPConnections <- dsCCPhosClient::ConnectToVirtualCCP(CCPTestData = TestData,
                                                      NumberOfServers = 3,
                                                      NumberOfPatientsPerServer = 1000,
                                                      AddedDsPackages = c("dsFreda",
                                                                          "dsTidyverse"))
                                      #Resources = list(TestResource = TestResource))


dsCCPhosClient::CCP.QuickProcessingRun()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check server requirements using dsCCPhosClient::CheckServerRequirements()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CheckServerRequirements()


# datashield.pkg_status(conns = CCPConnections)
# datashield.method_status(conns = CCPConnections)
# datashield.methods(conns = CCPConnections, type = "assign")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Raw Data Set (RDS) from Opal data base to R sessions on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CCP.LoadRawDataSet(ServerSpecifications = NULL)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check RDS tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSTableCheck <- ds.GetDataSetCheck(DataSetName = "CCP.RawDataSet",
                                    Module = "CCP",
                                    Stage = "Raw")

View(RDSTableCheck$TableStatus)

View(RDSTableCheck$TableRowCounts$Diagnosis)
View(RDSTableCheck$FeatureExistence$Diagnosis)
View(RDSTableCheck$FeatureTypes$Diagnosis)
View(RDSTableCheck$NonMissingValueRates$Diagnosis)

RDSTableCheck$TableStatus



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validate RDS data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# RDSValidationReports <- ds.GetRDSValidationReport()

# ValidationSummaries <- RDSValidationReports %>%
#                             map(function(Server)
#                                 {
#                                     map()
#                                 })summary(report))


# ValidationReportTables <- ValidationSummaries %>%
#                               map(\(summary) as.data.frame(summary, check.names = FALSE))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Optionally: Draw random sample from Raw Data Set on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ds.CCP.DrawSample(RawDataSetName = "CCP.RawDataSet",
                  SampleSize = 2000,
                  SampleName = "RDSSample")


Test <- DSLite::getDSLiteData(CCPConnections, "CCP.RawDataSet")
View(Test$ServerA$BioSampling)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform Raw Data Set (RDS) into Curated Data Set (CDS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Transform Raw Data Set (RDS) into Curated Data Set (CDS) (using default settings)
dsFredaClient::ds.CurateData(RawDataSetName = "CCP.RawDataSet",
                             Module = "CCP",
                             OutputName = "CCP.CurationOutput")

CDSTableCheck <- ds.GetDataSetCheck(DataSetName = "CCP.CuratedDataSet",
                                    Stage = "Curated")

# Get curation report
CurationReport <- ds.GetCurationReport(Module = "CCP")


FredaGUI::Widget.CurationReport(Module = "CCP",
                                CurationReport = CurationReport)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform Curated Data Set (CDS) into Augmented Data Set (ADS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Run ds.AugmentData
ds.AugmentData(CuratedDataSetName = "CCP.CuratedDataSet",
               OutputName = "CCP.AugmentationOutput")

ADSTableCheck <- ds.GetDataSetCheck(DataSetName = "CCP.AugmentedDataSet",
                                    Module = "CCP",
                                    Stage = "Augmented")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get overview of objects in server workspaces
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Using dsCCPhosClient::GetServerWorkspaceInfo() and dsCCPhosClient::ds.GetObjectMetaData()
#-------------------------------------------------------------------------------

# Collect comprehensive information about all workspace objects
ServerWorkspaceInfo <- GetServerWorkspaceInfo()

# Overview of all objects in server R sessions
View(ServerWorkspaceInfo$Overview$All)

# Detailed meta data of a particular object (also part of ServerWorkspaceInfo)
ObjectMetaData <- ds.GetObjectMetaData(ObjectName = "CDS_Patient")

# Explore Object meta data: Structural overview
View(ObjectMetaData$ServerA$Structure)

# Get type of feature 'PatientID'
ObjectMetaData$FirstEligible$DataTypes["PatientID"]



Exploration <-dsFredaClient::GetExplorationData(OrderList = list(CCP.ADS.Diagnosis = c("ICD10Code",
                                                                                       "ICDOTopographyCode",
                                                                                       "LocalizationSide",
                                                                                       "ICDOMorphologyCode",
                                                                                       "Grading",
                                                                                       "UICCStage",
                                                                                       "UICCStageCategory",
                                                                                       "TNM.T",
                                                                                       "TNM.N",
                                                                                       "TNM.M",
                                                                                       "PatientAgeAtDiagnosis",
                                                                                       "TimeDiagnosisToDeath",
                                                                                       "TimeFollowUp"),
                                                                 CCP.ADS.Patient = c("Sex",
                                                                                     "LastVitalStatus",
                                                                                     "CausesOfDeath",
                                                                                     "CountDiagnoses")))


Proc <- Widget.ServerExplorer(ServerWorkspaceInfo = ServerWorkspaceInfo,
                              ExplorationData = Exploration,
                              EnableLiveConnection = TRUE,
                              RunAutonomously = TRUE,
                              UseVirtualConnections = FALSE)

CCPhosApp::Widget.ServerExplorer()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process ADS tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Messages <- ds.JoinTables(TableNameA = "CCP.ADS.Patient",
                          TableNameB = "CCP.ADS.Diagnosis",
                          ByStatement = "PatientID",
                          OutputName = "AnalysisData")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Exploration
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CohortDescription <- ds.GetCohortDescription(DataSetName = "CCP.AugmentedDataSet",
                                             Stage = "Augmented")





ICD10 <- dsFredaClient::ds.GetFrequencyTable(TableName = "CCP.ADS.Diagnosis",
                              FeatureName = "ICD10Code",
                              MaxNumberCategories = 100)













# Transform data into display-friendly time series tables using auxiliary function 'DisplayTimeSeries()'
PatientCount.TimeSeries <- DisplayTimeSeries(TimeSeriesData = CohortDescription$CohortSize_OverTime,
                                             TimePointFeature = "DiagnosisYear",
                                             ValueFeature = "PatientCount",
                                             GroupingFeature = "Server",
                                             IncludeMissingTimePoints = TRUE)


Plot <- CohortDescription$CohortSize_OverTime %>%
            filter(Server == "All") %>%
            MakeColumnPlot(XFeature = DiagnosisYear,
                           YFeature = PatientCount,
                           GroupingFeature = "Server")


Plot <- CohortDescription$GenderDistribution %>%
            filter(Server != "All") %>%
            MakeColumnPlot(XFeature = Gender,
                           YFeature = N,
                           GroupingFeature = Server)

Plot <- CohortDescription$AgeDistribution %>%
            filter(Server != "All") %>%
            MakeColumnPlot(XFeature = AgeGroup,
                           YFeature = N,
                           GroupingFeature = Server)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform exemplary analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


TestCrossTab <- ds.GetCrossTab(TableName = "CCP.ADS.Patient",
                       FeatureNames = c("Sex", "LastVitalStatus"),
                       RemoveNA = TRUE)









ds.MutateTable(TableName = "ADS.Diagnosis",
               MutateExpression = "UICCStageClassification = case_when(str_starts(TNM.T, '3') ~ 'III', .default = '<NA>')",
               OutputName = "TestUICC")


ds.names("TestUICC")


Test <- dsBaseClient::ds.corTest(x = "AnalysisDataSet$PatientAgeAtDiagnosis",
                                 y = "AnalysisDataSet$TimeDiagnosisToDeath")


Test <- dsBaseClient::ds.glm()





Test <- ds.GetTTEModel(TableName = "AnalysisDataSet",
                       TimeFeature = "TimeFollowUp",
                       EventFeature = "IsDocumentedDeceased",
                       ModelType = "survfit",
                       CovariateA = "UICCStageCategory",
                       #CovariateB = "UICCStageCategory",
                       MinFollowUpTime = 20)

library(ggplot2)
library(ggsurvfit)

Test$ServerC %>%
    ggsurvfit()


Test <- ds.GetFeatureInfo(TableName = "AnalysisDataSet",
                          FeatureName = "TNM_T")

Test <- ds.GetSampleStatistics(TableName = "AnalysisDataSet",
                               MetricFeatureName = "PatientAgeAtDiagnosis")

Test <- ds.GetFrequencyTable(TableName = "ADS_Diagnosis",
                             FeatureName = "TNM_T",
                             MaxNumberCategories = 20)

RelativeFrequencies <- Test$RelativeFrequencies %>%
                            mutate(across(-Server, ~ paste0("(", round(.x * 100, 0), "%)")))

TableData <- Test$AbsoluteFrequencies %>%
                  mutate(across(everything(), as.character)) %>%
                  bind_rows(RelativeFrequencies) %>%
                  group_by(Server) %>%
                      summarize(across(everything(), ~ paste0(.x, collapse = "  ")))


library(ggplot2)

PlotData <- Test$AbsoluteFrequencies %>%
                pivot_longer(cols = -Server,
                             names_to = "Value",
                             values_to = "AbsoluteFrequency") %>%
                filter(Server != "All")

Plot <- ggplot(data = as.data.frame(PlotData),
               mapping = aes(fill = Server,
                             x = Value,
                             y = AbsoluteFrequency)) +
            geom_bar(position = "stack",
                     stat = "identity")

Plot <- MakeColumnPlot(DataFrame = PlotData,
                       XFeature = Value,
                       YFeature = AbsoluteFrequency,
                       GroupingFeature = Server)



Test <- ExploreFeature(TableName = "AnalysisDataSet",
                       FeatureName = "TimeDiagnosisToDeath")




ds.GetObjectMetaData(ObjectName = "AugmentationOutput")


ds.mean(x = "ADS_Patients$PatientAgeAtDiagnosis",
        datasources = CCPConnections)


MetaData_ADS_Patients <- ds.GetObjectMetaData(ObjectName = "ADS_Patients")

View(MetaData_ADS_Patients$ServerA$ContentOverview)


SampleStatistics <- ds.GetSampleStatistics(TableName = "ADS_Patients",
                                           MetricFeatureName = "PatientAgeAtDiagnosis")


TestPlot <- MakeBoxPlot(SampleStatistics = SampleStatistics,
                        AxisTitle_y = "Patient age at diagnosis",
                        FillPalette = c("All" = CCPhosColors$MediumGrey,
                                        "ServerA" = CCPhosColors$Primary,
                                        "ServerB" = CCPhosColors$Secondary,
                                        "ServerC" = CCPhosColors$Tertiary))

TestPlot


# dsSurvivalClient::ds.Surv(time = "ADS_Patients$TimeFollowUp",
#                           event = "ADS_Patients$IsDocumentedDeceased",
#                           objectname = "TestSurv",
#                           datasources = CCPConnections)
#
# dsSurvivalClient::ds.survfit(formula = 'TestSurv',
#                              object = "TestSurvfit")


#===============================================================================
# TEST dsSurvival (Xavier Escriba)
#===============================================================================


# perform conversion of data
ds.asNumeric(x.name = "AnalysisData$IsDocumentedDeceased",
             newobj = "EVENT",
             datasources = CCPConnections)

ds.asNumeric(x.name = "AnalysisData$TimeFollowUp",
             newobj = "SURVTIME",
             datasources = CCPConnections)


# ds.asFactor(input.var.name = "D$time.id",
#             newobj = "TID",
#             datasources = CCPConnections)

ds.log(x = "AnalysisData$TimeFollowUp",
       newobj = "log.surv",
       datasources = CCPConnections)


# ds.asNumeric(x.name = "D$starttime",
#              newobj = "STARTTIME",
#              datasources = CCPConnections)
#
# ds.asNumeric(x.name = "D$endtime",
#              newobj = "ENDTIME",
#              datasources = CCPConnections)



Test <- ds.GetTTEModel(TableName = "AnalysisDataSet",
                       TimeFeature = "TimeFollowUp",
                       EventFeature = "IsDocumentedDeceased",
                       ModelType = "survfit",
                       CovariateA = "UICCStageCategory",
                       #CovariateB = "UICCStageCategory",
                       MinFollowUpTime = 20)


# Not working! Can not find object 'EVENT' (-> Evaluation bug?)
dsSurvivalClient::ds.Surv(time='EVENT',
                          event = 'SURVTIME',
                          objectname='TestSurv',
                          type='right')

# Using own function as workaround to create Surv() object on servers
ds.CreateSurvObject(TableName = "AnalysisData",
                    TimeFeature = "TimeFollowUp",
                    EventFeature = "IsDocumentedDeceased",
                    MinFollowUpTime = 20,
                    OutputName = "SurvObject")

CCPhosApp::Widget.ServerExplorer()

# build Cox model
coxph_model_full <- dsSurvivalClient::ds.coxph.SLMA(formula = 'SurvObject~AnalysisData$UICCStageCategory')



dsSurvivalClient::ds.coxphSLMAassign(formula = 'surv_object~D$age+D$female',
                                     objectname = 'coxph_serverside')

dsSurvivalClient::ds.cox.zphSLMA(fit = 'coxph_serverside')

dsSurvivalClient::ds.coxphSummary(x = 'coxph_serverside')


# meta-analyze hazard ratios
input_logHR = c(coxph_model_full$server1$coefficients[1,2],
                coxph_model_full$server2$coefficients[1,2],
                coxph_model_full$server3$coefficients[1,2])

input_se    = c(coxph_model_full$server1$coefficients[1,3],
                coxph_model_full$server2$coefficients[1,3],
                coxph_model_full$server3$coefficients[1,3])

meta_model <- metafor::rma(input_logHR, sei = input_se, method = 'REML')


# forest plot
metafor::forest.rma(x = meta_model, digits = 4)


# plot survival curves
dsSurvivalClient::ds.survfit(formula='surv_object~1', objectname='survfit_object')
dsSurvivalClient::ds.plotsurvfit(formula = 'survfit_object')

# better looking survival curves using ggplot (courtesy Xavier Escriba Montagut)
res <- dsSurvivalClient::ds.plotsurvfit(formula = 'survfit_object', ggplot = TRUE)
(res[[1]] | res[[2]] | res[[3]])

########################
# get life table
res <- dsSurvivalClient::ds.life.table("survfit_object")
# plot life table
plot_life.table(res)
# stratified example
res_2 <- dsSurvivalClient::ds.life.table("coxph_serverside")
plot_life.table(res_2, "strata")




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
# SyntheticData <- SyntheticData$ServerTotal$Warning
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
