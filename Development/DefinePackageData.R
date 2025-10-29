
library(dplyr)
library(usethis)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CCPhosAlphaPalettes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CCPhosAlphaPalettes <- list(Levels_2 <- c(0.5, 0.9),
                            Levels_3 <- c(0.2, 0.5, 0.9),
                            Levels_4 <- c(0.3, 0.5, 0.7, 0.9),
                            Levels_5 <- c(0.4, 0.5, 0.6, 0.7, 0.8))

# Save data in .rda-file and make it part of package
use_data(CCPhosAlphaPalettes, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CCPhos Colors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CCPhosColors <- list(LightGrey = "#EDEDED",
                     MediumGrey = "#D0D0D0",
                     DarkGrey = "#595959",
                     #---------
                     Primary = "#054996",
                     PrimaryLight = "#05499650",
                     Secondary = "#8e1e39",
                     SecondaryLight = "#8e1e3950",
                     Tertiary = "#2B8C88",
                     TertiaryLight = "#2B8C8850",
                     #---------
                     Accent = "#960551",
                     AccentLight = "#96055150",
                     #---------
                     BlueNice = "#7EA6E0",
                     Green = "#269D27",
                     Orange = "#DE8F02",
                     Red = "#A90939")

# Save data in .rda-file and make it part of package
use_data(CCPhosColors, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data transported from dsCCPhos package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta.ADS <- dsCCPhos::Meta.ADS
Meta.Tables <- dsCCPhos::Meta.Tables
Meta.Features <- dsCCPhos::Meta.Features
Meta.Values <- dsCCPhos::Meta.Values
Set.DataHarmonization <- dsCCPhos::Set.DataHarmonization
Set.DiagnosisAssociation <- dsCCPhos::Set.DiagnosisAssociation
Set.DiagnosisRedundancy <- dsCCPhos::Set.DiagnosisRedundancy
Set.Dictionary <- dsCCPhos::Set.Dictionary
Set.FeatureObligations <- dsCCPhos::Set.FeatureObligations
Set.FeatureTracking <- dsCCPhos::Set.FeatureTracking
Set.FuzzyStringMatching <- dsCCPhos::Set.FuzzyStringMatching
Set.TransformativeExpressions <- dsCCPhos::Set.TransformativeExpressions


# Add feature to 'Meta.ADS' indicating if a feature should be part of an "order list" that can be passed to dsFredaClient::GetExplorationData()
Meta.ADS <- Meta.ADS %>%
                mutate(ExploreRoutinely = case_when(TableName == "Events" & FeatureName %in% c("EventClass",
                                                                                               "EventSubclass") ~ TRUE,
                                                    TableName == "Diagnosis" & FeatureName %in% c("ICD10Code",
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
                                                                                                  "TimeFollowUp") ~ TRUE,
                                                    TableName == "Patient" & FeatureName %in% c("Sex",
                                                                                                "LastVitalStatus",
                                                                                                "CausesOfDeath",
                                                                                                "CountDiagnoses") ~ TRUE,
                                                    .default = FALSE))

use_data(Meta.ADS, overwrite = TRUE)
use_data(Meta.Tables, overwrite = TRUE)
use_data(Meta.Features, overwrite = TRUE)
use_data(Meta.Values, overwrite = TRUE)
use_data(Set.DataHarmonization, overwrite = TRUE)
use_data(Set.DiagnosisAssociation, overwrite = TRUE)
use_data(Set.DiagnosisRedundancy, overwrite = TRUE)
use_data(Set.Dictionary, overwrite = TRUE)
use_data(Set.FeatureObligations, overwrite = TRUE)
use_data(Set.FeatureTracking, overwrite = TRUE)
use_data(Set.FuzzyStringMatching, overwrite = TRUE)
use_data(Set.TransformativeExpressions, overwrite = TRUE)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: Define server requirements that are checked before running of CCPhos functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Set.ServerRequirements <- list(#--- Data frame containing names of required packages ---
                                RequiredPackages = data.frame(PackageName = character()) %>%
                                                        add_row(PackageName = "dsBase") %>%
                                                        add_row(PackageName = "dsCCPhos"),
                                #--- Data frame containing names and types of required functions ---
                                RequiredFunctions = data.frame(FunctionName = character(),
                                                               FunctionType = character()) %>%
                                                        add_row(FunctionName = "GetReportingObjectDS", FunctionType = "aggregate") %>%
                                                        add_row(FunctionName = "AugmentDataDS", FunctionType = "assign") %>%
                                                        add_row(FunctionName = "CurateDataDS", FunctionType = "assign") %>%
                                                        add_row(FunctionName = "ExtractFromListDS", FunctionType = "assign"))

# Save data in .rda-file and make it part of package
use_data(Set.ServerRequirements, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Template data frame: Server specifications
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Initiate tibble that holds credentials of participating servers
ServerSpecifications <- tibble(ServerName = character(),
                               URL = character(),
                               ProjectName = character(),
                               Token = character())

# Add site "Sissy"
ServerSpecifications <- add_row(ServerSpecifications,
                                ServerName = "Sissi",
                                URL = "https://Sissi/",
                                ProjectName = "Project",
                                Token = "1234567890")

# Save data in .rda-file and make it part of package
use_data(ServerSpecifications, overwrite = TRUE)

