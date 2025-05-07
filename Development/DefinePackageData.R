
library(dplyr)
library(readxl)
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

Meta_Tables <- dsCCPhos::Meta_Tables
Meta_Features <- dsCCPhos::Meta_Features
Meta_Values <- dsCCPhos::Meta_Values
Meta_DataHarmonization <- dsCCPhos::Meta_DataHarmonization
Meta_DiagnosisRedundancy <- dsCCPhos::Meta_DiagnosisRedundancy
Meta_DiagnosisAssociation <- dsCCPhos::Meta_DiagnosisAssociation
Meta_FeatureObligations <- dsCCPhos::Meta_FeatureObligations

use_data(Meta_Tables, overwrite = TRUE)
use_data(Meta_Features, overwrite = TRUE)
use_data(Meta_Values, overwrite = TRUE)
use_data(Meta_DataHarmonization, overwrite = TRUE)
use_data(Meta_DiagnosisRedundancy, overwrite = TRUE)
use_data(Meta_DiagnosisAssociation, overwrite = TRUE)
use_data(Meta_FeatureObligations, overwrite = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: Define server requirements that are checked before running of CCPhos functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_ServerRequirements <- list(#--- Data frame containing names of required packages ---
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
use_data(Meta_ServerRequirements, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Template data frame: CCP site specifications
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Initiate tibble that holds credentials of participating CCP sites
CCPSiteSpecifications <- tibble(SiteName = character(),
                                URL = character(),
                                ProjectName = character(),
                                Token = character())

# Add site "Sissy"
CCPSiteSpecifications <- add_row(CCPSiteSpecifications,
                                 SiteName = "Sissi",
                                 URL = "https://Sissi/",
                                 ProjectName = "Project",
                                 Token = "1234567890")

# Save data in .rda-file and make it part of package
use_data(CCPSiteSpecifications, overwrite = TRUE)

