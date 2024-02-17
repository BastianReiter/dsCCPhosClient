
library(readxl)
library(usethis)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AlphaPalettes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AlphaPalettes <- list(Levels_2 <- c(0.5, 0.9),
                      Levels_3 <- c(0.2, 0.5, 0.9),
                      Levels_4 <- c(0.3, 0.5, 0.7, 0.9),
                      Levels_5 <- c(0.4, 0.5, 0.6, 0.7, 0.8))

# Save data in .rda-file and make it part of package
use_data(AlphaPalettes, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Colors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Colors <- list(LightGrey = "#EDEDED",
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
               BlueNice = "#7EA6E0")

# Save data in .rda-file and make it part of package
use_data(Colors, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data transported from dsCCPhos package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_TableNames <- dsCCPhos::Meta_TableNames
Meta_FeatureNames <- dsCCPhos::Meta_FeatureNames
Meta_ValueSets <- dsCCPhos::Meta_ValueSets
RuleSet_RawDataTransformation <- dsCCPhos::RuleSet_RawDataTransformation
RuleSet_DiagnosisRedundancy <- dsCCPhos::RuleSet_DiagnosisRedundancy
RuleSet_DiagnosisAssociation <- dsCCPhos::RuleSet_DiagnosisAssociation

use_data(Meta_TableNames, overwrite = TRUE)
use_data(Meta_FeatureNames, overwrite = TRUE)
use_data(Meta_ValueSets, overwrite = TRUE)
use_data(RuleSet_RawDataTransformation, overwrite = TRUE)
use_data(RuleSet_DiagnosisRedundancy, overwrite = TRUE)
use_data(RuleSet_DiagnosisAssociation, overwrite = TRUE)


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
                                                        add_row(FunctionName = "GetCurationReportDS", FunctionType = "aggregate") %>%
                                                        add_row(FunctionName = "AugmentDataDS", FunctionType = "assign") %>%
                                                        add_row(FunctionName = "CurateDataDS", FunctionType = "assign") %>%
                                                        add_row(FunctionName = "UnpackAugmentedDataSetDS", FunctionType = "assign") %>%
                                                        add_row(FunctionName = "UnpackCuratedDataSetDS", FunctionType = "assign"))

# Save data in .rda-file and make it part of package
use_data(Meta_ServerRequirements, overwrite = TRUE)

