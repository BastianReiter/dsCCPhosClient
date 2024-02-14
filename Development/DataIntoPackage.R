
library(readxl)
library(usethis)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: CCP Table names
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_TableNames <- read_excel(path = "./Development/MetaData/MetaDataCCP.xlsx",
                              sheet = "TableNames")

# Save data in .rda-file and make it part of package
use_data(Meta_TableNames, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: CCP Feature names
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_FeatureNames <- read_excel(path = "./Development/MetaData/MetaDataCCP.xlsx",
                                sheet = "FeatureNames")

# Save data in .rda-file and make it part of package
use_data(Meta_FeatureNames, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: CCP Value sets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_ValueSets <- read_excel(path = "./Development/MetaData/MetaDataCCP.xlsx",
                             sheet = "ValueSets")

# Save data in .rda-file and make it part of package
use_data(Meta_ValueSets, overwrite = TRUE)



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


