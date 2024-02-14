

#===============================================================================
#
# dsCCPhosClient Package DEVELOPMENT TRACKER
#
#===============================================================================


library(devtools)



# Set preferred license in description
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_ccby_license()


# Define part of project that should not be distributed in the package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_build_ignore("Development")


# Adding package dependencies using usethis::use_package()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_package("dplyr")
# use_package("DSI")
# use_package("ggplot2", type = "Suggests")
# use_package("gt", type = "Suggests")
# use_package("scales", type = "Suggests")
# use_package("sysfonts", type = "Suggests")
# use_package("tibble")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adding R script files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# General / Auxiliary functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_r("ds.ConnectToCCP.R")
# use_r("ds.CheckSiteRequirements.R")

# Linked to dataSHIELD AGGREGATE functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_r("ds.GetCurationReport.R")
# use_r("ds.GetObjectInfo.R")
# use_r("ds.GetSampleStatistics.R")
# use_r("ds.GetValidationReport_RawData.R")
# use_r("ds.GetValidationReport_CuratedData.R")
# use_r("ds.GetValidationReport_AugmentedData.R")

# Linked to dataSHIELD ASSIGN functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_r("ds.AugmentData.R")
# use_r("ds.CurateData.R")
# use_r("ds.UnpackAugmentationOutput.R")
# use_r("ds.UnpackCurationOutput.R")

# Data visualization
#~~~~~~~~~~~~~~~~~~~
# use_r("ExportPlot")
# use_r("ggTheme_CCP")
# use_r("gtTheme_CCP")
# use_r("MakeBoxPlot")
# use_r("MakeColumnPlot")


