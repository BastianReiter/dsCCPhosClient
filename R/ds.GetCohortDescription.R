
#' ds.GetCohortDescription
#'
#' Obtain site-specific and aggregated descriptive characteristics about the patient cohort to be analyzed
#'
#' Linked to server-side \code{AGGREGATE} function \code{dsCCPhos::GetCohortDescriptionDS()}.
#'
#' @param DataSetName \code{string} - Name of Data Set object (list) on server, usually "RawDataSet", "CuratedDataSet" or "AugmentedDataSet" - Default: "AugmentedDataSet"
#' @param CCPDataSetType \code{string} - Indicating the type of CCP data set that should be described, one of "RDS" / "CDS" / "ADS" - Default: "ADS"
#' @param DataSources \code{list} of \code{DSConnection} objects
#'
#' @return A \code{list} containing descriptive statistics characterizing patient cohort
#'         \itemize{\item }
#' @export
#'
#' @author Bastian Reiter
ds.GetCohortDescription <- function(DataSources = NULL,
                                    DataSetName = "AugmentedDataSet",
                                    CCPDataSetType = "ADS")
{
#--- For Testing Purposes ---
# DataSources <- CCPConnections
# DataSetName <- "AugmentedDataSet"
# CCPDataSetType <- "ADS"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check argument eligibility
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.null(DataSources))
{
    DataSources <- DSI::datashield.connections_find()
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Package requirements
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(dplyr)
require(purrr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Site returns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SiteReturns: Obtain descriptive data for each server calling dsCCPhos::GetCohortDescriptionDS()
SiteReturns <- DSI::datashield.aggregate(conns = DataSources,
                                         expr = call("GetCohortDescriptionDS",
                                                     DataSetName.S = DataSetName,
                                                     CCPDataSetType.S = CCPDataSetType))


# Transpose list (turning 'inside-out') for easier processing
SiteReturns <- SiteReturns %>% list_transpose(simplify = FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cohort Size
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cohort Size Summary (Cumulated Patient and Diagnosis Count)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CohortSize_Sites <- SiteReturns$CohortSize %>%
                        list_rbind(names_to = "Site")

CohortSize_All <- CohortSize_Sites %>%
                      summarize(PatientCount = sum(PatientCount),
                                DiagnosisCount = sum(DiagnosisCount)) %>%
                      mutate(Site = "All",
                             DiagnosesPerPatient = DiagnosisCount / PatientCount)

CohortSize <- CohortSize_Sites %>%
                  bind_rows(CohortSize_All)


# Cohort Size Time Series
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create coherent data.frame with site-specific data
CohortSize_OverTime_Sites <- SiteReturns$CohortSize_OverTime %>%
                                list_rbind(names_to = "Site")

# Get cumulated values
CohortSize_OverTime_All <- CohortSize_OverTime_Sites %>%
                                group_by(DiagnosisYear) %>%
                                    summarize(across(c(PatientCount, DiagnosisCount), ~ sum(.x, na.rm = TRUE))) %>%
                                ungroup() %>%
                                mutate(DiagnosesPerPatient = DiagnosisCount / PatientCount,
                                       Site = "All")

# # Get time-point-specific median values across sites
# CohortSize_OverTime_Mean <- CohortSize_OverTime_Sites %>%
#                                   group_by(DiagnosisYear) %>%
#                                       summarize(across(c(PatientCount, DiagnosisCount), ~ mean(.x, na.rm = TRUE))) %>%
#                                   ungroup() %>%
#                                   mutate(DiagnosesPerPatient = DiagnosisCount / PatientCount,
#                                          Site = "Mean")
#
# # Get time-point-specific median values across sites
# CohortSize_OverTime_Median <- CohortSize_OverTime_Sites %>%
#                                   group_by(DiagnosisYear) %>%
#                                       summarize(across(c(PatientCount, DiagnosisCount), ~ round(median(.x, na.rm = TRUE)))) %>%
#                                   ungroup() %>%
#                                   mutate(DiagnosesPerPatient = DiagnosisCount / PatientCount,
#                                          Site = "Median")

# Row-bind site-specific and cumulated data
CohortSize_OverTime <- CohortSize_OverTime_Sites %>%
                            bind_rows(CohortSize_OverTime_All)
                            # bind_rows(CohortSize_OverTime_Mean) %>%
                            # bind_rows(CohortSize_OverTime_Median)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AgeDistribution_Sites <- SiteReturns$Age %>%
                              list_rbind(names_to = "Site")

AgeDistribution_All <- AgeDistribution_Sites %>%
                            group_by(AgeGroup) %>%
                                summarize(N = sum(N)) %>%
                            ungroup() %>%
                            mutate(Site = "All",
                                   Proportion = N / sum(N))

AgeDistribution <- AgeDistribution_Sites %>%
                        bind_rows(AgeDistribution_All)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Gender
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GenderDistribution_Sites <- SiteReturns$Gender %>%
                                list_rbind(names_to = "Site")

GenderDistribution_All <- GenderDistribution_Sites %>%
                              group_by(Gender) %>%
                                  summarize(N = sum(N)) %>%
                              ungroup() %>%
                              mutate(Site = "All",
                                     Proportion = N / sum(N))

GenderDistribution <- GenderDistribution_Sites %>%
                          bind_rows(GenderDistribution_All)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
return(list(CohortSize = CohortSize,
            CohortSize_OverTime = CohortSize_OverTime,
            AgeDistribution = AgeDistribution,
            GenderDistribution = GenderDistribution))
}
