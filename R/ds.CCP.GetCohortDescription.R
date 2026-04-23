
#' ds.CCP.GetCohortDescription
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Obtain Server-specific and aggregated descriptive characteristics about the patient cohort to be analyzed.
#'
#' Linked to server-side \code{AGGREGATE} function \code{GetCohortDescriptionDS()}.
#'
#' @param DataSetName \code{string} - Name of Data Set object (list) on server, usually "RawDataSet", "CuratedDataSet" or "AugmentedDataSet" - Default: "AugmentedDataSet"
#' @param Stage \code{string} - Indicating the transformation stage of the described data set, one of "Raw" / "Curated" / "Augmented" - Default: "Augmented"
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{logical} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{FALSE}
#'
#' @return A \code{list} containing descriptive statistics characterizing patient cohort:
#'         \itemize{\item CohortSize
#'                  \item CohortSizeOverTime
#'                  \item AgeDistribution
#'                  \item SexDistribution}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.CCP.GetCohortDescription <- function(DataSetName = "CCP.AugmentedDataSet",
                                        Stage = "Augmented",
                                        DSConnections = NULL,
                                        DS.async = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DataSetName <- "AugmentedDataSet"
  # Stage <- "ADS"
  # DSConnections <- CCPConnections
  # DS.async <- FALSE

  # --- Argument Validation ---
  assert_that(is.string(DataSetName),
              is.string(Stage),
              is.flag(DS.async))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Server returns
#-------------------------------------------------------------------------------

  # ServerReturns: Obtain descriptive data for each server calling dsCCPhos::GetCohortDescriptionDS()
  ServerReturns <- DSI::datashield.aggregate(conns = DSConnections,
                                             expr = call("GetCohortDescriptionDS",
                                                         DataSetName.S = DataSetName,
                                                         Stage.S = Stage),
                                             async = DS.async)


  # Transpose list (turning 'inside-out') for easier processing
  ServerReturns <- ServerReturns %>% list_transpose(simplify = FALSE)


#-------------------------------------------------------------------------------
# Cohort Size
#-------------------------------------------------------------------------------

# Cohort Size Summary (Cumulated Patient and Diagnosis Count)
#-------------------------------------------------------------------------------
  CohortSize.Servers <- ServerReturns$CohortSize %>%
                            list_rbind(names_to = "Server")

  CohortSize.All <- CohortSize.Servers %>%
                        summarize(PatientCount = sum(PatientCount),
                                  DiagnosisCount = sum(DiagnosisCount)) %>%
                        mutate(Server = "All",
                               DiagnosesPerPatient = DiagnosisCount / PatientCount)

  CohortSize <- CohortSize.Servers %>%
                    bind_rows(CohortSize.All)


# Cohort Size Time Series
#-------------------------------------------------------------------------------

  # Create coherent data.frame with Server-specific data
  CohortSize.OverTime.Servers <- ServerReturns$CohortSize.OverTime %>%
                                    list_rbind(names_to = "Server")

  # Get cumulated values
  CohortSize.OverTime.All <- CohortSize.OverTime.Servers %>%
                                  group_by(DiagnosisYear) %>%
                                      summarize(across(c(PatientCount, DiagnosisCount), ~ sum(.x, na.rm = TRUE))) %>%
                                  ungroup() %>%
                                  mutate(DiagnosesPerPatient = DiagnosisCount / PatientCount,
                                         Server = "All")

  # # Get time-point-specific median values across Servers
  # CohortSize.OverTime.Mean <- CohortSize.OverTime.Servers %>%
  #                                   group_by(DiagnosisYear) %>%
  #                                       summarize(across(c(PatientCount, DiagnosisCount), ~ mean(.x, na.rm = TRUE))) %>%
  #                                   ungroup() %>%
  #                                   mutate(DiagnosesPerPatient = DiagnosisCount / PatientCount,
  #                                          Server = "Mean")
  #
  # # Get time-point-specific median values across Servers
  # CohortSize.OverTime.Median <- CohortSize.OverTime.Servers %>%
  #                                   group_by(DiagnosisYear) %>%
  #                                       summarize(across(c(PatientCount, DiagnosisCount), ~ round(median(.x, na.rm = TRUE)))) %>%
  #                                   ungroup() %>%
  #                                   mutate(DiagnosesPerPatient = DiagnosisCount / PatientCount,
  #                                          Server = "Median")

  # Row-bind Server-specific and cumulated data
  CohortSize.OverTime <- CohortSize.OverTime.Servers %>%
                              bind_rows(CohortSize.OverTime.All)
                              # bind_rows(CohortSize.OverTime.Mean) %>%
                              # bind_rows(CohortSize.OverTime.Median)


#-------------------------------------------------------------------------------
# Age
#-------------------------------------------------------------------------------

  AgeDistribution.Servers <- ServerReturns$Age %>%
                                list_rbind(names_to = "Server")

  AgeDistribution.All <- AgeDistribution.Servers %>%
                              group_by(AgeGroup) %>%
                                  summarize(N = sum(N)) %>%
                              ungroup() %>%
                              mutate(Server = "All",
                                     Proportion = N / sum(N))

  AgeDistribution <- AgeDistribution.Servers %>%
                          bind_rows(AgeDistribution.All)


#-------------------------------------------------------------------------------
# Sex
#-------------------------------------------------------------------------------

  SexDistribution.Servers <- ServerReturns$Sex %>%
                                  list_rbind(names_to = "Server")

  SexDistribution.All <- SexDistribution.Servers %>%
                              group_by(Sex) %>%
                                  summarize(N = sum(N)) %>%
                              ungroup() %>%
                              mutate(Server = "All",
                                     Proportion = N / sum(N))

  SexDistribution <- SexDistribution.Servers %>%
                          bind_rows(SexDistribution.All)


#-------------------------------------------------------------------------------
  return(list(CohortSize = CohortSize,
              CohortSize.OverTime = CohortSize.OverTime,
              AgeDistribution = AgeDistribution,
              SexDistribution = SexDistribution))
}
