
#' CCP.PreprocessingQuickRun
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Perform a complete data preprocessing run of FREDA and CCPhos functionality with default settings.
#'
#' @param ServerSpecifications \code{data.frame} - Same \code{data.frame} used for login. Used here only for acquisition of server-specific project names (in case they are differing) - Default: \code{NULL} for virtual project
#' @param GetReports \code{logical} - Indicating whether reporting objects should be obtained from the servers. Can be set to \code{FALSE} to save time. - Default: \code{TRUE}
#' @param RunAssignmentChecks \code{logical} Indicating whether assignment checks should be performed or omitted for reduced execution time - Default: \code{TRUE}
#' @param RunSeparately \code{logical} - Indicating whether time-consuming functions should be run separately for each server. This can be done for testing purposes or if timeout issues arise. - Default: \code{FALSE}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{logical} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{dsFredaClient::Set.DSSettings$DS.async}
#'
#' @return A \code{list} of report objects
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CCP.PreprocessingQuickRun <- function(ServerSpecifications = NULL,
                                      GetReports = TRUE,
                                      RunAssignmentChecks = FALSE,
                                      RunSeparately = FALSE,
                                      DSConnections = NULL,
                                      DS.async = dsFredaClient::Set.DSSettings$DS.async)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # ServerSpecifications <- NULL
  # RunAssignmentChecks <- TRUE
  # RunSeparately <- FALSE
  # DSConnections <- CCPConnections
  # DS.async <- FALSE

  # --- Argument Validation ---
  assert_that(is.flag(RunAssignmentChecks),
              is.flag(DS.async))
  if (!is.null(ServerSpecifications)) { is.data.frame(ServerSpecifications) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  Reports <- NULL

  Time.Initial <- Sys.time()


#===  Load Raw Data Set  =======================================================

  CCP.LoadRawDataSet(ServerSpecifications = ServerSpecifications)

  Time.AfterLoading <- Sys.time()


#===  Data Curation  ===========================================================

  ds.CurateData(RawDataSetName = "CCP.RawDataSet",
                Module = "CCP",
                OutputName = "CCP.CurationOutput",
                RunAssignmentChecks = FALSE,
                RunSeparately = RunSeparately)

  Time.AfterCuration <- Sys.time()


#===  Data Augmentation  =======================================================

  # Run ds.AugmentData
  ds.CCP.AugmentData(CuratedDataSetName = "CCP.CuratedDataSet",
                     OutputName = "CCP.AugmentationOutput",
                     RunAssignmentChecks = FALSE,
                     RunSeparately = RunSeparately)

  Time.AfterAugmentation <- Sys.time()
  Time.AfterReportAkquisition <- Time.AfterAugmentation

#-------------------------------------------------------------------------------

  if (GetReports == TRUE)
  {
      # Check RDS
      RDSTableCheck <- ds.GetDataSetCheck(DataSetName = "CCP.RawDataSet",
                                          Module = "CCP",
                                          Stage = "Raw")
      # Check CDS
      CDSTableCheck <- ds.GetDataSetCheck(DataSetName = "CCP.CuratedDataSet",
                                          Module = "CCP",
                                          Stage = "Curated")
      # Get curation report
      CurationReport <- ds.GetCurationReport()

      # Check ADS
      ADSTableCheck <- ds.GetDataSetCheck(DataSetName = "CCP.AugmentedDataSet",
                                          Module = "CCP",
                                          Stage = "Augmented")

      Reports <- list(RDSCheckData = RDSTableCheck,
                      CDSCheckData = CDSTableCheck,
                      ADSCheckData = ADSTableCheck,
                      CurationReport = CurationReport)

      Time.AfterReportAkquisition <- Sys.time()
  }

  # Create table of timed durations
  PerformanceMonitor <- tibble(Time.Initialization = Time.Initial,
                               Duration.Loading = as.double(lubridate::as.duration(Time.AfterLoading - Time.Initial)),
                               Duration.Curation = as.double(lubridate::as.duration(Time.AfterCuration - Time.AfterLoading)),
                               Duration.Augmentation = as.double(lubridate::as.duration(Time.AfterAugmentation - Time.AfterCuration)),
                               Duration.ReportAkquisition = as.double(lubridate::as.duration(Time.AfterReportAkquisition - Time.AfterAugmentation)))

#-------------------------------------------------------------------------------
  return(list(Reports = Reports,
              PerformanceMonitor = PerformanceMonitor))
}
