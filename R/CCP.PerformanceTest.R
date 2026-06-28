
#' CCP.PerformanceTest
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Perform multiple data preprocessing runs of FREDA and CCPhos functionality while tracking performance measures.
#'
#' @param ServerSpecifications \code{data.frame} - Same \code{data.frame} used for login. Used here only for acquisition of server-specific project names (in case they are differing) - Default: \code{NULL} for virtual project
#' @param ScenarioA.SampleSizes \code{integer}
#' @param DS.async \code{logical} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{dsFredaClient::Set.DSSettings$DS.async}
#'
#' @return A \code{list} of report objects
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CCP.PerformanceTest <- function(ServerSpecifications = NULL,
                                ScenarioA.SampleSizes = c(125, 250, 500, 1000, 2000),
                                DS.async = dsFredaClient::Set.DSSettings$DS.async)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # ServerSpecifications <- NULL
  # DS.async <- FALSE

  # --- Argument Validation ---
  assert_that(is.flag(DS.async))
  if (!is.null(ServerSpecifications)) { is.data.frame(ServerSpecifications) }

#-------------------------------------------------------------------------------

#===============================================================================
# Test scenario A)
#   - All servers
#   - Varying sample sizes
#===============================================================================

  ScenarioA.Time.Initial <- Sys.time()

  # Establish connection to servers using convenience function 'ConnectToCCP'
  ScenarioA.CCPConnections <- ConnectToCCP(ServerSpecifications = ServerSpecifications)

  ScenarioA.Time.AfterLogin <- Sys.time()

#===  Load Raw Data Set  =======================================================

  CCP.LoadRawDataSet(ServerSpecifications = ServerSpecifications)

  ScenarioA.Time.AfterLoading <- Sys.time()


  for (CurrentSampleSize in ScenarioA.SampleSizes)
  {
      #---  Draw RDS Samples  --------------------------------------------------

      ds.CCP.DrawSample(RawDataSetName = "CCP.RawDataSet",
                        SampleSize = CurrentSampleSize,
                        SampleName = "RDSSample",
                        DSConnections = ScenarioA.CCPConnections)

      Time.AfterSampleDrawing <- Sys.time()


      #---  Data Curation  -----------------------------------------------------

      ds.CurateData(RawDataSetName = "RDSSample",
                    Module = "CCP",
                    OutputName = "CCP.CurationOutput",
                    RunAssignmentChecks = FALSE,
                    RunSeparately = FALSE,
                    DSConnections = ScenarioA.CCPConnections)

      Time.AfterCuration <- Sys.time()

      # Get curation messages (more lightweight then full Curation Report)
      CurationMessages <- DSI::datashield.aggregate(conns = ScenarioA.CCPConnections,
                                                    expr = call("GetReportingObjectDS",
                                                                ObjectName.S = "Messages"),
                                                    async = DS.async)

      CurationDurations <- CurationMessages %>%
                                map(\(X) X$Process.Duration) %>%
                                list_rbind(names_to = "Server")

      #---  Data Augmentation  -------------------------------------------------

      # Run ds.AugmentData
      # ds.CCP.AugmentData(CuratedDataSetName = "CCP.CuratedDataSet",
      #                    OutputName = "CCP.AugmentationOutput",
      #                    RunAssignmentChecks = FALSE,
      #                    RunSeparately = RunSeparately)
      #
      # Time.AfterAugmentation <- Sys.time()

  }


#===============================================================================
# Test scenario B)
#   - Varying number of servers, these are randomly selected
#   - Fixed sample size
#===============================================================================


  # Create table of timed durations
  # PerformanceMonitor <- tibble(Time.Initialization = Time.Initial,
  #                              Duration.Loading = as.double(lubridate::as.duration(Time.AfterLoading - Time.Initial)),
  #                              Duration.Curation = as.double(lubridate::as.duration(Time.AfterCuration - Time.AfterLoading)),
  #                              Duration.Augmentation = as.double(lubridate::as.duration(Time.AfterAugmentation - Time.AfterCuration)),
  #                              Duration.ReportAkquisition = as.double(lubridate::as.duration(Time.AfterReportAkquisition - Time.AfterAugmentation)),
  #                              Duration.WorkspaceSaving = as.double(lubridate::as.duration(Time.AfterWorkspaceSaving - Time.AfterReportAkquisition)))

  # Add PerformanceMonitor to 'Report' list
  # Report <- c(Report,
  #             list(PerformanceMonitor))

#-------------------------------------------------------------------------------
  return(Report)
}
