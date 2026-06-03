
#' ConnectToVirtualCCP
#'
#' Sets up a virtual DataSHIELD infrastructure that enables trying out real dsCCPhos functionality on test data.
#'
#' @param CCPData \code{Named list} of \code{data.frames} - CCP test data
#' @param CCPData.Mode \code{string} - One of:
#'                                      \itemize{ \item 'Single' - Data in 'CCPData' contains data from a single site (or test data) that can be distributed randomly between virtual servers
#'                                                \item 'Multi' - Data in 'CCPData' contains a list with multi-site data
#'                                                \item 'Auto' - Functions tries to determine from structure of 'CCPData' which mode should be used (Default) }
#' @param NumberOfServers \code{integer} - The number of virtual servers to install
#' @param NumberOfPatientsPerServer \code{integer} - Optional value to restrict size of data set for faster testing - Default: NULL
#' @param AddedDsPackages \code{character vector} - Server-side DataSHIELD packages to be added to default (dsBase, dsCCPhos) - Default: NULL
#' @param Resources \code{Named list} of \code{resourcer::Resource} objects - Default: NULL
#' @param WorkingDirectory \code{string} - Optional custom working directory for virtual servers - Default: Hidden folder in R session's temporary directory (see \code{?DSLite::newDSLiteServer()})
#'
#' @return A \code{list} of \code{DSConnection}-objects
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ConnectToVirtualCCP <- function(CCPData,
                                CCPData.Mode = "Auto",
                                NumberOfServers = 1,
                                NumberOfPatientsPerServer = NULL,
                                AddedDsPackages = NULL,
                                Resources = NULL,
                                WorkingDirectory = file.path(tempdir(), ".dslite"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # CCPData <- TestData
  # CCPData <- readRDS("../Data/CCP/PanCancerDFCI/CCPData/TestData.PanCancerDFCI_1.rds")
  # CCPData.Mode <- "Auto"
  # NumberOfServers <- 3
  # NumberOfPatientsPerServer <- 1000
  # AddedDsPackages <- NULL
  # Resources <- NULL
  # WorkingDirectory <- file.path(tempdir(), ".dslite")
  # Resources <- list(TestResource = resourcer::newResource(name = "TestResource",
  #                                                         url = "file:///Development/Test/TestResource.csv",
  #                                                         format = "csv"))

  # --- Argument Validation ---
  assert_that(is.string(CCPData.Mode),
              is.count(NumberOfServers),
              is.string(WorkingDirectory))
  if (!is.null(NumberOfPatientsPerServer)) { assert_that(is.count(NumberOfPatientsPerServer)) }
  if (!is.null(AddedDsPackages)) { assert_that(is.character(AddedDsPackages)) }
  if (!(CCPData.Mode %in% c("Auto", "Single", "Multi"))) { stop("ERROR: Value of argument 'CCPData.Mode' must be one of 'Auto' / 'Single' / 'Multi'.") }
  # Limit value of NumberOfServers to 26 (= Letters in alphabet)
  if (NumberOfServers > 26) { stop("Maximum value for 'NumberOfServers' is 26.", call. = FALSE) }

#-------------------------------------------------------------------------------

  # Returns an environment
  LoginBuilder <- DSI::newDSLoginBuilder(.silent = FALSE)


  # Try to determine data mode from structure of 'CCPData'
  if (CCPData.Mode == "Auto")
  {
      if (all(names(CCPData) %in% dsCCPhosClient::Meta.Tables$TableName.Raw) | all(names(CCPData) %in% dsCCPhosClient::Meta.Tables$TableName.Curated))
      {
          CCPData.Mode <- "Single"
          dsFredaClient::PrintSoloMessage(c(Info = "Set data mode to 'Single', assuming single-site CCP data."))

      } else if (all(names(CCPData[[1]]) %in% dsCCPhosClient::Meta.Tables$TableName.Raw) | all(names(CCPData[[1]]) %in% dsCCPhosClient::Meta.Tables$TableName.Curated)) {

          CCPData.Mode <- "Multi"
          dsFredaClient::PrintSoloMessage(c(Info = "Set data mode to 'Multi', assuming multi-site CCP data."))

      } else {

          stop("ERROR: Can not determine structure of passed 'CCPData' in 'Auto' mode. Please specify 'Mode' or check 'CCPData'.")
      }
  }


  # Create distinct server data from single-site data passed in 'CCPData'
  if (CCPData.Mode == "Single")
  {
      # Determine names of virtual servers (here: ServerA, ServerB, ...)
      ServerNames <- paste0("Server", LETTERS[1:NumberOfServers])

      AllPatientIDs <- NULL

      # Get values from column holding patient IDs
      if ("Patient" %in% names(CCPData) && "PatientID" %in% names(CCPData$Patient))
      {
          AllPatientIDs <- CCPData$Patient$PatientID

      } else if ("patient" %in% names(CCPData) && "_id" %in% names(CCPData$patient)) {

          AllPatientIDs <- CCPData$patient$"_id"
      }

      CountTotalPatients <- n_distinct(AllPatientIDs)
      PatientsPerServer <- floor(CountTotalPatients / NumberOfServers)

      # Check if NumberOfPatientsPerServer has a feasible value and adopt it for PatientsPerServer
      if (!is.null(NumberOfPatientsPerServer))
      {
          if (NumberOfPatientsPerServer > PatientsPerServer)
          {
              stop(paste0("Not enough patients in 'CCPData' for entered 'NumberOfPatientsPerServer'. Proposal value is equal or lower than ", PatientsPerServer, ". Alternatively reduce 'NumberOfServers'."), call. = FALSE)
          } else {
              PatientsPerServer <- NumberOfPatientsPerServer
          }
      }

      ServerData <- list()

      # Get distinct server data for every virtual server
      for (i in 1:NumberOfServers)
      {
          # Get a random sample of PatientIDs
          ServerPatientIDs <- sample(AllPatientIDs,
                                     size = PatientsPerServer)

          # Get data subsets that relate to sampled PatientIDs
          ServerData[[ServerNames[i]]] <- CCPData %>%
                                              map(function(Table)
                                              {
                                                  if (length(Table) == 0)
                                                  {
                                                      return(NULL)

                                                  } else {

                                                      if ("patient-id" %in% names(Table))
                                                      {
                                                          return(Table %>% filter(.data[["patient-id"]] %in% ServerPatientIDs) %>% as.data.frame())

                                                      } else if ("PatientID" %in% names(Table)) {

                                                          return(Table %>% filter(.data[["PatientID"]] %in% ServerPatientIDs) %>% as.data.frame())

                                                      } else if ("_id" %in% names(Table)) {

                                                          return(Table %>% filter(.data[["_id"]] %in% ServerPatientIDs) %>% as.data.frame())

                                                      } else { return(Table) }
                                                  }
                                              })

          # Update AllPatientIDs: Delete used-up PatientIDs
          AllPatientIDs <- AllPatientIDs[!(AllPatientIDs %in% ServerPatientIDs)]
      }

  } else if (CCPData.Mode == "Multi") {

      ServerData <- CCPData
  }


  # Build virtual servers in global environment
  #-----------------------------------------------------------------------------
  for (i in 1:length(ServerData))
  {
      assign(x = paste0("Server_", names(ServerData)[i]),
             value = DSLite::newDSLiteServer(tables = ServerData[[i]],
                                             resources = Resources,
                                             config = DSLite::defaultDSConfiguration(include = c("dsBase",
                                                                                                 "resourcer",
                                                                                                 "dsCCPhos",
                                                                                                 "dsFreda",
                                                                                                 AddedDsPackages)),
                                             home = WorkingDirectory),
             envir = .GlobalEnv)

      # Add login data to login builder
      LoginBuilder$append(server = names(ServerData)[i],
                          url = paste0("Server_", names(ServerData)[i]),
                          driver = "DSLiteDriver")
  }


  # Returns a data.frame of login data
  LoginData <- LoginBuilder$build()

  # Get list of DSConnection objects of all servers
  CCPConnections <- DSI::datashield.login(logins = LoginData,
                                          assign = TRUE,
                                          opts = list(low_speed_time = 0, low_speed_limit = 0))

#-------------------------------------------------------------------------------
  return(CCPConnections)
}
