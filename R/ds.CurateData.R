
#' ds.CurateData
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Triggers transformation of Raw Data Set (RDS) into Curated Data Set (CDS) on server.
#'
#' Linked to server-side ASSIGN methods dsCCPhos::CurateDataDS() and dsCCPhos::ExtractFromListDS()
#'
#' @param RawDataSetName \code{character} - Name of Raw Data Set object (list) on server - Default: 'RawDataSet'
#' @param Settings \code{list} - Settings passed to function
#'                 \itemize{\item DataHarmonization \code{list}
#'                              \itemize{\item RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_DataHarmonization}
#'                                       \item Profile \code{character} - Profile name defining rule set to be used for data harmonization. Profile name must be stated in \code{DataHarmonization$RuleSet} - Default: 'Default'}
#'                          \item FeatureObligations \code{list}
#'                              \itemize{\item RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_FeatureObligations}
#'                                       \item Profile \code{character} - Profile name defining strict and trans-feature rules for obligatory feature content. Profile name must be stated in \code{FeatureObligations$RuleSet} - Default: 'Default'}
#'                          \item FeatureTracking \code{list}
#'                              \itemize{\item RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_FeatureTracking}
#'                                       \item Profile \code{character} - Profile name defining which features should be tracked/monitored during curation process. Profile name must be stated in \code{FeatureTracking$RuleSet} - Default: 'Default'}}
#' @param OutputName \code{character} - Name of output object to be assigned on server - Default: 'CurationOutput'
#' @param DataSources \code{list} of DSConnection objects
#'
#' @return \code{list} of following objects:
#'         \itemize{\item 'Messages' - Info messages concerning completion of \code{CurateDataDS()} and assignment of the following objects on server:
#'                        \itemize{\item CurationOutput (\code{list})
#'                                    \itemize{ \item CuratedDataSet \code{list}
#'                                                \itemize{ \item BioSampling
#'                                                          \item Diagnosis
#'                                                          \item DiseaseStatus
#'                                                          \item GeneralCondition
#'                                                          \item Histology
#'                                                          \item Metastasis
#'                                                          \item MolecularDiagnostics
#'                                                          \item OtherClassification
#'                                                          \item Patient
#'                                                          \item RadiationTherapy
#'                                                          \item Staging
#'                                                          \item Surgery
#'                                                          \item SystemicTherapy
#'                                                          \item TherapyRecommendation}
#'                                              \item CurationReport \code{list}
#'                                                \itemize{\item EntryCounts \code{tibble}
#'                                                          \itemize{ \item Table
#'                                                                    \item InitialCount
#'                                                                    \item ExcludedPrimary
#'                                                                    \item AfterPrimaryExclusion
#'                                                                    \item ExcludedSecondary
#'                                                                    \item AfterSecondaryExclusion
#'                                                                    \item ExcludedSecondaryRedundancy
#'                                                                    \item AfterSecondaryRedundancyExclusion}
#'                                                          \item Transformation (list of lists)
#'                                                            \itemize{ \item Monitors
#'                                                                      \item EligibilityOverviews
#'                                                                      \item ValueSetOverviews}}
#'                                              \item CurationMessages \code{list}}}
#'                  \item 'CurationCompletionCheck'}
#' @export
#'
#' @author Bastian Reiter
ds.CurateData <- function(RawDataSetName = "RawDataSet",
                          Settings = list(DataHarmonization = list(Profile = "Default"),
                                          FeatureObligations = list(Profile = "Default"),
                                          FeatureTracking = list(Profile = "Default")),
                          OutputName = "CurationOutput",
                          DataSources = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    # Look for DS connections
    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }

    # Ensure DataSources is a list of DSConnection-class
    if (!(is.list(DataSources) && all(unlist(lapply(DataSources, function(d) {methods::is(d,"DSConnection")})))))
    {
        stop("'DataSources' were expected to be a list of DSConnection-class objects", call. = FALSE)
    }


    ### For testing purposes only
    # RawDataSetName <- "RawDataSet"
    # OutputName <- "CurationOutput"
    # DataSources <- CCPConnections



    # Initiate output messaging objects
    Messages <- list()
    Messages$Assignment <- list()
    Messages$CurationCompletion <- list()


    # 1) Trigger dsCCPhos::CurateDataDS()
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Construct the server-side function call
    ServerCall <- call("CurateDataDS",
                       RawDataSetName.S = RawDataSetName,
                       Settings.S = Settings)

    # Execute the server-side function call
    DSI::datashield.assign(conns = DataSources,
                           symbol = OutputName,
                           value = ServerCall)

    # Call helper function to check if assignment of CurationOutput succeeded
    Messages$Assignment <- c(Messages$Assignment,
                             ds.GetObjectStatus(ObjectName = OutputName,
                                                DataSources = DataSources))



    # 2) Extract objects from list returned by CurateDataDS() and assign them to R server sessions
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    CurationOutputObjects <- c("CuratedDataSet",
                               "CurationReport",
                               "CurationMessages")

    for(i in 1:length(CurationOutputObjects))
    {
        # Construct the server-side function call
        ServerCall <- call("ExtractFromListDS",
                           ListName.S = OutputName,
                           ObjectName.S = CurationOutputObjects[i])

        # Execute server-side assign function
        DSI::datashield.assign(conns = DataSources,
                               symbol = CurationOutputObjects[i],
                               value = ServerCall)

        # Call helper function to check if object assignment succeeded
        Messages$Assignment <- c(Messages$Assignment,
                                 ds.GetObjectStatus(ObjectName = CurationOutputObjects[i],
                                                    DataSources = DataSources))
    }

    # Turn list into (named) vector
    Messages$Assignment <- purrr::list_c(Messages$Assignment)

    # Add topic element to start of vector
    Messages$Assignment <- c(Topic = "Object assignment on servers",
                             Messages$Assignment)



    # 3) Get CurationMessages objects from servers (as a list of lists) and create completion check object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ServerCall <- call("GetReportingObjectDS",
                       ObjectName.S = "CurationMessages")

    CurationMessages <- DSI::datashield.aggregate(conns = DataSources,
                                                  expr = ServerCall)

    # Create table object for output
    CurationCompletionCheck <- CurationMessages %>%
                                  map(\(SiteMessages) tibble(CheckCurationCompletion = SiteMessages$CheckCurationCompletion) ) %>%
                                  list_rbind(names_to = "SiteName")

    # Create vector of messages informing about curation completion
    Messages$CurationCompletion <- CurationMessages %>%
                                      imap(function(SiteMessages, sitename)
                                           {
                                              case_when(SiteMessages$CheckCurationCompletion == "green" ~ MakeFunctionMessage(Text = paste0("Curation on server '", sitename, "' performed successfully!"),
                                                                                                                              IsClassSuccess = TRUE),
                                                        SiteMessages$CheckCurationCompletion == "yellow" ~ MakeFunctionMessage(Text = paste0("Curation on server '", sitename, "' performed with warnings! \n",
                                                                                                                                             SiteMessages$FinalMessage),
                                                                                                                               IsClassWarning = TRUE),
                                                        SiteMessages$CheckCurationCompletion == "red" ~ MakeFunctionMessage(Text = paste0("Curation on server '", sitename, "' could not be performed! \n",
                                                                                                                                          SiteMessages$FinalMessage),
                                                                                                                            IsClassFailure = TRUE),
                                                        TRUE ~ MakeFunctionMessage(Text = paste0("Curation on server '", sitename, "' could not be assessed. \n",
                                                                                                 SiteMessages$FinalMessage),
                                                                                   IsClassFailure = TRUE))
                                           }) %>%
                                      list_c()

    # Add topic element to start of vector
    Messages$CurationCompletion <- c(Topic = "Curation process completion",
                                     Messages$CurationCompletion)



    # Print messages and return output
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Print messages on console
    PrintMessages(Messages)

    # Return Messages and Curation completion check object
    return(list(Messages = Messages,
                CurationCompletionCheck = CurationCompletionCheck))
}
