
#' LoadRawDataSet
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Load raw data set from Opal data base into R session on servers.
#'
#' @param ServerSpecifications \code{data.frame} - Same \code{data.frame} used for login. Used here only for acquisition of server-specific project names (in case they are differing) - Default: \code{NULL} for virtual project
#' @param OpalTableNames.Mapping \code{character vector} - Usually the expected/required Opal data base table names. Can optionally be a named vector, with the names being expected names of the Opal data base tables and the vector values being the names of the imported R tables / data.frames.
#' @param OpalTableNames.Dictionary Optional \code{list} of named \code{character vectors} - To enable server-specific mapping of deviating to required Opal data base table names. Names of list elements must match server names. For rules that should be applied on all servers, choose form \code{list(All = c('LookupName' = 'RequiredName'))}.
#' @param RunAssignmentChecks \code{logical} Indicating whether assignment checks should be performed or omitted for reduced execution time - Default: \code{TRUE}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of messages
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LoadRawDataSet <- function(ServerSpecifications = NULL,
                           OpalTableNames.Mapping = setNames(dsCCPhosClient::Meta.Tables$TableName.Curated, nm = dsCCPhosClient::Meta.Tables$TableName.Raw),
                           OpalTableNames.Dictionary = list(All = setNames(dsCCPhosClient::Meta.Tables$TableName.Raw, nm = dsCCPhosClient::Meta.Tables$TableName.Curated)),      # Include a dictionary mapping curated to raw Opal table names, because some servers might already have adopted 'curated' table names, while others have not.
                           RunAssignmentChecks = TRUE,
                           DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # ServerSpecifications <- NULL
  # OpalTableNames.Mapping <- setNames(dsCCPhosClient::Meta.Tables$TableName.Curated,
  #                                    nm = dsCCPhosClient::Meta.Tables$TableName.Raw)
  # OpalTableNames.Dictionary <- NULL
  # RunAssignmentChecks <- TRUE
  # DSConnections <- CCPConnections

  # --- Argument Validation ---
  assert_that(is.character(OpalTableNames.Mapping),
              is.flag(RunAssignmentChecks))
  if (!is.null(ServerSpecifications)) { is.data.frame(ServerSpecifications) }
  if (!is.null(OpalTableNames.Dictionary)) { is.list(OpalTableNames.Dictionary) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- dsFredaClient::CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Initiate output messaging objects
  Messages <- list()
  if (RunAssignmentChecks == TRUE) { Messages$Assignment <- c(Topic = "Object assignment on servers") }

  # Get server names
  ServerNames <- names(DSConnections)

  # If argument 'OpalTableNames.Mapping' is an unnamed vector, set vector values as names
  if (is.null(names(OpalTableNames.Mapping))) { names(OpalTableNames.Mapping) <- OpalTableNames.Mapping }

  # Check Opal table availability
  OpalDBInfo <- dsFredaClient::GetServerOpalDBInfo(ServerSpecifications = ServerSpecifications,
                                                   OpalTableNames.Required = names(OpalTableNames.Mapping),
                                                   OpalTableNames.Dictionary = OpalTableNames.Dictionary,
                                                   DSConnections = DSConnections)

#-------------------------------------------------------------------------------
# Assignment in R server sessions
#-------------------------------------------------------------------------------

  # Loop through all participating servers
  for (i in 1:length(ServerNames))
  {
      # Create data.frame containing mapping from Opal DB table names to R object names
      OpalDBToR <- OpalDBInfo$OpalTables.Available %>%
                        filter(Server == ServerNames[i],
                               IsAvailable == TRUE,
                               IsRequired == TRUE) %>%
                        mutate(RTableName = case_when(OpalTableName.Generic %in% names(OpalTableNames.Mapping) ~ paste0("RDS.", OpalTableNames.Mapping[OpalTableName.Generic]),
                                                      .default = paste0("RDS.", OpalTableName.Generic)))

      # Loop through available Opal DB tables and assign their content to objects (data.frames) in R session
      for (j in 1:nrow(OpalDBToR))
      {
          DSI::datashield.assign(conns = DSConnections[[i]],
                                 symbol = OpalDBToR$RTableName[j],
                                 value = OpalDBToR$OpalTableName[j],
                                 id.name = "_id")

          # Add message about Opal data base to R session mapping
          Messages$Assignment <- c(Messages$Assignment,
                                   Info = paste0("Server '", names(DSConnections)[i], "': Mapped Opal table '", OpalDBToR$OpalTableName[j], "' to data.frame '", OpalDBToR$RTableName[j], "'."))
      }
  }


#-------------------------------------------------------------------------------
# Check if assignment on servers succeeded
#-------------------------------------------------------------------------------

  if (RunAssignmentChecks == TRUE)
  {
      BundledMessages <- list()

      # Loop through all tables to get info about assignment on servers
      for(tablename in OpalTableNames.Mapping)
      {
          # Make sure assignment was successful on all servers
          ObjectStatus_Table <- dsFredaClient::ds.GetObjectStatus(ObjectName = paste0("RDS.", tablename),
                                                                  DSConnections = DSConnections)

          # Add info about table assignment to Messages
          BundledMessages <- c(BundledMessages,
                               ObjectStatus_Table["ObjectValidity"])   # Must select list element 'ObjectValidity' this way to keep naming of vector and thus class 'Success', 'Warning' and so forth
      }

      # Turn list into (named) vector and add it to Messages
      Messages$Assignment <- c(Messages$Assignment,
                               list_c(BundledMessages))
  }



#-------------------------------------------------------------------------------
# Assign list 'RawDataSet' on all servers
#-------------------------------------------------------------------------------

  # Create list of vectors (one for each server) containing names of actually existing data.frames
  ExistingRDSTables <- paste0("RDS.", OpalTableNames.Mapping) %>%
                            map(function(tablename)
                                {
                                    if (!is.na(tablename))
                                    {
                                        unlist(dsBaseClient::ds.exists(x = tablename, datasources = DSConnections))
                                    } else {
                                        return(NULL)
                                    }
                                }) %>%
                            set_names(paste0("RDS.", OpalTableNames.Mapping)) %>%
                            list_transpose() %>%
                            map(\(TableNames) names(TableNames[TableNames == TRUE]))

  # For every server, consolidate all existing Raw Data Set tables in one list object called "RawDataSet"
  ExistingRDSTables %>%
      iwalk(function(RDSTableNames, servername)
            {
                # Note: Tables within list 'RawDataSet' are named WITHOUT prefix 'RDS.'
                dsFredaClient::ds.MakeList(ObjectNames = setNames(object = RDSTableNames,
                                                                  nm = str_remove(RDSTableNames, "RDS.")),
                                           OutputName = "RawDataSet",
                                           DSConnections = DSConnections[servername])
           })

  if (RunAssignmentChecks == TRUE)
  {
      # Make sure assignment of RawDataSet was successful on all servers
      ObjectStatus_RawDataSet <- dsFredaClient::ds.GetObjectStatus(ObjectName = "RawDataSet",
                                                                   DSConnections = DSConnections)

      # Add info about RawDataSet assignment to Messages
      Messages$Assignment <- c(Messages$Assignment,
                               ObjectStatus_RawDataSet$ObjectValidity)
  }


#--- Print and invisibly return Messages object --------------------------------

  # Print messages on console
  dsFredaClient::PrintMessages(Messages)

  # Return Messages invisibly
  invisible(Messages)
}
