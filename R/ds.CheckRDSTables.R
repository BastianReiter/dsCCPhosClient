
#' ds.CheckRDSTables
#'
#' Check Raw Data Set (RDS) tables on servers on existence and completeness and return a coherent summary across servers.
#'
#' Linked to server-side AGGREGATE method CheckRDSTablesDS()
#'
#' @param DataSources List of DSConnection objects
#'
#' @return A list
#' @export
#'
#' @author Bastian Reiter
ds.CheckRDSTables <- function(DataSources = NULL,
                              RawDataSetName = "RawDataSet")
{
    require(dplyr)
    require(purrr)

    # For testing purposes
    #DataSources <- CCPConnections
    #RawDataSetName <- "RawDataSet"

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check argument eligibility
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (!(is.character(RawDataSetName)))
    {
        stop("Error: Argument 'RawDataSetName' must be a character string.", call. = FALSE)
    }

    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Server function call to get list of lists
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ServerCall <- call("CheckRDSTablesDS",
                       RawDataSetName.S = RawDataSetName)

    TableCheck <- DSI::datashield.aggregate(conns = DataSources,
                                            expr = ServerCall)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Transform into cumulated report objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Create data frame containing "traffic light" info about existence/completeness of RDS tables
    TableStatus <- TableCheck %>%
                        map(function(SiteTableCheck)
                            {
                                SiteTableStatus <- SiteTableCheck %>%
                                                      map_chr(function(TableInfo)
                                                              {
                                                                 case_when(TableInfo$TableExists == TRUE & TableInfo$TableComplete == TRUE ~ "green",
                                                                           TableInfo$TableExists == FALSE ~ "red",
                                                                           TableInfo$TableComplete == FALSE ~ "yellow",
                                                                           TRUE ~ "grey")
                                                              }) %>%
                                                      rbind() %>%
                                                      as_tibble()
                            }) %>%
                        list_rbind(names_to = "SiteName") %>%
                        mutate(CheckRDSTables = case_when(if_all(-SiteName, ~ .x == "green") ~ "green",
                                                          if_any(-SiteName, ~ .x == "red") ~ "red",
                                                          if_any(-SiteName, ~ .x == "yellow") ~ "yellow",
                                                          TRUE ~ "grey"))


    # Create list of data frames (one per RDS table) containing info about existence of table features
    FeatureStatus <- TableCheck %>%
                          list_transpose() %>%
                          map(function(TableInfo)
                              {
                                  TableInfo %>%
                                      map(\(SiteTableInfo) SiteTableInfo$FeatureExistence) %>%
                                      list_rbind(names_to = "SiteName") %>%
                                      pivot_wider(names_from = FeatureName,
                                                  values_from = Exists)
                              })


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Return statement
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(list(TableStatus = TableStatus,
                FeatureStatus = FeatureStatus))
}
