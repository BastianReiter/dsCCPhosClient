
#' CheckServerRequirements
#'
#' Check if technical requirements are met on every participating CCP server.
#'
#' @param CCPSiteSpecifications \code{data.frame} | Same data frame used for login. Used here only for akquisition of site-specific project names (in case they are differing). | Default: NULL for virtual project
#' @param ServerRequirements A list of data frames as defined in \code{\link{Meta_ServerRequirements}}.
#' @param DataSources List of DSConnection objects
#'
#' @return A list of messages
#' @export
#'
#' @examples
#' @author Bastian Reiter
CheckServerRequirements <- function(CCPSiteSpecifications = NULL,
                                    ServerRequirements = dsCCPhosClient::Meta_ServerRequirements,
                                    DataSources = NULL)
{
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check argument eligibility
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Package requirements
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    require(dplyr)
    require(DSI)
    require(tidyr)

    # For testing purposes
    # DataSources <- CCPConnections
    # ServerRequirements <- dsCCPhosClient::Meta_ServerRequirements

    # Initiate output messaging objects
    Messages <- list()
    Messages$PackageAvailability <- c(Topic = "Package availability")
    Messages$VersionOfdsCCPhos <- c(Topic = "Version of dsCCPhos")
    Messages$FunctionAvailability <- c(Topic = "Function availability")
    Messages$TableAvailability <- c(Topic = "Opal DB table availability")

    # Get server names (sorted alphabetically)
    ServerNames <- sort(names(DataSources))



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Package availability on servers
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get info about installed server packages
    PackageAvailability <- as_tibble(DSI::datashield.pkg_status(conns = DataSources)$package_status,
                                     rownames = "PackageName")

    # Check if defined set of packages is available on all servers
    RequiredPackageAvailability <- ServerRequirements$RequiredPackages %>%
                                        left_join(PackageAvailability, by = join_by(PackageName)) %>%
                                        rowwise() %>%
                                        mutate(across(all_of(ServerNames), ~ ifelse(is.na(.), FALSE, .)),      # Replace NA values with FALSE. NAs are introduced when a required package is not listed in 'PackageAvailability'.
                                               IsAvailableEverywhere = all(c_across(all_of(ServerNames)) == TRUE),
                                               NotAvailableAt = ifelse(IsAvailableEverywhere == FALSE,
                                                                       paste0(ServerNames[c_across(all_of(ServerNames)) == FALSE], collapse = ", "),
                                                                       NA)) %>%
                                        ungroup()

    # Compile output message concerning one package each and add it to Messages
    for (i in 1:nrow(RequiredPackageAvailability))
    {
        Row <- RequiredPackageAvailability[i, ]

        # Note: It's important to use 'dplyr::if_else()' instead of 'ifelse' here, otherwise the return won't be a named vector
        Message <- if_else(Row$IsAvailableEverywhere == TRUE,
                           MakeFunctionMessage(Text = paste0("Function '",
                                                             Row$PackageName,
                                                             "' is available on all servers!"),
                                               IsClassSuccess = TRUE),
                           MakeFunctionMessage(Text = paste0("Package '",
                                                             Row$PackageName,
                                                             "' is not available at ",
                                                             Row$NotAvailableAt),
                                               IsClassFailure = TRUE))

        Messages$PackageAvailability <- c(Messages$PackageAvailability,
                                          Message)
    }



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Available version of dsCCPhos
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get version number of dsCCPhos on all servers and check for equality
    VersionOfdsCCPhos <- as_tibble(DSI::datashield.pkg_status(conns = DataSources)$version_status,
                                   rownames = "PackageName") %>%
                            filter(PackageName == "dsCCPhos") %>%
                            select(-PackageName)

    if (nrow(VersionOfdsCCPhos > 0))
    {
        IsEqualEverywhere <- apply(VersionOfdsCCPhos, 1, function(Values) { all(Values == Values[1]) })
        MessageOverall <- NULL
        MessagesDetail <- NULL

        if (IsEqualEverywhere == TRUE)
        {
            MessageOverall <- MakeFunctionMessage(Text = paste0("Version of dsCCPhos is equal on all servers (Ver. ", VersionOfdsCCPhos[1, 1], ")!"),
                                                  IsClassSuccess = TRUE)
        }
        else
        {
            MessagesOverall <- MakeFunctionMessage(Text = paste0("Version of dsCCPhos varies between servers!"),
                                                   IsClassWarning = TRUE)

            for (i in 1:ncol(VersionOfdsCCPhos))
            {
                MessagesDetail <- c(Messages$VersionOfdsCCPhos,
                                    MakeFunctionMessage(Text = paste0(names(VersionOfdsCCPhos)[i], ": Ver. ", VersionOfdsCCPhos[, i]),
                                                        IsClassInfo = TRUE))
            }
        }

        Messages$VersionOfdsCCPhos <- c(Messages$VersionOfdsCCPhos,
                                        MessageOverall,
                                        MessagesDetail)
    }



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Function availability on servers
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get coherent data frame of general function availability on all servers
    FunctionAvailability <- rbind(# Get data frame of available AGGREGATE functions
                                  DSI::datashield.method_status(conns = DataSources,
                                                                type = "aggregate"),
                                  # Get data frame of available ASSIGN functions
                                  DSI::datashield.method_status(conns = DataSources,
                                                                type = "assign"))

    # Check if defined set of required functions is available on all servers
    RequiredFunctionAvailability <- ServerRequirements$RequiredFunctions %>%
                                        left_join(FunctionAvailability, by = join_by(FunctionName == name, FunctionType == type)) %>%
                                        rowwise() %>%
                                        mutate(across(ServerNames, ~ ifelse(is.na(.), FALSE, .)),      # Replace NA values with FALSE. NAs are introduced when a required function is not listed in 'FunctionAvailability'.
                                               IsAvailableEverywhere = all(c_across(all_of(ServerNames)) == TRUE),
                                               NotAvailableAt = ifelse(IsAvailableEverywhere == FALSE,
                                                                       paste0(ServerNames[c_across(all_of(ServerNames)) == FALSE], collapse = ", "),
                                                                       NA)) %>%
                                        ungroup()

    # Compile output message concerning one function each and add it to Messages
    for (i in 1:nrow(RequiredFunctionAvailability))
    {
        Row <- RequiredFunctionAvailability[i, ]

        # Note: It's important to use 'dplyr::if_else()' instead of 'ifelse' here, otherwise the return won't be a named vector
        Message <- if_else(Row$IsAvailableEverywhere == TRUE,
                           MakeFunctionMessage(Text = paste0("Function '",
                                                             Row$FunctionName,
                                                             "' is available on all servers!"),
                                               IsClassSuccess = TRUE),
                           MakeFunctionMessage(Text = paste0("Function '",
                                                             Row$FunctionName,
                                                             "' is not available at ",
                                                             Row$NotAvailableAt),
                                               IsClassWarning = TRUE))

        Messages$FunctionAvailability <- c(Messages$FunctionAvailability,
                                           Message)
    }



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Availability of Opal data base tables on servers
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get info on Opal table availability with dsCCPhosClient::GetServerOpalDBInfo()
    RequiredTableAvailability <- GetServerOpalInfo(CCPSiteSpecifications,
                                                   DataSources)

    # Compile output message concerning one table each and add it to Messages
    for (i in 1:nrow(RequiredTableAvailability))
    {
        Row <- RequiredTableAvailability[i, ]

        # Note: It's important to use 'dplyr::if_else()' instead of 'ifelse' here, otherwise the return won't be a named vector
        Message <- if_else(Row$IsAvailableEverywhere == TRUE,
                           MakeFunctionMessage(Text = paste0("Opal data base table '",
                                                             Row$TableName,
                                                             "' is available on all servers!"),
                                               IsClassSuccess = TRUE),
                           MakeFunctionMessage(Text = paste0("Opal data base table '",
                                                             Row$TableName,
                                                             "' is not available at ",
                                                             Row$NotAvailableAt),
                                               IsClassWarning = TRUE))

        Messages$TableAvailability <- c(Messages$TableAvailability,
                                        Message)
    }



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Print and return Messages object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Print messages on console
    PrintMessages(Messages)

    # Return messages
    return(Messages)
}
