
#' CheckServerRequirements
#'
#' Check if technical requirements are met on every participating CCP server.
#'
#' @param DataSources List of DSConnection objects
#' @param ServerRequirements A list of data frames as defined in \code{\link{Meta_ServerRequirements}}.
#'
#' @return A list of messages
#' @export
#'
#' @examples
#' @author Bastian Reiter
CheckServerRequirements <- function(DataSources = NULL,
                                    ServerRequirements = dsCCPhosClient::Meta_ServerRequirements)
{
    require(dplyr)
    require(DSI)
    require(tidyr)

    # For testing purposes
    # DataSources <- CCPConnections
    # ServerRequirements <- dsCCPhosClient::Meta_ServerRequirements

    # Initiate output messaging objects
    Messages <- list()
    Messages$PackageAvailability <- character()
    Messages$VersionOfdsCCPhos <- character()
    Messages$FunctionAvailability <- character()

    # Get server names
    ServerNames <- names(DataSources)


    # Package availability on servers
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get info about installed server packages
    PackageAvailability <- as_tibble(DSI::datashield.pkg_status(conns = DataSources)$package_status,
                                     rownames = "PackageName")

    # Check if defined set of packages is available on all servers
    RequiredPackageAvailability <- ServerRequirements$RequiredPackages %>%
                                        left_join(PackageAvailability, by = join_by(PackageName)) %>%
                                        rowwise() %>%
                                        mutate(across(ServerNames, ~ ifelse(is.na(.), FALSE, .)),      # Replace NA values with FALSE. NAs are introduced when a required package is not listed in 'PackageAvailability'.
                                               IsAvailableEverywhere = all(c_across(all_of(ServerNames)) == TRUE),
                                               NotAvailableAt = ifelse(IsAvailableEverywhere == FALSE,
                                                                       paste0(ServerNames[c_across(all_of(ServerNames)) == FALSE], collapse = ", "),
                                                                       NA)) %>%
                                        ungroup()

    # Compile output message concerning one package each and add it to Messages
    for (i in 1:nrow(RequiredPackageAvailability))
    {
        Row <- RequiredPackageAvailability[i, ]
        Message <- paste0("Package '",
                          Row$PackageName,
                          ifelse(Row$IsAvailableEverywhere == TRUE,
                                 "' is available on all servers!",
                                 paste0("' is not available on ", Row$NotAvailableAt)))
        Messages$PackageAvailability <- c(Messages$PackageAvailability,
                                          Message)
    }


    # Available version of dsCCPhos
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get version number of dsCCPhos on all servers and check for equality
    VersionOfdsCCPhos <- as_tibble(DSI::datashield.pkg_status(conns = DataSources)$version_status,
                                   rownames = "PackageName") %>%
                            filter(PackageName == "dsCCPhos") %>%
                            select(-PackageName)

    IsEqualEverywhere <- apply(VersionOfdsCCPhos, 1, function(Values) { all(Values == Values[1]) })

    if (IsEqualEverywhere == TRUE)
    {
        Messages$VersionOfdsCCPhos <- paste0("Version of dsCCPhos is equal on all servers (Ver. ", VersionOfdsCCPhos[1, 1], ")!")
    } else {
        Messages$VersionOfdsCCPhos <- paste0("Version of dsCCPhos varies between servers!")
        for (i in 1:ncol(VersionOfdsCCPhos))
        {
            Messages$VersionOfdsCCPhos <- c(Messages$VersionOfdsCCPhos,
                                            paste0(names(VersionOfdsCCPhos)[i], ": Ver. ", VersionOfdsCCPhos[, i]))
        }
    }



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
        Message <- paste0("Function '",
                          Row$FunctionName,
                          ifelse(Row$IsAvailableEverywhere == TRUE,
                                 "' is available on all servers!",
                                 paste0("' is not available on ", Row$NotAvailableAt)))
        Messages$FunctionAvailability <- c(Messages$FunctionAvailability,
                                           Message)
    }


    # Return statement
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Additionally to return, print messages on console
    cat(paste0(unlist(Messages), collapse = "\n"))

    return(Messages)
}
