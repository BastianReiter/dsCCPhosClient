
#' CheckServerRequirements
#'
#' Check if technical requirements are met on every participating CCP server.
#'
#' @param CCPSiteSpecifications \code{data.frame} | Same data frame used for login. Used here only for akquisition of site-specific project names (in case they are differing). | Default: NULL for virtual project
#' @param RequiredPackages A \code{character vector} naming required packages
#' @param RequiredFunctions A named \code{character vector} containing names of required functions. Their type ('aggregate' or 'assign') is defined by the correspondent element names.
#' @param DataSources List of DSConnection objects
#'
#' @return A list of data frames containing gathered info and messages
#' @export
#'
#' @author Bastian Reiter
CheckServerRequirements <- function(CCPSiteSpecifications = NULL,
                                    RequiredPackages = c("dsBase",
                                                         "dsCCPhos",
                                                         "dsTidyverse"),
                                    RequiredFunctions = c(aggregate = "GetReportingObjectDS",
                                                          assign = "AugmentDataDS",
                                                          assign = "CurateDataDS",
                                                          assign = "ExtractFromListDS",
                                                          assign = "DrawSampleDS"),
                                    DataSources = NULL)
{
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Package requirements
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    require(dplyr)
    require(DSI)
    require(tidyr)

    ### For testing purposes
    # DataSources <- CCPConnections
    # RequiredPackages = c("dsBase", "dsCCPhos")
    # RequiredFunctions = c(aggregate = "GetReportingObjectDS",
    #                       assign = "AugmentDataDS",
    #                       assign = "CurateDataDS",
    #                       assign = "ExtractFromListDS")


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
    RequiredPackageAvailability <- tibble(PackageName = RequiredPackages) %>%
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

    # Transform / Transpose data frame into more handy return object
    RequiredPackageAvailability <- RequiredPackageAvailability %>%
                                        select(-IsAvailableEverywhere,
                                               -NotAvailableAt) %>%
                                        pivot_longer(!PackageName,
                                                     names_to = "SiteName",
                                                     values_to = "IsAvailable") %>%
                                        pivot_wider(names_from = PackageName,
                                                    values_from = IsAvailable) %>%
                                        mutate(CheckPackageAvailability = case_when(if_all(-SiteName, ~ .x == TRUE) ~ "green",
                                                                                    TRUE ~ "red"))



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

    # Transform / Transpose data frame into more handy return object
    VersionOfdsCCPhos <- VersionOfdsCCPhos %>%
                              pivot_longer(everything(),
                                           names_to = "SiteName",
                                           values_to = "dsCCPhosVersion")



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
    RequiredFunctionAvailability <- data.frame(FunctionName = RequiredFunctions,
                                               FunctionType = names(RequiredFunctions)) %>%
                                        left_join(FunctionAvailability, by = join_by(FunctionName == name, FunctionType == type)) %>%
                                        rowwise() %>%
                                        mutate(across(all_of(ServerNames), ~ ifelse(is.na(.), FALSE, .)),      # Replace NA values with FALSE. NAs are introduced when a required function is not listed in 'FunctionAvailability'.
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

    # Transform / Transpose data frame into more handy return object
    RequiredFunctionAvailability <- RequiredFunctionAvailability %>%
                                        select(-FunctionType,
                                               -IsAvailableEverywhere,
                                               -NotAvailableAt) %>%
                                        pivot_longer(!FunctionName,
                                                     names_to = "SiteName",
                                                     values_to = "IsAvailable") %>%
                                        pivot_wider(names_from = FunctionName,
                                                    values_from = IsAvailable) %>%
                                        mutate(CheckFunctionAvailability = case_when(if_all(-SiteName, ~ .x == TRUE) ~ "green",
                                                                                     TRUE ~ "red"))


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Availability of Opal data base tables on servers
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get info on Opal table availability with dsCCPhosClient::GetServerOpalDBInfo()
    RequiredOpalTableAvailability <- GetServerOpalInfo(CCPSiteSpecifications,
                                                       DataSources)

    # Compile output message concerning one table each and add it to Messages
    for (i in 1:nrow(RequiredOpalTableAvailability))
    {
        Row <- RequiredOpalTableAvailability[i, ]

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

    # Transform / Transpose data frame into more handy return object
    RequiredOpalTableAvailability <- RequiredOpalTableAvailability %>%
                                        select(-IsAvailableEverywhere,
                                               -NotAvailableAt) %>%
                                        pivot_longer(!TableName,
                                                     names_to = "SiteName",
                                                     values_to = "IsAvailable") %>%
                                        pivot_wider(names_from = TableName,
                                                    values_from = IsAvailable) %>%
                                        mutate(CheckOpalTableAvailability = case_when(if_all(-SiteName, ~ .x == TRUE) ~ "green",
                                                                                      TRUE ~ "red"))



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Print messages and return list object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Print messages on console
    PrintMessages(Messages)

    # Return list containing data frames of gathered info and messages
    return(list(PackageAvailability = RequiredPackageAvailability,
                VersionOfdsCCPhos = VersionOfdsCCPhos,
                FunctionAvailability = RequiredFunctionAvailability,
                OpalTableAvailability = RequiredOpalTableAvailability,
                Messages = Messages))
}
