
#' ConnectToVirtualCCP
#'
#' Sets up a virtual dataSHIELD infrastructure that enables trying out real dsCCPhos functionality on test data.
#'
#' @param CCPTestData List of CCP test data
#' @param NumberOfSites Integer | The number of virtual sites to install
#' @param NumberOfPatientsPerSite Integer | Optional value to restrict size of data set for faster testing | Default: NULL
#'
#' @return A list of DSConnection-objects
#' @export
#'
#' @examples
#' @author Bastian Reiter
ConnectToVirtualCCP <- function(CCPTestData,
                                NumberOfSites = 1,
                                NumberOfPatientsPerSite = NULL)
{
    require(dplyr)
    require(DSLite)
    require(DSI)
    require(DSOpal)

    # For testing purposes
    # CCPTestData <- CCPTestData_Total
    # NumberOfSites <- 3
    # NumberOfPatientsPerSite <- 300

    # Check value of NumberOfSites
    if (NumberOfSites > 26) { stop("Maximum value for 'NumberOfSites' is 26.", call. = FALSE) }

    # Determine names of virtual sites (here: SiteA, SiteB, ...)
    SiteNames <- paste0("Site", LETTERS[1:NumberOfSites])

    # Returns an environment
    LoginBuilder <- DSI::newDSLoginBuilder(.silent = FALSE)

    # Calculate auxiliary variables
    AllPatientIDs <- CCPTestData$Patient$"_id"
    CountTotalPatients <- n_distinct(AllPatientIDs)
    PatientsPerSite <- floor(CountTotalPatients / NumberOfSites)

    # Check if NumberOfPatientsPerSite has a feasible value and adopt it for PatientsPerSite
    if (!is.null(NumberOfPatientsPerSite))
    {
        if (NumberOfPatientsPerSite > PatientsPerSite)
        {
            stop(paste0("Not enough patients in test data for entered 'NumberOfPatientsPerSite'. Proposal value is equal or lower than ", PatientsPerSite, ". Alternatively reduce 'NumberOfSites'."), call. = FALSE)
        } else {
            PatientsPerSite <- NumberOfPatientsPerSite
        }
    }


    for (i in 1:NumberOfSites)
    {
        # 1) Prepare site data
        #~~~~~~~~~~~~~~~~~~~~~~

        # Get a random sample of PatientIDs
        SitePatientIDs <- sample(AllPatientIDs, PatientsPerSite)

        # Get data subsets that relate to sampled PatientIDs
        SiteTestData <- list(as.data.frame(filter(CCPTestData$BioSampling, CCPTestData$BioSampling$"patient-id" %in% SitePatientIDs)),
                             as.data.frame(filter(CCPTestData$Diagnosis, CCPTestData$Diagnosis$"patient-id" %in% SitePatientIDs)),
                             as.data.frame(filter(CCPTestData$Histology, CCPTestData$Histology$"patient-id" %in% SitePatientIDs)),
                             as.data.frame(filter(CCPTestData$Metastasis, CCPTestData$Metastasis$"patient-id" %in% SitePatientIDs)),
                             as.data.frame(filter(CCPTestData$MolecularDiagnostics, CCPTestData$MolecularDiagnostics$"patient-id" %in% SitePatientIDs)),
                             as.data.frame(filter(CCPTestData$Patient, CCPTestData$Patient$"_id" %in% SitePatientIDs)),
                             as.data.frame(filter(CCPTestData$Progress, CCPTestData$Progress$"patient-id" %in% SitePatientIDs)),
                             as.data.frame(filter(CCPTestData$RadiationTherapy, CCPTestData$RadiationTherapy$"patient-id" %in% SitePatientIDs)),
                             as.data.frame(filter(CCPTestData$Staging, CCPTestData$Staging$"patient-id" %in% SitePatientIDs)),
                             as.data.frame(filter(CCPTestData$Surgery, CCPTestData$Surgery$"patient-id" %in% SitePatientIDs)),
                             as.data.frame(filter(CCPTestData$SystemicTherapy, CCPTestData$SystemicTherapy$"patient-id" %in% SitePatientIDs)))

        # Assign names to test data set tables (corresponding to real raw data on CCP servers)
        names(SiteTestData) <- dsCCPhosClient::Meta_TableNames$TableName_Raw


        # 2) Build virtual server in global environment
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        assign(x = paste0("Server_", SiteNames[i]),
               value = newDSLiteServer(tables = SiteTestData,
                                       config = DSLite::defaultDSConfiguration(include = c("dsBase",
                                                                                           "dsCCPhos"))),
               envir = .GlobalEnv)

        # 3) Add login data to login builder
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        LoginBuilder$append(server = SiteNames[i],
                            url = paste0("Server_", SiteNames[i]),
                            driver = "DSLiteDriver")


        # 3) Update AllPatientIDs: Delete used-up PatientIDs
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        AllPatientIDs <- AllPatientIDs[!(AllPatientIDs %in% SitePatientIDs)]
    }


    # Returns a data frame of login data
    LoginData <- LoginBuilder$build()

    # Get list of DSConnection objects of all servers
    CCPConnections <- DSI::datashield.login(logins = LoginData,
                                            assign = TRUE)

    return(CCPConnections)
}
