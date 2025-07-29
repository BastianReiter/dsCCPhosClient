
#' ConnectToVirtualCCP
#'
#' Sets up a virtual DataSHIELD infrastructure that enables trying out real dsCCPhos functionality on test data.
#'
#' @param CCPTestData \code{Named list} of \code{data.frames} - CCP test data
#' @param NumberOfSites \code{integer} - The number of virtual sites to install
#' @param NumberOfPatientsPerSite \code{integer} - Optional value to restrict size of data set for faster testing - Default: NULL
#' @param AddedDsPackages \code{character vector} - Server-side DataSHIELD packages to be added to default (dsBase, dsCCPhos) - Default: NULL
#' @param Resources \code{Named list} of \code{resourcer::Resource} objects - Default: NULL
#' @param WorkingDirectory \code{string} - Optional custom working directory for virtual servers - Default: Hidden folder in R session's temporary directory (see \code{?DSLite::newDSLiteServer()})
#'
#' @return A \code{list} of \code{DSConnection}-objects
#' @export
#'
#' @author Bastian Reiter
ConnectToVirtualCCP <- function(CCPTestData,
                                NumberOfSites = 1,
                                NumberOfPatientsPerSite = NULL,
                                AddedDsPackages = NULL,
                                Resources = NULL,
                                WorkingDirectory = file.path(tempdir(), ".dslite"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(DSLite)
    require(DSI)
    require(DSOpal)

    ### For testing purposes
    # CCPTestData <- TestData
    # NumberOfSites <- 3
    # NumberOfPatientsPerSite <- 1000
    # AddedDsPackages <- NULL
    # Resources <- list(TestResource = resourcer::newResource(name = "TestResource",
    #                                                         url = "file:///Development/Test/TestResource.csv",
    #                                                         format = "csv"))

    # Check value of NumberOfSites
    if (NumberOfSites > 26) { stop("Maximum value for 'NumberOfSites' is 26.", call. = FALSE) }

    # Determine names of virtual sites (here: SiteA, SiteB, ...)
    SiteNames <- paste0("Site", LETTERS[1:NumberOfSites])

    # Returns an environment
    LoginBuilder <- DSI::newDSLoginBuilder(.silent = FALSE)

    # Calculate auxiliary variables
    AllPatientIDs <- CCPTestData$patient$"_id"
    # AllPatientIDs <- CCPTestData$Patient$PatientID
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
        #~~~~~~~~~~~~~~~~~~~~~

        # Get a random sample of PatientIDs
        SitePatientIDs <- sample(AllPatientIDs,
                                 size = PatientsPerSite)

        # Get data subsets that relate to sampled PatientIDs
        SiteTestData <- list(sample = as.data.frame(filter(CCPTestData$sample, CCPTestData$sample$"patient-id" %in% SitePatientIDs)),
                             diagnosis = as.data.frame(filter(CCPTestData$diagnosis, CCPTestData$diagnosis$"patient-id" %in% SitePatientIDs)),
                             GeneralPerformance = NULL,
                             histology = as.data.frame(filter(CCPTestData$histology, CCPTestData$histology$"patient-id" %in% SitePatientIDs)),
                             metastasis = as.data.frame(filter(CCPTestData$metastasis, CCPTestData$metastasis$"patient-id" %in% SitePatientIDs)),
                             "molecular-marker" = NULL,
                             OtherClassification = NULL,
                             patient = as.data.frame(filter(CCPTestData$patient, CCPTestData$patient$"_id" %in% SitePatientIDs)),
                             progress = as.data.frame(filter(CCPTestData$progress, CCPTestData$progress$"patient-id" %in% SitePatientIDs)),
                             "radiation-therapy" = as.data.frame(filter(CCPTestData$"radiation-therapy", CCPTestData$"radiation-therapy"$"patient-id" %in% SitePatientIDs)),
                             tnm = as.data.frame(filter(CCPTestData$tnm, CCPTestData$tnm$"patient-id" %in% SitePatientIDs)),
                             surgery = as.data.frame(filter(CCPTestData$surgery, CCPTestData$surgery$"patient-id" %in% SitePatientIDs)),
                             "system-therapy" = as.data.frame(filter(CCPTestData$"system-therapy", CCPTestData$"system-therapy"$"patient-id" %in% SitePatientIDs)),
                             TherapyRecommendation = NULL)

        # # Get data subsets that relate to sampled PatientIDs
        # SiteTestData <- list(BioSampling = NULL,
        #                      Diagnosis = as.data.frame(filter(CCPTestData$Diagnosis, CCPTestData$Diagnosis$PatientID %in% SitePatientIDs)),
        #                      GeneralPerformance = NULL,
        #                      Histology = as.data.frame(filter(CCPTestData$Histology, CCPTestData$Histology$PatientID %in% SitePatientIDs)),
        #                      Metastasis = as.data.frame(filter(CCPTestData$Metastasis, CCPTestData$Metastasis$PatientID %in% SitePatientIDs)),
        #                      MolecularDiagnostics = NULL,
        #                      OtherClassification = NULL,
        #                      Patient = as.data.frame(filter(CCPTestData$Patient, CCPTestData$Patient$PatientID %in% SitePatientIDs)),
        #                      Progress = as.data.frame(filter(CCPTestData$Progress, CCPTestData$Progress$PatientID %in% SitePatientIDs)),
        #                      RadiationTherapy = as.data.frame(filter(CCPTestData$RadiationTherapy, CCPTestData$RadiationTherapy$PatientID %in% SitePatientIDs)),
        #                      Staging = as.data.frame(filter(CCPTestData$Staging, CCPTestData$Staging$PatientID %in% SitePatientIDs)),
        #                      Surgery = as.data.frame(filter(CCPTestData$Surgery, CCPTestData$Surgery$PatientID %in% SitePatientIDs)),
        #                      SystemicTherapy = as.data.frame(filter(CCPTestData$SystemicTherapy, CCPTestData$SystemicTherapy$PatientID %in% SitePatientIDs)),
        #                      TherapyRecommendation = NULL)

        # # *** TEMPORARY fix *** until table names are clear
        # names(SiteTestData) <- c("sample",
        #                          "diagnosis",
        #                          "GeneralPerformance",
        #                          "histology",
        #                          "metastasis",
        #                          "molecular-marker",
        #                          "OtherClassification",
        #                          "patient",
        #                          "progress",
        #                          "radiation-therapy",
        #                          "tnm",
        #                          "surgery",
        #                          "system-therapy",
        #                          "TherapyRecommendation")


        # 2) Build virtual server in global environment
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        assign(x = paste0("Server_", SiteNames[i]),
               value = newDSLiteServer(tables = SiteTestData,
                                       resources = Resources,
                                       config = DSLite::defaultDSConfiguration(include = c("dsBase",
                                                                                           "resourcer",
                                                                                           "dsCCPhos",
                                                                                           AddedDsPackages)),
                                       home = WorkingDirectory),
               envir = .GlobalEnv)


        # 3) Add login data to login builder
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        LoginBuilder$append(server = SiteNames[i],
                            url = paste0("Server_", SiteNames[i]),
                            driver = "DSLiteDriver")


        # 4) Update AllPatientIDs: Delete used-up PatientIDs
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        AllPatientIDs <- AllPatientIDs[!(AllPatientIDs %in% SitePatientIDs)]
    }


    # Returns a data.frame of login data
    LoginData <- LoginBuilder$build()

    # Get list of DSConnection objects of all servers
    CCPConnections <- DSI::datashield.login(logins = LoginData,
                                            assign = TRUE)

    return(CCPConnections)
}
