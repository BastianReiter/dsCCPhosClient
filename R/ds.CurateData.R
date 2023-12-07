
#' ds.CurateData
#'
#' What it does
#'
#' Linked to server-side ASSIGN method CurateDataDS()
#'
#' @param Name_RawData String | Name of raw data object (list) on server | Default: 'RawData'
#' @param Name_Output String | Name of assigned output object on server | Default: 'CurationOutput'
#' @param DataSources
#'
#' @return
#' @export
#'
#' @examples
ds.CurateData <- function(Name_RawData = "RawData",
                          Name_Output = "CurationOutput",
                          DataSources = NULL)
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

    if (is.null(Name_RawData))
    {
        stop("Please provide the name of the raw data object!", call. = FALSE)
    }


    # Construct the the server-side function call
    ServerCall <- call("CurateDataDS", Name_RawData)

    DSI::datashield.assign(conns = DataSources,
                           symbol = Name_Output,
                           value = ServerCall)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CHECK if all servers answered correctly using dsBase::testObjExistsDS()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# test.obj.name <- Name_Output
#
# # Call server-side test function
# CallPhrase <- call("testObjExistsDS", test.obj.name)
#
# object.info <- DSI::datashield.aggregate(DataSources, CallPhrase)
#
# # CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS
# # AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS
# num.datasources <- length(object.info)
#
#
# obj.name.exists.in.all.sources <- TRUE
# obj.non.null.in.all.sources <- TRUE
#
# for(j in 1:num.datasources){
# 	if(!object.info[[j]]$test.obj.exists){
# 		obj.name.exists.in.all.sources<-FALSE
# 		}
# 	if(is.null(object.info[[j]]$test.obj.class) || ("ABSENT" %in% object.info[[j]]$test.obj.class)){
# 		obj.non.null.in.all.sources<-FALSE																 	#
# 		}																								 	#
# 	}																									 	#
# 																											#
# if(obj.name.exists.in.all.sources && obj.non.null.in.all.sources){										 	#
# 																											#
# 	return.message<-																					 	#
#     paste0("A data object <", test.obj.name, "> has been created in all specified data sources")		 	#
# 																											#
# 																											#
# 	}else{																								 	#
# 																											#
#     return.message.1<-																					 	#
# 	paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources")	#
# 																											#
# 	return.message.2<-																					 	#
# 	paste0("It is either ABSENT and/or has no valid content/class,see return.info above")				 	#
# 																											#
# 	return.message.3<-																					 	#
# 	paste0("Please use ds.ls() to identify where missing")												 	#
# 																											#
# 																											#
# 	return.message<-list(return.message.1,return.message.2,return.message.3)							 	#
# 																											#
# 	}																										#
# 																											#
# 	calltext <- call("messageDS", test.obj.name)															#
#     studyside.message<-DSI::datashield.aggregate(datasources, calltext)											#
# 																											#
# 	no.errors<-TRUE																							#
# 	for(nd in 1:num.datasources){																			#
# 		if(studyside.message[[nd]]!="ALL OK: there are no studysideMessage(s) on this datasource"){			#
# 		no.errors<-FALSE																					#
# 		}																									#
# 	}																										#
# 																											#
# 																											#
# 	if(no.errors){																							#
# 	validity.check<-paste0("<",test.obj.name, "> appears valid in all sources")							    #
# 	return(list(is.object.created=return.message,validity.check=validity.check))						    #
# 	}																										#
# 																											#
# if(!no.errors){																								#
# 	validity.check<-paste0("<",test.obj.name,"> invalid in at least one source. See studyside.messages:")   #
# 	return(list(is.object.created=return.message,validity.check=validity.check,					    		#
# 	            studyside.messages=studyside.message))			                                            #
# 	}

}
