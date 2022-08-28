ConnectRiskServer = function(database='_adhoc_04', driver='SQL Server Native Client 11.0', 
    server, only_get_call=FALSE) {
 
  cstring = paste0("driver={",driver,"}; server=",server,"; database=",database,"; trusted_connection=yes")
 
  con = if(only_get_call) {
    paste0('RODBC::odbcDriverConnect(',cstring,')')
  } else {
    RODBC::odbcDriverConnect(cstring)
  }
 
  return(con)
 
}

GetOpenRODBCConnections = function(newest=TRUE, names=FALSE, envir=parent.frame()) {
 
  objs = ls(envir=envir)
  objs = objs[sapply(objs, function(x) 'RODBC' %in% class(get(x, envir=envir)))]
  objs = objs[sapply(objs, function(x) RODBC:::odbcValidChannel(get(x, envir=envir)))]
 
  if(newest) {
    timestamps = sapply(objs, function(x) as.vector(get(x, envir=envir)), simplify=TRUE)
    objs = objs[which.max(timestamps)]
  }
 
  cons = if(names) {
    objs
  } else if(length(objs)==1) {
    get(objs)
  } else {
    sapply(objs, get, simplify=FALSE)
  }
 
  return(cons)
 
}

CloseRiskServer = function(con) {

  if(missing(con) || is.null(con)) {
    RODBC::odbcCloseAll()
  } else {
    RODBC::odbcClose(con)
  }
 
}

QueryRS = function(query, con, as.is = FALSE, ...) {
  Query(con, query, as.is = as.is, ...)
}

QueryRSAsIs = function(query, con, as.is = TRUE, ...) {
  Query(con, query, as.is = as.is, ...)
} 