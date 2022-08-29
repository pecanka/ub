ConnectServer = function(database, server, driver='SQL Server Native Client 11.0', only_get_call=FALSE) {
 
  cstring = paste0("driver={",driver,"}; server=",server,"; database=",database,"; trusted_connection=yes")
 
  if(only_get_call) {
    paste0('RODBC::odbcDriverConnect(',cstring,')')
  } else {
    RODBC::odbcDriverConnect(cstring)
  }
 
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

DisconnectServer = function(con) {

  if(missing(con) || is.null(con)) {
    RODBC::odbcCloseAll()
  } else {
    RODBC::odbcClose(con)
  }
 
}

QueryRS = function(query, con, as.is = FALSE, ...) {
  RODBC::sqlQuery(con, query, as.is = as.is, ...)
}

QueryRSAsIs = function(query, con, as.is = TRUE, ...) {
  RODBC::sqlQuery(con, query, as.is = as.is, ...)
} 