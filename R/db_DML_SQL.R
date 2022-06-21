#DML and SQL operations. 

#' Insert data into the database
#'
#' Insert data into a named database table. 
#' @param data The data table object in R session
#' @param table The destination data table name in the database
#' @return a dataframe of newly created ids corresponding to the imported data. 
#' @export
data_insert<-function(data,table){
  
  #write data to temp file
  suppressWarnings(suppressMessages(file.remove("tempfile.csv")))
  write.csv(data,"tempfile.csv",row.names = FALSE)
  
  tempfilename= paste(getwd(),"/tempfile.csv",sep="")
  
  command = paste("dbuddy insert",table,tempfilename)
  
  if(table=="detections"){
    file.remove("ids.csv.gz")
  }
  
  if(table=="detections"){
    idsname = paste(getwd(),"/ids.csv.gz",sep="")
    command = paste(command,"--out",idsname)
  }  

  system(command)
  
  if(table=="detections"){
    return(read.csv(idsname))
    file.remove("ids.csv.gz")
  }
  
  file.remove("tempfile.csv")
  
}

#' Filegroup Pull
#'
#' Pull effort from a filegroup from the database by name. Uses INSTINCT format. 
#' @param Name the name of the filegroup
#' @return the filegroup dataframe object
#' @export
FG_pull<-function(Name){
  
  suppressWarnings(suppressMessages(file.remove("tempfile.csv")))
  outpath = paste(getwd(),"/tempfile.csv.gz",sep="")
  
  command = paste("dbuddy pull filegroups",outpath,"--FileGroup",Name)
  
  system(command)
  
  file = read.csv(outpath)
  
  file.remove(outpath)
  
  return(file)
}

#' Insert a filegroup into the database
#'
#' Insert a filegroup into the database by providing the filegroup table and some table metadata 
#' @param FG the filegroup object (specification:)
#' @param Name the name of the filegroup
#' @param SelectionMethod the SelectionMethod of the filegroups (ex: random, semi-random, procedural, hand)
#' @param Description a short text description of the filegroup. 
#' @return the filegroup dataframe object
#' @export
FG_insert<-function(FG,Name,SelectionMethod,Description){
  
  suppressWarnings(suppressMessages(file.remove("tempfile.csv")))
  # write.csv(data,"tempfile.csv",row.names = FALSE)
  
  write.csv(FG,"tempfile.csv",row.names = FALSE)
  
  tempfilename= paste(getwd(),"/tempfile.csv",sep="")
  
  command = paste("dbuddy insert filegroups",tempfilename,Name,
                  "--SelectionMethod",SelectionMethod,"--Description",Description)
  
  system(command)
  
  file.remove("tempfile.csv")
  
}

#' Modify an existing database table. 
#'
#' Modify data into a named database table. This method uses keys to match to existing data, which it deletes and replaces upon matches
#' @param data The data table object in R session
#' @param table The destination data table name in the database
#' @return not sure lol
#' @export
data_modify<-function(data,table){
  
  suppressWarnings(suppressMessages(file.remove("tempfile.csv")))
  
  write.csv(data,"tempfile.csv",row.names = FALSE)
  
  tempfilename= paste(getwd(),"/tempfile.csv",sep="")
  
  command = paste("dbuddy modify",table,tempfilename)
  
  system(command)
  
  file.remove("tempfile.csv")
  
}

#' Pull data from query
#'
#' Pull data from the database using an SQL query (currently SQLite syntax)
#' @param query The full query
#' @return a dataframe
#' @export
data_pull<-function(query){
  
  suppressWarnings(suppressMessages(file.remove("tempfile.csv.gz")))
  
  outpath = paste(getwd(),"/tempfile.csv.gz",sep="")
  
  command = paste("dbuddy pull direct",outpath,shQuote(query))
  
  system(command)
  
  file = read.csv(outpath)
  
  file.remove(outpath)
  
  return(file)
}

#' Delete records from table. 
#'
#' Delete records from a named table using the id of the data. 
#' @param data The data table object in R session
#' @param table The destination data table name in the database
#' @return not sure lol
#' @export
data_delete<-function(data,table){
  
  #write data to temp file
  suppressWarnings(suppressMessages(file.remove("tempfile.csv")))
  write.csv(data,"tempfile.csv",row.names = FALSE)
  
  tempfilename= paste(getwd(),"/tempfile.csv",sep="")
  
  command = paste("dbuddy delete",table,tempfilename)
  
  system(command)
  
  file.remove(tempfilename)
  #return IDs from data in the case of detections. 
  
}

#' @import utils
NULL
