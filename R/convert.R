#conversions of old (pre-NAS) 'Sfiles' to FG and GTfiles to GT. 



#FGdat<-read.csv(paste(folderName,SfilesName,sep="/"))

#GTfileName<-paste("//akc0ss-n086/NMML_CAEP_Acoustics/Detector/RavenBLEDscripts/Data/Selection tables/",Species,"/",data$MooringName[1],"Sum/",saveName,".txt",sep="")
#GTdata<-read.delim(GTfileName,row.names=NULL)

#' Convert old effort data to filegroup 
#'
#' Using old (pre-NAS) effort data, convert to FG format. 
#' @param data 'Sfiles' type effort data as data frame
#' @param oldMooringName The mooring name of the old effort data. 
#' @param source AFSC or DCLDE- different standards
#' @return FG dataset
#' @export
FGconvert<-function(data,oldMooringName,source){
  
  data$SFsh<-gsub("_", "-", data$SFsh)
  
  #for the outData, need to calculate difference to actual file start (sec in last 3 digits)
  mss<-paste("0",substr(data$SFsh,nchar(data$SFsh)-6,nchar(data$SFsh)-4),sep="")
  vals<-as.POSIXlt(mss,format="%M%S")
  vals<- vals$sec+vals$min*60
  
  dash2<-gregexpr("-",data$SFsh[1])[[1]][2]
  dot1<-gregexpr("\\.",data$SFsh[1])[[1]][1]
  dateTimeFormat<-substr(data$SFsh,dash2+1,dot1-1)
  
  if(source!="DCLDE2013"){
    if(!any(!nchar(dateTimeFormat)==15)){
      dateTimeFormat<-paste(substr(dateTimeFormat,3,12),"000",sep="")
    }
  }else{
    dateTimeFormat<-substr(dateTimeFormat,3,15)
  }
  
  data$SFsh<-paste(substr(data$SFsh,1,dash2),dateTimeFormat,".wav",sep="")
  
  und3<-gregexpr("_",data$MooringName[1])[[1]][2]
  siteID<-substr(data$MooringName,und3+1,nchar(as.character(data$MooringName[1])))
  
  outData<-data.frame(cbind(as.character(data$SFsh),"/",dateTimeFormat,data$Duration,as.character(data$MooringName),siteID,vals))
  colnames(outData)<-c("FileName","FullPath","StartTime","Duration","Deployment","SiteID","pngFileDiff")
  
  outData$Duration<-as.numeric(as.character(outData$Duration))
  
  outData$FileName<-as.character(outData$FileName)
  
  if(source!="DCLDE2013"){
    outData$pngFileDiff<-vals
  }else{
    outData$pngFileDiff<-0
  }
  #und5<-gregexpr("_",SfilesName)[[1]][5]
  #saveName<-substr(SfilesName,1,und5-1)
  outData$pngFileDiff<-as.numeric(outData$pngFileDiff)
  
  outData$cumsum<- cumsum(outData$Duration)-outData$Duration[1]
  outData$cumsum<- c(0,cumsum(outData$Duration)[1:(nrow(outData)-1)])
  
  outData$FileName = waveRename(outData$FileName,oldMooringName,lookup)
  
  return(outData)
  
}

#' Convert old effort data to filegroup 
#'
#' Using old (pre-NAS) effort data, convert to FG format. 
#' @param GTdata The mooring name of the old effort data.
#' @param FGdat The data of type 'Sfile' corresponding to the GT data. 
#' @param Species The two letter code corresponding to species/call type (RW, GS, etc)
#' @param labtype i_neg, det, etc.
#' @param vhz visible hz range of analysis (accepts 'max' as well as numeric)
#' @param a_id Integer ID for the analysis (must correspond to a record in 'analysis' table on the database)
#' @param source AFSC or DCLDE- different standards
#' @return FG dataset
#' @export
GTconvert <-function(GTdata,FGdat,Species,oldMooringName,labtype,vhz,a_id,source="AFSC"){
  
lookup<-read.csv("./mooring_name_lookup_edit.csv")
  
outData = FGconvert(FGdat,oldMooringName,source)

#load in the GT data 

GTdata<-GTdata[which(GTdata$View=="Spectrogram 1"),]
GTdata$Begin.Time..s.<-as.numeric(GTdata$Begin.Time..s.)
GTdata$End.Time..s.<-as.numeric(GTdata$End.Time..s.)

#outData$cumsum<- cumsum(outData$Duration)

#this is a super jenk way to do this but this part doesn't need to scale so I don't care

values<-foreach(i=1:nrow(GTdata)) %do% {
  #for(i in 1:nrow(GTdata)){
  startFile<-"UK"
  k=1
  while(startFile=="UK"&k<=nrow(outData)){
    if(GTdata$Begin.Time..s.[i]<outData$cumsum[k]){
      startFile<-outData$FileName[k-1]
      startTime<-GTdata$Begin.Time..s.[i]-outData$cumsum[k-1]
      startTime2<-startTime+outData$pngFileDiff[k-1]
    }else{
      k=k+1
    }
  }
  if(k>nrow(outData)){
    startFile<-outData$FileName[k-1]
    startTime<-GTdata$Begin.Time..s.[i]-outData$cumsum[k-1]
    startTime2<-startTime+outData$pngFileDiff[k-1]
  }
  endFile<-"UK"
  k=1
  while(endFile=="UK"&k<=nrow(outData)){
    if(GTdata$End.Time..s.[i]<outData$cumsum[k]){
      endFile<-outData$FileName[k-1]
      endTime<-GTdata$End.Time..s.[i]-outData$cumsum[k-1]
      endTime2<-endTime+outData$pngFileDiff[k-1]
    }else{
      k=k+1
    }
  }
  if(k>nrow(outData)){
    endFile<-outData$FileName[k-1]
    endTime<-GTdata$End.Time..s.[i]-outData$cumsum[k-1]
    endTime2<-endTime+outData$pngFileDiff[k-1]
    
  }
  
  return(c(startTime2,endTime2,startFile,endFile))
  
}

values<-do.call("rbind",values)

GTout<-data.frame(values[,c(1,2),drop = F],GTdata$Low.Freq..Hz.,GTdata$High.Freq..Hz.,values[,c(3,4),drop = F])

colnames(GTout)<-c("StartTime","EndTime","LowFreq","HighFreq","StartFile","EndFile")
#print(GTdata)
GTout$probs<-GTdata[,paste(Species,'prob',sep=".")]
GTout$label<-GTdata$Verification
GTout$Type<-labtype
GTout$SignalCode<-Species
GTout$VisibleHz<-vhz
GTout$Analysis_ID<-as.integer(a_id)
GTout$Comments <-GTdata$Comments

if(any(is.na(GTout$Comments))){
  GTout$Comments[which(is.na(GTout$Comments))]<-""
}

GTout[,1]<-as.numeric(GTout[,1])
GTout[,2]<-as.numeric(GTout[,2])

#rename waves: should work for everything. 

GTout$StartFile<-waveRename(GTout$StartFile,oldMooringName,lookup)
GTout$EndFile<-waveRename(GTout$EndFile,oldMooringName,lookup)

return(GTout)

}


#' Take a filegroup with arbitrary bin length and break it into SC size sections. 
#'
#' Using old (pre-NAS) effort data, convert to FG format. 
#' @param data A dataframe of type FG
#' @param binlen length in seconds of bin- standard are 300 (LOW) 225 (REG) and 90 (SHI). Other values will be marked as 'custom'
#' @return FG dataset
#' @export
breakbins<-function(data,binlen){
  
  #condition: if the row has a higher duration than the max of the bins, break it into multiple until condition is satisfied
  
  data$index<-1:nrow(data)
  
  newrows<-foreach(n=1:nrow(data)) %do% {
    #for(n in 1:nrow(data)){
    row = data[which(data$index==n),]
    #go row by row. if duration > binlen, expand row, delete old row, and stich back in. 
    if(row$SegDur>binlen){
      
      #uh oh - think I found a bug in here... 4/7/22. that means that FGs and binsFGs are suspect on the db!!
      #the case was a row which had a length of 375 that started on 225, being split into mids- it split this case into 0-225 and 225-375
      
      if(row$SegDur%%binlen==0){
        binints = rep(binlen,row$SegDur/binlen)
        binstarts = c(0,cumsum(binints)) + row$SegStart  #adding segstart is potential bugfix. needs to be tested. added on 4/7/22- all on database didn't use this!!
      }else{
        secs = sum(rep(binlen,row$SegDur/binlen))
        binints = c(rep(binlen,row$SegDur/binlen),row$SegDur-secs)
        binstarts= c(0,cumsum(binints)) + row$SegStart  #adding segstart is potential bugfix. needs to be tested. added on 4/7/22- all on database didn't use this!!
      }
      startends<-matrix(nrow=length(binints),ncol=2)
      for(p in 1:length(binints)){
        startends[p,]<-c(binstarts[p],binints[p])
      }
      
      #nice, so now have intervals. create the rows
      rows = data.frame(row[,1:5],startends,row[,8:9])
    }else{
      rows = row
    }
    #remove original row. easier to do it here. 
    data=data[-which(data$index==n),]
    
    colnames(rows)[c(6,7)]<-c("SegStart","SegDur")
    
    Standard_lengths = c(300,225,90) #LMH
    Standard_Names = c("LOW","REG","SHI")
    if(binlen %in% Standard_lengths){
      binName = Standard_Names[which(Standard_lengths==binlen)]
    }else{
      binName = "CUSTOM"
    }

    rows$Type = binName
    #check that bin is standard bin, and if not, relabel as CUSTOM bin. 
    
    for(n in nrow(rows)){
      
      STANDARD = TRUE
      #if start does not equal a multiple of bin size, not standard
      if(rows[n,]$SegStart%%binlen!=0){
        STANDARD = FALSE
      }
      if(!(rows[n,]$SegDur%%binlen==0 | (rows[n,]$SegStart+rows[n,]$SegDur)==rows[n,]$Duration)){
        STANDARD = FALSE
      }
      
      if(!STANDARD){
        rows[n,]$Type<-"CUSTOM"
      }
    }
    
    return(rows)
    
  }
  newrows =do.call("rbind",newrows)
  
  #if(!is.null(newrows)){
  #  colnames(newrows)[c(6,7)]<-c("SegStart","SegDur")
  #  data=rbind(newrows,data)
  #}
  
  #data<-data[order(data$index,data$SegStart),] #original order
  
  return(newrows)
  
}

waveRename <-function(wavvec,oldMooringName){
  
  lookup<-read.csv("./mooring_name_lookup_edit.csv")
  
  if((oldMooringName %in% lookup$Old.B.drive.name)){
    own=lookup[which(lookup$Old.B.drive.name==oldMooringName),"Old.wave.name"]
    nwn=lookup[which(lookup$Old.B.drive.name==oldMooringName),"New.wave.name"]
    
    oldYY = gregexpr("YY",own)[[1]][1]
    newYY = gregexpr("YY",nwn)[[1]][1]-1
    
    newprefix = substr(nwn,1,newYY)
    
    if(substr(wavvec[1],1,nchar(newprefix))!=newprefix){
      
      wavvec<-paste(newprefix,substr(wavvec,oldYY,nchar(wavvec)),sep="")
    }
    
    
  }
  
  return(wavvec)
}
#modify columns for database.
#' @import foreach
NULL
