FG_pull<-function(Name){
  
  suppressWarnings(suppressMessages(file.remove("tempfile.csv")))
  outpath = paste(getwd(),"/tempfile.csv.gz",sep="")
  
  command = paste("dbuddy pull filegroups",outpath,"--FileGroup",Name)
    
  system(command)
    
  file = read.csv(outpath)
    
  file.remove(outpath)
    
  return(file)
}

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

data_modify<-function(data,table){
 
  suppressWarnings(suppressMessages(file.remove("tempfile.csv")))
  
  write.csv(data,"tempfile.csv",row.names = FALSE)
  
  tempfilename= paste(getwd(),"/tempfile.csv",sep="")
  
  command = paste("dbuddy modify",table,tempfilename)
  
  system(command)
  
  file.remove("tempfile.csv")
  
}

data_pull<-function(query){
  
  suppressWarnings(suppressMessages(file.remove("tempfile.csv.gz")))
  
  outpath = paste(getwd(),"/tempfile.csv.gz",sep="")
  
  command = paste("dbuddy pull direct",outpath,shQuote(query))
  
  system(command)
  
  file = read.csv(outpath)
  
  file.remove(outpath)
  
  return(file)
}

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

#FGdat<-read.csv(paste(folderName,SfilesName,sep="/"))

#GTfileName<-paste("//akc0ss-n086/NMML_CAEP_Acoustics/Detector/RavenBLEDscripts/Data/Selection tables/",Species,"/",data$MooringName[1],"Sum/",saveName,".txt",sep="")
#GTdata<-read.delim(GTfileName,row.names=NULL)

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

