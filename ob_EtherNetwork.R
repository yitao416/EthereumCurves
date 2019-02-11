source("00_functions.R")

EtherNetwork <- R6Class("EtherNetwork",list(
  modelName = NULL,
  networkDF = NULL,
  origFrq = NULL,
  afterFrq = NULL,
  trimed = FALSE,
  A = NA,
  B = NA,
  a = 1,
  b = 10,
  periodList = NULL,
  orgInfo = NULL,
  topInfo = NULL,
  tkOutFolder =NULL,

  initialize = function(modelName, fileLoc, timeFormat,outFolder) {
    self$modelName <- modelName
    self$networkDF <- read_delim(fileLoc," ",col_names = FALSE,col_types =cols(.default = col_double())) %>% 
      setNames(.,c("from","to","time","value"))
    self$networkDF$time <- self$networkDF$time %>% anydate() %>% format(.,timeFormat) %>% as.factor()
    self$origFrq = table(self$networkDF$time)
    self$tkOutFolder = paste0(outFolder,modelName,"/")
    dir.create(self$tkOutFolder,showWarnings = FALSE)
    
  },
  dropExtreme = function(percentage = 1/500){
    if (self$trimed == FALSE) {
      rankdrop = nrow(self$networkDF)*percentage
      self$A = sort(self$networkDF$value)[rankdrop]
      self$B = sort(self$networkDF$value,decreasing = T)[rankdrop]
      
      self$networkDF <- subset(self$networkDF,value>self$A) %>% subset(.,value<self$B)
      self$afterFrq <- table(self$networkDF$time)
      self$trimed = TRUE
    }else{
      warning("The data set has already been trimmed.")
    }
    
    invisible(self)
  },
  record = function(){
    fileLoc = paste0(self$tkOutFolder,"dataSummary.txt")
    sink(fileLoc)
    print("Original:")
    print(self$origFrq)
    
    print(sprintf("Valid range: [%.2e, %.2e]",self$A,self$B))
    print("After drop extreme value:")
    print(self$afterFrq)
    print("Value summary:")
    print(summary(self$networkDF$value))
    sink()
    invisible(self)
    
  },
  procOrignal = function(recordFolder,distFolder){
    recordFolder = paste0(self$tkOutFolder,recordFolder)
    distFolder = paste0(self$tkOutFolder,distFolder)

    dir.create(recordFolder,showWarnings=FALSE)
    dir.create(distFolder,showWarnings=FALSE)
    
    self$periodList = self$networkDF$time %>% unique()
    totalInfo <- c()
    sink(paste0(recordFolder,"log.txt"))
    for (cur in self$periodList) {
      periodDF <- subset(self$networkDF,time==cur)
      # if (nrow(periodDF)<100) {
      #   next
      # }
      print(cur)
      periodDF <- selectTop(periodDF,topsize = nrow(self$networkDF))
      
      png(paste0(recordFolder,self$modelName,cur,".png"),width = 1200,height = 800)
      par(mfrow=c(1,2))
      gInfo <- graphInfo(periodDF[,c("from","to")],periodDF$value2,plotBefore = T,plotAfter = T,
                     extraTitle = paste(self$modelName,cur))
      dev.off()
      
      gInfo  <- bind_cols(time=cur,gInfo)
      totalInfo <- bind_rows(totalInfo,gInfo)
      
      DD <- coreDist(periodDF[,c("from","to")],periodDF$value2,plotBefore = F,plotAfter = F,
                     extraTitle = paste(self$modelName,cur))
      write.table(DD,file=paste0(distFolder,self$modelName,cur,".csv"),sep = ";",
                  row.names = FALSE,col.names = FALSE,eol = ";\n",append = FALSE)
      
    }
    sink()
    # self$info <- totalInfo
    write_csv(totalInfo,paste0(recordFolder,"tradSummary.csv"))
    self$orgInfo <- totalInfo
    invisible(self)
    
  },
  normalValue = function(){
    self$networkDF$value2 = lapply(self$networkDF$value, FUN = function(x) 1/(1+(x-self$A)*(self$b-self$a)/(self$B-self$A))) %>% 
      as.numeric() %>% round(.,digits = 5)
    invisible(self)
  },
  proc = function(graphFolder,distFolder,toprank = 150){
    graphFolder = paste0(self$tkOutFolder,graphFolder)
    distFolder = paste0(self$tkOutFolder,distFolder)

    dir.create(graphFolder,showWarnings = FALSE)
    dir.create(distFolder,showWarnings = FALSE)

    self$periodList = self$networkDF$time %>% unique()
    totalInfo <- c()
    
    sink(paste0(graphFolder,"log.txt"))
    for (cur in self$periodList) {
      periodDF <- subset(self$networkDF,time==cur)
      
      # if the transaction number (edge number) is less than 20 ignore that day
      # Acutally, for the continuous days, let's computate all days avaviable
      # if (nrow(periodDF)<20) {
      #   next
      # }
      print(cur)
      periodDF <- selectTop(periodDF,topsize = toprank)
      
      png(paste0(graphFolder,self$modelName,cur,".png"),width = 1200,height = 800)
      par(mfrow=c(1,2))
      DD <- coreDist(periodDF[,c("from","to")],periodDF$value2,plotBefore = T,plotAfter = T,
                     extraTitle = paste(self$modelName,cur))
      write.table(DD,file=paste0(distFolder,self$modelName,cur,".csv"),sep = ";",
                  row.names = FALSE,col.names = FALSE,eol = ";\n",append = FALSE)
      dev.off()
      
      gInfo <- graphInfo(periodDF[,c("from","to")],periodDF$value2,plotBefore = F,plotAfter = F,
                         extraTitle = paste(self$modelName,cur))
      gInfo  <- bind_cols(time=cur,gInfo)
      totalInfo <- bind_rows(totalInfo,gInfo)
    }
    sink()
    write_csv(totalInfo,paste0(graphFolder,"tradTopSummary.csv"))
    self$topInfo <- totalInfo
    invisible(self)
  },
  batFile = function(distFolder){
    distFolder = paste0(self$tkOutFolder,distFolder)
    dir.create(distFolder,showWarnings = FALSE)


    fileList <- list.files(distFolder,pattern = "csv$")
    
    batFile <- paste0(self$modelName,"_Run.bat")
    if (file.exists(batFile)) {
      file.remove(batFile)
    }
    
    for (f in fileList) {
      cmd = paste("rips_distance_matrix_persistence.exe -o",gsub(".csv",".txt",f))
      op = paste(" -r 5","-d 3 -p 2 ")
      cmd = paste0(cmd,op,"./",distFolder,f)
      cat(cmd,sep="",file=batFile,append = T)
      cat("\n",file = batFile,append = T)
    }
  },
  GUDHIRun = function(distFolder,outFolder){
    distFolder = paste0(self$tkOutFolder,distFolder)
    outFolder = paste0(self$tkOutFolder,outFolder)
    dir.create(distFolder,showWarnings = FALSE)
    dir.create(outFolder,showWarnings = FALSE)


    fileList <- list.files(distFolder,pattern = "csv$")
    for (f in fileList) {
      GUDHIpath = "~/R_project/GUDHI_2.1.0_OSX_UTILS/rips_distance_matrix_persistence"
      outFile = paste0("-o ",outFolder,gsub(".csv",".txt",f))
      inFile = paste0(distFolder,f)
      op = paste(" -r 5","-d 3 -p 2 ")
      
      cmd = paste(GUDHIpath,outFile,op,inFile)
      system(cmd)
    }
    invisible(self)
    
  },
  
  convertBetti = function(GUDHIFolder,bettiFolder){
    GUDHIFolder = paste0(self$tkOutFolder,GUDHIFolder)
    bettiFolder = paste0(self$tkOutFolder,bettiFolder)

    dir.create(GUDHIFolder,showWarnings = FALSE)
    dir.create(bettiFolder,showWarnings = FALSE)

    # GUDHIFolder = "GUF/"
    fileList <- list.files(GUDHIFolder,pattern = ".txt")
    
    # bettiFolder = "GUF_betti/"
    mainDir <- getwd()
    dir.create(file.path(mainDir, bettiFolder), showWarnings = FALSE)
    
    totalFile <- paste0(bettiFolder,"Summary.txt")
    if (file.exists(totalFile)) {
      file.remove(totalFile)
    }
    
    for (f in fileList) {
      tmp <- read.table(paste0(GUDHIFolder,f))
      tmp <- tmp[2:4]
      
      #find the unqiue interval, drop the Inf
      intervalSet <- c(as.vector(tmp[,2]),as.vector(tmp[,3])) %>%
        unique() %>% setdiff(.,c(Inf)) %>% sort()
      
      #elpsion from 0 to Inf
      steps <- length(intervalSet) + 1 
      
      betti <- matrix(0,steps,5)
      colnames(betti) <- c("Time",'B0','B1','B2','B3')
      betti[,1] <- c(intervalSet,Inf)
      
      #condense betti
      for (i in 1:nrow(tmp)) {
        targetBettiCol = tmp[i,1] + 2
        birthID = which(betti[,1] == tmp[i,2]) 
        deathID = which(betti[,1] == tmp[i,3])
        
        if (tmp[i,3]==Inf) {
          betti[birthID:deathID,targetBettiCol] <- betti[birthID:deathID,targetBettiCol] + 1
        }else{
          deathID = deathID -1 
          # interval add 1
          betti[birthID:deathID,targetBettiCol] <- betti[birthID:deathID,targetBettiCol] + 1
        }
      }
      
      #output betti
      f_out = gsub(".txt","_betti.txt",f)
      write.fwf(betti,file=paste0(bettiFolder,f_out),colnames = TRUE, sep = "\t")
      
      # total txt files
      cat(paste0(f,"\n"),file = totalFile,append = TRUE)
      write.fwf(betti,file = totalFile,colnames = TRUE,append = TRUE,sep = "\t")
      cat("\n",file = totalFile,append = TRUE)
    }
    invisible(self)
  },
  plotBetti = function(bettiFolder,bettiPlotFolder){
    bettiFolder = paste0(self$tkOutFolder,bettiFolder)
    bettiPlotFolder = paste0(self$tkOutFolder,bettiPlotFolder)

    dir.create(bettiFolder,showWarnings = FALSE)
    dir.create(bettiPlotFolder,showWarnings = FALSE)


    theme_update(plot.title = element_text(hjust = 0.5,size=20))
    # bettiFolder = "GUF_betti/"
    # bettiPlotFolder = "GUF_B/"
    file_list <- list.files(bettiFolder,pattern = "i.txt")
    
    for (f in file_list) {
      result <- read.table(paste0(bettiFolder,f),header = T)
      
      # For each type
      CM <- melt(result,id.vars = "Time", measure.vars = c("B0","B1","B2","B3"))
      gg1 <- ggplot(CM, aes(x=Time, y=value, color=variable)) +scale_color_manual(values=c("black", "red", "blue","grey")) +
        geom_point()+geom_line()  +  ggtitle(paste(self$modelName,f)) + xlim(0, 3)
      
      result <- bettinorm(result)
      CM <- melt(result,id.vars = "Time", measure.vars = c("B0","B1","B2","B3"))
      gg2 <- ggplot(CM, aes(x=Time, y=value, color=variable)) +scale_color_manual(values=c("black", "red", "blue","grey")) +
        geom_point()+geom_line()  +  ggtitle(paste("Percentage to Maximum")) + ylab("Percentage") + xlim(0, 3)
      
      # grid.arrange(gg1, gg2, nrow = 1)
      g <- arrangeGrob(gg1,gg2,nrow=1)
      picname = gsub(".txt",".png",f)
      ggsave(paste0(bettiPlotFolder,picname),g,width = 12,height = 6)
    }
    
    invisible(self)
  },
  joinBetti = function(input_folder,outFolder){
    input_folder = paste0(self$tkOutFolder,input_folder)
    outFolder = paste0(self$tkOutFolder,outFolder)

    dir.create(input_folder,showWarnings = FALSE)
    dir.create(outFolder,showWarnings = FALSE)

    modelList = list.files(input_folder,pattern = "betti")
    pat = "^([a-z]+)"
    
    modelList = str_extract(modelList,pat) %>% unique()
    
    for (model in modelList) {
      
      file_list <- list.files(input_folder,pattern = model)
      
      flag = 0
      for (f in file_list) {
        result <- read.table(paste0(input_folder,f),header = T)
        
        bettinames = names(result[,2:5])
        bettitype = gsub("_betti.txt","",f)
        setnames(result,old = bettinames, new = paste0(bettinames,bettitype))
        if (flag==0) {
          jdat <- result
          flag = 1
        }else{
          jdat <- join(jdat,result,by="Time",type="full")
          jdat <- jdat[order(jdat$Time),]
        }
      }
      jdat <- jdat %>% fill(names(jdat))
      jdat.sorted <- jdat[sort(names(jdat))] %>% select("Time",everything())
      jdat.transpose <- t(jdat.sorted) %>% as.data.frame()
      
      # write.table(jdat.transpose,file=paste0(outFolder,model,".csv"),sep = ",",
      #             row.names = T,col.names = F,append = FALSE)
      
      for (bn in bettinames) {
        jdat.b <- jdat.transpose[grepl(bn,rownames(jdat.transpose)),]
        jdat.b <- rbind(jdat.transpose[1,],jdat.b)
        write.table(jdat.b,file=paste0(outFolder,model,"_",bn,".csv"),sep = ",",
                    row.names = T,col.names = F,append = FALSE)
      }
      
    }
    invisible(self)
  },

  # convert to equal intervals
  reformatBetti = function(targetFolder,outputFolder,eqSeq = seq(0,3,by=0.001)){

    targetFolder = paste0(network$tkOutFolder,bettiFolder)
    outputFolder = paste0(self$tkOutFolder,outputFolder)
    dir.create(outputFolder,showWarnings = FALSE)

    bfiles = list.files(targetFolder,"betti")

    walk(bfiles,function(f){
      bf = read.table(paste0(targetFolder,f),header = T)
      eqbf = map_dfr(eqSeq,function(i){
        bf %>% filter(Time<i) %>% tail(1) %>% mutate(Time=i)
      }) %>% bind_rows(bf %>% tail(1))

      write.fwf(eqbf,file=paste0(outputFolder,f),colnames = TRUE, sep = "\t")
    })
    invisible(self)
  }
))

