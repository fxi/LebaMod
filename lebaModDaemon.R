##########################################################
# lebaModDaemon.R
##########################################################

# This script will lauch the function computePending periodically.
# Expect to be launched from lebamod directory root.
#
# usage :
# 1. Manually set path for this script in ../data/config/daemonConfig.r
# 2. Launch Rscrip : " $ Rscript daemon/lebaModDaemon.R "

source('../data/config/configDaemon.R')
# lebamod functions loader
source('fun/sourceFun.R')

# test if everything exists.
if(!file.exists(dbIn)){
  stop('Data not found. Please launch this script from a lebamod project directory')
}


saveRDS(sessionInfo(),'../data/logs/sessionInfo.rds')

##########################################################
# Run script inside while loop 
##########################################################
runDaemon = TRUE
message('launching lebaMod daemon')
timestamp()

while(runDaemon){
  conLogs<-file(logsFile,open='a')
  sink(conLogs, append=T,type='message')
  sink(conLogs, append=T, type='output')
  sourceFun('fun')
  
  jobsTime<-system.time(
    # compute pending retunr "0 job" if no jobs are found in db
    summaryTable<-try(computePending(dbIn,dbOut,models,ncpus,attributions=attrib,timeoutFit=5400))
    )
  
  if('try-error' %in% class(summaryTable)){
    ######### HANDLE ERROR ######################
    # in case of error NOT handled by computePending : 
    # shut down script and send email to admin
    
    message('Error in computePending. read log file.')
    warning(summaryTable)
    runDaemon=FALSE
    logs<-as.data.frame(readLines(logsFile))
    from <- host
    to <- adminMail
    subject <- "LEBAMOD : An error occured. Interruption of lebaModDaemon script. "
    body <- list(paste('On',date(),'LebaModDaemon encountered an error and stopped.'),
                 paste('Error returned is:',summaryTable,'.'),
                 paste('logs file in attachment.'),
                 mime_part(logs))
    sendmail(from, to, subject, body,control=list(smtpServer=smtpServer))
    
  }else if('data.frame' %in% class(summaryTable)){
    ######### SEND NOTIFICATION ######################
    # get all mails and jobId in jobTable, for sendmailR
    
    message('Sending notification.')
    mailTable<-unique(summaryTable[!summaryTable$email=='-',c('email','idJob')])
    
    for(e in unique(mailTable$email)){    
      jobs<-mailTable[mailTable$email==e,'idJob']
      from <- host
      to <- paste0('<',e,'>')
      subject <- paste("LEBAMOD job",paste(jobs,collapse=','),"done")
      body <- list(
        paste('Data available at http://sdm.unige.ch:3838/lebamod/'),
        mime_part(as.data.frame(summaryTable),
                  name=paste0('LebaModSummaryTable',paste(jobs,collapse='_')))
        )
      sendmail(from, to, subject, body,control=list(smtpServer=smtpServer))
    }
    
  }
  sink()
  closeAllConnections()
  Sys.sleep(60)
}
