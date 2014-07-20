
# Ask server to proceed a script  
# expect host defined in ~/.ssh/config
# expect RSA key on both machine
# expect expect key copied via ssh-copy-id

# change to shiny host when test finished.

remoteExecBundle <- function(bundle,bundlesPath,localProjPath,remoteProjPath,localSSH,remoteSSH){ 
	# function to
	# 1. save rds file that contain data, parameter and function
	#	 to execute on remote host.
	# 2. send RDS file to remote host via scp
	# 3. open remote R session
	# 4. execute function on data
	# 5.  save result on RDS file
	# 6. send back results on local host

	bName <- paste0(gsub('[- :CEST]','',Sys.time()),'.lebaBundle')

	# path relatice to project
	pathToSend<- file.path(bundlesPath,'toSend',bName)
	pathToRead<- file.path(bundlesPath,'toRead',bName)
	pathSession <- file.path(bundlesPath,'sessionInfoRserv','sess')
	# local paht
	bLocPathToSend  <- file.path(localProjPath,pathToSend)
	bLocPathToRead  <- file.path(localProjPath,pathToRead)
	# remote path
	bRemPathToSend <- file.path(remoteProjPath,pathToSend)
	bRemPathToRead <- file.path(remoteProjPath,pathToRead)
	sPath <- file.path(remoteProjPath,pathSession)
	## ssh path
	sshReturnToPath <- paste0(localSSH,':',bLocPathToRead)
	sshSendToPath <- paste0(remoteSSH,':',bRemPathToRead)

	saveRDS(bundle,bLocPathToSend)
	resultName= bundle$ret



	# setting quoted bash command, to be executed on remote host
	shR <- paste("\"Rscript -e \\\" ",
				 ".libPaths('/b10/moser/R/packages');",
				 "library(RSclient);",
				 "if(file.exists('",sPath,"')){c<-RSattach(readRDS('",sPath,"'))}else{c <- RSconnect()};",
				 "print(RSserverEval(c, 'getwd()'));",
				 "RSeval(c, quote(try({",
				 "setwd('",remoteProjPath,"');",
				 "bl=readRDS('",bRemPathToRead,"');",
				 "dat=bl$dat;",
				 "ret=bl$ret;",
				 "message(ret);",
				 "par=bl$par;",
				 "eval(bl$exp);",
				 "saveRDS(1:10,'",bRemPathToSend,"');",
							 "system(paste('scp','",bRemPathToSend,"','",sshReturnToPath,"'))})));",
				 "rSession<-RSdetach(c);",
				 "saveRDS(rSession,'",sPath,"');",
				 "\\\"\"",
				 sep='')

	# From local host
	#saveRDS(bundleList,bundleLocalPath)
	system(paste('scp',bLocPathToSend,sshSendToPath))
	system(paste('ssh',remoteSSH,shR), wait=T,ignore.stdout=F,ignore.stderr=F)
	message('Sending command on server : done. Waiting for results.')
	message(paste('directory of result is',bLocPathToRead))
	bLocPathToRead
}


