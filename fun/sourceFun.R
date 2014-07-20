
sourceFun <- function(dir) {
  # source all file in given directory
  if(file.exists(dir)){
  for (nm in list.files(dir, pattern = "\\.[Rr]$")) {
    source(file.path(dir, nm))
  }
  }else{
    message('Directory not found, sourceFun abord')
  }
}

loadOrInst<-function(pack,...){
  # load or install packages
  # package = name of package
  # ... passed to install.packages
  if(require(pack,character.only=T)){
          return('package:ok')
  } else {
    message(paste('check',pack,'not ok. Try to install'))
    install.packages(pack,character.only=T,...)
    if(require(pack,character.only=T)){
    } else {
      stop(paste("Installation failed for",pack))
    }
  }
}