#require(caret)
require(mlbench)
require(KODAMA)
require(gridExtra)
require(grid)

datasetNames_2d <- c("mlbench.spirals(n=
                     examples, cycles=2, sd=0.05)",
                     "mlbench.cassini(n=examples)",
                     "mlbench.shapes(n=examples)",
                     "mlbench.smiley(n=examples)",
                     "mlbench.2dnormals(n=examples, c=2)",
                     "mlbench.circle(n=examples, d=2)",
                     "mlbench.ringnorm(n=examples, d=2)",
                     "mlbench.threenorm(n=examples, d=2)",
                     "mlbench.xor(n=examples, d=2)",
                     "mlbench.hypercube(n=examples, d=2)",
                     "mlbench.simplex(n=examples, d=2)",
                     "KODAMA.spirals(n=c(rep(round(examples/2), 2)),sd=c(rep(0.1, 2)))"
                     )

datasetNames_3d <- c("mlbench.cuboids(n=examples)",
                     "mlbench.circle(n=examples, d=3)",
                     "mlbench.ringnorm(n=examples, d=3)",
                     "mlbench.threenorm(n=examples, d=3)",
                     "mlbench.xor(n=examples, d=3)",
                     "mlbench.hypercube(n=examples, d=3)",
                     "mlbench.simplex(n=examples, d=3)",
                     "KODAMA.dinisurface(N=examples)",
                     "KODAMA.helicoid(N=examples)",
                     "KODAMA.swissroll(N=examples)"
                     )

#WORKING WITH 2D DATASETS
plot2D <- function(datasetNames_2d, examples){
  par(mfrow=c(3,4))
  #p_2d <- list()
  for(datasetName in datasetNames_2d){
    #setFilename <- gsub("\\([\\(\\)\\w\\d\\s\\=\\.,]*\\)", "", datasetName, perl=TRUE, ignore.case=TRUE)
    setFilename <- sub("\\(.*", "", datasetName) # * before '('
    
    lib_name <- sub("\\..*", "", setFilename) # * before '.'
    func_name <- sub(".*\\.", "", setFilename)# * after '.'
    
    if (lib_name == 'mlbench'){
      dataset <- eval( parse( text=datasetName ) )
      #p_2d[[setFilename]] <- print(plot(dataset, main=setFilename))
      plot(dataset, main=setFilename)
    }
    else if (lib_name == 'KODAMA'){
      #remove 'kodama' from name
      datasetName <- sub(".*KODAMA.", "", datasetName) 
      #add code to create class
      datasetName <- paste0("cbind(",datasetName,", rep(1:2, each=round(examples/2)))")
      
      #evaluate the string as a function
      dataset <- eval( parse( text=datasetName ) )
      #change the col names to 0,1,'CLASS' #may be removed.
      colnames(dataset) <- c(0:1, "CLASS")
      #saving plot to a list
      #p_2d[[setFilename]] <- plot(dataset, main=setFilename, col=dataset[,3])
      plot(dataset, main=setFilename, col=dataset[,3])
    }
    else{
      print('error in library name.')
    }
  }
}


#WORKING WITH 3D DATASETS
plot3D <- function(datasetNames_3d, examples){
  p <- list()
  for(datasetName in datasetNames_3d){
    #setFilename <- gsub("\\([\\(\\)\\w\\d\\s\\=\\.,]*\\)", "", datasetName, perl=TRUE, ignore.case=TRUE)
    setFilename <- sub("\\(.*", "", datasetName) # * before '('
    
    lib_name <- sub("\\..*", "", setFilename) # * before '.'
    func_name <- sub(".*\\.", "", setFilename)# * after '.'
    
    if (lib_name == 'mlbench'){
      dataset <- eval( parse( text=datasetName ) )
      p[[setFilename]] <- cloud(x.3~x.1+x.2, groups=classes, data=as.data.frame(dataset), main=setFilename)
    }
    else if (lib_name == 'KODAMA'){
      #remove 'kodama' from name
      datasetName <- sub(".*KODAMA.", "", datasetName) 
      #add code to create class
      datasetName <- paste0("cbind(",datasetName,", rep(1:3, each=100))")
      
      #evaluate the string as a function
      dataset <- eval( parse( text=datasetName ) )
      #change the col names to 0,1,'CLASS' #may be removed.
      colnames(dataset) <- c("x.1", "x.2", "x.3", "classes")
      #saving plot to a list
      #p_2d[[setFilename]] <- plot(dataset, main=setFilename, col=dataset[,3])
      p[[setFilename]] <- cloud(x.3~x.1+x.2, groups=classes, data=as.data.frame(dataset), main=setFilename)
    }
  }
  #grid.arrange(grobs=p, ncol=3, top = textGrob("3D datasets"))
  grid.arrange(grobs=p, ncol=4)
}

plot2D(datasetNames_2d, examples = 3000)
plot3D(datasetNames_3d, examples = 3000)
