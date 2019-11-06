library(Rgraphviz)
library(bnlearn)

study <- function(dataSet,algorith){
  #
  #function take a dataFrame and return a graph
  #
  
  graph <- algorith(dataSet)
  return(graph)
}

subSample <- function(dataSet,nbOfSample){
  #
  #function take a dataFrame and number of samples and return a sub dataFrame of random nbOfSample rows of the input dataFrame
  #
  sub_sample<-dataSet[sample(1:nrow(dataSet), nbOfSample),]
  return(sub_sample)
}

evaluate <- function(dataSet,step,plotting=FALSE,algorithm=hc){
  #
  #function take dataFrame and number of step to show the variation of the score after increazing the number of samples
  #
  #remove rows having NaN values
  dataSet<-dataSet[complete.cases(dataSet),]
  
  #number of rows
  nbOfRows=nrow(dataSet)
  
  #check if the step>number of rows
  if(step>nbOfRows){
    return(NULL)
  }
  
  scores<-c()
  nbsOfSamples<-c()
  iteration<-c()
  variation<-c()
  lastScore<-0
  #study the result after each step
  for(n in 1:as.integer(nbOfRows/step)){
    #subset of the data set
    sub_set<-subSample(dataSet = dataSet,nbOfSample=n*step)
    
    #graph of this subset
    currentGraph<-study(dataSet = sub_set,algorith=algorithm)
    
    #plotting
    if(plotting){
      plot(currentGraph)
    }
    
    #Score of the result
    currentScore<-score(currentGraph,sub_set)
    scores<-c(scores,abs(currentScore))
    nbsOfSamples<-c(nbsOfSamples,n*step)
    iteration<-c(iteration,n)
    variation<-c(variation,(abs(currentScore)-abs(lastScore)))
    lastScore<-currentScore
  }
  #if there is other data
  if(nbOfRows>n*step){
    currentGraph<-study(dataSet = dataSet,algorith=algorithm)
    currentScore<-score(currentGraph,dataSet)
    scores<-c(scores,abs(currentScore))
    nbsOfSamples<-c(nbsOfSamples,nbOfRows)
    iteration<-c(iteration,n+1)
    variation<-c(variation,(abs(currentScore)-abs(lastScore)))
  }
  
  #plot of the result
  plot(unlist(nbsOfSamples),unlist(scores))
  
  #uncomment this function to show the variation graph
  #plot(unlist(nbsOfSamples),unlist(variation))
}

#evaluating

#load data
data=read.csv("Returns250d.txt",sep = " ")
dataSet <- data[,c("AIR.FRANCE.KLM",
             "ALCATEL.LUCENT",
             "AXA",
             "FAURECIA",
             "GAUMONT",
             "GEODIS",
             "PPR",
             "UNION.FINC.FRANC")]

#evaluating using Retunrs250d data set
evaluate(dataSet = dataSet,step=500,algorithm = hc)
evaluate(dataSet = dataSet,step=500,algorithm = mmhc)
