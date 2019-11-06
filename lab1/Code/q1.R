library(Rgraphviz)
library(bnlearn)

#function to create nbOfSamples samples
samples <- function(nbOfSamples){
  e1=rnorm(nbOfSamples)
  e2=rnorm(nbOfSamples)
  e3=rnorm(nbOfSamples)
  e4=rnorm(nbOfSamples)
  e5=rnorm(nbOfSamples)
  e6=rnorm(nbOfSamples)
  x1=e1
  x2=e2
  x3=-1*x1+e3
  x4=-2*x3+e4
  x5=-1*x2+e5
  x6=2*x3+x1-1*x2+e6
  return(data.frame(cbind(x1,x2,x3,x4,x5,x6)))
}

#for 40 samples
samples40 <-samples(40)
g1 <-gs(samples40)
g2 <-hc(samples40)
score(g2,samples40)
plot(g1)
plot(g2)
graphviz.compare(g1,g2)

#for 60 samples
samples60 <-samples(60)
g3 <-gs(samples60)
g4 <-hc(samples60)
score(g4,samples60)
plot(g3)
plot(g4)
graphviz.compare(g3,g4)

#for 100 samples
samples100 <-samples(100)
g5 <-gs(samples100)
g6 <-hc(samples100)
score(g6,samples100)
plot(g5)
plot(g6)
graphviz.compare(g5,g6)