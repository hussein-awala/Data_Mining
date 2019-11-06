library(Rgraphviz)
library(bnlearn)

#
# Question 2
#
# part 1
data=read.csv("Returns250d.txt",sep = " ")
d <- data[,c("AIR.FRANCE.KLM",
             "ALCATEL.LUCENT",
             "AXA",
             "FAURECIA",
             "GAUMONT",
             "GEODIS",
             "PPR",
             "UNION.FINC.FRANC")]
#d<-d[complete.cases(d),]

# part 2
g1 <- gs(d)
g2 <- hc(d)
plot(g1)
plot(g2)
score(g2,d)

# part 3
ci.test(d$ALCATEL.LUCENT,d$GEODIS)
ci.test(d$PPR,d$GEODIS)

# part 4
ci.test("GEODIS","GAUMONT",c("ALCATEL.LUCENT"),d)
ci.test("ALCATEL.LUCENT","UNION.FINC.FRANC",c("AXA","FAURECIA"),d)

# part 5
ci.test("GEODIS","UNION.FINC.FRANC",c("ALCATEL.LUCENT","GAUMONT"),d)

#
# Question 3
#

# part 2
g3<-mmhc(d)
plot(g3)

ci.test(d$ALCATEL.LUCENT,d$GEODIS)
ci.test(d$PPR,d$GEODIS)

ci.test("GEODIS","GAUMONT",c("ALCATEL.LUCENT"),d)
ci.test("ALCATEL.LUCENT","UNION.FINC.FRANC",c("AXA","FAURECIA"),d)


