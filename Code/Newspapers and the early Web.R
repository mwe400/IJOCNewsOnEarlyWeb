
  
  #===============================================#
  #                                               #
  #  Imitation in the quest to survive:           #
  #  Lessons from news media on the early Web     #
  #                                               #
  #  International Journal of Communication, 2017 #
  #  Weber, M., Ognyanova, K., Kosterich, A.      #
  #                                               #
  #===============================================#

  
# Using R 3.4.1 and RStudio 1.0.153
#
# Package versions:
# 
# data.table 1.10.4
# stringr 1.2.0
# igraph 1.1.2
# sna 2.4 
# network 1.13.0
# ggplot2 2.2.1
# grid 3.4.1
# reshape2 1.4.2


####  ------------------ DATA READ ------------------ ####

news.links <- data.frame(from=character(), to=character(), date=as.Date(character()),
                         text=character(), weight=numeric(), type=character(), size=numeric(), 
                         year=integer(), stringsAsFactors=F)
 
folder.n <- "./Data 1996-2000/"
nfiles <- paste0(folder.n, list.files(folder.n, pattern="(.*)part-r(.*)")) 
 

for(i in 1:length(nfiles)) {
  print(nfiles[i])
  if (file.exists(nfiles[i])) 
  {
    link.tmp <- read.csv(nfiles[i], sep="\t", header=F, as.is=T, quote = "", skipNul=T)  
    colnames(link.tmp) <- c("from", "to", "date", "text", "weight", "type", "size")
    link.tmp$year <- sub("(.*)([0-9]{4})-part-(.*)", "\\2", nfiles[i])
    news.links <- rbind(news.links, link.tmp)
    rm(link.tmp)
  }
}   
 

dim(news.links)
head(news.links)
tail(news.links)

# Save result:
 save(news.links, file="NewsLinks-DF-1996-to-2000.rda")


 

####  ------------------ DATA PREP ------------------ ####

load("NewsLinks-DF-1996-to-2000.rda")

 
library(data.table)

# Create a data table with the data:
news.dt <- data.table(news.links) 
setkey(news.dt, year, from, to)
head(news.dt)

news.dt$weight <- as.numeric(news.dt$weight)
news.dt$size <- as.numeric(news.dt$size)


# Info about the newspapers in the data from the Alliance for Audited Media :
np <- data.table(read.csv("./Newspaper data Alliance for Audited Media.csv", as.is=T))
setkey(np, url)
head(np)



# Replace links that contain newspaper URLs with the actual URLs
# (e.g. replace ".nytimes.com" or "x.nytimes.com" with "nytimes.com")

library(stringr) 
# if we want subdomains, use np$page here:
news.urls <- paste0(np$url, collapse="|") 

from.tmp <- str_extract(news.dt$from, news.urls)
to.tmp   <- str_extract(news.dt$to,   news.urls)
news.dt[!is.na(from.tmp)]$from <- from.tmp[!is.na(from.tmp)]
news.dt[!is.na(to.tmp)]$to <-     to.tmp[!is.na(to.tmp)]
rm(from.tmp); rm(to.tmp)

detach(package:stringr)

# Generate link.from and link.to indicators
# (Those show if the from/to of a link is in our newspaper set)

news.dt$link.from <- F
news.dt$link.to <- F
news.dt[from %in% np$url, link.from:= T]
news.dt[to %in% np$url, link.to:= T] 


table(news.dt$link.from, news.dt$link.to, useNA="ifany")
news.dt[!link.from & !link.to]
news.dt

# Combine with newspaper data (names, etc.) 
# from the Alliance for Audited Media:
npm <- data.table(to=np$url, name.to=np$organization)
news.dt <- merge(news.dt, npm, by="to", all.x=T)
npm <- data.table(from=np$url, name.from=np$organization)
news.dt <- merge(news.dt, npm, by="from", all.x=T)
setkey(news.dt, year, from, to)
news.dt[is.na(name.from), name.from:=from]
news.dt[is.na(name.to), name.to:=to]
news.dt
 


####  ------------------ DATA AGGREGATE ------------------ ####


news.dt$dates <- as.Date(as.character(news.dt$date),"%Y%m%d")
news.dt$years <- as.integer(format(news.dt$dates, format="%Y")) 
news.dt$month <- as.integer(format(news.dt$dates, format="%m"))
news.dt$day <- as.integer(format(news.dt$dates, format="%d")) 

table(news.dt$years, useNA="ifany")
table(news.dt$year, useNA="ifany")

dim(news.dt[year==years])
dim(news.dt[year!=years])

# Year is the year of the crawl, years is the year from the timestamp
# Remove cases where the two do not match (Internet Archive bugs):
news.dt <- news.dt[year==years] 
news.dt[,years:=NULL]

# Examine time distributions:
table(news.dt$year, useNA="ifany")
table(news.dt$month, useNA="ifany")
table(news.dt$day, useNA="ifany") 

hist(news.dt$dates, "years", format = "%Y", main="Crawl Times")
hist(news.dt$dates, "months", format = "%b-%Y", main="Crawl Times")

# Save results:
save(news.dt, file="NewsLinks-EarlyWeb-1996-2000.Rdata")

# Remove loops:
news.dt <- news.dt[from!=to,]
table(news.dt$weight, useNA="ifany")

# Remove outliers (mostly ad websites):
hist(news.dt$weight, main="Link Weight")
news.dt[weight>1000,]
news.dt <- news.dt[weight<1000,]

# Collapse the data by time period (averaging wights within period) :
# We do it by name (not url) because some newspapers have multiple URLs.
# By YEAR:
news.dty <- news.dt[, list(weight=round(mean(weight, na.rm=T),2), size=round(mean(size, na.rm=T)), 
                           link.from=link.from[1], link.to=link.to[1], 
                           from=from[1], to=to[1], crawls=.N), 
                     by=list(year, name.from, name.to)] 

# How many repeated links (crawls) per time period?
table(news.dty$crawls, useNA = "ifany") 

# How many crawls of newspaper sites?
table(news.dty[link.from!=link.to & link.from==T, ]$crawls, useNA = "ifany") 

# How many heavy links? 
news.dty[weight>100,]

# How many unique sites total?
length(unique(c(news.dty$from, news.dty$to)))

# How many unique sites per year?
news.dty[,list(sites = length(unique(c(from, to)))), by=year]

# How many unique links per year?
news.dty[,list(links =.N), by=year]

# Unique links to newspaper per year:
news.dty[,list(links = sum(link.to)), by=year]

# Unique links from newspaper per year:
news.dty[,list(links = sum(link.from)), by=year]

# Unique links among newspapers per year:
news.dty[name.from!=name.to, list(links = sum(link.from & link.to)), by=year]
news.dty[name.from!=name.to, list(links = sum(link.from & link.to & weight>1)), by=year]

# Unique sites linking to newspapers per year:
news.dty[,list(sites = length(unique(from[link.to]))), by=year]

# Unique sites linked to by newspapers per year:
news.dty[,list(sites = length(unique(to[link.from]))), by=year]

# Average link weight per year:
news.dt[, .(m.weight=mean(weight)), by=year]
  

# Some odd heavy links from WaPo to results96.politicsnow.com:
# table(news.dt[name.from=="Washington Post" & year=="1997",]$weight) 
# news.dt[name.from=="Washington Post" & to=="results96.politicsnow.com",] 


# Compile data on how many links (and to how many sites) each newspaper had per year
# (use 1 year - to ensure the sites were crawled at least once during the period) 

news.from <- news.dty[from %in% np$url, list(weight.from=sum(weight, na.rm=T), 
                                             name=name.from, n.from=as.numeric(.N)), 
                      by=list(year, name.from)]
setkey(news.from, name, year)

news.to <- news.dty[to %in% np$url, list(weight.to=sum(weight, na.rm=T), 
                                         name=name.to, n.to=as.numeric(.N)), 
                    by=list(year, name.to)]
setkey(news.to, name, year)

# Newspaper table with info about total from- and to- links:
news.ft <- merge(news.from, news.to, all=T)[,c(2,4,5,7,8,1), with=F]
setkey(news.ft, name, year)

# Link list that only contains links among the newspaper sites:
news.ins <-  news.dty[from %in% np$url & to %in% np$url][,c(2:10,1), with=F]
setkey(news.ins, name.from,name.to, year)

# How many newspapers:
 length(unique(news.ft$name))
# How many appear for less than 4 years: 
 news.ft[,.(n=.N),by=name][n<4] 
# How many years does each newspaper have nonzero links for: 
 news.ft[,sum(!is.na(weight.from)), by=name]
# How many newspapers have nonzero links per year: 
 news.ft[,sum(!is.na(weight.from)), by=year]

# Years need to be interger to match class for merge:
news.ins$year <- as.integer(news.ins$year)
news.ft$year <- as.integer(news.ft$year)

# Prepare & add circulation and other data from the Alliance for Audited Media`:
npu <- np[!duplicated(organization),c(-2,-3, -23:-11), with=F] 
setnames(npu, "organization", "name")
setkey(npu, name)

npu <- cbind( npu[rep(1:nrow(npu),5),1:2, with=F], year = rep(1996:2000, each=nrow(npu)),
              circulation=c(npu$X1996, npu$X1997, npu$X1998, npu$X1999, npu$X2000) )

news.ft <- merge(news.ft, npu, all=T, by=c("name", "year"))

# Add total number of from- links & sites to the links list
# (in case it is needed to normalize link weights for further analyses)
news.ins <- merge(news.ins, news.ft[,.(name.from=name, year=year, wf=weight.from, nf=n.from),], 
                  by=c("name.from", "year"), all.x=T)[,c(1,3:12,2), with=F]
# Weight as proportion of total links (pweight) and total sites (sweight)
news.ins$pweight <- round(100*news.ins$weight/news.ins$wf,2) # in percent
news.ins$sweight <- round(100*news.ins$weight/news.ins$nf,2) # in percent



####  ------------------ EXTRACT NETWORKS ------------------ ####


# Require that there's more than a single link between websites
nins <- news.ins[weight>1,]

# Remove loops
nins <- nins[name.from!=name.to,]

nft <- news.ft

# Add empty data for 1995 (helps with plotting):
nft <- rbind(nft,  cbind(nft[year==1996,c("name", "founded"), with=F], 
                         year=1995, weight.from=NA, n.from=NA, 
                         weight.to=NA, n.to=NA, circulation=NA))

library("igraph")

# Add color to node list - red for online, gray for offline:
nft$col <- "#EB5833"
nft$col[is.na(nft$weight.from) & is.na(nft$weight.to)] <- "#dddddd"

np95 <- simplify(graph.data.frame(nins[year==1995,], nft[year==1995,], directed=T))
np95

np96 <- simplify(graph.data.frame(nins[year==1996,], nft[year==1996,], directed=T))
np96

np97 <- simplify(graph.data.frame(nins[year==1997,], nft[year==1997,], directed=T))
np97

np98 <- simplify(graph.data.frame(nins[year==1998,], nft[year==1998,], directed=T))
np98

np99 <- simplify(graph.data.frame(nins[year==1999,], nft[year==1999,], directed=T))
np99

np00 <- simplify(graph.data.frame(nins[year==2000,], nft[year==2000,], directed=T))
np00

# In case we want the nodes to have the same place in each graph viz:
l <- layout_with_fr(np00)
plot(np00, layout=l)
     
     
net.update <- function(g) {
  V(g)$degree    <- degree(g, mode="all")
  V(g)$indegree  <- degree(g, mode="in")
  V(g)$outdegree <- degree(g, mode="out")
    
  V(g)$size <- (V(g)$indegree+5)*1.5
  V(g)$shape <- "circle"
  V(g)$color <- V(g)$col
  V(g)$frame.color <- "#95290F"
  V(g)$label <- V(g)$name
  V(g)$label.dist <- (V(g)$indegree+5)*.04
  V(g)$label.degree <- pi/2
  V(g)$label.color <- "#000000"
  V(g)$label.cex <- .6
  V(g)$label.font <- 2
  E(g)$arrow.size <- .2
  E(g)$width <- E(g)$weight*0.8
  E(g)$color <- "#B0B0B0"
  E(g)$curved <- 0
  return(g)
}

np95 <- net.update(np95)
np96 <- net.update(np96)
np97 <- net.update(np97)
np98 <- net.update(np98)
np99 <- net.update(np99)
np00 <- net.update(np00)


plot.gr <- function(g, yr=1996) {
           plot(g, vertex.size=(V(g)$indegree+5)*1.5, 
             vertex.shape="circle", vertex.color=V(g)$col,
             vertex.shape="circle", vertex.frame.color="#95290F", 
             vertex.label.degree=pi/2, vertex.label.dist=(V(g)$indegree+5)*.04, 
             vertex.label=V(g)$name, vertex.label.color="#000000", 
             vertex.label.cex=.6, 
             vertex.label.font=2, 
             edge.width=E(g)$weight*0.8, edge.color="#B0B0B0",
             edge.arrow.size=.2, edge.curved=0, #layout=l, 
             main=paste0("Newspaper Website Network ", yr) ) 
  
legend(x=-1.1, y=-1.1, c("Newspaper with a website","Newspaper without a website"), pch=21,
       col="#95290F", pt.bg=c("#EB5833", "#dddddd"), pt.cex=1.5, bty="n", ncol=1)
  }


plot.gr(np95, 1995)
plot.gr(np96, 1996)
plot.gr(np97, 1997)
plot.gr(np98, 1998)
plot.gr(np99, 1999)
plot.gr(np00, 2000)

 
# Compute network characteristics for each network:
nnets <- list(np96, np97, np98, np99, np00) 

npr <- data.frame(Density = sapply(nnets, graph.density),
                  Reciprocity = sapply(nnets, reciprocity),
                  Clustering = sapply(nnets, transitivity), # as undirected
                 `Indegree Centralization` =  unlist(sapply(nnets, centralization.degree, mode="in", normalized=T)[2,]),
                 `Outdegree Centralization` =  unlist(sapply(nnets, centralization.degree, mode="out", normalized=T)[2,]),
                 `Closeness Centralization` =  unlist(sapply(nnets, centralization.closeness, mode="total", normalized=T)[2,]),
                 `Betweenness Centralization` = unlist(sapply(nnets, centralization.betweenness, directed=T, normalized=T)[2,])
                 ) 
 
npm96w <- get.adjacency(np96, attr="weight", sparse=F)
npm97w <- get.adjacency(np97, attr="weight", sparse=F)
npm98w <- get.adjacency(np98, attr="weight", sparse=F)
npm99w <- get.adjacency(np99, attr="weight", sparse=F)
npm00w <- get.adjacency(np00, attr="weight", sparse=F)

npm96 <- get.adjacency(np96, sparse=F)
npm97 <- get.adjacency(np97, sparse=F)
npm98 <- get.adjacency(np98, sparse=F)
npm99 <- get.adjacency(np99, sparse=F)
npm00 <- get.adjacency(np00, sparse=F)

mnet <- list(npm96, npm97, npm98, npm99, npm00) 
mnetw <- list(npm96w, npm97w, npm98w, npm99w, npm00w) 

detach(package:igraph)



####  ------------------ NETWORKS MEASURES ------------------ ####

library(sna)
library(network) 

mnets <- mnet  # for UNWEIGHTED version
mnets <- mnetw # for WEIGHTED version


# Reciprocity - proportion of symmetric dyads
# dyadic - ratio of dyads where (i,j)==(j,i) to all dyads
# dyadic.nonnull - ratio of dyads where (i,j)==1 AND (j,i)==1 to all dyads where (i,j)==1
# edgewise - ratio of edges that are reciprocated to all edges where (i,j)==1

sapply(mnets,gden, mode="digraph")
sapply(mnets, mutuality)
sapply(mnets, grecip, measure="dyadic")
sapply(mnets, grecip, measure="dyadic.nonnull")
sapply(mnets, grecip, measure="edgewise")

# Transitivity
sapply(mnets, gtrans, mode="digraph", measure="weak")
sapply(mnets, gtrans, mode="digraph", measure="weakcensus")
sapply(mnets, gtrans, mode="digraph", measure="strong")
sapply(mnets, gtrans, mode="digraph", measure="strongcensus") 

# Degree
sapply(mnets, function(n) centralization(n, FUN=degree, mode="graph"))
sapply(mnets, function(n) centralization(n, FUN=degree, mode="digraph", cmode="indegree"))
sapply(mnets, function(n) centralization(n, FUN=degree, mode="digraph", cmode="outdegree"))

sapply(mnets, function(n) centralization(n, FUN=betweenness))
sapply(mnets, function(n) centralization(n, FUN=closeness))


# Conditional uniform graph tests:
cugr <- lapply(mnets, function(n) cug.test(n, grecip, cmode="edges", ignore.eval=T, 
                                           FUN.arg=list(measure="dyadic.nonnull")) ) 
cugtr <- lapply(mnets, function(n) cug.test(n, gtrans, cmode="edges", ignore.eval=T,
                                            FUN.arg=list( mode="digraph", measure="weak")))  
cugci <-  lapply(mnets, function(n) cug.test(n, centralization, cmode="edges", ignore.eval=T,
                                              FUN.arg=list(FUN=degree, cmode="indegree")))
cugco <-  lapply(mnets, function(n) cug.test(n, centralization, cmode="edges", ignore.eval=T,
                                              FUN.arg=list(FUN=degree, cmode="outdegree")))
lapply(cugr, plot)
lapply(cugtr, plot)
lapply(cugci, plot)
lapply(cugco, plot)



npr <- data.frame(Density = sapply(mnets,gden, mode="digraph"),
                  Reciprocity = sapply(mnets, grecip, measure="dyadic.nonnull"),
                  Transitivity = sapply(mnets, gtrans, mode="digraph", measure="weak"), # as undirected
                 `Indegree Centralization` =  sapply(mnets, function(n) centralization(n, FUN=degree, mode="digraph", cmode="indegree")),
                 `Outdegree Centralization` =  sapply(mnets, function(n) centralization(n, FUN=degree, mode="digraph", cmode="outdegree")))
 
library(ggplot2)
library(grid)
library(reshape2)
 
dat <- cbind(melt(npr), Year=as.character(rep(1996:2000, ncol(npr))))
colnames(dat)[1] <- "Measure (normalized)"

pl <- ggplot(dat, aes(x=Year, y=value, group=`Measure (normalized)`, color=`Measure (normalized)`,
                                        fill=`Measure (normalized)`, shape=`Measure (normalized)`  )) +
       ggtitle(paste0("Newspaper website network (", dat$Year[1], "-", dat$Year[nrow(dat)],")")) +
        scale_x_discrete(name="Year") +
        scale_y_continuous(name="Measure", breaks=seq(0,0.8,0.1)) +
        geom_point( alpha=0.7, size=4) +
        scale_shape_manual(values=21:25) + 
        geom_line(size = 1.2) +
        theme_bw()+
        theme(plot.margin=unit(c(1,1,1,1),"cm")) +
        theme(axis.title.x=element_text(size=14, vjust=-1.5)) +
        theme(axis.title.y=element_text(size=14, vjust=2.5)) + 
        theme(plot.title=element_text(size=16, vjust=2.5, face="bold")) +
        theme(strip.text.x = element_text(size = 14)) +
        theme(legend.title=element_text(size=14)) +
        theme(legend.text=element_text(size=12))+
        theme(panel.grid.major = element_line(colour="gray90", size=0.5)) +
        theme(legend.key.size=unit(1,"cm"))
pl


detach(package:ggplot2)
detach(package:grid)
detach(package:reshape2)




####  ------------------ PROFILE SIMILARITIES ------------------ ####


allf <- news.dty[from!=to, .(w=sum(weight, na.rm=T),n=.N), by=c("from", "year")]
allt <- news.dty[from!=to, .(w=sum(weight, na.rm=T),n=.N), by=c("to", "year")]
table(allf$n); table(allf$w); table(allf$year)
table(allt$n); table(allt$w); table(allt$year) 


# Take the top X heaviest from- websites each year, and extract their links to newspaper sites:

topx <- 5000

npa96t <- dcast.data.table(news.dty[year==1996 & name.to %in% news.ft$name & 
                           name.from %in% allf[year==1996][order(-w)[1:topx]]$from, 
                           2:4, with=F], name.from ~ name.to, value.var="weight", fun=sum)


npa97t <- dcast.data.table(news.dty[year==1997 & name.to %in% news.ft$name & 
                           name.from %in% allf[year==1997][order(-w)[1:topx]]$from, 
                           2:4, with=F], name.from ~ name.to, value.var="weight", fun=sum) 

npa98t <- dcast.data.table(news.dty[year==1998 & name.to %in% news.ft$name & 
                           name.from %in% allf[year==1998][order(-w)[1:topx]]$from, 
                           2:4, with=F], name.from ~ name.to, value.var="weight", fun=sum) 

npa99t <- dcast.data.table(news.dty[year==1999 & name.to %in% news.ft$name & 
                           name.from %in% allf[year==1999][order(-w)[1:topx]]$from, 
                           2:4, with=F], name.from ~ name.to, value.var="weight", fun=sum)

npa00t <- dcast.data.table(news.dty[year==2000 & name.to %in% news.ft$name & 
                           name.from %in% allf[year==2000][order(-w)[1:topx]]$from, 
                           2:4, with=F], name.from ~ name.to, value.var="weight", fun=sum) 


# Take the top X heaviest to- websites each year, and extract their links from newspaper sites:

npa96f <- dcast.data.table(news.dty[year==1996 & name.from %in% news.ft$name & 
                           name.to %in% allt[year==1996][order(-w)[1:topx]]$to, 
                           2:4, with=F], name.to ~ name.from, value.var="weight", fun=sum)


npa97f <- dcast.data.table(news.dty[year==1997 & name.from %in% news.ft$name & 
                           name.to %in% allt[year==1997][order(-w)[1:topx]]$to, 
                           2:4, with=F], name.to ~ name.from, value.var="weight", fun=sum) 

npa98f <- dcast.data.table(news.dty[year==1998 & name.from %in% news.ft$name & 
                           name.to %in% allt[year==1998][order(-w)[1:topx]]$to, 
                           2:4, with=F], name.to ~ name.from, value.var="weight", fun=sum) 

npa99f <- dcast.data.table(news.dty[year==1999 & name.from %in% news.ft$name & 
                           name.to %in% allt[year==1999][order(-w)[1:topx]]$to, 
                           2:4, with=F], name.to ~ name.from, value.var="weight", fun=sum)

npa00f <- dcast.data.table(news.dty[year==2000 & name.from %in% news.ft$name & 
                           name.to %in% allt[year==2000][order(-w)[1:topx]]$to, 
                           2:4, with=F], name.to ~ name.from, value.var="weight", fun=sum)


# Binary profile overlap - shared sites:

shared <- function(dt, rem1=F, mean=T) {
            if (rem1) dt <- dt[,-1, with=F]
            out <-  t(as.matrix(dt>0)) %*% as.matrix(dt>0)
            if (mean) return(round(mean(out[row(out)!=col(out)], na.rm=T),2)) else return(out) }
               
sapply(list(npa96t, npa97t, npa98t, npa99t, npa00t), shared, rem1=T)
sapply(list(npa96f, npa97f, npa98f, npa99f, npa00f), shared, rem1=T)


mean.cor <- function(dt, rem1=F, mean=T, sd=T) {
                    if (rem1) dt <- dt[,-1, with=F]
                    out <- cor(dt)
                    if (mean) return(round(mean(out[], na.rm=T),3))
                    if (sd)   return(round(sd(out[], na.rm=T),3))
                    return(out) }

sapply(list(npa96t, npa97t, npa98t, npa99t, npa00t), mean.cor, rem1=T, mean=F)
sapply(list(npa96f, npa97f, npa98f, npa99f, npa00f), mean.cor, rem1=T, mean=F)


# RANDOMIZATION TESTS:
# using 'sample' to generate density-conditioned graphs
# sample(g[upper.tri(g, diag = F) | lower.tri(g)])


resam <- function(dt, rem1=F) {
          if (rem1) dt <- dt[,-1, with=F]
          matrix(sample(as.matrix(dt)), nrow(dt), ncol(dt)) }

# Resample by column - takes longer:
resam <- function(dt, rem1=F) {
          if (rem1) dt <- dt[,-1, with=F]
          sapply(dt, sample)  }

  
rand.test <- function(npa, nrep=1000, func=function(x)mean(cor(x)),...) {
  npa <- npa[,-1, with=F]
  og <- func(npa,...)
  gt <- 0
  eq <- 0
  lt <- 0
  
  for (i in 1:nrep) {
    new.val <- func(resam(npa, rem1=F),...)
    gt <- gt + 1*(og < new.val)
    eq <- eq + 1*(og == new.val)
    lt <- lt + 1*(og > new.val)
  }
  return(list(greater.than.obs=gt, equal.to.obs=eq, less.than.obs=lt))
}
 

sapply(list(npa96t, npa97t, npa98t, npa99t, npa00t), rand.test, func=mean.cor, nrep=1000)
sapply(list(npa96f, npa97f, npa98f, npa99f, npa00f), rand.test, func=mean.cor, nrep=1000)
 


####  ------------------------------------------------------ ####

















 