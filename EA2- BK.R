#Setup#
library(igraph)
library(data.table)
library(readxl)
library(splitstackshape)
library(stringr)
library(plyr)

#Load the data into R#

d1 <- read.csv("~/Desktop/Social Networks/Funding_714.csv", header = TRUE)
d2 <- read_excel("~/Desktop/Social Networks/Funding_events_7.14_page2.xlsx")
d3 <- read.csv("~/Desktop/Social Networks/VC_outcomes.csv", header = TRUE)

#Standardize#
colnames(d1)<-colnames(d2)

#Combine and transform#
all<-rbind(d1,d2)
all$Investors <- as.character(all$Investors)

all$`Deal Date`<-as.Date(all$`Deal Date`,'%m/%d/%y')
all<-all[order(all$`Deal Date`),]
investors<-all[all$Investors!=''& !is.na(all$Investors),c(4,11)]
investors$Investors<-gsub(', Inc|, LLC|, Ltd|, Co|, Corp' , '',investors$Investors)
investors_list<-strsplit(investors$Investors,',')

#Get names#
il<-lapply(investors_list,length)>1
investors_list<-investors_list[il]

#Filter by Date#
date<-investors$`Deal Date`[il]
inv_combined<-lapply(investors_list,combn,m=2)

time<-matrix(unlist(lapply(inv_combined,function(x) length(x)/2)),ncol = 1)
time<-as.integer(time)
date<-rep(date,time)


#Create edgelist#
edgelist <- as.data.frame(matrix(unlist(inv_combined), ncol = 2, byrow = TRUE))

trim<-cbind(edgelist,date)
trim<-trim[!duplicated(trim[,c(1,2)]),]
trim<-na.omit(trim)

#Get closeness measures#
inv_df <- graph.data.frame(edgelist, directed = FALSE)
inv_df <- delete.edges(inv_df, E(inv_df)[get.edge.attribute(inv_df)==0])
close<-data.frame(closeness(inv_df))



#Find which firm has the highest closeness#
q1<-graph_from_edgelist(as.matrix(trim[,1:2]), directed = FALSE)
closeness <- closeness(q1)
max(closeness)
which.max(closeness)

#Intel Capital has the highest closeness in the network#

#Test whether the firm with the highest closeness has the shortest average path#
avg_path <- distances(q1, v = V(q1), to=V(q1))
avg_path[avg_path==Inf]<-dim(avg_path)[2]
avg <- colMeans(avg_path)
which.min(avg)

#The company with the highest closeness has the shortest path#



q2 <- cbind(edgelist,date)
q2 <- na.omit(q2)
q2a <- as.data.frame(matrix(ncol=2,nrow = 405))
for( i in 0:404){
  q2b <- as.character(q2[1,3]+30*i)
  plot <- q2[q2$date<=q2b,]
  graph <- graph_from_edgelist(as.matrix(plot[,1:2]), directed = FALSE)
  q2a[i+1,1]<-mean(coreness(graph))
}
plot(q2a[,1],type = 'o')

#We see that the graph, although sporadic at first,
#falls into a linear relationship later on as the index increases. 
#Looking at this, we can see that the relationship in the
#co-investment network are long lasting. 


q2c<-as.data.frame(matrix(ncol=2, nrow=405))
for (i in 0:404){
  q2a <- as.character(q2[1,3]+ 30*i)
  q2d <- as.character(q2[1,3]+ 30*i-1800)
  plot<- q2[q2$date<=q2a&trim$date>q2d,]
  graph <- graph_from_edgelist(as.matrix(plot[,1:2]), directed  = FALSE)
  q2c[i+1,1]<-mean(coreness(graph))
}
plot(q2c[,1], type = 'o')

#From the above graph, we can see that although the relationship within the network
#is long lasting and coreness increases quite linearly, it does drop at a certain point. 
#However, it doesnt drop to the earlier levels. Although coreness drops when expired 
#relationships are removed, it's clear that the lack of those relationships
#don't affect the situation too much. 



#I'm gonna be using correlation to test whether the hypothesis that 
#there exists a relationship between success of the company and closeness. 
sc<-d3[,c(1,2,3)]
colnames(close)<-"closeness"
close$companies<-rownames(close)


close[,3:36]<--1
for(i in 1:nrow(sc)){
  cf<-which(close$companies==sc[i,1])
  year<-sc[i,2]-1981+3
  close[cf,get('year')]<-sc[i,3]
}


sxc<-data.frame(1981:2014)
sxc[,2]<-0
for(i in 3:36){
  a<-close[,c(1,i)]
  a<-a[a[,2]!=-1,]
  sxc[i-2,2]<-cor(a[,1],a[,2])
}
mean(sxc[,2],na.rm = TRUE)

#After calculating the correlation between success and closeness for the various companies
#we take an average of them to see whether the hypothesis holds. 
#The average correlation was .0325. Now although there exists a positive relationship
#I wouldn't argue that there is indeed a significant relationship between 
#sucess and closeness. 

#Now let's see whether the hypothesis that 
#there exists a relationship between betwenness of the company and likelihood of 
#failure (less likely). 

bw<-data.frame(betweenness(inv_df))
colnames(bw)<-"betweenness"
bw$firm<-rownames(bw)
bw[,3:36]<--1
for(i in 1:nrow(sc)){
  j<-which(bw$firm==sc[i,1])
  year<-sc[i,2]-1981+3
  bw[j,get('year')]<-sc[i,3]
}

bxf<-data.frame(1981:2014)
bxf[,2]<-0
for(i in 3:36){
  a<-bw[,c(1,i)]
  a<-a[a[,2]!=-1,]
  bxf[i-2,2]<-cor(a[,1],a[,2])
}
mean(bxf[,2],na.rm = TRUE)

#After running the test, we can see that although the relationship is definelty
#stronger than that of closeness and success, I would argue that there still 
#isnt that strong of a correlation between being at the center of the network 
#and being less likely to fail 