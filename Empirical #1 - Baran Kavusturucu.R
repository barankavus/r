#Setup#
library(igraph)
library(data.table)

data <- fread("~/Desktop/Social Networks/social_and_task_network.csv", head = TRUE)

#Data Cleaning#
#Check the data, found a lot of zeros. Remove the zeroes from the data#
data
subset <- data[social_tie > 0 | task_tie > 0 ]
#After the subset, we end up with 79 observations in our network#
subset

network = graph.data.frame(subset) 

network

#As is, we have a network that is combined in terms of ties. We want to use the attributes 
#provided to us to seperate them#
#Next, we want to remove the edges with no social or task tie#
#we basically say in the following arguments to remove the edges where the edge attribute 
#social/task tie is equal to zero. This creates a subset#

social = delete.edges(network, E(network)[get.edge.attribute(network,name = "social_tie")==0])

task = delete.edges(network, E(network)[get.edge.attribute(network,name = "task_tie")==0])


plot(social)
plot(task)

#Once we plot, we see that there are some isolates that we want to remove#
#isolate means that it has a total degree of zero#

social = delete.vertices(social, V(social)[degree(social)==0])
task = delete.vertices(task, V(task)[degree(task)==0])

#Question 1

#Information for Social#
social_in_degree = degree(social, mode = "in")
social_out_degree = degree(social, mode = "out")
social_closeness = closeness(social, mode='total')
social_betweeness = betweenness(social)
social_rank = page_rank(social)$vector 
social_id = V(social)$name

social_info = as.data.table(list(ID = social_id, In_Degree = social_in_degree , Out_Degree = social_out_degree, Closeness = social_closeness, Betweenness = social_betweeness, PageRank = social_rank))

social_info

#Information for Task#
task_in_degree = degree(task, mode = "in")
task_out_degree = degree(task, mode = "out")
task_closeness = closeness(task, mode='total')
task_betweeness = betweenness(task)
task_rank = page_rank(task)$vector 
task_id = V(task)$name

task_info = as.data.table(list(ID = task_id, In_Degree = task_in_degree , Out_Degree = task_out_degree, Closeness = task_closeness, Betweenness = task_betweeness, PageRank = task_rank))

task_info


#Compare the same IDs (or else it just doesn't make sense)
cor(social_info[which(social_info$ID %in% task_info$ID),2:6], task_info[which(task_info$ID %in% social_info$ID),2:6])

#Insight: As an actor has higher amount of in and out degree connections in regards to tasks, their importance as a broker increases in social. 


#Question 2


# make an edge attribute indicating if a tie is considered strong or not. In this instance, we define a strong tie as one 
#that has a higher value than the mean 
social_strong = subset$social_tie > mean(subset$social_tie[subset$social_tie > 0])
task_strong = subset$task_tie > mean(subset$task_tie[subset$task_tie > 0])

#Adding strength to network 
network = set_edge_attr(network, "strong", index = E(network), social_strong == TRUE | task_strong == TRUE)

#subset the subset for strong ties only#
#just get the edge list of strong ties (retrieve ego, alter)
strong = subset[social_strong == TRUE | task_strong == TRUE, c("ego", "alter")]


#change directed network to undirected and look for strong ties in either direction
strong = rbindlist(list(strong, strong[, c("alter", "ego")]), use.names = FALSE)

#drop duplicates
strong = unique(strong)

# use chunks to indicate strong ties by rows 
strong[, min_two := .N > 1, by = ego]
strong = strong[min_two==TRUE]
strong = split(strong, by = "ego")

# look for second degree friends as implied by the triadic connection
strong_friends = lapply(seq_along(strong), function(i) t(combn(strong[[i]]$alter, 2)))
strong_friends = unique(do.call(rbind, strong_friends)) # do.call rbind is an efficient way of generating a data frame from a list where the columns of each list element represent the same variables

#check against actuals to see if triadic relatioship is actually satisfied 

actuals = rbindlist(list(subset[,c("ego", "alter")], subset[,c("alter", "ego")]), use.names = FALSE)

friends = paste(strong_friends[,1],strong_friends[,2],sep=",")
reals = paste(actuals$ego, actuals$alter, sep=",")

#check for open triads 
friends[!(friends %in% reals)]

#show proportion of realized closures
mean(friends %in% reals)


# make an edge attribute indicating if a tie is considered strong or not. In this instance, we define a strong tie as one 
#that has a higher value than the median
social_strong = subset$social_tie > quantile(subset$social_tie[subset$social_tie > 0],.5)
task_strong = subset$task_tie > quantile(subset$task_tie[subset$task_tie > 0],.5)

#Adding strength to network 
network = set_edge_attr(network, "strong", index = E(network), social_strong == TRUE | task_strong == TRUE)

#subset the subset for strong ties only#
#just get the edge list of strong ties (retrieve ego, alter)
strong = subset[social_strong == TRUE | task_strong == TRUE, c("ego", "alter")]


#change directed network to undirected and look for strong ties in either direction
strong = rbindlist(list(strong, strong[, c("alter", "ego")]), use.names = FALSE)

#drop duplicates
strong = unique(strong)

# use chunks to indicate strong ties by rows 
strong[, min_two := .N > 1, by = ego]
strong = strong[min_two==TRUE]
strong = split(strong, by = "ego")

# look for second degree friends as implied by the triadic connection
strong_friends = lapply(seq_along(strong), function(i) t(combn(strong[[i]]$alter, 2)))
strong_friends = unique(do.call(rbind, strong_friends)) # do.call rbind is an efficient way of generating a data frame from a list where the columns of each list element represent the same variables

#check against actuals to see if triadic relatioship is actually satisfied 

actuals = rbindlist(list(subset[,c("ego", "alter")], subset[,c("alter", "ego")]), use.names = FALSE)

friends = paste(strong_friends[,1],strong_friends[,2],sep=",")
reals = paste(actuals$ego, actuals$alter, sep=",")

#check for open triads 
friends[!(friends %in% reals)]

#show proportion of realized closures
mean(friends %in% reals)

#there is presence of triadic closure. since the mean is larget than median, some people hold really strong power in the network 

#Question 3

# find non-zeros
SB = edge.betweenness(network, e = E(network)[get.edge.attribute(network,name = "social_tie")!=0])
TB = edge.betweenness(network, e = E(network)[get.edge.attribute(network,name = "task_tie")!=0])


#combine strength info with betweeness

# mean
SS = subset$social_tie > mean(subset$task_tie[subset$task_tie > 0])
TS = subset$task_tie > mean(subset$task_tie[subset$task_tie > 0])

cor(SB, SS[get.edge.attribute(network,name = "social_tie")!=0])
cor(TB, TS[get.edge.attribute(network,name = "task_tie")!=0])


# median
SS = subset$social_tie > quantile(subset$task_tie[subset$task_tie > 0], .5)
task_strong = subset$task_tie > quantile(subset$task_tie[subset$task_tie > 0], .5)

cor(SB, SS[get.edge.attribute(network,name = "social_tie")!=0])
cor(TB, TS[get.edge.attribute(network,name = "task_tie")!=0])

#it appears that edges with high betweeness aren't correlated to strength for social but are for task. 
#The insight makes sense since there are certain actors who are powerful in connection and can thus act as brokers


#Question 4

NM = as_adjacency_matrix(network, sparse = FALSE)

#create multiple iterations of the matrix

#for power
NM2 = NM

#for checking reachability as steps increase 
NM3 = NM


#create a loop to incrementally increase the steps and count walks between two nodes
for(i in seq_len(nrow(NM) - 2)){
  NM2 = NM2%*%NM 
  NM3 = NM3 + NM2 
}

# number of pairs that dont have walks b/w one another 
sum(NM3 == 0)

#confirm
sum(distances(network) == Inf)




#Question 5

#Degree Centrality = 1: Star
 
star_setup = make_star(10, "undirected")

plot(star_setup)

#verify with igraph
centr_degree(star_setup)
centralize(degree(star_setup))

#verify with equation 
sum((max(degree(star_setup)) - degree(star_setup))/((vcount(star_setup) -1)* (vcount(star_setup)-2)))



#check degree with sna 
library(network)
starnetwork = network(as_adjacency_matrix(star_setup, sparse = FALSE))

detach("package:igraph", unload=TRUE)
library(sna)
centralization(starnetwork, degree)
detach("package:sna", unload=TRUE)
library(igraph)


#Degree Centrality = 0: Ring 
ring_setup = make_ring(10)

plot(ring_setup)

#verify with igraph
centr_degree(ring_setup)
centralize(degree(ring_setup))

#verify with equation
sum((max(degree(ring)) - degree(ring))/((vcount(ring) -1)* (vcount(ring)-2)))

ringnetwork = network(as_adjacency_matrix(ring, sparse = FALSE))

detach("package:igraph", unload=TRUE)
library(sna)

centralization(ringnetwork, degree)
