#Alina Rashid ar06647
#Amsal Malik am07264
#Zain Ul Haq zh05616


library(igraph)

d = read.csv("Weather Extremes.csv")
t = 0.05 #threshold

#euclidean distance to find similarity between sentiment scores
dist = abs(dist(d$sentiment, method = "euclidean"))

zeros = which(dist >= t) 
ones = which(dist < t)
dist[zeros] = 0
dist[ones] = 1

#to create edges between graphs, we use dist and if dist is 1, 
#there is an edge and vice versa

g = graph_from_adjacency_matrix(dist, mode = "undirected")
# Assign the stance and temperature attributes to the graph nodes
V(g)$stance <- as.character(d$stance) # 'stance' is categorical
V(g)$temperature <- d$temperature_avg # 'temperature_avg' is numeric

summary(g)
plot(g)

# Calculate network metrics
avg_degree <- mean(degree(g)) #Average Degree 
network_diameter <- diameter(g) #Network Diameter 
graph_density <- edge_density(g)
clustering_coefficient <- transitivity(g, type = "global")
avg_clustering_coefficient <- mean(transitivity(g, type = "local"))
avg_path_length <- mean_distance(g)
num_nodes <- vcount(g)
num_edges <- ecount(g)
degree_distribution <- degree(g)
min_degree= min(degree(g))
max_degree= max(degree(g))


# Print network metrics
cat("Average Degree:", avg_degree, "\n")
cat("Network Diameter:", network_diameter, "\n")
cat("Graph Density:", graph_density, "\n")
cat("Clustering Coefficient (Global):", clustering_coefficient, "\n")
cat("Average Clustering Coefficient (Local):", avg_clustering_coefficient, "\n")
cat("Average Path Length:", avg_path_length, "\n")
cat("Number of Nodes:", num_nodes, "\n")
cat("Number of Edges:", num_edges, "\n")
cat("Degree Distribution:", degree_distribution, "\n")

# Export to GraphML file
write_graph(g, "network.graphml", format = "graphml")