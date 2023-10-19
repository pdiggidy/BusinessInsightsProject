#install.packages("patentsview")
library(tidyr)
library(dplyr)
library(igraph)
library(data.table)
library(patentsview) #this package is needed for retrieving the data
library(tidyr) #this package is needed for quickly subsetting the data
#install.packages("devtools")
#devtools::install_github("stasvlasov/nstandr")
library(nstandr)
#install.packages("networkD3")
library(networkD3)
sdc_data <- readRDS("SDC_data_2021.rds")
sdc_data = sdc_data[sdc_data$deal_number %in% alliance_codes&sdc_data$SIC_primary %in% c(3711, 3714, 3537, 5012,3751),]

automotive = sdc_data %>% filter(status == "Completed/Signed",
                                 #type == "Strategic Alliance",
                                 date_announced > "2010-01-01",
                                 date_announced < "2022-12-31",
                                 #participant_nation == "United States",
                                 business_description %like% "%automotive%"|business_description %like% "vehicle"| business_description %like% "auto parts",
                                 
) %>%
  select(participants, date_announced, type, SIC_primary,
         participant_nation, deal_number)

alliance_codes = automotive$deal_number
alliance_codes
automotive_deals = sdc_data[sdc_data$deal_number %in% alliance_codes,]

automotive_deals$participants <- automotive_deals$participants %>%
  standardize_magerman()
head(automotive_deals$participants)

automotive_deals_net <- automotive_deals %>%
  select(participants, deal_number) %>%
  as.matrix()

automotive_deals_graph <- graph.data.frame(automotive_deals_net, directed = FALSE)

V(automotive_deals_graph)$type <- ifelse(V(automotive_deals_graph)$name %in%
                                           automotive_deals_net[, 2],
                                         yes = TRUE, no = FALSE)
automotive_deals_graph <- bipartite.projection(automotive_deals_graph)$proj1
automotive_deals_graph <- simplify(automotive_deals_graph, remove.loops = TRUE,
                                   remove.multiple = TRUE)
set.seed(2001525)
coords <- layout_with_lgl(automotive_deals_graph)
plot(automotive_deals_graph, layout = coords, vertex.color = "coral2",
     vertex.size = 1, edge.width = 0.002,
     edge.color = adjustcolor("black", alpha.f = 0.1), vertex.label = V(automotive_deals_graph)$name, vertex.label.cex = 0.01)

automotive_deals_net_d3 <- igraph_to_networkD3(automotive_deals_graph, group = V(automotive_deals_graph)$name)
automotive_deals_net_d3
interactive_graph <- forceNetwork(Links = automotive_deals_net_d3$links,
                                  Nodes = automotive_deals_net_d3$nodes,
                                  Source = 'source',
                                  Target = 'target',
                                  NodeID = 'name',
                                  Group = 'group',
                                  zoom = TRUE,
                                  linkColour = "black",
                                  fontFamily = "Times New Roman",
                                  fontSize = 11,
                                  opacity = 0.9)

interactive_graph

V(automotive_deals_graph)$name

#volkswagen

target_node_name <- "VOLKSWAGEN"

# Find the index of the "volkswagen" node in the graph
target_node_index <- which(V(automotive_deals_graph)$name == target_node_name)

# Find the connected components in the graph
components <- clusters(automotive_deals_graph)


# Extract the nodes in the component containing "volkswagen"
nodes_in_connected_component <- which(components$membership == 4)

# Create a subgraph containing only the nodes in the connected component
subgraph <- induced_subgraph(automotive_deals_graph, vids = nodes_in_connected_component)

# Plot the subgraph
plot(subgraph, layout = coords, vertex.color = "coral2",
     vertex.size = 1, edge.width = 0.002,
     edge.color = adjustcolor("black", alpha.f = 0.1),
     vertex.label = V(subgraph)$name, vertex.label.cex = 0.01)

# Convert the subgraph to a networkD3 object and visualize it
subgraph_net_d3 <- igraph_to_networkD3(subgraph, group = V(subgraph)$name)
interactive_subgraph <- forceNetwork(Links = subgraph_net_d3$links,
                                     Nodes = subgraph_net_d3$nodes,
                                     Source = 'source',
                                     Target = 'target',
                                     NodeID = 'name',
                                     Group = 'group',
                                     zoom = TRUE,
                                     linkColour = "black",
                                     fontFamily = "Times New Roman",
                                     fontSize = 11,
                                     opacity = 0.9)

interactive_subgraph

# Calculate degree centrality
degree_centrality <- degree(subgraph)

# Calculate betweenness centrality
betweenness_centrality <- betweenness(subgraph)

# Calculate closeness centrality
closeness_centrality <- closeness(subgraph)

# Calculate eigenvector centrality
eigenvector_centrality <- eigen_centrality(subgraph)

# Create a dataframe with the node names and their centrality measures
centrality_df <- data.frame(
  Node = V(subgraph)$name,
  DegreeCentrality = degree_centrality,
  BetweennessCentrality = betweenness_centrality,
  ClosenessCentrality = closeness_centrality,
  EigenvectorCentrality = eigenvector_centrality$vector
)

print(centrality_df)
centrality_df$ClosenessCentrality
order(centrality_df$DegreeCentrality)

write.csv(centrality_df, "centrality.csv")

shortlist = read.csv("companies.csv", header = FALSE)


plot(subgraph,layout = coords, vertex.color = "coral2",
     vertex.size = 1, edge.width = 0.002,
     edge.color = adjustcolor("black", alpha.f = 0.1),
     vertex.label = V(subgraph)$name, vertex.label.cex = 0.01)
