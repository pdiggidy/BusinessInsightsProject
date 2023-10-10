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
install.packages("networkD3")
library(networkD3)
sdc_data <- readRDS("SDC_data_2021.rds")

automotive = sdc_data %>% filter(status == "Completed/Signed",
                                 #type == "Strategic Alliance",
                                 date_announced > "2010-01-01",
                                 date_announced < "2022-12-31",
                                 #articipant_nation == "United States",
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

# Assuming your "volkswagen" node is identified by its name, replace "Volkswagen" with the actual name.
target_node_name <- "Volkswagen"

# Find the index of the "volkswagen" node in the graph
target_node_index <- which(V(automotive_deals_graph)$participants == target_node_name)

# Perform a breadth-first search starting from the "volkswagen" node
# to identify all connected nodes.
connected_nodes <- bfs(automotive_deals_graph, root = target_node_index, order = TRUE)$order

# Create a subgraph containing only the "volkswagen" network
subgraph <- induced_subgraph(automotive_deals_graph, vids = connected_nodes)

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

