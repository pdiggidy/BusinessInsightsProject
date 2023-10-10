
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
