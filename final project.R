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

#write.csv(sdc_data$business_description, "Descriptions.csv")

us_alliances <- sdc_data %>% filter(status == "Completed/Signed",
                                    #type == "Strategic Alliance",
                                    date_announced > "1970-01-01",
                                    date_announced < "2022-12-31",
                                    participant_nation == "United States",
                                    business_description %like% "%automotive%"|business_description %like% "vehicle"|business_description %like% "university"|business_description %like% "college" | business_description %like% "auto parts",
                                    
                                    ) %>%
  select(participants, date_announced, type, SIC_primary,
         participant_nation, deal_number)



us_alliances


us_alliances$participants <- us_alliances$participants %>%
  standardize_magerman()
head(us_alliances$participants)

us_alliances_net <- us_alliances %>%
  select(participants, deal_number) %>%
  as.matrix()

us_alliances_graph <- graph.data.frame(us_alliances_net, directed = FALSE)

V(us_alliances_graph)$type <- ifelse(V(us_alliances_graph)$name %in%
                                       us_alliances_net[,2],
                                     yes = TRUE, no = FALSE)
us_alliances_graph <- bipartite.projection(us_alliances_graph)$proj1
us_alliances_graph <- simplify(us_alliances_graph, remove.loops = TRUE,
                               remove.multiple = TRUE)
set.seed(2001525)
coords = layout_with_lgl(us_alliances_graph)
plot(us_alliances_graph, layout=coords, vertex.color = "coral2",
      vertex.size = 1, edge.width = 0.002,
     edge.color = adjustcolor("black", alpha.f = 0.1), vertex.label = V(us_alliances_graph)$name,vertex.label.cex =0.01)




pokemon_net_d3 <- igraph_to_networkD3(us_alliances_graph,group = V(us_alliances_graph)$name)
pokemon_net_d3
interactive_graph <- forceNetwork(Links = pokemon_net_d3$links,
                                  Nodes = pokemon_net_d3$nodes,
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


V(us_alliances_graph)$name


us_alliances_data <- data.frame(company_name = V(us_alliances_graph)$name,
                                degree = degree(us_alliances_graph),
                                betweenness = betweenness(us_alliances_graph))



# Specifying search query.
# The line below indicates that we want to obtain data for US assignees AND
# the year of 2019
query_us <- qry_funs$and(qry_funs$eq(patent_year = 2019),
                         qry_funs$eq(assignee_country = "US"))
query_us <- '{"_and": [{"patent_year": 2019}, {"assignee_country": "US"}]}'
# Specifying the fields of interest. In this step we indicate which
# variables do we want to obtain. A full list of possible fields is available
# via the following R command: View(patentsview::fieldsdf)
fields_us <- c("assignee_id", "assignee_organization",
               "assignee_total_num_patents", "app_country", "patent_date")
# Sending our request to the database. This returns a complex file consisting of
# many lists
search_us_2019 <- search_pv(query = query_us, fields = fields_us,
                            all_pages = TRUE,
                            sort = c("assignee_organization" = "asc"))
# Obtaining the data of interest from the output file. In essence, we get the
# data.frame that contains our variables of interest.
dat_us_2019 <- search_us_2019$data$patents %>% unnest(assignees)

#It appears that the same companies are listed in the file several times, so
#let's only keep the unique entities in there.
dat_us_2019 <- search_us_2019$data$patents %>%
  unnest(assignees) %>%
  distinct(assignee_key_id, .keep_all = T) %>%
  select(-applications) %>%
  filter(!is.na(assignee_organization))
#Name disambiguation
dat_us_2019$assignee_organization <- dat_us_2019$assignee_organization %>%
  standardize_magerman()

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
