d=readRDS('SDC_data_2021.rds')
d
dis = d$ownership_structure
unique(d$business_description)

library(data.table)
unique(d[d$business_description %like% "university", ]$business_description)
d[d$business_description %like% "university", ]


unique(d[d$business_description %like% "automotive" | d$business_description %like% "vehicle" & d$participant_nation %like% "Germany", ]$participants)
nrow(unique(d[d$business_description %like% "university" & d$participant_nation %like% "United States", ]))





d[d$deal_number %like% 216583045, ]


d[d$participants %like% "Volkswagen", ]$business_description
