library(nstandr)
library(tidyr)
library(data.table)
library(dplyr)
library(magrittr)
library(ggplot2)

li = read.delim('comps_new.txt', header = FALSE)
li$V1 = toupper(li$V1)
li$V1 = standardize_magerman(li$V1)

#This reads all the patents, it takes a long time so don tuse unless neccessary

#comps = read.delim("g_applicant_not_disambiguated.tsv")
#comps_names = comps$raw_applicant_organization
#comps$raw_applicant_organization = standardize_magerman(toupper(comps$raw_applicant_organization))

#filtered = comps[comps$raw_applicant_organization %like% paste(li$V1, collapse = "|"),]
#num = unique(filtered$raw_applicant_organization)

#colnames(comps)
#write.csv(filtered, file = "filtered_patents.csv")
#write.csv(comps, file = "standardised_patents.csv")

patents = read.csv("merged_patents.csv")

patents <- patents %>%
  select(-raw_applicant_name_first, -raw_applicant_name_last, -rule_47_flag)
shortlist = read.csv("companies.csv", header = FALSE)
patents = patents[patents$raw_applicant_organization %like% paste(shortlist$V1, collapse = "|"),]

colnames(patents)
length(unique(patents$raw_applicant_organization))

patents <- patents %>%
  mutate(raw_applicant_organization = trimws(raw_applicant_organization))
patents %<>% mutate(patents, filing_date= as.Date(filing_date, format= "%Y-%m-%d"))
patents_grouped = group_by(patents, by= raw_applicant_organization)
patents_grouped = arrange(patents_grouped, filing_date, by_group = TRUE)

companies = unique(patents_grouped$by)

# Filter the data for "HONDA MOTOR" and count the rows for each date
filtered_data <- patents_grouped %>%
  filter(by == "DAIMLER") %>%
  group_by(filing_date) %>%
  summarize(count = n())

# Create the plot
ggplot(data = filtered_data, aes(x = filing_date, y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Filing Date", y = "Number of Rows") +
  ggtitle("Number of Rows by Filing Date for DAIMLER")

unique(patents_grouped$by)
