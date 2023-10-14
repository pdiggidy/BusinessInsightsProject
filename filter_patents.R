library(nstandr)

li = read.delim('companies.txt', header = FALSE)
li$V1 = toupper(li$V1)
li$V1 = standardize_magerman(li$V1)

comps = read.delim("g_applicant_not_disambiguated.tsv")
comps_names = comps$raw_applicant_organization
comps$raw_applicant_organization = standardize_magerman(toupper(comps$raw_applicant_organization))

filtered = comps[comps$raw_applicant_organization %like% paste(li$V1, collapse = "|"),]
num = unique(filtered$raw_applicant_organization)
