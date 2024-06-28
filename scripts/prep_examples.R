examples <- list(
  assembly = c(
    "GCF_000002435.2",
    "GCF_000299415.1",
    "GCA_021389195.1"
  ),
  bioproject = "PRJEB54063",
  biosample = c(
    "SAMN02714232", # contact email
    "SAMD00057211", # different ids, duplicated attributes
    "SAMN32745369",
    "SAMEA114639742",
    "SAMN38450475",
    "SAMD00041425",
    "SAMN37751259",
    "SAMN36356470", # links
    "SAMN36698370",
    "SAMN00678218",
    "SAMN00778252",
    "SAMEA3317914",
    "SAMEA14036741",
    "SAMN03863711",
    "SAMN39003160",
    "SAMN01918814", # links
    "SAMN02692951", # owner
    "SAMN06220566", # ids
    "SAMN32317800", # empty description
    "SAMEA111396836", # unit in attributes
    "SAMN14387582"
  ),
  gene = "948356",
  protein = "AAA27507.1",
  sra = "DRX061127"
)

save(examples, file = "data/examples.rda")
