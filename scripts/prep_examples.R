examples <- list(
  assembly = c(
    "GCF_000002435.2",
    "GCF_000299415.1"
  ),
  biosample = c(
    "SAMN02714232", # contact email
    "SAMD00057211", # different ids, duplicated attributes
    "SAMN32745369",
    "SAMEA114639742"
  )
)

save(examples, file = "data/examples.rda")