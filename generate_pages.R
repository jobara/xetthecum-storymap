library(tidyverse)
library(UpSetR)


taxa <- read_csv('https://raw.githubusercontent.com/IMERSS/imerss-bioinfo/main/data/Xetthecum/reintegrated-withImages.csv') %>%
  rowwise() %>%
  mutate(cultural_values = sum(across(ends_with("value")), na.rm=T)) %>%
  filter(cultural_values > 0)

taxa %>%
  mutate(across(ends_with("value"), ~replace_na(.,0))) %>%
  select(ends_with("value")) %>%
  as.data.frame() %>%
  upset(order.by = "freq")


make_values <- function(row){
  row %>%
    select(ends_with('value')) %>%
    rename_with(~gsub(" Value", "", .)) %>%
    pivot_longer(everything()) %>%
    filter(value >0) %>%
    pull(name) %>% 
    paste(collapse ='\n  - ') %>%
    paste("  -", .)
}

escape_appostrope <- function(chr){
  gsub("\'", "\'\'", chr)
}


# make species page
make_page <- function(row){
  parent <- gsub(' ', '_', tolower(row["iNaturalist taxon name"]))
  if (!dir.exists(file.path('taxa', parent))){dir.create(file.path('taxa', parent))}
  contents <- paste0(
    '---\ntitle: ',
    row["Hulquminum Name"],
    '\nengine: knitr',
    '\ncategories:\n',
    make_values(row),
    '\nfreeze: auto',
    '\n---\n',
    '![](', row$iNaturalistTaxonImage, ')',
    "\n\n## **Hul'q'umi'num' Name**: ",row["Hulquminum Name"],
    "\n## **Taxon Name**: ",row["Taxon name"],
    "\n## **\"Common\" Name**: ",row["commonName"],
    "\n## **Cultural Values**:\n\n", make_values(row),
    "\n\n## **Ecological Community**",
    "\n## **Wikipedia summary**",
    "\n## **Observational Data**"
  )
  write(contents, file.path('taxa', parent, 'index.qmd'))
}

# run this line to regenerate taxa pages
#taxa %>% split(rownames(.)) %>%  map(~make_page(.x))
