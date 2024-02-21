library(tidyverse)
library(UpSetR)


taxa <- read_csv('https://raw.githubusercontent.com/IMERSS/imerss-bioinfo/main/data/Xetthecum/reintegrated-withImages.csv') %>%
  rowwise() %>%
  filter(!is.na(`Hulquminum Name`)) %>%
  mutate(wikipediaSummary = gsub('<p class=\"mw-empty-elt\">\n</p>\n<p>', '', wikipediaSummary),
         wikipediaSummary = gsub('\n</p>\n\n\n', '', wikipediaSummary))

taxa %>%
  mutate(across(ends_with("value"), ~replace_na(.,0))) %>%
  select(ends_with("value")) %>%
  as.data.frame() %>%
  upset(order.by = "freq")

obs <- read_csv('https://raw.githubusercontent.com/IMERSS/imerss-bioinfo/main/data/Xetthecum/reintegrated-obs.csv')

map_pal <- structure(c('#21776E', '#4DAF54', '#64AFA4', '#175012', '#7F7F7F', '#BF673A', 
                       '#64C3DD', '#5B7A52', '#7F7F7F', '#0085BC', '#544C36', '#BAB18C', 
                       '#726C54', '#3499C6', '#757575', '#90A68A', '#BCC9B8', '#0085BC'
                       ),
                     names = c("Subtidal", "Eelgrass Beds", "Shoreline", "Mature Forest", "Dock", "Woodland", 
                               "Riparian", "Young Forest", "Road", "Laughlin Lake", "Rural", "Agricultural",
                               "Developed", "Wetland", "Coastal Bluff", "Pole Sapling", "Clearcut", "Pond")
)

stopifnot(length(map_pal) == length(unique(obs$clazz)))

list_values <- function(row){
  values <- row %>%
    select(ends_with('value')) %>%
    rename_with(~gsub(" Value", "", .)) %>%
    pivot_longer(everything()) %>%
    filter(value >0) %>%
    pull(name) %>% 
    paste(collapse ='\n  - ') 
  
  if (values != ""){values <- paste("  -", values)}
  
  return(values)
}

icon_values <- function(row){
  row %>%
    select(ends_with('value')) %>%
    rename_with(~gsub(" Value", "", .)) %>%
    pivot_longer(everything()) %>%
    filter(value >0) %>%
    mutate(name = tolower(name), 
           img = paste0("![", name, "](/files/", name, ".svg){width=50px}")) %>%
    pull(img) %>% 
    paste(collapse = '\n\n')
}

make_community_plot <- function(row, parent, obs){
  obs %>% 
    filter(`iNaturalist taxon ID` == row[['iNaturalist taxon ID']]) %>%
    ggplot(aes(x = COMMUNITY, fill = clazz)) + 
    geom_bar(position = 'stack', width = 0.5) + 
    ggthemes::theme_economist() + 
    theme(text = element_text(size=10), legend.position='right') + 
    labs(fill = "Ecological community", y = "Number of observations", x = '' ) + 
    scale_fill_manual(values=map_pal) + 
    scale_y_continuous(breaks = ~round(unique(pretty(.))))
    
    ggsave(paste0('taxa/', parent, '/community_dist.png'), width = 5, height = 3)
}

count_obs <- function(row, obs){
  obs %>% 
    filter(`iNaturalist taxon ID` == row[['iNaturalist taxon ID']]) %>%
    nrow() 
}

get_community <- function(row, obs) {
  obs %>% 
    filter(`iNaturalist taxon ID` == row[['iNaturalist taxon ID']]) %>%
    pull(COMMUNITY) %>%
    unique() %>%
    paste0(collapse = ', ')
}

# make species page
make_page <- function(row, obs){
  parent <- gsub(' ', '_', tolower(row["iNaturalist taxon name"]))
  if (!dir.exists(file.path('taxa', parent))){dir.create(file.path('taxa', parent))}
  make_community_plot(row, parent, obs)
  contents <- paste0(
    '---\ntitle: ',
    row["Hulquminum Name"],
    '\nengine: knitr',
    '\ncategories:\n',
    list_values(row),
    '\nfreeze: auto',
    '\n---\n',
    '![](', row$iNaturalistTaxonImage, '){fig-alt=\"', row["Hulquminum Name"], ' - ', row["commonName"], "\"}",
    "\n\n**Hul'q'umi'num' Name**: ", row["Hulquminum Name"],
    "\n\n**Taxon Name**: ", row["Taxon name"],
    "\n\n**\"Common\" Name**: ", row["commonName"],
    "\n\n**Cultural Values**:  \n\n::: {layout-nrow=1 style=\"width:60%\"}\n", icon_values(row), "\n:::",
    "\n\n**Ecological Community**: ", get_community(row, obs),
    "\n\n**Wikipedia summary**:  \n", row["wikipediaSummary"], 
    "\n\n**Observational Data**: ",
    "  \nFirst reported: ", date(row[['First Date Reported']]),
    "  \nSource: ", row['First Source'],
    "  \nLast reported: ", date(row[['Last Date Reported']]), 
    "  \nSource: ", row['Last Source'],
    "  \nObervation count: ", count_obs(row,obs),
    "\n\n![](community_dist.png)"
  )
  stringi::stri_write_lines(contents, encoding = "UTF-8", file.path('taxa', parent, 'index.qmd'))
}

# run this line to regenerate taxa pages
taxa %>% split(rownames(.)) %>%  map(~make_page(.x, obs))
