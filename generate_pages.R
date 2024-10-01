library(whisker)
library(tidyverse)
library(readr)

# Found existence of these variables at https://quarto.org/docs/projects/scripts.html#pre-and-post-render
render_all <- nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))
cat("renderAll: ", render_all, "\n")

if (render_all) {
  template <- readr::read_file("templates/taxon.qmdt")
  
  taxa <- read_csv("https://raw.githubusercontent.com/IMERSS/imerss-bioinfo/main/data/Xetthecum/reintegrated-withImages.csv") %>%
    rowwise() %>%
    filter(!is.na(hulquminumName)) %>%
    mutate(wikipediaSummary = gsub('<p class=\"mw-empty-elt\">\n</p>\n<p>', '', wikipediaSummary),
           wikipediaSummary = gsub('\n</p>\n\n\n', '', wikipediaSummary)) %>%
    # https://stackoverflow.com/a/64264936/1381443 - whisker definitely can't deal with spaces in key names
    rename_with(~ gsub(" ","_", .x), contains(" "))
  
  obs <- read_csv("https://raw.githubusercontent.com/IMERSS/imerss-bioinfo/main/data/Xetthecum/reintegrated-obs.csv")
  
  map_pal <- structure(c('#21776E', '#4DAF54', '#64AFA4', '#175012', '#7F7F7F', '#BF673A',
                         '#64C3DD', '#5B7A52', '#7F7F7F', '#0085BC', '#544C36', '#BAB18C',
                         '#726C54', '#3499C6', '#757575', '#90A68A', '#BCC9B8', '#0085BC'
  ),
  names = c("Subtidal", "Eelgrass Beds", "Shoreline", "Mature Forest", "Dock", "Woodland",
            "Riparian", "Young Forest", "Road", "Laughlin Lake", "Rural", "Agricultural",
            "Developed", "Wetland", "Coastal Bluff", "Pole Sapling", "Clearcut", "Pond")
  )
  
  stopifnot(length(map_pal) == length(unique(obs$clazz)))
  
  list_values <- function (row) {
    values <- row %>%
      select(ends_with('value')) %>%
      rename_with(~gsub("_Value", "", .)) %>%
      pivot_longer(everything()) %>%
      filter(value > 0) %>%
      pull(name) %>%
      paste(collapse ='\n  - ')
    
    if (values != "") {
      values <- paste("  -", values)
    }
    
    return(values)
  }
  
  icon_values <- function (row) {
    row %>%
      select(ends_with('Value')) %>%
      rename_with(~gsub("Value", "", .)) %>%
      pivot_longer(everything()) %>%
      filter(value >0) %>%
      mutate(name = tolower(name),
             img = paste0("![", name, "](/files/", name, ".svg){width=106px}")) %>%
      pull(img) %>%
      paste(collapse = '\n\n')
  }
  
  make_audio <- function (row) {
    fn <- row %>%
      mutate(fn = gsub('\'', '_', hulquminumName),
             fn = paste0(fn, '_Levi_Wilson_&_Emily_Menzies_Galiano_Island_2022-02-20.mp3')) %>%
      pull(fn)
    
    if (file.exists(paste0('audio/', fn))){
      return (paste0('`r embedr::embed_audio(\'../../audio/', fn, '\')`'))
    } else {
      return("")
    }
  }
  
  make_community_plot <- function (row, parent, obs) {
    obs %>%
      filter(`iNaturalist taxon ID` == row[['iNaturalist_taxon_ID']]) %>%
      ggplot(aes(x = COMMUNITY, fill = clazz)) +
      geom_bar(position = 'stack', width = 0.5) +
      ggthemes::theme_economist() +
      theme(text = element_text(size=10), legend.position='right',
            panel.grid.major = element_blank()) +
      labs(fill = "Ecological community", y = "Number of observations", x = '' ) +
      coord_flip() +
      scale_fill_manual(values=map_pal) +
      scale_y_continuous(breaks = ~round(unique(pretty(.))))
    
    ggsave(paste0('taxa/', parent, '/community_dist.png'), width = 5, height = 3)
  }
  
  count_obs <- function (row, obs) {
    obs %>%
      filter(`iNaturalist taxon ID` == row[['iNaturalist_taxon_ID']]) %>%
      nrow()
  }
  
  get_communities <- function (row, obs) {
    obs %>%
      filter(`iNaturalist taxon ID` == row[['iNaturalist_taxon_ID']]) %>%
      pull(COMMUNITY) %>%
      unique() %>%
      paste0(collapse = ', ')
  }
  
  # make species page
  make_page <- function (row, obs) {
    parent <- gsub(' ', '_', tolower(row["iNaturalist_taxon_name"]))
    if (!dir.exists(file.path("taxa", parent))){dir.create(file.path("taxa", parent))}
    make_community_plot(row, parent, obs)
    
    extras <- list(
      categories = list_values(row),
      audio = make_audio(row),
      icons = icon_values(row),
      communities = get_communities(row, obs),
      firstReported = date(row$`First_Date_Reported`),
      lastReported = date(row$`Last_Date_Reported`),
      obsCount = count_obs(row, obs)
    )
    data <- c(as.list(row), extras)
    templated <- whisker.render(template, data=data)
    filename <- file.path("taxa", parent, "index.qmd")
    
    stringi::stri_write_lines(templated, encoding = "UTF-8", filename)
    writeLines(str_glue("Written {nchar(templated)} bytes to file {filename}"))
  }
  
  if (FALSE) {
    # Run just this line to generate one taxon
    make_page(row, obs)
  }
  
  nt <- nrow(taxa)
  # Run this block to generate all taxon pages
  for (i in seq_len(nt)) {
    cat(str_glue("[{i}/{nt}] "))
    make_page(taxa[i,], obs)
  }
} else {
  cat("Not rendering all files, skipping site generation")
}

