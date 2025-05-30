# graphs of cover and relative proportions

# INPUT: vector updated by vypocet_plochy.R (it should have cover area and percent of coverage of each polygon by each raster from raster list)
# OUTPUT: graphs in /out folder

#---#---#---#---#---#---#---#---#---#---#---#---# 
#                  User input                   #
#---#---#---#---#---#---#---#---#---#---#---#---#

id <- readline(prompt = "SEGMENT_ID") # should be defined collumn
id <- sym(id)
bio <- readline(prompt = "BIOTOP_CODES") # should be defined collumn
bio <- sym(bio)
N <- readline(prompt = "Number of top plotted ground truth cathegories: ") # should be between 1 and N of bio
thr <- as.numeric(readline(prompt = "Minimum cover of polygon to be taken as FOUND: ")) # should be percent

#############x
thr <- 10
N <- 10
id <- "SEGMENT_ID"
id <- sym(id)
bio <- "BIOTOP_CODES"
bio <- sym(bio)
##############

#---#---#---#---#---#---#---#---#---#---#---#---# 
#                   Functions                   #
#---#---#---#---#---#---#---#---#---#---#---#---#

mark_found <- function(vctr, threshold = thr){
  # which colls are percent
  percent_cols <- grep("_percent$", names(vctr), value = TRUE)
  # loop through each percent collumn
  for (col in percent_cols) {
    found_col <- gsub("_percent$", "_found", col)
    vctr[[found_col]] <- ifelse(vctr[[col]] > threshold, 1, 0)
  }
  return(vctr)
}

#---#---#---#---#---#---#---#---#---#---#---#---# 
#               Data preparation                #
#---#---#---#---#---#---#---#---#---#---#---#---#

v <- vector %>%
  mark_found() %>% # polygon found?
  as.data.frame() %>%
  select(!!id, !!bio, ends_with("_found")) %>%
  pivot_longer(
    cols = ends_with("_found"),
    names_to = "mod",
    values_to = "found"
  )

# calculate total number of segments in each cathegory
totals <- v %>%
  group_by(mod, !!bio) %>%
  summarise(total = n_distinct(!!id), .groups = "drop")

# calculate number of found segments in each cathegory
found <- v %>%
  filter(found == 1) %>%
  group_by(mod, !!bio) %>%
  summarise(Found = n_distinct(!!id), .groups = "drop")

# join total and found, calculate proportions
plot_df <- totals %>%
  left_join(found, by = c("mod", as.character(bio))) %>% # join
  mutate(
    Found = replace_na(Found, 0), # number of found
    Not_found = total - Found # number of notfound
  ) %>%
  select(mod, !!bio, Found, Not_found) %>%
  pivot_longer(
    cols = c(Found, Not_found), # convert to long format for easy plotting
    names_to = "found_status", # whether row represents founds/notfounds
    values_to = "n"
  ) %>%
  group_by(mod, !!bio) %>%
  mutate(prop = n / sum(n)) %>% # compute proportion
  ungroup()

# select cathegories with highest proportions
top <- plot_df %>%
  filter(found_status == "Found") %>%
  group_by(mod) %>%
  slice_max(order_by = prop, n = as.numeric(N)) %>%
  ungroup() %>%
  select(mod, !!bio)

# keep only top cathegories for plotting
plot_df_top <- plot_df %>%
  semi_join(top, by = c("mod", as.character(bio))) %>%
  left_join(totals, by = c("mod", as.character(bio)))

#---#---#---#---#---#---#---#---#---#---#---#---# 
#                     Plots                     #
#---#---#---#---#---#---#---#---#---#---#---#---#

# create output dir
if(!dir.exists("out/RP")){
  dir.create("out/RP", recursive = T)
}

# create output file
pdf_name <- paste0("Relative_proportions_top_", N, "_thr_", thr, ".pdf")
pdf(file = paste0("out/RP/", pdf_name), width = 10, height = 6)

for (model in unique(plot_df_top$mod)) {
  plot_data <- plot_df_top %>%
    filter(mod == model) # single mod output
  
  # arrange cathegories based on found proportion
  ordering <- plot_data %>%
    filter(found_status == "Found") %>%
    arrange(desc(prop)) %>%
    pull(!!bio) %>%
    unique()
  # apply ordering ↓
  plot_data <- plot_data %>% 
    mutate(!!bio := factor(!!bio, levels = ordering))
  
  # label preparation n = xxx
  labels_df <- plot_data %>%
    distinct(!!bio, total) %>%
    mutate(label = paste0("n = ", total))
  
  # plot it!
  p <- ggplot(plot_data, aes(x = !!bio, y = prop, fill = found_status)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(
      values = c(
        "Found" = "steelblue",
        "Not_found" = "orange"
      )
    ) +
    geom_text(
      data = labels_df,
      aes(x = !!bio, y = 1.02, label = label),
      inherit.aes = FALSE,
      size = 3
    ) +
    scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.15))) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = paste0("Top ", N, " ", as.character(bio), " by found (>= ", thr,"% of cover) polygon proportion"),
      subtitle = paste("Model:", gsub("_found$", "", model)),
      x = as.character(bio),
      y = "Proportion",
      fill = "Found Status"
    )
  print(p)
}

dev.off()

rm(pdf_name, plot_df, plot_df_top, top, totals, found, v, p, bio, id, model, N, thr, labels_df, plot_data, ordering)
gc()
