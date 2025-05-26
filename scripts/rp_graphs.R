# graphs of cover and relative proportions

# INPUT: vector updated by vypocet_plochy.R (it should have cover area and percent of coverage of each polygon by each raster from raster list)
# OUTPUT: graphs in /out folder

id <- readline(prompt = "SEGMENT_ID") # should be defined collumn
id <- sym(id)
bio <- readline(prompt = "BIOTOP_CODES") # should be defined collumn
bio <- sym(bio)
N <- readline(prompt = "Number of top plotted ground truth cathegories:") # should be between 1 and N of bio
thr <- as.numeric(readline(prompt = "Minimum cover of polygon to be taken as FOUND: ")) # should be percent

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

v <- vector %>%
  mark_found() %>%
  as.data.frame()

vv <- v %>%
  select(!!id, !!bio, ends_with("_found")) %>%
  pivot_longer(
    cols = ends_with("_found"),
    names_to = "mod",
    values_to = "found"
  )

# Step 1: Total and found counts
totals <- vv %>%
  group_by(mod, !!bio) %>%
  summarise(total = n_distinct(!!id), .groups = "drop")

found <- vv %>%
  filter(found == 1) %>%
  group_by(mod, !!bio) %>%
  summarise(Found = n_distinct(!!id), .groups = "drop")

# Step 2: Join and calculate proportions
plot_df <- totals %>%
  left_join(found, by = c("mod", as.character(bio))) %>%
  mutate(
    Found = replace_na(Found, 0),
    Not_found = total - Found
  ) %>%
  select(mod, !!bio, Found, Not_found) %>%
  pivot_longer(
    cols = c(Found, Not_found),
    names_to = "found_status",
    values_to = "n"
  ) %>%
  group_by(mod, !!bio) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Step 3: Identify top N bio per model based on "found" proportion
top <- plot_df %>%
  filter(found_status == "Found") %>%
  group_by(mod) %>%
  slice_max(order_by = prop, n = as.numeric(N)) %>%
  ungroup() %>%
  select(mod, !!bio)

# Step 4: Keep only those for plotting
plot_df_top <- plot_df %>%
  semi_join(top, by = c("mod", as.character(bio)))

# Step 5: Plot and save to PDF
pdf_name <- paste0("Relative_proportions_top_", N, "_thr_", thr, "%.pdf")
pdf(file = paste0("out/RP/", pdf_name), width = 10, height = 6)

for (model in unique(plot_df_top$mod)) {
  p <- plot_df_top %>%
    filter(mod == model) %>%
    ggplot(aes(x = reorder(!!bio, -prop), y = prop, fill = found_status)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_y_continuous(labels = scales::percent_format()) +
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
