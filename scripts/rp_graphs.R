# graphs of cover and relative proportions

# INPUT: vector updated by vypocet_plochy.R (it should have cover area and percent of coverage of each polygon by each raster from raster list)
# OUTPUT: graphs


mark_found <- function(vctr, threshold = 10){
  # which colls are percent
  percent_cols <- grep("_percent$", names(vctr), value = TRUE)
  # loop through each percent collumn
  for (col in percent_cols) {
    found_col <- gsub("_percent$", "_found", col)
    vctr[[found_col]] <- ifelse(vctr[[col]] > threshold, 1, 0)
  }
  return(vctr)
}


v <- vector_updated
v <- mark_found(v)
# str(v)
# writeVector(v, "data/processing/found3.gpkg")

v <- as.data.frame(v)

library(rlang)

id <- readline(prompt = "SEGMENT_ID")
id <- sym(id)
bio <- readline(prompt = "BIOTOP_CODES")
bio <- sym(bio)
N <- readline(prompt = "Number of top plotted ground truth cathegories:")

# kat <- unique(
#   names(
#     v[grep("_found$", names(v), value = TRUE)]
#     )
#   )
# kat <- unlist(strsplit(kat, "_found"))

vv <- v %>%
  select(!!id, !!bio, ends_with("_found")) %>%
  pivot_longer(
    cols = ends_with("_found"),
    names_to = "kat",
    values_to = "found"
  )

  
str(vv)
vv

# Step 1: Total and found counts
totals <- vv %>%
  group_by(kat, !!bio) %>%
  summarise(total = n_distinct(!!id), .groups = "drop")

found <- vv %>%
  filter(found == 1) %>%
  group_by(kat, !!bio) %>%
  summarise(found_n = n_distinct(!!id), .groups = "drop")

# Step 2: Join and calculate proportions
plot_df <- totals %>%
  left_join(found, by = c("kat", as.character(bio))) %>%
  mutate(
    found_n = replace_na(found_n, 0),
    not_found_n = total - found_n
  ) %>%
  select(kat, !!bio, found_n, not_found_n) %>%
  pivot_longer(
    cols = c(found_n, not_found_n),
    names_to = "found_status",
    values_to = "n"
  ) %>%
  group_by(kat, !!bio) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Step 3: Identify top N bio per model based on "found" proportion
top <- plot_df %>%
  filter(found_status == "found_n") %>%
  group_by(kat) %>%
  slice_max(order_by = prop, n = as.numeric(N)) %>%
  ungroup() %>%
  select(kat, !!bio)

# Step 4: Keep only those for plotting
plot_df_top <- plot_df %>%
  semi_join(top, by = c("kat", as.character(bio)))

# Step 5: Plot and save to PDF
pdf("out/top_found_by_model.pdf", width = 10, height = 6)

for (model in unique(plot_df_top$kat)) {
  p <- plot_df_top %>%
    filter(kat == model) %>%
    ggplot(aes(x = reorder(!!bio, -prop), y = prop, fill = found_status)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = paste("Top", N, as.character(bio), "by found polygon proportion"),
      subtitle = paste("Model:", model),
      x = as.character(bio),
      y = "Proportion",
      fill = "Found Status"
    )
  print(p)
}

dev.off()
