# graphs of cover and relative proportions

# INPUT: vector updated by vypocet_plochy.R (it should have cover area and percent of coverage of each polygon by each raster from raster list)
# OUTPUT: graphs in /out folder

#---#---#---#---#---#---#---#---#---#---#---#---#
#                  User input                   #
#---#---#---#---#---#---#---#---#---#---#---#---#

# colnames for next inputs
cols <- names(vector) %>%
  .[!grepl("(_area|_percent)$", .)]

#---#---#---#---#---#---#---#---#---#---#---#---#
# input for polygon ID
# user should choose collumn with ID of polygons

# call
cat("Which collumn contain unique identifier of polygon (ID)?:\n") # SEGMENT_ID
# print options
for (i in seq_along(cols)) {
  cat(sprintf("[%d] %s\n", i, cols[i]))
}

# wait for valid option selection
repeat {
  input <- readline(prompt = "Type column number or name: ")
  if (input %in% cols) {
    id_name <- input
    break
  }
  num <- suppressWarnings(as.numeric(input))
  if (!is.na(num) && num %in% seq_along(cols)) {
    id_name <- cols[num]
    break
    }
  cat("Invalid input. Please try again.\n")
}
id <- sym(id_name) # convert to symbol
# call out
cat("As ID you selected column:", id_name, "\n\n")
cat("------------------------------------------\n\n")

#---#---#---#---#---#---#---#---#---#---#---#---#
# input for biotope/habitat cathegory
# user should define which collumn contain habitat cathegories
# data will be divided by this collumn

# call
cat("Which collumn contain habitat cathegories?:\n") # BIOTOPE_CODES
# print options
for (i in seq_along(cols)) {
  cat(sprintf("[%d] %s\n", i, cols[i]))
}

# wait for valid option selection
repeat {
  input <- readline(prompt = "Type column number or name: ")
  if (input %in% cols) {
    bio_name <- input
    break
  }
  num <- suppressWarnings(as.numeric(input))
  if (!is.na(num) && num %in% seq_along(cols)) {
    bio_name <- cols[num]
    break
  }
  cat("Invalid input. Please try again.\n")
}
bio <- sym(bio_name) # convert to symbol
# call out
cat("As habitat cathegory you selected column:", bio_name, "\n\n")
cat("------------------------------------------\n\n")

#---#---#---#---#---#---#---#---#---#---#---#---#
# N
# represent number of cathegories which will be plotted
# should be between 1 and n of cathegories
### !! graphs with higher N are not readable anymore !!!!!!!!!

# upper limit (maximum of plottable cathegories)
n_vals <- nrow(unique(vector[[as.character(bio)]]))
# call
cat("How many cathegories you want to plot? max:", n_vals, "\n")
# wait for valid input
repeat {
  input <- readline(prompt = paste0("Select a number between 1 and ", n_vals, ": "))
  N <- suppressWarnings(as.integer(input)) # INTEGER
  
  if (!is.na(N) && N >= 1 && N <= n_vals) {
    break
  }
  cat("Invalid input. Please try again.\n")
}
# call out
cat("There will be", N, "cathegories plotted. \n\n")
cat("------------------------------------------\n\n")

#---#---#---#---#---#---#---#---#---#---#---#---#
# thr
# this is threshold, from which polygons are takjen as found in analysis
# it represents minimal percentage cover of polygon by model (classification raster)

cat("Minimum percent cover, to be taken as FOUND? \n")
# wait for valid input
repeat {
  input <- readline(prompt = paste0("Select a number between 0.1 and 100: "))
  thr <- suppressWarnings(as.double(input)) # handled as double
  
  if (!is.na(thr) && thr >= 0.1 && thr <= 100) { # valid?
    break
  }
  cat("Invalid input. Please try again.\n")
}
# call out
cat("All polygons with cover bigger than", thr, "percent will be taken as FOUND.\n\n")
cat("------------------------------------------\n\n")

#---#---#---#---#---#---#---#---#---#---#---#---# 
# cleaning
rm(bio_name, cols, i, id_name, input, n_vals, num)

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

#---#---#---#---#---#---#---#---#---#---#---#---#

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

#---#---#---#---#---#---#---#---#---#---#---#---#
# select only top cathegories based on N defined by user

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

#---#---#---#---#---#---#---#---#---#---#---#---#
# plot loop

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
cat("Output is saved in folder out/RP/ \n")

#---#---#---#---#---#---#---#---#---#---#---#---#
# cleaning

rm(pdf_name, plot_df, plot_df_top, top, totals, found, v, p, bio, id, model, N, thr, labels_df, plot_data, ordering)
gc()
