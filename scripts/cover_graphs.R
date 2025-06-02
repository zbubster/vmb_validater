# cover_graphs

#---#---#---#---#---#---#---#---#---#---#---#---#
#                  User input                   #
#---#---#---#---#---#---#---#---#---#---#---#---#

# select relevant cols
cols <- names(vector) %>%
  .[!grepl("(_area|_percent)$", .)]

#---#---#---#---#---#---#---#---#---#---#---#---#
# Select ID column

cat("Which column contains unique polygon ID?\n")
for (i in seq_along(cols)) cat(sprintf("[%d] %s\n", i, cols[i]))
repeat {
  input <- readline("Type column number or name: ")
  if (input %in% cols) { id_name <- input; break }
  num <- suppressWarnings(as.numeric(input))
  if (!is.na(num) && num %in% seq_along(cols)) { id_name <- cols[num]; break }
  cat("Invalid input. Please try again.\n")
}

#---#---#---#---#---#---#---#---#---#---#---#---#
# Select BIO column
# values in this column will be used on X axis

cat("\nWhich column contains habitat/biotope codes?\n")
for (i in seq_along(cols)) cat(sprintf("[%d] %s\n", i, cols[i]))
repeat {
  input <- readline("Type column number or name: ")
  if (input %in% cols) { bio_name <- input; break }
  num <- suppressWarnings(as.numeric(input))
  if (!is.na(num) && num %in% seq_along(cols)) { bio_name <- cols[num]; break }
  cat("Invalid input. Please try again.\n")
}

# make it symbol
id <- sym(id_name)
bio <- sym(bio_name)

# recap
cat("\nAs ID you selected:", id_name)
cat("\nAs bio group you selected:", bio_name, "\n\n")

#---#---#---#---#---#---#---#---#---#---#---#---#
# AREA or PERCENT

repeat {
  type_input <- tolower(readline("Do you want to plot [A]bsolute area or [P]ercent cover? [A/P]: "))
  if (type_input %in% c("a", "p")) break
  cat("Invalid input. Type 'A' or 'P'\n")
}
type_suffix <- ifelse(type_input == "a", "_area", "_percent")
cat("You selected:", ifelse(type_input == "a", "absolute area", "relative percent"), "\n\n")

#---#---#---#---#---#---#---#---#---#---#---#---#
# extract relevant columns

cols_use <- names(vector)[grepl(paste0(type_suffix, "$"), names(vector))]

#---#---#---#---#---#---#---#---#---#---#---#---# 
#               Data preparation                #
#---#---#---#---#---#---#---#---#---#---#---#---#

v <- vector %>%
  as.data.frame() %>%
  select(!!id, !!bio, all_of(cols_use)) %>%
  pivot_longer(
    cols = all_of(cols_use),
    names_to = "model",
    values_to = "value"
  ) %>%
  mutate(model = gsub(paste0(type_suffix, "$"), "", model))  # clean model name

#---#---#---#---#---#---#---#---#---#---#---#---# 
#                     Plots                     #
#---#---#---#---#---#---#---#---#---#---#---#---#

# create output dir
out_dir <- file.path("out", ifelse(type_suffix == "_area", "boxplot_area", "boxplot_percent"))
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
pdf_file <- file.path(out_dir, paste0("boxplots_", ifelse(type_suffix == "_area", "area", "percent"), ".pdf"))
pdf(pdf_file, width = 10, height = 6)

#---#---#---#---#---#---#---#---#---#---#---#---#
# plotting cycle
for (mod in unique(v$model)) {
  plot_data <- v %>% filter(model == mod)
  
  # if area, than exclude 0 and negative values (log transformation incoming)
  if (type_suffix == "_area") {
    zero_count <- sum(v$model == mod & v$value <= 0)
    if (zero_count > 0) cat("Warning: Removed", zero_count, "non-positive values for log-scale\n")
    plot_data <- plot_data %>% filter(value > 0)
  }
  
  # sort by median (both area and percent)
  ordering_df <- plot_data %>%
    group_by(!!bio) %>%
    summarise(med = median(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(med))
  
  ordering <- ordering_df %>% pull(!!bio) %>% as.character()
  
  # apply order on data
  plot_data[[as.character(bio)]] <- factor(plot_data[[as.character(bio)]], levels = ordering)
  
  # FOR PERCENT ONLY where is 5% threshold
  index_cut <- if (type_suffix == "_percent") which(ordering_df$med < 5)[1] else NA
  red_line <- if (!is.na(index_cut)) index_cut - 0.5 else NA
  
  # number of observations per bio
  counts_df <- plot_data %>%
    group_by(!!bio) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(!!as.character(bio) := factor(!!bio, levels = ordering))
  
  # relative positions of labels
  y_min <- min(plot_data$value, na.rm = TRUE)
  y_max <- max(plot_data$value, na.rm = TRUE)
  y_range <- y_max - y_min
  y_label_n <- y_max + y_range * 0.05
  y_label_cut <- y_min - y_range * 0.05
  
  # plot it!
  p <- ggplot(plot_data, aes(x = !!bio, y = value)) +
    geom_boxplot(outlier.size = 0.8, fill = "skyblue") +
    
    # n obs, vertically
    geom_text(
      data = counts_df,
      aes(x = !!bio, y = y_label_n, label = paste0("n = ", n)),
      angle = 90, hjust = -0.1, vjust = 0.5, size = 3
    ) +
    
    # PERCENT: red line
    { if (!is.na(red_line)) list(
      geom_vline(xintercept = red_line, color = "red", linetype = "dashed"),
      annotate("text", x = red_line + 0.5, y = y_label_cut,
               label = "median < 5%", color = "red", size = 3, hjust = 0)
    ) else NULL } +
    
    # Y axis labels (percent: 0:100, area: log10 scale)
    { if (type_suffix == "_percent") {
      scale_y_continuous(breaks = seq(0, 100, 10), expand = expansion(mult = c(0.02, 0.15)))
    } else {
      scale_y_log10(
        expand = expansion(mult = c(0.02, 0.15)),
        breaks = scales::log_breaks(base = 10)
      )
    }
    } +
    
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = paste("Coverage by model:", mod),
      x = as.character(bio),
      y = ifelse(type_suffix == "_area", "Area (log10 m²)", "Percent cover")
    )
  
  print(p)
}

# turn graphical device off
dev.off()
# heureca
cat("Output is saved in", out_dir, "\n")

#---#---#---#---#---#---#---#---#---#---#---#---#
# cleaning

rm(pdf_file, v, p, bio, bio_name, id, id_name, plot_data, ordering, i, index_cut, input, num, out_dir, red_label, red_line, reference_band, type_input, type_suffix, y_label_cut, y_label_n, y_max, y_min, y_range, zero_count, mod, cols, cols_use, counts_df, ordering_df)
gc()
