# cover_graphs

#---#---#--- User input: Select id and bio columns ---#---#---

cols <- names(vector) %>%
  .[!grepl("(_area|_percent)$", .)]

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
id <- sym(id_name)

# Select BIO column
cat("\nWhich column contains habitat/biotope codes?\n")
for (i in seq_along(cols)) cat(sprintf("[%d] %s\n", i, cols[i]))
repeat {
  input <- readline("Type column number or name: ")
  if (input %in% cols) { bio_name <- input; break }
  num <- suppressWarnings(as.numeric(input))
  if (!is.na(num) && num %in% seq_along(cols)) { bio_name <- cols[num]; break }
  cat("Invalid input. Please try again.\n")
}
bio <- sym(bio_name)

cat("\nAs ID you selected:", id_name)
cat("\nAs bio group you selected:", bio_name, "\n\n")

#---#---#--- User input: Area or Percent? ---#---#---

repeat {
  type_input <- tolower(readline("Do you want to plot [A]bsolute area or [P]ercent cover? [A/P]: "))
  if (type_input %in% c("a", "p")) break
  cat("Invalid input. Type 'A' or 'P'\n")
}

type_suffix <- ifelse(type_input == "a", "_area", "_percent")
cat("You selected:", ifelse(type_input == "a", "absolute area", "relative percent"), "\n\n")

#---#---#--- Extract relevant columns ---#---#---

cols_use <- names(vector)[grepl(paste0(type_suffix, "$"), names(vector))]

#---#---#--- Data prep ---#---#---

v <- vector %>%
  as.data.frame() %>%
  select(!!id, !!bio, all_of(cols_use)) %>%
  pivot_longer(
    cols = all_of(cols_use),
    names_to = "model",
    values_to = "value"
  ) %>%
  mutate(model = gsub(paste0(type_suffix, "$"), "", model))  # clean model name

#---#---#--- Plotting ---#---#---

# create output dir
out_dir <- file.path("out", ifelse(type_suffix == "_area", "boxplot_area", "boxplot_percent"))
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

pdf_file <- file.path(out_dir, paste0("boxplots_", ifelse(type_suffix == "_area", "area", "percent"), ".pdf"))
pdf(pdf_file, width = 10, height = 6)

for (mod in unique(v$model)) {
  plot_data <- v %>% filter(model == mod)
  
  p <- ggplot(plot_data, aes(x = !!bio, y = value)) +
    geom_boxplot(outlier.size = 0.8, fill = "skyblue") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = paste("Coverage by model:", mod),
      x = as.character(bio),
      y = ifelse(type_suffix == "_area", "Area (m²)", "Percent cover")
    )
  
  print(p)
}

dev.off()
cat("PDF with boxplots saved in:", out_dir, "\n")
