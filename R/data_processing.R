# R/data_processing.R

# Turns a wide "one row per patient / one column per timepoint" Excel sheet
# into the long source/target/value format that networkD3::sankeyNetwork() expects.


data_preparation <- function(dades) {

  dades <- dades[, -1]
  # dades[dades == "n/a"] <- NA

  temps <- colnames(dades)
  n_temps <- length(temps)

  # Step 1: build links for every consecutive pair of timepoints
  links_list <- list()

  for (i in 1:(n_temps - 1)) {
    from_col <- temps[i]
    to_col   <- temps[i + 1]

    df_pair <- dades[, c(from_col, to_col)]
    df_pair <- df_pair[complete.cases(df_pair), ]

    df_temp <- data.frame(
      source = paste(from_col, df_pair[[from_col]], sep = "_"),
      target = paste(to_col,   df_pair[[to_col]],   sep = "_")
    )

    links_list[[i]] <- df_temp
  }

  # Combine all links and count unique transitions
  df_links <- do.call(rbind, links_list)
  df_links <- as.data.frame(table(df_links))  # adds a "Freq" column
  colnames(df_links) <- c("source", "target", "value")
  df_links <- df_links[df_links$value != 0, ]

  # Step 2: build unique nodes
  nodes_labels <- unique(c(df_links$source, df_links$target))
  df_nodes <- data.frame(name = nodes_labels)

  # Step 3: map node names to numeric IDs for the links
  df_links$source <- match(df_links$source, df_nodes$name) - 1
  df_links$target <- match(df_links$target, df_nodes$name) - 1

  list(links = df_links, nodes = df_nodes, num_temps = n_temps, col_names = temps)
}
