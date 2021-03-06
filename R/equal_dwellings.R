#' Heatmaps of Sites with Equal Dwellings
#'
#' Function for creating heatmaps of sites that have an equal number of dwellings.
#'
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param dwellings Number of dwellings
#' @param filename Filename for PDF
#' @return A PDF with multiple heatmaps and a printing of the number of combinations in the console.
#' @examples
#' equal_dwellings(nrow = 3, ncol = 2, dwellings = 4, filename = "equal_dwellings_nrow3_ncol2_dwellings4.pdf")
#' @export

equal_dwellings = function(nrow, ncol, dwellings, filename) {

  nrow_plus = nrow
  ncol_plus = ncol

  if(nrow == 1) {nrow_plus = nrow+1}
  if(ncol == 1) {ncol_plus = ncol+1}

  # All possible column combinations for a matrix
  columns = t(permutations(n = dwellings+1, r = nrow_plus, v = c(0:dwellings), repeats.allowed = TRUE));
  columns = columns[, colSums(columns) >= 0];

  # Construct all possible combinations of column vectors
  l = lapply(as.data.frame(t(permutations(ncol(columns), ncol_plus, repeats.allowed = TRUE))), function(x) {
    m_c = columns[, x];
    if (all(rowSums(m_c) > 0) & all(colSums(m_c) > 0)) m_c else NULL;
    if (sum(m_c) == dwellings) m_c else NULL;
  })

  l = Filter(Negate(is.null), l);

  # Create dataframe and filter
  l_dwellings_df =
    l %>%
    reshape2::melt() %>%
    filter(Var1 <= nrow, Var2 <= ncol)  %>%
    mutate(s = ave(value, L1, FUN=sum)) %>%
    mutate(ID = match(L1, unique(L1))) %>%
    filter(s == dwellings)

  # Calculate number of combinations and print in console
  n_combinations =
    l_dwellings_df %>%
    group_by(ID) %>%
    summarise(uniqueid = n_distinct(ID), .groups = 'drop') %>%
    summarise(sum = sum(uniqueid), .groups = 'drop')
  
  # Print number of possible combinations
  cat("Number of possible combinations:", n_combinations$sum, " ")
  
  # Set border width for tiles
  if(n_combinations$sum < 250){b = 1} else if(n_combinations$sum >= 250 & n_combinations$sum < 500){b = 0.2} else{b = 0.05}

  # Palette from viridis package
  col = viridis(dwellings+1, option = "plasma")

  # Heatmaps
  plot_equal_dwellings_df =
    ggplot(data = l_dwellings_df, mapping = aes(x = factor(Var2), y = factor(Var1))) +
    geom_tile(aes(fill = factor(value)), colour = "#000000", size = b) +
    scale_fill_manual(values=col) +
    coord_equal() +
    facet_wrap(~ID) +
    labs(fill = "DWELLINGS") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.title = element_text(size = 9, face = "bold"),
          legend.position = 'bottom',
          legend.justification = 'center',
          legend.direction = "horizontal",
          legend.key.width = unit(0.90, "cm"),
          legend.key.height = unit(0.20, "cm"),
          legend.spacing.x = unit(0.099, "cm"),
          legend.spacing.y = unit(0.09, "cm"),
          panel.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank()) +
    guides(fill = guide_legend(nrow = 1, label.position = "bottom", title.position = "top", title.hjust = 0.5))

  if(dev.cur() > 1) dev.off()
  
  # Export to PDF
  ggsave(filename, plot_equal_dwellings_df, device = "pdf", height = 7, width = 7)
}
