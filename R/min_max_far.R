#' Heatmaps of Building Combinations with Measure of Min and Max FAR
#'
#' Function for specifying minimum and maximum FAR and creating heatmaps that show all combinations.
#'
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param min_far Minimum FAR as a fraction
#' @param max_far Maximum FAR as a fraction
#' @param filename Filename for PDF
#' @return A PDF with multiple heatmaps and a printing of the number of combinations in the console.
#' @examples
#' min_max_far(nrow = 2, ncol = 2, min_far = 3/4, max_far = 6/4, filename = "min_max_far_nrow2_ncol2_minfar3div4_maxfar6div4.pdf")
#' @export

min_max_far = function(nrow, ncol, min_far, max_far, filename) {

  grid = 1/nrow | 1/ncol

  max_far_calc = max_far/grid
  xy = (nrow * ncol) * grid
  max_far_calc_ii = max_far_calc * xy

  nrow_plus = nrow
  ncol_plus = ncol

  if(nrow == 1) {nrow_plus = nrow+1}
  if(ncol == 1) {ncol_plus = ncol+1}

  # All possible column combinations for matrix
  columns = t(permutations(n = max_far_calc_ii+1, r = nrow_plus, v = c(0:max_far_calc_ii), repeats.allowed = TRUE));
  columns = columns[, colSums(columns) >= 0];

  # Construct all possible combinations of column vectors
  l = lapply(as.data.frame(t(permutations(ncol(columns), ncol_plus, repeats.allowed = TRUE))), function(x) {
    m_c = columns[, x];
    if (all(rowSums(m_c) >= 0) & all(colSums(m_c) >= 0)) m_c else NULL;
  })

  l = Filter(Negate(is.null), l);

  # Create dataframe and filter
  l_min_max_far_df =
    l %>%
    reshape2::melt() %>%
    filter(Var1 <= nrow, Var2 <= ncol) %>%
    mutate(value_far = value / (max(Var1) * max(Var2))) %>%
    mutate(s_far = ave(value_far, L1, FUN=sum)) %>%
    filter(s_far <= max_far, s_far >= min_far, s_far > 0) %>%
    mutate(ID = match(L1, unique(L1))) %>%
    group_by(ID) %>%
    filter(n() == ncol*nrow)

  # Remove duplicate groups
  if(ncol == 1 | nrow == 1){
    x = split(l_min_max_far_df$value, l_min_max_far_df$ID, l_min_max_far_df$Var1, l_min_max_far_df$Var2)
    u = unique(x)

    df_m = data.frame(matrix(unlist(u)))

    value =
      df_m %>%
      rename(value = matrix.unlist.u..)

    m_e = as.data.frame(matrix(0, ncol = 2, nrow = nrow(value)))

    colnames(m_e) = c("Var1", "Var2")

    l_min_max_far_df =
      m_e %>%
      mutate(Var1 = rep(1:nrow, each=1, length.out=nrow(m_e))) %>%
      mutate(Var2 = rep(1:ncol, each=1, length.out=nrow(m_e))) %>%
      cbind(value) %>%
      mutate(ID = rep(1:nrow(m_e), each=nrow*ncol, length.out=nrow(m_e)))
  }

  # Calculate number of combinations and print in console
  n_combinations =
    l_min_max_far_df %>%
    group_by(ID) %>%
    summarise(uniqueid = n_distinct(ID), .groups = 'drop') %>%
    summarise(sum = sum(uniqueid), .groups = 'drop')

  cat("Number of possible combinations:", n_combinations$sum, " ")
  
  # Set border width for tiles
  if(n_combinations$sum < 250){b = 0.15} else if(n_combinations$sum >= 250 & n_combinations$sum < 500){b = 0.05} else{b = 0.01}
  
  # Palette from viridis package
  col = viridis((max(l_min_max_far_df$value) - min(l_min_max_far_df$value))+1, option = "viridis")

  # Heatmaps
  plot_l_min_max_far_df =
    ggplot(data = l_min_max_far_df, mapping = aes(x = factor(Var2), y = factor(Var1))) +
    geom_tile(aes(fill = factor(value)), colour = "#000000", size = b) +
    scale_fill_manual(values = col) +
    coord_equal() +
    facet_wrap(~ID) +
    labs(fill = "STOREYS") +
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
  ggsave(filename, plot_l_min_max_far_df, device = "pdf", height = 7, width = 7)
}
