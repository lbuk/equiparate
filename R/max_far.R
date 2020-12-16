#' Heatmaps of Building Combinations with Measure of Max FAR
#'
#' Function for specifying maximum FAR and creating heatmaps that show all combinations.
#'
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param max_far Maximum FAR
#' @param filename Filename for PDF
#' @return A PDF with multiple heatmaps and the number of combinations.
#' @examples
#' max_far(nrow = 2, ncol = 2, max_far = 6/4, filename = "max_far_nrow2_ncol2_maxfar6div4.pdf")
#' @export

max_far = function(nrow, ncol, max_far, filename) {

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

  # Filter dataset
  l_max_far_df =
    l %>%
    reshape2::melt() %>%
    filter(Var1 <= nrow, Var2 <= ncol) %>%
    mutate(value_far = value / (max(Var1) * max(Var2))) %>%
    mutate(s_far = ave(value_far, L1, FUN=sum)) %>%
    filter(s_far <= max_far, s_far > 0) %>%
    mutate(ID = match(L1, unique(L1)))

  # Remove duplicate rows
  if(ncol == 1 | nrow == 1){
    x = split(l_max_far_df$value, l_max_far_df$ID, l_max_far_df$Var1, l_max_far_df$Var2)
    u = unique(x)

    df_m = data.frame(matrix(unlist(u)))

    value =
      df_m %>%
      rename(value = matrix.unlist.u..)

    m_e = as.data.frame(matrix(0, ncol = 2, nrow = nrow(value)))

    colnames(m_e) = c("Var1", "Var2")

    l_max_far_df =
      m_e %>%
      mutate(Var1 = rep(1:nrow, each=1, length.out=nrow(m_e))) %>%
      mutate(Var2 = rep(1:ncol, each=1, length.out=nrow(m_e))) %>%
      cbind(value) %>%
      mutate(ID = rep(1:nrow(m_e), each=nrow*ncol, length.out=nrow(m_e)))
  }

  # Calculate number of combinations
  n_combinations =
    l_max_far_df %>%
    group_by(ID) %>%
    summarise(uniqueid = n_distinct(ID)) %>%
    summarise(sum = sum(uniqueid))

  # Print number of possible combinations
  cat("Number of possible combinations:", n_combinations$sum, " ")
  
  # Set border width for tiles
  if(n_combinations$sum < 250){b = 1} else if(n_combinations$sum >= 250 & n_combinations$sum < 500){b = 0.2} else{b = 0.05}

  # Palette from viridis package
  col = viridis(max(l_max_far_df$value)+1, option = "cividis")

  # Heatmaps
  plot_l_max_far_df =
    ggplot(data = l_max_far_df, mapping = aes(x = factor(Var2), y = factor(Var1))) +
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

  ggsave(filename, plot_l_max_far_df, device = "pdf", height = 7, width = 7)

}
