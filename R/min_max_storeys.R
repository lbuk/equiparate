#' Heatmaps of Building Combinations with Measure of Min and Max Storey
#'
#' Function for specifying minimum and maximum storey and creating heatmaps that show all combinations.
#'
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param min_storey Minimum storey
#' @param max_storey Maximum storey
#' @param filename Filename for PDF
#' @return A PDF with multiple heatmaps.
#' @examples
#' min_max_storeys(nrow = 1, ncol = 3, min_storey = 3, max_storey = 6, filename = "min_max_storeys_nrow1_ncol3_min3_max6.pdf")
#' @export

min_max_storeys = function(nrow, ncol, min_storey, max_storey, filename) {

  nrow_plus = nrow
  ncol_plus = ncol

  if(nrow == 1) {nrow_plus = nrow+1}
  if(ncol == 1) {ncol_plus = ncol+1}

  # All possible column combinations for a matrix
  columns = t(permutations(n = max_storey, r = nrow_plus, v = c(1:max_storey), repeats.allowed = TRUE));
  columns = columns[, colSums(columns) >= 0];

  # Construct all possible combinations of column vectors
  l = lapply(as.data.frame(t(permutations(ncol(columns), ncol_plus, repeats.allowed = TRUE))), function(x) {
    m_c = columns[, x];
    if (all(rowSums(m_c) > 0) & all(colSums(m_c) > 0)) m_c else NULL;
  })

  l = Filter(Negate(is.null), l);

  # Filter dataset
  l_min_max_storeys_df =
    l %>%
    reshape2::melt() %>%
    filter(Var1 <= nrow, Var2 <= ncol)  %>%
    mutate(z = ave(value, L1, FUN = function(x) sum(x == 0))) %>%
    mutate(s = ave(value, L1, FUN=sum)) %>%
    mutate(s_storeys = ave(value, L1, FUN=sum)) %>%
    filter(value >= min_storey) %>%
    mutate(ID = match(L1, unique(L1))) %>%
    group_by(ID) %>%
    filter(n() == ncol*nrow)

  # Remove duplicate groups
  if(ncol == 1 | nrow == 1){
    x = split(l_min_max_storeys_df$value, l_min_max_storeys_df$ID, l_min_max_storeys_df$Var1, l_min_max_storeys_df$Var2)
    u = unique(x)

    df_m = data.frame(matrix(unlist(u)))

    value =
      df_m %>%
      rename(value = matrix.unlist.u..)

    m_e = as.data.frame(matrix(0, ncol = 2, nrow = nrow(value)))

    colnames(m_e) = c("Var1", "Var2")

    l_min_max_storeys_df =
      m_e %>%
      mutate(Var1 = rep(1:nrow, each=1, length.out=nrow(m_e))) %>%
      mutate(Var2 = rep(1:ncol, each=1, length.out=nrow(m_e))) %>%
      cbind(value) %>%
      mutate(ID = rep(1:nrow(m_e), each=nrow*ncol, length.out=nrow(m_e)))
  }

  # Calculate number of combinations and print
  n_combinations =
    l_min_max_storeys_df %>%
    group_by(ID) %>%
    summarise(uniqueid = n_distinct(ID)) %>%
    summarise(sum = sum(uniqueid))

  cat("Number of possible combinations:", n_combinations$sum, " ")

  col = viridis((max_storey - min_storey)+1, option = "viridis")

  # Visualise using heatmaps
  plot_l_min_max_storeys_df =
    ggplot(data = l_min_max_storeys_df, mapping = aes(x = factor(Var2), y = factor(Var1))) +
    geom_tile(aes(fill = factor(value)), colour = "#000000", size = 1) +
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

  ggsave(filename, plot_l_min_max_storeys_df, device = "pdf", height = 7, width = 7)
}
