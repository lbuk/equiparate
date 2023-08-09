#' Heatmaps of Floorplans with an Equal Number of Units
#'
#' Function for comparing the adaptability of floorplans by creating heatmaps with an equal number of units.
#'
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param units Number of units
#' @return Multiple heatmaps and a print-out of the number of combinations in the console.
#' @examples
#' equal_units(nrow = 4, ncol = 3, units = 4)
#' @export

equal_units = function(nrow, ncol, units) {
  
  if(nrow == 1) {
    nrow_plus = nrow + 1
    
  } else {
    nrow_plus = nrow
  }
  
  if(ncol == 1) {
    ncol_plus = ncol + 1
    
  } else {
    ncol_plus = ncol
  }
  
  # All possible column combinations for a matrix
  columns = t(permutations(n = 2, r = nrow_plus, v = c(0:units), repeats.allowed = TRUE))
  columns = columns[, colSums(columns) >= 0]
  
  # Construct all possible combinations of column vectors
  l = lapply(as.data.frame(t(permutations(ncol(columns), ncol_plus, repeats.allowed = TRUE))), function(x) {
    m_c = columns[, x]
    
    if(all(rowSums(m_c) > 0) & all(colSums(m_c) > 0)) {
      m_c
      
    } else {
      NULL
    }
    
    if(sum(m_c) == units) {
      m_c
      
    } else {
      NULL
    }
  })
  
  l = Filter(Negate(is.null), l)
  
  # Create dataframe
  df = l %>%
    reshape2::melt() %>%
    filter(Var1 <= nrow, Var2 <= ncol)  %>%
    mutate(z = ave(value, L1, FUN = function(x) sum(x == 0)), s = ave(value, L1, FUN=sum)) %>%
    filter(s == units) %>%
    mutate(ID = match(L1, unique(L1))) %>%
    group_by(ID) %>%
    mutate(group_max = max(value)) %>% 
    ungroup() %>%
    group_by(group_max, ID) %>%
    dplyr::mutate(density_id = cur_group_id()) %>%
    arrange(density_id)
  
  # Calculate number of combinations
  n = df %>%
    group_by(ID) %>%
    summarise(uniqueid = n_distinct(ID), .groups = 'drop') %>%
    summarise(sum = sum(uniqueid), .groups = 'drop')
  
  # Print number of possible combinations
  cat("Number of possible combinations:", n$sum, " ")
  
  # Set border width for tiles
  if(n$sum < 250) {
    b = 0.08
    
  } else if(n$sum >= 250 & n$sum < 500) {
    b = 0.02
    
  } else {
    b = 0.01
  }
  
  # Heatmaps
  multi_heatmaps = ggplot(data = df, mapping = aes(x = factor(Var2), y = factor(Var1))) +
    geom_tile(aes(fill = factor(value)), colour = "#000000", linewidth = b) +
    scale_fill_manual(values = c('1' = "#1052B5", '0' = "#ffffff"), breaks = c("1"), labels = c("UNITS")) +
    coord_equal() +
    facet_wrap(~density_id) + 
    labs(fill = "") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.title = element_text(size = 9, face = "bold"),
          legend.position = 'top', 
          legend.justification = 'center',
          legend.direction = "horizontal",
          legend.key.width = unit(2.01, "cm"),
          legend.key.height = unit(0.18, "cm"),
          legend.spacing.x = unit(0.099, "cm"),
          legend.spacing.y = unit(0.09, "cm"),
          panel.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank()) +
    guides(fill = guide_legend(nrow = 1, label.position = "bottom", title.position = "top", title.hjust = 0.5))
  
  multi_heatmaps
}