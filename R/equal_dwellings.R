#' Heatmaps of Sites with Equal Dwellings
#'
#' Function for creating heatmaps of sites that have an equal number of dwellings.
#'
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param dwellings Number of dwellings
#' @return Multiple heatmaps and a print-out of the number of combinations in the console.
#' @examples
#' equal_dwellings(nrow = 3, ncol = 2, dwellings = 4)
#' @export

equal_dwellings = function(nrow, ncol, dwellings) {

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
  columns = t(permutations(n = dwellings+1, r = nrow_plus, v = c(0:dwellings), repeats.allowed = TRUE))
  columns = columns[, colSums(columns) >= 0]

  # Construct all possible combinations of column vectors
  l = lapply(as.data.frame(t(permutations(ncol(columns), ncol_plus, repeats.allowed = TRUE))), function(x) {
    m_c = columns[, x]
    
    if(all(rowSums(m_c) > 0) & all(colSums(m_c) > 0)) {
      m_c
      
    } else {
      NULL
    }
    
    if (sum(m_c) == dwellings) {
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
    mutate(s = ave(value, L1, FUN=sum), ID = match(L1, unique(L1))) %>%
    filter(s == dwellings) %>%
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
    b = 1
    
  } else if(n$sum >= 250 & n$sum < 500) {
    b = 0.2
    
  } else {
    b = 0.05
  }

  # Heatmaps
  multi_heatmaps = ggplot(data = df, mapping = aes(x = factor(Var2), y = factor(Var1))) +
    geom_tile(aes(fill = factor(value)), colour = "#000000", size = b) +
    scale_fill_manual(values = viridis(dwellings+1, option = "plasma")) +
    coord_equal() +
    facet_wrap(~density_id) +
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
  
  multi_heatmaps
}