#' Heatmaps of Building Combinations with Measure of Max FAR
#'
#' Function for specifying maximum FAR and creating heatmaps that show all combinations.
#'
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param max_far Maximum FAR as a fraction
#' @return Multiple heatmaps and a print-out of the number of combinations in the console.
#' @examples
#' max_far(nrow = 2, ncol = 2, max_far = 6/4)
#' @export

max_far = function(nrow, ncol, max_far) {
  
  grid = 1 / nrow | 1 / ncol
  
  far_calc = max_far / grid
  
  xy = (nrow * ncol) * grid
  
  far_calc = far_calc * xy
  
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
  
  # All possible column combinations for matrix
  columns = t(permutations(n = far_calc+1, r = nrow_plus, v = c(0:far_calc), repeats.allowed = TRUE))
  columns = columns[, colSums(columns) >= 0]
  
  # Construct all possible combinations of column vectors
  l = lapply(as.data.frame(t(permutations(ncol(columns), ncol_plus, repeats.allowed = TRUE))), function(x) {
    m_c = columns[, x]
    
    if(all(rowSums(m_c) >= 0) & all(colSums(m_c) >= 0)) {
      m_c
      
    } else {
      NULL
    }
  })
  
  l = Filter(Negate(is.null), l)
  
  # Create dataframe and filter
  df = l %>%
    reshape2::melt() %>%
    filter(Var1 <= nrow, Var2 <= ncol) %>%
    mutate(value_far = value / (max(Var1) * max(Var2)), s_far = ave(value_far, L1, FUN=sum)) %>%
    filter(s_far <= max_far, s_far > 0) %>%
    mutate(ID = match(L1, unique(L1)))
  
  # Remove duplicate rows 
  if(ncol == 1 | nrow == 1){
    x = split(df$value, df$ID, df$Var1, df$Var2)
    
    u = unique(x)
    
    value = data.frame(matrix(unlist(u))) %>%
      dplyr::rename(value = matrix.unlist.u..)
    
    e = as.data.frame(matrix(0, ncol = 2, nrow = nrow(value))) %>%
      dplyr::rename("Var1" = 1, "Var2" = 2)
    
    df = e %>%
      mutate(Var1 = rep(1:nrow, each=1, length.out=nrow(e)), Var2 = rep(1:ncol, each=1, length.out=nrow(e))) %>%
      cbind(value) %>%
      mutate(ID = rep(1:nrow(e), each=nrow*ncol, length.out=nrow(e)))
  }
  
  # Update dataframe
  df = df %>%
    group_by(ID) %>%
    mutate(group_max = max(value), group_sum = sum(value)) %>% 
    ungroup() %>%
    group_by(group_sum, group_max, ID) %>%
    dplyr::mutate(density_id = cur_group_id()) %>%
    arrange(density_id)
  
  # Calculate number of combinations
  ncombinations = df %>%
    group_by(ID) %>%
    summarise(uniqueid = n_distinct(ID), .groups = 'drop') %>%
    summarise(sum = sum(uniqueid), .groups = 'drop')
  
  # Print number of possible combinations
  cat("Number of possible combinations:", ncombinations$sum, " ")
  
  # Set border width for tiles
  if(ncombinations$sum < 250) {
    b = 0.15
    
  } else if(ncombinations$sum >= 250 & ncombinations$sum < 500) {
    b = 0.05
    
  } else {
    b = 0.01
  }
  
  # Heatmaps
  multi_heatmaps = ggplot(data = df, mapping = aes(x = factor(Var2), y = factor(Var1))) +
    geom_tile(aes(fill = factor(value)), colour = "#000000", linewidth = b) +
    scale_fill_manual(values = viridis(max(df$value)+1, option = "cividis")) +
    coord_equal() +
    facet_wrap(~density_id) +
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
  
  multi_heatmaps
}