#' Heatmaps of Building Combinations with Measure of Max Storeys
#'
#' Function for specifying maximum storeys and creating heatmaps that show all combinations.
#'
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param max_storey Maximum storey
#' @return Multiple heatmaps and a printing of the number of combinations in the console.
#' @examples
#' max_storeys(nrow = 1, ncol = 3, max_storey = 3)
#' @export

max_storeys = function(nrow, ncol, max_storey) {

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
  columns = t(permutations(n = max_storey, r = nrow_plus, v = c(1:max_storey), repeats.allowed = TRUE))
  columns = columns[, colSums(columns) >= 0]

  # Construct all possible combinations of column vectors
  l = lapply(as.data.frame(t(permutations(ncol(columns), ncol_plus, repeats.allowed = TRUE))), function(x) {
    m_c = columns[, x]
    
    if(all(rowSums(m_c) > 0) & all(colSums(m_c) > 0)) {
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
    mutate(ID = match(L1, unique(L1))) %>%
    filter(value <= max_storey)

  # Remove duplicate groups
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

  # Visualise using heatmaps
  multi_heatmaps = ggplot(data = df, mapping = aes(x = factor(Var2), y = factor(Var1))) +
    geom_tile(aes(fill = factor(value)), colour = "#000000", linewidth = b) +
    scale_fill_manual(values = viridis(max_storey, option = "cividis")) +
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