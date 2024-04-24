raincloud_numfeat = function(dt, i_col){
  data_x = as.data.frame(dt)
  ggplot(data_x, aes(x = rep(names(data_x)[i_col], nrow(data_x)), y = data_x[, i_col])) +
    xlab('') + ylab('Data distribution') +
    ggdist::stat_halfeye(adjust = 0.5, width = 0.6, .width = 0, justification = -0.2, point_colour = NA, fill = 'darkblue', alpha = 0.7) +
    geom_boxplot(width = 0.15, outlier.shape = NA, fill = 'lightgreen', alpha = 0.5) +
    gghalves::geom_half_point(side = 'l', range_scale = 0.4, alpha = 0.3) +
    coord_cartesian(xlim = c(1.2, NA), clip = 'off') +
    theme_minimal() +
    theme(legend.position = 'none') +
    theme(axis.text.x = element_text(angle = 90))
}
