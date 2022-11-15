# Project:   paper-template
# Objective: List of plotting functions used for this paper
# Author:    Edoardo Costantini
# Created:   2022-10-24
# Modified:  2022-10-24
# Notes: 

# Plot density of a variable ---------------------------------------------------

plotPDF <- function(x){
    plot(density(x))
}

# Bias -------------------------------------------------------------------------

plotPRB <- function(dat,
                    par_est,
                    sel_meths,
                    plot_x_axis,
                    plot_y_axis,
                    moderator,
                    grid_x_axis,
                    grid_y_axis,
                    x_axis_name,
                    y_axis_name,
                    scales = "free",
                    filters) {

  # Subset data
  dat_sub <- dat %>%
    filter(par == par_est) %>%
    filter(method %in% sel_meths)

  # Apply extra filters
  for (f in seq_along(filters)) {
    filter_factor <- names(filters)[f]
    filter_lvels <- filters[[f]]
    dat_sub <- dat_sub %>%
      filter(!!as.symbol(filter_factor) %in% filter_lvels)
  }

  # Make factors
  dat_sub[, plot_x_axis] <- factor(dat_sub[, plot_x_axis])
  dat_sub[, moderator] <- factor(dat_sub[, moderator])

  # Make a different factor for color coding
  dat_sub$mod <- dat_sub[, moderator]
  levels(dat_sub$mod) <- list(
    "0" = c("0"),
    "1 to 6" = as.character(1:6),
    "7 to 10" = as.character(7:10)
  )

  # Replace "Inf" with "infinity" for plotting symbol instead of word
  levels(dat_sub[, plot_x_axis])[levels(dat_sub[, plot_x_axis]) == "Inf"] <- "infinity"
  levels(dat_sub[, moderator])[levels(dat_sub[, moderator]) == "Inf"] <- "infinity"

  # Main Plot
  plot_main <- dat_sub %>%
    ggplot(aes_string(
      x = plot_x_axis,
      y = plot_y_axis,
      group = moderator,
      fill = "mod"
    )) +
    geom_bar(
      stat = "identity",
      position = "dodge", 
      colour = "black",
      size = .25
    ) +
    scale_y_continuous(breaks = c(0, 5, 10, 15)) +
    scale_x_discrete(labels = parse(text = levels(dat_sub[, plot_x_axis]))) +
    scale_fill_manual(moderator, values = c("white", gray.colors(2, start = 0.8, end = 0.5))) +
    geom_hline(aes(yintercept = 10),
      size = .25
    )

  # Grid
  plot_grid <- plot_main +
    facet_grid(reformulate(grid_x_axis, grid_y_axis),
      labeller = labeller(.rows = label_both, .cols = label_value),
      scales = "free"
    )

  # Format
  plot_themed <- plot_grid +
    theme(
      # Text
      text = element_text(size = 12),
      strip.text.y.right = element_text(angle = 0),
      # Legend
      legend.position = "right",
      # Backgorund
      panel.background = element_rect(fill = NA, color = "gray")
    ) +
    labs(
      x = x_axis_name,
      y = y_axis_name
    )

  # Return final plot
  plot_themed

}
