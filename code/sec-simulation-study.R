# Project:   paper-mispcr
# Objective: Poster plots
# Author:    Edoardo Costantini
# Created:   2022-10-12
# Modified:  2022-11-14

# ---- table-missing-data ----

# Define respondents' names
ids <- c("Esther", "Anton", "Leonie", "Joran", "...", "Mihai")

# Define how many columns with data we want
target_ncol <- 4 * 3

# Create the columns with data
set.seed(20221117)
items <- as.data.frame(matrix(
  sample(1:10, length(ids) * target_ncol, replace = TRUE),
  ncol = target_ncol
))

# Add missing values
items[c(2, 3, 5), 1] <- "-"
items[c(1, 2), 2] <- "-"
items[c(3, 5), 3] <- "-"
items[c(4, 7), 4] <- "-"

# # Put observations and data together
# info <- data.frame(ids, items)

# Add a ellipsis row
items <- rbind(items[1:4, ], " ", items[5, ])

# Add two ellipsis columns
items <- cbind(items[, 1:4], " ", items[, 5:8], " ", items[, -c(1:8)])

# Give meaningful names
colnames(items) <- c(
  "$x_1$", "$x_2$", "$x_3$", "$x_4$",
  "...",
  "$w_{141}$", "$w_{142}$", "$w_{143}$", "$w_{144}$",
  "...",
  "$z_{(p-3)}$", "$z_{(p-2)}$", "$z_{(p-1)}$", "$z_p$"
)

# Give meaningful rownames
rownames(items) <- ids

# Make latex table
kbl(items, booktabs = T, "latex", escape = FALSE, align = c("r", rep("c", ncol(items)-1))) %>%
  kable_classic(full_width = F, position = "center")

# ---- plot-prb ----
# Patchwork results

# Define plot inputs
input <- NULL
input$nla <- sort(unique(results$nla)) # [2:3]
input$mech <- levels(unique(results$mech))[2]
input$pm <- unique(results$pm)[3]
input$vars <- unique(results$vars)[1]
input$stat <- unique(results$stat)[1]
input$method <- levels(results$method)[1:4]
input$npcs[2] <- 149
input$npcs[1] <- 1
input$plot_y_axis <- "PRB"
input$plot_x_axis <- "npcs_f"
input$moderator <- "method"
input$grid_x_axis <- "nla"
input$grid_y_axis <- "method"

# > L = 2 ----------------------------------------------------------------------

# Plot 1 l = 2
p1 <- results %>%
  filter(
    nla %in% input$nla[1],
    mech %in% input$mech,
    pm %in% input$pm,
    vars == input$vars,
    stat == input$stat,
    method %in% input$method
  ) %>%
  ggplot(aes_string(
    x = input$plot_x_axis,
    y = input$plot_y_axis,
    fill = "PRB_quality"
  )) +
  geom_bar(
    stat = "identity",
    colour = "black",
    size = .25
  ) +
  # Facet grid
  facet_grid(
    cols = vars(nla),
    rows = vars(method),
    scales = "free", space = "free_x",
    switch = "y"
  ) +
  scale_y_continuous(position = "right") +
  theme(
    # Text
    text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(vjust = 1),
    # Legend
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.3, "cm"),
    # Background
    panel.background = element_rect(fill = NA, color = "gray")
  )

# > L = 10 ---------------------------------------------------------------------

# Filter npcs
npcs_target <- c(1:12, 29)

# Subset data of interest
current_levels <- (unique(results %>%
  filter(
    nla %in% input$nla[2],
    method %in% input$method,
    npcs %in% npcs_target
  ) %>%
  select(npcs)))[, 1]

# Define additions
add <- c(13)

# Add empty lelvels
current_levels <- c(current_levels, add)

# Sort them
target_levels <- target_labels <- sort(current_levels)
target_labels[target_levels %in% add] <- "..."

# Plot
p2 <- results %>%
  filter(
    nla %in% input$nla[2],
    mech %in% input$mech,
    pm %in% input$pm,
    vars == input$vars,
    stat == input$stat,
    method %in% input$method,
    npcs %in% npcs_target
  ) %>%
  ggplot(aes_string(
    x = input$plot_x_axis,
    y = input$plot_y_axis,
    fill = "PRB_quality"
  )) +
  geom_bar(
    stat = "identity",
    colour = "black",
    size = .25
  ) +
  # Facet grid
  facet_grid(
    cols = vars(nla),
    rows = vars(method),
    scales = "free", space = "free_x",
    switch = "y"
  ) +
  scale_y_continuous(position = "right") +
  scale_x_discrete(
    limits = factor(target_levels),
    labels = target_labels
  ) +
  theme(
    # Text
    text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(vjust = 1),
    # Grid
    strip.text.y = element_blank(),
    # Legend
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.3, "cm"),
    # Backgorund
    panel.background = element_rect(fill = NA, color = "gray")
  )

# > L = 50 ---------------------------------------------------------------------

# Filter npcs
npcs_target <- c(1:10, 48:52, 149)

# Subset data of interest
current_levels <- (unique(results %>%
  filter(
    nla %in% input$nla[3],
    method %in% input$method,
    npcs %in% npcs_target
  ) %>%
  select(npcs)))[, 1]

# Define additions
add <- c(11, 53)

# Add empty lelvels
current_levels <- c(current_levels, add)

# Sort them
target_levels <- target_labels <- sort(current_levels)
target_labels[target_levels %in% add] <- "..."

p3 <- results %>%
  filter(
    nla %in% input$nla[3],
    mech %in% input$mech,
    pm %in% input$pm,
    vars == input$vars,
    stat == input$stat,
    method %in% input$method,
    npcs %in% npcs_target
  ) %>%
  ggplot(aes_string(
    x = input$plot_x_axis,
    y = input$plot_y_axis,
    fill = "PRB_quality"
  )) +
  geom_bar(
    stat = "identity",
    colour = "black",
    size = .25
  ) +
  # Facet grid
  facet_grid(
    cols = vars(nla),
    rows = vars(method),
    scales = "free", space = "free_x",
    switch = "y"
  ) +
  scale_y_continuous(position = "right") +
  scale_x_discrete(
    limits = factor(target_levels),
    labels = target_labels
  ) +
  theme(
    # Text
    text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(vjust = 1),
    # Grid
    strip.text.y = element_blank(),
    # Legend
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.3, "cm"),
    # Background
    panel.background = element_rect(fill = NA, color = "gray")
  )

# > Collecting plots -----------------------------------------------------------

layout <- c(
  area(t = 1, l = 1, b = 2, r = 1 + 5 - 1),
  area(t = 1, l = 1 + 5, b = 2, r = 1 + 5 + 14 - 1),
  area(t = 1, l = 1 + 5 + 14, b = 2, r = 1 + 5 + 14 + 18 - 1)
)

p1 + p2 + p3 +
  plot_layout(
    design = layout,
    guides = "collect"
  ) &
  theme(legend.position = "bottom")

# ---- plot-cic ----

# Define plot inputs
input <- NULL
input$nla <- sort(unique(results$nla)) # [2:3]
input$mech <- levels(unique(results$mech))[2]
input$pm <- unique(results$pm)[3]
input$vars <- unique(results$vars)[1]
input$stat <- unique(results$stat)[1]
input$method <- levels(results$method)[1:4]
input$npcs[2] <- 149
input$npcs[1] <- 1
input$plot_y_axis <- "CIC"
input$plot_x_axis <- "npcs_f"
input$moderator <- "method"
input$grid_x_axis <- "nla"
input$grid_y_axis <- "method"
input$ylim <- c(-.15, .05)

# First subset the data
toplot <- results %>%
  filter(
    mech %in% input$mech,
    pm %in% input$pm,
    vars == input$vars,
    stat == input$stat,
    method %in% input$method
  )

# General scaling for the many plots
plot_scale <- scale_y_continuous(
  breaks = c(seq(
    from = min(input$ylim),
    to = max(input$ylim),
    by = .1
  ), 0),
  labels = c(seq(
    from = min(input$ylim),
    to = max(input$ylim),
    by = .1
  ), 0) + .95,
  position = "right"
)

# > L = 2 ----------------------------------------------------------------------

# Subset for 2 latent variables
toplot_l2 <- toplot %>%
  filter(
    nla %in% input$nla[1]
  )

# Then identify the results that will be out of the ylimits specified
toplot_out <- toplot_l2 %>%
  filter(
    .data[[input$plot_y_axis]] < min(input$ylim)
  )

# Make plot
p1 <- toplot_l2 %>%
  # Define main plot
  ggplot(aes_string(
    x = input$plot_x_axis,
    y = input$plot_y_axis,
    fill = "CIC_quality"
  )) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    colour = "black",
    size = .15
  ) +
  # Add text for out-of-bounds results
  geom_text(
    data = toplot_out,
    label = toplot_out$coverage,
    aes_string(
      x = input$plot_x_axis,
      y = min(input$ylim)
    ),
    angle = 90,
    hjust = -.05,
    size = 8
  ) +
  # Facet grid
  facet_grid(
    cols = vars(nla),
    rows = vars(method),
    scales = "free_x", space = "free_x",
    switch = "y"
  ) +
  # Modify the y-axis
  plot_scale +
  coord_cartesian(ylim = input$ylim) +
  theme(
    # Text
    text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(vjust = 1),
    # Legend
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.3, "cm"),
    # Background
    panel.background = element_rect(fill = NA, color = "gray")
  )

# > L = 10 ---------------------------------------------------------------------

# Filter npcs
npcs_target <- c(1:12, 29)

# Subset data of interest
current_levels <- (unique(results %>%
  filter(
    nla %in% input$nla[2],
    method %in% input$method,
    npcs %in% npcs_target
  ) %>%
  select(npcs)))[, 1]

# Define additions
add <- c(13)

# Add empty levels
current_levels <- c(current_levels, add)

# Sort them
target_levels <- target_labels <- sort(current_levels)
target_labels[target_levels %in% add] <- "..."

# Subset for 10 latent variables
toplot_l10 <- toplot %>%
  filter(
    nla %in% input$nla[2],
    npcs %in% npcs_target
  )

# Then identify the results that will be out of the ylimits specified
toplot_out <- toplot_l10 %>%
  filter(
    .data[[input$plot_y_axis]] < min(input$ylim),
  )

# Make plot
p2 <- toplot_l10 %>%
  # Define main plot
  ggplot(aes_string(
    x = input$plot_x_axis,
    y = input$plot_y_axis,
    fill = "CIC_quality"
  )) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    colour = "black",
    size = .15
  ) +
  # Add text for out-of-bounds results
  geom_text(
    data = toplot_out,
    label = toplot_out$coverage,
    aes_string(
      x = input$plot_x_axis,
      y = min(input$ylim)
    ),
    angle = 90,
    hjust = -.05,
    size = 8
  ) +
  # Facet grid
  facet_grid(
    cols = vars(nla),
    rows = vars(method),
    scales = "free_x", space = "free_x",
    switch = "y"
  ) +
  # Modify the y-axis
  plot_scale +
  coord_cartesian(ylim = input$ylim) +
  scale_x_discrete(
    limits = factor(target_levels),
    labels = target_labels
  ) +
  theme(
    # Text
    text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(vjust = 1),
    # Grid
    strip.text.y = element_blank(),
    # Legend
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.3, "cm"),
    # Backgorund
    panel.background = element_rect(fill = NA, color = "gray")
  )

# > L = 50 ---------------------------------------------------------------------

# Filter npcs
npcs_target <- c(1:10, 48:52, 149)

# Subset for 10 latent variables
toplot_l50 <- toplot %>%
  filter(
    nla %in% input$nla[3],
    npcs %in% npcs_target
  )

# Then identify the results that will be out of the ylimits specified
toplot_out <- toplot_l50 %>%
  filter(
    .data[[input$plot_y_axis]] < min(input$ylim),
  )

# Define objects for X axis
current_levels <- (unique(results %>%
  filter(
    nla %in% input$nla[3],
    method %in% input$method,
    npcs %in% npcs_target
  ) %>%
  select(npcs)))[, 1]

# Define additions
add <- c(11, 53)

# Add empty levels
current_levels <- c(current_levels, add)

# Sort them
target_levels <- target_labels <- sort(current_levels)
target_labels[target_levels %in% add] <- "..."

# Make plot
p3 <- toplot_l50 %>%
  # Define main plot
  ggplot(aes_string(
    x = input$plot_x_axis,
    y = input$plot_y_axis,
    fill = "CIC_quality"
  )) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    colour = "black",
    size = .15
  ) +
  # Add text for out-of-bounds results
  geom_text(
    data = toplot_out,
    label = toplot_out$coverage,
    aes_string(
      x = input$plot_x_axis,
      y = min(input$ylim)
    ),
    angle = 90,
    hjust = -.05,
    size = 8
  ) +
  # Facet grid
  facet_grid(
    cols = vars(nla),
    rows = vars(method),
    scales = "free_x", space = "free_x",
    switch = "y"
  ) +
  # Modify the y-axis
  plot_scale +
  coord_cartesian(ylim = input$ylim) +
  scale_x_discrete(
    limits = factor(target_levels),
    labels = target_labels
  ) +
  theme(
    # Text
    text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(vjust = 1),
    # Grid
    strip.text.y = element_blank(),
    # Legend
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.3, "cm"),
    # Backgorund
    panel.background = element_rect(fill = NA, color = "gray")
  )

# > Collecting plots -----------------------------------------------------------

layout <- c(
  area(t = 1, l = 1, b = 2, r = 1 + 5 - 1),
  area(t = 1, l = 1 + 5, b = 2, r = 1 + 5 + 14 - 1),
  area(t = 1, l = 1 + 5 + 14, b = 2, r = 1 + 5 + 14 + 18 - 1)
)

p1 + p2 + p3 +
  plot_layout(
    design = layout,
    guides = "collect"
  ) &
  theme(legend.position = "bottom")
