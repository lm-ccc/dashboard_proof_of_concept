
# This script contains some handy things for making nice CCC styled ggplots


library(tibble)
library(ggplot2)
library(magick)


# variables ----------

# the ccc colours for sectors

ccc_colours <- tibble(
  sector = c(
    "Electricity supply",
    "Manufacturing and construction",
    "Fuel supply",
    "Residential buildings",
    "Non-residential buildings",
    "Surface transport",
    "Aviation",
    "Shipping",
    "Agriculture",
    "LULUCF",
    "Waste",
    "F-gas",
    "Industry",
    "Agriculture & LULUCF",
    "Buildings"
  ),
  colour = c(
    rgb(255, 255, 75, maxColorValue = 255),
    rgb(174, 197, 235, maxColorValue = 255),
    rgb(26, 95, 49, maxColorValue = 255),
    rgb(255, 92, 0, maxColorValue = 255),
    rgb(255, 172, 0, maxColorValue = 255),
    rgb(113, 66, 255, maxColorValue = 255),
    rgb(140, 87, 204, maxColorValue = 255),
    rgb(202, 120, 128, maxColorValue = 255),
    rgb(161, 216, 0, maxColorValue = 255),
    rgb(205, 231, 176, maxColorValue = 255),
    rgb(150, 233, 255, maxColorValue = 255),
    rgb(40, 0, 73, maxColorValue = 255),
    rgb(174, 197, 235, maxColorValue = 255),
    rgb(161, 216, 0, maxColorValue = 255),
    rgb(255, 92, 0, maxColorValue = 255)
  )
)

ccc_colour_list <- ccc_colours$colour
names(ccc_colour_list) <- ccc_colours$sector



# ccc general theme colours ---------

ccc_purple <- rgb(113, 66, 255, maxColorValue = 255)



ccc_theme <- c(
    rgb(40, 0, 73, maxColorValue = 255),
    rgb(113, 66, 255, maxColorValue = 255),
    rgb(140, 87, 204, maxColorValue = 255),
    rgb(171, 107, 153, maxColorValue = 255),
    rgb(202, 120, 128, maxColorValue = 255),
    rgb(255, 172, 0, maxColorValue = 255),
    rgb(255, 92, 0, maxColorValue = 255),
    rgb(255, 255, 75, maxColorValue = 255),
    rgb(161, 216, 0, maxColorValue = 255),
    rgb(205, 231, 176, maxColorValue = 255),
    rgb(171, 197, 235, maxColorValue = 255),
    rgb(54, 153, 147, maxColorValue = 255),
    rgb(26, 95, 49, maxColorValue = 255)
)

# font sizes -------

base_size <- 12
half_line <- base_size/2

# specific figure font sizes
title_size <- 14
subtitle_size <- 12
xlabs_size <- 10
ylabs_size <- 10
legend_size <- 11
legend_title_size <- 9
ytitle_size <- 11
xtitle_size <- 11
caption_size <- 7

base_family <- "Century Gothic"

# functions -----


# the ccc theme 

theme_custom <- function (base_size = 12, base_family = "Century Gothic") {
  

  
  theme(
    line = element_line(color = "light grey", size = .5,
                        linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", color = ccc_purple,
                        size = .5, linetype = 1),
    text = element_text(family = base_family, face = "plain",
                        color = ccc_purple, size = base_size,
                        lineheight = .9, hjust = .5, vjust = .5,
                        angle = 0, margin = margin(), debug = FALSE),
    axis.line = element_blank(),
    axis.line.x = element_line(color = ccc_purple, size = 1),
    axis.line.y = element_line(color = ccc_purple, size = 1),
    axis.text = element_text(color = ccc_purple),
    axis.text.x = element_text(size = xlabs_size, margin = margin(t = .8 * half_line/2),
                               vjust = 1),
    axis.text.x.top = element_text(size = xlabs_size, margin = margin(b = .8 * half_line/2),
                                   vjust = 0),
    axis.text.y = element_text(size = ylabs_size, margin = margin(r = .8 * half_line/2),
                               hjust = 1),
    axis.text.y.right = element_text(size = ylabs_size, margin = margin(l = .8 * half_line/2),
                                     hjust = 0),
    axis.ticks = element_line(color = ccc_purple, size = .7),
    axis.ticks.length = unit(half_line / 1.5, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title.x = element_text(margin = margin(t = half_line),
                                vjust = 1, size = xtitle_size),
    axis.title.x.top = element_text(margin = margin(b = half_line),
                                    vjust = 0, size = xtitle_size),
    axis.title.y = element_text(angle = 90, vjust = 1,
                                margin = margin(r = half_line),
                                size = ytitle_size),
    axis.title.y.right = element_text(angle = -90, vjust = 0,
                                      margin = margin(l = half_line)),
    legend.background = element_rect(color = NA),
    legend.spacing = unit(.4, "cm"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = margin(.2, .2, .2, .2, "cm"),
    legend.key = element_rect(fill = "gray95", color = "white"),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = legend_size),
    legend.text.align = NULL,
    legend.title = element_blank(),
    #legend.title = element_text(size = legend_title_size, hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(.4, "cm"),
    panel.background = element_blank(),
    panel.border=element_blank(),
    #panel.border = element_rect(color = ccc_purple,fill = NA, size = 1),
    panel.grid.major.y = element_line(color = "light grey", size = .5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_line(color = "light grey", size = .5),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(base_size, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = element_rect(fill = "white", color = "gray30"),
    strip.text = element_text(color = "black", size = base_size),
    strip.text.x = element_text(margin = margin(t = half_line,
                                                b = half_line)),
    strip.text.y = element_text(angle = -90,
                                margin = margin(l = half_line,
                                                r = half_line)),
    strip.text.y.left = element_text(angle = 90),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),
    plot.background = element_rect(color = NA),
    plot.title = element_text(size = title_size, hjust = .5,
                              vjust = 1, colour = "black",
                              margin = margin(b = half_line * 1.2)),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = subtitle_size,
                                 hjust = 0, vjust = 1,
                                 margin = margin(b = half_line * .9)),
    plot.caption = element_text(size = caption_size, hjust = 1, vjust = 1,
                                margin = margin(t = half_line * .9)),
    plot.caption.position = "panel",
    plot.tag = element_text(size = rel(1.2), hjust = .5, vjust = .5),
    plot.tag.position = "topleft",
    plot.margin = margin(base_size, base_size, base_size, base_size),
    complete = TRUE
  )
}


# Add logo function

# adds a logo to a pre saved ggplot, by pasting on top of the png file

add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.02 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.02 * plot_width
    y_pos = plot_height - logo_height - 0.02 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.02 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}



# save with logo

# function builds on 'add logo' to save a plot with the logo in one step

save_with_logo <- function(path, # where to save plot
                           plot, # ggplot object
                           width = 10, height = 8, units = "in", dpi = 600,
                           logo_path = "logo/CCC_symbol_sml_DP+W_RGB.png", # url or local file for the logo
                           logo_position = "top right", # choose a corner
                           logo_scale = 10) {
  
  # requires the {magick} package
  
  ggsave(path, plot, width = width, height = height, units = units, dpi = dpi)
  
  plot_with_logo <- add_logo(
    plot_path = path, # url or local file for the plot
    logo_path = logo_path, # url or local file for the logo
    logo_position = logo_position, # choose a corner
    logo_scale = logo_scale
  )
  
  # save the image and write to working directory
  magick::image_write(plot_with_logo, path)
  
}




# flextable defaults

ccc_table <- function(df, pretty_cell_sizes = FALSE) {
  
  # for converting cm to inches
  cm_to_in <- function(x){(x*0.393701)}
  
  # custom colours
  ccc_purple
  dark_purple  <- "#280049"
  light_purple <- "#E2D9FF"

  # set defaults
  set_flextable_defaults(
    font.family = "Century Gothic", 
    font.size = 9,
    font.color = "black",
    table.layout = "fixed"
  )
  
  # make a flextable
  table <- flextable(df)
  
  #add theme
  table <- theme_zebra(
    table,
    odd_header = ccc_purple,
    odd_body = "#E2D9FF",
    even_header = "transparent",
    even_body = "transparent"
  )
  
  # set colors
  table <- color(table, part = "header", color = "white")
  table <- bold(table, part = "header", bold = TRUE)
  
  # draw borders
  brdr <- fp_border(color = dark_purple, width = 1)
  table <- border_outer(table, part="all", border = brdr )
  table <- border_inner(table, part="all", border = brdr )
  
  #fix widths
  table <- width(table, width = cm_to_in(18.8)/ncol(df))
  
  if (pretty_cell_sizes == TRUE) {
    
    width_list <- cm_to_in(18.8)*dim_pretty(table)$widths/sum(dim_pretty(table)$widths)
    table <- width(table, width = width_list)
    #table <- height(table, height = dim_pretty(table)$heights[-1])
  }

  table
}




