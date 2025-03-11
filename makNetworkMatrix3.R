#' Create a composite network matrix visualization
#'
#' This function creates a sophisticated visualization combining two correlation matrices into a
#' single composite plot. It places the upper triangle of matrix1 and the lower triangle of matrix2
#' in a combined visualization with color-coded network labels.
#'
#' @param matrix1 First correlation matrix to display (upper triangle)
#' @param matrix2 Second correlation matrix to display (lower triangle)
#' @param min1 Minimum value for color scale (applies to both matrices)
#' @param max1 Maximum value for color scale (applies to both matrices)
#' @param min2 Minimum value for second color scale (not currently used)
#' @param max2 Maximum value for second color scale (not currently used)
#' @param pal1 Color palette for first matrix (default: white to red gradient)
#' @param pal2 Color palette for second matrix (default: white to blue gradient)
#' @param title Title for the plot (not currently used)
#' @param Lgtitle Legend title
#' @param tmpfold Temporary folder for intermediate files
#' @param outfile Output file path for the final composite image
#' @return None (saves the composite image to outfile)
makeNetworkMatrix3 <- function(matrix1,
                               matrix2,
                               min1=NULL,
                               max1=NULL,
                               min2=NULL,
                               max2=NULL,
                               pal1= c("white","lightyellow", "yellow", "orange", "red"),
                               pal2= c("white", "#BFBFDF", "#7F7FBF", "#3F3F9F", "#00008B"),
                               title="",
                               Lgtitle="",
                               tmpfold="G:/SF/PHD_1/NeuroT/RES/Plot/tmp_heatmap/",
                               outfile="G:/SF/PHD_1/NeuroT//RES/Plot/tmp_heatmap/CB.png"){ 
  library(reshape2)
  library(scales)
  library(gtable)
  library(patchwork)
  library(ggplot2)
  library(magick)
  # Get lower triangle of the correlation matrix
  get_upper_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  # Get upper triangle of the correlation matrix
  get_lower_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  
  ####upper
  upper_tri <- get_upper_tri(matrix1)
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  melted_cormat[melted_cormat==0] <- NA 
  
  ####Plot
  # Calculate positions and colors for x and y axis labels
  n_labels_x <- length(unique(melted_cormat$Var2))
  n_labels_y <- length(unique(melted_cormat$Var1))
  label_colors <- c("red", "blue", "green", "purple", "yellow", 
                    "orange", "pink", "cyan", "gray", "darkred")
  
  # Create dataframe for x-axis color blocks (positioned at the top)
  color_blocks_x <- data.frame(
    xmin = seq(1, n_labels_x) - 0.45,
    xmax = seq(1, n_labels_x) + 0.45,
    ymin = rep(n_labels_y + 0.6, n_labels_x),  # Move to top
    ymax = rep(n_labels_y + 0.8, n_labels_x),  # Move to top
    fill = label_colors[1:n_labels_x]
  )
  
  # Create dataframe for y-axis color blocks (positioned on the left)
  color_blocks_y <- data.frame(
    xmin = rep(0.2, n_labels_y),    # Move to left
    xmax = rep(0.4, n_labels_y),    # Move to left
    ymin = seq(1, n_labels_y) - 0.45,
    ymax = seq(1, n_labels_y) + 0.45,
    fill = label_colors[1:n_labels_y]
  )
  
  # Create y-axis labels
  labels <- c( "Visual","Som/Mot","DorsAttn",
  "Salience","Control",
  "Limbic","Default",
  "Med-Temp","Thalamus",
  "Striatum")
  
  # Create the plot
  ggheatmap1 <- 
  ggplot(data = melted_cormat, aes(x=Var2, y=Var1, fill=value)) + 
  geom_tile(color = "black") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=5),
        legend.title = element_text(size=6),
        plot.margin = margin(50, 50, 50, 50, "pt"),  # Increase bottom margin
        panel.grid = element_blank(),
        # Adjust legend position and direction
        legend.direction = "horizontal",
        legend.box = "horizontal",
        # Adjust legend title position
        legend.title.align = 0.5,legend.position ="bottom") +
  coord_fixed(clip = "off") +
  scale_y_discrete(position = "right") + 
  scale_fill_gradientn(colours=pal1,
                       limits=c(min1, max1), 
                       na.value = "#eeeeee") +
  # Modify legend title position and style
  labs(fill = Lgtitle) + 
  # Modify legend style to horizontal
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 10,    # Adjust width of color bar
    barheight = 0.4,  # Adjust height of color bar
    nbin = 100,
    frame.colour = "black",
    ticks = TRUE
  )) +
  # Add x-axis color blocks
  geom_rect(data = color_blocks_x,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = color_blocks_x$fill,
            color = "black",
            inherit.aes = FALSE) +
  # Add y-axis color blocks
  geom_rect(data = color_blocks_y,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = color_blocks_y$fill,
            color = "black",
            inherit.aes = FALSE) +
  # Add y-axis text labels
  annotate("text", 
           x = -0.5, 
           y = 1:n_labels_y,
           label = labels,
           hjust = 1,
           size = 3
  )

###lower
lower_tri <- get_lower_tri(matrix2)
melted_cormat <- melt(lower_tri, na.rm = TRUE)
melted_cormat[melted_cormat==0] <- NA


# Create dataframe for x-axis color blocks
color_blocks_x <- data.frame(
  xmin = seq(1, n_labels_x) - 0.45,
  xmax = seq(1, n_labels_x) + 0.45,
  ymin = rep(0.2, n_labels_x),
  ymax = rep(0.4, n_labels_x),
  fill = label_colors[1:n_labels_x]
)

# Create dataframe for y-axis color blocks
color_blocks_y <- data.frame(
  xmin = rep(n_labels_x + 0.6, n_labels_y),
  xmax = rep(n_labels_x + 0.8, n_labels_y),
  ymin = seq(1, n_labels_y) - 0.45,
  ymax = seq(1, n_labels_y) + 0.45,
  fill = label_colors[1:n_labels_y]
)

# Create y-axis labels
labels <- c( "Visual","Som/Mot","DorsAttn",
             "Salience","Control",
             "Limbic","Default",
             "Med-Temp","Thalamus",
             "Striatum")

# Create plot
ggheatmap2 <- 
  ggplot(data = melted_cormat, aes(x=Var2, y=Var1, fill=value)) + 
  geom_tile(color = "black") +
  theme_minimal() +
  theme(
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=5),
        legend.title = element_text(size=6),
        plot.margin = margin(50, 50, 50, 50, "pt"),  # Increase bottom margin
        panel.grid = element_blank(),
        # Adjust legend position and direction
        legend.direction = "horizontal",
        legend.box = "horizontal",
        # Adjust legend title position
        legend.title.align = 0.5,legend.position = "bottom") +
  coord_fixed(clip = "off") +
  scale_y_discrete(position = "right") + 
  scale_fill_gradientn(colours=pal2,
                       limits=c(min1, max1), 
                       na.value = "#eeeeee") +
  # Modify legend title position and style
  labs(fill = Lgtitle)+ 
  # Modify legend style to horizontal
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 10,    # Adjust width of color bar
    barheight = 0.4,  # Adjust height of color bar
    nbin = 100,
    frame.colour = "black",
    ticks = TRUE
  )) +
  # Add x-axis color blocks
  geom_rect(data = color_blocks_x,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = color_blocks_x$fill,
            color = "black",
            inherit.aes = FALSE) +
  # Add y-axis color blocks
  geom_rect(data = color_blocks_y,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = color_blocks_y$fill,
            color = "black",
            inherit.aes = FALSE) +
  # Add y-axis text labels
  annotate("text", 
           x = n_labels_x + 1, 
           y = 1:n_labels_y,
           label = labels,
           hjust = 0,
           size = 3
  )


ggheatmap1 <- ggheatmap1 +
  theme(
    rect = element_rect(fill = "transparent") # all rectangles
  )

ggheatmap2 <- ggheatmap2 +
  theme(
    rect = element_rect(fill = "transparent") # all rectangles
  )
ggsave(plot = ggheatmap1,filename = file.path(tmpfold, "H1_tmp.png"),
       dpi = 300, width = 6,height = 4.8)
ggsave(plot = ggheatmap2,filename = file.path(tmpfold, "H2_tmp.png"),
       dpi = 300, width = 6,height = 4.8)


##Combine plots####
# Read and process images
imgs <- list(
  image_read(file.path(tmpfold, "H1_tmp.png")),
  image_read(file.path(tmpfold, "H2_tmp.png"))
) %>% lapply(function(img) {
  info <- image_info(img)
  list(
    up = image_crop(img, paste0(info$width, "x", info$height - 400, "+0+0")),
    low = image_crop(img, paste0(info$width, "x400+0+", info$height - 400))
  )
})

# Create combined image with color bars
image_blank(2000, 1300, "white") %>%
  image_composite(imgs[[1]]$up, operator = "over") %>%
  image_composite(imgs[[2]]$up, operator = "over", offset = "+250+10") %>%
  image_composite(
    image_composite(imgs[[1]]$low, 
                    image_crop(imgs[[2]]$low, paste0(image_info(imgs[[2]]$low)$width, "x130+0+90")),
                    operator = "over", offset = "+0+180"
    ),
    operator = "over", offset = "+80+1000"
  ) %>%
  image_crop(geometry_area(width = 1500, height = 1150, x_off = 250, y_off = 150)) %>%
  image_write(outfile)
}