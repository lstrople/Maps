

# Define the grid layout
grid_layout <- rbind(cote_plot, paul_plot, casc_plot, thib_plot)

# Set the dimensions for each plot
widths <- c(2, 3, 2, 3)  # Adjust these numbers as needed
heights <- c(3, 2, 3, 2)  # Adjust these numbers as needed

# Create the grid using grid.arrange
grid_figure <- grid.arrange(
  grobs = grid_layout,
  layout_matrix = rbind(widths, heights),
  nrow = 2, ncol = 2  # Adjust the number of rows and columns as needed
)

grid_figure

# Save or display the grid figure
ggsave("grid_figure.png", grid_figure, width = 10, height = 8)  # Adjust the dimensions as needed
