library(shiny)
library(dplyr)
library(ggplot2)
library(corpcor) # for pseudo-inverse

# Action button preferred to submit button
# https://shiny.rstudio.com/gallery/submitbutton-demo.html


# Define UI for application that draws a histogram
ui <- fluidPage(

# Application title
titlePanel("Minecraft Stronghold Triangulation Tool"),

# Sidebar with a slider input for number of bins 
sidebarLayout(
  sidebarPanel(
    textAreaInput('textinput', label = 'Eye of Ender Throws: X, Z, heading',
      value =
"0, 1000, -146
500, 0, 18.98
500, 500, 71.78
400, 700, 142",
      width = NULL, height = NULL, rows = 6),
    submitButton("Submit")
  ),
  
  # Show plot
  mainPanel(
    plotOutput("plot")
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot <- renderPlot({
    df <- read.table(textConnection(input$textinput), sep = ",")
    names(df) <- c('x', 'z', 'heading')
    
    df <- df %>%
      mutate(
        radians = pi * heading / 180.0,
        unit_x = -sin(radians),
        unit_z = cos(radians)
      )
    
    # Determine the 'best' point for the intersection of the lines,
    # by minimizing the perpendicular distances of the point to the lines
    #
    # From:
    # - "Least-Squares Intersection of Lines, by Johannes Traa - UIUC 2013"
    # - http://cal.cs.illinois.edu/~johannes/research/
    # - http://cal.cs.illinois.edu/~johannes/research/LS_line_intersect.pdf
    #
    # Also:
    # - https://math.stackexchange.com/questions/61719/finding-the-intersection-point-of-many-lines-in-3d-point-closest-to-all-lines
    
    k <- nrow(df) # number of lines
    dimension <- 2
    a <- df[, 1:2] %>% as.matrix() %>% t() # *columns* of origin points
    n <- df[, 5:6] %>% as.matrix() %>% t() # *columns* of the points' unit direction vectors
    R = matrix(data = 0, nrow = dimension, ncol = dimension) # initialize an empty matrix
    q = vector(mode = 'numeric', length = dimension) # initialize an empty vector
    # Generating a system of linear equations, with Rp = q, where p will be the 'best' point
    for (i in 1:k) {
      R <- R + (diag(dimension) - n[, i] %*% t(n[, i]))
      q <- q + (diag(dimension) - n[, i] %*% t(n[, i])) %*% a[, i]
    }
    # So p_hat = pseudoinverse(R) x q
    p_hat <- pseudoinverse(R) %*% q # column vector of the least squares fit best point
    
    # Turn solution into proper data frame to plot optimal point
    df_p <- t(p_hat) %>% data.frame # need to convert it to a row vector for data frame
    names(df_p) <- c('x', 'z') # and match the column names to plot
    
    
    # Generate line segment end points for plotting
    df <- df %>% # generate line segment end points that are 1.3 * the distance from the origin point to the optimal point
      mutate(
        p_x = p_hat[1],
        p_z = p_hat[2],
        distance = sqrt((p_x - x) ^2 + (p_z - z) ^2),
#        distance = 1000, # or, just set each line segment length to a constant
        x2 = x - 1.3 * distance * sin(radians),
        z2 = z + 1.3 * distance * cos(radians)
      )
    
    g <- df %>% ggplot(aes(x, z)) +
      geom_segment(aes(xend = x2, yend = z2), alpha = 0.4, arrow = arrow(length = unit(0.25, "cm"))) +
      geom_point(color = 'blue') +
      scale_y_reverse() + # because of the inverted 'z' axis of positive to North and negative to South
      geom_point(data = df_p, color = 'red') +
      geom_label(data = df_p, aes(
          label = paste0('x = ', round(df_p$x), ', z = ', round(df_p$z))
        ), size = 5, nudge_x = 0, nudge_y = 50) +
      theme_bw() +
      theme(text = element_text(size = 20))
    g
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

