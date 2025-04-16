
p <- ggplot(data = by_country,
            mapping = 
              aes(x = roads_mean, 
                  y = donors_mean))
p + geom_point() + 
  geom_text(mapping = 
              aes(label = country))



install.packages("ggrepel")
library(ggrepel)

socviz::elections_historic |> select(2:7) 

p <- ggplot(elections_historic, aes(x = popular_pct, y = ec_pct,
                                    label = winner_label))

p <- p + 
  geom_hline(yintercept = 0.5, 
             linewidth = 1.4, 
             color = "gray80") +
  geom_vline(xintercept = 0.5, 
             linewidth = 1.4, 
             color = "gray80") +
  geom_point() +
  geom_text_repel()
p


p_title <- "Presidential Elections: Popular & Electoral College Margins"
p_subtitle <- "1824-2016"
p_caption <- "Data for 2016 are provisional."
x_label <- "Winner's share of Popular Vote"
y_label <- "Winner's share of Electoral College Votes"

p + labs(x = x_label, 
         y = y_label, 
         title = p_title, 
         subtitle = p_subtitle,
         caption = p_caption)

p <- ggplot(data = by_country,
            mapping = 
              aes(x = gdp_mean, 
                  y = health_mean))

p + geom_point() +
  geom_text_repel(
    data = filter(by_country, 
                  gdp_mean > 25000),
    mapping = 
      aes(label = country))

p <- ggplot(data = by_country,
            mapping = 
              aes(x = gdp_mean, 
                  y = health_mean))
p + geom_point() +
  geom_text_repel(
    data = 
      filter(by_country,
             gdp_mean > 25000 | 
               health_mean < 1500 |
               country %in% "Belgium"),
    mapping = 
      aes(label = country))


# creating a dummy variable for labels
organdata <- organdata |> 
  mutate(ind = ccode %in% 
           c("Ita", "Spa") & 
           year > 1998)  

p <- ggplot(data = organdata,
            mapping = 
              aes(x = roads, 
                  y = donors, 
                  color = ind))
p + 
  geom_point() +
  geom_text_repel(
    data = filter(organdata, ind),
    mapping = aes(label = ccode)) +
  guides(label = "none", 
         color = "none")



mtcars <- datasets::mtcars
mtcars <- mtcars %>%   # A native pipe (|>) does not work here.
  mutate(car = rownames(.))
rownames(mtcars) <- 1:nrow(mtcars)

DT::datatable(mtcars)

# Load required libraries
library(ggplot2)
library(dplyr)
library(ggrepel)

# Load dataset and modify it
mtcars <- datasets::mtcars %>%
  mutate(car = rownames(.))  # Add car names as a column
rownames(mtcars) <- 1:nrow(mtcars)

# Create the scatter plot
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "red", size = 2) +  # Red points
  geom_text_repel(aes(label = ifelse(wt > 5, car, "")),  # Label heavy cars
                  size = 4, hjust = 0, nudge_x = 0.2) +
  labs(x = "wt", y = "mpg") +  # Axis labels
  theme_minimal()  # Minimal theme


p <- ggplot(data = organdata, 
            mapping = 
              aes(x = roads, 
                  y = donors))
p + geom_point() + 
  annotate(geom = "text", 
           x = 91, y = 33,
           label = "A surprisingly high \n recovery rate.",
           hjust = 0)

p <- ggplot(data = organdata,
            mapping = aes(x = roads, y = donors))
p + geom_point() +
  annotate(geom = "rect", 
           xmin = 125, xmax = 155,
           ymin = 30, ymax = 35, 
           fill = "red", 
           alpha = 0.2) + 
  annotate(geom = "text", 
           x = 157, y = 33,
           label = "A surprisingly high \n recovery rate.", 
           hjust = 0)

