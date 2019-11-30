df = data.frame(A=sample(1:75, 50, replace=TRUE),
                B=sample(1:100, 50, replace=TRUE))
library(ggplot2)
library(tidyverse)
library(gganimate)
library(directlabels)
library(png)
library(transformr)
library(grid)
p=ggplot(df, aes(A, B)) +
    geom_line() +
    transition_reveal(A) +
    labs(title = 'A: {frame_along}')
animate(p, renderer = gifski_renderer(loop = FALSE),duration = 14, width = 800, height = 400)
#
p = ggplot(df, aes(A, B)) +
    geom_line() +
    transition_reveal(A) +
    labs(title = 'A: {frame_along}')

animate(p, nframes=40, fps = 2)

##################
library(gganimate)
p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point()

plot(p)
#
anim <- p + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)

anim
############
devtools::install_github('thomasp85/gganimate')
library(gapminder)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')
#################
pdf("trans1.pdf", width = 6, height = 6)
library(ggplot2)
p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point()
plot(p)
#
library(gganimate)
anim = p + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)
anim
#
anim + 
  ggtitle('Now showing {closest_state}',
          subtitle = 'Frame {frame} of {nframes}')
#
anim + 
  ggtitle('Now showing {closest_state}',
          subtitle = 'Frame {frame} of {nframes}')
#
animate(
  anim ,
  renderer = av_renderer()
)
#
animate(
  anim ,
  width = 400, height = 600, res = 35
)
#
