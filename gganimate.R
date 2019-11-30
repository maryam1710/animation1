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
###############
library(animation)
ani.options(interval=.05)
col.range <- heat.colors(15)
saveGIF({
  layout(matrix(c(1, rep(2, 5)), 6, 1))
   par(mar=c(4,4,2,1) + 0.1)
     for (i in 1:150) {
      chunk <- rnorm(100)+sqrt(abs((i)-51))
      par(fg=1)
      plot(-5, xlim = c(1,150), ylim = c(0, .3), axes = F, xlab = "m", ylab = "n", main = "Iteration")
      abline(v=i, lwd=5, col = rgb(0, 0, 255, 255, maxColorValue=255))
      abline(v=i-1, lwd=5, col = rgb(0, 0, 255, 50, maxColorValue=255))
      abline(v=i-2, lwd=5, col = rgb(0, 0, 255, 25, maxColorValue=255))
      axis(1)
      par(fg = col.range[mean(chunk)+3])
plot(density(chunk), main = "", xlab = "X Value", xlim = c(-5, 15), ylim = c(0, .6))
     abline(v=mean(chunk), col = rgb(255, 0, 0, 255, maxColorValue=255))
      if (exists("lastmean")) {abline(v=lastmean, col = rgb(255, 0, 0, 50, maxColorValue=255)); prevlastmean <- lastmean;}
      if (exists("prevlastmean")) {abline(v=prevlastmean, col = rgb(255, 0, 0, 25, maxColorValue=255))}
      lastmean <- mean(chunk)
    }
})
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
library(demography)
nyears <- length(fr.mort$year)
for(i in 1:nyears)
{
    pdf(paste("frmale",i,".pdf",sep=""),height=4,width=6.5)
    x <- fr.mort
    if(i<nyears)
        x$rate$male[,(i+1):nyears] <- NA
    plot(x,series="male",ylim=c(-9.5,1.5),
        main=paste("French: male mortality (",fr.mort$year[1]-1+i,")",sep=""))
    if(i>1)
        x$rate$male[,1:(i-1)] <- NA
    lines(x,series='male',lwd=2,col=1)
    dev.off()
}
#################
colours <- c(
  'lawngreen',
  'gold',
  'white',
  'orchid',
  'royalblue',
  'yellow',
  'orange'
)
blast <- function(n, radius, x0, y0, time) {
  u <- runif(n, -1, 1)
  rho <- runif(n, 0, 2*pi)
  x <- radius * sqrt(1 - u^2) * cos(rho) + x0
  y <- radius * sqrt(1 - u^2) * sin(rho) + y0
  id <- sample(.Machine$integer.max, n + 1)
  data.frame(
    x = c(x0, rep(x0, n), x0, x),
    y = c(0, rep(y0, n), y0, y),
    id = rep(id, 2),
    time = c((time - y0) * runif(1), rep(time, n), time, time + radius + rnorm(n)),
    colour = c('white', rep(sample(colours, 1), n), 'white', rep(sample(colours, 1), n)),
    stringsAsFactors = FALSE
  )
}
n <- round(rnorm(20, 30, 4))
radius <- round(n + sqrt(n))
x0 <- runif(20, -30, 30)
y0 <- runif(20, 40, 80)
time <- runif(20, max = 100)
fireworks <- Map(blast, n = n, radius = radius, x0 = x0, y0 = y0, time = time)
fireworks <- dplyr::bind_rows(fireworks)
ggplot(fireworks) + 
  geom_path(aes(x = x, y = y, group = id, colour = colour)) + 
  scale_colour_identity()
ggplot(fireworks) + 
  geom_point(aes(x, y, colour = colour, group = id), size = 0.5, shape = 20) + 
  scale_colour_identity() + 
  coord_fixed(xlim = c(-65, 65), expand = FALSE, clip = 'off') +
  theme_void() + 
  theme(plot.background = element_rect(fill = 'black', colour = NA), 
        panel.border = element_blank()) + 
  transition_components(time, exit_length = 20) + 
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_wake(0.05, size = 3, alpha = TRUE, wrap = FALSE, 
              falloff = 'sine-in', exclude_phase = 'enter') + 
  exit_recolour(colour = 'black')
################
library(animation)
## brownian motion: note the 'loop' option in ani.opts and the careful settings in documentclass

saveLatex({
  par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0),
      tcl = -0.3, cex.axis = 0.8, cex.lab = 0.8, cex.main = 1)
  brownian.motion(pch = 21, cex = 5, col = 'red', bg = 'yellow',
                  main = 'Demonstration of Brownian Motion')
}, img.name = 'BM', ani.opts = 'controls,loop,width=0.95\\textwidth',
          latex.filename = 'brownian_motion.tex'),
          interval = 0.1, nmax = 10,
          ani.dev = 'pdf', ani.type = 'pdf', ani.width = 7, ani.height = 7,
          documentclass = paste('\\documentclass{article}',
                                '\\usepackage[papersize={7in,7in},margin=0.3in]{geometry}', sep = '\n'))
####################
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
# Video output
animate(
  anim ,
  renderer = av_renderer()
)
#
# Different size and resolution
animate(
  anim ,
  width = 400, height = 600, res = 35
)
#
anim_save("This PC/Local Disk(E:)/IUST-Thesis", anim)