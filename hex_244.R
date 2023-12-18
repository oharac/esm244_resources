library(tidyverse)
library(hexSticker)
library(palmerpenguins)
library(ggfortify)

library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
# font_add_google(name = "Josefin Sans", family = "js")
font_add_google(name = "Russo One", family = "r1")
font_add_google(name = "Montserrat", family = "mm")
## Automatically use showtext to render text for future devices
showtext_auto()

p <- penguins %>%
  drop_na()
peng_pca <- p %>%
  select(ends_with(c('_mm', '_g'))) %>%
  scale() %>%
  prcomp()

peng_df <- cbind(peng_pca$x[ , 1:2],  as.character(p$species)) %>%
  as.data.frame() %>%
  mutate(PC1 = as.numeric(PC1), PC2 = as.numeric(PC2), 
         species = factor(V3))

pc_df <- peng_pca$rotation %>%
  as.data.frame()
  

p <- ggplot(data = peng_df, aes(x = -PC2, y = PC1, colour = V3)) +
  # geom_point(size = 1, aes(shape = V3), alpha = .7, show.legend = FALSE) +
  # stat_ellipse(geom = "polygon", aes(fill = V3),
  #              linewidth = .25, alpha = .2, show.legend = FALSE) +
  geom_segment(data = pc_df, x = 0, y = 0,
               arrow = arrow(angle = 20, length = unit(.25, 'cm'),
                             type = 'closed'),
               color = 'cyan4',
               aes(xend = -3*PC2, yend = 3*PC1)) +
  scale_color_manual(values = c('purple4', 'cyan4', 'yellow2')) +
  scale_fill_manual(values = c('purple4', 'cyan4', 'yellow2')) +
  theme_void()


s <- sticker(subplot = p,
             s_x = 1, s_y = .65, s_width = 1, s_height = .6,
             package="244", p_size = 54, p_x = 1, p_y = 1.2, 
             p_family = 'mm', p_fontface = 'bold',
             h_fill = '#220022', h_color = 'cyan4',
             spotlight = TRUE, l_width = 4, l_x = 1, l_y = .6,
             filename="figures/hex_244.png")
