# Design a Bchron hex sticker

rm(list = ls())
library(Bchron)
library(hexSticker)
library(ggplot2)
library(showtext)
library(ggimage)

# data(Glendalough)
# data(Sluggan)
# 
# # Run in Bchronology - all but first age uses intcal13
# SlugOut = BchronCalibrate(ages=Sluggan$ages,
#                       ageSds=Sluggan$ageSds,
#                       calCurves=Sluggan$calCurves,
#                       positions=Sluggan$position,
#                       ids=Sluggan$id)
#plot(SlugOut, withPositions = TRUE)
#p = plot(SlugOut, dateLabels = FALSE, withPositions = TRUE) + theme_void() + theme_transparent()

ages1 = BchronCalibrate(ages=11553,
                        ageSds=230,
                        calCurves='intcal13',
                        ids='Ox-123456')
p = plot(ages1, dateLabels = FALSE, fillCol = "#F0AB00") + 
  geom_line(data = as.data.frame(ages1$`Ox-123456`), aes(x = ageGrid, y = densities), col = "#822327") +
  ggtitle('') + theme_void() + theme_transparent()


## Loading Google fonts (http://www.google.com/fonts)
fname = "Alegreya Sans" #sample(font_families_google(), 1)
font_add_google(fname)
## Automatically use showtext to render text for future devices
showtext_auto()

sticker(p, package="Bchron", p_size=10, s_x=1, s_y=0.8, s_width=1.3, s_height=1,p_family = fname,
        h_fill="#006778", h_color="#822327",
        filename="badge/Bchron_badge.png")
