setwd("~/")
require(dplyr)   # for data manipulation
require(tidyr)   # for reshaping the data frame
require(stringr) # string manipulation
require(ggplot2) # graphing
require(readr)


# file.path <- "Downloads/Cuestionario de análisis del tratamiento de los verbos de cambio en materiales ELE (respuestas) - Respuestas.csv"

#not run if you don't define file.path
cuestionario <- read_csv(file.path)

cuestionario <- cuestionario[cuestionario$eliminar == 0, ]
cuestionario$Code <- cuestionario$Manual
cuestionario$Manual <- paste0(cuestionario$Manual, " (R", cuestionario$Respuesta, ")")
cuestionario <- cuestionario[order(cuestionario$Diferencia, decreasing = FALSE), ]
cuestionario = cuestionario[order(cuestionario$Manual, decreasing = FALSE), ]

# create the data frame 
# (in wide format, as needed for the line segments):
dat_wide = tibble::as_tibble(cuestionario)

# a version reshaped to long format (for the points):
dat_long = dat_wide %>% 
  gather(key = 'Evaluador', value = 'Coeficiente', Análisis:Encuesta)

dat_long <- dat_long[order(dat_long$Diferencia, decreasing = FALSE), ]
dat_long <- dat_long[, c("Coeficiente", "Manual", "Evaluador")]
dat_wide <- dat_wide[order(dat_wide$index, decreasing = TRUE), ]

big = dat_wide
big <- data.frame(big, stringsAsFactors = FALSE)
big <- big[!names(big) %in% c("Respuesta", "ExisteCambio", "eliminar", "index", "Code")]
colnames(big) <- c("Material", "C1_A", "C1_T", "C2_A", "C2_T", "C3_A", "C3_T", "C4_A", "C4_T",
                   "C5_A", "C5_T", "C6_A", "C6_T", "C7_A", "C7_T", "C8_A", "C8_T", "C9_A", "C9_T",
                   "C10_A", "C10_T", "Análisis", "Encuesta", "Diferencia")

library(gt)
options(digits=2)

big$C2_T <- as.character(ifelse(big$C2_T == 1, "1.0", 
                                ifelse(big$C2_T == 2, "2.0",
                                       ifelse(big$C2_T == 3, "3.0",
                                              "4.0"))))

big$C3_T <- as.character(ifelse(big$C3_T == 1, "1.0", 
                                ifelse(big$C3_T == 2, "2.0",
                                       ifelse(big$C3_T == 3, "3.0",
                                              "4.0"))))

big$C4_T <- as.character(ifelse(big$C4_T == 1, "1.0", 
                                ifelse(big$C4_T == 2, "2.0",
                                       ifelse(big$C4_T == 3, "3.0",
                                              "4.0"))))

big$C5_T <- as.character(ifelse(big$C5_T == 1, "1.0", 
                                ifelse(big$C5_T == 2, "2.0",
                                       ifelse(big$C5_T == 3, "3.0",
                                              "4.0"))))

big$C7_T <- as.character(ifelse(big$C7_T == 1, "1.0", 
                                ifelse(big$C7_T == 2, "2.0",
                                       ifelse(big$C7_T == 3, "3.0",
                                              "4.0"))))

big$C8_T <- as.character(ifelse(is.na(big$C8_T), "", 
                                ifelse(big$C8_T == 1, "1.0", 
                                       ifelse(big$C8_T == 2, "2.0",
                                              ifelse(big$C8_T == 3, "3.0",
                                                     "4.0")))))

big$C3_A <- as.character(ifelse(big$C3_A == 1, "1.0", 
                                ifelse(big$C3_A == 2, "2.0",
                                       ifelse(big$C3_A == 3, "3.0",
                                              "4.0"))))

big$C4_A <- as.character(ifelse(big$C4_A == 1, "1.0", 
                                ifelse(big$C4_A == 2, "2.0",
                                       ifelse(big$C4_A == 3, "3.0",
                                              "4.0"))))

big$C5_A <- as.character(ifelse(big$C5_A == 1, "1.0", 
                                ifelse(big$C5_A == 2, "2.0",
                                       ifelse(big$C5_A == 3, "3.0",
                                              "4.0"))))

big$C6_A <- as.character(ifelse(big$C6_A == 1, "1.0", 
                                ifelse(big$C6_A == 2, "2.0",
                                       ifelse(big$C6_A == 3, "3.0",
                                              "4.0"))))

big$C7_A <- as.character(ifelse(big$C7_A == 1, "1.0", 
                                ifelse(big$C7_A == 2, "2.0",
                                       ifelse(big$C7_A == 3, "3.0",
                                              "4.0"))))

big$C8_A <- as.character(ifelse(is.na(big$C8_A), "", 
                                ifelse(big$C8_A == 1, "1.0", 
                                       ifelse(big$C8_A == 2, "2.0",
                                              ifelse(big$C8_A == 3, "3.0",
                                                     "4.0")))))

big$C9_A <- as.character(ifelse(big$C9_A == 1, "1.0", 
                                ifelse(big$C9_A == 2, "2.0",
                                       ifelse(big$C9_A == 3, "3.0",
                                              "4.0"))))


big1 = big[1:21, ]
big2 = big[22:nrow(big), ]

big1 <- big1 %>% gt() %>%
  opt_table_font(
    font = c(
      "Times New Roman",
      default_fonts()[-c(1:3)]
    )
  )

big1 %>%
  gtsave(
    "FG-04A.html"
  )


# create the graph:
ggplot() +
  geom_segment(data = dat_wide, 
               aes(x    = Encuesta, 
                   xend = Análisis, 
                   y    = reorder(Manual, index), 
                   yend = reorder(Manual, index)),
               size = 3, colour = '#D0D0D0') +
  geom_point(data = dat_long,
             aes(x      = Coeficiente, 
                 y      = Manual, 
                 xend = 40,
                 colour = Evaluador),
             size = 2) +
  annotate(
    "rect", xmin = 0, xmax = 10, ymin = 0, ymax = 35.5,
    fill = "dark grey", alpha = .1
  ) +
  annotate(
    "rect", xmin = 10, xmax = 20, ymin = 0, ymax = 35.5,
    fill = "#CC6666", alpha = .1
  ) +
  annotate(
    "rect", xmin = 20, xmax = 30, ymin = 0, ymax = 35.5,
    fill = "#66CC99", alpha = .1
  ) +
  annotate(
    "rect", xmin = 30, xmax = 40, ymin = 0, ymax = 35.5,
    fill = "#009E73", alpha = .1
  ) +
  labs(title = '',
       subtitle = '',
       caption = '',
       x = NULL, y = NULL) +
  scale_colour_manual(values = c('#1082CD', '#042B41')) +
  theme_bw(base_family = 'Times New Roman') +
  theme(legend.position = c(0.92, 0.942),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = 'black'),
        panel.border = element_blank(),
        axis.ticks = element_line(colour = '#E6E6E6'))

ggsave('FG-01.png', width = 20, height = 40, units = 'cm')


library(formattable)
library("htmltools")
library("webshot")    

cuestionario <- cuestionario[order(cuestionario$index, decreasing = TRUE), ]
cuestionario$colorAnalisis <- ifelse(cuestionario$Análisis <= 10, "dark grey",
                                     ifelse(cuestionario$Análisis <= 20, '#F5B7B1',
                                            ifelse(cuestionario$Análisis <= 30, '#ABEBC6',
                                                   "#009E73")))

cuestionario$colorEncuesta <- ifelse(cuestionario$Encuesta <= 10, "dark grey",
                                     ifelse(cuestionario$Encuesta <= 20, '#F5B7B1',
                                            ifelse(cuestionario$Encuesta <= 30, '#ABEBC6',
                                                   "#009E73")))

FT = formattable(cuestionario[, c("Manual", "Respuesta", "Análisis", "Encuesta",
                                  "Diferencia")], 
                 align = c("l",rep("r", NCOL(cuestionario) - 5)),
                 list(`Manual` = formatter("span", 
                                           style = ~ style(color = "black", 
                                                           #font.weight = "bold",
                                                           'font-family:Times New Roman')), 
                      `Respuesta` = formatter("span", 
                                              style = ~ style(color = "black", 
                                                              'font-family:Times New Roman')), 
                      `Análisis` = color_bar(cuestionario$colorAnalisis), 
                      `Encuesta` = color_bar(cuestionario$colorEncuesta), 
                      `Diferencia` = formatter("span", 
                                               ExisteCambio ~ icontext(ifelse(cuestionario$ExisteCambio == 0, "ok", "remove"), 
                                                                       cuestionario$Diferencia), 
                                               style = ExisteCambio ~ style(color = ifelse(cuestionario$ExisteCambio == 0, 
                                                                                           "green", "red"),
                                                                            'font-family:Times New Roman'),
                                               font.weight = "bold")))

export_formattable(FT,"FG-02.png")


export_formattable <- function(f, file, width = "50%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

