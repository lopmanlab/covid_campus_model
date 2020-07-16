#--------------------------------------------------------------------------
# Setting up ggplot Themes 
#--------------------------------------------------------------------------
library(ggplot2)
manuscript_theme <- theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
                         plot.subtitle = element_text(size = 16, face = "bold"),
                         axis.title.x = element_text(size = 16, face = "bold"),
                         axis.title.y = element_text(size = 16, face = "bold"),
                         axis.text = element_text(size = 14),
                         panel.background = element_rect(fill = "white", color = "black"),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.position = "right",
                         legend.key.width = unit(1, "cm"),
                         legend.title = element_text(size = 12),
                         legend.text = element_text(size = 12)
                        )

sm_theme1 <- theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
                 plot.subtitle = element_text(size = 12, face = "bold"),
                 axis.title.x = element_text(size = 14, face = "bold"),
                 axis.title.y = element_text(size = 14, face = "bold"),
                 axis.text = element_text(size = 12),
                 panel.background = element_rect(fill = "white", color = "black"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())

sm_theme2 <- theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
                  plot.subtitle = element_text(size = 10, face = "bold"),
                  axis.title.x = element_text(size = 12, face = "bold"),
                  axis.title.y = element_text(size = 12, face = "bold"),
                  axis.text = element_text(size = 10),
                  panel.background = element_rect(fill = "white", color = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())