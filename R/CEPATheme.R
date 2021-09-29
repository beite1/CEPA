CEPATheme <- function() {
  list(

    theme(panel.background = element_blank(),
          panel.grid.major = element_line(colour = "#C9CAC8"),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text  = element_text(color = "#002776"),
          axis.title =element_text(colour="#002776"),
          legend.title = element_text(colour = "#002776"),
          legend.text = element_text(colour = "#002776")

    ),

    scale_color_manual(values = c("#002776",
                                  "#5A85D7",
                                  "#F7403A",
                                  "#FFA02F",
                                  "#F3CF45",
                                  "#BED600",
                                  "#7765A0",
                                  "#009AA6",
                                  "#65cfe9",
                                  "#c9cac8"
    )),

    scale_fill_manual(values = c("#002776",
                                 "#5A85D7",
                                 "#F7403A",
                                 "#FFA02F",
                                 "#F3CF45",
                                 "#BED600",
                                 "#7765A0",
                                 "#009AA6",
                                 "#65cfe9",
                                 "#c9cac8"
    ))

    )


}
