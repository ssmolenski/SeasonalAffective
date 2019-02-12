g +
    theme_light(base_family="serif") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_point(aes(Date,High, color = "HighTemp"), 
                shape = 20, 
                alpha = 1/2
                ) +
    geom_smooth(mapping=aes(Date, High, color = "HighTemp"),
                se    = FALSE
                ) +
    geom_point(aes(Date, (Level*8+60), color = "Mood"), 
                shape = 20, 
                alpha = 1/2
                ) +
    geom_smooth(mapping=aes(Date, (Level*8+60), color = "Mood"),
                se    = FALSE
                ) +
    ylab("Temperature (F)") +
    ggtitle("Temperature and Mood") +
    scale_colour_manual( name   = "",
                         values = c(HighTemp = "indianred", 
                                    Mood="grey47"
                                    )
                        )