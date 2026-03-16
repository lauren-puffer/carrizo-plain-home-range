library(ggplot2)

timeline <- data.frame(
  event = c(
    "SB 722",
    "Tule Elk Reintroduction 1",
    "Pronghorn Reintroduction 1",
    "Pronghorn Reintroduction 2",
    "Carrizo Plain National Monument",
    "iNaturalist Launch",
    "Tule Elk Reintroduction 2",
    "Recent Sightings Data"
  ),
  year = c(1971, 1988, 1990, 1998, 2001, 2012, 2014, 2025)
)

timeline$label <- paste0(timeline$year, "; ", timeline$event)
timeline$y <- c(0.18, -0.18, 0.26, -0.26, 0.18, -0.18, 0.26, -0.26)

# create color groups
timeline$group <- ifelse(grepl("Tule Elk", timeline$event), "Tule Elk",
                         ifelse(grepl("Pronghorn", timeline$event), "Pronghorn",
                                timeline$event))

ticks <- data.frame(year = seq(1970, 2025, 5))

p <- ggplot() +
  
  # timeline
  geom_segment(aes(x = 1970, xend = 2025, y = 0, yend = 0), linewidth = 1.2) +
  
  # tick marks
  geom_segment(
    data = ticks,
    aes(x = year, xend = year, y = -0.03, yend = 0.03),
    color = "grey40",
    linewidth = 0.6
  ) +
  
  # year labels
  geom_text(
    data = ticks,
    aes(x = year, y = -0.08, label = year),
    size = 4,
    color = "black"
  ) +
  
  # event connectors
  geom_segment(
    data = timeline,
    aes(x = year, xend = year, y = 0, yend = y),
    linewidth = 0.6,
    color = "grey20"
  ) +
  
  # event points
  geom_point(
    data = timeline,
    aes(x = year, y = y, color = group),
    size = 3
  ) +
  
  # event labels
  geom_text(
    data = timeline,
    aes(
      x = year,
      y = y + ifelse(y > 0, 0.03, -0.03),
      label = label,
      color = group
    ),
    size = 3.8
  ) +
  
  scale_color_manual(values = c(
    "Tule Elk" = "#1b9e77",
    "Pronghorn" = "#d95f02",
    "SB 722" = "#7570b3",
    "Carrizo Plain National Monument" = "#66a61e",
    "iNaturalist Launch" = "#e7298a",
    "Recent Sightings Data" = "#e6ab02"
  )) +
  
  scale_x_continuous(
    limits = c(1970, 2025),
    breaks = NULL
  ) +
  
  scale_y_continuous(limits = c(-0.35, 0.35)) +
  
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

dir.create("data", showWarnings = FALSE)

ggsave(
  "data/carrizo_timeline.png",
  plot = p,
  width = 12,
  height = 4,
  dpi = 300,
  bg = "white"
)