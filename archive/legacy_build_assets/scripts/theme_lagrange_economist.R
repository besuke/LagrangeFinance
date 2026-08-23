economist_palette <- c(
  "#E3120B",
  "#006BA2",
  "#3EBCD2",
  "#379A8B",
  "#EBB434",
  "#9A607F",
  "#758D99",
  "#B4BA39"
)

theme_lagrange_economist <- function(
    base_size = 13,
    base_family = "Noto Sans JP"
) {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = "#DCE6E9",
        colour = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = "#DCE6E9",
        colour = NA
      ),
      legend.background = ggplot2::element_rect(
        fill = "#DCE6E9",
        colour = NA
      ),
      legend.key = ggplot2::element_rect(
        fill = "#DCE6E9",
        colour = NA
      ),
      strip.background = ggplot2::element_rect(
        fill = "#DCE6E9",
        colour = NA
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        colour = "white",
        linewidth = 0.55
      ),
      axis.line.x = ggplot2::element_line(
        colour = "#555555",
        linewidth = 0.4
      ),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(
        face = "bold",
        colour = "#222222"
      ),
      axis.text = ggplot2::element_text(
        colour = "#333333"
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = ggplot2::element_text(
        face = "bold",
        size = ggplot2::rel(1.35),
        hjust = 0,
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(0.95),
        colour = "#444444",
        hjust = 0,
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.8),
        colour = "#555555",
        hjust = 0,
        margin = ggplot2::margin(t = 8)
      ),
      legend.position = "bottom",
      legend.justification = "left",
      legend.title = ggplot2::element_text(
        face = "bold"
      ),
      strip.text = ggplot2::element_text(
        face = "bold",
        hjust = 0
      )
    )
}

options(
  ggplot2.discrete.colour = economist_palette,
  ggplot2.discrete.fill = economist_palette
)

ggplot2::theme_set(
  theme_lagrange_economist()
)

ggplot2::update_geom_defaults(
  "line",
  list(
    colour = "#006BA2",
    linewidth = 0.9
  )
)

ggplot2::update_geom_defaults(
  "point",
  list(
    colour = "#006BA2",
    size = 2.2
  )
)

ggplot2::update_geom_defaults(
  "bar",
  list(
    fill = "#006BA2",
    colour = NA
  )
)

ggplot2::update_geom_defaults(
  "col",
  list(
    fill = "#006BA2",
    colour = NA
  )
)

ggplot2::update_geom_defaults(
  "text",
  list(
    family = "Noto Sans JP"
  )
)

ggplot2::update_geom_defaults(
  "label",
  list(
    family = "Noto Sans JP"
  )
)

if (requireNamespace("ragg", quietly = TRUE)) {
  knitr::opts_chunk$set(
    dev = "ragg_png",
    dpi = 180,
    fig.width = 8,
    fig.height = 4.8,
    fig.align = "center"
  )
} else {
  knitr::opts_chunk$set(
    dpi = 180,
    fig.width = 8,
    fig.height = 4.8,
    fig.align = "center"
  )
}

