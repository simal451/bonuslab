library(dplyr)
library(plotly)
library(seriation)
library(nycflights13)
#' Visualizes the mean delay of flights for different airports
#'
#' @return link map and heat map to visual airplanes traffic delays
#' @import nycflights13 dplyr plotly seriation
#' @export visualize_airport_delays
#'
#' @examples visualize_airport_delays()
visualize_airport_delays <- function() {
  flights <- data.frame(nycflights13::flights)
  airports <- data.frame(nycflights13::airports)
  airports$to <- airports$faa

  filghts_dept_from <-
    flights[, c(13, 14, 6)] %>% dplyr::group_by(origin, dest) %>% dplyr::summarise(avg = mean(dep_delay, na.rm = T))
  colnames(filghts_dept_from) <- c("faa", "faa2", "arr_delay")
  filghts_dept_from <-
    dplyr::inner_join(filghts_dept_from, airports, by = "faa")
  filghts_dept_from <- filghts_dept_from[, c(1:6)]
  colnames(filghts_dept_from) <-
    c("from", "to", "arr_delay", "name", "start_lat", "start_lon")
  filghts_dept_from <-
    dplyr::inner_join(filghts_dept_from, airports, by = "to")
  filghts_dept_from <- filghts_dept_from[, c(1:6, 8:10)]
  colnames(filghts_dept_from) <-
    c(
      "from",
      "to",
      "arr_delay",
      "namefrom",
      "start_lat",
      "start_lon",
      "nameto",
      "end_lat",
      "end_lon"
    )

  # map projection
  geo <- list(scope = 'north america')

  p <- plot_geo(locationmode = 'USA-states', color = I("red")) %>%
    add_markers(
      data = airports,
      x = ~ lon,
      y = ~ lat,
      text = ~ name,
      size = I(5),
      hoverinfo = "text",
      alpha = 0.5
    ) %>%
    add_segments(
      data = filghts_dept_from,
      x = ~ start_lon,
      xend = ~ end_lon,
      y = ~ start_lat,
      yend = ~ end_lat,
      alpha = 0.3,
      size = ~ arr_delay ,
      hoverinfo = "text"
    ) %>%
    layout(title = "Link map for figths coonnection between airports",
           geo = geo)

  filghts_dept_from <- filghts_dept_from[, 1:3]
  mat <- tidyr::spread(filghts_dept_from, from, arr_delay)
  rownames(mat) <- mat$to
  mat_ <- mat[2:4]
  rownames(mat_) <- mat$to
  mat_[is.na(mat_)] <- 0
  mat_ <- as.matrix(mat_)

  mat_scale <- scale(mat_)
  rownames(mat_scale) <- mat$to
  mtscaled = mat_scale
  rowdist <- dist(mtscaled)
  coldist <- dist(t(mtscaled))

  order1 <- seriate(rowdist, "TSP")
  order2 <- seriate(coldist, "TSP")
  ord1 <- get_order(order1)
  ord2 <- get_order(order2)

  reordmatr <- mtscaled[rev(ord1), ord2]

  dims = list()
  for (i in 1:ncol(reordmatr)) {
    dims[[i]] = list(label = colnames(reordmatr)[i],
                     values = as.formula(paste("~", colnames(reordmatr)[i])))
  }


  fig1 <- plot_ly(
    x = colnames(reordmatr),
    y = rownames(reordmatr),
    z = reordmatr,
    type = "heatmap",
    colors = colorRamp(c("yellow", "red"))
  ) %>% layout(title = "Heatmap using for Dept delay")
  fig1

  subplot(p, fig1)
}
