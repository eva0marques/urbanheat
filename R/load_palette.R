load_palette <- function(name) {
  stopifnot(
    "name should be one of \"temp\", \"temp_ipcc\", \"sw\", \"res\", \"reds\", \"prior\", \"model\", or \"uhi\"" =
      name %in% c("temp", "temp_ipcc", "sw", "res", "reds", "prior", "model", "uhi")
  )
  if (name == "temp") {
    return(fields::tim.colors(n = 64, alpha = 1.0))
  } else if (name == "temp_ipcc") {
    pal_ipcc <- list(
      c(103, 0, 31),
      c(178, 24, 43),
      c(214, 96, 77),
      c(244, 165, 130),
      c(253, 219, 199),
      c(247, 247, 247),
      c(209, 229, 240),
      c(146, 197, 222),
      c(67, 147, 195),
      c(33, 102, 172),
      c(5, 48, 97)
    ) |>
      lapply(function(x) rgb(x[1], x[2], x[3], maxColorValue = 255)) |>
      rev()
    return(pal_ipcc)
  } else if (name == "sw") {
    return(RColorBrewer::brewer.pal(10, "RdYlBu"))
  } else if (name == "res") {
    return(c("dodgerblue4", "white", "firebrick4"))
  } else if (name == "reds") {
    return(c("white", "firebrick4"))
  } else if (name == "prior") {
    return(RColorBrewer::brewer.pal(10, "RdBu"))
  } else if (name == "uhi") {
    return(c("cadetblue3", "cornsilk", "yellow", "orange", "red", "firebrick"))
  } else if (name == "model") {
    #return(c("car" = "#BF47FF", "cws" = "#4787FF", "joint" = "#FF6347"))
    return(c("car" = "red", "cws" = "orange", "joint" = "#4787FF"))
  }
}
