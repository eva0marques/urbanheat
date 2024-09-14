load_palette <- function(name) {
  stopifnot(
    "name should be one of \"lcz\", \"temp\", \"temp_ipcc\", \"sw\", \"res\", \"reds\", \"prior\", \"model\", or \"uhi\"" =
      name %in% c("lcz", "temp", "temp_ipcc", "sw", "res", "reds", "prior", "model", "uhi")
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
  } else if (name == "lcz") {
    lcz <- rbind(
      c("1", "#8c0000", "1-compact high-rise"),
      c("2", "#d10000", "2-compact mid-rise"),
      c("3", "#ff0000", "3-compact low-rise"),
      c("4", "#bf4d00", "4-open high-rise"),
      c("5", "#ff6600", "5-open mid-rise"),
      c("6", "#ff9955", "6-open low-rise"),
      c("7", "#faee05", "7-lightweight low-rise"),
      c("8", "#bcbcbc", "8-large low-rise"),
      c("9", "#ffccaa", "9-sparsely built"),
      c("A", "#0a6f0a", "A-dense trees"),
      c("B", "#00a900", "B-scattered trees"),
      c("C", "#608323", "C-bush, scrub"),
      c("D", "#b9db79", "D-low plants"),
      c("E", "#000000", "E-bare rock or paved"),
      c("G", "#6a6afe", "G-water")) |>
      as.data.frame()
    colnames(lcz) <- c("class", "col", "meaning")
    return(lcz)
  }
}
