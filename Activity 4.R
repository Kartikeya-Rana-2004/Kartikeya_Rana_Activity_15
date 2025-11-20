# Activity 4 box

box_volume <- function(x, length, width) {
  if (x <= 0 || x >= min(length/2, width/2)) {
    return(0)
  }
  new_length <- length - 2*x
  new_width <- width - 2*x
  height <- x
  volume <- new_length * new_width * height
  return(volume)
}
