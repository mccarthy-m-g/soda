repeat_parameter <- function(x, p) {
  as.list(x) %>%
    rlang::set_names(rep(p, length(.)))
}
