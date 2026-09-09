## Allocate internal response names without changing existing predictor names.
.varpro.fresh.names <- function(proposed, existing = character()) {
  existing <- unique(existing)
  resolved <- make.unique(c(existing, proposed))
  resolved[length(existing) + seq_along(proposed)]
}
