overlap <- function(total, a, b) {
  duplicate = sample.int(total, a + b - total)
  aa = a - length(duplicate)
  bb = b - length(duplicate)

  population = setdiff(1:total, duplicate)
  aa = sample(population, aa)
  bb = setdiff(population, aa)

  aa = sort(c(aa, duplicate))
  bb = sort(c(bb, duplicate))
  list(first = aa, second = bb)
}
