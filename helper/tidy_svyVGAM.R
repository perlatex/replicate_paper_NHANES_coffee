tidy_svyVGAM <- function(
    x, 
    conf.int = FALSE, 
    conf.level = 0.95,
    exponentiate = FALSE, 
    ...
){
  # Replace `summary(x)$coefficients` with `summary(x)$coeftable`
  ret <- as_tibble(summary(x)$coeftable, rownames = "term")
  
  # All of this stays the same:
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
  coefs <- tibble::enframe(stats::coef(x), name = "term", value = "estimate")
  ret <- left_join(coefs, ret, by = c("term", "estimate"))
  if (conf.int) {
    ci <- broom:::broom_confint_terms(x, level = conf.level, ...)
    ret <- dplyr::left_join(ret, ci, by = "term")
  }
  if (exponentiate) {ret <- broom:::exponentiate(ret)}
  
  # This part only works for the multinomial case, and only if your covariates
  # have no ":" in their names - NOT FOR GENERAL USE
  ret %>% 
    separate(term, into = c("term", "y.level"), sep = ":") %>% 
    arrange(y.level) %>% 
    relocate(y.level, .before = term)
}