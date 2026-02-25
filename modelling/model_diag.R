library(spdep)

func_mult_diag <- function(model, longs, lats, k) {

  par(mfrow = c(2, 2))

  plot(resid(model) ~ fitted(model))
  abline(h = 0, col = "red")

  hist(resid(model))

  coords <- cbind(longs, lats)
  coords <- as.matrix(coords, longlat = TRUE)

  k1 <- knn2nb(knearneigh(coords, 
    k = k, longlat = TRUE)) #do the same of poly2nb

  is.symmetric.nb(k1)

  # plot(k1, coords, col = 'green', lwd = 2)

  max_dist <- max(unlist(nbdists(k1, coords)))

  spat_weight <- dnearneigh(coords, 0, max_dist, longlat = FALSE)

  plot(spat_weight, coords,
    col = "green", lwd = 2)

  spat_weight_w <-
    nb2listw(spat_weight, glist = NULL, style = "W", zero.policy = FALSE)

  # Morans I
  print(moran.test(residuals(model), listw = spat_weight_w,
                     randomisation = FALSE)$estimate[["Moran I statistic"]])

  print(moran.test(residuals(model), listw = spat_weight_w,
                      randomisation = TRUE)$estimate[["Moran I statistic"]] )

  print(moran.test(residuals(model), listw = spat_weight_w,
                      randomisation = FALSE,
                      alternative = "two.sided")$
                        estimate[["Moran I statistic"]])

  print(moran.mc(residuals(model),
    spat_weight_w, 100)$estimate[["Moran I statistic"]])

}
