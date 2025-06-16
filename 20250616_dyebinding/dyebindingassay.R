# pkgs
library(stats)

# x and y axes
protein_concentration_ul <- c(0,10,20,30,40,60)
absorbance <- c(0, 0.099, 0.263, 0.407, 0.504, 0.684)

# plot
plot(protein_concentration_ul, absorbance, col = "blue", pch = 19,
     xlab = "Protein Mass (µg)", ylab = "Absorbance",
     main = "Protein mass vs absorbance")

# linear regression

assay_lm <- lm(absorbance ~ protein_concentration_ul)
abline(assay_lm)
coef(assay_lm) # y = 0.0118*x + 0.0122

# tests
summary(assay_lm) #R^2 = 0.9821

# interpolation - function
interpolate_protein_concentration <- function(absorbance_value) {
  # Calculate the protein concentration for a given absorbance value
  assay_intercept <- coef(assay_lm)[1]
  assay_slope <- coef(assay_lm)[2]
  
  # Solve for x in y = mx + b
  protein_concentration <- (absorbance_value - assay_intercept) / assay_slope
  return(protein_concentration)
}

# values

interpolate_protein_concentration(0.309) #mass = 25.21 micrograms
interpolate_protein_concentration(0.440) #mass = 36.33 micrograms
interpolate_protein_concentration(0.433) # mass = 35.74 micrograms
