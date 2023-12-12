# Create a data frame with the hormonal concentrations
dogs <- data.frame(
  Hormone = rep(c("None","Oestrogen","Progesterone"), each=4),
  Concentration = c(117, 124, 40, 88, 440, 264, 221, 136, 605, 626, 385, 475)
)

# Calculate the mean and standard deviation by treatment
treatment_summary <- aggregate(Concentration ~ Hormone, data = dogs, 
                               FUN = function(x) c(Mean = mean(x), SD = sd(x)))

# Print the summary table
treatment_summary

# Conduct ANOVA, using aov(), and print the results
anova_results <- aov(Concentration ~ Hormone, data = dogs)
summary(anova_results)

# The most important value in the entire output is the p-value.
# Since the p-value in the ANOVA table is 0.0006, thus less than 0.05, we may reject the null hypothesis.
# This means that the mean hormonal concentrations is not equal between the three treatments.

# Now that we know that the mean values between treatments is not equal, we can perform a post hoc test,
# to check which treatment differ from the others.

TukeyHSD(anova_results)

# As we can observe in the results, the adjusted p-value for the mean difference between using 
# oestrogen or not, is greater 0.05, so is not a significance difference in their hormonal concentrations.

# For the other two cases, the adjusted p-value is less than 0.05, thus there is a significant difference
# in the hormonal concentrations when using progesterone.

