# Load necessary library
library(tidyverse)
library(irr)

# Assuming your data is in a CSV file named 'data.csv'
data <- read.csv("screeningResults.csv")

# Calculate agreements between Human #1 and GPT
agreement <- table(data$HUMAN.1, data$GPT)

# Calculate how often Human #2 agrees with Human #1 and with GPT
tiebreaker_agreement_human1 <- sum(data$HUMAN.1 == data$HUMAN.2, na.rm = TRUE)
tiebreaker_agreement_gpt <- sum(data$GPT == data$HUMAN.2, na.rm = TRUE)

# Summary of final decisions
final_decisions <- table(data$FINAL)

# Calculate the Cohen's Kappa for agreement between Human #1 and GPT
kappa_agreement <- kappa2(data.frame(data$HUMAN.1, data$GPT))$value

# Print the results
print(agreement)
print(paste("Tiebreaker agreement with Human #1:", tiebreaker_agreement_human1))
print(paste("Tiebreaker agreement with GPT:", tiebreaker_agreement_gpt))
print(final_decisions)
print(paste("Cohen's Kappa for agreement between Human #1 and GPT:", kappa_agreement))

# Export the summary to a CSV file
summary_data <- data.frame(agreement, tiebreaker_agreement_human1, tiebreaker_agreement_gpt, final_decisions)
write.csv(summary_data, "summary_data.csv")
