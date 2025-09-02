# Load packages for the analysis
library(psych)
library(ltm)

# Set seed for reproducibility
set.seed(2025)

# Define simulation parameters
n_respondents <- 251
n_items <- 8


### -------------------------------------------------------------------------
### Step 3: CTT Reliability Analysis (Alpha and Omega)
### -------------------------------------------------------------------------

# --- Cronbach's Alpha ---
cat("--- Calculating Cronbach's Alpha ---\n")
cat("NOTE: This is a common reliability measure for Likert scales, but it assumes all items are equally related to the construct, which is often not true.\n\n")

alpha_results <- alpha(sim_data)
print(alpha_results)

cat("\n--- Key takeaway from alpha() ---\n")
cat(paste("Cronbach's Alpha is:", round(alpha_results$total$raw_alpha, 2), "\n\n"))


# --- McDonald's Omega ---
cat("--- Calculating McDonald's Omega ---\n")
cat("NOTE: For Likert data, omega() often uses a polychoric correlation matrix, making it a more accurate estimate of reliability than alpha.\n\n")

omega_results <- omega(sim_data, check.keys = TRUE)
print(omega_results)

cat("\n--- Key takeaway from omega() ---\n")
cat(paste("McDonald's Omega Total is:", round(omega_results$omega.tot, 2), "\n"))
cat("This value is often considered a more accurate estimate of reliability for Likert scales.\n\n")


### -------------------------------------------------------------------------
### Step 4: Validity & IRT Analysis
### -------------------------------------------------------------------------

# --- Factor Analysis for Internal Structure Validity ---
cat("--- Factor Analysis on Polychoric Correlations ---\n")
cat("NOTE: For ordinal (Likert) data, a polychoric correlation matrix is essential. It estimates the correlation between the underlying continuous traits.\n\n")

poly_cor <- polychoric(sim_data)
fa_model <- fa(poly_cor$rho, nfactors = 1, n.obs = n_respondents)
print(fa_model)
cat("Look for high factor loadings (e.g., > 0.5) on the single factor (FA1). Note that Q8, our simulated weak item, has the lowest loading.\n\n")


# --- Item Response Theory (IRT) - Graded Response Model (GRM) ---
cat("\n--- Fitting a Graded Response Model (GRM) ---\n")
cat("NOTE: The GRM is the proper IRT model for ordinal/Likert data. It estimates one discrimination parameter (like the 2PL) and multiple difficulty-like 'threshold' parameters for each item.\n\n")

# The grm() function fits the Graded Response Model
irt_model_grm <- grm(sim_data)
print(summary(irt_model_grm))
cat("\nInterpretation:\n")
cat("- 'Dscrmn' is the discrimination (slope) parameter. Higher is better. Note Q8's low value.\n")
cat("- 'Location' parameters are the thresholds (b), the points of 50% probability of endorsing a category or higher.\n\n")


# --- Visualizations ---
cat("\n--- Generating IRT Plots ---\n")
# Plot 1: Item Characteristic Category Curves (ICCCs) for a specific item (e.g., Q1)
# This shows the probability of selecting each specific response category at different
# levels of the latent trait (Financial Vulnerability).
plot(irt_model_grm, type = "ICC", items = 1, main = "ICCC for Item Q1")

# Plot 2: Test Information Function (TIF)
# Shows where the overall scale is most precise.
plot(irt_model_grm, type = "IIC", main = "Test Information Function (GRM)")

cat("IRT plots have been generated in the 'Plots' pane.\n")



