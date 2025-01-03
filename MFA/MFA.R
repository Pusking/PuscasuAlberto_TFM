# Load necessary libraries
install.packages(c("FactoMineR", "factoextra"))
library(FactoMineR)
library(factoextra)
library(readxl)
library(dplyr)


# Load the data
statistics_df <- read_excel("statistics_df_D1_2 (1).xlsx")

# Remove columns not needed for the analysis (e.g., "ID", "Exposure_Group", "Exposure_Group_duplicate")
statistics_df_filtered <- statistics_df[, !colnames(statistics_df) %in% c("ID", "Gender", "Exposure_Group")]
metadata_vars <- c("Env_type", "Parents_liv_tog", "Parents_edu", "Same_edu")


statistics_df_filtered[metadata_vars] <- lapply(statistics_df_filtered[metadata_vars], as.factor)
numeric_cols <- statistics_df_filtered %>% select(where(is.numeric))
factor_cols <- statistics_df_filtered %>% select(where(is.factor))
statistics_df_reordered <- bind_cols(numeric_cols, factor_cols)
group1 <- c("8-oxo-dGUO", "8-OH-guanosine")  # Group 1
group2 <- c("NNAL", "OH-cotinine", "Cotinine")  # Group 2
group3 <- setdiff(colnames(statistics_df_reordered), c(group1, group2))  # Remaining metabolites

# Combine groups into a new order
new_order <- c(group1, group2, group3)

# Reorder the dataframe with metadata factors at the end
statistics_df_reordered_grouped <- statistics_df_reordered[, c(new_order, metadata_vars)]

# Check the new column order
colnames(statistics_df_reordered_grouped)

# Define groups of variables and types
group <- c(2, 3, 17, 11, 8)  # 21 metabolites 16 metadata factors
type <- c("s","s","s", "s", "n")  # continuous s (scaled) and categorical n 
name.group <- c("Oxidative-stress markers", "Tobacco-related chemicals", "Neurotransmitters", "Metadata continuous factors", "Metadata categorical factors")

# Run the MFA
res.mfa <- MFA(statistics_df_reordered_grouped,
               group = group,
               type = type,
               name.group = name.group,
               num.group.sup = NULL,
               graph = FALSE)

# Summary of MFA results
print(res.mfa)

# Visualize eigenvalues (variance explained by each dimension)
fviz_screeplot(res.mfa)

# Extract and visualize results for groups of variables
group_results <- get_mfa_var(res.mfa, "group")
fviz_mfa_var(res.mfa, "group")

# Visualize contributions of each group to the first two dimensions
fviz_contrib(res.mfa, "group", axes = 1)
fviz_contrib(res.mfa, "group", axes = 2)


# Create the plot
fviz_mfa_var(res.mfa, "quanti.var", palette = "jco",  
                         col.var.sup = "violet", repel = TRUE, 
                         geom = c("point", "text"), legend = "bottom")

# Contributions to dimension 1 and 2
fviz_contrib(res.mfa, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco")
fviz_contrib(res.mfa, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")
fviz_contrib(res.mfa, choice = "quanti.var", axes = 3, top = 20,
             palette = "jco")

#############################
#MFA ONLY WITH METABOLITES
#############################
metabolites <- read_excel("SG_standarized_metabolites_.xlsx")
# Remove columns not needed for the analysis (e.g., "ID", "Exposure_Group", "")
metabolites_df_filtered <- metabolites[, !colnames(metabolites) %in% c("Groups_encoded", 'ID', 'Exposure_Group')]
group1 <- c("8-oxo-dGUO", "8-OH-guanosine")  
group2 <- c("NNAL", "OH-cotinine", "Cotinine")  
group3 <- setdiff(colnames(metabolites_df_filtered), c(group1, group2)) 
new_order <- c(group1, group2, group3)
metabolites_df_filtered <- metabolites_df_filtered[, new_order]

# Define groups of variables and types
group <- c(2, 3, 17)  # 21 metabolites 16 metadata factors
type <- c("s", "s", "s")  # continuous c and categorical n 
name.group <- c("Oxidative-stress markers", "Tobacco-related chemicals", "Neurotransmitters")

# Run the MFA
res.mfa <- MFA(metabolites_df_filtered,
               group = group,
               type = type,
               name.group = name.group,
               num.group.sup = NULL,
               graph = FALSE)

# Summary of MFA results
print(res.mfa)

# Visualize eigenvalues (variance explained by each dimension)
fviz_screeplot(res.mfa)

# Extract and visualize results for groups of variables
group_results <- get_mfa_var(res.mfa, "group")
fviz_mfa_var(res.mfa, "group")

# Visualize contributions of each group to the first two dimensions
fviz_contrib(res.mfa, "group", axes = 1)
fviz_contrib(res.mfa, "group", axes = 2)


# Create the plot
fviz_mfa_var(res.mfa, "quanti.var", palette = "jco",  
             col.var.sup = "violet", repel = TRUE, 
             geom = c("point", "text"), legend = "bottom")

# Contributions to dimension 1 and 2
fviz_contrib(res.mfa, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco")
fviz_contrib(res.mfa, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")
fviz_contrib(res.mfa, choice = "quanti.var", axes = 3, top = 20,
             palette = "jco")
