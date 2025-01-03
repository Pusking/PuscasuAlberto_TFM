install.packages("mgm")
install.packages("qgraph")

# Load packages
library(mgm)
library(readxl)
library(qgraph)

conc <- read_excel("SG_standarized_metabolites_.xlsx") 
statistics_df <- read_excel("statistics_df_D1_2_.xlsx")
metadata <- read_excel("metadata_statistics_D1_1_.xlsx")

# Assume that all variables are numeric in statistics_df
# Remove the excluded columns
statistics_df_filtered <- statistics_df[, !colnames(statistics_df) %in% c("ID", "Exposure_Group", "Exposure_Group_duplicate")]
conc <- conc[, !colnames(conc) %in% c("ID", "Exposure_Group", "Groups_encoded")]
metadata <- metadata[, !colnames(metadata) %in% c("ID", "Exposure_Group")]

# Specify the types of each variable
types <- sapply(colnames(statistics_df_filtered), function(var) {
  if (var %in% c("Gender", "Env_type", "Parents_liv_tog", "Same_edu")) {
    "c"
  } else {
    "g"
  }
})

# Specify the levels for each variable
levels <- sapply(colnames(statistics_df_filtered), function(var) {
  if (var == "Gender") {
    2
  } else if (var %in% c("Env_type", "Same_edu")) {
    3
  } else if (var %in% c("Parents_liv_tog")) {
    2
  } else {
    1  # for continuous variables
  }
})

# Ensure the data is a matrix
data_matrix <- as.matrix(statistics_df_filtered)

# Run the MGM model
fit_mgm <- mgm(data = data_matrix, 
               type = types, 
               level = levels, 
               k = 2, 
               lambdaSel = "CV", 
               lambdaFolds = 10, 
               ruleReg = "AND")

# Load qgraph package
library(qgraph)

# Extract the weighted adjacency matrix and edge color from fit_mgm
wadj_matrix <- fit_mgm$pairwise$wadj
edge_colors <- fit_mgm$pairwise$edgecolor

# Define node names (e.g., column names from your original dataset)
node_names <- colnames(statistics_df_filtered)

# Plot with qgraph
groups <- list(
  "Oxidative stress markers" = c("8-oxo-dGUO", "8-OH-guanosine"),
  "Neurotransmitters" = c("Adenosine", "Anthranilic", "Dopamine", "Epinephrine", 
                          "GABA", "Glutamate", "Glutamine", "Histamine", 
                          "Kynurenic acid", "Kynurenine", "Melatonin", 
                          "Norepinephrine", "Quinolinic acid", 
                          "Serotonin", "Tryptophan", "Xanthurenic acid", "QA/KA"),
  "Tobacco-related chemicals" = c("NNAL", "Cotinine", "OH-cotinine"),
  "Metadata factors" = c("Gender", "Weight_kg", "Height_cm", "Smokers_in_home", 
                         "Cigarettes_in_home", "Cigarettes_outside_home", 
                         "Fruit_veg_days", "Fruit_veg_per_day", "High_fat_diet", 
                         "Exercise_days", "Env_type", "Parents_liv_tog", 
                         "Parents_edu", "Income", "BMI", "Same_edu")
)

# Assign colors to each group
group_colors <- c("skyblue", "orange", "#FFB3BA", "#FFFFBA")  # Colors for the groups

# Create a color vector for the nodes
node_colors <- rep(NA, length(node_names))  # Initialize a vector for node colors

# Assign colors based on group membership
for (i in seq_along(groups)) {
  node_colors[node_names %in% groups[[i]]] <- group_colors[i]
}

# Create a vector for the groups
node_groups <- sapply(node_names, function(name) {
  for (i in seq_along(groups)) {
    if (name %in% groups[[i]]) {
      return(names(groups)[i])  # Return the name of the group
    }
  }
  return(NA)  # In case the node does not belong to any group
})

# Run qgraph
qgraph(wadj_matrix,
       layout = "spring",
       edge.color = edge_colors,
       nodeNames = node_names,
       color = node_colors,  # Use the defined node colors
       groups = node_groups,  # Use the groups assigned to nodes
       legend = TRUE,
       legend.mode = "style2",
       legend.cex = 0.25,  # Smaller legend size
       vsize = 3.5,         # Adjust node size as needed
       esize = 15)  

# Metabolites only MGM
conc_filtered <- conc[, !colnames(conc) %in% c("ID", "Groups_encoded")]

# Specify the types of each variable (all metabolites are continuous)
types <- rep("g", ncol(conc_filtered))  # All continuous variables

# Specify the levels for each variable
levels <- rep(1, ncol(conc_filtered))  # All are continuous variables

# Ensure the data is a matrix
data_matrix_conc <- as.matrix(conc_filtered)

# Run the MGM model on the metabolites dataset
fit_mgm_conc <- mgm(data = data_matrix_conc, 
                    type = types, 
                    level = levels, 
                    k = 2, 
                    lambdaSel = "CV", 
                    lambdaFolds = 10, 
                    ruleReg = "AND")

# Extract the weighted adjacency matrix and edge color from fit_mgm_conc
wadj_matrix_conc <- fit_mgm_conc$pairwise$wadj
edge_colors_conc <- fit_mgm_conc$pairwise$edgecolor

# Define node names (e.g., column names from your original metabolites dataset)
node_names_conc <- colnames(conc_filtered)

# Create the groups for metabolites (excluding metadata factors)
groups_conc <- list(
  "Oxidative stress markers" = c("8-oxo-dGUO", "8-OH-guanosine"),
  "Neurotransmitters" = c("Adenosine", "Anthranilic", "Dopamine", "Epinephrine", 
                          "GABA", "Glutamate", "Glutamine", "Histamine", 
                          "Kynurenic acid", "Kynurenine", "Melatonin", 
                          "Norepinephrine", "Quinolinic acid", 
                          "Serotonin", "Tryptophan", "Xanthurenic acid", "QA/KA"),
  "Tobacco-related chemicals" = c("NNAL", "Cotinine", "OH-cotinine")
  # Excluded metadata factors from this list
)

# Assign colors to each group
group_colors_conc <- c("skyblue", "orange", "#FFB3BA")  # Colors for the groups

# Create a color vector for the nodes
node_colors_conc <- rep(NA, length(node_names_conc))  # Initialize a vector for node colors

# Assign colors based on group membership
for (i in seq_along(groups_conc)) {
  node_colors_conc[node_names_conc %in% groups_conc[[i]]] <- group_colors_conc[i]
}

# Create a vector for the groups
node_groups_conc <- sapply(node_names_conc, function(name) {
  for (i in seq_along(groups_conc)) {
    if (name %in% groups_conc[[i]]) {
      return(names(groups_conc)[i])  # Return the name of the group
    }
  }
  return(NA)  # In case the node does not belong to any group
})

# Run qgraph for the metabolites dataset
qgraph(wadj_matrix_conc,
       layout = "spring",
       edge.color = edge_colors_conc,
       nodeNames = node_names_conc,
       color = node_colors_conc,  # Use the defined node colors
       groups = node_groups_conc,  # Use the groups assigned to nodes
       legend = TRUE,
       legend.mode = "style2",
       legend.cex = 0.3,  # Smaller legend size
       vsize = 3.5,         # Adjust node size as needed
       esize = 15)          # Adjust edge size as needed

# MGM for metadata
metadata_filtered <- metadata[, !colnames(metadata) %in% c("ID", "Exposure_Group")]

# Specify the types of each variable
types <- sapply(colnames(metadata_filtered), function(var) {
  if (var %in% c("Gender", "Env_type", "Parents_liv_tog", "Same_edu")) {
    "c"  # Categorical
  } else {
    "g"  # Gaussian
  }
})

# Specify the levels for each variable
levels <- sapply(colnames(metadata_filtered), function(var) {
  if (var == "Gender") {
    2  # 2 levels for Gender
  } else if (var %in% c("Env_type", "Same_edu")) {
    3  # 3 levels for Env_type and Same_edu
  } else if (var == "Parents_liv_tog") {
    2  # 2 levels for Civil_status
  } else {
    1  # Continuous variables
  }
})

# Ensure the data is a matrix
data_matrix <- as.matrix(metadata_filtered)

# Run the MGM model
fit_mgm <- mgm(data = data_matrix, 
               type = types, 
               level = levels, 
               k = 2, 
               lambdaSel = "CV", 
               lambdaFolds = 10, 
               ruleReg = "AND")

# Extract the weighted adjacency matrix and edge color from fit_mgm
wadj_matrix <- fit_mgm$pairwise$wadj
edge_colors <- fit_mgm$pairwise$edgecolor

# Define node names
node_names <- colnames(metadata_filtered)

# Assign a single color for all nodes
node_colors <- rep("#FFFFBA", length(node_names))

# Run qgraph without groups, adding a title
qgraph(wadj_matrix,
       layout = "spring",
       edge.color = edge_colors,
       nodeNames = node_names,
       color = node_colors,  # Use the defined single color
       legend = TRUE,  # No legend
       vsize = 5,       # Adjust node size as needed
       esize = 15,      # Edge size
       title = "Metadata Factors")



###########################
# MGM for each group sepparately
###########################

statistics_0 <- subset(statistics_df, Exposure_Group == 0)
statistics_1 <- subset(statistics_df, Exposure_Group == 1)
statistics_2 <- subset(statistics_df, Exposure_Group == 2)
statistics_3 <- subset(statistics_df, Exposure_Group == 3)

# No_TSE
statistics_0_filtered <- statistics_0[, !colnames(statistics_0) %in% c("ID", "Exposure_Group", "Exposure_Group_duplicate", "Smokers_in_home", "Cigarettes_in_home", "Cigarettes_outside_home")]

# Specify the types of each variable
types <- sapply(colnames(statistics_0_filtered), function(var) {
  if (var %in% c("Gender", "Env_type", "Parents_liv_tog", "Same_edu")) {
    "c"
  } else {
    "g"
  }
})

# Specify the levels for each variable
levels <- sapply(colnames(statistics_0_filtered), function(var) {
  if (var == "Gender") {
    2
  } else if (var %in% c("Env_type", "Same_edu")) {
    3
  } else if (var %in% c("Parents_liv_tog")) {
    2
  } else {
    1  # for continuous variables
  }
})

# Ensure the data is a matrix
data_matrix <- as.matrix(statistics_0_filtered)

# Run the MGM model
fit_mgm <- mgm(data = data_matrix, 
               type = types, 
               level = levels, 
               k = 2, 
               lamdaSel = "CV", 
               lambdaFolds = 10, 
               ruleReg = "AND")

# Extract the weighted adjacency matrix and edge color from fit_mgm
wadj_matrix <- fit_mgm$pairwise$wadj
edge_colors <- fit_mgm$pairwise$edgecolor

# Define node names (e.g., column names from your original dataset)
node_names <- colnames(statistics_0_filtered)

# Plot with qgraph
groups <- list(
  "Oxidative stress markers" = c("8-oxo-dGUO", "8-OH-guanosine"),
  "Neurotransmitters" = c("Adenosine", "Anthranilic", "Dopamine", "Epinephrine", 
                          "GABA", "Glutamate", "Glutamine", "Histamine", 
                          "Kynurenic acid", "Kynurenine", "Melatonin", 
                          "Norepinephrine", "Quinolinic acid", 
                          "Serotonin", "Tryptophan", "Xanthurenic acid", "QA/KA"),
  "Tobacco-related chemicals" = c("NNAL", "Cotinine", "OH-cotinine"),
  "Metadata factors" = c("Gender", "Weight_kg", "Height_cm","Fruit_veg_days",
                         "Fruit_veg_per_day", "High_fat_diet", 
                         "Exercise_days", "Env_type", "Parents_liv_tog", 
                         "Parents_edu", "Income", "BMI", "Same_edu")
)

# Assign colors to each group
group_colors <- c("skyblue", "orange", "#FFB3BA", "#FFFFBA")  # Colors for the groups

# Create a color vector for the nodes
node_colors <- rep(NA, length(node_names))  # Initialize a vector for node colors

# Assign colors based on group membership
for (i in seq_along(groups)) {
  node_colors[node_names %in% groups[[i]]] <- group_colors[i]
}

# Create a vector for the groups
node_groups <- sapply(node_names, function(name) {
  for (i in seq_along(groups)) {
    if (name %in% groups[[i]]) {
      return(names(groups)[i])  # Return the name of the group
    }
  }
  return(NA)  # In case the node does not belong to any group
})

# Run qgraph
qgraph(wadj_matrix,
       layout = "spring",
       edge.color = edge_colors,
       nodeNames = node_names,
       color = node_colors,  # Use the defined node colors
       groups = node_groups,  # Use the groups assigned to nodes
       legend = TRUE,
       title = "MGM No_TSE",
       legend.mode = "style2",
       legend.cex = 0.25,  # Smaller legend size
       vsize = 3.5,         # Adjust node size as needed
       esize = 15)  


# THS 
statistics_2_filtered <- statistics_2[, !colnames(statistics_2) %in% c("ID", "Exposure_Group", "Exposure_Group_duplicate")]

# Specify the types of each variable
types <- sapply(colnames(statistics_2_filtered), function(var) {
  if (var %in% c("Gender", "Env_type", "Parents_liv_tog", "Same_edu")) {
    "c"
  } else {
    "g"
  }
})

# Specify the levels for each variable
levels <- sapply(colnames(statistics_2_filtered), function(var) {
  if (var == "Gender") {
    2
  } else if (var %in% c("Same_edu")) {
    3
  } else if (var %in% c("Parents_liv_tog", "Env_type")) {
    2
  } else {
    1  # for continuous variables
  }
})

# Ensure the data is a matrix
data_matrix <- as.matrix(statistics_2_filtered)

# Run the MGM model
fit_mgm <- mgm(data = data_matrix, 
               type = types, 
               level = levels, 
               k = 2, 
               lamdaSel = "CV", 
               lambdaFolds = 10, 
               ruleReg = "AND")

# Extract the weighted adjacency matrix and edge color from fit_mgm
wadj_matrix <- fit_mgm$pairwise$wadj
edge_colors <- fit_mgm$pairwise$edgecolor

# Define node names (e.g., column names from your original dataset)
node_names <- colnames(statistics_2_filtered)

# Plot with qgraph
groups <- list(
  "Oxidative stress markers" = c("8-oxo-dGUO", "8-OH-guanosine"),
  "Neurotransmitters" = c("Adenosine", "Anthranilic", "Dopamine", "Epinephrine", 
                          "GABA", "Glutamate", "Glutamine", "Histamine", 
                          "Kynurenic acid", "Kynurenine", "Melatonin", 
                          "Norepinephrine", "Quinolinic acid", 
                          "Serotonin", "Tryptophan", "Xanthurenic acid", "QA/KA"),
  "Tobacco-related chemicals" = c("NNAL", "Cotinine", "OH-cotinine"),
  "Metadata factors" = c("Gender", "Weight_kg", "Height_cm", "Smokers_in_home", 
                         "Cigarettes_in_home", "Cigarettes_outside_home", 
                         "Fruit_veg_days", "Fruit_veg_per_day", "High_fat_diet", 
                         "Exercise_days", "Env_type", "Parents_liv_tog", 
                         "Parents_edu", "Income", "BMI", "Same_edu")
)

# Assign colors to each group
group_colors <- c("skyblue", "orange", "#FFB3BA", "#FFFFBA")  # Colors for the groups

# Create a color vector for the nodes
node_colors <- rep(NA, length(node_names))  # Initialize a vector for node colors

# Assign colors based on group membership
for (i in seq_along(groups)) {
  node_colors[node_names %in% groups[[i]]] <- group_colors[i]
}

# Create a vector for the groups
node_groups <- sapply(node_names, function(name) {
  for (i in seq_along(groups)) {
    if (name %in% groups[[i]]) {
      return(names(groups)[i])  # Return the name of the group
    }
  }
  return(NA)  # In case the node does not belong to any group
})

# Run qgraph
qgraph(wadj_matrix,
       layout = "spring",
       edge.color = edge_colors,
       nodeNames = node_names,
       color = node_colors,  # Use the defined node colors
       groups = node_groups,  # Use the groups assigned to nodes
       legend = TRUE,
       title = "MGM THS",
       legend.mode = "style2",
       legend.cex = 0.25,  # Smaller legend size
       vsize = 3.5,         # Adjust node size as needed
       esize = 15)
       
       
# SHS
statistics_1_filtered <- statistics_1[, !colnames(statistics_1) %in% c("ID", "Exposure_Group", "Exposure_Group_duplicate")]
statistics_1_filtered$Env_type[statistics_1_filtered$Env_type == 1] <- 0 # Because there is only 1 sample with 1 in env_type

# Specify the types of each variable
types <- sapply(colnames(statistics_1_filtered), function(var) {
  if (var %in% c("Gender", "Env_type","Parents_edu", "Parents_liv_tog", "Same_edu")) {
    "c"
  } else {
    "g"
  }
})

# Specify the levels for each variable
levels <- sapply(colnames(statistics_1_filtered), function(var) {
  if (var == "Gender") {
    2
  } else if (var %in% c("Same_edu", "Parents_edu")) {
    3
  } else if (var %in% c("Parents_liv_tog", "Env_type")) {
    2
  } else {
    1  # for continuous variables
  }
})

# Ensure the data is a matrix
data_matrix <- as.matrix(statistics_1_filtered)

# Run the MGM model
fit_mgm <- mgm(data = data_matrix, 
               type = types, 
               level = levels, 
               k = 2, 
               lamdaSel = "CV", 
               lambdaFolds = 10, 
               ruleReg = "AND")

# Extract the weighted adjacency matrix and edge color from fit_mgm
wadj_matrix <- fit_mgm$pairwise$wadj
edge_colors <- fit_mgm$pairwise$edgecolor

# Define node names (e.g., column names from your original dataset)
node_names <- colnames(statistics_1_filtered)

# Plot with qgraph
groups <- list(
  "Oxidative stress markers" = c("8-oxo-dGUO", "8-OH-guanosine"),
  "Neurotransmitters" = c("Adenosine", "Anthranilic", "Dopamine", "Epinephrine", 
                          "GABA", "Glutamate", "Glutamine", "Histamine", 
                          "Kynurenic acid", "Kynurenine", "Melatonin", 
                          "Norepinephrine", "Quinolinic acid", 
                          "Serotonin", "Tryptophan", "Xanthurenic acid", "QA/KA"),
  "Tobacco-related chemicals" = c("NNAL", "Cotinine", "OH-cotinine"),
  "Metadata factors" = c("Gender", "Weight_kg", "Height_cm", "Smokers_in_home", 
                         "Cigarettes_in_home", "Cigarettes_outside_home", 
                         "Fruit_veg_days", "Fruit_veg_per_day", "High_fat_diet", 
                         "Exercise_days", "Env_type", "Parents_liv_tog", 
                         "Parents_edu", "Income", "BMI", "Same_edu")
)

# Assign colors to each group
group_colors <- c("skyblue", "orange", "#FFB3BA", "#FFFFBA")  # Colors for the groups

# Create a color vector for the nodes
node_colors <- rep(NA, length(node_names))  # Initialize a vector for node colors

# Assign colors based on group membership
for (i in seq_along(groups)) {
  node_colors[node_names %in% groups[[i]]] <- group_colors[i]
}

# Create a vector for the groups
node_groups <- sapply(node_names, function(name) {
  for (i in seq_along(groups)) {
    if (name %in% groups[[i]]) {
      return(names(groups)[i])  # Return the name of the group
    }
  }
  return(NA)  # In case the node does not belong to any group
})

# Run qgraph
qgraph(wadj_matrix,
       layout = "spring",
       edge.color = edge_colors,
       nodeNames = node_names,
       color = node_colors,  # Use the defined node colors
       groups = node_groups,  # Use the groups assigned to nodes
       legend = TRUE,
       title = "MGM SHS",
       legend.mode = "style2",
       legend.cex = 0.25,  # Smaller legend size
       vsize = 3.5,         # Adjust node size as needed
       esize = 15)

# TSE_out
statistics_3_filtered <- statistics_3[, !colnames(statistics_3) %in% c("ID", "Exposure_Group", "Exposure_Group_duplicate", "Smokers_in_home", "Cigarettes_in_home", "Cigarettes_outside_home")]

# Specify the types of each variable
types <- sapply(colnames(statistics_3_filtered), function(var) {
  if (var %in% c("Gender", "Env_type","Parents_edu", "Parents_liv_tog", "Same_edu")) {
    "c"
  } else {
    "g"
  }
})

# Specify the levels for each variable
levels <- sapply(colnames(statistics_3_filtered), function(var) {
  if (var == "Gender") {
    2
  } else if (var %in% c("Same_edu", "Parents_edu")) {
    3
  } else if (var %in% c("Parents_liv_tog", "Env_type")) {
    2
  } else {
    1  # for continuous variables
  }
})

# Ensure the data is a matrix
data_matrix <- as.matrix(statistics_3_filtered)

# Run the MGM model
fit_mgm <- mgm(data = data_matrix, 
               type = types, 
               level = levels, 
               k = 2, 
               lamdaSel = "CV", 
               lambdaFolds = 10, 
               ruleReg = "AND")

# Extract the weighted adjacency matrix and edge color from fit_mgm
wadj_matrix <- fit_mgm$pairwise$wadj
edge_colors <- fit_mgm$pairwise$edgecolor

# Define node names (e.g., column names from your original dataset)
node_names <- colnames(statistics_3_filtered)

# Plot with qgraph
groups <- list(
  "Oxidative stress markers" = c("8-oxo-dGUO", "8-OH-guanosine"),
  "Neurotransmitters" = c("Adenosine", "Anthranilic", "Dopamine", "Epinephrine", 
                          "GABA", "Glutamate", "Glutamine", "Histamine", 
                          "Kynurenic acid", "Kynurenine", "Melatonin", 
                          "Norepinephrine", "Quinolinic acid", 
                          "Serotonin", "Tryptophan", "Xanthurenic acid", "QA/KA"),
  "Tobacco-related chemicals" = c("NNAL", "Cotinine", "OH-cotinine"),
  "Metadata factors" = c("Gender", "Weight_kg", "Height_cm","Fruit_veg_days",
                         "Fruit_veg_per_day", "High_fat_diet", 
                         "Exercise_days", "Env_type", "Parents_liv_tog", 
                         "Parents_edu", "Income", "BMI", "Same_edu")
)

# Assign colors to each group
group_colors <- c("skyblue", "orange", "#FFB3BA", "#FFFFBA")  # Colors for the groups

# Create a color vector for the nodes
node_colors <- rep(NA, length(node_names))  # Initialize a vector for node colors

# Assign colors based on group membership
for (i in seq_along(groups)) {
  node_colors[node_names %in% groups[[i]]] <- group_colors[i]
}

# Create a vector for the groups
node_groups <- sapply(node_names, function(name) {
  for (i in seq_along(groups)) {
    if (name %in% groups[[i]]) {
      return(names(groups)[i])  # Return the name of the group
    }
  }
  return(NA)  # In case the node does not belong to any group
})

# Run qgraph
qgraph(wadj_matrix,
       layout = "spring",
       edge.color = edge_colors,
       nodeNames = node_names,
       color = node_colors,  # Use the defined node colors
       groups = node_groups,  # Use the groups assigned to nodes
       legend = TRUE,
       title = "MGM TSE_out",
       legend.mode = "style2",
       legend.cex = 0.25,  # Smaller legend size
       vsize = 3.5,         # Adjust node size as needed
       esize = 15)





