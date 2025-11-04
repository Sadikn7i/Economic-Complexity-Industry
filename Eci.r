git init# ============================================================================
# SETUP: Load Packages and Data (ONCE)
# ============================================================================

library(readxl)
library(dplyr)
library(tidyr)

# Load the data once
file_path <- "path to file"
data <- read_excel(file_path)

# ============================================================================
# STAGE 1: Basic Exploration
# ============================================================================

cat("=== DATASET OVERVIEW ===\n\n")
cat("Dimensions:", nrow(data), "rows x", ncol(data), "columns\n\n")

cat("Column names:\n")
print(names(data))

cat("\n=== KEY VARIABLES FOR ANALYSIS ===\n\n")

# Check provinces
cat("Provinces:\n")
cat("- Unique provinces:", n_distinct(data$province), "\n")
cat("- Example provinces:", head(unique(data$province), 10), "\n\n")

# Check sector2_num
cat("Sector (sector2_num):\n")
cat("- Unique sectors:", n_distinct(data$sector2_num), "\n")
cat("- Example sectors:", head(unique(data$sector2_num), 10), "\n\n")

# Check log_exports
cat("Log Exports:\n")
cat("- Min:", min(data$log_exports, na.rm = TRUE), "\n")
cat("- Max:", max(data$log_exports, na.rm = TRUE), "\n")
cat("- Mean:", mean(data$log_exports, na.rm = TRUE), "\n")
cat("- NAs:", sum(is.na(data$log_exports)), "\n\n")

# Check for missing values
cat("=== MISSING VALUES ===\n\n")
missing_summary <- data.frame(
  variable = c("province", "sector2_num", "log_exports"),
  missing = c(
    sum(is.na(data$province)),
    sum(is.na(data$sector2_num)),
    sum(is.na(data$log_exports))
  )
)
print(missing_summary)

# Show first few rows
cat("\n=== FIRST 10 ROWS (key columns) ===\n\n")
print(data %>% 
        select(province, sector2_num, log_exports) %>% 
        head(10))

cat("\n=== ACTION REQUIRED ===\n")
cat("1. Check if the data loaded correctly\n")
cat("2. Verify province, sector2_num, and log_exports look reasonable\n")
cat("3. Once confirmed, we'll move to Stage 2\n\n")

# ============================================================================
# STAGE 2: Create Export Matrix (Province x Sector)
# ============================================================================

cat("=== STAGE 2: CREATING EXPORT MATRIX ===\n\n")

# Step 1: Prepare the data
df <- data %>%
  select(province, sector2_num, log_exports) %>%
  rename(location = province,
         product = sector2_num,
         export_value = log_exports)

cat("Data prepared:\n")
cat("- Rows:", nrow(df), "\n")
cat("- Locations (provinces):", n_distinct(df$location), "\n")
cat("- Products (sectors):", n_distinct(df$product), "\n\n")

# Step 2: Aggregate exports by province-sector
export_aggregated <- df %>%
  group_by(location, product) %>%
  summarise(total_export = sum(export_value, na.rm = TRUE), .groups = 'drop')

cat("After aggregation:\n")
cat("- Unique province-sector pairs:", nrow(export_aggregated), "\n\n")

# Step 3: Create wide format matrix
export_wide <- export_aggregated %>%
  pivot_wider(names_from = product, 
              values_from = total_export, 
              values_fill = 0)

cat("Wide format matrix created:\n")
cat("- Dimensions:", nrow(export_wide), "provinces x", (ncol(export_wide)-1), "sectors\n\n")

# Step 4: Convert to matrix format
locations <- export_wide$location
X_matrix <- as.matrix(export_wide[, -1])
rownames(X_matrix) <- locations

cat("=== EXPORT MATRIX (X_cp) ===\n\n")
cat("Matrix dimensions:", nrow(X_matrix), "x", ncol(X_matrix), "\n")
cat("Province codes:", rownames(X_matrix), "\n")
cat("Number of sectors:", ncol(X_matrix), "\n\n")

# Check matrix properties
cat("Matrix summary:\n")
cat("- Total cells:", nrow(X_matrix) * ncol(X_matrix), "\n")
cat("- Non-zero cells:", sum(X_matrix > 0), "\n")
cat("- Zero cells:", sum(X_matrix == 0), "\n")
cat("- Sparsity:", round(sum(X_matrix == 0) / (nrow(X_matrix) * ncol(X_matrix)) * 100, 2), "%\n\n")

# Show a sample of the matrix
cat("=== SAMPLE OF EXPORT MATRIX ===\n")
cat("(First 5 provinces x first 6 sectors)\n\n")
print(round(X_matrix[1:min(5, nrow(X_matrix)), 1:min(6, ncol(X_matrix))], 2))

cat("\n=== TOTAL EXPORTS BY PROVINCE ===\n\n")
total_by_province <- data.frame(
  province = rownames(X_matrix),
  total_exports = rowSums(X_matrix)
)
print(total_by_province %>% arrange(desc(total_exports)))

cat("\n=== TOTAL EXPORTS BY SECTOR (Top 10) ===\n\n")
total_by_sector <- data.frame(
  sector = colnames(X_matrix),
  total_exports = colSums(X_matrix)
)
print(total_by_sector %>% arrange(desc(total_exports)) %>% head(10))

cat("\n=== ACTION REQUIRED ===\n")
cat("1. Verify the matrix dimensions look correct\n")
cat("2. Check that total exports by province make sense\n")
cat("3. Confirm matrix is not too sparse\n")
cat("4. Once confirmed, we'll move to Stage 3: Calculate RCA\n\n")

# ============================================================================
# STAGE 3: Calculate RCA (Revealed Comparative Advantage)
# ============================================================================

cat("=== STAGE 3: CALCULATING RCA ===\n\n")

cat("Step 1: Calculate totals\n")

# Total exports by province (row sums)
total_by_location <- rowSums(X_matrix)
cat("- Total exports by province: calculated\n")

# Total exports by sector (column sums)
total_by_product <- colSums(X_matrix)
cat("- Total exports by sector: calculated\n")

# Total world exports (grand total)
total_world <- sum(X_matrix)
cat("- Total world exports:", round(total_world, 2), "\n\n")

cat("Step 2: Calculate RCA for each province-sector pair\n")

# Initialize RCA matrix
RCA <- matrix(0, nrow = nrow(X_matrix), ncol = ncol(X_matrix))
rownames(RCA) <- rownames(X_matrix)
colnames(RCA) <- colnames(X_matrix)

# Calculate RCA
for (i in 1:nrow(X_matrix)) {
  for (j in 1:ncol(X_matrix)) {
    share_in_location <- X_matrix[i, j] / total_by_location[i]
    share_in_world <- total_by_product[j] / total_world
    if (share_in_world > 0) {
      RCA[i, j] <- share_in_location / share_in_world
    } else {
      RCA[i, j] <- 0
    }
  }
}

cat("RCA matrix calculated!\n\n")

cat("=== RCA MATRIX SUMMARY ===\n\n")
cat("Dimensions:", nrow(RCA), "provinces x", ncol(RCA), "sectors\n\n")

cat("RCA statistics:\n")
cat("- Min RCA:", round(min(RCA), 4), "\n")
cat("- Max RCA:", round(max(RCA), 4), "\n")
cat("- Mean RCA:", round(mean(RCA), 4), "\n")
cat("- Median RCA:", round(median(RCA), 4), "\n\n")

cat("Comparative advantage (RCA >= 1):\n")
cat("- Cells with RCA >= 1:", sum(RCA >= 1), "\n")
cat("- Cells with RCA < 1:", sum(RCA < 1 & RCA > 0), "\n")
cat("- Zero cells:", sum(RCA == 0), "\n")
cat("- % with comparative advantage:", 
    round(sum(RCA >= 1) / sum(RCA > 0) * 100, 2), "%\n\n")

cat("=== SAMPLE RCA VALUES ===\n")
cat("(First 5 provinces x first 6 sectors)\n\n")
print(round(RCA[1:min(5, nrow(RCA)), 1:min(6, ncol(RCA))], 3))

cat("\n=== PROVINCES WITH MOST COMPARATIVE ADVANTAGES ===\n\n")
comparative_advantages <- data.frame(
  province = rownames(RCA),
  num_advantages = rowSums(RCA >= 1)
)
print(comparative_advantages %>% arrange(desc(num_advantages)))

cat("\n=== SECTORS WITH HIGHEST AVERAGE RCA ===\n\n")
sector_rca <- data.frame(
  sector = colnames(RCA),
  avg_rca = colMeans(RCA),
  num_provinces_with_advantage = colSums(RCA >= 1)
)
print(sector_rca %>% 
        filter(num_provinces_with_advantage > 0) %>%
        arrange(desc(avg_rca)) %>% 
        head(10))

cat("\n=== HIGHEST RCA VALUES (Top 15) ===\n\n")
rca_long <- expand.grid(
  province = rownames(RCA),
  sector = colnames(RCA)
)
rca_long$rca <- as.vector(RCA)

top_rca <- rca_long %>%
  filter(rca > 0) %>%
  arrange(desc(rca)) %>%
  head(15)
print(top_rca)

cat("\n=== ACTION REQUIRED ===\n")
cat("1. Check if RCA values look reasonable\n")
cat("2. Verify that provinces have a mix of RCA >= 1 and RCA < 1\n")
cat("3. Once confirmed, we'll move to Stage 4: Create Binary Matrix M\n\n")

# ============================================================================
# STAGE 4: Create Binary Matrix M (Comparative Advantage Matrix)
# ============================================================================

cat("=== STAGE 4: CREATE BINARY MATRIX M ===\n\n")

cat("Creating binary matrix M...\n")
cat("Rule: M = 1 if RCA >= 1, else M = 0\n\n")

M <- ifelse(RCA >= 1, 1, 0)
rownames(M) <- rownames(RCA)
colnames(M) <- colnames(RCA)

cat("Binary matrix M created!\n\n")

cat("=== BINARY MATRIX SUMMARY ===\n\n")
cat("Dimensions:", nrow(M), "provinces x", ncol(M), "sectors\n\n")

cat("Matrix composition:\n")
cat("- Total cells:", nrow(M) * ncol(M), "\n")
cat("- Cells with M = 1 (comparative advantage):", sum(M == 1), "\n")
cat("- Cells with M = 0 (no comparative advantage):", sum(M == 0), "\n")
cat("- Density:", round(sum(M == 1) / (nrow(M) * ncol(M)) * 100, 2), "%\n\n")

cat("=== SAMPLE OF BINARY MATRIX M ===\n")
cat("(First 5 provinces x first 10 sectors)\n\n")
print(M[1:min(5, nrow(M)), 1:min(10, ncol(M))])

cat("\n=== DIVERSITY (k_c,0) ===\n")
cat("Number of products each province has comparative advantage in\n\n")

diversity <- rowSums(M)
diversity_df <- data.frame(
  province = names(diversity),
  diversity = diversity
)
print(diversity_df %>% arrange(desc(diversity)))

cat("\nDiversity statistics:\n")
cat("- Min diversity:", min(diversity), "\n")
cat("- Max diversity:", max(diversity), "\n")
cat("- Mean diversity:", round(mean(diversity), 2), "\n")
cat("- Median diversity:", median(diversity), "\n\n")

cat("=== UBIQUITY (k_p,0) ===\n")
cat("Number of provinces that have comparative advantage in each product\n\n")

ubiquity <- colSums(M)
ubiquity_df <- data.frame(
  sector = names(ubiquity),
  ubiquity = ubiquity
)

cat("Top 15 most ubiquitous sectors:\n")
print(ubiquity_df %>% arrange(desc(ubiquity)) %>% head(15))

cat("\nUbiquity statistics:\n")
cat("- Min ubiquity:", min(ubiquity), "\n")
cat("- Max ubiquity:", max(ubiquity), "\n")
cat("- Mean ubiquity:", round(mean(ubiquity[ubiquity > 0]), 2), "\n")
cat("- Median ubiquity:", median(ubiquity[ubiquity > 0]), "\n\n")

cat("Sectors present in 0 provinces:", sum(ubiquity == 0), "\n")
cat("Sectors present in 1 province:", sum(ubiquity == 1), "\n")
cat("Sectors present in 2+ provinces:", sum(ubiquity >= 2), "\n\n")

cat("=== DIVERSITY vs UBIQUITY RELATIONSHIP ===\n\n")
cat("Provinces with highest diversity:\n")
print(diversity_df %>% arrange(desc(diversity)) %>% head(5))

cat("\nSectors with highest ubiquity:\n")
print(ubiquity_df %>% arrange(desc(ubiquity)) %>% head(5))

cat("\n=== VISUALIZATION CHECK ===\n\n")
cat("Province 79 (most diverse) has comparative advantage in sectors:\n")
sectors_79 <- colnames(M)[M["79", ] == 1]
cat(sectors_79, "\n")

cat("\nSector 10 (most ubiquitous) has comparative advantage in provinces:\n")
provinces_10 <- rownames(M)[M[, "10"] == 1]
cat(provinces_10, "\n")

cat("\n=== ACTION REQUIRED ===\n")
cat("1. Check diversity values - do they make sense?\n")
cat("2. Check ubiquity values - are some sectors too rare or too common?\n")
cat("3. Once confirmed, we'll move to Stage 5: Calculate ECI and PCI\n\n")

# ============================================================================
# STAGE 5: Calculate ECI and PCI (Method of Reflections)
# ============================================================================

cat("=== STAGE 5: CALCULATE ECI AND PCI ===\n\n")

cat("Method: Iterative reflections algorithm\n")
cat("Starting with diversity and ubiquity...\n\n")

n_iterations <- 20
cat("Running", n_iterations, "iterations...\n\n")

# Initialize
k_c <- diversity
k_p <- ubiquity

# Store iteration history
eci_history <- matrix(0, nrow = length(k_c), ncol = n_iterations)
pci_history <- matrix(0, nrow = length(k_p), ncol = n_iterations)

for (iter in 1:n_iterations) {
  k_c_old <- k_c
  k_p_old <- k_p
  
  k_c_new <- (M %*% k_p) / diversity
  k_c_new[is.na(k_c_new) | is.infinite(k_c_new)] <- 0
  
  k_p_new <- (t(M) %*% k_c) / ubiquity
  k_p_new[is.na(k_p_new) | is.infinite(k_p_new)] <- 0
  
  k_c <- as.vector(k_c_new)
  k_p <- as.vector(k_p_new)
  
  eci_history[, iter] <- scale(k_c)[, 1]
  pci_history[, iter] <- scale(k_p)[, 1]
  
  if (iter %% 5 == 0) {
    cat("Iteration", iter, "complete\n")
  }
}

cat("\nIterations complete!\n\n")

# Final ECI and PCI (standardized)
eci_raw <- k_c
pci_raw <- k_p

eci <- scale(k_c)[, 1]
pci <- scale(k_p)[, 1]

names(eci) <- rownames(M)
names(pci) <- colnames(M)

cat("=== ECI (ECONOMIC COMPLEXITY INDEX) ===\n\n")

eci_df <- data.frame(
  province = names(eci),
  diversity = diversity,
  eci_raw = eci_raw,
  eci = eci
)

cat("ECI Statistics:\n")
cat("- Mean:", round(mean(eci), 4), "\n")
cat("- Std Dev:", round(sd(eci), 4), "\n")
cat("- Min:", round(min(eci), 4), "\n")
cat("- Max:", round(max(eci), 4), "\n\n")

cat("ECI Rankings (all provinces):\n")
print(eci_df %>% 
        arrange(desc(eci)) %>%
        mutate(rank = row_number()))

cat("\n=== PCI (PRODUCT COMPLEXITY INDEX) ===\n\n")

pci_df <- data.frame(
  sector = names(pci),
  ubiquity = ubiquity,
  pci_raw = pci_raw,
  pci = pci
)

cat("PCI Statistics:\n")
cat("- Mean:", round(mean(pci), 4), "\n")
cat("- Std Dev:", round(sd(pci), 4), "\n")
cat("- Min:", round(min(pci), 4), "\n")
cat("- Max:", round(max(pci), 4), "\n\n")

cat("PCI Rankings (Top 20 most complex sectors):\n")
print(pci_df %>% 
        arrange(desc(pci)) %>%
        mutate(rank = row_number()) %>%
        head(20))

cat("\nPCI Rankings (Bottom 10 least complex sectors):\n")
print(pci_df %>% 
        arrange(pci) %>%
        mutate(rank = row_number()) %>%
        head(10))

cat("\n=== ACTION REQUIRED ===\n")
cat("1. Check if ECI rankings make intuitive sense\n")
cat("2. Next, we'll move to Stage 6: Calculate Density\n\n")

# ============================================================================
# STAGE 6: Calculate Product Density
# ============================================================================

cat("=== STAGE 6: CALCULATE PRODUCT DENSITY ===\n\n")

cat("Step 1: Calculate Proximity Matrix (phi)\n\n")

n_products <- ncol(M)
proximity <- matrix(0, nrow = n_products, ncol = n_products)
rownames(proximity) <- colnames(M)
colnames(proximity) <- colnames(M)

cat("Calculating proximity...\n")

for (i in 1:n_products) {
  if (i %% 10 == 0) {
    cat("Progress:", i, "/", n_products, "\n")
  }
  
  for (j in 1:n_products) {
    if (i != j) {
      both <- sum(M[, i] * M[, j])
      prob_i_given_j <- ifelse(ubiquity[j] > 0, both / ubiquity[j], 0)
      prob_j_given_i <- ifelse(ubiquity[i] > 0, both / ubiquity[i], 0)
      proximity[i, j] <- min(prob_i_given_j, prob_j_given_i)
    } else {
      proximity[i, j] <- 1
    }
  }
}

cat("\nProximity matrix calculated!\n\n")

cat("Step 2: Calculate Density\n\n")

density_matrix <- matrix(0, nrow = nrow(M), ncol = ncol(M))
rownames(density_matrix) <- rownames(M)
colnames(density_matrix) <- colnames(M)

cat("Calculating density...\n")

for (i in 1:nrow(M)) {
  if (i %% 5 == 0) {
    cat("Progress:", i, "/", nrow(M), "\n")
  }
  
  for (j in 1:ncol(M)) {
    numerator <- sum(M[i, ] * proximity[j, ])
    denominator <- sum(proximity[j, ])
    density_matrix[i, j] <- ifelse(denominator > 0, numerator / denominator, 0)
  }
}

cat("\nDensity matrix calculated!\n\n")

cat("=== ACTION REQUIRED ===\n")
cat("1. Density calculated successfully\n")
cat("2. Moving to Stage 7: Calculate COI and COG\n\n")

# ============================================================================
# STAGE 7: Calculate COI and COG
# ============================================================================

cat("=== STAGE 7: CALCULATE COI AND COG ===\n\n")

# Calculate COI
coi <- numeric(nrow(M))
names(coi) <- rownames(M)

for (i in 1:nrow(M)) {
  not_produced <- (1 - M[i, ])
  coi[i] <- sum(not_produced * density_matrix[i, ] * pci)
}

# Calculate COG
cog <- coi / diversity
cog[is.na(cog) | is.infinite(cog)] <- 0
names(cog) <- rownames(M)

cat("=== COI (COMPLEXITY OUTLOOK INDEX) RESULTS ===\n\n")

coi_df <- data.frame(
  province = names(coi),
  diversity = diversity,
  eci = eci,
  coi = coi
)

cat("COI Rankings:\n")
print(coi_df %>% 
        arrange(desc(coi)) %>%
        mutate(rank = row_number()))

cat("\n=== COG (COMPLEXITY OUTLOOK GAIN) RESULTS ===\n\n")

cog_df <- data.frame(
  province = names(cog),
  diversity = diversity,
  eci = eci,
  coi = coi,
  cog = cog
)

cat("COG Rankings:\n")
print(cog_df %>% 
        arrange(desc(cog)) %>%
        mutate(rank = row_number()))

cat("\n=== STRATEGIC PROVINCE GROUPS ===\n\n")

median_eci <- median(eci)
median_cog <- median(cog)

leaders <- coi_df %>% filter(eci > median_eci & coi > median(coi))
cat("LEADERS (High ECI, High COI):\n")
print(leaders$province)

emerging <- cog_df %>% filter(eci <= median_eci & cog > median_cog)
cat("\nEMERGING (Low ECI, High COG):\n")
print(emerging$province)

mature <- coi_df %>% filter(eci > median_eci & coi <= median(coi))
cat("\nMATURE (High ECI, Low COI):\n")
print(mature$province)

lagging <- cog_df %>% filter(eci <= median_eci & cog <= median_cog)
cat("\nLAGGING (Low ECI, Low COG):\n")
print(lagging$province)

cat("\n=== ALL STAGES COMPLETE! ===\n")










# ============================================================================
# STAGE 8: Saving Results
# ============================================================================
#1. Set Output Directory



# Directory to save all outputs
output_dir <- "path"
dir.create(output_dir, showWarnings = FALSE)
install.packages("viridis")
library(viridis)


library(ggplot2)
library(writexl)
library(forcats)
library(dplyr)


# ============================================================================
# 1. ECI - Lollipop Chart (more visual than bars)
# ============================================================================
p_eci_lollipop <- ggplot(eci_df %>% arrange(desc(eci)), 
                         aes(x = fct_reorder(province, eci), y = eci)) +
  geom_segment(aes(x = fct_reorder(province, eci), xend = province, 
                   y = 0, yend = eci), color = "#006064", size = 1.5) +
  geom_point(aes(color = eci), size = 5, show.legend = FALSE) +
  coord_flip() +
  scale_color_viridis_c(option = "C", direction = -1) +
  labs(title = "Economic Complexity Index (ECI) by Province",
       subtitle = "Higher values indicate more complex economies",
       x = NULL, y = "ECI") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40")
  )
ggsave(file.path(output_dir, "eci_lollipop.png"), p_eci_lollipop, 
       width = 10, height = 8, dpi = 300)

# ============================================================================
# 2. PCI - Horizontal Barplot with gradient (Top 20 only for clarity)
# ============================================================================
p_pci_bar <- ggplot(pci_df %>% arrange(desc(pci)) %>% head(20),
                    aes(x = fct_reorder(sector, pci), y = pci, fill = pci)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_gradient2(low = "#F8BBD0", mid = "#E91E63", high = "#880E4F", 
                       midpoint = median(pci_df$pci, na.rm = TRUE)) +
  labs(title = "Product Complexity Index (PCI) - Top 20 Sectors",
       subtitle = "Complex products require sophisticated capabilities",
       x = NULL, y = "PCI") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    legend.position = "none",
    axis.text.y = element_text(size = 11, margin = margin(r = 10)),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40")
  )
ggsave(file.path(output_dir, "pci_bar_top20.png"), p_pci_bar, 
       width = 10, height = 9, dpi = 300)

# ============================================================================
# 3. Diversity - Point + Line Chart
# ============================================================================
p_diversity_point <- ggplot(diversity_df %>% arrange(desc(diversity)), 
                            aes(x = fct_reorder(province, diversity), 
                                y = diversity, group = 1)) +
  geom_line(color = "#5E35B1", size = 1.2, alpha = 0.7) +
  geom_point(aes(color = diversity), size = 4, show.legend = FALSE) +
  coord_flip() +
  scale_color_gradient(low = "#CE93D8", high = "#4A148C") +
  labs(title = "Province Diversity (Number of Competitive Sectors)",
       subtitle = "How many sectors each province exports competitively",
       x = NULL, y = "Diversity (# of sectors)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(size = 11, margin = margin(r = 10)),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40")
  )
ggsave(file.path(output_dir, "diversity_point_line.png"), p_diversity_point, 
       width = 10, height = 8, dpi = 300)

# ============================================================================
# 4. Ubiquity - Tile/Heatmap style (Top 25 sectors)
# ============================================================================
p_ubiquity_tile <- ggplot(ubiquity_df %>% arrange(desc(ubiquity)) %>% head(25),
                          aes(x = 1, y = fct_reorder(sector, ubiquity), fill = ubiquity)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = ubiquity), color = "white", fontface = "bold", size = 5) +
  scale_fill_gradient(low = "#AED581", high = "#1B5E20") +
  labs(title = "Sector Ubiquity (Top 25)",
       subtitle = "How many provinces export each sector",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 11, margin = margin(r = 10)),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40")
  )
ggsave(file.path(output_dir, "ubiquity_tile.png"), p_ubiquity_tile, 
       width = 8, height = 10, dpi = 300)

# ============================================================================
# 5. COI - Scatter with size (Bubble chart)
# ============================================================================
p_coi_bubble <- ggplot(coi_df, aes(x = diversity, y = coi, size = eci, color = eci)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = province), size = 3, hjust = -0.2, vjust = 0.5, check_overlap = TRUE) +
  scale_color_viridis_c(option = "plasma", name = "ECI") +
  scale_size_continuous(range = c(4, 12), name = "ECI") +
  labs(title = "Complexity Outlook Index (COI) vs Diversity",
       subtitle = "Bubble size = ECI",
       x = "Diversity (#sectors)", y = "COI") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "bottom"
  )
ggsave(file.path(output_dir, "coi_bubble.png"), p_coi_bubble, 
       width = 12, height = 8, dpi = 300)

# ============================================================================
# 6. COG - Diverging bar chart (positive/negative from median)
# ============================================================================
cog_df$cog_diff <- cog_df$cog - median(cog_df$cog)
cog_df$direction <- ifelse(cog_df$cog_diff > 0, "Above Median", "Below Median")

p_cog_diverging <- ggplot(cog_df %>% arrange(cog_diff), 
                          aes(x = fct_reorder(province, cog_diff), y = cog_diff, fill = direction)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("Above Median" = "#43A047", "Below Median" = "#E53935"),
                    name = NULL) +
  labs(title = "Complexity Outlook Gain (COG) - Deviation from Median",
       subtitle = "Provinces above/below median growth potential",
       x = NULL, y = "COG (deviation from median)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(size = 11, margin = margin(r = 10)),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "bottom"
  )
ggsave(file.path(output_dir, "cog_diverging.png"), p_cog_diverging, 
       width = 10, height = 8, dpi = 300)

# ============================================================================
# 7. ECI vs Diversity Scatter
# ============================================================================
p_eci_diversity <- ggplot(eci_df, aes(x = diversity, y = eci)) +
  geom_point(aes(color = eci), size = 5, alpha = 0.8) +
  geom_text(aes(label = province), size = 3, hjust = -0.2, vjust = 0.5, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = TRUE, color = "#FF6F00", linetype = "dashed", size = 1) +
  scale_color_gradient2(low = "#D32F2F", mid = "#FDD835", high = "#388E3C",
                        midpoint = 0, name = "ECI") +
  labs(title = "Economic Complexity vs Diversity",
       subtitle = "Relationship between ECI and number of sectors",
       x = "Diversity (#sectors)", y = "ECI") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "bottom"
  )
ggsave(file.path(output_dir, "eci_vs_diversity_scatter.png"), p_eci_diversity, 
       width = 12, height = 8, dpi = 300)

# ============================================================================
# SAVE ALL TABLES
# ============================================================================
write_xlsx(list(
  ECI = eci_df %>% arrange(desc(eci)),
  PCI = pci_df %>% arrange(desc(pci)),
  Diversity = diversity_df %>% arrange(desc(diversity)),
  Ubiquity = ubiquity_df %>% arrange(desc(ubiquity)),
  COI = coi_df %>% arrange(desc(coi)),
  COG = cog_df %>% arrange(desc(cog))
), file.path(output_dir, "all_results.xlsx"))

cat("\n✓ All plots and tables saved to:", output_dir, "\n")




#ah saving rca
# Save RCA matrix as Excel and CSV
library(writexl)

# Convert RCA matrix to a dataframe (with province as first column)
rca_df <- as.data.frame(RCA)
rca_df$province <- rownames(RCA)
rca_df <- rca_df[, c("province", setdiff(names(rca_df), "province"))]

# Save as Excel
write_xlsx(rca_df, file.path(output_dir, "rca_matrix.xlsx"))
