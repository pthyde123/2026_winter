
##  Create Genetic Relationship Matrix from VCF

library(tidyverse)
library(vcfR)
library(SNPRelate)
library(gdsfmt)
library(AGHmatrix)

## Set paths

vcf_file <- "data/WOP_genotype_output.vcf"        # Path to your VCF file
gds_file <- "data/your_data.gds"        # Temporary file for SNPRelate
output_file <- "data/GRM_matrix.csv"    # Output file for GRM



vcf <- read.vcfR("data/WOP_genotype_output.vcf")  # this vcf file was created by Mirza combining multiple sources of genotype data

# Keep only biallelic sites
biallelic_vcf <- vcf[is.biallelic(vcf), ]


cat("Original SNPs:", nrow(vcf@fix), "\n")
cat("Biallelic SNPs:", nrow(biallelic_vcf@fix), "\n")


snpgdsVCF2GDS(
  vcf.fn = "data/WOP_genotype_output.vcf",
  out.fn = "data/WOP_genotype_output.gds",
  method = "biallelic.only"
)  ### this generates an error because there are 2/2 calls the following steps address this


# point to your VCF path
vcf_path <- "data/WOP_genotype_output.vcf"

# read the first ~30 lines so you can see headers + early records
lines <- readLines(vcf_path, n = 200)
# print header lines
cat(lines[1:20], sep = "\n")

# print the exact problematic line reported by snpgds (LINE: 13)
cat("\n---- LINE 13 ----\n")
cat(lines[13], "\n")



# extract GT matrix (rows = variants, cols = samples)
gt_mat <- extract.gt(vcf, element = "GT", as.numeric = FALSE)

# function to detect allele indices >1 in a string, catches "2/2", "2|1", "3/.", etc.
has_gt_gt2 <- function(x) {
  any(grepl("(^|[\\|/])[2-9][0-9]*", x, perl = TRUE), na.rm = TRUE)
}

# apply per row
bad_rows_logical <- apply(gt_mat, 1, has_gt_gt2)

table(bad_rows_logical)  # shows number of TRUE (bad) and FALSE (good)


# select only rows that are clean (no allele index >1)
clean_vcf <- vcf[!bad_rows_logical, ]

cat("Original variants:", nrow(vcf@fix), "\n")
cat("Kept (biallelic GT-only) variants:", nrow(clean_vcf@fix), "\n")

# write filtered VCF
filtered_vcf_path <- "data/WOP_filtered_for_snpRelate.vcf"
write.vcf(clean_vcf, file = filtered_vcf_path)
cat("Filtered VCF saved to:", filtered_vcf_path, "\n")

## the previous steps removed 103 marks there are 2881 left




# Convert VCF to GDS format

snpgdsVCF2GDS(
  vcf.fn = filtered_vcf_path,
  out.fn = "data/WOP_filtered_for_snpRelate.gds",
  method = "biallelic.only"
)


# Open GDS file
genofile <- snpgdsOpen("data/WOP_filtered_for_snpRelate.gds")

#Perform basic SNP filtering
# remove SNPs with low MAF or missingness  ### are these good threasholds??? 
#  this removes about 800 markers!!
snpset <- snpgdsSelectSNP(genofile, maf = 0.05, missing.rate = 0.1, autosome.only = TRUE)

# Calculate GRM, Using VanRaden method

grm <- snpgdsGRM(genofile, sample.id = NULL, snp.id = snpset, method = "GCTA")

# Extract matrix
GRM_matrix <- grm$grm
rownames(GRM_matrix) <- grm$sample.id
colnames(GRM_matrix) <- grm$sample.id

# Save the GRM to CSV 
write.csv(GRM_matrix, output_file, quote = FALSE)


# Close GDS file
snpgdsClose(genofile)

# Heatmap of genetic relatedness
heatmap(GRM_matrix, symm = TRUE, main = "Genetic Relationship Matrix", col = heat.colors(256))




### Convert GRM to long formt with parent designations. 


# Convert the matrix to a data frame with rownames
grm_df <- as.data.frame(GRM_matrix)
grm_df$ID1 <- rownames(grm_df)

# Reshape into long format


grm_long <- grm_df  %>% 
  pivot_longer(
    cols = -ID1,          # all columns except ID1
    names_to = "ID2",     # new column name for column identifiers
    values_to = "GRM_Value"  # new column name for cell values
  ) %>% 
 rename(p1 = ID1) %>% 
  rename(p2 = ID2) %>% 
  mutate(cross = str_c(p1," X ", p2))

## Optional: remove duplicate pairs (since GRM is symmetric)
## grm_long_unique <- grm_long[as.numeric(factor(grm_long$ID1)) <= as.numeric(factor(grm_long$ID2)), ]

# Save to file
write.csv(grm_long, "data/GRM_long_format.csv", row.names = FALSE, quote = FALSE)














