library(easyPubMed)
library(xml2)
library(dplyr)
library(purrr)
library(ellmer)
library(glue)
library(writexl)

#Define Pubmed Query
query <- "SRTR AND transplant AND outcomes"

#Search Pubmed
results <- get_pubmed_ids(query)
papers <- fetch_pubmed_data(results)

# Parse raw XML with xml2
doc <- xml2::read_xml(papers)

# Extract all Pubmed article nodes directly
articles <- xml2::xml_find_all(doc, ".//PubmedArticle")

#Define extraction function
safe_extract <- function(article_xml) {
  
  # Extract all abstract sections
  abstract_nodes <- xml2::xml_find_all(article_xml, ".//AbstractText")
  
  abstract_full <-
    if (length(abstract_nodes) == 0) NA
  else paste(xml2::xml_text(abstract_nodes), collapse = " ")
  
  # Extract single fields safely
  get_field <- function(xpath) {
    node <- xml2::xml_find_first(article_xml, xpath)
    if (is.na(node) || length(node) == 0) return(NA)
    xml2::xml_text(node)
  }
  
  data.frame(
    pmid    = get_field(".//PMID"),
    title   = get_field(".//ArticleTitle"),
    abstract = abstract_full,
    journal = get_field(".//Journal//Title"),
    year    = get_field(".//PubDate//Year"),
    stringsAsFactors = FALSE
  )
}

#Extract to DF from XML, and select 100 articles.
pubmed_df <- purrr::map_df(articles, safe_extract)%>%
  slice(1:100)


#Call file with code to define prompt and schema
source("R/abstract-schema.R")

#Function to call ChatGPT
classify_chatgpt <- function(abstract) {
  chat <- chat_openai(
    model = "gpt-5.1",
    system_prompt = "You are an infectious diseases specialist."
  )
  chat$chat_structured(
    id_prompt(abstract),
    type = type_id_classification,
    echo = "none"
  )
}

#Function to call Claude
classify_claude <- function(abstract) {
  chat <- chat_anthropic(
    model = "claude-sonnet-4-5",
    system_prompt = "You are an infectious diseases specialist."
  )
  chat$chat_structured(
    id_prompt(abstract),
    type = type_id_classification,
    echo = "none"
  )
}


#Function to call Gemini
classify_gemini <- function(abstract) {
  chat <- chat_google_gemini(
    model = "gemini-2.5-flash",
    system_prompt = "You are an infectious diseases specialist."
  )
  chat$chat_structured(
    id_prompt(abstract),
    type = type_id_classification,
    echo = "none"
  )
}

classify_qwen14b <- function(abstract) {
  chat <- chat_ollama(
    model = "qwen2.5:14b",
    base_url = "http://localhost:11434",
    system_prompt = "You are an infectious diseases specialist."
  )
  chat$chat_structured(
    id_prompt(abstract),
    type = type_id_classification,
    echo = "none"
  )
}


print(Sys.time())
#Call LLMs and add columns to pubmed_df
classified_df <- pubmed_df %>%
  mutate(
    chatgpt  = map(abstract, classify_chatgpt))
print(Sys.time())

classified_df <- classified_df %>%
  mutate(
    claude  = map(abstract, classify_claude))
print(Sys.time())

classified_df <- classified_df %>%
  mutate(
    gemini  = map(abstract, classify_gemini))
print(Sys.time())

classified_df_ollama14b<-classified_df%>%
  mutate(
    chatqwen14b = map(abstract, classify_qwen14b))
print(Sys.time())


#Expand data set
classified_wide <- classified_df_ollama14b %>%
  mutate(
    chatgpt = map(chatgpt, as_tibble),
    claude  = map(claude,  as_tibble),
    gemini  = map(gemini,  as_tibble),
    chatqwen14b = map(chatqwen14b,  as_tibble)
  ) %>%
  tidyr::unnest_wider(chatgpt, names_sep = "_") %>%
  tidyr::unnest_wider(claude,  names_sep = "_") %>%
  tidyr::unnest_wider(gemini,  names_sep = "_")%>%
  tidyr::unnest_wider(chatqwen14b,  names_sep = "_")


#Save results
write_xlsx(classified_wide, "Results/Pubmed LLM 2025-11-16.xlsx")
write_xlsx(pubmed_df, "Results/Pubmed 100 abstract 2025-11-16.xlsx")



#Analyze results
##############################################
# 0. Setup
##############################################


library(tidyverse)
library(readxl)
library(janitor)
library(gtsummary)
library(irr)
library(psych)
library(gt)
library(webshot2)

# Nicer gtsummary defaults
theme_gtsummary_language("en")
theme_gtsummary_compact()

##############################################
# 1. Data set setup
##############################################

# Adjust path if needed
df_raw <- classified_wide

# Define the models you care about (must match prefixes in the column names)
model_prefixes <- c("chatgpt", "claude", "gemini", "chatqwen14b")

##############################################
# 2. Reshape to one row per (pmid, model)
##############################################

logic_cols <- df_raw |> 
  select(
    starts_with("chatgpt_"),
    starts_with("claude_"),
    starts_with("gemini_"),
    starts_with("chatqwen14b_")
  ) |> 
  select(where(is.logical)) |> 
  names()

# Long format: one row per pmid × model × variable
df_long <- df_raw |>
  select(pmid, all_of(logic_cols)) |>
  pivot_longer(
    cols = -pmid,
    names_to = c("model", "variable"),
    names_pattern = "^([^_]+)_(.*)$",
    values_to = "value"
  ) |>
  mutate(
    model = factor(model, levels = model_prefixes)
  )

# Wide-by-variable, with one row per pmid × model
df_model <- df_long |>
  tidyr::pivot_wider(
    id_cols   = c(pmid, model),
    names_from  = variable,
    values_from = value
  )

# Check that logicals look right
df_model |>
  select(model, where(is.logical)) |>
  glimpse()

##############################################
# 3. Descriptive summaries by model (gtsummary)
##############################################

# Use only logical (TRUE/FALSE) outputs for the main summary table
df_logic_only <- df_model |>
  select(model, where(is.logical))

# Main table: prevalence of TRUE for each variable, stratified by model
tbl_llm_prevalence <-
  df_logic_only |>
  tbl_summary(
    by = model,
    type      = all_dichotomous() ~ "categorical",
    statistic = all_dichotomous() ~ "{p}% ({n})",
    digits    = all_dichotomous() ~ c(1, 0)
  ) |>
  add_overall(last = TRUE) |>
  modify_header(
    label ~ "LLM output variable"
  ) |>
  modify_caption("**Prevalence of TRUE for each classifier output, by model**")

tbl_llm_prevalence

# gt object for exporting:
gt_llm_prevalence <- as_gt(tbl_llm_prevalence)
gtsave(gt_llm_prevalence, "Results/llm_prevalence_table.png")
gtsave(gt_llm_prevalence, "Results/llm_prevalence_table.docx")


##############################################
# 4. Define which boolean variables to use for agreement
##############################################

# Choose a subset of key binary variables to focus on
binary_suffixes <- c(
  # Core ID flag
  "is_id_related",
  
  # Organs
  "is_heart_transplant_related",
  "is_lung_transplant_related",
  "is_kidney_transplant_related",
  "is_liver_transplant_related",
  "is_pancreas_transplant_related",
  "is_multiorgan_related",
  
  # Pathogen classes
  "is_virus_related",
  "is_bacteria_related",
  "is_fungal_related",
  "is_parasite_related",
  
  # Disparities
  "addresses_health_disparities",
  "disparity_racial",
  "disparity_ethnic",
  "disparity_socioeconomic",
  "disparity_geographic",
  "disparity_insurance",
  "disparity_gender",
  
  # A few other clinically interesting flags
  "discusses_immunosuppression",
  "discusses_pediatric_population",
  "discusses_living_donor_transplant",
  "discusses_deceased_donor_transplant",
  "discusses_donor_quality",
  "discusses_policy",
  "discusses_waitlist_access",
  "discusses_diabetes",
  "discusses_cardiovascular_disease",
  "discusses_malignancy",
  "discusses_obesity"
)

# Helper to get the matrix for a given variable suffix
get_var_matrix <- function(data, suffix, model_prefixes) {
  cols <- paste0(model_prefixes, "_", suffix)
  # Keep only columns that actually exist
  cols <- intersect(cols, names(data))
  if (length(cols) < 2L) {
    return(NULL)
  }
  data |>
    select(all_of(cols)) |>
    # Keep only complete cases for kappas
    drop_na()
}

##############################################
# 5. Pairwise agreement (percent & Cohen's κ)
##############################################

pairwise_kappa_one_var <- function(data, suffix, model_prefixes) {
  mat <- get_var_matrix(data, suffix, model_prefixes)
  if (is.null(mat) || nrow(mat) == 0) return(tibble())
  
  out <- list()
  for (i in 1:(ncol(mat) - 1)) {
    for (j in (i + 1):ncol(mat)) {
      col_i <- mat[[i]]
      col_j <- mat[[j]]
      m1    <- names(mat)[i]
      m2    <- names(mat)[j]
      
      # Percent agreement
      pct_agree <- mean(col_i == col_j) * 100
      
      # Cohen's kappa
      k2 <- irr::kappa2(cbind(col_i, col_j))
      
      conf_low  <- ifelse(!is.null(k2$conf.int), k2$conf.int[1], NA_real_)
      conf_high <- ifelse(!is.null(k2$conf.int), k2$conf.int[2], NA_real_)
      
      out[[length(out) + 1]] <- tibble(
        variable      = suffix,
        model1        = sub("_.*", "", m1),
        model2        = sub("_.*", "", m2),
        n             = nrow(mat),
        pct_agreement = pct_agree,
        kappa         = k2$value,
        kappa_low     = conf_low,
        kappa_high    = conf_high
      )
    }
  }
  bind_rows(out)
}


pairwise_kappas <-
  map_dfr(binary_suffixes, ~pairwise_kappa_one_var(df_raw, .x, model_prefixes))

pairwise_kappas

write_xlsx(pairwise_kappas, "Results/pairwise_kappas.xlsx")

# Example: a gtsummary table for kappa values for is_id_related only

tbl_kappa_id_related <-
  pairwise_kappas |>
  filter(variable == "is_id_related") |>
  mutate(pair = paste(model1, "vs", model2)) |>
  select(pair, n, pct_agreement, kappa, kappa_low, kappa_high) |>
  gt() |>
  fmt_number(
    columns = c(pct_agreement, kappa, kappa_low, kappa_high),
    decimals = 3
  ) |>
  tab_header(
    title = md("**Pairwise agreement and Cohen's κ for is_id_related**")
  )

tbl_kappa_id_related

##############################################
# 6. Multi-rater agreement (Fleiss’ κ)
##############################################

fleiss_kappa_one_var <- function(data, suffix, model_prefixes) {
  mat <- get_var_matrix(data, suffix, model_prefixes)
  if (is.null(mat) || nrow(mat) == 0) return(tibble())
  k <- irr::kappam.fleiss(as.matrix(mat))
  tibble(
    variable     = suffix,
    n            = nrow(mat),
    fleiss_kappa = k$value
  )
}

fleiss_results <-
  map_dfr(binary_suffixes, ~fleiss_kappa_one_var(df_raw, .x, model_prefixes))

fleiss_results
write_xlsx(fleiss_results, "Results/fleiss_results.xlsx")

tbl_fleiss <-
  fleiss_results |>
  tbl_summary(
    type = fleiss_kappa ~ "continuous",
    statistic = fleiss_kappa ~ "{mean}",
    digits = fleiss_kappa ~ 4
  ) |>
  modify_header(label ~ "Variable") |>
  modify_caption("**Fleiss’ κ (multi-rater agreement) across all models**")

tbl_fleiss

##############################################
# 7. Inter-model distance & clustering
##############################################

# Build a per-abstract binary matrix for one key variable, e.g. is_id_related
mat_id <- get_var_matrix(df_raw, "is_id_related", model_prefixes)

if (!is.null(mat_id)) {
  # Distances between models based on disagreement rate
  # (1 - proportion agreement)
  dist_mat <- matrix(0, ncol = ncol(mat_id), nrow = ncol(mat_id))
  colnames(dist_mat) <- rownames(dist_mat) <- names(mat_id)
  
  for (i in 1:ncol(mat_id)) {
    for (j in 1:ncol(mat_id)) {
      dist_mat[i, j] <- 1 - mean(mat_id[[i]] == mat_id[[j]])
    }
  }
  
  hc <- hclust(as.dist(dist_mat), method = "average")
  plot(hc, main = "Clustering of LLMs based on is_id_related")
}

##############################################
# 8. Extract disagreement cases
##############################################

# Example: abstracts where at least one model differs on is_id_related
get_disagreements <- function(data, suffix, model_prefixes) {
  cols <- paste0(model_prefixes, "_", suffix)
  cols <- intersect(cols, names(data))
  if (length(cols) < 2L) return(tibble())
  
  data |>
    select(pmid, title, abstract, all_of(cols)) |>
    rowwise() |>
    mutate(
      n_true   = sum(c_across(all_of(cols))),
      n_false  = length(cols) - n_true,
      disagree = (n_true > 0 & n_false > 0)
    ) |>
    ungroup() |>
    filter(disagree)
}

disagree_id_related <- get_disagreements(df_raw, "is_id_related", model_prefixes)
disagree_id_related