# ──────────────────────────────────────────────────────────────────────────────
# State similarity by POLICY OPTIONS, derived from the QC data itself
# (option B of the transferability designs; fire-rate cosine is option A).
#
# Per state and ERA (2017-19 / 2022-24 separately - policies and their
# error consequences drift), a policy vector of de facto option measures:
#   reporting system mix (rep_sys: change / quarterly / simplified shares)
#   standard medical deduction demo (med_ded_demo)
#   SSI CAP participation (ssi_cap)
#   categorical eligibility (any + expanded/BBCE-style, cat_elig)
#   heating-SUA code mix (sua1 top codes)
#   certification periods: median + long-cert shares (overall and among
#     elderly/disabled - the latter is an ESAP proxy)
#   deduction-usage shares: child support, homeless, medical (among eld/dis)
#   expedited share, TANF share, pure-PA share, ABAWD share
#
# Deriving from QC keeps the vector era-matched and current; the FNS State
# Options Report serves as an external validation, not an input.
#
# Similarity: cosine on z-scored columns. Outputs -> methods/state_similarity_v2/
# (era-suffixed): policy_vector_by_state_<era>.csv, similarity_policy_<era>.csv,
# similarity_blended_<era>.csv (mean of policy cosine and the era's fire-rate
# sqrt cosine from similarity_matrix_sqrt_<era>.csv).
# Expects `reg_model_data`.
# ──────────────────────────────────────────────────────────────────────────────

suppressMessages(library(dplyr))

ERAS <- list("2017_2019" = c("2017", "2018", "2019"),
             "2022_2024" = c("2022", "2023", "2024"))
out_dir <- "methods/state_similarity_v2"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

num <- function(x) suppressWarnings(as.numeric(as.character(x)))
cos_sim <- function(a) {
  n <- a / sqrt(rowSums(a^2))
  tcrossprod(n)
}

for (era_name in names(ERAS)) {
  d <- reg_model_data %>% filter(fiscal_year %in% ERAS[[era_name]])
  d$cert_len <- num(d$months_certification_period_n)
  cat(sprintf("\n──── era %s: %d rows ────\n", era_name, nrow(d)))

  pv <- d %>%
    group_by(state) %>%
    summarise(
      n_cases            = n(),
      rep_change         = mean(num(rep_sys) == 1, na.rm = TRUE),
      rep_quarterly      = mean(num(rep_sys) == 3, na.rm = TRUE),
      rep_simplified     = mean(num(rep_sys) == 4, na.rm = TRUE),
      smd_demo           = mean(num(med_ded_demo) == 1, na.rm = TRUE),
      ssi_cap_share      = mean(num(ssi_cap) > 0, na.rm = TRUE),
      catelig_any        = mean(num(cat_elig) >= 1, na.rm = TRUE),
      catelig_expanded   = mean(num(cat_elig) >= 2, na.rm = TRUE),
      sua_code1          = mean(num(sua1) == 1, na.rm = TRUE),
      sua_code3          = mean(num(sua1) == 3, na.rm = TRUE),
      sua_code4          = mean(num(sua1) == 4, na.rm = TRUE),
      cert_median        = median(cert_len, na.rm = TRUE),
      cert_ge24          = mean(cert_len >= 24, na.rm = TRUE),
      cert_ge24_elddis   = mean(cert_len[elderly_disabled_i == 1] >= 24, na.rm = TRUE),
      csded_share        = mean(num(fscsded) > 0, na.rm = TRUE),
      homeless_ded_share = mean(num(homeless_ded) > 0, na.rm = TRUE),
      medded_share_ed    = mean(num(fsmedded)[elderly_disabled_i == 1] > 0, na.rm = TRUE),
      expedited_share    = mean(expedited_i == 1, na.rm = TRUE),
      tanf_share         = mean(num(tanf_ind) == 1, na.rm = TRUE),
      pure_pa_share      = mean(num(pure_pa) == 1, na.rm = TRUE),
      abawd_share        = mean(num(percent_abawd), na.rm = TRUE),
      .groups = "drop"
    )
  write.csv(pv, file.path(out_dir,
            sprintf("policy_vector_by_state_%s.csv", era_name)), row.names = FALSE)
  cat(sprintf("policy vector: %d states x %d measures\n", nrow(pv), ncol(pv) - 2))

  m <- as.matrix(pv %>% select(-state, -n_cases))
  rownames(m) <- pv$state
  m <- scale(m)
  m[is.na(m)] <- 0                     # degenerate (zero-variance) columns
  sim_pol <- cos_sim(m)
  write.csv(round(sim_pol, 4),
            file.path(out_dir, sprintf("similarity_policy_%s.csv", era_name)))

  fire_path <- file.path(out_dir, sprintf("similarity_matrix_sqrt_%s.csv", era_name))
  if (file.exists(fire_path)) {
    sim_fire <- as.matrix(read.csv(fire_path, row.names = 1, check.names = FALSE))
    colnames(sim_fire) <- rownames(sim_fire)  # guard against header name-mangling
    common <- intersect(rownames(sim_pol), rownames(sim_fire))
    blended <- (sim_pol[common, common] + sim_fire[common, common]) / 2
    write.csv(round(blended, 4),
              file.path(out_dir, sprintf("similarity_blended_%s.csv", era_name)))
    cat(sprintf("blended matrix over %d common states\n", length(common)))
  } else {
    cat("fire matrix for era not found - run state_similarity_fire_rates_v2.R first\n")
  }

  top_of <- function(S, st, k = 8) {
    v <- sort(S[st, setdiff(colnames(S), st)], decreasing = TRUE)[1:k]
    paste(sprintf("%s (%.3f)", names(v), v), collapse = ", ")
  }
  for (st in c("Louisiana", "Washington", "Virginia")) {
    if (st %in% rownames(sim_pol))
      cat(sprintf("%s policy neighbors: %s\n", st, top_of(sim_pol, st)))
  }
}
