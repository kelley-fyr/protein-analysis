neuroonc_initialize <- function(
  clear_env = TRUE,
  save = TRUE,
  thr_current_value = 0.90,
  input_dir_value  = "C:/Users/KelleyVanVaerenbergh/FYR Diagnostics/FYR-Research Lab - Documents/Kelley/Rdir/NeuroOnc/",
  output_dir_value = "C:/Users/KelleyVanVaerenbergh/FYR Diagnostics/FYR-Research Lab - Documents/Kelley/Rdir/NeuroOnc/sandbox/20251003",
  analysis_date_value = "20251003"
) {
  if (isTRUE(clear_env)) {
    keep <- c("neuroonc_initialize")
    rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)
  }

  suppressPackageStartupMessages({
    library(ggplot2)
    library(tidyverse)
    library(ggrepel)
    library(ggbeeswarm)
    library(scales)
    library(grid)
    library(ggtext)
    library(stringr)
  })

  assign_global <- function(name, value) assign(name, value, envir = .GlobalEnv)

  assign_global(".has_gridExtra", requireNamespace("gridExtra", quietly = TRUE))
  assign_global(".has_ggtext",    requireNamespace("ggtext",    quietly = TRUE))

  assign_global("input_dir",  input_dir_value)
  assign_global("output_dir", output_dir_value)
  assign_global("name",       "NeuroOnc")
  assign_global("date",       analysis_date_value)

  assign_global("thr_current", thr_current_value)
  assign_global("SAVE",        save)

  assign_global("table_thresholds", c(0.00, 0.50, 0.75, 0.90, 1.00))
  assign_global("thr_levels",       c(">0%", ">=50%", ">=75%", ">=90%", "=100%"))

  pal_list <- list(
    purple = "#7400B8",
    aqua   = "#80FFDB",
    cyan   = "#48BFE3",
    tp     = "#4361EE",
    i12    = "#F72585",
    black  = "#000000",
    white  = "#FFFFFF",
    greypt = "grey45"
  )
  assign_global("pal", pal_list)

  title_color_for_fn <- function(st_label) {
    pal <- get("pal", envir = .GlobalEnv)
    if (st_label == "SPARCTP") pal$tp else if (st_label == "SPARC12") pal$i12 else pal$black
  }
  assign_global("title_color_for", title_color_for_fn)

  fmt_st_bold_fn <- function(st_label) {
    if (isTRUE(get(".has_ggtext", envir = .GlobalEnv))) paste0("<b>", st_label, "</b>") else st_label
  }
  assign_global("fmt_st_bold", fmt_st_bold_fn)

  title_elem_fn <- function(colour) {
    if (isTRUE(get(".has_ggtext", envir = .GlobalEnv))) ggtext::element_markdown(hjust = 0.5, face = "bold", colour = colour)
    else element_text(hjust = 0.5, face = "bold", colour = colour)
  }
  assign_global("title_elem", title_elem_fn)

  subtitle_elem_fn <- function(colour) {
    if (isTRUE(get(".has_ggtext", envir = .GlobalEnv))) ggtext::element_markdown(hjust = 0.5, colour = colour)
    else element_text(hjust = 0.5, colour = colour)
  }
  assign_global("subtitle_elem", subtitle_elem_fn)

  threshold_label_fn <- function(x) {
    if (abs(x - 0) < 1e-12) return(">0%")
    if (abs(x - 1) < 1e-12) return("=100%")
    paste0(">=", as.integer(round(x * 100)), "%")
  }
  assign_global("threshold_label", threshold_label_fn)

  thr_for_subtitle_fn <- function(pretty_thr) gsub("^>=|^>|^=", "", pretty_thr)
  assign_global("thr_for_subtitle", thr_for_subtitle_fn)

  assign_global("sanitize",   function(s) gsub("[^A-Za-z0-9]+", "_", s))
  assign_global("cap_inches", function(x, max_in = 49.5) pmin(x, max_in))

  ggsave_if_fn <- function(filename, plot, width, height, dpi = 300, bg = "white") {
    if (isTRUE(get("SAVE", envir = .GlobalEnv))) {
      ggsave(filename, plot, width = width, height = height, dpi = dpi, bg = bg)
      message("Saved: ", filename)
    } else {
      message("SAVE = FALSE (preview only): ", filename)
    }
  }
  assign_global("ggsave_if", ggsave_if_fn)

  safe_write_csv_fn <- function(x, file) {
    tryCatch(
      {
        readr::write_csv(x, file)
      },
      error = function(e) {
        warning(
          "readr::write_csv failed for ", file,
          " - retrying with utils::write.csv. Error: ", conditionMessage(e)
        )
        utils::write.csv(x, file, row.names = FALSE)
      }
    )
  }
  assign_global("safe_write_csv", safe_write_csv_fn)

  passes_threshold_fn <- function(x, thr = get("thr_current", envir = .GlobalEnv)) {
    if (abs(thr) < 1e-12) x > 0 else x >= thr
  }
  assign_global("passes_threshold", passes_threshold_fn)

  save_plotdata_fn <- function(df, stem, prefix = paste0(get("name", envir = .GlobalEnv), "_", get("date", envir = .GlobalEnv), "_")) {
    if (!isTRUE(get("SAVE", envir = .GlobalEnv))) return(invisible(NULL))
    fn <- paste0(prefix, stem, ".csv")
    get("safe_write_csv", envir = .GlobalEnv)(df, fn)
    message("Saved: ", fn)
    invisible(fn)
  }
  assign_global("save_plotdata", save_plotdata_fn)

  thr_pretty_value      <- threshold_label_fn(thr_current_value)
  thr_label_short_value <- paste0("thr", ifelse(thr_current_value == 0, "gt0", as.integer(round(100 * thr_current_value))))
  thr_short_value       <- thr_for_subtitle_fn(thr_pretty_value)
  thr_file_tag_value    <- if (abs(thr_current_value) < 1e-12) "thr_gt0" else paste0("thr_", as.integer(round(thr_current_value * 100)), "pct")

  assign_global("thr_pretty",      thr_pretty_value)
  assign_global("thr_label_short", thr_label_short_value)
  assign_global("thr_short",       thr_short_value)
  assign_global("thr_file_tag",    thr_file_tag_value)

  section_filename_fn <- function(section, stem, ext = NULL) {
    name_val <- get("name", envir = .GlobalEnv)
    date_val <- get("date", envir = .GlobalEnv)
    section_tag <- toupper(as.character(section))
    if (!startsWith(section_tag, "S")) section_tag <- paste0("S", section_tag)
    base <- paste(name_val, date_val, section_tag, stem, sep = "_")
    if (length(ext) == 0 || is.null(ext) || identical(ext, "")) base else paste0(base, ".", ext)
  }
  assign_global("section_filename", section_filename_fn)

  assign_global("disease_fill_map", c(
    Healthy = pal_list$aqua,
    Disease = pal_list$purple,
    Benign  = "grey65",
    Control = "grey85"
  ))

  assign_global("sparc_types",   c("SPARCTP", "SPARC12"))
  assign_global("context_types", c("SPARCTP", "SPARC12", "Pooled", "Intersection"))

  assign_global("genes_of_interest", c(
    "MMP9","COMP","CHMP1B","CD9",
    "TCP1","CDC42","MMP8","ANXA7",
    "YWHAE","GSN","STOML3","PTPRF",
    "AVIL","VCP","ITGAM","SLC4A1"
  ))
}
neuroonc_initialize()

                
# I/O and data load
run_section0 <- function(save = SAVE) {
  old_save <- SAVE
  on.exit(assign("SAVE", old_save, envir = .GlobalEnv))
  assign("SAVE", save, envir = .GlobalEnv)
  evalq({
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    setwd(input_dir)
    df_raw <- read.csv("AllProteins_MSraw_NeuroOnc.csv", check.names = FALSE)
    ev     <- read.csv("EVmarkers.csv",           check.names = FALSE)
    walt  <- read.csv("Walt_EV_list.csv",     check.names = FALSE)
    setwd(output_dir)

    # Base DF
    # - Preserve original Disease; add DiseaseType (pool Benign/Healthy only when *counting* samples later)
    # - Add log2_abundance
    # - Rename SampleType: IEVTP -> SPARCTP ; IEV12 -> SPARC12
    df <- df_raw %>%
      mutate(
        RawAbundance   = suppressWarnings(as.numeric(RawAbundance)),
        log2_abundance = if_else(RawAbundance > 0, log2(RawAbundance), NA_real_),
        DiseaseType    = case_when(
          Disease == "Brain Cancer" ~ "Disease",
          Disease == "Brain Tumor"  ~ "Benign",
          Disease == "Healthy"      ~ "Healthy",
          Disease == "Control"      ~ "Control",
          TRUE                      ~ as.character(Disease)
        ),
        SampleType = case_when(
          SampleType %in% c("IEVTP","SPARCTP") ~ "SPARCTP",
          SampleType %in% c("IEV12","SPARC12") ~ "SPARC12",
          TRUE ~ as.character(SampleType)
        )
      )

    # Coverage scaffolding (HD-only samples used for coverage/observed sets)
    df_cov <- df %>% filter(DiseaseType %in% c("Healthy","Disease"))

    # Samples per SampleType (HD-only)
    n_per_type <- df_cov %>%
      distinct(SampleType, SampleName) %>%
      count(SampleType, name = "n_samples")
    n_per_type

    # Presence counts per gene & SampleType (HD-only)
    present_by_type <- df_cov %>%
      group_by(SampleType, gene_symbol) %>%
      summarise(
        n_present = n_distinct(SampleName[!is.na(RawAbundance) & RawAbundance > 0]),
        .groups = "drop"
      )

    # Coverage per SampleType (HD-only)
    presence_by_type <- present_by_type %>%
      tidyr::complete(
        SampleType  = unique(df_cov$SampleType),
        gene_symbol = unique(df_cov$gene_symbol),
        fill = list(n_present = 0)
      ) %>%
      left_join(n_per_type, by = "SampleType") %>%
      mutate(
        coverage     = if_else(n_samples > 0, n_present / n_samples, NA_real_),
        low_presence = coverage < 0.10
      )

    # Export per-type presence table (HD-only)
    readr::write_csv(presence_by_type, section_filename(0, "presence_by_type", "csv"))

    # Combined (pooled) coverage across the same subset (HD-only)
    n_samples_total <- n_distinct(df_cov$SampleName)
    gene_universe   <- unique(df_cov$gene_symbol)

    presence_overall <- df_cov %>%
      group_by(gene_symbol) %>%
      summarise(
        n_present = n_distinct(SampleName[!is.na(RawAbundance) & RawAbundance > 0]),
        .groups = "drop"
      ) %>%
      tidyr::complete(gene_symbol = gene_universe, fill = list(n_present = 0)) %>%
      mutate(n_samples = n_samples_total, coverage = if_else(n_samples > 0, n_present / n_samples, NA_real_))

    # Observed sets (HD-only)
    observed_by_type <- df_cov %>%
      group_by(SampleType) %>%
      summarise(observed_genes = list(unique(gene_symbol)), .groups = "drop")

    observed_overall <- unique(df_cov$gene_symbol)

    # EV genes
    ev_genes <- if ("geneSymbol" %in% names(ev)) unique(ev$geneSymbol) else character(0)

    # Walt EV list (metadata-aware)
    walt_tbl <- tibble::as_tibble(walt)
    if (!"Gene" %in% names(walt_tbl)) {
      stop("Walt_EV_list.csv must contain a 'Gene' column.", call. = FALSE)
    }

    get_walt_col <- function(df, nm) {
      if (nm %in% names(df)) df[[nm]] else rep(NA_character_, nrow(df))
    }

    walt_metadata <- tibble::tibble(
      gene_symbol  = as.character(walt_tbl$Gene),
      WaltCellType = dplyr::na_if(as.character(get_walt_col(walt_tbl, "CellType")), ""),
      WaltLocation = dplyr::na_if(as.character(get_walt_col(walt_tbl, "Location")), ""),
      WaltDataset  = dplyr::na_if(as.character(get_walt_col(walt_tbl, "Dataset")), "")
    ) %>%
      dplyr::filter(!is.na(gene_symbol) & gene_symbol != "") %>%
      dplyr::distinct()

    walt_genes <- walt_metadata$gene_symbol
    genes_in_dataset <- unique(df$gene_symbol)
    walt_missing <- setdiff(walt_genes, genes_in_dataset)
    if (length(walt_missing)) {
      note_sample <- paste(utils::head(walt_missing, 5), collapse = ", ")
      message(
        "Walt EV genes not found in data: ",
        note_sample,
        if (length(walt_missing) > 5) " ..." else ""
      )
    }

    n_celltypes <- walt_metadata %>%
      dplyr::filter(!is.na(WaltCellType)) %>%
      dplyr::summarise(n = dplyr::n_distinct(WaltCellType)) %>%
      dplyr::pull(n)
    if (!length(n_celltypes)) n_celltypes <- 0L

    n_locations <- walt_metadata %>%
      dplyr::filter(!is.na(WaltLocation)) %>%
      dplyr::summarise(n = dplyr::n_distinct(WaltLocation)) %>%
      dplyr::pull(n)
    if (!length(n_locations)) n_locations <- 0L

    walt_present <- sum(walt_genes %in% genes_in_dataset)
    ev_present   <- sum(ev_genes %in% genes_in_dataset)

    message(
      "Walt EV genes loaded: ", length(walt_genes),
      " | Cell types: ", n_celltypes,
      " | Locations: ", n_locations
    )
    message(
      "Walt EV genes present in dataset: ", walt_present, " / ", length(walt_genes)
    )
    message(
      "EV markers present in dataset: ", ev_present, " / ", length(ev_genes)
    )

    # --- New: INTERSECTION coverage context (HD-only) 
    # Build per-gene coverage in SPARCTP and SPARC12, then take per-gene min() as "intersection coverage".
    # This lets a gene "pass" a threshold only if it passes in *both* types.
    cov_wide <- presence_by_type %>%
      select(SampleType, gene_symbol, coverage) %>%
      mutate(SampleType = factor(SampleType, levels = c("SPARCTP","SPARC12"))) %>%
      tidyr::pivot_wider(names_from = SampleType, values_from = coverage, values_fill = 0)

    presence_intersection <- cov_wide %>%
      transmute(
        gene_symbol,
        coverage = pmin(SPARCTP, SPARC12, na.rm = TRUE)  # min coverage across types
      )

    # For convenience: â€œBoth observedâ€ set (genes observed in both types, regardless of coverage value)
    obs_SP_TP  <- observed_by_type %>% filter(SampleType == "SPARCTP") %>% pull(observed_genes) %>% { if (length(.)==0) list(character(0)) else . } %>% `[[`(1)
    obs_SP_12  <- observed_by_type %>% filter(SampleType == "SPARC12") %>% pull(observed_genes) %>% { if (length(.)==0) list(character(0)) else . } %>% `[[`(1)
    observed_both <- intersect(obs_SP_TP, obs_SP_12)

    # --- New: PASS SETS at current threshold (HD-only) 
    # Helper predicate
    pass_SP_TP <- presence_by_type %>%
      filter(SampleType == "SPARCTP") %>%
      filter(passes_threshold(coverage, thr_current)) %>%
      pull(gene_symbol) %>% unique()

    pass_SP_12 <- presence_by_type %>%
      filter(SampleType == "SPARC12") %>%
      filter(passes_threshold(coverage, thr_current)) %>%
      pull(gene_symbol) %>% unique()

    pass_pooled <- presence_overall %>%
      filter(passes_threshold(coverage, thr_current)) %>%
      pull(gene_symbol) %>% unique()

    pass_intersection <- intersect(pass_SP_TP, pass_SP_12)

    # HD-only convenience frames and keep-sets
    df_hd <- df %>% dplyr::filter(DiseaseType %in% c("Healthy", "Disease"))

    df_hd_present <- df_hd %>%
      dplyr::filter(!is.na(RawAbundance) & RawAbundance > 0 & !is.na(log2_abundance)) %>%
      dplyr::mutate(
        Disease    = factor(DiseaseType, levels = c("Healthy", "Disease")),
        SampleType = factor(SampleType, levels = sparc_types)
      )

    keep_SPARCTP <- presence_by_type %>%
      dplyr::filter(SampleType == "SPARCTP") %>%
      dplyr::filter(passes_threshold(coverage, thr_current)) %>%
      dplyr::pull(gene_symbol) %>% unique()

    keep_SPARC12 <- presence_by_type %>%
      dplyr::filter(SampleType == "SPARC12") %>%
      dplyr::filter(passes_threshold(coverage, thr_current)) %>%
      dplyr::pull(gene_symbol) %>% unique()

    keep_POOLED <- presence_overall %>%
      dplyr::filter(passes_threshold(coverage, thr_current)) %>%
      dplyr::pull(gene_symbol) %>% unique()

    keep_sets <- list(
      SPARCTP      = keep_SPARCTP,
      SPARC12      = keep_SPARC12,
      Pooled       = keep_POOLED,
      Intersection = pass_intersection
    )  # convenience lookups by context


    # Quick peek
    message("Rows in df (all): ", nrow(df))
    message("Distinct genes (all): ", length(unique(df$gene_symbol)))
    message("EV markers loaded: ", length(ev_genes))
    message("HD-only coverage contexts ready: SPARCTP, SPARC12, Pooled, Intersection")
    message("At thr=", thr_current, " pass counts â€” SPARCTP: ", length(pass_SP_TP),
            " | SPARC12: ", length(pass_SP_12),
            " | Pooled: ", length(pass_pooled),
            " | Intersection: ", length(pass_intersection))

    # --- Objects used downstream (Sections 1+) 
    # Per-type coverage     : presence_by_type  (SampleType âˆˆ {SPARCTP, SPARC12}, HD-only)
    # Pooled coverage       : presence_overall  (HD-only)
    # Intersection coverage : presence_intersection (per-gene min across types, HD-only)
    # Observed sets         : observed_by_type, observed_overall, observed_both
    # EV list               : ev_genes

    # (Optional) Save convenience CSVs for reproducibility  [SAFE FILENAMES]
    if (isTRUE(SAVE)) {
      readr::write_csv(walt_metadata,         section_filename(0, "walt_metadata_by_gene", "csv"))
      readr::write_csv(presence_overall,      section_filename(0, "presence_pooled_HD", "csv"))
      readr::write_csv(presence_intersection, section_filename(0, "presence_intersection_HD", "csv"))

      readr::write_csv(
        tibble::tibble(gene_symbol = pass_intersection),
        section_filename(0, paste0(thr_file_tag, "_passset_INTERSECTION"), "csv")
      )
    }
  }, envir = .GlobalEnv)
  invisible(NULL)
}

run_section0()

#### SECTION 1 â€” Threshold summary table + retention plots (% retained only; SPARC & Pooled/Intersection) ----

run_section1 <- function(save = SAVE) {
  old_save <- SAVE
  on.exit(assign("SAVE", old_save, envir = .GlobalEnv))
  assign("SAVE", save, envir = .GlobalEnv)
  evalq({
    # What this does (updated):
    # 1) Builds a summary table across thresholds for SPARCTP, SPARC12, Pooled, and Intersection.
    #    â€œGenes Remainingâ€ = distinct genes whose coverage among Healthy + Disease samples
    #    is >0% (for >0%) or >= threshold (for >=X%) within that contextâ€™s observed set.
    #    (Pooled = all HD samples together; Intersection = genes observed in BOTH SPARCTP & SPARC12 with per-gene min coverage.)
    # 2) Saves the table as CSV and a cleaner PNG (wrapped headers, tight spacing).
    # 3) Retention plots (ONLY % retained):
    #    â€¢ Figure A: SPARCTP & SPARC12 (stacked facets with colored facet labels)
    #    â€¢ Figure B: Pooled & Intersection (stacked facets)
    #    Data labels show â€œ% (n)â€, where n = genes retained at that threshold (EV labels use retained EV count).
    #    Extra top headroom so labels donâ€™t clip.
    # 4) Exports CSVs of the exact plot data used.

    # Threshold label helpers (used for filenames/subtitles)
    # ---
    # Helper to enforce table types
    .ts_numeric_cols <- c(
      "Thr","Genes_Remaining","Percent_of_Total_Genes","EV_Markers_Remaining",
      "Percent_of_Observed_EVs","Fisher_OR","Fisher_pvalue"
    )
    .ts_char_cols <- c("Context","Threshold_Label")
    .coerce_summary_types <- function(x) {
      for (nm in c(.ts_char_cols, .ts_numeric_cols)) if (!nm %in% names(x)) x[[nm]] <- NA
      x %>%
        dplyr::mutate(
          dplyr::across(intersect(names(.), .ts_char_cols),    ~ as.character(.)),
          dplyr::across(intersect(names(.), .ts_numeric_cols), ~ suppressWarnings(as.numeric(.)))
        ) %>%
        dplyr::select(dplyr::all_of(c(.ts_char_cols, .ts_numeric_cols)))
    }

    # Build rows per context
    compute_context_rows <- function(context, coverage_df, observed_genes_vec, thr_vec) {
      if (is.null(observed_genes_vec) || length(observed_genes_vec) == 0) observed_genes_vec <- character(0)
      observed_genes_vec <- as.character(observed_genes_vec)

      coverage_obs <- coverage_df %>% dplyr::filter(gene_symbol %in% observed_genes_vec)
      total_genes  <- length(observed_genes_vec)

      ev_obs_set      <- intersect(observed_genes_vec, ev_genes)
      ev_total_obs    <- length(ev_obs_set)
      nonEV_total_obs <- max(total_genes - ev_total_obs, 0)

      out <- purrr::map_dfr(thr_vec, function(thr) {
        kept <- if (abs(thr) < 1e-12) dplyr::filter(coverage_obs, coverage > 0) else dplyr::filter(coverage_obs, coverage >= thr)
        n_kept  <- dplyr::n_distinct(kept$gene_symbol)
        ev_kept <- dplyr::n_distinct(kept$gene_symbol[kept$gene_symbol %in% ev_obs_set])

        pct_kept      <- if (total_genes  > 0) n_kept / total_genes  else NA_real_
        pct_ev_of_obs <- if (ev_total_obs > 0) ev_kept / ev_total_obs else NA_real_

        a <- ev_kept; c_ <- n_kept - ev_kept; b <- ev_total_obs - a; d <- nonEV_total_obs - c_
        a <- pmax(a, 0); b <- pmax(b, 0); c_ <- pmax(c_, 0); d <- pmax(d, 0)
        ft <- tryCatch(stats::fisher.test(matrix(c(a,b,c_,d), nrow = 2), alternative = "greater"),
                       error = function(e) NULL)

        tibble::tibble(
          Context                 = as.character(context),
          Thr                     = as.numeric(thr),
          Threshold_Label         = as.character(threshold_label(thr)),
          Genes_Remaining         = as.numeric(n_kept),
          Percent_of_Total_Genes  = as.numeric(pct_kept),
          EV_Markers_Remaining    = as.numeric(ev_kept),
          Percent_of_Observed_EVs = as.numeric(pct_ev_of_obs),
          Fisher_OR               = as.numeric(if (!is.null(ft)) unname(ft$estimate) else NA_real_),
          Fisher_pvalue           = as.numeric(if (!is.null(ft)) ft$p.value else NA_real_)
        )
      })
      .coerce_summary_types(out)
    }

    # Observed gene sets & coverage frames (prepared in Section 0)
    get_obs <- function(st) {
      v <- observed_by_type %>% dplyr::filter(SampleType == st) %>% dplyr::pull(observed_genes)
      if (is.null(v) || length(v) == 0) character(0) else as.character(v[[1]])
    }
    cov_TP   <- presence_by_type      %>% dplyr::filter(SampleType == "SPARCTP") %>% dplyr::select(gene_symbol, coverage)
    obs_TP   <- get_obs("SPARCTP")
    cov_12   <- presence_by_type      %>% dplyr::filter(SampleType == "SPARC12") %>% dplyr::select(gene_symbol, coverage)
    obs_12   <- get_obs("SPARC12")
    cov_all  <- presence_overall      %>% dplyr::select(gene_symbol, coverage)
    obs_all  <- as.character(observed_overall)
    cov_int  <- presence_intersection %>% dplyr::select(gene_symbol, coverage)
    obs_both <- intersect(obs_TP, obs_12)

    ts_TP   <- compute_context_rows("SPARCTP",      cov_TP,  obs_TP,  table_thresholds)
    ts_12   <- compute_context_rows("SPARC12",      cov_12,  obs_12,  table_thresholds)
    ts_pool <- compute_context_rows("Pooled",       cov_all, obs_all, table_thresholds)
    ts_int  <- compute_context_rows("Intersection", cov_int, obs_both, table_thresholds)

    threshold_summary_tbl <- dplyr::bind_rows(ts_TP, ts_12, ts_pool, ts_int) %>%
      dplyr::mutate(Threshold = factor(Threshold_Label, levels = thr_levels)) %>%
      dplyr::arrange(Context, Threshold)

    print(threshold_summary_tbl, n = 20)

    # Save CSV + cleaner PNG table
    if (isTRUE(SAVE)) {
      csv_file <- section_filename(1, "threshold_summary_table", "csv")
      readr::write_csv(threshold_summary_tbl, csv_file)
      message("Saved: ", csv_file)

      if (.has_gridExtra) {
        df_show_chr <- threshold_summary_tbl %>%
          dplyr::mutate(
            `Genes Remaining`       = format(Genes_Remaining, trim = TRUE),
            `% Total Genes`         = ifelse(is.na(Percent_of_Total_Genes), "",
                                             scales::percent(Percent_of_Total_Genes, accuracy = 0.1)),
            `EV Markers Remaining`  = format(EV_Markers_Remaining, trim = TRUE),
            `% Observed EVs`        = ifelse(is.na(Percent_of_Observed_EVs), "",
                                             scales::percent(Percent_of_Observed_EVs, accuracy = 0.1)),
            `Fisher OR`             = ifelse(is.na(Fisher_OR), "NA", sprintf("%.2f", Fisher_OR)),
            `Fisher p`              = ifelse(is.na(Fisher_pvalue), "NA",
                                             formatC(Fisher_pvalue, format = "e", digits = 2))
          ) %>%
          dplyr::select(Context, Threshold,
                        `Genes Remaining`, `% Total Genes`,
                        `EV Markers Remaining`, `% Observed EVs`,
                        `Fisher OR`, `Fisher p`)

        hdr <- names(df_show_chr)
        names(df_show_chr) <- stringr::str_replace_all(stringr::str_wrap(hdr, width = 14), "\\n", "\n")

        tg <- gridExtra::tableGrob(
          df_show_chr, rows = NULL,
          theme = gridExtra::ttheme_minimal(
            base_size = 10,
            core    = list(fg_params = list(hjust = 1, x = 0.98)),
            colhead = list(fg_params = list(fontface = 2, hjust = 0.5))
          )
        )

        title_grob <- grid::textGrob("Threshold summary table",
                                     gp = grid::gpar(fontface = "bold", cex = 1.2))
        cap_text <- stringr::str_wrap(
          "â€œGenes Remainingâ€ counts distinct gene symbols whose coverage (fraction of non-zero observations among Healthy + Disease samples) meets the specified threshold within each contextâ€™s observed gene set. EV counts are within EV markers observed for that context.",
          width = 90
        )
        caption_grob <- grid::textGrob(cap_text, x = 0.5, just = "center", gp = grid::gpar(cex = 0.85))

        padded <- gridExtra::arrangeGrob(title_grob, tg, caption_grob,
                                         ncol = 1,
                                         heights = grid::unit.c(grid::unit(1.2, "lines"),
                                                                grid::unit(1, "null"),
                                                                grid::unit(1.6, "lines")))
        png_filename <- section_filename(1, "threshold_summary_table", "png")
        png(png_filename, width = 2200, height = 1500, res = 200)
        grid::grid.draw(padded); dev.off()
        message("Saved: ", png_filename)
      }
    }

    # ---
    # Retention plots â€” % retained ONLY (with â€œ% (n)â€ labels and colored SPARC facet labels)
    # ---
    df_long <- threshold_summary_tbl %>%
      dplyr::transmute(
        Context,
        ThrNum   = as.numeric(Thr) * 100,
        Genes    = as.numeric(Genes_Remaining),
        EVs      = as.numeric(EV_Markers_Remaining),
        PctGenes = as.numeric(Percent_of_Total_Genes),
        PctEVs   = as.numeric(Percent_of_Observed_EVs)
      ) %>%
      tidyr::pivot_longer(c(Genes, EVs, PctGenes, PctEVs),
                          names_to = "Metric", values_to = "Value") %>%
      dplyr::mutate(Group = ifelse(grepl("^Pct", Metric), "Pct", "Count"),
                    Type  = ifelse(grepl("Genes", Metric), "Genes", "EV Markers")) %>%
      dplyr::select(-Metric) %>%
      tidyr::pivot_wider(names_from = Group, values_from = Value) %>%
      dplyr::mutate(
        Pct   = ifelse(is.finite(Pct), Pct, NA_real_),
        label = ifelse(is.na(Pct), "NA",
                       paste0(scales::percent(Pct, accuracy = 0.1),
                              " (", format(Count, big.mark = ","), ")")),
        label_vjust = ifelse(Type == "Genes" & ThrNum == 0, 1.2, -0.6),
        label_x     = ifelse(ThrNum == 0, ThrNum + 2, ThrNum)
      )

    df_long_sparc <- df_long %>% dplyr::filter(Context %in% c("SPARCTP","SPARC12"))
    df_long_ctx   <- df_long %>% dplyr::filter(Context %in% c("Pooled","Intersection"))

    # Colored facet labels for SPARC contexts (if ggtext available)
    .make_retention_facet_labs_sparc <- function(thr) {
      pass_fun <- function(x) if (abs(thr) < 1e-12) x > 0 else x >= thr
      by_st <- presence_by_type %>%
        dplyr::group_by(SampleType) %>%
        dplyr::summarise(
          genes_pass = dplyr::n_distinct(gene_symbol[pass_fun(coverage)]),
          ev_pass    = dplyr::n_distinct(gene_symbol[gene_symbol %in% ev_genes & pass_fun(coverage)]),
          .groups = "drop"
        )
      cols <- c(SPARCTP = pal$tp, SPARC12 = pal$i12)
      labs <- paste0(by_st$SampleType, " â€” pass: genes ", by_st$genes_pass,
                     ", EVs ", by_st$ev_pass, " (", thr_pretty, ")")
      if (.has_ggtext) {
        labs <- paste0("<span style='color:", cols[by_st$SampleType], "'><b>",
                       by_st$SampleType, "</b></span> â€” pass: genes ", by_st$genes_pass,
                       ", EVs ", by_st$ev_pass, " (", thr_pretty, ")")
      }
      setNames(labs, by_st$SampleType)
    }
    facet_labs_pct_sparc <- .make_retention_facet_labs_sparc(thr_current)

    # Simple labels for Pooled/Intersection
    facet_labs_pct_ctx <- {
      tibble::tibble(
        Context    = c("Pooled","Intersection"),
        genes_pass = c(
          sum(df_long_ctx$Count[df_long_ctx$Context == "Pooled" & df_long_ctx$Type == "Genes" & df_long_ctx$ThrNum == thr_current*100][1], na.rm=TRUE),
          sum(df_long_ctx$Count[df_long_ctx$Context == "Intersection" & df_long_ctx$Type == "Genes" & df_long_ctx$ThrNum == thr_current*100][1], na.rm=TRUE)
        ),
        ev_pass    = c(
          sum(df_long_ctx$Count[df_long_ctx$Context == "Pooled" & df_long_ctx$Type == "EV Markers" & df_long_ctx$ThrNum == thr_current*100][1], na.rm=TRUE),
          sum(df_long_ctx$Count[df_long_ctx$Context == "Intersection" & df_long_ctx$Type == "EV Markers" & df_long_ctx$ThrNum == thr_current*100][1], na.rm=TRUE)
        )
      ) %>%
        dplyr::mutate(lbl = paste0(Context, " â€” pass: genes ", genes_pass,
                                   ", EVs ", ev_pass, " (", thr_pretty, ")")) %>%
        { setNames(.$lbl, .$Context) }
    }

    # A) % retained â€” SPARCTP & SPARC12 (stacked facets, colored facet labels)
    p_retention_pct_sparc <- ggplot(df_long_sparc, aes(x = ThrNum, y = Pct, color = Type, linetype = Type, shape = Type)) +
      geom_line() +
      geom_point(size = 2.4) +
      geom_text(aes(x = label_x, label = label, vjust = label_vjust),
                size = 3.2, show.legend = FALSE) +
      facet_wrap(~ Context, ncol = 1, labeller = labeller(Context = facet_labs_pct_sparc)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_x_continuous(limits = c(0, 100),
                         breaks = c(0, 50, 75, 90, 100),
                         labels = c("0%", "50%", "75%", "90%", "100%")) +
      scale_color_manual(values = c("Genes" = "grey40", "EV Markers" = pal$cyan)) +
      coord_cartesian(ylim = c(0, 1.08), clip = "off") +  # extra headroom to avoid clipping labels
      labs(
        title    = "Retention across thresholds â€” SPARCTP & SPARC12 (% retained)",
        subtitle = "% retained at each coverage threshold; labels = % (n)",
        x = "Coverage threshold (%)",
        y = "% retained",
        caption = stringr::str_wrap(
          "Genes: % of total observed genes retained. EV markers: % of observed EV markers retained. Observed sets computed among Healthy + Disease.",
          width = 90
        )
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text    = if (.has_ggtext) ggtext::element_markdown(size = 12, face = "bold")
        else element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.title  = element_blank(),
        legend.position = "top",
        plot.caption  = element_text(hjust = 0.5),
        plot.margin   = margin(t = 16, r = 14, b = 10, l = 10)
      )

    print(p_retention_pct_sparc)
    ggsave_if(
      filename = section_filename(1, paste0(thr_label_short, "_retention_PERCENT_SPARC"), "png"),
      plot     = p_retention_pct_sparc,
      width    = 7.6, height = 9.8, dpi = 300, bg = "white"
    )

    # B) % retained â€” Pooled & Intersection (stacked facets)
    p_retention_pct_ctx <- ggplot(df_long_ctx, aes(x = ThrNum, y = Pct, color = Type, linetype = Type, shape = Type)) +
      geom_line() +
      geom_point(size = 2.4) +
      geom_text(aes(x = label_x, label = label, vjust = label_vjust),
                size = 3.2, show.legend = FALSE) +
      facet_wrap(~ Context, ncol = 1, labeller = labeller(Context = facet_labs_pct_ctx)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_x_continuous(limits = c(0, 100),
                         breaks = c(0, 50, 75, 90, 100),
                         labels = c("0%", "50%", "75%", "90%", "100%")) +
      scale_color_manual(values = c("Genes" = "grey40", "EV Markers" = pal$cyan)) +
      coord_cartesian(ylim = c(0, 1.08), clip = "off") +
      labs(
        title    = "Retention across thresholds â€” Pooled & Intersection (% retained)",
        subtitle = "% retained at each coverage threshold; labels = % (n)",
        x = "Coverage threshold (%)",
        y = "% retained",
        caption = stringr::str_wrap(
          "Genes: % of total observed genes retained. EV markers: % of observed EV markers retained. Observed sets computed among Healthy + Disease.",
          width = 90
        )
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text    = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.title  = element_blank(),
        legend.position = "top",
        plot.caption  = element_text(hjust = 0.5),
        plot.margin   = margin(t = 16, r = 14, b = 10, l = 10)
      )

    print(p_retention_pct_ctx)
    ggsave_if(
      filename = section_filename(1, paste0(thr_label_short, "_retention_PERCENT_POOLED_INTERSECTION"), "png"),
      plot     = p_retention_pct_ctx,
      width    = 7.6, height = 9.6, dpi = 300, bg = "white"
    )

    # Save the exact plot data used
    if (isTRUE(SAVE)) {
      readr::write_csv(df_long_sparc, section_filename(1, paste0(thr_label_short, "_retention_plotdata_SPARC"), "csv"))
      readr::write_csv(df_long_ctx,   section_filename(1, paste0(thr_label_short, "_retention_plotdata_POOLED_INTERSECTION"), "csv"))
    }





  }, envir = .GlobalEnv)
  invisible(NULL)
}
run_section1()


#### SECTION 2 â€” Coverage histograms + ECDF + Coverage-bin tables (with concise subtitles & per-context table headers) ----

run_section2 <- function(save = SAVE) {
  old_save <- SAVE
  on.exit(assign("SAVE", old_save, envir = .GlobalEnv))
  assign("SAVE", save, envir = .GlobalEnv)
  evalq({
    # What this update changes:
    # 1) Histograms:
    #    â€¢ Subtitles now report dataset size only, e.g., "(N) total genes" or "(N) total EV markers".
    #    â€¢ No longer repeat caption text in the subtitle.
    #
    # 2) ECDF plots (overlay: grey = all proteins, cyan = EV markers):
    #    â€¢ Subtitles shortened to only: "(N) total genes; (N) EV markers" for each context
    #      (SPARCTP, SPARC12, and the stacked Pooled & Intersection figure).
    #
    # 3) Coverage-bin tables (10% bins):
    #    â€¢ Export FOUR separate CSVs and PNGs â€” one for each context: SPARCTP, SPARC12, Pooled, Intersection.
    #    â€¢ Each table PNG now includes a clear header above the table indicating the context.
    #    â€¢ Additionally, export a 2Ã—2 panel PNG that arranges the four tables together with per-table headers and a shared title/caption.
    #
    # Notes:
    # â€¢ Coverage computed from Healthy + Disease only (Control/Benign excluded).
    # â€¢ â€œIntersectionâ€ coverage = per-gene min(coverage_SPARCTP, coverage_SPARC12).
    # â€¢ All plots also export the exact data used to draw them as CSVs (bins / ECDF steps).

    # --- Short labels and helpers 
    thr_vline       <- ifelse(thr_current == 0, 1e-6, thr_current)  # avoid exactly 0 on vline

    # Base frames (Healthy + Disease only; built in Section 0)
    # - presence_by_type: per-gene coverage by SampleType âˆˆ {SPARCTP, SPARC12}
    # - presence_overall: pooled per-gene coverage
    # - presence_intersection: per-gene min(coverage_SPARCTP, coverage_SPARC12)
    # - ev_genes: character()
    presence_base_st  <- presence_by_type %>% mutate(is_EV = gene_symbol %in% ev_genes)
    presence_pooled   <- presence_overall   %>% mutate(is_EV = gene_symbol %in% ev_genes)
    presence_intersec <- presence_intersection %>% mutate(is_EV = gene_symbol %in% ev_genes)

    # Totals (for subtitles and labels)
    N_st_all <- presence_base_st %>% count(SampleType, name = "N") %>% tibble::deframe()   # rows per SPARC (genes within that SPARC)
    N_st_ev  <- presence_base_st %>% filter(is_EV) %>% count(SampleType, name = "N") %>% tibble::deframe()
    N_pool_all <- nrow(presence_pooled)
    N_pool_ev  <- sum(presence_pooled$is_EV, na.rm = TRUE)
    N_int_all  <- nrow(presence_intersec)
    N_int_ev   <- sum(presence_intersec$is_EV, na.rm = TRUE)

    # Also keep overall unique totals for SPARC-paired hist subtitles
    N_overall_genes <- length(observed_overall)
    N_overall_evs   <- length(intersect(observed_overall, ev_genes))

    # Pass counts (robust)
    pc_all_st <- presence_base_st %>%
      group_by(SampleType) %>%
      summarise(pass = n_distinct(gene_symbol[passes_threshold(coverage, thr_current)]), .groups = "drop")

    pc_ev_st <- presence_base_st %>%
      filter(is_EV) %>%
      group_by(SampleType) %>%
      summarise(pass = n_distinct(gene_symbol[passes_threshold(coverage, thr_current)]), .groups = "drop")

    pass_pooled_all <- sum(passes_threshold(presence_pooled$coverage, thr_current), na.rm = TRUE)
    pass_pooled_ev  <- sum(presence_pooled$is_EV & passes_threshold(presence_pooled$coverage, thr_current), na.rm = TRUE)

    pass_int_all <- sum(passes_threshold(presence_intersec$coverage, thr_current), na.rm = TRUE)
    pass_int_ev  <- sum(presence_intersec$is_EV & passes_threshold(presence_intersec$coverage, thr_current), na.rm = TRUE)

    # Facet labels with pass counts
    .make_strip_labs_sparc <- function(df_counts, label_suffix) {
      cols <- c(SPARCTP = pal$tp, SPARC12 = pal$i12)
      s <- df_counts$SampleType
      p <- df_counts$pass
      lbl <- paste0(s, " - ", p, " pass ", label_suffix)
      if (.has_ggtext) {
        lbl <- paste0("<span style='color:", cols[s], "'><b>", s,
                      "</b></span> - ", p, " pass ", label_suffix)
      }
      setNames(lbl, s)
    }
    .make_strip_labs_ctx <- function(df_counts_named, label_suffix) {
      labs <- paste0(names(df_counts_named), " - ", as.integer(df_counts_named), " pass ", label_suffix)
      setNames(labs, names(df_counts_named))
    }
    facet_labs_all <- .make_strip_labs_sparc(pc_all_st, thr_pretty)
    facet_labs_ev  <- .make_strip_labs_sparc(pc_ev_st,  thr_pretty)

    # CAPTIONS (wrapped width 95)
    cap_hist_common <- stringr::str_wrap(
      paste0("Bin width = 5%. Vertical dashed line marks threshold ", thr_pretty, ". Healthy + Disease only; Control & Benign excluded."),
      width = 95
    )
    cap_ecdf_common <- stringr::str_wrap(
      paste0("Grey = all proteins, cyan = EV markers. Vertical dashed line marks threshold ", thr_pretty, ". Healthy + Disease only."),
      width = 95
    )

    # --- Helpers to generate and SAVE plot-data CSVs 
    # 5% bin labeler that mirrors geom_histogram(binwidth=0.05, boundary=0, closed='right')
    .bin5_label <- function(x) {
      x <- pmax(pmin(x, 1), 0)         # clamp
      idx <- pmin(floor(x / 0.05), 19) # 0..19 (100% -> 19)
      start <- idx * 5
      end   <- start + 5
      end[idx == 19] <- 100
      levs <- paste0(seq(0, 95, by = 5), "â€“", c(seq(5, 95, by = 5), 100), "%")
      factor(paste0(start, "â€“", end, "%"), levels = levs, ordered = TRUE)
    }
    # 10% bin labeler (for tables)
    .bin10_label <- function(x) {
      pct <- floor(pmax(pmin(x, 1), 0) * 100 + 1e-8)
      start <- (pct %/% 10) * 10
      start[pct == 100] <- 90
      end <- start + 9
      end[start == 90] <- 100
      levs <- paste0(seq(0, 90, by = 10), "â€“", c(seq(9, 89, by = 10), 100), "%")
      factor(paste0(start, "â€“", end, "%"), levels = levs, ordered = TRUE)
    }
    # ===
    # A) HISTOGRAMS â€” SPARCTP & SPARC12 (ALL proteins; EV-only) with concise subtitles
    # ===
    # Plot data (ALL proteins)
    hist_bins_st_all <- presence_base_st %>%
      mutate(bin = .bin5_label(coverage)) %>%
      count(SampleType, bin, name = "n") %>%
      group_by(SampleType) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup()
    save_plotdata(hist_bins_st_all, paste0(thr_label_short, "_histbins_5pct_SPARC_ALLproteins"), prefix = paste0(name, "_", date, "_S2_"))

    # Plot data (EV-only)
    hist_bins_st_evs <- presence_base_st %>%
      filter(is_EV) %>%
      mutate(bin = .bin5_label(coverage)) %>%
      count(SampleType, bin, name = "n") %>%
      group_by(SampleType) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup()
    save_plotdata(hist_bins_st_evs, paste0(thr_label_short, "_histbins_5pct_SPARC_EVonly"), prefix = paste0(name, "_", date, "_S2_"))

    # Histograms â€” ALL proteins (faceted)
    hist_all <- ggplot(presence_base_st, aes(x = coverage)) +
      geom_histogram(binwidth = 0.05, boundary = 0, closed = "right",
                     color = "grey30", fill = "grey75") +
      geom_vline(xintercept = thr_vline, linetype = "dashed") +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                         limits = c(0, 1), expand = expansion(mult = c(0, 0.01))) +
      labs(
        title    = "Protein coverage across samples",
        subtitle = paste0(format(N_overall_genes, big.mark=","), " total genes"),
        x = "Coverage (fraction of samples with protein present)",
        y = "Count of proteins",
        caption = cap_hist_common
      ) +
      facet_wrap(~ SampleType, ncol = 1, labeller = labeller(SampleType = facet_labs_all)) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text    = if (.has_ggtext) ggtext::element_markdown(size = 12, face = "bold")
        else element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank(),
        plot.caption  = element_text(hjust = 0.5)
      )
    print(hist_all)
    if (isTRUE(SAVE)) {
      ggsave_if(section_filename("2", paste0(thr_label_short, "_hist_coverage_ALL_SPARC_faceted"), "png"),
                hist_all, width = 8.6, height = 9.8, dpi = 300, bg = "white")
    }

    # Histograms â€” EV markers only (faceted)
    presence_ev_only_st <- presence_base_st %>% filter(is_EV)
    hist_ev <- ggplot(presence_ev_only_st, aes(x = coverage)) +
      geom_histogram(binwidth = 0.05, boundary = 0, closed = "right",
                     color = "grey30", fill = pal$cyan) +
      geom_vline(xintercept = thr_vline, linetype = "dashed") +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                         limits = c(0, 1), expand = expansion(mult = c(0, 0.01))) +
      labs(
        title    = "Protein coverage across samples",
        subtitle = paste0(format(N_overall_evs, big.mark=","), " total EV markers"),
        x = "Coverage (fraction of samples with protein present)",
        y = "Count of EV markers",
        caption = cap_hist_common
      ) +
      facet_wrap(~ SampleType, ncol = 1, labeller = labeller(SampleType = facet_labs_ev)) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text    = if (.has_ggtext) ggtext::element_markdown(size = 12, face = "bold")
        else element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank(),
        plot.caption  = element_text(hjust = 0.5)
      )
    print(hist_ev)
    if (isTRUE(SAVE)) {
      ggsave_if(section_filename("2", paste0(thr_label_short, "_hist_coverage_EVONLY_SPARC_faceted"), "png"),
                hist_ev, width = 8.6, height = 9.8, dpi = 300, bg = "white")
    }

    # ===
    # B) HISTOGRAMS â€” Pooled & Intersection (stacked facets) with concise subtitles
    # ===
    # Combined plot data (ALL proteins)
    hist_bins_pool_int_all <- bind_rows(
      presence_pooled   %>% mutate(Context = "Pooled"),
      presence_intersec %>% mutate(Context = "Intersection")
    ) %>%
      mutate(bin = .bin5_label(coverage)) %>%
      count(Context, bin, name = "n") %>%
      group_by(Context) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup()
    save_plotdata(hist_bins_pool_int_all, paste0(thr_label_short, "_histbins_5pct_POOLED_INTERSECTION_ALLproteins"), prefix = paste0(name, "_", date, "_S2_"))

    # Combined plot data (EV-only)
    hist_bins_pool_int_evs <- bind_rows(
      presence_pooled   %>% filter(is_EV) %>% mutate(Context = "Pooled"),
      presence_intersec %>% filter(is_EV) %>% mutate(Context = "Intersection")
    ) %>%
      mutate(bin = .bin5_label(coverage)) %>%
      count(Context, bin, name = "n") %>%
      group_by(Context) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup()
    save_plotdata(hist_bins_pool_int_evs, paste0(thr_label_short, "_histbins_5pct_POOLED_INTERSECTION_EVonly"), prefix = paste0(name, "_", date, "_S2_"))

    # Facet labels for contexts
    labs_ctx_all <- .make_strip_labs_ctx(
      c(Pooled = pass_pooled_all, Intersection = pass_int_all), thr_pretty
    )
    labs_ctx_ev  <- .make_strip_labs_ctx(
      c(Pooled = pass_pooled_ev,  Intersection = pass_int_ev),  thr_pretty
    )

    # Stacked figure â€” ALL proteins
    hist_pool_int_all <- ggplot(bind_rows(
      presence_pooled   %>% mutate(Context = "Pooled"),
      presence_intersec %>% mutate(Context = "Intersection")
    ),
    aes(x = coverage)
    ) +
      geom_histogram(binwidth = 0.05, boundary = 0, closed = "right",
                     color = "grey30", fill = "grey75") +
      geom_vline(xintercept = thr_vline, linetype = "dashed") +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                         limits = c(0, 1), expand = expansion(mult = c(0, 0.01))) +
      labs(
        title    = "Protein coverage â€” Pooled & Intersection",
        subtitle = paste0(format(N_int_all, big.mark=","), " total genes"),
        x = "Coverage (fraction of samples with protein present)",
        y = "Count of proteins",
        caption = cap_hist_common
      ) +
      facet_wrap(~ Context, ncol = 1, labeller = labeller(Context = labs_ctx_all)) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text    = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank(),
        plot.caption  = element_text(hjust = 0.5)
      )
    print(hist_pool_int_all)
    if (isTRUE(SAVE)) {
      ggsave_if(section_filename("2", paste0(thr_label_short, "_hist_coverage_ALL_POOLED_INTERSECTION_STACKED"), "png"),
                hist_pool_int_all, width = 7.6, height = 9.6, dpi = 300, bg = "white")
    }

    # Stacked figure â€” EV-only
    hist_pool_int_ev <- ggplot(bind_rows(
      presence_pooled   %>% filter(is_EV) %>% mutate(Context = "Pooled"),
      presence_intersec %>% filter(is_EV) %>% mutate(Context = "Intersection")
    ),
    aes(x = coverage)
    ) +
      geom_histogram(binwidth = 0.05, boundary = 0, closed = "right",
                     color = "grey30", fill = pal$cyan) +
      geom_vline(xintercept = thr_vline, linetype = "dashed") +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                         limits = c(0, 1), expand = expansion(mult = c(0, 0.01))) +
      labs(
        title    = "Protein coverage â€” Pooled & Intersection",
        subtitle = paste0(format(N_int_ev, big.mark=","), " total EV markers"),
        x = "Coverage (fraction of samples with protein present)",
        y = "Count of EV markers",
        caption = cap_hist_common
      ) +
      facet_wrap(~ Context, ncol = 1, labeller = labeller(Context = labs_ctx_ev)) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text    = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank(),
        plot.caption  = element_text(hjust = 0.5)
      )
    print(hist_pool_int_ev)
    if (isTRUE(SAVE)) {
      ggsave_if(section_filename("2", paste0(thr_label_short, "_hist_coverage_EVONLY_POOLED_INTERSECTION_STACKED"), "png"),
                hist_pool_int_ev, width = 7.6, height = 9.6, dpi = 300, bg = "white")
    }

    # ===
    # C) ECDF (overlay) â€” SPARCTP & SPARC12 (stacked) + Pooled & Intersection (stacked)
    # ===
    .build_ecdf_steps <- function(df_cover, group_var = NULL) {
      if (!is.null(group_var)) {
        group_sym <- rlang::sym(group_var)
        df_cover %>%
          group_by(!!group_sym) %>%
          arrange(coverage, .by_group = TRUE) %>%
          transmute(
            !!group_sym := !!group_sym,
            x = coverage,
            y = dplyr::row_number() / dplyr::n()
          ) %>%
          ungroup()
      } else {
        df_cover %>%
          arrange(coverage) %>%
          mutate(y = seq_along(coverage) / dplyr::n()) %>%
          transmute(x = coverage, y = y)
      }
    }

    # SPARC types ECDF steps
    ecdf_all_st <- .build_ecdf_steps(presence_by_type, "SampleType")
    ecdf_ev_st  <- .build_ecdf_steps(presence_by_type %>% filter(gene_symbol %in% ev_genes), "SampleType")

    # Save ECDF plot data
    save_plotdata(ecdf_all_st, paste0(thr_label_short, "_ECDFsteps_ALLproteins_SPARC"), prefix = paste0(name, "_", date, "_S2_"))
    save_plotdata(ecdf_ev_st,  paste0(thr_label_short, "_ECDFsteps_EVonly_SPARC"), prefix = paste0(name, "_", date, "_S2_"))

    # Short ECDF subtitles per your spec
    subtitle_ecdf_sparc <- paste0(format(N_int_all, big.mark=","), " total genes; ", format(N_int_ev, big.mark=","), " total EV markers")

    # Pass-count facet labels for SPARC ECDF
    .make_ecdf_facet_labs_sparc <- function(thr) {
      pass_fun <- function(x) if (abs(thr) < 1e-12) x > 0 else x >= thr
      by_st <- presence_by_type %>%
        group_by(SampleType) %>%
        summarise(
          genes_pass = n_distinct(gene_symbol[pass_fun(coverage)]),
          ev_pass    = n_distinct(gene_symbol[gene_symbol %in% ev_genes & pass_fun(coverage)]),
          .groups = "drop"
        )
      cols <- c(SPARCTP = pal$tp, SPARC12 = pal$i12)
      labs <- paste0(by_st$SampleType, " â€” pass: genes ", by_st$genes_pass,
                     ", EVs ", by_st$ev_pass, " (", thr_pretty, ")")
      if (.has_ggtext) {
        labs <- paste0("<span style='color:", cols[by_st$SampleType], "'><b>",
                       by_st$SampleType, "</b></span> â€” pass: genes ", by_st$genes_pass,
                       ", EVs ", by_st$ev_pass, " (", thr_pretty, ")")
      }
      setNames(labs, by_st$SampleType)
    }
    facet_labs_ecdf_sparc <- .make_ecdf_facet_labs_sparc(thr_current)

    ecdf_plot_st <- ggplot() +
      geom_step(data = ecdf_all_st, aes(x = x, y = y, group = SampleType), color = "grey45") +
      geom_step(data = ecdf_ev_st,  aes(x = x, y = y, group = SampleType), color = pal$cyan, linewidth = 1.05) +
      geom_vline(xintercept = thr_vline, linetype = "dashed") +
      scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title    = "ECDF of protein coverage by SPARC type",
        subtitle = subtitle_ecdf_sparc,
        x = "Coverage (fraction of samples with protein present)",
        y = "Cumulative fraction of proteins",
        caption = cap_ecdf_common
      ) +
      facet_wrap(~ SampleType, ncol = 1, labeller = labeller(SampleType = facet_labs_ecdf_sparc)) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text    = if (.has_ggtext) ggtext::element_markdown(size = 12, face = "bold")
        else element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.caption  = element_text(hjust = 0.5)
      )
    print(ecdf_plot_st)
    if (isTRUE(SAVE)) {
      ggsave_if(section_filename("2", paste0(thr_label_short, "_ECDF_coverage_OVERLAY_SPARC_faceted"), "png"),
                ecdf_plot_st, width = 8.6, height = 9.8, dpi = 300, bg = "white")
    }

    # Pooled & Intersection ECDF steps + stacked plot
    ecdf_pool_all <- .build_ecdf_steps(presence_pooled)        %>% mutate(Context = "Pooled")
    ecdf_pool_ev  <- .build_ecdf_steps(presence_pooled %>% filter(is_EV)) %>% mutate(Context = "Pooled")
    ecdf_int_all  <- .build_ecdf_steps(presence_intersec)      %>% mutate(Context = "Intersection")
    ecdf_int_ev   <- .build_ecdf_steps(presence_intersec %>% filter(is_EV)) %>% mutate(Context = "Intersection")

    save_plotdata(bind_rows(ecdf_pool_all, ecdf_int_all), paste0(thr_label_short, "_ECDFsteps_ALLproteins_POOLED_INTERSECTION"), prefix = paste0(name, "_", date, "_S2_"))
    save_plotdata(bind_rows(ecdf_pool_ev,  ecdf_int_ev),  paste0(thr_label_short, "_ECDFsteps_EVonly_POOLED_INTERSECTION"), prefix = paste0(name, "_", date, "_S2_"))

    facet_labs_ecdf_ctx <- {
      tibble::tibble(
        Context    = c("Pooled","Intersection"),
        genes_pass = c(pass_pooled_all, pass_int_all),
        ev_pass    = c(pass_pooled_ev,  pass_int_ev)
      ) %>%
        mutate(lbl = paste0(Context, " â€” pass: genes ", genes_pass,
                            ", EVs ", ev_pass, " (", thr_pretty, ")")) %>%
        { setNames(.$lbl, .$Context) }
    }

    subtitle_ecdf_ctx <- paste0(format(N_int_all, big.mark=","), " total genes; ", format(N_int_ev, big.mark=","), " total EV markers")

    ecdf_plot_ctx <- ggplot() +
      geom_step(data = bind_rows(ecdf_pool_all, ecdf_int_all), aes(x = x, y = y, group = Context), color = "grey45") +
      geom_step(data = bind_rows(ecdf_pool_ev,  ecdf_int_ev),  aes(x = x, y = y, group = Context), color = pal$cyan, linewidth = 1.05) +
      geom_vline(xintercept = thr_vline, linetype = "dashed") +
      scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title    = "ECDF of protein coverage â€” Pooled & Intersection",
        subtitle = subtitle_ecdf_ctx,
        x = "Coverage (fraction of samples with protein present)",
        y = "Cumulative fraction of proteins",
        caption = cap_ecdf_common
      ) +
      facet_wrap(~ Context, ncol = 1, labeller = labeller(Context = facet_labs_ecdf_ctx)) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text    = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.caption  = element_text(hjust = 0.5)
      )
    print(ecdf_plot_ctx)
    if (isTRUE(SAVE)) {
      ggsave_if(section_filename("2", paste0(thr_label_short, "_ECDF_coverage_OVERLAY_POOLED_INTERSECTION_STACKED"), "png"),
                ecdf_plot_ctx, width = 7.6, height = 9.6, dpi = 300, bg = "white")
    }

    # ===
    # D) Coverage-bin tables (10% bins) â€” FOUR per-context CSVs + per-context PNGs + 2Ã—2 PNG panel
    # ===
    # Build bin labels
    presence_base_st  <- presence_base_st  %>% mutate(bin10 = .bin10_label(coverage))
    presence_pooled   <- presence_pooled   %>% mutate(bin10 = .bin10_label(coverage))
    presence_intersec <- presence_intersec %>% mutate(bin10 = .bin10_label(coverage))

    # Helper to make per-context table (data.frame) and return with its label
    make_bin_table_ctx <- function(df_ctx, ctx_label) {
      df1 <- df_ctx %>%
        group_by(bin10) %>%
        summarise(
          Genes_n = n(),
          EV_n    = sum(is_EV, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          total_genes = sum(Genes_n),
          total_evs   = sum(EV_n),
          Genes_pct   = ifelse(total_genes > 0, Genes_n / total_genes, NA_real_),
          EV_pct      = ifelse(total_evs   > 0, EV_n    / total_evs,   NA_real_)
        ) %>%
        select(Bin = bin10, Genes_n, Genes_pct, EV_n, EV_pct)
      list(ctx = ctx_label, tbl = df1)
    }

    # Build four tables
    tbl_SPARCTP <- make_bin_table_ctx(presence_base_st %>% filter(SampleType == "SPARCTP"), "SPARCTP")
    tbl_SPARC12 <- make_bin_table_ctx(presence_base_st %>% filter(SampleType == "SPARC12"), "SPARC12")
    tbl_Pooled  <- make_bin_table_ctx(presence_pooled, "Pooled")
    tbl_Inter   <- make_bin_table_ctx(presence_intersec, "Intersection")

    # Export CSVs
    if (isTRUE(SAVE)) {
      readr::write_csv(tbl_SPARCTP$tbl, section_filename("2", "coverage_bins_10pct_SPARCTP", "csv"))
      readr::write_csv(tbl_SPARC12$tbl, section_filename("2", "coverage_bins_10pct_SPARC12", "csv"))
      readr::write_csv(tbl_Pooled$tbl,  section_filename("2", "coverage_bins_10pct_POOLED", "csv"))
      readr::write_csv(tbl_Inter$tbl,   section_filename("2", "coverage_bins_10pct_INTERSECTION", "csv"))
    }

    # Per-context PNGs + 2Ã—2 panel
    if (isTRUE(SAVE) && .has_gridExtra) {
      wrap_headers <- function(nms, width = 16) vapply(nms, function(x)
        paste(strwrap(x, width = width), collapse = "\n"), character(1))

      prettify_tbl <- function(df) {
        df %>%
          mutate(
            Genes_pct = ifelse(is.na(Genes_pct), NA_character_, scales::percent(Genes_pct, accuracy = 0.1)),
            EV_pct    = ifelse(is.na(EV_pct),    NA_character_, scales::percent(EV_pct,    accuracy = 0.1))
          ) %>%
          { colnames(.) <- wrap_headers(colnames(.), width = 16); . }
      }

      caption_txt <- stringr::str_wrap(
        "Bins are based on coverage %. Coverage computed from Healthy + Disease samples only. Intersection means gene must exist in both SPARCTP & SPARC12 at the given threshold.",
        width = 95
      )

      make_table_grob <- function(df_tbl, ctx_label) {
        tbl_g <- gridExtra::tableGrob(prettify_tbl(df_tbl),
                                      rows = NULL,
                                      theme = gridExtra::ttheme_minimal(
                                        base_size = 10,
                                        core    = list(fg_params = list(hjust = 1, x = 0.98)),
                                        colhead = list(fg_params = list(fontface = 2, hjust = 0.5))
                                      ))
        gridExtra::arrangeGrob(tbl_g,
                               top    = grid::textGrob(ctx_label, gp = grid::gpar(fontface = "bold", cex = 1.05)),
                               bottom = grid::textGrob(caption_txt, gp = grid::gpar(cex = 0.9)))
      }

      # Individual PNGs (with per-table headers)
      grob_SPARCTP <- make_table_grob(tbl_SPARCTP$tbl, "Coverage bins (10%) â€” SPARCTP")
      grob_SPARC12 <- make_table_grob(tbl_SPARC12$tbl, "Coverage bins (10%) â€” SPARC12")
      grob_Pooled  <- make_table_grob(tbl_Pooled$tbl,  "Coverage bins (10%) â€” Pooled")
      grob_Inter   <- make_table_grob(tbl_Inter$tbl,   "Coverage bins (10%) â€” Intersection")

      png_file_stp <- section_filename("2", "coverage_bins_10pct_SPARCTP", "png")
      png(png_file_stp, width = 2300, height = 1300, res = 200); grid::grid.draw(grob_SPARCTP); dev.off(); message("Saved: ", png_file_stp)

      png_file_s12 <- section_filename("2", "coverage_bins_10pct_SPARC12", "png")
      png(png_file_s12, width = 2300, height = 1300, res = 200); grid::grid.draw(grob_SPARC12); dev.off(); message("Saved: ", png_file_s12)

      png_file_pool <- section_filename("2", "coverage_bins_10pct_POOLED", "png")
      png(png_file_pool, width = 2300, height = 1300, res = 200); grid::grid.draw(grob_Pooled); dev.off(); message("Saved: ", png_file_pool)

      png_file_int <- section_filename("2", "coverage_bins_10pct_INTERSECTION", "png")
      png(png_file_int, width = 2300, height = 1300, res = 200); grid::grid.draw(grob_Inter); dev.off(); message("Saved: ", png_file_int)

      # 2Ã—2 panel (reuse the per-table headers inside each cell)
      title_g <- grid::textGrob("Coverage bins (10%) â€” SPARCTP â€¢ SPARC12 â€¢ Pooled â€¢ Intersection",
                                gp = grid::gpar(fontface = "bold", cex = 1.15))
      grid_2x2 <- gridExtra::arrangeGrob(
        grob_SPARCTP, grob_SPARC12, grob_Pooled, grob_Inter,
        ncol = 2, top = title_g
      )
      png_file_panel <- section_filename("2", "coverage_bins_10pct_PANEL_2x2", "png")
      png(png_file_panel, width = 2600, height = 2000, res = 200); grid::grid.draw(grid_2x2); dev.off()
      message("Saved: ", png_file_panel)
    }



  }, envir = .GlobalEnv)
  invisible(NULL)
}
run_section2()


#### SECTION 3 â€” Proteins per sample (UNFILTERED and filtered at `thr_current`) [SPARC + exact plot-data CSVs] ----

run_section3 <- function(save = SAVE) {
  old_save <- SAVE
  on.exit(assign("SAVE", old_save, envir = .GlobalEnv))
  assign("SAVE", save, envir = .GlobalEnv)
  evalq({
    # What this does:
    # 1) Using only Healthy + Disease samples for coverage decisions (Control/Benign excluded for filtering; Benign pooled with Healthy in captions),
    #    compute per-sample counts of detected proteins and EV markers for SPARCTP and SPARC12.
    # 2) Make four violin plots (SPARCTP vs SPARC12 side-by-side), with sample points overlaid:
    #      a) UNFILTERED (>0%): All proteins
    #      b) UNFILTERED (>0%): EV markers only
    #      c) FILTERED (coverage >= `thr_current`): All proteins
    #      d) FILTERED (coverage >= `thr_current`): EV markers only
    #    Median line per violin. Points filled by DiseaseType (Benign pooled with Healthy in caption).
    # 3) Export the EXACT data used to draw each plot as CSVs, along with the per-sample count tables:
    #      - <name>_<date>_plotdata_violin_counts_AllGenes_UNFILTERED.csv
    #      - <name>_<date>_plotdata_violin_counts_EVMRK_UNFILTERED.csv
    #      - <name>_<date>_<thr_tag>_plotdata_violin_counts_AllGenes.csv
    #      - <name>_<date>_<thr_tag>_plotdata_violin_counts_EVMRK.csv
    #    where <thr_tag> is "thrgt0" for >0% or "thrXX" for threshold X%.
    # Notes:
    # â€¢ â€œPooledâ€ and â€œIntersectionâ€ are gene-level contexts and do not produce per-sample violins; Section 3 focuses on SPARCTP vs SPARC12.

    # Labels used in this section
    thr_label_short <- paste0("thr", ifelse(thr_current == 0, "gt0",
                                            as.integer(round(100 * thr_current))))
    thr_text_unf    <- threshold_label(0)   # ">0%"

    # --- Build / refresh per-sample tables 
    # UNFILTERED (>0% presence in raw â€” sample-level)
    if (!exists("per_sample_unf") || !is.data.frame(per_sample_unf)) {
      per_sample_unf <- df %>%
        dplyr::filter(!is.na(RawAbundance) & RawAbundance > 0) %>%
        dplyr::group_by(SampleName, SampleType, DiseaseType) %>%
        dplyr::summarise(
          n_genes = dplyr::n_distinct(gene_symbol),
          n_evs   = dplyr::n_distinct(gene_symbol[gene_symbol %in% ev_genes]),
          .groups = "drop"
        )
    }

    # FILTERED (keep only genes that meet the SPARC-specific coverage threshold among Healthy + Disease)
    if (!exists("per_sample_thr") || !is.data.frame(per_sample_thr)) {
      .keep_lookup <- presence_by_type %>%
        dplyr::filter(if (abs(thr_current) < 1e-12) coverage > 0 else coverage >= thr_current) %>%
        dplyr::select(SampleType, gene_symbol)

      per_sample_thr <- df %>%
        dplyr::filter(!is.na(RawAbundance) & RawAbundance > 0) %>%
        dplyr::semi_join(.keep_lookup, by = c("SampleType","gene_symbol")) %>%
        dplyr::group_by(SampleName, SampleType, DiseaseType) %>%
        dplyr::summarise(
          n_genes = dplyr::n_distinct(gene_symbol),
          n_evs   = dplyr::n_distinct(gene_symbol[gene_symbol %in% ev_genes]),
          .groups = "drop"
        )
    }

    # --- Helpers 
    # Compact caption: per-SampleType n & median + Wilcoxon p across SPARCTP vs SPARC12
    .build_caption_counts <- function(title_stub, df_counts, y_col, thr_txt) {
      d2 <- df_counts %>%
        dplyr::filter(SampleType %in% c("SPARCTP","SPARC12")) %>%
        dplyr::select(SampleType, y = .data[[y_col]])

      sums <- d2 %>%
        dplyr::group_by(SampleType) %>%
        dplyr::summarise(
          n_samples = dplyr::n(),
          median_y  = stats::median(y, na.rm = TRUE),
          .groups = "drop"
        )

      tp_med <- sums$median_y[sums$SampleType == "SPARCTP"]; tp_n <- sums$n_samples[sums$SampleType == "SPARCTP"]
      s12_med<- sums$median_y[sums$SampleType == "SPARC12"]; s12_n<- sums$n_samples[sums$SampleType == "SPARC12"]

      wt <- tryCatch({
        if (length(unique(d2$SampleType)) == 2) stats::wilcox.test(y ~ SampleType, data = d2, exact = FALSE) else NULL
      }, error = function(e) NULL)
      ptxt <- if (!is.null(wt)) paste0("Wilcoxon p=", signif(wt$p.value, 3)) else "Wilcoxon p=NA"

      paste0(
        title_stub, " | Filter: ", thr_txt, ". ",
        "SPARCTP â€” n=", ifelse(length(tp_n)==0, "NA", tp_n), ", median=", ifelse(length(tp_med)==0, "NA", round(tp_med, 1)), ". ",
        "SPARC12 â€” n=", ifelse(length(s12_n)==0, "NA", s12_n), ", median=", ifelse(length(s12_med)==0, "NA", round(s12_med, 1)), ". ",
        ptxt, ". Benign is pooled with Healthy."
      ) %>% stringr::str_wrap(width = 90)
    }

    # Shared plotting helper for count violins (side-by-side SPARCTP vs SPARC12)
    .make_violin_counts <- function(df_counts, y_col, ttl_main, ttl_sub, cap_text) {
      d <- df_counts %>%
        dplyr::filter(SampleType %in% c("SPARCTP","SPARC12")) %>%
        dplyr::mutate(SampleType = factor(SampleType, levels = c("SPARCTP","SPARC12")))

      # Color map for points (DiseaseType fill)
      disease_fills <- c(
        "Healthy" = pal$aqua,
        "Disease" = pal$purple,
        "Benign"  = "grey65",
        "Control" = "grey85"
      )

      # Median per SampleType (for line annotation)
      med_tbl <- d %>%
        dplyr::group_by(SampleType) %>%
        dplyr::summarise(med = stats::median(.data[[y_col]], na.rm = TRUE), .groups = "drop")

      ggplot(d, aes(x = SampleType, y = .data[[y_col]])) +
        geom_violin(fill = "grey88", color = "black", width = 0.9, trim = FALSE) +
        # median line (draw after violin so it sits on top)
        geom_segment(
          data = med_tbl,
          aes(x = as.numeric(SampleType) - 0.35, xend = as.numeric(SampleType) + 0.35,
              y = med, yend = med),
          inherit.aes = FALSE, linewidth = 1
        ) +
        ggbeeswarm::geom_quasirandom(
          aes(fill = DiseaseType),
          shape = 21, size = 2.6, stroke = 0.6, width = 0.18, alpha = 0.9, color = "black"
        ) +
        scale_fill_manual(values = disease_fills, drop = FALSE) +
        labs(
          title    = ttl_main,
          subtitle = ttl_sub,
          x = NULL, y = if (y_col == "n_genes") "Proteins per sample" else "EV markers per sample",
          caption  = cap_text
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title    = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "right",
          legend.title = element_blank(),
          plot.caption = element_text(hjust = 0.5),
          axis.text.x  = element_text(face = "bold")
        )
    }

    # ---------- 1) UNFILTERED â€” All genes 
    cap_genes_unf <- .build_caption_counts("Proteins per sample (All genes)", per_sample_unf, "n_genes", thr_text_unf)
    plotdata_genes_unf <- per_sample_unf %>%
      dplyr::filter(SampleType %in% c("SPARCTP","SPARC12")) %>%
      dplyr::select(SampleName, SampleType, DiseaseType, n_genes) %>%
      dplyr::rename(y = n_genes)
    save_plotdata(plotdata_genes_unf, "plotdata_violin_counts_AllGenes_UNFILTERED", prefix = paste0(name, "_", date, "_S3_"))

    p_genes_unf <- .make_violin_counts(
      per_sample_unf, "n_genes",
      ttl_main = "Proteins per sample â€” All genes",
      ttl_sub  = "UNFILTERED (>0%)",
      cap_text = cap_genes_unf
    )
    print(p_genes_unf)
    if (isTRUE(SAVE)) {
      file_genes_unf <- section_filename("3", "violin_counts_AllGenes_UNFILTERED", "png")
      ggsave_if(file_genes_unf, p_genes_unf, width = 7.8, height = 5.6, dpi = 300, bg = "white")
    }

    # ---------- 2) UNFILTERED â€” EV markers 
    cap_evs_unf <- .build_caption_counts("Proteins per sample (EV markers)", per_sample_unf, "n_evs", thr_text_unf)
    plotdata_evs_unf <- per_sample_unf %>%
      dplyr::filter(SampleType %in% c("SPARCTP","SPARC12")) %>%
      dplyr::select(SampleName, SampleType, DiseaseType, n_evs) %>%
      dplyr::rename(y = n_evs)
    save_plotdata(plotdata_evs_unf, "plotdata_violin_counts_EVMRK_UNFILTERED", prefix = paste0(name, "_", date, "_S3_"))

    p_evs_unf <- .make_violin_counts(
      per_sample_unf, "n_evs",
      ttl_main = "EV markers per sample",
      ttl_sub  = "UNFILTERED (>0%)",
      cap_text = cap_evs_unf
    )
    print(p_evs_unf)
    if (isTRUE(SAVE)) {
      file_evs_unf <- section_filename("3", "violin_counts_EVMRK_UNFILTERED", "png")
      ggsave_if(file_evs_unf, p_evs_unf, width = 7.8, height = 5.6, dpi = 300, bg = "white")
    }

    # ---------- 3) FILTERED â€” All genes 
    cap_genes_thr <- .build_caption_counts("Proteins per sample (All genes)", per_sample_thr, "n_genes", thr_pretty)
    plotdata_genes_thr <- per_sample_thr %>%
      dplyr::filter(SampleType %in% c("SPARCTP","SPARC12")) %>%
      dplyr::select(SampleName, SampleType, DiseaseType, n_genes) %>%
      dplyr::rename(y = n_genes)
    save_plotdata(plotdata_genes_thr, paste0(thr_label_short, "_plotdata_violin_counts_AllGenes"), prefix = paste0(name, "_", date, "_S3_"))

    p_genes_thr <- .make_violin_counts(
      per_sample_thr, "n_genes",
      ttl_main = "Proteins per sample â€” All genes",
      ttl_sub  = paste0("Filtered at ", thr_pretty),
      cap_text = cap_genes_thr
    )
    print(p_genes_thr)
    if (isTRUE(SAVE)) {
      file_genes_thr <- section_filename("3", paste0(thr_label_short, "_violin_counts_AllGenes"), "png")
      ggsave_if(file_genes_thr, p_genes_thr, width = 7.8, height = 5.6, dpi = 300, bg = "white")
    }

    # ---------- 4) FILTERED â€” EV markers 
    cap_evs_thr <- .build_caption_counts("Proteins per sample (EV markers)", per_sample_thr, "n_evs", thr_pretty)
    plotdata_evs_thr <- per_sample_thr %>%
      dplyr::filter(SampleType %in% c("SPARCTP","SPARC12")) %>%
      dplyr::select(SampleName, SampleType, DiseaseType, n_evs) %>%
      dplyr::rename(y = n_evs)
    save_plotdata(plotdata_evs_thr, paste0(thr_label_short, "_plotdata_violin_counts_EVMRK"), prefix = paste0(name, "_", date, "_S3_"))

    p_evs_thr <- .make_violin_counts(
      per_sample_thr, "n_evs",
      ttl_main = "EV markers per sample",
      ttl_sub  = paste0("Filtered at ", thr_pretty),
      cap_text = cap_evs_thr
    )
    print(p_evs_thr)
    if (isTRUE(SAVE)) {
      file_evs_thr <- section_filename("3", paste0(thr_label_short, "_violin_counts_EVMRK"), "png")
      ggsave_if(file_evs_thr, p_evs_thr, width = 7.8, height = 5.6, dpi = 300, bg = "white")
    }

    # ---------- Per-sample count CSVs (summary tables) 
    if (isTRUE(SAVE)) {
      csv_unf <- section_filename("3", "perSampleCounts_UNFILTERED", "csv")
      csv_thr <- section_filename("3", paste0(thr_label_short, "_perSampleCounts"), "csv")
      readr::write_csv(per_sample_unf, csv_unf)
      readr::write_csv(per_sample_thr, csv_thr)
      message("Saved: ", csv_unf)
      message("Saved: ", csv_thr)
    }


  }, envir = .GlobalEnv)
  invisible(NULL)
}
run_section3()


#### SECTION 3A â€” EV-marker retention across samples (violin) ----

run_section3A <- function(save = SAVE) {
  old_save <- SAVE
  on.exit(assign("SAVE", old_save, envir = .GlobalEnv))
  assign("SAVE", save, envir = .GlobalEnv)
  evalq({
    # Goal
    # â€¢ Show enrichment/retention visually after thresholding by coverage among Healthy + Disease (Control/Benign excluded for coverage).
    # â€¢ For EACH SAMPLE, compute:
    #     - % of its observed genes retained after threshold  (All genes)
    #     - % of its observed EV markers retained after threshold (EV markers only)
    #   â€œObservedâ€ = detected in that sample (>0 RawAbundance).
    # â€¢ Visualization (Violin): two violins (All genes vs EV markers), each point = one sample (y = % retained).
    # â€¢ Exports: wide per-sample retention table + long CSV

    # ---------- Ensure inputs from earlier sections exist 
    # per_sample_unf: per-sample counts UNFILTERED (>0%) with n_genes, n_evs
    # per_sample_thr: per-sample counts FILTERED (gene passes SPARC-specific coverage >= thr_current) with n_genes, n_evs
    stopifnot(exists("per_sample_unf"), exists("per_sample_thr"))

    # ---------- Build per-sample retention table (wide) 
    retention_wide <- per_sample_unf %>%
      dplyr::filter(SampleType %in% c("SPARCTP","SPARC12")) %>%
      dplyr::select(SampleName, SampleType, DiseaseType,
                    n_genes_unf = n_genes,
                    n_evs_unf   = n_evs) %>%
      dplyr::left_join(
        per_sample_thr %>%
          dplyr::select(SampleName, SampleType,
                        n_genes_thr = n_genes,
                        n_evs_thr   = n_evs),
        by = c("SampleName","SampleType")
      ) %>%
      dplyr::mutate(
        n_genes_thr = dplyr::coalesce(n_genes_thr, 0L),
        n_evs_thr   = dplyr::coalesce(n_evs_thr,   0L),
        # Derived â€œotherâ€ (non-EV) counts
        other_unf   = pmax(n_genes_unf - n_evs_unf, 0L),
        other_thr   = pmax(n_genes_thr - n_evs_thr, 0L),
        # % retained relative to OWN category denominators
        pct_genes_ret = dplyr::if_else(n_genes_unf > 0, n_genes_thr / n_genes_unf, NA_real_),
        pct_evs_ret   = dplyr::if_else(n_evs_unf   > 0, n_evs_thr   / n_evs_unf,   NA_real_),
        pct_other_ret = dplyr::if_else(other_unf   > 0, other_thr   / other_unf,   NA_real_),
        # % of TOTAL observed genes that are retained (used for stacked bars)
        pct_ev_of_total_ret    = dplyr::if_else(n_genes_unf > 0, n_evs_thr  / n_genes_unf, NA_real_),
        pct_other_of_total_ret = dplyr::if_else(n_genes_unf > 0, other_thr / n_genes_unf, NA_real_)
      )

    # Save wide table (master for this section)
    if (isTRUE(SAVE)) {
      fn_wide <- section_filename("3A", paste0(thr_label_short, "_perSample_retention_wide"), "csv")
      readr::write_csv(retention_wide, fn_wide)
      message("Saved: ", fn_wide)
    }

    # ---------- Plot 1: VIOLIN â€” % retained per sample (All vs EV markers) 
    plot_violin_df <- retention_wide %>%
      dplyr::select(SampleName, SampleType, DiseaseType,
                    pct_genes_ret, pct_evs_ret) %>%
      tidyr::pivot_longer(
        c(pct_genes_ret, pct_evs_ret),
        names_to = "Metric", values_to = "Pct"
      ) %>%
      dplyr::mutate(
        Metric = dplyr::recode(Metric,
                               pct_genes_ret = "All genes",
                               pct_evs_ret   = "EV markers"),
        Metric = factor(Metric, levels = c("All genes","EV markers"))
      )

    # Export exact plot data
    if (isTRUE(SAVE)) {
      fn_vdat <- section_filename("3A", paste0(thr_label_short, "_plotdata_EVretention_violin"), "csv")
      readr::write_csv(plot_violin_df, fn_vdat)
      message("Saved: ", fn_vdat)
    }

    # Aesthetics
    disease_fills <- c("Healthy" = pal$aqua, "Disease" = pal$purple, "Benign" = "grey65", "Control" = "grey85")

    cap_violin <- stringr::str_wrap(
      paste0("Each point is a sample. Y-axis is the fraction of that sampleâ€™s observed (unfiltered) entities retained after applying ",
             thr_pretty, " coverage among Healthy + Disease (Control/Benign excluded for coverage)."),
      width = 95
    )

    p_violin_ret <- ggplot(plot_violin_df %>% dplyr::filter(!is.na(Pct)),
                           aes(x = Metric, y = Pct)) +
      geom_violin(fill = "grey88", color = "black", width = 0.9, trim = FALSE) +
      # median lines per violin
      stat_summary(fun = median, geom = "crossbar", width = 0.55, linewidth = 0.8, color = "black") +
      ggbeeswarm::geom_quasirandom(
        aes(fill = DiseaseType), shape = 21, size = 2.4, stroke = 0.6,
        width = 0.18, alpha = 0.9, color = "black"
      ) +
      scale_fill_manual(values = disease_fills, drop = FALSE) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(
        title    = "Retention after thresholding (per sample)",
        subtitle = paste0("All genes vs EV markers â€” ", thr_pretty),
        x = NULL, y = "% retained (per sample)",
        caption  = cap_violin
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right",
        legend.title = element_blank(),
        plot.caption  = element_text(hjust = 0.5),
        axis.text.x   = element_text(face = "bold")
      )

    print(p_violin_ret)
    if (isTRUE(SAVE)) {
      fn_v <- section_filename("3A", paste0(thr_label_short, "_violin_EVretention"), "png")
      ggsave_if(fn_v, p_violin_ret, width = 7.2, height = 5.6, dpi = 300, bg = "white")
    }


  }, envir = .GlobalEnv)
  invisible(NULL)
}
run_section3A()


#### SECTION 4 â€” Gene-level log2 abundance (mean-based): EV markers vs Other genes by SPARC; UNFILTERED & FILTERED ----

run_section4 <- function(save = SAVE) {
  old_save <- SAVE
  on.exit(assign("SAVE", old_save, envir = .GlobalEnv))
  assign("SAVE", save, envir = .GlobalEnv)
  evalq({
    # Tweaks in this revision:
    # â€¢ Remove plot-level significance adornments: NO bracket line and NO â€œWelch p=â€¦â€ text.
    # â€¢ Keep the two-line group labels above each violin (â€œn = â€¦â€ / â€œmean = â€¦â€).
    # â€¢ Keep mean-centric summaries (mean_log2, sd_log2, cv_log2) and Welchâ€™s t-test in the CAPTION only.

    # ---- Local labels
    # ---- HD-only slice

    # ---- Helper: coverage pass function (same as earlier)
    # ---- Build keep-sets for FILTERED version (per SPARC)
    # ---- Core gene-level stats builder for a given SampleType and filter-mode
    # filter_mode âˆˆ {"UNFILTERED","FILTERED"}
    .build_gene_stats <- function(st, filter_mode = c("UNFILTERED","FILTERED")) {
      filter_mode <- match.arg(filter_mode)
      d_st <- df_hd %>% dplyr::filter(SampleType == st, !is.na(log2_abundance))

      # Total HD samples in this SampleType (denominator for coverage)
      n_samples_st <- d_st %>% dplyr::distinct(SampleName) %>% nrow()

      # Restrict to present>0 rows for gene-level summaries
      d_present <- d_st %>% dplyr::filter(!is.na(RawAbundance) & RawAbundance > 0)

      # Decide gene universe
      if (filter_mode == "UNFILTERED") {
        gene_set <- d_present %>% dplyr::distinct(gene_symbol) %>% dplyr::pull()
      } else {
        gene_set <- if (st == "SPARCTP") keep_SPARCTP else keep_SPARC12
      }
      if (length(gene_set) == 0) {
        return(tibble::tibble(
          SampleType = character(), gene_symbol = character(), is_EV = logical(),
          n_present = integer(), n_samples = integer(), coverage = numeric(),
          mean_log2 = numeric(), sd_log2 = numeric(), cv_log2 = numeric(), median_log2 = numeric(),
          Filter = character()
        ))
      }

      # Compute gene-level stats on the chosen gene_set
      stats_df <- d_present %>%
        dplyr::filter(gene_symbol %in% gene_set) %>%
        dplyr::group_by(gene_symbol) %>%
        dplyr::summarise(
          is_EV       = any(gene_symbol %in% ev_genes),
          n_present   = dplyr::n_distinct(SampleName),
          n_samples   = n_samples_st,
          coverage    = ifelse(n_samples > 0, n_present / n_samples, NA_real_),
          mean_log2   = suppressWarnings(mean(log2_abundance, na.rm = TRUE)),
          sd_log2     = suppressWarnings(stats::sd(log2_abundance, na.rm = TRUE)),
          median_log2 = suppressWarnings(stats::median(log2_abundance, na.rm = TRUE)),
          .groups     = "drop"
        ) %>%
        dplyr::mutate(
          cv_log2  = dplyr::if_else(is.finite(mean_log2) & abs(mean_log2) > 0,
                                    sd_log2 / abs(mean_log2), NA_real_),
          SampleType = st,
          Filter     = filter_mode,
          .before = 1
        )

      stats_df
    }

    # ---- Build stats tables for all combinations
    stats_STP_UNF <- .build_gene_stats("SPARCTP", "UNFILTERED")
    stats_STP_FIL <- .build_gene_stats("SPARCTP", "FILTERED")
    stats_S12_UNF <- .build_gene_stats("SPARC12", "UNFILTERED")
    stats_S12_FIL <- .build_gene_stats("SPARC12", "FILTERED")

    # ---- Export full per-gene stats (one CSV per table)
    if (isTRUE(SAVE)) {
      readr::write_csv(stats_STP_UNF, section_filename("4", "SPARCTP_log2GeneStatsMean_UNFILTERED", "csv"))
      readr::write_csv(stats_S12_UNF, section_filename("4", "SPARC12_log2GeneStatsMean_UNFILTERED", "csv"))
      readr::write_csv(stats_STP_FIL, section_filename("4", paste0("SPARCTP_", thr_label_short, "_log2GeneStatsMean_FILTERED"), "csv"))
      readr::write_csv(stats_S12_FIL, section_filename("4", paste0("SPARC12_", thr_label_short, "_log2GeneStatsMean_FILTERED"), "csv"))
    }

    # ---- Helper: caption builder (Welch only; still reported in caption)
    .build_caption_log2 <- function(st, df_stats, filter_mode) {
      ev_vals    <- df_stats %>% dplyr::filter(is_EV) %>% dplyr::pull(mean_log2)
      other_vals <- df_stats %>% dplyr::filter(!is_EV) %>% dplyr::pull(mean_log2)
      n_ev       <- sum(is.finite(ev_vals))
      n_other    <- sum(is.finite(other_vals))

      tt <- tryCatch({
        if (n_ev > 1 && n_other > 1) stats::t.test(ev_vals, other_vals, var.equal = FALSE) else NULL
      }, error = function(e) NULL)
      p_t   <- if (!is.null(tt)) paste0("Welch t-test p = ", formatC(tt$p.value, format = "g", digits = 3)) else "Welch t-test p = NA"

      head_txt <- if (filter_mode == "UNFILTERED") "UNFILTERED. " else paste0("Filtered at ", thr_pretty, ". ")

      paste0(
        head_txt,
        "Comparing per-gene mean log2 abundance between groups. ",
        "EV markers n = ", n_ev, "; Other genes n = ", n_other, ". ",
        p_t
      ) %>% stringr::str_wrap(width = 72)
    }

    # ---- Helper: exact plot-data export
    # ---- Plot builder for one SampleType & one table (no bracket or p-label; keep n/mean labels)
    .make_violin_log2_plot <- function(st, df_stats, filter_mode) {
      # Prepare plot data (use mean_log2)
      plot_df <- df_stats %>%
        dplyr::transmute(
          SampleType, Filter,
          Class = factor(ifelse(is_EV, "EV markers", "Other genes"),
                         levels = c("Other genes","EV markers")),
          gene_symbol, mean_log2
        )

      # Export exact plot data
      stem <- paste0(
        if (filter_mode == "UNFILTERED") "plotdata_violin_log2Mean_EVvsOther_" else paste0(thr_label_short, "_plotdata_violin_log2Mean_EVvsOther_"),
        st, "_", filter_mode
      )
      save_plotdata(plot_df, stem, prefix = paste0(name, "_", date, "_S4_"))

      # Counts (for subtitle)
      n_ev    <- sum(plot_df$Class == "EV markers", na.rm = TRUE)
      n_other <- sum(plot_df$Class == "Other genes", na.rm = TRUE)

      # Group labels (two lines)
      grp_info <- plot_df %>%
        dplyr::group_by(Class) %>%
        dplyr::summarise(
          n  = dplyr::n(),
          mu = suppressWarnings(mean(mean_log2, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        dplyr::mutate(lbl = paste0("n = ", n, "\nmean = ", sprintf("%.2f", mu)))

      # Title with colored SampleType
      ttl <- if (.has_ggtext) {
        paste0("Gene-level log2 abundance â€” ",
               "<span style='color:", title_color_for(st), ";'><b>", st, "</b></span>")
      } else {
        paste0("Gene-level log2 abundance â€” ", st)
      }

      # Short subtitle (your format)
      subl <- if (filter_mode == "UNFILTERED") {
        paste0("UNFILTERED â€¢ EV markers (n=", n_ev, ") vs Other (n=", n_other, ")")
      } else {
        paste0("Filtered at ", thr_pretty, " â€¢ EV (n=", n_ev, ") vs Other (n=", n_other, ")")
      }
      cap  <- .build_caption_log2(st, df_stats, filter_mode)

      # ---- Y-axis cap & label placements (target top â‰ˆ 27)
      y_max_data <- max(plot_df$mean_log2, na.rm = TRUE)
      y_cap <- max(27, y_max_data + 1.0)
      y_grp <- min(y_max_data + 0.45, y_cap - 0.90)  # group labels comfortably below the top

      # Slight horizontal offsets for group labels
      x_pos_other <- 0.88
      x_pos_ev    <- 2.12

      p <- ggplot(plot_df, aes(x = Class, y = mean_log2)) +
        geom_violin(fill = "grey92", color = "black", width = 0.92, alpha = 1, trim = FALSE) +
        stat_summary(fun = mean, geom = "crossbar", width = 0.55, linewidth = 0.9, color = "black") +
        ggbeeswarm::geom_quasirandom(
          data = subset(plot_df, Class == "Other genes"),
          shape = 21, size = 2.0, stroke = 0.5, width = 0.18, alpha = 0.40,
          color = "grey35", fill = "grey75"
        ) +
        ggbeeswarm::geom_quasirandom(
          data = subset(plot_df, Class == "EV markers"),
          shape = 21, size = 2.5, stroke = 0.6, width = 0.12, alpha = 0.98,
          color = "black", fill = pal$cyan
        ) +
        # group labels above each violin (two lines; no bracket or p-label)
        annotate("text", x = x_pos_other, y = y_grp,
                 label = grp_info$lbl[grp_info$Class == "Other genes"]) +
        annotate("text", x = x_pos_ev,    y = y_grp,
                 label = grp_info$lbl[grp_info$Class == "EV markers"]) +
        labs(
          title    = ttl,
          subtitle = subl,
          x = NULL, y = "Mean log2 abundance (HD-present)",
          caption  = cap
        ) +
        coord_cartesian(ylim = c(NA, y_cap), clip = "off") +
        scale_x_discrete(drop = FALSE) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title    = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5, face = "bold")
          else element_text(hjust = 0.5, face = "bold", color = title_color_for(st)),
          plot.subtitle = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5)
          else element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          plot.caption  = element_text(hjust = 0.5),
          plot.margin   = margin(10, 22, 10, 10),
          axis.text.x   = element_text(face = "bold")
        ) +
        scale_y_continuous(expand = expansion(mult = c(0.03, 0.02)))

      if (isTRUE(SAVE)) {
        fn <- paste0(
          name, "_", date, "_",
          if (filter_mode == "UNFILTERED") "" else paste0(thr_label_short, "_"),
          "violin_log2Mean_EVvsOther_", st, "_", filter_mode, ".png"
        )
        ggsave_if(fn, p, width = 7.2, height = 6.0, dpi = 300, bg = "white")
      }
      print(p)
    }

    # ---- Make all four figures
    .make_violin_log2_plot("SPARCTP", stats_STP_UNF, "UNFILTERED")
    .make_violin_log2_plot("SPARCTP", stats_STP_FIL, "FILTERED")
    .make_violin_log2_plot("SPARC12", stats_S12_UNF, "UNFILTERED")
    .make_violin_log2_plot("SPARC12", stats_S12_FIL, "FILTERED")



  }, envir = .GlobalEnv)
  invisible(NULL)
}
run_section4()


#### SECTION 5 â€” Disease vs Healthy: per-gene mean(log2) with EV overlay (by SPARC) [Wilcoxon + per-plot summaries] ----

run_section5 <- function(save = SAVE) {
  old_save <- SAVE
  on.exit(assign("SAVE", old_save, envir = .GlobalEnv))
  assign("SAVE", save, envir = .GlobalEnv)
  evalq({
    # What this does (updates):
    # â€¢ EV points are TIGHTER than Other genes (smaller quasirandom width for EVs).
    # â€¢ Use Wilcoxon rank-sum (Mannâ€“Whitney) for Healthy vs Disease comparisons
    #   (both for ALL genes and EV-only). P-values appear in the CAPTION (no in-plot stats).
    # â€¢ CAPTION now also reports group means:
    #     â€“ mean log2 (All genes) for Healthy and Disease
    #     â€“ mean log2 (EV-only) for Healthy and Disease
    # â€¢ Export a per-plot SUMMARY TABLE listing, for each group (Healthy/Disease):
    #     n_all, mean_all, sd_all, cv_all, n_ev, mean_ev, sd_ev, cv_ev, plus p_all, p_ev (same on both rows).
    # â€¢ Still exports:
    #     â€“ full per-gene wide stats (Healthy/Disease columns)
    #     â€“ exact plot-data CSV used for each violin
    #
    # Notes:
    # â€¢ Data include Healthy & Disease only. Coverage thresholding for FILTERED uses HD-only coverage.
    # â€¢ Benign/Control are excluded for this section.

    # ---- Local labels


    # ---- Restrict to Healthy vs Disease (and keep log2 present values only for summaries)

    # ---- Helper: coverage pass (same as earlier)
    # Keep-sets for FILTERED (from HD-only coverage, per SPARC)
    # ---- Build per-gene group means (Healthy vs Disease) for one SPARC + context
    # filter_mode âˆˆ {"UNFILTERED","FILTERED"}
    .build_gene_group_means <- function(st, filter_mode = c("UNFILTERED","FILTERED")) {
      filter_mode <- match.arg(filter_mode)
      d_st <- df_hd %>%
        dplyr::filter(SampleType == st, !is.na(RawAbundance) & RawAbundance > 0, !is.na(log2_abundance)) %>%
        dplyr::mutate(Group = factor(DiseaseType, levels = c("Healthy","Disease")))

      # Choose gene universe
      gene_set <- if (filter_mode == "UNFILTERED") {
        unique(d_st$gene_symbol)
      } else if (st == "SPARCTP") keep_SPARCTP else keep_SPARC12

      if (length(gene_set) == 0L) {
        return(list(stats_wide = tibble::tibble(), plot_long = tibble::tibble()))
      }

      d_st <- d_st %>% dplyr::filter(gene_symbol %in% gene_set)

      # Per-gene, per-group summaries (present-only)
      grp <- d_st %>%
        dplyr::group_by(gene_symbol, Group) %>%
        dplyr::summarise(
          mean_log2 = suppressWarnings(mean(log2_abundance, na.rm = TRUE)),
          sd_log2   = suppressWarnings(stats::sd(log2_abundance, na.rm = TRUE)),
          .groups   = "drop"
        ) %>%
        dplyr::mutate(cv_log2 = dplyr::if_else(is.finite(mean_log2) & abs(mean_log2) > 0,
                                               sd_log2/abs(mean_log2), NA_real_))

      # Wide stats to csv
      stats_wide <- grp %>%
        tidyr::pivot_wider(
          names_from  = Group,
          values_from = c(mean_log2, sd_log2, cv_log2),
          names_glue  = "{.value}_{Group}"
        ) %>%
        dplyr::mutate(
          is_EV     = gene_symbol %in% ev_genes,
          SampleType= st,
          Filter    = filter_mode,
          .before   = 1
        )

      # Long plot data (All genes & EV tag)
      plot_long <- grp %>%
        dplyr::mutate(
          Class      = ifelse(gene_symbol %in% ev_genes, "EV markers", "Other genes"),
          Class      = factor(Class, levels = c("Other genes","EV markers")),
          SampleType = st,
          Filter     = filter_mode
        ) %>%
        dplyr::select(SampleType, Filter, gene_symbol, Class, Group, mean_log2)

      list(stats_wide = stats_wide, plot_long = plot_long)
    }

    # ---- Build all four combinations
    build_STP_UNF <- .build_gene_group_means("SPARCTP", "UNFILTERED")
    build_STP_FIL <- .build_gene_group_means("SPARCTP", "FILTERED")
    build_S12_UNF <- .build_gene_group_means("SPARC12", "UNFILTERED")
    build_S12_FIL <- .build_gene_group_means("SPARC12", "FILTERED")

    # ---- Export full per-gene DvH stats
    if (isTRUE(SAVE)) {
      readr::write_csv(build_STP_UNF$stats_wide, section_filename("5", "SPARCTP_log2GeneStatsMean_DvH_UNFILTERED", "csv"))
      readr::write_csv(build_S12_UNF$stats_wide, section_filename("5", "SPARC12_log2GeneStatsMean_DvH_UNFILTERED", "csv"))
      readr::write_csv(build_STP_FIL$stats_wide, section_filename("5", paste0("SPARCTP_", thr_label_short, "_log2GeneStatsMean_DvH_FILTERED"), "csv"))
      readr::write_csv(build_S12_FIL$stats_wide, section_filename("5", paste0("SPARC12_", thr_label_short, "_log2GeneStatsMean_DvH_FILTERED"), "csv"))
    }

    # ---- Helpers: plot-data saver, caption, and per-plot summary table
    # Build summary table (returned + optionally saved)
    .make_summary_table <- function(st, plot_long) {
      # Group summaries (All genes)
      sum_all <- plot_long %>%
        dplyr::group_by(Group) %>%
        dplyr::summarise(
          n_all   = dplyr::n(),
          mean_all= suppressWarnings(mean(mean_log2, na.rm = TRUE)),
          sd_all  = suppressWarnings(stats::sd(mean_log2, na.rm = TRUE)),
          .groups = "drop"
        ) %>% dplyr::mutate(cv_all = dplyr::if_else(is.finite(mean_all) & abs(mean_all) > 0,
                                                    sd_all/abs(mean_all), NA_real_))

      # EV-only summaries
      sum_ev <- plot_long %>%
        dplyr::filter(Class == "EV markers") %>%
        dplyr::group_by(Group) %>%
        dplyr::summarise(
          n_ev    = dplyr::n(),
          mean_ev = suppressWarnings(mean(mean_log2, na.rm = TRUE)),
          sd_ev   = suppressWarnings(stats::sd(mean_log2, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        dplyr::mutate(cv_ev = dplyr::if_else(is.finite(mean_ev) & abs(mean_ev) > 0,
                                             sd_ev/abs(mean_ev), NA_real_))

      # Merge
      dplyr::full_join(sum_all, sum_ev, by = "Group") %>%
        dplyr::mutate(SampleType = st,
                      Filter     = unique(plot_long$Filter),
                      .before = 1)
    }

    # Caption with Wilcoxon + means for All and EV-only
    .build_caption_dvh <- function(plot_long) {
      # Means per group (All)
      m_all <- plot_long %>%
        dplyr::group_by(Group) %>%
        dplyr::summarise(mu = suppressWarnings(mean(mean_log2, na.rm = TRUE)), .groups = "drop") %>%
        tibble::deframe()

      # Means per group (EV-only)
      m_ev <- plot_long %>%
        dplyr::filter(Class == "EV markers") %>%
        dplyr::group_by(Group) %>%
        dplyr::summarise(mu = suppressWarnings(mean(mean_log2, na.rm = TRUE)), .groups = "drop") %>%
        tibble::deframe()

      # Wilcoxon p-values
      H_all <- plot_long %>% dplyr::filter(Group == "Healthy") %>% dplyr::pull(mean_log2)
      D_all <- plot_long %>% dplyr::filter(Group == "Disease") %>% dplyr::pull(mean_log2)
      p_all <- tryCatch({
        if (sum(is.finite(H_all)) > 0 && sum(is.finite(D_all)) > 0)
          stats::wilcox.test(H_all, D_all, exact = FALSE)$p.value else NA_real_
      }, error = function(e) NA_real_)

      ev_H <- plot_long %>% dplyr::filter(Group == "Healthy", Class == "EV markers") %>% dplyr::pull(mean_log2)
      ev_D <- plot_long %>% dplyr::filter(Group == "Disease", Class == "EV markers") %>% dplyr::pull(mean_log2)
      p_ev <- tryCatch({
        if (sum(is.finite(ev_H)) > 0 && sum(is.finite(ev_D)) > 0)
          stats::wilcox.test(ev_H, ev_D, exact = FALSE)$p.value else NA_real_
      }, error = function(e) NA_real_)

      lead <- if (unique(plot_long$Filter) == "UNFILTERED") "UNFILTERED. " else paste0("Filtered at ", thr_pretty, ". ")

      paste0(
        lead,
        "Group means (All genes): Healthy = ", ifelse(is.null(m_all[["Healthy"]]), "NA", sprintf("%.2f", m_all[["Healthy"]])),
        "; Disease = ", ifelse(is.null(m_all[["Disease"]]), "NA", sprintf("%.2f", m_all[["Disease"]])), ". ",
        "Group means (EV-only): Healthy = ", ifelse(is.null(m_ev[["Healthy"]]), "NA", sprintf("%.2f", m_ev[["Healthy"]])),
        "; Disease = ", ifelse(is.null(m_ev[["Disease"]]), "NA", sprintf("%.2f", m_ev[["Disease"]])), ". ",
        "Wilcoxon p (All) = ", ifelse(is.na(p_all), "NA", formatC(p_all, format = "g", digits = 3)), "; ",
        "Wilcoxon p (EV-only) = ", ifelse(is.na(p_ev), "NA", formatC(p_ev, format = "g", digits = 3)), "."
      ) %>% stringr::str_wrap(width = 86)
    }

    # ---- Plot builder (one SPARC + context)
    .make_violin_dvh <- function(st, build_obj) {
      plot_long <- build_obj$plot_long
      if (!nrow(plot_long)) return(invisible(NULL))

      # Save plot data
      filter_tag <- unique(plot_long$Filter)
      stem_prefix <- if (filter_tag == "UNFILTERED") "" else paste0(thr_label_short, "_")
      stem <- paste0(stem_prefix, "plotdata_violin_log2Mean_DvH_", st, "_", filter_tag)
      save_plotdata(plot_long, stem, prefix = paste0(name, "_", date, "_S5_"))

      # Build & save per-plot summary table
      summary_tbl <- .make_summary_table(st, plot_long)

      # Add p-values to the summary (same value on both rows for convenience)
      # (All genes)
      H_all <- plot_long %>% dplyr::filter(Group == "Healthy") %>% dplyr::pull(mean_log2)
      D_all <- plot_long %>% dplyr::filter(Group == "Disease") %>% dplyr::pull(mean_log2)
      p_all <- tryCatch({
        if (sum(is.finite(H_all)) > 0 && sum(is.finite(D_all)) > 0)
          stats::wilcox.test(H_all, D_all, exact = FALSE)$p.value else NA_real_
      }, error = function(e) NA_real_)
      # (EV-only)
      ev_H <- plot_long %>% dplyr::filter(Group == "Healthy", Class == "EV markers") %>% dplyr::pull(mean_log2)
      ev_D <- plot_long %>% dplyr::filter(Group == "Disease", Class == "EV markers") %>% dplyr::pull(mean_log2)
      p_ev <- tryCatch({
        if (sum(is.finite(ev_H)) > 0 && sum(is.finite(ev_D)) > 0)
          stats::wilcox.test(ev_H, ev_D, exact = FALSE)$p.value else NA_real_
      }, error = function(e) NA_real_)

      summary_tbl <- summary_tbl %>%
        dplyr::mutate(
          p_wilcox_all = p_all,
          p_wilcox_ev  = p_ev
        )

      if (isTRUE(SAVE)) {
        stem_prefix <- if (unique(plot_long$Filter) == "UNFILTERED") "" else paste0(thr_label_short, "_")
        fn_sum <- section_filename("5", paste0(stem_prefix, "summary_DvH_", st, "_", unique(plot_long$Filter)), "csv")
        readr::write_csv(summary_tbl, fn_sum); message("Saved: ", fn_sum)
      }

      # Group labels (two lines above violins)
      grp_lbl <- plot_long %>%
        dplyr::group_by(Group) %>%
        dplyr::summarise(
          n  = dplyr::n(),
          mu = suppressWarnings(mean(mean_log2, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        dplyr::mutate(lbl = paste0("n = ", n, "\nmean = ", sprintf("%.2f", mu)))

      # Colors
      disease_fills <- c("Healthy" = pal$aqua, "Disease" = pal$purple)

      # Title with SPARC color
      ttl <- if (.has_ggtext) {
        paste0("Disease vs Healthy â€” log2 abundance â€” ",
               "<span style='color:", title_color_for(st), ";'><b>", st, "</b></span>")
      } else {
        paste0("Disease vs Healthy â€” log2 abundance â€” ", st)
      }
      subl <- if (unique(plot_long$Filter) == "UNFILTERED") {
        paste0("UNFILTERED â€¢ Healthy (n=", grp_lbl$n[grp_lbl$Group=="Healthy"],
               ") vs Disease (n=", grp_lbl$n[grp_lbl$Group=="Disease"], ")")
      } else {
        paste0("Filtered at ", thr_pretty, " â€¢ Healthy (n=", grp_lbl$n[grp_lbl$Group=="Healthy"],
               ") vs Disease (n=", grp_lbl$n[grp_lbl$Group=="Disease"], ")")
      }
      cap <- .build_caption_dvh(plot_long)

      # y-axis room (consistent with earlier look)
      y_max <- max(plot_long$mean_log2, na.rm = TRUE)
      y_cap <- max(27, y_max + 1.0)
      y_lbl <- min(y_max + 0.45, y_cap - 0.90)

      # Slight horizontal offsets for labels
      x_pos_H <- 0.88
      x_pos_D <- 2.12

      p <- ggplot(plot_long, aes(x = Group, y = mean_log2)) +
        geom_violin(aes(fill = Group), color = "black", width = 0.92, alpha = 1, trim = FALSE) +
        stat_summary(fun = mean, geom = "crossbar", width = 0.55, linewidth = 0.9, color = "black") +
        # Other genes first (wider spread), EV markers on top (tighter spread)
        ggbeeswarm::geom_quasirandom(
          data = subset(plot_long, Class == "Other genes"),
          shape = 21, size = 2.0, stroke = 0.5, width = 0.20, alpha = 0.40,
          color = "grey35", fill = "grey75"
        ) +
        ggbeeswarm::geom_quasirandom(
          data = subset(plot_long, Class == "EV markers"),
          shape = 21, size = 2.5, stroke = 0.6, width = 0.08, alpha = 0.98,  # tighter spread for EVs
          color = "black", fill = pal$cyan
        ) +
        scale_fill_manual(values = disease_fills, drop = FALSE) +
        # Two-line group labels
        annotate("text", x = x_pos_H, y = y_lbl,
                 label = grp_lbl$lbl[grp_lbl$Group == "Healthy"]) +
        annotate("text", x = x_pos_D, y = y_lbl,
                 label = grp_lbl$lbl[grp_lbl$Group == "Disease"]) +
        labs(
          title    = ttl,
          subtitle = subl,
          x = NULL, y = "Mean log2 abundance (present only)",
          caption  = cap
        ) +
        coord_cartesian(ylim = c(NA, y_cap), clip = "off") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title    = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5, face = "bold")
          else element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5)
          else element_text(hjust = 0.5),
          legend.position = "none",
          panel.grid.minor = element_blank(),
          plot.caption  = element_text(hjust = 0.5),
          plot.margin   = margin(10, 22, 10, 10),
          axis.text.x   = element_text(face = "bold")
        ) +
        scale_y_continuous(expand = expansion(mult = c(0.03, 0.02)))

      print(p)
      if (isTRUE(SAVE)) {
        fn <- section_filename("5", paste0(stem_prefix, "violin_log2Mean_DvH_", st, "_", filter_tag), "png")
        ggsave_if(fn, p, width = 7.4, height = 6.0, dpi = 300, bg = "white")
      }
    }

    # ---- Render & save all 4 figures (+ their summaries)
    .make_violin_dvh("SPARCTP", build_STP_UNF)
    .make_violin_dvh("SPARCTP", build_STP_FIL)
    .make_violin_dvh("SPARC12", build_S12_UNF)
    .make_violin_dvh("SPARC12", build_S12_FIL)



  }, envir = .GlobalEnv)
  invisible(NULL)
}
run_section5()


#### SECTION 6 â€” Publication-style volcano plots (viewâ†’save one-by-one; per-plot axes; |log2FC| cutoff = 1) ----

run_section6 <- function(save = SAVE) {
  old_save <- SAVE
  on.exit(assign("SAVE", old_save, envir = .GlobalEnv))
  assign("SAVE", save, envir = .GlobalEnv)
  evalq({
    # Updates in this revision:
    # â€¢ Label ALL significant EV markers (no top-N cap). Non-EV labels still top-N up & down.
    # â€¢ All text labels bold; EV labels cyan, non-EV labels black.
    # â€¢ Non-significant points more transparent (ns alpha = 0.15). Robust CSV write via tryCatch.

    # ---------- Parameters / helpers
    fdr_thr <- 0.05
    fc_thr  <- 1.0

    thr_pretty_unf  <- threshold_label(0)


    .color_for_ctx <- function(st_label) {
      if (st_label %in% c("SPARCTP","IEVTP")) pal$tp
      else if (st_label %in% c("SPARC12","IEV12")) pal$i12
      else pal$black
    }

    # ---------- Base data (Healthy + Disease only; present-only)
    df_hd_vol <- df_hd_present %>%
      dplyr::mutate(DiseaseType = factor(DiseaseType, levels = c("Healthy", "Disease")))

    keep_INTER <- keep_sets[["Intersection"]]

    # Observed (UNFILTERED) sets within HD-only data
    obs_SPARCTP <- df_hd_vol %>% dplyr::filter(SampleType == "SPARCTP") %>% dplyr::pull(gene_symbol) %>% unique()
    obs_SPARC12 <- df_hd_vol %>% dplyr::filter(SampleType == "SPARC12") %>% dplyr::pull(gene_symbol) %>% unique()
    obs_POOLED  <- df_hd_vol %>% dplyr::pull(gene_symbol) %>% unique()
    obs_INTER   <- intersect(obs_SPARCTP, obs_SPARC12)

    # ---------- Volcano computation

    compute_volcano <- function(df_in, group_var, case_label, ctrl_label) {
      gv <- rlang::ensym(group_var)
      dat <- df_in %>%
        dplyr::select(gene_symbol, log2_abundance, !!gv) %>%
        dplyr::filter(!is.na(log2_abundance), !is.na(!!gv), !!gv %in% c(case_label, ctrl_label))
      volcano <- dat %>%
        dplyr::group_by(gene_symbol) %>%
        dplyr::summarise(
          mean_case = mean(log2_abundance[!!gv == case_label], na.rm = TRUE),
          mean_ctrl = mean(log2_abundance[!!gv == ctrl_label], na.rm = TRUE),
          log2FC    = mean_case - mean_ctrl,
          pval = {
            x <- log2_abundance[!!gv == case_label]
            y <- log2_abundance[!!gv == ctrl_label]
            if (sum(!is.na(x)) >= 2 && sum(!is.na(y)) >= 2) {
              tryCatch(stats::t.test(x, y, var.equal = FALSE)$p.value, error = function(e) NA_real_)
            } else NA_real_
          },
          n_case = sum(!is.na(log2_abundance[!!gv == case_label])),
          n_ctrl = sum(!is.na(log2_abundance[!!gv == ctrl_label])),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          FDR_raw_in = dplyr::if_else(is.na(pval), 1, pval),
          FDR        = p.adjust(FDR_raw_in, method = "BH"),
          is_EV      = gene_symbol %in% ev_genes,
          neglog10FDR= -log10(pmax(FDR, .Machine$double.eps)),
          sig_flag   = dplyr::case_when(FDR <= fdr_thr & abs(log2FC) >= fc_thr ~ "sig", TRUE ~ "ns")
        ) %>%
        dplyr::select(-FDR_raw_in)
      volcano
    }

    # ---------- Plot factory 
    .build_volcano <- function(context = c("SPARCTP","SPARC12","Pooled","Intersection"),
                               mode = c("UNFILTERED","FILTERED"),
                               top_label_n = 10) {
      context <- match.arg(context)
      mode    <- match.arg(mode)

      d_ctx <- if (context %in% c("SPARCTP","SPARC12")) df_hd_vol %>% dplyr::filter(SampleType == context) else df_hd_vol
      genes <- switch(context,
                      "SPARCTP"      = if (mode == "UNFILTERED") obs_SPARCTP else keep_SPARCTP,
                      "SPARC12"      = if (mode == "UNFILTERED") obs_SPARC12 else keep_SPARC12,
                      "Pooled"       = if (mode == "UNFILTERED") obs_POOLED  else keep_POOLED,
                      "Intersection" = if (mode == "UNFILTERED") obs_INTER   else keep_INTER
      )
      if (length(genes) == 0L) {
        message("No genes available for ", context, " [", mode, "].")
        return(list(plot = ggplot(), table = tibble::tibble(), base = NULL))
      }

      vtab <- compute_volcano(
        d_ctx %>% dplyr::filter(gene_symbol %in% genes),
        group_var = DiseaseType, case_label = "Disease", ctrl_label = "Healthy"
      )

      # Label set: ALL significant EVs + top-N up/down non-EVs (to avoid clutter)
      sig_ev <- vtab %>% dplyr::filter(sig_flag == "sig", is_EV)
      sig_non_ev <- vtab %>% dplyr::filter(sig_flag == "sig", !is_EV)
      top_non_ev <- dplyr::bind_rows(
        sig_non_ev %>% dplyr::arrange(dplyr::desc(log2FC)) %>% dplyr::slice_head(n = top_label_n),
        sig_non_ev %>% dplyr::arrange(log2FC)               %>% dplyr::slice_head(n = top_label_n)
      ) %>% dplyr::distinct(gene_symbol, .keep_all = TRUE)
      lab_df <- dplyr::bind_rows(sig_ev, top_non_ev) %>% dplyr::distinct(gene_symbol, .keep_all = TRUE)

      total_genes <- sum(!is.na(vtab$FDR))
      total_evs   <- sum(vtab$is_EV, na.rm = TRUE)
      up_n        <- sum(vtab$FDR <= fdr_thr & vtab$log2FC >= fc_thr, na.rm = TRUE)
      down_n      <- sum(vtab$FDR <= fdr_thr & vtab$log2FC <= -fc_thr, na.rm = TRUE)
      ev_up       <- sum(vtab$is_EV & vtab$FDR <= fdr_thr & vtab$log2FC >= fc_thr, na.rm = TRUE)
      ev_down     <- sum(vtab$is_EV & vtab$FDR <= fdr_thr & vtab$log2FC <= -fc_thr, na.rm = TRUE)

      cap_txt <- paste0(
        "Total genes: ", total_genes,
        " | EV markers: ", total_evs,
        " | Genes up: ", up_n,
        " | Genes down: ", down_n,
        " | EV up: ", ev_up,
        " | EV down: ", ev_down,
        " | guides: FDR=0.05, log2FC=+/-", fc_thr
      )

      ctx_col <- .color_for_ctx(context)
      thr_sub <- if (mode == "UNFILTERED") thr_for_subtitle(thr_pretty_unf) else thr_for_subtitle(thr_pretty)

      vtab_plot <- vtab %>%
        dplyr::filter(is.finite(log2FC), is.finite(neglog10FDR))

      p <- ggplot(vtab_plot, aes(x = log2FC, y = neglog10FDR)) +
        # Points (ns more transparent)
        geom_point(data = subset(vtab_plot, !is_EV),
                   aes(alpha = sig_flag),
                   shape = 21, size = 2.0, stroke = 0.25,
                   fill = "grey70", color = "grey25") +
        geom_point(data = subset(vtab_plot,  is_EV),
                   aes(alpha = sig_flag),
                   shape = 21, size = 2.2, stroke = 0.45,
                   fill = pal$cyan, color = "black") +
        scale_alpha_manual(values = c(sig = 0.95, ns = 0.15), guide = "none") +
        # Guides
        geom_vline(xintercept = c(-fc_thr, 0, fc_thr), linetype = c("dashed","solid","dashed")) +
        geom_hline(yintercept = -log10(fdr_thr), linetype = "dashed") +
        # Labels â€” bold for all; EV = cyan, non-EV = black; repel tuned to avoid overlap
        ggrepel::geom_text_repel(
          data = lab_df %>% dplyr::filter(is.finite(log2FC), is.finite(neglog10FDR)),
          aes(label = gene_symbol, color = is_EV),
          fontface = "bold", size = 3.0,
          box.padding = 0.5, point.padding = 0.35,
          force = 1.6, max.time = 2.5, max.iter = 30000,
          min.segment.length = 0,
          segment.alpha = 0.45,
          max.overlaps = Inf, show.legend = FALSE, seed = 12
        ) +
        scale_color_manual(values = c(`TRUE` = pal$cyan, `FALSE` = "black"), guide = "none") +
        scale_x_continuous("log2 Fold Change (Disease - Healthy)",
                           expand = expansion(mult = c(0.02, 0.02))) +
        scale_y_continuous(expression(-log[10]("FDR")),
                           expand = expansion(mult = c(0.02, 0.10))) +
        labs(
          title    = "Differential Expression",
          subtitle = paste0("Disease vs Healthy - ", fmt_st_bold(context), " - ", thr_sub),
          caption  = cap_txt
        ) +
        theme_minimal(base_size = 14) +
        theme(
          legend.position  = "none",
          plot.title       = title_elem(ctx_col),
          plot.subtitle    = subtitle_elem(ctx_col),
          plot.caption     = element_text(size = 8, hjust = 0.5),
          panel.grid.minor = element_blank()
        )

      tag  <- paste0(ifelse(mode == "UNFILTERED", "UNF", thr_label_short), "_", toupper(context))
      base <- section_filename("6", paste0(tag, "_volcano_DvH"))

      list(plot = p,
           table = vtab %>% dplyr::select(gene_symbol, log2FC, pval, FDR, neglog10FDR, is_EV, mean_case, mean_ctrl, n_case, n_ctrl, sig_flag),
           base  = base)
    }

    # ---------- Render helper 
    .render_volcano <- function(label, builder_fn) {
      b <- builder_fn()
      if (is.null(b$base)) return(invisible(NULL))
      print(b$plot)
      if (isTRUE(SAVE)) {
        ggsave_if(paste0(b$base, ".png"), b$plot, width = 8.6, height = 6.4, dpi = 300, bg = pal$white)
        tryCatch({
          readr::write_csv(b$table, paste0(b$base, ".csv"), na = "")
        }, error = function(e) {
          warning("Could not save CSV for ", label, ": ", conditionMessage(e))
        })
      }
      invisible(NULL)
    }

    # ---------- Named builders 
    build_STP_UNF  <- function() .build_volcano("SPARCTP", "UNFILTERED")
    build_STP_FIL  <- function() .build_volcano("SPARCTP", "FILTERED")
    build_S12_UNF  <- function() .build_volcano("SPARC12", "UNFILTERED")
    build_S12_FIL  <- function() .build_volcano("SPARC12", "FILTERED")
    build_POOL_UNF <- function() .build_volcano("Pooled",  "UNFILTERED")
    build_POOL_FIL <- function() .build_volcano("Pooled",  "FILTERED")
    build_INT_UNF  <- function() .build_volcano("Intersection", "UNFILTERED")
    build_INT_FIL  <- function() .build_volcano("Intersection", "FILTERED")

    # ---- Render & save
    .render_volcano("SPARCTP - UNFILTERED", build_STP_UNF)
    .render_volcano("SPARCTP - FILTERED",   build_STP_FIL)
    .render_volcano("SPARC12 - UNFILTERED", build_S12_UNF)
    .render_volcano("SPARC12 - FILTERED",   build_S12_FIL)
    .render_volcano("Pooled - UNFILTERED",  build_POOL_UNF)
    .render_volcano("Pooled - FILTERED",    build_POOL_FIL)
    .render_volcano("Intersection - UNFILTERED", build_INT_UNFILTERED <- build_INT_UNF)
    .render_volcano("Intersection - FILTERED",   build_INT_FIL)



  }, envir = .GlobalEnv)
  invisible(NULL)
}
run_section6()


#### SECTION 7 â€” Boxplots of log2 abundance for topÂ±5 genes by log2FC ####

run_section7 <- function(save = SAVE) {
  old_save <- SAVE
  on.exit(assign("SAVE", old_save, envir = .GlobalEnv))
  assign("SAVE", save, envir = .GlobalEnv)
  evalq({
    # Adds: threshold text in subtitles, bold "n=Y" labels, and 4 stacked (Positive over Negative) plots.

    # --- Labels & palette

    disease_fills   <- c("Healthy" = pal$aqua, "Disease" = pal$purple)

    # --- Volcano CSV path reused from Section 6 (FILTERED context)
    .volcano_csv_for_sparc <- function(st) section_filename("6", paste0(thr_label_short, "_", st, "_volcano_DvH"), "csv")

    # --- One-time per SPARC: write the two requested log2FC CSVs (All genes, EV-only)
    .write_log2fc_csvs_for_sparc <- function(st) {
      fn <- .volcano_csv_for_sparc(st)
      if (!file.exists(fn)) { warning("Volcano CSV not found for ", st, ": ", fn, call. = FALSE); return(invisible(NULL)) }
      vt <- suppressMessages(readr::read_csv(fn, show_col_types = FALSE))
      if (!"is_EV" %in% names(vt)) vt <- vt %>% dplyr::mutate(is_EV = gene_symbol %in% ev_genes)

      vt_info <- vt %>%
        dplyr::select(gene_symbol, log2FC, pval, FDR, neglog10FDR, is_EV,
                      mean_case, mean_ctrl, n_case, n_ctrl) %>%
        dplyr::mutate(SampleType = st, .before = 1)

      if (isTRUE(SAVE)) {
        readr::write_csv(vt_info, section_filename("7", paste0(thr_label_short, "_", st, "_log2FC_ALLGENES"), "csv"))
        readr::write_csv(vt_info %>% dplyr::filter(is_EV), section_filename("7", paste0(thr_label_short, "_", st, "_log2FC_EVONLY"), "csv"))
        message("Saved log2FC tables for ", st)
      }
    }

    # --- Select top 5 by log2FC from saved volcano table (optionally EV-only)
    .get_top5_from_volcano <- function(st, direction = c("Positive","Negative"), n_top = 5, ev_only = FALSE) {
      direction <- match.arg(direction)
      fn <- .volcano_csv_for_sparc(st)
      if (!file.exists(fn)) stop("Required volcano CSV not found for ", st, ": ", fn, call. = FALSE)
      vt <- suppressMessages(readr::read_csv(fn, show_col_types = FALSE)) %>%
        dplyr::filter(is.finite(log2FC))
      if (!"is_EV" %in% names(vt)) vt <- vt %>% dplyr::mutate(is_EV = gene_symbol %in% ev_genes)
      if (isTRUE(ev_only)) vt <- vt %>% dplyr::filter(is_EV)

      sel <- if (direction == "Positive") vt %>% dplyr::arrange(dplyr::desc(log2FC)) %>% dplyr::slice_head(n = n_top)
      else                           vt %>% dplyr::arrange(log2FC)               %>% dplyr::slice_head(n = n_top)

      sel %>%
        dplyr::mutate(
          Direction  = direction,
          gene_label = paste0(gene_symbol, " (", sprintf("%.2f", log2FC), ")")
        ) %>%
        dplyr::select(gene_symbol, gene_label, log2FC, Direction, is_EV)
    }

    # --- Build ONE plot (per SPARC, per direction, per flavor = "ALL" or "EVONLY")
    # Returns the ggplot object (so we can stack later). Still saves PNG if SAVE=TRUE.
    .make_boxplot_for_sparc <- function(st,
                                        direction = c("Positive","Negative"),
                                        flavor = c("ALL","EVONLY")) {
      direction <- match.arg(direction)
      flavor    <- match.arg(flavor)
      ev_only_flag <- (flavor == "EVONLY")

      # 1) Pick genes (top5, by log2FC, optionally EV-only)
      sel <- .get_top5_from_volcano(st, direction = direction, ev_only = ev_only_flag)
      genes_sel <- unique(sel$gene_symbol)
      if (!length(genes_sel)) { message("No genes for ", st, " / ", direction, " / ", flavor, "."); return(NULL) }

      # Facet order by log2FC (pos: desc; neg: asc)
      gene_levels <- if (direction == "Positive") {
        sel %>% dplyr::arrange(dplyr::desc(log2FC)) %>% dplyr::pull(gene_label)
      } else {
        sel %>% dplyr::arrange(log2FC) %>% dplyr::pull(gene_label)
      }

      # 2) Per-sample data (Healthy/Disease; present-only; per SPARC)
      d_plot <- df %>%
        dplyr::filter(
          DiseaseType %in% c("Healthy","Disease"),
          SampleType == st,
          !is.na(RawAbundance) & RawAbundance > 0,
          !is.na(log2_abundance),
          gene_symbol %in% genes_sel
        ) %>%
        dplyr::mutate(Disease = factor(DiseaseType, levels = c("Healthy","Disease"))) %>%
        dplyr::left_join(sel, by = "gene_symbol") %>%
        dplyr::mutate(gene_label = factor(gene_label, levels = gene_levels))

      # 3) n-labels per box (bold)
      n_labels <- d_plot %>%
        dplyr::group_by(gene_label, Disease) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(n_lab = paste0("n=", n))

      # Titles & subtitle; include threshold text
      ttl <- if (.has_ggtext) {
        paste0("Top 5 by log2FC â€” ",
               "<span style='color:", title_color_for(st), ";'><b>", st, "</b></span> (",
               if (flavor == "ALL") "All genes" else "EV markers only", ")")
      } else {
        paste0("Top 5 by log2FC â€” ", st, " (", if (flavor == "ALL") "All genes" else "EV markers only", ")")
      }
      comp_txt <- if (direction == "Positive") "Disease > Healthy" else "Healthy > Disease"
      subl <- paste0(comp_txt, " â€¢ Filter: ", thr_pretty)

      p <- ggplot(d_plot, aes(x = Disease, y = log2_abundance, fill = Disease)) +
        geom_boxplot(outlier.shape = NA, width = 0.70, color = "black") +
        ggbeeswarm::geom_quasirandom(shape = 21, size = 2.2, stroke = 0.5, width = 0.14,
                                     alpha = 0.95, color = "black") +
        scale_fill_manual(values = disease_fills, drop = FALSE) +
        labs(
          title    = ttl,
          subtitle = subl,
          x = NULL, y = "log2 abundance (present only)",
          caption  = stringr::str_wrap(
            "Boxes and points colored by Disease. Genes were selected based on log2FC (Disease âˆ’ Healthy). Healthy + Disease only; coverage filtering per SPARC.",
            width = 90
          )
        ) +
        facet_wrap(~ gene_label, ncol = 5, scales = "fixed") +   # SAME y-axis across facets in each plot
        theme_minimal(base_size = 14) +
        theme(
          plot.title       = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5, face = "bold")
          else element_text(hjust = 0.5, face = "bold", color = title_color_for(st)),
          plot.subtitle    = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5) else element_text(hjust = 0.5),
          legend.position  = "right",
          legend.title     = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          plot.caption     = element_text(hjust = 0.5),
          axis.text.x      = element_text(face = "bold")
        ) +
        # bottom bold "n=Y" under EACH box (two per facet)
        geom_text(
          data = n_labels,
          aes(x = Disease, y = -Inf, label = n_lab),
          inherit.aes = FALSE, vjust = -0.4, size = 3.4, fontface = "bold"
        ) +
        coord_cartesian(clip = "off")

      # Save individual plot
      if (isTRUE(SAVE)) {
        sign_tag   <- if (direction == "Positive") "TOP5_POS" else "TOP5_NEG"
        flavor_tag <- if (flavor == "ALL") "ALLGENES" else "EVONLY"
        fn <- section_filename("7", paste0(thr_label_short, "_", sign_tag, "_", flavor_tag, "_boxplot_", st), "png")
        ggsave_if(fn, p, width = 10.5, height = 4.6, dpi = 300, bg = "white")
      }
      return(p)
    }

    # ---- Generate CSVs + individual plots (8 total) and then stack Positive over Negative (4 stacked plots)
    plots <- list()

    for (st in c("SPARCTP","SPARC12")) {
      # Write the two log2FC CSVs (All genes, EV-only)
      .write_log2fc_csvs_for_sparc(st)

      # Individual plots
      plots[[paste0(st,"_POS_ALL")]] <- .make_boxplot_for_sparc(st, "Positive", "ALL")
      plots[[paste0(st,"_NEG_ALL")]] <- .make_boxplot_for_sparc(st, "Negative", "ALL")
      plots[[paste0(st,"_POS_EVO")]] <- .make_boxplot_for_sparc(st, "Positive", "EVONLY")
      plots[[paste0(st,"_NEG_EVO")]] <- .make_boxplot_for_sparc(st, "Negative", "EVONLY")

      # Stacked composites: Positive on top, Negative on bottom â€” one for ALL, one for EVONLY
      if (.has_gridExtra) {
        # Titles for the composite images
        title_all <- grid::textGrob(
          paste0("Top 5 by log2FC â€” ", st, " (All genes): Positive & Negative"),
          gp = grid::gpar(fontface = "bold", cex = 1.05)
        )
        title_evo <- grid::textGrob(
          paste0("Top 5 by log2FC â€” ", st, " (EV markers only): Positive & Negative"),
          gp = grid::gpar(fontface = "bold", cex = 1.05)
        )

        # Compose with gridExtra
        stack_all <- gridExtra::arrangeGrob(
          grobs = list(plots[[paste0(st,"_POS_ALL")]], plots[[paste0(st,"_NEG_ALL")]]),
          ncol = 1, top = title_all
        )
        stack_evo <- gridExtra::arrangeGrob(
          grobs = list(plots[[paste0(st,"_POS_EVO")]], plots[[paste0(st,"_NEG_EVO")]]),
          ncol = 1, top = title_evo
        )

        # Save stacked PNGs
        if (isTRUE(SAVE)) {
          fn_all <- section_filename("7", paste0(thr_label_short, "_TOP5_STACKED_ALL_", st), "png")
          png(fn_all, width = 2200, height = 1800, res = 200, bg = "white")
          grid::grid.draw(stack_all); dev.off(); message("Saved: ", fn_all)

          fn_evo <- section_filename("7", paste0(thr_label_short, "_TOP5_STACKED_EVONLY_", st), "png")
          png(fn_evo, width = 2200, height = 1800, res = 200, bg = "white")
          grid::grid.draw(stack_evo); dev.off(); message("Saved: ", fn_evo)
        }
      } else {
        message("gridExtra not available; stacked plots were not created.")
      }
    }







  }, envir = .GlobalEnv)
  invisible(NULL)
}
run_section7()


#### SECTION 8 â€” Least-variable genes (QC candidates) â€” Top 5 by lowest CV among top-50% abundant (passes threshold) ----

run_section8 <- function(save = SAVE) {
  old_save <- SAVE
  on.exit(assign("SAVE", old_save, envir = .GlobalEnv))
  assign("SAVE", save, envir = .GlobalEnv)
  evalq({
    # What this section does
    # â€¢ Uses only Healthy + Disease, present values (RawAbundance > 0) that also have log2_abundance.
    # â€¢ For each context (SPARCTP, SPARC12, Pooled) and for All genes vs EV markers only:
    #     1) Start from genes that PASS the coverage threshold (thr_current) in that context.
    #     2) Among those, compute per-gene mean(log2) across all samples in that context.
    #     3) Keep only the top 50% of genes by that mean(log2) abundance.
    #     4) Rank by CV = sd(log2)/|mean(log2)| (on present values); pick the 5 smallest CV.
    # â€¢ Make boxplots (faceted by GENE) with Disease on the x-axis.
    #     - SPARC-only plots: points filled by Disease, black borders, no SPARC legend.
    #     - Pooled plots: boxes filled by Disease, points filled by SPARC (Section 0 colors), black borders.
    # â€¢ Facet label (with ggtext if available): 
    #       GENE (bold)
    #       mean = X
    #       CV = Y
    #       FC = Z
    #   where FC = mean_Disease - mean_Healthy (still in log2 units; shorter label â€œFCâ€ by request).
    # â€¢ Bold n=Y sample counts placed on a single baseline across all facets.
    # â€¢ Exports (if SAVE = TRUE): for each context & mode (ALLGENES, EVONLY) a CSV with the FULL ranked list (not only top 5).

    # ---------- Inputs expected from prior sections 
    # pal (with $tp, $i12, $aqua, $purple), title_color_for(), thr_current, thr_pretty, thr_for_subtitle(),
    # presence_by_type, presence_overall, ev_genes (character vector), SAVE, name, date, thr_file_tag, .has_ggtext

    # ---------- Small utilities 
    # ---------- Build core data slice once (HD-only present values) 
    df_hd_present <- df %>%
      dplyr::filter(DiseaseType %in% c("Healthy","Disease"),
                    !is.na(RawAbundance) & RawAbundance > 0,
                    !is.na(log2_abundance)) %>%
      dplyr::mutate(
        Disease    = factor(DiseaseType, levels = c("Healthy","Disease")),
        SampleType = dplyr::case_when(
          SampleType %in% c("IEVTP","SPARCTP") ~ "SPARCTP",
          SampleType %in% c("IEV12","SPARC12") ~ "SPARC12",
          TRUE ~ as.character(SampleType)
        )
      )

    # ---------- Keep-sets (genes that pass coverage threshold) 
    # ---------- Ranking helper: filter to top-50% by mean(log2), then compute CV and pick top-5 lowest CV 
    .rank_leastvar <- function(d_ctx, keep_genes, ev_only = FALSE, top_n = 5) {
      d0 <- dplyr::filter(d_ctx, gene_symbol %in% keep_genes)
      if (ev_only) d0 <- dplyr::filter(d0, gene_symbol %in% ev_genes)

      if (!nrow(d0)) return(list(ranked = dplyr::tibble(), top5 = dplyr::tibble()))

      # per-gene stats on present log2 values
      gstats <- d0 %>%
        dplyr::group_by(gene_symbol) %>%
        dplyr::summarise(
          mean_log2 = suppressWarnings(mean(log2_abundance, na.rm = TRUE)),
          sd_log2   = suppressWarnings(stats::sd(log2_abundance, na.rm = TRUE)),
          n_vals    = dplyr::n(),
          .groups   = "drop"
        ) %>%
        dplyr::mutate(
          cv_log2 = dplyr::if_else(is.finite(mean_log2) & abs(mean_log2) > 0,
                                   sd_log2 / abs(mean_log2), NA_real_)
        )

      # abundance filter: keep top 50% by mean_log2 (ties handled naturally)
      cutoff <- stats::quantile(gstats$mean_log2, probs = 0.5, na.rm = TRUE, type = 7)
      gstats_f <- gstats %>% dplyr::filter(is.finite(mean_log2), mean_log2 >= cutoff)

      ranked <- gstats_f %>%
        dplyr::arrange(cv_log2, dplyr::desc(mean_log2)) %>%
        dplyr::mutate(rank_cv = dplyr::row_number())

      top5 <- ranked %>% dplyr::slice_head(n = min(top_n, nrow(ranked)))

      list(ranked = ranked, top5 = top5)
    }

    # ---------- Plot builder (one context, all-genes vs EV-only) 
    .make_leastvar_plot_one <- function(context = c("SPARCTP","SPARC12","Pooled"),
                                        ev_only = FALSE) {
      context <- match.arg(context)

      # Context slice & gene pass-set
      if (context == "SPARCTP") {
        d_ctx   <- df_hd_present %>% dplyr::filter(SampleType == "SPARCTP")
        keep_gs <- keep_SPARCTP
      } else if (context == "SPARC12") {
        d_ctx   <- df_hd_present %>% dplyr::filter(SampleType == "SPARC12")
        keep_gs <- keep_SPARC12
      } else {
        d_ctx   <- df_hd_present
        keep_gs <- keep_POOLED
      }

      if (!length(keep_gs)) {
        warning("No genes pass coverage threshold for context: ", context)
        return(invisible(NULL))
      }

      # Rank (top-50% by mean log2, then lowest CV)
      R <- .rank_leastvar(d_ctx, keep_gs, ev_only = ev_only, top_n = 5)
      if (!nrow(R$ranked)) {
        warning("No genes after abundance (top50%) + CV filtering for ", context,
                " (mode = ", if (ev_only) "EVONLY" else "ALLGENES", ")")
        return(invisible(NULL))
      }

      # Save full ranked list
      if (isTRUE(SAVE)) {
        mode_tag <- if (ev_only) "EVONLY" else "ALLGENES"
        fn_rank  <- paste0(name, "_", date, "_S8_LeastVarRank_", context, "_", mode_tag, "_", thr_file_tag, ".csv")
        safe_write_csv(
          R$ranked %>% dplyr::mutate(Context = context, Mode = mode_tag, .before = 1),
          fn_rank
        )
        message("Saved: ", fn_rank)
      }

      top5 <- R$top5
      if (!nrow(top5)) return(invisible(NULL))

      # Extra per-gene stats for facet labels
      gene_stats <- d_ctx %>%
        dplyr::filter(gene_symbol %in% top5$gene_symbol) %>%
        dplyr::group_by(gene_symbol) %>%
        dplyr::summarise(
          mean_overall = suppressWarnings(mean(log2_abundance, na.rm = TRUE)),
          mean_H       = suppressWarnings(mean(log2_abundance[Disease == "Healthy"], na.rm = TRUE)),
          mean_D       = suppressWarnings(mean(log2_abundance[Disease == "Disease"], na.rm = TRUE)),
          FC           = mean_D - mean_H,
          .groups = "drop"
        )

      labs_df <- top5 %>%
        dplyr::left_join(gene_stats, by = "gene_symbol") %>%
        dplyr::mutate(
          facet_lab = if (.has_ggtext) {
            paste0(
              "<b>", gene_symbol, "</b><br>",
              "<span style='font-size:9pt'>mean = ", sprintf("%.2f", mean_overall), "</span><br>",
              "<span style='font-size:9pt'>CV = ",   sprintf("%.3f", cv_log2),      "</span><br>",
              "<span style='font-size:9pt'>FC = ",   sprintf("%.2f", FC),           "</span>"
            )
          } else {
            paste0(gene_symbol, "\n",
                   "mean = ", sprintf("%.2f", mean_overall), "\n",
                   "CV = ",   sprintf("%.3f", cv_log2),      "\n",
                   "FC = ",   sprintf("%.2f", FC))
          }
        )

      d_plot <- d_ctx %>%
        dplyr::filter(gene_symbol %in% labs_df$gene_symbol) %>%
        dplyr::mutate(
          gene_label = factor(gene_symbol,
                              levels = labs_df$gene_symbol,
                              labels = labs_df$facet_lab)
        )

      # Shared baseline for bold n=Y
      y_limits <- d_plot %>%
        dplyr::summarise(ymin = min(log2_abundance, na.rm = TRUE),
                         ymax = max(log2_abundance, na.rm = TRUE))
      y_range <- if (is.finite(y_limits$ymax - y_limits$ymin)) (y_limits$ymax - y_limits$ymin) else 1
      y_baseline <- y_limits$ymin - max(y_range * 0.12, 0.8)

      counts_pos <- d_plot %>%
        dplyr::count(gene_label, Disease, name = "n") %>%
        dplyr::mutate(y_lab = y_baseline)

      # Titles / subtitle
      ttl_ctx <- if (.has_ggtext) {
        paste0("Least-variable genes (Top 5) â€” ",
               "<span style='color:", title_color_for(context), ";'><b>", context, "</b></span>",
               if (ev_only) " â€” EV markers" else " â€” All genes")
      } else {
        paste0("Least-variable genes (Top 5) â€” ", context,
               if (ev_only) " â€” EV markers" else " â€” All genes")
      }
      thr_short <- thr_for_subtitle(thr_pretty)
      subtitle_txt <- paste0("Threshold ", thr_short, " â€¢ Abundance filter: top 50% by mean log2")

      disease_fills <- c("Healthy" = pal$aqua, "Disease" = pal$purple)

      p <- ggplot(d_plot, aes(x = Disease, y = log2_abundance)) +
        # Boxes and points both fill by Disease (ALL contexts)
        geom_boxplot(aes(fill = Disease), outlier.shape = NA, width = 0.74, color = "black") +
        ggbeeswarm::geom_quasirandom(
          aes(fill = Disease),
          shape = 21, size = 2.2, stroke = 0.6, color = "black",
          width = 0.14, alpha = 0.95, show.legend = TRUE
        ) +
        # Bold n=Y baseline labels
        geom_text(
          data = counts_pos,
          aes(x = Disease, y = y_lab, label = paste0("n=", n)),
          inherit.aes = FALSE, fontface = "bold", size = 3.6, vjust = 1.1, color = "black"
        ) +
        # Disease-only fill scale
        scale_fill_manual(values = disease_fills, drop = FALSE) +
        labs(
          title    = ttl_ctx,
          subtitle = subtitle_txt,
          x = NULL,
          y = "log2 abundance",
          caption  = "Genes ranked by lowest CV (sd/mean) after applying coverage + top-50% abundance filters. CSV includes full ranked list."
        ) +
        facet_wrap(~ gene_label, ncol = 5, scales = "fixed") +
        expand_limits(y = y_baseline) +
        coord_cartesian(clip = "off") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title    = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5, face = "bold")
          else element_text(hjust = 0.5, face = "bold", color = title_color_for(context)),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "right",
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          strip.text = if (.has_ggtext) ggtext::element_markdown(size = 11, lineheight = 1.05)
          else element_text(size = 11),
          axis.text.x = element_text(face = "bold", color = "black"),
          plot.caption = element_text(hjust = 0.5),
          plot.margin  = margin(t = 10, r = 18, b = 22, l = 10)
        )

      print(p)

      if (isTRUE(SAVE)) {
        mode_tag <- if (ev_only) "EVONLY" else "ALLGENES"
        fn_plot  <- paste0(name, "_", date, "_S8_LeastVar_Box_", context, "_", mode_tag, "_", thr_file_tag, ".png")
        ggsave_if(fn_plot, p, width = 10.5, height = 5.0, dpi = 300, bg = "white")
        message("Saved: ", fn_plot)
      }
      invisible(p)
    }


    # ---------- Render all six plots (SPARCTP, SPARC12, Pooled) Ã— (ALLGENES, EVONLY)
    .make_leastvar_plot_one("SPARCTP", ev_only = FALSE)
    .make_leastvar_plot_one("SPARCTP", ev_only = TRUE)
    .make_leastvar_plot_one("SPARC12", ev_only = FALSE)
    .make_leastvar_plot_one("SPARC12", ev_only = TRUE)
    .make_leastvar_plot_one("Pooled",  ev_only = FALSE)
    .make_leastvar_plot_one("Pooled",  ev_only = TRUE)


  }, envir = .GlobalEnv)
  invisible(NULL)
}
run_section8()


#### SECTION 9 â€” Genes of Interest (GOI): SPARCTP, SPARC12, and POOLED ####

run_section9 <- function(save = SAVE) {
  old_save <- SAVE
  on.exit(assign("SAVE", old_save, envir = .GlobalEnv))
  assign("SAVE", save, envir = .GlobalEnv)
  evalq({
    # This version implements:
    # 1) POOLED: points are filled by SPARC (shape 21, black border), boxes filled by Disease â€” no ggnewscale.
    # 2) POOLED SÃ—D plots: only one "Healthy" and one "Disease" x-axis label per facet.
    # 3) POOLED BIG (Disease on x): corrected colors for SPARC points (same scheme as chunks).

    # ---------- Parameters 
    HIGH_CUTOFF <- 17

    # ---------- Utilities / presence of optional packages 
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    # Combined fill palette covering Disease + SPARC (used everywhere for POOLED)
    .fill_values_combined <- c("Healthy" = pal$aqua, "Disease" = pal$purple,
                               "SPARCTP" = pal$tp,   "SPARC12" = pal$i12)

    # Helper used throughout this project
    # Ensure HD-present slice exists (present-only rows with log2_abundance)
    # Chunk a character vector into groups of k
    .chunk_vec <- function(v, k = 5L) {
      if (!length(v)) return(list())
      idx <- split(seq_along(v), ceiling(seq_along(v)/k))
      lapply(idx, function(ii) v[ii])
    }

    # EV-aware facet label (works with NA stats)
    .build_facet_label <- function(sym, mean_overall, cv_log2, FC, ev_flag) {
      mean_txt <- ifelse(is.finite(mean_overall), sprintf("%.2f", mean_overall), "NA")
      cv_txt   <- ifelse(is.finite(cv_log2),     sprintf("%.3f", cv_log2),       "NA")
      fc_txt   <- ifelse(is.finite(FC),          sprintf("%.2f", FC),            "NA")
      if (.has_ggtext) {
        name_part <- if (isTRUE(ev_flag)) {
          paste0("<span style='color:", pal$cyan, ";'><b>", sym, "</b></span>")
        } else paste0("<b>", sym, "</b>")
        paste0(
          name_part, "<br>",
          "<span style='font-size:9pt'>mean = ", mean_txt, "</span><br>",
          "<span style='font-size:9pt'>CV = ",   cv_txt,   "</span><br>",
          "<span style='font-size:9pt'>FC = ",   fc_txt,   "</span>"
        )
      } else {
        paste0(sym, "\n",
               "mean = ", mean_txt, "\n",
               "CV = ",   cv_txt,   "\n",
               "FC = ",   fc_txt)
      }
    }

    # Screen GOI for a context, compute stats for retained genes, and report present/missing
    .prepare_goi_context <- function(context = c("SPARCTP","SPARC12","Pooled"), goi_syms = genes_of_interest) {
      context <- match.arg(context)
      keep_set <- keep_sets[[context]]

      goi_syms <- unique(as.character(goi_syms))
      present  <- intersect(goi_syms, keep_set)
      missing  <- setdiff(goi_syms, keep_set)

      screen_tbl <- tibble::tibble(gene_symbol = goi_syms,
                                   retained    = gene_symbol %in% present)
      if (isTRUE(SAVE)) {
        readr::write_csv(screen_tbl, paste0(name, "_", date, "_S9_GOI_screen_", context, "_", thr_file_tag, ".csv"))
      }
      message("Section 9 â€” ", context, " â€” GOI retained (", length(present), "): ",
              ifelse(length(present)==0,"(none)", paste(present, collapse=", ")))
      message("Section 9 â€” ", context, " â€” GOI missing (", length(missing), "): ",
              ifelse(length(missing)==0,"(none)", paste(missing, collapse=", ")))

      d_ctx <- if (context %in% c("SPARCTP","SPARC12")) {
        df_hd_present %>% dplyr::filter(SampleType == context)
      } else {
        df_hd_present
      }

      stats_ctx <- d_ctx %>%
        dplyr::filter(gene_symbol %in% present) %>%
        dplyr::group_by(gene_symbol) %>%
        dplyr::summarise(
          mean_overall = suppressWarnings(mean(log2_abundance, na.rm = TRUE)),
          sd_overall   = suppressWarnings(stats::sd(log2_abundance, na.rm = TRUE)),
          cv_log2      = dplyr::if_else(is.finite(mean_overall) & abs(mean_overall) > 0,
                                        sd_overall/abs(mean_overall), NA_real_),
          mean_H       = suppressWarnings(mean(log2_abundance[Disease=="Healthy"], na.rm = TRUE)),
          mean_D       = suppressWarnings(mean(log2_abundance[Disease=="Disease"], na.rm = TRUE)),
          FC           = mean_D - mean_H,
          .groups      = "drop"
        )
      if (isTRUE(SAVE)) {
        readr::write_csv(stats_ctx, paste0(name, "_", date, "_S9_GOI_stats_", context, "_", thr_file_tag, ".csv"))
      }
      list(context   = context,
           data_pass = d_ctx %>% dplyr::filter(gene_symbol %in% present),
           stats_ctx = stats_ctx,
           present   = present,
           missing   = missing)
    }

    # Build facet labels for ALL GOI in a context (present get stats; missing get NA)
    .build_labels_for_all <- function(res_ctx, all_goi_syms) {
      s <- res_ctx$stats_ctx %>%
        dplyr::select(gene_symbol, mean_overall, cv_log2, FC)
      tibble::tibble(gene_symbol = all_goi_syms) %>%
        dplyr::left_join(s, by = "gene_symbol") %>%
        dplyr::mutate(
          is_EV     = gene_symbol %in% ev_genes,
          facet_lab = mapply(.build_facet_label, gene_symbol, mean_overall, cv_log2, FC, is_EV,
                             SIMPLIFY = TRUE, USE.NAMES = FALSE)
        ) %>%
        dplyr::select(gene_symbol, facet_lab)
    }

    # Shared plotting core for SPARC/POOLED (x = Disease); supports empty facets via geom_blank
    .make_chunk_plot <- function(d_pass, context_name, labels_map, chunk_syms,
                                 y_limits_global = NULL, chunk_id = 1L, subtitle_tag = NULL) {
      disease_fills <- c("Healthy" = pal$aqua, "Disease" = pal$purple)
      ttl <- if (.has_ggtext) {
        paste0("Genes of Interest â€” ",
               "<span style='color:", title_color_for(context_name), ";'><b>", context_name, "</b></span>")
      } else paste0("Genes of Interest â€” ", context_name)
      subl <- paste0("Threshold ", thr_short, if (!is.null(subtitle_tag)) paste0(" â€¢ ", subtitle_tag) else "")

      # y-limits
      if (is.null(y_limits_global)) {
        y_min <- suppressWarnings(min(d_pass$log2_abundance, na.rm = TRUE))
        y_max <- suppressWarnings(max(d_pass$log2_abundance, na.rm = TRUE))
      } else { y_min <- y_limits_global[1]; y_max <- y_limits_global[2] }
      if (!is.finite(y_min)) y_min <- 0
      if (!is.finite(y_max)) y_max <- 1
      y_rng <- max(y_max - y_min, 1e-6)
      y_lab <- y_min + max(0.02 * y_rng, 0.25)

      lab_vec <- setNames(labels_map$facet_lab, labels_map$gene_symbol)

      d_plot <- d_pass %>% dplyr::filter(gene_symbol %in% chunk_syms) %>%
        dplyr::mutate(gene_label = factor(gene_symbol,
                                          levels = chunk_syms,
                                          labels = unname(lab_vec[chunk_syms])))

      # Dummy rows to force empty facets for genes with no data in this context
      base_df <- tibble::tibble(
        gene_label       = factor(unname(lab_vec[chunk_syms]), levels = unname(lab_vec[chunk_syms])),
        Disease          = factor("Healthy", levels = c("Healthy","Disease")),
        log2_abundance   = NA_real_
      )

      counts_pos <- if (nrow(d_plot)) {
        d_plot %>% dplyr::count(gene_label, Disease, name = "n") %>% dplyr::mutate(y_lab = y_lab)
      } else tibble::tibble()

      p <- ggplot(d_plot, aes(x = Disease, y = log2_abundance)) +
        geom_blank(data = base_df, aes(x = Disease, y = log2_abundance)) +  # ensure empty facets
        geom_boxplot(aes(fill = Disease), outlier.shape = NA, width = 0.74, color = "black") +
        ggbeeswarm::geom_quasirandom(
          aes(fill = Disease),
          shape = 21, size = 2.2, stroke = 0.6, color = "black",
          width = 0.14, alpha = 0.95, show.legend = TRUE
        ) +
        geom_text(
          data = counts_pos,
          aes(x = Disease, y = y_lab, label = paste0("n=", n)),
          inherit.aes = FALSE, fontface = "bold", size = 3.6, vjust = 1.0, color = "black"
        ) +
        scale_fill_manual(values = disease_fills, drop = FALSE, name = "Disease") +
        labs(
          title    = ttl,
          subtitle = subl,
          x = NULL, y = "log2 abundance",
          caption  = "Healthy + Disease present values."
        ) +
        facet_wrap(~ gene_label, ncol = 5, scales = "fixed", drop = FALSE) +
        coord_cartesian(ylim = c(y_min, y_max), clip = "off") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title       = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5, face = "bold")
          else element_text(hjust = 0.5, face = "bold", color = title_color_for(context_name)),
          plot.subtitle    = element_text(hjust = 0.5),
          legend.position  = "right",
          panel.grid.minor = element_blank(),
          panel.spacing.y  = grid::unit(1.2, "lines"),     # extra row spacing
          strip.background = element_blank(),
          strip.text       = if (.has_ggtext) ggtext::element_markdown(size = 11, lineheight = 1.05)
          else element_text(size = 11),
          axis.text.x      = element_text(face = "bold", color = "black"),
          plot.caption     = element_text(hjust = 0.5),
          plot.margin      = margin(t = 10, r = 18, b = 22, l = 10)
        )

      print(p)
      fn <- paste0(name, "_", date, "_S9_GOI_Box_", context_name, "_chunk", sprintf("%02d", chunk_id), "_", thr_file_tag, ".png")
      if (isTRUE(SAVE)) ggsave_if(fn, p, width = 10.5, height = 5.0, dpi = 300, bg = "white")
      invisible(p)
    }

    # --- POOLED chunk plot (x = Disease) with boxes by Disease and points by SPARC (filled, black border)
    .make_chunk_plot_pooled_byDisease <- function(d_pass, labels_map, chunk_syms,
                                                  y_limits_global = NULL, chunk_id = 1L, subtitle_tag = NULL) {
      context_name <- "Pooled"
      ttl <- "Genes of Interest â€” Pooled"
      subl <- paste0("Threshold ", thr_short, if (!is.null(subtitle_tag)) paste0(" â€¢ ", subtitle_tag) else "")

      # y-limits
      if (is.null(y_limits_global)) {
        y_min <- suppressWarnings(min(d_pass$log2_abundance, na.rm = TRUE))
        y_max <- suppressWarnings(max(d_pass$log2_abundance, na.rm = TRUE))
      } else { y_min <- y_limits_global[1]; y_max <- y_limits_global[2] }
      if (!is.finite(y_min)) y_min <- 0
      if (!is.finite(y_max)) y_max <- 1
      y_rng <- max(y_max - y_min, 1e-6)
      y_lab <- y_min + max(0.02 * y_rng, 0.25)

      lab_vec <- setNames(labels_map$facet_lab, labels_map$gene_symbol)

      d_plot <- d_pass %>% dplyr::filter(gene_symbol %in% chunk_syms) %>%
        dplyr::mutate(gene_label = factor(gene_symbol,
                                          levels = chunk_syms,
                                          labels = unname(lab_vec[chunk_syms])))

      base_df <- tibble::tibble(
        gene_label       = factor(unname(lab_vec[chunk_syms]), levels = unname(lab_vec[chunk_syms])),
        Disease          = factor("Healthy", levels = c("Healthy","Disease")),
        SampleType       = factor("SPARCTP", levels = c("SPARCTP","SPARC12")),
        log2_abundance   = NA_real_
      )

      counts_pos <- if (nrow(d_plot)) {
        d_plot %>% dplyr::count(gene_label, Disease, name = "n") %>% dplyr::mutate(y_lab = y_lab)
      } else tibble::tibble()

      p <- ggplot(d_plot, aes(x = Disease, y = log2_abundance)) +
        geom_blank(data = base_df, aes(x = Disease, y = log2_abundance)) +
        # Boxes by Disease (fill uses Disease keys)
        geom_boxplot(aes(fill = Disease), outlier.shape = NA, width = 0.74, color = "black", show.legend = TRUE) +
        # Points by SPARC (fill uses SPARC keys), shape 21 w/ black border
        ggbeeswarm::geom_quasirandom(
          aes(fill = SampleType),
          shape = 21, size = 2.2, stroke = 0.6, color = "black",
          width = 0.14, alpha = 0.95, show.legend = TRUE
        ) +
        geom_text(
          data = counts_pos,
          aes(x = Disease, y = y_lab, label = paste0("n=", n)),
          inherit.aes = FALSE, fontface = "bold", size = 3.6, vjust = 1.0, color = "black"
        ) +
        # Single fill scale for both sets of categories
        scale_fill_manual(values = .fill_values_combined,
                          breaks = c("Healthy","Disease","SPARCTP","SPARC12"),
                          name = NULL,
                          drop = FALSE) +
        labs(
          title    = ttl,
          subtitle = subl,
          x = NULL, y = "log2 abundance",
          caption  = "Healthy + Disease present values. Boxes by Disease; points by SPARC."
        ) +
        facet_wrap(~ gene_label, ncol = 5, scales = "fixed", drop = FALSE) +
        coord_cartesian(ylim = c(y_min, y_max), clip = "off") +
        guides(fill = guide_legend(override.aes = list(shape = c(22,22,21,21),
                                                       size  = c(4,4,3,3),
                                                       color = c("black","black","black","black")))) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title       = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle    = element_text(hjust = 0.5),
          legend.position  = "right",
          panel.grid.minor = element_blank(),
          panel.spacing.y  = grid::unit(1.2, "lines"),
          strip.background = element_blank(),
          strip.text       = if (.has_ggtext) ggtext::element_markdown(size = 11, lineheight = 1.05)
          else element_text(size = 11),
          axis.text.x      = element_text(face = "bold", color = "black"),
          plot.caption     = element_text(hjust = 0.5),
          plot.margin      = margin(t = 10, r = 18, b = 22, l = 10)
        )

      print(p)
      fn <- paste0(name, "_", date, "_S9_GOI_Box_Pooled_chunk", sprintf("%02d", chunk_id), "_Dx", "_", thr_file_tag, ".png")
      if (isTRUE(SAVE)) ggsave_if(fn, p, width = 10.5, height = 5.0, dpi = 300, bg = "white")
      invisible(p)
    }

    # --- POOLED chunk plot with x = (SPARC Ã— Disease), boxes by Disease and points by SPARC
    .make_chunk_plot_pooled_DxS <- function(d_pass, labels_map, chunk_syms,
                                            y_limits_global = NULL, chunk_id = 1L, subtitle_tag = NULL) {
      ttl  <- "Genes of Interest â€” Pooled (by SPARC Ã— Disease)"
      subl <- paste0("Threshold ", thr_short, if (!is.null(subtitle_tag)) paste0(" â€¢ ", subtitle_tag) else "")

      d_plot <- d_pass %>%
        dplyr::filter(gene_symbol %in% chunk_syms) %>%
        dplyr::mutate(
          Group = factor(paste(SampleType, Disease, sep = " - "),
                         levels = c("SPARCTP - Healthy","SPARC12 - Healthy","SPARCTP - Disease","SPARC12 - Disease")),
          Disease    = factor(Disease,    levels = c("Healthy","Disease")),
          SampleType = factor(SampleType, levels = c("SPARCTP","SPARC12"))
        )

      if (is.null(y_limits_global)) {
        y_min <- suppressWarnings(min(d_plot$log2_abundance, na.rm = TRUE))
        y_max <- suppressWarnings(max(d_plot$log2_abundance, na.rm = TRUE))
      } else { y_min <- y_limits_global[1]; y_max <- y_limits_global[2] }
      if (!is.finite(y_min)) y_min <- 0
      if (!is.finite(y_max)) y_max <- 1
      y_rng <- max(y_max - y_min, 1e-6)
      y_lab <- y_min + max(0.02 * y_rng, 0.25)

      lab_vec <- setNames(labels_map$facet_lab, labels_map$gene_symbol)
      d_plot <- d_plot %>%
        dplyr::mutate(gene_label = factor(gene_symbol,
                                          levels = chunk_syms,
                                          labels = unname(lab_vec[chunk_syms])))

      base_df <- tibble::tibble(
        gene_label       = factor(unname(lab_vec[chunk_syms]), levels = unname(lab_vec[chunk_syms])),
        Group            = factor("SPARCTP - Healthy",
                                  levels = c("SPARCTP - Healthy","SPARC12 - Healthy","SPARCTP - Disease","SPARC12 - Disease")),
        Disease          = factor("Healthy", levels = c("Healthy","Disease")),
        SampleType       = factor("SPARCTP", levels = c("SPARCTP","SPARC12")),
        log2_abundance   = NA_real_
      )

      counts_pos <- if (nrow(d_plot)) {
        d_plot %>% dplyr::count(gene_label, Group, name = "n") %>% dplyr::mutate(y_lab = y_lab)
      } else tibble::tibble()

      # Only two axis labels per facet (first SPARC of each disease pair)
      breaks_simple <- c("SPARCTP - Healthy","SPARCTP - Disease")
      labels_simple <- c("Healthy","Disease")

      p <- ggplot(d_plot, aes(x = Group, y = log2_abundance)) +
        geom_blank(data = base_df, aes(x = Group, y = log2_abundance)) +
        geom_boxplot(aes(fill = Disease), outlier.shape = NA, width = 0.74, color = "black", show.legend = TRUE) +
        ggbeeswarm::geom_quasirandom(
          aes(fill = SampleType),
          shape = 21, size = 2.2, stroke = 0.6, color = "black",
          width = 0.14, alpha = 0.95, show.legend = TRUE
        ) +
        geom_text(
          data = counts_pos,
          aes(x = Group, y = y_lab, label = paste0("n=", n)),
          inherit.aes = FALSE, fontface = "bold", size = 3.4, vjust = 1.0, color = "black"
        ) +
        scale_x_discrete(breaks = breaks_simple, labels = labels_simple, drop = FALSE) +
        scale_fill_manual(values = .fill_values_combined,
                          breaks = c("Healthy","Disease","SPARCTP","SPARC12"),
                          name = NULL,
                          drop = FALSE) +
        guides(fill = guide_legend(override.aes = list(shape = c(22,22,21,21),
                                                       size  = c(4,4,3,3),
                                                       color = c("black","black","black","black")))) +
        labs(
          title    = ttl,
          subtitle = subl,
          x = NULL, y = "log2 abundance",
          caption  = "Boxes by Disease; points by SPARC. X-axis shows Disease only; SPARC is indicated by the legend."
        ) +
        facet_wrap(~ gene_label, ncol = 5, scales = "fixed", drop = FALSE) +
        coord_cartesian(ylim = c(y_min, y_max), clip = "off") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title       = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle    = element_text(hjust = 0.5),
          legend.position  = "right",
          panel.grid.minor = element_blank(),
          panel.spacing.y  = grid::unit(1.2, "lines"),
          strip.background = element_blank(),
          strip.text       = if (.has_ggtext) ggtext::element_markdown(size = 11, lineheight = 1.05)
          else element_text(size = 11),
          axis.text.x      = element_text(face = "bold", lineheight = 0.95),
          plot.caption     = element_text(hjust = 0.5),
          plot.margin      = margin(t = 10, r = 18, b = 22, l = 10)
        )

      print(p)
      fn <- paste0(name, "_", date, "_S9_GOI_Box_Pooled_SxD_chunk", sprintf("%02d", chunk_id), "_", thr_file_tag, ".png")
      if (isTRUE(SAVE)) ggsave_if(fn, p, width = 10.5, height = 5.0, dpi = 300, bg = "white")
      invisible(p)
    }

    # BIG plot for one context (x = Disease) â€” supports points_by_sparc for POOLED; blank facets; extra row spacing
    .make_big_plot_context <- function(d_pass, context_name, labels_map, all_syms_ordered, abundance_tag = c("LOW","HIGH"),
                                       points_by_sparc = FALSE) {
      abundance_tag <- match.arg(abundance_tag)
      if (!length(all_syms_ordered)) return(invisible(NULL))

      disease_fills <- c("Healthy" = pal$aqua, "Disease" = pal$purple)
      ttl <- if (.has_ggtext) {
        paste0("Genes of Interest â€” ",
               "<span style='color:", title_color_for(context_name), ";'><b>", context_name, "</b></span>")
      } else paste0("Genes of Interest â€” ", context_name)
      subl <- paste0("Threshold ", thr_short, " â€¢ ", abundance_tag, " abundance (n=", length(all_syms_ordered), " genes)")

      # y-limits from this contextâ€™s data
      y_min <- suppressWarnings(min(d_pass$log2_abundance, na.rm = TRUE))
      y_max <- suppressWarnings(max(d_pass$log2_abundance, na.rm = TRUE))
      if (!is.finite(y_min)) y_min <- 0
      if (!is.finite(y_max)) y_max <- 1
      y_rng <- max(y_max - y_min, 1e-6)
      y_lab <- y_min + max(0.02 * y_rng, 0.25)

      lab_vec <- setNames(labels_map$facet_lab, labels_map$gene_symbol)

      d_plot <- d_pass %>%
        dplyr::filter(gene_symbol %in% all_syms_ordered) %>%
        dplyr::mutate(gene_label = factor(gene_symbol,
                                          levels = all_syms_ordered,
                                          labels = unname(lab_vec[all_syms_ordered])))

      counts_pos <- if (nrow(d_plot)) {
        d_plot %>% dplyr::count(gene_label, Disease, name = "n") %>% dplyr::mutate(y_lab = y_lab)
      } else tibble::tibble()

      # Dummy rows to force empty facets
      base_df <- tibble::tibble(
        gene_label       = factor(unname(lab_vec[all_syms_ordered]),
                                  levels = unname(lab_vec[all_syms_ordered])),
        Disease          = factor("Healthy", levels = c("Healthy","Disease")),
        SampleType       = factor("SPARCTP", levels = c("SPARCTP","SPARC12")),
        log2_abundance   = NA_real_
      )

      cap_txt <- stringr::str_wrap(paste0(
        "Coverage threshold: ", thr_pretty, ". ",
        if (abundance_tag == "LOW") paste0("LOW abundance = combined mean log2 â‰¤ ", HIGH_CUTOFF, ". ")
        else paste0("HIGH abundance = combined mean log2 > ", HIGH_CUTOFF, ". "),
        "Blank facet = gene is in GOI but FAILED the coverage threshold in this context."
      ), width = 100)

      p <- ggplot(d_plot, aes(x = Disease, y = log2_abundance)) +
        geom_blank(data = base_df, aes(x = Disease, y = log2_abundance)) +
        geom_boxplot(aes(fill = Disease), outlier.shape = NA, width = 0.74, color = "black", show.legend = TRUE) +
        {
          if (points_by_sparc) {
            ggbeeswarm::geom_quasirandom(
              aes(fill = SampleType),
              shape = 21, size = 2.2, stroke = 0.6, color = "black",
              width = 0.14, alpha = 0.95, show.legend = TRUE
            )
          } else {
            ggbeeswarm::geom_quasirandom(
              aes(fill = Disease),
              shape = 21, size = 2.2, stroke = 0.6, color = "black",
              width = 0.14, alpha = 0.95, show.legend = TRUE
            )
          }
        } +
        # Unified fill scale (covers Disease + SPARC so colors are consistent with chunks)
        scale_fill_manual(values = if (points_by_sparc) .fill_values_combined else c("Healthy"=pal$aqua,"Disease"=pal$purple),
                          breaks = if (points_by_sparc) c("Healthy","Disease","SPARCTP","SPARC12") else c("Healthy","Disease"),
                          name   = NULL,
                          drop   = FALSE) +
        { if (points_by_sparc)
          guides(fill = guide_legend(override.aes = list(shape = c(22,22,21,21),
                                                         size  = c(4,4,3,3),
                                                         color = c("black","black","black","black"))))
          else guides(fill = guide_legend(override.aes = list(shape = c(22,22),
                                                              size  = c(4,4),
                                                              color = c("black","black")))) } +
        geom_text(
          data = counts_pos,
          aes(x = Disease, y = y_lab, label = paste0("n=", n)),
          inherit.aes = FALSE, fontface = "bold", size = 3.6, vjust = 1.0, color = "black"
        ) +
        labs(title = ttl, subtitle = subl, x = NULL, y = "log2 abundance", caption = cap_txt) +
        facet_wrap(~ gene_label, ncol = 5, scales = "fixed", drop = FALSE) +
        coord_cartesian(ylim = c(y_min, y_max), clip = "off") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title       = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5, face = "bold")
          else element_text(hjust = 0.5, face = "bold", color = title_color_for(context_name)),
          plot.subtitle    = element_text(hjust = 0.5),
          legend.position  = "right",
          panel.grid.minor = element_blank(),
          panel.spacing.y  = grid::unit(1.4, "lines"),   # extra space between rows
          strip.background = element_blank(),
          strip.text       = if (.has_ggtext) ggtext::element_markdown(size = 11, lineheight = 1.05)
          else element_text(size = 11),
          axis.text.x      = element_text(face = "bold", color = "black"),
          plot.caption     = element_text(hjust = 0.5),
          plot.margin      = margin(t = 10, r = 18, b = 22, l = 10)
        )

      print(p)
      if (isTRUE(SAVE)) {
        n_rows <- ceiling(length(all_syms_ordered) / 5)
        fn <- paste0(name, "_", date, "_S9_GOI_BigPlot_", abundance_tag, "_", context_name, "_", thr_file_tag, ".png")
        ggsave_if(fn, p, width = 12.5, height = max(6.0, n_rows * 5.2), dpi = 300, bg = "white")
      }
      invisible(p)
    }

    # BIG plot for POOLED with x = (SPARC Ã— Disease) â€” boxes fill by Disease; points fill by SPARC
    .make_big_plot_pooled_DxS <- function(d_pass, labels_map, all_syms_ordered, abundance_tag = c("LOW","HIGH")) {
      abundance_tag <- match.arg(abundance_tag)
      if (!length(all_syms_ordered)) return(invisible(NULL))

      d_plot <- d_pass %>%
        dplyr::filter(gene_symbol %in% all_syms_ordered) %>%
        dplyr::mutate(
          Group = factor(paste(SampleType, Disease, sep = " - "),
                         levels = c("SPARCTP - Healthy","SPARC12 - Healthy",
                                    "SPARCTP - Disease","SPARC12 - Disease")),
          Disease    = factor(Disease,    levels = c("Healthy","Disease")),
          SampleType = factor(SampleType, levels = c("SPARCTP","SPARC12"))
        )

      y_min <- suppressWarnings(min(d_plot$log2_abundance, na.rm = TRUE))
      y_max <- suppressWarnings(max(d_plot$log2_abundance, na.rm = TRUE))
      if (!is.finite(y_min)) y_min <- 0
      if (!is.finite(y_max)) y_max <- 1
      y_rng <- max(y_max - y_min, 1e-6)
      y_lab <- y_min + max(0.02 * y_rng, 0.25)

      lab_vec <- setNames(labels_map$facet_lab, labels_map$gene_symbol)
      d_plot <- d_plot %>%
        dplyr::mutate(gene_label = factor(gene_symbol,
                                          levels = all_syms_ordered,
                                          labels = unname(lab_vec[all_syms_ordered])))

      base_df <- tibble::tibble(
        gene_label       = factor(unname(lab_vec[all_syms_ordered]),
                                  levels = unname(lab_vec[all_syms_ordered])),
        Group            = factor("SPARCTP - Healthy",
                                  levels = c("SPARCTP - Healthy","SPARC12 - Healthy",
                                             "SPARCTP - Disease","SPARC12 - Disease")),
        Disease          = factor("Healthy", levels = c("Healthy","Disease")),
        SampleType       = factor("SPARCTP", levels = c("SPARCTP","SPARC12")),
        log2_abundance   = NA_real_
      )

      counts_pos <- if (nrow(d_plot)) {
        d_plot %>% dplyr::count(gene_label, Group, name = "n") %>% dplyr::mutate(y_lab = y_lab)
      } else tibble::tibble()

      breaks_simple <- c("SPARCTP - Healthy","SPARCTP - Disease")
      labels_simple <- c("Healthy","Disease")

      cap_txt <- stringr::str_wrap(paste0(
        "Coverage threshold: ", thr_pretty, ". ",
        if (abundance_tag == "LOW") paste0("LOW abundance = combined mean log2 â‰¤ ", HIGH_CUTOFF, ". ")
        else paste0("HIGH abundance = combined mean log2 > ", HIGH_CUTOFF, ". "),
        "Blank facet = gene is in GOI but FAILED the pooled coverage threshold."
      ), width = 100)

      ttl <- "Genes of Interest â€” Pooled (by SPARC Ã— Disease)"
      subl <- paste0("Threshold ", thr_short, " â€¢ ", abundance_tag, " abundance (n=", length(all_syms_ordered), " genes)")

      p <- ggplot(d_plot, aes(x = Group, y = log2_abundance)) +
        geom_blank(data = base_df, aes(x = Group, y = log2_abundance)) +
        geom_boxplot(aes(fill = Disease), outlier.shape = NA, width = 0.74, color = "black", show.legend = TRUE) +
        ggbeeswarm::geom_quasirandom(
          aes(fill = SampleType),
          shape = 21, size = 2.2, stroke = 0.6, color = "black",
          width = 0.14, alpha = 0.95, show.legend = TRUE
        ) +
        geom_text(
          data = counts_pos,
          aes(x = Group, y = y_lab, label = paste0("n=", n)),
          inherit.aes = FALSE, fontface = "bold", size = 3.4, vjust = 1.0, color = "black"
        ) +
        scale_x_discrete(breaks = breaks_simple, labels = labels_simple, drop = FALSE) +
        scale_fill_manual(values = .fill_values_combined,
                          breaks = c("Healthy","Disease","SPARCTP","SPARC12"),
                          name = NULL,
                          drop = FALSE) +
        guides(fill = guide_legend(override.aes = list(shape = c(22,22,21,21),
                                                       size  = c(4,4,3,3),
                                                       color = c("black","black","black","black")))) +
        labs(title = ttl, subtitle = subl, x = NULL, y = "log2 abundance", caption = cap_txt) +
        facet_wrap(~ gene_label, ncol = 5, scales = "fixed", drop = FALSE) +
        coord_cartesian(ylim = c(y_min, y_max), clip = "off") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title       = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle    = element_text(hjust = 0.5),
          legend.position  = "right",
          panel.grid.minor = element_blank(),
          panel.spacing.y  = grid::unit(1.4, "lines"),
          strip.background = element_blank(),
          strip.text       = if (.has_ggtext) ggtext::element_markdown(size = 11, lineheight = 1.05)
          else element_text(size = 11),
          axis.text.x      = element_text(face = "bold", lineheight = 0.95),
          plot.caption     = element_text(hjust = 0.5),
          plot.margin      = margin(t = 10, r = 18, b = 22, l = 10)
        )

      print(p)
      if (isTRUE(SAVE)) {
        n_rows <- ceiling(length(all_syms_ordered) / 5)
        fn <- paste0(name, "_", date, "_S9_GOI_BigPlot_", abundance_tag, "_Pooled_SxD_", thr_file_tag, ".png")
        ggsave_if(fn, p, width = 13.5, height = max(6.0, n_rows * 5.2), dpi = 300, bg = "white")
      }
      invisible(p)
    }

    # ---------- Pipeline 
    goi_syms <- unique(as.character(genes_of_interest))

    # Prepare all three contexts
    res_TP   <- .prepare_goi_context("SPARCTP", goi_syms)
    res_S12  <- .prepare_goi_context("SPARC12", goi_syms)
    res_POOL <- .prepare_goi_context("Pooled",  goi_syms)

    # Combined mean ranking across SPARCTP+SPARC12 (present-only)
    combined_means <- dplyr::bind_rows(
      res_TP$data_pass  %>% dplyr::select(gene_symbol, log2_abundance),
      res_S12$data_pass %>% dplyr::select(gene_symbol, log2_abundance)
    ) %>%
      dplyr::group_by(gene_symbol) %>%
      dplyr::summarise(combined_mean_log2 = suppressWarnings(mean(log2_abundance, na.rm = TRUE)),
                       .groups = "drop")

    goi_order_tbl <- tibble::tibble(gene_symbol = goi_syms) %>%
      dplyr::left_join(combined_means, by = "gene_symbol") %>%
      dplyr::arrange(dplyr::desc(combined_mean_log2))  # NAs last

    master_order <- goi_order_tbl$gene_symbol
    high_syms <- goi_order_tbl %>% dplyr::filter(is.finite(combined_mean_log2) & combined_mean_log2 > HIGH_CUTOFF) %>%
      dplyr::pull(gene_symbol)
    low_syms  <- setdiff(master_order, high_syms)

    # Shared y-limits across SPARCs (for chunk plots) within LOW and within HIGH
    .get_shared_limits <- function(sym_subset) {
      if (!length(sym_subset)) return(c(NA_real_, NA_real_))
      d1 <- res_TP$data_pass  %>% dplyr::filter(gene_symbol %in% sym_subset)
      d2 <- res_S12$data_pass %>% dplyr::filter(gene_symbol %in% sym_subset)
      y_min <- suppressWarnings(min(c(d1$log2_abundance, d2$log2_abundance), na.rm = TRUE))
      y_max <- suppressWarnings(max(c(d1$log2_abundance, d2$log2_abundance), na.rm = TRUE))
      if (!is.finite(y_min) || !is.finite(y_max)) c(NA_real_, NA_real_) else c(y_min, y_max)
    }
    lims_low  <- .get_shared_limits(low_syms)
    lims_high <- .get_shared_limits(high_syms)

    # Label maps (ALL GOI, per context)
    labels_all_TP   <- .build_labels_for_all(res_TP,   master_order)
    labels_all_S12  <- .build_labels_for_all(res_S12,  master_order)
    labels_all_POOL <- .build_labels_for_all(res_POOL, master_order)

    # ---- Chunked plots (LOW) â€” SPARCTP, SPARC12, POOLED
    low_chunks <- .chunk_vec(low_syms, 5L)
    for (i in seq_along(low_chunks)) {
      syms <- low_chunks[[i]]
      # SPARCs
      .make_chunk_plot(res_TP$data_pass   %>% dplyr::filter(gene_symbol %in% syms),
                       "SPARCTP", labels_all_TP  %>% dplyr::filter(gene_symbol %in% syms),
                       syms, if (all(is.finite(lims_low))) lims_low else NULL, i, "LOW abundance")
      .make_chunk_plot(res_S12$data_pass  %>% dplyr::filter(gene_symbol %in% syms),
                       "SPARC12", labels_all_S12 %>% dplyr::filter(gene_symbol %in% syms),
                       syms, if (all(is.finite(lims_low))) lims_low else NULL, i, "LOW abundance")
      # POOLED: (a) x = Disease, (b) x = (SPARC Ã— Disease)
      .make_chunk_plot_pooled_byDisease(res_POOL$data_pass %>% dplyr::filter(gene_symbol %in% syms),
                                        labels_all_POOL %>% dplyr::filter(gene_symbol %in% syms),
                                        syms, NULL, i, "LOW abundance")
      .make_chunk_plot_pooled_DxS(res_POOL$data_pass %>% dplyr::filter(gene_symbol %in% syms),
                                  labels_all_POOL %>% dplyr::filter(gene_symbol %in% syms),
                                  syms, NULL, i, "LOW abundance")
    }

    # ---- Chunked plots (HIGH) â€” SPARCTP, SPARC12, POOLED
    high_chunks <- .chunk_vec(high_syms, 5L)
    for (i in seq_along(high_chunks)) {
      syms <- high_chunks[[i]]
      # SPARCs
      .make_chunk_plot(res_TP$data_pass   %>% dplyr::filter(gene_symbol %in% syms),
                       "SPARCTP", labels_all_TP  %>% dplyr::filter(gene_symbol %in% syms),
                       syms, if (all(is.finite(lims_high))) lims_high else NULL, i, "HIGH abundance")
      .make_chunk_plot(res_S12$data_pass  %>% dplyr::filter(gene_symbol %in% syms),
                       "SPARC12", labels_all_S12 %>% dplyr::filter(gene_symbol %in% syms),
                       syms, if (all(is.finite(lims_high))) lims_high else NULL, i, "HIGH abundance")
      # POOLED
      .make_chunk_plot_pooled_byDisease(res_POOL$data_pass %>% dplyr::filter(gene_symbol %in% syms),
                                        labels_all_POOL %>% dplyr::filter(gene_symbol %in% syms),
                                        syms, NULL, i, "HIGH abundance")
      .make_chunk_plot_pooled_DxS(res_POOL$data_pass %>% dplyr::filter(gene_symbol %in% syms),
                                  labels_all_POOL %>% dplyr::filter(gene_symbol %in% syms),
                                  syms, NULL, i, "HIGH abundance")
    }

    # ---- BIG plots (x = Disease) â€” only if >5 genes in that abundance group
    build_big_plot_if_needed <- function(syms, context_res, labels_map, context_name, abundance_tag, points_by_sparc = FALSE) {
      if (length(syms) <= 5) return(NULL)
      .make_big_plot_context(
        d_pass           = context_res$data_pass %>% dplyr::filter(gene_symbol %in% syms),
        context_name     = context_name,
        labels_map       = labels_map %>% dplyr::filter(gene_symbol %in% syms),
        all_syms_ordered = syms,
        abundance_tag    = abundance_tag,
        points_by_sparc  = points_by_sparc
      )
    }
    # LOW
    p_big_low_TP   <- build_big_plot_if_needed(low_syms,  res_TP,   labels_all_TP,   "SPARCTP", "LOW",  points_by_sparc = FALSE)
    p_big_low_S12  <- build_big_plot_if_needed(low_syms,  res_S12,  labels_all_S12,  "SPARC12", "LOW",  points_by_sparc = FALSE)
    p_big_low_POOL <- build_big_plot_if_needed(low_syms,  res_POOL, labels_all_POOL, "Pooled",  "LOW",  points_by_sparc = TRUE)
    # HIGH
    p_big_high_TP   <- build_big_plot_if_needed(high_syms, res_TP,   labels_all_TP,   "SPARCTP", "HIGH", points_by_sparc = FALSE)
    p_big_high_S12  <- build_big_plot_if_needed(high_syms, res_S12,  labels_all_S12,  "SPARC12", "HIGH", points_by_sparc = FALSE)
    p_big_high_POOL <- build_big_plot_if_needed(high_syms, res_POOL, labels_all_POOL, "Pooled",  "HIGH", points_by_sparc = TRUE)

    # ---- BIG plots for POOLED with x = (SPARC Ã— Disease) â€” only if >5 genes
    build_big_pooled_SxD_if_needed <- function(syms, context_res, labels_map, abundance_tag) {
      if (length(syms) <= 5) return(NULL)
      .make_big_plot_pooled_DxS(
        d_pass           = context_res$data_pass %>% dplyr::filter(gene_symbol %in% syms),
        labels_map       = labels_map %>% dplyr::filter(gene_symbol %in% syms),
        all_syms_ordered = syms,
        abundance_tag    = abundance_tag
      )
    }
    p_big_low_POOL_SxD  <- build_big_pooled_SxD_if_needed(low_syms,  res_POOL, labels_all_POOL, "LOW")
    p_big_high_POOL_SxD <- build_big_pooled_SxD_if_needed(high_syms, res_POOL, labels_all_POOL, "HIGH")




  }, envir = .GlobalEnv)
  invisible(NULL)
}
run_section9()


#### SECTION 10 â€” SPARC vs SPARC comparisons (Intersection & Unique; SPARC-grouped + SÃ—D where requested) ----

run_section10 <- function(save = SAVE) {
  old_save <- SAVE
  on.exit(assign("SAVE", old_save, envir = .GlobalEnv))
  assign("SAVE", save, envir = .GlobalEnv)
  evalq({
    # Assumes Section 0â€“3A objects & helpers exist:
    #   pal, title_color_for(), threshold_label(), thr_for_subtitle(), thr_current, thr_pretty, thr_label_short,
    #   presence_by_type, presence_overall, ev_genes, df, df_hd_present (HD-only present rows, with log2_abundance),
    #   SAVE, name, date, .has_ggtext, ggsave_if(), fmt_st_bold(), etc.


    # 10.0 â€” Utilities & Sets

    genes_INTER   <- intersect(keep_SPARCTP, keep_SPARC12)
    genes_TPonly  <- setdiff(keep_SPARCTP, keep_SPARC12)
    genes_12only  <- setdiff(keep_SPARC12, keep_SPARCTP)
    # Unified fills
    .fill_values_combined <- c("Healthy" = pal$aqua, "Disease" = pal$purple,
                               "SPARCTP" = pal$tp,   "SPARC12" = pal$i12)

    # EV-aware facet-label (VECTORIZED). If EV gene, color the ENTIRE label cyan. 4-line format.
    .label4_ev <- function(sym, line2 = "", line3 = "", line4 = "") {
      sym      <- as.character(sym)
      line2    <- ifelse(is.na(line2), "", line2)
      line3    <- ifelse(is.na(line3), "", line3)
      line4    <- ifelse(is.na(line4), "", line4)
      is_ev    <- sym %in% ev_genes
      if (.has_ggtext) {
        content <- paste0("<b>", sym, "</b>",
                          ifelse(nchar(line2)>0, "<br>", ""), line2,
                          ifelse(nchar(line3)>0, "<br>", ""), line3,
                          ifelse(nchar(line4)>0, "<br>", ""), line4)
        ifelse(is_ev,
               paste0("<span style='color:#48BFE3;'>", content, "</span>"),
               content)
      } else {
        paste0(sym,
               ifelse(nchar(line2)>0, "\n", ""), line2,
               ifelse(nchar(line3)>0, "\n", ""), line3,
               ifelse(nchar(line4)>0, "\n", ""), line4)
      }
    }

    # SÃ—D order (requested)
    .make_SxD_group <- function(st, dz) factor(
      paste(st, dz, sep = " - "),
      levels = c("SPARC12 - Healthy","SPARC12 - Disease","SPARCTP - Healthy","SPARCTP - Disease")
    )

    # Axis helpers for SÃ—D layouts (with â€œ(n)â€ labels + centered Healthy/Disease headers)
    .baseline_and_labels <- function(d_plot) {
      y_min <- suppressWarnings(min(d_plot$log2_abundance, na.rm = TRUE)); if (!is.finite(y_min)) y_min <- 0
      y_max <- suppressWarnings(max(d_plot$log2_abundance, na.rm = TRUE)); if (!is.finite(y_max)) y_max <- 1
      rng <- max(y_max - y_min, 1e-6)
      list(y_min = y_min, y_max = y_max, y_lab = y_min + max(0.02*rng, 0.22))
    }
    .make_nlabel_df <- function(d_plot, facet_key = "gene_label", x_key = c("Disease","Group"), y_lab) {
      x_key <- match.arg(x_key)
      d_plot %>%
        dplyr::count(.data[[facet_key]], .data[[x_key]], name = "n") %>%
        dplyr::mutate(y_lab = y_lab, n_lab = paste0("(", n, ")"))
    }
    .make_centered_SxD_labels_df <- function(d_plot, facet_key = "gene_label", y_min, y_pad = 0.05) {
      d_plot %>%
        dplyr::distinct(.data[[facet_key]]) %>%
        dplyr::mutate(x = list(c(1.5, 3.5)), lab = list(c("Healthy","Disease"))) %>%
        tidyr::unnest(cols = c(x, lab)) %>%
        dplyr::rename(gene_label = !!facet_key) %>%
        dplyr::mutate(y = y_min - y_pad)
    }

    # Safe CSV
    # Dynamic height (prevents squishing when multiple facet rows)
    .dynamic_height <- function(n_facets, ncol = 5, base_h = 5.2, extra = 0) {
      nrows <- max(1L, ceiling(n_facets / ncol))
      base_h * nrows + extra
    }

    # Caption theming: center + 95% width if ggtext is available
    .caption_theme <- function() {
      if (.has_ggtext) ggtext::element_textbox_simple(width = grid::unit(0.95, "npc"), halign = 0.5, margin = margin(t = 6))
      else element_text(hjust = 0.5)
    }


    # 10.1 â€” Venn diagram (ggforce two-circle; legend title removed)

    .make_venn_sparc <- function() {
      keep_TP <- keep_SPARCTP
      keep_12 <- keep_SPARC12
      n_tp   <- length(keep_TP)
      n_12   <- length(keep_12)
      n_int  <- length(intersect(keep_TP, keep_12))
      n_tp_u <- n_tp - n_int
      n_12_u <- n_12 - n_int

      library(ggplot2); library(ggforce)
      df_circles <- tibble::tibble(
        x0 = c(-1, 1), y0 = c(0, 0), r = c(2.2, 2.2),
        grp = factor(c("SPARCTP","SPARC12"), levels = c("SPARCTP","SPARC12"))
      )

      sub_md <- if (.has_ggtext) {
        paste0(
          "<span style='color:", pal$tp,  ";'><b>SPARCTP</b></span> vs ",
          "<span style='color:", pal$i12, ";'><b>SPARC12</b></span> â€¢ Threshold ", thr_short
        )
      } else paste0("SPARCTP vs SPARC12 â€¢ Threshold ", thr_short)

      p <- ggplot() +
        ggforce::geom_circle(
          data = df_circles,
          aes(x0 = x0, y0 = y0, r = r, fill = grp, color = grp),
          alpha = 0.25, linewidth = 0.6
        ) +
        scale_fill_manual(values = c("SPARCTP" = pal$tp, "SPARC12" = pal$i12), name = NULL) +  # <- legend title removed
        scale_color_manual(values = c("SPARCTP" = "black", "SPARC12" = "black"), guide = "none") +
        annotate("text", x = -2.0, y =  0.0, label = n_tp_u,  size = 6) +
        annotate("text", x =  2.0, y =  0.0, label = n_12_u, size = 6) +
        annotate("text", x =  0.0, y =  0.0, label = n_int,  fontface = "bold", size = 6) +
        coord_equal(xlim = c(-4, 4), ylim = c(-3, 3), clip = "off") +
        labs(
          title    = "Venn â€” Genes passing threshold",
          subtitle = sub_md,
          x = NULL, y = NULL,
          caption  = "Intersection genes are present in both SPARCTP and SPARC12 at the Healthy+Disease coverage threshold."
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title    = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5) else element_text(hjust = 0.5),
          plot.caption  = .caption_theme(),
          panel.grid    = element_blank(),
          axis.text     = element_blank(),
          axis.title    = element_blank(),
          plot.margin   = margin(10, 22, 10, 10),
          legend.position = "bottom"
        )
      print(p)
      if (isTRUE(SAVE)) {
        fn <- paste0(name, "_", date, "_S10_Venn_SPARC_threshold_", thr_file_tag, ".png")
        ggsave_if(fn, p, width = 7.6, height = 6.4, dpi = 300, bg = "white")
      }
      invisible(p)
    }
    .make_venn_sparc()


    # 10.2 â€” Intersection genes

    # 10.2a â€” Overall means per SPARC (all samples) â†’ rank by |12 âˆ’ TP|
    .intersection_overall_table <- function() {
      if (!length(genes_INTER)) return(tibble::tibble())
      means_tab <- df_hd_present %>%
        dplyr::filter(gene_symbol %in% genes_INTER, SampleType %in% c("SPARCTP","SPARC12")) %>%
        dplyr::group_by(gene_symbol, SampleType) %>%
        dplyr::summarise(mu = mean(log2_abundance, na.rm = TRUE),
                         n  = dplyr::n(), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = SampleType, values_from = c(mu, n), names_sep = "_") %>%
        dplyr::mutate(
          delta_12_vs_TP = mu_SPARC12 - mu_SPARCTP,
          abs_delta      = abs(delta_12_vs_TP)
        ) %>%
        dplyr::arrange(dplyr::desc(abs_delta)) %>%
        dplyr::mutate(
          rank_abs_desc   = dplyr::row_number(),
          rank_dir_12gtTP = rank(-delta_12_vs_TP, ties.method = "first"),
          rank_dir_TPgt12 = rank( delta_12_vs_TP, ties.method = "first")
        )
      if (isTRUE(SAVE)) {
        fn <- paste0(name, "_", date, "_S10_Intersection_Overall_Delta_", thr_file_tag, ".csv")
        safe_write_csv(means_tab, fn)
      }
      means_tab
    }
    delta_overall <- .intersection_overall_table()

    # ---- 10.2b â€” Most different across SPARCs: Overall (SPARC-grouped)
    .plot_intersection_top10_bySPARC <- function(delta_tbl, direction = c("S12_GT_TP","TP_GT_S12")) {
      if (!nrow(delta_tbl)) return(invisible(NULL))
      direction <- match.arg(direction)
      if (direction == "S12_GT_TP") {
        genes_sel <- delta_tbl %>% dplyr::arrange(dplyr::desc(delta_12_vs_TP)) %>% dplyr::slice_head(n = 10) %>% dplyr::pull(gene_symbol)
        plot_tag  <- "Top10_S12_GT_TP"
        subtitle_extra <- "Top 10 genes where SPARC12 > SPARCTP (overall means)"
      } else {
        genes_sel <- delta_tbl %>% dplyr::arrange(delta_12_vs_TP) %>% dplyr::slice_head(n = 10) %>% dplyr::pull(gene_symbol)
        plot_tag  <- "Top10_TP_GT_S12"
        subtitle_extra <- "Top 10 genes where SPARCTP > SPARC12 (overall means)"
      }
      if (!length(genes_sel)) return(invisible(NULL))

      d_plot <- df_hd_present %>%
        dplyr::filter(gene_symbol %in% genes_sel, SampleType %in% c("SPARCTP","SPARC12")) %>%
        dplyr::left_join(delta_overall %>% dplyr::select(gene_symbol, mu_SPARCTP, mu_SPARC12, delta_12_vs_TP), by = "gene_symbol") %>%
        dplyr::mutate(gene_label = factor(gene_symbol, levels = genes_sel))

      lab_map <- d_plot %>%
        dplyr::distinct(gene_symbol, mu_SPARCTP, mu_SPARC12, delta_12_vs_TP) %>%
        dplyr::mutate(lbl = .label4_ev(
          gene_symbol,
          paste0("TP = ", sprintf("%.2f", mu_SPARCTP)),
          paste0("12 = ", sprintf("%.2f", mu_SPARC12)),
          paste0("log2FC = ", sprintf("%.2f", delta_12_vs_TP))
        )) %>%
        dplyr::select(gene_symbol, lbl)
      d_plot <- d_plot %>% dplyr::mutate(gene_label = factor(gene_symbol,
                                                             levels = genes_sel,
                                                             labels = setNames(lab_map$lbl, lab_map$gene_symbol)[genes_sel]))

      ttl <- if (.has_ggtext) {
        paste0("Intersection â€” Most different across SPARCs (",
               "<span style='color:", pal$tp,  ";'><b>TP</b></span> vs ",
               "<span style='color:", pal$i12, ";'><b>12</b></span>)")
      } else "Intersection â€” Most different across SPARCs (TP vs 12)"
      subl <- paste0(subtitle_extra, " â€¢ Threshold ", thr_short)

      p <- ggplot(d_plot, aes(x = SampleType, y = log2_abundance)) +
        geom_boxplot(aes(fill = SampleType), outlier.shape = NA, width = 0.74, color = "black", show.legend = TRUE) +
        ggbeeswarm::geom_quasirandom(
          aes(fill = Disease),
          shape = 21, size = 2.2, stroke = 0.6, color = "black",
          width = 0.14, alpha = 0.95, show.legend = TRUE
        ) +
        scale_x_discrete(labels = NULL, drop = FALSE) +
        scale_fill_manual(values = .fill_values_combined,
                          breaks = c("SPARCTP","SPARC12","Healthy","Disease"),
                          name = NULL, drop = FALSE) +
        guides(fill = guide_legend(override.aes = list(shape = c(22,22,21,21),
                                                       size  = c(4,4,3,3),
                                                       color = c("black","black","black","black")))) +
        labs(
          title = ttl, subtitle = subl, x = NULL, y = "log2 abundance",
          caption = "Intersection genes: present in both SPARCs at the HD coverage threshold. Boxes: SPARC; points: Disease. log2FC = mean(12) âˆ’ mean(TP) across all samples."
        ) +
        facet_wrap(~ gene_label, ncol = 5, scales = "fixed", drop = FALSE) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title    = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5, face = "bold") else element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5) else element_text(hjust = 0.5),
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          strip.text = if (.has_ggtext) ggtext::element_markdown(size = 11, lineheight = 1.05) else element_text(size = 11),
          axis.text.x = element_blank(),
          plot.caption = .caption_theme(),
          plot.margin  = margin(t = 10, r = 18, b = 20, l = 10)
        )

      print(p)
      if (isTRUE(SAVE)) {
        h <- .dynamic_height(length(genes_sel), ncol = 5, base_h = 5.2)
        fn <- paste0(name, "_", date, "_S10_Intersection_", plot_tag, "_SPARCgroup_", thr_file_tag, ".png")
        ggsave_if(fn, p, width = 12.0, height = h, dpi = 300, bg = "white")
      }
      invisible(p)
    }
    .plot_intersection_top10_bySPARC(delta_overall, "S12_GT_TP")
    .plot_intersection_top10_bySPARC(delta_overall, "TP_GT_S12")

    # ---- New: 10.2b.1 â€” Most different, Healthy slice (SÃ—D grouping; no axis labels)
    .plot_intersection_top10_slice_SxD <- function(delta_tbl, slice = c("Healthy","Disease"),
                                                   direction = c("S12_GT_TP","TP_GT_S12")) {
      if (!nrow(delta_tbl)) return(invisible(NULL))
      slice <- match.arg(slice); direction <- match.arg(direction)

      # Recompute deltas on the slice
      calc_slice <- df_hd_present %>%
        dplyr::filter(gene_symbol %in% genes_INTER,
                      SampleType %in% c("SPARCTP","SPARC12"),
                      Disease == slice) %>%
        dplyr::group_by(gene_symbol, SampleType) %>%
        dplyr::summarise(mu = mean(log2_abundance, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = SampleType, values_from = mu, names_prefix = "mu_") %>%
        dplyr::mutate(delta_12_vs_TP = mu_SPARC12 - mu_SPARCTP)

      ord <- if (direction == "S12_GT_TP") dplyr::desc else identity
      genes_sel <- delta_tbl %>%
        dplyr::select(gene_symbol) %>% dplyr::distinct() %>%
        dplyr::inner_join(calc_slice, by = "gene_symbol") %>%
        dplyr::arrange(ord(delta_12_vs_TP)) %>%
        dplyr::slice_head(n = 10) %>%
        dplyr::pull(gene_symbol)
      if (!length(genes_sel)) return(invisible(NULL))

      d_plot <- df_hd_present %>%
        dplyr::filter(gene_symbol %in% genes_sel,
                      SampleType %in% c("SPARCTP","SPARC12"),
                      Disease %in% c("Healthy","Disease")) %>%
        dplyr::mutate(Group = .make_SxD_group(SampleType, Disease),
                      gene_label = factor(gene_symbol, levels = genes_sel)) %>%
        dplyr::left_join(calc_slice, by = "gene_symbol")

      lab_map <- d_plot %>%
        dplyr::distinct(gene_symbol, mu_SPARCTP, mu_SPARC12, delta_12_vs_TP) %>%
        dplyr::mutate(lbl = .label4_ev(
          gene_symbol,
          paste0("TP = ", sprintf("%.2f", mu_SPARCTP)),
          paste0("12 = ", sprintf("%.2f", mu_SPARC12)),
          paste0("log2FC = ", sprintf("%.2f", delta_12_vs_TP))
        )) %>%
        dplyr::select(gene_symbol, lbl)
      d_plot <- d_plot %>% dplyr::mutate(gene_label = factor(gene_symbol,
                                                             levels = genes_sel,
                                                             labels = setNames(lab_map$lbl, lab_map$gene_symbol)[genes_sel]))

      tag <- paste0("Top10_", slice, "_", ifelse(direction=="S12_GT_TP","S12_GT_TP","TP_GT_S12"))
      ttl <- if (.has_ggtext) {
        paste0("Intersection â€” Most different (", slice, " slice): ",
               "<span style='color:", pal$tp,  ";'><b>TP</b></span> vs ",
               "<span style='color:", pal$i12, ";'><b>12</b></span>")
      } else paste0("Intersection â€” Most different (", slice, " slice): TP vs 12")
      subl <- paste0("Top 10 where ", ifelse(direction=="S12_GT_TP","12 > TP","TP > 12"), " â€¢ Threshold ", thr_short)

      p <- ggplot(d_plot, aes(x = Group, y = log2_abundance)) +
        geom_boxplot(aes(fill = Disease), outlier.shape = NA, width = 0.74, color = "black", show.legend = TRUE) +
        ggbeeswarm::geom_quasirandom(
          aes(fill = SampleType),
          shape = 21, size = 2.2, stroke = 0.6, color = "black",
          width = 0.14, alpha = 0.95, show.legend = TRUE
        ) +
        scale_x_discrete(labels = rep("", 4), drop = FALSE) +  # no x labels (legend suffices)
        scale_fill_manual(values = .fill_values_combined,
                          breaks = c("Healthy","Disease","SPARCTP","SPARC12"),
                          name = NULL, drop = FALSE) +
        guides(fill = guide_legend(override.aes = list(shape = c(22,22,21,21),
                                                       size  = c(4,4,3,3),
                                                       color = c("black","black","black","black")))) +
        labs(
          title = ttl, subtitle = subl, x = NULL, y = "log2 abundance",
          caption = "Intersection genes at threshold. Slice-specific log2FC = mean(12) âˆ’ mean(TP) within the slice. Boxes: Disease; points: SPARC."
        ) +
        facet_wrap(~ gene_label, ncol = 5, scales = "fixed", drop = FALSE) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title    = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5, face = "bold") else element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5) else element_text(hjust = 0.5),
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          strip.text = if (.has_ggtext) ggtext::element_markdown(size = 11, lineheight = 1.05) else element_text(size = 11),
          axis.text.x = element_text(face = "bold", color = "black", margin = margin(t = 10)),
          plot.caption = .caption_theme(),
          plot.margin  = margin(t = 10, r = 18, b = 28, l = 10)
        )

      print(p)
      if (isTRUE(SAVE)) {
        h <- .dynamic_height(length(genes_sel), ncol = 5, base_h = 5.2)
        fn <- paste0(name, "_", date, "_S10_Intersection_MostDifferent_", tag, "_SxD_", thr_file_tag, ".png")
        ggsave_if(fn, p, width = 12.0, height = h, dpi = 300, bg = "white")
      }
      invisible(p)
    }

    # Render Healthy & Disease slices for most-different (both directions)
    .plot_intersection_top10_slice_SxD(delta_overall, "Healthy", "S12_GT_TP")
    .plot_intersection_top10_slice_SxD(delta_overall, "Healthy", "TP_GT_S12")
    .plot_intersection_top10_slice_SxD(delta_overall, "Disease", "S12_GT_TP")
    .plot_intersection_top10_slice_SxD(delta_overall, "Disease", "TP_GT_S12")

    # ---- 10.2c â€” â€œMost similarâ€ across SPARCs (Same as before; Overall = SPARC-grouped; Healthy/Disease = SÃ—D)
    .intersection_most_similar_tables_and_plots <- function(make_plots = TRUE) {
      if (!length(genes_INTER)) return(invisible(NULL))

      calc_slice <- function(slice = c("Overall","Healthy","Disease")) {
        slice <- match.arg(slice)
        d0 <- df_hd_present %>%
          dplyr::filter(gene_symbol %in% genes_INTER, SampleType %in% c("SPARCTP","SPARC12"))
        if (slice == "Healthy") d0 <- d0 %>% dplyr::filter(Disease == "Healthy")
        if (slice == "Disease") d0 <- d0 %>% dplyr::filter(Disease == "Disease")
        tab <- d0 %>%
          dplyr::group_by(gene_symbol, SampleType) %>%
          dplyr::summarise(mu = mean(log2_abundance, na.rm = TRUE),
                           n  = dplyr::n(), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = SampleType, values_from = c(mu, n), names_sep = "_") %>%
          dplyr::mutate(delta_12_vs_TP = mu_SPARC12 - mu_SPARCTP,
                        abs_delta = abs(delta_12_vs_TP),
                        slice = slice) %>%
          dplyr::arrange(abs_delta) %>%
          dplyr::mutate(rank_most_similar = dplyr::row_number(), .before = 1)
        tab
      }
      t_overall <- calc_slice("Overall")
      t_healthy <- calc_slice("Healthy")
      t_disease <- calc_slice("Disease")

      if (isTRUE(SAVE)) {
        safe_write_csv(t_overall, paste0(name, "_", date, "_S10_Intersection_MostSimilar_Overall_", thr_file_tag, ".csv"))
        safe_write_csv(t_healthy, paste0(name, "_", date, "_S10_Intersection_MostSimilar_Healthy_", thr_file_tag, ".csv"))
        safe_write_csv(t_disease, paste0(name, "_", date, "_S10_Intersection_MostSimilar_Disease_", thr_file_tag, ".csv"))
      }
      if (!isTRUE(make_plots)) return(invisible(NULL))

      # Overall â€” SPARC-grouped (boxes by SPARC; points by Disease)
      genes_sel_overall <- t_overall %>% dplyr::slice_head(n = 10) %>% dplyr::pull(gene_symbol)
      if (length(genes_sel_overall)) {
        d_plot <- df_hd_present %>%
          dplyr::filter(gene_symbol %in% genes_sel_overall, SampleType %in% c("SPARCTP","SPARC12")) %>%
          dplyr::left_join(t_overall %>% dplyr::select(gene_symbol, mu_SPARCTP, mu_SPARC12, delta_12_vs_TP), by = "gene_symbol") %>%
          dplyr::mutate(gene_label = factor(gene_symbol, levels = genes_sel_overall))
        lab_map <- d_plot %>%
          dplyr::distinct(gene_symbol, mu_SPARCTP, mu_SPARC12, delta_12_vs_TP) %>%
          dplyr::mutate(lbl = .label4_ev(
            gene_symbol,
            paste0("TP = ", sprintf("%.2f", mu_SPARCTP)),
            paste0("12 = ", sprintf("%.2f", mu_SPARC12)),
            paste0("log2FC = ", sprintf("%.2f", delta_12_vs_TP))
          )) %>%
          dplyr::select(gene_symbol, lbl)
        d_plot <- d_plot %>% dplyr::mutate(gene_label = factor(gene_symbol,
                                                               levels = genes_sel_overall,
                                                               labels = setNames(lab_map$lbl, lab_map$gene_symbol)[genes_sel_overall]))
        ttl <- if (.has_ggtext) {
          paste0("Intersection â€” Most similar across SPARCs (Overall: ",
                 "<span style='color:", pal$tp,  ";'><b>TP</b></span> vs ",
                 "<span style='color:", pal$i12, ";'><b>12</b></span>)")
        } else "Intersection â€” Most similar across SPARCs (Overall: TP vs 12)"
        subl <- paste0("Top 10 smallest |log2FC| â€¢ Threshold ", thr_short)
        p <- ggplot(d_plot, aes(x = SampleType, y = log2_abundance)) +
          geom_boxplot(aes(fill = SampleType), outlier.shape = NA, width = 0.74, color = "black", show.legend = TRUE) +
          ggbeeswarm::geom_quasirandom(
            aes(fill = Disease),
            shape = 21, size = 2.2, stroke = 0.6, color = "black",
            width = 0.14, alpha = 0.95, show.legend = TRUE
          ) +
          scale_x_discrete(labels = NULL, drop = FALSE) +
          scale_fill_manual(values = .fill_values_combined,
                            breaks = c("SPARCTP","SPARC12","Healthy","Disease"),
                            name = NULL, drop = FALSE) +
          guides(fill = guide_legend(override.aes = list(shape = c(22,22,21,21),
                                                         size  = c(4,4,3,3),
                                                         color = c("black","black","black","black")))) +
          labs(
            title = ttl, subtitle = subl, x = NULL, y = "log2 abundance",
            caption = "Intersection genes: present in both SPARCs. Boxes: SPARC; points: Disease. log2FC = mean(12) âˆ’ mean(TP) within Overall slice."
          ) +
          facet_wrap(~ gene_label, ncol = 5, scales = "fixed", drop = FALSE) +
          theme_minimal(base_size = 14) +
          theme(
            plot.title    = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5, face = "bold") else element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5) else element_text(hjust = 0.5),
            legend.position = "bottom",
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            strip.text = if (.has_ggtext) ggtext::element_markdown(size = 11, lineheight = 1.05) else element_text(size = 11),
            axis.text.x = element_blank(),
            plot.caption = .caption_theme(),
            plot.margin  = margin(t = 10, r = 18, b = 20, l = 10)
          )
        print(p)
        if (isTRUE(SAVE)) {
          h <- .dynamic_height(length(genes_sel_overall), ncol = 5, base_h = 5.2)
          fn <- paste0(name, "_", date, "_S10_Intersection_MostSimilar_Overall_Top10_SPARCgroup_", thr_file_tag, ".png")
          ggsave_if(fn, p, width = 12.0, height = h, dpi = 300, bg = "white")
        }
      }

      # Healthy / Disease â€” SÃ—D (boxes by Disease; points by SPARC; centered H/D headers + n labels)
      plot_slice_SxD <- function(tt, slice_tag) {
        genes_sel <- tt %>% dplyr::slice_head(n = 10) %>% dplyr::pull(gene_symbol)
        if (!length(genes_sel)) return(invisible(NULL))
        d_plot <- df_hd_present %>%
          dplyr::filter(gene_symbol %in% genes_sel, SampleType %in% c("SPARCTP","SPARC12")) %>%
          dplyr::mutate(Group = .make_SxD_group(SampleType, Disease),
                        gene_label = factor(gene_symbol, levels = genes_sel)) %>%
          dplyr::left_join(tt %>% dplyr::select(gene_symbol, mu_SPARCTP, mu_SPARC12, delta_12_vs_TP), by = "gene_symbol")

        lab_map <- d_plot %>%
          dplyr::distinct(gene_symbol, mu_SPARCTP, mu_SPARC12, delta_12_vs_TP) %>%
          dplyr::mutate(lbl = .label4_ev(
            gene_symbol,
            paste0("TP = ", sprintf("%.2f", mu_SPARCTP)),
            paste0("12 = ", sprintf("%.2f", mu_SPARC12)),
            paste0("log2FC = ", sprintf("%.2f", delta_12_vs_TP))
          )) %>%
          dplyr::select(gene_symbol, lbl)
        d_plot <- d_plot %>% dplyr::mutate(gene_label = factor(gene_symbol,
                                                               levels = genes_sel,
                                                               labels = setNames(lab_map$lbl, lab_map$gene_symbol)[genes_sel]))

        base <- .baseline_and_labels(d_plot)
        n_df  <- .make_nlabel_df(d_plot, facet_key = "gene_label", x_key = "Group", y_lab = base$y_lab)
        axis_df <- .make_centered_SxD_labels_df(d_plot, facet_key = "gene_label", y_min = base$y_min, y_pad = 0.05)

        ttl <- if (.has_ggtext) {
          paste0("Intersection â€” Most similar (", slice_tag, "): ",
                 "<span style='color:", pal$tp,  ";'><b>TP</b></span> vs ",
                 "<span style='color:", pal$i12, ";'><b>12</b></span>")
        } else paste0("Intersection â€” Most similar (", slice_tag, "): TP vs 12")
        subl <- paste0("Top 10 smallest |log2FC| â€¢ Threshold ", thr_short)

        p <- ggplot(d_plot, aes(x = Group, y = log2_abundance)) +
          geom_boxplot(aes(fill = Disease), outlier.shape = NA, width = 0.74, color = "black", show.legend = TRUE) +
          ggbeeswarm::geom_quasirandom(
            aes(fill = SampleType),
            shape = 21, size = 2.2, stroke = 0.6, color = "black",
            width = 0.14, alpha = 0.95, show.legend = TRUE
          ) +
          scale_x_discrete(labels = rep("", 4), drop = FALSE) +
          geom_text(data = axis_df, aes(x = x, y = y, label = lab),
                    inherit.aes = FALSE, vjust = 1.4, fontface = "bold", size = 3.6) +
          geom_text(data = n_df, aes(x = Group, y = y_lab, label = n_lab),
                    inherit.aes = FALSE, vjust = 1.0, size = 3.4, fontface = "bold") +
          scale_fill_manual(values = .fill_values_combined,
                            breaks = c("Healthy","Disease","SPARCTP","SPARC12"),
                            name = NULL, drop = FALSE) +
          guides(fill = guide_legend(override.aes = list(shape = c(22,22,21,21),
                                                         size  = c(4,4,3,3),
                                                         color = c("black","black","black","black")))) +
          labs(
            title = ttl, subtitle = subl, x = NULL, y = "log2 abundance",
            caption = "Intersection genes at threshold; SÃ—D layout. Boxes: Disease; points: SPARC. log2FC = mean(12) âˆ’ mean(TP) within slice."
          ) +
          facet_wrap(~ gene_label, ncol = 5, scales = "fixed", drop = FALSE) +
          theme_minimal(base_size = 14) +
          theme(
            plot.title    = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5, face = "bold") else element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5) else element_text(hjust = 0.5),
            legend.position = "bottom",
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            strip.text = if (.has_ggtext) ggtext::element_markdown(size = 11, lineheight = 1.05) else element_text(size = 11),
            axis.text.x = element_text(face = "bold", color = "black", margin = margin(t = 10)),
            plot.caption = .caption_theme(),
            plot.margin  = margin(t = 10, r = 18, b = 28, l = 10)
          )
        print(p)
        if (isTRUE(SAVE)) {
          h <- .dynamic_height(length(genes_sel), ncol = 5, base_h = 5.2)
          fn <- paste0(name, "_", date, "_S10_Intersection_MostSimilar_", slice_tag, "_Top10_SxD_", thr_file_tag, ".png")
          ggsave_if(fn, p, width = 12.0, height = h, dpi = 300, bg = "white")
        }
      }
      plot_slice_SxD(t_healthy, "Healthy")
      plot_slice_SxD(t_disease, "Disease")
    }
    .intersection_most_similar_tables_and_plots(make_plots = TRUE)

    # ---- 10.2d â€” Î” log2FC across SPARCs per gene (SÃ—D layout; keep (n) labels)
    .delta_FC_table_and_plots <- function() {
      if (!length(genes_INTER)) return(invisible(NULL))
      FC_tab <- df_hd_present %>%
        dplyr::filter(gene_symbol %in% genes_INTER, SampleType %in% c("SPARCTP","SPARC12")) %>%
        dplyr::group_by(gene_symbol, SampleType, Disease) %>%
        dplyr::summarise(mu = mean(log2_abundance, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = Disease, values_from = mu) %>%
        dplyr::mutate(FC = Disease - Healthy) %>%
        dplyr::select(gene_symbol, SampleType, FC) %>%
        tidyr::pivot_wider(names_from = SampleType, values_from = FC, names_prefix = "FC_") %>%
        dplyr::mutate(delta_FC = FC_SPARC12 - FC_SPARCTP,
                      abs_delta_FC = abs(delta_FC)) %>%
        dplyr::arrange(dplyr::desc(abs_delta_FC)) %>%
        dplyr::mutate(rank_abs_desc = dplyr::row_number(),
                      rank_smallest = rank(abs_delta_FC, ties.method = "first"))

      if (isTRUE(SAVE)) {
        safe_write_csv(FC_tab, paste0(name, "_", date, "_S10_Intersection_DeltaFC_Table_", thr_file_tag, ".csv"))
      }

      make_plot <- function(sel_genes, tag, subtitle_extra) {
        if (!length(sel_genes)) return(invisible(NULL))
        d_plot <- df_hd_present %>%
          dplyr::filter(gene_symbol %in% sel_genes, SampleType %in% c("SPARCTP","SPARC12")) %>%
          dplyr::mutate(Group = .make_SxD_group(SampleType, Disease),
                        gene_label = factor(gene_symbol, levels = sel_genes)) %>%
          dplyr::left_join(FC_tab %>% dplyr::select(gene_symbol, FC_SPARCTP, FC_SPARC12, delta_FC), by = "gene_symbol")

        lab_map <- d_plot %>%
          dplyr::distinct(gene_symbol, FC_SPARCTP, FC_SPARC12, delta_FC) %>%
          dplyr::mutate(lbl = .label4_ev(
            gene_symbol,
            paste0("TP-FC = ", sprintf("%.2f", FC_SPARCTP)),
            paste0("12-FC = ", sprintf("%.2f", FC_SPARC12)),
            paste0("Î” = ", sprintf("%.2f", delta_FC))
          )) %>%
          dplyr::select(gene_symbol, lbl)
        d_plot <- d_plot %>% dplyr::mutate(gene_label = factor(gene_symbol,
                                                               levels = sel_genes,
                                                               labels = setNames(lab_map$lbl, lab_map$gene_symbol)[sel_genes]))

        base <- .baseline_and_labels(d_plot)
        n_df  <- .make_nlabel_df(d_plot, facet_key = "gene_label", x_key = "Group", y_lab = base$y_lab)
        axis_df <- .make_centered_SxD_labels_df(d_plot, facet_key = "gene_label", y_min = base$y_min, y_pad = 0.05)

        ttl <- if (.has_ggtext) {
          paste0("Intersection â€” Î” log2FC across SPARCs (",
                 "<span style='color:", pal$tp,  ";'><b>TP</b></span> vs ",
                 "<span style='color:", pal$i12, ";'><b>12</b></span>)")
        } else "Intersection â€” Î” log2FC across SPARCs (TP vs 12)"
        subl <- paste0(subtitle_extra, " â€¢ Threshold ", thr_short)

        p <- ggplot(d_plot, aes(x = Group, y = log2_abundance)) +
          geom_boxplot(aes(fill = Disease), outlier.shape = NA, width = 0.74, color = "black", show.legend = TRUE) +
          ggbeeswarm::geom_quasirandom(
            aes(fill = SampleType),
            shape = 21, size = 2.2, stroke = 0.6, color = "black",
            width = 0.14, alpha = 0.95, show.legend = TRUE
          ) +
          scale_x_discrete(labels = rep("",4), drop = FALSE) +
          geom_text(data = axis_df, aes(x = x, y = y, label = lab),
                    inherit.aes = FALSE, vjust = 1.4, fontface = "bold", size = 3.6) +
          geom_text(data = n_df, aes(x = Group, y = y_lab, label = n_lab),
                    inherit.aes = FALSE, vjust = 1.0, size = 3.4, fontface = "bold") +
          scale_fill_manual(values = .fill_values_combined,
                            breaks = c("Healthy","Disease","SPARCTP","SPARC12"),
                            name = NULL, drop = FALSE) +
          guides(fill = guide_legend(override.aes = list(shape = c(22,22,21,21),
                                                         size  = c(4,4,3,3),
                                                         color = c("black","black","black","black")))) +
          labs(
            title = ttl, subtitle = subl, x = NULL, y = "log2 abundance",
            caption = "Intersection genes: present in both SPARCs. Î” = (Diseaseâˆ’Healthy)_12 âˆ’ (Diseaseâˆ’Healthy)_TP. Boxes: Disease; points: SPARC. SÃ—D layout with centered H/D labels and (n) counts."
          ) +
          facet_wrap(~ gene_label, ncol = 5, scales = "fixed", drop = FALSE) +
          theme_minimal(base_size = 14) +
          theme(
            plot.title    = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5, face = "bold") else element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5) else element_text(hjust = 0.5),
            legend.position = "bottom",
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            strip.text = if (.has_ggtext) ggtext::element_markdown(size = 11, lineheight = 1.05) else element_text(size = 11),
            axis.text.x = element_text(face = "bold", color = "black", margin = margin(t = 10)),
            plot.caption = .caption_theme(),
            plot.margin  = margin(t = 10, r = 18, b = 28, l = 10)
          )

        print(p)
        if (isTRUE(SAVE)) {
          h <- .dynamic_height(length(sel_genes), ncol = 5, base_h = 5.2)
          fn <- paste0(name, "_", date, "_S10_Intersection_DeltaFC_", tag, "_SxD_", thr_file_tag, ".png")
          ggsave_if(fn, p, width = 12.0, height = h, dpi = 300, bg = "white")
        }
        invisible(p)
      }

      # Top10 largest |Î”| and smallest |Î”|
      genes_large <- FC_tab %>% dplyr::slice_head(n = 10) %>% dplyr::pull(gene_symbol)
      make_plot(genes_large, "Top10_LargestAbs", "Top 10 genes with largest |Î”|")
      genes_small <- FC_tab %>% dplyr::arrange(abs_delta_FC) %>% dplyr::slice_head(n = 10) %>% dplyr::pull(gene_symbol)
      make_plot(genes_small, "Top10_SmallestAbs", "Top 10 genes with smallest |Î”|")
    }
    .delta_FC_table_and_plots()


    # 10.3 â€” Unique genes: Summary CSVs + â€œTop10 lowest other-SPARC coverageâ€ plots (SÃ—D)

    .unique_summary_table <- function(context = c("SPARCTP","SPARC12")) {
      context <- match.arg(context)
      if (context == "SPARCTP") {
        unique_genes <- genes_TPonly
        other_cov <- presence_by_type %>% dplyr::filter(SampleType == "SPARC12") %>% dplyr::select(gene_symbol, coverage_other = coverage)
      } else {
        unique_genes <- genes_12only
        other_cov <- presence_by_type %>% dplyr::filter(SampleType == "SPARCTP") %>% dplyr::select(gene_symbol, coverage_other = coverage)
      }
      if (!length(unique_genes)) return(tibble::tibble())

      means_both <- df_hd_present %>%
        dplyr::filter(gene_symbol %in% unique_genes, SampleType %in% c("SPARCTP","SPARC12")) %>%
        dplyr::group_by(gene_symbol, SampleType) %>%
        dplyr::summarise(mu = mean(log2_abundance, na.rm = TRUE),
                         n  = dplyr::n(), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = SampleType, values_from = c(mu, n), names_sep = "_")

      tab <- tibble::tibble(gene_symbol = unique_genes) %>%
        dplyr::left_join(other_cov, by = "gene_symbol") %>%
        dplyr::left_join(means_both, by = "gene_symbol") %>%
        dplyr::arrange(coverage_other, dplyr::desc(mu_SPARC12 %||% -Inf), dplyr::desc(mu_SPARCTP %||% -Inf))

      if (isTRUE(SAVE)) {
        fn <- paste0(name, "_", date, "_S10_UniqueSummary_", context, "_", thr_file_tag, ".csv")
        safe_write_csv(tab, fn)
      }
      tab
    }

    .plot_unique_top10_lowOtherCoverage <- function(context = c("SPARCTP","SPARC12"), summary_tab) {
      context <- match.arg(context)
      if (!nrow(summary_tab)) return(invisible(NULL))
      top10 <- summary_tab %>% dplyr::arrange(coverage_other) %>% dplyr::slice_head(n = 10) %>% dplyr::pull(gene_symbol)
      if (!length(top10)) return(invisible(NULL))

      d_plot <- df_hd_present %>%
        dplyr::filter(gene_symbol %in% top10, SampleType %in% c("SPARCTP","SPARC12")) %>%
        dplyr::mutate(Group = .make_SxD_group(SampleType, Disease),
                      gene_label = factor(gene_symbol, levels = top10))

      lab_map <- tibble::tibble(gene_symbol = top10) %>%
        dplyr::mutate(lbl = .label4_ev(gene_symbol, "", "", "")) %>%
        dplyr::select(gene_symbol, lbl)
      d_plot <- d_plot %>% dplyr::mutate(gene_label = factor(gene_symbol,
                                                             levels = top10,
                                                             labels = setNames(lab_map$lbl, lab_map$gene_symbol)[top10]))

      ttl <- if (.has_ggtext) {
        paste0("Unique genes â€” ",
               "<span style='color:", title_color_for(context), ";'><b>", context, "</b></span> (Top 10 lowest other-SPARC coverage)")
      } else paste0("Unique genes â€” ", context, " (Top 10 lowest other-SPARC coverage)")
      subl <- paste0("Threshold ", thr_short, " â€¢ Data = UNFILTERED present; Boxes: Disease; points: SPARC")

      base <- .baseline_and_labels(d_plot)
      n_df  <- .make_nlabel_df(d_plot, facet_key = "gene_label", x_key = "Group", y_lab = base$y_lab)
      axis_df <- .make_centered_SxD_labels_df(d_plot, facet_key = "gene_label", y_min = base$y_min, y_pad = 0.05)

      p <- ggplot(d_plot, aes(x = Group, y = log2_abundance)) +
        geom_boxplot(aes(fill = Disease), outlier.shape = NA, width = 0.74, color = "black", show.legend = TRUE) +
        ggbeeswarm::geom_quasirandom(
          aes(fill = SampleType),
          shape = 21, size = 2.2, stroke = 0.6, color = "black",
          width = 0.14, alpha = 0.95, show.legend = TRUE
        ) +
        scale_x_discrete(labels = rep("",4), drop = FALSE) +
        geom_text(data = axis_df, aes(x = x, y = y, label = lab),
                  inherit.aes = FALSE, vjust = 1.4, fontface = "bold", size = 3.6) +
        geom_text(data = n_df, aes(x = Group, y = y_lab, label = n_lab),
                  inherit.aes = FALSE, vjust = 1.0, size = 3.4, fontface = "bold") +
        scale_fill_manual(values = .fill_values_combined,
                          breaks = c("Healthy","Disease","SPARCTP","SPARC12"),
                          name = NULL, drop = FALSE) +
        guides(fill = guide_legend(override.aes = list(shape = c(22,22,21,21),
                                                       size  = c(4,4,3,3),
                                                       color = c("black","black","black","black")))) +
        labs(
          title = ttl, subtitle = subl, x = NULL, y = "log2 abundance",
          caption = "Unique at threshold in the labeled SPARC; both SPARCs shown (UNFILTERED present). SÃ—D layout with centered H/D labels and (n) counts. Boxes: Disease; points: SPARC."
        ) +
        facet_wrap(~ gene_label, ncol = 5, scales = "fixed", drop = FALSE) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title    = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5, face = "bold") else element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5) else element_text(hjust = 0.5),
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          strip.text = if (.has_ggtext) ggtext::element_markdown(size = 11, lineheight = 1.05) else element_text(size = 11),
          axis.text.x = element_text(face = "bold", color = "black", margin = margin(t = 10)),
          plot.caption = .caption_theme(),
          plot.margin  = margin(t = 10, r = 18, b = 28, l = 10)
        )

      print(p)
      if (isTRUE(SAVE)) {
        h <- .dynamic_height(length(top10), ncol = 5, base_h = 5.2)
        fn <- paste0(name, "_", date, "_S10_Unique_", context, "_Top10_lowOtherCoverage_", thr_file_tag, ".png")
        ggsave_if(fn, p, width = 12.5, height = h, dpi = 300, bg = "white")
      }
      invisible(p)
    }

    if (length(genes_TPonly)) {
      sum_TP <- .unique_summary_table("SPARCTP")
      .plot_unique_top10_lowOtherCoverage("SPARCTP", sum_TP)
    }
    if (length(genes_12only)) {
      sum_12 <- .unique_summary_table("SPARC12")
      .plot_unique_top10_lowOtherCoverage("SPARC12", sum_12)
    }


    # Notes
    # â€¢ No per-plot "plotdata" CSVs are produced in Section 10.
    # â€¢ Summary CSVs:
    #     - S10_Venn (PNG only)
    #     - S10_Intersection_Overall_Delta_<thr>.csv
    #     - S10_Intersection_MostSimilar_[Overall|Healthy|Disease]_<thr>.csv
    #     - S10_Intersection_DeltaFC_Table_<thr>.csv
    #     - S10_UniqueSummary_[SPARCTP|SPARC12]_<thr>.csv
    # â€¢ Plot conventions applied here:
    #     - Legend at bottom; x-axis labels removed (legend suffices), except centered H/D headers on SÃ—D when specified.
    #     - SPARC mentions colorized via pal; EV facet labels fully cyan; 4-line facet labels (GENE, TP=, 12=, stat).
    #     - SÃ—D layouts use order: 12-H, 12-D, TP-H, TP-D. (n) labels retained where requested.
    #     - Figure height scales with facet rows to avoid squished boxes/points.
  }, envir = .GlobalEnv)
  invisible(NULL)
}
run_section10()


#### SECTION 11 - Walt EV gene abundance (Disease x SPARC boxplots) ----

run_section11 <- function(save = SAVE) {
  old_save <- SAVE
  on.exit(assign("SAVE", old_save, envir = .GlobalEnv))
  assign("SAVE", save, envir = .GlobalEnv)
  evalq({
    required_objs <- c("df", "walt_metadata", "walt_genes")
    missing_objs <- required_objs[!vapply(required_objs, exists, logical(1), envir = .GlobalEnv)]
    if (length(missing_objs)) {
      stop("Section 11 requires objects: ", paste(missing_objs, collapse = ", "), call. = FALSE)
    }

    if (!is.data.frame(df) || !nrow(df)) {
      warning("Data frame 'df' is unavailable or empty; run Section 0 first.", call. = FALSE)
      return(invisible(NULL))
    }
    if (!is.data.frame(walt_metadata) || !nrow(walt_metadata)) {
      warning("Walt metadata not available; run Section 0 first.", call. = FALSE)
      return(invisible(NULL))
    }

    walt_syms <- sort(unique(as.character(walt_genes)))
    walt_syms <- walt_syms[!is.na(walt_syms) & walt_syms != ""]
    if (!length(walt_syms)) {
      warning("Walt EV gene list is empty after preprocessing.", call. = FALSE)
      return(invisible(NULL))
    }

    disease_palette <- c(
      Healthy = pal$aqua,
      Benign  = "grey65",
      Disease = pal$purple,
      Control = "grey85"
    )
    sparc_palette <- c(
      SPARCTP = pal$tp,
      SPARC12 = pal$i12
    )
    fill_values_combined <- c(disease_palette, sparc_palette)

    df_walt <- df %>%
      dplyr::filter(gene_symbol %in% walt_syms) %>%
      dplyr::filter(!is.na(log2_abundance)) %>%
      dplyr::filter(DiseaseType %in% c("Healthy", "Disease")) %>%
      dplyr::mutate(
        SampleType = dplyr::case_when(
          SampleType %in% c("IEVTP", "SPARCTP") ~ "SPARCTP",
          SampleType %in% c("IEV12", "SPARC12") ~ "SPARC12",
          TRUE ~ as.character(SampleType)
        ),
        DiseaseType = as.character(DiseaseType)
      ) %>%
      dplyr::filter(SampleType %in% sparc_types)

    if (!nrow(df_walt)) {
      warning("None of the Walt EV genes have log2 abundance values in the dataset.", call. = FALSE)
      return(invisible(NULL))
    }

    if (!isTRUE(.has_ggtext)) {
      message('Section 11: package "ggtext" is not available; facet labels will remain default colour. Install ggtext for coloured labels.')
    }

    present_diseases <- c("Healthy", "Disease")[c("Healthy", "Disease") %in% unique(df_walt$DiseaseType)]
    if (!length(present_diseases)) {
      warning("No DiseaseType values available for Walt EV genes.", call. = FALSE)
      return(invisible(NULL))
    }

    df_walt <- df_walt %>%
      dplyr::mutate(
        Disease = factor(DiseaseType, levels = present_diseases),
        SampleType = factor(SampleType, levels = sparc_types)
      ) %>%
      dplyr::filter(!is.na(Disease))

    walt_present_genes <- sort(unique(as.character(df_walt$gene_symbol)))

    meta_long <- walt_metadata %>%
      dplyr::filter(gene_symbol %in% walt_present_genes) %>%
      dplyr::mutate(
        gene_symbol  = as.character(gene_symbol),
        WaltCellType = dplyr::na_if(as.character(WaltCellType), ""),
        WaltLocation = dplyr::na_if(as.character(WaltLocation), ""),
        WaltDataset  = dplyr::na_if(as.character(WaltDataset),  "")
      ) %>%
      dplyr::distinct(gene_symbol, WaltCellType, WaltLocation, WaltDataset)

    meta_compact <- meta_long %>%
      dplyr::group_by(gene_symbol) %>%
      dplyr::summarise(
        WaltCellType = {
          vals <- unique(WaltCellType[!is.na(WaltCellType)])
          if (!length(vals)) NA_character_ else paste(sort(vals), collapse = "; ")
        },
        WaltLocation = {
          vals <- unique(WaltLocation[!is.na(WaltLocation)])
          if (!length(vals)) NA_character_ else paste(sort(vals), collapse = "; ")
        },
        WaltDataset = {
          vals <- unique(WaltDataset[!is.na(WaltDataset)])
          if (!length(vals)) NA_character_ else paste(sort(vals), collapse = "; ")
        },
        .groups = "drop"
      )

    meta_celltype <- meta_long %>%
      dplyr::mutate(
        CellType = dplyr::coalesce(WaltCellType, "Unknown"),
        Location = dplyr::coalesce(WaltLocation, "Unknown")
      ) %>%
      dplyr::group_by(gene_symbol, CellType) %>%
      dplyr::summarise(
        Locations = {
          locs <- unique(Location[!is.na(Location)])
          if (!length(locs)) "Unknown" else paste(sort(locs), collapse = "; ")
        },
        PrimaryLocation = {
          locs <- unique(Location[!is.na(Location)])
          if (!length(locs)) "Unknown" else sort(locs)[1]
        },
        .groups = "drop"
      )

    missing_meta_genes <- setdiff(walt_present_genes, meta_celltype$gene_symbol)
    if (length(missing_meta_genes)) {
      meta_celltype <- dplyr::bind_rows(
        meta_celltype,
        tibble::tibble(
          gene_symbol     = missing_meta_genes,
          CellType        = "Unknown",
          Locations       = "Unknown",
          PrimaryLocation = "Unknown"
        )
      )
    }

    location_levels <- sort(unique(meta_celltype$PrimaryLocation))
    if (!length(location_levels)) location_levels <- "Unknown"
    location_palette <- setNames(scales::hue_pal()(length(location_levels)), location_levels)
    if ("Unknown" %in% names(location_palette)) location_palette["Unknown"] <- "grey55"

    hex_to_colour_name <- local({
      base_cols <- grDevices::colors()
      base_rgb <- grDevices::col2rgb(base_cols)
      function(hex) {
        rgb_val <- tryCatch(grDevices::col2rgb(hex), error = function(e) NULL)
        if (is.null(rgb_val)) return(hex)
        if (!is.matrix(rgb_val)) rgb_val <- matrix(rgb_val, nrow = 3)
        if (ncol(rgb_val) != 1) rgb_val <- rgb_val[, 1, drop = FALSE]
        diff_mat <- base_rgb - matrix(rgb_val, nrow = 3, ncol = ncol(base_rgb))
        idx <- which.min(colSums(diff_mat^2))
        base_cols[[idx]]
      }
    })
    location_palette_names <- vapply(location_palette, hex_to_colour_name, character(1))

    celltype_levels <- sort(unique(meta_celltype$CellType))
    if (!length(celltype_levels)) celltype_levels <- "Unknown"

    summary_tbl <- df_walt %>%
      dplyr::group_by(gene_symbol, SampleType, Disease) %>%
      dplyr::summarise(
        n_detected  = dplyr::n_distinct(SampleName),
        mean_log2   = suppressWarnings(mean(log2_abundance, na.rm = TRUE)),
        sd_log2     = suppressWarnings(stats::sd(log2_abundance, na.rm = TRUE)),
        median_log2 = suppressWarnings(stats::median(log2_abundance, na.rm = TRUE)),
        cv_log2     = dplyr::if_else(is.finite(mean_log2) & abs(mean_log2) > 0, sd_log2 / abs(mean_log2), NA_real_),
        min_log2    = suppressWarnings(min(log2_abundance, na.rm = TRUE)),
        max_log2    = suppressWarnings(max(log2_abundance, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      tidyr::complete(
        gene_symbol,
        SampleType = factor(sparc_types, levels = sparc_types),
        Disease    = factor(present_diseases, levels = present_diseases),
        fill = list(
          n_detected  = 0L,
          mean_log2   = NA_real_,
          sd_log2     = NA_real_,
          median_log2 = NA_real_,
          cv_log2     = NA_real_,
          min_log2    = NA_real_,
          max_log2    = NA_real_
        )
      ) %>%
      dplyr::arrange(gene_symbol, SampleType, Disease)

    log2fc_tbl <- summary_tbl %>%
      dplyr::select(gene_symbol, SampleType, Disease, mean_log2) %>%
      tidyr::pivot_wider(names_from = Disease, values_from = mean_log2) %>%
      dplyr::mutate(log2FC = .data[["Disease"]] - .data[["Healthy"]])

    summary_tbl <- summary_tbl %>%
      dplyr::left_join(log2fc_tbl %>% dplyr::select(gene_symbol, SampleType, log2FC),
                       by = c("gene_symbol", "SampleType")) %>%
      dplyr::mutate(n_detected = as.integer(n_detected)) %>%
      dplyr::left_join(meta_compact, by = "gene_symbol")

    if (isTRUE(SAVE)) {
      summary_path <- section_filename("11", "Walt_EV_log2_summary", "csv")
      readr::write_csv(summary_tbl, summary_path)
      message("Saved: ", summary_path)
    }

    build_stats <- df_walt %>%
      dplyr::group_by(gene_symbol) %>%
      dplyr::summarise(
        mean_overall = suppressWarnings(mean(log2_abundance, na.rm = TRUE)),
        sd_overall   = suppressWarnings(stats::sd(log2_abundance, na.rm = TRUE)),
        cv_overall   = dplyr::if_else(is.finite(mean_overall) & abs(mean_overall) > 0, sd_overall / abs(mean_overall), NA_real_),
        mean_healthy = suppressWarnings(mean(log2_abundance[Disease == "Healthy"], na.rm = TRUE)),
        mean_disease = suppressWarnings(mean(log2_abundance[Disease == "Disease"], na.rm = TRUE)),
        FC           = mean_disease - mean_healthy,
        .groups = "drop"
      )

    make_facet_label <- function(gene_row, location_palette) {
      gene <- gene_row$gene_symbol
      primary_loc <- gene_row$PrimaryLocation
      label_col <- location_palette[[primary_loc]]
      if (is.null(label_col) || is.na(label_col)) label_col <- location_palette[["Unknown"]] %||% "grey45"
      mean_txt <- ifelse(is.finite(gene_row$mean_overall), sprintf("%.2f", gene_row$mean_overall), "NA")
      cv_txt   <- ifelse(is.finite(gene_row$cv_overall),   sprintf("%.3f", gene_row$cv_overall),   "NA")
      fc_txt   <- ifelse(is.finite(gene_row$FC),           sprintf("%.2f", gene_row$FC),           "NA")
      if (.has_ggtext) {
        paste0(
          "<span style='color:", label_col, ";'><b>", gene, "</b><br>",
          "<span style='font-size:9pt'>mean = ", mean_txt, "</span><br>",
          "<span style='font-size:9pt'>CV = ",   cv_txt,   "</span><br>",
          "<span style='font-size:9pt'>FC = ",   fc_txt,   "</span></span>"
        )
      } else {
        paste0(gene, "
mean = ", mean_txt, "
CV = ", cv_txt, "
FC = ", fc_txt)
      }
    }

    make_celltype_plot <- function(cell_type) {
      gene_meta <- meta_celltype %>% dplyr::filter(CellType == cell_type)
      genes_in_ct <- sort(unique(gene_meta$gene_symbol))
      if (!length(genes_in_ct)) {
        message("Section 11 - no genes available for cell type '", cell_type, "'.")
        return(invisible(NULL))
      }

      gene_info <- gene_meta %>%
        dplyr::left_join(build_stats, by = "gene_symbol") %>%
        dplyr::mutate(
          Locations = ifelse(is.na(Locations) | Locations == "", "Unknown", Locations),
          PrimaryLocation = ifelse(is.na(PrimaryLocation) | PrimaryLocation == "", "Unknown", PrimaryLocation)
        )

      gene_info <- gene_info %>% dplyr::arrange(gene_symbol)

      lab_vec <- setNames(
        purrr::pmap_chr(
          list(
            gene_symbol    = gene_info$gene_symbol,
            PrimaryLocation= gene_info$PrimaryLocation,
            mean_overall   = gene_info$mean_overall,
            cv_overall     = gene_info$cv_overall,
            FC             = gene_info$FC
          ),
          function(gene_symbol, PrimaryLocation, mean_overall, cv_overall, FC) {
            make_facet_label(
              tibble::tibble(
                gene_symbol     = gene_symbol,
                PrimaryLocation = PrimaryLocation,
                mean_overall    = mean_overall,
                cv_overall      = cv_overall,
                FC              = FC
              ),
              location_palette
            )
          }
        ),
        gene_info$gene_symbol
      )

      group_levels <- as.vector(outer(c("SPARC12", "SPARCTP"), present_diseases, paste, sep = " - "))

      d_plot <- df_walt %>%
        dplyr::filter(gene_symbol %in% genes_in_ct) %>%
        dplyr::mutate(
          Group = factor(paste(SampleType, Disease, sep = " - "), levels = group_levels),
          gene_label = factor(gene_symbol, levels = names(lab_vec), labels = unname(lab_vec))
        )

      if (!nrow(d_plot)) {
        message("Section 11 - no expression data for cell type '", cell_type, "'.")
        return(invisible(NULL))
      }

      n_facets <- length(lab_vec)
      n_cols <- 5L
      n_rows <- max(1L, ceiling(n_facets / n_cols))
      base_height <- 4.8
      plot_height <- base_height * (1 + 0.5 * (n_rows - 1))

      y_min <- suppressWarnings(min(d_plot$log2_abundance, na.rm = TRUE))
      y_max <- suppressWarnings(max(d_plot$log2_abundance, na.rm = TRUE))
      if (!is.finite(y_min)) y_min <- suppressWarnings(min(df_walt$log2_abundance, na.rm = TRUE))
      if (!is.finite(y_max)) y_max <- suppressWarnings(max(df_walt$log2_abundance, na.rm = TRUE))
      if (!is.finite(y_min)) y_min <- 0
      if (!is.finite(y_max)) y_max <- 1
      y_range <- max(y_max - y_min, 1e-6)
      y_baseline <- y_min - max(y_range * 0.12, 0.8)

      base_df <- tidyr::expand_grid(
        gene_label = factor(unname(lab_vec), levels = unname(lab_vec)),
        Group = factor(group_levels, levels = group_levels)
      ) %>%
        dplyr::mutate(log2_abundance = NA_real_)

      counts_pos <- tidyr::expand_grid(
        gene_label = factor(unname(lab_vec), levels = unname(lab_vec)),
        Group = factor(group_levels, levels = group_levels)
      ) %>%
        dplyr::left_join(
          d_plot %>% dplyr::count(gene_label, Group, name = "n"),
          by = c("gene_label", "Group")
        ) %>%
        dplyr::mutate(
          n = dplyr::coalesce(n, 0L),
          y_lab = y_baseline
        )

      legend_breaks <- c(present_diseases, names(sparc_palette))
      legend_shapes <- c(rep(22, length(present_diseases)), rep(21, length(sparc_palette)))
      legend_sizes  <- c(rep(4, length(present_diseases)), rep(3, length(sparc_palette)))
      legend_colors <- rep("black", length(legend_breaks))

      ct_title <- if (identical(cell_type, "Unknown")) "Unknown cell type" else cell_type
      title_txt <- if (.has_ggtext) paste0("<b>Walt EV genes</b> - ", ct_title) else paste0("Walt EV genes - ", ct_title)
      subtitle_txt <- "Boxes split by SPARC x Disease; facet colours follow primary location."
      location_key <- paste0(names(location_palette), " = ", location_palette_names[names(location_palette)])
      colour_caption <- paste(location_key, collapse = "; ")
      caption_txt <- stringr::str_wrap(paste0(
        "Log2 abundance values come from the unfiltered dataset (RawAbundance > 0). Boxes use Disease fill; points are coloured by SPARC sample type. Location colours: ",
        colour_caption,
        "."
      ), width = 110)

      p <- ggplot(d_plot, aes(x = Group, y = log2_abundance)) +
        geom_blank(data = base_df, aes(x = Group, y = log2_abundance)) +
        geom_boxplot(aes(fill = Disease), outlier.shape = NA, width = 0.74, color = "black", show.legend = TRUE) +
        ggbeeswarm::geom_quasirandom(
          aes(fill = SampleType),
          shape = 21, size = 2.2, stroke = 0.6, color = "black",
          width = 0.14, alpha = 0.95, show.legend = TRUE
        ) +
        geom_text(
          data = counts_pos,
          aes(x = Group, y = y_lab, label = paste0("n=", n)),
          inherit.aes = FALSE, fontface = "bold", size = 3.4, vjust = 1.1, color = "black"
        ) +
        scale_fill_manual(
          values = fill_values_combined,
          breaks = legend_breaks,
          drop = FALSE,
          name = NULL
        ) +
        scale_x_discrete(drop = FALSE, labels = rep("", length(group_levels))) +
        labs(
          title    = title_txt,
          subtitle = subtitle_txt,
          x = NULL,
          y = "log2 abundance",
          caption = caption_txt
        ) +
        facet_wrap(~ gene_label, ncol = 5, scales = "fixed", drop = FALSE) +
        expand_limits(y = y_baseline) +
        coord_cartesian(ylim = c(min(y_min, suppressWarnings(min(df_walt$log2_abundance, na.rm = TRUE))),
                                max(y_max, suppressWarnings(max(df_walt$log2_abundance, na.rm = TRUE)))),
                        clip = "off") +
        guides(
          fill = guide_legend(
            override.aes = list(
              shape = legend_shapes,
              size = legend_sizes,
              color = legend_colors
            )
          )
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title   = if (.has_ggtext) ggtext::element_markdown(hjust = 0.5, face = "bold") else element_text(hjust = 0.5, face = "bold"),
          plot.subtitle= element_text(hjust = 0.5),
          legend.position = "right",
          panel.grid.minor = element_blank(),
          panel.spacing.y  = grid::unit(2.5, "lines"),
          strip.background = element_blank(),
          strip.text = if (.has_ggtext) ggtext::element_markdown(size = 11, lineheight = 1.05, margin = margin(b = 6), colour = NA) else element_text(size = 11, margin = margin(b = 6)),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          plot.caption = element_text(hjust = 0.5),
          plot.margin  = margin(t = 12, r = 20, b = 24, l = 12)
        )

      print(p)
      if (isTRUE(SAVE)) {
        fn <- section_filename("11", paste0("Walt_EV_celltype_", sanitize(cell_type), "_boxplot"), "png")
        ggsave_if(fn, p, width = 10.5, height = plot_height, dpi = 300, bg = "white")
      }
      message("Section 11 - cell type '", cell_type, "': ", length(genes_in_ct), " genes plotted.")
      invisible(NULL)
    }

    purrr::walk(celltype_levels, make_celltype_plot)
    message("Section 11 - Walt EV genes processed: ", length(walt_present_genes), " genes across ", length(celltype_levels), " cell types.")
  }, envir = .GlobalEnv)
  invisible(NULL)
}
run_section11()
section_registry <- list(
  `0`  = run_section0,
  `1`  = run_section1,
  `2`  = run_section2,
  `3`  = run_section3,
  `3A` = run_section3A,
  `4`  = run_section4,
  `5`  = run_section5,
  `6`  = run_section6,
  `7`  = run_section7,
  `8`  = run_section8,
  `9`  = run_section9,
  `10` = run_section10,
  `11` = run_section11
)


