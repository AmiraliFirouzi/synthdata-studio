# =====================================================================
# SynthData Studio — single-file Shiny app
# Generate statistically faithful synthetic data from CSV / TSV / Excel.
#
# v2.1 — density & polish fixes
#   * the whole page now renders at 80% density via one CSS zoom on
#     <body> (no more manual browser zoom); the custom dropdown menus
#     compensate for the zoom so they always align with their field
#   * Configure: engine & row-count are back to the original compact
#     select / numeric inputs; all five fields sit level in one row
#   * dark native scrollbars everywhere (color-scheme + scrollbar-color)
#   * removed the duplicated download icon (Shiny adds one by default)
#
# v2.0 — experience & polish pass
#   * layout: the studio now sits directly under the hero (tools first),
#     "How it works" moved below the workspace; nav got scroll-spy
#   * method field only appears for model-based engines
#   * results: colour-coded fidelity score, live "run summary" bar,
#     quality-tab overall chip, animated progress bar in the overlay
#   * a11y/robustness: engine locked during a run, defensive numeric
#     input validation, comma-delimited .txt fallback, higher-contrast
#     hint text, version badge, theme-color meta for mobile
#
# v1.1 — engineering & design review pass
#   * dropdowns: dark glassy menus that live at <body> level (never
#     clipped by the card's overflow:hidden), capped height, search
#     filter for long option lists, full keyboard + ARIA support
#   * native <select> fallbacks styled dark (no white flash on re-render)
#   * robust CSV/TSV reading (UTF-8 with automatic fallback)
#   * seed validation, safer cancel handling, vectorised fidelity scan
#   * date axes formatted as dates in the Compare plot
#   * polish: stat count-up, floating orbs, icon toasts, meta/OG tags
#   * v2.2: upload progress bar + instant client-side size/type checks
#
# Engine: synthpop (model-based, runs in a callr background process)
#         with an automatic ladder of fallbacks and a Turbo resampler.
# =====================================================================

library(shiny)
library(shinyjs)

options(shiny.maxRequestSize = 50 * 1024^2)

MAX_FILE_MB     <- 25
MAX_ROWS        <- 1e6
MAX_LEVELS      <- 50
MAX_SYN_SECONDS <- 900
MISSING_TOKENS  <- c("?", "", "NA", "N/A", "na", "NaN", "null", "NULL")
APP_VERSION     <- "2.2"
GITHUB_URL      <- "https://github.com/AmiraliFirouzi"

ENGINE_PRESETS <- list(
  turbo    = list(label = "Turbo",         fit = 0L,    pred = 0L, budget = 0),
  balanced = list(label = "Balanced",      fit = 1500L, pred = 3L, budget = 150),
  high     = list(label = "High fidelity", fit = 4000L, pred = 0L, budget = 420)
)

`%||%` <- function(a, b) if (is.null(a)) b else a

type_labs <- c(num = "numeric", cat = "categorical", date = "date", other = "other")

col_type_label <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) "date"
  else if (is.logical(x)) "cat"
  else if (is.numeric(x)) "num"
  else if (is.factor(x) || is.character(x)) "cat"
  else "other"
}

num_like <- function(x)
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) as.numeric(x) else x

fmt_bytes <- function(b) {
  if (is.na(b)) return("—")
  if (b >= 1024^2) paste0(round(b / 1024^2, 1), " MB") else paste0(round(b / 1024, 1), " KB")
}

fmt_time <- function(x) {
  if (is.na(x)) return("—")
  if (x < 60) paste0(sprintf("%.1f", x), "s") else paste0(floor(x / 60), "m ", round(x %% 60), "s")
}

fmt_num <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) "—"
    else if (is.numeric(v) && abs(v) < 1e15)
      prettyNum(v, big.mark = ",", scientific = FALSE, digits = 6)
    else as.character(v)
  }, character(1), USE.NAMES = FALSE)
}

sanitize_str <- function(x) {
  vapply(as.character(x), function(s) {
    out <- tryCatch(enc2utf8(s), error = function(e) NULL)
    if (is.null(out)) return("?")
    out <- tryCatch(iconv(out, from = "UTF-8", to = "UTF-8", sub = "?"), error = function(e) NULL)
    if (is.null(out) || is.na(out)) "?" else out
  }, character(1), USE.NAMES = FALSE)
}

estimate_label <- function(rows, cols, engine, n_out) {
  if (engine == "turbo") return("instant")
  p <- ENGINE_PRESETS[[engine]]
  fit_rows <- min(rows, p$fit)
  preds    <- if (p$pred > 0) min(max(cols - 1, 1), p$pred) else max(cols - 1, 1)
  cells    <- fit_rows * cols * preds
  big_out  <- !is.na(n_out) && n_out >= 250000
  if (engine == "balanced") {
    est <- if (cells < 2e6) "typically seconds"
           else if (cells < 2e7) "typically under a minute"
           else "can be long — consider Turbo"
  } else {
    est <- if (cells < 2e7) "a few minutes" else "can be long — use Balanced"
  }
  if (big_out) est <- paste0(est, " · large row counts add time")
  est
}

# =====================================================================
# DATA HYGIENE
# =====================================================================
id_like_numeric <- function(v) {
  nn <- !is.na(v); n <- sum(nn)
  if (n < 100) return(FALSE)
  u <- unique(v[nn])
  isTRUE(length(u) == n) && all(u == floor(u)) && is.finite(max(u)) && (max(u) - min(u)) <= n
}

# =====================================================================
# VALUE CONSTRAINTS (semantic ranges + observed ranges)
# Keeps synthesised values logically valid: percentages in 0-100,
# proportions in 0-1, counts/ages/salaries non-negative, whole numbers
# whole, and every numeric column within its observed range.
# =====================================================================
PCT_NAMES    <- "%|percent|pct|perc|share|proportion|ratio|probability|coverage|discount|tax|completion|progress|accuracy|success_rate"
PCT_EXCLUDE  <- "change|growth|delta|diff|increase|decrease|error|deviation|margin"
NONNEG_NAMES <- "count|quantity|amount|total|salary|income|wage|earning|price|cost|fee|weight|height|views|clicks|visits|sales|revenue|profit|units|items|votes|likes|followers|customers|orders|transactions|distance|area|duration|length"
NONNEG_TOKENS <- "(^|_|\\.)(age|size|sum|num|number|qty|n)(_|\\.|$)"

build_constraints <- function(df) {
  cons <- list(); notes <- character(0)
  for (cn in names(df)) {
    x <- df[[cn]]
    if (!is.numeric(x) || inherits(x, c("Date", "POSIXct", "POSIXlt"))) next
    nn <- x[!is.na(x)]
    nn <- nn[is.finite(nn)]
    if (length(nn) < 1) next
    lo <- min(nn); hi <- max(nn)
    if (!is.finite(lo) || !is.finite(hi)) next
    is_int <- all(nn == floor(nn))
    nm     <- tolower(cn)
    pctish <- grepl(PCT_NAMES, nm) && !grepl(PCT_EXCLUDE, nm)
    nonneg <- grepl(NONNEG_NAMES, nm) || grepl(NONNEG_TOKENS, nm)
    sem_txt <- NULL

    if (lo >= 0 && hi <= 1 && hi > lo) {              # proportions stored 0-1
      lo <- 0; hi <- 1; sem_txt <- "proportion range 0-1"
    } else if (pctish) {                              # percentages 0-100
      if (lo < 0) { lo <- 0; sem_txt <- "percentage range 0-100" }
      if (hi > 100) { hi <- 100; if (is.null(sem_txt)) sem_txt <- "percentage range 0-100" }
    }
    if (nonneg && lo < 0) { lo <- 0; sem_txt <- "non-negative range" }

    if (!is.null(sem_txt))
      notes <- c(notes, paste0("'", cn, "': ", sem_txt))
    cons[[length(cons) + 1]] <- list(name = cn, lo = lo, hi = hi,
                                     integer = is_int, sem = sem_txt)
  }
  list(cons = cons, notes = notes)
}

enforce_constraints <- function(syn_d, cons) {
  notes_out <- character(0)
  if (is.null(cons) || !length(cons)) return(list(data = syn_d, notes = notes_out))
  for (cc in cons) {
    tryCatch({
      cn <- cc$name
      if (!cn %in% names(syn_d)) next
      v <- syn_d[[cn]]
      if (!is.numeric(v)) next
      lo <- cc$lo; hi <- cc$hi
      if (!is.finite(lo) || !is.finite(hi) || lo > hi) next
      n_bad <- sum(is.finite(v) & (v < lo | v > hi))
      v2 <- pmin(pmax(v, lo), hi)
      if (isTRUE(cc$integer)) v2 <- round(v2)
      syn_d[[cn]] <- v2
      if (n_bad > 0)
        notes_out <- c(notes_out,
                       sprintf("'%s': %d out-of-range value(s) clipped to [%s, %s]",
                               cn, n_bad, format(lo), format(hi)))
    }, error = function(e) NULL)
  }
  list(data = syn_d, notes = notes_out)
}

# Smart date parser (tries common formats, accepts >80% match)
try_parse_date <- function(x) {
  if (!is.character(x) || length(x) == 0) return(NULL)
  fmts <- c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%d/%m/%Y", "%m-%d-%Y", "%d-%m-%Y", "%Y%m%d")
  for (f in fmts) {
    d <- suppressWarnings(as.Date(x, format = f))
    if (sum(!is.na(d)) / length(x) > 0.8) return(d)
  }
  p_fmts <- c("%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%m/%d/%Y %H:%M:%S", "%d/%m/%Y %H:%M:%S")
  for (f in p_fmts) {
    p <- tryCatch(suppressWarnings(as.POSIXct(x, format = f)), error = function(e) NULL)
    if (!is.null(p) && sum(!is.na(p)) / length(x) > 0.8) return(p)
  }
  return(NULL)
}

clean_data <- function(df) {
  df <- as.data.frame(df)
  notes <- character(0); drop <- character(0)

  for (j in seq_len(ncol(df))) {
    x <- df[[j]]; nm <- names(df)[j]
    if (is.list(x) || is.matrix(x)) {
      drop <- c(drop, nm)
      notes <- c(notes, paste0("'", nm, "': unsupported column type — dropped"))
      next
    }

    # drop completely empty columns (nothing to model, would break the engine)
    if (all(is.na(x))) {
      drop <- c(drop, nm)
      notes <- c(notes, paste0("'", nm, "': empty column (all values missing) — dropped"))
      next
    }

    # Detect discrete numeric variables (like 0/1, 1/2/3/4) and treat as categorical
    if (is.numeric(x)) {
      # replace non-finite values (Inf/-Inf) with NA so models can fit
      n_inf <- sum(!is.finite(x) & !is.na(x))
      if (n_inf > 0) {
        x[!is.finite(x) & !is.na(x)] <- NA
        df[[j]] <- x
        notes <- c(notes, paste0("'", nm, "': ", n_inf, " infinite value(s) set to NA"))
      }
      uniq_vals <- unique(x[!is.na(x)])
      if (length(uniq_vals) <= 15 && all(uniq_vals == floor(uniq_vals))) {
        df[[j]] <- factor(x)
        notes <- c(notes, paste0("'", nm, "': low-cardinality integer treated as categorical"))
        next
      }

      if (id_like_numeric(x)) {
        drop <- c(drop, nm)
        notes <- c(notes, paste0("'", nm, "': identifier-like numeric column — dropped"))
        next
      }
      next
    }

    if (is.logical(x) || inherits(x, c("Date", "POSIXct", "POSIXlt"))) next

    raw <- as.character(x)
    ch  <- trimws(raw)
    ch[!is.na(ch) & ch %in% MISSING_TOKENS] <- NA
    nn <- !is.na(ch)
    if (!any(nn)) { drop <- c(drop, nm); next }

    parsed_dt <- try_parse_date(ch)
    if (!is.null(parsed_dt)) {
      df[[j]] <- parsed_dt
      notes <- c(notes, paste0("'", nm, "': parsed string to date"))
      next
    }

    n_n <- sum(nn); uniq <- length(unique(ch[nn]))
    if (n_n >= 50 && uniq >= 30 && uniq / n_n >= 0.95) {
      drop <- c(drop, nm)
      notes <- c(notes, paste0("'", nm, "': identifier-like column — dropped"))
      next
    }

    numv <- suppressWarnings(as.numeric(ch[nn]))
    frac <- mean(!is.na(numv))

    if (frac >= 0.8) {
      v <- suppressWarnings(as.numeric(ch))
      if (id_like_numeric(v)) {
        drop <- c(drop, nm)
        notes <- c(notes, paste0("'", nm, "': identifier-like column — dropped"))
        next
      }
      tok <- sum(!is.na(raw) & is.na(ch))
      bad <- sum(nn & is.na(v))
      parts <- "converted to numeric"
      if (tok > 0) parts <- paste0(parts, "; ", tok, " missing-value marker(s) set to NA")
      if (bad > 0) parts <- paste0(parts, "; ", bad, " unparseable value(s) set to NA")
      notes <- c(notes, paste0("'", nm, "': ", parts))
      df[[j]] <- v
    } else {
      changed <- sum(!is.na(raw) & (is.na(ch) | raw != ch))
      if (changed > 0) notes <- c(notes, paste0("'", nm, "': cleaned ", changed, " cell(s)"))
      df[[j]] <- factor(ch)
    }
  }

  if (length(drop)) {
    df[drop] <- NULL
    notes <- c(notes, paste0("dropped column(s): ", paste(paste0("'", sort(drop), "'"), collapse = ", ")))
  }
  list(df = df, notes = notes)
}

# Robust table reader: UTF-8 first, automatic fallback to the platform
# default, and a semicolon-separated retry (European CSV exports).
read_table_safe <- function(path, ext) {
  if (ext %in% c("tsv", "txt")) {
    d <- tryCatch(suppressWarnings(utils::read.delim(path, stringsAsFactors = FALSE,
                                                     fileEncoding = "UTF-8")),
                  error = function(e) NULL)
    if (is.null(d) || ncol(d) < 2)
      d <- tryCatch(utils::read.delim(path, stringsAsFactors = FALSE),
                    error = function(e) NULL)
    if ((is.null(d) || ncol(d) < 2) && ext == "txt")
      d <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
                    error = function(e) NULL)
    return(d)
  }
  cands <- list(
    function() utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
    function() utils::read.csv(path, stringsAsFactors = FALSE),
    function() utils::read.csv(path, stringsAsFactors = FALSE, sep = ";", fileEncoding = "UTF-8"),
    function() utils::read.csv(path, stringsAsFactors = FALSE, sep = ";")
  )
  for (f in cands) {
    d <- tryCatch(suppressWarnings(f()), error = function(e) NULL)
    if (!is.null(d) && ncol(d) > 1) return(d)
  }
  tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
}

# =====================================================================
# TURBO ENGINE
# =====================================================================
turbo_synthesize <- function(data, n_rows, seed, cons = NULL) {
  set.seed(if (is.null(seed)) sample.int(.Machine$integer.max, 1) else seed)
  idx <- sample.int(nrow(data), n_rows, replace = TRUE)
  out <- data[idx, , drop = FALSE]

  for (j in which(vapply(out, is.numeric, logical(1)))) {
    x <- out[[j]]
    s0 <- suppressWarnings(stats::sd(x, na.rm = TRUE))
    if (length(s0) == 1 && is.finite(s0) && s0 > 0) {
      noise <- stats::rnorm(length(x), 0, s0 * 0.03)
      noise[is.na(x)] <- 0
      out[[j]] <- x + noise
    }
  }

  # Enforce value constraints (semantic + observed ranges)
  cres <- tryCatch(enforce_constraints(out, cons),
                   error = function(e) list(data = out, notes = character(0)))
  out <- cres$data
  note_txt <- "Turbo engine: whole-row resampling. Distributions preserved, rows near-copies."
  if (length(cres$notes)) note_txt <- paste(note_txt, paste(cres$notes, collapse = " "))

  rownames(out) <- NULL
  list(data = out, engine = "turbo", msg = NULL,
       note = note_txt,
       prepped = data, syn_num = out)
}

# =====================================================================
# SYNTHESIS WORKER (Optimized m=1 Architecture)
# Runs inside a clean callr background process, so every helper it
# needs (constraint enforcement, predictor-matrix validation, ...) is
# defined INSIDE the function body.
#
# NOTE (synthpop API): the number of synthetic rows is controlled by the
# `k` argument of synthpop::syn().  Passing `n = <rows>` (as earlier
# versions did) never reaches a "rows" parameter — R partially matches
# it to `numtocat`, producing the misleading error
# "Column indices in visit.sequence must be between 1 and <ncol>".
# =====================================================================
synthesis_worker <- function(df, n_rows, method, seed, cons = NULL,
                             max_levels = 50, fit_sample_max = 2000L,
                             predictor_limit = 5L) {

  # -- fail fast with a clean message if the engine is missing ---------
  if (!requireNamespace("synthpop", quietly = TRUE)) {
    return(list(data = NULL, engine = "synthpop-missing",
                msg = paste0("The 'synthpop' package is not installed in this R ",
                             "environment, so model-based engines cannot run. Install ",
                             "it with install.packages('synthpop', repos = 'https://cloud.r-project.org') ",
                             "and restart the app. (The Turbo engine works without synthpop.)"),
                note = NULL, prepped = NULL, syn_num = NULL))
  }

  # -- sanitise the seed so set.seed() can never throw here ------------
  if (!is.null(seed)) {
    seed <- suppressWarnings(as.integer(seed))
    if (is.na(seed) || seed < 0 || seed > .Machine$integer.max) seed <- NULL
  }

  # ---- helpers defined inside so the background process finds them ----
  safe_txt <- function(x) {
    vapply(as.character(x), function(s) {
      out <- tryCatch(enc2utf8(s), error = function(e) NULL)
      if (is.null(out)) return("?")
      out <- tryCatch(iconv(out, from = "UTF-8", to = "UTF-8", sub = "?"),
                      error = function(e) NULL)
      if (is.null(out) || is.na(out)) "?" else out
    }, character(1), USE.NAMES = FALSE)
  }

  # Enforce per-column value constraints (semantic + observed range).
  enforce_constraints <- function(syn_d, cons_list) {
    notes_out <- character(0)
    if (is.null(cons_list) || !length(cons_list))
      return(list(data = syn_d, notes = notes_out))
    for (cc in cons_list) {
      tryCatch({
        cn <- cc$name
        if (!cn %in% names(syn_d)) next
        v <- syn_d[[cn]]
        if (!is.numeric(v)) next
        lo <- cc$lo; hi <- cc$hi
        if (!is.finite(lo) || !is.finite(hi) || lo > hi) next
        n_bad <- sum(is.finite(v) & (v < lo | v > hi))
        v2 <- pmin(pmax(v, lo), hi)
        if (isTRUE(cc$integer)) v2 <- round(v2)
        syn_d[[cn]] <- v2
        if (n_bad > 0)
          notes_out <- c(notes_out,
                         sprintf("'%s': %d out-of-range value(s) clipped to [%s, %s]",
                                 cn, n_bad, format(lo), format(hi)))
      }, error = function(e) NULL)
    }
    list(data = syn_d, notes = notes_out)
  }

  # Validate the predictor matrix against the frame being synthesised:
  # correct dimensions, dimnames identical to the data names in order,
  # zero diagonal, only 0/1 entries.  Anything else -> use synthpop's
  # default predictor matrix instead of failing.
  validate_pm <- function(pmx, d) {
    if (is.null(pmx)) return(NULL)
    nms <- names(d); n <- length(nms)
    ok <- is.matrix(pmx) && nrow(pmx) == n && ncol(pmx) == n &&
      identical(dimnames(pmx)[[1]], nms) && identical(dimnames(pmx)[[2]], nms) &&
      all(diag(pmx) == 0) && all(pmx %in% c(0, 1))
    if (isTRUE(ok)) pmx else NULL
  }

  say <- function(...) {
    txt <- tryCatch(paste0(...), error = function(e) "…")
    try(cat("MSG|", safe_txt(txt)[1], "\n", sep = ""), silent = TRUE)
    flush(stdout())
  }

  engine <- "synthpop"; msg <- NULL; notes <- character(0)

  say("Preparing data…")
  data <- as.data.frame(df)
  date_cols <- character(0)

  for (j in seq_len(ncol(data))) {
    nm <- names(data)[j]; x <- data[[j]]
    if (inherits(x, c("POSIXct", "POSIXlt", "Date"))) {
      data[[j]] <- as.numeric(x)
      date_cols <- c(date_cols, nm)
    } else if (is.logical(x)) {
      data[[j]] <- factor(ifelse(is.na(x), NA_character_, ifelse(x, "TRUE", "FALSE")))
    } else if (is.character(x)) {
      data[[j]] <- factor(x)
    }
    x <- data[[j]]
    if (is.factor(x)) {
      x <- droplevels(x); nl <- nlevels(x)
      if (nl > max_levels) {
        tab <- sort(table(x), decreasing = TRUE)
        top <- names(tab)[seq_len(max_levels - 1)]
        lv  <- as.character(x)
        lv[!is.na(lv) & !(lv %in% top)] <- "(other)"
        x <- factor(lv)
        notes <- c(notes, sprintf("'%s' had %d categories — grouped rest as (other).", nm, nl))
      }
      data[[j]] <- x
    }
  }

  # defensive: drop completely empty factor columns (nothing to model)
  empty_cols <- names(data)[vapply(data, function(x)
    is.factor(x) && nlevels(droplevels(x)) == 0, logical(1))]
  if (length(empty_cols)) {
    data[empty_cols] <- NULL
    notes <- c(notes, paste0("empty column(s) removed: ",
                             paste(empty_cols, collapse = ", ")))
  }

  n_src <- nrow(data)

  fit_data <- data
  if (n_src > fit_sample_max) {
    set.seed(if (!is.null(seed)) seed else sample.int(.Machine$integer.max, 1))
    fit_data <- data[sample.int(n_src, fit_sample_max), , drop = FALSE]
    notes <- c(notes, sprintf("models fitted on %s of %s rows",
                              format(nrow(fit_data), big.mark = ","),
                              format(n_src, big.mark = ",")))
    say(sprintf("Fitting on a sample of %s rows…", format(nrow(fit_data), big.mark = ",")))
  }

  if (method == "ctree" && nrow(fit_data) * ncol(fit_data) > 6e4) {
    notes <- c(notes, "ctree is slow at this size — switched to CART.")
    method <- "cart"
  }
  if (method == "parametric") {
    mx <- max(vapply(fit_data, function(x) if (is.factor(x)) nlevels(x) else 0L,
                     integer(1)), 0L)
    if (mx > 20) {
      notes <- c(notes, "parametric with high cardinality is slow — switched to CART.")
      method <- "cart"
    }
  }

  pm <- NULL
  if (predictor_limit > 0 && ncol(fit_data) > 3) {
    if (ncol(fit_data) <= 15) {
      say("Scoring column associations…")

      pair_assoc <- function(x, y) {
        tryCatch({
          if (is.numeric(x) && is.numeric(y)) {
            a <- suppressWarnings(stats::cor(x, y, use = "pairwise.complete.obs"))
            if (is.finite(a)) abs(a) else 0
          } else if (is.factor(x) && is.factor(y)) {
            tab <- table(x, y)
            if (min(dim(tab)) < 2 || sum(tab) == 0) return(0)
            ch <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
            if (!is.finite(ch$statistic) || ch$statistic <= 0) 0
            else min(1, sqrt(as.numeric(ch$statistic) / (sum(tab) * (min(dim(tab)) - 1))))
          } else {
            num <- if (is.numeric(x)) x else y
            fct <- if (is.factor(x)) x else y
            ok  <- !is.na(num) & !is.na(fct)
            num <- num[ok]; fct <- droplevels(fct[ok])
            if (length(num) < 10 || nlevels(fct) < 2) return(0)
            fit <- summary(stats::aov(num ~ fct))[[1]]
            if (nrow(fit) < 2 || !is.finite(fit[1, 2]) || fit[2, 2] <= 0) 0
            else min(1, fit[1, 2] / (fit[1, 2] + fit[2, 2]))
          }
        }, error = function(e) 0)
      }

      build_pred_matrix <- function(d, k) {
        nms <- names(d); n <- length(nms)
        if (n < 3) return(NULL)
        if (!is.null(seed)) set.seed(seed)
        sample_n <- min(nrow(d), 1000L)
        dd <- if (nrow(d) > sample_n) d[sample.int(nrow(d), sample_n), , drop = FALSE] else d
        A <- matrix(0, n, n, dimnames = list(nms, nms))
        for (a in seq_len(n - 1)) {
          for (b in (a + 1):n) {
            v <- pair_assoc(dd[[a]], dd[[b]])
            A[a, b] <- v; A[b, a] <- v
          }
        }
        pm2 <- matrix(0, n, n, dimnames = list(nms, nms))
        for (pos in 2:n) {
          j <- nms[pos]; prev <- nms[seq_len(pos - 1)]
          keep <- if (length(prev) > k)
            names(sort(A[prev, j], decreasing = TRUE))[seq_len(k)] else prev
          pm2[j, keep] <- 1
        }
        pm2
      }

      pm <- tryCatch(build_pred_matrix(fit_data, as.integer(predictor_limit)),
                     error = function(e) NULL)
      if (!is.null(pm))
        notes <- c(notes, sprintf("fast mode: top %d predictors used", predictor_limit))

    } else {
      say("Wide dataset detected — using fast banded predictors…")
      pm <- tryCatch({
        nms <- names(fit_data); n <- length(nms)
        pm0 <- matrix(0, n, n, dimnames = list(nms, nms))
        kk <- as.integer(predictor_limit)
        for (pos in 2:n) {
          prev_idx <- max(1, pos - kk):(pos - 1)
          pm0[pos, nms[prev_idx]] <- 1
        }
        pm0
      }, error = function(e) NULL)
      if (!is.null(pm))
        notes <- c(notes, sprintf("fast mode: each column modelled from its previous %d columns",
                                  predictor_limit))
    }
    # never let an inconsistent matrix reach the engine
    pm <- validate_pm(pm, fit_data)
  }

  impute_for_syn <- function(d) {
    out <- d
    for (j in seq_len(ncol(out))) {
      x <- out[[j]]
      if (anyNA(x)) {
        if (is.numeric(x)) {
          v <- stats::median(x, na.rm = TRUE)
          if (!is.finite(v)) v <- 0
        } else {
          tab <- table(x)
          v <- if (length(tab)) names(tab)[which.max(tab)] else NULL
          if (is.null(v)) next
        }
        x[is.na(x)] <- v
        out[[j]] <- x
      }
    }
    out
  }

  reapply_missing <- function(syn_d, orig) {
    cols <- intersect(names(syn_d), names(orig))
    if (!length(cols)) return(syn_d)
    na_mask <- is.na(orig[, cols, drop = FALSE])
    if (!any(na_mask)) return(syn_d)
    sel <- sample.int(nrow(na_mask), nrow(syn_d), replace = TRUE)
    for (j in seq_along(cols)) {
      pos <- which(na_mask[sel, j])
      if (length(pos)) syn_d[[cols[j]]][pos] <- NA
    }
    syn_d
  }

  restore_dates <- function(syn_d, orig, dcols) {
    for (cn in intersect(dcols, names(syn_d))) {
      if (inherits(orig[[cn]], "Date")) {
        syn_d[[cn]] <- as.Date(round(syn_d[[cn]]), origin = "1970-01-01")
      } else {
        tz <- attr(orig[[cn]], "tzone")
        if (is.null(tz) || tz == "") tz <- "UTC"
        syn_d[[cn]] <- as.POSIXct(syn_d[[cn]], origin = "1970-01-01", tz = tz)
      }
    }
    syn_d
  }

  # ---- fit one synthesis with the engine -----------------------------
  # rows are requested via `k` (synthpop's actual parameter).
  fit_one <- function(d, mth, n_target, use_pm = TRUE) {
    if (!is.null(seed)) set.seed(seed)

    pm_use <- if (isTRUE(use_pm)) pm else NULL    # pm is already validated
    args <- list(data = d, method = mth, m = 1L,
                 k = max(1L, as.integer(n_target)))
    if (!is.null(pm_use)) args$predictor.matrix <- pm_use
    if (!is.null(seed)) args$seed <- seed

    # keep only arguments the installed synthpop::syn actually accepts
    # (protects against API differences between synthpop versions)
    fmls <- names(formals(synthpop::syn))
    args <- args[names(args) %in% fmls]

    say(sprintf("Fitting %d column models (generating %s rows)…",
                ncol(d), format(n_target, big.mark = ",")))
    s <- do.call(synthpop::syn, args)
    as.data.frame(s$syn)
  }

  has_na <- any(vapply(fit_data, anyNA, logical(1)))

  # Attempt ladder: each method is retried without the custom predictor
  # matrix and (if the data has NAs) with imputed values, so a rejected
  # predictor set or NA-related failure never forces the low-quality
  # bootstrap fallback on its own.
  attempts <- list()
  add <- function(tag, mth, imp, use_pm)
    attempts[[length(attempts) + 1]] <<- list(tag = tag, mth = mth,
                                              imp = imp, use_pm = use_pm)
  pm_tag <- if (is.null(pm)) "all predictors" else "fast predictors"
  add(paste0("method '", method, "' (", pm_tag, ")"), method, FALSE, !is.null(pm))
  if (!is.null(pm))
    add(paste0("method '", method, "' (all predictors)"), method, FALSE, FALSE)
  if (has_na) {
    add(paste0("method '", method, "', imputed (", pm_tag, ")"), method, TRUE, !is.null(pm))
    if (!is.null(pm))
      add(paste0("method '", method, "', imputed (all predictors)"), method, TRUE, FALSE)
  }
  if (method != "cart") {
    add(paste0("robust CART (", pm_tag, ")"), "cart", FALSE, !is.null(pm))
    if (!is.null(pm))
      add("robust CART (all predictors)", "cart", FALSE, FALSE)
    if (has_na) {
      add(paste0("robust CART, imputed (", pm_tag, ")"), "cart", TRUE, !is.null(pm))
      if (!is.null(pm))
        add("robust CART, imputed (all predictors)", "cart", TRUE, FALSE)
    }
  }

  imputed <- NULL; syn_out <- NULL; used_tag <- NULL; used_imp <- FALSE
  skip_noncart <- FALSE; k <- 0

  for (a in attempts) {
    if (skip_noncart && a$mth != "cart") next
    k <- k + 1
    d <- fit_data
    if (isTRUE(a$imp)) {
      if (is.null(imputed)) {
        say("Imputing missing values for modelling…")
        imputed <- impute_for_syn(fit_data)
      }
      d <- imputed
    }
    say(sprintf("Fitting column models (attempt %d: %s)…", k, a$tag))
    t1 <- Sys.time()
    syn_out <- tryCatch(fit_one(d, a$mth, n_rows, a$use_pm),
                        error = function(e) { msg <<- conditionMessage(e); NULL })
    el <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
    if (!is.null(syn_out)) {
      msg <- NULL   # success: don't surface the previous attempt's error
      used_tag <- a$tag; used_imp <- isTRUE(a$imp)
      break
    }
    say(sprintf("Attempt failed after %.0fs: %s", el, msg))
    if (el > 45 && a$mth != "cart") {
      skip_noncart <- TRUE
      notes <- c(notes, "selected method failed — switching directly to CART")
    }
  }

  if (!is.null(syn_out)) {
    if (k > 1)
      notes <- c(notes, paste0("engine note: used ", used_tag,
                               " after an earlier attempt failed"))
    if (used_imp) {
      notes <- c(notes, "missing values were imputed for modelling, then re-introduced")
      syn_out <- tryCatch(reapply_missing(syn_out, data), error = function(e) syn_out)
    }
  } else {
    engine <- "fallback"
    set.seed(if (!is.null(seed)) seed else sample.int(.Machine$integer.max, 1))
    say("Engine failed — using a simple bootstrap sampler as a low-quality preview…")
    idx <- sample.int(n_src, n_rows, replace = TRUE)
    syn_out <- data[idx, , drop = FALSE]
    for (j in which(vapply(syn_out, is.numeric, logical(1)))) {
      s0 <- suppressWarnings(stats::sd(syn_out[[j]], na.rm = TRUE))
      if (length(s0) == 1 && is.finite(s0) && s0 > 0)
        syn_out[[j]] <- syn_out[[j]] + stats::rnorm(nrow(syn_out), 0, s0 * 0.02)
    }
  }

  rownames(syn_out) <- NULL
  if (!all(names(data) %in% names(syn_out)))
    notes <- c(notes, "engine returned fewer columns than expected")

  say("Restoring column types & enforcing value constraints…")
  syn_num <- syn_out

  cres <- enforce_constraints(syn_num, cons)
  syn_num <- cres$data
  notes  <- c(notes, cres$notes)

  if (length(date_cols)) {
    syn_out <- tryCatch(restore_dates(syn_num, df, date_cols),
                        error = function(e) syn_num)
  } else {
    syn_out <- syn_num
  }
  say("Done.")

  list(data = syn_out, engine = engine, msg = msg,
       note = if (length(notes)) paste(notes, collapse = " · ") else NULL,
       prepped = data, syn_num = syn_num)
}

# =====================================================================
# QUALITY METRICS
# =====================================================================
col_similarity <- function(r, s) {
  tryCatch({
    if (is.numeric(r) && is.numeric(s)) {
      r2 <- r[is.finite(r)]; s2 <- s[is.finite(s)]
      if (length(r2) == 0 || length(s2) == 0) return(NA_real_)
      rng <- range(c(r2, s2))
      if (diff(rng) == 0) rng <- rng + c(-0.5, 0.5)
      br <- seq(rng[1], rng[2], length.out = 16)
      pr <- hist(r2, breaks = br, plot = FALSE)$counts
      ps <- hist(s2, breaks = br, plot = FALSE)$counts
      1 - 0.5 * sum(abs(pr / sum(pr) - ps / sum(ps)))
    } else {
      r2 <- as.character(r); s2 <- as.character(s)
      lv <- unique(c(r2[!is.na(r2)], s2[!is.na(s2)]))
      if (length(lv) == 0) return(NA_real_)
      tr <- table(factor(r2, levels = lv)); ts <- table(factor(s2, levels = lv))
      if (sum(tr) == 0 || sum(ts) == 0) return(NA_real_)
      1 - 0.5 * sum(abs(tr / sum(tr) - ts / sum(ts)))
    }
  }, error = function(e) NA_real_)
}

compute_fidelity <- function(real, syn) {
  common <- intersect(names(real), names(syn))
  if (!length(common)) return(list(overall = NA_integer_, corr = NA_real_, per = NULL))

  sims <- vapply(common, function(cn) col_similarity(real[[cn]], syn[[cn]]),
                 numeric(1))
  per <- data.frame(column = common,
                    type = vapply(real[common], col_type_label, character(1)),
                    sim = sims, stringsAsFactors = FALSE)
  overall <- if (any(!is.na(sims))) round(100 * mean(sims, na.rm = TRUE)) else NA_integer_

  corr_fid <- NA_real_
  num_cols <- intersect(common, names(real)[vapply(real, is.numeric, logical(1))])
  if (length(num_cols) >= 2) {
    cr <- suppressWarnings(tryCatch(cor(as.data.frame(real[num_cols]), use = "pairwise.complete.obs"), error = function(e) NULL))
    cs <- suppressWarnings(tryCatch(cor(as.data.frame(syn[num_cols]), use = "pairwise.complete.obs"), error = function(e) NULL))
    if (!is.null(cr) && !is.null(cs)) {
      lr <- cr[lower.tri(cr)]; ls <- cs[lower.tri(cs)]
      keep <- is.finite(lr) & is.finite(ls)
      if (any(keep)) corr_fid <- round(100 * max(0, 1 - mean(abs(lr[keep] - ls[keep])) / 2), 1)
    }
  }
  list(overall = overall, corr = corr_fid, per = per)
}

# =====================================================================
# PREVIEW TABLE + DEMO DATA
# =====================================================================
make_preview_table <- function(df, n = 6) {
  df <- as.data.frame(df)
  if (nrow(df) == 0 || ncol(df) == 0)
    return(HTML("<p class='sdg-muted'>Empty dataset.</p>"))
  esc   <- htmltools::htmlEscape
  types <- vapply(df, col_type_label, character(1))
  hd    <- head(df, n)

  fmt_col <- function(x, t) {
    out <- if (t == "num") fmt_num(x) else as.character(x)
    out[is.na(out)] <- "—"
    vapply(out, function(s) {
      s <- as.character(s)
      if (is.na(s) || s == "NA") "—"
      else if (nchar(s) > 28) paste0(substr(s, 1, 25), "…") else s
    }, character(1), USE.NAMES = FALSE)
  }

  th <- vapply(seq_along(df), function(j) {
    sprintf("<th><span class='th-name'>%s</span><span class='tbadge tbadge-%s'>%s</span></th>",
            esc(names(df)[j]), types[j], type_labs[[types[j]]])
  }, character(1))

  tr <- vapply(seq_len(nrow(hd)), function(i) {
    tds <- vapply(seq_along(df), function(j) {
      v <- esc(fmt_col(hd[[j]][i], types[j]))
      if (types[j] == "num") sprintf("<td class='mono'>%s</td>", v) else sprintf("<td>%s</td>", v)
    }, character(1))
    paste0("<tr>", paste(tds, collapse = ""), "</tr>")
  }, character(1))

  HTML(sprintf(
    paste0("<div class='sdg-table-wrap'><table class='sdg-table'>",
           "<thead><tr>%s</tr></thead><tbody>%s</tbody></table></div>",
           "<p class='tbl-note'>Showing first %d of %s rows · %d columns · '—' = missing</p>"),
    paste(th, collapse = ""), paste(tr, collapse = ""),
    min(n, nrow(df)), format(nrow(df), big.mark = ","), ncol(df)))
}

make_demo_data <- function(n = 240) {
  set.seed(20240)
  dept <- sample(c("Engineering", "Product", "Research", "Sales", "Support"), n, TRUE, prob = c(.25, .15, .20, .25, .15))
  experience <- pmax(0, round(rnorm(n, 7, 4)))
  level <- cut(experience, breaks = c(-1, 3, 7, 12, Inf), labels = c("Junior", "Mid", "Senior", "Lead"))
  salary <- round(38e3 + experience * 3600 + 14e3 * (level == "Lead") + rnorm(n, 0, 4500), -2)
  performance <- round(pmin(100, pmax(20, 56 + 0.8 * experience + rnorm(n, 0, 9))), 1)
  satisfaction <- round(pmin(10, pmax(1, 5.4 + 0.5 * as.numeric(scale(salary)) + 0.02 * (performance - 70) + rnorm(n, 0, 1.1))), 1)
  work_mode <- sample(c("Hybrid", "On-site", "Remote"), n, TRUE, prob = c(.5, .3, .2))
  promoted <- factor(ifelse(performance + 2 * satisfaction > 82, "Yes", "No"))
  data.frame(dept, experience, level, salary, performance, satisfaction, work_mode, promoted, stringsAsFactors = FALSE)
}

# =====================================================================
# UI
# =====================================================================
ui <- fluidPage(
  title = "SynthData Studio · Synthetic data that preserves statistical relationships",
  useShinyjs(),

  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(name = "description", content = paste0("Generate statistically faithful synthetic datasets from CSV, TSV or Excel files ",
                      "— distributions, correlations and category structure preserved. ",
                      "Runs entirely inside the app's R session — private by design, no external services.")),
    tags$meta(property = "og:title", content = "SynthData Studio"),
    tags$meta(property = "og:description", content = "Synthetic data generator that preserves real statistical relationships — private by design, no external services."),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(name = "theme-color", content = "#0b0b10"),
    tags$link(rel = "icon", type = "image/svg+xml",
              href = paste0("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 32 32'%3E",
                            "%3Ccircle cx='16' cy='16' r='13' fill='none' stroke='%238b5cf6' stroke-width='3'/%3E",
                            "%3Cpath d='M10 16l4 4 8-8' fill='none' stroke='%233b82f6' stroke-width='3' stroke-linecap='round'/%3E%3C/svg%3E")),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = TRUE),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&family=JetBrains+Mono:wght@400;600&display=swap"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css"),

    tags$style(HTML("
:root{
  color-scheme:dark;
  --bg:#0b0b10; --panel:#14141c; --panel2:#191924;
  --line:rgba(255,255,255,.08); --line2:rgba(255,255,255,.05);
  --txt:#e4e4ea; --dim:#9aa0ac; --faint:#6f7685;
  --violet:#8b5cf6; --blue:#3b82f6; --lav:#a78bfa;
  --ok:#34d399; --warn:#fbbf24; --err:#f87171;
  --grad:linear-gradient(135deg,#7c3aed,#2563eb);
  --mono:'JetBrains Mono',ui-monospace,SFMono-Regular,monospace;
}
*{box-sizing:border-box}
html{scroll-behavior:smooth;scrollbar-color:rgba(255,255,255,.16) transparent;scrollbar-width:thin}
body{font-family:'Inter',system-ui,-apple-system,sans-serif;background:var(--bg);color:var(--txt);margin:0;-webkit-font-smoothing:antialiased;zoom:.8}
body .container-fluid{padding:0}
::selection{background:rgba(139,92,246,.35)}
::-webkit-scrollbar{width:9px;height:9px}
::-webkit-scrollbar-track{background:transparent}
::-webkit-scrollbar-thumb{background:rgba(255,255,255,.12);border-radius:5px}
.sdg-muted{color:var(--faint);font-size:13px}

body::before{content:'';position:fixed;top:0;left:0;right:0;bottom:0;z-index:-1;pointer-events:none;background:
  radial-gradient(ellipse 80% 55% at 12% 0%, rgba(139,92,246,.20) 0%, transparent 60%),
  radial-gradient(ellipse 70% 60% at 90% 12%, rgba(59,130,246,.16) 0%, transparent 60%),
  radial-gradient(ellipse 95% 60% at 50% 112%, rgba(236,72,153,.10) 0%, transparent 55%),
  radial-gradient(ellipse 45% 40% at 82% 62%, rgba(139,92,246,.10) 0%, transparent 55%),
  radial-gradient(ellipse 50% 40% at 15% 72%, rgba(59,130,246,.07) 0%, transparent 55%)}

.g-orb{position:fixed;border-radius:50%;filter:blur(90px);pointer-events:none;z-index:-1}
.g-orb-1{width:430px;height:430px;background:rgba(139,92,246,.30);top:-150px;left:-130px;animation:sdg-orb-a 26s ease-in-out infinite alternate}
.g-orb-2{width:380px;height:380px;background:rgba(59,130,246,.20);top:-90px;right:-130px;animation:sdg-orb-b 32s ease-in-out infinite alternate}
.g-orb-3{width:320px;height:320px;background:rgba(236,72,153,.09);bottom:-140px;left:28%;animation:sdg-orb-c 22s ease-in-out infinite alternate}
@keyframes sdg-orb-a{from{transform:translate(0,0)}to{transform:translate(46px,30px)}}
@keyframes sdg-orb-b{from{transform:translate(0,0)}to{transform:translate(-38px,24px)}}
@keyframes sdg-orb-c{from{transform:translate(0,0)}to{transform:translate(30px,-24px)}}

.sdg-nav{position:sticky;top:0;z-index:60;display:flex;align-items:center;justify-content:space-between;gap:12px;flex-wrap:wrap;padding:13px 28px;background:rgba(11,11,16,.72);backdrop-filter:blur(14px);-webkit-backdrop-filter:blur(14px);border-bottom:1px solid var(--line2)}
.sdg-logo{display:flex;align-items:center;gap:10px;font-weight:700;font-size:19px;color:#fff;letter-spacing:-.01em}
.sdg-logo svg{width:30px;height:30px;transition:transform .3s ease}
.sdg-logo:hover svg{transform:rotate(-8deg) scale(1.08)}
.sdg-links{display:flex;gap:8px;align-items:center;flex-wrap:wrap}
.sdg-links a{color:var(--dim);text-decoration:none;font-size:13.5px;font-weight:500;padding:8px 14px;border-radius:9px;transition:.2s}
.sdg-links a:hover{color:#fff;background:rgba(255,255,255,.05)}
.sdg-links a i{margin-right:7px}
.sdg-gh{border:1px solid var(--line)}
.sdg-gh:hover{border-color:rgba(139,92,246,.5)!important;background:rgba(139,92,246,.08)!important}

.sdg-hero{position:relative;text-align:center;padding:78px 24px 46px;max-width:800px;margin:0 auto}
.sdg-hero::before{content:'';position:absolute;top:-30px;left:10%;right:10%;bottom:0;background:radial-gradient(ellipse 60% 50% at 50% 40%, rgba(139,92,246,.14), transparent 70%);pointer-events:none}
.sdg-hero h1{font-size:clamp(30px,5vw,46px);font-weight:750;line-height:1.08;margin:0 0 18px;color:#fff;letter-spacing:-.03em}
.sdg-hero h1 span{background:linear-gradient(135deg,#c084fc,#60a5fa,#c084fc);background-size:200% 200%;-webkit-background-clip:text;background-clip:text;-webkit-text-fill-color:transparent;animation:sdg-grad 9s ease infinite}
.sdg-hero p{font-size:16.5px;color:var(--dim);margin:0 auto 30px;max-width:560px;line-height:1.65}
.sdg-btns{display:flex;justify-content:center;gap:14px;flex-wrap:wrap}
.sdg-trust{display:flex;justify-content:center;gap:26px;flex-wrap:wrap;margin-top:38px}
.sdg-trust-item{display:flex;align-items:center;gap:8px;font-size:13px;color:var(--dim);transition:color .2s}
.sdg-trust-item:hover{color:#fff}
.sdg-trust-item i{color:var(--ok);font-size:13px}
.sdg-scroll{text-align:center;padding-bottom:26px;color:rgba(255,255,255,.28)}
.sdg-scroll i{font-size:20px;animation:sdg-bounce 2.2s ease infinite}
.sdg-scroll p{margin:2px 0 0;font-size:11.5px;color:rgba(255,255,255,.28)}
@keyframes sdg-bounce{0%,100%{transform:translateY(0)}50%{transform:translateY(6px)}}

.sdg-btn{display:inline-flex;align-items:center;gap:9px;padding:12px 22px;border-radius:11px;font-size:14px;font-weight:600;cursor:pointer;transition:transform .2s,box-shadow .2s,filter .2s,background .2s,border-color .2s;font-family:inherit;line-height:1;text-decoration:none}
.sdg-btn:active{transform:scale(.97)}
.sdg-btn i{transition:transform .25s ease}
.sdg-btn:hover i{transform:translateY(-1px) scale(1.06)}
.sdg-btn-pri{background-color:#7c3aed!important;background-image:var(--grad)!important;color:#fff!important;border:1px solid transparent!important;text-shadow:none!important;box-shadow:0 8px 24px rgba(124,58,237,.35);background-size:200% 200%;animation:sdg-grad 7s ease infinite}
.sdg-btn-pri:hover{transform:translateY(-1px);box-shadow:0 14px 34px rgba(124,58,237,.5);filter:brightness(1.08);color:#fff!important}
.sdg-btn-pri:disabled{opacity:.55;cursor:not-allowed;transform:none;box-shadow:none;filter:none;animation:none}
.sdg-btn-sec{background-color:transparent!important;background-image:none!important;color:var(--txt)!important;border:1px solid var(--line)!important;text-shadow:none!important;box-shadow:none!important}
.sdg-btn-sec:hover{background-color:rgba(255,255,255,.06)!important;border-color:rgba(255,255,255,.2)!important;color:#fff!important;transform:translateY(-1px)}
.sdg-btn:focus-visible{outline:2px solid rgba(167,139,250,.7);outline-offset:2px}
@keyframes sdg-grad{0%,100%{background-position:0% 50%}50%{background-position:100% 50%}}

.sdg-features{max-width:1040px;margin:0 auto;padding:54px 24px 6px;scroll-margin-top:92px}
.sdg-features h2{font-size:27px;font-weight:700;color:#fff;margin:0 0 8px;text-align:center;letter-spacing:-.02em}
.feat-sub{text-align:center;max-width:560px;margin:0 auto 26px;color:var(--dim);font-size:14.5px}
.sdg-feat-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));gap:16px}
.sdg-feat{background:var(--panel);border:1px solid var(--line);border-radius:14px;padding:24px;transition:transform .28s ease,border-color .28s ease,box-shadow .28s ease}
.sdg-feat:hover{transform:translateY(-5px);border-color:rgba(139,92,246,.4);box-shadow:0 16px 44px rgba(0,0,0,.5),0 0 0 1px rgba(139,92,246,.15)}
.feat-ico{width:42px;height:42px;border-radius:11px;background:linear-gradient(135deg,rgba(139,92,246,.2),rgba(59,130,246,.16));border:1px solid rgba(139,92,246,.25);display:flex;align-items:center;justify-content:center;color:var(--lav);font-size:17px;margin-bottom:14px;transition:transform .28s ease,border-color .28s ease}
.sdg-feat:hover .feat-ico{transform:scale(1.1) rotate(-4deg);border-color:rgba(167,139,250,.55)}
.sdg-feat h3{font-size:16px;font-weight:650;color:#fff;margin:0 0 6px}
.sdg-feat p{font-size:13.5px;color:var(--dim);line-height:1.55;margin:0}
.sdg-feat:hover p{color:#b9beca}

.sdg-app-wrap{max-width:1040px;margin:44px auto 64px;padding:0 24px;scroll-margin-top:92px}
.sdg-app{position:relative;background:linear-gradient(180deg,rgba(24,24,34,.93),rgba(16,16,23,.95));border:1px solid var(--line);border-radius:18px;box-shadow:0 24px 70px rgba(0,0,0,.5), inset 0 1px 0 rgba(255,255,255,.04);overflow:hidden}
.sdg-app::before{content:'';display:block;height:2px;background:linear-gradient(90deg,transparent 4%,rgba(139,92,246,.6),rgba(59,130,246,.6),transparent 96%)}
.sdg-app-head{display:flex;align-items:center;justify-content:space-between;gap:14px;flex-wrap:wrap;padding:20px 28px;border-bottom:1px solid var(--line2)}
.sdg-app-title{display:flex;align-items:center;gap:10px;font-size:18px;font-weight:700;color:#fff}
.sdg-app-title i{color:var(--lav);font-size:15px}
.sdg-app-sub{font-size:12.5px;color:var(--dim);margin:4px 0 0}
.sdg-steps{display:flex;border-bottom:1px solid var(--line2);overflow-x:auto;scrollbar-width:none}
.sdg-step{flex:1;min-width:132px;display:flex;align-items:center;gap:9px;padding:13px 16px;color:var(--faint);font-size:13px;font-weight:600;position:relative;white-space:nowrap;transition:color .25s,background .25s}
.sdg-step:hover{background:rgba(255,255,255,.03);color:var(--dim)}
.st-ico{width:25px;height:25px;border-radius:7px;display:inline-flex;align-items:center;justify-content:center;background:rgba(255,255,255,.04);border:1px solid var(--line);font-size:11.5px;font-weight:700;flex:none;transition:.25s}
.sdg-step.active{color:#fff}
.sdg-step.active:hover{color:#fff}
.sdg-step.active .st-ico{background-image:var(--grad);border-color:transparent;color:#fff}
.sdg-step.active::after{content:'';position:absolute;left:0;right:0;bottom:-1px;height:2px;background:var(--grad)}
.sdg-step.done{color:var(--ok)}
.sdg-step.done .st-ico{background:rgba(52,211,153,.1);border-color:rgba(52,211,153,.25);color:var(--ok);font-size:0}
.sdg-step.done .st-ico::after{content:'\\f00c';font-family:'Font Awesome 6 Free';font-weight:900;font-size:12px}
.sdg-sec{padding:26px 28px;border-bottom:1px solid var(--line2)}
.sdg-sec:last-child{border-bottom:none}
.sdg-sec-head{display:flex;align-items:center;gap:12px;margin-bottom:4px}
.sdg-num{display:inline-flex;align-items:center;justify-content:center;width:26px;height:26px;border-radius:8px;background-image:var(--grad);color:#fff;font-size:13px;font-weight:700;flex:none;box-shadow:0 4px 14px rgba(124,58,237,.35)}
.sdg-sec-title{font-size:16.5px;font-weight:650;color:#fff}
.sdg-sec-desc{font-size:13.5px;color:var(--dim);line-height:1.55;margin:6px 0 18px;max-width:660px}

.chip{display:inline-flex;align-items:center;gap:7px;padding:7px 14px;border-radius:999px;font-size:12px;font-weight:600;border:1px solid var(--line);background:rgba(255,255,255,.03);color:var(--dim);transition:border-color .2s,color .2s}
.chip:hover{border-color:rgba(255,255,255,.22);color:#fff}
.chip i{font-size:11px}
.chip-ok{color:var(--ok);border-color:rgba(52,211,153,.25);background:rgba(52,211,153,.07)}
.chip-done{color:var(--lav);border-color:rgba(167,139,250,.3);background:rgba(139,92,246,.1)}

.sdg-upload{position:relative;border:2px dashed rgba(255,255,255,.14);border-radius:14px;padding:38px 24px 32px;text-align:center;cursor:pointer;transition:.25s;background:radial-gradient(ellipse 60% 80% at 50% 0%,rgba(139,92,246,.07),transparent 65%)}
.sdg-upload:hover{border-color:var(--violet);background-color:rgba(139,92,246,.05)}
.sdg-upload.drag{border-color:var(--violet);background-color:rgba(139,92,246,.08);transform:scale(1.01)}
.up-ico{width:64px;height:64px;margin:0 auto 14px;border-radius:18px;background:linear-gradient(135deg,rgba(139,92,246,.18),rgba(59,130,246,.14));border:1px solid rgba(139,92,246,.28);display:flex;align-items:center;justify-content:center;color:var(--lav);font-size:24px;transition:transform .28s ease,box-shadow .28s ease,border-color .28s ease}
.sdg-upload:hover .up-ico{transform:translateY(-5px);border-color:rgba(167,139,250,.55);box-shadow:0 14px 34px rgba(139,92,246,.28)}
.up-title{font-size:16.5px;font-weight:600;color:var(--txt);margin:0 0 6px}
.up-sub{font-size:12.5px;color:var(--faint);margin:0 0 16px;text-transform:uppercase;letter-spacing:.08em}
.sdg-upload .sdg-btn-sec{padding:11px 22px}
.up-hint{font-size:11.5px;color:var(--faint);margin:16px 0 0}
.up-progress{margin-top:14px;display:none;flex-direction:column;align-items:center;gap:8px}
.up-progress.on{display:flex}
.up-status{font-size:12.5px;color:var(--dim);letter-spacing:.01em}
.up-status.err{color:var(--err)}
.up-track{width:240px;height:5px;border-radius:99px;background:rgba(255,255,255,.10);overflow:hidden}
.up-bar{height:100%;width:0%;background-image:var(--grad);border-radius:99px;transition:width .25s ease}
.up-progress.nopct .up-bar{width:40%!important;animation:sdg-slide 1.25s linear infinite}
.up-progress.err .up-track{display:none}
.vh-file{position:absolute;top:0;left:0;width:1px;height:1px;opacity:0;overflow:hidden;pointer-events:none}
.sdg-alt{margin-top:14px;text-align:center}
.sdg-linkbtn{background-color:transparent!important;background-image:none!important;border:none!important;color:var(--lav)!important;font-size:13.5px!important;font-weight:600;padding:7px 12px!important;border-radius:9px!important;box-shadow:none!important;text-shadow:none!important;font-family:inherit;transition:background .2s,color .2s}
.sdg-linkbtn:hover{background-color:rgba(139,92,246,.12)!important;color:#c4b5fd!important}
.sdg-linkbtn:disabled{opacity:.5}

.sdg-meta{display:flex;flex-wrap:wrap;gap:10px;margin-top:18px}
.m-chip{display:inline-flex;align-items:center;gap:8px;padding:9px 14px;border-radius:10px;background:var(--panel2);border:1px solid var(--line);font-size:13px;color:var(--dim);transition:border-color .2s,transform .2s,color .2s}
.m-chip:hover{border-color:rgba(139,92,246,.35);transform:translateY(-1px)}
.m-chip:hover i{color:#c4b5fd}
.m-chip i{color:var(--lav);font-size:12.5px;transition:color .2s}
.m-chip .m-val{font-family:var(--mono);font-weight:600;color:#fff;font-size:12.5px}
.m-chip .m-name{color:var(--txt);font-weight:600;max-width:220px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
button.m-chip-clean{display:inline-flex;align-items:center;gap:8px;background-color:rgba(139,92,246,.1)!important;background-image:none!important;border:1px solid rgba(167,139,250,.35)!important;color:var(--lav)!important;font-family:inherit;font-weight:600;font-size:12.5px!important;border-radius:999px!important;padding:7px 14px!important;cursor:pointer;box-shadow:none!important;text-shadow:none!important;transition:border-color .2s,background-color .2s,color .2s}
button.m-chip-clean:hover{border-color:rgba(167,139,250,.6)!important;background-color:rgba(139,92,246,.18)!important;color:#d6c5ff!important}
button.m-chip-clean i{font-size:11px}
.m-remove{background-color:transparent!important;background-image:none!important;border:1px solid var(--line)!important;color:var(--dim)!important;font-size:12px!important;font-weight:600;padding:9px 14px!important;border-radius:10px!important;box-shadow:none!important;text-shadow:none!important;font-family:inherit;cursor:pointer;transition:border-color .2s,color .2s,background .2s}
.m-remove:hover{border-color:rgba(248,113,113,.4)!important;color:var(--err)!important;background-color:rgba(248,113,113,.06)!important}

.clean-panel{margin-top:12px;background:var(--panel2);border:1px solid rgba(167,139,250,.25);border-radius:12px;padding:16px 18px;max-width:780px}
.clean-head{display:flex;align-items:center;gap:9px;font-size:14px;font-weight:650;color:#fff;margin-bottom:8px}
.clean-head i{color:var(--lav)}
.clean-row{display:flex;align-items:flex-start;gap:9px;font-size:13px;color:var(--dim);padding:6px 0;border-bottom:1px solid var(--line2);line-height:1.5}
.clean-row:last-child{border-bottom:none}
.clean-row i{color:var(--ok);font-size:11px;margin-top:4px;flex:none}

.sdg-table-wrap{overflow-x:auto;border-radius:12px;border:1px solid var(--line);margin-top:16px}
.sdg-table{width:100%;border-collapse:collapse;font-size:13px}
.sdg-table th{background:rgba(255,255,255,.03);font-weight:600;color:var(--dim);font-size:11px;text-transform:uppercase;letter-spacing:.06em;padding:10px 14px;border-bottom:1px solid var(--line);white-space:nowrap;text-align:left}
.sdg-table td{padding:10px 14px;text-align:left;border-bottom:1px solid var(--line2);white-space:nowrap;color:var(--txt);max-width:340px;overflow:hidden;text-overflow:ellipsis}
.sdg-table tbody tr{transition:background .15s}
.sdg-table tbody tr:hover{background:rgba(139,92,246,.07)}
.sdg-table tbody tr:hover td:first-child{box-shadow:inset 2px 0 0 0 var(--lav)}
.sdg-table tr:last-child td{border-bottom:none}
.mono{font-family:var(--mono);font-size:12.5px}
.tbadge{display:inline-block;padding:2px 7px;border-radius:5px;font-size:10px;font-weight:600;letter-spacing:.03em}
.tbadge-num{background:rgba(59,130,246,.14);color:#60a5fa}
.tbadge-cat{background:rgba(139,92,246,.14);color:#a78bfa}
.tbadge-date{background:rgba(236,72,153,.12);color:#f472b6}
.tbadge-other{background:rgba(255,255,255,.07);color:var(--dim)}
.tbl-note{font-size:11.5px;color:var(--faint);margin:8px 2px 0}

.sdg-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(170px,1fr));gap:14px}
.sdg-field label{display:block;font-size:12.5px;color:var(--dim);font-weight:600;margin-bottom:7px;cursor:default}
.sdg-field .form-group{margin-bottom:0}
.sdg-field input{
  width:100%;height:auto;padding:10px 12px;border-radius:10px;
  border:1px solid var(--line)!important;
  background-color:rgba(255,255,255,.03)!important;background-image:none!important;
  color:var(--txt)!important;font-size:14px;font-family:inherit;outline:none;line-height:1.45;
  transition:border-color .2s,box-shadow .2s;box-shadow:none!important}
.sdg-field input:hover{border-color:rgba(255,255,255,.2)!important}
.sdg-field input:focus{border-color:var(--violet)!important;box-shadow:0 0 0 3px rgba(139,92,246,.18)!important}
.sdg-field input::placeholder{color:var(--faint)}
.sdg-field input[type=number]{-moz-appearance:textfield}
.sdg-field input[type=number]::-webkit-outer-spin-button,
.sdg-field input[type=number]::-webkit-inner-spin-button{-webkit-appearance:none;margin:0}
.sdg-hint{font-size:11.5px;color:var(--faint);margin:6px 0 0}

/* ---- native <select>: dark styling so it never flashes white
       (fallback before/without the custom widget) ---- */
.sdg-field select{
  width:100%;height:auto;padding:10px 36px 10px 12px;border-radius:10px;
  border:1px solid var(--line)!important;
  background-color:rgba(255,255,255,.03)!important;
  background-image:url('data:image/svg+xml,%3Csvg xmlns=%27http://www.w3.org/2000/svg%27 width=%2712%27 height=%2712%27 viewBox=%270 0 12 12%27%3E%3Cpath d=%27M2 4l4 4 4-4%27 fill=%27none%27 stroke=%27%239aa0ac%27 stroke-width=%271.6%27 stroke-linecap=%27round%27 stroke-linejoin=%27round%27/%3E%3C/svg%3E')!important;
  background-repeat:no-repeat!important;background-position:right 12px center!important;
  color:var(--txt)!important;font-size:14px;font-family:inherit;outline:none;line-height:1.45;
  cursor:pointer;box-shadow:none!important;
  appearance:none;-webkit-appearance:none;-moz-appearance:none;
  transition:border-color .2s,box-shadow .2s}
.sdg-field select:hover{border-color:rgba(255,255,255,.2)!important}
.sdg-field select:focus{border-color:var(--violet)!important;box-shadow:0 0 0 3px rgba(139,92,246,.18)!important}
.sdg-field select option,.sdg-field select optgroup{background:#17171f;color:#e4e4ea}

/* ---- custom select: dark glassy menu attached to <body>, so no
       ancestor (overflow:hidden / transform) can clip it ---- */
.cs{position:relative;display:block}
.cs .cs-native{position:absolute;top:0;left:0;width:1px!important;height:1px!important;opacity:0!important;overflow:hidden!important;pointer-events:none!important;padding:0!important;border:none!important;background-image:none!important}
.cs-toggle{width:100%;display:flex;align-items:center;justify-content:space-between;gap:10px;padding:10px 12px;border-radius:10px;border:1px solid var(--line);background-color:rgba(255,255,255,.03);background-image:none;color:var(--txt);font-size:14px;font-family:inherit;cursor:pointer;line-height:1.45;transition:border-color .2s,box-shadow .2s,background .2s}
.cs-toggle:hover{border-color:rgba(255,255,255,.2);background-color:rgba(255,255,255,.05)}
.cs.cs-open .cs-toggle{border-color:var(--violet);box-shadow:0 0 0 3px rgba(139,92,246,.18)}
.cs-toggle:focus-visible{outline:2px solid rgba(167,139,250,.7);outline-offset:2px}
.cs-label{flex:1;text-align:left;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
.cs-toggle i{font-size:11px;color:var(--dim);transition:transform .2s;flex:none}
.cs.cs-open .cs-toggle i{transform:rotate(180deg)}
.cs-menu{position:fixed;z-index:4000;display:none;
  background:rgba(22,22,31,.97);backdrop-filter:blur(18px);-webkit-backdrop-filter:blur(18px);
  border:1px solid rgba(255,255,255,.12);border-radius:14px;padding:6px;
  box-shadow:0 24px 64px rgba(0,0,0,.65),0 0 0 1px rgba(139,92,246,.08)}
.cs-menu.open{display:block;animation:sdg-fade-up .18s cubic-bezier(.2,.8,.3,1)}
.cs-list{display:flex;flex-direction:column;gap:1px;max-height:260px;overflow-y:auto;overscroll-behavior:contain;scrollbar-width:thin}
.cs-opt{display:flex;align-items:center;gap:8px;flex:none;width:100%;text-align:left;padding:8px 10px;border-radius:9px;background:none;border:none;color:var(--txt);font-size:13.5px;font-family:inherit;cursor:pointer;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;transition:background .12s,color .12s}
.cs-opt:hover{background:rgba(139,92,246,.14);color:#fff}
.cs-opt.sel{background:rgba(139,92,246,.18);color:#fff;font-weight:600}
.cs-opt.sel::after{content:'\\f00c';font-family:'Font Awesome 6 Free';font-weight:900;font-size:10px;color:var(--lav);margin-left:auto;flex:none}
.cs-opt:focus-visible{outline:2px solid rgba(167,139,250,.7);outline-offset:-2px}
.cs-search{display:block;width:calc(100% - 4px);margin:2px 2px 6px;padding:8px 10px;border-radius:9px;border:1px solid var(--line);background:rgba(255,255,255,.04);color:var(--txt);font-family:inherit;font-size:13px;outline:none;transition:border-color .2s,box-shadow .2s}
.cs-search:focus{border-color:var(--violet);box-shadow:0 0 0 3px rgba(139,92,246,.15)}
.cs-search::placeholder{color:var(--faint)}
.cs-empty{padding:12px;text-align:center;font-size:12.5px;color:var(--faint);display:none}

.gen-actions,.res-actions{display:flex;gap:12px;flex-wrap:wrap;margin-top:18px}

.sdg-overlay{position:fixed;top:0;left:0;right:0;bottom:0;z-index:2000;display:none;align-items:center;justify-content:center;background:rgba(8,8,12,.7);backdrop-filter:blur(7px);-webkit-backdrop-filter:blur(7px)}
.sdg-overlay.open{display:flex}
.sdg-overlay-card{text-align:center;padding:36px 46px;background:var(--panel);border:1px solid var(--line);border-radius:16px;box-shadow:0 30px 80px rgba(0,0,0,.6);max-width:480px}
.sdg-overlay-card h3{margin:0 0 8px;font-size:17px;color:#fff;font-weight:650}
.ov-meta{display:flex;justify-content:space-between;align-items:baseline;gap:18px;margin:0 0 6px}
#ovStatus{margin:0;font-size:13px;color:var(--lav);font-family:var(--mono);flex:1;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;text-align:left}
#ovElapsed{font-size:12px;color:var(--dim);font-family:var(--mono);flex:none}
.ov-log{max-height:96px;overflow-y:auto;scrollbar-width:thin;font-family:var(--mono);font-size:11px;color:var(--faint);background:rgba(255,255,255,.03);border:1px solid var(--line2);border-radius:9px;padding:8px 10px;margin:10px 0 16px;text-align:left;min-width:300px;white-space:pre-wrap;word-break:break-word}
.ov-log:empty{display:none}
.ov-cancel:hover{border-color:rgba(248,113,113,.5)!important;color:var(--err)!important;background-color:rgba(248,113,113,.06)!important}
.ov-sub{font-size:12px;color:var(--faint);margin:10px 0 0}
.spinner{width:46px;height:46px;margin:0 auto 18px;border-radius:50%;border:3px solid rgba(255,255,255,.08);border-top-color:var(--violet);border-right-color:var(--blue);animation:sdg-spin .9s linear infinite}
@keyframes sdg-spin{to{transform:rotate(360deg)}}

.sdg-stats{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:12px;margin-bottom:16px}
.sdg-stat{padding:16px 18px;border-radius:12px;background:rgba(255,255,255,.03);border:1px solid var(--line);position:relative;overflow:hidden;transition:transform .22s ease,border-color .22s ease,box-shadow .22s ease}
.sdg-stat::before{content:'';position:absolute;top:0;left:0;right:0;height:2px;background:var(--grad);opacity:0;transition:opacity .22s}
.sdg-stat:hover{transform:translateY(-4px);border-color:rgba(139,92,246,.35);box-shadow:0 14px 36px rgba(0,0,0,.45)}
.sdg-stat:hover::before{opacity:1}
.sdg-stat-val{font-size:20px;font-weight:600;color:#fff;font-family:var(--mono);margin-top:8px}
.sdg-stat-lbl{font-size:11px;color:var(--dim);margin-top:4px;text-transform:uppercase;letter-spacing:.05em}
.stat-ico i{color:var(--lav);font-size:14px;transition:transform .2s,color .2s}
.sdg-stat:hover .stat-ico i{transform:scale(1.18);color:#c4b5fd}

.engine-warn{display:flex;gap:10px;align-items:flex-start;color:var(--warn);font-size:13px;background:rgba(251,191,36,.07);border:1px solid rgba(251,191,36,.25);padding:12px 16px;border-radius:10px;margin-bottom:16px;line-height:1.5}
.engine-warn i{margin-top:2px}
.engine-reason{margin:4px 0 0;font-size:12.5px;color:#d3ab52}
.engine-info{display:flex;gap:10px;align-items:flex-start;color:var(--lav);font-size:13px;background:rgba(139,92,246,.08);border:1px solid rgba(139,92,246,.25);padding:12px 16px;border-radius:10px;margin-bottom:16px;line-height:1.5}
.engine-info i{margin-top:2px}

.sdg-app .nav-tabs{border-bottom:1px solid var(--line);margin-bottom:18px;flex-wrap:wrap}
.sdg-app .nav-tabs>li>a{color:var(--dim);border:none;background:transparent;font-size:14px;font-weight:600;padding:10px 16px;margin-right:4px;border-radius:9px 9px 0 0;transition:.2s}
.sdg-app .nav-tabs>li>a:hover{color:#fff;background:rgba(255,255,255,.04);border:none}
.sdg-app .nav-tabs>li>a>i{margin-right:7px}
.sdg-app .nav-tabs>li.active>a,.sdg-app .nav-tabs>li.active>a:hover,.sdg-app .nav-tabs>li.active>a:focus{color:#fff;background:rgba(139,92,246,.1);border:none;border-bottom:2px solid var(--violet)}
.sdg-app .tab-content{min-height:120px}
.sdg-app .shiny-plot-output img{border-radius:12px;background:transparent}

.q-corr{display:inline-flex;align-items:center;gap:8px;font-size:13.5px;color:var(--txt);background:rgba(139,92,246,.08);border:1px solid rgba(139,92,246,.2);padding:8px 14px;border-radius:9px;margin:0 0 10px}
.q-corr i{color:var(--lav)}
.q-corr strong{font-family:var(--mono);color:var(--lav)}
.q-note{font-size:11.5px;color:var(--faint);margin:0 0 12px}
.q-list{border:1px solid var(--line2);border-radius:12px;padding:4px 16px;max-height:430px;overflow-y:auto;overscroll-behavior:contain;scrollbar-width:thin}
.q-row{display:grid;grid-template-columns:minmax(150px,230px) 1fr 52px;gap:14px;align-items:center;padding:10px 0;border-bottom:1px solid var(--line2);transition:background .15s}
.q-row:hover{background:rgba(255,255,255,.02)}
.q-row:last-child{border-bottom:none}
.q-name{font-size:13px;color:var(--txt);font-weight:600;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
.q-name .tbadge{margin-left:7px}
.q-track{height:8px;border-radius:4px;background:rgba(255,255,255,.06);overflow:hidden}
.q-fill{height:100%;border-radius:4px;animation:qgrow .7s ease}
@keyframes qgrow{from{width:0}}
.q-hi{background:linear-gradient(90deg,#34d399,#10b981)}
.q-mid{background:linear-gradient(90deg,#fbbf24,#f59e0b)}
.q-lo{background:linear-gradient(90deg,#f87171,#ef4444)}
.q-val{font-family:var(--mono);font-size:12.5px;color:var(--dim);text-align:right}

.sdg-footer{text-align:center;padding:28px 20px 44px;color:var(--faint);font-size:12.5px}
.sdg-footer a{color:var(--dim);text-decoration:none;transition:color .2s}
.sdg-footer a:hover{color:var(--lav)}

.shiny-notification{background:var(--panel2);color:var(--txt);border:1px solid var(--line);border-radius:12px;box-shadow:0 16px 50px rgba(0,0,0,.5);font-family:'Inter',sans-serif;font-size:13.5px}
.shiny-notification.error,.shiny-notification-error{border-color:rgba(248,113,113,.45)}
.shiny-notification.warning,.shiny-notification-warning{border-color:rgba(251,191,36,.45)}
.shiny-notification.message,.shiny-notification-message{border-color:rgba(52,211,153,.4)}
.js-toast{position:fixed;bottom:26px;left:50%;transform:translate(-50%,16px);display:flex;align-items:center;gap:9px;background:var(--panel2);color:var(--txt);border:1px solid rgba(52,211,153,.35);padding:11px 20px;border-radius:11px;font-size:13.5px;font-weight:600;opacity:0;transition:all .3s;z-index:3000;box-shadow:0 12px 40px rgba(0,0,0,.5)}
.js-toast i{color:var(--ok);font-size:14px}
.js-toast.show{opacity:1;transform:translate(-50%,0)}
.js-toast.err{border-color:rgba(248,113,113,.4)}
.js-toast.err i{color:var(--err)}

.reveal{opacity:0;transform:translateY(14px);transition:opacity .55s ease,transform .55s ease}
.reveal.in{opacity:1;transform:none}

@keyframes sdg-fade-up{from{opacity:0;transform:translateY(10px)}to{opacity:1;transform:translateY(0)}}
.fade-up{animation:sdg-fade-up .45s ease both}
@media (prefers-reduced-motion:reduce){*,*::before,*::after{animation:none!important;transition:none!important}}
@media (max-width:720px){
  .sdg-nav{padding:12px 16px}
  .sdg-links a{padding:7px 10px;font-size:12.5px}
  .sdg-hero{padding:44px 16px 30px}
  .sdg-sec{padding:20px 16px}
  .sdg-app-head{padding:16px}
  .sdg-trust{gap:14px}
}

/* =====================================================================
   v2.0 — experience upgrades
   ===================================================================== */
.ver-badge{font-size:10px;font-weight:700;letter-spacing:.06em;color:var(--lav);background:rgba(139,92,246,.12);border:1px solid rgba(139,92,246,.32);padding:2px 7px;border-radius:999px;margin-left:2px}
.sdg-links a.active{color:#fff;background:rgba(255,255,255,.06)}

.sdg-badge{display:inline-flex;align-items:center;gap:9px;padding:7px 16px;border-radius:999px;font-size:12px;font-weight:600;color:#c4b5fd;background:rgba(139,92,246,.10);border:1px solid rgba(139,92,246,.30);margin-bottom:22px;letter-spacing:.02em}
.sdg-badge .dot{width:6px;height:6px;border-radius:50%;background:var(--ok);box-shadow:0 0 10px rgba(52,211,153,.9);animation:sdg-pulse 2.4s ease infinite}
@keyframes sdg-pulse{0%,100%{opacity:1}50%{opacity:.35}}

/* ---- run summary bar ---- */
.est-bar{display:flex;align-items:center;gap:12px;flex-wrap:wrap;margin-top:18px;padding:13px 17px;border-radius:12px;background:linear-gradient(90deg,rgba(139,92,246,.07),rgba(59,130,246,.05));border:1px solid rgba(139,92,246,.20);font-size:12.5px;color:var(--dim)}
.est-item{display:inline-flex;align-items:center;gap:8px}
.est-item i{color:var(--lav);font-size:12px}
.est-item b{color:#fff;font-family:var(--mono);font-weight:600;font-size:12.5px}
.est-sep{color:rgba(255,255,255,.18);font-size:11px}
.est-time b{color:#c4b5fd}

/* ---- colour-coded fidelity score ---- */
.sdg-stat-val.v-good{color:var(--ok)}
.sdg-stat-val.v-mid{color:var(--warn)}
.sdg-stat-val.v-low{color:var(--err)}

/* ---- overlay progress ---- */
.ov-bar{height:4px;border-radius:99px;background:rgba(255,255,255,.06);overflow:hidden;margin:12px 0 2px;min-width:300px}
.ov-bar::after{content:'';display:block;height:100%;width:42%;border-radius:99px;background-image:var(--grad);animation:sdg-slide 1.5s ease-in-out infinite}
@keyframes sdg-slide{0%{transform:translateX(-110%)}100%{transform:translateX(340%)}}

@media (max-width:720px){
  body{zoom:1}
  .est-bar{gap:8px}
}
")),

    tags$script(HTML("
(function () {
  'use strict';

  var reduceMotion = false;
  try { reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches; } catch (e) {}

  /* =============================================================
     CUSTOM SELECT — dark glassy dropdowns
     The native <select> stays in the DOM (so the Shiny input binding
     keeps working) but is visually replaced. The open menu is appended
     to document.body, so no ancestor with overflow:hidden or a
     lingering transform can clip or shift it. Long option lists get a
     search filter; the whole widget is keyboard accessible.
     ============================================================= */
  var registry = [];

  /* the app renders at 80% via CSS zoom on <body>; menus appended to
     the body share that zoom, so all layout maths below is done in the
     zoomed coordinate space (visual viewport / zoom factor) */
  function zoomFactor() {
    try {
      var z = parseFloat(window.getComputedStyle(document.body).zoom);
      return (isFinite(z) && z > 0) ? z : 1;
    } catch (e) { return 1; }
  }

  function enhanceSelect(sel) {
    if (sel.getAttribute('data-cs') === '1') return;
    if (!sel.options || !sel.options.length) return;
    sel.setAttribute('data-cs', '1');

    var wrap = document.createElement('div');
    wrap.className = 'cs';
    sel.parentNode.insertBefore(wrap, sel);
    wrap.appendChild(sel);
    sel.classList.add('cs-native');
    sel.setAttribute('tabindex', '-1');
    sel.setAttribute('aria-hidden', 'true');

    var toggle = document.createElement('button');
    toggle.type = 'button';
    toggle.className = 'cs-toggle';
    toggle.setAttribute('aria-haspopup', 'listbox');
    toggle.setAttribute('aria-expanded', 'false');
    var label = document.createElement('span');
    label.className = 'cs-label';
    var caret = document.createElement('i');
    caret.className = 'fa-solid fa-chevron-down';
    caret.setAttribute('aria-hidden', 'true');
    toggle.appendChild(label);
    toggle.appendChild(caret);
    wrap.appendChild(toggle);

    var menu = document.createElement('div');
    menu.className = 'cs-menu';
    menu.setAttribute('role', 'listbox');
    menu.style.display = 'none';
    document.body.appendChild(menu);

    var opts = Array.prototype.slice.call(sel.options);
    var needFilter = opts.length > 10;
    var items = [];
    var filter = null;
    var isOpen = false;
    var scrollHandler = null;

    var rec = { wrap: wrap, menu: menu };
    registry.push(rec);

    function visible(i) { return items[i] && items[i].style.display !== 'none'; }

    function focusFirst() {
      for (var i = 0; i < items.length; i++) if (visible(i)) { items[i].focus(); return; }
    }
    function move(from, dir) {
      var n = items.length, idx = from, steps = 0;
      if (!n) return;
      do { idx = (idx + dir + n) % n; steps++; } while (!visible(idx) && steps <= n);
      if (visible(idx)) items[idx].focus();
    }
    function firstVisibleIdx() {
      for (var i = 0; i < items.length; i++) if (visible(i)) return i;
      return -1;
    }

    function applyFilter() {
      var q = filter ? filter.value.toLowerCase() : '';
      var any = false;
      for (var i = 0; i < items.length; i++) {
        var match = !q || (opts[i].textContent || '').toLowerCase().indexOf(q) >= 0;
        items[i].style.display = match ? '' : 'none';
        if (match) any = true;
      }
      var em = menu.querySelector('.cs-empty');
      if (em) em.style.display = any ? 'none' : 'block';
    }

    function setText(el, txt) { if (el.textContent !== txt) el.textContent = txt; }

    function syncLabel() {
      var i = sel.selectedIndex;
      var o = sel.options[i];
      setText(label, o ? o.textContent : '');
      if (toggle.title !== label.textContent) toggle.title = label.textContent;
      for (var j = 0; j < items.length; j++) {
        var on = j === i;
        items[j].classList.toggle('sel', on);
        items[j].setAttribute('aria-selected', on ? 'true' : 'false');
      }
    }

    function place() {
      var z = zoomFactor();
      var r = toggle.getBoundingClientRect();
      /* convert the toggle rect (visual px) into the zoomed layout
         space the menu lives in */
      var vw = window.innerWidth / z, vh = window.innerHeight / z;
      var rl = r.left / z, rt = r.top / z, rb = r.bottom / z, rw = r.width / z;
      var PAD = 10 / z, GAP = 8 / z;
      var below = vh - rb - PAD - GAP;
      var above = rt - PAD - GAP;
      var up = below < 200 / z && above > below;
      var avail = Math.max(120 / z, up ? above : below);

      var w = Math.min(Math.max(rw, 200 / z), 460 / z);
      menu.style.width = w + 'px';
      menu.style.left = Math.max(PAD, Math.min(rl, vw - w - PAD)) + 'px';
      menu.style.top = (rb + GAP) + 'px';

      /* the .cs-list is the scroller (nothing but rows inside the clip
         box), so its height can be snapped to whole rows exactly */
      var list = menu.querySelector('.cs-list');
      var unit = items.length ? items[0].offsetHeight + 1 : 33;
      var menuChrome = menu.offsetHeight - list.offsetHeight;
      var availList = avail - menuChrome;
      var CAP = 260 / z;
      if (availList > CAP) availList = CAP;
      if (availList < unit * 3) availList = unit * 3;
      var nRows = Math.floor(availList / unit);
      list.style.maxHeight = (nRows * unit) + 'px';

      var h = menu.offsetHeight;
      if (up) menu.style.top = Math.max(PAD, rt - h - GAP) + 'px';
    }

    function centerSelected() {
      try {
        var list = menu.querySelector('.cs-list');
        var si = sel.selectedIndex;
        if (si >= 0 && items[si]) {
          var it = items[si];
          var rel = it.offsetTop - list.offsetTop;
          var target = rel + it.offsetHeight / 2 - list.clientHeight / 2;
          if (list.scrollTop < target - 8 || list.scrollTop + list.clientHeight < rel + it.offsetHeight)
            list.scrollTop = Math.max(0, target);
        }
      } catch (e) {}
    }

    function open() {
      if (isOpen) return;
      isOpen = true;
      wrap.classList.add('cs-open');
      menu.classList.add('open');
      toggle.setAttribute('aria-expanded', 'true');
      if (filter) { filter.value = ''; applyFilter(); }
      menu.style.display = 'block';
      place();
      centerSelected();
      if (filter) {
        filter.focus();
      } else if (sel.selectedIndex >= 0 && items[sel.selectedIndex]) {
        items[sel.selectedIndex].focus();
      } else {
        focusFirst();
      }
      scrollHandler = rafThrottle(place);
      window.addEventListener('scroll', scrollHandler, true);
      window.addEventListener('resize', scrollHandler);
    }

    function close(refocus) {
      if (!isOpen) return;
      isOpen = false;
      menu.style.display = 'none';
      menu.classList.remove('open');
      wrap.classList.remove('cs-open');
      toggle.setAttribute('aria-expanded', 'false');
      if (scrollHandler) {
        window.removeEventListener('scroll', scrollHandler, true);
        window.removeEventListener('resize', scrollHandler);
        scrollHandler = null;
      }
      if (refocus) { try { toggle.focus(); } catch (e) {} }
    }

    function rafThrottle(fn) {
      var queued = false;
      return function () {
        if (queued) return;
        queued = true;
        requestAnimationFrame(function () { queued = false; if (isOpen) fn(); });
      };
    }

    function pick(i) {
      if (i < 0 || i >= opts.length) return;
      if (sel.selectedIndex !== i) {
        sel.selectedIndex = i;
        try { sel.dispatchEvent(new Event('change', { bubbles: true })); } catch (e) {}
      }
      syncLabel();
      close(true);
    }

    function build() {
      menu.innerHTML = '';
      items = [];
      if (needFilter) {
        filter = document.createElement('input');
        filter.type = 'text';
        filter.className = 'cs-search';
        filter.placeholder = 'Filter ' + opts.length + ' options…';
        filter.setAttribute('aria-label', 'Filter options');
        filter.autocomplete = 'off';
        filter.spellcheck = false;
        filter.addEventListener('input', applyFilter);
        filter.addEventListener('keydown', function (e) {
          if (e.key === 'ArrowDown') { e.preventDefault(); move(firstVisibleIdx(), 1); }
          else if (e.key === 'Enter') { e.preventDefault(); var fi = firstVisibleIdx(); if (fi >= 0) pick(fi); }
          else if (e.key === 'Escape') { e.preventDefault(); close(true); }
        });
        menu.appendChild(filter);
      }
      var list = document.createElement('div');
      list.className = 'cs-list';
      opts.forEach(function (o, i) {
        var it = document.createElement('button');
        it.type = 'button';
        it.className = 'cs-opt' + (o.selected ? ' sel' : '');
        it.setAttribute('role', 'option');
        it.setAttribute('aria-selected', o.selected ? 'true' : 'false');
        it.textContent = o.textContent;
        it.title = o.textContent;
        it.addEventListener('click', function () { pick(i); });
        it.addEventListener('keydown', function (e) {
          if (e.key === 'ArrowDown') { e.preventDefault(); move(i, 1); }
          else if (e.key === 'ArrowUp') { e.preventDefault(); move(i, -1); }
          else if (e.key === 'Home') { e.preventDefault(); focusFirst(); }
          else if (e.key === 'End') { e.preventDefault(); for (var k = items.length - 1; k >= 0; k--) if (visible(k)) { items[k].focus(); break; } }
          else if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); pick(i); }
          else if (e.key === 'Escape') { e.preventDefault(); close(true); }
          else if (e.key === 'Tab') { close(false); }
        });
        list.appendChild(it);
        items.push(it);
      });
      menu.appendChild(list);
      var empty = document.createElement('div');
      empty.className = 'cs-empty';
      empty.textContent = 'No matching options';
      menu.appendChild(empty);
    }

    toggle.addEventListener('click', function (e) {
      e.stopPropagation();
      if (isOpen) close(false);
      else { closeOthers(rec); open(); }
    });
    toggle.addEventListener('keydown', function (e) {
      if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
        e.preventDefault();
        if (!isOpen) { closeOthers(rec); open(); }
      } else if (e.key === 'Escape') {
        close(false);
      }
    });
    sel.addEventListener('change', syncLabel);

    rec.close = close;
    rec.sync = syncLabel;
    rec.destroy = function () {
      close(false);
      if (menu.parentNode) menu.parentNode.removeChild(menu);
    };

    build();
    syncLabel();
  }

  function closeOthers(except) {
    for (var i = 0; i < registry.length; i++) {
      if (registry[i] !== except && registry[i].close) registry[i].close(false);
    }
  }

  function enhanceAll() {
    var selects = document.querySelectorAll('.sdg-field select');
    Array.prototype.forEach.call(selects, enhanceSelect);
    var alive = [];
    for (var i = 0; i < registry.length; i++) {
      if (!registry[i].wrap.isConnected) { if (registry[i].destroy) registry[i].destroy(); }
      else alive.push(registry[i]);
    }
    registry = alive;
    for (var j = 0; j < registry.length; j++) {
      if (registry[j].sync) registry[j].sync();
    }
  }

  document.addEventListener('click', function (e) {
    for (var i = 0; i < registry.length; i++) {
      var r = registry[i];
      if (r.close && r.wrap.classList.contains('cs-open') &&
          !r.wrap.contains(e.target) && !r.menu.contains(e.target)) r.close(false);
    }
  });
  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') closeOthers(null);
  });

  /* clicking a <label for=...> of an enhanced select must not open the
     native OS picker — focus our toggle instead */
  document.addEventListener('click', function (e) {
    var lb = e.target && e.target.closest ? e.target.closest('label[for]') : null;
    if (!lb) return;
    var id = lb.getAttribute('for');
    if (!id) return;
    var sel = document.getElementById(id);
    if (sel && sel.classList.contains('cs-native')) {
      e.preventDefault();
      var tg = sel.parentNode ? sel.parentNode.querySelector('.cs-toggle') : null;
      if (tg) tg.focus();
    }
  }, true);

  /* =============================================================
     TOASTS
     ============================================================= */
  function showToast(msg, isErr) {
    var t = document.createElement('div');
    t.className = 'js-toast' + (isErr ? ' err' : '');
    var i = document.createElement('i');
    i.className = isErr ? 'fa-solid fa-circle-exclamation' : 'fa-solid fa-circle-check';
    i.setAttribute('aria-hidden', 'true');
    var s = document.createElement('span');
    s.textContent = msg;
    t.appendChild(i);
    t.appendChild(s);
    document.body.appendChild(t);
    requestAnimationFrame(function () { t.classList.add('show'); });
    setTimeout(function () {
      t.classList.remove('show');
      setTimeout(function () { if (t.parentNode) t.parentNode.removeChild(t); }, 350);
    }, 2600);
  }

  /* =============================================================
     STAT COUNT-UP (respects prefers-reduced-motion)
     ============================================================= */
  function animateVal(el) {
    var txt = (el.textContent || '').trim();
    if (!txt || txt === '—') return;
    var m = txt.replace(/,/g, '').match(/^(\\d+(?:\\.\\d+)?)(%|s|ms)?$/);
    if (!m) return;
    var target = parseFloat(m[1]);
    if (!isFinite(target) || target <= 0) return;
    var dec = m[1].indexOf('.') >= 0 ? 1 : 0;
    var suffix = m[2] || '';
    var t0 = null, dur = 650, run = {};
    el.__sdgAnim = run;
    el.__sdgAnimating = true;
    function fmt(v) {
      var s = dec ? v.toFixed(1) : String(Math.round(v));
      if (!dec) { try { s = Number(s).toLocaleString('en-US'); } catch (e) {} }
      return s + suffix;
    }
    function step(ts) {
      if (t0 === null) t0 = ts;
      var p = Math.min(1, (ts - t0) / dur);
      var ease = 1 - Math.pow(1 - p, 3);
      el.textContent = fmt(target * ease);
      if (p < 1 && el.__sdgAnim === run) requestAnimationFrame(step);
      else {
        el.textContent = fmt(target);
        el.__sdgAnimating = false;
        el.__sdgPrev = fmt(target);
      }
    }
    requestAnimationFrame(step);
  }

  function watchStats() {
    var root = document.querySelector('.sdg-stats');
    if (!root || !('MutationObserver' in window)) return;
    var t = null;
    var mo = new MutationObserver(function () {
      if (t) clearTimeout(t);
      t = setTimeout(function () {
        var els = root.querySelectorAll('.sdg-stat-val');
        Array.prototype.forEach.call(els, function (el) {
          var cur = (el.textContent || '').trim();
          if (el.__sdgAnimating) { el.__sdgPrev = cur; return; }
          if (cur && cur !== el.__sdgPrev) {
            el.__sdgPrev = cur;
            if (!reduceMotion) animateVal(el);
          }
        });
      }, 40);
    });
    mo.observe(root, { childList: true, subtree: true, characterData: true });
  }

  /* =============================================================
     NAV SCROLL-SPY
     ============================================================= */
  function initSpy() {
    try {
      if (!('IntersectionObserver' in window)) return;
      var all = document.querySelectorAll('.sdg-links a');
      var links = [];
      Array.prototype.forEach.call(all, function (a) {
        var h = a.getAttribute('href') || '';
        if (h.charAt(0) === '#') links.push(a);
      });
      if (!links.length) return;
      var map = {};
      Array.prototype.forEach.call(links, function (a) { map[a.getAttribute('href').slice(1)] = a; });
      var io = new IntersectionObserver(function (entries) {
        entries.forEach(function (en) {
          if (!en.isIntersecting) return;
          var a = map[en.target.id];
          if (!a) return;
          Array.prototype.forEach.call(links, function (x) { x.classList.remove('active'); });
          a.classList.add('active');
        });
      }, { rootMargin: '-40% 0px -55% 0px' });
      Array.prototype.forEach.call(Object.keys(map), function (id) {
        var el = document.getElementById(id);
        if (el) io.observe(el);
      });
    } catch (e) {}
  }

  /* =============================================================
     DRAG & DROP + SCROLL REVEAL
     ============================================================= */
  function initDnD() {
    var zone = document.getElementById('dropzone');
    var fileEl = document.getElementById('file1');
    if (!zone) return;
    zone.addEventListener('click', function (e) {
      if (e.target.closest('button, a, input, .vh-file')) return;
      if (fileEl && !fileEl.disabled) fileEl.click();
    });
    ['dragenter', 'dragover'].forEach(function (ev) {
      zone.addEventListener(ev, function (e) {
        if (e.dataTransfer && Array.prototype.indexOf.call(e.dataTransfer.types, 'Files') >= 0) {
          e.preventDefault();
          zone.classList.add('drag');
        }
      });
    });
    ['dragleave', 'drop'].forEach(function (ev) {
      zone.addEventListener(ev, function () { zone.classList.remove('drag'); });
    });
    zone.addEventListener('drop', function (e) {
      if (e.defaultPrevented) return;
      var files = e.dataTransfer && e.dataTransfer.files;
      if (!files || !files.length) return;
      e.preventDefault();
      if (!fileEl || fileEl.disabled) return;
      try {
        var dt = new DataTransfer();
        for (var i = 0; i < files.length; i++) dt.items.add(files[i]);
        fileEl.files = dt.files;
        fileEl.dispatchEvent(new Event('change', { bubbles: true }));
      } catch (err) {
        showToast('Drag & drop is not supported here — click Browse instead', true);
      }
    });
    ['dragover', 'drop'].forEach(function (ev) {
      window.addEventListener(ev, function (e) {
        if (e.dataTransfer && Array.prototype.indexOf.call(e.dataTransfer.types, 'Files') >= 0)
          e.preventDefault();
      }, false);
    });
  }

  /* =============================================================
     UPLOAD UX — instant validation + visible progress.
     The native file input is visually hidden, so a slow upload
     used to look like 'nothing happens'. This mirrors the hidden
     progress bar and shows clear states.
     ============================================================= */
  function initUploadUX() {
    var fin = document.getElementById('file1');
    if (!fin) return;
    var wrap = document.getElementById('upProgress');
    var tx = document.getElementById('upStatusTx');
    var bar = document.getElementById('upBar');
    var MAXMB = 25;
    var obs = null;
    function nativeBar() {
      var c = fin.closest('.shiny-input-container') || fin.parentElement;
      return c ? c.querySelector('.progress-bar') : null;
    }
    function sync() {
      var p = nativeBar();
      if (p && bar) bar.style.width = (p.style.width || '0%');
    }
    function stopObs() { if (obs) { obs.disconnect(); obs = null; } }
    function busy(on) {
      ['browseBtn', 'loadDemo', 'generateBtn', 'resetBtn'].forEach(function (id) {
        var el = document.getElementById(id);
        if (!el) return;
        el.disabled = !!on;
        el.style.opacity = on ? '.55' : '';
        el.style.pointerEvents = on ? 'none' : '';
      });
    }
    function showErr(msg) {
      stopObs();
      if (wrap) wrap.className = 'up-progress on err';
      if (tx) { tx.className = 'up-status err'; tx.textContent = msg; }
      if (bar) bar.style.width = '0%';
      try { fin.value = ''; } catch (e) {}
      busy(false);
      showToast(msg, true);
    }
    window.sdgUploadPhase = function (msg) {
      stopObs();
      if (wrap) wrap.className = 'up-progress on';
      if (tx) { tx.className = 'up-status'; tx.textContent = msg; }
      if (bar) bar.style.width = '100%';
    };
    window.sdgUploadDone = function () {
      stopObs();
      if (wrap) wrap.className = 'up-progress';
      if (tx) { tx.className = 'up-status'; tx.textContent = ''; }
      if (bar) bar.style.width = '0%';
      busy(false);
    };
    fin.addEventListener('change', function () {
      var f = fin.files && fin.files[0];
      if (!f) return;
      var ext = (f.name.split('.').pop() || '').toLowerCase();
      if (['csv', 'tsv', 'txt', 'xlsx', 'xls'].indexOf(ext) < 0) {
        showErr('Unsupported file type - use CSV, TSV or Excel.');
        return;
      }
      var mb = f.size / 1048576;
      if (mb > MAXMB) {
        showErr('File is too large (' + mb.toFixed(1) + ' MB). Keep it under ' + MAXMB + ' MB.');
        return;
      }
      stopObs();
      if (wrap) wrap.className = 'up-progress on';
      if (tx) {
        tx.className = 'up-status';
        tx.textContent = 'Uploading ' + f.name + ' (' +
          (mb < 0.1 ? Math.max(1, Math.round(f.size / 1024)) + ' KB' : mb.toFixed(1) + ' MB') + ')...';
      }
      if (bar) bar.style.width = '0%';
      busy(true);
      var p = nativeBar();
      if (p) {
        wrap.classList.remove('nopct');
        obs = new MutationObserver(sync);
        obs.observe(p, { attributes: true, attributeFilter: ['style'] });
        sync();
      } else if (wrap) {
        wrap.classList.add('nopct');
      }
    });
  }

  function initReveal() {
    try {
      if (!('IntersectionObserver' in window)) return;
      var io = new IntersectionObserver(function (entries) {
        entries.forEach(function (en) {
          if (en.isIntersecting) { en.target.classList.add('in'); io.unobserve(en.target); }
        });
      }, { threshold: 0.12 });
      var els = document.querySelectorAll('.sdg-feat, .sdg-stat');
      Array.prototype.forEach.call(els, function (el) {
        el.classList.add('reveal');
        io.observe(el);
      });
    } catch (e) {}
  }

  /* =============================================================
     BOOTSTRAP — enhance immediately, attach Shiny handlers when
     Shiny (async) becomes available.
     ============================================================= */
  var inited = false;
  function initBase() {
    if (inited) return;
    inited = true;
    enhanceAll();
    initDnD();
    initUploadUX();
    initReveal();
    initSpy();
    watchStats();
    var moT = null;
    try {
      var mo = new MutationObserver(function () {
        if (moT) clearTimeout(moT);
        moT = setTimeout(enhanceAll, 80);
      });
      mo.observe(document.body, { childList: true, subtree: true });
    } catch (e) {}
  }

  var tries = 0;
  function initShiny() {
    if (!window.Shiny) {
      if (++tries < 40) setTimeout(initShiny, 200);
      return;
    }

    Shiny.addCustomMessageHandler('sdg_stage', function (n) {
      document.querySelectorAll('.sdg-step').forEach(function (el) {
        var s = parseInt(el.getAttribute('data-n'), 10);
        el.classList.toggle('done', s < n);
        el.classList.toggle('active', s === n);
      });
    });

    Shiny.addCustomMessageHandler('sdg_ov_start', function () {
      var s = document.getElementById('ovStatus');
      var e = document.getElementById('ovElapsed');
      var l = document.getElementById('ovLog');
      if (s) s.textContent = 'Starting…';
      if (l) l.innerHTML = '';
      if (e) e.textContent = '0s';
      var t0 = Date.now();
      if (window.__sdgOvTimer) clearInterval(window.__sdgOvTimer);
      window.__sdgOvTimer = setInterval(function () {
        if (!e) return;
        var sec = Math.floor((Date.now() - t0) / 1000);
        var m = Math.floor(sec / 60), r = sec % 60;
        e.textContent = m > 0 ? (m + 'm ' + (r < 10 ? '0' : '') + r + 's') : (sec + 's');
      }, 1000);
    });
    Shiny.addCustomMessageHandler('sdg_ov_stop', function () {
      if (window.__sdgOvTimer) { clearInterval(window.__sdgOvTimer); window.__sdgOvTimer = null; }
    });
    Shiny.addCustomMessageHandler('sdg_status', function (m) {
      var s = document.getElementById('ovStatus');
      if (s && m && m.text) s.textContent = m.text;
    });
    Shiny.addCustomMessageHandler('sdg_log', function (m) {
      var l = document.getElementById('ovLog');
      if (!l || !m || !m.lines) return;
      l.innerHTML = '';
      m.lines.forEach(function (t) {
        var d = document.createElement('div');
        d.textContent = t;
        l.appendChild(d);
      });
    });

    Shiny.addCustomMessageHandler('sdg_copy', function (msg) {
      function legacy(text) {
        var ta = document.createElement('textarea');
        ta.value = text;
        ta.style.cssText = 'position:fixed;top:0;left:0;opacity:0;';
        document.body.appendChild(ta);
        ta.focus(); ta.select();
        var ok = false;
        try { ok = document.execCommand('copy'); } catch (e) {}
        document.body.removeChild(ta);
        showToast(ok ? 'Copied to clipboard' : 'Copy failed — clipboard is blocked here', !ok);
      }
      if (navigator.clipboard && window.isSecureContext) {
        navigator.clipboard.writeText(msg.text)
          .then(function () { showToast('Copied to clipboard', false); },
                function () { legacy(msg.text); });
      } else legacy(msg.text);
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initBase);
    initShiny();
  } else {
    initBase();
    initShiny();
  }
})();
"))
  ),

  div(class = "g-orb g-orb-1"),
  div(class = "g-orb g-orb-2"),
  div(class = "g-orb g-orb-3"),

  div(class = "sdg-nav",
      div(class = "sdg-logo",
          HTML('<svg viewBox="0 0 32 32" fill="none"><circle cx="16" cy="16" r="14" stroke="url(#lg)" stroke-width="2.5"/><path d="M10 16l4 4 8-8" stroke="url(#lg)" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"/><defs><linearGradient id="lg" x1="0" y1="0" x2="32" y2="32"><stop stop-color="#8b5cf6"/><stop offset="1" stop-color="#3b82f6"/></linearGradient></defs></svg>'),
          "SynthData",
          span(class = "ver-badge", paste0("v", APP_VERSION))),
      div(class = "sdg-links",
          tags$a(href = "#app", "Studio"),
          tags$a(href = "#how", "How it works"),
          tags$a(href = GITHUB_URL,
                 target = "_blank", rel = "noopener noreferrer", class = "sdg-gh",
                 tags$i(class = "fa-brands fa-github"), "GitHub"))
  ),

  div(class = "sdg-hero",
      div(class = "sdg-badge", span(class = "dot"),
          "Private by design — no external services, nothing shared"),
      HTML("<h1>Generate <span>synthetic data</span> that preserves real relationships</h1>"),
      tags$p("Upload a real dataset and get a statistically faithful synthetic version — distributions, correlations and category structure preserved. Choose your speed: seconds with Turbo, or model-based Balanced and High fidelity. Everything runs inside this session — private by design."),
      div(class = "sdg-btns",
          tags$button(class = "sdg-btn sdg-btn-pri", type = "button",
                      onclick = "document.getElementById('app').scrollIntoView({behavior:'smooth'})",
                      tags$i(class = "fa-solid fa-arrow-down"), "Open the studio"),
          tags$a(class = "sdg-btn sdg-btn-sec", href = "#how",
                 tags$i(class = "fa-solid fa-circle-info"), "How it works")),
      div(class = "sdg-trust",
          div(class = "sdg-trust-item", tags$i(class = "fa-solid fa-bolt"), "Seconds, not minutes"),
          div(class = "sdg-trust-item", tags$i(class = "fa-solid fa-shield-halved"), "Private by design — no third parties"),
          div(class = "sdg-trust-item", tags$i(class = "fa-solid fa-circle-check"), "No account, no limits"))
  ),
  div(class = "sdg-scroll", tags$i(class = "fa-solid fa-chevron-down"),
      tags$p("Scroll to explore")),

  div(class = "sdg-app-wrap", id = "app",
      div(class = "sdg-app",

          div(class = "sdg-app-head",
              div(
                div(class = "sdg-app-title", tags$i(class = "fa-solid fa-database"), "Synthesis studio"),
                tags$p(class = "sdg-app-sub", "Upload → configure → generate → validate → export")
              ),
              uiOutput("statusChip")
          ),

          div(class = "sdg-steps",
              div(class = "sdg-step active", `data-n` = "1", span(class = "st-ico", "1"), span("Dataset")),
              div(class = "sdg-step", `data-n` = "2", span(class = "st-ico", "2"), span("Configure")),
              div(class = "sdg-step", `data-n` = "3", span(class = "st-ico", "3"), span("Generate")),
              div(class = "sdg-step", `data-n` = "4", span(class = "st-ico", "4"), span("Validate & export"))
          ),

          div(class = "sdg-sec",
              div(class = "sdg-sec-head", span(class = "sdg-num", "1"), span(class = "sdg-sec-title", "Load your dataset")),
              tags$p(class = "sdg-sec-desc", "Drop a CSV, TSV or Excel file below — or load the built-in demo dataset. Your file is auto-cleaned on import; click the report chip afterwards to see exactly what changed."),
              div(class = "sdg-upload", id = "dropzone",
                  div(class = "up-ico", tags$i(class = "fa-solid fa-cloud-arrow-up")),
                  tags$p(class = "up-title", "Drag & drop your file here"),
                  tags$p(class = "up-sub", "— or —"),
                  tags$button(id = "browseBtn", class = "sdg-btn sdg-btn-sec", type = "button",
                              onclick = "document.getElementById('file1').click()",
                              tags$i(class = "fa-solid fa-folder-open"), "Browse files"),
                  tags$p(class = "up-hint", "CSV · TSV · Excel · up to 25 MB — processed only within this session."),
                  div(class = "up-progress", id = "upProgress",
                      div(class = "up-status", id = "upStatusTx", ""),
                      div(class = "up-track", div(class = "up-bar", id = "upBar"))),
                  div(class = "vh-file",
                      fileInput("file1", NULL, accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls"),
                                buttonLabel = "Browse", placeholder = "No file selected"))
              ),
              div(class = "sdg-alt",
                  actionButton("loadDemo",
                               tagList(tags$i(class = "fa-solid fa-flask"), "No file handy? Load the demo dataset"),
                               class = "sdg-linkbtn")
              ),
              uiOutput("dataMeta"),
              hidden(div(id = "cleanPanel", class = "clean-panel", uiOutput("cleanDetails"))),
              uiOutput("previewUI")
          ),

          hidden(div(class = "sdg-sec fade-up", id = "cfgSec",
                     div(class = "sdg-sec-head", span(class = "sdg-num", "2"), span(class = "sdg-sec-title", "Configure the synthesis")),
                     tags$p(class = "sdg-sec-desc", "Pick a speed tier, the number of rows, and the export format. If a model-based run ever exceeds its time budget, you automatically get Turbo output instead — you never leave empty-handed."),
                     div(class = "sdg-grid",
                         div(class = "sdg-field",
                             tags$label(`for` = "engine", "Engine"),
                             selectInput("engine", NULL,
                                         choices = list(
                                           "Turbo — seconds (resampling)" = "turbo",
                                           "Balanced — fast, model-based (recommended)" = "balanced",
                                           "High fidelity — slower, model-based" = "high"),
                                         selected = "balanced", selectize = FALSE),
                             tags$p(class = "sdg-hint", "Speed vs statistical fidelity")),
                         div(class = "sdg-field",
                             tags$label(`for` = "nRows", "Synthetic rows"),
                             numericInput("nRows", NULL, value = 1000, min = 1, max = 1000000, step = 100),
                             tags$p(class = "sdg-hint", "How many rows to generate")),
                         conditionalPanel(
                             condition = "input.engine != 'turbo'",
                             div(class = "sdg-field",
                                 tags$label(`for` = "method", "Method"),
                                 selectInput("method", NULL,
                                             choices = list("CART (recommended)" = "cart",
                                                            "Parametric formulas" = "parametric",
                                                            "Conditional inference trees" = "ctree"),
                                             selectize = FALSE),
                                 tags$p(class = "sdg-hint", "CART is the fastest and most robust"))),
                         div(class = "sdg-field",
                             tags$label(`for` = "seed", "Random seed"),
                             numericInput("seed", NULL, value = 42, min = 0),
                             tags$p(class = "sdg-hint", "Same seed → same output")),
                         div(class = "sdg-field",
                             tags$label(`for` = "fmt", "Export format"),
                             selectInput("fmt", NULL, choices = list("CSV" = "csv", "Excel (.xlsx)" = "xlsx", "JSON" = "json"),
                                         selectize = FALSE),
                             tags$p(class = "sdg-hint", "Format of the downloaded file"))
                     )
          )),

          hidden(div(class = "sdg-sec fade-up", id = "genSec",
                     div(class = "sdg-sec-head", span(class = "sdg-num", "3"), span(class = "sdg-sec-title", "Generate")),
                     tags$p(class = "sdg-sec-desc", "Runs in a background process with live progress and a cancel button. Time budgets: Turbo ~instant · Balanced 150s · High fidelity 420s — anything slower degrades to Turbo automatically."),
                     div(class = "gen-actions",
                         actionButton("generateBtn",
                                      tagList(tags$i(class = "fa-solid fa-wand-magic-sparkles"), "Generate synthetic data"),
                                      class = "sdg-btn sdg-btn-pri"),
                         actionButton("resetBtn",
                                      tagList(tags$i(class = "fa-solid fa-rotate-left"), "Start over"),
                                      class = "sdg-btn sdg-btn-sec")),
                     uiOutput("estUI")
          )),

          hidden(div(class = "sdg-sec fade-up", id = "resSec",
                     div(class = "sdg-sec-head", span(class = "sdg-num", "4"), span(class = "sdg-sec-title", "Results & validation")),
                     div(class = "sdg-stats",
                         div(class = "sdg-stat", div(class = "stat-ico", tags$i(class = "fa-solid fa-list-ol")),
                             div(class = "sdg-stat-val", textOutput("sRows")),
                             div(class = "sdg-stat-lbl", "Synthetic rows")),
                         div(class = "sdg-stat", div(class = "stat-ico", tags$i(class = "fa-solid fa-table-columns")),
                             div(class = "sdg-stat-val", textOutput("sCols")),
                             div(class = "sdg-stat-lbl", "Columns")),
                         div(class = "sdg-stat", div(class = "stat-ico", tags$i(class = "fa-solid fa-gauge")),
                             uiOutput("sFid"),
                             div(class = "sdg-stat-lbl", "Distribution match")),
                         div(class = "sdg-stat", div(class = "stat-ico", tags$i(class = "fa-solid fa-stopwatch")),
                             div(class = "sdg-stat-val", textOutput("sTime")),
                             div(class = "sdg-stat-lbl", "Generation time")),
                         div(class = "sdg-stat", div(class = "stat-ico", tags$i(class = "fa-solid fa-microchip")),
                             div(class = "sdg-stat-val", textOutput("sEng")),
                             div(class = "sdg-stat-lbl", "Engine"))
                     ),
                     uiOutput("engineNote"),
                     tabsetPanel(id = "resTabs",
                                 tabPanel("Preview", value = "tabPreview",
                                          icon = tags$i(class = "fa-solid fa-table"),
                                          uiOutput("resTable")),
                                 tabPanel("Compare", value = "tabCompare",
                                          icon = tags$i(class = "fa-solid fa-chart-column"),
                                          uiOutput("cmpVarUI"),
                                          plotOutput("cmpPlot", height = "340px"),
                                          tags$p(class = "sdg-muted", "Original (blue) vs synthetic (violet) — the closer the shapes, the better the synthesis.")),
                                 tabPanel("Quality", value = "tabQuality",
                                          icon = tags$i(class = "fa-solid fa-gauge"),
                                          uiOutput("qualityUI"))
                     ),
                     div(class = "res-actions",
                         downloadButton("downloadBtn",
                                        tagList(tags$i(class = "fa-solid fa-download"), "Download dataset"),
                                        class = "sdg-btn sdg-btn-pri", icon = NULL),
                         actionButton("copyBtn",
                                      tagList(tags$i(class = "fa-solid fa-copy"), "Copy first 100 rows"),
                                      class = "sdg-btn sdg-btn-sec"))
          ))
      )
  ),

  div(class = "sdg-features", id = "how",
      tags$h2("How it works"),
      tags$p(class = "feat-sub", "Three speed tiers, one workflow: upload, generate, validate, download."),
      div(class = "sdg-feat-grid",
          div(class = "sdg-feat",
              div(class = "feat-ico", tags$i(class = "fa-solid fa-bolt")),
              tags$h3("Turbo — seconds"),
              tags$p("Whole-row resampling with small perturbations. Marginals and correlations are preserved almost exactly; rows stay close to real records — great for demos, tests and quick mock datasets.")),
          div(class = "sdg-feat",
              div(class = "feat-ico", tags$i(class = "fa-solid fa-gauge-high")),
              tags$h3("Balanced — fast & model-based"),
              tags$p("Column models are fitted on a sample of your rows, each using its strongest predictors, then any number of rows is generated. Usually seconds to under a minute, even for large files.")),
          div(class = "sdg-feat",
              div(class = "feat-ico", tags$i(class = "fa-solid fa-gem")),
              tags$h3("High fidelity"),
              tags$p("Bigger fit sample and all predictors, for the best statistical fidelity when you can afford a few minutes. Quality is always validated against your full dataset."))
      )
  ),

  div(id = "overlay", class = "sdg-overlay",
      div(class = "sdg-overlay-card",
          div(class = "spinner"),
          tags$h3("Synthesizing your data"),
          div(class = "ov-meta",
              tags$p(id = "ovStatus", "Starting…"),
              tags$span(id = "ovElapsed", "0s")),
          div(class = "ov-bar"),
          div(id = "ovLog", class = "ov-log"),
          actionButton("cancelSyn",
                       tagList(tags$i(class = "fa-solid fa-ban"), "Cancel"),
                       class = "sdg-btn sdg-btn-sec ov-cancel"),
          tags$p(class = "ov-sub", "Runs in a background process — the app stays responsive.")
      )
  ),

  div(class = "sdg-footer",
      HTML(paste0("SynthData Studio v", APP_VERSION,
                  " — everything runs inside your R session; private by design, nothing shared.")),
      tags$br(),
      HTML("Built with R · Shiny · synthpop"), tags$br(),
      tags$a(href = GITHUB_URL, target = "_blank", rel = "noopener noreferrer",
             "View source on GitHub"), " · MIT License")
)

# =====================================================================
# SERVER
# =====================================================================
server <- function(input, output, session) {

  rv <- reactiveValues(
    data = NULL, data_name = NULL, data_size = NA_real_, clean_notes = NULL,
    syn = NULL, time = NA_real_, fid = NULL, engine = NULL, engine_label = NULL,
    engine_msg = NULL, proc = NULL, t0 = NULL, req_n = NULL, req_seed = NULL,
    soft_budget = 0, soft_hit = FALSE, cancelled = FALSE, timeout_hit = FALSE,
    log = character(0), poll_warned = FALSE, req_cons = NULL, req_engine = NULL
  )

  notify <- function(msg, type = "message", duration = 5)
    showNotification(msg, type = type, duration = duration)

  send_status <- function(txt)
    try(session$sendCustomMessage("sdg_status", list(text = sanitize_str(txt)[1])), silent = TRUE)
  send_log <- function(lines)
    try(session$sendCustomMessage("sdg_log", list(lines = sanitize_str(lines))), silent = TRUE)

  set_stage <- function(n) session$sendCustomMessage("sdg_stage", n)

  clear_file_input <- function()
    shinyjs::runjs("(function(){var f=document.getElementById('file1');if(f)f.value='';})()")

  set_busy <- function(busy) {
    if (busy) {
      shinyjs::disable("generateBtn"); shinyjs::disable("resetBtn")
      shinyjs::disable("removeFile");  shinyjs::disable("loadDemo")
      shinyjs::disable("browseBtn");   shinyjs::disable("file1")
      shinyjs::disable("engine")
      shinyjs::runjs("document.getElementById('overlay').classList.add('open')")
      session$sendCustomMessage("sdg_ov_start", TRUE)
    } else {
      session$sendCustomMessage("sdg_ov_stop", TRUE)
      shinyjs::runjs("document.getElementById('overlay').classList.remove('open')")
      shinyjs::enable("generateBtn"); shinyjs::enable("resetBtn")
      shinyjs::enable("removeFile");  shinyjs::enable("loadDemo")
      shinyjs::enable("browseBtn");   shinyjs::enable("file1")
      shinyjs::enable("engine")
    }
  }

  finish_generation <- function(res, label) {
    rv$time <- as.numeric(difftime(Sys.time(), rv$t0, units = "secs"))
    if (is.null(res) || is.null(res$data) || nrow(res$data) == 0) {
      notify(res$msg %||% "Synthesis failed for an unknown reason.", "error", 10)
      set_stage(2)
      return(invisible())
    }
    if (identical(res$engine, "synthpop-missing")) {
      notify(paste0("Model-based synthesis is unavailable: the 'synthpop' package is missing ",
                    "from this R environment. Install it with ",
                    "install.packages('synthpop', repos = 'https://cloud.r-project.org'), ",
                    "restart the app, then try again. (The Turbo engine works without it.)"),
             "error", 15)
      set_stage(2)
      return(invisible())
    }
    if (identical(res$engine, "fallback"))
      notify("The model-based engine could not fit this dataset — a simple sampler was used instead (limited fidelity).", "warning", 10)
    else if (!is.null(res$msg))
      notify(res$msg, "warning", 8)
    if (!is.null(res$note)) notify(paste0("Notes: ", res$note), "message", 12)

    rv$syn <- res$data
    rv$engine <- res$engine
    rv$engine_label <- label
    rv$engine_msg <- res$msg
    rv$fid <- if (!is.null(res$prepped)) compute_fidelity(res$prepped, res$syn_num %||% res$data) else NULL

    set_stage(4)
    shinyjs::show("resSec", anim = TRUE, animType = "fade")
    updateTabsetPanel(session, "resTabs", selected = "tabPreview")
    shinyjs::runjs("document.getElementById('resSec').scrollIntoView({behavior:'smooth', block:'nearest'})")
    notify(paste0("Synthetic dataset generated in ", fmt_time(rv$time), " · engine: ", label, "."), "message", 4)
  }

  ingest <- function(df, name, size_mb) {
    df <- as.data.frame(df)
    names(df) <- sub("^\ufeff", "", names(df))   # strip BOM before any name mangling
    names(df) <- make.names(names(df))
    empty_nm <- is.na(names(df)) | names(df) == ""
    if (any(empty_nm)) names(df)[empty_nm] <- paste0("V", which(empty_nm))
    names(df) <- make.unique(names(df))

    cl <- clean_data(df)
    df <- cl$df
    rv$clean_notes <- if (length(cl$notes)) cl$notes else NULL

    if (nrow(df) < 3 || ncol(df) < 2) {
      notify("After auto-cleaning, the dataset needs at least 3 rows and 2 usable columns.", "error", 7)
      return(invisible(FALSE))
    }
    if (length(cl$notes)) {
      shown <- utils::head(cl$notes, 4)
      more  <- if (length(cl$notes) > 4) paste0(" … and ", length(cl$notes) - 4, " more") else ""
      notify(paste0("Auto-cleaned on import — click the report chip for details: ",
                    paste(shown, collapse = "; "), more), "message", 9)
    }

    rv$data <- df; rv$data_name <- name; rv$data_size <- size_mb
    rv$syn <- NULL; rv$time <- NA_real_; rv$fid <- NULL
    rv$engine <- NULL; rv$engine_label <- NULL; rv$engine_msg <- NULL
    shinyjs::hide("resSec", anim = FALSE)
    shinyjs::hide("cleanPanel", anim = FALSE)
    shinyjs::show("cfgSec", anim = TRUE, animType = "fade")
    shinyjs::show("genSec", anim = TRUE, animType = "fade")
    set_stage(2)
    notify(paste0("Loaded ", name, " — ", format(nrow(df), big.mark = ","), " rows × ", ncol(df), " columns."), "message", 4)
    shinyjs::runjs("document.getElementById('app').scrollIntoView({behavior:'smooth', block:'start'})")
    invisible(TRUE)
  }

  observeEvent(input$file1, {
    fin <- input$file1
    if (is.null(fin) || identical(fin$name, "")) return()

    shinyjs::runjs("window.sdgUploadPhase && window.sdgUploadPhase('Upload received - processing...')")

    size_mb <- fin$size / 1024^2
    if (size_mb > MAX_FILE_MB) {
      notify(paste0("File is too large (", round(size_mb, 1), " MB). Please keep it under ",
                    MAX_FILE_MB, " MB."), "error", 7)
      shinyjs::reset("file1"); clear_file_input()
      shinyjs::runjs("window.sdgUploadDone && window.sdgUploadDone();")
      return()
    }

    ext <- tolower(tools::file_ext(fin$name))
    err <- NULL
    df <- tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        if (!requireNamespace("readxl", quietly = TRUE))
          stop("install.packages('readxl') is needed for Excel files.")
        as.data.frame(readxl::read_excel(fin$datapath))
      } else if (ext %in% c("csv", "tsv", "txt")) {
        d <- read_table_safe(fin$datapath, ext)
        if (is.null(d)) stop("the file could not be parsed as a table.")
        d
      } else {
        stop("unsupported file type. Use CSV, TSV or Excel.")
      }
    }, error = function(e) { err <<- conditionMessage(e); NULL })

    if (is.null(df)) {
      notify(paste0("Could not read the file: ", err), "error", 8)
      shinyjs::reset("file1"); clear_file_input()
      shinyjs::runjs("window.sdgUploadDone && window.sdgUploadDone();")
      return()
    }
    if (nrow(df) < 3 || ncol(df) < 2) {
      notify("Synthesis needs at least 3 rows and 2 columns.", "error", 6)
      shinyjs::reset("file1"); clear_file_input()
      shinyjs::runjs("window.sdgUploadDone && window.sdgUploadDone();")
      return()
    }

    ingest(df, fin$name, size_mb)
    shinyjs::runjs("window.sdgUploadDone && window.sdgUploadDone();")
  })

  observeEvent(input$loadDemo, {
    demo <- make_demo_data(240)
    ingest(demo, "demo_workforce.csv", as.numeric(object.size(demo)) / 1024^2)
  })

  observeEvent(input$removeFile, {
    if (!is.null(rv$proc)) return()
    rv$data <- NULL; rv$data_name <- NULL; rv$data_size <- NA_real_
    rv$clean_notes <- NULL
    rv$syn <- NULL; rv$time <- NA_real_; rv$fid <- NULL
    rv$engine <- NULL; rv$engine_label <- NULL; rv$engine_msg <- NULL
    shinyjs::reset("file1"); clear_file_input()
    shinyjs::hide("cfgSec"); shinyjs::hide("genSec"); shinyjs::hide("resSec")
    shinyjs::hide("cleanPanel")
    set_stage(1)
    notify("Dataset removed.", "message", 3)
  })

  observeEvent(input$cleanInfoBtn, {
    shinyjs::toggle("cleanPanel", anim = TRUE, animType = "fade")
  })

  output$cleanDetails <- renderUI({
    if (is.null(rv$clean_notes) || !length(rv$clean_notes)) return(NULL)
    tagList(
      div(class = "clean-head", tags$i(class = "fa-solid fa-broom"), "Auto-clean report"),
      lapply(rv$clean_notes, function(n)
        div(class = "clean-row", tags$i(class = "fa-solid fa-check"), span(n)))
    )
  })

  output$statusChip <- renderUI({
    if (!is.null(rv$syn))
      tags$span(class = "chip chip-done", tags$i(class = "fa-solid fa-circle-check"),
                paste0("Ready · ", rv$engine_label %||% "—"))
    else if (!is.null(rv$proc))
      tags$span(class = "chip chip-done", tags$i(class = "fa-solid fa-spinner fa-spin"), "Generating…")
    else if (!is.null(rv$data))
      tags$span(class = "chip chip-ok", tags$i(class = "fa-solid fa-database"), "Dataset loaded")
    else
      tags$span(class = "chip", tags$i(class = "fa-solid fa-circle-info"), "Waiting for data")
  })

  output$dataMeta <- renderUI({
    if (is.null(rv$data)) return(NULL)
    df <- rv$data
    div(class = "sdg-meta",
        div(class = "m-chip", tags$i(class = "fa-solid fa-file-lines"),
            span(class = "m-name", rv$data_name %||% "dataset")),
        div(class = "m-chip", tags$i(class = "fa-solid fa-list-ol"),
            span(class = "m-val", format(nrow(df), big.mark = ",")), "rows"),
        div(class = "m-chip", tags$i(class = "fa-solid fa-table-columns"),
            span(class = "m-val", ncol(df)), "columns"),
        div(class = "m-chip", tags$i(class = "fa-solid fa-circle-exclamation"),
            span(class = "m-val", format(sum(is.na(df)), big.mark = ",")), "missing cells"),
        if (!is.na(rv$data_size))
          div(class = "m-chip", tags$i(class = "fa-solid fa-hard-drive"),
              span(class = "m-val", fmt_bytes(rv$data_size * 1024^2))),
        if (!is.null(rv$clean_notes))
          actionButton("cleanInfoBtn",
                       tagList(tags$i(class = "fa-solid fa-broom"), "auto-cleaned · view report"),
                       class = "m-chip-clean"),
        actionButton("removeFile",
                     tagList(tags$i(class = "fa-solid fa-xmark"), "Remove dataset"),
                     class = "m-remove")
    )
  })

  output$previewUI <- renderUI({
    if (is.null(rv$data))
      return(tags$p(class = "sdg-muted", "A live preview of your data will appear here after upload."))
    make_preview_table(rv$data, 6)
  })

  output$estUI <- renderUI({
    req(rv$data)
    rows <- nrow(rv$data); cols <- ncol(rv$data)
    eng <- input$engine %||% "balanced"
    if (is.null(ENGINE_PRESETS[[eng]])) eng <- "balanced"
    n_out <- suppressWarnings(as.integer(input$nRows %||% 0))
    if (length(n_out) != 1 || is.na(n_out) || n_out < 0) n_out <- 0
    est <- estimate_label(rows, cols, eng, n_out)
    div(class = "est-bar",
        span(class = "est-item", tags$i(class = "fa-solid fa-database"),
             tags$b(format(rows, big.mark = ",")), "rows ×", tags$b(cols), "columns"),
        span(class = "est-sep", tags$i(class = "fa-solid fa-arrow-right")),
        span(class = "est-item", tags$i(class = "fa-solid fa-wand-magic-sparkles"),
             tags$b(format(n_out, big.mark = ",")), "synthetic rows"),
        span(class = "est-sep", "·"),
        span(class = "est-item", tags$i(class = "fa-solid fa-microchip"), ENGINE_PRESETS[[eng]]$label),
        span(class = "est-item est-time", tags$i(class = "fa-solid fa-stopwatch"),
             tags$b(est)))
  })

  observeEvent(input$generateBtn, {
    if (!is.null(rv$proc)) return()
    if (is.null(rv$data)) { notify("Upload a dataset first (or load the demo).", "error"); return() }

    n <- input$nRows
    if (is.null(n) || length(n) != 1 || is.na(n) || n < 1 || n > MAX_ROWS || n != floor(n)) {
      notify(paste0("Row count must be a whole number between 1 and ",
                    format(MAX_ROWS, big.mark = ","), "."), "error")
      return()
    }

    # validate the seed up front: set.seed() throws on negative / NA /
    # out-of-range values and would otherwise surface as a confusing
    # engine failure deep inside the worker
    seed <- input$seed
    if (is.null(seed) || length(seed) != 1 || is.na(seed)) {
      seed <- NULL
    } else if (!is.finite(seed) || seed < 0 || seed > .Machine$integer.max || seed != floor(seed)) {
      notify(paste0("Random seed must be a whole number between 0 and ",
                    format(.Machine$integer.max, big.mark = ","), "."), "error", 6)
      return()
    } else {
      seed <- as.integer(seed)
    }

    eng <- input$engine %||% "balanced"
    preset <- ENGINE_PRESETS[[eng]]
    df <- rv$data; method <- input$method %||% "cart"

    if (eng != "turbo" && !requireNamespace("synthpop", quietly = TRUE)) {
      notify(paste0("The 'synthpop' package is not installed in this R session, so the '",
                    preset$label, "' engine cannot run. Install it with ",
                    "install.packages('synthpop', repos = 'https://cloud.r-project.org'), ",
                    "restart the app, and try again. (The Turbo engine works without it.)"),
             "error", 15)
      return()
    }

    cbuild <- build_constraints(df)
    cons   <- cbuild$cons
    rv$req_cons <- cons
    if (length(cbuild$notes))
      notify(paste0("Range constraints: ", paste(cbuild$notes, collapse = "; ")), "message", 8)

    set_stage(3)
    set_busy(TRUE)
    rv$t0 <- Sys.time(); rv$req_n <- as.integer(n); rv$req_seed <- seed
    rv$req_engine <- eng
    rv$cancelled <- FALSE; rv$timeout_hit <- FALSE; rv$soft_hit <- FALSE
    rv$soft_budget <- preset$budget; rv$log <- character(0); rv$poll_warned <- FALSE

    if (eng == "turbo") {
      shinyjs::delay(300, {
        res <- tryCatch(turbo_synthesize(df, as.integer(n), seed, cons),
                        error = function(e)
                          list(data = NULL, engine = NULL,
                               msg = paste("Synthesis failed:", conditionMessage(e)),
                               note = NULL, prepped = NULL, syn_num = NULL))
        set_busy(FALSE)
        finish_generation(res, "Turbo")
      })
      return()
    }

    if (requireNamespace("callr", quietly = TRUE)) {
      rv$proc <- tryCatch(
        callr::r_bg(
          synthesis_worker,
          args = list(df = df, n_rows = as.integer(n), method = method, seed = seed,
                      cons = cons, max_levels = MAX_LEVELS,
                      fit_sample_max = preset$fit, predictor_limit = preset$pred),
          supervise = TRUE,
          libpath = .libPaths()
        ),
        error = function(e) {
          notify(paste("Could not start the background process:", conditionMessage(e)), "error", 8)
          NULL
        })
      if (is.null(rv$proc)) { set_busy(FALSE); set_stage(2); return() }
    } else {
      notify("Install 'callr' for background processing and cancel support — running in-session instead (the app will freeze while it works).", "warning", 8)
      shinyjs::delay(300, {
        res <- tryCatch(synthesis_worker(df, as.integer(n), method, seed, cons,
                                         MAX_LEVELS, preset$fit, preset$pred),
                        error = function(e)
                          list(data = NULL, engine = NULL,
                               msg = paste("Synthesis failed:", conditionMessage(e)),
                               note = NULL, prepped = NULL, syn_num = NULL))
        set_busy(FALSE)
        finish_generation(res, preset$label)
      })
    }
  })

  observe({
    proc <- rv$proc
    if (is.null(proc)) return()
    invalidateLater(600, session)

    tryCatch({

      new_lines <- character(0)
      try(new_lines <- proc$read_output_lines(), silent = TRUE)
      if (length(new_lines)) {
        is_msg <- startsWith(new_lines, "MSG|")
        msgs <- new_lines[is_msg]
        raw  <- new_lines[!is_msg]
        raw  <- raw[trimws(raw) != ""]
        if (length(msgs)) {
          last <- trimws(gsub("^MSG\\|", "", utils::tail(msgs, 1)))
          send_status(last)
        }
        if (length(raw)) {
          rv$log <- utils::tail(c(rv$log, raw), 80)
          send_log(utils::tail(rv$log, 5))
        }
      }

      if (!proc$is_alive()) {
        rv$proc <- NULL
        set_busy(FALSE)

        if (isTRUE(rv$cancelled)) {
          notify("Generation cancelled.", "warning", 5); set_stage(2); return()
        }
        if (isTRUE(rv$timeout_hit)) {
          notify(sprintf(paste0("Generation was stopped automatically after %d minutes. ",
                                "Try the Turbo or Balanced engine."),
                         MAX_SYN_SECONDS %/% 60), "error", 12)
          set_stage(2); return()
        }
        if (isTRUE(rv$soft_hit)) {
          notify(paste0("The model-based engine exceeded its time budget on this server — ",
                        "delivering fast-mode (Turbo) output instead."), "warning", 12)
          res <- tryCatch(turbo_synthesize(rv$data, rv$req_n, rv$req_seed, rv$req_cons),
                          error = function(e) NULL)
          if (is.null(res)) { set_stage(2); return() }
          res$engine <- "turbo"
          res$note <- paste0("The model-based run exceeded its time budget and was replaced ",
                             "by the fast Turbo engine (whole-row resampling + perturbation). ",
                             "Marginals and correlations are preserved closely, but rows remain ",
                             "near-copies of real records. Retrying or using a smaller file may ",
                             "let the model-based engine finish.",
                             if (!is.null(res$note)) paste0(" ", res$note) else "")
          finish_generation(res, "Turbo (auto-fallback)")
          return()
        }

        errmsg <- NULL
        res <- tryCatch(proc$get_result(),
                        error = function(e) { errmsg <<- conditionMessage(e); NULL })
        if (is.null(res)) {
          notify(paste("Background process failed:", errmsg), "error", 10)
          set_stage(2); return()
        }
        label <- if (identical(rv$req_engine, "high")) "High fidelity" else "Balanced"
        finish_generation(res, label)
        return()
      }

      elapsed <- as.numeric(difftime(Sys.time(), rv$t0, units = "secs"))
      if (elapsed > MAX_SYN_SECONDS) {
        rv$timeout_hit <- TRUE
        try(proc$kill(), silent = TRUE)
      } else if (rv$soft_budget > 0 && elapsed > rv$soft_budget) {
        rv$soft_hit <- TRUE
        try(proc$kill(), silent = TRUE)
      }

    }, error = function(e) {
      if (!isTRUE(rv$poll_warned)) {
        rv$poll_warned <- TRUE
        try(showNotification(paste("Progress stream hiccup (generation continues):",
                                   conditionMessage(e)), type = "warning", duration = 6),
            silent = TRUE)
      }
    })
  })

  observeEvent(input$cancelSyn, {
    proc <- rv$proc
    if (!is.null(proc)) {
      # only mark as cancelled if we actually killed a running process —
      # a process that already finished should still deliver its result
      killed <- tryCatch(isTRUE(proc$kill()), error = function(e) FALSE)
      if (killed) {
        rv$cancelled <- TRUE
        notify("Cancelling…", "message", 3)
      }
    }
  })

  session$onSessionEnded(function() {
    p <- isolate(rv$proc) # isolate is needed here because onSessionEnded is outside reactive context
    if (!is.null(p)) try(p$kill(), silent = TRUE)
  })

  output$sRows <- renderText({ if (is.null(rv$syn)) "—" else format(nrow(rv$syn), big.mark = ",") })
  output$sCols <- renderText({ if (is.null(rv$syn)) "—" else as.character(ncol(rv$syn)) })
  output$sFid <- renderUI({
    if (is.null(rv$fid) || is.na(rv$fid$overall))
      return(div(class = "sdg-stat-val", "—"))
    pct <- rv$fid$overall
    cls <- if (pct >= 90) "v-good" else if (pct >= 75) "v-mid" else "v-low"
    div(class = paste("sdg-stat-val", cls), paste0(pct, "%"))
  })
  output$sTime <- renderText({ fmt_time(rv$time) })
  output$sEng  <- renderText({
    if (is.null(rv$engine_label)) return("—")
    if (identical(rv$engine, "fallback")) paste0(rv$engine_label, " → fallback sampler")
    else rv$engine_label
  })

  output$engineNote <- renderUI({
    lbl <- rv$engine_label
    if (is.null(lbl)) return(NULL)

    if (identical(lbl, "Turbo (auto-fallback)")) {
      tags$div(class = "engine-warn",
               tags$i(class = "fa-solid fa-triangle-exclamation"),
               tags$div(
                 tags$strong("Time budget exceeded — fast-mode output delivered."),
                 tags$p(class = "engine-reason",
                        "The model-based engine was cancelled on this server and the Turbo engine produced your data instead: distributions and correlations are preserved closely, but rows are near-copies of real records. Try again, use a smaller file, or pick a different engine.")))
    } else if (identical(lbl, "Turbo")) {
      tags$div(class = "engine-info",
               tags$i(class = "fa-solid fa-bolt"),
               tags$div(
                 tags$strong("Turbo engine output."),
                 tags$p(class = "engine-reason",
                        "Whole-row resampling with small perturbations: marginals and correlations are preserved closely, but rows remain near-copies of real records. Use Balanced or High fidelity when records must not resemble real ones.")))
    } else if (identical(rv$engine, "fallback")) {
      tags$div(class = "engine-warn",
               tags$i(class = "fa-solid fa-triangle-exclamation"),
               tags$div(
                 tags$strong("Fallback sampler used — statistical fidelity is limited."),
                 if (!is.null(rv$engine_msg))
                   tags$p(class = "engine-reason", "Reason: ", rv$engine_msg),
                 tags$p(class = "engine-reason",
                        "For full model-based synthesis the 'synthpop' package must be installed on the server ",
                        "(install.packages('synthpop')). If it is already installed, try the CART method, ",
                        "a smaller row count, or the Turbo engine.")))
    } else if (identical(rv$engine, "synthpop")) {
      tags$div(class = "engine-info",
               tags$i(class = "fa-solid fa-circle-check"),
               tags$div(
                 tags$strong("Model-based engine output."),
                 tags$p(class = "engine-reason",
                        "Every row is fully synthetic — no real record is copied. Column models were fitted ",
                        "from your data with the strongest predictors; see the Quality tab for per-column fidelity.")))
    } else NULL
  })

  output$resTable <- renderUI({
    req(rv$syn)
    make_preview_table(rv$syn, 6)
  })

  output$cmpVarUI <- renderUI({
    req(rv$syn, rv$data)
    cols <- intersect(names(rv$data), names(rv$syn))
    if (!length(cols)) return(NULL)
    nums <- cols[vapply(rv$data[cols], function(x) is.numeric(x) || inherits(x, "Date"), logical(1))]
    sel  <- if (length(nums)) nums[1] else cols[1]
    div(class = "sdg-field", style = "max-width:340px;margin-bottom:14px",
        tags$label(`for` = "cmpVar", "Variable to compare"),
        selectInput("cmpVar", NULL, choices = cols, selected = sel, selectize = FALSE))
  })

  output$cmpPlot <- renderPlot({
    req(rv$data, rv$syn, input$cmpVar)
    v <- input$cmpVar
    r <- num_like(rv$data[[v]])
    s <- num_like(rv$syn[[v]])

    if (is.numeric(r) && is.numeric(s)) {
      r <- r[is.finite(r)]; s <- s[is.finite(s)]
      if (length(r) < 2 || length(s) < 2) return(NULL)
      rng <- range(c(r, s))
      if (diff(rng) == 0) rng <- rng + c(-0.5, 0.5)
      br <- seq(rng[1], rng[2], length.out = 20)
      hr <- hist(r, breaks = br, plot = FALSE)
      hs <- hist(s, breaks = br, plot = FALSE)

      # format the x axis as dates when the source column is a date
      src <- rv$data[[v]]
      is_date  <- inherits(src, "Date")
      is_posix <- inherits(src, "POSIXct")
      hide_x   <- is_date || is_posix

      op <- par(bg = "transparent", fg = "#343a48",
                col.axis = "#8a8f9a", col.lab = "#c9cdd6",
                cex.axis = .85, cex.lab = .95, mar = c(4.6, 4.4, 1.4, 1.4), tcl = -.3)
      on.exit(par(op), add = TRUE)

      plot(hr, col = adjustcolor("#3b82f6", .35), border = adjustcolor("#60a5fa", .90),
           main = "", xlab = v, ylab = "Frequency", xaxt = if (hide_x) "n" else "s",
           ylim = c(0, max(c(hr$counts, hs$counts)) * 1.08))
      hist(s, breaks = br, add = TRUE,
           col = adjustcolor("#8b5cf6", .50), border = adjustcolor("#a78bfa", .95))
      grid(nx = NA, ny = NULL, col = adjustcolor("#ffffff", .06), lty = 1)
      box(col = "#343a48")
      if (hide_x) {
        ax <- graphics::axTicks(1)
        ax <- ax[ax >= rng[1] & ax <= rng[2]]
        lbl <- if (is_date)
          format(as.Date(ax, origin = "1970-01-01"))
        else
          format(as.POSIXct(ax, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d")
        axis(1, at = ax, labels = lbl, col.axis = "#8a8f9a", cex.axis = .8)
      }
      legend("topright", c("Original", "Synthetic"), bty = "n", cex = .9, text.col = "#d6d9e0",
             fill = c(adjustcolor("#3b82f6", .55), adjustcolor("#8b5cf6", .65)),
             border = c("#60a5fa", "#a78bfa"))
    } else {
      rl <- as.character(rv$data[[v]]); sl <- as.character(rv$syn[[v]])
      lv <- unique(c(rl[!is.na(rl)], sl[!is.na(sl)]))
      if (!length(lv)) return(NULL)
      if (length(lv) > 14) {
        top <- names(sort(table(rl[!is.na(rl)]), decreasing = TRUE))[1:12]
        rl  <- ifelse(is.na(rl), NA, ifelse(rl %in% top, rl, "(other)"))
        sl  <- ifelse(is.na(sl), NA, ifelse(sl %in% top, sl, "(other)"))
        lv  <- c(top, "(other)")
      }
      pr <- prop.table(table(factor(rl, levels = lv)))
      ps <- prop.table(table(factor(sl, levels = lv)))

      op <- par(bg = "transparent", fg = "#343a48",
                col.axis = "#8a8f9a", col.lab = "#c9cdd6",
                cex.axis = .85, cex.lab = .95, mar = c(7.5, 4.4, 1.4, 1.4), tcl = -.3)
      on.exit(par(op), add = TRUE)

      barplot(rbind(Original = pr, Synthetic = ps), beside = TRUE,
              col = c(adjustcolor("#3b82f6", .55), adjustcolor("#8b5cf6", .65)),
              border = c("#60a5fa", "#a78bfa"),
              ylab = "Proportion", cex.names = .75, las = 2,
              ylim = c(0, max(c(pr, ps), 0.01) * 1.15))
      grid(nx = NA, ny = NULL, col = adjustcolor("#ffffff", .06), lty = 1)
      box(col = "#343a48")
      legend("topright", c("Original", "Synthetic"), bty = "n", cex = .9, text.col = "#d6d9e0",
             fill = c(adjustcolor("#3b82f6", .55), adjustcolor("#8b5cf6", .65)),
             border = c("#60a5fa", "#a78bfa"))
    }
  }, bg = "transparent")

  output$qualityUI <- renderUI({
    req(rv$fid)
    f <- rv$fid
    if (is.null(f$per) || nrow(f$per) == 0)
      return(tags$p(class = "sdg-muted", "Not enough comparable columns to score."))

    rows <- lapply(seq_len(nrow(f$per)), function(i) {
      nm  <- f$per$column[i]
      ty  <- f$per$type[i]
      sim <- f$per$sim[i]
      if (is.na(sim)) {
        div(class = "q-row",
            span(class = "q-name", nm, span(class = paste0("tbadge tbadge-", ty), type_labs[[ty]])),
            span(class = "sdg-muted", "not comparable"),
            span(class = "q-val", "—"))
      } else {
        pct <- round(100 * sim)
        cls <- if (pct >= 90) "q-hi" else if (pct >= 75) "q-mid" else "q-lo"
        div(class = "q-row",
            span(class = "q-name", nm, span(class = paste0("tbadge tbadge-", ty), type_labs[[ty]])),
            div(class = "q-track",
                div(class = paste0("q-fill ", cls), style = sprintf("width:%d%%", pct))),
            span(class = "q-val", paste0(pct, "%")))
      }
    })

    tagList(
      if (!is.na(f$overall))
        tags$p(class = "q-corr", tags$i(class = "fa-solid fa-circle-check"),
               "Overall distribution match: ", strong(paste0(f$overall, "%"))) else NULL,
      if (!is.na(f$corr))
        tags$p(class = "q-corr", tags$i(class = "fa-solid fa-chart-line"),
               "Correlation-matrix fidelity: ", strong(paste0(f$corr, "%")))
      else NULL,
      tags$p(class = "q-note",
             "Distribution similarity = 1 − ½ · total-variation distance between the original and synthetic distributions (100% = identical shape), computed against your full dataset."),
      div(class = "q-list", rows)
    )
  })

  output$downloadBtn <- downloadHandler(
    filename = function() {
      ext  <- switch(input$fmt, xlsx = "xlsx", json = "json", "csv")
      base <- gsub("[^A-Za-z0-9]+", "_", tools::file_path_sans_ext(rv$data_name %||% "data"))
      sprintf("synthetic_%s_%s.%s", base, format(Sys.time(), "%Y%m%d_%H%M%S"), ext)
    },
    content = function(file) {
      req(rv$syn)
      if (input$fmt == "xlsx") {
        if (requireNamespace("writexl", quietly = TRUE)) { writexl::write_xlsx(rv$syn, file); return() }
        notify("Install 'writexl' for Excel export — saving as CSV instead.", "warning", 6)
        utils::write.csv(rv$syn, file, row.names = FALSE, na = "")
      } else if (input$fmt == "json") {
        if (requireNamespace("jsonlite", quietly = TRUE)) {
          jsonlite::write_json(rv$syn, file, dataframe = "rows", na = "null"); return() }
        notify("Install 'jsonlite' for JSON export — saving as CSV instead.", "warning", 6)
        utils::write.csv(rv$syn, file, row.names = FALSE, na = "")
      } else {
        utils::write.csv(rv$syn, file, row.names = FALSE, na = "")
      }
    }
  )

  observeEvent(input$copyBtn, {
    req(rv$syn)
    txt <- paste(utils::capture.output(
      utils::write.csv(utils::head(rv$syn, 100), "", row.names = FALSE)), collapse = "\n")
    session$sendCustomMessage("sdg_copy", list(text = txt))
  })

  observeEvent(input$resetBtn, {
    if (!is.null(rv$proc)) return()
    rv$data <- NULL; rv$data_name <- NULL; rv$data_size <- NA_real_
    rv$clean_notes <- NULL
    rv$syn <- NULL; rv$time <- NA_real_; rv$fid <- NULL
    rv$engine <- NULL; rv$engine_label <- NULL; rv$engine_msg <- NULL
    set_busy(FALSE)
    shinyjs::reset("file1"); clear_file_input()
    shinyjs::reset("nRows"); shinyjs::reset("seed")
    shinyjs::hide("cfgSec"); shinyjs::hide("genSec"); shinyjs::hide("resSec")
    shinyjs::hide("cleanPanel")
    set_stage(1)
    shinyjs::runjs("document.getElementById('app').scrollIntoView({behavior:'smooth'})")
  })
}

shinyApp(ui, server)
