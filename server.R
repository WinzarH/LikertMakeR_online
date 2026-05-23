###
##### LikertMakeR_online
###
##### Hume Winzar
##### February 2026
###


###
##### Server logic
###
server <- function(input, output, session) {
  is_valid_matrix <- function(x) is.matrix(x) && nrow(x) >= 2 && ncol(x) == nrow(x)

  safe_name <- function(x) {
    x <- trimws(x)
    x <- ifelse(nzchar(x), x, "Scale")
    make.unique(x, sep = " ")
  }

  disable_ui <- function(x) {
    tags$div(
      style = "pointer-events:none; opacity:0.6;",
      x
    )
  }


  # ---- matrix helpers ----

  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x


  read_csv_no_blank_lines <- function(path) {
    x <- readLines(path, warn = FALSE)
    x <- x[nzchar(trimws(x))] # drop empty/whitespace-only lines
    tmp <- tempfile(fileext = ".csv")
    writeLines(x, tmp, useBytes = TRUE)
    tmp
  }

  is_default_vnames <- function(nm) length(nm) > 0 && all(grepl("^V\\d+$", nm))
  clean_label <- function(z) gsub('^\"+|\"+$', "", trimws(as.character(z)))


  parse_corr_csv <- function(path, tol = 1e-8) {
    path2 <- read_csv_no_blank_lines(path)

    df <- read.csv(path2,
      header = TRUE,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    names(df) <- trimws(names(df))

    # Drop fully blank rows (Excel trailing blank line)
    is_blank_row <- function(r) all(is.na(r) | trimws(as.character(r)) == "")
    df <- df[!apply(df, 1, is_blank_row), , drop = FALSE]

    # Drop fully blank columns (rare, but safe)
    is_blank_col <- function(x) all(is.na(x) | trimws(as.character(x)) == "")
    df <- df[, !vapply(df, is_blank_col, logical(1)), drop = FALSE]

    if (nrow(df) < 2 || ncol(df) < 2) stop("CSV is too small to be a correlation matrix.")

    to_num <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))
    is_all_numericish <- function(x) {
      y <- to_num(x)
      any(is.finite(y)) && all(is.finite(y) | is.na(y))
    }

    # --- Fallback for: left-hand labels only, but no header row 
    # Example: first column is labels, remaining columns are numeric,
    # and the first data row got swallowed as the header (k-1 x (k+1)).

    looks_like_rowlabels_no_header <- function(df) {
      if (ncol(df) != nrow(df) + 2) {
        return(FALSE)
      }

      # first column should be non-numeric (labels)
      if (is_all_numericish(df[[1]])) {
        return(FALSE)
      }

      # the remaining column names came from the first data row and should be numeric-ish
      if (!all(is.finite(to_num(names(df)[-1])))) {
        return(FALSE)
      }

      # and the remaining columns should contain numeric-ish values
      if (!all(vapply(df[-1], is_all_numericish, logical(1)))) {
        return(FALSE)
      }

      TRUE
    }

    if (looks_like_rowlabels_no_header(df)) {
      df <- read.csv(path2,
        header = FALSE,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      names(df) <- trimws(names(df))
    }


    # --- Fallback for headerless numeric matrices 
    # If the file has NO header row, read.csv(header=TRUE) will eat the first row,
    # giving (k-1) x k. Detect that pattern and re-read with header=FALSE.

    looks_like_headerless_matrix <- function(df) {
      # "k-1 x k" shape is the giveaway for a k x k matrix with no header line
      if (ncol(df) != nrow(df) + 1) {
        return(FALSE)
      }

      # Column names came from the first data row; if they're all numeric-ish,
      # it's almost certainly a headerless numeric matrix.
      cn_num <- to_num(names(df))
      if (!all(is.finite(cn_num))) {
        return(FALSE)
      }

      # And the remaining columns should all be numeric-ish too
      if (!all(vapply(df, is_all_numericish, logical(1)))) {
        return(FALSE)
      }

      TRUE
    }

    if (looks_like_headerless_matrix(df)) {
      df <- read.csv(path2,
        header = FALSE,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      # If read.csv(header=FALSE) created default names V1..Vk, treat as "no labels"
      # (so we don't propagate V1.. into var_names)
      names(df) <- trimws(names(df))
    }


    # Detect label column (first col text, rest numeric-ish)
    has_label_col <- !is_all_numericish(df[[1]]) && all(vapply(df[-1], is_all_numericish, logical(1)))
    row_labels <- NULL
    if (has_label_col) {
      row_labels <- clean_label(df[[1]])
      df <- df[-1]
    }


    # Detect label row via column names (not all numeric-ish)
    nm <- trimws(names(df))
    default_header <- is_default_vnames(nm)
    header_is_numericish <- all(is.finite(to_num(nm)))
    col_labels <- if (!header_is_numericish && !default_header) clean_label(nm) else NULL


    # Build numeric matrix
    m <- as.matrix(data.frame(lapply(df, to_num), check.names = FALSE))
    storage.mode(m) <- "numeric"

    if (any(!is.finite(m))) {
      stop("Non-numeric values found inside the numeric block.")
    }

    if (nrow(m) != ncol(m) || nrow(m) < 2) {
      stop(
        sprintf(
          "Uploaded matrix must be square. Got %dx%d after parsing.", nrow(m), ncol(m)
        )
      )
    }

    # Decide var_names
    var_names <- NULL
    if (!is.null(row_labels) && !is.null(col_labels)) {
      if (!identical(row_labels, col_labels)) {
        stop("Row/column labels do not match exactly (same names, same order required).")
      }
      var_names <- row_labels
    } else if (!is.null(row_labels)) {
      var_names <- row_labels
    } else if (!is.null(col_labels)) {
      var_names <- col_labels
    }

    if (!is.null(var_names)) {
      rownames(m) <- var_names
      colnames(m) <- var_names
    }

    # Corr sanity
    sym_err <- max(abs(m - t(m)))

    list(
      m = m,
      var_names = var_names,
      meta = list(
        has_label_col = has_label_col,
        has_label_row = !is.null(col_labels),
        sym_err = sym_err
      )
    )
  }


  coercion_warning <- reactiveVal(NULL)
  scale_params_csv <- reactiveVal(NULL) # store uploaded params as data.frame

  # Stores last successfully validated uploaded matrix
  M_upload_ok <- reactiveVal(NULL)
  M_upload_names <- reactiveVal(NULL)


  # Upload messaging
  upload_error <- reactiveVal(NULL)
  upload_status <- reactiveVal(NULL)
  # stores from validate_matrix_upload()$details
  upload_diag <- reactiveVal(NULL) 


  default_scale_names <- reactive({
    k <- k_scales()

    nm <- if (identical(input$matrixSource, "upload")) M_upload_names() else NULL
    if (!is.null(nm) && length(nm) == k) {
      nm
    } else {
      paste0("Scale", sprintf("%02d", seq_len(k)))
    }
  })


  # Preferred scale names for the Generate tab:
  # - upload mode: parsed labels (M_upload_names) if present
  # - otherwise: colnames(M()) if present
  # - otherwise: Scale01, Scale02, ...
  matrix_scale_names <- reactive({
    m <- M()
    if (is.null(m)) {
      return(NULL)
    }

    k <- ncol(m)

    nm <- NULL
    if (identical(input$matrixSource, "upload")) {
      nm <- M_upload_names()
    }
    if (is.null(nm) || length(nm) != k) {
      nm <- colnames(m)
    }
    if (is.null(nm) || length(nm) != k) {
      nm <- paste0("Scale", sprintf("%02d", seq_len(k)))
    }

    nm <- trimws(as.character(nm))
    nm[nm == "" | is.na(nm)] <- paste0("Scale", sprintf("%02d", which(nm == "" | is.na(nm))))
    nm
  })


  M_for_display <- reactive({
    m <- M()
    req(is_valid_matrix(m))

    # choose names based on source
    nm <- NULL
    if (identical(input$matrixSource, "upload")) nm <- M_upload_names()
    if (identical(input$matrixSource, "alpha")) nm <- paste0("V", seq_len(ncol(m)))

    if (!is.null(nm) && length(nm) == ncol(m)) {
      dimnames(m) <- list(nm, nm)
    }
    m
  })


  validate_matrix_upload <- function(df_or_m,
                                     sym_fix = c("none", "average", "lower_to_upper", "upper_to_lower"),
                                     use_near_pd = FALSE,
                                     sym_tol = 1e-8) {
    sym_fix <- match.arg(sym_fix)

    res <- list(
      ok = FALSE,
      msg = NULL,
      details = list(),
      m = NULL
    )

    # ---- coerce to numeric matrix ----
    m <- df_or_m
    if (is.data.frame(m)) m <- as.matrix(m)
    if (!is.matrix(m)) {
      res$msg <- "Uploaded object is not a matrix/data.frame."
      return(res)
    }

    storage.mode(m) <- "numeric"

    # ---- shape ----
    if (nrow(m) < 2 || ncol(m) != nrow(m)) {
      res$msg <- "Uploaded CSV must be a square numeric matrix (k x k), with k >= 2."
      res$details$dim <- paste0(nrow(m), " x ", ncol(m))
      return(res)
    }

    # ---- finite ----
    if (!all(is.finite(m))) {
      bad <- which(!is.finite(m), arr.ind = TRUE)
      res$msg <- "Matrix contains NA/Inf values."
      res$details$bad_cells <- utils::head(bad, 10)
      return(res)
    }

    # ---- diagonal ----
    diag(m) <- 1

    # ---- range ----
    mx <- max(abs(m))
    if (mx > 1) {
      res$msg <- "Matrix has values outside [-1, 1]."
      res$details$max_abs <- mx
      return(res)
    }

    # ---- symmetry ----
    sym_err <- max(abs(m - t(m)))
    res$details$sym_err <- sym_err

    if (sym_err > sym_tol) {
      if (identical(sym_fix, "none")) {
        res$msg <- sprintf(
          "Matrix is not symmetric (max |M - t(M)| = %.3g). Choose a symmetry repair option.",
          sym_err
        )
        return(res)
      }

      if (identical(sym_fix, "average")) {
        m <- (m + t(m)) / 2
      } else if (identical(sym_fix, "lower_to_upper")) {
        m[upper.tri(m)] <- t(m)[upper.tri(m)]
      } else if (identical(sym_fix, "upper_to_lower")) {
        m[lower.tri(m)] <- t(m)[lower.tri(m)]
      }

      diag(m) <- 1
      res$details$sym_fixed <- TRUE
      res$details$sym_err_after <- max(abs(m - t(m)))
    } else {
      res$details$sym_fixed <- FALSE
      res$details$sym_err_after <- sym_err
    }

    # ---- positive definiteness ----
    pd <- matrixcalc::is.positive.definite(m)
    res$details$pd_before_nearpd <- pd

    if (!pd) {
      if (!isTRUE(use_near_pd)) {
        res$msg <- "Matrix is not positive-definite. Turn on nearPD repair or upload a PD matrix."
        return(res)
      }

      m <- as.matrix(Matrix::nearPD(m, corr = TRUE)$mat)
      diag(m) <- 1
      m <- (m + t(m)) / 2

      res$details$nearpd_applied <- TRUE
      res$details$pd_after_nearpd <- matrixcalc::is.positive.definite(m)
    } else {
      res$details$nearpd_applied <- FALSE
      res$details$pd_after_nearpd <- pd
    }

    # ---- eigenvalue detail (helpful UX) ----
    eig <- tryCatch(eigen(m, only.values = TRUE)$values, error = function(e) NA_real_)
    res$details$min_eigen <- if (all(is.na(eig))) NA_real_ else min(eig)

    # ---- success ----
    res$ok <- TRUE
    res$m <- m
    res$msg <- sprintf(
      "Upload OK. Applied %dx%d matrix.%s%s",
      ncol(m), ncol(m),
      if (isTRUE(res$details$sym_fixed)) " (symmetry repaired)" else "",
      if (isTRUE(res$details$nearpd_applied)) " (nearPD applied)" else ""
    )

    res
  }


  # ---- Scale-parameters CSV (status + helpers) ----
  scale_params_status <- reactiveVal(NULL) # list(type="ok|err|info", msg="...")
  scale_params_applied <- reactiveVal(FALSE)

  set_scale_params_status <- function(type = c("info", "ok", "err"), msg) {
    type <- match.arg(type)
    scale_params_status(list(type = type, msg = msg))
  }

  # Read + validate scale-params CSV
  read_scale_params_csv <- function(path) {
    df <- read.csv(path,
      header = TRUE,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(df) <- trimws(names(df))

    required <- c("scale", "mean", "sd", "lower", "upper", "nItems")
    missing <- setdiff(required, names(df))
    if (length(missing)) {
      stop("Missing required columns: ", paste(missing, collapse = ", "))
    }

    # Keep only required cols (ignore extras quietly)
    df <- df[, required, drop = FALSE]

    # Coerce numeric columns
    num_cols <- c("mean", "sd", "lower", "upper", "nItems")
    for (nm in num_cols) {
      df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    }

    df
  }


  output$uploadMatrixStatus <- renderUI({
    if (!is.null(upload_error())) {
      txt <- if (!is.null(M_upload_ok())) {
        paste0(
          "Upload failed: ", upload_error(),
          " — still using previous matrix (k = ", ncol(M_upload_ok()), ")."
        )
      } else {
        paste0("Upload failed: ", upload_error())
      }

      return(tags$div(class = "alert alert-danger lm-alert-oneline", txt))
    }

    if (!is.null(upload_status())) {
      return(tags$div(class = "alert alert-success lm-alert-oneline", upload_status()))
    }

    NULL
  })


  output$scaleParamsControls <- renderUI({
    # only allow if we have a usable matrix (so we know K)
    has_m <- tryCatch(is_valid_matrix(M()), error = function(e) FALSE)

    dl_btn <- downloadButton("downloadScaleParamsTemplate",
      "Download CSV template",
      class = "lm-btn-auto"
    )

    up_in <- tags$div(
      class = "lm-file-auto",
      fileInput(
        "scaleParamsFile",
        "Upload scale parameters (.csv)",
        accept = c(".csv")
      )
    )


    if (has_m) {
      tagList(dl_btn, up_in)
    } else {
      tagList(
        bslib::tooltip(
          disable_ui(dl_btn),
          "Create or upload a correlation matrix first."
        ),
        bslib::tooltip(
          disable_ui(up_in),
          "Create or upload a correlation matrix first."
        )
      )
    }
  })

  output$scaleParamsStatus <- renderUI({
    st <- scale_params_status()
    if (is.null(st)) {
      return(NULL)
    }

    cls <- switch(st$type,
      "ok"   = "alert alert-success py-1 px-2 small mb-0",
      "err"  = "alert alert-danger py-1 px-2 small mb-0",
      "info" = "alert alert-secondary py-1 px-2 small mb-0"
    )

    tags$div(class = cls, st$msg)
  })


  output$downloadScaleParamsTemplate <- downloadHandler(
    filename = function() paste0("LikertMakeR-scale-params-template-", Sys.Date(), ".csv"),
    content = function(file) {
      req(M())
      k <- ncol(M())

      df <- data.frame(
        scale = paste0("Scale", sprintf("%02d", seq_len(k))),
        mean = rep(3, k),
        sd = rep(1, k),
        lower = rep(1, k),
        upper = rep(5, k),
        nItems = rep(1, k),
        stringsAsFactors = FALSE
      )
      write.csv(df, file, row.names = FALSE)
    }
  )


  output$paramModeUI <- renderUI({
    req(input$paramMode)

    if (identical(input$paramMode, "manual")) {
      bslib::card(
        bslib::card_header("Manual entry"),
        h5("Input scale_name and parameters"),
        numericInput("n", "Number of Observations",
          value = 64, min = 4, max = 512
        ),
        uiOutput("dynamicInputs")
      )
    } else {
      bslib::card(
        bslib::card_header("Upload scale parameters (.csv)"),
        uiOutput("scaleParamsControls"),
        uiOutput("scaleParamsStatus"),
        numericInput("n", "Number of Observations",
          value = 64, min = 4, max = 512
        )
      )
    }
  })

  output$dataGenSidebar <- renderUI({
    tagList(
      h5("Scale parameters"),
      radioButtons(
        "paramMode", "Enter parameters via:",
        choices = c("Manual entry" = "manual", "Upload CSV" = "csv"),
        selected = "manual",
        inline = TRUE
      ),
      uiOutput("paramModeUI"),
      div(
        class = "d-flex flex-wrap gap-2 align-items-center mt-2",
        actionButton("generate", "Generate my data", icon("paper-plane"), class = "btn btn-primary"),
        uiOutput("downloadDataBtn"),
        uiOutput("coercionNotice"),
        uiOutput("codebookBtn")
      )
    )
  })


  observeEvent(input$scaleParamsFile,
    {
      req(identical(input$paramMode, "csv"))
      req(input$scaleParamsFile)
      req(M()) # ensure we know K

      set_scale_params_status("info", "Reading scale-parameters CSV…")
      scale_params_applied(FALSE)
      # optional but good: clear any previous df so you can't accidentally generate using stale params
      scale_params_csv(NULL)

      k <- ncol(M())

      df <- tryCatch(
        read_scale_params_csv(input$scaleParamsFile$datapath),
        error = function(e) {
          set_scale_params_status("err", paste0("Scale-params upload failed: ", conditionMessage(e)))
          return(NULL)
        }
      )
      if (is.null(df)) {
        return()
      }

      if (nrow(df) != k) {
        set_scale_params_status(
          "err",
          paste0(
            "Scale-params CSV has ", nrow(df), " row(s), but your correlation matrix has ",
            k, " scale(s). Please upload a CSV with exactly ", k, " row(s)."
          )
        )
        return()
      }

      # These should already be numeric if read_scale_params_csv() coerces them
      if (any(!is.finite(df$mean)) || any(!is.finite(df$sd)) ||
        any(!is.finite(df$lower)) || any(!is.finite(df$upper)) || any(!is.finite(df$nItems))) {
        set_scale_params_status("err", "Scale-params CSV contains missing or non-numeric values.")
        return()
      }
      if (any(df$sd <= 0)) {
        set_scale_params_status("err", "All SD values must be > 0.")
        return()
      }

      # integer-ish coercion consistent with your app
      lower_i <- floor(df$lower)
      upper_i <- floor(df$upper)
      nItems_i <- pmax(1L, floor(df$nItems))

      # IMPORTANT: validate bounds BEFORE saving into reactiveVal
      if (any(upper_i <= lower_i)) {
        set_scale_params_status("err", "Each upper bound must be > lower bound (after integer coercion).")
        return()
      }
      if (any(df$mean <= lower_i | df$mean >= upper_i)) {
        set_scale_params_status("err", "Each mean must lie strictly between lower and upper bounds.")
        return()
      }

      # Only now store the cleaned df for syntheticData() to use
      df2 <- df
      df2$lower <- lower_i
      df2$upper <- upper_i
      df2$nItems <- nItems_i
      scale_params_csv(df2)

      # Apply to inputs (optional, but nice)
      for (i in seq_len(k)) {
        updateTextInput(session, paste0("scaleName", i),
          value = as.character(df$scale[i])
        )
        updateNumericInput(session, paste0("mean", i), value = df$mean[i])
        updateNumericInput(session, paste0("sd", i), value = df$sd[i])
        updateNumericInput(session, paste0("lower", i), value = lower_i[i])
        updateNumericInput(session, paste0("upper", i), value = upper_i[i])
        updateNumericInput(session, paste0("items", i), value = nItems_i[i])
      }

      if (any(df$lower != lower_i) || any(df$upper != upper_i) || any(df$nItems != nItems_i)) {
        coercion_warning("Scale-params CSV: lower/upper/nItems were coerced to integers (floored).")
      } else {
        coercion_warning(NULL)
      }

      scale_params_applied(TRUE)
      set_scale_params_status("ok", paste0("Applied scale parameters for ", k, " scale(s)."))
    },
    ignoreInit = TRUE
  )

  observeEvent(input$symFix,
    {
      req(identical(input$matrixSource, "upload"))
      req(M_upload_ok())

      v <- validate_matrix_upload(
        df_or_m = M_upload_ok(),
        sym_fix = input$symFix,
        use_near_pd = isTRUE(input$useNearPD)
      )
      if (isTRUE(v$ok)) {
        M_upload_ok(v$m)
        upload_diag(v$details)
        upload_status(v$msg)
        upload_error(NULL)
      }
    },
    ignoreInit = TRUE
  )


  output$coercionNotice <- renderUI({
    msg <- coercion_warning()
    if (is.null(msg)) {
      return(NULL)
    }

    tags$div(
      class = "alert alert-warning py-1 px-2 small mb-0",
      msg
    )
  })


  output$matrixAlphaControls <- renderUI({
    req(input$matrixSource)

    if (identical(input$matrixSource, "alpha")) {
      div(
        class = "lm-sidebar-block",
        numericInput("scales", "Number of items",
          value = 4, min = 2, step = 1
        ),
        sliderInput("alpha", "Target Cronbach's Alpha",
          min = 0.40, max = 1.00, value = 0.80, step = 0.01
        ),
        sliderInput("variance", "Variance (optional)",
          min = 0, max = 1.5, value = 0.50, step = 0.01
        ),
        actionButton("makeMatrix", "Generate correlation matrix",
          icon = icon("calculator"), class = "btn-primary w-100"
        )
      )
    } else {
      div(
        class = "lm-sidebar-block",
        fileInput("matrixFile", "Upload correlation matrix CSV",
          accept = c(".csv"),
          buttonLabel = "Browse…"
        ),
        uiOutput("matrixUploadPreview")
      )
    }
  })


  fmt_num <- function(x, digits = 3) {
    if (is.null(x) || length(x) == 0 || anyNA(x) || !is.finite(x)) {
      return("—")
    }
    formatC(x, format = "f", digits = digits)
  }


  # check matrix correct
  matrix_attempted <- reactiveVal(FALSE)

  matrix_error <- reactiveVal(NULL)


  observeEvent(input$matrixSource,
    {
      bslib::accordion_panel_close(
        id = "matrixAccordion",
        values = TRUE,
        session = session
      )

      # Only do upload-only stuff when in upload mode
      if (identical(input$matrixSource, "upload")) {
        nm <- M_upload_names()
        if (!is.null(nm) && !isTRUE(scale_names_touched())) {
          for (i in seq_along(nm)) {
            updateTextInput(session, paste0("scaleName", i), value = nm[i])
          }
        }
      }
    },
    ignoreInit = TRUE
  )


  observeEvent(matrix_error(),
    {
      req(identical(input$matrixSource, "alpha"))
      if (!is.null(matrix_error()) && nzchar(matrix_error())) {
        bslib::accordion_panel_open(
          id = "matrixAccordion",
          values = "diag",
          session = session
        )
      }
    },
    ignoreInit = TRUE
  )


  observeEvent(upload_error(),
    {
      req(identical(input$matrixSource, "upload"))
      if (!is.null(upload_error()) && nzchar(upload_error())) {
        bslib::accordion_panel_open(
          id = "matrixAccordion",
          values = "diag",
          session = session
        )
      }
    },
    ignoreInit = TRUE
  )


  # ---- alpha-generated matrix ----
  M_alpha <- eventReactive(input$makeMatrix, {
    matrix_attempted(TRUE)
    matrix_error(NULL)
    tryCatch(
      {
        m <- makeCorrAlpha(
          items = input$scales,
          alpha = input$alpha,
          variance = input$variance
        )
        if (!matrixcalc::is.positive.definite(m)) {
          stop("Alpha generation produced a non–positive-definite matrix.\nTry reducing Variance or Alpha.")
        }
        m
      },
      error = function(e) {
        matrix_error(conditionMessage(e))
        NULL
      }
    )
  })


  # If alpha generation succeeds, auto-open the matrix panel
  observeEvent(M_alpha(),
    {
      req(identical(input$matrixSource, "alpha"))
      if (!is.null(M_alpha())) {
        bslib::accordion_panel_open(
          id = "matrixAccordion",
          values = "matrix",
          session = session
        )
      }
    },
    ignoreInit = TRUE
  )


  output$downloadCurrentMatrixBtn <- renderUI({
    req(input$matrixSource)

    if (input$matrixSource == "alpha" && is.null(M_alpha())) {
      disable_ui(
        downloadButton(
          "downloadCurrentMatrix", "Download current matrix"
        )
      )
    } else if (input$matrixSource == "upload" && is.null(input$matrixFile)) {
      disable_ui(
        downloadButton(
          "downloadCurrentMatrix", "Download current matrix"
        )
      )
    } else {
      downloadButton(
        "downloadCurrentMatrix", "Download current matrix"
      )
    }
  })


  # ---- uploaded matrix ----

  observeEvent(input$matrixFile,
    {
      req(input$matrixSource == "upload")
      req(input$matrixFile)

      upload_error(NULL)
      upload_diag(NULL)
      upload_status("Reading CSV…")

      parsed <- NULL # <-- keep in scope for error handler / future preview

      tryCatch(
        {
          # 1) parse CSV -> numeric matrix + optional names
          parsed <- parse_corr_csv(input$matrixFile$datapath)

          # 2) validate/repair numeric matrix
          message("symFix = ", input$symFix)

          out <- validate_matrix_upload(
            df_or_m = parsed$m,
            sym_fix = input$symFix,
            use_near_pd = isTRUE(input$useNearPD)
          )
          if (!isTRUE(out$ok)) stop(out$msg)

          # 3) commit
          m_ok <- out$m

          # (1) store matrix "bare" (no dimnames)
          dimnames(m_ok) <- NULL

          M_upload_ok(m_ok)

          # (2) store names separately if present and the right length
          nm <- parsed$var_names
          if (!is.null(nm) && length(nm) == ncol(m_ok)) {
            M_upload_names(nm)
          } else {
            M_upload_names(NULL)
          }

          if (!is.null(parsed$var_names)) {
            colnames(out$m) <- parsed$var_names
            rownames(out$m) <- parsed$var_names
          }
          M_upload_ok(out$m)
          M_upload_names(parsed$var_names)


          upload_status(out$msg)
          upload_diag(out$details)
          upload_error(NULL)

          bslib::accordion_panel_open(
            "matrixAccordion",
            values = "matrix",
            session = session
          )
        },
        error = function(e) {
          upload_error(conditionMessage(e))

          # keep the last status message rather than blanking it (optional)
          # upload_status(NULL)

          upload_diag(NULL)
          bslib::accordion_panel_open("matrixAccordion",
            values = "diag", session = session
          )
        }
      )
    },
    ignoreInit = TRUE
  )


  # ---- active matrix used everywhere downstream ----
  M <- reactive({
    req(input$matrixSource)

    if (identical(input$matrixSource, "alpha")) {
      return(M_alpha())
    }
    if (identical(input$matrixSource, "upload")) {
      return(M_upload_ok())
    }

    NULL
  })


  k_scales <- reactive({
    req(M())
    ncol(M())
  })

  observeEvent(M(),
    {
      scale_params_csv(NULL)
      scale_params_applied(FALSE)
      scale_params_status(NULL)
    },
    ignoreInit = TRUE
  )


  output$matrixDiagBoxes <- renderUI({
    req(input$matrixSource)

    icon_safe <- function(name) {
      tryCatch(bsicons::bs_icon(name), error = function(e) NULL)
    }

    diag_tile <- function(title,
                          value,
                          status = "secondary",
                          icon_name = NULL) {
      tags$div(
        class = "card border-0 shadow-sm mb-2",
        tags$div(
          class = paste0(
            "card-body py-2 px-2 border-start border-4 border-", status
          ),
          tags$div(
            class = "d-flex justify-content-between align-items-center",
            tags$div(
              tags$div(class = "small text-muted", title),
              tags$div(class = "fw-semibold", value)
            ),
            icon_safe(icon_name)
          )
        )
      )
    }

    # Not ready yet
    # Alpha path: not tried yet
    if (identical(input$matrixSource, "alpha") && is.null(M_alpha())) {
      if (!isTRUE(matrix_attempted())) {
        return(
          diag_tile(
            "Status", "Click 'Generate Matrix' to create a matrix",
            "secondary", "info-circle"
          )
        )
      }
      if (!is.null(matrix_error())) {
        return(
          diag_tile(
            "Alpha generation failed", matrix_error(),
            "danger", "exclamation-triangle"
          )
        )
      }
      return(
        diag_tile(
          "Status", "No matrix available yet", "secondary", "info-circle"
        )
      )
    }

    if (identical(input$matrixSource, "upload") && is.null(input$matrixFile)) {
      return(diag_tile("Status", "No file uploaded", "secondary", "upload"))
    }
    if (identical(input$matrixSource, "upload") && is.null(M_upload_ok()) && !is.null(upload_error())) {
      return(diag_tile(
        "Upload failed", upload_error(),
        "danger", "exclamation-triangle"
      ))
    }


    # Alpha path: tried, but failed
    if (identical(input$matrixSource, "alpha") && is.null(M_alpha()) && !is.null(matrix_error())) {
      return(
        bslib::layout_columns(
          col_widths = 12,
          diag_tile(
            "Alpha generation failed", matrix_error(),
            "danger", "exclamation-triangle"
          )
        )
      )
    }

    if (identical(input$matrixSource, "alpha") &&
      isTRUE(matrix_attempted()) &&
      is.null(M_alpha()) &&
      (is.null(matrix_error()) || !nzchar(matrix_error()))) {
      return(
        bslib::layout_columns(
          col_widths = 12,
          diag_tile(
            "Alpha generation failed", "No details available.",
            "danger", "exclamation-triangle"
          )
        )
      )
    }


    if (identical(input$matrixSource, "upload") && is.null(input$matrixFile)) {
      return(
        bslib::layout_columns(
          col_widths = 12,
          diag_tile("Status", "No file uploaded", "secondary", "upload")
        )
      )
    }

    if (identical(input$matrixSource, "upload") && is.null(M_upload_ok()) && !is.null(upload_error())) {
      return(
        bslib::layout_columns(
          col_widths = 12,
          diag_tile(
            "Upload failed", upload_error(), "danger", "exclamation-triangle"
          )
        )
      )
    }


    # Compute matrix (upload path can error/validate)
    m <- tryCatch(M(), error = function(e) e)
    if (inherits(m, "error") || is.null(m)) {
      msg <- if (inherits(m, "error")) conditionMessage(m) else "No matrix available."
      return(
        diag_tile(
          "Matrix error", msg, "danger", "exclamation-triangle"
        )
      )
    }

    pd <- matrixcalc::is.positive.definite(m)
    sym_err <- max(abs(m - t(m)))

    eig <- tryCatch(eigen(m, only.values = TRUE)$values, error = function(e) NA_real_)
    min_eig <- if (all(is.na(eig))) NA_real_ else min(eig)

    c_alpha <- tryCatch(LikertMakeR::alpha(m), error = function(e) NA_real_)

    # Use fluidRow/column (more robust than bslib layout helpers in renderUI)
    fluidRow(
      column(
        3,
        diag_tile(
          "Positive definite",
          if (pd) "OK" else "Fail",
          if (pd) "success" else "danger",
          if (pd) "check-circle" else "x-circle"
        )
      ),
      column(
        3,
        diag_tile(
          "Symmetry",
          paste0("Δ = ", signif(sym_err, 3)),
          if (sym_err < 1e-8) "success" else "warning",
          "arrows-angle-contract"
        )
      ),
      column(
        3,
        diag_tile(
          "Min eigenvalue",
          if (is.na(min_eig)) "—" else formatC(min_eig, format = "f", digits = 4),
          if (!is.na(min_eig) && min_eig > 0) "success" else "warning",
          "bar-chart"
        )
      ),
      column(
        3,
        diag_tile(
          "α (from matrix)",
          if (is.na(c_alpha)) "—" else formatC(c_alpha, format = "f", digits = 3),
          "info",
          "calculator" # <<--- replace "sigma" with a real icon
        )
      )
    )
  })


  output$matrixDiagDetail <- renderPrint({
    req(input$matrixSource)

    # ---- ALPHA path ----
    if (identical(input$matrixSource, "alpha")) {
      if (is.null(M_alpha())) {
        cat("Click 'Generate Matrix' to create a correlation matrix.\n")
        if (!is.null(matrix_error())) {
          cat("\nLast message:\n", matrix_error(), "\n", sep = "")
        }
        return()
      }

      m <- M_alpha()
      eig <- tryCatch(
        eigen(m, only.values = TRUE)$values,
        error = function(e) NA_real_
      )

      cat("Details (alpha-generated)\n")
      cat("----------------------\n")
      cat("Dimensions:", nrow(m), "x", ncol(m), "\n")
      cat("Diagonal all 1s:", all(abs(diag(m) - 1) < 1e-12), "\n")
      cat("Symmetry max |M - t(M)|:", signif(max(abs(m - t(m))), 4), "\n")
      cat("Positive definite:", matrixcalc::is.positive.definite(m), "\n")
      cat("Min/Max:", signif(min(m), 4), "/", signif(max(m), 4), "\n")
      cat("Min eigenvalue:", if (all(is.na(eig))) "—" else signif(min(eig), 5), "\n")
      cat("Eigenvalues:", if (all(is.na(eig))) "—" else paste(round(eig, 3), collapse = ", "), "\n")

      if (!is.null(matrix_error())) {
        cat("\nAlpha-generation message:\n")
        cat(matrix_error(), "\n")
      }
      return()
    }

    # ---- UPLOAD path ----
    if (identical(input$matrixSource, "upload")) {
      if (is.null(input$matrixFile) && is.null(M_upload_ok())) {
        cat("Upload a CSV file to load a correlation matrix.\n")
        return()
      }

      if (!is.null(upload_error())) {
        cat("Upload failed:\n")
        cat(upload_error(), "\n\n")
        cat("Tip: check symmetry, value range [-1, 1], and PD status.\n")
        return()
      }

      m <- M_upload_ok()
      if (is.null(m)) {
        cat("No uploaded matrix is currently applied.\n")
        return()
      }

      d <- upload_diag()

      # Fallback if details weren't stored for some reason
      if (is.null(d)) {
        eig <- tryCatch(eigen(m, only.values = TRUE)$values, error = function(e) NA_real_)
        cat("Details (uploaded)\n")
        cat("-----------------\n")
        cat("Dimensions:", nrow(m), "x", ncol(m), "\n")
        cat("Symmetry max |M - t(M)|:", signif(max(abs(m - t(m))), 4), "\n")
        cat("Positive definite:", matrixcalc::is.positive.definite(m), "\n")
        cat("Min eigenvalue:", if (all(is.na(eig))) "—" else signif(min(eig), 5), "\n")
        return()
      }

      cat("Details (uploaded)\n")
      cat("-----------------\n")
      cat("Dimensions:", nrow(m), "x", ncol(m), "\n")

      cat("Symmetry max |M - t(M)| (before): ", signif(d$sym_err %||% NA_real_, 4), "\n", sep = "")
      cat("Symmetry repaired:", isTRUE(d$sym_fixed), "\n")
      cat("Symmetry max |M - t(M)| (after):  ", signif(d$sym_err_after %||% NA_real_, 4), "\n", sep = "")

      cat("Positive definite (before nearPD):", isTRUE(d$pd_before_nearpd), "\n")
      cat("nearPD applied:", isTRUE(d$nearpd_applied), "\n")
      cat("Positive definite (after nearPD): ", isTRUE(d$pd_after_nearpd), "\n", sep = "")

      cat("Min eigenvalue:", signif(d$min_eigen %||% NA_real_, 5), "\n")

      # Helpful quick checks on the FINAL matrix
      cat("Min/Max:", signif(min(m), 4), "/", signif(max(m), 4), "\n")
      cat("Diagonal all 1s:", all(abs(diag(m) - 1) < 1e-12), "\n")

      return()
    }
  })


  # ---- matrix display outputs in Correlation Matrix panel ----
  output$corrMatrixDT <- DT::renderDT({
    m <- M_for_display()

    DT::datatable(
      round(m, 3),
      rownames = TRUE,
      options = list(
        scrollX = TRUE,
        pageLength = nrow(m),
        dom = "t"
      )
    )
  })

    # ---- plot + stats cache (data validation) ----
  plotStorage <- reactiveVal(NULL)
  plot_key <- reactiveVal(NULL)


  plotPngPath <- reactiveVal(NULL)
  plotPngKey <- reactiveVal(NULL)
  plotPngWH <- reactiveVal(NULL) # c(width, height, res)
  stats_cache <- reactiveVal(NULL)
  stats_key <- reactiveVal(NULL)

  # We only build the heavy GGally plot when the user actually visits the Validation tab
  validation_seen <- reactiveVal(FALSE)

  # Prevent duplicate builds if user clicks around while a plot is still being generated
  plot_building <- reactiveVal(FALSE)

  observeEvent(input$main_nav,
    {
      if (identical(input$main_nav, "validate")) validation_seen(TRUE)
    },
    ignoreInit = TRUE
  )

  # Cheap fingerprint for when the generated dataset changes
  data_key <- reactive({
    df <- syntheticData()
    req(is.data.frame(df), nrow(df) > 0, ncol(df) > 0)
    paste0(nrow(df), "x", ncol(df), "|", paste(names(df), collapse = ","))
  })


  # If syntheticData changes for any reason other than the Generate button (e.g., uploads / restores),
  # make sure cached validation artefacts are cleared.
  observeEvent(syntheticData(),
    {
      key_now <- tryCatch(data_key(), error = function(e) NULL)
      if (is.null(key_now)) {
        return(invisible())
      }

      if (!is.null(plot_key()) && !identical(plot_key(), key_now)) {
        plotStorage(NULL)
        plot_key(NULL)
      }
      if (!is.null(plotPngKey()) && !identical(plotPngKey(), key_now)) {
        plotPngPath(NULL)
        plotPngKey(NULL)
        plotPngWH(NULL)
      }
    },
    ignoreInit = TRUE
  )


  # When the user generates data: compute fast summaries immediately; invalidate plot cache
  observeEvent(input$generate,
    {
      df <- syntheticData()
      req(is.data.frame(df), nrow(df) > 0, ncol(df) > 0)

      key <- paste0(nrow(df), "x", ncol(df), "|", paste(names(df), collapse = ","))

      # Use numeric columns only (protect against factor/character columns)
      num_cols <- vapply(df, is.numeric, logical(1))
      dfn <- if (any(num_cols)) df[, num_cols, drop = FALSE] else NULL

      if (is.null(dfn) || ncol(dfn) == 0) {
        stats_cache(list(
          n = nrow(df),
          p = ncol(df),
          means = NULL,
          sds = NULL,
          mins = NULL,
          maxs = NULL,
          eigen_vals = NULL,
          alpha = NA_real_,
          note = "No numeric columns available for summary statistics."
        ))
        stats_key(key)

        plotStorage(NULL)
        plot_key(NULL)

        plotPngPath(NULL)
        plotPngKey(NULL)
        plotPngWH(NULL)

        return(invisible())
      }

      # ---- fast stats first (always) ----
      means <- round(colMeans(dfn, na.rm = TRUE), 3)
      sds <- round(vapply(dfn, sd, numeric(1), na.rm = TRUE), 3)
      mins <- round(vapply(dfn, min, numeric(1), na.rm = TRUE), 3)
      maxs <- round(vapply(dfn, max, numeric(1), na.rm = TRUE), 3)

      # Set cache immediately so summaries can render even if later steps fail
      stats_cache(list(
        n = nrow(dfn),
        p = ncol(dfn),
        means = means,
        sds = sds,
        mins = mins,
        maxs = maxs,
        eigen_vals = NULL,
        alpha = NA_real_,
        note = NULL
      ))
      stats_key(key)

      # ---- optional extras (guarded) ----
      alpha_val <- tryCatch(
        {
          LikertMakeR::alpha(NULL, dfn)
        },
        error = function(e) NA_real_
      )

      eig_vals <- NULL
      if (ncol(dfn) >= 2 && ncol(dfn) <= 60) {
        cor_dat <- tryCatch(cor(dfn, use = "pairwise.complete.obs") |> as.matrix(), error = function(e) NULL)
        eig_vals <- if (is.null(cor_dat)) NULL else tryCatch(eigen(cor_dat, only.values = TRUE)$values, error = function(e) NULL)
        eig_vals <- if (is.null(eig_vals)) NULL else round(eig_vals, 2)
      }

      # Update cache with extras
      stats_cache(list(
        n = nrow(dfn),
        p = ncol(dfn),
        means = means,
        sds = sds,
        mins = mins,
        maxs = maxs,
        eigen_vals = eig_vals,
        alpha = alpha_val,
        note = NULL
      ))
      stats_key(key)

      # ---- invalidate plot cache (data changed) ----
      plotStorage(NULL)
      plot_key(NULL)

      plotPngPath(NULL)
      plotPngKey(NULL)
      plotPngWH(NULL)
    },
    ignoreInit = TRUE
  )


  # Keep your dynamicInputs in sync with uploaded dimension

  observeEvent(M_upload_ok(),
    {
      req(input$matrixSource == "upload")
      req(M_upload_ok())
      updateNumericInput(session, "scales",
        value = ncol(M_upload_ok())
      )
    },
    ignoreInit = TRUE
  )

  scale_names_touched <- reactiveVal(FALSE)

  observeEvent(grep("^scaleName\\d+$", names(input), value = TRUE),
    {
      scale_names_touched(TRUE)
    },
    ignoreInit = TRUE
  )


  # ---- download handlers ----
  output$downloadMatrixTemplate <- downloadHandler(
    filename = function() paste0("correlation-matrix-template-", Sys.Date(), ".csv"),
    content = function(file) {
      k <- if (!is.null(input$scales)) input$scales else 4
      m <- diag(1, k)
      colnames(m) <- paste0("V", seq_len(k))
      out <- cbind(
        Var = paste0("V", seq_len(k)), as.data.frame(m)
      )
      write.csv(out, file, row.names = FALSE)
    }
  )

  output$downloadCurrentMatrix <- downloadHandler(
    filename = function() paste0("correlation-matrix-", Sys.Date(), ".csv"),
    content = function(file) {
      m <- M()
      req(m)

      rn <- rownames(m)
      if (is.null(rn)) rn <- paste0("V", seq_len(nrow(m)))
      out <- cbind(Var = rn, as.data.frame(m))
      write.csv(out, file, row.names = FALSE)
    }
  )


  output$matrix <- renderTable({
    req(M())
    M()
  })


  output$codebookBtn <- renderUI({
    req(M())
    downloadButton("downloadCodebook", "Download codebook",
      style = "width:200px; margin-left:5px;"
    )
  })


  ## Dynamic inputs for variable parameters
  output$dynamicInputs <- renderUI({
    numScales <- k_scales()

    # Prefer uploaded labels, else fall back to colnames(M()), else Scale01...
    nm <- NULL
    if (identical(input$matrixSource, "upload")) {
      nm <- M_upload_names()
    }
    if (is.null(nm) || length(nm) == 0) {
      nm <- colnames(M())
    }
    if (is.null(nm) || length(nm) == 0) {
      nm <- paste0("Scale", sprintf("%02d", seq_len(numScales)))
    }

    nm <- trimws(as.character(nm))

    # Pad/truncate to exactly numScales
    if (length(nm) < numScales) {
      nm <- c(nm, paste0("Scale", sprintf("%02d", seq.int(length(nm) + 1, numScales))))
    }
    nm <- nm[seq_len(numScales)]

    # Replace blanks/NA, ensure uniqueness
    bad <- is.na(nm) | nm == ""
    if (any(bad)) {
      nm[bad] <- paste0("Scale", sprintf("%02d", which(bad)))
    }
    nm <- make.unique(nm, sep = " ")

    header <- div(
      class = "lm-header-row",
      fluidRow(
        class = "g-0",
        column(2, tags$div(class = "lm-header", "Scale")),
        column(2, tags$div(class = "lm-header", "Mean")),
        column(2, tags$div(class = "lm-header", "SD")),
        column(2, tags$div(class = "lm-header", "Lower")),
        column(2, tags$div(class = "lm-header", "Upper")),
        column(2, tags$div(class = "lm-header", "nItems"))
      )
    )

    rows <- lapply(seq_len(numScales), function(i) {
      fluidRow(
        class = "g-0",
        column(
          width = 2,
          div(
            class = "lm-cell",
            textInput(
              paste0("scaleName", i),
              label = NULL, value = nm[i], width = "100%",
              placeholder = "Scale name"
            )
          )
        ),
        column(
          width = 2,
          div(
            class = "lm-cell",
            numericInput(paste0("mean", i),
              label = NULL, value = 3, width = "100%"
            )
          )
        ),
        column(
          width = 2,
          div(
            class = "lm-cell",
            numericInput(paste0("sd", i),
              label = NULL, value = 1, width = "100%"
            )
          )
        ),
        column(
          width = 2,
          div(
            class = "lm-cell",
            numericInput(paste0("lower", i),
              label = NULL, value = 1, width = "100%"
            )
          )
        ),
        column(
          width = 2,
          div(
            class = "lm-cell",
            numericInput(paste0("upper", i),
              label = NULL, value = 5, width = "100%"
            )
          )
        ),
        column(
          width = 2,
          div(
            class = "lm-cell",
            numericInput(paste0("items", i),
              label = NULL, value = 1, min = 1, width = "100%"
            )
          )
        )
      )
    })

    div(class = "lm-scale-grid", tagList(header, rows))
  })


  # bounds for pairs plot
  bounds <- reactiveValues(lower_scale = NULL, upper_scale = NULL)

  # Synthetic data generation logic with dynamic inputs
  data_error <- reactiveVal(NULL)


  syntheticData <- eventReactive(input$generate, {
    data_error(NULL)
    k <- k_scales()

    # We'll always define *_raw so the coercion/warning code works in BOTH modes
    lower_raw <- upper_raw <- items_raw <- NULL

    if (identical(input$paramMode, "csv")) {
      dfp <- scale_params_csv()
      validate(
        need(
          !is.null(dfp), "Upload a scale-parameters CSV first."
        )
      )
      validate(
        need(
          nrow(dfp) == k, "Scale-params CSV rows do not match matrix dimension."
        )
      )

      scaleName <- as.character(dfp$scale)

      validate(
        need(
          all(nzchar(trimws(scaleName))), "Scale-params CSV: 'scale' names cannot be blank."
        )
      )

      means <- suppressWarnings(as.numeric(dfp$mean))
      sds <- suppressWarnings(as.numeric(dfp$sd))

      validate(
        need(
          length(means) == k && all(is.finite(means)), "Scale-params CSV: 'mean' must be numeric (no blanks/NA)."
        ),
        need(
          length(sds) == k && all(is.finite(sds)), "Scale-params CSV: 'sd' must be numeric (no blanks/NA)."
        )
      )


      # define raw versions for warning logic
      lower_raw <- dfp$lower
      upper_raw <- dfp$upper
      items_raw <- dfp$nItems

      lower_num <- suppressWarnings(as.numeric(lower_raw))
      upper_num <- suppressWarnings(as.numeric(upper_raw))
      items_num <- suppressWarnings(as.numeric(items_raw))

      # pre-floor guard (prevents "non-numeric argument to floor")
      validate(
        need(
          length(lower_num) == k && all(is.finite(lower_num)), "Scale-params CSV: 'lower' must be numeric (no blanks/NA)."
        ),
        need(
          length(upper_num) == k && all(is.finite(upper_num)), "Scale-params CSV: 'upper' must be numeric (no blanks/NA)."
        ),
        need(
          length(items_num) == k && all(is.finite(items_num)), "Scale-params CSV: 'nItems' must be numeric (no blanks/NA)."
        )
      )

      lower <- floor(lower_num)
      upper <- floor(upper_num)
      items_vec <- pmax(1L, floor(items_num))
    } else {
      # MANUAL mode
      scaleName <- sapply(seq_len(k), \(i) input[[paste0("scaleName", i)]])
      means <- sapply(seq_len(k), \(i) input[[paste0("mean", i)]])
      sds <- sapply(seq_len(k), \(i) input[[paste0("sd", i)]])

      lower_raw <- sapply(seq_len(k), \(i) input[[paste0("lower", i)]])
      upper_raw <- sapply(seq_len(k), \(i) input[[paste0("upper", i)]])
      items_raw <- sapply(seq_len(k), \(i) input[[paste0("items", i)]])

      lower_num <- suppressWarnings(as.numeric(lower_raw))
      upper_num <- suppressWarnings(as.numeric(upper_raw))
      items_num <- suppressWarnings(as.numeric(items_raw))

      # pre-floor guard (prevents "non-numeric argument to floor")
      validate(
        need(
          length(lower_num) == k && all(is.finite(lower_num)), "Some Lower inputs are missing or not numeric."
        ),
        need(
          length(upper_num) == k && all(is.finite(upper_num)), "Some Upper inputs are missing or not numeric."
        ),
        need(
          length(items_num) == k && all(is.finite(items_num)), "Some nItems inputs are missing or not numeric."
        )
      )

      lower <- floor(lower_num)
      upper <- floor(upper_num)
      items_vec <- pmax(1L, floor(items_num))
    }

    # One coercion warning block (works in both modes)
    if (any(suppressWarnings(as.numeric(lower_raw)) != lower, na.rm = TRUE) ||
      any(suppressWarnings(as.numeric(upper_raw)) != upper, na.rm = TRUE) ||
      any(suppressWarnings(as.numeric(items_raw)) != items_vec, na.rm = TRUE)) {
      coercion_warning("Some scale parameters (lower/upper/nItems) were rounded down to whole numbers.")
    } else {
      coercion_warning(NULL)
    }

    #  validation
    validate(
      need(
        length(means) == k && all(is.finite(means)), "Some Mean inputs are missing or not numeric."
      ),
      need(
        length(sds) == k && all(is.finite(sds)), "Some SD inputs are missing or not numeric."
      ),
      need(
        length(lower) == k && all(is.finite(lower)), "Some Lower inputs are missing or not numeric."
      ),
      need(
        length(upper) == k && all(is.finite(upper)), "Some Upper inputs are missing or not numeric."
      ),
      need(
        length(items_vec) == k && all(is.finite(items_vec)), "Some nItems inputs are missing or not numeric."
      ),
      need(
        all(sds > 0), "All SDs must be > 0."
      ),
      need(
        all(items_vec >= 1), "All nItems values must be >= 1."
      ),
      need(
        all(upper > lower), "Each upper bound must be > lower bound."
      ),
      need(
        all(means > lower & means < upper), "Means must lie between lower and upper bounds."
      )
    )

    m0 <- M()
    validate(
      need(
        is_valid_matrix(m0), "Please generate or upload a valid correlation matrix first."
      )
    )

    # Re-validate/re-repair using CURRENT UI settings (symFix / nearPD)
    v <- validate_matrix_upload(
      df_or_m = m0,
      sym_fix = input$symFix,
      use_near_pd = isTRUE(input$useNearPD)
    )
    validate(need(isTRUE(v$ok), v$msg))

    m <- v$m


    # ensure symmetry checks don't fail because of dimnames
    dimnames(m) <- NULL

    tryCatch(
      {
        dat <- makeScales(
          n = input$n,
          means = means, sds = sds,
          lowerbound = lower, upperbound = upper,
          items = items_vec,
          cormatrix = m
        )

        scale_prefix <- safe_name(scaleName)

        if (ncol(dat) != length(scale_prefix)) {
          stop(sprintf(
            "Internal check failed: ncol(dat) = %d but length(scaleName) = %d",
            ncol(dat), length(scale_prefix)
          ))
        }

        colnames(dat) <- scale_prefix
        bounds$lower_scale <- lower
        bounds$upper_scale <- upper
        dat
      },
      error = function(e) {
        data_error(e$message)
        NULL
      }
    )
  })


  output$syntheticData <- renderDT({
    if (!is.null(data_error())) {
      return(DT::datatable(data.frame(Error = data_error())))
    }
    req(syntheticData())
    DT::datatable(syntheticData(), options = list(pageLength = 10))
  })


  # ---- Data validation: always reflect latest synthetic data ----
  plot_df <- reactive({
    req(syntheticData())
    syntheticData()
  })


  # Calculate and display the eigenvalues of generated dataframe
  output$eigenSummary <- renderPrint({
    s <- stats_cache()
    req(!is.null(s))
    if (is.null(s$eigen_vals)) {
      cat("—")
    } else {
      cat(paste(s$eigen_vals, collapse = ", "))
    }
  })


  STYLE <- list(
    point_colour = "steelblue",
    line_colour = "firebrick",
    point_alpha = 0.6,
    point_size = 1.5,
    line_size = 1.2,
    jitter_w = 0.2,
    jitter_h = 0.2,
    cor_digits = 2,
    cor_text_size = 6,
    plot_base_size = 14,
    fill_colour = "lightblue"
  )


  custom_smooth <- function(data, mapping, ...) {
    ggplot(data = data, mapping = mapping) +
      geom_point(
        alpha = STYLE$point_alpha,
        color = STYLE$point_colour,
        position = position_jitter(
          width = STYLE$jitter_w,
          height = STYLE$jitter_h
        ),
        size = STYLE$point_size
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE,
        color = STYLE$line_colour,
        linewidth = STYLE$line_size
      )
  }

  cor_only <- function(data, mapping, method = "pearson",
                       digits = STYLE$cor_digits, ...) {
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)
    corr <- cor(x, y, method = method, use = "pairwise.complete.obs")
    label <- formatC(corr, format = "f", digits = digits)

    ggally_text(
      label = label,
      mapping = aes(),
      xP = 0.5, yP = 0.5,
      size = 6, ...
    ) +
      theme_void()
  }


  custom_bar_diag <- function(data, mapping, ...) {
    ggplot(data, mapping) +
      geom_bar(color = "black", fill = STYLE$fill_colour)
  }


  generatePairsPlot <- function(data) {
    ggpairs(
      data,
      lower = list(continuous = custom_smooth),
      diag = list(continuous = custom_bar_diag),
      upper = list(continuous = wrap(cor_only, digits = 2))
    ) +
      theme_minimal(base_size = 14) + # changes default sizes
      theme(
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.line = element_line(color = "black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()
      )
  }

  # Calculate and display means and standard deviations
  output$dataSummary <- renderTable(
    {
      s <- stats_cache()
      req(!is.null(s))
      if (!is.null(s$note)) {
        return(data.frame(Message = s$note, check.names = FALSE))
      }
      if (is.null(s$means) || is.null(s$sds)) {
        return(data.frame(Message = "No summary statistics available.", check.names = FALSE))
      }

      tab <- rbind(
        Mean = s$means,
        SD   = s$sds,
        Min  = s$mins,
        Max  = s$maxs
      )
      as.data.frame(tab, check.names = FALSE)
    },
    rownames = TRUE
  )


  # Calculate and display Cronbach's Alpha
  output$cronbachAlphaOutput <- renderText({
    s <- stats_cache()
    req(!is.null(s))
    if (is.na(s$alpha)) {
      return("—")
    }
    sprintf("%.3f", s$alpha)
  })


  # Ensure key validation summaries compute even when the tab is hidden
  outputOptions(output, "dataSummary", suspendWhenHidden = FALSE)
  outputOptions(output, "cronbachAlphaOutput", suspendWhenHidden = FALSE)
  outputOptions(output, "eigenSummary", suspendWhenHidden = FALSE)

  # Render plot on-demand (only after user visits Validation), and cache it until data change


  output$dataVis <- renderPlot({
    req(syntheticData())

    # Only build the heavy plot when the user is actually on the Validation tab
    if (!isTRUE(validation_seen())) {
      plot.new()
      title(main = "Visual summary not built yet", cex.main = 1.1)
      text(0.5, 0.55, "Open the Data validation tab to build the pairs plot", cex = 1.0)
      text(0.5, 0.45, "Summary statistics are shown below the plot.", cex = 0.9)
      return(invisible())
    }

    # If a build is already in progress, show a friendly placeholder
    if (isTRUE(plot_building())) {
      plot.new()
      title(main = "Building visual summary…", cex.main = 1.1)
      text(0.5, 0.5, "Please wait — this can take a moment for large matrices.", cex = 0.95)
      return(invisible())
    }

    # Key for the *current* dataset
    key <- tryCatch(data_key(), error = function(e) NULL)
    req(!is.null(key))


    # If we already have a plot for this dataset, just redraw it (no recompute)
    if (!is.null(plotStorage()) && identical(plot_key(), key)) {
      print(plotStorage())
      return(invisible())
    }

    # Otherwise build it now
    withProgress(message = "Preparing visual summary (pairs plot)...", value = 0, {
      plot_building(TRUE)
      on.exit(plot_building(FALSE), add = TRUE)

      p <- generatePairsPlot(syntheticData())
      plotStorage(p)
      plot_key(key)
      print(p)
    })
  })

  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("LikertMakeR_pairs_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(plotStorage())
      key <- plot_key()
      req(!is.null(key))
      # Target image size proportional to number of scales (keeps text sizing consistent)
      k <- k_scales()
      w <- max(800, k * 200)
      h <- max(800, k * 200)
      res <- 144

      # If we already have a PNG for this dataset (and at the current requested size), copy it instantly
      want_wh <- c(as.integer(w), as.integer(h), as.integer(res))
      have_wh <- plotPngWH()
      ok_size <- !is.null(have_wh) && identical(as.integer(have_wh), want_wh)

      if (!is.null(plotPngPath()) &&
        identical(plotPngKey(), key) &&
        file.exists(plotPngPath()) &&
        ok_size) {
        file.copy(plotPngPath(), file, overwrite = TRUE)
        return(invisible())
      }

      # Fallback: build PNG once (still no recompute of ggpairs)

      ragg::agg_png(file, width = w, height = h, res = res, scaling = 1, background = "white")
      on.exit(dev.off(), add = TRUE)
      print(plotStorage())

      # Also cache it for next time
      tmp_png <- tempfile(fileext = ".png")
      file.copy(file, tmp_png, overwrite = TRUE)
      plotPngPath(tmp_png)
      plotPngKey(key)
      plotPngWH(c(w, h, res))
    }
  )

  output$downloadDataBtn <- renderUI({
    if (is.null(syntheticData())) {
      bslib::tooltip(
        disable_ui(
          downloadButton("downloadData", "Download as CSV",
            style = "width:200px; margin-left:5px;"
          )
        ),
        "Generate data first."
      )
    } else {
      downloadButton("downloadData", "Download as CSV",
        style = "width:200px; margin-left:5px;"
      )
    }
  })

  # Add download handler for downloading the synthetic data
  output$downloadData <- downloadHandler(
    filename = function() paste0("likertmaker-", Sys.Date(), ".csv"),
    content = function(file) {
      req(syntheticData()) # <-- guard
      write.csv(syntheticData(), file, row.names = FALSE)
    }
  )

  codebook_df <- reactive({
    req(M())
    numScales <- ncol(M())

    scaleName <- sapply(seq_len(numScales), \(i) input[[paste0("scaleName", i)]])

    fmt <- function(x) {
      formatC(x, format = "f", digits = 3, big.mark = ",", decimal.mark = ".")
    }

    data.frame(
      scale = safe_name(scaleName),
      mean = fmt(sapply(seq_len(numScales), \(i) input[[paste0("mean", i)]])),
      sd = fmt(sapply(seq_len(numScales), \(i) input[[paste0("sd", i)]])),
      lower = fmt(sapply(seq_len(numScales), \(i) input[[paste0("lower", i)]])),
      upper = fmt(sapply(seq_len(numScales), \(i) input[[paste0("upper", i)]])),
      items = sapply(seq_len(numScales), \(i) input[[paste0("items", i)]]),
      stringsAsFactors = FALSE
    )
  })

  output$downloadCodebook <- downloadHandler(
    filename = function() paste0("likertmaker-codebook-", Sys.Date(), ".csv"),
    content = function(file) {
      req(M())
      write.csv(codebook_df(), file, row.names = FALSE)
    }
  )
}
