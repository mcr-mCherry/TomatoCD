source("global.R")
source("config.R")
library(shiny)
library(dplyr)
library(DT)
library(stringr)
library(jsonlite)
library(httr)

if (USE_PROXY) {
  httr::set_config(httr::use_proxy(PROXY_HOST, PROXY_PORT))
}

# Server logic
server <- function(input, output, session) {
  
  # Function to generate data URL (global scope)
  get_hf_url <- function(path) {
    if (USE_EXTERNAL_URL) {
      # Use external URL
      return(paste0(EXTERNAL_URLS$jbrowse2, "/data/", path))
    } else if (HUGGINGFACE_CONFIG$use_huggingface) {
      paste0(HUGGINGFACE_CONFIG$base_url, "/jbrowse2/data/", path)
    } else {
      paste0("data/", path)
    }
  }
  
  # Function to get file path - supports external URL or local path
  get_file_path <- function(folder_type, relative_path) {
    if (USE_EXTERNAL_URL && !is.null(EXTERNAL_URLS[[folder_type]])) {
      return(paste0(EXTERNAL_URLS[[folder_type]], "/", relative_path))
    } else {
      local_base <- switch(folder_type,
                          data = file.path(BASE_DIR, "data"),
                          jbrowse2 = file.path(BASE_DIR, "jbrowse2"),
                          www = WWW_DIR,
                          gene_search = file.path(BASE_DIR, "gene_search"),
                          file.path(BASE_DIR, folder_type))
      return(file.path(local_base, relative_path))
    }
  }
  
  # Function to read data file from URL or local path
  read_from_url <- function(url, sep = "\t", header = TRUE, stringsAsFactors = FALSE, ...) {
    tryCatch({
      if (grepl("^https?://", url)) {
        response <- httr::GET(url, httr::timeout(120))
        if (httr::status_code(response) == 200) {
          temp_file <- tempfile(fileext = ".txt")
          writeBin(httr::content(response, as = "raw"), temp_file)
          data <- read.delim(temp_file, sep = sep, header = header, stringsAsFactors = stringsAsFactors, ...)
          unlink(temp_file)
          return(data)
        } else {
          warning(paste("Failed to download file from:", url, "Status code:", httr::status_code(response)))
          return(data.frame())
        }
      } else {
        local_path <- file.path(BASE_DIR, url)
        if (!file.exists(local_path)) {
          www_path <- file.path(BASE_DIR, "www", sub("^/+", "", url))
          if (file.exists(www_path)) {
            local_path <- www_path
          }
        }
        data <- read.delim(local_path, sep = sep, header = header, stringsAsFactors = stringsAsFactors, ...)
        return(data)
      }
    }, error = function(e) {
      warning(paste("Error reading file from:", url, "Error:", e))
      return(data.frame())
    })
  }

  read_cached_file <- function(cache_key, folder_type, relative_path, sep = "\t", header = TRUE, stringsAsFactors = FALSE, ...) {
    get_cached_data(cache_key, function() {
      read_from_url(
        get_file_path(folder_type, relative_path),
        sep = sep,
        header = header,
        stringsAsFactors = stringsAsFactors,
        ...
      )
    })
  }

  get_network_edges_data <- function(network_type) {
    if (identical(network_type, "physical")) {
      read_cached_file(
        "physical_network_edges",
        "www",
        "physical_network/physcial_network_edges.csv",
        sep = ",",
        header = TRUE,
        stringsAsFactors = FALSE
      )
    } else {
      read_cached_file(
        "cor_network_edges",
        "www",
        "cor_network/cor_network_edges.csv",
        sep = ",",
        header = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }

  get_network_nodes_data <- function(network_type) {
    if (identical(network_type, "physical")) {
      read_cached_file(
        "physical_network_nodes",
        "www",
        "physical_network/physcial_network_nodes.csv",
        sep = ",",
        header = TRUE,
        stringsAsFactors = FALSE
      )
    } else {
      read_cached_file(
        "cor_network_nodes",
        "www",
        "cor_network/cor_network_nodes.csv",
        sep = ",",
        header = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }

  get_network_subtable_source <- function(network_type) {
    if (identical(network_type, "physical")) {
      read_cached_file(
        "physical_network_subtable",
        "www",
        "physical_network/subnetwork_details_total.tsv",
        sep = "\t",
        header = TRUE,
        stringsAsFactors = FALSE
      )
    } else {
      read_cached_file(
        "cor_network_subtable",
        "www",
        "cor_network/subnetwork_details_total.tsv",
        sep = "\t",
        header = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }

  normalize_gene_value <- function(gene) {
    if (is.null(gene) || !nzchar(trimws(gene))) {
      return("")
    }

    gene <- trimws(gene)
    if (grepl(" ", gene, fixed = TRUE)) {
      gene <- strsplit(gene, " ", fixed = TRUE)[[1]][1]
    }
    if (grepl("%20", gene, fixed = TRUE)) {
      gene <- strsplit(gene, "%20", fixed = TRUE)[[1]][1]
    }

    gene
  }

  get_filtered_network_edges <- function(network_type, gene) {
    normalized_gene <- normalize_gene_value(gene)
    if (!nzchar(normalized_gene)) {
      return(data.frame(
        `Source ID` = character(0),
        `Source CommonName` = character(0),
        `Target ID` = character(0),
        `Target CommonName` = character(0),
        Weight = numeric(0),
        `Target Gene Description` = character(0),
        stringsAsFactors = FALSE
      ))
    }

    cache_key <- paste0("network_edges_filtered::", network_type, "::", normalized_gene)
    get_cached_data(cache_key, function() {
      if (identical(network_type, "physical")) {
        df <- get_network_subtable_source("physical")
        df$Weight <- suppressWarnings(as.numeric(df$Weight))
        df <- df[!is.na(df$Weight), ]
        filtered_df <- df[df$Source.ID == normalized_gene | df$Target.ID == normalized_gene, ]

        if (nrow(filtered_df) > 0) {
          data.frame(
            `Source ID` = filtered_df$Source.ID,
            `Source CommonName` = filtered_df$Source.CommonName,
            `Target ID` = filtered_df$Target.ID,
            `Target CommonName` = filtered_df$Target.CommonName,
            Weight = filtered_df$Weight,
            `Target Gene Description` = filtered_df$Target.Gene.Description,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            `Source ID` = character(0),
            `Source CommonName` = character(0),
            `Target ID` = character(0),
            `Target CommonName` = character(0),
            Weight = numeric(0),
            `Target Gene Description` = character(0),
            stringsAsFactors = FALSE
          )
        }
      } else {
        df <- get_network_edges_data("cor")
        df$Weight <- suppressWarnings(as.numeric(df$Weight))
        df <- df[!is.na(df$Weight), ]
        filtered_df <- df[df$Source == normalized_gene | df$Target == normalized_gene, ]

        if (nrow(filtered_df) > 0) {
          data.frame(
            `Source ID` = filtered_df$Source,
            `Source CommonName` = filtered_df$Source,
            `Target ID` = filtered_df$Target,
            `Target CommonName` = filtered_df$Target,
            Weight = filtered_df$Weight,
            `Target Gene Description` = "",
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            `Source ID` = character(0),
            `Source CommonName` = character(0),
            `Target ID` = character(0),
            `Target CommonName` = character(0),
            Weight = numeric(0),
            `Target Gene Description` = character(0),
            stringsAsFactors = FALSE
          )
        }
      }
    })
  }

  get_network_subtable_filtered <- function(network_type, gene) {
    normalized_gene <- normalize_gene_value(gene)
    if (!nzchar(normalized_gene)) {
      return(data.frame(
        `Source ID` = character(0), `Source CommonName` = character(0),
        `Target ID` = character(0), `Target CommonName` = character(0),
        Weight = numeric(0), `Target Gene Description` = character(0),
        stringsAsFactors = FALSE
      ))
    }

    cache_key <- paste0("network_subtable_filtered::", network_type, "::", normalized_gene)
    get_cached_data(cache_key, function() {
      empty_df <- data.frame(
        `Source ID` = character(0), `Source CommonName` = character(0),
        `Target ID` = character(0), `Target CommonName` = character(0),
        Weight = numeric(0), `Target Gene Description` = character(0),
        stringsAsFactors = FALSE
      )

      if (identical(network_type, "physical")) {
        df <- get_network_subtable_source("physical")
      } else {
        df <- get_network_subtable_source("cor")
      }

      filtered_df <- df[df$Source.ID == normalized_gene | df$Target.ID == normalized_gene, ]
      if (nrow(filtered_df) == 0) {
        return(empty_df)
      }

      colnames(filtered_df) <- c("Source ID", "Source CommonName", "Target ID", "Target CommonName", "Weight", "Target Gene Description")
      filtered_df
    })
  }

  asset_version <- as.integer(Sys.time())
  
  # Navigation link click event handlers
  observeEvent(input$home_link, {
    # Show home page content, hide other pages
    updateTextInput(session, "currentPage", value = "home")
    # Clear selected_gene value
    updateTextInput(session, "selected_gene", value = "")
  })

  observeEvent(input$help_link, {
    session$sendCustomMessage(
      "triggerExternalDownload",
      list(url = paste0("/help/index.html?t=", as.integer(Sys.time())))
    )
  })
  
  observeEvent(input$module_motif, {
    # Show Motif search page, hide home page content
    updateTextInput(session, "currentPage", value = "motif")
    # Force trigger data loading
    updateTextInput(session, "motif_search", value = " ")
    updateTextInput(session, "motif_search", value = "")
  })
  
  # Module card click event handlers
  observeEvent(input$network_module, {
    # Clear selected_gene value
    updateTextInput(session, "selected_gene", value = "")
    # Navigate to Network page
    updateTextInput(session, "currentPage", value = "network")
  })
  
  observeEvent(input$protein_binding_module, {
    updateTextInput(session, "currentPage", value = "jbrowse_visualization")
  })
  
  observeEvent(input$motif_module, {
    # Show Motif search page, hide home page content
    updateTextInput(session, "currentPage", value = "motif")
    # Force trigger data loading
    updateTextInput(session, "motif_search", value = " ")
    updateTextInput(session, "motif_search", value = "")
  })
  
  observeEvent(input$mc_preference_module, {
    # Navigate to Methylation sensitivity page
    updateTextInput(session, "currentPage", value = "mc_preference")
    # Force trigger data loading
    updateTextInput(session, "mc_preference_search", value = " ")
    updateTextInput(session, "mc_preference_search", value = "")
  })
  
  observeEvent(input$gene_annotation_module, {
    # Show Gene search page, hide home page content
    updateTextInput(session, "currentPage", value = "gene_search")
    # Force trigger data loading
    updateTextInput(session, "gene_search_input", value = " ")
    updateTextInput(session, "gene_search_input", value = "")
  })
  
  observeEvent(input$expression_module, {
    # Navigate to Metabolic Node Sub-Graphs page
    updateTextInput(session, "currentPage", value = "metabolic_subgraphs")
  })
  
  # Handle Gene Search module card click event
  observeEvent(input$gene_search_module, {
    # Show Gene search page, hide home page content
    updateTextInput(session, "currentPage", value = "gene_search")
    # Force trigger data loading
    updateTextInput(session, "gene_search_input", value = " ")
    updateTextInput(session, "gene_search_input", value = "")
  })
  
  # Other module click event handlers
  observeEvent(input$module_network, {
    # Clear selected_gene value
    updateTextInput(session, "selected_gene", value = "")
    # Navigate to Network page
    updateTextInput(session, "currentPage", value = "network")
  })
  
  observeEvent(input$module_protein_binding, {
    updateTextInput(session, "currentPage", value = "jbrowse_visualization")
  })
  
  observeEvent(input$module_mc_preference, {
    # Navigate to Methylation sensitivity page
    updateTextInput(session, "currentPage", value = "mc_preference")
    # Force trigger data loading
    updateTextInput(session, "mc_preference_search", value = " ")
    updateTextInput(session, "mc_preference_search", value = "")
  })
  
  observeEvent(input$module_gene_annotation, {
    # Show Gene search page, hide home page content
    updateTextInput(session, "currentPage", value = "gene_search")
    # Force trigger data loading
    updateTextInput(session, "gene_search_input", value = " ")
    updateTextInput(session, "gene_search_input", value = "")
  })
  
  # Handle Gene Search module click event
  observeEvent(input$module_gene_search, {
    # Show Gene search page, hide home page content
    updateTextInput(session, "currentPage", value = "gene_search")
    # Force trigger data loading
    updateTextInput(session, "gene_search_input", value = " ")
    updateTextInput(session, "gene_search_input", value = "")
  })
  
  observeEvent(input$module_expression_analysis, {
    # Navigate to Metabolic Node Sub-Graphs page
    updateTextInput(session, "currentPage", value = "metabolic_subgraphs")
  })
  
  observeEvent(input$module_metabolic_subgraphs, {
    # Navigate to Metabolic Node Sub-Graphs page
    updateTextInput(session, "currentPage", value = "metabolic_subgraphs")
    # Default to Ethylene pathway
    updateRadioButtons(session, "pathway_type", selected = "ethylene")
  })
  
  # Handle Metabolic Node Sub-Graphs module card click event
  observeEvent(input$metabolic_subgraphs_module, {
    # Navigate to Metabolic Node Sub-Graphs page
    updateTextInput(session, "currentPage", value = "metabolic_subgraphs")
    # Default to Ethylene pathway
    updateRadioButtons(session, "pathway_type", selected = "ethylene")
  })
  
  # Tools dropdown menu click event handler
  observeEvent(input$tools_blast, {
    showNotification("BLAST tool will be implemented soon!", type = "message")
  })
  
  observeEvent(input$tools_kegg_enrichment, {
    # Show KEGG Enrichment page
    updateTextInput(session, "currentPage", value = "kegg_enrichment")
  })
  
  observeEvent(input$tools_go_enrichment, {
    # Show GO Enrichment page
    updateTextInput(session, "currentPage", value = "go_enrichment")
  })
  
  observeEvent(input$tools_metabolic, {
    updateTextInput(session, "currentPage", value = "metabolic_subgraphs")
    updateRadioButtons(session, "pathway_type", selected = "ethylene")
  })
  
  # Navbar tab switch handler
  observeEvent(input$navbarMenu, {
    cat("Navbar menu item selected:", input$navbarMenu, "\n")
  })
  
  # Motif module: dynamically update search box placeholder
  observeEvent(input$motif_search_column, {
    if (input$motif_search_column == "All") {
      updateTextInput(session, "motif_search", placeholder = "Search by Transcription Factor, GeneID, Method, etc.")
    } else {
      updateTextInput(session, "motif_search", placeholder = paste("Search by", input$motif_search_column))
    }
  })
  
  # Methylation Sensitivity module: dynamically update search box placeholder
  observeEvent(input$mc_preference_search_column, {
    if (input$mc_preference_search_column == "All") {
      updateTextInput(session, "mc_preference_search", placeholder = "Search by CommonName, GeneID, Family, etc.")
    } else {
      updateTextInput(session, "mc_preference_search", placeholder = paste("Search by", input$mc_preference_search_column))
    }
  })
  
  # Gene Search module: dynamically update search box placeholder
  observeEvent(input$gene_search_column, {
    if (input$gene_search_column == "All") {
      updateTextInput(session, "gene_search_input", placeholder = "Search by TF CommonName, Target Gene ID, Sequence, etc.")
    } else {
      updateTextInput(session, "gene_search_input", placeholder = paste("Search by", input$gene_search_column))
    }
  })

  # Peak-based Methylation Sensitivity module: dynamically update search box placeholder
  observeEvent(input$mc_peak_search_column, {
    if (input$mc_peak_search_column == "All") {
      updateTextInput(session, "mc_peak_search", placeholder = "Search by TFid, motifchr, etc.")
    } else {
      updateTextInput(session, "mc_peak_search", placeholder = paste("Search by", input$mc_peak_search_column))
    }
  })

  # Network module: dynamically update search box placeholder
  observeEvent(input$network_search_column, {
    if (input$network_search_column == "All") {
      updateTextInput(session, "network_search", placeholder = "Search by Source ID, Target ID, etc.")
    } else {
      updateTextInput(session, "network_search", placeholder = paste("Search by", input$network_search_column))
    }
  })
  
  # Read Motif data
  motif_data <- reactive({
    get_cached_data("motif_data", function() {
      if (USE_EXTERNAL_URL) {
        # Use external URL
        url <- get_file_path("data", "web_pwm_seqlogo/total_unique_index.txt")
        return(read_from_url(url, sep = "", stringsAsFactors = FALSE, header = TRUE))
      } else if (HUGGINGFACE_CONFIG$use_huggingface) {
        url <- paste0(HUGGINGFACE_CONFIG$base_url, "/web_pwm_seqlogo/total_unique_index.txt")
        
        response <- httr::GET(
          url, 
          httr::timeout(HUGGINGFACE_CONFIG$timeout)
        )
        
        if (httr::status_code(response) != 200) {
          stop(paste("Failed to download:", url))
        }
        
        # Save to temporary file
        temp_file <- tempfile(fileext = ".txt")
        writeBin(httr::content(response, as = "raw"), temp_file)
        
        data <- read.delim(temp_file, 
                       sep = "", 
                       stringsAsFactors = FALSE, 
                       header = TRUE)
        unlink(temp_file)
        return(data)
      } else {
        read.delim(file.path(BASE_DIR, "data", "web_pwm_seqlogo", "total_unique_index.txt"), 
                   sep = "",  # Empty string means any whitespace character as delimiter
                   stringsAsFactors = FALSE, 
                   header = TRUE)
      }
    })
  })
  
  # Read Sum_Pattern_Seqlets data
  sum_seqlets_data <- reactive({
    get_cached_data("sum_seqlets_data", function() {
      if (USE_EXTERNAL_URL) {
        # Use external URL
        url <- get_file_path("data", "web_pwm_seqlogo/Sum_Pattern_Seqlets.txt")
        data <- read_from_url(url, sep = "", stringsAsFactors = FALSE, header = TRUE)
        if (ncol(data) == 3) {
          colnames(data) <- c("index", "pattern", "n-seqlets")
        }
        return(data)
      } else if (HUGGINGFACE_CONFIG$use_huggingface) {
        url <- paste0(HUGGINGFACE_CONFIG$base_url, "/web_pwm_seqlogo/Sum_Pattern_Seqlets.txt")
        
        response <- httr::GET(url, httr::timeout(HUGGINGFACE_CONFIG$timeout))
        
        if (httr::status_code(response) != 200) {
          stop(paste("Failed to download:", url))
        }
        
        # Save to temporary file
        temp_file <- tempfile(fileext = ".txt")
        writeBin(httr::content(response, as = "raw"), temp_file)
        
        data <- read.delim(temp_file, 
                           sep = "", 
                           stringsAsFactors = FALSE, 
                           header = TRUE)
        unlink(temp_file)
        
        if (ncol(data) == 3) {
          colnames(data) <- c("index", "pattern", "n-seqlets")
        }
        return(data)
      } else {
        tryCatch({
          data <- read.delim(file.path(BASE_DIR, "data", "web_pwm_seqlogo", "Sum_Pattern_Seqlets.txt"), 
                             sep = "",  # Empty string means any whitespace character as delimiter
                             stringsAsFactors = FALSE, 
                             header = TRUE)
          
          if (ncol(data) == 3) {
            colnames(data) <- c("index", "pattern", "n-seqlets")
          }
          return(data)
        }, error = function(e) {
          print(paste("Error reading Sum_Pattern_Seqlets.txt:", e))
          return(data.frame(index = character(), 
                        pattern = character(), 
                        "n-seqlets" = numeric(), 
                        stringsAsFactors = FALSE))
        })
      }
    })
  })
  
  # Process user input
  user_input <- reactive({
    # Prioritize file upload
    if (!is.null(input$motif_file)) {
      file_path <- input$motif_file$datapath
      # Read file content, handle different file types
      if (endsWith(input$motif_file$name, ".csv")) {
        data <- read.csv(file_path, stringsAsFactors = FALSE)
        # Assume the first column is input data
        return(unique(trimws(as.character(data[[1]]))))
      } else {
        # Text file, one per line
        lines <- readLines(file_path, warn = FALSE)
        return(unique(trimws(lines[lines != ""])))
      }
    }
    
    # Then use the multi-line input box
    if (input$motif_input != "") {
      lines <- str_split(input$motif_input, "\n")[[1]]
      return(unique(trimws(lines[lines != ""])))
    }
    
    # Finally use the search box
    if (input$motif_search != "") {
      return(trimws(input$motif_search))
    }
    
    # If no input provided, return empty vector
    return(character(0))
  })
  
  generate_embed_code <- function(file_name, folder_name, bpnet_urls = NULL, traditional_urls = NULL) {
    file_ext <- tools::file_ext(file_name)
    
    file_path <- paste0('motif_images/', folder_name, '/', file_name)
    
    if (file_ext == "pdf") {
      paste0('<div class="pdf-wrapper"><embed src="', file_path, '#view=FitH&toolbar=0" type="application/pdf" style="width: 280px; height: 37px;"></div>')
    } else {
      paste0('<img src="', file_path, '" style="width: 100%; height: 100%; object-fit: contain; display: block; margin: 0 auto;">')
    }
  }
  
  # Cache folder list to avoid repeated reads
  cached_folders <- reactiveVal(NULL)
  
  # Get all folder list (with caching)
  get_folders <- reactive({
    if (is.null(cached_folders())) {
      if (USE_EXTERNAL_URL) {
        # When using external URL, cannot directly list remote directory
        # Need to predefine folder list or get from other data sources
        # Here we extract all CommonNames from motif_data as folder list
        data <- motif_data()
        if (nrow(data) > 0 && "CommonName" %in% colnames(data)) {
          folders <- unique(as.character(data$CommonName))
        } else {
          folders <- character(0)
        }
        cached_folders(folders)
      } else {
        folder_path <- file.path(BASE_DIR, "data", "web_pwm_seqlogo")
        folders <- list.files(folder_path)
        cached_folders(folders)
      }
    }
    return(cached_folders())
  })
  
  motif_results <- reactive({
    get_cached_data("motif_results_table", function() {
      tsv_data <- read_cached_file(
        "motif_summary_patterns",
        "data",
        "motif_summary_patterns.tsv",
        sep = "\t",
        header = TRUE,
        stringsAsFactors = FALSE
      )

      if (is.null(tsv_data) || nrow(tsv_data) == 0) {
        return(data.frame(
          `Transcription Factor` = character(),
          GeneID = character(),
          Method = character(),
          Pattern = character(),
          `Seqlets Number` = numeric(),
          `PWM Motif Seqlogo Fwd` = character(),
          `PWM Motif Seqlogo RC` = character(),
          stringsAsFactors = FALSE,
          check.names = FALSE
        ))
      }

      results <- list()
      for (i in 1:nrow(tsv_data)) {
        row <- tsv_data[i, ]
        tf <- as.character(row$Transcription_Factor)
        gene_id <- as.character(row$GeneID)
        method <- as.character(row$Method)
        pattern <- as.character(row$Pattern)
        seqlets <- suppressWarnings(as.numeric(row$Seqlets_Number))

        if (method == "BPNet") {
          p_num <- gsub("[^0-9]", "", pattern)
          if (!nzchar(p_num)) next
          fwd_pdf <- paste0(tf, "_BPNet_pattern_", p_num, "_fwd.pdf")
          rc_pdf <- paste0(tf, "_BPNet_pattern_", p_num, "_rc.pdf")
        } else {
          fwd_pdf <- paste0(tf, "_Traditional_fwd.pdf")
          rc_pdf <- paste0(tf, "_Traditional_rc.pdf")
        }

        formatted_pattern <- if (nzchar(pattern)) {
          gsub("_", " ", pattern)
        } else {
          ""
        }

        new_row <- data.frame(
          `Transcription Factor` = tf,
          GeneID = gene_id,
          Method = method,
          Pattern = formatted_pattern,
          `Seqlets Number` = seqlets,
          `PWM Motif Seqlogo Fwd` = as.character(generate_embed_code(fwd_pdf, tf)),
          `PWM Motif Seqlogo RC` = as.character(generate_embed_code(rc_pdf, tf)),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        results[[length(results) + 1]] <- new_row
      }

      if (length(results) > 0) {
        do.call(rbind, results)
      } else {
        data.frame(
          `Transcription Factor` = character(),
          GeneID = character(),
          Method = character(),
          Pattern = character(),
          `Seqlets Number` = numeric(),
          `PWM Motif Seqlogo Fwd` = character(),
          `PWM Motif Seqlogo RC` = character(),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    })
  })
  
  # Filter Motif data based on search box and selected column
  filtered_motif_data <- reactive({
    data <- motif_results()
    search_term <- input$motif_search
    search_column <- input$motif_search_column
    
    # If search box is empty, return all data
    if (is.null(search_term) || search_term == "") {
      return(data)
    }
    
    # Search specified column or all columns
    search_lower <- tolower(search_term)
    
    if (search_column == "All" || search_column == "") {
      # Search all columns (excluding image columns)
      filtered_rows <- apply(data[, !colnames(data) %in% c("PWM Motif Seqlogo Fwd", "PWM Motif Seqlogo RC"), drop = FALSE], 1, function(row) {
        any(grepl(search_lower, tolower(as.character(row)), fixed = TRUE))
      })
    } else {
      # Search specified column only
      if (search_column %in% colnames(data)) {
        filtered_rows <- grepl(search_lower, tolower(as.character(data[[search_column]])), fixed = TRUE)
      } else {
        filtered_rows <- logical(nrow(data))
      }
    }
    
    return(data[filtered_rows, , drop = FALSE])
  })
  
  # Render Motif data table
  output$motif_results <- DT::renderDataTable({
    req(input$currentPage == "motif")
    data <- filtered_motif_data()
    
    # If no data, return empty table
    if (nrow(data) == 0) {
      return(DT::datatable(
        data.frame(
          `Transcription Factor` = character(),
          GeneID = character(),
          Method = character(),
          Pattern = character(),
          `Seqlets Number` = numeric(),
          `PWM Motif Seqlogo Fwd` = character(),
          `PWM Motif Seqlogo RC` = character(),
          stringsAsFactors = FALSE,
          check.names = FALSE
        ),
        options = list(
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = FALSE,
          searching = FALSE,
          autoWidth = FALSE,
          columnDefs = list(
            list(targets = c(0), width = '120px'),  # Column 0: Transcription Factor (with checkbox)
            list(targets = c(1), width = '120px'),  # Column 1: GeneID
            list(targets = c(2), width = '80px'),   # Column 2: Method
            list(targets = c(3), width = '100px'),  # Column 3: Pattern
            list(targets = c(4), width = '20px'),   # Column 4: Seqlets Number
            list(targets = c(5), width = '280px'),  # Column 5: PWM Motif Seqlogo Fwd
            list(targets = c(6), width = '280px')   # Column 6: PWM Motif Seqlogo RC (same width as Fwd)
          ),
          rowCallback = htmlwidgets::JS("function(row, data, index) {
            // Set row height
            $(row).css('height', '47px');
            $(row).css('min-height', '47px');
            $(row).find('td').css('vertical-align', 'middle');
          }")
        ),
        escape = FALSE,
        filter = 'none',
        rownames = FALSE,
        selection = 'none'
      ))
    }
    
    # Add checkbox before Transcription Factor column (flex layout: 1/4 checkbox, 3/4 text)
    data$`Transcription Factor` <- paste0(
      '<div style="display: flex; align-items: center; width: 100%;">',
      '<div style="width: 25%; text-align: center; padding: 2px;"><input type="checkbox" class="motif-checkbox" style="width: 16px; height: 16px; cursor: pointer;" onclick="updateMotifSelection(this)"/></div>',
      '<div style="width: 75%; padding-left: 5px;">', data$`Transcription Factor`, '</div>',
      '</div>'
    )
    
    # Render data table
    DT::datatable(
      data,
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = FALSE,
        searching = FALSE,
        autoWidth = FALSE,
        columnDefs = list(
          list(targets = c(0), width = '120px'),  # Column 0: Transcription Factor (with checkbox)
          list(targets = c(1), width = '120px'),  # Column 1: GeneID
          list(targets = c(2), width = '80px'),   # Column 2: Method
          list(targets = c(3), width = '100px'),  # Column 3: Pattern
          list(targets = c(4), width = '20px'),   # Column 4: Seqlets Number
          list(targets = c(5), width = '280px'),  # Column 5: PWM Motif Seqlogo Fwd
          list(targets = c(6), width = '280px')   # Column 6: PWM Motif Seqlogo RC (same width as Fwd)
        ),
        rowCallback = htmlwidgets::JS("function(row, data, index) {
          // Set row height
          $(row).css('height', '47px');
          $(row).css('min-height', '47px');
          $(row).find('td').css('vertical-align', 'middle');
        }"),
        initComplete = htmlwidgets::JS("function(settings, json) {
          // Define global function to handle checkbox selection
          window.updateMotifSelection = function(checkbox) {
            var table = $(checkbox).closest('table').DataTable();
            var selectedCount = table.$('input.motif-checkbox:checked').length;
            var countEl = document.getElementById('motif_selected_count');
            if (countEl) {
              countEl.textContent = selectedCount.toLocaleString();
            }
          };
        }")
      ),
      escape = FALSE,
      filter = 'none',
      rownames = FALSE,
      selection = 'none'
    )
  }, server = TRUE)
  
  # Network page back to home button
  observeEvent(input$back_to_home_network, {
    updateTextInput(session, "single_gene", value = "")
    updateTextAreaInput(session, "multi_genes", value = "")
    updateTextInput(session, "selected_gene", value = "")
    updateTextInput(session, "network_search", value = "")
    updateTextInput(session, "currentPage", value = "home")
  })
  
  # Monitor currentPage changes, clear when leaving Network page
  observeEvent(input$currentPage, {
    if (input$currentPage != "network") {
      updateTextInput(session, "selected_gene", value = "")
      updateTextInput(session, "network_search", value = "")
    }
  })

  # Test button handler
  observeEvent(input$test_button, {
    # Extract gene ID (part before the space)
    gene <- normalize_gene_value(input$test_gene)
    
    # Update extracted gene ID to selected_gene
    updateTextInput(session, "selected_gene", value = gene)
  })

  # Monitor selected_gene changes, update test input box
  observeEvent(input$selected_gene, {
    updateTextInput(session, "test_gene", value = input$selected_gene)
  })

  # Test output - for debugging
  output$test_output <- renderPrint({
    cat("Current selected_gene:", input$selected_gene, "\n")
    cat("Current network_type:", input$network_type, "\n")
    cat("Current currentPage:", input$currentPage, "\n")
  })

  # Motif page back to home button
  observeEvent(input$back_to_home_motif, {
    # Clear Motif page search box
    updateTextInput(session, "motif_search", value = "")
    updateTextAreaInput(session, "motif_input", value = "")
    # Navigate to home page
    updateTextInput(session, "currentPage", value = "home")
  })

  # Pathway Analysis page related logic
  
  # Pathway visualization output
  output$pathway_visualization <- renderUI({
    req(input$currentPage == "metabolic_subgraphs")
    if (input$pathway_type == "ethylene") {
      HTML(paste0('<iframe src="/ethylene_pathway/ethylene_multilevel_network_searchable.html?v=', asset_version, '" width="100%" height="800px" frameborder="0" style="border: none; overflow: hidden;"></iframe>'))
    } else if (input$pathway_type == "lycopene") {
      HTML(paste0('<iframe src="/lycopene_pathway/lycopene_v3_main.html?v=', asset_version, '" width="100%" height="800px" frameborder="0" style="border: none; overflow: hidden;"></iframe>'))
    }
  })

  # Metabolic Node Sub-Graphs page back to home button
  observeEvent(input$back_to_home_metabolic, {
    # Navigate to home page
    updateTextInput(session, "currentPage", value = "home")
  })
  
  # Methylation sensitivity page back to home button
  observeEvent(input$back_to_home_mc_preference, {
    # Clear Methylation sensitivity page search box
    updateTextInput(session, "mc_preference_search", value = "")
    # Navigate to home page
    updateTextInput(session, "currentPage", value = "home")
  })
  

  
  # Filter network data - based on selected gene and network type
  filter_network_data <- reactive({
    network_type <- if (is.null(input$network_type) || input$network_type == "") "physical" else input$network_type
    get_filtered_network_edges(network_type, input$selected_gene)
  })
  
  # Convert network data to JSON format
  convert_to_network_json <- reactive({
    filtered_data <- filter_network_data()
    
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    # Get all nodes
    if (input$network_type == "physical") {
      nodes <- unique(c(filtered_data$`Source ID`, filtered_data$`Target ID`))
    } else {
      nodes <- unique(c(filtered_data$`Source ID`, filtered_data$`Target ID`))
    }
    
    # Create node list - use blue tones, similar to Gephi style
    node_list <- lapply(nodes, function(node_id) {
      # Generate blue-toned color
      r <- sample(50:100, 1)
      g <- sample(100:150, 1)
      b <- sample(150:255, 1)
      color <- sprintf("rgb(%d, %d, %d)", r, g, b)
      
      # Add layout attributes, support ForceAtlas2
      list(
        id = node_id,
        label = node_id,
        x = runif(1, -100, 100),
        y = runif(1, -100, 100),
        color = color,
        size = 5,
        attr = list(
          attributes = list(
            family = "gene"
          )
        ),
        # ForceAtlas2 attributes
        forceatlas2 = list(
          mass = 1.0
        )
      )
    })
    
    # Create edge list
    edge_list <- lapply(1:nrow(filtered_data), function(i) {
      row <- filtered_data[i, ]
      
      # Select correct column names based on network type
      if (input$network_type == "physical") {
        source_id <- row$`Source ID`
        target_id <- row$`Target ID`
        weight_value <- row$Weight
      } else {
        source_id <- row$`Source ID`
        target_id <- row$`Target ID`
        weight_value <- row$Weight
      }
      
      # Ensure weight is numeric type
      weight_numeric <- as.numeric(weight_value)
      # Generate semi-transparent gray for edges, closer to Gephi style
      list(
        id = paste0("edge_", i),
        source = source_id,
        target = target_id,
        weight = weight_numeric,
        label = sprintf("%.4f", weight_numeric),
        color = "rgba(200, 200, 200, 0.7)",
        size = weight_numeric * 2,
        attr = list()
      )
    })
    
    # Create complete JSON data
    network_json <- list(
      nodes = node_list,
      edges = edge_list,
      # Add layout information
      layout = list(
        algorithm = "forceatlas2",
        parameters = list(
          scalingRatio = 100,
          gravity = 0.2
        )
      )
    )
    
    return(toJSON(network_json, auto_unbox = TRUE))
  })
  
  # Network visualization output
  output$network_visualization <- renderUI({
    req(input$currentPage == "network")
    network_type <- input$network_type
    if (is.null(network_type) || network_type == "physical") {
      network_dir <- "physical_network"
    } else {
      network_dir <- "cor_network"
    }
    
    # Create iframe with improved Shiny communication script
    shiny_script <- HTML(paste0(
      '<script>
      // Listen for messages from iframe
      window.addEventListener("message", function(event) {
        console.log("Received message from iframe:", event.data);
        if (event.data.type === "gene_selected") {
          console.log("Setting selected_gene to:", event.data.gene);
          Shiny.setInputValue("selected_gene", event.data.gene, {priority: "event"});
        }
      });
      
      // Listen for iframe load completion
      function handleIframeLoad() {
        console.log("Iframe loaded successfully");
      }
      </script>',
      sprintf('<iframe id="network_iframe" src="/%s/index.html?v=%d" width="100%%" height="800px" frameborder="0" style="border: none; overflow: hidden;" onload="handleIframeLoad()"></iframe>', network_dir, asset_version)
    ))
    
    shiny_script
  })
  
  # Network data download handler - use reactiveVal to store download data
  network_download_data <- reactiveVal(NULL)
  
  # Monitor download button click
  network_download_res <- reactiveVal(NULL)

  observeEvent(input$download_network_data, {
    if (is.null(input$network_type) || input$network_type == "physical") {
      src_dir <- file.path(BASE_DIR, "www", "physical_network")
      zip_name <- "physical_network.zip"
    } else {
      src_dir <- file.path(BASE_DIR, "www", "cor_network")
      zip_name <- "cor_network.zip"
    }
    if (!dir.exists(src_dir)) {
      showNotification("Network folder not found!", type = "error")
      return()
    }
    old_res <- network_download_res()
    if (!is.null(old_res)) {
      tryCatch({
        removeResourcePath(old_res$path)
        unlink(old_res$tmp_dir, recursive = TRUE)
      }, error = function(e) {})
    }
    tmp_dir <- tempfile(pattern = "netzip_")
    dir.create(tmp_dir)
    tmp_zip <- file.path(tmp_dir, zip_name)
    old_wd <- getwd()
    setwd(src_dir)
    zip(tmp_zip, files = list.files(".", recursive = TRUE), flags = "-r")
    setwd(old_wd)
    res_name <- paste0("netzip_", as.integer(Sys.time()))
    addResourcePath(res_name, tmp_dir)
    network_download_res(list(path = res_name, tmp_dir = tmp_dir))
    session$sendCustomMessage("triggerExternalDownload", list(url = paste0("/", res_name, "/", zip_name)))
    showNotification("Network download started!", type = "message")
  })
  
  # Load table data
  table_data <- reactive({
    if (input$network_type == "physical") {
      edges_df <- get_network_edges_data("physical")
      nodes_df <- get_network_nodes_data("physical")
      list(edges = edges_df, nodes = nodes_df)
    } else {
      edges_df <- get_network_edges_data("cor")
      nodes_df <- get_network_nodes_data("cor")
      list(edges = edges_df, nodes = nodes_df)
    }
  })
  
  # Raw network subgraph data (without search filter)
  network_subtable_data <- reactive({
    network_type <- if (is.null(input$network_type) || input$network_type == "") "physical" else input$network_type
    get_network_subtable_filtered(network_type, input$selected_gene)
  })

  # Search-filtered network subgraph data
  filtered_network_data <- reactive({
    data <- network_subtable_data()
    search_term <- input$network_search
    search_column <- input$network_search_column

    if (is.null(search_term) || search_term == "") return(data)

    search_lower <- tolower(search_term)

    if (search_column == "All" || search_column == "") {
      filtered_rows <- apply(data, 1, function(row) {
        any(grepl(search_lower, tolower(as.character(row)), fixed = TRUE))
      })
    } else {
      if (search_column %in% colnames(data)) {
        filtered_rows <- grepl(search_lower, tolower(as.character(data[[search_column]])), fixed = TRUE)
      } else {
        filtered_rows <- logical(nrow(data))
      }
    }

    return(data[filtered_rows, , drop = FALSE])
  })

  # Update Network table count (Download All Data shows total rows minus 1)
  observe({
    req(input$currentPage == "network")
    network_type <- input$network_type
    if (network_type == "physical") {
      edges_df <- get_network_edges_data("physical")
    } else {
      edges_df <- get_network_edges_data("cor")
    }
    count <- format(max(nrow(edges_df) - 1, 0), big.mark = ",")
    session$sendCustomMessage("updateCount", list(id = "network_total_count", value = count))
  })

  observe({
    req(input$currentPage == "network")
    data <- filtered_network_data()
    count <- format(nrow(data), big.mark = ",")
    session$sendCustomMessage("updateCount", list(id = "network_filtered_count", value = count))
  })

  # Handle selected_gene input, update table
  output$network_table <- DT::renderDataTable({
    req(input$currentPage == "network")
    data <- filtered_network_data()

    # If data is empty, return empty table
    if (nrow(data) == 0) {
      return(DT::datatable(data, options = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100)), rownames = FALSE))
    }

    data$`Source ID` <- paste0(
      '<div style="display: flex; align-items: center; width: 100%;">',
      '<div style="width: 25%; text-align: center; padding: 2px;"><input type="checkbox" class="network-checkbox" style="width: 16px; height: 16px; cursor: pointer;" onclick="updateNetworkSelection(this)"/></div>',
      '<div style="width: 75%; padding-left: 5px;">', data$`Source ID`, '</div>',
      '</div>'
    )

    DT::datatable(
      data,
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = FALSE,
        searching = FALSE,
        columnDefs = list(
          list(targets = 0, width = '215px'),
          list(targets = 1, width = '100px'),
          list(targets = 2, width = '100px'),
          list(targets = 3, width = '100px'),
          list(targets = 4, width = '100px'),
          list(targets = 5, width = '475px')
        ),
        rowCallback = htmlwidgets::JS("function(row, data, index) {
          $(row).css('height', '47px');
          $(row).css('min-height', '47px');
          $(row).find('td').css('vertical-align', 'middle');
        }"),
        initComplete = htmlwidgets::JS("function(settings, json) {
          window.updateNetworkSelection = function(checkbox) {
            var table = $(checkbox).closest('table').DataTable();
            var selectedCount = table.$('input.network-checkbox:checked').length;
            var countEl = document.getElementById('network_selected_count');
            if (countEl) {
              countEl.textContent = selectedCount.toLocaleString();
            }
          };
        }")
      ),
      escape = FALSE,
      filter = 'none',
      rownames = FALSE,
      selection = 'none'
    )
  })
  

  
  # Protein binding sites module back to home button handler
  observeEvent(input$back_to_home_protein_binding, {
    # Clear Protein binding page input box
    updateTextInput(session, "single_gene_input", value = "")
    updateTextAreaInput(session, "multi_gene_input", value = "")
    # Navigate to home page
    updateTextInput(session, "currentPage", value = "home")
  })
  
  # JBrowse visualization page back button handler
  observeEvent(input$back_to_protein_binding, {
    updateTextInput(session, "currentPage", value = "protein_binding")
    
    # Restore config.json to initial state (only fixed tracks)
    restore_initial_config()
  })
  
  # JBrowse visualization page back to home button handler
  observeEvent(input$back_to_home_jbrowse, {
    # Clear JBrowse page search box
    updateTextInput(session, "gene_search", value = "")
    # Navigate to home page
    updateTextInput(session, "currentPage", value = "home")
  })
  
  # Read gene search data
  gene_search_data <- reactive({
    get_cached_data("gene_search_data", function() {
      if (USE_EXTERNAL_URL) {
        # Use external URL
        url <- get_file_path("gene_search", "Final_GeneSearch_Combined_new.tsv")
        data <- read_from_url(url, sep = "\t", stringsAsFactors = FALSE, header = FALSE, fill = TRUE, quote = "", check.names = FALSE)
      } else {
        file_path <- file.path(BASE_DIR, "gene_search", "Final_GeneSearch_Combined_new.tsv")
        if (!file.exists(file_path)) {
          return(data.frame())
        }
        data <- read.table(file_path, 
                           sep = "\t", 
                           stringsAsFactors = FALSE, 
                           header = FALSE, 
                           fill = TRUE, 
                           quote = "",
                           check.names = FALSE)
      }

      # Custom column names
      colnames(data) <- c(
        "TF CommonName", 
        "TF ID", 
        "Target Gene CommonName", 
        "Target Gene ID", 
        "Chr", 
        "Seqlet Start", 
        "Seqlet End", 
        "Distance To TSS", 
        "Contribution Score", 
        "In DHS", 
        "Sequence", 
        "Type activation or Repression", 
        "Target Gene Description"
      )
      
      # Format Contribution Score column to 4 decimal places
      if ("Contribution Score" %in% colnames(data)) {
        data[["Contribution Score"]] <- sapply(data[["Contribution Score"]], function(x) {
          if (is.na(x) || x == "NA") {
            return("NA")
          }
          num <- as.numeric(x)
          if (is.na(num)) {
            return("NA")
          }
          # Use format function instead of sprintf, safer
          return(format(round(num, 4), nsmall = 4))
        })
      }
      
      # Format Type activation or Repression column to 4 decimal places
      if ("Type activation or Repression" %in% colnames(data)) {
        data[["Type activation or Repression"]] <- sapply(data[["Type activation or Repression"]], function(x) {
          if (is.na(x) || x == "NA") {
            return("NA")
          }
          num <- as.numeric(x)
          if (is.na(num)) {
            return("NA")
          }
          # Use format function instead of sprintf, safer
          return(format(round(num, 4), nsmall = 4))
        })
      }
      
      return(data)
    })
  })
  
  # Read Methylation sensitivity data
  mc_preference_data <- reactive({
    get_cached_data("mc_preference_data", function() {
      if (USE_EXTERNAL_URL) {
        # Use external URL
        url <- get_file_path("data", "5mC_preference/Supplementary_Table_4_TFs_Classification.tsv")
        data <- read_from_url(url, sep = "\t", stringsAsFactors = FALSE, header = TRUE, fill = TRUE, quote = "", check.names = FALSE)
      } else {
        file_path <- file.path(BASE_DIR, "data", "5mC_preference", "Supplementary_Table_4_TFs_Classification.tsv")
        if (!file.exists(file_path)) {
          return(data.frame())
        }
        data <- read.table(file_path, 
                           sep = "\t", 
                           stringsAsFactors = FALSE, 
                           header = TRUE, 
                           fill = TRUE, 
                           quote = "",
                           check.names = FALSE)
      }

      return(data)
    })
  })

  # Read Peak-based Methylation sensitivity data (from column 4 TFid, removing first 3 columns TF_pair/amp/DAP)
  mc_peak_data <- reactive({
    get_cached_data("mc_peak_data", function() {
      if (USE_EXTERNAL_URL) {
        url <- get_file_path("data", "5mC_preference/Figure3A_M_motifs_pos41.tsv")
        data <- tryCatch({
          read_from_url(url, sep = "\t", stringsAsFactors = FALSE, header = TRUE, quote = "", check.names = FALSE)
        }, error = function(e) {
          warning(paste("Error reading mc_peak_data:", e))
          return(data.frame())
        })
      } else {
        file_path <- file.path(BASE_DIR, "data", "5mC_preference", "Figure3A_M_motifs_pos41.tsv")
        if (!file.exists(file_path)) {
          return(data.frame())
        }
        data <- data.table::fread(file_path,
                                  sep = "\t",
                                  stringsAsFactors = FALSE,
                                  header = TRUE,
                                  quote = "",
                                  check.names = FALSE,
                                  data.table = FALSE)
      }

      keep_cols <- setdiff(colnames(data), c("TF_pair", "amp", "DAP"))
      data <- data[, keep_cols, drop = FALSE]

      return(data)
    })
  })
  filtered_gene_search_data <- reactive({
    data <- gene_search_data()
    search_term <- input$gene_search_input
    search_column <- input$gene_search_column
    
    # If search box is empty, return all data
    if (is.null(search_term) || search_term == "") {
      return(data)
    }
    
    # Search specified column or all columns
    search_lower <- tolower(search_term)
    
    if (search_column == "All" || search_column == "") {
      # Search all columns
      filtered_rows <- apply(data, 1, function(row) {
        any(grepl(search_lower, tolower(as.character(row)), fixed = TRUE))
      })
    } else {
      # Search specified column only
      if (search_column %in% colnames(data)) {
        filtered_rows <- grepl(search_lower, tolower(as.character(data[[search_column]])), fixed = TRUE)
      } else {
        filtered_rows <- logical(nrow(data))
      }
    }
    
    return(data[filtered_rows, , drop = FALSE])
  })
  
  # Filter Methylation Sensitivity data based on search box and selected column
  filtered_mc_preference_data <- reactive({
    data <- mc_preference_data()
    search_term <- input$mc_preference_search
    search_column <- input$mc_preference_search_column
    
    # If search box is empty, return all data
    if (is.null(search_term) || search_term == "") {
      return(data)
    }
    
    # Search specified column or all columns
    search_lower <- tolower(search_term)
    
    if (search_column == "All" || search_column == "") {
      # Search all columns
      filtered_rows <- apply(data, 1, function(row) {
        any(grepl(search_lower, tolower(as.character(row)), fixed = TRUE))
      })
    } else {
      # Search specified column only
      if (search_column %in% colnames(data)) {
        filtered_rows <- grepl(search_lower, tolower(as.character(data[[search_column]])), fixed = TRUE)
      } else {
        filtered_rows <- logical(nrow(data))
      }
    }
    
    return(data[filtered_rows, , drop = FALSE])
  })
  
  # Filter Peak-based Methylation Sensitivity data based on search box and selected column
  filtered_mc_peak_data <- reactive({
    data <- mc_peak_data()
    search_term <- input$mc_peak_search
    search_column <- input$mc_peak_search_column
    
    if (is.null(search_term) || search_term == "") {
      return(data)
    }
    
    search_lower <- tolower(search_term)
    
    if (search_column == "All" || search_column == "") {
      filtered_rows <- Reduce(`|`, lapply(names(data), function(col) {
        grepl(search_lower, tolower(as.character(data[[col]])), fixed = TRUE)
      }))
    } else {
      if (search_column %in% colnames(data)) {
        filtered_rows <- grepl(search_lower, tolower(as.character(data[[search_column]])), fixed = TRUE)
      } else {
        filtered_rows <- logical(nrow(data))
      }
    }
    
    return(data[filtered_rows, , drop = FALSE])
  })

  # Render gene search data table
  output$gene_search_results <- DT::renderDataTable({
    req(input$currentPage == "gene_search")
    data <- filtered_gene_search_data()
    
    # If no data, return empty table
    if (nrow(data) == 0) {
      return(DT::datatable(
        data.frame(
          `Gene Name` = character(),
          GeneID = character(),
          `Expression Level` = character(),
          stringsAsFactors = FALSE
        ),
        options = list(
          pageLength = 20,
          lengthMenu = c(10, 20, 50, 100),
          scrollX = TRUE,
          searching = FALSE,
          autoWidth = FALSE,
          columnDefs = list(
            list(targets = c(0), width = '180px'),
            list(targets = "_all", className = "dt-center")
          )
        ),
        filter = 'none',
        rownames = FALSE,
        selection = 'none',
        class = 'display gene-search-table'
      ))
    }
    
    # Add checkbox before first column TF CommonName (flex layout: 1/4 checkbox, 3/4 text)
    first_col <- colnames(data)[1]
    data[[first_col]] <- paste0(
      '<div style="display: flex; align-items: center; width: 100%;">',
      '<div style="width: 25%; text-align: center; padding: 2px;"><input type="checkbox" class="gene-search-checkbox" style="width: 16px; height: 16px; cursor: pointer;" onclick="updateGeneSearchSelection(this)"/></div>',
      '<div style="width: 75%; padding-left: 5px; text-align: left;">', data[[first_col]], '</div>',
      '</div>'
    )
    
    # Render data table (server-side processing due to large data volume)
    DT::datatable(
      data,
      options = list(
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100),
        scrollX = FALSE,
        searching = FALSE,
        autoWidth = TRUE,
        pagingType = "full_numbers",
        columnDefs = list(
          list(targets = c(0), width = '180px'),   # TF CommonName (with checkbox)
          list(targets = c(1), width = '130px'),   # TF ID
          list(targets = c(2), width = '110px'),   # Target Gene CommonName (reduced 30px)
          list(targets = c(3), width = '130px'),   # Target Gene ID
          list(targets = c(4), width = '120px'),   # Chr
          list(targets = c(5), width = '100px'),   # Seqlet Start
          list(targets = c(6), width = '100px'),   # Seqlet End
          list(targets = c(7), width = '120px'),   # Distance To TSS
          list(targets = c(8), width = '130px'),   # Contribution Score
          list(targets = c(9), width = '80px'),    # In DHS
          list(targets = c(10), width = '300px'),  # Sequence
          list(targets = c(11), width = '150px'),  # Type activation or Repression
          list(targets = c(12), width = '320px'),  # Target Gene Description (increased 30px)
          list(targets = "_all", className = "dt-center")
        ),
        initComplete = htmlwidgets::JS("function(settings, json) {
          window.updateGeneSearchSelection = function(checkbox) {
            var table = $(checkbox).closest('table').DataTable();
            var selectedCount = table.$('input.gene-search-checkbox:checked').length;
            var countEl = document.getElementById('gene_selected_count');
            if (countEl) {
              countEl.textContent = selectedCount.toLocaleString();
            }
          };
        }")
      ),
      escape = FALSE,
      filter = 'none',
      rownames = FALSE,
      selection = 'none',
      class = 'display gene-search-table'
    )
  }, server = TRUE)
  
  # Render Methylation sensitivity data table
  output$mc_preference_results <- DT::renderDataTable({
    req(input$currentPage == "mc_preference")
    data <- filtered_mc_preference_data()
    
    # If no data, return empty table
    if (nrow(data) == 0) {
      return(DT::datatable(
        data.frame(
          `Transcription Factor` = character(),
          GeneID = character(),
          Classification = character(),
          stringsAsFactors = FALSE
        ),
        options = list(
          pageLength = 20,
          lengthMenu = c(10, 20, 50, 100),
          scrollX = TRUE,
          searching = FALSE,
          autoWidth = TRUE,
          columnDefs = list(
            list(targets = c(0), width = '202px'),
            list(targets = "_all", className = "dt-center")
          )
        ),
        filter = 'none',
        rownames = FALSE,
        selection = 'none'
      ))
    }
    
    # Add checkbox before first column (flex layout: 1/4 checkbox, 3/4 text)
    first_col <- colnames(data)[1]
    data[[first_col]] <- paste0(
      '<div style="display: flex; align-items: center; width: 100%;">',
      '<div style="width: 25%; text-align: center; padding: 2px;"><input type="checkbox" class="mc-checkbox" style="width: 16px; height: 16px; cursor: pointer;" onclick="updateMcSelection(this)"/></div>',
      '<div style="width: 75%; padding-left: 5px; text-align: left;">', data[[first_col]], '</div>',
      '</div>'
    )
    
    # Render data table
    DT::datatable(
      data,
      options = list(
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100),
        scrollX = FALSE,
        searching = FALSE,
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = c(0), width = '202px'),
          list(targets = 1:(ncol(data)-1), className = "dt-center")
        ),
        initComplete = htmlwidgets::JS("function(settings, json) {
          window.updateMcSelection = function(checkbox) {
            var table = $(checkbox).closest('table').DataTable();
            var selectedCount = table.$('input.mc-checkbox:checked').length;
            var countEl = document.getElementById('mc_selected_count');
            if (countEl) {
              countEl.textContent = selectedCount.toLocaleString();
            }
          };
          var api = this.api();
          var lastColIdx = api.columns().count() - 1;
          var headerCell = $(api.column(lastColIdx).header());
          var origWidth = headerCell.outerWidth();
          if (origWidth > 0) {
            var newWidth = origWidth + 40;
            api.column(lastColIdx).header().style.width = newWidth + 'px';
            api.column(lastColIdx).header().style.minWidth = newWidth + 'px';
            api.column(lastColIdx).nodes().each(function(cell) { cell.style.width = newWidth + 'px'; cell.style.minWidth = newWidth + 'px'; });
          }
        }")
      ),
      escape = FALSE,
      filter = 'none',
      rownames = FALSE,
      selection = 'none'
    )
  }, server = TRUE)

  output$mc_peak_results <- DT::renderDataTable({
    req(input$currentPage == "mc_preference")
    data <- filtered_mc_peak_data()

    if (nrow(data) == 0) {
      return(DT::datatable(
        data.frame(
          TFid = character(), motifchr = character(), motifstart = character(),
          motifend = character(), methylation_level = character(),
          stringsAsFactors = FALSE
        ),
        options = list(
          pageLength = 20,
          lengthMenu = c(10, 20, 50, 100),
          scrollX = TRUE,
          searching = FALSE,
          autoWidth = TRUE,
          columnDefs = list(
            list(targets = c(0), width = '200px'),
            list(targets = "_all", className = "dt-center")
          )
        ),
        filter = 'none',
        rownames = FALSE,
        selection = 'none'
      ))
    }

    first_col <- colnames(data)[1]
    data[[first_col]] <- paste0(
      '<div style="display: flex; align-items: center; width: 100%;">',
      '<div style="width: 25%; text-align: center; padding: 2px;"><input type="checkbox" class="mc-peak-checkbox" style="width: 16px; height: 16px; cursor: pointer;" onclick="updateMcPeakSelection(this)"/></div>',
      '<div style="width: 75%; padding-left: 5px; text-align: left;">', data[[first_col]], '</div>',
      '</div>'
    )

    DT::datatable(
      data,
      options = list(
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100),
        scrollX = FALSE,
        searching = FALSE,
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = c(0), width = '200px'),
          list(targets = 1:(ncol(data)-1), className = "dt-center")
        ),
        initComplete = htmlwidgets::JS("function(settings, json) {
          window.updateMcPeakSelection = function(checkbox) {
            var table = $(checkbox).closest('table').DataTable();
            var selectedCount = table.$('input.mc-peak-checkbox:checked').length;
            var countEl = document.getElementById('mc_peak_selected_count');
            if (countEl) {
              countEl.textContent = selectedCount.toLocaleString();
            }
          };
          var api = this.api();
          var lastColIdx = api.columns().count() - 1;
          var headerCell = $(api.column(lastColIdx).header());
          var origWidth = headerCell.outerWidth();
          if (origWidth > 0) {
            var newWidth = origWidth + 40;
            api.column(lastColIdx).header().style.width = newWidth + 'px';
            api.column(lastColIdx).header().style.minWidth = newWidth + 'px';
            api.column(lastColIdx).nodes().each(function(cell) { cell.style.width = newWidth + 'px'; cell.style.minWidth = newWidth + 'px'; });
          }
        }")
      ),
      escape = FALSE,
      filter = 'none',
      rownames = FALSE,
      selection = 'none'
    )
  }, server = TRUE)

  # Gene search page back to home button
  observeEvent(input$back_to_home_gene_search, {
    # Clear Gene search page search box
    updateTextInput(session, "gene_search_input", value = "")
    # Navigate to home page
    updateTextInput(session, "currentPage", value = "home")
  })
  
  # KEGG Enrichment page back to home button
  observeEvent(input$back_to_home_kegg, {
    # Clear KEGG Enrichment page input box
    updateTextAreaInput(session, "kegg_multi_genes", value = "")
    # Navigate to home page
    updateTextInput(session, "currentPage", value = "home")
  })
  
  # GO Enrichment page back to home button
  observeEvent(input$back_to_home_go, {
    # Clear GO Enrichment page input box
    updateTextAreaInput(session, "go_multi_genes", value = "")
    # Navigate to home page
    updateTextInput(session, "currentPage", value = "home")
  })
  
  # KEGG Enrichment results page back to KEGG Enrichment page button
  observeEvent(input$back_to_kegg, {
    updateTextInput(session, "currentPage", value = "kegg_enrichment")
  })
  
  # KEGG Enrichment results page back to home button
  observeEvent(input$back_to_home_kegg_results, {
    # Clear KEGG Enrichment page input box
    updateTextAreaInput(session, "kegg_multi_genes", value = "")
    # Navigate to home page
    updateTextInput(session, "currentPage", value = "home")
  })
  
  # GO Enrichment results page back to GO Enrichment page button
  observeEvent(input$back_to_go, {
    updateTextInput(session, "currentPage", value = "go_enrichment")
  })
  
  # GO Enrichment results page back to home button
  observeEvent(input$back_to_home_go_results, {
    # Clear GO Enrichment page input box
    updateTextAreaInput(session, "go_multi_genes", value = "")
    # Navigate to home page
    updateTextInput(session, "currentPage", value = "home")
  })
  
  # Add JBrowse 2 resource path mapping
  if (!USE_EXTERNAL_URL && !HUGGINGFACE_CONFIG$use_huggingface) {
    addResourcePath("jbrowse2", "jbrowse2")
  }
  
  # Add motif_images resource path mapping (needed for browser to load PDF/PNG)
  addResourcePath("motif_images", file.path(BASE_DIR, "data", "web_pwm_seqlogo"))
  
  # Reactive value to store agent results
  agent_result <- reactiveVal("")
  
  # Agent backend communication
  observeEvent(input$agent_submit, {
    user_input <- input$agent_input
    if (nchar(user_input) > 0) {
      # First display loading state
      agent_result(paste0("<p style='color: blue;'>Processing your request...</p>"))
      
      # Determine request type
      request_type <- "general"
      module_name <- ""
      
      if (grepl("motif|Motif|motif", user_input, ignore.case = TRUE)) {
        request_type <- "motif"
        module_name <- "Motif"
      } else if (grepl("gene|Gene|gene|ORF|Solyc", user_input, ignore.case = TRUE)) {
        request_type <- "gene"
        module_name <- "gene"
      } else if (grepl("network|network|Network|interaction|co-expression", user_input, ignore.case = TRUE)) {
        request_type <- "network"
        module_name <- "network"
      } else if (grepl("methylation|methyl|5mC|methylation", user_input, ignore.case = TRUE)) {
        request_type <- "methylation"
        module_name <- "methylation"
      } else if (grepl("metabolic|metabolic|metabolicnetwork|KEGGmetabolic", user_input, ignore.case = TRUE)) {
        request_type <- "metabolic"
        module_name <- "metabolicnetwork"
      } else if (grepl("富集|enrich|GO|KEGG|pathway analysis", user_input, ignore.case = TRUE)) {
        request_type <- "enrichment"
        module_name <- "enrichment analysis"
      }
      
      tryCatch({
        if (request_type == "motif") {
          # Call Motif search API
          response <- httr::POST(
            url = "http://localhost:8000/api/search_motif",
            body = list(user_input = user_input),
            encode = "json",
            httr::timeout(30)
          )
          
          if (httr::status_code(response) == 200) {
            result <- httr::content(response, "parsed")
            if (result$status == "success") {
              if (length(result$results) > 0) {
                result_html <- paste0("<h4>🔍 ", module_name, "search results (", result$message, ")</h4>")
                result_html <- paste0(result_html, "<table style='border-collapse: collapse; width: 100%;'>")
                result_html <- paste0(result_html, "<tr><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>Family</th><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>GeneID</th><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>CommonName</th></tr>")
                for (item in result$results) {
                  result_html <- paste0(result_html, "<tr>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$Family, "</td>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$GeneID, "</td>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$CommonName, "</td>")
                  result_html <- paste0(result_html, "</tr>")
                }
                result_html <- paste0(result_html, "</table>")
                result_html <- paste0(result_html, "<p style='font-size: 12px; color: #666; margin-top: 10px;'>� Tip: click the ", module_name, " module for details</p>")
                agent_result(result_html)
              } else {
                agent_result(paste0("<p style='color: orange;'>", result$message, "</p>"))
              }
            } else {
              agent_result(paste0("<p style='color: red;'>Search failed: ", result$message, "</p>"))
            }
          } else {
            agent_result(paste0("<p style='color: red;'>", module_name, "Search API request failed, status code: ", httr::status_code(response), "</p>"))
          }
        } else if (request_type == "gene") {
          # Call Gene search API
          response <- httr::POST(
            url = "http://localhost:8000/api/search_gene",
            body = list(user_input = user_input),
            encode = "json",
            httr::timeout(30)
          )
          
          if (httr::status_code(response) == 200) {
            result <- httr::content(response, "parsed")
            if (result$status == "success") {
              if (length(result$results) > 0) {
                result_html <- paste0("<h4>🔍 ", module_name, "search results (", result$message, ")</h4>")
                result_html <- paste0(result_html, "<table style='border-collapse: collapse; width: 100%;'>")
                result_html <- paste0(result_html, "<tr><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>Family</th><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>GeneID</th><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>CommonName</th></tr>")
                for (item in result$results) {
                  result_html <- paste0(result_html, "<tr>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$Family, "</td>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$GeneID, "</td>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$CommonName, "</td>")
                  result_html <- paste0(result_html, "</tr>")
                }
                result_html <- paste0(result_html, "</table>")
                agent_result(result_html)
              } else {
                agent_result(paste0("<p style='color: orange;'>", result$message, "</p>"))
              }
            } else {
              agent_result(paste0("<p style='color: red;'>Search failed: ", result$message, "</p>"))
            }
          } else {
            agent_result(paste0("<p style='color: red;'>", module_name, "Search API request failed, status code: ", httr::status_code(response), "</p>"))
          }
        } else if (request_type == "network") {
          # Call Network search API
          response <- httr::POST(
            url = "http://localhost:8000/api/search_network",
            body = list(user_input = user_input),
            encode = "json",
            httr::timeout(30)
          )
          
          if (httr::status_code(response) == 200) {
            result <- httr::content(response, "parsed")
            if (result$status == "success") {
              if (length(result$results) > 0) {
                result_html <- paste0("<h4>🔍 ", module_name, "search results (", result$message, ")</h4>")
                result_html <- paste0(result_html, "<table style='border-collapse: collapse; width: 100%;'>")
                result_html <- paste0(result_html, "<tr><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>Type</th><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>Source Gene</th><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>Target Gene</th><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>Weight</th></tr>")
                for (item in result$results) {
                  result_html <- paste0(result_html, "<tr>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$Type, "</td>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$SourceID, "</td>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$TargetID, "</td>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$Weight, "</td>")
                  result_html <- paste0(result_html, "</tr>")
                }
                result_html <- paste0(result_html, "</table>")
                result_html <- paste0(result_html, "<p style='font-size: 12px; color: #666; margin-top: 10px;'>💡 Tip: click the Network module for detailed network visualization</p>")
                agent_result(result_html)
              } else {
                agent_result(paste0("<p style='color: orange;'>", result$message, "</p>"))
              }
            } else {
              agent_result(paste0("<p style='color: red;'>Search failed: ", result$message, "</p>"))
            }
          } else {
            agent_result(paste0("<p style='color: red;'>", module_name, "Search API request failed, status code: ", httr::status_code(response), "</p>"))
          }
        } else if (request_type == "methylation") {
          # Call Methylation search API
          response <- httr::POST(
            url = "http://localhost:8000/api/search_methylation",
            body = list(user_input = user_input),
            encode = "json",
            httr::timeout(30)
          )
          
          if (httr::status_code(response) == 200) {
            result <- httr::content(response, "parsed")
            if (result$status == "success") {
              if (length(result$files) > 0) {
                result_html <- paste0("<h4>🔍 ", module_name, "data files (", result$message, ")</h4>")
                result_html <- paste0(result_html, "<table style='border-collapse: collapse; width: 100%;'>")
                result_html <- paste0(result_html, "<tr><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>File Path</th><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>Size (KB)</th></tr>")
                for (item in result$files) {
                  result_html <- paste0(result_html, "<tr>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$path, "</td>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$size_kb, "</td>")
                  result_html <- paste0(result_html, "</tr>")
                }
                result_html <- paste0(result_html, "</table>")
                result_html <- paste0(result_html, "<p style='font-size: 12px; color: #666; margin-top: 10px;'>💡 Tip: click the 5mC module for methylation preference analysis</p>")
                agent_result(result_html)
              } else {
                agent_result(paste0("<p style='color: orange;'>", result$message, "</p>"))
              }
            } else {
              agent_result(paste0("<p style='color: red;'>Search failed: ", result$message, "</p>"))
            }
          } else {
            agent_result(paste0("<p style='color: red;'>", module_name, "Search API request failed, status code: ", httr::status_code(response), "</p>"))
          }
        } else if (request_type == "metabolic") {
          # Call Metabolic network search API
          response <- httr::POST(
            url = "http://localhost:8000/api/search_metabolic",
            body = list(user_input = user_input),
            encode = "json",
            httr::timeout(30)
          )
          
          if (httr::status_code(response) == 200) {
            result <- httr::content(response, "parsed")
            if (result$status == "success") {
              if (length(result$files) > 0) {
                result_html <- paste0("<h4>🔍 ", module_name, "data files (", result$message, ")</h4>")
                result_html <- paste0(result_html, "<table style='border-collapse: collapse; width: 100%;'>")
                result_html <- paste0(result_html, "<tr><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>File Path</th><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>Size (KB)</th></tr>")
                for (item in result$files) {
                  result_html <- paste0(result_html, "<tr>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$path, "</td>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$size_kb, "</td>")
                  result_html <- paste0(result_html, "</tr>")
                }
                result_html <- paste0(result_html, "</table>")
                result_html <- paste0(result_html, "<p style='font-size: 12px; color: #666; margin-top: 10px;'>💡 Tip: click the metabolic network module for detailed analysis</p>")
                agent_result(result_html)
              } else {
                agent_result(paste0("<p style='color: orange;'>", result$message, "</p>"))
              }
            } else {
              agent_result(paste0("<p style='color: red;'>Search failed: ", result$message, "</p>"))
            }
          } else {
            agent_result(paste0("<p style='color: red;'>", module_name, "Search API request failed, status code: ", httr::status_code(response), "</p>"))
          }
        } else if (request_type == "enrichment") {
          # Call Enrichment analysis search API
          response <- httr::POST(
            url = "http://localhost:8000/api/search_enrichment",
            body = list(user_input = user_input),
            encode = "json",
            httr::timeout(30)
          )
          
          if (httr::status_code(response) == 200) {
            result <- httr::content(response, "parsed")
            if (result$status == "success") {
              if (length(result$files) > 0) {
                result_html <- paste0("<h4>🔍 ", module_name, "data files (", result$message, ")</h4>")
                result_html <- paste0(result_html, "<table style='border-collapse: collapse; width: 100%;'>")
                result_html <- paste0(result_html, "<tr><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>File Path</th><th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>Size (KB)</th></tr>")
                for (item in result$files) {
                  result_html <- paste0(result_html, "<tr>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$path, "</td>")
                  result_html <- paste0(result_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", item$size_kb, "</td>")
                  result_html <- paste0(result_html, "</tr>")
                }
                result_html <- paste0(result_html, "</table>")
                result_html <- paste0(result_html, "<p style='font-size: 12px; color: #666; margin-top: 10px;'>💡 Tip: click the enrichment analysis module for GO/KEGG analysis</p>")
                agent_result(result_html)
              } else {
                agent_result(paste0("<p style='color: orange;'>", result$message, "</p>"))
              }
            } else {
              agent_result(paste0("<p style='color: red;'>Search failed: ", result$message, "</p>"))
            }
          } else {
            agent_result(paste0("<p style='color: red;'>", module_name, "Search API request failed, status code: ", httr::status_code(response), "</p>"))
          }
        } else {
          # Call general Agent API
          response <- httr::POST(
            url = "http://localhost:8000/api/agent",
            body = list(user_input = user_input),
            encode = "json",
            httr::timeout(120)
          )
          
          if (httr::status_code(response) == 200) {
            result <- httr::content(response, "parsed")
            agent_result(result$result)
          } else {
            agent_result(paste0("<p style='color: red;'>API request failed, status code: ", httr::status_code(response), "</p>"))
          }
        }
      }, error = function(e) {
        agent_result(paste0("<p style='color: red;'>Request failed: ", e$message, "</p>"))
        cat("Agent API Error:", e$message, "\n")
      })
    }
  })
  
  # Render agent output
  output$agent_output <- renderUI({
    HTML(agent_result())
  })
  
  # Read bpnet_match_output.txt file
  bpnet_match_data <- reactive({
    if (USE_EXTERNAL_URL) {
      # Use external URL
      url <- get_file_path("data", "bpnet_match_output.txt")
      data <- read_from_url(url, sep = "\t", stringsAsFactors = FALSE, header = TRUE)
      return(data)
    } else if (HUGGINGFACE_CONFIG$use_huggingface) {
      url <- paste0(HUGGINGFACE_CONFIG$base_url, "/data/bpnet_match_output.txt")
      
      response <- httr::GET(url, httr::timeout(HUGGINGFACE_CONFIG$timeout))
      
      if (httr::status_code(response) != 200) {
        stop(paste("Failed to download:", url))
      }
      
      # Save to temporary file
      temp_file <- tempfile(fileext = ".txt")
      writeBin(httr::content(response, as = "raw"), temp_file)
      
      data <- read.table(temp_file, header = TRUE, stringsAsFactors = FALSE)
      unlink(temp_file)
      return(data)
    } else {
      data <- read.table(file.path(BASE_DIR, "data", "bpnet_match_output.txt"), 
                         header = TRUE, 
                         stringsAsFactors = FALSE)
      return(data)
    }
  })
  
  # Store selected genes and corresponding bw files
  selected_bw_files <- reactiveVal(list())
  
  # Store KEGG and GO analysis results
  kegg_results_data <- reactiveVal(data.frame())
  go_results_data <- reactiveVal(data.frame())
  
  # Handle KEGG Enrichment gene input
  process_kegg_gene_input <- reactive({
    genes <- c()
    
    # Handle multiple gene input
    if (!is.null(input$kegg_multi_genes) && input$kegg_multi_genes != "") {
      multi_genes <- str_split(input$kegg_multi_genes, "\n")[[1]]
      multi_genes <- trimws(multi_genes)
      multi_genes <- multi_genes[multi_genes != ""]
      genes <- c(genes, multi_genes)
    }
    
    # Handle file upload
    if (!is.null(input$kegg_file)) {
      file_path <- input$kegg_file$datapath
      file_ext <- tools::file_ext(input$kegg_file$name)
      
      if (file_ext == "csv") {
        file_genes <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)$V1
      } else {
        file_genes <- readLines(file_path)
      }
      
      file_genes <- trimws(file_genes)
      file_genes <- file_genes[file_genes != ""]
      genes <- c(genes, file_genes)
    }
    
    # Deduplicate and return
    unique(genes)
  })
  
  # Handle GO Enrichment gene input
  process_go_gene_input <- reactive({
    genes <- c()
    
    # Handle multiple gene input
    if (!is.null(input$go_multi_genes) && input$go_multi_genes != "") {
      multi_genes <- str_split(input$go_multi_genes, "\n")[[1]]
      multi_genes <- trimws(multi_genes)
      multi_genes <- multi_genes[multi_genes != ""]
      genes <- c(genes, multi_genes)
    }
    
    # Handle file upload
    if (!is.null(input$go_file)) {
      file_path <- input$go_file$datapath
      file_ext <- tools::file_ext(input$go_file$name)
      
      if (file_ext == "csv") {
        file_genes <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)$V1
      } else {
        file_genes <- readLines(file_path)
      }
      
      file_genes <- trimws(file_genes)
      file_genes <- file_genes[file_genes != ""]
      genes <- c(genes, file_genes)
    }
    
    # Deduplicate and return
    unique(genes)
  })
  
  # KEGG Enrichment analysis function
  perform_kegg_enrichment <- reactive({
    genes <- process_kegg_gene_input()
    
    if (length(genes) == 0) {
      return(data.frame())
    }
    
    # Simulate KEGG analysis results
    # In production, this should call a real KEGG analysis tool
    kegg_pathways <- c(
      "Pathway 1: Metabolic pathways",
      "Pathway 2: Biosynthesis of secondary metabolites",
      "Pathway 3: Plant hormone signal transduction",
      "Pathway 4: Carbon metabolism",
      "Pathway 5: Biosynthesis of amino acids"
    )
    
    # Generate simulated data
    result <- data.frame(
      Pathway = kegg_pathways,
      `Gene Count` = sample(1:length(genes), 5, replace = TRUE),
      `P Value` = runif(5, 0.001, 0.05),
      FDR = runif(5, 0.001, 0.05),
      Genes = sapply(1:5, function(i) {
        sample(genes, min(i, length(genes)), replace = FALSE) %>% paste(collapse = ", ")
      }),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    # Sort by P-value
    result <- result[order(result$`P Value`), ]
    
    return(result)
  })
  
  # GO Enrichment analysis function
  perform_go_enrichment <- reactive({
    genes <- process_go_gene_input()
    
    if (length(genes) == 0) {
      return(data.frame())
    }
    
    # Simulate GO analysis results
    # In production, this should call a real GO analysis tool
    go_terms <- c(
      "GO:0005634: nucleus",
      "GO:0003674: molecular_function",
      "GO:0008150: biological_process",
      "GO:0005575: cellular_component",
      "GO:0006979: response to oxidative stress"
    )
    
    # Generate simulated data
    result <- data.frame(
      `GO Term` = go_terms,
      `Gene Count` = sample(1:length(genes), 5, replace = TRUE),
      `P Value` = runif(5, 0.001, 0.05),
      FDR = runif(5, 0.001, 0.05),
      Genes = sapply(1:5, function(i) {
        sample(genes, min(i, length(genes)), replace = FALSE) %>% paste(collapse = ", ")
      }),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    # Sort by P-value
    result <- result[order(result$`P Value`), ]
    
    return(result)
  })
  
  # Render KEGG Enrichment results table
  output$kegg_results <- DT::renderDataTable({
    data <- kegg_results_data()
    
    # If no data, return empty table
    if (nrow(data) == 0) {
      return(DT::datatable(
        data.frame(),
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE
        )
      ))
    }
    
    # Render data table
    DT::datatable(
      data,
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        searching = TRUE
      ),
      filter = 'top',
      rownames = FALSE
    )
  })
  
  # Render GO Enrichment results table
  output$go_results <- DT::renderDataTable({
    data <- go_results_data()
    
    # If no data, return empty table
    if (nrow(data) == 0) {
      return(DT::datatable(
        data.frame(),
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE
        )
      ))
    }
    
    # Render data table
    DT::datatable(
      data,
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        searching = TRUE
      ),
      filter = 'top',
      rownames = FALSE
    )
  })
  
  # Process user input
  process_gene_input <- reactive({
    genes <- c()
    
    # Handle single gene input
    if (!is.null(input$single_gene_input) && input$single_gene_input != "") {
      genes <- c(genes, trimws(input$single_gene_input))
    }
    
    # Handle multiple gene input
    if (!is.null(input$multi_gene_input) && input$multi_gene_input != "") {
      multi_genes <- str_split(input$multi_gene_input, "\n")[[1]]
      multi_genes <- trimws(multi_genes)
      multi_genes <- multi_genes[multi_genes != ""]
      genes <- c(genes, multi_genes)
    }
    
    # Handle file upload
    if (!is.null(input$gene_file_upload)) {
      file_path <- input$gene_file_upload$datapath
      file_ext <- tools::file_ext(input$gene_file_upload$name)
      
      if (file_ext == "csv") {
        file_genes <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)$V1
      } else {
        file_genes <- readLines(file_path)
      }
      
      file_genes <- trimws(file_genes)
      file_genes <- file_genes[file_genes != ""]
      genes <- c(genes, file_genes)
    }
    
    # Deduplicate and return
    unique(genes)
  })
  
  # Find corresponding index based on gene information
  build_gene_indexes <- function(genes) {
    if (length(genes) == 0) {
      return(list())
    }

    cache_key <- paste0("bpnet_gene_indexes::", paste(sort(unique(genes)), collapse = "|"))
    get_cached_data(cache_key, function() {
      data <- bpnet_match_data()
      indexes <- list()

      for (gene in genes) {
        matched_rows <- data[str_detect(data$GeneID, regex(paste0("^", gene), ignore_case = TRUE)) |
                            str_detect(data$GeneID, regex(paste0("^", gene, "\\."), ignore_case = TRUE)), ]

        if (nrow(matched_rows) == 0) {
          matched_rows <- data[str_detect(data$CommonName, regex(paste0("^", gene), ignore_case = TRUE)), ]
        }

        if (nrow(matched_rows) == 0) {
          matched_rows <- data[str_detect(data$Family, regex(gene, ignore_case = TRUE)), ]
        }

        if (nrow(matched_rows) > 0) {
          for (i in 1:nrow(matched_rows)) {
            index <- matched_rows$Original_ID[i]
            indexes[[index]] <- list(
              gene = gene,
              index = index,
              group = gsub("[0-9]+", "", index),
              GeneID = matched_rows$GeneID[i],
              CommonName = matched_rows$CommonName[i],
              Family = matched_rows$Family[i]
            )
          }
        }
      }

      indexes
    })
  }

  build_bw_files <- function(indexes) {
    bw_files <- list()
    
    if (USE_EXTERNAL_URL) {
      bw_url_base <- paste0(EXTERNAL_URLS$jbrowse2, "/data/Jbrowse_bw/")
      if (length(indexes) > 0) {
        for (index_info in indexes) {
          index <- index_info$index
          group <- index_info$group
          tf_name <- index_info$gene
          if (is.null(tf_name) || !nzchar(tf_name)) next
          patterns <- c(
            paste0(tf_name, "_ampDAP_1.bw"),
            paste0(tf_name, "_ampDAP_2.bw"),
            paste0(tf_name, "_ampDAP_3.bw"),
            paste0(tf_name, "_ampDAP_control_1.bw"),
            paste0(tf_name, "_ampDAP_control_2.bw"),
            paste0(tf_name, "_ampDAP_control_3.bw"),
            paste0(tf_name, "_DAP_1.bw"),
            paste0(tf_name, "_DAP_2.bw"),
            paste0(tf_name, "_DAP_3.bw"),
            paste0(tf_name, "_DAP_control_1.bw"),
            paste0(tf_name, "_DAP_control_2.bw"),
            paste0(tf_name, "_DAP_control_3.bw"),
            paste0(tf_name, "_BS_seq.bw")
          )
          for (file_name in patterns) {
            bw_files[[file_name]] <- list(
              file_path = paste0(bw_url_base, file_name),
              index = index,
              group = group,
              track_name = gsub("_", " ", tools::file_path_sans_ext(file_name))
            )
          }
        }
      }
      return(bw_files)
    } else {
    }

    # Only get all bw files in Jbrowse_bw folder
    jbrowse_bw_dir <- file.path(BASE_DIR, "jbrowse2", "data", "Jbrowse_bw")
    all_bw_files <- list()
    if (dir.exists(jbrowse_bw_dir)) {
      all_bw_files <- list.files(jbrowse_bw_dir, pattern = "\\.bw$", full.names = TRUE)
    }

    if (length(indexes) > 0) {
      for (index_info in indexes) {
        index <- index_info$index
        group <- index_info$group
        patterns <- c(
          paste0("amp", index, ".*\\.bw"),
          paste0(index, ".*\\.bw")
        )

        for (pattern in patterns) {
          matched_files <- all_bw_files[grepl(pattern, basename(all_bw_files))]
          if (length(matched_files) > 0) {
            for (file in matched_files) {
              file_name <- basename(file)
              track_name <- gsub("_", " ", tools::file_path_sans_ext(file_name))
              bw_files[[file_name]] <- list(
                file_path = file,
                index = index,
                group = group,
                track_name = track_name
              )
            }
          }
        }
      }
    } else {
      for (file in all_bw_files) {
        file_name <- basename(file)
        track_name <- gsub("_", " ", tools::file_path_sans_ext(file_name))
        bw_files[[file_name]] <- list(
          file_path = file,
          index = "all",
          group = "all",
          track_name = track_name
        )
      }
    }

    bw_files
  }
  
  # Handle submit button click event
  observeEvent(input$submit_gene_input, {
    genes <- process_gene_input()
    indexes <- build_gene_indexes(genes)
    bw_cache_key <- paste0("jbrowse_bw_files::", paste(sort(unique(genes)), collapse = "|"))
    bw_files <- get_cached_data(bw_cache_key, function() build_bw_files(indexes))
    
    # Debug: output found bw files
    print(paste("Found", length(bw_files), "bw files:"))
    if (length(bw_files) > 0) {
      for (i in 1:length(bw_files)) {
        print(paste("  ", names(bw_files)[i], "-", bw_files[[i]]$track_name))
      }
    }
    
    # Store bw files
    selected_bw_files(bw_files)
    
    # Navigate to JBrowse visualization page
    updateTextInput(session, "currentPage", value = "jbrowse_visualization")
  })
  
  # KEGG Enrichment analysis button handler
  observeEvent(input$kegg_analysis_button, {
    # Validate gene input
    req(process_kegg_gene_input())
    
    genes <- process_kegg_gene_input()
    if (length(genes) == 0) {
      showNotification("Please enter at least one gene!", type = "error")
      return()
    }
    
    # Execute KEGG analysis
    results <- perform_kegg_enrichment()
    
    # Store results
    kegg_results_data(results)
    
    # Navigate to KEGG results page
    updateTextInput(session, "currentPage", value = "kegg_results")
  })
  
  # GO Enrichment analysis button handler
  observeEvent(input$go_analysis_button, {
    # Validate gene input
    req(process_go_gene_input())
    
    genes <- process_go_gene_input()
    if (length(genes) == 0) {
      showNotification("Please enter at least one gene!", type = "error")
      return()
    }
    
    # Execute GO analysis
    results <- perform_go_enrichment()
    
    # Store results
    go_results_data(results)
    
    # Navigate to GO results page
    updateTextInput(session, "currentPage", value = "go_results")
  })
  
  # KEGG results download functionality
  output$download_kegg_results <- downloadHandler(
    filename = function() {
      "kegg_enrichment_results.csv"
    },
    content = function(file) {
      write.csv(kegg_results_data(), file, row.names = FALSE)
    }
  )
  
  # GO results download functionality
  output$download_go_results <- downloadHandler(
    filename = function() {
      "go_enrichment_results.csv"
    },
    content = function(file) {
      write.csv(go_results_data(), file, row.names = FALSE)
    }
  )
  
  # Parse GFF3 file to get gene positions
  parse_gff3 <- reactive({
    get_cached_data("gff3_gene_map", function() {
      if (USE_EXTERNAL_URL) {
        gz_path <- file.path(BASE_DIR, "jbrowse2", "data", "ITAG4.0.sorted.gff3.gz")
        plain_path <- file.path(BASE_DIR, "jbrowse2", "data", "ITAG4.0.sorted.gff3")
        gff3_data <- tryCatch({
          if (file.exists(gz_path)) {
            con <- gzfile(gz_path, "r")
            on.exit(close(con), add = TRUE)
            read.table(con, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
          } else if (file.exists(plain_path)) {
            con <- file(plain_path, "r")
            on.exit(close(con), add = TRUE)
            read.table(con, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
          } else {
            return(NULL)
          }
        }, error = function(e) {
          return(NULL)
        })
      } else {
        gff3_path <- file.path(BASE_DIR, "jbrowse2", "data", "ITAG4.0.sorted.gff3.gz")
        if (!file.exists(gff3_path)) {
          gff3_path <- file.path(BASE_DIR, "jbrowse2", "data", "ITAG4.0.sorted.gff3")
          if (!file.exists(gff3_path)) {
            return(NULL)
          }
        }
        if (endsWith(gff3_path, ".gz")) {
          con <- gzfile(gff3_path, "r")
        } else {
          con <- file(gff3_path, "r")
        }
        on.exit(close(con), add = TRUE)
        gff3_data <- read.table(con, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
      }

      if (is.null(gff3_data) || nrow(gff3_data) == 0) {
        return(NULL)
      }

      # Filter gene features
      genes <- gff3_data[gff3_data$V3 == "gene", ]
      
      # Extract gene names and positions
      gene_map <- list()
      for (i in 1:nrow(genes)) {
        row <- genes[i, ]
        seqid <- row$V1
        start <- as.integer(row$V4)
        end <- as.integer(row$V5)
        
        # Extract gene name from attribute column
        attributes <- row$V9
        gene_id_match <- regmatches(attributes, regexpr("ID=([^;]+)", attributes))
        gene_name_match <- regmatches(attributes, regexpr("Name=([^;]+)", attributes))
        
        if (length(gene_id_match) > 0) {
          gene_id <- sub("ID=", "", gene_id_match)
          gene_map[[gene_id]] <- list(seqid = seqid, start = start, end = end)
        }
        
        if (length(gene_name_match) > 0) {
          gene_name <- sub("Name=", "", gene_name_match)
          gene_map[[gene_name]] <- list(seqid = seqid, start = start, end = end)
        }
      }

      return(gene_map)
    })
  })
  
  # Gene search function
  observeEvent(input$gene_search_button, {
    gene_name <- trimws(input$gene_search)
    if (gene_name == "") {
      showNotification("Please enter a gene name or ID", type = "warning")
      return
    }
    
    gene_map <- parse_gff3()
    if (is.null(gene_map)) {
      showNotification("GFF3 file not found", type = "error")
      return
    }
    
    if (gene_name %in% names(gene_map)) {
      gene_info <- gene_map[[gene_name]]
      start <- max(1, gene_info$start - 2000)
      end <- gene_info$end + 2000
      loc <- paste0(gene_info$seqid, ":", start, "-", end)
      
      session$sendCustomMessage("navigateJbrowse", list(assembly = "SL4.0", loc = loc))
      showNotification(paste("Found gene", gene_name, "at", loc), type = "message")
    } else {
      showNotification(paste("Gene", gene_name, "not found"), type = "error")
    }
  })
  
 # Render JBrowse 2 visualization
  output$jbrowse2_visualization <- renderUI({
    req(input$currentPage == "jbrowse_visualization")
    bw_files <- selected_bw_files()
    if (is.null(bw_files)) {
      bw_files <- list()
    }
    
    # Debug: output current bw_files
    print(paste("Rendering JBrowse 2 with", length(bw_files), "bw files:"))

    # Build JBrowse URL - use stable version number to avoid bypassing browser cache on page switch
    if (USE_EXTERNAL_URL) {
      jbrowse_url <- paste0(EXTERNAL_URLS$jbrowse2, "/index.html?assembly=SL4.0&loc=SL4.0ch00:1-2000000&trackSelector=open&view=LinearGenomeView&widgets=hierarchicalTrackSelector&v=", asset_version)
    } else {
      jbrowse_config_key <- paste0("jbrowse_config::", paste(sort(names(bw_files)), collapse = "|"))
      config <- get_cached_data(jbrowse_config_key, function() {
        config <- list(
          assemblies = list(
            list(
              name = "SL4.0",
              sequence = list(
                type = "ReferenceSequenceTrack",
                trackId = "SL4.0-ReferenceSequenceTrack",
                name = "ITAG4.0 Reference Sequence",
                category = list("Genome Reference"),
                adapter = list(
                  type = "IndexedFastaAdapter",
                  fastaLocation = list(
                    uri = get_hf_url("Slycopersicum_691_SL4.0.fa"),
                    locationType = "UriLocation"
                  ),
                  faiLocation = list(
                    uri = get_hf_url("Slycopersicum_691_SL4.0.fa.fai"),
                    locationType = "UriLocation"
                  )
                )
              )
            )
          ),
          tracks = list(
            list(
              type = "FeatureTrack",
              trackId = "ITAG4.0.sorted.gff3",
              name = "ITAG4.0 GFF3",
              category = list("Genome Reference"),
              adapter = list(
                type = "Gff3TabixAdapter",
                gffGzLocation = list(
                  uri = get_hf_url("ITAG4.0.sorted.gff3.gz"),
                  locationType = "UriLocation"
                ),
                index = list(
                  location = list(
                    uri = get_hf_url("ITAG4.0.sorted.gff3.gz.tbi"),
                    locationType = "UriLocation"
                  ),
                  indexType = "TBI"
                )
              ),
              assemblyNames = list("SL4.0")
            )
          ),
          defaultSession = list(
            name = "New Session",
            view = list(
              id = "LinearGenomeView",
              type = "LinearGenomeView",
              tracks = list(
                list(type = "ReferenceSequenceTrack", configuration = "SL4.0-ReferenceSequenceTrack"),
                list(type = "FeatureTrack", configuration = "ITAG4.0.sorted.gff3")
              ),
              location = "SL4.0ch00:1-2000000"
            ),
            widgets = list(
              hierarchicalTrackSelector = list(
                id = "hierarchicalTrackSelector",
                type = "HierarchicalTrackSelectorWidget",
                viewId = "LinearGenomeView"
              )
            ),
            activeWidgets = list(
              hierarchicalTrackSelector = "hierarchicalTrackSelector"
            )
          ),
          configuration = list(
            "SL4.0-ReferenceSequenceTrack" = list(
              category = list("Genome Reference")
            )
          )
        )

        if (length(bw_files) > 0) {
          for (i in 1:length(bw_files)) {
            bw_file <- bw_files[[i]]
            track_id <- paste0("bw_track_", i)
            file_path <- paste0("Jbrowse_bw/", basename(bw_file$file_path))
            track_name <- bw_file$track_name

            if (grepl("BS seq", track_name, ignore.case = TRUE) || grepl("BS_seq", basename(bw_file$file_path), ignore.case = TRUE)) {
              category <- list("BS seq")
            } else if (grepl("ampDAP.*control", track_name, ignore.case = TRUE)) {
              category <- list("ampDAP Control")
            } else if (grepl("DAP.*control", track_name, ignore.case = TRUE)) {
              category <- list("DAP Control")
            } else if (grepl("ampDAP", track_name, ignore.case = TRUE)) {
              category <- list("ampDAP")
            } else if (grepl("DAP", track_name, ignore.case = TRUE)) {
              category <- list("DAP")
            } else {
              category <- list("Other")
            }

            config$tracks[[length(config$tracks) + 1]] <- list(
              type = "QuantitativeTrack",
              trackId = track_id,
              name = bw_file$track_name,
              category = category,
              adapter = list(
                type = "BigWigAdapter",
                bigWigLocation = list(
                  uri = get_hf_url(file_path),
                  locationType = "UriLocation"
                )
              ),
              assemblyNames = list("SL4.0")
            )
          }
        }

        dhs_bw_url <- get_hf_url("Jbrowse_bw/DHS_47DPA.bw")
        config$tracks[[length(config$tracks) + 1]] <- list(
          type = "QuantitativeTrack",
          trackId = "dhs_track_47dpa",
          name = "DHS 47DPA",
          category = list("DHS"),
          adapter = list(
            type = "BigWigAdapter",
            bigWigLocation = list(
              uri = dhs_bw_url,
              locationType = "UriLocation"
            )
          ),
          assemblyNames = list("SL4.0")
        )

        config
      })

      config_path <- file.path(BASE_DIR, "jbrowse2", "config.json")
      write(toJSON(config, auto_unbox = TRUE, pretty = TRUE), config_path)
      jbrowse_url <- "jbrowse2/index.html?assembly=SL4.0&loc=SL4.0ch00:1-2000000&trackSelector=open&view=LinearGenomeView&widgets=hierarchicalTrackSelector"
    }
    
    HTML(paste0('<div id="jbrowse2-container" style="width: 100%; height: 700px; border: none;">
  <iframe 
    id="jbrowse-iframe"
    src="', jbrowse_url, '" 
    width="100%" 
    height="700px" 
    frameborder="0" 
    style="border: none; display: block;"
  ></iframe>
</div>'))
  })
  
  # ============ Download configuration storage (normal environment, safe to read in filename/contentType) ============
  dl_config <- new.env(parent = emptyenv())
  dl_config$gene  <- list(mode = "all", format = "csv", selected_rows = NULL, timestamp = "")
  dl_config$mc    <- list(mode = "all", format = "csv", selected_rows = NULL, timestamp = "")
  dl_config$network <- list(mode = "all", format = "csv", selected_rows = NULL, timestamp = "")
  dl_config$mc_peak <- list(mode = "all", format = "csv", selected_rows = NULL, timestamp = "")

  # ============ Motif module download functionality ============

  motif_create_zip <- function(matched_tsv, zip_path) {
    pkg_dir <- file.path(tempdir(), paste0("motif_pkg_", format(Sys.time(), "%H%M%S%f")))
    if (dir.exists(pkg_dir)) unlink(pkg_dir, recursive = TRUE)
    dir.create(pkg_dir, recursive = TRUE)
    on.exit(unlink(pkg_dir, recursive = TRUE), add = TRUE)

    write.table(matched_tsv, file.path(pkg_dir, "motif_summary_patterns.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)

    pdf_local_dir <- file.path(pkg_dir, "pdfs")
    dir.create(pdf_local_dir, recursive = TRUE)

    for (i in seq_len(nrow(matched_tsv))) {
      row_i <- matched_tsv[i, ]
      tf <- as.character(row_i$Transcription_Factor)
      method <- as.character(row_i$Method)
      pattern <- as.character(row_i$Pattern)

      tf_dir <- file.path(pdf_local_dir, tf)
      dir.create(tf_dir, recursive = TRUE, showWarnings = FALSE)

      pdf_names <- character(0)
      if (method == "BPNet" && !is.na(pattern) && nzchar(pattern)) {
        p_num <- gsub("[^0-9]", "", pattern)
        if (nzchar(p_num)) {
          pdf_names <- c(paste0(tf, "_BPNet_pattern_", p_num, "_fwd.pdf"), paste0(tf, "_BPNet_pattern_", p_num, "_rc.pdf"))
        }
      } else if (method == "Traditional") {
        pdf_names <- c(paste0(tf, "_Traditional_fwd.pdf"), paste0(tf, "_Traditional_rc.pdf"))
      }

      for (pdf_name in pdf_names) {
        dest <- file.path(tf_dir, pdf_name)
        tryCatch({
          src_local <- file.path(BASE_DIR, "data", "web_pwm_seqlogo", tf, pdf_name)
          if (file.exists(src_local)) {
            file.copy(src_local, dest)
          }
        }, error = function(e) {})
      }
    }

    old_wd <- getwd()
    setwd(pkg_dir)
    tryCatch({
      utils::zip(zip_path, c("motif_summary_patterns.tsv", "pdfs"), flags = "-r9X")
    }, error = function(e) {
      tryCatch({
        system2("zip", args = c("-r9", shQuote(zip_path), "motif_summary_patterns.tsv", "pdfs"), stdout = TRUE, stderr = TRUE)
      }, error = function(e2) {})
    })
    setwd(old_wd)
  }

  observe({
    req(input$currentPage == "motif")
    count <- format(nrow(motif_results()), big.mark = ",")
    session$sendCustomMessage("updateCount", list(id = "motif_total_count", value = count))
  })

  observe({
    req(input$currentPage == "motif")
    selected <- input$motif_selected_rows
    count <- if (is.null(selected) || length(selected) == 0) 0 else length(selected)
    count <- format(count, big.mark = ",")
    session$sendCustomMessage("updateCount", list(id = "motif_selected_count", value = count))
  })

  observe({
    req(input$currentPage == "motif")
    count <- nrow(filtered_motif_data())
    count <- format(count, big.mark = ",")
    session$sendCustomMessage("updateCount", list(id = "motif_filtered_count", value = count))
  })

  observeEvent(input$motif_download_all, {
    motif_url <- get_file_path("data", "motif_summary_patterns.tsv")
    session$sendCustomMessage("triggerFetchDownload", list(url = motif_url, filename = "motif_summary_patterns.tsv"))
    showNotification("Motif TSV download started!", type = "message")
  })

  observeEvent(input$motif_download_selected, {
    selected_rows <- input$motif_selected_rows
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("No rows selected!", type = "warning")
      return()
    }
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    zip_name <- paste0("Motif_Selected_", ts, ".zip")
    zip_path <- file.path(tempdir(), zip_name)

    tsv_url <- get_file_path("data", "motif_summary_patterns.tsv")
    all_tsv <- tryCatch(read_from_url(tsv_url, sep = "\t", stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(all_tsv) || nrow(all_tsv) == 0) { showNotification("Cannot load TSV!", type = "error"); return(invisible(NULL)) }

    tbl <- filtered_motif_data()
    n <- nrow(tbl)
    safe_rows <- as.integer(selected_rows)
    safe_rows <- safe_rows[!is.na(safe_rows) & safe_rows >= 1 & safe_rows <= n]
    if (length(safe_rows) == 0) { showNotification("No valid rows selected!", type = "warning"); return(invisible(NULL)) }

    sel_tfs <- trimws(as.character(tbl[safe_rows, "Transcription Factor"]))
    sel_methods <- trimws(as.character(tbl[safe_rows, "Method"]))
    sel_patterns <- trimws(as.character(tbl[safe_rows, "Pattern"]))
    matched_tsv <- all_tsv[0, , drop = FALSE]
    for (j in seq_along(sel_tfs)) {
      m <- all_tsv[trimws(as.character(all_tsv$Transcription_Factor)) == sel_tfs[j] & trimws(as.character(all_tsv$Method)) == sel_methods[j], , drop = FALSE]
      if (nzchar(sel_patterns[j])) m <- m[trimws(as.character(m$Pattern)) == sel_patterns[j], , drop = FALSE]
      if (nrow(m) > 0) matched_tsv <- rbind(matched_tsv, m)
    }
    matched_tsv <- unique(matched_tsv)
    if (nrow(matched_tsv) == 0) { showNotification("No matching rows!", type = "warning"); return(invisible(NULL)) }

    motif_create_zip(matched_tsv, zip_path)
    if (!file.exists(zip_path) || file.info(zip_path)$size == 0) { showNotification("ZIP failed!", type = "error"); return(invisible(NULL)) }
    zip_dir <- file.path(tempdir(), "motif_downloads")
    dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(zip_path, file.path(zip_dir, zip_name), overwrite = TRUE)
    addResourcePath("motif_zip", zip_dir)
    cache_bust <- as.numeric(Sys.time()) * 1000
    session$sendCustomMessage("triggerFetchDownload", list(url = paste0("/motif_zip/", zip_name, "?t=", cache_bust), filename = zip_name))
    showNotification("Motif Selected download complete!", type = "message")
  })

  observeEvent(input$motif_download_filtered, {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    zip_name <- paste0("Motif_Filtered_", ts, ".zip")
    zip_path <- file.path(tempdir(), zip_name)

    tsv_url <- get_file_path("data", "motif_summary_patterns.tsv")
    all_tsv <- tryCatch(read_from_url(tsv_url, sep = "\t", stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(all_tsv) || nrow(all_tsv) == 0) { showNotification("Cannot load TSV!", type = "error"); return(invisible(NULL)) }

    tbl <- filtered_motif_data()
    if (is.null(tbl) || nrow(tbl) == 0) { showNotification("No filtered data!", type = "warning"); return(invisible(NULL)) }
    filt_tfs <- unique(trimws(as.character(tbl[, "Transcription Factor"])))
    matched_tsv <- all_tsv[trimws(as.character(all_tsv$Transcription_Factor)) %in% filt_tfs, , drop = FALSE]
    if (nrow(matched_tsv) == 0) { showNotification("No matching rows!", type = "warning"); return(invisible(NULL)) }

    motif_create_zip(matched_tsv, zip_path)
    if (!file.exists(zip_path) || file.info(zip_path)$size == 0) { showNotification("ZIP failed!", type = "error"); return(invisible(NULL)) }
    zip_dir <- file.path(tempdir(), "motif_downloads")
    dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(zip_path, file.path(zip_dir, zip_name), overwrite = TRUE)
    addResourcePath("motif_zip", zip_dir)
    cache_bust <- as.numeric(Sys.time()) * 1000
    session$sendCustomMessage("triggerFetchDownload", list(url = paste0("/motif_zip/", zip_name, "?t=", cache_bust), filename = zip_name))
    showNotification("Motif Filtered download complete!", type = "message")
  })

  # ============ Network module download functionality ============

  # Helper function: get external URL for edges file by networkType
  get_network_edges_url <- function(network_type) {
    if (network_type == "physical") {
      paste0(EXTERNAL_URLS$www, "/physical_network/physcial_network_edges.csv")
    } else {
      paste0(EXTERNAL_URLS$www, "/cor_network/cor_network_edges.csv")
    }
  }

  get_network_subtable_url <- function(network_type) {
    if (network_type == "physical") {
      paste0(EXTERNAL_URLS$www, "/physical_network/subnetwork_details_total.tsv")
    } else {
      paste0(EXTERNAL_URLS$www, "/cor_network/subnetwork_details_total.tsv")
    }
  }

  # Download All Data - download raw edges file directly
  observeEvent(input$network_download_all, {
    if (input$network_type == "physical") {
      fname <- "physical_network_edges.csv"
    } else {
      fname <- "cor_network_edges.csv"
    }
    edges_url <- get_network_edges_url(input$network_type)
    session$sendCustomMessage("triggerFetchDownload", list(url = edges_url, filename = fname))
    showNotification("Network download started!", type = "message")
  })

  # Download Selected - download raw edges data for selected rows
  observeEvent(input$network_download_selected, {
    selected_rows <- input$network_selected_rows
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("No rows selected!", type = "warning")
      return()
    }
    
    # Get current table data
    fdata <- filtered_network_data()
    if (is.null(fdata) || nrow(fdata) == 0) {
      showNotification("No data available!", type = "warning")
      return()
    }
    
    # Get Source ID and Target ID of selected rows
    n <- nrow(fdata)
    safe_rows <- selected_rows[selected_rows >= 1 & selected_rows <= n]
    if (length(safe_rows) == 0) {
      showNotification("No valid rows selected!", type = "warning")
      return()
    }
    
    selected_data <- fdata[safe_rows, , drop = FALSE]
    
    # Read raw edges file from external URL
    edges_url <- get_network_edges_url(input$network_type)
    
    tryCatch({
      edges_data <- if (identical(input$network_type, "physical")) get_network_edges_data("physical") else get_network_edges_data("cor")

      if (nrow(edges_data) == 0) {
        showNotification("Failed to load edges data from remote server!", type = "error")
        return()
      }
      
      source_ids <- as.character(selected_data$`Source ID`)
      target_ids <- as.character(selected_data$`Target ID`)
      
      matched_rows <- logical(nrow(edges_data))
      for (i in seq_along(source_ids)) {
        matched_rows <- matched_rows | 
          (edges_data$Source == source_ids[i] & edges_data$Target == target_ids[i])
      }
      
      result_data <- edges_data[matched_rows, , drop = FALSE]
      
      if (nrow(result_data) == 0) {
        showNotification("No matching data found in edges file!", type = "warning")
        return()
      }
      
      tmp <- tempfile(fileext = ".csv")
      write.csv(result_data, tmp, row.names = FALSE)
      csv_content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
      unlink(tmp)
      session$sendCustomMessage("triggerDownload", list(
        content = base64enc::base64encode(charToRaw(csv_content)),
        filename = paste0("selected_edges_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      ))
      
      showNotification("Selected edges download complete!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error downloading edges file:", e$message), type = "error")
    })
  })

  # Download Filtered - download edges data filtered by search box
  observeEvent(input$network_download_filtered, {
    fdata <- filtered_network_data()
    if (is.null(fdata) || nrow(fdata) == 0) {
      showNotification("No filtered data available for download!", type = "warning")
      return()
    }

    # Read raw edges file from external URL
    edges_url <- get_network_edges_url(input$network_type)

    tryCatch({
      edges_data <- if (identical(input$network_type, "physical")) get_network_edges_data("physical") else get_network_edges_data("cor")

      if (nrow(edges_data) == 0) {
        showNotification("Failed to load edges data from remote server!", type = "error")
        return()
      }

      source_ids <- as.character(fdata$`Source ID`)
      target_ids <- as.character(fdata$`Target ID`)

      matched_rows <- logical(nrow(edges_data))
      for (i in seq_along(source_ids)) {
        matched_rows <- matched_rows |
          (edges_data$Source == source_ids[i] & edges_data$Target == target_ids[i])
      }

      result_data <- edges_data[matched_rows, , drop = FALSE]

      if (nrow(result_data) == 0) {
        showNotification("No matching edges found for filtered data!", type = "warning")
        return()
      }

      tmp <- tempfile(fileext = ".csv")
      write.csv(result_data, tmp, row.names = FALSE)
      csv_content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
      unlink(tmp)
      session$sendCustomMessage("triggerDownload", list(
        content = base64enc::base64encode(charToRaw(csv_content)),
        filename = paste0("filtered_edges_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      ))

      showNotification("Filtered edges download complete!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error downloading edges file:", e$message), type = "error")
    })
  })

  # ============ Gene Search module download functionality ============

  observe({
    req(input$currentPage == "gene_search")
    count <- nrow(gene_search_data())
    session$sendCustomMessage("updateCount", list(id = "gene_total_count", value = format(count, big.mark = ",")))
  })

  observe({
    req(input$currentPage == "gene_search")
    selected <- input$gene_selected_rows
    count <- if (is.null(selected) || length(selected) == 0) 0 else length(selected)
    count <- format(count, big.mark = ",")
    session$sendCustomMessage("updateCount", list(id = "gene_selected_count", value = count))
  })

  observe({
    req(input$currentPage == "gene_search")
    count <- nrow(filtered_gene_search_data())
    count <- format(count, big.mark = ",")
    session$sendCustomMessage("updateCount", list(id = "gene_filtered_count", value = count))
  })

  observeEvent(input$gene_download_all, {
    gene_url <- get_file_path("gene_search", "Final_GeneSearch_Combined_new.tsv")
    session$sendCustomMessage("triggerFetchDownload", list(url = gene_url, filename = "Final_GeneSearch_Combined_new.tsv"))
    showNotification("Gene Search data download started!", type = "message")
  })

  observeEvent(input$gene_download_selected, {
    selected_rows <- input$gene_selected_rows
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("No rows selected!", type = "warning")
      return()
    }

    fdata <- filtered_gene_search_data()
    if (is.null(fdata) || nrow(fdata) == 0) {
      showNotification("No data available!", type = "warning")
      return()
    }

    n <- nrow(fdata)
    safe_rows <- selected_rows[selected_rows >= 1 & selected_rows <= n]
    if (length(safe_rows) == 0) {
      showNotification("No valid rows selected!", type = "warning")
      return()
    }

    selected_data <- fdata[safe_rows, , drop = FALSE]

    tmp <- tempfile(fileext = ".tsv")
    write.table(selected_data, tmp, sep = "\t", row.names = FALSE, quote = FALSE)
    tsv_content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
    unlink(tmp)
    session$sendCustomMessage("triggerDownload", list(
      content = base64enc::base64encode(charToRaw(tsv_content)),
      filename = paste0("selected_gene_search_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tsv")
    ))

    showNotification("Selected rows download complete!", type = "message")
  })

  observeEvent(input$gene_download_filtered, {
    fdata <- filtered_gene_search_data()
    if (is.null(fdata) || nrow(fdata) == 0) {
      showNotification("No filtered data available for download!", type = "warning")
      return()
    }

    tmp <- tempfile(fileext = ".tsv")
    write.table(fdata, tmp, sep = "\t", row.names = FALSE, quote = FALSE)
    tsv_content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
    unlink(tmp)
    session$sendCustomMessage("triggerDownload", list(
      content = base64enc::base64encode(charToRaw(tsv_content)),
      filename = paste0("filtered_gene_search_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tsv")
    ))

    showNotification("Filtered data download complete!", type = "message")
  })

  # ============ Methylation Sensitivity module download functionality ============

  observe({
    req(input$currentPage == "mc_preference")
    count <- nrow(mc_preference_data())
    count <- format(count, big.mark = ",")
    session$sendCustomMessage("updateCount", list(id = "mc_total_count", value = count))
  })

  observe({
    req(input$currentPage == "mc_preference")
    selected <- input$mc_selected_rows
    count <- if (is.null(selected) || length(selected) == 0) 0 else length(selected)
    count <- format(count, big.mark = ",")
    session$sendCustomMessage("updateCount", list(id = "mc_selected_count", value = count))
  })

  observe({
    req(input$currentPage == "mc_preference")
    count <- nrow(filtered_mc_preference_data())
    count <- format(count, big.mark = ",")
    session$sendCustomMessage("updateCount", list(id = "mc_filtered_count", value = count))
  })

  observeEvent(input$mc_download_all, {
    mc_url <- get_file_path("data", "5mC_preference/Supplementary_Table_4_TFs_Classification.tsv")
    session$sendCustomMessage("triggerFetchDownload", list(url = mc_url, filename = "Supplementary_Table_4_TFs_Classification.tsv"))
    showNotification("Methylation Sensitivity data download started!", type = "message")
  })

  observeEvent(input$mc_download_selected, {
    selected_rows <- input$mc_selected_rows
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("No rows selected!", type = "warning")
      return()
    }

    fdata <- filtered_mc_preference_data()
    if (is.null(fdata) || nrow(fdata) == 0) {
      showNotification("No data available!", type = "warning")
      return()
    }

    n <- nrow(fdata)
    safe_rows <- selected_rows[selected_rows >= 1 & selected_rows <= n]
    if (length(safe_rows) == 0) {
      showNotification("No valid rows selected!", type = "warning")
      return()
    }

    selected_data <- fdata[safe_rows, , drop = FALSE]

    tmp <- tempfile(fileext = ".tsv")
    write.table(selected_data, tmp, sep = "\t", row.names = FALSE, quote = FALSE)
    tsv_content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
    unlink(tmp)
    session$sendCustomMessage("triggerDownload", list(
      content = base64enc::base64encode(charToRaw(tsv_content)),
      filename = paste0("selected_methylation_sensitivity_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tsv")
    ))

    showNotification("Selected rows download complete!", type = "message")
  })

  observeEvent(input$mc_download_filtered, {
    fdata <- filtered_mc_preference_data()
    if (is.null(fdata) || nrow(fdata) == 0) {
      showNotification("No filtered data available for download!", type = "warning")
      return()
    }

    tmp <- tempfile(fileext = ".tsv")
    write.table(fdata, tmp, sep = "\t", row.names = FALSE, quote = FALSE)
    tsv_content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
    unlink(tmp)
    session$sendCustomMessage("triggerDownload", list(
      content = base64enc::base64encode(charToRaw(tsv_content)),
      filename = paste0("filtered_methylation_sensitivity_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tsv")
    ))

    showNotification("Filtered data download complete!", type = "message")
  })

  # ============ Peak-based Methylation Sensitivity module download functionality ============

  observe({
    req(input$currentPage == "mc_preference")
    count <- nrow(mc_peak_data())
    count <- format(count, big.mark = ",")
    session$sendCustomMessage("updateCount", list(id = "mc_peak_total_count", value = count))
  })

  observe({
    req(input$currentPage == "mc_preference")
    selected <- input$mc_peak_selected_rows
    count <- if (is.null(selected) || length(selected) == 0) 0 else length(selected)
    count <- format(count, big.mark = ",")
    session$sendCustomMessage("updateCount", list(id = "mc_peak_selected_count", value = count))
  })

  observe({
    req(input$currentPage == "mc_preference")
    count <- nrow(filtered_mc_peak_data())
    count <- format(count, big.mark = ",")
    session$sendCustomMessage("updateCount", list(id = "mc_peak_filtered_count", value = count))
  })

  observeEvent(input$mc_peak_download_all, {
    mc_peak_url <- get_file_path("data", "5mC_preference/Figure3A_M_motifs_pos41.tsv")
    session$sendCustomMessage("triggerFetchDownload", list(url = mc_peak_url, filename = "Figure3A_M_motifs_pos41.tsv"))
    showNotification("Peak-based Methylation Sensitivity data download started!", type = "message")
  })

  observeEvent(input$mc_peak_download_selected, {
    selected_rows <- input$mc_peak_selected_rows
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("No rows selected!", type = "warning")
      return()
    }

    fdata <- filtered_mc_peak_data()
    if (is.null(fdata) || nrow(fdata) == 0) {
      showNotification("No data available!", type = "warning")
      return()
    }

    n <- nrow(fdata)
    safe_rows <- selected_rows[selected_rows >= 1 & selected_rows <= n]
    if (length(safe_rows) == 0) {
      showNotification("No valid rows selected!", type = "warning")
      return()
    }

    selected_data <- fdata[safe_rows, , drop = FALSE]

    tmp <- tempfile(fileext = ".tsv")
    write.table(selected_data, tmp, sep = "\t", row.names = FALSE, quote = FALSE)
    tsv_content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
    unlink(tmp)
    session$sendCustomMessage("triggerDownload", list(
      content = base64enc::base64encode(charToRaw(tsv_content)),
      filename = paste0("selected_peak_methylation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tsv")
    ))

    showNotification("Selected rows download complete!", type = "message")
  })

  observeEvent(input$mc_peak_download_filtered, {
    fdata <- filtered_mc_peak_data()
    if (is.null(fdata) || nrow(fdata) == 0) {
      showNotification("No filtered data available for download!", type = "warning")
      return()
    }

    tmp <- tempfile(fileext = ".tsv")
    write.table(fdata, tmp, sep = "\t", row.names = FALSE, quote = FALSE)
    tsv_content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
    unlink(tmp)
    session$sendCustomMessage("triggerDownload", list(
      content = base64enc::base64encode(charToRaw(tsv_content)),
      filename = paste0("filtered_peak_methylation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tsv")
    ))

    showNotification("Filtered data download complete!", type = "message")
  })
}
