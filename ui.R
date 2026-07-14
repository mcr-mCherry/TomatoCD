library(shiny)
library(shinydashboard)

# Define UI
ui <- fluidPage(
  # Navbar style
  theme = "bootstrap.css",
  # Set page margins and padding
  tags$head(
    tags$style(HTML("body { margin: 0; padding: 0; }
    /* PDF container style - fixed dimensions, scale controlled via JavaScript */
    .pdf-wrapper {
      position: relative;
      width: 300px;
      height: 40px;
      margin: 0 auto;
      overflow: hidden;
      background-color: #ffffff;
      border-radius: 0;
      box-shadow: none;
      border: 1px solid #f0f0f0; /* Light border for debugging */
      transform-origin: 0 0;
      transform: scale(1);
    }
    .pdf-wrapper embed {
      position: absolute;
      top: 0;
      left: 0;
      width: 300px !important;
      height: 40px !important;
      border: none;
      background-color: #ffffff;
      transform-origin: 0 0;
      transform: scale(1);
    }
    
    /* Special styles for PDF containers in tables */
    table.dataTable td .pdf-wrapper {
      width: 280px;
      height: 37px;
      margin: 0 auto;
      padding: 0;
    }
    table.dataTable td .pdf-wrapper embed {
      width: 280px !important;
      height: 37px !important;
    }
    
    /* Force remove all scrollbars */
    .pdf-wrapper::-webkit-scrollbar {
      display: none !important;
    }
    .pdf-wrapper embed::-webkit-scrollbar {
      display: none !important;
    }
    embed {
      overflow: hidden !important;
    }
    
    /* Ensure table cells properly contain PDF containers */
    table.dataTable td {
      vertical-align: middle !important;
      padding: 4px !important;
      line-height: 1.2;
    }
    
    /* Optimize table scrolling and layout */
    .dataTables_wrapper {
      overflow-x: auto;
    }
    
    .dataTables_scrollBody {
      overflow-x: auto !important;
    }
    
    /* Table styles specifically for PDF columns */
    table.dataTable td:nth-child(6),
    table.dataTable td:nth-child(7) {
      width: 280px !important;
      min-width: 280px !important;
      max-width: 280px !important;
      text-align: center !important;
      padding: 0 !important;
    }
    /* Style for Seqlets Number column (column 5) */
    table.dataTable td:nth-child(5) {
      width: 20px !important;
      min-width: 20px !important;
      max-width: 20px !important;
      padding: 0 !important;
      text-align: center !important;
    }
    /* Style for Transcription Factor column (column 1) */
    table.dataTable td:nth-child(1) {
      width: 120px !important;
      min-width: 120px !important;
      max-width: 120px !important;
    }
    /* Completely remove filter bar for PWM Motif Seqlogo FWD and PWM Motif Seqlogo RC columns */
    table.dataTable thead th:nth-child(5) .filter, 
    table.dataTable thead th:nth-child(6) .filter {
      display: none !important;
      width: 0 !important;
      height: 0 !important;
      padding: 0 !important;
      margin: 0 !important;
      border: none !important;
      background: transparent !important;
    }
    /* Ensure no extra padding in table header */
    table.dataTable thead th:nth-child(5), 
    table.dataTable thead th:nth-child(6) {
      padding-bottom: 8px !important;
    }
    /* Adjust table layout */
    .dataTables_wrapper .dataTables_filter {
      float: right;
      text-align: right;
      margin-bottom: 10px;
    }
    .dataTables_wrapper .dataTables_length {
      float: left;
      margin-bottom: 10px;
    }
    /* Center-align table content */
    table.dataTable tbody td {
      text-align: center !important;
      vertical-align: middle !important;
    }
    table.dataTable thead th {
      text-align: center !important;
      vertical-align: middle !important;
    }
    /* Overall table style */
    table.dataTable {
      border-collapse: collapse !important;
      width: 100% !important;
    }
    /* Table header row style - ensure only one row */
    table.dataTable thead tr {
      height: 40px !important;
      line-height: 40px !important;
    }
    /* Table header style - use navbar blue background and white text */
    table.dataTable thead th {
      background-color: #3498db !important;
      color: white !important;
      font-weight: bold !important;
      border: 1px solid #2980b9 !important;
      padding: 8px !important;
      white-space: nowrap !important;
      overflow: hidden !important;
      text-overflow: ellipsis !important;
      height: 40px !important;
      line-height: 40px !important;
      vertical-align: middle !important;
      margin: 0 !important;
    }
    /* Table header sorting arrow color */
    table.dataTable thead th.sorting_asc,
    table.dataTable thead th.sorting_desc,
    table.dataTable thead th.sorting {
      background-color: #3498db !important;
      color: white !important;
    }
    /* Table header sorting icon color */
    table.dataTable thead .sorting_asc::after,
    table.dataTable thead .sorting_desc::after {
      color: white !important;
    }
    /* Ensure only one header row */
    table.dataTable thead {
      display: table-header-group !important;
    }
    table.dataTable thead tr {
      display: table-row !important;
    }
    /* Hide all header rows except the first */
    table.dataTable thead tr:not(:first-child) {
      display: none !important;
      height: 0 !important;
      line-height: 0 !important;
      padding: 0 !important;
    }
    /* Ensure only one tr in thead */
    table.dataTable thead > tr:not(:first-child) {
      display: none !important;
    }
    /* Hide any extra thead elements */
    table.dataTable + thead,
    .dataTables_scrollHeadInner thead:not(:first-of-type) {
      display: none !important;
    }
    /* Alternating row background colors */
    table.dataTable tbody tr.even {
      background-color: #ffffff !important;
    }
    table.dataTable tbody tr.odd {
      background-color: #f5f5f5 !important;
    }
    /* Table row hover effect */
    table.dataTable tbody tr:hover {
      background-color: #e8f4f8 !important;
    }
    /* Table cell borders */
    table.dataTable tbody td {
      border: 1px solid #ddd !important;
      padding: 8px !important;
    }

    /* ============ Gene Search table specific styles ============ */
    /* Gene Search table does not apply Motif module column width constraints */
    table.gene-search-table td:nth-child(1),
    table.gene-search-table td:nth-child(5),
    table.gene-search-table td:nth-child(6),
    table.gene-search-table td:nth-child(7) {
      width: auto !important;
      min-width: auto !important;
      max-width: none !important;
      padding: 8px !important;
    }
    /* Gene Search table header style */
    table.gene-search-table thead th {
      white-space: nowrap !important;
      overflow: hidden !important;
      text-overflow: ellipsis !important;
      height: 40px !important;
      line-height: 40px !important;
      padding: 8px !important;
    }
    /* Gene Search table ensure only one header row */
    table.gene-search-table thead tr:not(:first-child),
    table.gene-search-table thead > tr:not(:first-child) {
      display: none !important;
    }

    /* Module grid styles */
    .modules-grid {
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      gap: 20px;
      margin: 20px 0;
    }
    .module-card {
      background-color: white;
      border-radius: 15px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      padding: 20px;
      transition: transform 0.3s ease, box-shadow 0.3s ease;
    }
    .module-card:hover {
      transform: translateY(-5px);
      box-shadow: 0 5px 15px rgba(0,0,0,0.15);
    }
    .module-icon {
      font-size: 32px;
      margin-bottom: 10px;
    }
    .module-card h3 {
      margin-top: 0;
      margin-bottom: 10px;
    }
    .module-card p {
      color: #666;
      margin-bottom: 0;
    }
    /* Optimize link styles */
    a {
      text-decoration: none;
    }
    a:hover {
      text-decoration: none;
    }
    /* Unified page content width */
    .main-container {
      max-width: 95%;
      margin: 0 auto;
      padding: 0 20px;
    }
    /* Navbar content width */
    .navbar-content {
      max-width: 95%;
      margin: 0 auto;
      padding: 0 20px;
    }
    /* Double-row navbar style */
    .double-navbar {
      width: 100%;
      box-sizing: border-box;
      background-color: #f8f9fa;
      padding: 0;
    }
    /* Navbar content style */
    .navbar-top,
    .navbar-bottom {
      width: 100%;
      box-sizing: border-box;
    }
    /* Navbar brand and nav item styles */
    .navbar-brand-large {
      display: flex;
      align-items: center;
      gap: 10px;
    }
    .navbar-nav-bottom {
      display: flex;
      gap: 20px;
      align-items: center;
    }
    /* Center-align dropdown select text */
    select.form-control {
      text-align: center !important;
    }
    select.form-control option {
      text-align: center !important;
    }

    @keyframes floatUp1 {
      0%, 100% { transform: translateY(0); }
      50% { transform: translateY(-12px); }
    }
    @keyframes floatUp2 {
      0%, 100% { transform: translateY(0); }
      50% { transform: translateY(-10px); }
    }
    @keyframes floatUp3 {
      0%, 100% { transform: translateY(-8px); }
      50% { transform: translateY(4px); }
    }
    @keyframes floatUp4 {
      0%, 100% { transform: translateY(4px); }
      50% { transform: translateY(-10px); }
    }
    @keyframes floatUp5 {
      0%, 100% { transform: translateY(-6px); }
      50% { transform: translateY(8px); }
    }
    @keyframes floatUp6 {
      0%, 100% { transform: translateY(8px); }
      50% { transform: translateY(-12px); }
    }")),
    
    # JavaScript code
    tags$script(HTML("
      // Update count display
      Shiny.addCustomMessageHandler('updateCount', function(data) {
        document.getElementById(data.id).innerHTML = data.value;
      });
      
      // Trigger download button click
      Shiny.addCustomMessageHandler('triggerDownload', function(data) {
        if (data.content && data.filename) {
          var byteCharacters = atob(data.content);
          var byteNumbers = new Array(byteCharacters.length);
          for (var i = 0; i < byteCharacters.length; i++) {
            byteNumbers[i] = byteCharacters.charCodeAt(i);
          }
          var byteArray = new Uint8Array(byteNumbers);
          var mimeType = data.mimeType || 'text/csv';
          var blob = new Blob([byteArray], {type: mimeType});
          
          var link = document.createElement('a');
          link.href = URL.createObjectURL(blob);
          link.download = data.filename;
          document.body.appendChild(link);
          link.click();
          document.body.removeChild(link);
          URL.revokeObjectURL(link.href);
        } else {
          // Legacy method: click the button
          setTimeout(function() {
            var btn = document.getElementById(data.id);
            if (btn) {
              btn.click();
            }
          }, 300);
        }
      });
      
      Shiny.addCustomMessageHandler('triggerExternalDownload', function(data) {
        if (data.url) {
          window.open(data.url, '_blank');
        }
      });

      Shiny.addCustomMessageHandler('triggerFetchDownload', function(data) {
        if (data.url) {
          var filename = data.filename || data.url.split('/').pop() || 'download';
          fetch(data.url, {cache: 'no-store'})
            .then(function(response) {
              if (!response.ok) throw new Error('Network response was not ok');
              return response.blob();
            })
            .then(function(blob) {
              var link = document.createElement('a');
              link.href = URL.createObjectURL(blob);
              link.download = filename;
              document.body.appendChild(link);
              link.click();
              document.body.removeChild(link);
              URL.revokeObjectURL(link.href);
            })
            .catch(function(err) {
              console.error('Download failed:', err);
              window.open(data.url, '_blank');
            });
        }
      });
      
      Shiny.addCustomMessageHandler('navigateJbrowse', function(data) {
        var iframe = document.getElementById('jbrowse-iframe');
        if (iframe) {
          var baseUrl = 'jbrowse2/index.html';
          var params = new URLSearchParams({
            assembly: data.assembly,
            loc: data.loc,
            trackSelector: 'open',
            view: 'LinearGenomeView',
            widgets: 'hierarchicalTrackSelector'
          });
          iframe.src = baseUrl + '?' + params.toString();
        }
      });
      
      // Get the real TABLE element from the container DIV
      // (DT renders data tables inside wrapping divs)
      function getDtInstance(containerId) {
        var container = document.getElementById(containerId);
        if (!container) return null;
        var table = container.querySelector('table');
        if (!table) return null;
        if (typeof $ === 'undefined' || typeof $.fn === 'undefined' || typeof $.fn.dataTable === 'undefined') return null;
        if (!$.fn.dataTable.isDataTable(table)) return null;
        return $(table).DataTable();
      }
      
      // Generic table checkbox click handler function
      function updateCheckboxCount(containerId, checkboxClass, countId) {
        var dt = getDtInstance(containerId);
        if (!dt) return;
        var selectedCount = dt.$('input.' + checkboxClass + ':checked').length;
        var countEl = document.getElementById(countId);
        if (countEl) {
          countEl.textContent = selectedCount.toLocaleString();
        }
      }
      
      // Motif table checkbox click handler function
      function motifCheckboxClick(checkbox) {
        updateCheckboxCount('motif_results', 'motif-checkbox', 'motif_selected_count');
      }
      
      // Bind events after page load
      document.addEventListener('DOMContentLoaded', function() {
        // Bind event to Download Selected menu item to get selected rows
        var dropdowns = document.querySelectorAll('.download-section .dropdown-menu');
        
        dropdowns.forEach(function(dropdown, index) {
          var items = dropdown.querySelectorAll('.dropdown-item');
          if (items.length >= 3) {
            // Second option - Download Selected - get selected rows and send to Shiny
            items[1]?.addEventListener('click', function(e) {
              e.preventDefault();
              e.stopPropagation();
              var moduleNames = ['motif', 'network', 'gene', 'mc', 'mc_peak'];
              var containerIds = ['motif_results', 'network_table', 'gene_search_results', 'mc_preference_results', 'mc_peak_results'];
              var checkboxClasses = ['motif-checkbox', 'network-checkbox', 'gene-search-checkbox', 'mc-checkbox', 'mc-peak-checkbox'];
              var selectedRows = [];
              var dt = getDtInstance(containerIds[index]);
              if (dt) {
                var filteredNodes = dt.rows({search: 'applied'}).nodes().to$();
                filteredNodes.each(function(i, row) {
                  var checkbox = $(row).find('input.' + checkboxClasses[index]);
                  if (checkbox.length > 0 && checkbox[0].checked) {
                    selectedRows.push(i + 1);
                  }
                });
                Shiny.setInputValue(moduleNames[index] + '_selected_rows', selectedRows);
              }
              Shiny.setInputValue(moduleNames[index] + '_download_selected', Math.random());
            });
            
            // Third option - Download Filtered
            items[2]?.addEventListener('click', function(e) {
              e.preventDefault();
              e.stopPropagation();
              var moduleNames = ['motif', 'network', 'gene', 'mc', 'mc_peak'];
              Shiny.setInputValue(moduleNames[index] + '_download_filtered', Math.random());
            });
          }
        });
        
        // Wait for table container to appear and update selection count (DT selection is disabled, using custom checkboxes)
        var containerIds = ['motif_results', 'gene_search_results', 'mc_preference_results', 'mc_peak_results', 'network_table'];
        var checkboxClasses = ['motif-checkbox', 'gene-search-checkbox', 'mc-checkbox', 'mc-peak-checkbox', 'network-checkbox'];
        var countIds = ['motif_selected_count', 'gene_selected_count', 'mc_selected_count', 'mc_peak_selected_count', 'network_selected_count'];
        
        var checkInterval = setInterval(function() {
          if (typeof $ === 'undefined' || typeof $.fn.dataTable === 'undefined') return;
          
          var allReady = true;
          for (var i = 0; i < containerIds.length; i++) {
            var dt = getDtInstance(containerIds[i]);
            if (!dt) {
              allReady = false;
              break;
            }
          }
          
          if (allReady) {
            clearInterval(checkInterval);
            for (var i = 0; i < containerIds.length; i++) {
              updateCheckboxCount(containerIds[i], checkboxClasses[i], countIds[i]);
            }
          }
        }, 300);
      });
    ")),

    tags$script(HTML("
      function updateMcPeakSelection(checkbox) {
        var table = $(checkbox).closest('table').DataTable();
        var selectedCount = table.$('input.mc-peak-checkbox:checked').length;
        var countEl = document.getElementById('mc_peak_selected_count');
        if (countEl) {
          countEl.textContent = selectedCount.toLocaleString();
        }
      }

      document.addEventListener('DOMContentLoaded', function() {
        function getDtInstance(containerId) {
          var container = document.getElementById(containerId);
          if (!container) return null;
          var table = container.querySelector('table');
          if (!table) return null;
          if (typeof $ === 'undefined' || typeof $.fn === 'undefined' || typeof $.fn.dataTable === 'undefined') return null;
          if (!$.fn.dataTable.isDataTable(table)) return null;
          return $(table).DataTable();
        }

        $(document).on('click', '#mc_peak_download_selected', function(e) {
          e.preventDefault();
          e.stopPropagation();
          var dt = getDtInstance('mc_peak_results');
          var selectedRows = [];
          if (dt) {
            dt.$('input.mc-peak-checkbox:checked').each(function() {
              var tr = $(this).closest('tr');
              var rowIdx = dt.row(tr).index();
              selectedRows.push(rowIdx + 1);
            });
          }
          Shiny.setInputValue('mc_peak_selected_rows', selectedRows);
        });

        $(document).on('click', '#mc_peak_download_filtered', function(e) {
          e.preventDefault();
          e.stopPropagation();
        });

        $(document).on('click', '#mc_download_selected', function(e) {
          e.preventDefault();
          e.stopPropagation();
          var dt = getDtInstance('mc_preference_results');
          var selectedRows = [];
          if (dt) {
            dt.$('input.mc-checkbox:checked').each(function() {
              var tr = $(this).closest('tr');
              var rowIdx = dt.row(tr).index();
              selectedRows.push(rowIdx + 1);
            });
          }
          Shiny.setInputValue('mc_selected_rows', selectedRows);
        });

        $(document).on('click', '#mc_download_filtered', function(e) {
          e.preventDefault();
          e.stopPropagation();
          Shiny.setInputValue('mc_download_filtered', Math.random());
        });
      });
    "))
  ),
  
  # Double-row navbar
  div(class = "double-navbar",
      # Row 1: Site title
      div(class = "navbar-top navbar-content",
              div(class = "navbar-brand-large",
                  tags$img(src = "cistrome_web_logo.png", height = "40px"),
                  "Tomato Cis-Regulatory Database"
              )
      ),
      
      # Row 2: Navigation links
      div(class = "navbar-bottom navbar-content",
              div(class = "navbar-nav-bottom",
                  # Home link
                  div(class = "nav-item",
                      actionLink("home_link", "Home", class = "nav-link")
                  ),
                  
                  # Module dropdown menu
                  div(class = "nav-item dropdown",
                      tags$button(class = "nav-link dropdown-toggle", 
                                  `data-toggle` = "dropdown",
                                  `aria-haspopup` = "true",
                                  `aria-expanded` = "false",
                                  "Module"),
                      div(class = "dropdown-menu",
                          actionLink("module_network", "Network", class = "dropdown-item"),
                          actionLink("module_protein_binding", "JBrowse2", class = "dropdown-item"),
                          actionLink("module_motif", "Motif", class = "dropdown-item"),
                          actionLink("module_mc_preference", "Methylation Sensitivity", class = "dropdown-item"),
                          actionLink("module_gene_search", "Gene Search", class = "dropdown-item"),
                          actionLink("module_metabolic_subgraphs", "Metabolic Node Sub-Graphs", class = "dropdown-item")
                      )
                  ),
                  
                  # Download link
                  div(class = "nav-item",
                      actionLink("download_link", "Download", class = "nav-link")
                  ),
                  
                  # Help link
                  div(class = "nav-item",
                      actionLink("help_link", "Help", class = "nav-link")
                  ),
                  
                  # Tools dropdown menu
                  div(class = "nav-item dropdown",
                      tags$button(class = "nav-link dropdown-toggle", 
                                  `data-toggle` = "dropdown",
                                  `aria-haspopup` = "true",
                                  `aria-expanded` = "false",
                                  "Tools"),
                      div(class = "dropdown-menu",
                          actionLink("tools_metabolic", "Metabolic Node Sub-Graphs", class = "dropdown-item")
                      )
                  )
              )
      )
  ),
  
  # Page content
  # Conditional panel: Home page content
  conditionalPanel(
    condition = "input.currentPage == 'home'",
    # Overview info section (at the top)
    div(class = "main-container",
        div(class = "info-section", style = "position: relative; padding: 0; margin: 20px 0; border-radius: 15px; box-shadow: 0 8px 20px rgba(44, 62, 80, 0.3); overflow: hidden;",
            tags$video(
                src = "vedio_tomato.mp4",
                type = "video/mp4",
                autoplay = NA,
                muted = NA,
                loop = NA,
                playsinline = NA,
                style = "position: absolute; top: 50%; left: 50%; width: 100%; height: 166.67%; transform: translate(-50%, -50%); object-fit: cover; opacity: 0.5; z-index: 0; clip-path: inset(20% 0 20% 0);"
            ),
            div(style = "position: relative; z-index: 1; padding: 40px 20px;",
                div(style = "text-align: center;",
                    div(style = "display: inline-block; padding: 10px 30px 5px 30px; background: rgba(44, 62, 80, 0.4); border-radius: 10px; position: relative; z-index: 2;",
                        h2("Welcome to the Tomato Cis-Regulatory Database", style = "text-align: center; margin-bottom: 15px; color: #ffffff; font-size: 45px; font-weight: bold; letter-spacing: 1px; text-shadow: 0 2px 8px rgba(0,0,0,0.5);"),
                        h2("A comprehensive platform for genomic and transcriptomic analysis of tomato species", style = "text-align: center; margin-bottom: 10px; color: #ffffff; font-size: 20px; font-weight: normal; letter-spacing: 0.5px; text-shadow: 0 2px 6px rgba(0,0,0,0.5);")
                    )
                ),

                div(class = "info-content", style = "display: flex; flex-wrap: wrap; justify-content: center; align-items: center; gap: 50px; margin: 30px 0 40px 0;",
                    div(class = "network-images", style = "display: flex; align-items: center; justify-content: center; gap: 72px; width: 100%; max-width: 1200px;",
                        div(style = "width: 385.8px; height: 278.2px; border-radius: 12px; overflow: hidden; border: 3px solid rgba(255,255,255,0.6); background: transparent; box-shadow: none; flex-shrink: 0;",
                            tags$img(src = "homepage_Fig1.png", style = "width: 100%; height: 100%; object-fit: contain; opacity: 0.85;")
                        ),
                        div(style = "width: 392.2px; height: 278.2px; border-radius: 12px; overflow: hidden; border: 3px solid rgba(255,255,255,0.6); background: transparent; box-shadow: none; flex-shrink: 0;",
                            tags$img(src = "homepage_Fig7.png", style = "width: 100%; height: 100%; object-fit: contain; opacity: 0.85;")
                        )
                    )
                ),
                
                div(class = "data-stats", style = "max-width: 1200px; margin: 0 auto;",
                    div(class = "stats-grid", style = "display: flex; flex-wrap: nowrap; justify-content: center; align-items: center; gap: 30px;",
                        div(class = "stat-item", style = "background-color: transparent; padding: 20px; border-radius: 50%; width: 190px; min-width: 190px; height: 190px; display: flex; flex-direction: column; justify-content: center; align-items: center; text-align: center; box-shadow: none; transition: all 0.3s ease; border: 3px solid rgba(255,255,255,0.7); margin: 0 auto; animation: floatUp1 3s ease-in-out infinite;",
                            h3("Physical Network", style = "font-size: 15px; margin-bottom: 10px; color: #ffffff; font-weight: bold; text-shadow: 0 1px 4px rgba(0,0,0,0.5); white-space: nowrap;"),
                            p("Nodes: 19,596", style = "font-size: 14px; margin: 5px 0; color: #ffffff; font-weight: 600; text-shadow: 0 1px 3px rgba(0,0,0,0.5); white-space: nowrap;"),
                            p("Edges: 103,689", style = "font-size: 14px; margin: 5px 0; color: #ffffff; font-weight: 600; text-shadow: 0 1px 3px rgba(0,0,0,0.5); white-space: nowrap;")
                        ),
                        
                        div(class = "stat-item", style = "background-color: transparent; padding: 20px; border-radius: 50%; width: 190px; min-width: 190px; height: 190px; display: flex; flex-direction: column; justify-content: center; align-items: center; text-align: center; box-shadow: none; transition: all 0.3s ease; border: 3px solid rgba(255,255,255,0.7); margin: 0 auto; animation: floatUp2 3s ease-in-out infinite;",
                            h3("Co-expression Network", style = "font-size: 15px; margin-bottom: 10px; color: #ffffff; font-weight: bold; text-shadow: 0 1px 4px rgba(0,0,0,0.5); white-space: nowrap;"),
                            p("Nodes: 12,882", style = "font-size: 14px; margin: 5px 0; color: #ffffff; font-weight: 600; text-shadow: 0 1px 3px rgba(0,0,0,0.5); white-space: nowrap;"),
                            p("Edges: 1,093,860", style = "font-size: 14px; margin: 5px 0; color: #ffffff; font-weight: 600; text-shadow: 0 1px 3px rgba(0,0,0,0.5); white-space: nowrap;")
                        ),
                        
                        div(class = "stat-item", style = "background-color: transparent; padding: 20px; border-radius: 50%; width: 190px; min-width: 190px; height: 190px; display: flex; flex-direction: column; justify-content: center; align-items: center; text-align: center; box-shadow: none; transition: all 0.3s ease; border: 3px solid rgba(255,255,255,0.7); margin: 0 auto; animation: floatUp3 3s ease-in-out infinite;",
                            h3("Motif", style = "font-size: 15px; margin-bottom: 10px; color: #ffffff; font-weight: bold; text-shadow: 0 1px 4px rgba(0,0,0,0.5); white-space: nowrap;"),
                            p("TFs: 84", style = "font-size: 14px; margin: 4px 0; color: #ffffff; font-weight: 600; text-shadow: 0 1px 3px rgba(0,0,0,0.5); white-space: nowrap;"),
                            p("BPNet Motifs: 55", style = "font-size: 14px; margin: 4px 0; color: #ffffff; font-weight: 600; text-shadow: 0 1px 3px rgba(0,0,0,0.5); white-space: nowrap;"),
                            p("Traditional: 31", style = "font-size: 14px; margin: 4px 0; color: #ffffff; font-weight: 600; text-shadow: 0 1px 3px rgba(0,0,0,0.5); white-space: nowrap;")
                        ),
                        
                        div(class = "stat-item", style = "background-color: transparent; padding: 20px; border-radius: 50%; width: 190px; min-width: 190px; height: 190px; display: flex; flex-direction: column; justify-content: center; align-items: center; text-align: center; box-shadow: none; transition: all 0.3s ease; border: 3px solid rgba(255,255,255,0.7); margin: 0 auto; animation: floatUp4 3s ease-in-out infinite;",
                            h3("DAP & AmpDAP", style = "font-size: 15px; margin-bottom: 10px; color: #ffffff; font-weight: bold; text-shadow: 0 1px 4px rgba(0,0,0,0.5); white-space: nowrap;"),
                            p("DAP: 492", style = "font-size: 14px; margin: 5px 0; color: #ffffff; font-weight: 600; text-shadow: 0 1px 3px rgba(0,0,0,0.5); white-space: nowrap;"),
                            p("AmpDAP: 486", style = "font-size: 14px; margin: 5px 0; color: #ffffff; font-weight: 600; text-shadow: 0 1px 3px rgba(0,0,0,0.5); white-space: nowrap;")
                        ),
                        
                        div(class = "stat-item", style = "background-color: transparent; padding: 20px; border-radius: 50%; width: 190px; min-width: 190px; height: 190px; display: flex; flex-direction: column; justify-content: center; align-items: center; text-align: center; box-shadow: none; transition: all 0.3s ease; border: 3px solid rgba(255,255,255,0.7); margin: 0 auto; animation: floatUp5 3s ease-in-out infinite;",
                            h3("Gene Annotation", style = "font-size: 15px; margin-bottom: 10px; color: #ffffff; font-weight: bold; text-shadow: 0 1px 4px rgba(0,0,0,0.5); white-space: nowrap;"),
                            p("CommonName: 1,816", style = "font-size: 14px; margin: 5px 0; color: #ffffff; font-weight: 600; text-shadow: 0 1px 3px rgba(0,0,0,0.5); white-space: nowrap;"),
                            p("Description: 24,251", style = "font-size: 14px; margin: 5px 0; color: #ffffff; font-weight: 600; text-shadow: 0 1px 3px rgba(0,0,0,0.5); white-space: nowrap;")
                        ),
                        
                        div(class = "stat-item", style = "background-color: transparent; padding: 20px; border-radius: 50%; width: 190px; min-width: 190px; height: 190px; display: flex; flex-direction: column; justify-content: center; align-items: center; text-align: center; box-shadow: none; transition: all 0.3s ease; border: 3px solid rgba(255,255,255,0.7); margin: 0 auto; animation: floatUp6 3s ease-in-out infinite;",
                            h3("Methylation Sensitivity", style = "font-size: 15px; margin-bottom: 10px; color: #ffffff; font-weight: bold; text-shadow: 0 1px 4px rgba(0,0,0,0.5); white-space: nowrap;"),
                            p("Inhibited: 43", style = "font-size: 14px; margin: 4px 0; color: #ffffff; font-weight: 600; text-shadow: 0 1px 3px rgba(0,0,0,0.5); white-space: nowrap;"),
                            p("Preferred: 8", style = "font-size: 14px; margin: 4px 0; color: #ffffff; font-weight: 600; text-shadow: 0 1px 3px rgba(0,0,0,0.5); white-space: nowrap;"),
                            p("Insensitive: 18", style = "font-size: 14px; margin: 4px 0; color: #ffffff; font-weight: 600; text-shadow: 0 1px 3px rgba(0,0,0,0.5); white-space: nowrap;")
                        )
                    )
                )
            )
        )
    ),
      
    # Module section (in the middle)
    div(class = "main-container",
        div(class = "modules-section", style = "padding: 40px 20px; background: linear-gradient(180deg, #ffffff 0%, #f8f9fa 100%); margin: 30px 0; border-radius: 15px; box-shadow: 0 4px 15px rgba(44, 62, 80, 0.1);",
            h2("Modules", style = "text-align: center; margin-bottom: 40px; color: #2c3e50; font-size: 56px; font-weight: bold; letter-spacing: 0.5px;"),
          
            # Module grid layout
            div(class = "modules-grid",
                # Network module
                div(class = "module-card",
                    actionLink("network_module", 
                               div(
                                 div(class = "module-icon", "🔗"),
                                 h3("Network"),
                                 p("Analyze gene-gene interaction networks. Identify functional modules using advanced algorithms.")
                               ), style = "text-decoration: none;")
                ),
                
                # JBrowse2 module
                div(class = "module-card",
                    actionLink("protein_binding_module", 
                               div(
                                 div(class = "module-icon", "📍"),
                                 h3("JBrowse2"),
                                 p("Explore protein-DNA interaction sites. Visualize binding patterns across the genome.")
                               ), style = "text-decoration: none;")
                ),
                
                # Motif module
                div(class = "module-card",
                    actionLink("motif_module", 
                               div(
                                 div(class = "module-icon", "🧬"),
                                 h3("Motif"),
                                 p("Discover regulatory DNA motifs. Analyze motif enrichment and conservation.")
                               ), style = "text-decoration: none;")
                ),
                
                # Methylation Sensitivity module
                div(class = "module-card",
                    actionLink("mc_preference_module", 
                               div(
                                 div(class = "module-icon", "📊"),
                                 h3("Methylation Sensitivity"),
                                 p("Study DNA methylation patterns. Analyze methylation sensitivity and distribution.")
                               ), style = "text-decoration: none;")
                ),
                
                # Gene Search module
                div(class = "module-card",
                    actionLink("gene_search_module", 
                               div(
                                 div(class = "module-icon", "📚"),
                                 h3("Gene Search"),
                                 p("Access comprehensive gene information. Browse gene sequences and annotations.")
                               ), style = "text-decoration: none;")
                ),
                
                # Metabolic Node Sub-Graphs module
                div(class = "module-card",
                    actionLink("metabolic_subgraphs_module", 
                               div(
                                 div(class = "module-icon", "📈"),
                                 h3("Metabolic Node Sub-Graphs"),
                                 p("Explore metabolic pathways and their regulatory networks.")
                               ), style = "text-decoration: none;")
                )
            )
        )
    ),
      
    # Links and Global Visitors section (side by side)
    div(class = "main-container",
        div(class = "links-visitors-container", style = "display: flex; gap: 20px; margin: 30px 0 40px 0; flex-wrap: wrap; align-items: stretch;",
          
            # Links section
            div(class = "links-section", style = "flex: 1; min-width: 250px; padding: 40px 20px; background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%); border-radius: 15px; box-shadow: 0 4px 15px rgba(44, 62, 80, 0.2); display: flex; flex-direction: column;",
                h2("Links", style = "text-align: center; margin-bottom: 30px; color: #ffffff; font-size: 35px; font-weight: bold; letter-spacing: 0.5px;"),
                div(class = "links-list", style = "text-align: center; padding: 0 10px; display: flex; flex-direction: column; gap: 15px; align-items: center; flex: 1;",
                    p(a("Sol Genomics Network", href = "https://solgenomics.net/", target = "_blank", style = "font-size: 20px; color: #ffffff; text-decoration: none; transition: all 0.3s ease; padding: 10px 20px; background-color: rgba(255,255,255,0.1); border-radius: 8px; border: 2px solid rgba(255,255,255,0.2); display: inline-block;"), style = "margin: 0;"),
                    p(a("MEME-ChIP", href = "https://meme-suite.org/meme/tools/meme-chip", target = "_blank", style = "font-size: 20px; color: #ffffff; text-decoration: none; transition: all 0.3s ease; padding: 10px 20px; background-color: rgba(255,255,255,0.1); border-radius: 8px; border: 2px solid rgba(255,255,255,0.2); display: inline-block;"), style = "margin: 0;"),
                    p(a("JASPAR", href = "https://jaspar.elixir.no/", target = "_blank", style = "font-size: 20px; color: #ffffff; text-decoration: none; transition: all 0.3s ease; padding: 10px 20px; background-color: rgba(255,255,255,0.1); border-radius: 8px; border: 2px solid rgba(255,255,255,0.2); display: inline-block;"), style = "margin: 0;"),
                    p(a("PlantTFDB", href = "https://planttfdb.gao-lab.org/", target = "_blank", style = "font-size: 20px; color: #ffffff; text-decoration: none; transition: all 0.3s ease; padding: 10px 20px; background-color: rgba(255,255,255,0.1); border-radius: 8px; border: 2px solid rgba(255,255,255,0.2); display: inline-block;"), style = "margin: 0;"),
                    p(a("LangLab Platform", href = "https://little-ant.shinyapps.io/langlab-platform/", target = "_blank", style = "font-size: 20px; color: #ffffff; text-decoration: none; transition: all 0.3s ease; padding: 10px 20px; background-color: rgba(255,255,255,0.1); border-radius: 8px; border: 2px solid rgba(255,255,255,0.2); display: inline-block;"), style = "margin: 0;")
                )
            ),
          
            # Visitor statistics section
            div(class = "visitors-section", style = "flex: 1; min-width: 250px; padding: 40px 20px; background: linear-gradient(180deg, #ffffff 0%, #f8f9fa 100%); border-radius: 15px; box-shadow: 0 4px 15px rgba(44, 62, 80, 0.1); display: flex; flex-direction: column;",
                h2("Global Visitors", style = "text-align: center; margin-bottom: 25px; color: #2c3e50; font-size: 35px; font-weight: bold; letter-spacing: 0.5px;"),
                
                # Static image embed code
                div(class = "clustrmaps-wrapper", style = "width: 100%; height: 250px; display: flex; align-items: center; justify-content: center; margin-top: 30px;",
                    tags$a(href = "https://mapmyvisitors.com/web/1c6bc", title = "Visit tracker",
                           tags$img(src = "https://mapmyvisitors.com/map.png?cl=2c3e50&w=760&t=tt&d=1M1_aPy-KHbx1gYEZa5hukHU9nbLbzUZ_FoAec1dX40&co=ffffff", style = "width: 100%; height: 100%; object-fit: contain;")
                    )
                )
            ),
          
            # Contact Us section
            div(class = "contact-section", style = "flex: 1; min-width: 250px; padding: 40px 20px; background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%); border-radius: 15px; box-shadow: 0 4px 15px rgba(44, 62, 80, 0.2); display: flex; flex-direction: column; color: white;",
                h2("Contact Us", style = "text-align: center; margin-bottom: 50px; color: #ffffff; font-size: 35px; font-weight: bold; letter-spacing: 0.5px;"),
                div(class = "contact-content", style = "text-align: center; padding: 0 10px; display: flex; flex-direction: column; gap: 15px; align-items: center; flex: 1;",
                    p("If have any questions or suggestions, please feel free to contact us.", style = "margin: 0; line-height: 1.5; font-size: 20px;"),
                    p("Emails: ", style = "margin: 5px 0; line-height: 1.5; font-size: 20px;"),
                    p("may3@sustech.edu.cn", style = "margin: 5px 0; line-height: 1.5; font-size: 20px;"),
                    p("1024738136@qq.com", style = "margin: 5px 0; line-height: 1.5; font-size: 20px;")
                )
            )
        )
    )
  ),

  # Conditional panel: Motif search page
  conditionalPanel(
    condition = "input.currentPage == 'motif'",
    # Motif search page - display all data directly
    div(id = "motif-search-page", class = "motif-search-section", style = "display: flex; flex-direction: column; align-items: center; width: 100%;",
        h2("Motif", style = "text-align: center; width: 100%; margin: 12px 0; font-weight: normal; font-size: 2.2em; font-family: Arial, Helvetica, sans-serif;"),
          
        # Back button
        div(class = "back-button", style = "text-align: center; margin: 5px 0; width: 100%;",
            actionButton("back_to_home_motif", "Back to Home", style = "background-color: #34495e; border-color: #34495e; color: white; padding: 4px 16px; font-size: 14px;")
        ),
          
        # Search box and column selection
        div(class = "search-box", style = "margin: 5px 0 3px 0; display: flex; justify-content: center;",
            div(style = "width: 700px; display: flex; gap: 10px; align-items: center;",
                selectInput("motif_search_column", "",
                            choices = c("All", "Transcription Factor", "GeneID", "Method", "Pattern", "Seqlets Number"),
                            selected = "All",
                            width = "260px"),
                textInput("motif_search", "", "",
                          placeholder = "Search by Transcription Factor, GeneID, Method, etc.",
                          width = "100%")
            )
        ),
          
        # Download button area
        div(class = "download-section", style = "width: 95%; max-width: 1600px; margin: 2px 0 8px 0; text-align: left;",
            div(class = "dropdown", style = "display: inline-block;",
                tags$button(class = "btn btn-primary dropdown-toggle", 
                            `data-toggle` = "dropdown",
                            `aria-haspopup` = "true",
                            `aria-expanded` = "false",
                            "Download"),
                div(class = "dropdown-menu",
                    actionLink("motif_download_all", HTML("Download All Data (<span id='motif_total_count'>0</span>)"), class = "dropdown-item"),
                    actionLink("motif_download_selected", HTML("Download Selected (<span id='motif_selected_count'>0</span>)"), class = "dropdown-item"),
                    actionLink("motif_download_filtered", HTML("Download Filtered (<span id='motif_filtered_count'>0</span>)"), class = "dropdown-item")
                )
            )
        ),

        # Data table
        div(class = "motif-results", style = "margin: 10px 0; width: 95%; max-width: 1600px;",
            DT::dataTableOutput("motif_results")
        )
    )
  ),
    
  # Conditional panel: Network page
  conditionalPanel(
    condition = "input.currentPage == 'network'",
    # Network page
    div(id = "network-page", class = "network-section", style = "display: flex; flex-direction: column; align-items: center; width: 100%; padding: 10px 0;",
        h2("Network", style = "text-align: center; width: 100%; margin: 12px 0; font-weight: normal; font-size: 2.2em; font-family: Arial, Helvetica, sans-serif;"),
          
        # Back button
        div(class = "back-button", style = "text-align: center; margin: 5px 0; width: 100%;",
            actionButton("back_to_home_network", "Back to Home", style = "background-color: #34495e; border-color: #34495e; color: white; padding: 4px 16px; font-size: 14px;")
        ),
          
        # Network type selection
        div(class = "network-type-section", style = "text-align: center; width: 100%; margin: 5px 0 10px 0;",
            h3("Network Mode", style = "text-align: center; margin: 0 0 6px 0;"),
            div(class = "radio-buttons", style = "display: inline-block; text-align: left;",
                radioButtons("network_type", "",
                             choices = list("Physical Network" = "physical",
                                            "Co-expression Network" = "coexpression"),
                             selected = "physical")
            )
        ),
          
        # Download button - right aligned
        div(class = "network-download", style = "text-align: right; margin: 2px 20px 5px 0;",
            actionButton("download_network_data", "Download Network HTML", style = "background-color: #34495e; border-color: #34495e; color: white; padding: 4px 16px; font-size: 14px;")
        ),
          
        # Network visualization area
        div(class = "network-visualization", style = "width: 100%; height: 800px; margin: 0 auto; overflow: hidden;",
            htmlOutput("network_visualization")
        ),
        
        # Data table - only shown when a gene is selected
        conditionalPanel(
          condition = "input.selected_gene && input.selected_gene !== ''",
          div(class = "network-table", style = "width: 1800px; margin: 20px auto; overflow-x: auto;",
              h3("Subnetwork Details", style = "color: black; font-weight: bold; margin: 20px 0 10px 0; text-align: center;"),
              div(class = "search-box", style = "margin: 5px 0 10px 0; display: flex; justify-content: flex-start;",
                  div(style = "width: 700px; display: flex; gap: 10px; align-items: center;",
                      selectInput("network_search_column", "",
                                  choices = c("All", "Source ID", "Source CommonName", "Target ID", "Target CommonName", "Weight", "Target Gene Description"),
                                  selected = "All",
                                  width = "260px"),
                      textInput("network_search", "", "",
                                placeholder = "Search by Source ID, Target ID, etc.",
                                width = "100%")
                  )
              ),
              div(class = "download-section", style = "width: 100%; margin: 0 0 8px 0; text-align: left;",
                  div(class = "dropdown", style = "display: inline-block;",
                      tags$button(class = "btn btn-primary dropdown-toggle",
                                  `data-toggle` = "dropdown",
                                  `aria-haspopup` = "true",
                                  `aria-expanded` = "false",
                                  "Download"),
                      div(class = "dropdown-menu",
                          actionLink("network_download_all", HTML("Download All Data (<span id='network_total_count'>0</span>)"), class = "dropdown-item"),
                          div(class = "dropdown-item", style = "cursor: pointer;", HTML("Download Selected (<span id='network_selected_count'>0</span>)")),
                          div(class = "dropdown-item", style = "cursor: pointer;", HTML("Download Filtered (<span id='network_filtered_count'>0</span>)"))
                      )
                  )
              ),
              DT::dataTableOutput("network_table")
          )
        )
    )
  ),
    
  # Conditional panel: JBrowse2 module
  conditionalPanel(
    condition = "input.currentPage == 'protein_binding'",
    # JBrowse2 page
    div(id = "protein-binding-page", class = "protein-binding-section", style = "width: 100%; padding: 10px; margin: 0;",
        h2("JBrowse2", style = "text-align: center; width: 100%; margin: 12px 0; font-weight: normal; font-size: 2.2em; font-family: Arial, Helvetica, sans-serif;"),
          
        # Back button
        div(class = "back-button", style = "text-align: center; margin: 5px 0; width: 100%;",
            actionButton("back_to_home_protein_binding", "Back to Home", class = "btn-primary", style = "padding: 4px 16px; font-size: 14px;")
        ),
          
        # User input area
        div(class = "gene-input-section", style = "display: flex; flex-direction: column; align-items: center; width: 100%; margin: 10px 0;",
            h3("Gene Input", style = "text-align: center; width: 100%;"),
              
            # Single gene input
            div(class = "single-gene-input", style = "margin: 10px 0; text-align: center;",
                div(style = "margin: 0 auto; width: 600px;",
                    textInput("single_gene_input", "Single Gene Input:",
                              placeholder = "Enter Solyc ID, CommonName, or family (e.g., Solyc02g084230, EIL2, WRKY)",
                              width = "100%")
                )
            ),
              
            # Multiple genes input
            div(class = "multi-gene-input", style = "margin: 10px 0; text-align: center;",
                textAreaInput("multi_gene_input", "Multiple Genes Input (one per line):",
                              value = "",
                              rows = 8,
                              placeholder = "Example:\nSolyc02g084230\nEIL2\nWRKY",
                              width = "600px")
            ),
              
            # File upload
            div(class = "file-upload", style = "margin: 10px 0; text-align: center;",
                div(style = "display: inline-block;",
                    fileInput("gene_file_upload", "Upload file with genes (one per line):",
                              accept = c(".txt", ".csv"))
                )
            ),
              
            # Submit button
            div(class = "submit-button", style = "margin: 20px 0; text-align: center;",
                actionButton("submit_gene_input", "Submit", class = "btn-primary")
            )
        )
    )
  ),
    
  # Conditional panel: JBrowse 2 visualization module
  conditionalPanel(
    condition = "input.currentPage == 'jbrowse_visualization'",
    # JBrowse2 visualization page
    div(id = "jbrowse-visualization-page", class = "jbrowse-visualization-section", style = "width: 100%; padding: 15px 20px; margin: 0; background-color: #fff; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
        h2("JBrowse2", style = "text-align: center; width: 100%; margin: 0 0 8px 0; color: #2c3e50; font-weight: normal; font-size: 2.2em; font-family: Arial, Helvetica, sans-serif;"),
          
        div(class = "back-button", style = "text-align: center; margin: 0 0 8px 0; width: 100%;",
            actionButton("back_to_home_jbrowse", "Back to Home", class = "btn-primary", style = "padding: 4px 16px; font-size: 14px;")
        ),
          
        div(style = "display: flex; align-items: center; justify-content: center; gap: 10px; margin: 0 0 8px 0; width: 100%;",
            tags$label("Search Gene:", style = "font-weight: bold; white-space: nowrap; margin: 0; line-height: 34px; margin-top: -19px;"),
            div(style = "flex: 0 1 400px;",
                textInput("gene_search", NULL, "", placeholder = "Enter gene ID (e.g., Solyc01g006430)", width = "100%")
            ),
            actionButton("gene_search_button", "Search", class = "btn-primary", style = "height: 34px; padding: 0 16px; line-height: 34px; margin-top: -15px;")
        ),
          
        div(class = "jbrowse2-container", style = "width: 100%; height: 700px; border: 1px solid #ddd; margin: 0; padding: 0;",
            htmlOutput("jbrowse2_visualization")
        )
    )
  ),
    
  # Conditional panel: Gene Search page
  conditionalPanel(
    condition = "input.currentPage == 'gene_search'",
    # Gene Search page
    div(id = "gene-search-page", class = "gene-search-section", style = "display: flex; flex-direction: column; align-items: center; width: 100%;",
        h2("Gene Search", style = "text-align: center; width: 100%; margin: 12px 0; font-weight: normal; font-size: 2.2em; font-family: Arial, Helvetica, sans-serif;"),
          
        # Back button
        div(class = "back-button", style = "text-align: center; margin: 5px 0; width: 100%;",
            actionButton("back_to_home_gene_search", "Back to Home", style = "background-color: #34495e; border-color: #34495e; color: white; padding: 4px 16px; font-size: 14px;")
        ),
          
        # Search box and column selection
        div(class = "search-box", style = "margin: 5px 0 10px 0; display: flex; justify-content: center;",
            div(style = "width: 900px; display: flex; gap: 10px; align-items: center;",
                selectInput("gene_search_column", "",
                            choices = c("All", "TF CommonName", "TF ID", "Target Gene CommonName", "Target Gene ID", "Chr", "Seqlet Start", "Seqlet End", "Distance To TSS", "Contribution Score", "In DHS", "Sequence", "Type activation or Repression", "Target Gene Description"),
                            selected = "All",
                            width = "250px"),
                textInput("gene_search_input", "", "",
                          placeholder = "Search by TF CommonName, Target Gene ID, Sequence, etc.",
                          width = "410px")
            )
        ),
          
        # Download button area
        div(class = "download-section", style = "width: 95%; max-width: 2000px; margin: 5px 0 10px 0; text-align: left;",
            div(class = "dropdown", style = "display: inline-block;",
                tags$button(class = "btn btn-primary dropdown-toggle", 
                            `data-toggle` = "dropdown",
                            `aria-haspopup` = "true",
                            `aria-expanded` = "false",
                            "Download"),
                div(class = "dropdown-menu",
                    actionLink("gene_download_all", HTML("Download All Data (<span id='gene_total_count'>0</span>)"), class = "dropdown-item"),
                    div(class = "dropdown-item", style = "cursor: pointer;", HTML("Download Selected (<span id='gene_selected_count'>0</span>)")),
                    div(class = "dropdown-item", style = "cursor: pointer;", HTML("Download Filtered (<span id='gene_filtered_count'>0</span>)"))
                )
            )
        ),

        # Data table
        div(class = "gene-results", style = "margin: 10px 0; width: 95%; max-width: 2000px;",
            DT::dataTableOutput("gene_search_results")
        )
    )
  ),

  # Conditional panel: KEGG Enrichment page
  conditionalPanel(
    condition = "input.currentPage == 'kegg_enrichment'",
    # KEGG Enrichment page
    div(id = "kegg-enrichment-page", class = "enrichment-section", style = "display: flex; flex-direction: column; align-items: center; width: 100%; padding: 10px 0;",
        h2("KEGG Enrichment", style = "text-align: center; width: 100%; margin: 12px 0; color: #2c3e50; font-weight: normal; font-size: 2.2em; font-family: Arial, Helvetica, sans-serif;"),
          
        # Back button
        div(class = "back-button", style = "text-align: center; margin: 5px 0; width: 100%;",
            actionButton("back_to_home_kegg", "Back to Home", style = "background-color: #34495e; border-color: #34495e; color: white; padding: 4px 16px; font-size: 14px;")
        ),
          
        # Gene input options
        div(class = "gene-input-section", style = "display: flex; flex-direction: column; align-items: center; width: 100%; margin: 5px 0 6px 0;",
            h3("Gene Input", style = "text-align: center; width: 100%; margin: 0 0 6px 0;"),
              
            # Multiple genes input
            div(class = "multi-gene-input", style = "margin: 0 0 10px 0; text-align: center;",
                textAreaInput("kegg_multi_genes", "Multiple Genes Input (one per line):",
                              value = "",
                              rows = 8,
                              placeholder = "Example:\nSolyc01g006430\nSolyc03g081230\nSolyc03g097870",
                              width = "600px")
            ),
              
            # File upload
            div(class = "file-upload", style = "margin: 0 0 0 0; text-align: center;",
                div(style = "display: inline-block;",
                    fileInput("kegg_file", "Upload Gene List File (one gene per line):",
                              accept = c(".txt", ".csv"))
                )
            ),
              
            # Analysis button
            div(class = "analysis-button", style = "margin: 0 0 10px 0; text-align: center;",
                actionButton("kegg_analysis_button", "Run Analysis", style = "background-color: #34495e; border-color: #34495e; color: white;")
            )
        )
    )
  ),
    
  # Conditional panel: GO Enrichment page
  conditionalPanel(
    condition = "input.currentPage == 'go_enrichment'",
    # GO Enrichment page
    div(id = "go-enrichment-page", class = "enrichment-section", style = "display: flex; flex-direction: column; align-items: center; width: 100%; padding: 10px 0;",
        h2("GO Enrichment", style = "text-align: center; width: 100%; margin: 12px 0; color: #2c3e50; font-weight: normal; font-size: 2.2em; font-family: Arial, Helvetica, sans-serif;"),
          
        # Back button
        div(class = "back-button", style = "text-align: center; margin: 5px 0; width: 100%;",
            actionButton("back_to_home_go", "Back to Home", style = "background-color: #34495e; border-color: #34495e; color: white; padding: 4px 16px; font-size: 14px;")
        ),
          
        # Gene input options
        div(class = "gene-input-section", style = "display: flex; flex-direction: column; align-items: center; width: 100%; margin: 5px 0 6px 0;",
            h3("Gene Input", style = "text-align: center; width: 100%; margin: 0 0 6px 0;"),
              
            # Multiple genes input
            div(class = "multi-gene-input", style = "margin: 0 0 10px 0; text-align: center;",
                textAreaInput("go_multi_genes", "Multiple Genes Input (one per line):",
                              value = "",
                              rows = 8,
                              placeholder = "Example:\nSolyc01g006430\nSolyc03g081230\nSolyc03g097870",
                              width = "600px")
            ),
              
            # File upload
            div(class = "file-upload", style = "margin: 0 0 0 0; text-align: center;",
                div(style = "display: inline-block;",
                    fileInput("go_file", "Upload Gene List File (one gene per line):",
                              accept = c(".txt", ".csv"))
                )
            ),
              
            # Analysis button
            div(class = "analysis-button", style = "margin: 0 0 10px 0; text-align: center;",
                actionButton("go_analysis_button", "Run Analysis", style = "background-color: #34495e; border-color: #34495e; color: white;")
            )
        )
    )
  ),
    
  # Conditional panel: KEGG Enrichment results page
  conditionalPanel(
    condition = "input.currentPage == 'kegg_results'",
    # KEGG Enrichment results page
    div(id = "kegg-results-page", class = "enrichment-results-section", style = "width: 100%; padding: 10px; margin: 0;",
        h2("KEGG Enrichment Results", style = "text-align: center; width: 100%; margin: 12px 0; color: #2c3e50; font-weight: normal; font-size: 2.2em; font-family: Arial, Helvetica, sans-serif;"),
          
        # Back button
        div(class = "back-button", style = "text-align: center; margin: 5px 0; width: 100%;",
            actionButton("back_to_kegg", "Back to KEGG Enrichment", style = "background-color: #34495e; border-color: #34495e; color: white; padding: 4px 16px; font-size: 14px; margin-right: 10px;"),
            actionButton("back_to_home_kegg_results", "Back to Home", style = "background-color: #34495e; border-color: #34495e; color: white; padding: 4px 16px; font-size: 14px;")
        ),
          
        # Download button
        div(class = "download-button", style = "text-align: center; margin: 5px 0 10px 0; width: 100%;",
            downloadButton("download_kegg_results", "Download Results", class = "btn-primary")
        ),
          
        # Results display area
        div(class = "enrichment-results", style = "margin: 10px 0; width: 90%; max-width: 1200px; margin-left: auto; margin-right: auto;",
            DT::dataTableOutput("kegg_results")
        )
    )
  ),
    
  # Conditional panel: GO Enrichment results page
  conditionalPanel(
    condition = "input.currentPage == 'go_results'",
    # GO Enrichment results page
    div(id = "go-results-page", class = "enrichment-results-section", style = "width: 100%; padding: 10px; margin: 0;",
        h2("GO Enrichment Results", style = "text-align: center; width: 100%; margin: 12px 0; color: #2c3e50; font-weight: normal; font-size: 2.2em; font-family: Arial, Helvetica, sans-serif;"),
          
        # Back button
        div(class = "back-button", style = "text-align: center; margin: 5px 0; width: 100%;",
            actionButton("back_to_go", "Back to GO Enrichment", style = "background-color: #34495e; border-color: #34495e; color: white; padding: 4px 16px; font-size: 14px; margin-right: 10px;"),
            actionButton("back_to_home_go_results", "Back to Home", style = "background-color: #34495e; border-color: #34495e; color: white; padding: 4px 16px; font-size: 14px;")
        ),
          
        # Download button
        div(class = "download-button", style = "text-align: center; margin: 5px 0 10px 0; width: 100%;",
            downloadButton("download_go_results", "Download Results", class = "btn-primary")
        ),
          
        # Results display area
        div(class = "enrichment-results", style = "margin: 10px 0; width: 90%; max-width: 1200px; margin-left: auto; margin-right: auto;",
            DT::dataTableOutput("go_results")
        )
    )
  ),
    
  # Current page state (for conditional panels)
  div(style = "display: none;",
      textInput("currentPage", "", "home")
  ),
  
  # Conditional panel: Metabolic Node Sub-Graphs page
  conditionalPanel(
    condition = "input.currentPage == 'metabolic_subgraphs'",
    # Metabolic Node Sub-Graphs page
    div(id = "metabolic-subgraphs-page", class = "metabolic-subgraphs-section", style = "display: flex; flex-direction: column; align-items: center; width: 100%; padding: 10px 0;",
        h2("Metabolic Node Sub-Graphs", style = "text-align: center; width: 100%; margin: 12px 0; font-weight: normal; font-size: 2.2em; font-family: Arial, Helvetica, sans-serif;"),
          
        # Back button
        div(class = "back-button", style = "text-align: center; margin: 5px 0; width: 100%;",
            actionButton("back_to_home_metabolic", "Back to Home", style = "background-color: #34495e; border-color: #34495e; color: white; padding: 4px 16px; font-size: 14px;")
        ),
          
        # Pathway type selection
        div(class = "pathway-type-section", style = "text-align: center; width: 100%; margin: 5px 0 10px 0;",
            h3("Pathway", style = "text-align: center; margin: 0 0 6px 0;"),
            div(class = "radio-buttons", style = "display: inline-block; text-align: left;",
                radioButtons("pathway_type", "",
                             choices = list("Ethylene pathway" = "ethylene",
                                            "Lycopene pathway" = "lycopene"),
                             selected = "ethylene")
            )
        ),
          
        # Pathway visualization area
        div(class = "pathway-visualization", style = "width: 100%; height: 800px; margin: 0 auto; overflow: hidden;",
            htmlOutput("pathway_visualization")
        )
    )
  ),
  
  # Conditional panel: Methylation Sensitivity page
  conditionalPanel(
    condition = "input.currentPage == 'mc_preference'",
    # Methylation Sensitivity page
    div(id = "mc-preference-page", class = "mc-preference-section", style = "display: flex; flex-direction: column; align-items: center; width: 100%;",
        h2("Methylation Sensitivity", style = "text-align: center; width: 100%; margin: 12px 0; font-weight: normal; font-size: 2.2em; font-family: Arial, Helvetica, sans-serif;"),

        # Back button
        div(class = "back-button", style = "text-align: center; margin: 5px 0; width: 100%;",
            actionButton("back_to_home_mc_preference", "Back to Home", style = "background-color: #34495e; border-color: #34495e; color: white; padding: 4px 16px; font-size: 14px;")
        ),

        # Data type selection
        div(style = "text-align: center; width: 100%; margin: 5px 0 8px 0;",
            div(class = "radio-buttons", style = "display: inline-block; text-align: left;",
                radioButtons("mc_data_type", "",
                             choices = list("TF-based Methylation Sensitivity" = "tf",
                                            "Peak-based Methylation Sensitivity" = "peak"),
                             selected = "tf", inline = TRUE)
            )
        ),

        # TF-based Methylation Sensitivity
        conditionalPanel(
            condition = "input.mc_data_type == 'tf'",
            div(style = "display: flex; flex-direction: column; align-items: center; width: 100%;",
                # Search box and column selection
                div(class = "search-box", style = "margin: 5px 0 10px 0; display: flex; justify-content: center;",
                    div(style = "width: 900px; display: flex; gap: 10px; align-items: center;",
                        selectInput("mc_preference_search_column", "",
                                    choices = c("All", "CommonName", "GeneID", "Family", "DAP-methylated/DAP-unmethylated", "Amp-methylated/Amp-unmethylated", "Correlation_5mC_vs_BindingFC", "P_Value", "Group"),
                                    selected = "All",
                                    width = "620px"),
                        textInput("mc_preference_search", "", "",
                                  placeholder = "Search by CommonName, GeneID, Family, etc.",
                                  width = "100%")
                    )
                ),

                # Download button area
                div(class = "download-section", style = "width: 95%; max-width: 2000px; margin: 5px 0 10px 0; text-align: left;",
                    div(class = "dropdown", style = "display: inline-block;",
                        tags$button(class = "btn btn-primary dropdown-toggle",
                                    `data-toggle` = "dropdown",
                                    `aria-haspopup` = "true",
                                    `aria-expanded` = "false",
                                    "Download"),
                        div(class = "dropdown-menu",
                            actionLink("mc_download_all", HTML("Download All Data (<span id='mc_total_count'>0</span>)"), class = "dropdown-item"),
                            actionLink("mc_download_selected", HTML("Download Selected (<span id='mc_selected_count'>0</span>)"), class = "dropdown-item"),
                            actionLink("mc_download_filtered", HTML("Download Filtered (<span id='mc_filtered_count'>0</span>)"), class = "dropdown-item")
                        )
                    )
                ),

                # Data table
                div(class = "mc-preference-results", style = "margin: 10px 0; width: 100%; overflow-x: auto;",
                    DT::dataTableOutput("mc_preference_results")
                )
            )
        ),

        # Peak-based Methylation Sensitivity
        conditionalPanel(
            condition = "input.mc_data_type == 'peak'",
            div(style = "display: flex; flex-direction: column; align-items: center; width: 100%;",
                # Search box and column selection
                div(class = "search-box", style = "margin: 5px 0 10px 0; display: flex; justify-content: center;",
                    div(style = "width: 900px; display: flex; gap: 10px; align-items: center;",
                        selectInput("mc_peak_search_column", "",
                                    choices = c("All", "TFid", "motifchr", "motifstart", "motifend", "methylation_level", "DAPsignal", "ampDAP_signal", "DAPsignal_corr", "ampDAP_signal_corr"),
                                    selected = "All",
                                    width = "620px"),
                        textInput("mc_peak_search", "", "",
                                  placeholder = "Search by TFid, motifchr, etc.",
                                  width = "100%")
                    )
                ),

                # Download button area
                div(class = "download-section", style = "width: 95%; max-width: 2000px; margin: 5px 0 10px 0; text-align: left;",
                    div(class = "dropdown", style = "display: inline-block;",
                        tags$button(class = "btn btn-primary dropdown-toggle",
                                    `data-toggle` = "dropdown",
                                    `aria-haspopup` = "true",
                                    `aria-expanded` = "false",
                                    "Download"),
                        div(class = "dropdown-menu",
                            actionLink("mc_peak_download_all", HTML("Download All Data (<span id='mc_peak_total_count'>0</span>)"), class = "dropdown-item"),
                            actionLink("mc_peak_download_selected", HTML("Download Selected (<span id='mc_peak_selected_count'>0</span>)"), class = "dropdown-item"),
                            actionLink("mc_peak_download_filtered", HTML("Download Filtered (<span id='mc_peak_filtered_count'>0</span>)"), class = "dropdown-item")
                        )
                    )
                ),

                # Data table
                div(class = "mc-preference-results", style = "margin: 10px 0; width: 95%; max-width: 2070px;",
                    DT::dataTableOutput("mc_peak_results")
                )
            )
        )
    )
  ),

  # Hidden input for receiving gene info from iframe
  div(style = "display: none;",
      textInput("selected_gene", "", "")
  ),
  
  # Add JavaScript code to listen for hash changes in iframe
  tags$script(HTML("// Listen for hash changes in iframe
$(document).ready(function() {
  console.log('JavaScript loaded - Network Module');
  
  // Store last hash value to avoid duplicate triggers
  var lastHash = '';
  var iframeLoaded = false;
  var checkInterval = null;
  
  // Wait for iframe to load
  function checkIframeLoaded() {
    var iframe = $('iframe');
    console.log('Checking iframes found:', iframe.length);
    
    if (iframe.length > 0) {
      var foundNetworkIframe = false;
      
      // Iterate through all iframes to find the network visualization iframe
      iframe.each(function() {
        var src = $(this).attr('src');
        console.log('iframe src:', src);
        if (src && (src.indexOf('/physical_network/index.html') !== -1 || src.indexOf('/network_gephi/index.html') !== -1 || src.indexOf('/cor_network/index.html') !== -1 || src.indexOf('physical_network/index.html') !== -1 || src.indexOf('network_gephi/index.html') !== -1 || src.indexOf('cor_network/index.html') !== -1)) {
          console.log('Found network iframe:', this);
          foundNetworkIframe = true;
          
          // Wait for iframe to fully load
          $(this).on('load', function() {
            console.log('Network iframe loaded');
            iframeLoaded = true;
            startHashMonitoring(this);
          });
          
          // If iframe has already loaded
          if (this.contentWindow && this.contentWindow.document.readyState === 'complete') {
            console.log('Network iframe already loaded');
            iframeLoaded = true;
            startHashMonitoring(this);
          }
        }
      });
      
      if (!foundNetworkIframe) {
        console.log('Network iframe not found yet, will retry...');
        setTimeout(checkIframeLoaded, 1000);
      }
    } else {
      console.log('No iframes found yet, will retry...');
      setTimeout(checkIframeLoaded, 1000);
    }
  }
  
  // Start listening for hash changes
  function startHashMonitoring(iframeElement) {
    console.log('Starting hash monitoring...');
    
    // Clear previous timer
    if (checkInterval) {
      clearInterval(checkInterval);
    }
    
    // Listen for postMessage events (cross-origin communication)
    window.addEventListener('message', function(event) {
      // Verify message source
      if (event.source === iframeElement.contentWindow) {
        console.log('Received message from iframe:', event.data);
        
        if (event.data && event.data.type === 'nodeClick' && event.data.gene) {
          var gene = event.data.gene;
          // Extract the part before the space as gene ID
          if (gene.indexOf(' ') !== -1) {
            gene = gene.split(' ')[0];
          }
          // Handle URL-encoded spaces
          if (gene.indexOf('%20') !== -1) {
            gene = gene.split('%20')[0];
          }
          console.log('Node clicked, gene:', gene);
          
          // Update the hidden input value
          var selectedGeneInput = $('#selected_gene');
          if (selectedGeneInput.length > 0) {
            selectedGeneInput.val(gene);
            console.log('Updated selected_gene input to:', gene);
            
            // Trigger Shiny input event
            if (typeof Shiny !== 'undefined' && Shiny.onInputChange) {
              Shiny.onInputChange('selected_gene', gene);
              console.log('Triggered Shiny input change event for gene:', gene);
              // Update lastHash to avoid duplicate processing in periodic checks
              lastHash = '#' + gene;
            } else {
              console.log('Shiny not available');
            }
          } else {
            console.log('selected_gene input not found');
          }
        }
      }
    });
    
    // Send message to iframe requesting it to set up postMessage listener
    try {
      if (iframeElement.contentWindow) {
        iframeElement.contentWindow.postMessage({type: 'setupPostMessageListener'}, '*');
        console.log('Sent setup message to iframe');
      }
    } catch (e) {
      console.log('Error sending message to iframe:', e.message);
    }
    
    // Keep original periodic check as fallback
    checkInterval = setInterval(function() {
      try {
        // Get iframe contentWindow object
        var iframeWindow = iframeElement.contentWindow;
        
        if (!iframeWindow) {
          console.log('iframeWindow not available');
          return;
        }
        
        // Get iframe hash
        var hash = iframeWindow.location.hash;
        console.log('iframe hash:', hash);
        
        // If hash exists and is not empty, and differs from last time
        if (hash && hash !== '#' && hash !== lastHash) {
          // Extract gene name (remove # prefix)
          var gene = hash.substring(1);
          console.log('Raw gene label:', gene);
          
          // If gene name contains a comma, take only the part before the comma
          if (gene.indexOf(',') !== -1) {
            gene = gene.split(',')[0];
          }
          
          // Extract the part before the space as gene ID
          if (gene.indexOf(' ') !== -1) {
            gene = gene.split(' ')[0];
          }
          // Handle URL-encoded spaces
          if (gene.indexOf('%20') !== -1) {
            gene = gene.split('%20')[0];
          }
          
          // Remove any potential whitespace
          gene = gene.trim();
          
          console.log('Extracted gene ID:', gene);
          
          // Update last hash value
          lastHash = hash;
          
          // Update the hidden input value
          var selectedGeneInput = $('#selected_gene');
          if (selectedGeneInput.length > 0) {
            selectedGeneInput.val(gene);
            console.log('Updated selected_gene input to:', gene);
            
            // Trigger Shiny input event
            if (typeof Shiny !== 'undefined' && Shiny.onInputChange) {
              Shiny.onInputChange('selected_gene', gene);
              console.log('Triggered Shiny input change event for gene:', gene);
            } else {
              console.log('Shiny not available');
            }
          } else {
            console.log('selected_gene input not found');
          }
        }
      } catch (e) {
        // Cross-origin access may fail, ignore error
        console.log('Error accessing iframe:', e.message);
      }
    }, 500); // Check every 0.5 seconds
  }
  
  // Start checking iframe loading
  setTimeout(checkIframeLoaded, 1000);
});")
,

  # PDF zoom synchronization JavaScript code
  tags$script(HTML("document.addEventListener('DOMContentLoaded', function() {
    function syncPDFScale() {
      const pdfWrappers = document.querySelectorAll('.pdf-wrapper');
      const zoomLevel = window.devicePixelRatio || 1;
      
      pdfWrappers.forEach(wrapper => {
        const embed = wrapper.querySelector('embed');
        if (embed) {
          const originalWidth = wrapper.getAttribute('data-original-width') || wrapper.offsetWidth;
          const originalHeight = wrapper.getAttribute('data-original-height') || wrapper.offsetHeight;
          
          if (!wrapper.hasAttribute('data-original-width')) {
            wrapper.setAttribute('data-original-width', originalWidth);
            wrapper.setAttribute('data-original-height', originalHeight);
          }
          
          const currentWidth = wrapper.offsetWidth;
          const scaleX = currentWidth / originalWidth;
          const newHeight = originalHeight * scaleX;
          
          wrapper.style.height = newHeight + 'px';
          embed.style.width = currentWidth + 'px';
          embed.style.height = newHeight + 'px';
        }
      });
    }
    
    syncPDFScale();
    window.addEventListener('resize', syncPDFScale);
    
    let lastZoom = window.devicePixelRatio;
    setInterval(() => {
      const currentZoom = window.devicePixelRatio;
      if (Math.abs(currentZoom - lastZoom) > 0.01) {
        lastZoom = currentZoom;
        setTimeout(syncPDFScale, 100);
      }
    }, 500);
  });"))
)
)
