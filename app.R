#==============================================================================#
# Missing Data Scenarios in RCTs App                                           #
#==============================================================================#

library(shiny)
library(bslib)
library(shinyWidgets)
library(DiagrammeR)
library(dplyr)
library(stringr)
library(ggplot2)
library(gt)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 130)  # necessary for font size to stay consistent
sysfonts::font_add('Roboto', 'www/Roboto-Medium.ttf')

# ==== Setup ===================================================================

## --- Plots & Tables ----------------------------------------------------------

df_x <- arrow::read_parquet('data/data.parquet') |> filter(pred == 'x')
df_ypre <- arrow::read_parquet('data/data.parquet') |> filter(pred == 'ypre')

res <- arrow::read_parquet('data/results.parquet') |> 
  mutate(
    miss_mech = str_extract(full_spec, '(.+)\\.(.+)\\.(.+)\\.(.+)', 2),
    miss_var = str_extract(full_spec, '(.+)\\.(.+)\\.(.+)\\.(.+)', 3),
    miss_handle = str_extract(full_spec, '(.+)\\.(.+)\\.(.+)\\.(.+)', 1),
    pred = str_extract(full_spec, '(.+)\\.(.+)\\.(.+)\\.(.+)', 4),
    miss_handle = case_match(
      miss_handle,
      'cmplt' ~ '"True" Data', 'lwd' ~ 'LWD', 'mean_imp' ~ 'Mean Imp',
      'fiml' ~ 'FIML', 'mult_imp' ~ 'Mult Imp'
    )
  ) |> 
  select(-ends_with('x'))
res_x <- res |> 
  filter(pred == 'x') |> select(-int, -pred) |> relocate(miss_handle)
res_ypre <- res |> 
  filter(pred == 'ypre') |> select(-int, -pred) |> relocate(miss_handle)
rm(res)

## --- DAGs --------------------------------------------------------------------

df_node_x <- data.frame(
  id = rep(1:5, 2),
  label = c(
    "Income", "Pre-Treatment\nTest Score", "Probability\nMiss Test",
    "Treatment", "Test Score",
    "Income", "Pre-Treatment\nTest Score", "Probability\nMiss Income",
    "Treatment", "Test Score"
  ),
  miss_var = c(rep("y", 5), rep("x", 5))
)

df_node_ypre <- data.frame(
  id = rep(1:5, 2),
  label = c(
    "Pre-Treatment\nTest Score", "Income", "Probability\nMiss Test",
    "Treatment", "Test Score",
    "Pre-Treatment\nTest Score", "Income", "Probability\nMiss Pre-Test",
    "Treatment", "Test Score"
  ),
  miss_var = c(rep("y", 5), rep("x", 5))
)

df_edge <- bind_rows(
  data.frame(  # universal X
    from = c(1,1,2,4), to = c(5,2,5,5),
    line = c("", "dashed", "dashed", ""),
    miss_mech = "mcar",
    version = "x"
  ),
  data.frame(  # universal Y-pre
    from = c(1,2,2,4), to = c(5,1,5,5),
    line = c("", "dashed", "dashed", ""),
    miss_mech = "mcar",
    version = "ypre"
  ),
  data.frame(  # x
    from = 1, to = 3,
    line = "dashed",
    miss_mech = "x",
    version = "both"
  ),
  data.frame(  # x + treat
    from = c(1,4), to = c(3,3),
    line = c("dashed", "dashed"),
    miss_mech = "x_trt",
    version = "both"
  ),
  data.frame(  # y
    from = 2, to = 3,
    line = "dashed",
    miss_mech = "y",
    version = "both"
  ),
  data.frame(  # y + treat
    from = c(2,4), to = c(3,3),
    line = c("dashed", "dashed"),
    miss_mech = "y_trt",
    version = "both"
  )
)

make_dag <- function(df_node, df_edge) {
  create_graph() |> 
    add_nodes_from_table(table = df_node, label_col = label) |> 
    add_edges_from_table(
      table = df_edge,
      from_col = from,
      to_col = to,
      from_to_map = id_external
    ) |> 
    set_node_position(node = 2, x = 1, y = 1) |> 
    set_node_position(node = 3, x = 2, y = 2) |> 
    set_node_position(node = 1, x = 1, y = 3) |> 
    set_node_position(node = 4, x = 2.75, y = 3) |> 
    set_node_position(node = 5, x = 4, y = 2) |> 
    set_node_attrs(node_attr = shape, values = "rectangle") |> 
    set_node_attrs(node_attr = fixedsize, values = F) |> 
    set_node_attrs(color, "black") |> 
    set_node_attrs(fillcolor, "white") |> 
    set_edge_attrs(color, "black") |> 
    set_node_attrs(fontname, "Roboto") |> 
    set_node_attrs(fontsize, 13) |> 
    select_nodes(id %in% 2:3) |> 
    set_node_attrs_ws(style, "dashed") |> 
    clear_selection() |> 
    select_edges(conditions = line == "dashed") |> 
    set_edge_attrs_ws(style, "dashed") |> 
    clear_selection() |> 
    render_graph()
}

## --- Equations ---------------------------------------------------------------

df_fmla <- tidyr::expand_grid(
  miss_mech = c('mcar', 'x', 'x_trt', 'y', 'y_trt'),
  miss_var = c('y', 'x')
) |> 
  mutate(
    fmla_x = c(
      '$$P(miss_{test}) \\perp \\!\\!\\! \\perp test, income, treat$$',
      '$$P(miss_{income}) \\perp \\!\\!\\! \\perp income, test, treat$$',
      '$$P(miss_{test}) = f(income)$$',
      '$$P(miss_{income}) = f(income)$$',
      '$$P(miss_{test}) = f(income, treat)$$',
      '$$P(miss_{income}) = f(income, treat)$$',
      '$$P(miss_{test}) = f(test_{pre})$$',
      '$$P(miss_{income}) = f(test_{pre})$$',
      '$$P(miss_{test}) = f(test_{pre}, treat)$$',
      '$$P(miss_{income}) = f(test_{pre}, treat)$$'
    ),
    fmla_ypre = c(
      '$$P(miss_{test}) \\perp \\!\\!\\! \\perp test, test_{pre}, treat$$',
      '$$P(miss_{test_{pre}}) \\perp \\!\\!\\! \\perp test_{pre}, test, treat$$',
      '$$P(miss_{test}) = f(test_{pre})$$',
      '$$P(miss_{test_{pre}}) = f(test_{pre})$$',
      '$$P(miss_{test}) = f(test_{pre}, treat)$$',
      '$$P(miss_{test_{pre}}) = f(test_{pre}, treat)$$',
      '$$P(miss_{test}) = f(test)$$',
      '$$P(miss_{test_{pre}}) = f(test)$$',
      '$$P(miss_{test}) = f(test, treat)$$',
      '$$P(miss_{test_{pre}}) = f(test, treat)$$'
    )
  )

## --- Summary Table -----------------------------------------------------------

df_tbl_summ <- readr::read_csv(
  'data/rct_missingness_summary.csv', show_col_types = F, progress = F
)

# ==== User Interface ==========================================================

ui <- page_navbar(
  navbar_options = list(bg = "#202123", theme = 'dark'),
  header = tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  theme = bs_theme(
    version = 5,
    base_font = font_face(
      family = "Roboto",
      src = "url('../Roboto-Medium.ttf') format('truetype')"
    ),
    heading_font = font_face(
      family = "Roboto",
      src = "url('../Roboto-Medium.ttf') format('truetype')"
    )
  ),
  title = "Missing Data Scenarios in RCTs",
  nav_spacer(),
  selected = 'No baseline outcome in model',
  
  ## --- Sidebar ---------------------------------------------------------------
  
  sidebar = sidebar(
    prettyRadioButtons(
      inputId = "miss_var",
      label = "Missing Var",
      choices = c("Y", "X"),
      selected = "Y",
      status = 'success'
    ),
    prettyRadioButtons(
      inputId = "miss_mech",
      label = "Missingness Function",
      choices = c("Random", "X", "X + Treat", 'Y', 'Y + Treat'),
      selected = "Random",
      status = 'success'
    ),
    prettyRadioButtons(
      inputId = "miss_handle",
      label = "Approach",
      choices = c('"True" Data', "LWD", "Mean Imp", "FIML", "Mult Imp"),
      selected = '"True" Data',
      status = 'success'
    ),
    width = 160,
    bg = "#483D8B",
  ),
  
  ## --- Content ---------------------------------------------------------------
  
  nav_panel('Overview', includeMarkdown('overview.md')),
  nav_panel(
    "No baseline outcome in model",
    layout_columns(
      card(plotOutput("plot_x")),
      card(gt_output("tbl_x")),
      uiOutput("fmla_x", fill = T),
      grVizOutput('dag_x', width = "auto"),
      col_widths = c(8, 4, -1, 5, 5, -1),
      row_heights = c(5, 2)
    ),
  ),
  nav_panel(
    "Baseline outcome in model",
    layout_columns(
      card(plotOutput("plot_ypre")),
      card(gt_output("tbl_ypre")),
      uiOutput("fmla_ypre", fill = T),
      grVizOutput('dag_ypre'),
      col_widths = c(8, 4, -1, 5, 5, -1),
      row_heights = c(5, 2)
    ),
  ),
  nav_panel(
    "Summary & Recs",
    textOutput('summ_text'),
    gt_output("tbl_summ")
  )
)

# ==== Server ==================================================================

server <- function(input, output) {
  
  ## --- Page 1: Income Model --------------------------------------------------
  
  ### --- Plot -----------------------------------------------------------------
  
  output$plot_x <- renderPlot({
    # Filters
    df_x_plt <- switch(
      input$miss_mech,
      "Random" = filter(df_x, miss_mech == "mcar"),
      "X" = filter(df_x, miss_mech == "x_rand"),
      "X + Treat" = filter(df_x, miss_mech == "x_trt"),
      "Y" = filter(df_x, miss_mech == "ypre"),
      "Y + Treat" = filter(df_x, miss_mech == "ypre_trt"),
    )
    df_x_plt <- switch(
      input$miss_var,
      "Y" = filter(df_x_plt, miss_var == "y"),
      "X" = filter(df_x_plt, miss_var == "x")
    )
    df_x_plt <- switch(
      input$miss_handle,
      '"True" Data' = filter(df_x_plt, miss_handle == "cmplt"),
      "LWD" = filter(df_x_plt, miss_handle == "lwd"),
      "Mean Imp" = filter(df_x_plt, miss_handle == "mean_imp"),
      "FIML" = filter(df_x_plt, miss_handle == "fiml"),
      "Mult Imp" = filter(df_x_plt, miss_handle == "mult_imp")
    )
    # Plot
    ggplot(
      filter(df_x_plt, !is.na(x_plt), !is.na(y_plt)), 
      aes(x_plt, y_plt, color = factor(treat, levels = c(1, 0)), shape = factor(miss))
    ) +
      geom_point() +
      geom_abline(
        intercept = mean(df_x_plt$int), slope = mean(df_x_plt$coef_x), 
        color = "deepskyblue4", lwd = 1
      ) +
      geom_abline(
        intercept = mean(df_x_plt$int) + mean(df_x_plt$coef_treat), 
        slope = mean(df_x_plt$coef_x), 
        color = "deeppink3", lwd = 1
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        text = element_text(size = 20, family = "Roboto"),
        legend.title = element_blank()
      ) +
      scale_x_continuous(
        labels = scales::dollar_format(suffix = "K"), 
        breaks = c(0, 25, 75, 125),
        limits = c(10, 160)
      ) +
      scale_color_discrete(
        type = c("deeppink", "deepskyblue2"),
        labels = c("Treatment", "Control")
      ) +
      scale_shape_manual(
        values = c(16, 4),
        labels = c("Observed", "Missing")
      ) +
      ylim(0, 25.8) +
      ylab("Test Score") +
      xlab("Income")
  })
  
  ### --- Table ----------------------------------------------------------------
  
  output$tbl_x <- render_gt({
    # Filters
    outputArgs = list(
      res_x_tbl <- switch(
        input$miss_mech,
        "Random" = filter(res_x, miss_mech == "mcar"),
        "X" = filter(res_x, miss_mech == "x"),
        "X + Treat" = filter(res_x, miss_mech == "x_trt"),
        "Y" = filter(res_x, miss_mech == "ypre"),
        "Y + Treat" = filter(res_x, miss_mech == "ypre_trt"),
      ),
      res_x_tbl <- switch(
        input$miss_var,
        "Y" = filter(res_x_tbl, miss_var == "y"),
        "X" = filter(res_x_tbl, miss_var == "x")
      ),
      res_x_tbl <- switch(
        input$miss_handle,
        '"True" Data' = mutate(res_x_tbl, bld = ifelse(miss_handle == '"True" Data', 1, 0)),
        "LWD" = mutate(res_x_tbl, bld = ifelse(miss_handle == 'LWD', 1, 0)),
        "Mean Imp" = mutate(res_x_tbl, bld = ifelse(miss_handle == 'Mean Imp', 1, 0)),
        "FIML" = mutate(res_x_tbl, bld = ifelse(miss_handle == 'FIML', 1, 0)),
        "Mult Imp" = mutate(res_x_tbl, bld = ifelse(miss_handle == 'Mult Imp', 1, 0))
      )
    )
    # Table
    expr = gt(res_x_tbl) |> 
      tab_style(
        cell_fill(color = "#94EFD7"),
        cells_body(rows = bld == 1)
      ) |> 
      tab_style(
        cell_text(weight = 'bold'),
        cells_body(rows = bld == 1)
      ) |> 
      tab_style(
        cell_fill(color = "#B2B2FA"),
        cells_body(rows = miss_handle == '"True" Data')
      ) |> 
      cols_hide(c(full_spec, miss_mech, miss_var, bld)) |> 
      cols_label(
        miss_handle = 'Approach', coef_treat = 'ATE', se_treat = 'SE'
      ) |> 
      fmt_number(decimals = 2) |> 
      cols_width(miss_handle ~ 120) |> 
      opt_table_font("Roboto") |> 
      tab_header('Effect Estimates')
  })
  
  ### --- Equation -------------------------------------------------------------
  
  output$fmla_x <- renderUI({
    # Filters
    df_fmla_x <- switch(
      input$miss_mech,
      "Random" = filter(df_fmla, miss_mech == 'mcar'),
      "X" = filter(df_fmla, miss_mech == 'x'),
      "X + Treat" = filter(df_fmla, miss_mech == 'x_trt'),
      "Y" = filter(df_fmla, miss_mech == 'y'),
      "Y + Treat" = filter(df_fmla, miss_mech == 'y_trt')
    )
    df_fmla_x <- switch(
      input$miss_var,
      "Y" = filter(df_fmla_x, miss_var == 'y'),
      "X" = filter(df_fmla_x, miss_var == 'x'),
    )
    # Formula
    withMathJax(df_fmla_x$fmla_x)
  })
  
  ### --- DAG ------------------------------------------------------------------
  
  output$dag_x <- renderGrViz({
    # Filter
    df_edge_x <- switch(
      input$miss_mech,
      "Random" = filter(
        df_edge, miss_mech == 'mcar' & version == 'x'
      ),
      "X" = filter(
        df_edge, miss_mech %in% c('mcar', 'x') & version %in% c('x', 'both')
      ),
      "X + Treat" = filter(
        df_edge, miss_mech %in% c('mcar', 'x_trt') & version %in% c('x', 'both')
      ),
      "Y" = filter(
        df_edge, miss_mech %in% c('mcar', 'y') & version %in% c('x', 'both')
      ),
      "Y + Treat" = filter(
        df_edge, miss_mech %in% c('mcar', 'y_trt') & version %in% c('x', 'both')
      ),
    )
    df_node_x_dgr <- switch(
      input$miss_var,
      "Y" = filter(df_node_x, miss_var == 'y'),
      "X" = filter(df_node_x, miss_var == 'x'),
    )
    # DAG
    make_dag(df_node_x_dgr, df_edge_x)
  })
  
  ## --- Page 2: Baseline Outcome Model ----------------------------------------
  
  ### --- Plot -----------------------------------------------------------------
  
  output$plot_ypre <- renderPlot({
    # Filters
    df_ypre_plt <- switch(
      input$miss_mech,
      "Random" = filter(df_ypre, miss_mech == "mcar"),
      "X" = filter(df_ypre, miss_mech == "ypre"),
      "X + Treat" = filter(df_ypre, miss_mech == "ypre_trt"),
      "Y" = filter(df_ypre, miss_mech == "y_rand"),
      "Y + Treat" = filter(df_ypre, miss_mech == "y_trt"),
    )
    df_ypre_plt <- switch(
      input$miss_var,
      "Y" = filter(df_ypre_plt, miss_var == "y"),
      "X" = filter(df_ypre_plt, miss_var == "ypre")
    )
    df_ypre_plt <- switch(
      input$miss_handle,
      '"True" Data' = filter(df_ypre_plt, miss_handle == "cmplt"),
      "LWD" = filter(df_ypre_plt, miss_handle == "lwd"),
      "Mean Imp" = filter(df_ypre_plt, miss_handle == "mean_imp"),
      "FIML" = filter(df_ypre_plt, miss_handle == "fiml"),
      "Mult Imp" = filter(df_ypre_plt, miss_handle == "mult_imp")
    )
    # Plot
    ggplot(
      filter(df_ypre_plt, !is.na(ypre_plt), !is.na(y_plt), ypre_plt >= 0), 
      aes(ypre_plt, y_plt, color = factor(treat, levels = c(1, 0)), shape = factor(miss))
    ) +
      geom_point() +
      geom_abline(
        intercept = mean(df_ypre_plt$int), slope = mean(df_ypre_plt$coef_x), 
        color = "deepskyblue4", lwd = 1
      ) +
      geom_abline(
        intercept = mean(df_ypre_plt$int) + mean(df_ypre_plt$coef_treat), 
        slope = mean(df_ypre_plt$coef_x), 
        color = "deeppink3", lwd = 1
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        text = element_text(size = 20, family = "Roboto"),
        legend.title = element_blank()
      ) +
      scale_x_continuous(breaks = c(0, 10, 20), limits = c(0, 20)) +
      scale_color_discrete(
        type = c("deeppink", "deepskyblue2"),
        labels = c("Treatment", "Control")
      ) +
      scale_shape_manual(
        values = c(16, 4),
        labels = c("Observed", "Missing")
      ) +
      ylim(0, 25.8) + 
      ylab("Test Score") +
      xlab("Pre-Treatment Test Score")
  })
  
  ### --- Table ----------------------------------------------------------------
  
  output$tbl_ypre <- render_gt({
    # Filters
    outputArgs = list(
      res_ypre_tbl <- switch(
        input$miss_mech,
        "Random" = filter(res_ypre, miss_mech == "mcar"),
        "X" = filter(res_ypre, miss_mech == "ypre"),
        "X + Treat" = filter(res_ypre, miss_mech == "ypre_trt"),
        "Y" = filter(res_ypre, miss_mech == "y_rand"),
        "Y + Treat" = filter(res_ypre, miss_mech == "y_trt"),
      ),
      res_ypre_tbl <- switch(
        input$miss_var,
        "Y" = filter(res_ypre_tbl, miss_var == "y"),
        "X" = filter(res_ypre_tbl, miss_var == "ypre")
      ),
      res_ypre_tbl <- switch(
        input$miss_handle,
        '"True" Data' = mutate(res_ypre_tbl, bld = ifelse(miss_handle == '"True" Data', 1, 0)),
        "LWD" = mutate(res_ypre_tbl, bld = ifelse(miss_handle == 'LWD', 1, 0)),
        "Mean Imp" = mutate(res_ypre_tbl, bld = ifelse(miss_handle == 'Mean Imp', 1, 0)),
        "FIML" = mutate(res_ypre_tbl, bld = ifelse(miss_handle == 'FIML', 1, 0)),
        "Mult Imp" = mutate(res_ypre_tbl, bld = ifelse(miss_handle == 'Mult Imp', 1, 0))
      )
    )
    # Table
    expr = gt(res_ypre_tbl) |> 
      tab_style(
        cell_fill(color = "#94EFD7"),
        cells_body(rows = bld == 1)
      ) |> 
      tab_style(
        cell_text(weight = 'bold'),
        cells_body(rows = bld == 1)
      ) |> 
      tab_style(
        cell_fill(color = "#B2B2FA"),
        cells_body(rows = miss_handle == '"True" Data')
      ) |> 
      cols_hide(c(full_spec, miss_mech, miss_var, bld)) |> 
      cols_label(
        miss_handle = 'Approach', coef_treat = 'ATE', se_treat = 'SE'
      ) |> 
      fmt_number(decimals = 2) |> 
      cols_width(miss_handle ~ 120) |> 
      opt_table_font("Roboto") |> 
      tab_header('Effect Estimates')
  })
  
  ### --- Equation -------------------------------------------------------------
  
  output$fmla_ypre <- renderUI({
    # Filters
    df_fmla_ypre <- switch(
      input$miss_mech,
      "Random" = filter(df_fmla, miss_mech == 'mcar'),
      "X" = filter(df_fmla, miss_mech == 'x'),
      "X + Treat" = filter(df_fmla, miss_mech == 'x_trt'),
      "Y" = filter(df_fmla, miss_mech == 'y'),
      "Y + Treat" = filter(df_fmla, miss_mech == 'y_trt')
    )
    df_fmla_ypre <- switch(
      input$miss_var,
      "Y" = filter(df_fmla_ypre, miss_var == 'y'),
      "X" = filter(df_fmla_ypre, miss_var == 'x'),
    )
    # Formula
    withMathJax(df_fmla_ypre$fmla_ypre)
  })
  
  ### --- DAG ------------------------------------------------------------------
  
  output$dag_ypre <- renderGrViz({
    # Filter
    df_edge_ypre <- switch(
      input$miss_mech,
      "Random" = filter(
        df_edge, miss_mech == 'mcar' & version == 'ypre'
      ),
      "X" = filter(
        df_edge, miss_mech %in% c('mcar', 'x') & version %in% c('ypre', 'both')
      ),
      "X + Treat" = filter(
        df_edge, miss_mech %in% c('mcar', 'x_trt') & version %in% c('ypre', 'both')
      ),
      "Y" = filter(
        df_edge, miss_mech %in% c('mcar', 'y') & version %in% c('ypre', 'both')
      ),
      "Y + Treat" = filter(
        df_edge, miss_mech %in% c('mcar', 'y_trt') & version %in% c('ypre', 'both')
      ),
    )
    df_node_ypre_dgr <- switch(
      input$miss_var,
      "Y" = filter(df_node_ypre, miss_var == 'y'),
      "X" = filter(df_node_ypre, miss_var == 'x'),
    )
    # DAG
    make_dag(df_node_ypre_dgr, df_edge_ypre)
  })
  
  ## --- Page 3: Summary -------------------------------------------------------
  
  ### --- Text -----------------------------------------------------------------
  
  output$summ_text <- renderText({
    'The table below compares the performance of each missing data handling approach in identifying the treatment effect under each missingness scenario considered. Keep in mind that this is a high-level overview of a simplistic simulation. "Good" does not necessarily mean unbiased, just that it came reasonably close to estimating the correct ATE.'
  })
  
  ### --- Table ----------------------------------------------------------------
  
  output$tbl_summ <- render_gt({
    gt(df_tbl_summ) |> 
      opt_table_font('Roboto') |> 
      tab_style(
        list(cell_fill(color = '#FFBF00'), cell_text(align = 'center')),
        cells_body(columns = 4:7)
      ) |> 
      tab_style_body(
        list(cell_fill(color = 'seagreen3'), cell_text(align = 'center')),
        values = 'good'
      ) |> 
      tab_style_body(
        list(cell_fill(color = '#DE3163'), cell_text(align = 'center')),
        values = 'bad'
      ) |> 
      tab_style(
        list(cell_text(weight = 'bold', align = 'center')), 
        cells_column_labels()
      ) |> 
      tab_style(
        cell_borders("all", style = 'hidden'), 
        cells_body(columns = 4:7)
      ) |>
      tab_style(
        cell_borders("top"),
        cells_body(columns = 4:7, rows = 1)
      ) |> 
      tab_style(
        cell_borders("bottom"),
        cells_body(columns = 4:7, rows = nrow(df_tbl_summ))
      ) |> 
      sub_missing(missing_text = '')
  })

}

shinyApp(ui = ui, server = server)