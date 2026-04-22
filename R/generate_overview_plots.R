library(ggplot2)
library(plotly)


# Script for generating plots for the Overview  tab panel-----------------------------------------------------------------------------------

generate_open_access_pie <- function(expanded_works, output_path, output) {
  if (!file.exists(output_path)) {
    table_open_access <- table(expanded_works$nodes$is_oa)
    pie_data <- data.frame(
      category = c("Limited Access", "Open Access"),
      value = as.numeric(table_open_access)
    )
    total_count <- sum(pie_data$value)
    
    # Save just the data
    saveRDS(list(pie_data = pie_data, total_count = total_count), output_path)
  }
  
  open_access_data <- readRDS(output_path)
  
  output$open_access_pie <- renderPlotly({
    pie_data <- open_access_data$pie_data
    total_count <- open_access_data$total_count
    hover_text <- paste("See", pie_data$value, "references")
    
    plot_ly(
      pie_data,
      labels = ~category,
      values = ~value,
      type = 'pie',
      textinfo = 'percent+label',
      text = hover_text,
      hoverinfo = 'text',
      textposition = 'outside',
      hole = 0.4,
      textfont = list(size = 18),
      marker = list(
        colors = col_diverging_scale(4)[c(2, 3)],
        line = list(color = "black", width = 1.5)
      ),
      source = "open_access_pie"
    ) %>%
      layout(
        showlegend = FALSE,
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        annotations = list(
          text = paste(total_count, "References", sep = "\n"),
          x = 0.5, y = 0.5,
          font = list(size = 22),
          showarrow = FALSE
        )
      ) %>%
      event_register("plotly_click")
  })
  
  #return(output$open_access_pie)
}

generate_data_year_barplot <- function(expanded_works, output_path, output) {
  
  if (!file.exists(output_path)) {
    years <- as.numeric(format(expanded_works$nodes$publication_date, "%Y"))
    breaks <- seq(min(years), max(years), by = 1)
    
    h <- hist(years, breaks = breaks, plot = FALSE)
    
    mids <- h$mids
    counts <- h$counts
    breaks <- h$breaks
    
    # Save intermediate data
    saveRDS(list(
      mids = mids,
      counts = counts,
      breaks = breaks
    ), output_path)
  }
  
  year_data <- readRDS(output_path)
  mids <- year_data$mids
  counts <- year_data$counts
  
  df_plot <- data.frame(
    year = floor(mids),
    count = counts
  )
  
  # Replace 0s with NA to avoid log-scale display issues
  df_plot$count[df_plot$count == 0] <- NA
  
  output$data_year_barplot <- renderPlotly({
    plot_ly(
      data = df_plot,
      x = ~year,
      y = ~count,
      type = 'bar',
      marker = list(
        color = col_oasis[2],
        line = list(color = "black", width = 1.2)
      ),
      hoverinfo = "x+y",
      source = "data_year_barplot"   
    ) %>%
      layout(
        title = "",
        xaxis = list(title = "Year", tickangle = -45),
        yaxis = list(title = "Number of Publications", type = "log", autorange = TRUE),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      event_register("plotly_click")
  })
  
}

generate_data_type_pie <- function(expanded_works, output_path, output) {
  
  if (!file.exists(output_path)) {
    table_type <- as.data.frame(table(expanded_works$nodes$type))
    colors <- col_diverging_scale(18)
    table_type$colors <- colors[1:nrow(table_type)]
    total_count <- sum(table_type$Freq)
    
    saveRDS(list(
      pie_data = table_type,
      total_count = total_count
    ), output_path)
  }
  
  data_type_data <- readRDS(output_path)
  
  output$data_type_pie <- renderPlotly({
    pie_data <- data_type_data$pie_data
    total_count <- data_type_data$total_count
    
    plot_ly(
      pie_data,
      labels = ~paste0(Var1, ": ",Freq, " (", round(Freq / total_count * 100,1), "%)"),
      values = ~Freq,
      type = 'pie',
      textinfo = 'none',
      hoverinfo = 'label+values',
      hole = 0.4,
      height = 400,
      marker = list(
        colors = pie_data$colors,
        line = list(color = "black", width = 1.5)
      ),
      customdata = ~Var1,
      source = "data_type_pie"  # 👈 for plotly_click tracking
    ) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 20, b = 20, l = 80, r = 0),
        plot_bgcolor = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        legend = list(
          x = 0.8, y = 0.5,
          xanchor = "left",
          yanchor = "middle",
          font = list(size = 14)
        ),
        annotations = list(
          text = paste(total_count, "Data Types", sep = "\n"),
          x = 0.5, y = 0.5,
          font = list(size = 22),
          showarrow = FALSE
        )
      ) %>%
      event_register("plotly_click")
  })
}

generate_source_tree <- function(expanded_works, output_path, output) {
  if (!file.exists(output_path)) {
    table_journals <- as.data.frame(table(expanded_works$nodes$source_display_name))
    sorted_journals <- table_journals[order(-table_journals$Freq), ]
    top_journals <- head(sorted_journals, 100)  # Top 100 sources
    colnames(top_journals) <- c("source", "count")  # 👈 important: use `source` as column name
    
    top_journals$label <- paste0(top_journals$source)
    
    saveRDS(list(
      treemap_data = top_journals
    ), output_path)
  }
  
  # Load intermediate data
  data_tree <- readRDS(output_path)
  source_data <- data_tree$treemap_data
  
  
  
  output$source_tree <- renderPlotly({
    plot_ly(
      type = "treemap",
      labels = source_data$label,
      parents = rep("", nrow(source_data)),  # flat hierarchy
      values = source_data$count,
      textinfo = "label",
      marker = list(colors = col_oasis_scale(nrow(source_data))),
      textfont = list(size = 18),
      customdata = source_data$source,
      source = "source_tree"
    ) %>%
      layout(
        margin = list(t = 1, l = 20, r = 20, b = 20),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)", pathbar = list(visible = FALSE) 
      ) %>%
      event_register("plotly_click")
  })
  
}



#Fourth row, global map of author affiliations
generate_global_map_institute <- function(expanded_works, output_path) {
  
  all_country_codes <- unlist(
    lapply(expanded_works$nodes$authorships, function(authorship) {
      if (is.list(authorship) && !is.null(authorship$affiliations)) {
        lapply(authorship$affiliations, function(aff) {
          if (!is.null(aff$country_code)) aff$country_code else NA_character_
        })
      } else {
        NA_character_
      }
    })
  )
  
  all_country_codes <- all_country_codes[!is.na(all_country_codes)]
  country_data <- as.data.frame(table(all_country_codes))
  colnames(country_data) <- c("iso_a2_eh", "count")
  
  world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  merged_data <- dplyr::left_join(world_map, country_data, by = "iso_a2_eh")
  
  country_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = merged_data,
                     aes(fill = count, text = paste(iso_a2_eh, ":", count)),
                     color = "#333", size = 0.1, na.rm = TRUE) +
    ggplot2::scale_fill_gradientn(colors = col_oasis[2:1], na.value = "gray", name = "Count") +
    ggplot2::coord_sf(crs = "+proj=eqearth") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.grid.minor = ggplot2::element_line(color = "#333", size = 0.3),
      plot.margin = ggplot2::margin(-20, 0, 0, 0)
    )
  
  plotly_obj <- plotly::ggplotly(country_plot, tooltip = "text") %>%
    plotly::layout(
      autosize = TRUE,
      margin = list(t = 0, l = 0, r = 0, b = 0)
    )
  
  saveRDS(plotly_obj, output_path)
}
# Fifth row, bar plot displaying 30 most abundant sources and their frequency

generate_treemap_journals <- function(expanded_works, output_path) {
  # Count journals
  table_journals <- as.data.frame(table(expanded_works$nodes$source_display_name))
  sorted_journals <- table_journals[order(-table_journals$Freq), ]
  top_journals <- head(sorted_journals, 100)
  colnames(top_journals) <- c("Journal", "Count")
  
  # Create PNG file
  png(output_path, width = 1000, height = 500, bg = "transparent")
  
  # Generate treemap
  treemap(
    top_journals,
    index = "Journal",
    vSize = "Count",
    type = "index",
    title = "Top 100 Sources",
    fontsize.labels = 12,
    fontcolor.labels = "black",
    bg.labels = "#FFFFFFA0",  # semi-transparent background for readability
    align.labels = list(c("center", "center")),
    border.col = "white",
    palette = col_oasis_scale(100)
  )
  
  dev.off()
}

generate_wordcloud_topics <- function(expanded_works, output_path) {
  all_topics <- unlist(
    lapply(expanded_works$nodes$topics, function(topic) {
      if (!is.null(topic$display_name)) topic$display_name else NA_character_
    })
  )
  
  all_topics <- all_topics[!is.na(all_topics)]
  table_topics <- as.data.frame(table(all_topics))
  sorted_topics <- table_topics[order(-table_topics$Freq), ]
  colnames(sorted_topics) <- c("word", "freq")
  
  saveRDS(sorted_topics, output_path)
}





# End Statistics for Overview  -----------------------------------------------------------------------------------------------------------
