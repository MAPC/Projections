# Queries the Census API to retrieve PUMS data.
# var.list: list of variables to include in the data query.
# yr: end year of the survey.
# srvy: one year or five year PUMS. Use either "acs1" or "acs5".
pums_query <- function(var.list, yr, srvy){
  get_pums(
    variables = var.list,
    state = "MA",
    year = yr,
    survey = srvy
  )
}

# Various helper functions for sorting unique data
sun <- function(x){
  sort(unique(x))}
lun <- function(x){
  length(unique(x))}
sna <- function(x){
  sort(names(x))}

# Visualization functions
# Single Variable Bar Charts

sv.bar.chart_ <- function(df, x_var, y_var, x_label, y_label, custom_title, geo){
  
  graph <- ggplot(data = get(df), aes(x = get(x_var), y = get(y_var))) +
    geom_col(
      state = "identity",
      width = .5,
      fill = "lavender",
      color = "black"
    ) +
    geom_text(aes(label = scales::comma(get(y_var))), vjust = -1) +
    labs(
      title = paste0(custom_title, ", ", geo ," (2020-2050)"),
      x = x_label,
      y = y_label,
      caption = "Source: MAPC Data Services - Research Division"
    ) +
    scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, .1))) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        size = 1
      ),
      plot.title = element_text(color = "black", size = 18),
      axis.title.x = element_text(color = "black", size = 16),
      axis.title.y = element_text(color = "black", size = 16),
      axis.text.x = element_text(color = "black", size = 14),
      axis.text.y = element_text(color = "black", size = 14)
    )
  
  return(graph)
  
}

# Multi-Variable Stacked Bar Charts

mv.bar.chart_ <- function(df, x_var, y_var, group_var, x_label, y_label, group_label, custom_title, geo){
  
  graph <- ggplot(data = get(df), aes(fill = get(group_var), x = get(x_var), y = get(y_var))) +
    geom_col(
      position = "stack",
      stat = "identity",
      width = .5,
      # fill = group_var,
      color = "black"
    ) +
    geom_text(aes(label = scales::comma(get(y_var))), vjust = -1, position = position_dodge(.9)) +
    labs(
      title = paste0(custom_title, ", ", geo ," (2020-2050)"),
      x = x_label,
      y = y_label,
      fill = group_label,
      caption = "Source: MAPC Data Services - Research Division"
    ) +
    scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, .1))) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        size = 1
      ),
      plot.title = element_text(color = "black", size = 18),
      axis.title.x = element_text(color = "black", size = 16),
      axis.title.y = element_text(color = "black", size = 16),
      axis.text.x = element_text(color = "black", size = 14),
      axis.text.y = element_text(color = "black", size = 14)
    )
  
  return(graph)
  
}

# Single Variable Line Charts

sv.line.chart <- function(){
  
}

# Multivariable Line Chart
mv.line.chart <- function(){
  
}