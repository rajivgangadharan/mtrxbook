# common.R - Contains common methods for mtrxbook-minimal bookdown project
#
# Rajiv Gangadharan <rajiv.gangadharan@gmail.com>
# Copyright 2021 Rajiv Gangadharan <rajiv.gangadharan@gmail.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#  MA 02110-1301, USA.


library(dplyr)
library(finmetrics)
library(ggplot2)
library(viridis)
library(here)
library(roxygen2)
library(scales)
library(plotly)
library(shiny)


abnormalities.drop <- function(tib) {
  tib %>% filter(cldt < crdt)
}

non_abnormalities.select <- function(tib) {
  filter(tib, cldt > crdt)
}

# Viridis scale
viridis_indigo=       '#440154FF'
viridis_blue=         '#33638DFF'
viridis_green=        '#29AF7FFF'
viridis_greenyellow = '#87D549FF' 
viridis_yellow=       '#DCE319FF'

cbPalette <- c("#D55E00", "#E69F00", "#56B4E9",  "#F0E442", "#009E73", "#0072B2", "#CC79A7", "#999999")

cbPalette_cols <- c(
  "Critical" = cbPalette[1],
  "High" = cbPalette[2],
  "Medium" = cbPalette[3],
  "Low" = cbPalette[4],
  "None" = cbPalette[8]
)

viridis_cols <- c(
  "Critical" = viridis_indigo,
  "High" = viridis_blue,
  "Medium" = viridis_green,
  "Low" = viridis_greenyellow,
  "None" = viridis_yellow
)

# The variables color_scale, color_scale_line will control the colors that is used for manual fill
# Change the values option for a different color scheme
colour_scale <- scale_color_manual(values = cbPalette_cols,
                                   aesthetics = c("colour", "fill"),
                                   na.value = "grey50")

colour_scale_line <- scale_linetype_manual(values = cbPalette_cols, 
                                           aesthetics = c("colour", "fill"),
                                           breaks = waiver(), 
                                           na.value = "blank")
plotly_color_scale <- cbPalette_cols


violin_plot.CycleTimeVsPriority <- function(tib, pal_option = 'D') {
  # Need to pass tib with Priority as a factor variable
  tib$Priority <- as.factor(tib$Priority)
  plt <- ggplot(tib, aes(x = Priority, y = cylt, fill = Priority)) +
    geom_violin(draw_quantiles = c(0.5), alpha = 0.25) +
    theme_minimal() + theme(legend.position = "bottom")
  
  ifelse(hasArg(pal_option),
    plt <- plt + scale_fill_viridis(discrete = TRUE, option = pal_option),
    plt <- plt + colour_scale)
  
  plt
}



line_plot.NumClosed_For_FloorDate <-
  function(tib,
           plt_caption = "Closed per Week",
           pal_option = 'D',
           show_trend = FALSE) {
    plt <- ggplot(tib,
                  aes(x = as.Date(FloorDate),
                      y = NumClosed)) +
      geom_line() +
      geom_point() +
      xlab("Reporting Period") + ylab("Count of work items") +
      scale_x_date(date_labels = "%b/%y", date_breaks = "8 weeks") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(caption = plt_caption)
 
    # If trend line is opted for
    ifelse (show_trend == TRUE,
            plt <- plt + geom_smooth(formula = 'y~x', 
                                     se = FALSE, 
                                     method = "loess"),
            plt )
    
    # Color Scale
    ifelse(
      hasArg(pal_option),
      plt <-
        plt + scale_color_viridis(discrete = TRUE, option = pal_option),
      plt <- plt + colour_scale
    )
    
    plt
  }

# Alias to align with function
line_plot.Throughput <- line_plot.NumClosed_For_FloorDate

#' @name compute.FloorDateBased.Aggregates
#' @title Generates a tibble with the Date, Priority and Number of Work Items
#' Closed
#' @description Receives a tibble with complete cases (closed cases) and then
#' generates a tibble with the Date, Priority and Number of Work Items
#' Closed
#' @param tib_df Tibble containing complete cases
#' @seealso compute.Week()
#' get.FilteredTibble(fileName="data/example_delivery.csv") %>%  get.ClosedCases() %>% compute.CycleTime() %>% compute.Week() %>% compute.PriorityBased.ClosureAggregates()
#' @return A tibble aggregated on the col_date (Default: FloorDate)
#' @export
compute.FloorDateBased.Aggregates <- 
  function(tib, include_priority = FALSE) {
    tib_result <- tibble()
    ifelse(
      include_priority == FALSE,
      tib_result <- tib %>%
        finmetrics::compute.FloorDateBased.ClosureAggregates(),
      tib_result <- tib %>%
        finmetrics::compute.PriorityBased.ClosureAggregates()
    )
    tib_result
  }


gen_ds.NumClosed_For_FloorDate <-
  function(tib, priority_based = FALSE) {
    tib_result <- tibble()
    ifelse(
      priority_based == FALSE,
      tib_result <- tib %>%
        finmetrics::compute.FloorDateBased.ClosureAggregates(),
      tib_result <- tib %>%
        finmetrics::compute.PriorityBased.ClosureAggregates()
    )
    tib_result
  }

#' @name compute.Opened.Week
#' @title Adds a week number to the tibble and returns
#' @importFrom tibble is_tibble
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom lubridate isoweek
#' @importFrom lubridate floor_date
#' @description
#' Adds a column called Week using the the Floor Date
#' derived from Opened date
#' @param tib_df Input tibble with the Opened Date
#' @param col_closed_date The Column name which has the opened date
#' @export
compute.Opened.Week <- function(tib_df, col_opened_date = "crdt") {
  stopifnot(exprs = {tibble::is_tibble(tib_df);
    is.character(col_opened_date)})

  computeWeeklyFloorDates <-  function()
    tib_df %>%
    dplyr::mutate(FloorDate =
                    lubridate::floor_date(tib_df[[col_opened_date]],
                                          unit = "weeks",
                                          week_start = 1))
  tib_df <- computeWeeklyFloorDates()
  tib_df %>% mutate(Week = lubridate::isoweek(tib_df$FloorDate))
}

tabulate.CycleTimeStat <-
  function(tib, tab_caption = "5 Number Summary") {
    compute.CycleTimeStat <- function() {
      tib.summ <- tib %>%
        summarise_at(vars(cylt),
                     list(
                       Min = min,
                       LoQ = ~ quantile(., probs = 0.25),
                       Med = median,
                       UpQ = ~ quantile(., probs = 0.75),
                       Max = max
                     ))
    }

    knitr::kable(compute.CycleTimeStat(),
                 caption = tab_caption) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("basic", "striped"),
        position = "center",
        full_width = FALSE
      )
  }

tabulate.PriorityBasedCycleTimeStat <-
  function(tib, tab_caption = "5 Number Summary") {
    compute.CycleTimeStat <- function() {
      tib.summ <- tib %>% dplyr::group_by(Priority) %>%
        dplyr::summarise_at(vars(cylt),
                     list(Min=min,
                          LoQ= ~quantile(., probs = 0.25),
                          Med=median,
                          UpQ= ~quantile(., probs = 0.75),
                          Max=max))
        }

    knitr::kable(compute.CycleTimeStat(),
                 caption = tab_caption) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("basic", "striped"),
        position = "center",
        full_width = FALSE
      )
  }

tabulate.TypeAndPriorityBasedCycleTimeStat <-
  function(tib, tab_caption = "5 Number Summary") {
    compute.CycleTimeStat <- function() {
      tib.summ <- tib %>% dplyr::group_by(Type, Priority) %>%
        dplyr::summarise_at(vars(cylt),
                     list(Min=min,
                          LoQ= ~quantile(., probs = 0.25),
                          Med=median,
                          UpQ= ~quantile(., probs = 0.75),
                          Max=max))
    }

    knitr::kable(compute.CycleTimeStat(),
                 caption = tab_caption) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("basic", "striped"),
        position = "center",
        full_width = FALSE
      )
  }

loess_plot.CycleTimeTrend <- function(tib,
                                      plt_caption = "Cycle Time Trend",
                                      col_date = "cldt",
                                      pal_option='D') {
  plt <- ggplot(tib,
                aes(x = .data[[col_date]], y = cylt)) +
    geom_point(aes(colour = Priority)) +
    geom_smooth(method = "loess", formula = 'y~x') +
    xlab("Reporting Period") +  ylab("Cycle Time (Days)") +
    scale_x_date(
      date_labels = "%b/%y",
      date_breaks = "8 weeks",
      minor_breaks = "2 weeks"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs("Priority", caption = plt_caption)

  # Adding the quantile like at 75%
  q <- quantile(tib$cylt, prob = c(0.1, 0.2, 0.5, .75, 1))
  plt + geom_hline(yintercept = q["75%"],
                                     linetype = 2,
                                     alpha = 0.5)
  
  ifelse(hasArg(pal_option),
         plt <- plt + scale_color_viridis(discrete = TRUE, option = pal_option),
         plt <- plt + colour_scale)
  
  plt
}


get.InflowOutflowTibble.Story <- function(tib,
                                          include_priority = TRUE) {
  get.InflowOutflowTibble(tib, itemType = "Story", include_priority)
}

get.InflowOutflowTibble.Defect <- function(tib,
                                           include_priority = TRUE) {
  get.InflowOutflowTibble(tib, itemType = "Defect", include_priority)
}

get.InflowOutflowTibble.Epic <- function(tib,
                                         include_priority = TRUE) {
  get.InflowOutflowTibble(tib, itemType = "Epic", include_priority)
}

merge.InflowOutFlow.withPriority <- function(inflow, outflow) {
  stopifnot({is_tibble(inflow); is_tibble(outflow)})
  flow.merged <- full_join(
    flow.opened,
    flow.closed,
    by = c("FloorDate", "Priority"),
    sort = "FloorDate"
  ) %>%
    transform("Opened" = NumClosed.x, "Closed" = NumClosed.y) %>%
    select(-c("NumClosed.x", "NumClosed.y"))
  flow.merged$Priority <- as.factor(flow.merged$Priority)
  flow.merged[is.na(flow.merged)] <- 0
  flow.merged
}

merge.InflowOutFlow.withOutPriority <- function(inflow, outflow) {
  stopifnot({is_tibble(inflow); is_tibble(outflow)})
  flow.opened <- tib.opened %>% compute.FloorDateBased.Aggregates()
  flow.closed <-
    tib.closed %>% compute.FloorDateBased.Aggregates()
  flow.merged <- full_join(flow.opened,
                           flow.closed,
                           by = c("FloorDate"),
                           sort = "FloorDate") %>%
    transform("Opened" = NumClosed.x, "Closed" = NumClosed.y) %>%
    select(-c("NumClosed.x", "NumClosed.y"))
  flow.merged[is.na(flow.merged)] <- 0
}


get.InflowOutflowTibble <- function(tib,
                                    itemType,
                                    include_priority = TRUE) {
  if (missing(itemType))
    stop("itemType is required, has to be one of Story, Epic or Defect")

  tib.closed <- tib %>% gen_ds.ClosedCases(itemType = itemType)
  tib.opened <- tib %>% gen_ds.OpenedCases(itemType = itemType)
    

  if (include_priority == TRUE) {
    flow.closed <- tib.closed %>%
      compute.FloorDateBased.Aggregates(include_priority = TRUE)
    flow.opened <- tib.opened %>%
      compute.FloorDateBased.Aggregates(include_priority = TRUE)
    flow.merged <- full_join(
      flow.opened,
      flow.closed,
      by = c("FloorDate", "Priority"),
      sort = "FloorDate"
    ) %>%
      transform("Opened" = NumClosed.x, "Closed" = NumClosed.y) %>%
      select(-c("NumClosed.x", "NumClosed.y"))
    flow.merged$Priority <- as.factor(flow.merged$Priority)
    flow.merged[is.na(flow.merged)] <- 0
    flow.merged.cumsum <- flow.merged %>% 
      arrange(FloorDate,Priority) %>% group_by(Priority) %>% 
      mutate(CumSum = cumsum(Opened - Closed))
  } else {
    flow.opened <- tib.opened %>% compute.FloorDateBased.Aggregates()
    flow.closed <-
      tib.closed %>% compute.FloorDateBased.Aggregates()
    flow.merged <- full_join(flow.opened,
                             flow.closed,
                             by = c("FloorDate"),
                             sort = "FloorDate") %>%
      transform("Opened" = NumClosed.x, "Closed" = NumClosed.y) %>%
      select(-c("NumClosed.x", "NumClosed.y"))
    flow.merged[is.na(flow.merged)] <- 0
    flow.merged.cumsum <- flow.merged  %>% 
      arrange(FloorDate) %>% 
      mutate(CumSum = cumsum(Opened - Closed)) 
  }

  flow.merged.cumsum
}

gen_ds.InflowOutflowTibble <- get.InflowOutflowTibble


line_plot.InflowOutflow.CumSum <- function(tib, pal_option = 'D') {
  plt <-
    ggplot(tib, aes(x = FloorDate, y = CumSum, color = Priority)) +
    geom_line(size = 1) +
    geom_point() +
    scale_x_date(date_labels = "%b/%y", 
                 breaks = "8 weeks", 
                 minor_breaks = "2 weeks") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction = "horizontal")
  
  ifelse(hasArg(pal_option),
    plt <- plt + scale_fill_viridis(discrete = TRUE, option = pal_option),
    plt <- plt + colour_scale
  )
  
  plt
}

line_plotly.InflowOutflow.CumSum <- function(tib, pal_option = 'D') {
  if (hasArg(pal_option)) {
    plt <- plotly::plot_ly(
      tib,
      x = ~ FloorDate,
      y = ~ CumSum,
      color = ~ Priority,
      type = 'scatter',
      mode = 'lines+markers',
      colors = viridis_pal(option = pal_option)(5)
    )
  } else {
    plt <- plotly::plot_ly(
      tib,
      x = ~ FloorDate,
      y = ~ CumSum,
      color = ~ Priority,
      type = 'scatter',
      mode = 'lines+markers',
      colors = plotly_color_scale
    )
  }
  
  plt <- plt %>% plotly::config(displaylogo = FALSE,
                                modeBarButtonsToRemove = c("zoomIn2d",
                                                           "zoomOut2d"))
  
  plt <- plt %>% plotly::layout(legend = 
                                  list(orientation = 'h', 
                                       xanchor = 'center', 
                                       x = 0.5,
                                       y = -0.2))
  
  fillPage(
    tags$style(type = "text/css",
               ".plot-fill { width: 80%; height: 100%; }",
               ".center {
                          display: block; 
                          margin-left: auto; 
                          margin-right: auto;
                        }",
               "#left_plt { float: left; background-color: #ccffcc;  }",
               "#right_plt { float: right; background-color: #ccffcc; }",
               "#center_plt { float: center; background-color: #ccffcc; }"
    ),
    div(id = "center_plt", class= "plot-fill center",
       plt
    ),
    theme = "www/bootstrap.css"
    #, padding = 10
  )
}


#' @name cycleTimeQuantiles
#' @title Generates the Quantiles from the given dataframe
#' @importFrom tibble is_tibble
#' @importFrom dplyr %>%
#' @importFrom dplyr summarize_at
#' @importFrom dplyr group_by
#' @importFrom lubridate isoweek
#' @importFrom lubridate floor_date
#' @description
#' Takes a tibble with a column named cylt and with the columns
#' Priority and Type to be grouped on for generating the quantiles
#' @param tib_df Input tibble with variables cylt, Type and Priority
#' @export
cycleTimeQuantiles <- function(tib) {
  tib %>% dplyr::group_by(Type, Priority) %>%
    dplyr::summarise_at(vars(cylt),
                        list(Min=min,
                             LoQ= ~quantile(., probs = 0.25),
                             Med=median,
                             UpQ= ~quantile(., probs = 0.75),
                             Max=max))
}

#' @name areaPlot.WiP
#' @title Generates an areaPlot to visualize with in progress (WiP)
#' @import viridis
#' @importFrom  ggplot2 ggplot
#' @description
#' Takes a tibble and then plots an area charts with it for the number 
#' of work items in progress
#' @param tib_df Input tibble with variables Date and WIPInDays
#' @param pal_option Decides if viridis has to be used if yes which palette
#' @export
areaPlot.WiP <- function(tib, pal_option = 'D') {
  plt <- ggplot(tib,
         aes(x = Date, y = WIPInDays)
  ) +
    geom_area() +
    xlab("Dates") + ylab("WiP (Days)") +
    scale_x_date(date_labels = "%b/%y", 
                 date_breaks = "8 weeks",
                 minor_breaks = "2 weeks") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ifelse(hasArg(pal_option),
    plt <- plt + scale_fill_viridis(discrete = TRUE, option = pal_option),
    plt <- plt + colour_scale
  )
  
  plt
}
# For adhering to the naming convention
area_plot.WiP <- areaPlot.WiP

#' @name bar_plot.InflowOutflow
#' @title Generates a bidirectional plot showing the throughout
#' @import viridis
#' @importFrom  ggplot2 ggplot
#' @description
#' Takes a tibble with the Open and Closed variables and creates a bidirectional
#' stacked plot coloured by priority. 
#' @param tib  Input tibble with variables Date and WIPInDays
#' @param pal_option Decides if viridis has to be used if yes which palette
#' @param data_values Controls if the plot will have values 
#' @param plt_scale optional scale 
#' @param plt_theme optional theme
#' @export
bar_plot.InflowOutflow <-
  function(tib,
           pal_option = 'D',
           data_values = FALSE, plt_scale, plt_theme) {

    p <- ggplot2::ggplot(tib, aes(x = FloorDate, fill = Priority))
    
    bar_open <- ggplot2::geom_bar(aes(y = Opened, fill = Priority),
                        stat = "identity")
    bar_closed <- ggplot2::geom_bar(aes(y = -Closed, fill = Priority),
                        stat = "identity")
    
    bar_open_with_values <- geom_bar(stat = "identity", aes(y = Opened))
    bartext_open_with_values <-  geom_text(
      data = subset(tib, Opened != 0),
      size = 2,
      aes(y = Opened, label = Opened),
      position = position_stack(vjust = 0.5)
    )
    bar_closed_with_values <- geom_bar(stat = "identity", aes(y = -Closed))
    bartext_closed_with_values <- geom_text(
      data = subset(tib, Closed != 0),
      size = 2,
      aes(y = -Closed, label = Closed),
      position = position_stack(vjust = 0.5)
    ) 
    
    if(missing(plt_scale)) {
      plt_scale <-  ggplot2::scale_x_date(
        date_labels = "%b/%y",
        date_breaks = "8 weeks",
        minor_breaks = "2 weeks"
      ) 
    }
    
    if(missing(plt_theme)) {
      plt_theme <- theme_minimal() +
        theme(legend.position = "bottom",
              legend.direction = "horizontal") 
    }
    
    
    ifelse(hasArg(data_values) && data_values == FALSE,
      plt <- p + 
        bar_open + 
        bar_closed +
        xlab("Date") + ylab("Count") +
        plt_scale + 
        plt_theme,
      
      plt <- p + 
        bar_open_with_values + bartext_open_with_values + 
        bar_closed_with_values + bartext_closed_with_values + 
        xlab("Date") + ylab("Count") + 
        plt_scale +
        plt_theme
    )

    ifelse(hasArg(pal_option),
      plt <- plt + scale_fill_viridis(discrete = TRUE, option = pal_option),
      plt <- plt + colour_scale
    )
    
    plt
  }


bar_plot.NumClosed_For_FloorDate <- function(tib,
                                             pal_option = 'D',
                                             data_values = FALSE, 
                                             plt_scale, 
                                             plt_theme) {
  p <- ggplot2::ggplot(tib, aes(x = FloorDate, fill = Priority))
  bar_closed <- ggplot2::geom_bar(aes(y = NumClosed, fill = Priority),
                                  stat = "identity")
  bar_closed_with_values <- geom_bar(stat = "identity", aes(y = NumClosed))
  bartext_closed_with_values <- geom_text(
    data = subset(tib, NumClosed != 0),
    size = 2,
    aes(y = NumClosed, label = NumClosed),
    position = position_stack(vjust = 0.5)
  ) 
  
  if(missing(plt_scale)) {
    plt_scale <-  ggplot2::scale_x_date(
      date_labels = "%b/%y",
      date_breaks = "8 weeks",
      minor_breaks = "2 weeks"
    ) 
  }
  
  if(missing(plt_theme)) {
    plt_theme <- theme_minimal() +
      theme(legend.position = "bottom",
            legend.direction = "horizontal") 
  }
  
  
  ifelse(hasArg(data_values) && data_values == FALSE,
         plt <- p + 
           bar_closed +
           xlab("Date") + ylab("Count") +
           plt_scale + 
           plt_theme,
         
         plt <- p + 
           bar_closed_with_values + bartext_closed_with_values + 
           xlab("Date") + ylab("Count") + 
           plt_scale +
           plt_theme
  )
  
  ifelse(hasArg(pal_option),
         plt <- plt + scale_fill_viridis(discrete = TRUE, option = pal_option),
         plt <- plt + colour_scale
  )
  
  plt
}

# Alias to align with function
bar_plot.Throughput <- bar_plot.NumClosed_For_FloorDate

gen_ds.ClosedCases <- function(tib, itemType)  {
  if (missing(itemType))
    stop("itemType should be provided as a scalar or vector")
  
  tib.closed <- tib %>%
    finmetrics::exclude.OpenCases() %>%
    finmetrics::compute.Week()
    
  ifelse(is.vector(itemType), 
    tib.closed <- tib.closed %>% filter(Type %in% itemType),
    tib.closed <- tib.closed %>% filter(Type == itemType)
  )
  
  tib.closed
}

gen_ds.OpenedCases <- function(tib, itemType, dt_col="crdt") {
  if (missing(itemType))
    stop("itemType should be provided as a scalar or vector")
  
  tib.opened <-
    tib %>%
    finmetrics::compute.Week(dt_col)
 
  ifelse(is.vector(itemType), 
         tib.opened <- tib.opened %>% filter(Type %in% itemType),
         tib.opened <- tib.opened %>% filter(Type == itemType)
  )
  
  tib.opened
}

#' @name bar_plot_grouped.NumWorkItemsAcrossProjects
#' @title Generates a bidirectional plot showing the throughout
#' @import viridis
#' @importFrom  ggplot2 ggplot
#' @description
#' Takes a tibble with the Open and Closed variables and creates a bidirectional
#' stacked plot coloured by priority. 
#' @param tib  Input tibble with variables Date and WIPInDays
#' @param pal_option Decides if viridis has to be used if yes which palette
#' @param data_values Controls if the plot will have values 
#' @param plt_scale optional scale 
#' @param plt_theme optional theme
#' @export

bar_plot_grouped.NumWorkItemsAcrossProjects <- function(tib,
                                             pal_option = 'D',
                                             data_values = FALSE, 
                                             plt_scale, 
                                             plt_theme) {
  p <- ggplot2::ggplot(tib, aes(x = FloorDate, fill = Priority))
  bar_closed <- ggplot2::geom_bar(aes(y = Count, fill = Project),
                                  stat = "identity")
  bar_closed_with_values <- geom_bar(stat = "identity", aes(y = Count))
  bartext_closed_with_values <- geom_text(
    data = subset(tib, Count != 0),
    size = 2,
    aes(y = NumClosed, label = Count),
    position = position_stack(vjust = 0.5)
  ) 

  if(missing(plt_scale)) {
    plt_scale <-  ggplot2::scale_x_date(
      date_labels = "%b/%y",
      date_breaks = "8 weeks",
      minor_breaks = "2 weeks"
    ) 
  }
  
  if(missing(plt_theme)) {
    plt_theme <- theme_minimal() +
      theme(legend.position = "bottom",
            legend.direction = "horizontal") 
  }
  
  
  ifelse(hasArg(data_values) && data_values == FALSE,
         plt <- p + 
           bar_closed +
           xlab("Date") + ylab("Count") +
           plt_scale + 
           plt_theme,
         
         plt <- p + 
           bar_closed_with_values + bartext_closed_with_values + 
           xlab("Date") + ylab("Count") + 
           plt_scale +
           plt_theme
  )
  
  ifelse(hasArg(pal_option),
         plt <- plt + scale_fill_viridis(discrete = TRUE, option = pal_option),
         plt <- plt + colour_scale
  )
  
  plt
}
