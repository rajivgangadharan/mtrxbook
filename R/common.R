library(dplyr)
library(finmetrics)
library(ggplot2)
library(viridis)
library(here)


violin_plot.CycleTimeVsPriority <- function(tib, pal_option = 'D') {
  # Need to pass tib with Priority as a factor variable
  tib$Priority <- as.factor(tib$Priority)
  ggplot(tib, aes(x = Priority, y = cylt, fill = Priority)) +
    geom_violin(draw_quantiles = c(0.5), alpha = 0.25) +
    scale_fill_viridis(discrete = TRUE, option = pal_option) +
  theme_minimal() +
    theme(legend.position = "bottom")
}

bar_plot.NumClosed_For_FloorDate <- function(tib, pal_option="D") {
  ggplot(tib, aes(
    x = FloorDate,
    y = NumClosed,
    fill = Priority
  )) +
    geom_bar(stat = "identity") +
    xlab("Reporting Period") +
    ylab("Throughput") +
    scale_x_date(date_labels = "%b/%y", date_breaks = "8 weeks") +
    scale_fill_viridis(discrete = TRUE, option = pal_option) +
    theme_minimal() +
    theme(legend.position = "bottom")
}


line_plot.NumClosed_For_FloorDate <- function(tib, plt_caption="Closed per Week", pal_option='D')
  ggplot(tib,
         aes(x = as.Date(FloorDate),
             y = NumClosed)) +
  geom_line() +
  geom_point() +
  xlab("Reporting Period") + ylab("Count of work items") +
  scale_x_date(date_labels = "%b/%y", date_breaks = "8 weeks") +
  scale_color_viridis(discrete = TRUE, option = pal_option) +
  scale_fill_viridis(discrete = TRUE, option = pal_option) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(caption=plt_caption)

#'
#' @name compute.FloorDateBased.Aggregates
#' @title Generates a tibble with the Date, Priority and Number of Work Items
#' Closed
#' @description Receives a tibble with complete cases (closed cases) and then
#' generates a tibble with the Date, Priority and Number of Work Items
#' Closed
#' @param tib_df Tibble containing complete cases
#' @param col_date Column Name of with the Floor Date derived from
#' the Closed Date
#' @param col_priority Column name of the priority of the closed items
#' @seealso compute.Week()
#' get.FilteredTibble(fileName="data/example_delivery.csv") %>%  get.ClosedCases() %>% compute.CycleTime() %>% compute.Week() %>% compute.PriorityBased.ClosureAggregates()
#' @export
compute.FloorDateBased.Aggregates <-
  function(tib,
           col_date = "FloorDate",
           col_priority = "Priority",
           include_priority = FALSE) {
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

loess_plot.CycleTimeTrend <- function(tib, plt_caption = "Cycle Time Trend - Loess Plot", pal_option='D') {
  cycle_time_trend_plot <- ggplot(tib,
                                  aes(x = cldt, y = cylt)) +
    geom_point(aes(colour = Priority)) +
    geom_smooth(method = "loess", formula = 'y~x') +
    xlab("Reporting Period") +  ylab("Cycle Time (Days)") +
    scale_x_date(date_labels = "%b/%y", date_breaks = "8 weeks") +
    scale_color_viridis(discrete = TRUE, option = pal_option) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs("Priority", caption=plt_caption)

  q <- quantile(tib$cylt, prob = c(0.1, 0.2, 0.5, .75, 1))

  cycle_time_trend_plot + geom_hline(yintercept = q["75%"],
                                     linetype = 2,
                                     alpha = 0.5)
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


get.InflowOutflowTibble <- function(tib,
                                    itemType,
                                    include_priority = TRUE,
                                    col_opened_date = "crdt",
                                    col_closed_date = "cldt") {
  if (missing(itemType))
    stop("itemType is required, has to be one of Story, Epic or Defect")

  tib.closed <- tib %>%
    finmetrics::exclude.OpenCases() %>%
    finmetrics::compute.CycleTime() %>%
    finmetrics::compute.Week(col_closed_date = col_closed_date) %>%
    filter(Type == itemType)
  tib.opened <-
    tib %>%
    finmetrics::compute.Week(col_closed_date = col_opened_date) %>%
    filter(Type == itemType)

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
  }
  flow.merged[is.na(flow.merged)] <- 0
  flow.merged %>% group_by(Priority) %>% mutate(CumSum = cumsum(Opened - Closed))
}


bar_plot.InflowOutflow <- function(tib, pal_option='D') {
  # Bar Chart showing opening and closing
  ggplot2::ggplot() +
    ggplot2::geom_bar(data = tib,
                      aes(x=FloorDate, y=Opened, fill=Priority),
                      stat = "identity") +
    ggplot2::geom_bar(data = tib,
                      aes(x=FloorDate, y=-Closed, fill=Priority),
                      stat = "identity")  +
    xlab("Date") +
    ylab("Count") +
    ggplot2::scale_x_date(date_labels="%b/%y",
                          date_breaks = "8 weeks",
                          minor_breaks = "2 weeks") +
    scale_fill_viridis(discrete=TRUE, option=pal_option) +
    theme_minimal() +
    theme(legend.position="bottom",
          legend.direction="horizontal")
}

line_plot.InflowOutflow.CumSum <- function(tib, pal_option = 'D') {
  ggplot(tib, aes(x = FloorDate, y = CumSum, color = Priority)) +
    geom_line(size = 1) +
    geom_point() +
    scale_x_date(date_labels = "%b/%y", breaks = "8 weeks") +
    scale_fill_viridis(discrete = TRUE, option = pal_option) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction = "horizontal")
}



