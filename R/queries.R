# ==================== import data ====================

covid <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

world_ggplot2 <- ggplot2::map_data("world")

# world map shape file
# download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip", destfile="data/world_shape_file.zip")
world_spdf <- rgdal::readOGR(
  # dsn="/Users/tomzhang/_R/covidviz/R/data/",
                             dsn= base::paste0(base::getwd(),"/inst/extdata/"),
                      layer="TM_WORLD_BORDERS_SIMPL-0.3",
                      verbose=FALSE)

# ==================== fix names ====================

covid <-
  covid %>%
  dplyr::mutate(location = dplyr::case_when(location == "Congo" ~ "Republic of Congo",
                              location == "Cote d'Ivoire" ~ "Ivory Coast",
                              location == "Czechia" ~ "Czech Republic",
                              location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
                              location == "Eswatini" ~ "Swaziland",
                              location == "Faeroe Islands" ~ "Faroe Islands",
                              location == "Timor" ~ "Timor-Leste",
                              location == "Micronesia (country)" ~ "Micronesia (country)",
                              location == "Pitcairn Islands" ~ "Pitcairn",
                              location == "Sint Maarten (Dutch part)" ~ "Sint Maarten",
                              location == "Timor" ~ "Timor-Leste",

                              TRUE ~ location))

ggplot2 <-
  world_ggplot2 %>%
  dplyr::mutate(region = dplyr::case_when(region == "Antigua" ~ "Antigua and Barbuda",
                            region == "Barbuda" ~ "Antigua and Barbuda",
                            region == "Bonaire" ~ "Bonaire Sint Eustatius and Saba",
                            region == "Sint Eustatius" ~ "Bonaire Sint Eustatius and Saba",
                            region == "Saba" ~ "Bonaire Sint Eustatius and Saba",
                            region == "Virgin Islands" ~ "British Virgin Islands",
                            region == "Saint Kitts" ~ "Saint Kitts and Nevis",
                            region == "Nevis" ~ "Saint Kitts and Nevis",
                            region == "Saint Vincent" ~ "Saint Vincent and the Grenadines",
                            region == "Grenadines" ~ "Saint Vincent and the Grenadines",
                            region == "Trinidad" ~ "Trinidad and Tobago",
                            region == "Tobago" ~ "Trinidad and Tobago",
                            region == "UK" ~ "United Kingdom",
                            region == "USA" ~ "United States",
                            TRUE ~ region))

world_spdf@data <-
  world_spdf@data %>%
  dplyr::mutate(NAME = dplyr::case_when(NAME == "Brunei Darussalam" ~ "Brunei",
                          NAME == "Congo" ~ "Republic of Congo",
                          NAME == "Cote d'Ivoire" ~ "Ivory Coast",
                          NAME == "Falkland Islands (Malvinas)" ~ "Falkland Islands",
                          NAME == "Iran (Islamic Republic of)" ~ "Iran",
                          NAME == "Lao People's Democratic Republic" ~ "Laos",
                          NAME == "Libyan Arab Jamahiriya" ~ "Libya",
                          NAME == "Republic of Moldova" ~ "Moldova",
                          NAME == "Burma" ~ "Myanmar",
                          NAME == "Pitcairn Islands" ~ "Pitcairn",
                          NAME == "Korea, Republic of" ~ "South Korea",
                          NAME == "Syrian Arab Republic" ~ "Syria",
                          NAME == "United Republic of Tanzania" ~ "Tanzania",
                          NAME == "Holy See (Vatican City)" ~ "Vatican",
                          NAME == "Viet Nam" ~ "Vietnam",
                          NAME == "Wallis and Futuna Islands" ~ "Wallis and Futuna",
                          TRUE ~ NAME))

# ==================== helper functions/constants ====================

# a function that returns a function
transform <- function(log_scale) {
  if (log_scale) {
    return(base::log)  # returns a log function
  } else {
    return(function(x) {x})  # returns an identify function
  }
}

# to prettify plot titles
fill_name <- function(fill, log_scale) {
  dplyr::if_else(log_scale, base::paste0("log(", fill, ")"), fill)
}

# get all numeric variables in dataset
num_vars <- NULL
for (col in base::names(covid)) {
  if (base::is.numeric(covid[[col]])) {
    num_vars <- c(num_vars, col)
  }
}

# totally useless - simply a temporary fix for a leaflet bug
# although the dynamic plots do not use the "sp" package directly
# it is a required dependency
# we're using it here so that it wouldn't get loaded later in the code
fix_sp_bug <- function() {
  tmp <- sp::merge(base::data.frame(), base::data.frame())
}

# ==================== query 1 ====================

#' Query 1
#'
#' For each country, count the total number of cases, deaths, and the number of days passed since the first day of the outbreak (day of the first recorded new case) up to the day of the first peak stringency index (both inclusive).
#'
#' @param plot_type "static" | "dynamic"
#' @param fill "cases" |"deaths" |"days" - The map will be filled by this variable.
#' @param log_scale FALSE | TRUE - Apply a log transformation on the selected variable? Could be useful if variable contains outliers.
#' @param projection "mercator" | "globular" | "gilbert"
#'
#' @return Either a static map produced with ggplot2 or a dynamic one produced with leaflet.
#' @export
query1 <- function(plot_type = "static", fill = "cases",
                   log_scale = FALSE, projection = "mercator") {
  # basic argument validation
  base::stopifnot(plot_type %in% c("static", "dynamic"),
            fill %in% c("cases", "deaths", "days"),
            base::is.logical(log_scale),
            projection %in% c("mercator", "globular", "gilbert"))

  # data wrangling

  # df of each country's outbreak start date
  outbreak_start <-
    covid %>%
    dplyr::select(location, date, new_cases) %>%
    dplyr::group_by(location) %>%
    dplyr::filter(new_cases > 0) %>%
    dplyr::slice(1) %>%  # keep earliest observation where new_cases is nonzero (data is already sorted)
    dplyr::select(location, date) %>%
    dplyr::ungroup()

  # a helper df
  lag_str_index <-
    outbreak_start %>%
    dplyr::select(location) %>%
    dplyr::left_join(covid, by = "location") %>%  # only use locations that have outbreaks (new_cases not missing)
    dplyr::select(location, date, stringency_index) %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(prev_str_index = dplyr::lag(stringency_index),
           prev_date = dplyr::lag(date)) %>%
    dplyr::ungroup()

  # df of each country's str index peak date (day before first decrease)
  first_str_peak <-
    lag_str_index %>%
    dplyr::group_by(location) %>%
    dplyr::filter(stringency_index < prev_str_index) %>%  # this means there is a decrease in str index
    dplyr::slice(1) %>%  # keep first peak
    dplyr::select(location, prev_date) %>%
    dplyr::rename(date = prev_date) %>%
    dplyr::ungroup()

  # corner case: str index is non-decreasing
  corner_str_peak <-
    lag_str_index %>%
    dplyr::anti_join(first_str_peak, by = "location") %>%  # locations with non-decreasing str index
    dplyr::group_by(location) %>%
    tidyr::drop_na(stringency_index) %>%  # ignore locations with missing str index
    dplyr::slice(dplyr::n()) %>%  # consider most recent date as peak
    dplyr::select(location, date) %>%
    dplyr::ungroup()
  # note: as of now (03/21/2022), no country falls within this category; we have excluded NA's

  # a complete list of first peak of str index for each country
  fixed_first_str_peak <-
    first_str_peak %>%
    dplyr::bind_rows(corner_str_peak) %>%
    dplyr::arrange(location)

  res <-
    covid %>%
    dplyr::select(location, date, new_cases, new_deaths) %>%
    dplyr::right_join(outbreak_start, by = "location", suffix = c("", "_outbreak_start")) %>%
    dplyr::right_join(fixed_first_str_peak, by = "location", suffix = c("", "_first_str_peak")) %>%
    dplyr::group_by(location) %>%
    dplyr::filter(date >= date_outbreak_start & date <= date_first_str_peak) %>%
    dplyr::summarise(
      cases = base::sum(new_cases, na.rm = TRUE),
      deaths = base::sum(new_deaths, na.rm = TRUE),
      days = dplyr::n(),
      start = base::min(date_outbreak_start),
      end = base::min(date_first_str_peak)
    ) %>%
    dplyr::ungroup()

  # plot

  if (plot_type == "static") {

    world_ggplot2 %>%
      dplyr::left_join(res, by = c("region" = "location")) %>%
      ggplot2::ggplot(., ggplot2::aes(x = long, y = lat, group = group)) +
      ggplot2::geom_polygon(ggplot2::aes(fill = transform(log_scale)(.data[[fill]])), color = "black") +
      ggplot2::coord_map(projection = projection, xlim = c(-180, 180)) +  # range of longitude in data exceeds 180 - need to manually limit
      ggplot2::labs(title = base::paste0("World Map of ", fill_name(fill, log_scale), " by Country"),
           subtitle = "From Outbreak Start to First Peak of Stringency Index",
           fill = fill_name(fill, log_scale)) +
      ggplot2::scale_fill_distiller(palette = "YlOrBr", direction = 1) +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(fill = NA, color = "black")
      )

  } else {

    fix_sp_bug()

    world_spdf@data <- dplyr::left_join(world_spdf@data, res, by = c("NAME" = "location"))

    # tooltip text
    label <- base::paste0(
      "Country: ", world_spdf@data$NAME, "<br/>",
      "Cases: ", world_spdf@data$cases, "<br/>",
      "Deaths: ", world_spdf@data$deaths, "<br/>",
      "Days: ", world_spdf@data$days, "<br/>",
      "Start: ", world_spdf@data$start, "<br/>",
      "End: ", world_spdf@data$end, "<br/>") %>%
      base::lapply(htmltools::HTML)

    # palette
    vals <- transform(log_scale)(world_spdf@data[[fill]])
    vals <- base::replace(vals, vals == -Inf, 0)  # in case values are extreme
    palette <- leaflet::colorNumeric("YlOrBr", vals, na.color = "transparent")

    # map
    leaflet::leaflet(world_spdf) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lat = 10, lng = 0, zoom = 2) %>%
      leaflet::addPolygons(
        fillColor = ~palette(vals),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = label,
        labelOptions = leaflet::labelOptions(
          style = base::list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      leaflet::addLegend(pal = palette, values = ~vals, opacity = 0.9,
                title = fill_name(fill, log_scale), position = "bottomleft")

  }
}

# ==================== query 2 ====================

#' Query 2
#'
#' For the countries that have vaccines, compare the average daily new cases (or deaths) of the days before the first vaccine dose administered and the days after that. Also, construct a 95% confidence interval for each.
#'
#' @param plot_type "static" | "dynamic"
#' @param fill "avg_daily_new_cases_pre_vac" | "avg_daily_new_cases_post_vac" | "avg_daily_new_deaths_pre_vac" | "avg_daily_new_deaths_post_vac" - The map will be filled by this variable.
#' @param log_scale FALSE | TRUE - Apply a log transformation on the selected variable? Could be useful if variable contains outliers.
#' @param projection "mercator" | "globular" | "gilbert"
#'
#' @return Either a static map produced with ggplot2 or a dynamic one produced with leaflet.
#' @export
query2 <- function(plot_type = "static", fill = "avg_daily_new_cases_pre_vac",
                   log_scale = FALSE, projection = "mercator") {
  # basic argument validation
  base::stopifnot(plot_type %in% c("static", "dynamic"),
            fill %in% c("avg_daily_new_cases_pre_vac",
                        "avg_daily_new_cases_post_vac",
                        "avg_daily_new_deaths_pre_vac",
                        "avg_daily_new_deaths_post_vac"),
            base::is.logical(log_scale),
            projection %in% c("mercator", "globular", "gilbert"))

  # data wrangling

  # df of each country's outbreak start date
  outbreak_start <-
    covid %>%
    dplyr::select(location, date, new_cases) %>%
    dplyr::group_by(location) %>%
    dplyr::filter(new_cases > 0) %>%
    dplyr::slice(1) %>%  # keep earliest observation where new_cases is nonzero (data is already sorted)
    dplyr::ungroup() %>%
    dplyr::select(location, date) %>%
    dplyr::rename(outbreak_start = date)

  # a helper df
  lag_vac <-
    outbreak_start %>%
    dplyr::select(location) %>%
    dplyr::left_join(covid, by = "location") %>%  # only use locations that have outbreaks (new_cases not missing)
    dplyr::select(location, date, new_vaccinations) %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(prev_date = dplyr::lag(date)) %>%
    dplyr::ungroup()

  # df of the day (and previous day) of first recorded vaccine dose for each country
  vac_date <-
    lag_vac %>%
    dplyr::filter(new_vaccinations > 0) %>%  # only keep countries that have vaccines
    dplyr::group_by(location) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(location, date, prev_date) %>%
    dplyr::rename(vac_date = date, pre_vac_date = prev_date)

  # df of the most recent date found in dataset for each country
  most_recent <-
    covid %>%
    dplyr::select(location, date) %>%
    dplyr::group_by(location) %>%
    dplyr::slice(dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::rename(most_recent = date)

  # aggregate these dfs
  vac_time_frames <-
    vac_date %>%
    dplyr::left_join(outbreak_start, by = "location") %>%
    dplyr::left_join(most_recent, by = "location")

  # Suppress summarise info
  base::options(dplyr.summarise.inform = FALSE)

  res <-
    vac_time_frames %>%
    dplyr::left_join(covid, by = "location") %>%
    dplyr::select(location, outbreak_start, pre_vac_date, vac_date, most_recent,
           new_cases, new_deaths, date) %>%
    dplyr::mutate(time_frame = dplyr::case_when(date >= outbreak_start & date <= pre_vac_date ~ "pre_vac",
                                  date >= vac_date & date <= most_recent ~ "post_vac")) %>%
    dplyr::filter(!base::is.na(time_frame)) %>%  # identified two time frames of interest for each country
    tidyr::drop_na(new_cases, new_deaths) %>%
    dplyr::group_by(location, time_frame) %>%
    dplyr::summarise(
      avg_daily_new_cases = base::mean(new_cases),
      lower_cases = avg_daily_new_cases - 1.96 * stats::sd(new_cases) / base::sqrt(dplyr::n()),
      upper_cases = avg_daily_new_cases + 1.96 * stats::sd(new_cases) / base::sqrt(dplyr::n()),
      avg_daily_new_deaths = base::mean(new_deaths),
      lower_deaths = avg_daily_new_deaths - 1.96 * stats::sd(new_deaths) / base::sqrt(dplyr::n()),
      upper_deaths = avg_daily_new_deaths + 1.96 * stats::sd(new_deaths) / base::sqrt(dplyr::n())
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = time_frame,
                values_from = c(avg_daily_new_cases, lower_cases, upper_cases,
                                avg_daily_new_deaths, lower_deaths, upper_deaths)) %>%
    dplyr::left_join(vac_time_frames, by = "location")

  # plot

  if (plot_type == "static") {

    world_ggplot2 %>%
      dplyr::left_join(res, by = c("region" = "location")) %>%
      ggplot2::ggplot(., ggplot2::aes(x = long, y = lat, group = group)) +
      ggplot2::geom_polygon(ggplot2::aes(fill = transform(log_scale)(.data[[fill]])), color = "black") +
      ggplot2::coord_map(projection = projection, xlim = c(-180, 180)) +
      ggplot2::labs(title = base::paste0("World Map of ", fill_name(fill, log_scale), " by Country"),
           subtitle = "From Outbreak Start to Date of First Vaccine Dose Administered",
           fill = fill_name(fill, log_scale)) +
      ggplot2::scale_fill_distiller(palette = "YlOrBr", direction = 1) +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(fill = NA, color = "black")
      )

  } else {

    fix_sp_bug()

    world_spdf@data <- dplyr::left_join(world_spdf@data, res, by = c("NAME" = "location"))

    # tooltip text
    label <- base::paste0(
      "Country: ", world_spdf@data$NAME, "<br/>",
      "<br/>",
      "Pre-vaccine: ", base::format(world_spdf@data$outbreak_start, "%m/%d/%Y"), " - ",
      base::format(world_spdf@data$pre_vac_date, "%m/%d/%Y"), "<br/>",
      "&emsp;Avg Daily New Cases (Pre): ",
      base::round(world_spdf@data$avg_daily_new_cases_pre_vac),
      "<br/>",
      "&emsp;Confidence Interval (Pre): ", "(",
      base::round(world_spdf@data$lower_cases_pre_vac),
      ", ", base::round(world_spdf@data$upper_cases_pre_vac), ")", "<br/>",
      "&emsp;Avg Daily New Deaths (Pre): ",
      base::round(world_spdf@data$avg_daily_new_deaths_pre_vac),
      "<br/>",
      "&emsp;Confidence Interval (Pre): ", "(",
      base::round(world_spdf@data$lower_deaths_pre_vac),
      ", ", base::round(world_spdf@data$upper_deaths_pre_vac), ")", "<br/>",
      "<br/>",
      "Post-vaccine: ", base::format(world_spdf@data$vac_date, "%m/%d/%Y"), " - ",
      base::format(world_spdf@data$most_recent, "%m/%d/%Y"), "<br/>",
      "&emsp;Avg Daily New Cases (Post): ",
      base::round(world_spdf@data$avg_daily_new_cases_post_vac),
      "<br/>",
      "&emsp;Confidence Interval (Post): ", "(",
      base::round(world_spdf@data$lower_cases_post_vac),
      ", ", round(world_spdf@data$upper_cases_post_vac), ")", "<br/>",
      "&emsp;Avg Daily New Deaths (Post): ",
      base::round(world_spdf@data$avg_daily_new_deaths_post_vac),
      "<br/>",
      "&emsp;Confidence Interval (Post): ", "(",
      base::round(world_spdf@data$lower_deaths_post_vac),
      ", ", base::round(world_spdf@data$upper_deaths_post_vac), ")", "<br/>") %>%
      base::lapply(htmltools::HTML)

    # palette
    vals <- transform(log_scale)(world_spdf@data[[fill]])
    vals <- base::replace(vals, vals == -Inf, 0)  # in case values are extreme
    palette <- leaflet::colorNumeric("YlOrBr", vals, na.color = "transparent")

    # map
    leaflet::leaflet(world_spdf) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lat = 10, lng = 0, zoom = 2) %>%
      leaflet::addPolygons(
        fillColor = ~palette(vals),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = label,
        labelOptions = leaflet::labelOptions(
          style = base::list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      leaflet::addLegend(pal = palette, values = ~vals, opacity = 0.9,
                title = fill_name(fill, log_scale), position = "bottomleft")

  }
}

# ==================== query 3 ====================

#' Query 3
#'
#' For each country, identify the top 3 peaks given a variable (e.g., daily new cases).
#'
#' @param plot_type "static" | "dynamic"
#' @param fill "new_cases" | "new_deaths" | "new_vaccinations" - The map will be filled by this variable.
#' @param log_scale FALSE | TRUE - Apply a log transformation on the selected variable? Could be useful if variable contains outliers.
#' @param projection "mercator" | "globular" | "gilbert"
#'
#' @return Either a static map produced with ggplot2 or a dynamic one produced with leaflet.
#' @export
query3 <- function(plot_type = "static", fill = "new_cases",
                   log_scale = FALSE, projection = "mercator") {
  # basic argument validation
  base::stopifnot(plot_type %in% c("static", "dynamic"),
            fill %in% c("new_cases", "new_deaths", "new_vaccinations"),
            base::is.logical(log_scale),
            projection %in% c("mercator", "globular", "gilbert"))

  # data wrangling

  # df of each country's outbreak start date
  outbreak_start <-
    covid %>%
    dplyr::select(location, date, new_cases) %>%
    dplyr::group_by(location) %>%
    dplyr::filter(new_cases > 0) %>%
    dplyr::slice(1) %>%  # keep earliest observation where new_cases is nonzero (data is already sorted)
    dplyr::ungroup() %>%
    dplyr::select(location, date) %>%
    dplyr::rename(outbreak_start = date)

  # suppress loess() warning
  warn <- base::getOption("warn")
  base::options(warn = -1)

  # a helper df
  lag_lead <-
    outbreak_start %>%
    dplyr::select(location) %>%
    dplyr::left_join(covid, by = "location") %>%  # only use locations that have outbreaks (new_cases not missing)
    dplyr::select(location, date, .data[[fill]]) %>%
    tidyr::drop_na(.data[[fill]]) %>%
    dplyr::group_by(location) %>%
    dplyr::group_modify(~ {
      data <-
        .x %>%
        tidyr::drop_na(.data[[fill]]) %>%
        dplyr::mutate(idx = seq(nrow(.))) %>%  # because loess() doesn't support dates
        dplyr::select(date, idx, .data[[fill]])

      # fit a local regression model
      mod_local <- loess(base::paste0(fill, " ~ idx"), span = 65 / nrow(data), data = data)

      data %>% dplyr::mutate(var_loess = predict(mod_local))
    }) %>%
    dplyr::mutate(prev_var = dplyr::lag(.data[[fill]]),
           next_var = dplyr::lead(.data[[fill]]),
           prev_var_loess = dplyr::lag(var_loess),
           next_var_loess = dplyr::lead(var_loess),
           prev_date = dplyr::lag(date),
           next_date = dplyr::lead(date)) %>%
    dplyr::rename(curr_date = date,
           curr_var = .data[[fill]]) %>%
    dplyr::ungroup()

  # restore previous warning setting
  base::options(warn = warn)

  # df of each country's top-3-peak dates
  top_peaks <-
    lag_lead %>%
    dplyr::group_by(location) %>%
    dplyr::filter(prev_var_loess < var_loess &  # a peak
             var_loess > next_var_loess) %>%
    dplyr::arrange(desc(var_loess)) %>%
    dplyr::slice(1:3) %>%  # keep top 3 peaks
    dplyr::ungroup()

  # corner case: var is non-decreasing
  corner_peak <-
    lag_lead %>%
    dplyr::anti_join(top_peaks, by = "location") %>%  # locations with non-decreasing var
    dplyr::group_by(location) %>%
    tidyr::drop_na(curr_var) %>%  # ignore locations with missing var
    dplyr::slice(dplyr::n()) %>%  # consider most recent date as peak
    dplyr::select(location, curr_date) %>%
    dplyr::ungroup()
  # note: as of now (03/28/2022), only Micronesia falls in this category

  # a complete base::list of first peak of var for each country
  fixed_top_peaks <-
    top_peaks %>%
    dplyr::bind_rows(corner_peak) %>%
    dplyr::arrange(location)

  # because peaks are identified with loess(), the actual var on that day may not be the highest - look around to find the max - i.g., prev, curr, next dates
  true_top_peaks <-
    fixed_top_peaks %>%
    dplyr::select(location, prev_date, curr_date, next_date, prev_var, curr_var, next_var) %>%
    dplyr::group_by(location) %>%
    tidyr::pivot_longer(cols = contains("date"), names_to = "date_type", values_to = "date_val") %>%
    tidyr::pivot_longer(cols = contains("var"), names_to = "var_type", values_to = "var_val") %>%
    dplyr::mutate(date_group = stringr::str_extract(date_type, "[^_]*"),
           var_group = stringr::str_extract(var_type, "[^_]*")) %>%
    dplyr::filter(date_group == var_group) %>%
    dplyr::mutate(group = rep(1:3, each = 3, length.out = dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(location, group) %>%
    dplyr::arrange(desc(var_val)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(location, date_val, var_val) %>%
    dplyr::rename(date = date_val, var = var_val)

  res <-
    true_top_peaks %>%
    dplyr::select(location, date, var) %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(peak = seq(dplyr::n())) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = peak, values_from = c(var, date))

  # plot

  if (plot_type == "static") {

    world_ggplot2 %>%
      dplyr::left_join(res, by = c("region" = "location")) %>%
      ggplot2::ggplot(., ggplot2::aes(x = long, y = lat, group = group)) +
      ggplot2::geom_polygon(ggplot2::aes(fill = transform(log_scale)(var_1)), color = "black") +
      ggplot2::coord_map(projection = projection, xlim = c(-180, 180)) +
      ggplot2::labs(title = base::paste0("World Map of ", fill_name(fill, log_scale),
                          " Top Peak by Country"),
           subtitle = "Peaks Identified with loess()",
           fill = fill_name(fill, log_scale)) +
      ggplot2::scale_fill_distiller(palette = "YlOrBr", direction = 1) +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(fill = NA, color = "black")
      )

  } else {

    fix_sp_bug()

    world_spdf@data <- dplyr::left_join(world_spdf@data, res, by = c("NAME" = "location"))

    # tooltip text
    label <- base::paste0(
      "Country: ", world_spdf@data$NAME, "<br/>",
      "Peak 1: ", base::format(world_spdf@data$date_1, "%m/%d/%Y"), "<br/>",
      fill, ": ", base::round(world_spdf@data$var_1), "<br/>",
      "Peak 2: ", base::format(world_spdf@data$date_2, "%m/%d/%Y"), "<br/>",
      fill, ": ", base::round(world_spdf@data$var_2), "<br/>",
      "Peak 3: ", base::format(world_spdf@data$date_3, "%m/%d/%Y"), "<br/>",
      fill, ": ", base::round(world_spdf@data$var_3), "<br/>") %>%
      base::lapply(htmltools::HTML)

    # palette
    vals <- transform(log_scale)(world_spdf@data$var_1)
    vals <- base::replace(vals, vals == -Inf, 0)  # in case values are extreme
    palette <- leaflet::colorNumeric("YlOrBr", vals, na.color = "transparent")

    # map
    leaflet::leaflet(world_spdf) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lat = 10, lng = 0, zoom = 2) %>%
      leaflet::addPolygons(
        fillColor = ~palette(vals),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = label,
        labelOptions = leaflet::labelOptions(
          style = base::list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      leaflet::addLegend(pal = palette, values = ~vals, opacity = 0.9,
                title = base::paste0(fill_name(fill, log_scale), " Top Peak"),
                position = "bottomleft")

  }
}

# ==================== query 4 ====================

#' Query 4
#'
#' This is a more flexible query - essentially, a user can specify a time frame, then investigate any variable of their choice.
#'
#' @param plot_type "static" | "dynamic"
#' @param start The start date of the time frame; this can be specified with one of the following:
#' - "outbreak_start": The default.
#' - "05-22-2020": Any date string in the format "%m-%d-%Y".
#' - c("new_cases" = 10000): The date when a variable first exceeded a threshold. This variable can be any of the numeric variables in the OWID COVID dataset found here: \url{https://covid.ourworldindata.org/data/owid-covid-data.csv}
#' @param end The end date of the time frame; this can be specified with one of the following:
#' - "most_recent": The default.
#' - "07-20-2021": Any date string in the format "%m-%d-%Y".
#' - c("new_deaths" = 1000): The date when a variable first exceeded a threshold.
#' @param var "new_cases" | "new_deaths" | "new_vaccinations" - or any other numeric variable in the OWID COVID dataset found here: \url{https://covid.ourworldindata.org/data/owid-covid-data.csv}
#' @param func "sum" | "mean" | "median" | "min" | "max"
#' @param projection "mercator" | "globular" | "gilbert"
#'
#' @return Either a static map produced with ggplot2 or a dynamic one produced with leaflet.
#' @export
#'
#' @examples
#' query4(plot_type = "static", start = "outbreak_start", end = "most_recent", var = "new_cases", func = "sum", projection = "mercator")
#' query4(plot_type = "dynamic", start = c("new_deaths" = 1000), end = "04-33-2021", var = "new_vaccinations", func = "median", projection = "gilbert")
#' query4(plot_type = "dynamic", start = "03-03-2020", end = c("population" = 30000), var = "new_cases", func = "max", projection = "globular")
query4 <- function(plot_type = "static", start = "outbreak_start", end = "most_recent",
                   var = "new_cases", func = "sum", projection = "mercator") {
  # basic argument validation
  base::stopifnot(plot_type %in% c("static", "dynamic"),
            func %in% c("sum", "mean", "median", "min", "max"),
            projection %in% c("mercator", "globular", "gilbert"))

  # advanced argument validation

  # get all numeric variables in dataset
  num_vars <- NULL
  for (col in base::names(covid)) {
    if (is.numeric(covid[[col]])) {
      num_vars <- c(num_vars, col)
    }
  }

  # helper function
  validate_date_arg <- function(date, type) {
    # construct error message
    if (type == "start") {
      default = "outbreak_start"
    } else {
      default = "most_recent"
    }
    err_msg <- base::paste0('Argument ', type,' must be either "', default, '"\n',
                      'a date string in the format of "%m-%d-%Y",
               or a vector in the format of c(any_numeric_variable_in_df_covid, a_number)')

    # conditions
    if (base::is.character(date)) {
      # if start is not default arg
      if (date != default) {
        # if cannot parse start as date string
        if (base::is.na(base::as.Date(date, "%m-%d-%Y"))) {
          stop(err_msg)
        }
      }
    } else if (is.numeric(date)) {
      # if not in the format of c(num_var = number)
      if (length(date) != 1 | is.null(base::names(date))) {
        stop(err_msg)
      } else if (! base::names(date) %in% num_vars) {
        stop(err_msg)
      }
    } else if (!base::is.character(date) & !is.numeric(date)) {
      stop(err_msg)
    }
  }

  # validate time frame args
  validate_date_arg(start, "start")
  validate_date_arg(end, "end")

  # validate var
  if (! var %in% num_vars) {
    stop(base::paste0("Argument var must be one of: ", base::paste0(num_vars, collapse = ", ")))
  }

  # data wrangling

  # arg start
  if (start == "outbreak_start") {
    df_start <-
      covid %>%
      dplyr::select(location, date, .data[[var]]) %>%
      dplyr::group_by(location) %>%
      dplyr::filter(.data[[var]] > 0) %>%
      dplyr::slice(1) %>%  # keep earliest observation where new_cases is nonzero (data is already sorted)
      dplyr::select(location, date) %>%
      dplyr::ungroup()
  } else if (is.numeric(start)) {  # start = c(num_var, number)
    df_start <-
      covid %>%
      dplyr::select(location, date, base::names(start)[1]) %>%
      dplyr::group_by(location) %>%
      dplyr::filter(.data[[base::names(start)[1]]] >= start[1]) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(location, date)
  } else  {  # start = "%m-%d-%Y"
    df_start <-
      covid %>%
      dplyr::select(location, date) %>%
      dplyr::filter(date == base::as.Date(start, "%m-%d-%Y")) %>%
      dplyr::group_by(location) %>%
      dplyr::slice(1)
  }

  # arg end
  if (end == "most_recent") {
    df_end <-
      covid %>%
      dplyr::select(location, date) %>%
      dplyr::group_by(location) %>%
      dplyr::slice(dplyr::n()) %>%
      dplyr::ungroup()
  } else if (is.numeric(end)) {  # start = c(num_var, number)
    # note: apply rule after the specified start date for each country
    df_end <-
      covid %>%
      dplyr::select(location, date, base::names(end)[1]) %>%
      dplyr::right_join(df_start, by = "location", suffix = c("", "_start")) %>%
      dplyr::mutate(after_start = dplyr::if_else(date > date_start, 1, 0)) %>%
      dplyr::filter(after_start == 1) %>%
      dplyr::group_by(location) %>%
      dplyr::filter(.data[[base::names(end)[1]]] >= end[1]) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(location, date)
  } else {  # start = "%m-%d-%Y"
    df_end <-
      covid %>%
      dplyr::select(location, date) %>%
      dplyr::filter(date == base::as.Date(end, "%m-%d-%Y")) %>%
      dplyr::group_by(location) %>%
      dplyr::slice(1)
  }

  # joint df start and end
  time_frame <- dplyr::inner_join(df_start, df_end, by = "location",
                           suffix = c("_start", "_end"))

  # check if time frame exists
  if (nrow(time_frame) == 0) {
    stop("Time frame does not exist.")
  }

  # var calc
  aggregate_func <- function(func) {
    if (func == "sum") {
      return(base::sum)
    } else if (func == "mean") {
      return(base::mean)
    } else if (func == "median") {
      return(stats::median)
    } else if (func == "min") {
      return(base::min)
    } else if (func == "max") {
      return(base::max)
    }
  }

  res <-
    covid %>%
    dplyr::select(location, date, .data[[var]]) %>%
    dplyr::right_join(time_frame, by = "location") %>%
    dplyr::filter(date >= date_start, date <= date_end) %>%
    dplyr::group_by(location) %>%
    tidyr::drop_na(.data[[var]]) %>%
    dplyr::summarise(var_agg = aggregate_func(func)(.data[[var]]),
              date_start = date_start[1], date_end = date_end[1])

  # plot

  prettify_aggregate_format <- function(var, func) {
    base::paste0(func, "(", var, ")")
  }

  if (plot_type == "static") {

    world_ggplot2 %>%
      dplyr::left_join(res, by = c("region" = "location")) %>%
      ggplot2::ggplot(., ggplot2::aes(x = long, y = lat, group = group)) +
      ggplot2::geom_polygon(ggplot2::aes(fill = var_agg), color = "black") +
      ggplot2::coord_map(projection = projection, xlim = c(-180, 180)) +
      ggplot2::labs(title = base::paste0("World Map of ", prettify_aggregate_format(var, func),
                          " by Country"),
           subtitle = "In Custom Time Frames",
           fill = prettify_aggregate_format(var, func)) +
      ggplot2::scale_fill_distiller(palette = "YlOrBr", direction = 1) +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(fill = NA, color = "black")
      )

  } else {

    fix_sp_bug()

    world_spdf@data <- dplyr::left_join(world_spdf@data, res, by = c("NAME" = "location"))

    # tooltip text
    label <- base::paste0(
      "Country: ", world_spdf@data$NAME, "<br/>",
      "Time Frame: ", base::format(world_spdf@data$date_start, "%m/%d/%Y"), " - ",
      base::format(world_spdf@data$date_end, "%m/%d/%Y"), "<br/>",
      prettify_aggregate_format(var, func), ": ", world_spdf@data$var_agg, "<br/>") %>%
      base::lapply(htmltools::HTML)

    # palette
    if (base::sum(base::is.na(world_spdf@data$var_agg)) == base::nrow(world_spdf@data)) {
      base::stop("Invalid timeframe.")
    }

    palette <- leaflet::colorNumeric("YlOrBr", world_spdf@data$var_agg, na.color = "transparent")

    # map
    leaflet::leaflet(world_spdf) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lat = 10, lng = 0, zoom = 2) %>%
      leaflet::addPolygons(
        fillColor = ~palette(var_agg),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = label,
        labelOptions = leaflet::labelOptions(
          style = base::list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      leaflet::addLegend(pal = palette, values = ~var_agg, opacity = 0.9,
                title = prettify_aggregate_format(var, func), position = "bottomleft")

  }
}

# ==================== query 5 ====================

#' Query 5
#'
#' Produces a time-series plot of a variable for either one country or multiple countries. Time frame can be a custom argument.
#'
#' @param plot_type "static" | "dynamic"
#' @param var "new_cases" | "new_deaths" | "new_vaccinations" - or any other numeric variable in the OWID COVID dataset found here: \url{https://covid.ourworldindata.org/data/owid-covid-data.csv}
#' @param country "United States" | "Canada" | "United Kingdom" - or any other country name.
#' @param start The start date of the time frame; by default (NULL) starts from the first available record in the OWID COVID dataset. Or specify any date string in the format "%m-%d-%Y" such as "05-22-2020".
#' @param end The end date of the time frame; by default (NULL) includes the most recent record in the OWID COVID dataset. Or specify any date string in the format "%m-%d-%Y" such as "07-22-2021".
#' @param group_by Further group time frame by: week | month; NULL by default.
#' @return Either a static map produced with ggplot2 or a dynamic one produced with leaflet.
#' @export
query5 <- function(plot_type = "static", var = "new_cases", country = "United States",
                   start = NULL, end = NULL, group_by = NULL) {
  # basic argument validation
  base::stopifnot(plot_type %in% c("static", "dynamic"),
                  group_by %in% c("week", "month"))

  # advanced argument validation
  if (!var %in% num_vars) {
    base::stop(base::paste0("var must be a numeric variable in the covid dataset: ",
                            base::paste0(num_vars, collapse = ", ")))
  }

  diff <- dplyr::setdiff(country, base::unique(covid$location))

  if (base::length(diff) > 0) {
    base::stop(base::paste0("Invalid country name(s): ",
                            base::paste0(diff, collapse = ", ")))
  }

  validate_date <- function(date, type) {
    if (!base::is.null(date)) {
      if (base::is.na(base::as.Date(date, "%m-%d-%Y"))) {
        base::stop(base::paste0(type, " must be a date string in the format %m-%d-%Y"))
      }
    }
  }

  validate_date(start, "start")
  validate_date(end, "end")

  # filter country
  res <-
    covid %>%
    dplyr::filter(location %in% country) %>%
    dplyr::select(location, date, .data[[var]])

  # filter time frame
  if (!base::is.null(start)) {
    res <- dplyr::filter(res, date >= base::as.Date(start, format = "%m-%d-%Y"))
  }

  if (!base::is.null(end)) {
    res <- dplyr::filter(res, date <= base::as.Date(end, format = "%m-%d-%Y"))
  }

  # group time frame
  if (!base::is.null(group_by)) {
    base::options(dplyr.summarise.inform = FALSE)
    res <-
      res %>%
      dplyr::mutate(group = dplyr::case_when(group_by == "week" ~ lubridate::week(date),
                                             group_by == "month" ~ lubridate::month(date)),
             year = lubridate::year(date),
             group_in_year = base::paste0(group, "-", year),
             group_in_year = forcats::as_factor(group_in_year)) %>%
      dplyr::group_by(location, group_in_year) %>%
      dplyr::summarise(
        span = dplyr::if_else(group_by == "week",
                       base::paste0(base::format(base::min(date), "%m/%d/%Y"), " - ",
                                    base::format(base::max(date), "%m/%d/%Y")),
                       base::as.character(base::unique(group_in_year))),
        mean_var = base::mean(.data[[var]], na.rm = TRUE),
        s = stats::sd(.data[[var]], na.rm = TRUE),
        n = dplyr::n(),
        se = s / base::sqrt(n),
        upper = mean_var + se,
        lower = mean_var - se,
        min_date = base::min(date),
        max_date = base::max(date)
      ) %>%
      dplyr::ungroup()
  }

  # plot title
  if (base::is.null(group_by)) {
    title <- base::paste0("Time-Series Plot of ", var)
  } else {
    title <- base::paste0("Time Series Error Bars of ", var, " by ", group_by)
  }
  if (base::length(country) == 1) {
    title <- base::paste0(title, " for ", country)
  }
  subtitle <- base::paste0(base::format(base::min(res$min_date), "%m/%d/%Y"), " - ",
                           base::format(base::max(res$max_date), "%m/%d/%Y"))

  # plot line or error bars
  if (base::is.null(group_by)) {
    # line plot
    if (base::length(country) == 1) {
      g <- ggplot2::ggplot(res, ggplot2::aes(x = date, y = .data[[var]]))
    } else {
      g <- ggplot2::ggplot(res, ggplot2::aes(x = date, y = .data[[var]], color = location))
    }

    g <-
      g +
      ggplot2::geom_line() +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title, subtitle = subtitle)
  } else {
    # error bars
    if (base::length(country) == 1) {
      g <- ggplot2::ggplot(res, ggplot2::aes(x = forcats::fct_inorder(span), y = mean_var))
    } else {
      g <- ggplot2::ggplot(res, ggplot2::aes(x = forcats::fct_inorder(span), y = mean_var,
                                             color = location))
    }

    g <-
      g +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.4) +
      ggplot2::labs(title = title, subtitle = subtitle, x = group_by, y = var) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45))
  }

  # plot type
  if (plot_type == "static") {
    if (!base::is.null(group_by)) {
      # fix x-axis text position
      g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 0))
    }
    base::options(warn = -1)
    g
  } else {
    if (!base::is.null(group_by)) {



      # return(res)
      # TODO fix tooltips bug





      # custom tooltips
      g <-
      g %>%
      plotly::style(text = base::paste0("Span: ", res$span,
                                        "\nCountry: ", res$location,
                          "\nUpper: ", base::round(res$upper),
                          "\nmean(", var, "): ", base::round(res$mean_var),
                          "\nLower: ", base::round(res$lower)))
      # remove redundant tooltips
      if (base::length(country) == 1) {
        g <- plotly::style(g, hoverinfo = "skip", traces = 2)
      } else {
        # traces = -1, -2, -3 (do not skip these)
        g <- plotly::style(g, hoverinfo = "skip",
                           traces = base::seq(1, base::length(country)) * -1)
      }
    }

    # subtitle automatically disappears, here's the fix
    plotly::ggplotly(g) %>% plotly::layout(hovermode = "x",
                            title = base::list(text = base::paste0(title, "<br>",
                                                 "<sup>", subtitle, "</sup>")))
  }
}
