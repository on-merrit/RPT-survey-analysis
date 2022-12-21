create_var_overview <- function(path) {
  df <- read_csv(path, col_names = FALSE, n_max = 1, col_types = "c")

  out <- df %>%
    pivot_longer(everything(), names_to = "var_id", values_to = "var_full")

  write_csv(out, "data/processed/var_overview.csv")

  out
}

clean_raw_data <- function(df, wb_countries) {
  # remove test case
  df <- df %>%
    filter(X1 != "8")

  # remove cases that are not on a research contract
  df <- df %>%
    filter(X11 == "Yes")

  # remove tokens
  df <- df %>%
    select(-X6)

  # remove qualitative questions which might expose participants
  df <- df %>%
    select(-c(X14, X20, X42, X43, X46, X70, X74, X77, X79, X81, X83, X86, X92))

  # remove further columns which are superfluous/uniform across all responses
  df <- df %>%
    select(-c(X2, X4, X5, X13))

  # add country information
  # Categories from:
  # https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
  wb_country_selection <- wb_countries %>%
    select(Economy, Region)

  df <- df %>%
    left_join(wb_country_selection, by = c("X12" = "Economy"))

  # check for errors
  no_missing_countries <- df %>%
    filter(is.na(Region)) %>%
    nrow(.) == 0
  stopifnot(no_missing_countries)


  # return final df
  df
}


merge_disciplines <- function(df, disciplines_df) {
  left_join(df, disciplines_df, by = c("X90", "X91"))
}

read_clean_wb_countries <- function(file) {
  readxl::read_excel(file) %>%
    filter(!is.na(Region)) %>%
    mutate(Economy = str_remove(Economy, ",.*"),
           Economy = recode(Economy, `Russian Federation` = "Russia",
                            `Slovak Republic` = "Slovakia"))
}


custom_adorns <- function(tabyl) {
  tabyl %>%
    adorn_totals(where = c("row", "col")) %>%
    adorn_percentages() %>%
    adorn_pct_formatting() %>%
    adorn_ns()
}


# functions for comparing personal and institutional scores
# the function was further adapted to also incorporate other variants of how
# variables were set up
get_numeric_val <- function(x) {
  case_when(
    x == "Very important" ~ 1L,
    x == "Somewhat important" ~ 2L,
    x == "Important" ~ 2L,
    x == "Neither important nor unimportant" ~ 3L,
    x == "Somewhat unimportant" ~ 4L,
    x == "Unimportant" ~ 4L,
    x == "Very unimportant" ~ 5L,
    TRUE ~ NA_integer_
  )
}

get_values <- function(df, vars, source = c("institutional", "personal"),
                       var_overview) {
  base <- df %>%
    select(X1, {{ vars }}) %>%
    pivot_longer(-X1, names_to = "var", values_to = "val") %>%
    mutate(num_val = get_numeric_val(val)) %>%
    filter(!is.na(num_val)) %>%
    mutate(source = source)

  # get labels
  vars_char <- deparse(substitute(vars))
  begin <- str_extract(vars_char, "\\d{2}") %>% as.numeric()
  end <- str_extract(vars_char, "\\d{2}$") %>% as.numeric()

  labels <- var_overview %>%
    filter(var_id %in% paste0("X", begin:end)) %>%
    mutate(label = str_extract(var_full, "(?<=\\[).*?(?=\\s?\\])"),
           # clean up labels
           label = str_remove(label, "\\(.*"),
           label = str_remove(label, ", as assessed .*")) %>%
    select(var_id, label)

  base %>%
    left_join(labels, by = c("var" = "var_id")) %>%
    select(-var)
}

bootstrap_values <- function(df) {
  df %>%
    group_by(label, source) %>%
    summarise(res = list(Hmisc::smean.cl.boot(num_val))) %>%
    unnest_wider(res)
}

five_point_scale <- function(compact = FALSE) {
  if (compact) {
    list(scale_x_continuous(
      breaks = c(1, 5),
      labels = c("Important", "Unimportant")),
      coord_cartesian(xlim = c(1, 5))
    )
  } else {
    list(scale_x_continuous(
      breaks = c(1, 3, 5),
      labels = c("Very important", "Neither/nor", "Very unimportant")),
      coord_cartesian(xlim = c(1, 5))
    )
  }
}


# correspondence analysis functions -----
#' ca plots with ggplot2
#'
#' @export
plot_ca <- function(object, font_size = 3, dimensions = c(1, 2),
                    show.legend = F, map = "symmetric", keep_labels = FALSE) {
  ca_class <- class(object)
  if (!identical(ca_class, "ca") & !identical(ca_class, "mjca")) {
    stop("Input object must be of type 'ca' or 'mjca'")
  }
  assertthat::assert_that(length(dimensions) == 2)


  # find variances for labelling of axes
  variances <- suppressWarnings(summary(object))

  dim1 <- variances$scree[dimensions[1], 3] %>% round(., 2) %>% unname()
  dim2 <- variances$scree[dimensions[2], 3] %>% round(., 2) %>% unname()

  # create labels
  x_label <- paste0("Dimension ", dimensions[1], " (", dim1, "%)")
  y_label <- paste0("Dimension ", dimensions[2], " (", dim2, "%)")


  augmented_data <- extract_ca_data(object, dimensions, map, keep_labels)




  # separate plotting for 'ca' and 'mjca'
  if (identical(ca_class, "ca")) {
    if (sum(!is.na(augmented_data$row_data$sup_var)) == 0) { # catch case with no sup_vars
      # stop("Keine Sup_vars, muss ich noch implementieren")
      ggplot(augmented_data$full_data, aes(x = x, y = y, colour = Profil)) +
        ggrepel::geom_text_repel(aes(label = rowname), size = font_size,
                                 show.legend = F, max.iter = 5000, force = 4) +
        geom_point(show.legend = show.legend, size = 2) +
        geom_hline(yintercept = 0, linetype = "dashed", alpha = .5) +
        geom_vline(xintercept = 0, linetype = "dashed", alpha = .5) +
        labs(x = x_label,
             y = y_label,
             colour = NULL)
    } else {
      ggplot(augmented_data$full_data, aes(x = x, y = y, colour = Profil,
                                           shape = sup_var)) +
        ggrepel::geom_text_repel(aes(label = rowname), size = font_size,
                                 show.legend = F, max.iter = 5000, force = 4) +
        geom_point(show.legend = show.legend, size = 2) +
        geom_hline(yintercept = 0, linetype = "dashed", alpha = .5) +
        geom_vline(xintercept = 0, linetype = "dashed", alpha = .5) +
        labs(x = x_label,
             y = y_label,
             shape = NULL,
             colour = NULL)
    }
  } else if (identical(ca_class, "mjca")) {

    if (sum(!is.na(augmented_data$col_data$sup_var)) == 0) {
      # catch case with no sup_vars
      ggplot(augmented_data$col_data, aes(x = x, y = y)) +
        plot_parts(font_size = font_size, x_label = x_label, y_label = y_label,
                   show.legend = show.legend)
    } else {
      ggplot(augmented_data$col_data, aes(x = x, y = y, colour = sup_var)) +
        plot_parts(font_size = font_size, x_label = x_label, y_label = y_label,
                   show.legend = show.legend)
    }
  }

}




#' Parts for plotting ca
plot_parts <- function(font_size = font_size, x_label = x_label,
                       y_label = y_label, show.legend = show.legend) {
  list(
    geom_point(show.legend = show.legend),
    ggrepel::geom_text_repel(aes(label = rowname), size = font_size, show.legend = F,
                             force = 2, max.iter = 5000),
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .5),
    geom_vline(xintercept = 0, linetype = "dashed", alpha = .5),
    labs(x = x_label,
         y = y_label,
         shape = NULL,
         colour = NULL),
    theme(legend.position = "bottom", legend.direction = "horizontal")
  )
}

#' Add rownames as column
#'
#' Helper function to add rownames
prepare_data <- function(x) {
  x <- x %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  colnames(x) <- c("rowname", "x", "y")
  x
}

#' @export
extract_ca_data <- function(object, dimensions = c(1, 2),
                            map = "symmetric", keep_labels = FALSE) {

  ca_class <- class(object)

  # find points to plot
  pdf(file = NULL)
  base_data <- plot(object, dim = dimensions, map)
  dev.off()

  # add rownames
  all_data <- base_data %>%
    map(prepare_data)

  # extract col_data
  col_data <- all_data[["cols"]] %>%
    mutate(Profil = rep("Spaltenprofil", length(rowname)),
           rowname = if (!keep_labels) {
             stringr::str_replace(rowname, "(.*):", "")
           } else { rowname } )


  # find supplementary cols
  if (identical(ca_class, "mjca")) {
    col_data <- col_data %>%
      slice(object$colsup) %>%
      mutate(sup_var = factor("Supplementary Variables", levels =
                                c("Contributing Variables", "Supplementary Variables"))) %>%
      dplyr::select(rowname, sup_var) %>%
      full_join(col_data, by = "rowname") %>%
      replace_na(list(sup_var = "Contributing Variables"))

    return(list(col_data = col_data))

  } else if (identical(ca_class, "ca")) {
    # create rowdata, in case class is 'ca'
    row_data <- all_data[["rows"]] %>%
      mutate(Profil = rep("Zeilenprofil", length(rowname)))

    if (length(object$rowsup) > 0) {
      row_data <- row_data %>%
        slice(object$rowsup) %>%
        mutate(sup_var = factor("Supplementary Variables", levels =
                                  c("Contributing Variables", "Supplementary Variables"))) %>%
        dplyr::select(rowname, sup_var) %>%
        full_join(row_data, by = "rowname") %>%
        replace_na(list(sup_var = "Contributing Variables"))
    }
    full_data <- bind_rows(col_data, row_data) %>%
      replace_na(list(sup_var = "Contributing Variables"))
  }

  list(full_data = full_data, col_data = col_data, row_data = row_data)
}
