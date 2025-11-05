# --- 1. LIBRARIES ---
# Load all required libraries once at the beginning
library(ggplot2)
library(tidyverse)
library(data.table)
library(ggforce)
library(lubridate) # Added for floor_date, month, year

# --- 2. GLOBAL VARIABLES ---
# Calculate the 'from' date once, as it's the same for all plots
# This creates a date for the current month and day in the "dummy" water year
from_date <- as.Date(
  paste0(
    ifelse(month(Sys.Date()) >= 1 & month(Sys.Date()) <= 9,
      "1980-",
      "1979-"
    ),
    format(Sys.Date(), "%m-%d")
  )
)

# --- 3. PLOT FUNCTION ---
# Define a single function to download, process, and plot the data
# This eliminates ~90% of the code duplication

#' Generate and save a Snow Water Equivalent (SWE) plot
#'
#' @param data_url URL to the .csv data file
#' @param plot_title The main title for the chart
#' @param y_axis_interval The numeric interval for y-axis breaks (e.g., 2 or 5)
#' @param rect_fill The fill color for the 'future' geom_rect
#' @param anno_y_base The base y-coordinate for annotation arrows
#' @param anno_y_text The y-coordinate for annotation text ("PAST"/"FUTURE")
#' @param output_filename The filename to save the plot as (e.g., "plot.png")
#' @param current_wy_col The string name of the column for the "current" water year (e.g., "WY2026")
#' @param previous_wy_col The string name of the column for the "previous" water year (e.g., "WY2025")

create_swe_plot <- function(data_url,
                            plot_title,
                            y_axis_interval,
                            rect_fill,
                            anno_y_base,
                            anno_y_text,
                            output_filename,
                            current_wy_col,
                            previous_wy_col) {
  # --- 3a. Data Loading and Processing ---
  snow_data <- data.table::fread(data_url)

  # Get location of column names that end with a number (years)
  cnames <- grep("\\D+$", colnames(snow_data), invert = TRUE)

  # Add the prefix "WY" to the column names selected above
  colnames(snow_data)[cnames] <- paste0("WY", colnames(snow_data)[cnames])

  # Define the columns to select dynamically
  selected_cols <- c("date", previous_wy_col, current_wy_col, "Median ('91-'20)")

  # Create labels dynamically from the column names
  current_wy_label <- paste("Water Year:", sub("WY", "", current_wy_col))
  previous_wy_label <- paste("Water Year:", sub("WY", "", previous_wy_col))

  # Filter and shape the data
  # NOTE: This now dynamically selects the columns passed to the function
  snow_filt <- snow_data[, ..selected_cols] %>%
    mutate(
      mon = as.numeric(str_extract(date, "^[0-9]{2}")),
      # Create a "dummy" date series from 1979-10-01 to 1980-09-30 for plotting
      date = as.Date(paste0(date, "-", ifelse(mon > 9, 1979, 1980)), "%m-%d-%Y")
    ) %>%
    pivot_longer(!c(date, mon), names_to = "WY", values_to = "swe") %>%
    mutate(
      WY = case_when(
        # Use the dynamic column and label variables
        WY == current_wy_col ~ current_wy_label,
        WY == previous_wy_col ~ previous_wy_label,
        .default = "Median 1991-2020"
      )
    )

  # --- 3b. Plot Calculations ---
  # Get the most recent data point for the current water year
  curWY <- snow_filt %>%
    filter(WY == current_wy_label & !is.na(swe)) %>% # Use dynamic label
    filter(date == max(date))

  # Find the peak SWE for the current water year
  curWY_maxSWE <- snow_filt %>%
    filter(WY == current_wy_label) %>% # Use dynamic label
    filter(swe == max(swe, na.rm = T))

  # Find the median SWE on the same date as the current year's peak
  med_SWE <- snow_filt %>%
    filter(str_detect(WY, "Median")) %>%
    filter(date == curWY_maxSWE$date)

  # Dynamic Y-axis limit calculation
  maxswe <- max(snow_filt$swe, na.rm = TRUE)
  base <- y_axis_interval
  ymax <- ifelse((maxswe %% base) > (base / 2),
    (maxswe - (maxswe %% base)) + 2 * base,
    (maxswe - (maxswe %% base)) + base
  )

  # --- 3c. Generate Plot ---
  swe_plot <- ggplot(data = snow_filt) +
    # Add rectangle for the "future" part of the water year
    geom_rect(
      data = data.frame(
        from = from_date,
        to = as.Date("1980-09-30")
      ),
      aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf),
      fill = rect_fill, alpha = 0.7
    ) +
    # Add SWE lines
    geom_line(aes(x = date, y = swe, color = factor(WY), linewidth = factor(WY))) + # Changed size to linewidth
    scale_color_manual(values = c("black", "#9A3324", "#25819C")) +
    scale_linewidth_manual(values = c(0.8, 0.8, 1.2)) + # Changed scale_size_manual to scale_linewidth_manual
    scale_x_date(
      date_breaks = "1 months", date_labels = "%b",
      limits = c(as.Date("1979-10-01"), as.Date("1980-09-30")),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, ymax),
      breaks = seq(0, ymax, y_axis_interval),
      expand = c(0, 0)
    ) +
    labs(
      title = plot_title,
      subtitle = paste0("(as of ", format(curWY$date, "%B %d, "), year(Sys.Date()), ")"),
      color = "",
      x = NULL, linewidth = "", y = "Snow Water Equivalent (in)" # Changed size to linewidth
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5,
                                color = "black",
                                size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "black", size = 12),
      panel.grid.major = element_line(colour = "grey85"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(
        vjust = 0.5,
        hjust = 0.5, color = "black",
        size = 10
      ),
      axis.text.y = element_text(color = "black", size = 10),
      axis.title = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 0.15, l = 0.1, unit = "in")),
      legend.position = "bottom",
      legend.text = element_text(size = 12)
    )

  # --- 3d. Conditional Annotations ---

  # Add "PAST" annotation
  if (curWY$date > min(snow_filt$date) + 20) {
    swe_plot <- swe_plot +
      annotate(
        geom = "segment",
        x = curWY$date - 1, yend = anno_y_base,
        xend = curWY$date - 18, y = anno_y_base,
        linewidth = 0.8,
        arrow = arrow(length = unit(2, "mm"))
      ) +
      annotate(
        geom = "text", x = curWY$date - 3, y = anno_y_text,
        label = "PAST", size = 2.4,
        hjust = "right"
      )
  }

  # Add "FUTURE" annotation
  if (curWY$date < max(snow_filt$date, na.rm = TRUE) - 20) {
    swe_plot <- swe_plot + annotate(
      geom = "segment",
      x = curWY$date + 1, yend = anno_y_base,
      xend = curWY$date + 18, y = anno_y_base,
      linewidth = 0.8,
      arrow = arrow(length = unit(2, "mm"))
    ) +
      annotate(
        geom = "text", x = curWY$date + 3, y = anno_y_text,
        label = "FUTURE", size = 2.4,
        hjust = "left"
      )
  } else {
    # Handle case where "FUTURE" arrow would go off-plot
    swe_plot <- swe_plot + annotate(
      geom = "segment",
      x = curWY$date + 1, yend = anno_y_base,
      xend = curWY$date - 2 +
        as.double(difftime(max(snow_filt$date, na.rm = TRUE),
          curWY$date,
          units = c("days")
        )), y = anno_y_base,
      linewidth = 0.6,
      arrow = arrow(length = unit(2, "mm"))
    )
  }

  # --- 3e. (Commented) Peak SWE Annotation ---
  # This is the original commented-out block, placed inside the function
  # You can re-enable and customize coordinates as needed

  # caption = paste(strwrap(paste0(
  #   "SWE peaked at ",
  #   round(curWY_maxSWE$swe/med_SWE$swe*100,0),
  #   "% of the peak seasonal median on ",
  #   format(curWY_maxSWE$Date, "%B %d, %Y"),"."),
  #   25),
  #   collapse = "\n")
  #
  # swe_plot <- swe_plot +
  #   annotate(
  #     geom = "curve", xend = curWY_maxSWE$date,
  #     yend = curWY_maxSWE$swe + 0.2,x = as.Date("1980-01-31"), y= (ymax * 0.8), # Example dynamic Y
  #     curvature = -.3, arrow = arrow(length = unit(2, "mm"))
  #   ) +
  #   annotate("label",
  #     x = as.Date("1979-12-31"),
  #     y = (ymax * 0.8), # Example dynamic Y
  #     label = caption,
  #     size = 3,
  #     label.padding=unit(0.5, "lines"),
  #     fill = "grey95")

  # --- 3f. Save Plot ---
  ggsave(output_filename,
    plot = swe_plot,
    width = 6.5, height = 4.75
  )
  
  # Optional: Print the plot to the R session
  print(swe_plot)
}


# --- 4. FUNCTION CALLS ---
# Now, just call the function with the specific parameters for each plot

# Plot 1: Salt-Verde
# Uses WY2026 as the current year and WY2025 as the previous year
create_swe_plot(
  data_url = "https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC4/1506_Salt.csv",
  plot_title = "Salt-Verde River Basin",
  y_axis_interval = 2,
  rect_fill = "gray95",
  anno_y_base = 7.4,
  anno_y_text = 7.7,
  output_filename = "Salt-VerdeSWE.png",
  current_wy_col = "WY2026",
  previous_wy_col = "WY2025"
)

# Plot 2: Upper Colorado
# Uses WY2026 as the current year and WY2025 as the previous year
create_swe_plot(
  data_url = "https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC2/14_Upper_Colorado_Region.csv",
  plot_title = "Colorado River Basin Above Lake Powell",
  y_axis_interval = 5,
  rect_fill = "gray97",
  anno_y_base = 18.5,
  anno_y_text = 19.2,
  output_filename = "AbvPowellSWE.png",
  current_wy_col = "WY2026",
  previous_wy_col = "WY2025"
)

# Plot 3: Lake Mead
# Uses WY2026 as the current year and WY2025 as the previous year
create_swe_plot(
  data_url = "https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/150100_Lower_Colorado-Lake_Mead.csv",
  plot_title = "Lower Colorado Lake Mead Basin",
  y_axis_interval = 5,
  rect_fill = "gray97",
  anno_y_base = 12.5,
  anno_y_text = 13.2,
  output_filename = "LakeMeadSWE.png",
  current_wy_col = "WY2026",
  previous_wy_col = "WY2025"
)

# Plot 3: Lake Mead
# Uses WY2026 as the current year and WY2025 as the previous year
create_swe_plot(
  data_url = "https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/PREC/assocHUC6/150100_Lower_Colorado-Lake_Mead.csv",
  plot_title = "Lower Colorado Lake Mead Basin",
  y_axis_interval = 5,
  rect_fill = "gray97",
  anno_y_base = 18.5,
  anno_y_text = 19.2,
  output_filename = "LakeMeadPrecip.png",
  current_wy_col = "WY2026",
  previous_wy_col = "WY2025"
)
