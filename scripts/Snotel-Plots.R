# --- 1. LIBRARIES ---
# Load all required libraries once at the beginning
library(ggplot2)
library(tidyverse)
library(data.table)
library(ggforce)
library(lubridate) # Added for floor_date, month, year

# --- NEW: Increase download timeout for slow government servers ---
options(timeout = 300)

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

#' Generate and save a Snow Water Equivalent (SWE) or Precip plot
#'
#' @param data_url URL to the .csv data file
#' @param plot_title The main title for the chart
#' @param y_axis_interval The numeric interval for y-axis breaks
#' @param output_filename The filename to save the plot as
#' @param current_wy_col The string name of the column for the "current" water year (e.g., "WY2026")
#' @param previous_wy_col The string name of the column for the "previous" water year (e.g., "WY2025")
#' @param y_axis_label The label for the y-axis.
#' @param show_future_rect (Boolean) Toggle to show/hide the gray "future" rectangle.
#' @param show_past_future_anno (Boolean) Toggle to show/hide "PAST" / "FUTURE" arrows.
#' @param anno_text_offset (Numeric) Vertical offset for annotation text.
#' @param days_to_remove (Numeric) Number of days to strip from the end of the current WY (to fix bad data).

create_swe_plot <- function(data_url,
                            plot_title,
                            y_axis_interval,
                            output_filename,
                            current_wy_col,
                            previous_wy_col,
                            y_axis_label = "Snow Water Equivalent (in)",
                            show_future_rect = TRUE,
                            show_past_future_anno = TRUE,
                            anno_text_offset = 0.7,
                            days_to_remove = 0) {
  
  # --- 3a. Data Loading and Processing ---
  csv_name <- paste0(tools::file_path_sans_ext(output_filename), "_data.csv")
  
  # Direct connection using httr from the new macOS runner IP
  response <- httr::GET(
    data_url,
    httr::add_headers(
      `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
      `Accept` = "text/csv, application/csv, */*"
    ),
    httr::write_disk(csv_name, overwrite = TRUE)
  )
  
  # Check if the download was successful
  if (httr::status_code(response) != 200) {
    stop(paste("Download failed - HTTP status:", httr::status_code(response)))
  }
  
  # Read from the successfully downloaded local file
  snow_data <- data.table::fread(csv_name)

  # NEW: Save the downloaded CSV to the current folder
  # This creates a name like "Salt-VerdeSWE_data.csv" based on your plot name
  csv_name <- paste0(tools::file_path_sans_ext(output_filename), "_data.csv")
  data.table::fwrite(snow_data, csv_name)

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
  snow_filt <- snow_data[, ..selected_cols] %>%
    mutate(
      mon = as.numeric(str_extract(date, "^[0-9]{2}")),
      # Create a "dummy" date series from 1979-10-01 to 1980-09-30 for plotting
      date = as.Date(paste0(date, "-", ifelse(mon > 9, 1979, 1980)), "%m-%d-%Y")
    ) %>%
    pivot_longer(!c(date, mon), names_to = "WY", values_to = "swe") %>%
    mutate(
      WY = case_when(
        WY == current_wy_col ~ current_wy_label,
        WY == previous_wy_col ~ previous_wy_label,
        .default = "Median 1991-2020"
      )
    )
  
  # --- LEAP YEAR CHECK ---
  target_year <- as.numeric(gsub("\\D", "", current_wy_col))
  if (!lubridate::leap_year(target_year)) {
    snow_filt <- snow_filt %>%
      mutate(swe = ifelse(format(date, "%m-%d") == "02-29" & WY == current_wy_label, 
                          NA, swe))
  }

  # --- TRIMMING BAD DATA AT END ---
  # If the user specifies days to remove, we find the last entries for the current WY and make them NA
  if (days_to_remove > 0) {
    # Find row indices for the current water year that aren't already NA
    current_wy_indices <- which(snow_filt$WY == current_wy_label & !is.na(snow_filt$swe))
    
    if (length(current_wy_indices) >= days_to_remove) {
      # Target the very last N indices recorded
      to_remove <- tail(current_wy_indices, days_to_remove)
      snow_filt$swe[to_remove] <- NA
    }
  }

  # --- 3b. Plot Calculations ---
  # Get the most recent data point for the current water year (post-trimming)
  curWY <- snow_filt %>%
    filter(WY == current_wy_label & !is.na(swe)) %>% 
    filter(date == max(date))

  # Find the peak SWE for the current water year
  curWY_maxSWE <- snow_filt %>%
    filter(WY == current_wy_label) %>% 
    filter(swe == max(swe, na.rm = TRUE))

  # Dynamic Y-axis limit calculation
  maxswe <- max(snow_filt$swe, na.rm = TRUE)
  base <- y_axis_interval
  ymax <- ifelse((maxswe %% base) > (base / 2),
    (maxswe - (maxswe %% base)) + 2 * base,
    (maxswe - (maxswe %% base)) + base
  )

  # --- 3c. Generate Plot ---
  swe_plot <- ggplot(data = snow_filt) +
    {
      if (show_future_rect) {
        geom_rect(
          data = data.frame(
            from = from_date,
            to = as.Date("1980-09-30")
          ),
          aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf),
          fill = "gray95", alpha = 0.7
        )
      }
    } +
    geom_line(aes(x = date, y = swe, color = factor(WY), linewidth = factor(WY))) + 
    scale_color_manual(values = c("black", "#9A3324", "#25819C")) +
    scale_linewidth_manual(values = c(0.8, 0.8, 1.2)) + 
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
      color = "", x = NULL, linewidth = "", y = y_axis_label
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, color = "black", size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "black", size = 12),
      panel.grid.major = element_line(colour = "grey85"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "black", size = 10),
      axis.title.y = element_text(margin = margin(r = 0.15, l = 0.1, unit = "in")),
      legend.position = "bottom",
      legend.text = element_text(size = 12)
    )

  # --- 3d. Conditional Annotations ---
  if (show_past_future_anno) {
    anno_y_base <- ymax - (base / 2)
    anno_y_text <- anno_text_offset + anno_y_base 

    if (curWY$date > min(snow_filt$date) + 20) {
      swe_plot <- swe_plot +
        annotate("segment", x = curWY$date - 1, yend = anno_y_base, xend = curWY$date - 18, 
                 y = anno_y_base, linewidth = 0.8, arrow = arrow(length = unit(2, "mm"))) +
        annotate("text", x = curWY$date - 3, y = anno_y_text, label = "PAST", size = 2.4, hjust = "right")
    }

    if (curWY$date < max(snow_filt$date, na.rm = TRUE) - 20) {
      swe_plot <- swe_plot + 
        annotate("segment", x = curWY$date + 1, yend = anno_y_base, xend = curWY$date + 18, 
                 y = anno_y_base, linewidth = 0.8, arrow = arrow(length = unit(2, "mm"))) +
        annotate("text", x = curWY$date + 3, y = anno_y_text, label = "FUTURE", size = 2.4, hjust = "left")
    }
  }

  # --- 3f. Save Plot ---
  ggsave(output_filename, plot = swe_plot, width = 6.5, height = 4.75)
}


# --- 4. FUNCTION CALLS ---
# Now, just call the function twice with the specific parameters for each plot

# Plot 1: Salt-Verde
# Uses WY2026 as the current year and WY2025 as the previous year
create_swe_plot(
  data_url = "https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC4/1506_Salt.csv",
  plot_title = "Salt-Verde River Basin Snowpack",
  y_axis_interval = 2,
  output_filename = "Salt-VerdeSWE.png",
  current_wy_col = "WY2026",
  previous_wy_col = "WY2025",
  anno_text_offset = 0.3,
  days_to_remove = 0
  # y_axis_label uses default
  # Toggles are left as default (TRUE)
  # Example to turn off:
  # show_future_rect = FALSE,
  # show_past_future_anno = FALSE
)

# Plot 2: Upper Colorado
# Uses WY2025 as the current year and WY2024 as the previous year
# This matches the probable intent of your original script and fixes the bug.
create_swe_plot(
  data_url = "https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC2/14_Upper_Colorado_Region.csv",
  plot_title = "Colorado River Basin Snowpack Above Lake Powell",
  y_axis_interval = 5,
  output_filename = "AbvPowellSWE.png",
  current_wy_col = "WY2026",
  previous_wy_col = "WY2025",
  days_to_remove = 0
  # y_axis_label uses default
)

# Plot 3: Lake Mead
# Uses WY2026 as the current year and WY2025 as the previous year
create_swe_plot(
  data_url = "https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC6/150100_Lower_Colorado-Lake_Mead.csv",
  plot_title = "Lower Colorado Lake Mead Basin Snowpack",
  y_axis_interval = 5,
  output_filename = "LakeMeadSWE.png",
  current_wy_col = "WY2026",
  previous_wy_col = "WY2025",
  days_to_remove = 0
)

# Plot 4: Salt Precip
# Uses WY2026 as the current year and WY2025 as the previous year
create_swe_plot(
  data_url = "https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/PREC/assocHUC4/1506_Salt.csv",
  plot_title = "Salt-Verde Basin",
  y_axis_interval = 5,
  output_filename = "Salt-VerdePrecip.png",
  current_wy_col = "WY2026",
  previous_wy_col = "WY2025",
  y_axis_label = "Precipitation Accumulation (in.)",
  show_future_rect = FALSE,
  show_past_future_anno = FALSE
)

# Plot 5: Upper Colorado Precip
# Uses WY2026 as the current year and WY2025 as the previous year
create_swe_plot(
  data_url = "https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/PREC/assocHUC2/14_Upper_Colorado_Region.csv",
  plot_title = "Upper Colorado Basin Above Lake Powell",
  y_axis_interval = 5,
  output_filename = "UpperColoradoPrecip.png",
  current_wy_col = "WY2026",
  previous_wy_col = "WY2025",
  y_axis_label = "Precipitation Accumulation (in.)",
  show_future_rect = FALSE,
  show_past_future_anno = FALSE
)

# Plot 6: Lake Mead Precip
# Uses WY2026 as the current year and WY2025 as the previous year
create_swe_plot(
  data_url = "https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/PREC/assocHUC6/150100_Lower_Colorado-Lake_Mead.csv",
  plot_title = "Lower Colorado Lake Mead Basin",
  y_axis_interval = 5,
  output_filename = "LakeMeadPrecip.png",
  current_wy_col = "WY2026",
  previous_wy_col = "WY2025",
  y_axis_label = "Precipitation Accumulation (in.)",
  show_future_rect = FALSE,
  show_past_future_anno = FALSE
)
