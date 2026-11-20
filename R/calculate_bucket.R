#' Calculate Bucket
#' Find the natural breaks (jenks) of  a set of data values.
#' If data contains negative and positive values, breaks are split at zero.
#' Used primarily as an input to chloropleth mapping.

#' @param values_df Dataframe column or vector. The set of values that you want to find the natural breaks of
#' @param max_bin Numeric Max number of bins you want to return. May return fewer bins based on data distribution
#' @param color_below Character Hex code of color to use for negative values. Defaults to '#e34a33'
#' @param color_above Character Hex code of color to use for positive values. Defaults to '#2166ac'
#' @param color_center Character Hex code of color to use for zero. Defaults to '#FFFFFF'
#' @returns List of breaks and labels for break ranges
#'
#' @export
#' @examples
#' #cars <- mtcars
#' #test <-calculate_bucket(values_df = cars$mpg)
#'
calculate_bucket <- function(
  values_df,
  max_bin = 10,
  color_below = '#e34a33',
  color_above = '#2166ac',
  color_center = 'white'
) {
  if (sum(unique(values_df)) == 0) {
    #handle cases where all values are zero

    breaks <- c(0, 0)
  } else if ((min(values_df) * max(values_df)) >= 0) {
    #changed this to not reference rounded values as it was causing issues with situations where 1 was getting rounded down to 0
    ## when both are negative/positive/invovles 0
    print("oneside")

    pos_bins <- BAMMtools::getJenksBreaks(values_df, k = max_bin)

    if (0 %in% (as.vector(pos_bins))) {
      breaks <- as.vector(pos_bins)
    } else {
      breaks <- c(0, as.vector(pos_bins))
    }
  } else {
    ## when one is positive and one is negative
    ## need to make sure 0 is always in the breaks

    #figure out what percent of the data is below/above zero
    values_df <- values_df[values_df != 0] #remove zeros from data set
    percent_positive <- length(values_df[values_df > 0]) / length(values_df)

    pos_bin_num <- round(percent_positive * max_bin, 0) #multiply by bin num to split bins based on data distribution

    neg_bin_num <- if (max_bin - pos_bin_num < 2) {
      3
    } else {
      max_bin - pos_bin_num
    }

    pos_bin_num_rev <- if (pos_bin_num < 2) {
      3
    } else {
      pos_bin_num
    }

    neg_df <- values_df[values_df < 0]

    neg_jenks <- BAMMtools::getJenksBreaks(neg_df, k = neg_bin_num) #

    pos_df <- values_df[values_df > 0]

    pos_jenks <- BAMMtools::getJenksBreaks(pos_df, k = pos_bin_num_rev)

    breaks <- c(neg_jenks, 0, pos_jenks)
  }

  breaks_adjusted_for_label <- unique(breaks) #added unique because the percent change was resulting in two -100s for some reason

  breaks_above <- breaks_adjusted_for_label[breaks_adjusted_for_label >= 0]
  breaks_below <- breaks_adjusted_for_label[breaks_adjusted_for_label <= 0]

  max_bin_oneside <- max(
    length(breaks_adjusted_for_label[breaks_adjusted_for_label <= 0]),
    length(breaks_adjusted_for_label[breaks_adjusted_for_label >= 0])
  ) #includes zerod

  if (sum(breaks_below) == 0) {
    ## when there are two 0 in breaks (only one region), we want to create a unique color group 100-100 that's white
    ## also max_bin_oneside gets two 0 counted, need to take 1 out
    ## in the end we end up with 3 colors: slightly above, at center, slightly below
    color_val <- colorRampPalette(
      colors = c(color_center, color_above),
      space = "Lab"
    ) #(breaks_above) #only pos bins, all neg are zero
  } else if (sum(breaks_above) == 0) {
    color_val <- colorRampPalette(
      colors = c(color_below, color_center),
      space = "Lab"
    ) #(breaks_below) #only neg bins, all pos are zero
  } else {
    # create symmetric colors with max_bin_oneside+1 (considering white color) then take white out
    # then for each side take the effective ones (drop the ones not needed)

    # negative: (length(neg_jenks)-1) is number of effective color groups for negative ones; if it's 1 we want to take the last color
    color_val_negative <- colorRampPalette(
      colors = c(color_below, color_center),
      space = "Lab"
    ) #(breaks_below)

    # positive: (length(pos_jenks)-1) is number of effective color groups for positive ones; if it's 1 we want to take the first color
    color_val_positive <- colorRampPalette(
      colors = c(color_center, color_above),
      space = "Lab"
    ) #(breaks_above)
    neg_out <- color_val_negative(length(breaks_below))
    pos_out <- color_val_positive(length(breaks_above))[-1] #remove white from positive list to avoid doubles

    color_val <- c(
      neg_out,
      pos_out
    )
  }
  #[-1] removes first value from all returns as it is an empty value for the labels
  return(list(
    breaks = unique(breaks)[-1],
    metric_color_group = color_val[-1],
    metric_color_label = sapply(
      1:length(breaks_adjusted_for_label),
      function(x) {
        if (x == 1 & breaks_adjusted_for_label[x + 1] == 0) {
          #adding case for when there is one value below zero and that value is first in the palette list
          paste0(round(breaks_adjusted_for_label[x], 2), ' to 0')
        } else if (x > 1) {
          paste0(
            round(breaks_adjusted_for_label[x - 1], 2),
            ' to ',
            round(breaks_adjusted_for_label[x], 2)
          )
        } else if (breaks_adjusted_for_label[x] == 0) {
          paste0("0")
        } else {
          ''
        }
      }
    )[-1]
  ))
}
