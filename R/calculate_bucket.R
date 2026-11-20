#' Calculate Bucket
#' Find the natural breaks (jenks) of  a set of data values.
#' If data contains negative and positive values, breaks are split at zero.
#' Used primarily as an input to chloropleth mapping.

#' @param values_df Dataframe column or vector. The set of values that you want to find the natural breaks of
#' @param max_bin Max number of bins you want to return. May return fewer bins based on data distribution
#'
#' @returns List of breaks and labels for break ranges
#'
#' @export
#' @examples
#' #cars <- mtcars
#' #test <-calculate_bucket(values_df = cars$mpg)
#'
calculate_bucket <- function(
  values_df,
  max_bin = 10
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

  return(list(
    breaks = unique(breaks),
    breaks_label = sapply(1:length(breaks_adjusted_for_label), function(x) {
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
    })[-1]
  ))
}
