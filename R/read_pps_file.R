read_pps_file <- function(filename) {
  # read the header line
  # it looks like this
  # POSITION,5,D:\Hypercapnia\2025-07-02\Trial10-01.cine,mm,828,797,0.144888,
  # where the manual says that the information is
  #   "Indicates the type of report file this is, the number of points per image
  #    being tracked, the location of the file the measurements are being
  #    performed on, the unit of measure, the origin coordinates, and a scale
  #    factor used to calculate the results."
  hdr <- readLines(filename, n = 1) |>
    str_split_fixed(pattern = ',', n = 8)

  npt <- hdr[[2]] |> as.numeric()
  units <- hdr[[4]]

  data <-
    # read the rest of the file
    withr::with_options(
      list(rlib_name_repair_verbosity = "quiet"),
      readr::read_csv(
        filename,
        skip = 1, # skip the header line
        col_select = 1:(2 * npt + 3), # only take the correct number of columns
        show_col_types = FALSE # don't show info about the column types
      )
    ) |>

    # rename some of the columns with weird names
    rename(
      frame = `ImageNr.`,
      timefromtrig.s = `TimeFromTrig.`,
      abstime = `Absolute Time`
    )

  # process the absolute time
  # the format is weird: Wed Jul  2 2025 12:02:41.101812.75
  # I don't understand the extra decimal at the end, so we remove it
  data <- data |>
    mutate(
      abstime = str_match(abstime, '(.+)\\.\\d+$')[, 2],
      abstime = parse_date_time(abstime, orders = 'a b d Y H:M:OS')
    )

  # add the units to the X and Y coordinates
  data <- data |>
    rename_with(
      .cols = c(starts_with('X'), starts_with('Y')),
      \(n) paste(n, units, sep = '.')
    )

  # and add the remaining header information to the file
  data <- data |>
    mutate(
      cinefile = hdr[[3]],
      originx.pix = as.numeric(hdr[[5]]),
      originy.pix = as.numeric(hdr[[6]]),
      scale.mmperpix = as.numeric(hdr[[7]])
    )

  return(data)
}
