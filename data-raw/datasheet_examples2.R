summary_input <- "inst/extdata/initial_example/datasheets/summary_input.xls"
elg_input <- "inst/extdata/initial_example/elg/"
csv_output <- "inst/extdata/initial_example/datasheets/summary.csv"
a <- create_summary(summary_input = summary_input,
               elg_input = elg_input,
               csv_folder = "~/Desktop",
               csv_filename = "summary.csv")

data_input <- "inst/extdata/initial_example/datasheets/bottle_input.xls"
summary_input <- "inst/extdata/initial_example/datasheets/summary.csv"
data_type <- "bottle"
ros_input = "inst/extdata/initial_example/ctd/Cnv/"

a <- create_datasheet(data_input,
                      summary_input,
                      csv_folder = "~/Desktop",
                      csv_filename = "test.csv",
                      data_type = data_type,
                      ros_input = ros_input)


data_input <- "inst/extdata/initial_example/datasheets/ctd_input.xls"
summary_input <- "~/Desktop/summary.csv"
data_type <- "CTD"

a <- create_datasheet(data_input,
                      summary_input,
                      csv_folder = "~/Desktop",
                      csv_filename = "test.csv",
                      data_type = data_type)

data_input <- "inst/extdata/initial_example/datasheets/neuston_input.xls"
summary_input <- "~/Desktop/summary.csv"
data_type <- "neuston"

a <- create_datasheet(data_input,
                      summary_input,
                      csv_folder = "~/Desktop",
                      csv_filename = "test.csv",
                      data_type = data_type)