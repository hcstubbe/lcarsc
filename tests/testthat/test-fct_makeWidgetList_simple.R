test_that("multiplication works", {
  library(readr)
  library(shiny)
  widgets_table = readr::read_csv("test_widgets.csv")
  widget_data = widgets_table[widgets_table$widget == TRUE,]
  widget_list = makeWidgetList_simple(widget_data = widget_data,
                              ns = shiny::NS("test"),
                              pid = "test-pid",
                              tbl_id = "test_tbl_id")
  expect_equal(length(widget_list), 36)
  for (i in 1:length(widget_list)) {
    expect_equal(class(widget_list[[i]]), "shiny.tag")
  }
})
