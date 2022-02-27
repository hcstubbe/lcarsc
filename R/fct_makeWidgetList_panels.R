#' makeWidgetList_panels
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# Make widget list
makeWidgetList_panels = function(widget_data, all_tabs, all_visits, visit_id, row_id = NULL, ns, pid, tbl_id, col_size = 4){

  makeWidgetList_visits = function(widget_data, ns, pid, widget_subset, tbl_id){
    define_widget(widget_data = widget_data, ns = ns, pid = pid, selection = which(widget_data$panel == widget_subset), tbl_id = tbl_id)
  }



  ## Compile widget list
  widget_data_i = widget_data[widget_data$widget == TRUE & widget_data[,visit_id] == TRUE,]
  widget_data_i_levels = levels(factor(widget_data_i$panel))
  if(length(widget_data_i_levels) > 1){
    widget_list=sapply(widget_data_i_levels,
                       function(x){
                         makeWidgetList_visits(widget_data = widget_data_i, widget_subset = x, ns=ns, pid=pid)
                       }
    )
  }else{
    widget_list = makeWidgetList_visits(widget_data = widget_data_i, widget_subset = widget_data_i_levels, ns=ns, pid=pid)
  }


  ## Function puts widgets into panels
  make_panels = function(widget_i){
    div(tabPanel("Plot",
                 wellPanel(
                   (
                     widget_i
                   ))))
  }



  ## Compile all panels

  if(length(widget_data_i_levels) > 1){
    panel_list = lapply(widget_list, make_panels)
  }else{
    panel_list = list(make_panels(widget_list))
  }

  ## Function for putting panels into tabs of navbar
  make_navbar_tabs = function(i, panel_list, all_tabs, visit_id){

    tab_title_i = all_tabs$tab_title[i]
    panels = list(panel1up = panel_list[all_tabs$panel1up[i]],
                  panel2up = panel_list[all_tabs$panel2up[i]],
                  panel1down = panel_list[all_tabs$panel1down[i]],
                  panel2down = panel_list[all_tabs$panel2down[i]])

    if(length(panel_list)>1){panels = panels[!unlist(lapply(panels, function(x) lapply(x, is.null)))]

    panel1up = panels[1]
    panel2up = panels[2]
    panel1down = panels[3]
    panel2down = panels[4]
    }else{
      panel1up = panel_list[[1]]
      panel2up = NULL
      panel1down = NULL
      panel2down = NULL
    }


    tabPanel(tab_title_i,
             column(col_size,
                    panel1up,
                    panel1down),
             column(col_size,
                    panel2up,
                    panel2down))
  }

  ## Compile all tabs for each visit
  app_title = all_visits[all_visits$visit_id == visit_id,"visit_title"]

  all_tabs_sub = all_tabs[unlist(all_visits[all_visits$visit_id == visit_id,all_tabs$tab_id]),]

  if(length(widget_data_i_levels) > 1){
    fun_to_eval = paste("navbarPage(app_title,", paste(sapply(1:nrow(all_tabs_sub), function(i) paste("make_navbar_tabs(", i, ",panel_list, all_tabs_sub)", sep = "")), collapse = ","), ")",sep="")
    visit_ui_i = eval(parse(text=fun_to_eval))
  }else{
    visit_ui_i = navbarPage(app_title,
                            make_navbar_tabs(1,panel_list, all_tabs_sub, visit_id))
  }


  visit_ui_i
}
