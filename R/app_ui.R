#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(
      dashboardHeader( title ="Wheat Portal"),
      dashboardSidebar(
        sidebarMenu(
           menuItem("Home", tabName="home", icon=icon("home"))
          # menuItem("Contact Us",  tabName="contact_info", icon=icon("th")),
          # menuItem("MAPs",  tabName="maps", icon=icon("th")),
          # menuItem("Gene families",  tabName="gene_family", icon=icon("chart-bar")),
          # menuItem("Gene Search",  tabName="gene_search", icon=icon("th")),
          # menuItem("Markers",  tabName="markers", icon=icon("th")),
          # menuItem("JBrowse",  tabName="Jbrowse", icon=icon("th")),
          # menuItem("Publications",  tabName="publications", icon=icon("file-alt")),
          # menuItem("Downloads",  tabName="downloads", icon=icon("th")),
          # menuItem("Manuals",  tabName="manual", icon=icon("file-alt"))
        )
      ),
      dashboardBody(
        tags$head(
          tags$link(rel="stylesheet", text="text/css", href="custom.css"),
          ),

        tabItems(
          tabItem(tabName = "home",


                  fluidRow(
                    includeHTML("inst/app/www/index.html")

                  )# end fluidrow

          )# end of Home
        )
      )
      #               htmlOutput("sidebar_output")
      #               #htmlTemplate("inst/app/www/html.html")
      #
      #             #   fluidRow(
      #             #     # First column taking up 25% of the width (3 out of 12)
      #             #     column(3,
      #             #            box(title = "Box 1", solidHeader = TRUE, status = "primary", "Content for Box 1")
      #             #
      #             #
      #             #
      #             #  tabItem(tabName = "docs",
      #             #                    fluidRow(
      #             #                      box(width = 12, title = "Documentation", solidHeader = TRUE, status = "success",
      #             #                          h2("User Guide & Documentation"),
      #             #                          p("Find detailed documentation on how to use the Genome Browser and its various tools.")
      #             #                      )
      #             #                    )
      #             #            ),
      #             #
      #             #
      #             #   includeHTML("www/html.html"))
      #             #
      #             # )
      #



    )
  )


}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "wheatdb"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
