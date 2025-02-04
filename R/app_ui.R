library(dplyr)
library(shinydashboard)
library(shiny)
#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'


app_ui <- function(request) {

  frontPage <- div(
    HTML('
      <div id="image" class="main-content">
              <img src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSRLtZNUd3macbfno8gEEcJRSlLlNJq6cFzBA&s" alt="wheat picture">
      </div>

      <div id="introduction" class="container">
        <h2>Welcome to Genome Browser</h2>
          <p>Wheat is one of the world\'s three leading cereal crops that include rice and corn, providing about 20%
         of the calories we consume every day, and is also a leading source of protein. However, the global
         wheat supply and demand has become critical in recent years due to global environmental changes
         and population growth, and an increase in wheat production by more than 60% over the next 40 years
         has been deemed indispensable. In addition, in response to the emergence of new threats such as
         the outbreak of wheat blast disease, it is urgent to develop wheat varieties that are resistant to
         environmental changes for sustainable production. In Japan, it is also necessary to increase domestic
         wheat production and develop varieties with high yield, excellent milling quality, as well as varieties
         with sufficient resistance to pre-harvest sprouting, Fusarium head blight etc. often caused by Japan\'s
wet climate
        </p>
      </div>


             <div class=parent">
             <h2>RELATED WEBSITES</h2>
               <div class="child">
              <a href="https://www.ecpgr.org/resources/germplasm-databases/ecpgr-central-crop-databases" target="_blank>
                     <link rel="icon" href="favicon.ico" sizes="16x16" type="image/x-icon">

                     <img src="https://www.ecpgr.org/fileadmin/templates/ecpgr.org/Assets/images/ECPGRtext_black.png" style="width=50px;height=50px">
                       <h4>ECPGR central crop databases</h4>
                     </a>
                     <p>The databases hold passport data and, to varying degrees, characterization and primary evaluation data of the major collections of the respective crops in Europe.</p>
               </div>
               <div class="link"
               <a href="https://www.ncbi.nlm.nih.gov/" target="_blank>
                   <span></span><link rel="icon" href="favicon.ico" sizes="16x16" type="image/x-icon">

                  <img src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSfIHPYlZzIl63NHd2gfVIPrhwx6f_IYpMODQ&s" style="width=50px;height=50px">
                  <h4>National Center for biological Information</h4>
                  </a>
                  <p>The National centre for Biotechnology Information advances science and health by providing access to biomedical and genomic information</p>
               </div>
               </div>


       ')
  )

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(
      dashboardHeader( title ="Wheat Portal"),
      dashboardSidebar(
        shinydashboard::sidebarMenu(
           menuItem("Home", tabName="home", icon=icon("home")),
           menuItem("SNPs", tabName="snps") # icon=icon("caret-down"))


        )
      ),
      dashboardBody(
        tags$head(
          tags$link(rel="stylesheet", text="text/css", href="custom.css"),
                 ),

        tabItems(
          tabItem(tabName = "home",
                  fluidRow(

                     # h2("Hello"),

                      frontPage
                       #includeHTML("inst/app/www/index.html")

                          )# end fluidrow
                 ),# end of Home

          tabItem(tabName = "snps",

                    snp_table_ui("table")

                    #end fliudRow
                  ) #end snps


        ) #end tabItems
      )#end dashboardbody




    ) #end dashboardPage
  ) #end taglist


} #end app_ui


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
