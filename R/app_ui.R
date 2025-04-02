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
     <img src = "data:image/png;base64,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" class = "logo" >
      <p>Wheat Portal</p>
      <div class="main-content">
      <h2>Wheat Genomic Portal</h2>

      </div>

      <div class = "carousel.animate">

  <img src = "www/pixabaywheat1.jpg">
 <img src = "www/pixabaywheat2.jpg">
 <img src = "www/pixabaywheat3.jpg">
  <img src = "www/pixabaywheat4.jpg">

      </div>
     <br>
         <div class="container">


         <span><p>Wheat is one of the world\'s three leading cereal crops that include rice and corn, providing about 20% of the calories we consume every day, and is also a leading source of protein.  Wheat is a staple crop and a primary source of food for
         for billions, playing a crutial role in global food security. Advancing genetic and genomic resources for wheat is of utmost importance for improving yield, stress tolerance and disease resistance. Wheat is mainly grown in temperate region. It thrives well in diverse climates with rain-fed plains to irrigated regions making it
         staple food across Asia, North America , Russia and Europe.
         However, the global wheat supply and demand has become critical in recent years due to global environmental changes
         and population growth, and an increase in wheat production by more than 60% over the next 40 years
         has been deemed indispensable. In addition, in response to the emergence of new threats such as
         the outbreak of wheat blast disease, it is urgent to develop wheat varieties that are resistant to
         environmental changes for sustainable production. In Japan, it is also necessary to increase domestic
         wheat production and develop varieties with high yield, excellent milling quality, as well as varieties
         with sufficient resistance to pre-harvest sprouting, Fusarium head blight etc. often caused by Japan\'s wet climate
        </p></span>
      </div>


            <div class= "parent">
             <h2>RELATED WEBSITES</h2>
             </div>


             <div id = "link-container">

                   <div class="child">
                   <a href="https://www.ecpgr.org/" target="_blank>
                   <link rel="icon" href="favicon.ico" sizes="16x16" type="image/x-icon">
                   <img src="https://www.ecpgr.org/fileadmin/templates/ecpgr.org/Assets/images/ECPGRtext_black.png" style="width=50px;height=50px">
                       <h4>ECPGR central crop databases</h4>
                     </a>
                     <p>The databases hold passport data and, to varying degrees, characterization and primary evaluation data of the major collections of the respective crops in Europe.</p>
               </div>

               <div class="link">
               <a href="https://www.ncbi.nlm.nih.gov/" target="_blank>
                   <link rel="icon" href="favicon.ico" sizes="16x16" type="image/x-icon">

                  <img src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSfIHPYlZzIl63NHd2gfVIPrhwx6f_IYpMODQ&s" style="width=50px;height=50px">
                  <h4>National Center for biological Information</h4>
                  </a>
                  <p>The National centre for Biotechnology Information advances science and health by providing access to biomedical and genomic information</p>
               </div>
                <div class = "another"
               <a href = "https://genomeindia.in/" target = "_blank>
                <link rel="icon" href="favicon.ico" sizes="16x16" type="image/x-icon">

                <img src="www/genome.jpg" style="width=50px;height=50px">
                <h4>Genome India Project</h4>
               </a>
               <p>A government-funded initiative to sequence the genomes of 10,000 healthy Indians. The project\'s goal is to create a comprehensive catalog of genetic variations in the Indian population. </p>
              </div>


             </div>


       ')
  )


 # bs_theme(
 #    version = 5,
 #    bg = "#FFFFFF",
 #    fg = "#000000",
 #    primary = "#0199F8",
 #    secondary = "#FF374B",
 #    base_font = "Maven Pro"
 #  )

#  bs_theme_preview(theme = custom_theme, with_themer = FALSE)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(
      dashboardHeader( title ="Wheat Portal"),
      # bs_theme(
      #   bg = "#FFFFFF",
      #   fg = "#FFF",
      # ),
      dashboardSidebar(
        width = 150,
        shinydashboard::sidebarMenu(

           menuItem("Home", tabName="home", icon=icon("home")),
           menuItem("Markers", tabName="markers") # icon=icon("caret-down"))


        )
      ),

       # dashboardFooter(
       #   div {
       #   left = "Copyright All Rights Reserved",
       #   right = "2025",
       #
       #               position:absolute;
       #               height:50px;
       #               color: white;
       #               padding: 10px;
       #               background-color: black;
       #   }
       #
       #
       # ),
      dashboardBody(
        tags$head(
          tags$link(rel="stylesheet", text="text/css", href="custom.css"),
                 ),

        tabItems(
          tabItem(tabName = "home",
                  fluidRow(

                     # h2("Hello"),

                      column(12, frontPage),
                       #includeHTML("inst/app/www/index.html")

                          )# end fluidrow
                 ),# end of Home

          tabItem(tabName = "markers",
                  fluidRow(

                    column(12, snp_table_ui("table")
                           ),

                   ) #end fliudRow
                  ) #end markers


        ) #end tabItems
      )#end dashboardbody




    ), #end dashboardPage

    #footer
      footer <- div(
        HTML('
             <div class="footermsg">
             <h3>Copyright All Rights Reserved 2025</h3>
              ')
      )

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
