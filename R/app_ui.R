

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

       <br>
       <br>

       <div class="header-font">
       <h2>Wheat Genomic Portal</h2>
       </div>


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
                      <a href=" http://223.31.159.7/chickpea" target="_blank>
                       <link rel="icon" href="favicon.ico" sizes="16x16" type="image/x-icon">
                       <h4>Chickpea genome database</h4>
                      </a>
                      <p>The databases hold passport data and, to varying degrees, characterization and primary evaluation data of the major collections of the respective crops in Europe.</p>
                   </div>

                   <div class="link">
                     <a href="http://223.31.159.7/cicer" target="_blank>
                      <link rel="icon" href="favicon.ico" sizes="16x16" type="image/x-icon">
                     <h4>Chickpea methylation database</h4>
                     </a>
                     <p>The National centre for Biotechnology Information advances science and health by providing access to biomedical and genomic information</p>
                   </div>


                   <div class = "another"
                     <a href = "http://223.31.159.7/ctdb/" target = "_blank>
                      <link rel="icon" href="favicon.ico" sizes="16x16" type="image/x-icon">
                     <h4>Chickpea transcriptome Database</h4>
                     </a>
                     <p>A government-funded initiative to sequence the genomes of 10,000 healthy Indians. The project\'s goal is to create a comprehensive catalog of genetic variations in the Indian population. </p>
                   </div>


                   <div class = "ricebean"
                     <a href = "http://ricebeanportal.com/" target = "_blank>
                      <link rel="icon" href="favicon.ico" sizes="16x16" type="image/x-icon">
                     <h4>Ricebean Portal</h4>
                     </a>
                     <p>A government-funded initiative to sequence the genomes of 10,000 healthy Indians. The project\'s goal is to create a comprehensive catalog of genetic variations in the Indian population. </p>
                   </div>


                   <div class = "riceblight"
                     <a href = " http://223.31.159.7/RSB/public/" target = "_blank>
                      <link rel="icon" href="favicon.ico" sizes="16x16" type="image/x-icon">
                     <h4>Rice Sheath Blight Database</h4>
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
      skin = "purple",
      dashboardHeader(
        tags$li(class = "dropdown",
                tags$style(".main-header {max-height: 30px}"),
                tags$style(".main-header .logo {height: 60px;}")
        ),


       # class = "header",
        title = tags$img(src="www/new_download.png", width = '30%')),
#       (
#        tags$img(src="www/new_download.jpg", width = '%'),
#
#       ),
#      dropdownMenuOutput(disable = TRUE),
#
# ),


      # bs_theme(
      #   bg = "#FFFFFF",
      #   fg = "#FFF",
      # ),
      dashboardSidebar(
        width = 150,
        br(),
        shinydashboard::sidebarMenu(
          menuItem("Home", tabName="home", icon=icon("home")),
          menuItem("Markers", tabName="markers", icon = icon("table")),
          menuItem("BLAST", tabName="BLAST", icon = icon("list"))
        )

        #
        #       div(
        #         style = "font-size: 20px;
        #                 font-family: Lucida Console;
        #                 text-align: center;
        #                position: fixed;
        #                margin-top: 40px;
        #
        #         ",
        #            menuItem("Home", tabName="home", icon=icon("home")),
        #       ),
        #          div(
        #            style = "font-size: 20px;
        #            font-family: Lucida Console;
        #            text-align: center;
        #            position: fixed;
        #            margin-top: 80px;
        #            ",
        #            menuItem("Markers", tabName="markers", icon = icon("table")),
        #          ),
        #
        #          div(
        #            style = "font-size: 20px;
        #            font-family: Lucida Console;
        #            text-align: center;
        #            position: fixed;
        #            margin-top: 120px;
        #            ",
        #            menuItem("BLAST", tabName="BLAST", icon = icon("list"))
        #          )
        #


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

                    column(12, frontPage),

                  )# end fluidrow
          ),# end of Home

          tabItem(tabName = "markers",
                  fluidRow(

                    column(12, snp_table_ui("table")),

                  ) #end fliudRow
          ),#end markers

          tabItem(tabName = "BLAST",
                  fluidRow(

                    column(12, blast_ui("blast")),

                  ) #end fliudRow
          ) #end blast


        ) #end tabItems
      ),#end dashboardbody
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
