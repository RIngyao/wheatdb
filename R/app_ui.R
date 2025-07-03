

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'


app_ui <- function(request) {
  # front page html---------------------------------------------------------------
  frontPage <- div(
    HTML('
        <div class="container">

        <p class="about-text">
          Wheat, a climate sensitive crop, is grown on 30.5 million hectares in India and the majority of wheat growing area faces several biotic and abiotic stresses resulting in poor quality grains and reduced yield. Exploration of untapped genetic diversity leading to resistant and better performing cultivars is needed to ensure food security. Breeding wheat varieties with high yield under stress environments has been challenging due to high genotype x environment interaction, lack of truly resistant germplasm, and absence of reliable markers linked with key QTLs. Despite emphasis on exploiting diversity, only few successes exist deploying alleles from traditional landraces in elite breeding programs. There is consensus among gene banks, geneticists, and breeders on the urgent need for systematic evaluation to realize the genomic and resistance potential of large seed collections. Characterization and documentation of valuable germplasm are prerequisites for breeding and genomic studies. The goal of this project is to accelerate germplasm and genomic information usage in wheat breeding programs to minimize yield losses across India. This network project <span style="font-style: italic; font-weight: bold;";> “Germplasm Characterization and Trait Discovery in Wheat using Genomic Approaches and its Integration for Improving Climate Resilience, Productivity and Nutritional Quality” </span> is funded by the  <span style="font-weight: bold;";>Department of Biotechnology, Ministry of Science and Technology, Government of India </span>.
        </p>

        <h2 class="section-title">Lab\'s Related Websites</h2>

        <div class="link-container">

          <div class="website">
            <a href="http://223.31.159.7/chickpea" target="_blank">
              <h4>Chickpea Genome Database</h4>
            </a>
            <p>A comprehensive resource for genes, markers, and primers of chickpea.</p>
          </div>

          <div class="website">
            <a href="http://223.31.159.7/cicer" target="_blank">
              <h4>Chickpea Methylation Database</h4>
            </a>
            <p>Database focused on DNA methylation variation in wild and cultivated chickpea accessions.</p>
          </div>

          <div class="website">
            <a href="http://223.31.159.7/ctdb" target="_blank">
              <h4>Chickpea Transcriptome Database</h4>
            </a>
            <p>Database on whole-genome assembly and annotation of chickpea genomes from NIPGR.</p>
          </div>

          <div class="website">
            <a href="http://ricebeanportal.com" target="_blank">
              <h4>Ricebean Portal</h4>
            </a>
            <p>Multi-institutional project exploring molecular signatures and gene regulation for rice bean improvement.</p>
          </div>

          <div class="website">
            <a href="http://223.31.159.7/RSB/public" target="_blank">
              <h4>Rice Sheath Blight Database</h4>
            </a>
            <p>Database on rice sheath blight disease and ongoing research in rice.</p>
          </div>

        </div>

      </div>
             <br>
             <br>


       ')
  )


# end of front page html --------------------------------------------------

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(

      dashboardHeader(
        tags$li(class = "dropdown",
                tags$style(".main-header {max-height: 30px}"),
                tags$style(".main-header .logo {height: 60px; background-color: #208381 !important;}"), # 373e02
                tags$style(".main-header .navbar {background-color: #208381 !important;}") #189c8c
                ),

          title = tags$div(
          style = "display: flex; align-items: center;",
          tags$img(src = "www/new_download.png", height = "60px"),
          tags$span("  WheatDB", style = "margin-left: 10px; font-weight: bold; font-size: 20px;"),

)
),

dashboardSidebar(
  width = 200,
  br(),

  tags$head(
    tags$style(
      HTML("

                   .main-sidebar {
                      display: flex;
                      flex-direction: column;
                      height: 100vh;
                      /*background-color: white !important;*/
                      overflow: hidden;
                    }

                    /* Sidebar menu stays on top */
                    .main-sidebar .sidebar-menu {
                      flex-shrink: 0;
                      z-index: 2;
                    }

                    /* Image fills the remaining space */
                    .sidebar-bottom-image {
                      position: absolute;
                      top: auto;
                      bottom: 0;
                      left: 0;
                      /*right: 0;*/
                      top: calc(100% - 150vh + 0px); /* Adjust 200px based on menu height */
                      height: calc(150vh - 0px);
                      z-index: -10;
                      overflow: hidden;
                    }

                    .sidebar-bottom-image img {
                      width: 100%;
                      height: 100%;
                      object-fit: cover;
                      mask-image: linear-gradient(to top, rgba(255, 255, 255, 0.75) 0%, transparent 100%);
                      -webkit-mask-image: linear-gradient(to top, rgba(255, 255, 255, 0.75) 0%, transparent 100%);

                      /*mask-image: linear-gradient(to top, white 0%, transparent 100%);
                      -webkit-mask-image: linear-gradient(to top, white 0%, transparent 100%);*/
                    }
                   ")
    ) # end of tags style
  ), # end of tags head

  # background image for side bar
  tags$div(
    class = "sidebar-bottom-image",
    tags$img(src = "www/images/wheat_8.jpg")
  ),

  shinydashboard::sidebarMenu(

    tags$li(class = "dropdown",
            tags$style(".main-sidebar {background-color: #d28d04 !important;}"), #E49B0F
            tags$style("
                             /* Change label color for sidebar menu */
                              .main-sidebar .sidebar-menu li a {
                                color: #1a1a1a !important;
                                font-weight: 600;
                              }

                              /* Active (selected) menu item */
                              .main-sidebar .sidebar-menu li.active a {
                                color: #ffffff !important;
                                background-color: #d28d04 !important;
                              }

                              /* Hover effect */
                              .main-sidebar .sidebar-menu li a:hover {
                                color: #ffffff !important;
                                background-color: #d28d04 !important;
                              }
                             ")

    ),
    menuItem("Home", tabName="home", icon=icon("home")),
    menuItem("Variants", tabName="markers", icon = icon("table")),
    menuItem(HTML("Seed & Spikelet Archive"), tabName="Genetics", icon = icon("list")),
    # if href is used, don't use tabName and newTab
    menuItem(text = tags$span(
      HTML("Genome Tracks"),
      title = "JBrowse2"
    ), tabName="jbwheatdb", icon = icon("dna")),

    menuItem("BLAST", tabName="BLAST", icon = icon("list"))
  )

),


      dashboardBody(


        tags$head(
          tags$link(rel="stylesheet", type="text/css", href="custom.css")
        ),

        tabItems(
          tabItem(tabName = "home",
                  fluidRow(
                      carousel(
                        id = "frontimg",
                        indicators = FALSE,
                        width = "100%",
                       #height = 600px
                        carouselItem(
                         tags$img(src = "www/images/slide_1.jpg", style = "width: 1500px; height:400px;")

                        ),
                        carouselItem(
                          tags$img(src = "www/images/slide_2.jpg", style = "width: 1500px; height:400px;")

                        )

                      ),
                        column(12, frontPage),

                  )# end fluidrow
          ),# end of Home

          tabItem(tabName = "markers",
                  snp_table_ui("table")
          ),#end markers

          # genetic resources
          tabItem(tabName = "Genetics",
                  genetic_resources_ui("resource")
                  ),

          tabItem(tabName = "BLAST",
                  blast_ui("blast")
          ), #end blast

          tabItem(tabName = "jbwheatdb",
                  tags$iframe(
                    src = "https://223.31.159.7/jb_wheatdb/?config=config.json&assembly=wheat&loc=Chr1A:39670..41695&tracks=wheat-ReferenceSequenceTrack,gene-annotations,variants",
                    height = "900px",
                    width = "100%",
                  )
          ) #end jbwheat

        ) #end tabItems
      ),#end dashboardbody
    ), #end dashboardPage

      #footer
      footer <- div(
        HTML('
        <div class="footer-wrapper">
             <div class="footermsg">
             <h3>Copyright All Rights Reserved 2025</h3>
             <div class = "logo-pair">
             <div class="logo-box1">
             <img src="www/images/f-l1.jpg" alt="logo1"><img src="www/images/f-l2.png" alt="logo2"><img src="www/images/f-l3.jpg" alt="logo3"><img src="www/images/f-l4.jpg" alt="logo4"><img src="www/images/f-l5.png" alt="logo5"><img src="www/images/f-l6.png" alt="logo6">
             </div>
             <div class="logo-box2">
             <img src="www/images/f-l7.jpg" alt="logo7"><img src="www/images/f-l8.jpg" alt="logo8"><img src="www/images/f-l9.jpg" alt="logo9"><img src="www/images/f-l10.jpg" alt="logo10"><img src="www/images/f-l11.png" alt="logo11">
             </div>
             </div>
             </div>
          </div>
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


