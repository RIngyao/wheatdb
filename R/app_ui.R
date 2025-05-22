

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


         <div class="container">

         <br>
         <span><p>Wheat, a climate sensitive crop, is grown on 30.5 million hectares in India and the majority of wheat growing area faces several biotic and abiotic stresses resulting in poor quality grains and finally, reduce yield. Exploration of the untapped genetic diversity leading to the development of resistant and better performing cultivars is needed to ensure the food security. Breeding wheat varieties that may give high yield under different stress environment has not made much headway due to high genotype x environment interaction, non-availability of truly resistant germplasm and non-availability of reliable markers linked with the QTLs having significant impact on resistance to biotic stresses. Despite consistent emphasis on exploiting diversity in the crop improvement, there has been only a few success stories to date on the deployment of alleles originating from traditional landraces in elite breeding programs. There is a consensus among the gene banks, plant geneticists and breeders that there is an urgent need for better ways to systematically evaluate and realize the genomic and resistance potential of large seed collections. Characterization and documentation of these valuable germplasm lines are prerequisite for germplasm utilization in the breeding and genomics study. The main goal of this project is to accelerate use of germplasms and their genomic information in the wheat breeding programs in order to address the various challenges of wheat production in different parts of India that in turn would minimize the yield losses. This network project “Germplasm Characterization and Trait Discovery in Wheat using Genomic Approaches and its Integration for Improving Climate Resilience, Productivity and Nutritional quality” has been undertaken with a funding from the Department of Biotechnology, Ministry of Science and Technology, Government of India.</p></span>

      </div>


            <div class= "parent">
             <h2>RELATED WEBSITES</h2>
             </div>


             <div class = "link-container">

                   <div class="website">
                      <a href=" http://223.31.159.7/chickpea" target="_blank>
                       <link rel="icon" href="favicon.ico" sizes="16x16" type="image/x-icon">
                       <h4>Chickpea genome database</h4>
                      </a>
                      <p>This database presents graphical scanning of genes, simple sequence repeats, primer sequences and some of the characterized genetic markers altogether or separately along eight chromosomes of chickpea in a single window.</p>
                   </div>

                   <div class="link">
                     <a href="http://223.31.159.7/cicer" target="_blank>
                      <link rel="icon" href="favicon.ico" sizes="16x16" type="image/x-icon">
                     <h4>Chickpea methylation database</h4>
                     </a>
                     <p>This database is a web based resource focused on DNA methylation variation present in diverse natural accessions of wild [Cicer reticulatum (ICC17160) and cultivated [desi (ICC4958, ICC5590) and kabuli (ICC8155, ICC8261) chickpea.</p>
                   </div>


                   <div class = "another">
                     <a href = "http://223.31.159.7/ctdb" target = "_blank>
                      <link rel="icon" href="favicon.ico" sizes="16x16" type="image/x-icon">
                     <h4>Chickpea transcriptome Database</h4>
                     </a>
                     <p>The website highlights the information of Whole genome assembly and annotation of Chickpea genomes sequenced at NIPGR. </p>
                   </div>


                   <div class = "ricebean">
                     <a href = "http://ricebeanportal.com" target = "_blank>
                      <link rel="icon" href="favicon.ico" sizes="16x16" type="image/x-icon">
                     <h4>Ricebean Portal</h4>
                     </a>
                     <p>This multi-institutional project aims to delineate relevant potential molecular signatures and gene regulatory mechanism governing desirable agro-morphological and nutritional quality component traits at a genome-wide scale that are essential for popularization, genetic enhancement and accelerated domestication of rice bean.</p>
                   </div>


                   <div class = "riceblight">
                     <a href = " http://223.31.159.7/RSB/public" target = "_blank>
                      <link rel="icon" href="favicon.ico" sizes="16x16" type="image/x-icon">
                     <h4>Rice Sheath Blight Database</h4>
                     </a>
                     <p>This website provides information about the disease, including its pathogen (Rhizoctonia solani), potential yield losses, and ongoing research on resistance mechanisms in rice. </p>
                   </div>


             </div>
             <br>
             <br>


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
         title = tags$img(src="www/new_download.png", width = '30%')),
                        # taags$li(class = "text_title",
                        #          tags$p("Wheat Genome Portal")
                        #          )),


      dashboardSidebar(
        width = 150,
        br(),
        shinydashboard::sidebarMenu(
          menuItem("Home", tabName="home", icon=icon("home")),
          menuItem("Markers", tabName="markers", icon = icon("table")),
          menuItem("BLAST", tabName="BLAST", icon = icon("list"))
        )

      ),


      dashboardBody(


        tags$head(
          tags$link(rel="stylesheet", text="text/css", href="custom.css"),
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
                     # )
                      ),
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
             <div class = "logo-pair">
             <div class="logo-box1">
             <img src="www/images/f-l1.jpg" alt="logo1"><img src="www/images/f-l2.png" alt="logo2"><img src="www/images/f-l3.jpg" alt="logo3"><img src="www/images/f-l4.jpg" alt="logo4"><img src="www/images/f-l5.png" alt="logo5"><img src="www/images/f-l6.png" alt="logo6">
             </div>
             <div class="logo-box2">
             <img src="www/images/f-l7.jpg" alt="logo7"><img src="www/images/f-l8.jpg" alt="logo8"><img src="www/images/f-l9.jpg" alt="logo9"><img src="www/images/f-l10.jpg" alt="logo10"><img src="www/images/f-l11.png" alt="logo11">
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


         # Wheat is one of the world\'s three leading cereal crops that include rice and corn, providing about 20% of the calories we consume every day, and is also a leading source of protein.  Wheat is a staple crop and a primary source of food for
         # for billions, playing a crutial role in global food security. Advancing genetic and genomic resources for wheat is of utmost importance for improving yield, stress tolerance and disease resistance. Wheat is mainly grown in temperate region. It thrives well in diverse climates with rain-fed plains to irrigated regions making it
         # staple food across Asia, North America , Russia and Europe.
         # However, the global wheat supply and demand has become critical in recent years due to global environmental changes
         # and population growth, and an increase in wheat production by more than 60% over the next 40 years
         # has been deemed indispensable. In addition, in response to the emergence of new threats such as
         # the outbreak of wheat blast disease, it is urgent to develop wheat varieties that are resistant to
         # environmental changes for sustainable production. In Japan, it is also necessary to increase domestic
         # wheat production and develop varieties with high yield, excellent milling quality, as well as varieties
         # with sufficient resistance to pre-harvest sprouting, Fusarium head blight etc. often caused by Japan\'s wet climate
