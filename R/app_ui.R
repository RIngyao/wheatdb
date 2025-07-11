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
  )
}

app_ui <- function(request) {
  fluidPage(
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",  # Use any other like "lux", "minty", "yeti", etc.
      primary = "#208381",
      base_font = bslib::font_google("Open Sans")
    ),

    golem_add_external_resources(),

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    div(id = "app-main",  # Flex container
        navbarPage(
          title = div(
            tags$img(src = "www/new_download.png", height = "50px", style = "margin-right: 0px; margin-bottom: 0;"),
            span("WheatDB", style = "font-weight: bold; font-size: 30px; color: orange; margin: 0; padding-right: 0; padding-bottom: 0;")
          ),
          windowTitle = "WheatDB",
          id = "main_nav",
          collapsible = TRUE,
          # inverse = TRUE,  # Dark navbar
          fluid = TRUE,

          # --------- Home Page ---------
          tabPanel(
            "Home",
            # div(class = "page-wrapper",

              fluidRow(
                # carousel-------------------------
                HTML('
                      <div id="carouselExampleIndicators" class="carousel slide" data-bs-ride="carousel">
                        <div class="carousel-indicators">
                          <button type="button" data-bs-target="#carouselExampleIndicators" data-bs-slide-to="0" class="active"></button>
                          <button type="button" data-bs-target="#carouselExampleIndicators" data-bs-slide-to="1"></button>
                        </div>
                        <div class="carousel-inner">
                          <div class="carousel-item active">
                          <img src="www/images/slide_1.jpg" class="d-block w-100" style="height: 400px;">
                          </div>
                          <div class="carousel-item">
                            <img src="www/images/slide_2.jpg" class="d-block w-100" style="height: 400px;">
                          </div>
                        </div>
                        <button class="carousel-control-prev" type="button" data-bs-target="#carouselExampleIndicators" data-bs-slide="prev">
                          <span class="carousel-control-prev-icon"></span>
                          <span class="visually-hidden">Previous</span>
                        </button>
                        <button class="carousel-control-next" type="button" data-bs-target="#carouselExampleIndicators" data-bs-slide="next">
                          <span class="carousel-control-next-icon"></span>
                          <span class="visually-hidden">Next</span>
                        </button>
                    </div>
                  '),
                # Descriptions---------------------
                column(12,
                       # div(
                       #   class = "descriptions",
                         HTML('
                            <div class="descriptions">
                              <p class="about-text">
                                Wheat, a climate sensitive crop, is grown on 30.5 million hectares in India and the majority of wheat growing area faces several biotic and abiotic stresses resulting in poor quality grains and reduced yield. Exploration of untapped genetic diversity leading to resistant and better performing cultivars is needed to ensure food security. Breeding wheat varieties with high yield under stress environments has been challenging due to high genotype x environment interaction, lack of truly resistant germplasm, and absence of reliable markers linked with key QTLs. Despite emphasis on exploiting diversity, only few successes exist deploying alleles from traditional landraces in elite breeding programs. There is consensus among gene banks, geneticists, and breeders on the urgent need for systematic evaluation to realize the genomic and resistance potential of large seed collections. Characterization and documentation of valuable germplasm are prerequisites for breeding and genomic studies. The goal of this project is to accelerate germplasm and genomic information usage in wheat breeding programs to minimize yield losses across India. This network project <span style="font-style: italic; font-weight: bold;";> “Germplasm Characterization and Trait Discovery in Wheat using Genomic Approaches and its Integration for Improving Climate Resilience, Productivity and Nutritional Quality” </span> is funded by the  <span style="font-weight: bold;";>Department of Biotechnology, Ministry of Science and Technology, Government of India </span>.
                              </p>

                              <h2 class="section-title">Lab\'s Related Websites</h2>

                              <div class="link-descriptions">

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
                                  <a href="http://223.31.159.7/rsb" target="_blank">
                                    <h4>Rice Sheath Blight Database</h4>
                                  </a>
                                  <p>Database on rice sheath blight disease and ongoing research in rice.</p>
                                </div>

                            </div>

                          </div>
                                 <br>
                                 <br>
                        ') # end of front page descriptions and link
                ) # end of column
              ) # end of fluidRow
            # ) # div for centering
            # ) # end page wrapper div
          ),
          # other panel-----------------------
          # div(class = "centered-content", # div for centering
          # --------- Accesion info ---------
          tabPanel("Accessions",
                   div(class = "page-wrapper",
                       value = "Accessions detail")
          ),
          # --------- variants ---------
          tabPanel("Variants",
                   div(class = "page-wrapper",
                       snp_table_ui("table")
                       )
                   ),
          # --------- Morphology ---------
          tabPanel("Morphology",
                   div(class = "page-wrapper",
                       genetic_resources_ui("resource")
                       )
                   ),
          # --------- Genome tracks ---------
          tabPanel("Genome-Tracks",
                   div(class = "page-wrapper",
                     tags$iframe(
                       src = "https://223.31.159.7/jb_wheatdb/?config=config.json&assembly=wheat&loc=Chr1A:39670..41695&tracks=wheat-ReferenceSequenceTrack,gene-annotations,variants",
                       height = "900px",
                       width = "100%",
                       style = "border: none;"
                     )
                   )
          ),
          # --------- Blast ---------
          tabPanel("BLAST", div(class = "page-wrapper",
                                blast_ui("blast")
                                )
          ),
          # --------- Publication ---------
          tabPanel("Publications", div(class = "page-wrapper",
                                       value = "publication page"
                                       )
          )
        ) # end of nav bar
    ),

    # --------- Footer ---------
    div(class = "footer-wrapper",
        HTML('
          <div class="footermsg">
            <h3>Copyright All Rights Reserved 2025</h3>
            <div class="logo-pair">
              <div class="logo-box1">
                <img src="www/images/f-l1.jpg" alt="logo1">
                <img src="www/images/f-l2.png" alt="logo2">
                <img src="www/images/f-l3.jpg" alt="logo3">
                <img src="www/images/f-l4.jpg" alt="logo4">
                <img src="www/images/f-l5.png" alt="logo5">
                <img src="www/images/f-l6.png" alt="logo6">
              </div>
              <div class="logo-box2">
                <img src="www/images/f-l7.jpg" alt="logo7">
                <img src="www/images/f-l8.jpg" alt="logo8">
                <img src="www/images/f-l9.jpg" alt="logo9">
                <img src="www/images/f-l10.jpg" alt="logo10">
                <img src="www/images/f-l11.png" alt="logo11">
              </div>
            </div>
          </div>
        ')
    )
  )
}
