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
        bootswatch = "flatly",
        primary = "#208381",
        base_font = bslib::font_google("Open Sans")
      ),

      golem_add_external_resources(),
      useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),
        tags$script(HTML("
                    document.addEventListener('DOMContentLoaded', function () {
                      document.querySelectorAll('.navbar-center a').forEach(function (link) {
                        link.addEventListener('click', function (e) {
                          e.preventDefault();
                          const pageId = this.textContent.toLowerCase().replace(/\\s+/g, '-');

                          // Set input$page for Shiny
                          Shiny.setInputValue('page', pageId, { priority: 'event' });

                          // Remove 'active' class from all links
                          document.querySelectorAll('.navbar-center a').forEach(function (l) {
                            l.classList.remove('active');
                          });

                          // Add 'active' class to the clicked link
                          this.classList.add('active');
                        });
                      });
                    });
                    "))

      ),
      # ----------- Custom Navbar -----------
      div(class = "custom-navbar",
          div(class = "navbar-base"), # add this so that the content page is block behind custom-navbar
          # Background image div (with fade)
          div(class = "navbar-bg"),

          div(class = "navbar-left",
              tags$img(src = "www/new_download.png", height = "50px"),
              span("WheatDB", class = "app-title")
          ),
          div(class = "navbar-center",
              div(class = "navbar-links",
              tags$a(href = "#", onclick = "Shiny.setInputValue('page', 'home')", "Home"),
              tags$a(href = "#", onclick = "Shiny.setInputValue('page', 'accessions')", "Accessions"),
              tags$a(href = "#", onclick = "Shiny.setInputValue('page', 'variants')", "Variants"),
              tags$a(href = "#", onclick = "Shiny.setInputValue('page', 'morphology')", "Morphology"),
              tags$a(href = "#", onclick = "Shiny.setInputValue('page', 'genome-tracks')", "Genome-Tracks"),
              tags$a(href = "#", onclick = "Shiny.setInputValue('page', 'blast')", "BLAST"),
              tags$a(href = "#", onclick = "Shiny.setInputValue('page', 'publications')", "Publications")
              )
          ),
          div(class = "navbar-right",
              # empty just in case we need to add logo
              # tags$img(src = "www/images/wheat_8.jpg", class = "fade-wheat")
          )
      ),

      # Content Area
      div(class = "page-content",
          # Define All Pages, But Hide by Default
          div(
            id = "home", class = "page-home", style = "display: block;",
            # home------------------
            fluidRow(
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
                            <img src="www/images/front_seed_spike.jpg" class="d-block w-100" style="height: 400px;">
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
              # Descriptions
              column(12,
                     # div(
                     #   class = "descriptions",
                     HTML('
                        <div class="descriptions">
                          <div class = "front-two">
                            <div class="about-text">
                              <h1>About</h1>
                              <p>Wheat is a staple crop grown on over 30.5 million hectares across India. However, the majority of wheat-growing regions face multiple biotic (pests and diseases) and abiotic (climate and soil) stresses that severely impact grain quality and crop yields. Addressing these challenges is critical to ensuring national food security.</p>

                              <p>While India holds vast genetic diversity in wheat, especially within traditional landraces, this diversity remains largely underutilized in modern breeding programs. There is strong consensus among breeders, gene banks, and geneticists on the urgent need for systematic evaluation and integration of this diversity into breeding pipelines. Characterization and documentation of valuable germplasm are prerequisites for breeding and genomic studies.</p>

                              <p>This database provides a variant map of Indian wheat germplasm, developed as part of the national network project -
                                <span class="highlight">"Germplasm Characterization and Trait Discovery in Wheat using Genomic Approaches and its Integration for Improving Climate Resilience, Productivity and Nutritional Quality"</span>,
                                supported by the
                                <span class="funding">Department of Biotechnology, Ministry of Science and Technology, Government of India</span>.
                              </p>

                              <p>The goal of this project is to accelerate the use of valuable germplasm and genomic data in wheat breeding programs to minimize yield losses across India.</p>
                            </div> <!-- about-test -->

                            <div class="about-study">
                            <h2 class="study-title">Study</h2>
                              <h2 class="study-title">Data Overview</h2>

                              <div class="first-group">
                                <div class="acc-no">
                                  <h2>126</h2>
                                </div>
                                <div class="group-name">
                                  <a href="#" onclick="Shiny.setInputValue(\'page\', \'accessions\')"
                                  style="color: white; text-decoration: underline; cursor: pointer;">Indian wheat landrace (ILR)</a>
                                </div>
                              </div>

                              <div class="first-group">
                                <div class="acc-no">
                                  <h2>27</h2>
                                </div>
                                <div class="group-name">
                                  <a href="#" onclick="Shiny.setInputValue(\'page\', \'accessions\')"
                                  style="color: white; text-decoration: underline; cursor: pointer;">Pre-Green-Revolution variety (IPR)</a>
                                </div>
                              </div>

                              <div class="first-group">
                                <div class="acc-no">
                                  <h2>42</h2>
                                </div>
                                <div class="group-name">
                                  <a href="#" onclick="Shiny.setInputValue(\'page\', \'accessions\')"
                                  style="color: white; text-decoration: underline; cursor: pointer;">Post-Green-Revolution variety (IPoR)</a>
                                </div>
                              </div>

                              <div class="first-group">
                                <div class="acc-no">
                                  <h2>26</h2>
                                </div>
                                <div class="group-name">
                                  <a href="#" onclick="Shiny.setInputValue(\'page\', \'accessions\')"
                                  style="color: white; text-decoration: underline; cursor: pointer;">Indian dwarf wheat (<i>T. sphaerococcum</i>)</a>
                                </div>
                              </div>

                              <div class="first-group">
                                <div class="acc-no">
                                  <h2>3</h2>
                                </div>
                                <div class="group-name">
                                  <a href="#" onclick="Shiny.setInputValue(\'page\', \'accessions\')"
                                  style="color: white; text-decoration: underline; cursor: pointer;">Indian <i>T. durum</i></a>
                                </div>
                              </div>

                              <div class="first-group">
                                <div class="acc-no">
                                  <h2>3</h2>
                                </div>
                                <div class="group-name">
                                  <a href="#" onclick="Shiny.setInputValue(\'page\', \'accessions\')"
                                  style="color: white; text-decoration: underline; cursor: pointer;">Wild variety</a>
                                </div>
                              </div>
                            </div>


                          </div> <!-- front-two -->

                          <div class="front-genetic">
                          <h2>Genectic landscape of the study</h2>
                          <div class="front-image-wrapper">
                            <img src="www/images/map2.png" alt="Genetic landscape map" class="front-map">
                          </div>
                          <div class="front-image-wrapper">
                            <img src="www/images/genetic_landscape.jpg" alt="Genetic landscape" class="front-image">
                          </div>
                          </div>
                          <h2 class="section-title">Useful Links</h2>

                          <div class="link-container">

                            <div class="website">
                              <a href="https://blast.ncbi.nlm.nih.gov/Blast.cgi" target="_blank">
                                <h4>NCBI BLAST</h4>
                              </a>
                              <p>NCBI\'s Basic Local Alignment Search Tool for comparing nucleotide or protein sequences.</p>
                            </div>

                            <div class="website">
                            <a href="https://www.wheatgenome.org/" target="_blank">
                            <h4>International Wheat Genome Sequencing Consortium (IWGSC)</h4>
                            </a>
                            <p>An international collaborative effort established in 2005 to advance wheat genome research.</p>
                            </div>

                            <div class="website">
                            <a href="https://plants.ensembl.org/index.html" target="_blank">
                            <h4>Ensembl Plants</h4>
                            </a>
                            <p>A genome-centric portal providing access to plant species of scientific interest.</p>
                            </div>

                            </div>

                            </div>
                            <br><br>
                            '),
 # end of front page descriptions and link
              ) # end of column
            )
            # end home------------------
          ),
          div(
            id = "accessions", class = "page-wrapper", style = "display: none;",
            mod_accessions_ui("accessions")
          ),
          div(
            id = "variants", class = "page-wrapper", style = "display: none;",
            snp_table_ui("table")
          ),
          div(
            id = "morphology", class = "page-wrapper", style = "display: none;",
            genetic_resources_ui("resource")
          ),

          div(
            id = "genome-tracks", class = "page-wrapper", style = "display: none;",
            tags$iframe(
              src = "https://223.31.159.7/jb_wheatdb/?config=config.json&assembly=wheat&loc=Chr1A:39670..41695&tracks=wheat-ReferenceSequenceTrack,gene-annotations,All",
              height = "800px",
              width = "100%",
              style = "border: none;"
            )
          ),


          div(
            id = "blast", class = "page-wrapper", style = "display: none;",
            blast_ui("blast")
          ),
          div(
            id = "publications", class = "page-wrapper", style = "display: none;",
            h3("Publications content coming soon...")
          ),
          # --------- Footer ---------
          div(class = "footer-wrapper",
              HTML('
          <div class="footermsg">
            <h3>Copyright All Rights Reserved 2025, NIPGR</h3>
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
          ) # end of footer
      )
    )

}
