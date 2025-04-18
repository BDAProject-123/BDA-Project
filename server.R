library(shiny)
library(dplyr)
library(readr)
library(stringr)

shinyServer(function(input, output, session) {
  
  # Load dataset
  walmart_data <- read_csv("walmart_dataset.csv")
  
  # Clean and prepare data
  walmart_data <- walmart_data %>%
    mutate(
      product_category = str_trim(`Product Category`),
      product_rating = `Product Rating`,
      product_name = `Product Name`,
      product_price = `Product Price`,
      product_brand = `Product Brand`,
      product_reviews = `Product Reviews Count`,
      image_url = str_split(`Product Image Url`, "\\|") %>% sapply(function(x) str_trim(x[1]))
    ) %>%
    filter(
      !is.na(product_category), 
      !is.na(product_rating), 
      !is.na(product_name), 
      !is.na(image_url)
    )
  
  # Extract unique keywords from category
  category_keywords <- unique(unlist(strsplit(walmart_data$product_category, split = " > ")))
  category_keywords <- sort(unique(trimws(category_keywords)))
  
  # Populate dropdown
  observe({
    updateSelectInput(session, "category", choices = category_keywords)
  })
  
  # Inject the modal HTML and JS into the UI
  insertUI(selector = "body", where = "beforeEnd", ui = tags$div(
    # Modal HTML
    HTML('
      <div class="modal fade" id="imageModal" tabindex="-1" role="dialog">
        <div class="modal-dialog modal-lg" role="document">
          <div class="modal-content">
            <div class="modal-header">
              <h5 class="modal-title">Zoomed Image</h5>
              <button type="button" class="close" data-bs-dismiss="modal" aria-label="Close">
                <span aria-hidden="true">&times;</span>
              </button>
            </div>
            <div class="modal-body text-center">
              <img id="zoomedImage" src="" style="max-width: 100%; height: auto;" />
            </div>
          </div>
        </div>
      </div>
    '),
    # Modal JS
    tags$script(HTML("
      $(document).on('click', '.zoomable-image', function() {
        var src = $(this).data('full-src');
        $('#zoomedImage').attr('src', src);
        $('#imageModal').modal('show');
      });
    "))
  ))
  
  # Render top-rated products
  output$product_cards <- renderUI({
    req(input$category)
    
    filtered <- walmart_data %>%
      filter(str_detect(product_category, fixed(input$category, ignore_case = TRUE))) %>%
      arrange(desc(product_rating)) %>%
      head(5)
    
    if (nrow(filtered) == 0) {
      return(tags$p("No products found for this category."))
    }
    
    # Show product cards
    lapply(1:nrow(filtered), function(i) {
      prod <- filtered[i, ]
      
      img_src <- ifelse(is.na(prod$image_url) || prod$image_url == "", 
                        "https://via.placeholder.com/100x100?text=No+Image", 
                        prod$image_url)
      
      tags$div(style = "margin-bottom: 25px; padding: 10px; border: 1px solid #ddd; border-radius: 8px;",
               tags$img(src = img_src, 
                        `data-full-src` = img_src,
                        class = "zoomable-image",
                        height = "120px", 
                        style = "display: block; margin-bottom: 10px; cursor: zoom-in;",
                        onerror = "this.onerror=null;this.src='https://via.placeholder.com/100x100?text=No+Image';"),
               tags$h4(prod$product_name),
               tags$p(HTML(paste0(
                 "<strong>Brand:</strong> ", ifelse(is.na(prod$product_brand), "N/A", prod$product_brand), "<br/>",
                 "<strong>Price:</strong> $", ifelse(is.na(prod$product_price), "N/A", prod$product_price), "<br/>",
                 "<strong>Rating:</strong> ", prod$product_rating, " ⭐<br/>",
                 "<strong>Reviews:</strong> ", ifelse(is.na(prod$product_reviews), "0", prod$product_reviews)
               )))
      )
    })
  })
})
