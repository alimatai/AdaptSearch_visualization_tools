library(shiny)
library(shinythemes)
library(plotly)
library(FactoMineR)
#library(DT)

ui <- navbarPage(theme=shinytheme('sandstone'), 'FCA analysis',
                 
                 tabPanel('Main',
                          sidebarLayout(
                              sidebarPanel(
                                  fileInput("input_file", "choose a data file", multiple=FALSE, accept=NULL, width="300px"),
                                  actionButton("start", "Start"),
                                  div(style="display:inline-block; width:150px", selectInput(inputId="what", label="Choose what to plot", c("Counts" = "counts", "Frequencies"="freqs"))),
                                  numericInput(inputId="axisX", label="Axis X", value=1, min=1, max=NA, step=1, width="75px"),
                                  div(style="display:inline-block; width:75px", numericInput(inputId="axisY", label="Axis Y", value=2, min=1, max=NA, step=1)),
                                  selectInput(inputId="what_row", label="rows color range", c("cos2" = "cos2", "Contribution" = "contrib"), width="150px"),
                                  selectInput(inputId="what_col", label="columns color range", c("cos2" = "cos2", "Contribution" = "contrib"), width="150px"),
                                  numericInput(inputId="row_colr", label="Axis for rows color range", value=1, min=1, max=NA, step=1, width="210"),
                                  numericInput(inputId="col_colr", label="Axis for columns color range", value=1, min=1, max=NA, step=1, width="210")
                              ),
                              mainPanel(
                                  div(style="display:inline-block; width:150px", checkboxInput("show_labels", label="Show labels", value=TRUE)),
                                  div(style="display:inline-block; width:200px", checkboxInput("hide_data", label="Hide unrepresentative data", value=FALSE)),
                                  conditionalPanel(condition ="input.hide_data",
                                                   selectInput(inputId="hide_what", label="Reference", c("cos2" = "cos2", "Contribution" = "contrib")),
                                                   sliderInput("hide_range", "Hide values under (%of the max value)...", step=5, min=0, max=100, value=0)
                                  ),
                                  h4('CA factor map'),
                                  plotlyOutput('CA_factor_map'),
                                  h4('Explained variance per dimension'),
                                  plotlyOutput('CA_eigen')
                              )
                          )
                 ),
                 
                 tabPanel('Rows data',
                          mainPanel(
                              h4('Cos2'),
                              dataTableOutput('rowcos2'),
                              h4('Contribution'),
                              dataTableOutput('rowcontrib')
                          )
                 ),
                 
                 tabPanel('Columns data',
                          mainPanel(
                              h4('Cos2'),
                              dataTableOutput('colcos2'),
                              h4('Contribution'),
                              dataTableOutput('colcontrib')
                          )
                 )
)

server <- function (input, output) {
    
    # get data and compute PCA
    data <- reactive({
        if (is.null(input$input_file)) {return(NULL)}
        data <-read.table(input$input_file$datapath, header=TRUE, dec=".", sep=",", row.names=1)
        
        if (input$input_file$name != "gc_and_others.csv") {
            if (grepl('transitions', input$input_file$name)) {
                if (input$what == "counts") {selected_data <- data[seq(1, nrow(data), 4),]} 
                else if (input$what == "freqs") {selected_data <- data[seq(2, nrow(data), 4),]}
                row.names(selected_data) <- substrLeft(row.names(selected_data),5) # faire plutôt un strsplit ?
            } else {
                if (input$what == "counts") {selected_data <- data[seq(1, nrow(data), 3),]} 
                else if (input$what == "freqs") {selected_data <- data[seq(2, nrow(data), 3),]}
                row.names(selected_data) <- substrLeft(row.names(selected_data),2)
            }
            
        } else {
            selected_data <- data
        }
        
        res.ca <- CA(selected_data, graph=F, axes=c(1,2))
        # indx <- (apply(res.pca$cos2, 2, function(x) any(is.na(x))))
        return(res.ca)
    })
    
    observe({
        if(input$start != 0) {
            
            # Plotting both individuals and variables
            output$CA_factor_map <- renderPlotly({
                
                row_coord <- scale(as.data.frame(data()$row$coord), center=TRUE, scale=TRUE)
                if (input$what_row == "cos2") {colors <- as.data.frame(data()$row$cos2)}
                else if (input$what_row == "contrib") {colors <- as.data.frame(data()$row$contrib)}
                
                col_coord <- scale(as.data.frame(data()$col$coord), center=TRUE, scale=TRUE)
                
                if (input$what_col == "cos2") {colors2 <- as.data.frame(data()$col$cos2)}
                else if (input$what_col == "contrib") {colors2 <- as.data.frame(data()$col$contrib)}
                
                if (input$show_labels) {mod="markers+text"}
                else {mod="markers"}
                
                biplot <- plot_ly(
                    x=row_coord[,input$axisX],
                    y=row_coord[,input$axisY],
                    type = 'scatter',
                    text=rownames(row_coord),
                    textposition='top',
                    mode=mod,
                    #color=colors[,input$ind_colr],
                    #colosr="OrRd
                    marker=list(color="rgb(225, 50, 50)", symbol=27, size=11)) %>%
                    
                    add_trace(x=col_coord[,input$axisX],
                              y=col_coord[,input$axisY],
                              type = 'scatter',
                              text=rownames(col_coord),
                              textposition='top',
                              mode=mod,
                              #color=colors2[,input$var_colr],
                              #colors="BuGn",
                              marker=list(color="rgb(86,135,212)", symbol=4, size=11))
                
                biplot <- layout(biplot, title="CA factor map",
                                 showlegend=FALSE,
                                 xaxis = list(title=input$axisX),
                                 yaxis = list(title=input$axisY))
                
                # p6 <- plot_ly(showscale = TRUE) %>%
                #     add_surface(z = ~z1, cmin = min(z1), cmax = max(z2), colorscale = list(c(0,1),c("rgb(255,112,184)","rgb(128,0,64)"))) %>%
                #     add_surface(z = ~z2, cmin = min(z1), cmax = max(z2), colorscale = list(c(0,1),c("rgb(107,184,214)","rgb(0,90,124)"))) %>%
                #     layout(title="SURFACE 1 + SURFACE 2\n(Distinct colorscales as defined)", scene = list(zaxis = list(range = c(min(z1),max(z2)))))
                
                biplot
            })
            
            output$CA_eigen <- renderPlotly({
                eig <- as.data.frame(data()$eig)
                eig_names <- factor(rownames(eig), levels = rownames(eig))
                eigen <- plot_ly(eig) %>%
                    add_trace(x=eig_names,
                              y=eig[,2],
                              type='bar',
                              name="Explained variation") %>%
                    add_trace(x=eig_names,
                              y=eig[,3],
                              type='scatter',
                              mode='lines',
                              line=list(color="rgb(225, 50, 50)"),
                              name="Cumulative explained variation") %>%
                    layout(legend = list(orientation = 'h'))
                
                eigen
            })
            
            output$rowcos2 <- renderDataTable(data()$row$cos2)
            output$rowcontrib <- renderDataTable(data()$row$contrib)
            output$colcos2 <- renderDataTable(data()$col$cos2)
            output$colcontrib <- renderDataTable(data()$col$contrib)
        }
    })
}

substrLeft <- function(x, n){
    sapply(x, function(xx)
        substr(xx, 0, n)
    )
}

shinyApp(ui=ui, server=server)
