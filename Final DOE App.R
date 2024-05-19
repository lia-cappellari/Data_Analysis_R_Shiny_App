library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(broom)
library(shinythemes)
library(data.table)
library(ggplot2)
library(car)
library(olsrr)
library(stats)
library(nortest)
library(FrF2)
library(gridExtra)
library(rsm)
library(qcc)


####################### Define UI for app #####################

main_page <- tabPanel(
  title="Step 0",
  titlePanel("Import Coded Data"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Select CSV / Text File to Import", accept = c(".csv",".txt")),
      uiOutput("dependent"),
      uiOutput("independent"),
      uiOutput("blocking"),
      actionButton("create", "Create Design Matrix")),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Coded Design Matrix",
          fluidRow(column(width = 12, tableOutput("matrix"))),
        ),
        tabPanel(
          title="Response Variable Stats",
          fluidRow(column(width = 12), plotOutput("response_hist")),
          column(width = 6, tableOutput("response_var_summary_table")),
          
        )
      )
    )
  ))

page_1 <- tabPanel(
  title = "Step 1",
  titlePanel("Determine Model/Factor Significance"),
  sidebarPanel(
    uiOutput("checkboxes")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "ANOVA",
        tableOutput("anova_table")
      ),
      tabPanel(
        title = "Pareto Chart of Effects", 
        textOutput("pareto_note"),
        plotOutput("pareto")
      )
    )
    
  ))

page_2 <- tabPanel(
  title = "Step 2",
  titlePanel("Determine Model Accuracy"),
  mainPanel(
    tableOutput("R_values"),
    fluidRow(
      column(width = 10, plotOutput("r_squared_pie", height = "250px", width = "250px")),
      column(width = 10, plotOutput("adj_r_squared_pie", height = "250px", width = "250px"))
      
    )
  ))

page_3 <- tabPanel(
  title = "Step 3",
  titlePanel("Formulate Model Equation"),
  mainPanel(
    tableOutput("summary_table"),
    textOutput("model_equation")
  ))

page_4 <- tabPanel(
  title = "Step 4",
  titlePanel("Analyze Residuals"),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "Data",
        tableOutput("data_table")
      ),
      tabPanel(
        title = "Cook's Distance Plot",
        plotOutput("Cooks_plot")
      ),
      tabPanel(
        title = "Standard Residuals Plot",
        plotOutput("residuals_plot")
      )
    )
  )
)

page_5 <- tabPanel(
  title = "Step 5",
  titlePanel("Verify ANOVA Model Assumptions"),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "Normality Plot",
        plotOutput("normality")
      ),
      tabPanel(
        title = "Constant Variance Plot",
        plotOutput("variance")
      ),
      tabPanel(
        title = "Independence Plot",
        plotOutput("independence")
      )
    )
  )
)

page_6 <- tabPanel(
  title = "Step 6", 
  titlePanel("Visualize Data"),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "Main Effect Plots",
        uiOutput("interactions_checkboxes"),
        plotOutput("effect_plot")
      ),
      tabPanel(
        title = "Interaction Plots",
        uiOutput("interactions_checkboxes_2"),
        plotOutput("interaction_plot")
      ),
      tabPanel(
        title = "BoxPlot of Factors",
        uiOutput("factor_checkboxes"),
        plotOutput("factor_plot")
      ),
      tabPanel(
        title = "Contour Plot",
        uiOutput("contour_checkboxes"),
        plotOutput("contour")
      )
    )
  )
)

ui <- navbarPage(
  title = "DOE 7 Step Analysis",
  theme = shinytheme('united'),
  main_page,
  page_1,
  page_2,
  page_3,
  page_4,
  page_5,
  page_6
)

####################### Define server logic ######################
server <- function(input, output) {
  
  # Load data
  data <- reactive({
    req(input$file1)
    
    # Read csv or txt file
    if (grepl("\\.csv$", input$file1$name)) {
      read.csv(input$file1$datapath)
    } else if (grepl("\\.txt$", input$file1$name)) {
      read.table(input$file1$datapath, header = TRUE)
    } else {
      stop("Unsupported file type")
    }
  })
  
  
  # Get variable names for select inputs
  output$dependent <- renderUI({
    req(data())
    selectInput("dep_var", "Select Response Variable", choices = names(data()))
  })
  
  output$independent <- renderUI({
    req(data())
    checkboxGroupInput("ind_var", "Select Independent Variables", 
                       choices = names(data()), 
                       selected = NULL, 
                       inline = FALSE)
  })
  
  output$blocking <- renderUI({
    req(data())
    selectInput("block_var", "Select Blocking Variable (Optional)", choices = c("Not Selected", names(data())), selected = "Not Selected")
  })
  
  ############## Create design matrix################
  matrixData <- eventReactive(input$create, {
    req(input$ind_var, input$dep_var, data())
    if(input$block_var != "Not Selected"){
      data() %>% 
        select(all_of(input$block_var), all_of(input$ind_var), all_of(input$dep_var)) %>% 
        drop_na() %>% 
        mutate(across(all_of(input$block_var), factor)) %>%  # Convert block variable to a factor
        mutate_all(as.factor) %>% 
        as.matrix()
    } else{
      data() %>% 
        select(all_of(input$ind_var), all_of(input$dep_var)) %>% 
        drop_na() %>% 
        mutate_all(as.factor) %>% 
        as.matrix()
    }
  })
  
  
  # Display design matrix
  output$matrix <- renderTable({
    req(matrixData())
    matrixData()
  })
  
  ################## histogram for response variable ################
  
  output$response_hist <- renderPlot({
    req(input$dep_var, data())
    ggplot(data=data(), aes(x=!!sym(input$dep_var))) +
      geom_histogram(bins=15, fill="orange", alpha=0.5)+
      labs(title = bquote(bold("Histogram of Response Variable")), x=input$dep_var, y="Frequency")+
      theme(plot.title = element_text(size = 20))
  })
  
  
  ############# Display summary statistics of response variable#############
  output$response_var_summary_table <- renderTable({
    req(input$dep_var, data())
    tbl <- data() %>% 
      summarize(
        Mean = mean(!!sym(input$dep_var)),
        Median = median(!!sym(input$dep_var)),
        SD = sd(!!sym(input$dep_var)),
        Min = min(!!sym(input$dep_var)),
        Max = max(!!sym(input$dep_var))
      )
    tbl
  })
  
  ############### Create check boxes for selected variables##############
  output$checkboxes <- renderUI({
    req(matrixData())
    if (input$block_var != "Not Selected") {
      choices <- c("Block" = input$block_var, input$ind_var)
    } else {
      choices <- input$ind_var
    }
    
    # Remove the blocking variable from the input variables list
    input_vars <- setdiff(choices, input$block_var)
    
    # Create all possible two- and three-factor interactions
    interactions <- combn(input_vars, 2, simplify = FALSE)
    if (length(input_vars) >= 3) {
      interactions <- c(interactions, combn(input_vars, 3, simplify = FALSE))
    }
    interactions <- lapply(interactions, paste, collapse = "*")
    
    checkboxGroupInput(
      "checkboxes",
      "Select variables for ANOVA table",
      choices = c(choices, interactions),
      selected = c(choices, interactions)
    )
  })
  
  
  ################## Generate and Display ANOVA table##################
  output$anova_table <- renderTable({
    req(input$checkboxes, matrixData())
    selected_cols <- input$checkboxes
    
    # Separate out interactions
    interactions <- grep("\\*", selected_cols, value = TRUE)
    
    if("Block" %in% selected_cols){
      selected_cols <- c(selected_cols[selected_cols != "Block"], "block")
    }
    
    # Create formula with interactions
    if (length(interactions) > 0) {
      formula <- as.formula(paste(input$dep_var, paste(selected_cols, collapse = "+"), sep = "~"))
    } else {
      formula <- as.formula(paste(input$dep_var, paste(selected_cols, collapse = "+"), sep = "~"))
    }
    
    # Run ANOVA
    anova_results <- aov(formula, data = as.data.frame(matrixData()))
    
    # Check if Residuals degrees of freedom is 0
    if (anova_results$df.residual == 0) {
      message <- "Out of degrees of freedom"
      anova_df <- data.frame(Error = message)}
     else {
      # Tidy results
      tidy_results <- tidy(anova_results)
      tidy_results$p.value <- sprintf("%.5f", tidy_results$p.value) # format p-value
      tidy_results$statistic <- sprintf("%.2f", tidy_results$statistic)
      tidy_results$term[tidy_results$term == input$block_var] <- 'Block'
      
      total_df <- data.frame(term = "Total", 
                             df = sum(tidy_results$df), 
                             sumsq = sum(tidy_results$sumsq), 
                             statistic = "",
                             p.value = "")
      tidy_results <- subset(tidy_results, select = -c(meansq))
      
      anova_df <- rbind(tidy_results, total_df)
    }
    
    anova_df
  })
  
###################### Pareto Chart #################################
  output$pareto <- renderPlot({
    # Get the selected independent variables
    independent_vars <- input$ind_var
    interactions <- combn(independent_vars, 2, paste, collapse = ":")
    vars <- c(independent_vars, interactions)
    formula <- as.formula(paste(input$dep_var, paste(vars, collapse = "+"), sep = "~"))
    model.lm <- lm(formula, data=data())
    
    effects <-abs(2*model.lm$coefficients)
    effect.int <-names(effects) %in% c("(Intercept)")
    effects <- effects[!effect.int]
    pareto.chart(effects, 
                 ylab = "Abs(Effects)", 
                 main="Pareto Chart of Effects", 
                 col=rainbow(length(effects)))
  })
  
  output$pareto_note <- renderPrint({
    message = "** Only use the Pareto Chart if you recieve the error message: 'out of degrees of freedom' **"
    cat(message)
  })
  
  ################## calculate R sqr and adj R sqr###################
  calculate_r_squared <- function(formula, data) {
    lm_model <- lm(formula, data = data)
    r_squared <- summary(lm_model)$r.squared * 100
    adjusted_r_squared <- summary(lm_model)$adj.r.squared * 100
    return(data.frame(R_Squared = paste0(round(r_squared, 2),"%"), Adjusted_R_Squared = paste0(round(adjusted_r_squared, 2),"%")))
  }
  
  
  output$R_values <- renderTable({
    req(input$dep_var, input$checkboxes, matrixData())
    selected_cols <- input$checkboxes
    
    # Separate out interactions
    interactions <- grep("\\*", selected_cols, value = TRUE)
    
    if("Block" %in% selected_cols){
      selected_cols <- c(selected_cols[selected_cols != "Block"], "block")
    }
    
    # Create formula with interactions
    if (length(interactions) > 0) {
      formula <- as.formula(paste(input$dep_var, paste(selected_cols, collapse = "+"), sep = "~"))
    } else {
      formula <- as.formula(paste(input$dep_var, paste(selected_cols, collapse = "+"), sep = "~"))
    }
    
    calculate_r_squared(formula, data = as.data.frame(matrixData()))
  })
  
  ####################### display pie charts #########################
  
  output$r_squared_pie <- renderPlot({
    req(input$checkboxes, matrixData())
    selected_cols <- input$checkboxes
    interactions <- grep("\\*", selected_cols, value = TRUE)
    if("Block" %in% selected_cols){
      selected_cols <- c(selected_cols[selected_cols != "Block"], "block")
    }
    if (length(interactions) > 0) {
      formula <- as.formula(paste(input$dep_var, paste(selected_cols, collapse = "+"), sep = "~"))
    } else {
      formula <- as.formula(paste(input$dep_var, paste(selected_cols, collapse = "+"), sep = "~"))
    }
    
    model <- lm(formula, as.data.frame(matrixData()))
    r_squared <- summary(model)$r.squared * 100
    ggplot(data = data.frame(names = c("R-squared", "Remaining"), 
                             values = c(r_squared, 100-r_squared)),
           aes(x = "", y = values, fill = names)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(round(values, 2), "%")), 
                position = position_stack(vjust = 0.5)) +
      theme_void() +
      ggtitle("R-Squared")+
      theme(plot.title = element_text(face = "bold"), legend.position = "bottom") +
      labs(fill = "")
  })
  
  output$adj_r_squared_pie <- renderPlot({
    req(input$checkboxes, matrixData())
    selected_cols <- input$checkboxes
    interactions <- grep("\\*", selected_cols, value = TRUE)
    if("Block" %in% selected_cols){
      selected_cols <- c(selected_cols[selected_cols != "Block"], "block")
    }
    if (length(interactions) > 0) {
      formula <- as.formula(paste(input$dep_var, paste(selected_cols, collapse = "+"), sep = "~"))
    } else {
      formula <- as.formula(paste(input$dep_var, paste(selected_cols, collapse = "+"), sep = "~"))
    }
    
    model <- lm(formula, as.data.frame(matrixData()))
    adj_r_squared <- summary(model)$adj.r.squared * 100
    ggplot(data = data.frame(names = c("Adj R-squared", "Remaining"), 
                             values = c(adj_r_squared, 100-adj_r_squared)),
           aes(x = "", y = values, fill = names)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(round(values, 2), "%")), 
                position = position_stack(vjust = 0.5)) +
      theme_void() +
      ggtitle("Adjusted R Squared")+
      theme(plot.title = element_text(face = "bold"), legend.position = "bottom") +
      labs(fill = "")
  })
  
  
  ####################### Generate and Display Summary Table #################
  output$summary_table <- renderTable({
    req(input$checkboxes, matrixData())
    selected_cols <- input$checkboxes
    formula <- as.formula(paste0(input$dep_var, " ~ ", paste(selected_cols, collapse = "+")))
    summary_out <- summary(lm(formula, data = as.data.frame(matrixData())))
    summary_df <- tidy(summary_out)
    summary_df$p.value <- sprintf("%.10f", summary_df$p.value)
    summary_df[,1:2]
  })
  
  ## Display Model Equation with coefficients
  
  model_equation <- reactive({
    req(input$checkboxes, data())
    selected_cols <- input$checkboxes
    formula <- as.formula(paste0(input$dep_var, " ~ ", paste(selected_cols, collapse = "+")))
    model <- lm(formula, data = data())
    model_eqn <- paste( 
      paste0(input$dep_var, " = ", coefficients(model)[1], " + ", 
             paste(paste0(coefficients(model)[-1], " * ", selected_cols), collapse = " + ")))
    cat(model_eqn)
    
  })
  
  output$model_equation <- renderPrint({
    req(input$checkboxes)
    model_equation()
  })
  
  ########################## data table with residual values #################
  output$data_table <- renderTable({
    req(matrixData(), input$dep_var)
    fit <- lm(as.formula(paste(input$dep_var, paste(input$ind_var, collapse="+"), sep = " ~ ")), data = as.data.frame(matrixData()))
    # Extract residuals and Cook's distance
    std_residuals <- round(abs(rstandard(fit)),3)
    cooks_distance <- round(cooks.distance(fit),3)
    # Combine original data with residuals and Cook's distance
    table = cbind(matrixData(),
                  std_residuals,
                  cooks_distance)
    table
    
  })
  
  ########################### Cook's distance plot #######################
  output$Cooks_plot <- renderPlot({
    req(matrixData(), input$dep_var)
    fit <- lm(as.formula(paste(input$dep_var, paste(input$ind_var, collapse="+"), sep = " ~ ")), data = as.data.frame(matrixData()))
    plot(cooks.distance(fit),
         type="p",main='Cooks Distance',
         xlab='Observation', 
         ylim=c(0,2))
    abline(h=1.0, col="red")
  })
  
  ############################# Standard Residuals Plot ####################
  output$residuals_plot <- renderPlot({
    req(matrixData(), input$dep_var)
    fit <- lm(as.formula(paste(input$dep_var, paste(input$ind_var, collapse="+"), sep = " ~ ")), data = as.data.frame(matrixData()))
    residuals <- round(fit$residuals,3)
    stdres<-round(rstandard(fit),3)
    abs.stdres <-abs(stdres)
    cooksD<-round(cooks.distance(fit),3)
    pred<-round(fit$fitted.values,3)
    plot(abs.stdres,type="p",
         main='Abs(Std. Residuals)',
         ylab="Std. Residuals", 
         ylim=c(0,4))
    abline(h=2.0,col="yellow")
    abline(h=3.0,col="red")
  })
  
  ########################### ANOVA Assumptions Tests ######################
  
  ##normal test
  output$normality <- renderPlot({
    req(matrixData(), input$dep_var)
    fit <- lm(as.formula(paste(input$dep_var, paste(input$ind_var, collapse="+"), sep = " ~ ")), data = as.data.frame(matrixData()))
    residuals <- round(fit$residuals,3)
    stdres<-round(rstandard(fit),3)
    abs.stdres <-abs(stdres)
    qqPlot(stdres,
           main='Normal Probability Plot',
           xlab="Normal Scores",
           ylab="Std. Residuals",
           col="black",
           col.lines="red")
    
    ## Anderson-Darling Normality Test of residuals           
    ADTEST<-ad.test(residuals)
    pvalue <- paste("p =",signif(ADTEST$p.value, digits=3))
    mtext("AD Normality Test", side=1, line=2, adj=0,col = "red")
    mtext(pvalue, side=1, line=3, adj=0)
    
    ## Shapiro-Wilk Normality Test 
    STEST<-shapiro.test(residuals)
    pvalue2 <- paste("p =",signif(STEST$p.value, digits=3))
    mtext("Shapiro-Wilk Test", side=1, line=2, adj=1,col = "red")
    mtext(pvalue2, side=1, line=3, adj=1)
  })
  
  ##variance test
  output$variance <- renderPlot({
    req(matrixData(), input$dep_var)
    fit <- lm(as.formula(paste(input$dep_var, paste(input$ind_var, collapse="+"), sep = " ~ ")), data = as.data.frame(matrixData()))
    residuals <- round(fit$residuals,3)
    stdres<-round(rstandard(fit),3)
    abs.stdres <-abs(stdres)
    
    plot(fit$fitted.values,
         stdres,main='Residuals vs. Predicted ',
         xlab="Predicted Values (Fitted)",
         ylab="Std. Residuals")
    abline(h=0,col="red")
    
    
  })
  
  ## independence test
  output$independence <- renderPlot({
    req(matrixData(), input$dep_var)
    fit <- lm(as.formula(paste(input$dep_var, paste(input$ind_var, collapse="+"), sep = " ~ ")), data = as.data.frame(matrixData()))
    residuals <- round(fit$residuals,3)
    stdres<-round(rstandard(fit),3)
    abs.stdres <-abs(stdres)
    plot(stdres,
         type="o",
         main='Residuals vs. Order ',
         xlab="Order",
         ylab="Std. Residuals" )
    abline(h=0,col="red")
  })

  ###################### Visualizing Data Plots #############################
  
## Effect plot
  output$interactions_checkboxes <- renderUI({
    div(
      tags$h2("Select Factor(s) for Effect Plot"),
      checkboxGroupInput(
        "interactions_checkboxes", 
        "May be Numeric or Categorical",
        choices = setdiff(names(data()), input$dep_var),
        selected = NULL,
        inline = FALSE
      )
    )
  })
  
  output$effect_plot <- renderPlot({
    req(input$interactions_checkboxes, input$dep_var, data())
    selected_cols <- input$interactions_checkboxes
    dep_var <- sym(input$dep_var)
    
    # Calculate y-axis limits based on minimum and maximum mean_val values
    y_min <- Inf
    y_max <- -Inf
    for (col in selected_cols) {
      means <- data() %>%
        group_by(across(all_of(as.character(col)))) %>%
        summarise(mean_val = mean(!!dep_var)) %>%
        ungroup()
      y_min <- min(y_min, min(means$mean_val)-2)
      y_max <- max(y_max, max(means$mean_val)+2)
    }
    
    plot_list <- lapply(selected_cols, function(col) {
      means <- data() %>%
        group_by(across(all_of(as.character(col)))) %>%
        summarise(mean_val = mean(!!dep_var)) %>%
        ungroup()
      
      ggplot(data = means, aes(x = !!sym(col), y = mean_val, group = 1)) +
        geom_line() +
        geom_point() +
        labs(x = as.character(col), y = as.character(dep_var),
             title = paste("Effect Plot of", col)) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              panel.grid.major = element_blank()) +
        ylim(y_min, y_max)
      
    })
    
    gridExtra::grid.arrange(grobs = plot_list, ncol = 2, widths = c(0.5, 0.5))
  })
  
  
## interactions plot 
  output$interactions_checkboxes_2 <- renderUI({
    div(
      tags$h2("Select Factors for Interaction Plot"),
      checkboxGroupInput(
        "interactions_checkboxes_2", 
        "Select 2-3 variables, Must be 2 levels, May be numeric or categorical",
        choices = setdiff(names(data()), input$dep_var),
        selected = NULL,
        inline = FALSE
      )
    )
  })
  
  output$interaction_plot <- renderPlot({
    req(input$interactions_checkboxes_2, input$dep_var, data())
    
    variables <- input$interactions_checkboxes_2
    dep_var <- sym(input$dep_var)
    
    # Convert non-numeric variables to factors
    data_factors <- data()
    for (var in variables) {
      if (!is.numeric(data_factors[[var]])) {
        data_factors[[var]] <- factor(data_factors[[var]])
      }
    }
    
    formula_str <- paste(as.character(dep_var), "~")
    
    for (i in seq_along(variables)) {
      var <- sym(variables[i])
      formula_str <- paste(formula_str, as.character(var))
      if (i < length(variables)) {
        formula_str <- paste(formula_str, "*")
      }
    }
    
    formula_obj <- formula(formula_str)
    
    model <- lm(formula_obj, data = data_factors)
    
    IAPlot(model, main = "Interaction Plot", show.alias = FALSE)
  })
  
##Boxplot of Factors
  output$factor_checkboxes <- renderUI({
    div(
      tags$h2("Select Factor(s) for Box Plot"),
      checkboxGroupInput(
        "factor_checkboxes", 
        "May be Categorical or Numeric",
        choices = setdiff(names(data()), input$dep_var),
        selected = NULL,
        inline = FALSE
      )
    )
  })
  
  output$factor_plot <- renderPlot({
    req(input$factor_checkboxes, input$dep_var, data())
    
    selected_cols <- input$factor_checkboxes
    dep_var <- sym(input$dep_var)
    formula_obj <- as.formula(paste(input$dep_var, paste(selected_cols, collapse = "+"), sep = "~"))
    
    boxplot(formula_obj, data=data(),
            main=paste("Boxplot of", paste(selected_cols)),
            ylab=as.character(dep_var),
            xlab=as.character(selected_cols),
            col="lightblue")
    
    meansAB <- aggregate(formula_obj, data=data(), mean)
    meansAB
    points(meansAB[,ncol(meansAB)],col="red",pch=18)  ##uses column [4] of data for plot
    lines(meansAB[,ncol(meansAB)])
  })
  
  
  ## Contour Plot 
  output$contour_checkboxes <- renderUI({
    div(
      tags$h2("Select Factors for Contour Plot"),
      checkboxGroupInput(
        "contour_checkboxes", 
        "Only Select 2, Must be Numeric",
        choices = setdiff(names(data()), input$dep_var),
        selected = NULL,
        inline = FALSE
      )
    )
  })
  
  output$contour <- renderPlot({
    req(input$contour_checkboxes, input$dep_var, data())
    
    selected_cols <- input$contour_checkboxes
    dep_var <- sym(input$dep_var)
    
    # Create formula string
    formula_str <- paste(as.character(dep_var), "~")
    for (col in selected_cols) {
      if (is.numeric(data()[[col]])) {
        formula_str <- paste(formula_str, col)
      } else {
        formula_str <- paste(formula_str, "factor(", col, ")")
      }
      formula_str <- paste(formula_str, "+")
    }
    # Remove last "+" and create formula object
    formula_obj <- as.formula(substr(formula_str, 1, nchar(formula_str) - 1))
    
    model <- lm(formula_obj, data=data())
    
    if (length(selected_cols) == 2) {
      # Create a grid of x and y values to evaluate the model
      x <- seq(min(data()[[selected_cols[1]]]), max(data()[[selected_cols[1]]]), length.out = 100)
      y <- seq(min(data()[[selected_cols[2]]]), max(data()[[selected_cols[2]]]), length.out = 100)
      xygrid <- expand.grid(x, y)
      colnames(xygrid) <- selected_cols
      
      # Evaluate the model at each point in the grid
      z <- predict(model, newdata = xygrid)
      zmat <- matrix(z, ncol = length(x))
      
      # Create a contour plot
      contour(x, y, zmat, 
              xlab = selected_cols[1], ylab = selected_cols[2],
              main = paste("Contour Plot of", input$dep_var),
              mar = c(5, 5, 3, 3))
    } else {
      # Show an error message if not enough variables
      plot.new()
      text(0.5, 0.5, "Please select 2 variables to create a contour plot.")
    }
  })
  
}
######################### Run the application ####################
shinyApp(ui = ui, server = server)
