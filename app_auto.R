## libraries
library(shiny)
library(shinyjs)
library(bslib)
library(bsicons)
library(tidyverse)
library(plotly)
options("scipen" = 100)


theme_set(theme_bw())





# yeti color
theme_color <- list(
    primary = '#008CBA',  
    secondary = '#EEEEEE',  
    success = "#43AC6A",  
    info = "#5BC0DE",  
    warning = "#FFCE67",  
    danger = "#E99002",
    light = "#EEEEEE",
    dark = "#333333"
)

## data ##
library(GLMsData)
data(motorins)
data <- motorins %>% rename(Kilometers = Kilometres)

data %>% mutate(
    Zone = factor(Zone),
    Make = factor(Make),
    Kilometers = factor(Kilometers),
    Bonus=factor(Bonus) ) -> data 

df_fit1 <- filter(data, Insured > 0)
df_fit2 <- filter(data, Claims > 0, Payment > 0)

# offset으로 변경
glm_poiss <-glm(Claims ~ Kilometers*Bonus+Make+Zone + offset(log(Insured)), family = poisson(link="log"), data=df_fit1)
glm_gamma <- glm(Payment/Claims~ Kilometers+Bonus+Make+Zone, weights = Claims, family=Gamma(link='log'), data=df_fit2)



premium <- function(A,B,new){
    
    
    # EN: Expected claim frequency
    EN <- predict(glm_poiss, newdata = new, type= "response")
    
    # EY: Expected claim severity
    B <- ifelse(B==0, Inf, B)   # limit
    
    # before deductible/limit...
    mu <- predict(glm_gamma, newdata = new, type= "response")
    shape <- 1/summary(glm_gamma)$dispersion
    scale <- mu/shape
    
    # \int yf(y)dy = mu*F_{Gamma(shape+1,scale)}(y)
    truncated_mean <- mu*(
        pgamma(A+B, shape = shape+1, scale = scale) - 
            pgamma(A, shape = shape+1, scale = scale)
        )
    
    
    F_U <- pgamma(A+B, shape = shape, scale = scale)
    F_A <- pgamma(A, shape = shape, scale = scale)
    
    deductible_adj <- -A*(F_U - F_A)
    
    
    tail_limit <- if(is.infinite(B)){
        0
    } else {
        B*(1-F_U)
    }
    
    EY <- truncated_mean + deductible_adj + tail_limit
    
    #Premium
    price <-EN*EY
    
    return(list(price=price, EN = EN, EY = EY))
}






## 퍼센트 계산용 

risk_reference_grid <- tidyr::expand_grid(
    Kilometers = levels(data$Kilometers),
    Bonus = levels(data$Bonus),
    Make = levels(data$Make),
    Zone = levels(data$Zone)
) %>%
    mutate(
        Kilometers = factor(Kilometers, levels = levels(data$Kilometers)),
        Bonus = factor(Bonus, levels = levels(data$Bonus)),
        Make = factor(Make, levels = levels(data$Make)),
        Zone = factor(Zone, levels = levels(data$Zone)),
        Insured = 1
    )


make_model_row <- function(kilometers, bonus, make, zone) {
    data.frame(
        Kilometers = factor(as.character(kilometers), levels = levels(data$Kilometers)),
        Bonus = factor(as.character(bonus), levels = levels(data$Bonus)),
        Make = factor(as.character(make), levels = levels(data$Make)),
        Zone = factor(as.character(zone), levels = levels(data$Zone)),
        Insured = 1
    )
}


compute_reference_premium_table <- function(a = 0, b = 0) {
    
    ref_grid <- as.data.frame(risk_reference_grid)
    
    premium_result <- premium(
        A = a,
        B = b,
        new = ref_grid
    )
    
    bind_cols(ref_grid, premium_result) %>%
        rename(
            expected_claim_frequency = EN,
            expected_claim_severity = EY,
            estimated_premium = price
        ) %>%
        arrange(estimated_premium)
}


calc_premium_percentile <- function(current_premium, reference_premiums) {
    reference_premiums <- reference_premiums[!is.na(reference_premiums)]
    
    if (length(reference_premiums) == 0 || is.na(current_premium)) {
        return(NA_real_)
    }
    
    mean(reference_premiums <= current_premium) * 100
}


# plot용

make_plot_grid <- function(var, input) {
    
    base <- make_model_row(
        kilometers = input$Kilometers,
        bonus = input$Bonus + 1,
        make = input$Make,
        zone = input$Zone
    )
    
    values <- levels(data[[var]])
    grid <- base[rep(1, length(values)), ]
    grid[[var]] <- factor(values, levels = levels(data[[var]]))
    
    grid
}


get_plot_labels <- function(var, values) {
    
    values <- as.character(values)
    
    if (var == "Bonus") {
        return(as.character(as.numeric(values) - 1))
    }
    
    if (var == "Kilometers") {
        kilo_labels <- c(
            "1" = "~ 1,000",
            "2" = "1,000 ~ 15,000",
            "3" = "15,000 ~ 20,000",
            "4" = "20,000 ~ 25,000",
            "5" = "25,000 ~"
        )
        return(unname(kilo_labels[values]))
    }
    
    if (var == "Zone") {
        return(unname(zone_labels[values]))
    }
    
    if (var == "Make") {
        return(unname(make_labels[values]))
    }
}




# 적용 환율 (고정값 - 원래 실시간 API 연동이었으나 배포용으로 고정)
displayExRate <- list(
    SEKUSD = 0.10,
    USDKRW = 1350
)

### contents ###

### input ###
# 
# 자기부담금, 보상한도
# 
aInput <- numericInput(
    inputId = "a",
    label = "Deductible",
    value = 500,
    min = 0,
    max = 1000
)
bInput <- numericInput(
    inputId = "b",
    label = "Coverage Limit",
    value = 1000000,  
    min = 0,
    max = 2000000
)


limitDetails <- accordion_panel(
    title = "Deductible & Limits",
    icon = bs_icon("calculator-fill"),
    aInput,
    bInput,
    checkboxInput("noLimits", "No deductible / No coverage limit", TRUE)
)

## 

zone_choices <- list(
    "Metropolitan Area" = 1,
    "Large City" = 2,
    "Suburban Area" = 3,
    "Mid-sized City" = 4,
    "Rural Area" = 5,
    "Remote Area" = 6,
    "Other Region" = 7
)
make_choices <- list(
    "Compact SUV" = 1,
    "Mid-size SUV" = 2,
    "Compact Car" = 3,
    "Sedan" = 4,
    "Small SUV" = 5,
    "Van / MPV" = 6,
    "Light Car" = 7,
    "Premium Sedan" = 8,
    "Large Sedan" = 9
)

make_labels <- setNames(names(make_choices), as.character(unlist(make_choices)))
zone_labels <- setNames(names(zone_choices), as.character(unlist(zone_choices)))



kiloInput <- selectInput(inputId = "Kilometers",
                          label = "Kilometers per Year",
                          choices = list("~ 1,000" = 1,
                                         "1,000 ~ 15,000" = 2,
                                         "15,000 ~ 20,000" = 3,
                                         "20,000 ~ 25,000" = 4,
                                         "25,000 ~" = 5),
                          selected = 1)

zoneInput <- selectInput(inputId = "Zone",
                          label = span("Driving Region",
                                       tooltip(bs_icon("info-circle"),
                                               HTML("Region labels are for display purposes only.<br>
     This does not represent actual Swedish regional insurance rates."))),
                          choices = zone_choices,
                          selected = 1)

bonusInput <- sliderInput(inputId = "Bonus",
                          label = "No-Claim Bonus Period",
                          min = 0,
                          max = 6,
                          value = 6)

sensitivityInput <- toolbar_input_select(id = "sensitivity_var",
                                         label = "Select a factor to vary in the chart",
                                choices = c(
                                    "No-Claim Bonus Period" = "Bonus",
                                    "Kilometers per year" = "Kilometers",
                                    "Vehicle Type" = "Make",
                                    "Driving Region" = "Zone"
                                ),
                                selected = "Bonus",
                                icon = icon("ellipsis-vertical")
                            )


## 차종 + 몇년식
# 랜덤설정

carInput <- selectInput(inputId = "Make",
                          label = span("Vehicle Type",
                                       tooltip(bs_icon("info-circle"),
                                               HTML("Vehicle type labels are for display purposes only.<br>
     This does not represent actual insurance rates or loss ratios by vehicle type."))),
                          choices = make_choices,
                          selected = 1)

userInfo <- accordion_panel(
    title = "Risk Profile",
    icon = bs_icon("car-front-fill"),
    carInput, kiloInput, bonusInput, zoneInput)


# ## output ##
# 
# ## premium box - kr, usd, won (1크로나 = 0.22 달러 / 1달러 = 484원)
# 현재 환율 사용? -> 체크박스 옵션 선택


vbs_KPI <- list(
    # 보험료
    value_box(
        title = "Estimated Annual Premium",
        value = textOutput("p_won_kpi", inline = TRUE),
        theme = "primary",
        showcase = bs_icon("cash-stack"),
        p(textOutput("p_sub_kpi", inline = TRUE))
    ),
    
    # 평균 사고건수, 평균 사고 심도
    value_box(
        title = HTML("Expected Claim Frequency"),
        value = textOutput("EN"),
        theme = "light",
        showcase = bs_icon("activity"),
        p("Expected claims per insured")
    ),
    value_box(
        title = HTML("Expected Claim Severity"),
        value = textOutput("EY"),
        theme = "light",
        showcase = bs_icon("bar-chart-line"),
        p("Expected payment per claim")
    ),
    value_box(
        title = "Premium Percentile",
        value = textOutput("premium_percentile_kpi", inline = TRUE),
        theme = "light",
        showcase = bs_icon("speedometer2"),
        p(textOutput("percentile_note", inline = TRUE))
    )
)


kpiCards <- layout_column_wrap(style = "text-align: center;",
                               width = 1/4,
                               !!!vbs_KPI)

## plot - bonus period
bonusCard <- card(class = "border-0", 
                  card_header(class = "border-0", 
                              "Premium Sensitivity Analysis",
                              toolbar(
                                  align = "right",
                                  sensitivityInput
                              )),
                  card_body(plotlyOutput("plt"),
                            p(
                                "Only the selected factor changes; all other profile settings are held constant.",
                                style = "font-size: 13px; color: #666666; margin-top: 8px;"
                            )
                        ))



selectedProfilecard <- card(class = "border-0", 
                            card_header(class = "border-0", 
                                        "Selected Profile Summary"),
                            card_body(
                                
                                
                            p(
                                textOutput("exchange_rate_note", inline = TRUE),
                                style = "font-size: 12px; color: #666666; margin-top: 4px;"
                            ),
                            
                            hr(),
                            
                            h6("Estimated Annual Premium"),
                            textOutput("p_won_summary"),
                            p(
                                textOutput("p_sub_summary", inline = TRUE),
                                style = "color: #666666;"
                            ),
                            
                            hr(),
                            
                            h6("Premium Percentile"),
                            h3(textOutput("premium_percentile_summary", inline = TRUE)),
                            # h3(textOutput(...))처럼 쓸 때는 inline = TRUE를 넣는 게 더 안전합니다.
                            # textOutput()은 기본적으로 <div>를 만들기 때문에
                            # 제목 태그 안에 들어가면 HTML 구조가 어색해질 수 있습니다.
                            p(
                                "Compared across reference risk profiles under the selected deductible and coverage limit.",
                                style = "font-size: 13px; color: #666666;"
                            )
                    )
            )




## UI
ui <- page_navbar(
    title = "Auto Insurance Premium Simulator",
    id = "nav",
    theme = bs_theme(version = 5, bootswatch = "yeti"),
    underline = TRUE,
    
    # sidebar inputs
    sidebar = sidebar(
        useShinyjs(),
        width = "350px",
        accordion(limitDetails,
                  userInfo)
    ),
    
    
    nav_panel(title = "Premium Calculator",
              
              kpiCards,
              
              layout_columns(col_widths = c(8,4),
                             bonusCard,
                             selectedProfilecard),
              
              
              layout_columns(
                  col_widths = c(6, 6),
                  
                  card(
                      card_header("Pricing Logic"),
                      p("The estimated premium combines expected claim frequency and expected claim severity."),
                      p("Claim frequency is estimated using a Poisson GLM with exposure offset."),
                      p("The frequency model includes an interaction between annual mileage and no-claim bonus period."),
                      p("Claim severity is estimated using a Gamma GLM with claim-count weights."),
                      p("Deductible and coverage limit settings are applied to the expected claim severity.")
                  ),
                  
                  card(
                      card(
                          card_header("Data & Interpretation Note"),
                          p("This simulator demonstrates a model-based auto insurance pricing workflow."),
                          p("The underlying data is the ", em("motorins"), " dataset from the R package ", em("GLMsData"), " (Dunn & Smyth), based on Swedish motor insurance records."),
                          p("Vehicle type and region labels are generalized for display purposes and do not correspond to actual Swedish categories."),
                          p("Premiums and KRW reference values are ", strong("model outputs only"), " and should not be interpreted as real-world insurance quotes.")
                      )
                  )
              )
              
              
              )
    
)

## Server
server <- function(input, output, session) {
    
    # 체크박스 상태에 따라 selectInput 활성화/비활성화
    observe({
        if (input$noLimits) {
            disable("a")
            disable("b") # 체크박스가 체크되면 selectInput 비활성화
        } else {
            enable("a")
            enable("b")   # 체크박스가 체크되지 않으면 selectInput 활성화
        }
    })
    
    
    coverage_terms <- reactive({
        
        if (isTRUE(input$noLimits)) {
            list(a = 0, b = 0)
        } else {
            list(a = input$a, b = input$b)
        }
    })
    
    
    percentile_reference <- reactive({
        cov <- coverage_terms()
        
        compute_reference_premium_table(
            a = cov$a,
            b = cov$b
        )
    }) %>%
        bindCache(input$noLimits, input$a, input$b)
    
    
    percentile_value <- reactive({
        current_premium <- as.numeric(new()$price)
        ref_table <- percentile_reference()
        
        calc_premium_percentile(
            current_premium = current_premium,
            reference_premiums = ref_table$estimated_premium
        )
    })
    
    

    new <- reactive({
        df<-make_model_row(kilometers = input$Kilometers,
                           bonus = input$Bonus+1,
                           make = input$Make,
                           zone = input$Zone)
        
        cov <- coverage_terms()
        
        res <- premium(cov$a, cov$b, df)
        
        return(res)
        
    })
    

    
    
    out<-reactive({
        p  <- new()$price
        
        list(p_kr = round(p),
             p_won = round(p * displayExRate$SEKUSD * displayExRate$USDKRW)
            )
        
    })
    
    
    premium_main_text <- reactive({
        res <- out()
        paste("KRW", format(res$p_won, big.mark = ","))
    })
    
    premium_sub_text <- reactive({
        res <- out()
        paste(
            "Original model-scale premium:", format(res$p_kr, big.mark = ",")
        )
    })
    
    premium_percentile_text <- reactive({
        pct <- percentile_value()
        
        if (is.na(pct)) {
            return("N/A")
        }
        
        paste0(round(pct, 1), "%")
    })
    
    output$p_won_kpi <- renderText({
        premium_main_text()
    })
    
    output$p_won_summary <- renderText({
        premium_main_text()
    })
    
    output$p_sub_kpi <- renderText({
        premium_sub_text()
    })
    
    output$p_sub_summary <- renderText({
        premium_sub_text()
    })
    
    output$premium_percentile_kpi <- renderText({
        premium_percentile_text()
    })

    output$premium_percentile_summary <- renderText({
        premium_percentile_text()
    })
    
    
    output$exchange_rate_note <- renderText({
        paste0(
            "KRW reference display uses fixed FX: SEK/USD ",
            round(displayExRate$SEKUSD, 4),
            " · USD/KRW ",
            format(displayExRate$USDKRW, big.mark = ","),
            ". FX conversion affects display values only and does not change the premium model or percentile ranking."
        )
    })
    
    
    output$EN <- renderText({
        new <- new()
        round(new$EN, 4)
    })
    
    output$EY <- renderText({
        new <- new()
        round(new$EY, 2)
    })
    

    
    output$plt <- renderPlotly({
        
        var <- input$sensitivity_var
        grid <- make_plot_grid(var, input)
        cov <- coverage_terms()
        
        premium_result <- premium(
            A = cov$a,
            B = cov$b,
            new = grid
        )
        
        plot_df <- grid %>%
            mutate(
                premium_base = as.numeric(premium_result$price),
                premium_krw = premium_base * displayExRate$SEKUSD * displayExRate$USDKRW,
                label = get_plot_labels(var, .data[[var]])
            )
        
        if (var == "Bonus") {
            
            plot_df <- plot_df %>%
                mutate(x_value = as.numeric(as.character(Bonus)) - 1)
            
            plt <- ggplot(
                plot_df,
                aes(
                    x = x_value,
                    y = premium_krw,
                    text = paste0(
                        "No-Claim Bonus Period: ", label,
                        "<br>Premium: KRW ", format(round(premium_krw), big.mark = ",")
                    )
                )
            ) +
                geom_line(aes(group = 1), color = theme_color$primary) +
                geom_point(color = theme_color$primary) +
                geom_vline(xintercept = input$Bonus, color = theme_color$dark) +
                scale_x_continuous(breaks = seq(0, 6, by = 1)) +
                labs(
                    title = "Premium Change by No-Claim Bonus Period",
                    x = "No-Claim Bonus Period",
                    y = "Estimated Annual Premium (KRW Reference)"
                ) +
                theme(
                    plot.title = element_text(face = "bold"),
                    panel.grid.minor = element_blank()
                )
            
        } else if (var == "Kilometers") {
            
            plot_df <- plot_df %>%
                mutate(x_value = as.numeric(as.character(Kilometers)) - 1)
            
            plt <- ggplot(
                plot_df,
                aes(
                    x = x_value,
                    y = premium_krw,
                    text = paste0(
                        "Kilometers per Year: ", label,
                        "<br>Premium: KRW ", format(round(premium_krw), big.mark = ",")
                    )
                )
            ) +
                geom_line(aes(group = 1), color = theme_color$primary) +
                geom_point(color = theme_color$primary) +
                geom_vline(xintercept = as.numeric(input$Kilometers), color = theme_color$dark) +
                scale_x_continuous(
                    breaks = 1:5,
                    labels = get_plot_labels("Kilometers", as.character(1:5))
                ) +
                labs(
                    title = "Premium Change by Annual Mileage",
                    x = "Kilometers per Year",
                    y = "Estimated Annual Premium (KRW Reference)"
                ) +
                theme(
                    plot.title = element_text(face = "bold"),
                    panel.grid.minor = element_blank()
                )
            
        } else {
            
            plot_df <- plot_df %>%
                mutate(
                    is_selected = case_when(
                        var == "Make" ~ as.character(Make) == as.character(input$Make),
                        var == "Zone" ~ as.character(Zone) == as.character(input$Zone),
                        TRUE ~ FALSE
                    ),
                    label = factor(label, levels = label[order(premium_krw)])
                )
            
            title_text <- ifelse(
                var == "Make",
                "Premium Comparison by Vehicle Type",
                "Premium Comparison by Driving Region"
            )
            
            x_text <- ifelse(
                var == "Make",
                "Vehicle Type",
                "Driving Region"
            )
            
            plt <- ggplot(
                plot_df,
                aes(
                    x = label,
                    y = premium_krw,
                    fill = is_selected,
                    text = paste0(
                        x_text, ": ", label,
                        "<br>Premium: KRW ", format(round(premium_krw), big.mark = ","),
                        ifelse(is_selected, "<br>Selected profile", "")
                    )
                )
            ) +
                geom_col(width = 0.68) +
                coord_flip() +
                scale_fill_manual(
                    values = c(
                        "TRUE" = theme_color$primary,
                        "FALSE" = "#D8DEE9"
                    ),
                    guide = "none"
                ) +
                labs(
                    title = title_text,
                    subtitle = "The selected option is highlighted; all other profile settings are held constant",
                    x = x_text,
                    y = "Estimated Annual Premium (KRW Reference)"
                ) +
                theme(
                    plot.title = element_text(face = "bold"),
                    panel.grid.minor = element_blank(),
                    panel.grid.major.y = element_blank()
                )
        }
        
        ggplotly(plt, tooltip = "text") %>%
            layout(showlegend = FALSE)
    })
    
    
    output$percentile_note <- renderText({
        "Compared across model-based reference risk profiles under the selected deductible and coverage limit."
    })
    
    
}

## Run the application 
shinyApp(ui = ui, server = server)
