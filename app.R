# =============================================================================
# APP.R
# Geovanny Reyes
# Samuel Renteria
# Sebastian Orozco
# =============================================================================

source("global.R")

# =============================================================================
# UI
# =============================================================================
ui <- page_navbar(
  title = "Riesgo Laboral Juvenil",
  theme = app_theme,
  fillable = TRUE,
  bg = PAL$azul_profundo,

  # ---------------------------------------------------------------------------
  # PESTAÑA 1: CONTEXTO DEL PROBLEMA
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "1. Contexto",
    icon = icon("compass"),
    layout_columns(
      col_widths = c(8, 4),

      card(
        card_header("Problema de politica publica"),
        card_body(
          h4("Riesgo laboral juvenil y desempleo"),
          p("El desempleo y la informalidad juvenil tienen efectos persistentes
            sobre los ingresos, la acumulacion de experiencia y la movilidad
            social de las personas jovenes. Un ministerio o una alcaldia
            requiere anticipar que jovenes presentan alta probabilidad de
            desempleo o informalidad, con el fin de focalizar programas de
            formacion e intermediacion laboral antes de que la trayectoria
            de exclusion se consolide."),
          tags$ul(
            tags$li(strong("Poblacion de analisis: "),
                    "jovenes entre 15 y 28 anos, encuesta tipo GEIH (DANE)."),
            tags$li(strong("Variable dependiente (Y): "),
                    "1 si el joven esta desempleado o en informalidad; ",
                    "0 si esta ocupado formalmente."),
            tags$li(strong("Variables explicativas: "),
                    "edad, sexo, nivel educativo, experiencia laboral (meses),
                    estrato socioeconomico y zona (cabecera / resto)."),
            tags$li(strong("Metodologia: "),
                    "Modelo de Probabilidad Lineal (LPM) con errores robustos,
                    Logit y Probit, efectos marginales promedio (APE) y
                    evaluacion predictiva fuera de muestra (ROC/AUC, matriz
                    de confusion, sensibilidad/especificidad).")
          ),
          hr(class = "sep"),
          h5("Decisiones de politica que apoya este aplicativo"),
          p("Focalizacion de programas de primer empleo, subsidios a la
            contratacion juvenil, formacion tecnica e intermediacion laboral
            territorial, priorizando a los jovenes con mayor probabilidad
            estimada de desempleo o informalidad.")
        )
      ),

      card(
        card_header("Ficha tecnica de los datos"),
        card_body(
          tags$table(class = "table table-sm",
            tags$tr(tags$td("Observaciones totales (GEIH)"), tags$td(strong(comma(n_total_geih)))),
            tags$tr(tags$td("Con dato valido de Y"), tags$td(strong(comma(n_con_Y)))),
            tags$tr(tags$td("Muestra final de estimacion"), tags$td(strong(comma(n_modelo)))),
            tags$tr(tags$td("Entrenamiento (70%)"), tags$td(strong(comma(nrow(datos_train))))),
            tags$tr(tags$td("Prueba (30%)"), tags$td(strong(comma(nrow(datos_test))))),
            tags$tr(tags$td("Semilla aleatoria"), tags$td(strong("2026")))
          ),
          hr(class = "sep"),
          p(tags$em("Nota metodologica: los jovenes con valor faltante en la
            variable dependiente (no aplica, fuera de fuerza de trabajo o
            sin dato) se excluyen de la estimacion, siguiendo el mismo
            tratamiento documentado en el codigo reproducible del equipo."))
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # PESTAÑA 2: EXPLORACION DE DATOS
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "2. Exploracion",
    icon = icon("magnifying-glass-chart"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filtros de exploracion",
        width = 280,
        sliderInput("exp_edad", "Rango de edad",
                    min = min(datos$EDAD), max = max(datos$EDAD),
                    value = c(min(datos$EDAD), max(datos$EDAD)), step = 1),
        checkboxGroupInput("exp_sexo", "Sexo",
                            choices = c("Hombre", "Mujer"),
                            selected = c("Hombre", "Mujer")),
        checkboxGroupInput("exp_zona", "Zona",
                            choices = c("Cabecera", "Resto (rural)"),
                            selected = c("Cabecera", "Resto (rural)")),
        hr(class = "sep"),
        p(tags$em("Estos filtros solo afectan los graficos descriptivos de
                   esta pestana; los modelos econometricos se estiman sobre
                   la muestra completa de jovenes con dato valido de Y."))
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(title = "Jovenes en la muestra filtrada", value = textOutput("kpi_n"),
                   showcase = icon("users"), theme = "primary"),
        value_box(title = "Tasa de riesgo laboral", value = textOutput("kpi_tasa"),
                   showcase = icon("triangle-exclamation"), theme = "secondary"),
        value_box(title = "Experiencia promedio (meses)", value = textOutput("kpi_exp"),
                   showcase = icon("briefcase"), theme = "success")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Distribucion de la variable dependiente (Y)"),
             card_body(plotOutput("plot_balance_y", height = 320))),
        card(card_header("Riesgo laboral por nivel educativo"),
             card_body(plotOutput("plot_riesgo_educ", height = 320)))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Riesgo laboral por sexo y zona"),
             card_body(plotOutput("plot_riesgo_sexo_zona", height = 320))),
        card(card_header("Experiencia laboral segun riesgo"),
             card_body(plotOutput("plot_experiencia_box", height = 320)))
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # PESTAÑA 3: ESTIMACION DE MODELOS
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "3. Modelos",
    icon = icon("calculator"),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Especificacion econometrica"),
        card_body(
          p("Se estima la probabilidad de desempleo o informalidad juvenil
            mediante tres especificaciones comparables:"),
          tags$div(style = "text-align:center; font-family: 'Lora', serif; font-size: 1.05rem; margin: 0.8rem 0;",
            "P(riesgo_laboral = 1 | X) = G(\u03B2\u2080 + \u03B2\u2081 EDAD + \u03B2\u2082 SEXO + \u03B2\u2083 NIVEL_EDUCATIVO + \u03B2\u2084 EXPERIENCIA_MESES + \u03B2\u2085 ESTRATO + \u03B2\u2086 ZONA)"
          ),
          p(strong("LPM: "), "G(\u00B7) es la funcion identidad (estima la probabilidad
            directamente). ", strong("Logit: "), "G(\u00B7) es la funcion logistica. ",
            strong("Probit: "), "G(\u00B7) es la funcion de distribucion normal estandar."),
          p(tags$em("Advertencia metodologica: los coeficientes de Logit y Probit
            no son interpretables como cambios directos en probabilidad. La
            lectura correcta se realiza mediante efectos marginales promedio
            (APE), reportados en la tabla inferior."))
        )
      )
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Tabla comparativa de coeficientes (errores robustos)"),
        card_body(DTOutput("tabla_coeficientes"))
      ),
      card(
        card_header("Efectos marginales promedio (APE)"),
        card_body(
          p(tags$em("Cambio en la probabilidad de riesgo laboral ante un
                      incremento de una unidad en cada variable, manteniendo
                      lo demas constante.")),
          DTOutput("tabla_ape")
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Lectura de resultados"),
        card_body(uiOutput("interpretacion_modelos"))
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # PESTAÑA 4: EVALUACION PREDICTIVA
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "4. Evaluacion predictiva",
    icon = icon("bullseye"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Umbral de clasificacion",
        width = 280,
        p("El modelo predictivo (Logit, entrenado en el 70% de la muestra)
           asigna una probabilidad de riesgo laboral a cada joven de la
           muestra de prueba (30% restante). Ajuste el umbral de decision
           para explorar el balance entre falsos positivos y falsos
           negativos."),
        sliderInput("umbral", "Umbral de clasificacion",
                    min = 0.1, max = 0.9, value = 0.5, step = 0.01),
        hr(class = "sep"),
        p(tags$em("Un umbral bajo prioriza detectar a la mayoria de jovenes
                   en riesgo (mayor sensibilidad, mas falsos positivos).
                   Un umbral alto reduce falsas alarmas pero deja mas casos
                   de riesgo sin detectar (mayor especificidad, mas falsos
                   negativos). La eleccion del umbral debe basarse en el
                   costo institucional relativo de cada tipo de error."))
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(title = "AUC (area bajo la curva ROC)",
                   value = paste0(round(auc_valor, 3)),
                   showcase = icon("chart-line"), theme = "primary"),
        value_box(title = "Sensibilidad (umbral elegido)",
                   value = textOutput("kpi_sens"),
                   showcase = icon("check"), theme = "success"),
        value_box(title = "Especificidad (umbral elegido)",
                   value = textOutput("kpi_esp"),
                   showcase = icon("shield"), theme = "secondary")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Curva ROC (interactiva)"),
             card_body(plotlyOutput("plot_roc", height = 340))),
        card(card_header("Matriz de confusion (muestra de prueba)"),
             card_body(plotOutput("plot_confusion", height = 340)))
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Metricas de desempeno segun el umbral elegido"),
          card_body(
            p(tags$em("Recuerde: en problemas con clases desbalanceadas, la
                       exactitud (accuracy) no es suficiente. Se reportan
                       tambien sensibilidad, especificidad, precision y F1-score.")),
            tableOutput("tabla_metricas_umbral")
          )
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # PESTAÑA 5: SIMULADOR DE PERFILES
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "5. Simulador",
    icon = icon("sliders"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Construya un perfil hipotetico",
        width = 320,
        sliderInput("sim_edad", "Edad", min = 15, max = 28, value = 20, step = 1),
        radioButtons("sim_sexo", "Sexo", choices = c("Hombre", "Mujer"), selected = "Hombre"),
        selectInput("sim_educ", "Nivel educativo",
                    choices = names(niveles_educativos), selected = "Media academica o tecnica"),
        sliderInput("sim_exp", "Experiencia laboral (meses)", min = 0, max = 120, value = 6, step = 1),
        sliderInput("sim_estrato", "Estrato socioeconomico", min = 0, max = 6, value = 2, step = 1),
        radioButtons("sim_zona", "Zona", choices = c("Cabecera", "Resto (rural)"), selected = "Cabecera"),
        hr(class = "sep"),
        p(tags$em("Este simulador usa el modelo Logit estimado en la pestana
                   anterior para calcular la probabilidad de riesgo laboral
                   de un joven con estas caracteristicas. Es una herramienta
                   de apoyo a la decision, no una prediccion individual
                   determinista."))
      ),
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("Probabilidad estimada de riesgo laboral"),
          card_body(
            plotOutput("plot_gauge", height = 260),
            uiOutput("etiqueta_riesgo_sim")
          )
        ),
        card(
          card_header("Como se compara este perfil con la muestra"),
          card_body(plotOutput("plot_comparacion_sim", height = 320))
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Recomendacion de focalizacion"),
          card_body(uiOutput("recomendacion_sim"))
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # PESTAÑA 6: PRIORIZACION Y DESCARGA
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "6. Priorizacion",
    icon = icon("list-check"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Criterios de priorizacion",
        width = 280,
        sliderInput("prior_umbral", "Probabilidad minima de riesgo a priorizar",
                    min = 0, max = 1, value = 0.6, step = 0.01),
        selectInput("prior_zona", "Filtrar por zona",
                    choices = c("Todas", "Cabecera", "Resto (rural)"), selected = "Todas"),
        selectInput("prior_sexo", "Filtrar por sexo",
                    choices = c("Todos", "Hombre", "Mujer"), selected = "Todos"),
        hr(class = "sep"),
        downloadButton("descargar_priorizados", "Descargar lista priorizada (.csv)",
                        class = "btn-secondary w-100")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(title = "Jovenes priorizados", value = textOutput("kpi_priorizados"),
                   showcase = icon("flag"), theme = "secondary"),
        value_box(title = "% del total filtrado", value = textOutput("kpi_pct_priorizados"),
                   showcase = icon("percent"), theme = "primary"),
        value_box(title = "Probabilidad promedio del grupo", value = textOutput("kpi_prob_prom"),
                   showcase = icon("chart-simple"), theme = "success")
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Listado de jovenes priorizados (ordenado por probabilidad de riesgo)"),
          card_body(DTOutput("tabla_priorizados"))
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # PESTAÑA 7: MAPA TERRITORIAL
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "7. Mapa territorial",
    icon = icon("map-location-dot"),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Riesgo laboral juvenil por departamento"),
        card_body(
          p("El modelo individual (pestañas 3 a 5) no usa el departamento como
             variable explicativa: las diferencias territoriales que se ven
             aqui son ", strong("puramente descriptivas"), " y reflejan
             composicion poblacional, estructura productiva e informalidad
             regional, no un efecto causal estimado por el modelo. Sirven
             como insumo complementario para decidir ", em("donde"),
             " focalizar programas, mientras el simulador (pestaña 5) ayuda
             a decidir ", em("a quien"), " priorizar dentro de un territorio.")
        )
      )
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Mapa interactivo: tasa de riesgo laboral por departamento"),
        card_body(leafletOutput("mapa_riesgo", height = 480))
      ),
      card(
        card_header("Ranking de departamentos"),
        card_body(DTOutput("tabla_riesgo_dpto"))
      )
    )
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text text-light",
                      "Univalle | Econometria II"))
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # PESTAÑA 2: EXPLORACION
  # ---------------------------------------------------------------------------
  datos_filtrados <- reactive({
    datos %>%
      filter(
        EDAD >= input$exp_edad[1], EDAD <= input$exp_edad[2],
        as.character(SEXO_lbl) %in% input$exp_sexo,
        as.character(ZONA_lbl) %in% input$exp_zona
      )
  })

  output$kpi_n <- renderText({ comma(nrow(datos_filtrados())) })
  output$kpi_tasa <- renderText({
    df <- datos_filtrados()
    if (nrow(df) == 0) return("N/D")
    percent(mean(df$riesgo_laboral), accuracy = 0.1)
  })
  output$kpi_exp <- renderText({
    df <- datos_filtrados()
    if (nrow(df) == 0) return("N/D")
    round(mean(df$EXPERIENCIA_MESES), 1)
  })

  output$plot_balance_y <- renderPlot({
    df <- datos_filtrados()
    ggplot(df, aes(x = riesgo_lbl, fill = riesgo_lbl)) +
      geom_bar() +
      geom_text(stat = "count", aes(label = scales::comma(after_stat(count))), vjust = -0.4) +
      scale_fill_manual(values = c("Ocupado formal" = PAL$verde_salvia,
                                    "Desempleo/Informalidad" = PAL$terracota)) +
      labs(x = NULL, y = "Numero de jovenes") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  output$plot_riesgo_educ <- renderPlot({
    df <- datos_filtrados() %>%
      mutate(educ_lbl = factor(NIVEL_EDUCATIVO, levels = niveles_educativos,
                                labels = names(niveles_educativos))) %>%
      group_by(educ_lbl) %>%
      summarise(tasa = mean(riesgo_laboral), n = n(), .groups = "drop") %>%
      filter(n >= 10)
    ggplot(df, aes(x = tasa, y = reorder(educ_lbl, tasa))) +
      geom_col(fill = PAL$azul_medio) +
      scale_x_continuous(labels = percent) +
      labs(x = "Tasa de riesgo laboral", y = NULL) +
      theme_minimal(base_size = 12)
  })

  output$plot_riesgo_sexo_zona <- renderPlot({
    df <- datos_filtrados() %>%
      group_by(SEXO_lbl, ZONA_lbl) %>%
      summarise(tasa = mean(riesgo_laboral), .groups = "drop")
    ggplot(df, aes(x = SEXO_lbl, y = tasa, fill = ZONA_lbl)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(values = c("Cabecera" = PAL$azul_medio, "Resto (rural)" = PAL$terracota)) +
      labs(x = NULL, y = "Tasa de riesgo laboral", fill = "Zona") +
      theme_minimal(base_size = 13)
  })

  output$plot_experiencia_box <- renderPlot({
    df <- datos_filtrados()
    ggplot(df, aes(x = riesgo_lbl, y = EXPERIENCIA_MESES, fill = riesgo_lbl)) +
      geom_boxplot(alpha = 0.85, outlier.alpha = 0.3) +
      scale_fill_manual(values = c("Ocupado formal" = PAL$verde_salvia,
                                    "Desempleo/Informalidad" = PAL$terracota)) +
      labs(x = NULL, y = "Experiencia laboral (meses)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  # ---------------------------------------------------------------------------
  # PESTAÑA 3: MODELOS
  # ---------------------------------------------------------------------------
  output$tabla_coeficientes <- renderDT({
    cf_lpm    <- coef_lpm_robusto
    cf_logit  <- summary(modelo_logit)$coefficients
    cf_probit <- summary(modelo_probit)$coefficients

    vars <- rownames(cf_lpm)
    tabla <- data.frame(
      Variable   = vars,
      LPM        = round(cf_lpm[, 1], 4),
      `LPM (p-valor)`  = round(cf_lpm[, 4], 4),
      Logit      = round(cf_logit[vars, 1], 4),
      `Logit (p-valor)` = round(cf_logit[vars, 4], 4),
      Probit     = round(cf_probit[vars, 1], 4),
      `Probit (p-valor)` = round(cf_probit[vars, 4], 4),
      check.names = FALSE
    )
    datatable(tabla, rownames = FALSE, options = list(pageLength = 7, dom = "t"))
  })

  output$tabla_ape <- renderDT({
    tabla <- data.frame(
      Variable = ape_logit$factor,
      `APE Logit`  = round(ape_logit$AME, 4),
      `APE Probit` = round(ape_probit$AME[match(ape_logit$factor, ape_probit$factor)], 4),
      check.names = FALSE
    )
    datatable(tabla, rownames = FALSE, options = list(pageLength = 7, dom = "t"))
  })

  output$interpretacion_modelos <- renderUI({
    ape_edu <- ape_logit$AME[ape_logit$factor == "NIVEL_EDUCATIVO"]
    ape_exp <- ape_logit$AME[ape_logit$factor == "EXPERIENCIA_MESES"]
    tagList(
      p("Los tres modelos (LPM, Logit, Probit) son consistentes en el signo
         de sus coeficientes, lo cual es esperable dado que estiman la misma
         relacion subyacente con distintas funciones de enlace."),
      p(strong("Nivel educativo: "), "el efecto marginal promedio del Logit
         indica que un punto adicional en la escala de nivel educativo se
         asocia con un cambio de ", strong(percent(ape_edu, accuracy = 0.01)),
         " en la probabilidad de riesgo laboral, manteniendo lo demas constante."),
      p(strong("Experiencia laboral: "), "cada mes adicional de experiencia
         se asocia con un cambio de ", strong(percent(ape_exp, accuracy = 0.01)),
         " en la probabilidad de riesgo laboral."),
      p(tags$em("Estos efectos marginales son asociaciones estimadas bajo el
         modelo, no relaciones causales: no existe en este diseno una
         estrategia de identificacion (variables instrumentales, diseno
         cuasi-experimental, panel con efectos fijos) que permita descartar
         sesgo por variables omitidas o causalidad inversa."))
    )
  })

  # ---------------------------------------------------------------------------
  # PESTAÑA 4: EVALUACION PREDICTIVA
  # ---------------------------------------------------------------------------
  metricas_umbral <- reactive({
    calcular_metricas(prob_pred_test, datos_test$riesgo_laboral, input$umbral)
  })

  output$kpi_sens <- renderText({ percent(metricas_umbral()$sensibilidad, accuracy = 0.1) })
  output$kpi_esp  <- renderText({ percent(metricas_umbral()$especificidad, accuracy = 0.1) })

  output$plot_roc <- renderPlotly({
    df_roc <- data.frame(
      especificidad = rev(curva_roc$specificities),
      sensibilidad  = rev(curva_roc$sensitivities),
      umbral_pt     = rev(curva_roc$thresholds)
    )
    p <- ggplot(df_roc, aes(x = 1 - especificidad, y = sensibilidad,
                             text = paste0("Umbral: ", round(umbral_pt, 2),
                                           "<br>Sensibilidad: ", round(sensibilidad, 3),
                                           "<br>Especificidad: ", round(especificidad, 3)))) +
      geom_line(color = PAL$azul_medio, linewidth = 1.1, aes(group = 1)) +
      geom_abline(linetype = "dashed", color = "grey60") +
      annotate("text", x = 0.65, y = 0.15,
               label = paste0("AUC = ", round(auc_valor, 3)), size = 4.2, color = PAL$azul_profundo) +
      labs(x = "1 - Especificidad (falsos positivos)", y = "Sensibilidad (verdaderos positivos)") +
      theme_minimal(base_size = 13)
    ggplotly(p, tooltip = "text")
  })

  output$plot_confusion <- renderPlot({
    cm <- metricas_umbral()$cm$table
    df_cm <- as.data.frame(cm)
    colnames(df_cm) <- c("Prediccion", "Real", "Freq")
    ggplot(df_cm, aes(x = Real, y = Prediccion, fill = Freq)) +
      geom_tile(color = "white") +
      geom_text(aes(label = comma(Freq)), size = 6, color = "white") +
      scale_fill_gradient(low = PAL$azul_medio, high = PAL$azul_profundo) +
      labs(x = "Clase real", y = "Clase predicha") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  output$tabla_metricas_umbral <- renderTable({
    m <- metricas_umbral()
    data.frame(
      Metrica = c("Exactitud (Accuracy)", "Sensibilidad", "Especificidad", "Precision", "F1-score"),
      Valor = percent(c(m$accuracy, m$sensibilidad, m$especificidad, m$precision, m$f1), accuracy = 0.1)
    )
  }, striped = TRUE, bordered = TRUE, width = "60%")

  # ---------------------------------------------------------------------------
  # PESTAÑA 5: SIMULADOR
  # ---------------------------------------------------------------------------
  perfil_sim <- reactive({
    data.frame(
      EDAD = input$sim_edad,
      SEXO = ifelse(input$sim_sexo == "Hombre", 1, 2),
      NIVEL_EDUCATIVO = niveles_educativos[[input$sim_educ]],
      EXPERIENCIA_MESES = input$sim_exp,
      ESTRATO = input$sim_estrato,
      ZONA = ifelse(input$sim_zona == "Cabecera", 1, 2)
    )
  })

  prob_sim <- reactive({
    predict(modelo_logit, newdata = perfil_sim(), type = "response")
  })

  output$plot_gauge <- renderPlot({
    p <- as.numeric(prob_sim())
    color_gauge <- if (p < 0.4) PAL$verde_salvia else if (p < 0.65) "#D9A441" else PAL$terracota
    ggplot(data.frame(x = 1, y = p)) +
      geom_col(aes(x = x, y = 1), fill = "#E5E1D6", width = 1) +
      geom_col(aes(x = x, y = y), fill = color_gauge, width = 1) +
      geom_text(aes(x = x, y = 0.5, label = percent(p, accuracy = 0.1)),
                size = 11, color = "white", fontface = "bold") +
      coord_flip() +
      ylim(0, 1) +
      theme_void()
  })

  output$etiqueta_riesgo_sim <- renderUI({
    p <- as.numeric(prob_sim())
    nivel <- if (p < 0.4) "Riesgo bajo" else if (p < 0.65) "Riesgo medio" else "Riesgo alto"
    color <- if (p < 0.4) PAL$verde_salvia else if (p < 0.65) "#D9A441" else PAL$terracota
    tags$h4(style = paste0("text-align:center; color:", color, ";"), nivel)
  })

  output$plot_comparacion_sim <- renderPlot({
    p_sim <- as.numeric(prob_sim())
    ggplot(datos, aes(x = prob_riesgo)) +
      geom_histogram(bins = 30, fill = PAL$azul_medio, alpha = 0.75) +
      geom_vline(xintercept = p_sim, color = PAL$terracota, linewidth = 1.3, linetype = "dashed") +
      annotate("text", x = p_sim, y = Inf, label = "  Perfil simulado", vjust = 2,
               hjust = ifelse(p_sim > 0.7, 1.05, -0.05), color = PAL$terracota, size = 4.5) +
      labs(x = "Probabilidad estimada de riesgo laboral", y = "Numero de jovenes (muestra)") +
      theme_minimal(base_size = 13)
  })

  output$recomendacion_sim <- renderUI({
    p <- as.numeric(prob_sim())
    if (p >= 0.65) {
      tagList(
        p(strong("Recomendacion: "), "este perfil deberia priorizarse para
           rutas de intermediacion laboral, formacion tecnica intensiva o
           subsidios a la primera contratacion."),
        p("La probabilidad estimada de desempleo o informalidad para este
           joven es ", strong(percent(p, accuracy = 0.1)), ", superior al
           umbral de alta prioridad.")
      )
    } else if (p >= 0.4) {
      tagList(p(strong("Recomendacion: "), "este perfil presenta riesgo
        intermedio. Puede beneficiarse de orientacion vocacional o
        acompanamiento preventivo, sin requerir necesariamente
        intervencion intensiva inmediata."))
    } else {
      tagList(p(strong("Recomendacion: "), "este perfil presenta riesgo bajo
        de desempleo o informalidad bajo las condiciones actuales del
        modelo. No se sugiere priorizacion en programas de focalizacion."))
    }
  })

  # ---------------------------------------------------------------------------
  # PESTAÑA 6: PRIORIZACION
  # ---------------------------------------------------------------------------
  datos_priorizacion <- reactive({
    df <- datos
    if (input$prior_zona != "Todas") df <- df %>% filter(as.character(ZONA_lbl) == input$prior_zona)
    if (input$prior_sexo != "Todos") df <- df %>% filter(as.character(SEXO_lbl) == input$prior_sexo)
    df
  })

  priorizados <- reactive({
    datos_priorizacion() %>%
      filter(prob_riesgo >= input$prior_umbral) %>%
      arrange(desc(prob_riesgo)) %>%
      mutate(
        id_joven = row_number(),
        prob_riesgo_pct = percent(prob_riesgo, accuracy = 0.1)
      ) %>%
      select(id_joven, EDAD, SEXO_lbl, ZONA_lbl, NIVEL_EDUCATIVO,
             EXPERIENCIA_MESES, ESTRATO, prob_riesgo_pct, prob_riesgo)
  })

  output$kpi_priorizados <- renderText({ comma(nrow(priorizados())) })
  output$kpi_pct_priorizados <- renderText({
    base <- nrow(datos_priorizacion())
    if (base == 0) return("N/D")
    percent(nrow(priorizados()) / base, accuracy = 0.1)
  })
  output$kpi_prob_prom <- renderText({
    if (nrow(priorizados()) == 0) return("N/D")
    percent(mean(priorizados()$prob_riesgo), accuracy = 0.1)
  })

  output$tabla_priorizados <- renderDT({
    df <- priorizados() %>% select(-prob_riesgo)
    colnames(df) <- c("ID", "Edad", "Sexo", "Zona", "Nivel educativo (codigo)",
                       "Experiencia (meses)", "Estrato", "Prob. riesgo")
    datatable(df, rownames = FALSE, options = list(pageLength = 10))
  })

  output$descargar_priorizados <- downloadHandler(
    filename = function() paste0("jovenes_priorizados_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(priorizados() %>% select(-prob_riesgo), file, row.names = FALSE)
    }
  )

  # ---------------------------------------------------------------------------
  # PESTAÑA 7: MAPA TERRITORIAL
  # ---------------------------------------------------------------------------
  output$tabla_riesgo_dpto <- renderDT({
    tabla <- tasa_por_dpto %>%
      mutate(`Tasa de riesgo` = percent(tasa_riesgo, accuracy = 0.1)) %>%
      select(Departamento = departamento, `Tasa de riesgo`, `Jovenes en la muestra` = n_jovenes)
    datatable(tabla, rownames = FALSE, options = list(pageLength = 12, dom = "ftp"))
  })

  output$mapa_riesgo <- renderLeaflet({
    paleta <- colorNumeric(
      palette = c(PAL$verde_salvia, "#D9A441", PAL$terracota),
      domain = mapa_riesgo_dpto$tasa_riesgo,
      na.color = "#E5E1D6"
    )

    texto_tasa <- ifelse(is.na(mapa_riesgo_dpto$tasa_riesgo), "Sin dato",
                          percent(mapa_riesgo_dpto$tasa_riesgo, accuracy = 0.1))
    texto_n    <- ifelse(is.na(mapa_riesgo_dpto$n_jovenes), "Sin dato",
                          comma(mapa_riesgo_dpto$n_jovenes))

    texto_etiqueta <- paste0(
      "<strong>", mapa_riesgo_dpto$departamento, "</strong><br/>",
      "Tasa de riesgo laboral: ", texto_tasa, "<br/>",
      "Jovenes en la muestra: ", texto_n
    )
    etiquetas <- lapply(texto_etiqueta, htmltools::HTML)

    leaflet(mapa_riesgo_dpto) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~paleta(tasa_riesgo),
        weight = 1, color = "white", fillOpacity = 0.85,
        highlightOptions = highlightOptions(weight = 2, color = PAL$azul_profundo, bringToFront = TRUE),
        label = etiquetas,
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      addLegend(pal = paleta, values = ~tasa_riesgo, opacity = 0.85,
                title = "Tasa de riesgo", position = "bottomright")
  })
}

# =============================================================================
# LANZAR LA APLICACION
# =============================================================================
shinyApp(ui, server)
