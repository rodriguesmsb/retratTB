#to do later
plot_bar <- function(data){
  ggplot(data, aes(x = reorder(nome, value), y = Prop, fill = name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("#008000", "#ff0000")) +
  coord_flip() +
  theme_bw(base_size = 48) +
  labs(x = "", y = "") +
  theme(legend.position = "top")
}


create_bar_data <- function(data){
  
  bar_plot <- data %>% 
    mutate(outcome = ifelse(situa_ence == "Cure", "Favorable", "Unfavorable")) %>% 
    left_join(names) %>% 
    group_by(outcome, nome) %>% 
    summarise(cases = n()) %>% 
    spread(key = outcome, value = cases) %>% 
    mutate(total = Favorable + Unfavorable) %>% 
    mutate(Favorable = Favorable/total * 100,
           Unfavorable = Unfavorable/total * 100) %>% 
    pivot_longer(cols = c("Favorable", "Unfavorable"), values_to = "Prop") %>% 
    na.omit() %>% 
    mutate(value = ifelse(name == "Unfavorable", Prop, 0))
    return(bar_plot)
}


#create sankey plots
create_sankey_data <- function(data, population){

  sankey_data <- data %>% 
    dplyr::select(situa_ence) %>% 
    mutate(outcome = ifelse(situa_ence == "Cure","Favorable", "Unfavorable"),
           pop = population) %>% 
    mutate(situa_ence = ifelse(situa_ence %in% c("Transfer", "Diagnostic change", "Regimen switch"),
                            "Other", situa_ence))
  pop <- nrow(sankey_data)
  
  sankey_data <- sankey_data %>%
    make_long(pop, outcome, situa_ence)
  
  counts <- sankey_data %>% 
    dplyr::group_by(node) %>% 
    tally()
  
  counts <- counts %>%
    dplyr::group_by(node)%>%
    dplyr::mutate(pct = n/pop)
  
  sankey_data <- merge(sankey_data,
                       counts, 
                       by.x = 'node',
                       by.y = 'node',
                       all.x = TRUE)
  return(sankey_data)
}





plot_sankey <- function(data){
  ggplot(data,
       aes(x = x, 
           next_x = next_x, 
           node = node, 
           next_node = next_node,
           label = paste0(node," n=", n, ' (',  round(pct* 100,2), '%)' ),
           fill = as.factor(node))) + 
      geom_sankey(flow.alpha = 0.5,
                  node.color = "black",
                  show.legend = TRUE) +
      geom_sankey_label(size = 6, color = "black", fill= "white", hjust = -0.2) +
      theme_bw(base_size = 48) +
      theme(legend.position = "none") +
      theme(axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(), 
            panel.grid = element_blank()) +
      labs(fill = 'Nodes')
}


create_coef <- function(model){
  coef <- exp(model$coefficients)
  coef <- data.frame(coef)
  ci <- confint(model)
  ci <-  data.frame(ci)
  
  log_for <- tibble(lower = round(exp(ci$X2.5..),2),
                    upper = round(exp(ci$X97.5..),2),
                    mean = round(coef$coef,2),
                    study = rownames(coef))
  
  log_for <- log_for %>% 
    filter(study != "(Intercept)") %>% 
    mutate(feature = study,
           OR = as.character(mean),
           LB = as.character(lower),
           UB = as.character(upper))
  return(log_for)
}

plot_forest <- function(data){
  data |> 
      forestplot(labeltext = c(study, OR, LB, UB),
                 xlog = FALSE,
                 zero = 1,
                 boxsize = 0.2) |>
      fp_set_style(box = "royalblue",
                   line = "darkblue",
                   summary = "royalblue",
                   txt_gp = fpTxtGp(cex = 4))|>
      fp_add_header(study = c("Feature"),
                    OR = c("OR"),
                    LB = c("LB"),
                    UB = c("UB")) |>
      fp_set_zebra_style("#EFEFEF")
}

create_mult_ci <- function(multinomial_model){
  coefs <- data.frame(exp(coef(multinomial_model)))
  coefs <-  coefs %>% 
    mutate(model = rownames(coefs)) %>%
    pivot_longer(cols = -model) %>% 
    filter(name != "X.Intercept.")
  
  coefs <- coefs %>% 
    mutate(key = 1:nrow(coefs),
           mean = value)
  
  ci <- exp(confint(multinomial_model))
  ci <- data.frame(ci) %>% 
    filter(row_number() !=1)  
  
  coefs <- coefs %>% 
    add_column(lower = c(ci$X2.5...Death, ci$X2.5...Treatment.incomplete),
               upper = c(ci$X97.5...Death, ci$X97.5...Treatment.incomplete)) %>%
    mutate(feature = name,
           study = name,
           OR = as.character(round(mean,2)),
           LB = as.character(round(lower,2)),
           UB = as.character(round(upper,2)))
               

  return(coefs)
               
}

plot_survival <-  function(model){
  ggsurvplot(model,
             pval = TRUE, conf.int = TRUE,
             risk.table = TRUE, # Add risk table
             risk.table.col = "strata", # Change risk table color by groups
             linetype = "strata", # Change line type by groups
             surv.median.line = "hv", # Specify median survival
             ggtheme = theme_bw(base_size = 28), # Change ggplot2 theme
             palette = c("#E7B800", "#2E9FDF"))
}

compare_groups <- function(data, pop, name){
  data <- data %>% 
    filter(pop_vul %in% c(pop, "Non vul pop"))
    print(data)
    results <- compareGroups(pop_vul ~ forma + agravaids +  agravalcoo + agravdiabe + agravdoenc + agravtabac +
                           bacilosc_e + raiox_tora + histopatol + cultura_es + test_sensi + tratsup_at +
                           idade + cs_raca + cs_sexo + situa_ence + cs_escol_n,
                           include.miss = TRUE,
                           data = data)
   export2word(createTable(results, show.all = TRUE), file = paste0("../results/Table_",name,".docx"))
}





