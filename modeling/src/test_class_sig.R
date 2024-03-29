test_class_sig <- function(dataset, band){

  st <- shapiro_test(dataset, !!sym(band))
  
  if (st$p < 0.05) {
    krus <- kruskal_test(dataset,
                         {{sym(band)}} ~ class)
    if (krus$p < 0.05) {
      # indication that at least one group is statistically different. Next we 
      # have to do post-hoc testing. In this case, our post-hoc testing will
      # do multiple pairwise test via dunn and wilcox tests
      dt <- dunn_test(dataset,
                      formula = sym(band) ~ class,
                      p.adjust.method = "bonferroni") %>% 
        mutate(test = "dunn")
      wt <- wilcox_test(dataset,
                        formula = sym(band) ~ class,
                        p.adjust.method = "bonferroni") %>% 
        mutate(test = "wilcox")
      rbind(dt, wt)
    } else {
      message("Kruskal test did not detect significant differences between classes")
      
    }
  } else {
    # https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
    anov <- anova_test(dataset,
                       sym(band) ~ class)
    if (anov$p < 0.05) {
      dt <- dunn_test(dataset,
                      formula = sym(band) ~ class,
                      p.adjust.method = "bonferroni")
      wt <- wilcox_test(dataset,
                        formula = sym(band) ~ class,
                        p.adjust.method = "bonferroni") %>% 
        mutate(test = "wilcox")
      rbind(dt, wt)
    } else {
      message("anova did not detect significant differences between classes")
    }
    }
  }
}
