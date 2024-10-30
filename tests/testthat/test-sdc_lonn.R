test_that("sdc_lonn m/long", {
  a <- sdclonn_data("syntetisk_5000")
  out2 <- sdc_lonn(a, between = ~yrke3 + (yrke2 + yrke1) * sektor3,  within = ~arb_heldeltid,
                   k1 = 80, k2 =85)
  expect_equal(sum(out2[["prikket_manedslonn_nedre_kvartil"]], na.rm = TRUE), 5691893.115)
  expect_equal(sum(out2[["prikket_antall_arbeidsforhold"]], na.rm = TRUE),  62744)
  expect_equal(sum(out2[["prikket_antall_heltidsekvivalenter"]], na.rm = TRUE),  48544.0954)
  
  long1 <- long_sdclonn(out2)
  expect_equal(dim(long1), c(9360L, 6L))
  expect_equal(sum(long1[["value"]], na.rm = TRUE), 56555989.8525784)
  
  long2 <- long_sdclonn(out2, prikket = FALSE)
  expect_equal(dim(long2), c(9360L, 6L))
  expect_equal(sum(long2[["value"]]), 105540427.392225)
  
  expect_equal(long1[1:5], long2[1:5]) 
  expect_equal(max(abs(long2[["value"]] - long1[["value"]]), na.rm = TRUE), 0)
  
  expect_equal(sum2(out2), 
               c(prikket_antall_arbeidsforhold = 153, prikket_antall_heltidsekvivalenter = 156, 
                 prikket_manedslonn_gjennomsnitt = 151, prikket_manedslonn_nedre_kvartil = 151, 
                 prikket_manedslonn_median = 151, prikket_manedslonn_ovre_kvartil = 151, 
                 prikket_avtalt_gjennomsnitt = 151, prikket_avtalt_nedre_kvartil = 151, 
                 prikket_avtalt_median = 151, prikket_avtalt_ovre_kvartil = 151, 
                 prikket_bonus_gjennomsnitt = 151, prikket_bonus_nedre_kvartil = 151, 
                 prikket_bonus_median = 151, prikket_bonus_ovre_kvartil = 151, 
                 prikket_uregtil_gjennomsnitt = 151, prikket_uregtil_nedre_kvartil = 151, 
                 prikket_uregtil_median = 151, prikket_uregtil_ovre_kvartil = 151, 
                 prikket_overtid_gjennomsnitt = 151, prikket_overtid_nedre_kvartil = 151, 
                 prikket_overtid_median = 151, prikket_overtid_ovre_kvartil = 151, 
                 krav_ikke_offentlig = 117, krav_dominant1_n = 12, krav_dominant2_n = 12, 
                 krav_dominant1_l = 12, krav_dominant2_l = 12, 
                 krav_dominant = 12, krav_n_within = 1, krav_istot = 104, krav_grenseverdi = 151, 
                 krav_grenseverdi2 = 2, 
                 krav_primary = 8, krav_primary2 = 5, krav_suppressed = 30, krav_suppressed2 = 10, 
                 krav_secondary = 22, krav_secondary2 = 8, krav_lonn = 151, krav_arbeidsforhold = 153, 
                 krav_ekvivalent = 156))
})

test_that("sdc_lonn div", {
  a <- sdclonn_data("syntetisk_5000")
  a$hundre <- 100
  out <- sdc_lonn(a, 
                  between = ~yrke3 + (yrke2 + yrke1) * sektor3, 
                  within = ~arb_heldeltid, 
                  var_vektet = c(v = "hundre"), 
                  var_uvektet = c(u = "hundre"), 
                  k1 = 80, k2 = 90)
  out <- out[out$antall_arbeidsforhold > 0, ]
  expect_equal(range(out$v_nedre_kvartil), c(100, 100))
  expect_equal(range(out$u_nedre_kvartil), c(100, 100))
  expect_equal(range(out$v_gjennomsnitt), c(100, 100))
  expect_equal(range(out$u_gjennomsnitt), c(100, 100))
  
  out6 <- sdc_lonn(a, formula = ~arb_fylke * yrke1 * arb_heldeltid * pers_kjoenn 
                   + arb_fylke * yrke1 * pers_kjoenn, # Overflødig linje, men skader ikke 
                   within = c("pers_kjoenn", "arb_heldeltid"), 
                   var_med_privat = "sektor3", 
                   dim_var_extra = c("yrke2", "pers_kommnr"), 
                   k1 = 80, k2 = 90)
  
  out7 <- sdc_lonn(a, formula = ~arb_fylke * (yrke1 + yrke2) * arb_heldeltid * pers_kjoenn, 
                   within = c("pers_kjoenn", "arb_heldeltid"), 
                   var_med_privat = "sektor3", 
                   dim_var_extra = c("yrke2", "pers_kommnr"), 
                   suppressed_data = out6,
                   k1 = 80, k2 = 90)
  expect_equal(c(sum(which(out7$krav_suppressed==1)), sum(which(out7$krav_suppressed2==1))), c(1311670L, 88944L))
  
  out7_ <- sdc_lonn(a, formula = ~arb_fylke * (yrke1 + yrke2) * arb_heldeltid * pers_kjoenn, 
                    within = c("pers_kjoenn", "arb_heldeltid"), 
                    var_med_privat = "sektor3", 
                    dim_var_extra = c("yrke2", "pers_kommnr"), 
                    suppressed_data = list(out6[seq(1, nrow(out6), 2), ], out6[seq(2, nrow(out6), 2), ]),
                    k1 = 80, k2 = 90)
  expect_equal(out7, out7_)
  
  
  out8 <- suppressWarnings(sdc_lonn(a, formula = ~(arb_fylke + pers_kommnr) * yrke1 * pers_kjoenn, 
                   within = "pers_kjoenn", 
                   var_med_privat = "sektor3", 
                   dim_var_extra = c("yrke2", "pers_kommnr"), 
                   suppressed_data = out6, 
                   k1 = 90, k2 = 95))
  expect_equal(c(sum(which(out8$krav_suppressed==1)), sum(which(out8$krav_suppressed2==1))), c(34451, 6344))
  
  out8_ <- suppressWarnings(sdc_lonn(a, formula = ~(arb_fylke + pers_kommnr) * yrke1 * pers_kjoenn, 
                   within = "pers_kjoenn", 
                   var_med_privat = "sektor3", 
                   dim_var_extra = c("yrke2", "pers_kommnr"), 
                   suppressed_data = list(out6[seq(1, nrow(out6), 2), ], out6[seq(2, nrow(out6), 2), ]),
                   k1 = 90, k2 = 95))
  
  expect_equal(out8, out8_)
  
  
  out9 <- sdc_lonn(a, formula = ~arb_fylke * yrke1 * arb_heldeltid * pers_kjoenn 
                   + arb_fylke * yrke1 * pers_kjoenn, # Overflødig linje, men skader ikke 
                   within = c("pers_kjoenn", "arb_heldeltid"), 
                   var_med_privat = "sektor3", 
                   dim_var_extra = c("yrke2", "pers_kommnr"), 
                   roundBase = 3, roundMultiple=2,
                   k1 = 95, k2 = 98)
  
  expect_equal(c(sum(which(out9$krav_suppressed==1)), sum(which(out9$krav_suppressed2==1))), c(0L, 133537L))
  
  
  
})

