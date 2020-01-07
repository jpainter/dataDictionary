# search strings

malaria_search_words =  as.character( expression( malaria , paludisme, Pf, plasmodium , falciparum, vivax, RDT, TDR, rapid, slide, goutte ) ) %>% paste( collapse = ', ')
malaria_search_strings =  as.character( expression( palu, Pf, plasmodi , micro , rapid) ) %>% paste( collapse = ', ')

anc_iptp_search_words =  as.character( expression( ANC, CPN, IPT , TPI ) ) %>% paste( collapse = ', ')
anc_iptp_search_strings =  as.character( expression( ANC, CPN, IPT , TPI ) ) %>% paste( collapse = ', ')

attendance_search_words =  as.character( expression( attendance , patient, consultation , fever, fievre ) ) %>% paste( collapse = ', ')
attendance_search_strings =  as.character( expression( attend , consult ) ) %>% paste( collapse = ', ')

chw_search_words =  as.character( expression( imci, iccm, commun, CHW, chd, hsa,  village, VHW ) ) %>% paste( collapse = ', ')
chw_search_strings =  as.character( expression(  ) ) %>% paste( collapse = ', ')

stock_search_words =  as.character( expression( RDT, TDR,  ACT, ASAQ, AL, APT, SP, fansidar , itn, llin, milda, net ) ) %>% paste( collapse = ', ')
stock_search_strings =  as.character( expression( artem , lufen , pyr  ) ) %>% paste( collapse = ', ')

death_search_words =  "" # as.character( expression( mortality, death, décés ) ) %>% paste( collapse = ', ')
death_search_strings = "" # as.character( expression( mort, death, décé ) ) %>% paste( collapse = ', ')

population_search_words =  as.character( expression(  population  ) ) %>% paste( collapse = ', ')
population_search_strings =  as.character( expression(  pop ) ) %>% paste( collapse = ', ')

not_malaria_search_words = as.character( expression( bcg, ART, yellow, polio, rabies, rage, mening, LAL, plague, measles, bite, paralysis , cholera , trauma ) ) %>% paste( collapse = ', ')
not_malaria_search_strings = as.character( expression( TB, HIV , VIH, AIDS, SIDA, PMTCT, tuberc, malnut, typh, hemorr, lass, tetan, mening, diarr, cesar, urolo , amoxi , dentist, Bilharz, intestin) ) %>% paste( collapse = ', ')



hiv_search_words = as.character( expression( ART, HIV ) ) %>% paste( collapse = ', ') 
hiv_search_strings = as.character( expression( HIV , VIH, AIDS, SIDA, PMTCT ) ) %>% paste( collapse = ', ')

tb_search_words = as.character( expression( bcg, TB ) ) %>% paste( collapse = ', ')
tb_search_strings = as.character( expression( TB, tuberc ) ) %>% paste( collapse = ', ')

immunization_search_words = as.character( expression( polio, rabies, mening,  measles ) ) %>% paste( collapse = ', ')
immunization_search_strings = as.character( expression( vacc , immuni ) ) %>% paste( collapse = ', ')
