/******************************************
	PROJET: ANALYSE DE LA PAUVRETE 
	Premons Nom : Ndoasnan Armand DJEKONBE
	ENSAE-AS2|2024-2025
*/****************************************** 

/***************************
 Environnement de travail
*/**************************
clear all

* Charger la base individu depuis un lien Github
use "https://raw.githubusercontent.com/djekonbe1er/Analyse_Pauvrete/main/ehcvm_individu_SEN2018.dta", replace

* Création des variables de scénarios
  capture drop bebe
  gen bebe =cond(age <= 2,1,0)
  capture drop under5
  gen under5=cond(age<=5,1,0)
  capture drop under18
  gen under18=cond(age<18,1,0)
  capture drop elder
  gen elder=cond(age>65,1,0)
  capture drop handicap
  gen handicap=cond(handit==1,1,0)

* Garder uniquement ces variables pour le merging avec la base welfare(lien Github)
  keep hhid bebe under5 under18 elder handicap
*
  merge m:1 hhid using "https://raw.githubusercontent.com/djekonbe1er/Analyse_Pauvrete/main/ehcvm_welfare_SEN2018.dta"
  drop _merge
* Nouveau revenu
 capture drop pcexp_2018
 gen pcexp_2018 = dtot / (hhsize * def_spa * def_temp)
 
/*******************************************************
	AGING DE LA BASE 2018 EN BASE 2023
    Taux d'accroisement de la pop 2023 =0,153
	Taux d'accroisement du PIB = 0,258
	Taux d'inflation 2023 = 0,059
*/******************************************************
* Population 
   capture drop hhweight_2023
   gen hhweight_2023= 1.153*hhweight
*  Inflation
   capture drop zref_2023
   gen zref_2023= 1.059*zref
*** 3. Revenu PIB
    capture drop pcexp_2023
    gen pcexp_2023= 1.258 * pcexp_2018	
* Sauvegarde de la base d'analyse complete(2018|2023|Scénario)
  save Base_Analyse_complete.dta, replace 
/**************************
	ANALYSE A L'ANNEE 2018 
*/**************************  
  use "Base_Analyse_complete.dta", replace
* Enregistrement du fichier Excel
  /*putexcel set Résultat.xlsx, replace
* En-têtes de colonnes 
  putexcel A1 = "Scénarios" 
  putexcel B1 = "Ref_2018"
  putexcel C1 = "Ref_2023"   
  putexcel D1 = "Transf. universel"
  putexcel E1 = "Transf. rural"   
  putexcel F1 = "Moins de 2 ans"
  putexcel G1 = "Moins 2 ans rural" 
  putexcel H1 = "Moins de 5 ans"
  putexcel I1 = "Moins de 18 ans"
  putexcel J1 = "≥ 65 ans"  
  putexcel K1 = "Handicap"     
* En-têtes de lignes *
  putexcel A2 = "Incidence (P0)"
  putexcel A3 = "Profondeur (Gap)"   
  putexcel A4 = "Sévérité (Gap2)"
  putexcel A5 = "Indice de Gini"
 */

* Pondération individuelle pour 2018
 capture drop indweight_2018
 gen indweight_2018 = hhweight

** Calcul des indicateurs
* Identification des pauvres | Incidence de la pauvreté
capture drop pov_2018
gen pov_2018 = cond(pcexp_2018 < zref, 1, 0)
summarize pov_2018 [iw=indweight_2018]
scalar P0_2018= round(100 * r(mean), .01)
* Les Gaps : Gap | Gap2
capture drop gap_2018
gen gap_2018 = pov_2018 * ((zref - pcexp_2018) / zref)
summarize gap_2018 [iw=indweight_2018]
scalar gap_2018 = round(100 * r(mean), .01)

capture drop gap2_2018
gen gap2_2018 = pov_2018 * (((zref - pcexp_2018) / zref)^2)
summarize gap2_2018 [iw=indweight_2018]
scalar gap2_2018 = round(100 * r(mean), .01)

** Indice de GINI
capture drop totw toty wf tf Y area
* Calcul des totaux et parts
egen totw = total(indweight_2018)
egen toty = total(indweight_2018 * pcexp_2018)
gen wf = indweight_2018 / totw
gen tf = (indweight_2018 * pcexp_2018) / toty

* Trier et calculer le cumul de revenu
sort pcexp_2018
gen Y = sum(tf)

* Calcul direct de l'aire sous la courbe de Lorenz
gen area = (Y + Y[_n-1]) * wf / 2
replace area = Y * wf / 2 in 1

* Somme des aires et indice de Gini
summarize area
scalar A = r(sum)
scalar Gini_2018 = 1 - 2 * A
local gini2018 = round(100 * Gini_2018, .01)
display " l'indice de Gini 2018=" `gini2018'

* Calcul optimisé de l'indice de Gini par milieu 
capture program drop gini_by_milieu
program define gini_by_milieu
    args year weightvar pcexpvar

    foreach m in 1 2 {
        preserve
        keep if milieu == `m'
        capture drop totw toty wf tf Y area
        egen totw = total(`weightvar')
        egen toty = total(`weightvar' * `pcexpvar')
        gen wf = `weightvar' / totw
        gen tf = (`weightvar' * `pcexpvar') / toty
        sort `pcexpvar'
        gen Y = sum(tf)
        gen area = (Y + Y[_n-1]) * wf / 2
        replace area = Y * wf / 2 in 1
        summarize area, meanonly
        scalar A = r(sum)
        scalar Gini_milieu = 1 - 2 * A
        local gini = round(100 * Gini_milieu, 0.01)
        local label = cond(`m'==1, "Urbain", "Rural")
        display "Indice de Gini `year' - `label' : " `gini'
        restore
    }
end

* Indice de Gini par milieu 2018 
gini_by_milieu 2018 indweight_2018 pcexp_2018

/*************************************************
	ANALYSE A L'ANNEE 2023: SIUATION DE REFERENCE
*/************************************************
* Pondération 2023
  capture drop indweight_2023
  gen indweight_2023=hhweight_2023*hhsize
** Calcul des indicateurs
* Identification des pauvres| Incidence de la pauvreté
  capture drop pov_2023
  gen pov_2023 = cond(pcexp_2023 < zref_2023,1,0)
  summarize pov_2023[iw= indweight_2023]
  scalar P0_2023= round(100 * r(mean), .01)
  display "P0 de 2023=" `P0_2023' 

* Les Gaps: Gap | Gap2
  capture drop gap_2023
  gen gap_2023  = pov_2023 * ((zref_2023 - pcexp_2023) / zref_2023)
  summarize gap_2023 [iw=indweight_2023]
  scalar gap_2023= round(100 * r(mean), .01)
  display "Gap de 2023=" `gap_2023'
 
  capture drop gap2_2023
  gen gap2_2023 = pov_2023* (((zref_2023 - pcexp_2023) / zref_2023)^2)
  summarize gap2_2023 [iw=indweight_2023]
  scalar gap2_2023= round(100 * r(mean), .01)
  display "Gap2 de 2023=" `gap2_2023'
  
** Indice de GINI
  capture drop totw toty wf tf Y area 
* Calcul des totaux et parts
  egen totw = total(indweight_2023)
  egen toty = total(indweight_2023 * pcexp_2023)
  gen wf = indweight_2023 / totw                    
  gen tf = (indweight_2023 * pcexp_2023) / toty     
* Trier et calculer le cumul de revenu
  sort pcexp_2023
  gen Y = sum(tf)
* Calcul direct de l'aire sous la courbe de Lorenz
  gen area = (Y + Y[_n-1]) * wf / 2
  replace area = Y * wf / 2 in 1             
* Somme des aires et indice de Gini
  summarize area
  scalar A = r(sum)
  scalar Gini_2023= 1 - 2 * A
  local gini2023 = round(100*Gini_2023,.01)
  display "Indice de Gini 2023=" `gini2023'
 
* Indice de Gini par milieu 2023
 gini_by_milieu 2023 indweight_2023 pcexp_2023

/*****************************************************
	ANALYSE DES SCENARIOS
	PIB en 2023: 1827176000000000  FCFA
*/****************************************************
  scalar pib_2023 = 1827176000000000 

/**********************************
 Scénario 1 : Transfert universel 
*/********************************
* Tranfert de 100 000 FCFA à tous les ménages
  capture drop transfert1
  gen transfert1= 100000
  capture drop pcexp_S1
  gen pcexp_S1=(pcexp_2023 + transfert1/(hhweight_2023*def_spa*def_temp))
* Identification des pauvres| P0
  capture drop pauvre_S1
  gen pauvre_S1 = cond(pcexp_S1 < zref_2023,1,0)
  summarize pauvre_S1[iw=indweight_2023]
  scalar P0_S1= round(100 * r(mean), .01)
  display "Le P0 du Scénario 1=" `P0_S1' 
****** Les Gaps: Gap | Gap2  ********************************
* Gap
  capture drop gap_S1
  gen gap_S1  = pauvre_S1 * ((zref_2023 - pcexp_S1) / zref_2023)
  summarize gap_S1 [iw=indweight_2023]
  scalar gap_S1= round(100 * r(mean), .01)
  display "Le Gap du Scénario 1=" `gap_S1'    
* Gap2
  capture drop gap2_S1
  gen gap2_S1 = pauvre_S1*(((zref_2023 - pcexp_S1) / zref_2023)^2)
  summarize gap2_S1 [iw=indweight_2023]
  scalar gap2_S1= round(100 * r(mean), .01)
  display "Le Gap2 du Scénario 1=" `gap2_S1'
 
********* Indice de GINI *****************************
  capture drop totw toty wf tf Y area 
* Calcul des totaux et parts
  egen totw = total(indweight_2023)
  egen toty = total(indweight_2023 * pcexp_S1)
  gen wf = indweight_2023 / totw                    
  gen tf = (indweight_2023 * pcexp_S1) / toty     

* Trier et calculer le cumul de revenu
  sort pcexp_S1
  gen Y = sum(tf)

* Calcul direct de l'aire sous la courbe de Lorenz
  gen area = (Y + Y[_n-1]) * wf / 2
  replace area = Y * wf / 2 in 1             
1827176 
* Somme des aires et indice de Gini
  summarize area
  scalar A = r(sum)
  scalar Gini_S1= 1 - 2 * A
  local giniS1 = round(100*Gini_S1,.01)
  display "Indice de Gini du Scénario 1=" `giniS1'
* Indice de Gini par milieu du Scénario 1
  gini_by_milieu 2023_S1 indweight_2023 pcexp_S1 
**********  Couts du Scénario: Absolu|PIB  *************
  capture drop cout_abs_S1
  gen cout_abs_S1=transfert1*indweight_2023
  display "le cout absolu du Scénario 1="  cout_abs_S1
  capture drop cout_PIB_S1
  gen cout_PIB_S1=(cout_abs_S1/pib_2023)*1000000
  display "le cout rapporté au PIB du Scénario 1="  cout_PIB_S1
**********    Efficacité  ************************** 
  capture drop effc_S1
  gen effc_S1 = (gap_2023 - gap_S1) /cout_abs_S1
  display "L'efficacité du Scénario 1="  effc_S1 
/**********************************************
Contribution à la réduction des indicateurs FGT 
*/**********************************************
* Calcul du FGT selon les milieux 
capture program drop fgt_by_milieu
program define fgt_by_milieu
    args scenario pcexpvar weightvar zrefvar
    foreach m in 1 2 {
        preserve
        keep if milieu == `m'
        gen pauvre = `pcexpvar' < `zrefvar'
        gen gap = pauvre * ((`zrefvar' - `pcexpvar') / `zrefvar')
        gen gap2 = pauvre * ((`zrefvar' - `pcexpvar') / `zrefvar')^2
        summarize pauvre [iw=`weightvar'], meanonly
        scalar P0_`scenario'_`m' = 100 * r(mean)
        summarize gap [iw=`weightvar'], meanonly
        scalar P1_`scenario'_`m' = 100 * r(mean)
        summarize gap2 [iw=`weightvar'], meanonly
        scalar P2_`scenario'_`m' = 100 * r(mean)
        restore
    }
end
** Application Baseline 2023
*  Milieu Urbain (milieu=1)
 fgt_by_milieu 2023 pcexp_2023 indweight_2023 zref_2023
 display P0_2023_1  P1_2023_1  P2_2023_1
*  Milieu Urbain (milieu=2)
  display P0_2023_2  P1_2023_2  P2_2023_2
** Application au Scénario 1
  fgt_by_milieu S1 pcexp_S1 indweight_2023 zref_2023
*  Milieu Urbain (milieu=1)
   display P0_S1_1  P1_S1_1  P2_S1_1
*  Milieu Urbain (milieu=2)
  display P0_S1_2  P1_S1_2  P2_S1_2
* Gain zone urbaine
scalar delta_P0_S1_1 = P0_2023_1 - P0_S1_1
scalar delta_P1_S1_1 = P1_2023_1 - P1_S1_1
scalar delta_P2_S1_1 = P2_2023_1 - P2_S1_1
display delta_P0_S1_1  delta_P1_S1_1  delta_P2_S1_1
* Gain zone rurale
scalar delta_P0_S1_2 = P0_2023_2 - P0_S1_2
scalar delta_P1_S1_2 = P1_2023_2 - P1_S1_2
scalar delta_P2_S1_2 = P2_2023_2 - P2_S1_2
display delta_P0_S1_2  delta_P1_S1_2  delta_P2_S1_2

*******  SCENARIO 2: Transfert universel rural  *********************   
* Tranfert de 100 000 FCFA à tous les ménages ruraux
  capture drop transfert2
  gen transfert2 = 100000* (milieu == 2)
  capture drop pcexp_S2
  gen pcexp_S2=(pcexp_2023 + transfert2/(hhweight_2023*def_spa*def_temp))
 
* Identification des pauvres| P0
  capture drop pauvre_S2
  gen pauvre_S2 = cond(pcexp_S2 < zref_2023,1,0)
  summarize pauvre_S2[iw=indweight_2023]
  scalar P0_S2= round(100 * r(mean), .01)
  display "Le P0 du Scénario 2=" `P0_S2'
* Les Gaps: Gap | Gap2
  capture drop gap_S2
  gen gap_S2  = pauvre_S2 * ((zref_2023 - pcexp_S2) / zref_2023)
  summarize gap_S2 [iw=indweight_2023]
  scalar gap_S2= round(100 * r(mean), .01)
  display "Le Gap du Scénario 2=" `gap_S2' 
  
  capture drop gap2_S2
  gen gap2_S2 = pauvre_S2*(((zref_2023 - pcexp_S2) / zref_2023)^2)
  summarize gap2_S2 [iw=indweight_2023]
  scalar gap2_S2= round(100 * r(mean), .01)
  display "Le Gap2 du Scénario 2=" `gap2_S2'

** Indice de GINI
  capture drop totw toty wf tf Y area 
* Calcul des totaux et parts
  egen totw = total(indweight_2023)
  egen toty = total(indweight_2023 * pcexp_S2)
  gen wf = indweight_2023 / totw                    
  gen tf = (indweight_2023 * pcexp_S2) / toty     

* Trier et calculer le cumul de revenu
  sort pcexp_S2
  gen Y = sum(tf)

* Calcul direct de l'aire sous la courbe de Lorenz
  gen area = (Y + Y[_n-1]) * wf / 2
  replace area = Y * wf / 2 in 1             

* Somme des aires et indice de Gini
  summarize area
  scalar A = r(sum)
  scalar Gini_S2= 1 - 2 * A
  local giniS2 = round(100*Gini_S2,.01)
  display "Indice de Gini du Scénario 2=" `giniS2'
* Indice de Gini par milieu du Scénario 2
  gini_by_milieu 2023_S2 indweight_2023 pcexp_S2  
**********  Couts du Scénario 2: Absolu|PIB  *************
  capture drop cout_abs_S2
  gen cout_abs_S2=transfert2*indweight_2023
  display "le cout absolu du Scénario 2="  cout_abs_S2
  capture drop cout_PIB_S2
  gen cout_PIB_S2=(cout_abs_S2/pib_2023)*1000000
  display "le cout rapporté au PIB du Scénario 2="  cout_PIB_S2
**********    Efficacité  ************************** 
  capture drop effc_S2
  gen effc_S2 = (gap_2023 - gap_S2) /cout_abs_S2
  display "L'efficacité du Scénario 1="  effc_S2 
/**********************************************
Contribution à la réduction des indicateurs FGT 
*/**********************************************
* Calcul du FGT selon les milieux 
  fgt_by_milieu S2 pcexp_S2 indweight_2023 zref_2023
*  Milieu Urbain (milieu=1)
   display P0_S2_1  P1_S2_1  P2_S2_1
*  Milieu Urbain (milieu=2)
  display P0_S2_2  P1_S2_2  P2_S2_2
* Gain zone urbaine
scalar delta_P0_S2_1 = P0_2023_1 - P0_S2_1
scalar delta_P1_S2_1 = P1_2023_1 - P1_S2_1
scalar delta_P2_S2_1 = P2_2023_1 - P2_S2_1
display delta_P0_S2_1  delta_P1_S2_1  delta_P2_S2_1
* Gain zone rurale
scalar delta_P0_S2_2 = P0_2023_2 - P0_S2_2
scalar delta_P1_S2_2 = P1_2023_2 - P1_S2_2
scalar delta_P2_S2_2 = P2_2023_2 - P2_S2_2
display delta_P0_S2_2  delta_P1_S2_2  delta_P2_S2_2

/******************************
* Scénario 3 : Enfants ≤ 2 ans
*/*****************************
* Tranfert de 100 000 FCFA à tous les ménages ayant les enfants de moins de 2ans
  capture drop menage_bebe
  bysort hhid: egen menage_bebe = max(bebe)
  capture drop transfert3
  gen transfert3 = 100000* menage_bebe
  capture drop pcexp_S3
  gen pcexp_S3=(pcexp_2023 + transfert3/(hhweight_2023*def_spa*def_temp))
 
* 1. Identification des pauvres| P0
  capture drop pauvre_S3
  gen pauvre_S3 = cond(pcexp_S3 < zref_2023,1,0)
  summarize pauvre_S3[iw=indweight_2023]
  scalar P0_S3= round(100 * r(mean), .01)
  display "Le P0 du Scénario 3=" `P0_S3'
* Les Gaps: Gap | Gap2
  capture drop gap_S3
  gen gap_S3  = pauvre_S3 * ((zref_2023 - pcexp_S3) / zref_2023)
  summarize gap_S3 [iw=indweight_2023]
  scalar gap_S3= round(100 * r(mean), .01)
  display "Le Gap du Scénario 3=" `gap_S3'  
 
  capture drop gap2_S3
  gen gap2_S3 = pauvre_S3*(((zref_2023 - pcexp_S3) / zref_2023)^2)
  summarize gap2_S3 [iw=indweight_2023]
  scalar gap2_S3= round(100 * r(mean), .01)
  display "Le Gap2 du Scénario 3=" `gap2_S3'
** Indice de GINI
  capture drop totw toty wf tf Y area 
* Calcul des totaux et parts
  egen totw = total(indweight_2023)
  egen toty = total(indweight_2023 * pcexp_S3)
  gen wf = indweight_2023 / totw                    
  gen tf = (indweight_2023 * pcexp_S3) / toty     
* Trier et calculer le cumul de revenu
  sort pcexp_S3
  gen Y = sum(tf)
* Calcul direct de l'aire sous la courbe de Lorenz
  gen area = (Y + Y[_n-1]) * wf / 2
  replace area = Y * wf / 2 in 1             
* Somme des aires et indice de Gini
  summarize area
  scalar A = r(sum)
  scalar Gini_S3= 1 - 2 * A
  local giniS3 = round(100*Gini_S3,.01)
  display "Indice de Gini du Scénario 1=" `giniS3'
* Indice de Gini par milieu du Scénario 3
  gini_by_milieu 2023_S3 indweight_2023 pcexp_S3  
**********  Couts du Scénario 3: Absolu|PIB  *************
  capture drop cout_abs_S3
  gen cout_abs_S3=transfert3*indweight_2023
  display "le cout absolu du Scénario 3="  cout_abs_S3
  capture drop cout_PIB_S3
  gen cout_PIB_S3=(cout_abs_S3/pib_2023)*1000000
  display "le cout rapporté au PIB du Scénario 3="  cout_PIB_S3
**********    Efficacité  ************************** 
  capture drop effc_S3
  gen effc_S3 = (gap_2023 - gap_S3) /cout_abs_S3
  display "L'efficacité du Scénario 1="  effc_S3 
/**********************************************
Contribution à la réduction des indicateurs FGT 
*/**********************************************
* Calcul du FGT selon les milieux 
  fgt_by_milieu S3 pcexp_S3 indweight_2023 zref_2023
*  Milieu Urbain (milieu=1)
   display P0_S3_1  P1_S3_1  P2_S3_1
*  Milieu Urbain (milieu=2)
  display P0_S3_2  P1_S3_2  P2_S3_2
* Gain zone urbaine
scalar delta_P0_S3_1 = P0_2023_1 - P0_S3_1
scalar delta_P1_S3_1 = P1_2023_1 - P1_S3_1
scalar delta_P2_S3_1 = P2_2023_1 - P2_S3_1
display delta_P0_S3_1  delta_P1_S3_1  delta_P2_S3_1
* Gain zone rurale
scalar delta_P0_S3_2 = P0_2023_2 - P0_S3_2
scalar delta_P1_S3_2 = P1_2023_2 - P1_S3_2
scalar delta_P2_S3_2 = P2_2023_2 - P2_S3_2
display delta_P0_S3_2  delta_P1_S3_2  delta_P2_S3_2
    
/**********************************************
 * Scénario 4 : Enfant ≤ 2 ans en milieu rural
*/*********************************************
* Tranfert de 100 000 FCFA à tous les ménages ayant les enfants de moins de 2ans vivant dans le milieu rural
  capture drop menage_bebe_rural
  bysort hhid: egen menage_bebe_rural = max(bebe)
  replace menage_bebe_rural = 0 if milieu == 1  
  capture drop transfert4
  gen transfert4 = 100000* menage_bebe
  capture drop pcexp_S4
  gen pcexp_S4=(pcexp_2023 + transfert4/(hhweight_2023*def_spa*def_temp))

* Identification des pauvres| P0
  capture drop pauvre_S4
  gen pauvre_S4 = cond(pcexp_S4 < zref_2023,1,0)
  summarize pauvre_S4[iw=indweight_2023]
  scalar P0_S4= round(100 * r(mean), .01)
  display "Le P0 du Scénario 2=" `P0_S4'

* Les Gaps: Gap | Gap2
  capture drop gap_S4
  gen gap_S4  = pauvre_S4 * ((zref_2023 - pcexp_S4) / zref_2023)
  summarize gap_S4 [iw=indweight_2023]
  scalar gap_S4= round(100 * r(mean), .01)
  display "Le Gap du Scénario 4=" `gap_S4'
  
  capture drop gap2_S4
  gen gap2_S4 = pauvre_S4*(((zref_2023 - pcexp_S4) / zref_2023)^2)
  summarize gap2_S4 [iw=indweight_2023]
  scalar gap2_S4= round(100 * r(mean), .01)
  display "Le Gap2 du Scénario 4=" `gap2_S4'
 
** Indice de GINI
  capture drop totw toty wf tf Y area 
* Calcul des totaux et parts
  egen totw = total(indweight_2023)
  egen toty = total(indweight_2023 * pcexp_S4)
  gen wf = indweight_2023 / totw                    
  gen tf = (indweight_2023 * pcexp_S4) / toty     

* Trier et calculer le cumul de revenu
  sort pcexp_S4
  gen Y = sum(tf)

* Calcul direct de l'aire sous la courbe de Lorenz
  gen area = (Y + Y[_n-1]) * wf / 2
  replace area = Y * wf / 2 in 1             

* Somme des aires et indice de Gini
  summarize area
  scalar A = r(sum)
  scalar Gini_S4= 1 - 2 * A
  local giniS4 = round(100*Gini_S4,.01)
  display "Indice de Gini du Scénario 1=" `giniS4'

* Indice de Gini par milieu du Scénario 4
  gini_by_milieu 2023_S4 indweight_2023 pcexp_S4 
**********  Couts du Scénario 2: Absolu|PIB  *************
  capture drop cout_abs_S4
  gen cout_abs_S4=transfert4*indweight_2023
  display "le cout absolu du Scénario 4="  cout_abs_S4
  capture drop cout_PIB_S4
  gen cout_PIB_S4=(cout_abs_S4/pib_2023)*1000000
  display "le cout rapporté au PIB du Scénario 4="  cout_PIB_S4
**********    Efficacité  ************************** 
  capture drop effc_S2
  gen effc_S4 = (gap_2023 - gap_S4) /cout_abs_S4
  display "L'efficacité du Scénario 1="  effc_S4 
/**********************************************
Contribution à la réduction des indicateurs FGT 
**********************************************/
* Calcul du FGT selon les milieux 
fgt_by_milieu S4 pcexp_S4 indweight_2023 zref_2023

*  Milieu Urbain (milieu=1)
display P0_S4_1  P1_S4_1  P2_S4_1

*  Milieu Rural (milieu=2)
display P0_S4_2  P1_S4_2  P2_S4_2

* Gain zone urbaine
scalar delta_P0_S4_1 = P0_2023_1 - P0_S4_1
scalar delta_P1_S4_1 = P1_2023_1 - P1_S4_1
scalar delta_P2_S4_1 = P2_2023_1 - P2_S4_1
display delta_P0_S4_1  delta_P1_S4_1  delta_P2_S4_1

* Gain zone rurale
scalar delta_P0_S4_2 = P0_2023_2 - P0_S4_2
scalar delta_P1_S4_2 = P1_2023_2 - P1_S4_2
scalar delta_P2_S4_2 = P2_2023_2 - P2_S4_2
display delta_P0_S4_2  delta_P1_S4_2  delta_P2_S4_2

/********************************
 * Scénario 5 :  Enfants ≤ 5 ans
*/*******************************
* Tranfert de 100 000 FCFA à tous les ménages ayant les enfants de moins de 5ans 
  capture drop menage_under5
  bysort hhid: egen menage_under5 = max(under5)  
  capture drop transfert5
  gen transfert5 = 100000* menage_under5
  capture drop pcexp_S5
  gen pcexp_S5=(pcexp_2023 + transfert5/(hhweight_2023*def_spa*def_temp))
* Identification des pauvres| P0
  capture drop pauvre_S5
  gen pauvre_S5 = cond(pcexp_S5 < zref_2023,1,0)
  summarize pauvre_S5[iw=indweight_2023]
  scalar P0_S5= round(100 * r(mean), .01)
  display "Le P0 du Scénario 5=" `P0_S5'

* Les Gaps: Gap | Gap2
  capture drop gap_S5
  gen gap_S5  = pauvre_S5 * ((zref_2023 - pcexp_S5) / zref_2023)
  summarize gap_S5 [iw=indweight_2023]
  scalar gap_S5= round(100 * r(mean), .01)
  display "Le Gap du Scénario 5=" `gap_S5' 
  
  capture drop gap2_S5
  gen gap2_S5 = pauvre_S5*(((zref_2023 - pcexp_S5) / zref_2023)^2)
  summarize gap2_S5 [iw=indweight_2023]
  scalar gap2_S5= round(100 * r(mean), .01)
  display "Le Gap2 du Scénario 5=" `gap2_S5'

** Indice de GINI
  capture drop totw toty wf tf Y area 
* Calcul des totaux et parts
  egen totw = total(indweight_2023)
  egen toty = total(indweight_2023 * pcexp_S5)
  gen wf = indweight_2023 / totw                    
  gen tf = (indweight_2023 * pcexp_S5) / toty     

* Trier et calculer le cumul de revenu
  sort pcexp_S5
  gen Y = sum(tf)

* Calcul direct de l'aire sous la courbe de Lorenz
  gen area = (Y + Y[_n-1]) * wf / 2
  replace area = Y * wf / 2 in 1             

* Somme des aires et indice de Gini
  summarize area
  scalar A = r(sum)
  scalar Gini_S5= 1 - 2 * A
  local giniS5 = round(100*Gini_S5,.01)
  display "Indice de Gini du Scénario 1=" `giniS5'
* Indice de Gini par milieu du Scénario 5
gini_by_milieu 2023_S5 indweight_2023 pcexp_S5  

**********  Coûts du Scénario 5 : Absolu | PIB  *************
capture drop cout_abs_S5
gen cout_abs_S5 = transfert5 * indweight_2023
display "le coût absolu du Scénario 5 = " cout_abs_S5

capture drop cout_PIB_S5
gen cout_PIB_S5 = (cout_abs_S5 / pib_2023) * 1000000
display "le coût rapporté au PIB du Scénario 5 = " cout_PIB_S5

**********    Efficacité  ************************** 
capture drop effc_S5
gen effc_S5 = (gap_2023 - gap_S5) / cout_abs_S5
display "L'efficacité du Scénario 5 = " effc_S5

/**********************************************
Contribution à la réduction des indicateurs FGT 
**********************************************/
* Calcul du FGT selon les milieux 
fgt_by_milieu S5 pcexp_S5 indweight_2023 zref_2023

*  Milieu Urbain (milieu=1)
display P0_S5_1  P1_S5_1  P2_S5_1

*  Milieu Rural (milieu=2)
display P0_S5_2  P1_S5_2  P2_S5_2

* Gain zone urbaine
scalar delta_P0_S5_1 = P0_2023_1 - P0_S5_1
scalar delta_P1_S5_1 = P1_2023_1 - P1_S5_1
scalar delta_P2_S5_1 = P2_2023_1 - P2_S5_1
display delta_P0_S5_1  delta_P1_S5_1  delta_P2_S5_1

* Gain zone rurale
scalar delta_P0_S5_2 = P0_2023_2 - P0_S5_2
scalar delta_P1_S5_2 = P1_2023_2 - P1_S5_2
scalar delta_P2_S5_2 = P2_2023_2 - P2_S5_2
display delta_P0_S5_2  delta_P1_S5_2  delta_P2_S5_2
  

/*******************************************
 * Scénario 6 : Ménages avec enfant < 18 ans
*/******************************************
* Tranfert de 100 000 FCFA à tous les ménages ayant les enfants de moins de 18ans 
  capture drop menage_under18
  bysort hhid: egen menage_under18 = max(under18)  
  capture drop transfert6
  gen transfert6 = 100000 * menage_under18
  capture drop pcexp_S6
  gen pcexp_S6=(pcexp_2023 + transfert6/(hhweight_2023*def_spa*def_temp))
* Identification des pauvres| P0
  capture drop pauvre_S6
  gen pauvre_S6= cond(pcexp_S6 < zref_2023,1,0)
  summarize pauvre_S6[iw=indweight_2023]
  scalar P0_S6= round(100 * r(mean), .01)
  display "Le P0 du Scénario 6=" `P0_S6'

* Les Gaps: Gap | Gap2
  capture drop gap_S6
  gen gap_S6  = pauvre_S6 * ((zref_2023 - pcexp_S6) / zref_2023)
  summarize gap_S6 [iw=indweight_2023]
  scalar gap_S6= round(100 * r(mean), .01)
  display "Le Gap du Scénario 6=" `gap_S6' 
  
  capture drop gap2_S6
  gen gap2_S6 = pauvre_S6*(((zref_2023 - pcexp_S6) / zref_2023)^2)
  summarize gap2_S6 [iw=indweight_2023]
  scalar gap2_S6= round(100 * r(mean), .01)
  display "Le Gap du Scénario 6=" `gap2_S6' 
** Indice de GINI
  capture drop totw toty wf tf Y area 
* Calcul des totaux et parts
  egen totw = total(indweight_2023)
  egen toty = total(indweight_2023 * pcexp_S6)
  gen wf = indweight_2023 / totw                    
  gen tf = (indweight_2023 * pcexp_S6) / toty     

* Trier et calculer le cumul de revenu
  sort pcexp_S6
  gen Y = sum(tf)

* Calcul direct de l'aire sous la courbe de Lorenz
  gen area = (Y + Y[_n-1]) * wf / 2
  replace area = Y * wf / 2 in 1             

* Somme des aires et indice de Gini
  summarize area
  scalar A = r(sum)
  scalar Gini_S6= 1 - 2 * A
  local giniS6 = round(100*Gini_S6,.01)
  display "Indice de Gini du Scénario 1=" `giniS6'
* Indice de Gini par milieu du Scénario 6
gini_by_milieu 2023_S6 indweight_2023 pcexp_S6  

**********  Coûts du Scénario 6 : Absolu | PIB  *************
capture drop cout_abs_S6
gen cout_abs_S6 = transfert6 * indweight_2023
display "le coût absolu du Scénario 6 = " cout_abs_S6

capture drop cout_PIB_S6
gen cout_PIB_S6 = (cout_abs_S6 / pib_2023) * 1000000
display "le coût rapporté au PIB du Scénario 6 = " cout_PIB_S6

**********    Efficacité  ************************** 
capture drop effc_S6
gen effc_S6 = (gap_2023 - gap_S6) / cout_abs_S6
display "L'efficacité du Scénario 6 = " effc_S6

/**********************************************
Contribution à la réduction des indicateurs FGT 
**********************************************/
* Calcul du FGT selon les milieux 
fgt_by_milieu S6 pcexp_S6 indweight_2023 zref_2023

*  Milieu Urbain (milieu=1)
display P0_S6_1  P1_S6_1  P2_S6_1

*  Milieu Rural (milieu=2)
display P0_S6_2  P1_S6_2  P2_S6_2

* Gain zone urbaine
scalar delta_P0_S6_1 = P0_2023_1 - P0_S6_1
scalar delta_P1_S6_1 = P1_2023_1 - P1_S6_1
scalar delta_P2_S6_1 = P2_2023_1 - P2_S6_1
display delta_P0_S6_1  delta_P1_S6_1  delta_P2_S6_1

* Gain zone rurale
scalar delta_P0_S6_2 = P0_2023_2 - P0_S6_2
scalar delta_P1_S6_2 = P1_2023_2 - P1_S6_2
scalar delta_P2_S6_2 = P2_2023_2 - P2_S6_2
display delta_P0_S6_2  delta_P1_S6_2  delta_P2_S6_2

***********************************************
* Scénario 7: Ménages avec personne âgée > 65 ans
**********************************************
* Tranfert de 100 000 FCFA à tous les ménages ayant des individus de plus de 65 ans 
  capture drop menage_elder
  bysort hhid: egen menage_elder = max(elder) 
  capture drop transfert7
  gen transfert7 = 100000 * menage_elder
  capture drop pcexp_S7
  gen pcexp_S7=(pcexp_2023 + transfert7/(hhweight_2023*def_spa*def_temp))
* Identification des pauvres| P0
  capture drop pauvre_S7
  gen pauvre_S7 = cond(pcexp_S7 < zref_2023,1,0)
  summarize pauvre_S7[iw=indweight_2023]
  scalar P0_S7= round(100 * r(mean), .01)
  display "Le P0 du Scénario 7=" `P0_S7'

* Les Gaps: Gap | Gap2
  capture drop gap_S7
  gen gap_S7  = pauvre_S7 * ((zref_2023 - pcexp_S7) / zref_2023)
  summarize gap_S7 [iw=indweight_2023]
  scalar gap_S7= round(100 * r(mean), .01)
  display "Le Gap du Scénario 7=" `gap_S7' 
  
  capture drop gap2_S7
  gen gap2_S7 = pauvre_S7*(((zref_2023 - pcexp_S7) / zref_2023)^2)
  summarize gap2_S7 [iw=indweight_2023]
  scalar gap2_S7= round(100 * r(mean), .01)
  display "Le Gap du Scénario 7=" `gap2_S7' 
** Indice de GINI
  capture drop totw toty wf tf Y area 
* Calcul des totaux et parts
  egen totw = total(indweight_2023)
  egen toty = total(indweight_2023 * pcexp_S7)
  gen wf = indweight_2023 / totw                    
  gen tf = (indweight_2023 * pcexp_S7) / toty     

* Trier et calculer le cumul de revenu
  sort pcexp_S7
  gen Y = sum(tf)

* Calcul direct de l'aire sous la courbe de Lorenz
  gen area = (Y + Y[_n-1]) * wf / 2
  replace area = Y * wf / 2 in 1             

* Somme des aires et indice de Gini
  summarize area
  scalar A = r(sum)
  scalar Gini_S7= 1 - 2 * A
  local giniS7 = round(100*Gini_S7,.01)
  display "Indice de Gini du Scénario 1=" `giniS7'
* Indice de Gini par milieu du Scénario 7
gini_by_milieu 2023_S7 indweight_2023 pcexp_S7  

**********  Coûts du Scénario 7 : Absolu | PIB  *************
capture drop cout_abs_S7
gen cout_abs_S7 = transfert7 * indweight_2023
display "le coût absolu du Scénario 7 = " cout_abs_S7

capture drop cout_PIB_S7
gen cout_PIB_S7 = (cout_abs_S7 / pib_2023) * 1000000
display "le coût rapporté au PIB du Scénario 7 = " cout_PIB_S7

**********    Efficacité  ************************** 
capture drop effc_S7
gen effc_S7 = (gap_2023 - gap_S7) / cout_abs_S7
display "L'efficacité du Scénario 7 = " effc_S7

/**********************************************
Contribution à la réduction des indicateurs FGT 
**********************************************/
fgt_by_milieu S7 pcexp_S7 indweight_2023 zref_2023

* Milieu urbain
display P0_S7_1  P1_S7_1  P2_S7_1
scalar delta_P0_S7_1 = P0_2023_1 - P0_S7_1
scalar delta_P1_S7_1 = P1_2023_1 - P1_S7_1
scalar delta_P2_S7_1 = P2_2023_1 - P2_S7_1
display delta_P0_S7_1 delta_P1_S7_1 delta_P2_S7_1

* Milieu rural
display P0_S7_2  P1_S7_2  P2_S7_2
scalar delta_P0_S7_2 = P0_2023_2 - P0_S7_2
scalar delta_P1_S7_2 = P1_2023_2 - P1_S7_2
scalar delta_P2_S7_2 = P2_2023_2 - P2_S7_2
display delta_P0_S7_2 delta_P1_S7_2 delta_P2_S7_2
 
/********************************************************
* Scénario 8 : Ménages avec au moins un membre handicapé
*/*******************************************************
* Tranfert de 100 000 FCFA à tous les ménages ayant un membre de la famille handicapé
  capture drop menage_handicap
  bysort hhid: egen menage_handicap = max(handicap)
  capture drop transfert8
  gen transfert8 = 100000* menage_handicap
  capture drop pcexp_S8
  gen pcexp_S8=(pcexp_2023 + transfert8/(hhweight_2023*def_spa*def_temp))
 * Identification des pauvres| P0
  capture drop pauvre_S8
  gen pauvre_S8 = cond(pcexp_S8 < zref_2023,1,0)
  summarize pauvre_S8[iw=indweight_2023]
  scalar P0_S8= round(100 * r(mean), .01)
  display "Le P0 du Scénario 8=" `P0_S8'

* Les Gaps: Gap | Gap2
  capture drop gap_S8
  gen gap_S8  = pauvre_S8 * ((zref_2023 - pcexp_S8) / zref_2023)
  summarize gap_S8 [iw=indweight_2023]
  scalar gap_S8= round(100 * r(mean), .01)
  display "Le Gap du Scénario 8=" `gap_S8' 
  
  capture drop gap2_S8
  gen gap2_S8 = pauvre_S8*(((zref_2023 - pcexp_S8) / zref_2023)^2)
  summarize gap2_S8 [iw=indweight_2023]
  scalar gap2_S8= round(100 * r(mean), .01)
  display "Le Gap du Scénario 8=" `gap2_S8' 
  
** Indice de GINI
  capture drop totw toty wf tf Y area 
* Calcul des totaux et parts
  egen totw = total(indweight_2023)
  egen toty = total(indweight_2023 * pcexp_S8)
  gen wf = indweight_2023 / totw                    
  gen tf = (indweight_2023 * pcexp_S8) / toty     

* Trier et calculer le cumul de revenu
  sort pcexp_S8
  gen Y = sum(tf)

* Calcul direct de l'aire sous la courbe de Lorenz
  gen area = (Y + Y[_n-1]) * wf / 2
  replace area = Y * wf / 2 in 1             

* Somme des aires et indice de Gini
  summarize area
  scalar A = r(sum)
  scalar Gini_S8= 1 - 2 * A
  local giniS8 = round(100*Gini_S8,.01)
  display "Indice de Gini du Scénario 1=" `giniS8'
* Indice de Gini par milieu du Scénario 8
gini_by_milieu 2023_S8 indweight_2023 pcexp_S8  

**********  Coûts du Scénario 8 : Absolu | PIB  *************
capture drop cout_abs_S8
gen cout_abs_S8 = transfert8 * indweight_2023
display "le coût absolu du Scénario 8 = " cout_abs_S8

capture drop cout_PIB_S8
gen cout_PIB_S8 = (cout_abs_S8 / pib_2023) * 1000000
display "le coût rapporté au PIB du Scénario 8 = " cout_PIB_S8

**********    Efficacité  ************************** 
capture drop effc_S8
gen effc_S8 = (gap_2023 - gap_S8) / cout_abs_S8
display "L'efficacité du Scénario 8 = " effc_S8
/**********************************************
Contribution à la réduction des indicateurs FGT 
**********************************************/
fgt_by_milieu S8 pcexp_S8 indweight_2023 zref_2023
* Milieu urbain
display P0_S8_1  P1_S8_1  P2_S8_1
scalar delta_P0_S8_1 = P0_2023_1 - P0_S8_1
scalar delta_P1_S8_1 = P1_2023_1 - P1_S8_1
scalar delta_P2_S8_1 = P2_2023_1 - P2_S8_1
display delta_P0_S8_1 delta_P1_S8_1 delta_P2_S8_1
* Milieu rural
display P0_S8_2  P1_S8_2  P2_S8_2
scalar delta_P0_S8_2 = P0_2023_2 - P0_S8_2
scalar delta_P1_S8_2 = P1_2023_2 - P1_S8_2
scalar delta_P2_S8_2 = P2_2023_2 - P2_S8_2
display delta_P0_S8_2 delta_P1_S8_2 delta_P2_S8_2


 