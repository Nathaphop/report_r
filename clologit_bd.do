clear
set more off 
cd"D:\conditionlogistic\" 
use "clologit_bd.dta", clear

// id is red because it coppy from word we can manage by//
drop if id == ""
gen id2= _n  //gen new variable for id 
drop id
rename id2 id


*Exercise Cat3_matched case-control study
*Primary outcome = Endometrial cancer (1=Case; 0=Control)*

sort id 
list

/***** Remember it's very important for match case control ---> condition logistic regression is
to generate new variable from id ----> idp in this dataset case: control = 1:4 *****/

gen idp = id if case ==1  //for case =1  control = 0 --->missing
replace idp = idp[_n-1] if idp == .
destring idp, replace


*LOCF => Last observation carry forward 
*=======
 
 *===========================================================
 *Table I crude analysis  outcome vs one independent variable
 *============================================================
 *****  remameber match with age so donot add age in the analysis
 
 
*Table 1: gall // match case control so we use %event occure (%column)
tab gall case, col
clogit case gall, group(idp) or

*Table 1: hpt
tab hpt case, col
clogit case hpt, group(idp) or

* Table 1: obes
tab obes case, col
clogit case obes, group(idp) or

*Table 1: euse
tab euse case, col
clogit case euse, group(idp) or

*Table 1: edose
gen edosex = edose
replace edosex = 3 if edose > 3
xi:clogit case i.edosex, group(idp) or

*Table 1: leveluse
replace leveluse = . if leveluse > 4
clogit case leveluse, group(idp) or

*Table 1: druguse
clogit case druguse, group(idp) or

*Table 1: onuse
xtile monuse4g =  monuse, nq(4)
 /*  monuse ---->for continuous data test for linearity
by generate new variable con---> quartile or tirtile  */

tabstat monuse, stat(min max n) by(monuse4g)
tab monuse4g case, col      /*it show linearity between outcom vs monuse4g 
%of event (case =1) increase or decrease respectively */

clogit case i.monuse4g, group(idp) or  // use monuse as cat by quartile

clogit case monuse, group(idp) or // if use monuse in continuous form

*Table 1: time 
xtile age4g = age, nq(4)  // the same way to test linearlity  as monuse
tabstat age, stat(min max n) by(age4g)
tab age4g case, col // not linearity (%of case not increase on quartile
xi:clogit case i.age4g, group(idp) or 
  
 /*appropiate use it in the model with categorical form because it not linearlity with outcome */
clogit case age, group(idp) or  // not appropiate ---> not linearity


*=========================================
*Table 2  multivariated analysis ---> adj OR

*==========================================
clogit case gall hpt obes euse i.edosex leveluse druguse monuse age , group(idp) or /*Full -Not recommended*/

gen x = edosex*monuse      //---> effect modifier (interaction term) edosex* vs. monuse 

/* innitial model */

xi:clogit case gall hpt obes i.edosex monuse age x, group(idp) or
est store A   // backward elimination  

xi:clogit case gall hpt obes i.edosex monuse age , group(idp) or // remove x from model//
lrtest A  // pvalue .005 we can remove x from model and fit new model without x

xi:clogit case gall hpt obes i.edosex monuse age , group(idp) or
est store B // hpt p vaue more than another variable so we remove it from model

xi:clogit case gall obes i.edosex monuse age , group(idp) or
lrtest B // p >0.05 we can remove hpt and fit new model without htp
est store C // fit model after remove monuse from the model

xi:clogit case gall obes i.edosex monuse , group(idp) or
lrtest C, force /* use force because obs in the 2 model we compare don't equally
but Cannot remove monuse */

xi:clogit case gall i.edosex monuse, group(idp) or
lrtest C, force /*Cannot use obes*/
est store D

xi:clogit case gall monuse, group(idp) or
lrtest D

*Final model
xi:clogit case gall i.edosex monuse, group(idp) or

*Final model -> monuse as every 6 months   // use monuse increase 1 points is difficult to impretate
gen monuse6 = monuse/6 
xi:clogit case gall i.edosex monuse6, group(idp) or

