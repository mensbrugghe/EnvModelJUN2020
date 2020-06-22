$macro defaultInit 1
* $macro defaultInit (0.5+uniform(0,1)) ;

$macro Agg3(mat3,i,mapi,a,mapa)  sum((i0,a0)$(mapi0(i0,i) and mapa0(a0,a)), mat3(i0,a0,r))
$macro Agg2(mat2,i,mapi)         sum(i0$mapi0(i0,i), mat2(i0,r))

* --------------------------------------------------------------------------------------------------
*
*  Initialize prices
*
* --------------------------------------------------------------------------------------------------

pat0(r,i)       = defaultInit ;

pxghg0(r,a)     = defaultInit ;
pxp0(r,a)       = defaultInit ;
uc0(r,a)        = defaultInit ;

trent0(r)       = defaultInit ;
ptland0(r)      = defaultInit ;
pf0(r,f,a)      = defaultInit ;

pnd10(r,a)      = defaultInit ;
pnd20(r,a)      = defaultInit ;
pwat0(r,a)      = defaultInit ;
pnrg0(r,a)      = defaultInit ;
paNRG0(r,a,NRG) = defaultInit ;
pnely0(r,a)     = defaultInit ;
polg0(r,a)      = defaultInit ;

plab10(r,a)     = defaultInit ;
plab20(r,a)     = defaultInit ;
plabb0(r,wb,a)  = defaultInit ;

pks0(r,a)       = defaultInit ;
pksw0(r,a)      = defaultInit ;
pkf0(r,a)       = defaultInit ;
pkef0(r,a)      = defaultInit ;

pva0(r,a)       = defaultInit ;
pva10(r,a)      = defaultInit ;
pva20(r,a)      = defaultInit ;

ps0(r,i)        = defaultInit ;

pwmg0(s,i,d)    = defaultInit ;

pmt0(r,i)       = defaultInit ;

ptmg0(img)      = defaultInit ;

* --------------------------------------------------------------------------------------------------
*
*  Price/volume splits for energy
*
* --------------------------------------------------------------------------------------------------

Parameters
   xatNRG00(r,e)        "Total energy absorption in MTOE"
   xaNRG00(r,e,aa)      "Total energy absorption in MTOE by agent"
   patNRG00(r,e)        "Average price of energy in $/MTOE"
   paNRG00(r,e,aa)      "Agents' price in $/MTOE"
;

gamma_eda(r,i,aa) = 1 ;
gamma_edd(r,i,aa) = 1 ;
gamma_edm(r,i,aa) = 1 ;

if(ifNRG,

*  Initialize initial volumes

   xaNRG00(r,e,a)   = escale*(sum((i0,a0)$(mapi0(i0,e) and mapa0(a0,a)), nrgdf(i0,a0,r))
                    + sum((i0,a0)$(mapi0(i0,e) and mapa0(a0,a)), nrgmf(i0,a0,r))) ;
   xaNRG00(r,e,h)   = escale*(sum(i0$mapi0(i0,e), nrgdp(i0,r)) + sum(i0$mapi0(i0,e), nrgmp(i0,r))) ;
   xaNRG00(r,e,gov) = escale*(sum(i0$mapi0(i0,e), nrgdg(i0,r)) + sum(i0$mapi0(i0,e), nrgmg(i0,r))) ;
$iftheni "%RD_MODULE%" == "ON"
   xaNRG00(r,e,r_d) = escale*(sum(i0$mapi0(i0,e), nrgdr(i0,r)) + sum(i0$mapi0(i0,e), nrgmr(i0,r))) ;
$endif
   xaNRG00(r,e,inv) = escale*(sum(i0$mapi0(i0,e), nrgdi(i0,r)) + sum(i0$mapi0(i0,e), nrgmi(i0,r))) ;
   xatNRG00(r,e)    = sum(aa, xaNRG00(r,e,aa)) ;

*  Initialize cumbustion ratio

   phiNRG(r,fuel,aa)   = 1 ;
   phiNRG(r,fuel,a)    = eScale*sum((i0,a0)$(mapi0(i0,fuel) and mapa0(a0,a)), nrgComb(i0,a0,r)) ;
   phiNRG(r,fuel,a)$xaNRG00(r,fuel,a) = phiNRG(r,fuel,a)/xaNRG00(r,fuel,a) ;
   phiNRG(r,fuel,aa)$(phiNRG(r,fuel,aa) > 1) = 1 ;

*  Initialize values excl sales taxes (using price matrices temporarily)

   paNRG00(r,e,a)    = inscale*(Agg3(vdfb,e,mapi,a,mapa) + Agg3(vmfb,e,mapi,a,mapa)) ;
   paNRG00(r,e,h)    = inscale*(Agg2(vdpb,e,mapi) + Agg2(vmpb,e,mapi)) ;
   paNRG00(r,e,gov)  = inscale*(Agg2(vdgb,e,mapi) + Agg2(vmgb,e,mapi)) ;
$iftheni "%RD_MODULE%" == "ON"
   paNRG00(r,e,r_d)  = inscale*(Agg2(vdrb,e,mapi) + Agg2(vmrb,e,mapi)) ;
$endif
   paNRG00(r,e,inv)  = inscale*(Agg2(vdib,e,mapi) + Agg2(vmib,e,mapi)) ;

*  Check for consistency -- xaNrg contains energy volumes, paNrg contains SAM values

   if(1,
      put screen ;
      screen.nd = 9 ;
      put / ;
      work = 0 ;
      loop((r,e,aa),
         if(xaNRG00(r,e,aa) le 0 and paNRG00(r,e,aa) ne 0,
            put "WARNING: NRG=0, SAM<>0 --> ", r.tl, e.tl, aa.tl, (xaNRG00(r,e,aa)/escale):15:8,
               (paNRG00(r,e,aa)/inscale):15:8 / ;
            work = work + 1 ;
         elseif(xaNRG00(r,e,aa) ne 0 and paNRG00(r,e,aa) le 0),
            put "WARNING: NRG<>0, SAM=0 --> ", r.tl, e.tl, aa.tl, (xaNRG00(r,e,aa)/escale):15:8,
               (paNRG00(r,e,aa)/inscale):15:8 / ;
            work = work + 1 ;
         ) ;
      ) ;
      if(work > 0, Abort "Inconsistent energy statistics" ; ) ;
   ) ;

*  Calculate average price

   patNRG00(r,e) = (sum(aa, paNRG00(r,e,aa))/xaTNRG00(r,e))$(xatNRG00(r,e))
                 + (1)$(xatNRG00(r,e) eq 0)
                 ;

*  Calculate end-user price

   paNRG00(r,e,aa) = (paNRG00(r,e,aa)/xaNRG00(r,e,aa))$(xaNRG00(r,e,aa))
                   + (1)$(xaNRG00(r,e,aa) eq 0) ;

*  Calculate price adjustment factor

   gamma_eda(r,e,aa)$(xatNRG00(r,e)) = paNRG00(r,e,aa)/patNRG00(r,e) ;

   pat0(r,e)    = patNRG00(r,e) ;
   xatNRG(r,e)  = xatNRG00(r,e) ;
) ;

*  Incorporate trade dimension of energy volumes

gamma_ew(r,i,rp) = 1 ;
gamma_esd(r,i)   = 1 ;
gamma_ese(r,i)   = 1 ;

Parameters
   xwNRG0(r,e,rp)       "Volume of bilateral trade in MTOE"
   peNRG0(r,e,rp)       "Price of bilateral trade at producer prices"
   petNRG0(r,e)         "Average price of exports"
   xetNRG0(r,e)         "Total exports in MTOE"
   xmtNRG0(r,e)         "Total imports in MTOE"
   pmtNRG0(r,e)         "Price of aggregate imports"
   xdNRG0(r,e)          "Domestic absorption of domestic output"
   xsNRG0(r,e)          "Domestic supply"
;

if(ifNRG,

*  Initialize volumes and values, using pe to hold values

   xwNRG0(r,e,rp) = escale*sum(i0$mapi0(i0,e),exi(i0,r,rp)) ;
   peNRG0(r,e,rp) = inscale*sum(i0$mapi0(i0,e),vxsb(i0, r, rp)) ;

*  Check consistency of flows

   put screen ; put / ;
   loop((r,e,rp)$((xwNRG0(r,e,rp) eq 0 and peNRG0(r,e,rp)) or
                  (xwNRG0(r,e,rp) and peNRG0(r,e,rp) eq 0)),
      put "Inconsistent energy data: ", r.tl, e.tl, rp.tl, "xw = ", (xwNRG0(r,e,rp)/escale):15:6,
         "  SAM = ", (peNRG0(r,e,rp)/inscale) / ;
   ) ;

*  Calculate total exports and average export price

   xetNRG0(r,e) = sum(d, xwNRG0(r,e,d)) ;
   petNRG0(r,e) = (sum(d, peNRG0(r,e,d))/xetNRG0(r,e))$xetNRG0(r,e)
                + 1$(xetNRG0(r,e) eq 0) ;

*  Calculate bilateral export price

   peNRG0(s,e,d) = (peNRG0(s,e,d)/xwNRG0(s,e,d))$xwNRG0(s,e,d)
                 + (1)$(not xwNRG0(s,e,d)) ;

*  Calculate price adjustment factors

   gamma_ew(s,e,d)$xwNRG0(s,e,d) = peNRG0(s,e,d)/petNRG0(s,e) ;

   pet0(r,e) = petNRG0(r,e) ;

*  Calculate aggregate import price

   xmtNRG0(r,e) = sum(s, xwNRG0(s,e,r)) ;
   pmtNRG0(r,e) = inscale*(sum(i0$mapi0(i0,e),sum(s,vmsb(i0, s, r)))) ;
   pmtNRG0(r,e) = (pmtNRG0(r,e)/xmtNRG0(r,e))$(xmtNRG0(r,e))
                + (1)$(xmtNRG0(r,e) eq 0) ;

   xdNRG0(r,e)  = xatNRG00(r,e) - xmtNRG0(r,e) ;
   pdt0(r,e)    = ((patNRG00(r,e)*xatNRG00(r,e) - pmtNRG0(r,e)*xmtNRG0(r,e))/xdNRG0(r,e))
                $(xdNRG0(r,e) gt 0)
                + 1$(xdnrg0(r,e) le 0) ;

*  !!!! 03-Mar-2017 (DvdM)
*  Have a tolerance level for pd
*  We should review the energy consistencies--it gets messy with the 'make' system
   if(1,
      pdt0(r,e)$(pdt0(r,e) lt 0 or abs(pdt0(r,e)) le 1e-5) = 0 ;
      loop(t0, xdNRG0(r,e)$(pdt0(r,e) eq 0) = 0 ; ) ;
      pdt0(r,e)$(pdt0(r,e) = 0) = 1 ;
   ) ;

   xsNRG0(r,e)  = xdNRG0(r,e) + xetNRG0(r,e) ;
   ps0(r,e)     = ((pdt0(r,e)*xdNRG0(r,e) + petNRG0(r,e)*xetNRG0(r,e))/xsNRG0(r,e))
                $(xsNRG0(r,e) gt 0)
                + 1$(xsNRG0(r,e) le 0) ;

   gamma_esd(r,e) = pdt0(r,e)/ps0(r,e) ;
   gamma_ese(r,e) = pet0(r,e)/ps0(r,e) ;
) ;

* display xdNRG0, xsNRG0, xetNRG0, pdt0, petnrg0, pmtnrg0 ;

*  Set producer price of exports to aggregate producer price

pdt0(r,i)  = gamma_esd(r,i)*ps0(r,i) ;
pet0(r,i)  = gamma_ese(r,i)*ps0(r,i) ;
pe0(s,i,d) = gamma_ew(s,i,d)*pet0(s,i) ;

loop((i,k)$mapk(i,k),
   cmat(i,k,r) = Agg2(vdpp,i,mapi) + Agg2(vmpp,i,mapi) ;
) ;

*  Initialize Armington elasticities

*  !!!! NEEDS REVIEW -- SIGMAM ALSO READ IN !!!!
*  USE GTAP elasticities

$ifthen %ifGTAPArm% == 1

   sigmamt(r,i) = sum(i0$mapi0(i0,i), sum(a0, vdfp(i0,a0,r) + vmfp(i0,a0,r))
                +                             vdpp(i0,r)    + vmpp(i0,r)
                +                             vdgp(i0,r)    + vmgp(i0,r)
                +                             vdip(i0,r)    + vmip(i0,r)) ;
   sigmamt(r,i)$sigmamt(r,i)
                = sum(i0$mapi0(i0,i), ESUBD(i0,r)*(sum(a0, vdfp(i0,a0,r) + vmfp(i0,a0,r))
                +                                          vdpp(i0,r)    + vmpp(i0,r)
                +                                          vdgp(i0,r)    + vmgp(i0,r)
                +                                          vdip(i0,r)    + vmip(i0,r)))
                / sigmamt(r,i) ;

   sigmaw(r,i) = sum(i0$mapi0(i0,i), sum(a0, vmfp(i0,a0,r))
               +                             vmpp(i0,r)
               +                             vmgp(i0,r)
               +                             vmip(i0,r)) ;
   sigmaw(r,i)$sigmaw(r,i)
               = sum(i0$mapi0(i0,i), ESUBM(i0,r)*(sum(a0, vmfp(i0,a0,r))
               +                                          vmpp(i0,r)
               +                                          vmgp(i0,r)
               +                                          vmip(i0,r)))
               / sigmaw(r,i) ;

sigmam(r,i,aa) = sigmamt(r,i) ;

$endif

*  Initialize household utility parameters

eh0(k,r) = sum(i$mapk(i,k), sum(i0$mapi0(i0,i), vdpp(i0,r) + vmpp(i0,r))) ;
bh0(k,r) = eh0(k,r) ;
eh0(k,r)$eh0(k,r)
          = sum(i$mapk(i,k), sum(i0$mapi0(i0,i), INCPAR(i0,r)*(vdpp(i0,r) + vmpp(i0,r))))
          / eh0(k,r) ;
bh0(k,r)$bh0(k,r)
          = sum(i$mapk(i,k), sum(i0$mapi0(i0,i), SUBPAR(i0,r)*(vdpp(i0,r) + vmpp(i0,r))))
          / bh0(k,r) ;

*  Aggregate other elasticities

loop((nrs,t0),
   etanrs.l(r,a,t0) = sum(a0$mapa0(a0,a), evfb(nrs, a0, r)) ;
   etanrsx(r,a,lh)$etanrs.l(r,a,t0)
      = sum(a0$mapa0(a0,a), etanrsx0(r,a0,lh)*evfb(nrs, a0, r))/etanrs.l(r,a,t0) ;
) ;

*  Initialize labor market assumptions

loop((r,l),
   if(ifLSeg(r,l) eq 1,
*     Segmented labor markets
      lsFlag(r,l,rur)   = yes ;
      lsFlag(r,l,urb)   = yes ;
      lsFlag(r,l,nsg)   = no ;
      migr0(r,l)        = 0.01*labHyp(r,l,"migr0") ;
      uez0(r,l,rur)     = 0.01*labHyp(r,l,"uezRur0") ;
      uez0(r,l,urb)     = 0.01*labHyp(r,l,"uezUrb0") ;
      ueMinz0(r,l,rur)  = 0.01*labHyp(r,l,"ueMinzRur0") ;
      ueMinz0(r,l,urb)  = 0.01*labHyp(r,l,"ueMinzUrb0") ;
      resWage0(r,l,rur) = labHyp(r,l,"resWageRur0") ;
      resWage0(r,l,urb) = labHyp(r,l,"resWageUrb0") ;
      if(labHyp(r,l,"ueFlagRur0") eq 1,
         ueFlag(r,l,rur) = resWageUE ;
      elseif(labHyp(r,l,"ueFlagRur0") eq 2),
         ueFlag(r,l,rur) = MonashUE ;
      else
         ueFlag(r,l,rur) = fullEmpl ;
      ) ;
      if(labHyp(r,l,"ueFlagUrb0") eq 1,
         ueFlag(r,l,urb) = resWageUE ;
      elseif(labHyp(r,l,"ueFlagUrb0") eq 2),
         ueFlag(r,l,urb) = MonashUE ;
      else
         ueFlag(r,l,urb) = fullEmpl ;
      ) ;
   else
*     Integrated labor markets
      lsFlag(r,l,rur)   = no ;
      lsFlag(r,l,urb)   = no ;
      lsFlag(r,l,nsg)   = yes ;
      migr0(r,l)        = 0 ;
      uez0(r,l,nsg)     = 0.01*labHyp(r,l,"uezUrb0") ;
      ueMinz0(r,l,nsg)  = 0.01*labHyp(r,l,"ueMinzUrb0") ;
      resWage0(r,l,nsg) = labHyp(r,l,"resWageUrb0") ;
      if(labHyp(r,l,"ueFlagUrb0") eq 1,
         ueFlag(r,l,nsg) = resWageUE ;
      elseif(labHyp(r,l,"ueFlagUrb0") eq 2),
         ueFlag(r,l,nsg) = MonashUE ;
      else
         ueFlag(r,l,nsg) = fullEmpl ;
      ) ;
   ) ;
   omegarwg(r,l,z)  = labHyp(r,l,"omegarwg") ;
   omegarwue(r,l,z) = labHyp(r,l,"omegarwue") ;
   omegarwp(r,l,z)  = labHyp(r,l,"omegarwp") ;
) ;

*  Checks

put screen ; put / ;
work = 0 ;
loop(lsFlag(r,l,z),
   if(uez0(r,l,z) lt ueMinz0(r,l,z),
      put "Initial unemployment is less than minimum UE: ", r.tl, l.tl, z.tl / ;
      work = work + 1 ;
   ) ;
   if(resWage0(r,l,z) ne na and ueFlag(r,l,z) eq resWageUE,
      if(resWage0(r,l,z) gt 1,
         put "Initial reservation wage is greater than actual wage: ", r.tl, l.tl, z.tl / ;
         work = work + 1 ;
      ) ;
   ) ;
) ;

if(work > 0,
   put "Invalid UE initialization..." / ;
   abort "Check parameter file..." ;
) ;

* --------------------------------------------------------------------------------------------------
*
*  Initialize Armington demand
*
* --------------------------------------------------------------------------------------------------

*  Value of agent specific Armington tax

paTax0(r,i,a)   = inscale*(Agg3(vdfp,i,mapi,a,mapa) + Agg3(vmfp,i,mapi,a,mapa)) ;
paTax0(r,i,h)   = inscale*(Agg2(vdpp,i,mapi) + Agg2(vmpp,i,mapi)) ;
paTax0(r,i,gov) = inscale*(Agg2(vdgp,i,mapi) + Agg2(vmgp,i,mapi)) ;
$iftheni "%RD_MODULE%" == "ON"
paTax0(r,i,r_d) = inscale*(Agg2(vdrp,i,mapi) + Agg2(vmrp,i,mapi)) ;
$endif
paTax0(r,i,inv) = inscale*(Agg2(vdip,i,mapi) + Agg2(vmip,i,mapi)) ;

pdTax0(r,i,a)   = inscale*Agg3(vdfp,i,mapi,a,mapa) ;
pdTax0(r,i,h)   = inscale*Agg2(vdpp,i,mapi) ;
pdTax0(r,i,gov) = inscale*Agg2(vdgp,i,mapi) ;
$iftheni "%RD_MODULE%" == "ON"
pdTax0(r,i,r_d) = inscale*Agg2(vdrp,i,mapi) ;
$endif
pdTax0(r,i,inv) = inscale*Agg2(vdip,i,mapi) ;

pmTax0(r,i,a)   = inscale*Agg3(vmfp,i,mapi,a,mapa) ;
pmTax0(r,i,h)   = inscale*Agg2(vmpp,i,mapi) ;
pmTax0(r,i,gov) = inscale*Agg2(vmgp,i,mapi) ;
$iftheni "%RD_MODULE%" == "ON"
pmTax0(r,i,r_d) = inscale*Agg2(vmrp,i,mapi) ;
$endif
pmTax0(r,i,inv) = inscale*Agg2(vmip,i,mapi) ;

paTax0(r,i,aa)  = pdTax0(r,i,aa) + pmTax0(r,i,aa) ;

*  Value of agent specific Armington consumption at market price

xd0(r,i,a)    = inscale*Agg3(vdfb,i,mapi,a,mapa) ;
xd0(r,i,h)    = inscale*Agg2(vdpb,i,mapi) ;
xd0(r,i,gov)  = inscale*Agg2(vdgb,i,mapi) ;
$iftheni "%RD_MODULE%" == "ON"
xd0(r,i,r_d)  = inscale*Agg2(vdrb,i,mapi)  ;
$endif
xd0(r,i,inv)  = inscale*Agg2(vdib,i,mapi) ;

xm0(r,i,a)    = inscale*Agg3(vmfb,i,mapi,a,mapa) ;
xm0(r,i,h)    = inscale*Agg2(vmpb,i,mapi) ;
xm0(r,i,gov)  = inscale*Agg2(vmgb,i,mapi) ;
$iftheni "%RD_MODULE%" == "ON"
xm0(r,i,r_d)  = inscale*Agg2(vmrb,i,mapi)  ;
$endif
xm0(r,i,inv)  = inscale*Agg2(vmib,i,mapi) ;

xa0(r,i,aa)      = xd0(r,i,aa) + xm0(r,i,aa) ;

*  Agent specific tax rate

paTax0(r,i,aa)$xa0(r,i,aa) = paTax0(r,i,aa) / xa0(r,i,aa) - 1 ;
pdTax0(r,i,aa)$xd0(r,i,aa) = pdTax0(r,i,aa) / xd0(r,i,aa) - 1 ;
pmTax0(r,i,aa)$xm0(r,i,aa) = pmTax0(r,i,aa) / xm0(r,i,aa) - 1 ;

*  Impose price/volume split

xa0(r,i,aa) = xa0(r,i,aa) / (gamma_eda(r,i,aa)*pat0(r,i)) ;
xd0(r,i,aa) = xd0(r,i,aa) / (gamma_edd(r,i,aa)*pdt0(r,i)) ;
xm0(r,i,aa) = xm0(r,i,aa) / (gamma_edm(r,i,aa)*pmt0(r,i)) ;

*  Determine agent' specific Armington price, i.e. tax inclusive Armington price

pa0(r,i,aa) = gamma_eda(r,i,aa) * pat0(r,i) * (1 + paTax0(r,i,aa)) ;
pd0(r,i,aa) = gamma_edd(r,i,aa) * pdt0(r,i) * (1 + pdTax0(r,i,aa)) ;
pm0(r,i,aa) = gamma_edm(r,i,aa) * pmt0(r,i) * (1 + pmTax0(r,i,aa)) ;

*  Initialize aggregate final demand

yfd0(r,fd) = sum(i, pa0(r,i,fd)*xa0(r,i,fd)) ;
pfd0(r,fd) = 1 ;
xfd0(r,fd) = yfd0(r,fd) / pfd0(r,fd) ;
lambdafd(r,i,fd,t) = 1 ;

loop(t0,
   fdFlag(r,fd)$xfd0(r,fd) = 1 ;
) ;

* --------------------------------------------------------------------------------------------------
*
*  Initialize the 'make/use' module
*
* --------------------------------------------------------------------------------------------------

*  !!!! Need to insure that when running the model with perfect transformation and perfect
*       substitution that the prices align (one way is to have fixed price adjusters)
*  Set 'pp' to ps as it is more likely that demand has perfect substitutes than
*  supply

*  Calculate x tax-inclusive

x0(r,a,i)        = inscale*sum((a0,i0)$(mapa0(a0,a) and mapi0(i0,i)), makb(i0,a0,r)) ;
lambdas(r,a,i,t) = 1 ;
ptax0(r,a,i)     = inscale*sum((a0,i0)$(mapa0(a0,a) and mapi0(i0,i)), maks(i0,a0,r)) ;
ptax0(r,a,i)$ptax0(r,a,i) = x0(r,a,i)/ptax0(r,a,i) - 1 ;
x0(r,a,i)        = x0(r,a,i) / ps0(r,i) ;
pp0(r,a,i)       = ps0(r,i) ;
p0(r,a,i)        = pp0(r,a,i)/(1+ptax0(r,a,i)) ;

xs0(r,i) = sum(a, pp0(r,a,i)*x0(r,a,i))/ps0(r,i) ;

loop(t$(ord(t) eq 1),
   xsFlag(r,i)$xs0(r,i) = 1 ;
) ;

*  Re-base pxp, uc -- assume it is equal to p.l for fossil fuels

loop((affl,e),
   pxp0(r,affl)$x0(r,affl,e) = p0(r,affl,e) ;
) ;
*  !!!! Will require revision if GHG bundles are not zero in the base year
uc0(r,affl) = pxp0(r,affl) ;

* --------------------------------------------------------------------------------------------------
*
*  Initialize the production variables
*
* --------------------------------------------------------------------------------------------------

*  Labor demand

xf0(r,l,a) = (inscale*sum(a0$mapa0(a0,a),evfb(l,a0,r)))$(sum(a0$mapa0(a0,a),empl(l,a0,r)) eq na)
           + (lscale*sum(a0$mapa0(a0,a),empl(l,a0,r)))$(sum(a0$mapa0(a0,a),empl(l,a0,r)) ne na) ;
loop(t0,
   pf0(r,l,a) = (inscale*sum(a0$mapa0(a0,a),evfb(l,a0,r)) / xf0(r,l,a))$(xf0(r,l,a))
              + (1)$(xf0(r,l,a) eq 0) ;
) ;

*  Capital demand

pk0(r,a)     = trent0(r) ;
pf0(r,cap,a) = trent0(r) ;

loop(cap,
   kv0(r,a)     = inscale*(sum(a0$mapa0(a0,a),evfb(cap,a0,r))) ;
   xf0(r,cap,a) = kv0(r,a) ;
) ;

*  Capital price split

kv0(r,a) = kv0(r,a)/pk0(r,a) ;

arent0(r)  = 1 ;
rtrent0(r) = trent0(r) ;

*  Land demand

ptland0(r)$(etat(r) eq inf)     = 1 ;
pf0(r,lnd,a)$(omegat(r) eq inf) = ptland0(r) ;

xf0(r,lnd,a) = (inscale*(sum(a0$mapa0(a0,a),evfb(lnd,a0,r))))/pf0(r,lnd,a) ;

*  Natural resource demand

xf0(r,nrs,a) = (inscale*(sum(a0$mapa0(a0,a),evfb(nrs,a0,r)))) / pf0(r,nrs,a) ;

*   Water demand

pf0(r,wat,a) = 1 ;
loop(wat$IFWATER,
   xf0(r,wat,a) = watScale*sum(a0$mapa0(a0,a),h2oCrp(a0,r)) ;
   pf0(r,wat,a)$xf0(r,wat,a) = inscale*sum(a0$mapa0(a0,a),evfb(wat,a0,r))/xf0(r,wat,a) ;
) ;

*  Factor tax rate and purchasers' price of factors

pftax0(r,f,a)  = inscale*sum(a0$mapa0(a0,a),evfb(f,a0,r)) ;
pftax0(r,f,a)$pftax0(r,f,a)
   = inscale*sum(a0$mapa0(a0,a), evfp(f,a0,r)) / pftax0(r,f,a) - 1 ;

pfp0(r,f,a) = pf0(r,f,a)*(1 + pftax0(r,f,a)) ;
loop(cap,
   pkp0(r,a) = pk0(r,a)*(1 + pftax0(r,cap,a)) ;
) ;
chiFtax.fx(r,t)     = 1 ;
alphaFtax.fx(r,t)   = 0 ;
ifMFTaxFlag(r,f,a)  = 0 ;
ifAFTaxFlag(r,f,a)  = 0 ;

cpi0(r,h,cpindx) = 1 ;
pfact0(r)        = 1 ;
pwfact0          = 1 ;

*  Construct the intermediate demand bundles

nd10(r,a)       = sum(i$mapi1(i,a), pa0(r,i,a)*xa0(r,i,a)) / pnd10(r,a) ;
nd20(r,a)       = sum(i$mapi2(i,a), pa0(r,i,a)*xa0(r,i,a)) / pnd20(r,a) ;
xwat0(r,a)      = (sum(i$iw(i), pa0(r,i,a)*xa0(r,i,a))
                +   sum(wat, pfp0(r,wat,a)*xf0(r,wat,a))) / pwat0(r,a) ;

xnrg0(r,a)      = sum(e, pa0(r,e,a)*xa0(r,e,a)) / pnrg0(r,a) ;
xaNRG0(r,a,NRG) = sum(e$mape(NRG,e), pa0(r,e,a)*xa0(r,e,a)) / paNRG0(r,a,NRG) ;
xolg0(r,a)      = (paNRG0(r,a,"GAS")*xaNRG0(r,a,"GAS")
                +  paNRG0(r,a,"OIL")*xaNRG0(r,a,"OIL")) / polg0(r,a) ;
xnely0(r,a)     = (paNRG0(r,a,"COA")*xaNRG0(r,a,"COA")
                +  polg0(r,a)*xolg0(r,a)) / pnely0(r,a) ;
xnrg0(r,a)      = ((paNRG0(r,a,"ELY")*xaNRG0(r,a,"ELY")
                +   pnely0(r,a)*xnely0(r,a)) / pnrg0(r,a))$ifNRGNest
                + (sum(e, pa0(r,e,a)*xa0(r,e,a)) / pnrg0(r,a))$(not ifNRGNest)
                ;

*  Construct the labor demand bundles

labb0(r,wb,a)   = sum(mapl(wb,l), pfp0(r,l,a)*xf0(r,l,a)) / plabb0(r,wb,a) ;
lab10(r,a)      = sum(wb$maplab1(wb), plabb0(r,wb,a)*labb0(r,wb,a)) / plab10(r,a) ;
lab20(r,a)      = sum(wb$(not maplab1(wb)), plabb0(r,wb,a)*labb0(r,wb,a)) / plab20(r,a) ;

*  Construct the KF/KEF bundles

ks0(r,a)  = (pkp0(r,a)*kv0(r,a) + lab20(r,a)*plab20(r,a)) / pks0(r,a) ;

ksw0(r,a) = (pks0(r,a)*ks0(r,a) + xwat0(r,a)*pwat0(r,a)) / pksw0(r,a) ;

kf0(r,a)  = (pksw0(r,a)*ksw0(r,a) + sum(nrs, pfp0(r,nrs,a)*xf0(r,nrs,a))) / pkf0(r,a) ;

kef0(r,a) = (pkf0(r,a)*kf0(r,a) + pnrg0(r,a)*xnrg0(r,a)) / pkef0(r,a) ;

*  Construct the VA bundles

va20(r,a) = ((sum(lnd, pfp0(r,lnd,a)*xf0(r,lnd,a)) + pkef0(r,a)*kef0(r,a))
          / pva20(r,a))$acr(a)
          + ((sum(lnd, pfp0(r,lnd,a)*xf0(r,lnd,a)) + pnd20(r,a)*nd20(r,a))
          / pva20(r,a))$alv(a)
          ;

va10(r,a) = ((pnd20(r,a)*nd20(r,a) + pva20(r,a)*va20(r,a)) / pva10(r,a))$acr(a)
          + ((lab10(r,a)*plab10(r,a) + pkef0(r,a)*kef0(r,a)) / pva10(r,a))$alv(a)
          + ((sum(lnd, pfp0(r,lnd,a)*xf0(r,lnd,a)) + pkef0(r,a)*kef0(r,a)) / pva10(r,a))$ax(a)
          ;

va0(r,a) = ((lab10(r,a)*plab10(r,a) + pva10(r,a)*va10(r,a)) / pva0(r,a))$acr(a)
         + ((pva20(r,a)*va20(r,a)   + pva10(r,a)*va10(r,a)) / pva0(r,a))$alv(a)
         + ((lab10(r,a)*plab10(r,a) + pva10(r,a)*va10(r,a)) / pva0(r,a))$ax(a) ;

*  Construct the XPX bundle

xpx0(r,a) = (pnd10(r,a)*nd10(r,a) + pva0(r,a)*va0(r,a)) / pxp0(r,a) ;

*  Incorporate GHG bundle

* !!!! TO BE DONE

xghg0(r,a)  = 0 ;
uctax0(r,a) = 0 ;

* Construct the XPV bundle

xp0(r,a)    = (pxp0(r,a)*xpx0(r,a) + pxghg0(r,a)*xghg0(r,a)) / uc0(r,a) ;
xpv0(r,a)   = xp0(r,a) ;

pxv0(r,a) = uc0(r,a)*(1 + uctax0(r,a)) ;

*  Calculate cost of production pre-tax in value terms

xp0(r,a) = sum(vOld, pxv0(r,a)*xp0(r,a)) ;

px0(r,a) = pxv0(r,a) ;
xp0(r,a) = xp0(r,a) / px0(r,a) ;

*  Re-base natural resource in fossil fuels

*display pf0, xf0 ;

loop(nrs,
   pf0(r,nrs,affl) = (pf0(r,nrs,affl)*xf0(r,nrs,affl) / xp0(r,affl))$(xf0(r,nrs,affl))
                   + 1$(xf0(r,nrs,affl) eq 0) ;
   xf0(r,nrs,affl)$xf0(r,nrs,affl) = xp0(r,affl) ;
   pfp0(r,nrs,affl) = pf0(r,nrs,affl)*(1 + pftax0(r,nrs,affl)) ;
   xfNot0(r,a)      = xf0(r,nrs,a) ;
   xfs0(r,nrs,a)    = xf0(r,nrs,a) ;
   xfGap.l(r,a,t)   = 0 ;
) ;

*display xfNot0, xfs0, xf0 ;

* display uc0, px0, pp0 ;

*  Initialize the technology parameters

axghg.l(r,a,v,t)         = 1 ;
tfp.l(r,a,v,t)           = 1 ;
ftfp.l(r,a,v,t)          = 1 ;
lambdaxp.l(r,a,v,t)      = 1 ;
lambdaghg.l(r,a,v,t)     = 1 ;
lambdaf.l(r,f,a,t)       = 1 ;
lambdak.l(r,a,v,t)       = 1 ;
capu.fx(r,a,v,t)         = 1 ;
chiglab.fx(r,l,t)        = 0 ;
lambdaio.l(r,i,a,t)      = 1 ;
lambdae.l(r,e,a,v,t)     = 1 ;
lambdace.l(r,e,k,h,t)    = 1 ;
lambdac(r,i,k,h,t)       = 1 ;
lambdapb(r,elya,elyc,t)  = 1 ;
lambdapow(r,pb,elyc,t)   = 1 ;

lambdah2obnd.l(r,wbnd,t) = 1 ;

*  Initialize production flags

loop((t, vOld)$(ord(t) eq 1),
   xpFlag(r,a)$xp0(r,a)               = 1 ;
   ghgFlag(r,a)$xghg0(r,a)            = 1 ;

   nd1Flag(r,a)$nd10(r,a)             = 1 ;
   nd2Flag(r,a)$nd20(r,a)             = 1 ;
   watFlag(r,a)$xwat0(r,a)            = 1 ;
   lab1Flag(r,a)$lab10(r,a)           = 1 ;
   lab2Flag(r,a)$lab20(r,a)           = 1 ;
   labbFlag(r,wb,a)$labb0(r,wb,a)     = 1 ;

   va1Flag(r,a)$va10(r,a)             = 1 ;
   va2Flag(r,a)$va20(r,a)             = 1 ;
   kefFlag(r,a)$kef0(r,a)             = 1 ;
   xfFlag(r,f,a)$xf0(r,f,a)           = 1 ;
   kfFlag(r,a)$kf0(r,a)               = 1 ;
   kFlag(r,a)$kv0(r,a)                = 1 ;
   xnrgFlag(r,a)$xnrg0(r,a)           = 1 ;
   xnelyFlag(r,a)$xnely0(r,a)         = 1 ;
   xolgFlag(r,a)$xolg0(r,a)           = 1 ;
   xaFlag(r,i,aa)$xa0(r,i,aa)         = 1 ;
   xdFlag(r,i,aa)$xd0(r,i,aa)         = 1 ;
   xmFlag(r,i,aa)$xm0(r,i,aa)         = 1 ;
   xaNRGFlag(r,a,NRG)$xaNRG0(r,a,NRG) = 1 ;
) ;

* --------------------------------------------------------------------------------------------------
*
*  Initialize household demand module
*
* --------------------------------------------------------------------------------------------------

*  Top level demand

cmat(i,k,r)  = inscale*cmat(i,k,r) ;
pc0(r,k,h)   = 1 ;
xc0(r,k,h)   = sum(i, cmat(i,k,r)) / pc0(r,k,h) ;
hshr0(r,k,h) = (pc0(r,k,h)*xc0(r,k,h))/yfd0(r,h) ;
loop(t0,
   xcFlag(r,k,h)$xc0(r,k,h) = 1 ;
) ;

*  Initialize income elasticity for ELES calibration
*  !!!! TAKEN FROM CDE FUNCTION
loop(h,
   incElas(k,r) = ((eh0(k,r)*bh0(k,r)
                - sum(kp$xcFlag(r,kp,h), hshr0(r,kp,h)*eh0(kp,r)*bh0(kp,r)))
                / sum(kp$xcFlag(r,kp,h), hshr0(r,kp,h)*eh0(kp,r)) - (bh0(k,r)-1)
                + sum(kp$xcFlag(r,kp,h), hshr0(r,kp,h)*bh0(kp,r))) ;
) ;

kron(k,k) = 1 ;

*  Non-energy demand

pcnnrg0(r,k,h) = 1 ;
xcnnrg0(r,k,h) = sum(i$(not e(i)), cmat(i,k,r)) / pcnnrg0(r,k,h) ;

pcnrg0(r,k,h)  = 1 ;
xcnrg0(r,k,h)  = sum(e, cmat(e,k,r)) / pcnrg0(r,k,h) ;

loop(t0,
   xcnnrgFlag(r,k,h)$xcnnrg0(r,k,h) = 1 ;
   xcnrgFlag(r,k,h)$xcnrg0(r,k,h)   = 1 ;
) ;

pacNRG0(r,k,h,NRG) = 1 ;
xacNRG0(r,k,h,NRG) = sum(e$mape(NRG,e), cmat(e,k,r)) / pacNRG0(r,k,h,NRG) ;
pcolg0(r,k,h)      = 1 ;
xcolg0(r,k,h)      = (pacNRG0(r,k,h,"GAS")*xacNRG0(r,k,h,"GAS")
                   +  pacNRG0(r,k,h,"OIL")*xacNRG0(r,k,h,"OIL")) / pcolg0(r,k,h) ;
pcnely0(r,k,h)     = 1 ;
xcnely0(r,k,h)     = (pacNRG0(r,k,h,"COA")*xacNRG0(r,k,h,"COA")
                   +  pcolg0(r,k,h)*xcolg0(r,k,h)) / pcnely0(r,k,h) ;

loop(t0,
   xcnelyFlag(r,k,h)$xcnely0(r,k,h) = 1 ;
   xcolgFlag(r,k,h)$xcolg0(r,k,h)   = 1 ;
   xacNRGFlag(r,k,h,NRG)$xacNRG0(r,k,h,NRG) = 1 ;
) ;

*  Waste module

alphawc(r,i,h)$(alphawc(r,i,h) eq na or alphawc(r,i,h) eq 0) = 0 ;
loop(t0,
   hWasteFlag(r,i,h)$(alphawc(r,i,h) ne 0 and xa0(r,i,h)) = 1 ;
) ;

shr0_ac(r,i,h)$hWasteFlag(r,i,h)
                  = (1 - alphawc(r,i,h)) ;
shr0_wc(r,i,h)    = 1 - shr0_ac(r,i,h) ;
paac0(r,i,h)      = pa0(r,i,h) ;
pawc0(r,i,h)      = pa0(r,i,h) ;
xawc0(r,i,h)$xa0(r,i,h)
                  = alphawc(r,i,h)*xa0(r,i,h) ;
xaac0(r,i,h)      = xa0(r,i,h) - xawc0(r,i,h) ;
paacc0(r,i,h)     = pa0(r,i,h) ;
lambdaac(r,i,h,t) = 1 ;
lambdawc(r,i,h,t) = 1 ;

phhTaxFlag(r) = no ;
iphh(r,i)     = no ;
phhTax.fx(r,i,t) = 0 ;

* display hWasteFlag, xa0, xaac0, xawc0, paac0, pawc0, paacc0 ;

* --------------------------------------------------------------------------------------------------
*
*  Initialize bilateral trade
*
* --------------------------------------------------------------------------------------------------

xw0(s,i,d)   = inscale*sum(i0$mapi0(i0,i),vxsb(i0, s, d)) / pe0(s,i,d) ;
xwFlag(s,i,d)$xw0(s,i,d) = 1 ;
etax0(s,i,d) = inscale*(sum(i0$mapi0(i0,i),vfob(i0, s, d))
             - sum(i0$mapi0(i0,i),vxsb(i0, s, d))) ;
etax0(s,i,d)$xwFlag(s,i,d) = etax0(s,i,d)/(pe0(s,i,d)*xw0(s,i,d)) ;

*  FOB price equals producer price plus export tax/subsidy

pwe0(s,i,d)  = (1+etax0(s,i,d))*pe0(s,i,d) ;

*  CIF/FOB margins

tmarg0(s,i,d) = inscale*(sum(i0$mapi0(i0,i),vcif(i0, s, d))
              - sum(i0$mapi0(i0,i),vfob(i0, s, d))) ;
tmarg0(s,i,d)$xw0(s,i,d) = tmarg0(s,i,d)/(pwmg0(s,i,d)*xw0(s,i,d)) ;

*  CIF price equals FOB price plus margin

pwm0(s,i,d) = pwe0(s,i,d) + pwmg0(s,i,d)*tmarg0(s,i,d) ;

*  Import tariff

mtax0(s,i,d)   = inscale*(sum(i0$mapi0(i0,i), vmsb(i0, s, d))
               - sum(i0$mapi0(i0,i), vcif(i0, s, d))
               - sum(i0$mapi0(i0,i), vntm(i0, s, d))) ;
ntmAVE0(s,i,d) = inscale*sum(i0$mapi0(i0,i), vntm(i0, s, d)) ;
ntmY0(d)       = sum((i,s), ntmAVE0(s,i,d)) ;

mtax0(s,i,d)$xw0(s,i,d)   = mtax0(s,i,d) / (pwm0(s,i,d)*xw0(s,i,d)) ;
ntmAVE0(s,i,d)$xw0(s,i,d) = ntmAVE0(s,i,d) / (pwm0(s,i,d)*xw0(s,i,d)) ;

* display ntmAVE0, mtax0 ;

*  Default to NTM revenues go to importing government

chigNTM(s,d,t) = 0 ;
chihNTM(s,d,t) = 0 ;
chigNTM(r,r,t) = 1 ;

*  End-user price of imports

pdm0(s,i,d) = (1 + mtax0(s,i,d) + ntmAVE0(s,i,d))*pwm0(s,i,d) ;
xmt0(r,i)   = sum(s, pdm0(s,i,r)*xw0(s,i,r)) / pmt0(r,i) ;

$iftheni "%MRIO_MODULE%" == "ON"

   if(not ArmFlag,

      put screen ; put / ;
      put ">>>> Terminating simulation..." / ;
      put ">>>> MRIO requested but ArmFlag = 0, set ArmFlag to 1 in 'Opt' file." / / ;
      putclose screen ;
      Abort "MRIO requested but ArmFlag = 0, set ArmFlag to 1 in 'Opt' file" ;

   ) ;

   scalar ifxwCSV / 0 / ;
   file xwcsv / xw.csv / ;
   if(ifxwCSV,
      put xwcsv ;
      put "Var,Importer,Commodity,Exporter,Agent,Value" / ;
      xwcsv.pc=5 ;
      xwcsv.nd=9 ;
   ) ;

   $$ifthen exist "%BASENAME%MRIO.gdx"

      viums(i,amrio,s,d) = sum(i0$mapi0(i0,i), viums0(i0,amrio,s,d)) ;
      viuws(i,amrio,s,d) = sum(i0$mapi0(i0,i), viuws0(i0,amrio,s,d)) ;

      mtaxa0(s,i,d,a)$viuws(i,"INT",s,d)    = viums(i,"INT",s,d)/viuws(i,"INT",s,d) - 1 ;
      mtaxa0(s,i,d,h)$viuws(i,"CONS",s,d)   = viums(i,"CONS",s,d)/viuws(i,"CONS",s,d) - 1 ;
      mtaxa0(s,i,d,gov)$viuws(i,"CONS",s,d) = viums(i,"CONS",s,d)/viuws(i,"CONS",s,d) - 1 ;
$iftheni "%RD_MODULE%" == "ON"
      mtaxa0(s,i,d,r_d)$viuws(i,"CONS",s,d) = viums(i,"CONS",s,d)/viuws(i,"CONS",s,d) - 1 ;;
$endif
      mtaxa0(s,i,d,inv)$viuws(i,"CGDS",s,d) = viums(i,"CGDS",s,d)/viuws(i,"CGDS",s,d) - 1 ;

      pdma0(s,i,d,aa)  = (1 + mtaxa0(s,i,d,aa))*pwm0(s,i,d) ;

*     Value share of intermediate trade from region 's' into region 'd'

      xwa0(s,i,d,a)$sum(r, VIUMS(i,"INT",r,d)) = VIUMS(i,"INT",s,d)/sum(r, VIUMS(i,"INT",r,d)) ;
      xwa0(s,i,d,a) = xwa0(s,i,d,a)*xm0(d,i,a)*pmt0(d,i)/ pdma0(s,i,d,a) ;

*     Value share of 'cons' trade from region 's' into region 'd'
      xwa0(s,i,d,h)$sum(r, VIUMS(i,"CONS",r,d))
         = VIUMS(i,"CONS",s,d)/sum(r, VIUMS(i,"CONS",r,d)) ;
      xwa0(s,i,d,h) = xwa0(s,i,d,h)*xm0(d,i,h)*pmt0(d,i)/ pdma0(s,i,d,h) ;
      xwa0(s,i,d,gov)$sum(r, VIUMS(i,"CONS",r,d))
         = VIUMS(i,"CONS",s,d)/sum(r, VIUMS(i,"CONS",r,d)) ;
      xwa0(s,i,d,gov) = xwa0(s,i,d,gov)*xm0(d,i,gov)*pmt0(d,i)/ pdma0(s,i,d,gov) ;
$iftheni "%RD_MODULE%" == "ON"
      xwa0(s,i,d,r_d)$sum(r, VIUMS(i,"CONS",r,d)) = VIUMS(i,"CONS",s,d)/sum(r, VIUMS(i,"CONS",r,d)) ;
      xwa0(s,i,d,r_d) = xwa0(s,i,d,r_d)*xm0(d,i,r_d)*pmt0(d,i)/ pdma0(s,i,d,r_d) ;
$endif

*     Value share of 'inv' trade from region 's' into region 'd'
      xwa0(s,i,d,inv)$sum(r, VIUMS(i,"CGDS",r,d))
         = VIUMS(i,"CGDS",s,d)/sum(r, VIUMS(i,"CGDS",r,d)) ;
      xwa0(s,i,d,inv) = xwa0(s,i,d,inv)*xm0(d,i,inv)*pmt0(d,i)/ pdma0(s,i,d,inv) ;

      pma0(r,i,aa) = (sum(s, pdma0(s,i,r,aa)*xwa0(s,i,r,aa)) / xm0(r,i,aa))$xm0(r,i,aa)
                   + 1$(xm0(r,i,aa) eq 0) ;

   $$else

*     Initialize MRIO assuming uniformity across agents

      mtaxa0(s,i,r,aa) = mtax0(s,i,r) ;
      pdma0(s,i,r,aa)  = (1 + mtaxa0(s,i,r,aa))*pwm0(s,i,r) ;
*     Initialize agent-specific bilateral trade to share of aggregatate imports from region s
      xwa0(s,i,r,aa)$xmt0(r,i) = pdm0(s,i,r)*xw0(s,i,r)
                               / sum(rp, pdm0(rp,i,r)*xw0(rp,i,r)) ;
      xwa0(s,i,r,aa) = xwa0(s,i,r,aa)*xm0(r,i,aa)*pmt0(r,i)
                     / pdma0(s,i,r,aa) ;
      pma0(r,i,aa) = (sum(s, pdma0(s,i,r,aa)*xwa0(s,i,r,aa)) / xm0(r,i,aa))$xm0(r,i,aa)
                   + 1$(xm0(r,i,aa) eq 0) ;


   $$endif

   xwaFlag(s,i,r,aa)$xwa0(s,i,r,aa) = 1 ;

   if(ifxwCSV,
      loop((d,i),
         loop(s,
            put "xw", d.tl, i.tl, s.tl, "Tot", (xw0(s,i,d)/inscale) / ;
            put "pwm", d.tl, i.tl, s.tl, "Tot", pwm0(s,i,d) / ;
            put "pdm", d.tl, i.tl, s.tl, "Tot", pdm0(s,i,d) / ;
            put "mtax", d.tl, i.tl, s.tl, "Tot", mtax0(s,i,d) / ;
            loop(aa,
               put "xwa",  d.tl, i.tl, s.tl, aa.tl, (xwa0(s,i,d,aa)/inscale) / ;
               put "pdma", d.tl, i.tl, s.tl, aa.tl, (pdma0(s,i,d,aa)) / ;
               put "mtaxa", d.tl, i.tl, s.tl, aa.tl, (mtaxa0(s,i,d,aa)) / ;
            ) ;
            if(1,
               loop(amrio,
                  put "VIUMS", d.tl, i.tl, s.tl, amrio.tl, (VIUMS(i,amrio,s,d)) / ;
                  put "VIUWS", d.tl, i.tl, s.tl, amrio.tl, (VIUWS(i,amrio,s,d)) / ;
               ) ;
            ) ;
         ) ;
         put "xmt", d.tl, i.tl, "Tot", "Tot", (xmt0(d,i)/inscale) / ;
         put "pmt", d.tl, i.tl, "Tot", "Tot", (pmt0(d,i)) / ;
         loop(aa,
            put "xm", d.tl, i.tl, "Tot", aa.tl, (xm0(d,i,aa)/inscale) / ;
            put "pmt", d.tl, i.tl, "Tot", aa.tl, (pmt0(d,i)) / ;
            put "pma", d.tl, i.tl, "Tot", aa.tl, (pma0(d,i,aa)) / ;
         ) ;
      ) ;
      abort "Temp" ;
   ) ;
$endif

* --------------------------------------------------------------------------------------------------
*
*  Initialize domestic supply of international trade and transport services
*
* --------------------------------------------------------------------------------------------------

xtt0(r,i) = inscale*sum(i0$mapi0(i0,i), vst(i0,r))/pdt0(r,i) ;

* --------------------------------------------------------------------------------------------------
*
*  Initialize domestic closure
*
* --------------------------------------------------------------------------------------------------

deprY0(r)         = inscale*vdep(r) ;
remit0(s,l,d)     = inscale*remit00(l, s, d) ;
yqtf0(r)          = inscale*yqtf0(r) ;
yqht0(r)          = inscale*yqht0(r) ;
trustY0           = sum(r, yqtf0(r)) ;

ODAIn0(r)         = inscale*ODAIn0(r) ;
ODAOut0(r)        = inscale*ODAOut0(r) ;
ODAGbl0           = sum(r, ODAIn0(r)) ;

itransfers.fx(r,t) = 0 ;
evRatioFlag(r,ra)  = no ;
betaEV.l(r,t)      = 1 ;
iTransferFlag(ra)  = no ;

pnum0  = 1 ;
pwsav0 = pnum0 ;
pmuv0  = pnum0 ;
pwgdp0 = pnum0 ;

savf0(r)  = (sum((i,s), pwm0(s,i,r)*xw0(s,i,r))
          -    sum((i,d), pwe0(r,i,d)*xw0(r,i,d))
          -    sum(img, pdt0(r,img)*xtt0(r,img))
          -    (sum((l,d), remit0(r,l,d) - remit0(d,l,r)))
          -    (yqht0(r) - yqtf0(r))
          -    (ODAIn0(r) - ODAOut0(r)) ) / pwsav0 ;

savg0(r)   = 0 ;
pgdpmp0(r) = 1 ;
rsg0(r)    = savg0(r) / pgdpmp0(r) ;

gdpmp0(r) = sum(fd, yfd0(r,fd))
          + sum((i,d), pwe0(r,i,d)*xw0(r,i,d) - pwm0(d,i,r)*xw0(d,i,r))
          + sum(img, pdt0(r,img)*xtt0(r,img)) ;

savfRat0(r) = savf0(r)/gdpmp0(r) ;

*  Override of capital stock data and depreciation rates
*  kadj0 = 1, or comes from the macro model if the 'InvTgt' file is present

kstock0(r) = inscale*vkb(r)*kadj0(r)
           * (cap_out_Ratio0(r)$(cap_out_Ratio0(r) ne na)
           +  1$(cap_out_Ratio0(r) eq na)) ;

depr(r,t) = (deprT(r,t))$(deprT(r,t) ne na)
          + (deprY0(r)/sum(inv, pfd0(r,inv)*kstock0(r)))$(deprT(r,t) eq na)
          ;

fdepr(r,t) = depr(r,t) ;
deprY0(r)  = sum((inv,t0), fdepr(r,t0)*pfd0(r,inv)*kstock0(r)) ;

loop((h,inv),
   savh0(r,h) = yfd0(r,inv) - deprY0(r) - pwsav0*savf0(r) - savg0(r) ;
) ;
ev0(r,h)     = yfd0(r,h) + savh0(r,h)$(%utility% eq ELES) ;
evBaU(r,h,t) = ev0(r,h) ;

* --------------------------------------------------------------------------------------------------
*
*  Initialize income variables
*
* --------------------------------------------------------------------------------------------------

*  Factor income taxes

kappaf0(r,f,a)$xf0(r,f,a)
   = 1 - inscale*sum(a0$mapa0(a0,a), evos(f,a0,r)) / (pf0(r,f,a)*xf0(r,f,a)) ;

yh0(r) = sum((f,a)$xfFlag(r,f,a), (1-kappaf0(r,f,a))*pf0(r,f,a)*xf0(r,f,a))
       -    sum((l,s), remit0(s,l,r))  +  sum((l,d), remit0(r, l, d))
       -    yqtf0(r) + yqht0(r) - deprY0(r)
       ;

loop(h,
   kappah0(r)   = (yh0(r) - savh0(r,h) - yfd0(r,h))/yh0(r) ;
   yd0(r)       = (1 - kappah0(r))*yh0(r) ;
   aps0(r,h)    = savh0(r,h) / yd0(r) ;
   yc0(r,h)     = yd0(r) - savh0(r,h) ;
   chiaps0(r,h) = aps0(r,h) ;
   chiAPSBaU(r,h,t) = chiAPS0(r,h) ;
) ;

ygov0(r,gy) = (sum((a,i), ptax0(r,a,i)*p0(r,a,i)*x0(r,a,i)
            +   sum(v, uc0(r,a)*uctax0(r,a)*xpv0(r,a))))$ptx(gy)
            +  (sum((f,a)$xfFlag(r,f,a), pftax0(r,f,a)*pf0(r,f,a)*xf0(r,f,a)))$vtx(gy)
            +  (sum((i,aa)$xaFlag(r,i,aa),
                     paTax0(r,i,aa)*gamma_eda(r,i,aa)*pat0(r,i)*xa0(r,i,aa)))$itx(gy)
            +  (sum((i,s)$xwFlag(s,i,r),
                     mtax0(s,i,r)*pwm0(s,i,r)*xw0(s,i,r)))$mtx(gy)
            +  (sum((i,d)$xwFlag(r,i,d),
                     etax0(r,i,d)*pe0(r,i,d)*xw0(r,i,d)))$etx(gy)
            +  (sum((f,a), kappaf0(r,f,a)*pf0(r,f,a)*xf0(r,f,a))
            +   kappah0(r)*yh0(r))$dtx(gy)
            +  (0)$ctx(gy)
            ;

* --------------------------------------------------------------------------------------------------
*
*  Initialize trade block
*
* --------------------------------------------------------------------------------------------------

*  Top level Armington

xat0(r,i) = sum(aa, gamma_eda(r,i,aa)*xa0(r,i,aa)) ;
xet0(r,i) = sum(d, pe0(r,i,d)*xw0(r,i,d)) / pet0(r,i) ;
xdt0(r,i) = (ps0(r,i)*xs0(r,i) - pet0(r,i)*xet0(r,i))/pdt0(r,i) ;
xdt0(r,i)$(xdt0(r,i) lt 0) = 0 ;

xatFlag(r,i)$xat0(r,i) = 1 ;
xddFlag(r,i)$((xdt0(r,i) - xtt0(r,i)) gt 0) = 1 ;
xmtFlag(r,i)$xmt0(r,i) = 1 ;

xdtFlag(r,i)$xdt0(r,i) = 1 ;
xetFlag(r,i)$xet0(r,i) = 1 ;

* --------------------------------------------------------------------------------------------------
*
*  Initialize trade margins block
*
* --------------------------------------------------------------------------------------------------

alias(i0,j0) ;

xtmg0(img)         = sum(r, pdt0(r,img)*xtt0(r,img)) / ptmg0(img) ;
xmgm0(img,s,i,d)   = inscale*sum((j0,i0)$(mapi0(j0,img) and mapi0(i0,i)), VTWR(j0, i0, s, d))
                   / ptmg0(img) ;
xwmg0(s,i,d)       = tmarg0(s,i,d)*xw0(s,i,d) ;
lambdamg(img,s,i,d,t) = 1 ;

xttFlag(r,img)$xtt0(r,img)  = 1 ;
tmgFlag(s,i,d)$xwmg0(s,i,d) = 1 ;

* --------------------------------------------------------------------------------------------------
*
*  Initialize factor supply block
*
* --------------------------------------------------------------------------------------------------

lsFlag(r,l,z)$(lsFlag(r,l,z) and sum(a$mapz(z,a), xf0(r,l,a)) = 0) = no ;
ueFlag(r,l,z)$(ueFlag(r,l,z) ne fullEmpl and not lsFlag(r,l,z)) = fullEmpl ;
awagez0(r,l,z) = sum(a$mapz(z,a), xf0(r,l,a)) ;
awagez0(r,l,z)$awagez0(r,l,z) = sum(a$mapz(z,a), pf0(r,l,a)*xf0(r,l,a))
                              / awagez0(r,l,z) ;
twage0(r,l) = sum(a, pf0(r,l,a)*xf0(r,l,a))/sum(a, xf0(r,l,a)) ;

uez.l(r,l,z,t)$(ueFlag(r,l,z) ne fullEmpl) = uez0(r,l,z) ;
urbPrem0(r,l)                = sum(rur, (1-uez0(r,l,rur))*awagez0(r,l,rur)) ;
urbPrem0(r,l)$urbPrem0(r,l)  = sum(urb, (1-uez0(r,l,urb))*awagez0(r,l,urb)) / urbPrem0(r,l) ;

ldz0(r,l,z)$lsFlag(r,l,z) = sum(a$mapz(z,a), xf0(r,l,a)) ;
lsz0(r,l,z)$lsFlag(r,l,z) = ldz0(r,l,z)/(1 - uez0(r,l,z)) ;
ls0(r,l)                  = sum(z$lsFlag(r,l,z), lsz0(r,l,z)) ;
tls0(r)                   = sum(l, ls0(r,l)) ;
tlabFlag(r,l)$ls0(r,l)    = 1 ;

migrFlag(r,l) = no ;
migrFlag(r,l)$(omegam(r,l) ne inf) = yes ;

loop(rur,
   migr0(r,l)$migrFlag(r,l) = migr0(r,l)*lsz0(r,l,rur) ;
) ;
migrmult.l(r,l,z,t) = 1 ;

*  Check migration assumptions

work = 0 ;
loop((r,l)$(omegam(r,l) ne inf),
   if(ifLSeg(r,l) eq 0,
      if(work eq 0,
         put screen ;
         put / ;
         put 'Invalid assumption(s) for labor market segmentation: ' / ;
         work = 1 ;
      ) ;
      put '   ', r.tl:<12, 'omegam = ', omegam(r,l):10:2, ' but no labor market segmentation.' / ;
   ) ;
) ;
if(work, Abort$(1) "Check labor market assumptions" ; ) ;

ewagez0(r,l,z)$lsFlag(r,l,z) = awagez0(r,l,z) ;
rwage0(r,l,z)     = ewagez0(r,l,z) ;
rwageBaU(r,l,z,t) = 1 ;
ldzBaU(r,l,z,t)   = 1 ;

loop(mapz(z,a),
   wPrem.l(r,l,a,t)$lsFlag(r,l,z) = pf0(r,l,a)/ewagez0(r,l,z) ;
) ;

resWage0(r,l,z)$(resWage0(r,l,z) eq 0 or resWage0(r,l,z) eq na) = 0.1 ;
resWage0(r,l,z)$(ueFlag(r,l,z) eq resWageUE)
   = resWage0(r,l,z)*ewagez0(r,l,z) ;
chirw0(r,l,z)$(ueFlag(r,l,z) eq resWageUE)
   = resWage0(r,l,z)
   *  ((1+0)**omegarwg(r,l,z))
   *  ((1)**omegarwue(r,l,z))
   *  (1**omegarwp(r,l,z))
   ;

ueMinz(r,l,z,t)$(ueFlag(r,l,z) eq resWageUE) = ueMinz0(r,l,z) ;
uez.lo(r,l,z,t)$(ueFlag(r,l,z) eq resWageUE) = ueMinz0(r,l,z) ;

glab.l(r,l,t)       = 0.0 ;
glabz.l(r,l,z,t)    = 0.0 ;

skillprem0(r,l)$ls0(r,l) = (sum(lr, twage0(r,lr)*ls0(r,lr))
                         /  sum(lr, ls0(r,lr)))/twage0(r,l) - 1 ;

tkaps0(r) = sum((a), pk0(r,a)*kv0(r,a)) / trent0(r) ;

chiKaps0(r)  = tkaps0(r)/kstock0(r) ;
k00(r,a)     = kv0(r,a) ;
rrat0(r,a)   = 1 ;
invGFact0(r) = 20 ;

*  !!!! NEEDS to be reviewed if we have the right price/volume split
*       NEED to add wedges if we allow for infinite transformation

tland0(r) = sum((lnd,a), pf0(r,lnd,a)*xf0(r,lnd,a))/ptland0(r) ;
tlandFlag(r)$tland0(r) = 1 ;

xlb0(r,lb) = sum((lnd,a)$maplb(lb,a), xf0(r,lnd,a)) ;
plb0(r,lb)$xlb0(r,lb) =
   sum((lnd,a)$maplb(lb,a), pf0(r,lnd,a)*xf0(r,lnd,a))/xlb0(r,lb) ;
plbndx0(r,lb) = plb0(r,lb) ;

* display tland, xlb, maplb ;

xnlb0(r)          = sum(lb$(not lb1(lb)), xlb0(r,lb)) ;
pnlb0(r)$xnlb0(r) = sum(lb$(not lb1(lb)), plb0(r,lb)*xlb0(r,lb))/xnlb0(r) ;
pnlb0(r)$(xnlb0(r) eq 0) = 1 ;
pnlbndx0(r)       = pnlb0(r) ;

tland0(r)            = sum(lb1, xlb0(r,lb1)) + xnlb0(r) ;
ptland0(r)$tland0(r) = (sum(lb1, plb0(r,lb1)*xlb0(r,lb1)) + pnlb0(r)*xnlb0(r))/tland0(r) ;
ptlandndx0(r)        = ptland0(r) ;

landmax.fx(r,t) = landMax0(r)*tland0(r) ;

* --------------------------------------------------------------------------------------------------
*
*  Water supply module
*
* --------------------------------------------------------------------------------------------------

h2obnd0(r,wbnd) = watScale*h2oUse(wbnd, r)$IFWATER ;

*  !!!! FOR THE MOMENT, assume water price is uniform across broad aggregates

ph2obnd0(r,wbnd) = sum((wat,acr), xf0(r,wat,acr)) ;
ph2obnd0(r,wbnd)$ph2obnd0(r,wbnd)
   = sum((wat,acr), pf0(r,wat,acr)*xf0(r,wat,acr)) / ph2obnd0(r,wbnd) ;
ph2obndndx0(r,wbnd) = ph2obnd0(r,wbnd) ;

*  Build the nested bundles

h2obnd0(r,wbnd1) = sum(wbnd2$mapw1(wbnd1, wbnd2), h2obnd0(r,wbnd2)) ;
ph2obnd0(r,wbnd1)$h2obnd0(r,wbnd1) =
   sum(wbnd2$mapw1(wbnd1, wbnd2), ph2obnd0(r,wbnd2)*h2obnd0(r,wbnd2))/h2obnd0(r,wbnd1) ;

th2om0(r) = sum(wbnd1, h2obnd0(r,wbnd1)) ;
pth2o0(r)$th2om0(r) =
   sum(wbnd1, ph2obnd0(r,wbnd1)*h2obnd0(r,wbnd1))/th2om0(r) ;
th2o0(r) = th2om0(r) + sum(wbnd$wbndEx(wbnd), h2obnd0(r,wbnd)) ;
pth2ondx0(r) = pth2o0(r) ;

h2oMax.fx(r,t) = h2oMax0(r)*th2o0(r) ;

th2oFlag(r)$th2o0(r) = 1 ;
h2obndFlag(r,wbnd)$h2obnd0(r,wbnd) = 1 ;
h2obndFlag(r,wbndEx) = 0 ;

*  !!!! This might need revision if the matrix is not diagonal or if we have
*       an independent source for production volumes

xp0(r,a)  = sum(i, p0(r,a,i)*x0(r,a,i))/px0(r,a) ;
pxv0(r,a) = px0(r,a) ;
uc0(r,a)  = pxv0(r,a)/(1 + uctax0(r,a)) ;
xpv0(r,a) = xp0(r,a) ;
xpx0(r,a) = (xp0(r,a)*uc0(r,a) - pxghg0(r,a)*xghg0(r,a)) /  pxp0(r,a) ;

*  !!!! NEED TO RESOLVE

$ontext
put screen ;
put / ;
loop((r,a)$(xp0(r,a) < 0),
   put r.tl / ;
   put uc0(r,a), px0(r,a), pp0(r,a,i), xpFlag(r,a), xp0(r,a):15:8 / ;
) ;
loop((r,i)$(xs0(r,i) < 0),
   put r.tl / ;
   put xsFlag(r,i), xs0(r,i):15:8 / ;
) ;
abort "Temp" ;

*  !!!! Temporary fix to energy problem -- quite dangerous

loop(t0,
   xpFlag(r,a)$(xp0(r,a) < 0) = 0 ;
   ghgFlag(r,a)$(xpFlag(r,a) = 0) = 0 ;
   nd1Flag(r,a)$(xpFlag(r,a) = 0) = 0 ;
   nd2Flag(r,a)$(xpFlag(r,a) = 0) = 0 ;
   lab1Flag(r,a)$(xpFlag(r,a) = 0) = 0 ;
   lab2Flag(r,a)$(xpFlag(r,a) = 0) = 0 ;
   kFlag(r,a)$(xpFlag(r,a) = 0) = 0 ;
   kfFlag(r,a)$(xpFlag(r,a) = 0) = 0 ;
   kefFlag(r,a)$(xpFlag(r,a) = 0) = 0 ;
   va1Flag(r,a)$(xpFlag(r,a) = 0) = 0 ;
   va2Flag(r,a)$(xpFlag(r,a) = 0) = 0 ;
   xnrgFlag(r,a)$(xpFlag(r,a) = 0) = 0 ;
   xwatfFlag(r,a)$(xpFlag(r,a) = 0) = 0 ;
   xaFlag(r,i,a)$(xpFlag(r,a) = 0) = 0 ;
   xnelyFlag(r,a)$(xpFlag(r,a) = 0) = 0 ;
   xolgFlag(r,a)$(xpFlag(r,a) = 0) = 0 ;
   xaNRGFlag(r,a,NRG)$(xpFlag(r,a) = 0) = 0 ;
) ;
$offtext

*  Price electricity consumption in gwhr

put screen ;

if(IFPOWER,

   gwhr(r,elya) = inscale*sum(a0$mapa0(a0,elya), gwhr0(a0,r)) ;

*  Reprice make matrix in terms of $ per gwhr

   loop(t0$(1),

*     !!!! Check: it is apparently possible to have px*xp = 0 and not gwhr!

      loop((r,elya,elyc),
         if(px0(r,elya)*xp0(r,elya) eq 0 and gwhr(r,elya) ne 0,
            put "XP=0, GWHR<>0: ", r.tl, elya.tl, (gwhr(r,elya)/inscale):15:4 / ;
         ) ;
         if(px0(r,elya)*xp0(r,elya) ne 0 and gwhr(r,elya) eq 0,
            put "XP<>0, GWHR=0: ", r.tl, elya.tl, (px0(r,elya)*xp0(r,elya)/inscale):15:4 / ;
         ) ;
      ) ;

      loop(elya,
*        Price per Gwhr
         rwork(r)$gwhr(r,elya) = (px0(r,elya)*xp0(r,elya))/gwhr(r,elya) ;
*        display rwork ;
         xp0(r,elya)$rwork(r)  = (px0(r,elya)*xp0(r,elya))/rwork(r) ;
         px0(r,elya)           = rwork(r)$xp0(r,elya)
                               + 1$(xp0(r,elya) eq 0) ;
      ) ;
*     !!!! Caution here...
      pxv0(r,elya) = px0(r,elya) ;
      uc0(r,elya)  = px0(r,elya) ;
      xpv0(r,elya) = xp0(r,elya) ;

*     !!!! Assume each power activity produces only one commodity
      x0(r,elya,elyc)  = xp0(r,elya) ;
      p0(r,elya,elyc)  = px0(r,elya) ;
      pp0(r,elya,elyc) = p0(r,elya,elyc)*(1+ptax0(r,elya,elyc)) ;

*     !!!! Reprice electricity supply

      xs0(r,elyc) = sum(elya, x0(r,elya,elyc)) ;
      ps0(r,elyc) = (sum(elya, pp0(r,elya,elyc)*x0(r,elya,elyc)) / xs0(r,elyc))
                     $     xs0(r,elyc)
                     + 1$(xs0(r,elyc) eq 0) ;

*     Gamma's in effect convert supply from Gwhr to MTOE

      gamma_esd(r,elyc) = pdt0(r,elyc)/ps0(r,elyc) ;
      gamma_ese(r,elyc) = pet0(r,elyc)/ps0(r,elyc) ;
   ) ;
) ;

putclose screen ;

*  Initialization of power nesting

if(IFPOWER,

*  Create the power bundles and reprice 'etd'

   loop((r,elyc),

      xpb0(r,pb,elyc)    = sum(elya$mappow(pb,elya), x0(r,elya,elyc)) ;
      ppb0(r,pb,elyc)    = (sum(elya$mappow(pb,elya), pp0(r,elya,elyc)*x0(r,elya,elyc))
                         / xpb0(r,pb,elyc))$xpb0(r,pb,elyc)
                         + 1$(xpb0(r,pb,elyc) eq 0) ;
      ppbndx0(r,pb,elyc) = ppb0(r,pb,elyc) ;
      xpow0(r,elyc)      = sum(pb, xpb0(r,pb,elyc)) ;
      ppow0(r,elyc)      = (sum(pb, ppb0(r,pb,elyc)*xpb0(r,pb,elyc))
                         / xpow0(r,elyc))$xpow0(r,elyc)
                         + 1$(xpow0(r,elyc) eq 0) ;
      ppowndx0(r,elyc)   = ppow0(r,elyc) ;
*     Reprice etd services
*     !!!! Check taxes
      shr0_s(r,etd,elyc) = pp0(r,etd,elyc)*x0(r,etd,elyc) ;
      x0(r,etd,elyc)     = x0(r,etd,elyc) ;
      pp0(r,etd,elyc)    = (shr0_s(r,etd,elyc)/x0(r,etd,elyc))
                         $x0(r,etd,elyc)
                         + 1
                         $(x0(r,etd,elyc) eq 0) ;
      p0(r,etd,elyc)     = pp0(r,etd,elyc)/(1+ptax0(r,etd,elyc)) ;
   ) ;

   phTaxpb.fx(r,pb,elyc,t) = 0 ;
   phtaxpbY.fx(r,t)        = 0 ;
   chiphpb.fx(r,t)         = 0 ;
   phTaxpbFlag(r)          = no ;
   iphpb(r,pb)             = no ;

) ;

scalar ifpowcsv / 0 / ;
file fpow / pow.csv / ;
if(ifpowcsv,
   put fpow ;
   put "Var,Region,Activity,Commodity,Value" / ;
   fpow.pc=5 ;
   fpow.nd=9 ;
   loop((r,elya,elyc),
      put "x", r.tl, elya.tl, elyc.tl, (x0(r,elya,elyc)/inscale) / ;
      put "p", r.tl, elya.tl, elyc.tl, (p0(r,elya,elyc)) / ;
      put "pp", r.tl, elya.tl, elyc.tl, (pp0(r,elya,elyc)) / ;
      put "ptax", r.tl, elya.tl, elyc.tl, (ptax0(r,elya,elyc)) / ;
   ) ;
   loop((r,elya),
      put "xp", r.tl, elya.tl, "", (xp0(r,elya)/inscale) / ;
      put "px", r.tl, elya.tl, "", (px0(r,elya)) / ;
   ) ;
   loop((r,elyc),
      put "xs", r.tl, "", elyc.tl, (xs0(r,elyc)/inscale) / ;
      put "ps", r.tl, "", elyc.tl, (ps0(r,elyc)) / ;
   ) ;
) ;

kxRat0(r,a)$xpFlag(r,a) = kv0(r,a)/xpv0(r,a) ;

* --------------------------------------------------------------------------------------------------
*
*  Initialization of capital account module
*
* --------------------------------------------------------------------------------------------------

loop(t0,
   kstocke0(r) = (1-depr(r,t0))*kstock0(r) + sum(inv, xfd0(r,inv)) ;

   ror0(r)  = sum((a,cap),(1-kappaf0(r,cap,a))*pf0(r,cap,a)*xf0(r,cap,a))/kstock0(r) ;

   rorc0(r) = ror0(r)/sum(inv, pfd0(r,inv)) - depr(r,t0) ;

   rore0(r) = rorc0(r)*(kstocke0(r)/kstock0(r))**(-epsRor(r,t0)) ;

   rorg0    = (sum(r, rore0(r)*sum(inv, pfd0(r,inv)*(xfd0(r,inv) - depr(r,t0)*kstock0(r))))
            /  sum((d,inv), pfd0(d,inv)*(xfd0(d,inv) - depr(d,t0)*kstock0(d))))
            $(savfFlag ne capFlexUSAGE) ;

   riskPrem(r,t) = (rorg0 / rore0(r))$(savfFlag ne capFlexINF)
                 + (rorg0 / rorc0(r))$(savfFlag eq capFlexINF)
                 ;
) ;

savfBar(r,t)  = savf0(r) ;

*  Direct initialization

loop((inv,t0),
   grK.l(r,t)    = xfd0(r,inv)/kstock0(r) - depr(r,t0) ;
   if(savfFlag eq capFlexUSAGE,
      devRoR.l(r,t) = log(((grKMax(r,t0) - grKTrend(r,t0))/(grKTrend(r,t0) - grKMin(r,t0)))
                    * (grK.l(r,t0) - grKMin(r,t0))/(grKMax(r,t0) - grK.l(r,t0)))
                    / chigrK(r,t) ;
      rore0(r)      = (ror0(r)/pfd0(r,inv) + (1-depr(r,t0)))/1.05 - 1 ;
      rorg0         = 0 ;
      rord.l(r,t)   = rore0(r) - rorn(r,t) - rorg0 - devRor.l(r,t) ;
   ) ;
) ;

if(0,
   option decimals=6 ;
   display grk.l, devRoR.l, rore0, rorg0, rord.l,
   grkMin, grkMax, grkTrend, chigrK, rorn ;

   abort "Temp" ;
) ;

ifDepl(r,a) = no ;

$iftheni.CS "%simType%" == "RcvDyn"
$iftheni.DEPL "%DEPL_MODULE%" == "ON"

* --------------------------------------------------------------------------------------------------
*
*  Initialization of depletion module
*
* --------------------------------------------------------------------------------------------------

ifDsc(r,a)  = no ;

prat.l(r,a,t)     = 1 ;
ptrend(r,a,t)     = ptrend0(r,a,"ref") ;
omegard(r,a,pt,t) = omegard0(r,a,pt) ;
omegar.l(r,a,t)   = 0.5*(omegard(r,a,"lo",t) + omegard(r,a,"hi",t)) ;
omegae(r,a,t)     = omegae0(r,a) ;

loop((t0,nrs),
   ifDepl(r,affl)$xf0(r,nrs,affl) = yes ;
*  ifDisc(r,affl)$(ifDepl(r,affl) and reserv00('ref',r,affl,'ytd')) = yes ;
   ifDsc(r,affl)$(ifDepl(r,affl) and not sameas(affl,'coa-a')) = yes ;
) ;

dscRate.l(r,affl,t)$ifDsc(r,affl)  = dscRate0(r,affl,"ref") ;
chidscRate.l(r,a,t) = 1 ;
chiextRate.l(r,a,t) = 1 ;

ytdres0(r,a)  = rscale*sum((a0,pt)$(mapa0(a0,a) and dscScn0(r,a0,pt)), ytd0Adj(r,a0)*ytdreserves(r,a0,pt)) ;
res0(r,a)     = rscale*sum((a0,pt)$(mapa0(a0,a) and resScn0(r,a0,pt)), res0Adj(r,a0)*reserves(r,a0,pt)) ;
extr0(r,a)    = rscale*sum(a0$mapa0(a0,a), extraction(r,a0)) ;
extRate.l(r,a,t)$res0(r,a) = extr0(r,a)/res0(r,a) ;

loop(mapa0(a0,a),
*  Should only be for coal
   res0(r,a)$(res0(r,a) eq 0 and r_p(r,a0,"ref"))
      = sum((nrs,pt)$resScn0(r,a0,pt), xf0(r,nrs,a)*res0Adj(r,a0)*r_p(r,a0,pt)) ;
   extr0(r,a)$(extr0(r,a) eq 0 and res0(r,a)) = sum(nrs, xf0(r,nrs,a)) ;
   extRate.l(r,a,t)$(extRate.l(r,a,t) eq 0 and res0(r,a))
      = extr0(r,a) / res0(r,a) ;
) ;
ytdres0(r,a)$(ytdres0(r,a) eq 0) = 0 ;
cumExt0(r,a) = extr0(r,a) ;
resp0(r,a)   = res0(r,a) ;
resGap.l(r,a,t) = 0 ;
xfPot0(r,a)  = extr0(r,a) ;

ifDsc(r,a)  = no ;
ifDepl(r,a) = no ;

ifDsc(r,a)$(ytdres0(r,a)) = yes ;
ifDepl(r,a)$(res0(r,a)) = yes ;
$endif.DEPL
$endif.CS

* --------------------------------------------------------------------------------------------------
*
*  Initialization of knowledge module
*
* --------------------------------------------------------------------------------------------------

$iftheni "%RD_MODULE%" == "ON"
   valk(ky) = ord(ky) - 1 ;

   loop(r,
      if(KnowledgeData0(r,"delta") <> na,

         gamPrm(r,"delta")  = KnowledgeData0(r,"delta") ;
         gamPrm(r,"lambda") = KnowledgeData0(r,"lambda") ;
         gamPrm(r,"N")      = KnowledgeData0(r,"N") ;
         kdepr(r,t)         = 0.01*KnowledgeData0(r,"depr") ;

         gamma(r,ky)$(valk(ky) le gamPrm(r,"N"))
                  = ((valk(ky) + 1)**(gamPrm(r,"delta")/(1-gamPrm(r,"delta"))))
                  *   gamPrm(r,"lambda")**valk(ky) ;
         gamma(r,ky) = gamma(r,ky) / sum(kky, gamma(r,kky)) ;

         knowFlag(r) = yes ;

      else

         knowFlag(r) = no ;

      ) ;
   ) ;

   alias(l,lp) ;

   if(not ifDyn,
      knowFlag(r) = no ;
   ) ;

   loop(r,
      if(knowFlag(r),

*        Initialize the knowledge stock given initial R&D expenditures--assume steady-state

         kn0(r) = ((1 + 0.01*KnowledgeData0(r,"g0"))
                /  (0.01*KnowledgeData0(r,"g0") + 0.01*KnowledgeData0(r,"depr")))
                * (sum(ky$(valk(ky) le gamPrm(r,"N")), gamma(r,ky)/power(1 + 0.01*KnowledgeData0(r,"g0"), valk(ky)))) ;

         loop(r_d,
            kn0(r) = xfd0(r,r_d)*kn0(r) ;
         ) ;

*        Initialize R&D

         rd0(r) = sum(r_d, xfd0(r,r_d)) ;
         rd.l(r,t) = rd0(r) ;
         loop(t,
            rd.l(r,tt)$(years(tt) gt baseYear and years(tt) gt years(t-1) and years(tt) lt years(t))
               = (rd.l(r,t-1)**((years(t) - years(tt))/gap(t)))
               * (rd.l(r,t)**((gap(t) - (years(t) - years(tt)))/gap(t))) ;
         ) ;

*        Back cast R&D

         loop(t0,
            for(work = years(t0)-1 downto FirstYear by 1,
               loop(tt$(years(tt) eq work),
                  rd.fx(r,tt) = rd.l(r,tt+1) / (1 + 0.01*KnowledgeData0(r,"g0")) ;
               ) ;
            ) ;
         ) ;

*        Initialize the endogenous part of labor productivity

         if(ifSklBias,
*           !!!! Requires more thought, especially if there are multiple skill levels
            loop(l$lr(l),
               gammar(r,l,a,t) = KnowledgeData0(r,"gammar") ;
               loop((t0),
*                 Use average share of skill
                  work = sum((lp,a), pf0(r,lp,a)*xf0(r,lp,a)) ;
                  if(work ne 0,
                     work = sum(a, pf0(r,l,a)*xf0(r,l,a))/work ;
                     epsr(r,l,a,t)$work  = KnowledgeData0(r,"epsr")/work ;
                  ) ;
               ) ;
               pik.l(r,l,a,t)  = gammar(r,l,a,t)*epsr(r,l,a,t)*0.01*KnowledgeData0(r,"g0") ;
            ) ;
         else
            gammar(r,l,a,t) = KnowledgeData0(r,"gammar") ;
            epsr(r,l,a,t)   = KnowledgeData0(r,"epsr") ;
            pik.l(r,l,a,t)  = gammar(r,l,a,t)*epsr(r,l,a,t)*0.01*KnowledgeData0(r,"g0") ;
         ) ;

      else
         pik.fx(r,l,a,t) = 0 ;
      )
   ) ;

*  display epsr ;

$else

   knowFlag(r) = no ;
   pik.fx(r,l,a,t) = 0 ;

$endif

* --------------------------------------------------------------------------------------------------
*
*  Initialization of emissions module
*
* --------------------------------------------------------------------------------------------------

*  The 'm' variables are in millions of tons of CO2

*  Conversion factor for emissions

work = cscale*(1$(not ifCEQ) + (12/44)$ifCEQ) ;

*  Aggregate emissions

emird(r,"co2",i,a)   = work*(sum((i0,a0)$(mapi0(i0,i) and mapa0(a0,a)), mdf(i0,a0,r))) ;
emirm(r,"co2",i,a)   = work*(sum((i0,a0)$(mapi0(i0,i) and mapa0(a0,a)), mmf(i0,a0,r))) ;

emird(r,"co2",i,h)   = work*(sum(i0$mapi0(i0,i), mdp(i0,r))) ;
emirm(r,"co2",i,h)   = work*(sum(i0$mapi0(i0,i), mmp(i0,r))) ;

emird(r,"co2",i,gov) = work*(sum(i0$mapi0(i0,i), mdg(i0,r))) ;
emirm(r,"co2",i,gov) = work*(sum(i0$mapi0(i0,i), mmg(i0,r))) ;

emird(r,"co2",i,inv) = work*(sum(i0$mapi0(i0,i), mdi(i0,r))) ;
emirm(r,"co2",i,inv) = work*(sum(i0$mapi0(i0,i), mmi(i0,r))) ;

emir(r,"co2",i,aa)   = emird(r,"co2",i,aa) + emirm(r,"co2",i,aa) ;
emi0(r,"co2",i,aa)   = emir(r,"co2",i,aa) ;

*  Non-CO2 emissions are not source specific

loop(em$(ghg(em) and not sameas(em,"CO2")),
   emir(r,em,i,a)    = work*sum((i0,a0)$(mapi0(i0,i) and mapa0(a0,a)), nc_trad_ceq(em, i0, a0, r)) ;
   emir(r,em,fp,a)   = work*sum(a0$mapa0(a0,a), nc_endw_ceq(em, fp, a0, r)) ;
   emir(r,em,tot,a)  = work*sum(a0$mapa0(a0,a), nc_qo_ceq(em, a0, r)) ;
   emir(r,em,i,h)    = work*sum(i0$mapi0(i0,i), nc_hh_ceq(em, i0, r)) ;
) ;

loop(em$nghg(em),
   emir(r,em,i,a)    = work*sum((i0,a0)$(mapi0(i0,i) and mapa0(a0,a)), nc_trad(em, i0, a0, r)) ;
   emir(r,em,fp,a)   = work*sum(a0$mapa0(a0,a), nc_endw(em, fp, a0, r)) ;
   emir(r,em,tot,a)  = work*sum(a0$mapa0(a0,a), nc_qo(em, a0, r)) ;
   emir(r,em,i,h)    = work*sum(i0$mapi0(i0,i), nc_hh(em, i0, r)) ;
) ;

*  Calculate the coeffiecients

emi0(r,em,is,aa) = emir(r,em,is,aa) ;

*  Calibrate the emissions coefficients

emir(r,em,i,aa)$xa0(r,i,aa) = emi0(r,em,i,aa) / xa0(r,i,aa) ;

emird(r,"CO2",i,aa)$xd0(r,i,aa) = emird(r,"CO2",i,aa) / xd0(r,i,aa) ;
emirm(r,"CO2",i,aa)$xm0(r,i,aa) = emirm(r,"CO2",i,aa) / xm0(r,i,aa) ;

emird(r,emn,i,aa)$(xd0(r,i,aa) + xm0(r,i,aa))
   = emi0(r,emn,i,aa) / (xd0(r,i,aa) + xm0(r,i,aa)) ;
emirm(r,emn,i,aa) = emird(r,emn,i,aa) ;

emir(r,em,f,a)$xf0(r,f,a) = emi0(r,em,f,a) / xf0(r,f,a) ;
emir(r,em,tot,a)$xp0(r,a) = emi0(r,em,tot,a) / xp0(r,a) ;

gwp("co2") = 1 ;
if(ifNCO2,
   gwp(emn) = sum(r, sum((i,a), sum((i0,a0)$(mapi0(i0,i) and mapa0(a0,a)), nc_trad(emn, i0, a0, r)))
            +                   sum((fp,a),sum(a0$mapa0(a0,a), nc_endw(emn, fp, a0, r)))
            +                   sum(a, sum(a0$mapa0(a0,a), nc_qo(emn, a0, r)))
            +                   sum(i, sum(i0$mapi0(i0,i), nc_hh(emn, i0, r)))) ;
   gwp(emn) = (1000*sum(r, sum((i,a), sum((i0,a0)$(mapi0(i0,i) and mapa0(a0,a)),
                                nc_trad_ceq(emn, i0, a0, r)))
            +             sum((fp,a), sum(a0$mapa0(a0,a), nc_endw_ceq(emn, fp, a0, r)))
            +             sum(a, sum(a0$mapa0(a0,a), nc_qo_ceq(emn, a0, r)))
            +             sum(i, sum(i0$mapi0(i0,i), nc_hh_ceq(emn, i0, r))))
            / gwp(emn))$gwp(emn) + (1)$(not gwp(emn)) ;
else
   gwp(emn) = 1 ;
) ;

if(0,
   display nc_qo_ceq, nc_qo ;
   display gwp ;
   work = 1000*sum(r, sum(a, sum(a0$mapa0(a0,a), nc_qo_ceq("fgas", a0, r)))) ;
   display "nc_qo_ceq(fgas) = ", work ;
   work = sum(r, sum(a, sum(a0$mapa0(a0,a), nc_qo("fgas", a0, r)))) ;
   display "nc_qo(fgas) = ", work ;
) ;

chiEmi.fx(em,t)    = 1 ;
emiOth.fx(r,em,t)  = 0 ;
emiOthGbl.fx(em,t) = 0 ;

loop(t0,
   emiTot0(r,em)   = emiOth.l(r,em,t0) + sum((is,aa), emi0(r,em,is,aa)) ;
   emiGbl0(em)     = emiOthGbl.l(em,t0) + sum(r, emiTot0(r,em)) ;
) ;

*  Intialize tax regime components

*  No tax exemptions
part(r,em,i,aa)       = 1 ;

emiTotETS0(r,em,aets) = sum((is,aa)$mapETS(aets,aa), emi0(r,em,is,aa)) ;

emiCap.fx(ra,em,aets,t)    = 0 ;
emiCTax.fx(ra,em,aets,t)   = 0 ;
emiTax.fx(r,em,aa,t)       = 0 ;
emiTaxX.fx(s,i,d,t)        = 0 ;
emiQuota.fx(r,em,aets,t)   = 0 ;
emiQuotaY.fx(r,em,aets,t)  = 0 ;
chiCap.fx(ra,em,aetsTgt,t) = 1 ;

*  Default assumptions

emiTotETSFlag(r,em,aets) = no ;
ifEmiRCap(r,em,aa)       = no ;
ifEmiCap(ra,em,aets)     = no ;
ifEmiRQuota(r,em,aets)   = no ;

ifXCap(ra,em,aets)       = yes ;
maprq(ra,rq)             = no ;
ifRQCap(ra,em,aetstgt)   = no ;

* --------------------------------------------------------------------------------------------------
*
*  Miscellaneous initializations
*
* --------------------------------------------------------------------------------------------------

gdpmp0(r) = sum(fd, yfd0(r,fd))
          + sum((i,d), pwe0(r,i,d)*xw0(r,i,d) - pwm0(d,i,r)*xw0(d,i,r))
          + sum(img, pdt0(r,img)*xtt0(r,img)) ;

rgdpmp0(r) = gdpmp0(r) ;

rfdshr0(r,fd)$fdFlag(r,fd) = xfd0(r,fd) / rgdpmp0(r) ;
nfdshr0(r,fd)$fdFlag(r,fd) = yfd0(r,fd) / gdpmp0(r) ;

*  Initialization for comparative static model
pop0(r)$(ifDyn eq 0) = popScale*popg(r) ;

rgdppc0(r)      = rgdpmp0(r) / pop0(r) ;
grrgdppc.l(r,t) = 0 ;
gl.l(r,t)       = 0 ;

klrat0(r) = sum(a, pk0(r,a)*kv0(r,a))
          / sum((a,l), pf0(r,l,a)*xf0(r,l,a)) ;

pw0(a) = 1 ;

pwtrend(a,tt) = na ;
pwshock(a,tt) = na ;

walras.l(t)   = 0 ;

*  Welfare measures

chisave.l(t) = 1 ;
psave0(r)    = 1 ;
evf0(r,fdc)  = pfd0(r,fdc)*(yfd0(r,fdc)/pfd0(r,fdc)) ;
evs0(r)      = sum(h, psave0(r)*(savh0(r,h) + savg0(r)))/psave0(r) ;

*  Parameters need to be defined in the parameter file

loop(t0,
   sw0   = (sum((r,h), welfwgt(r,t0)*pop0(r)
         *   (ev0(r,h)/(pop0(r)))**(1-epsw(t0)))/(1-epsw(t0)))
         /  sum(r,pop0(r)) ;

   swt0  = (sum((r,h), welftwgt(r,t0)*pop0(r)
         *   ((ev0(r,h) + sum(gov, evf0(r,gov)))/(pop0(r)))**(1-epsw(t0)))/(1-epsw(t0)))
         /  sum(r,pop0(r)) ;

   swt20 = (sum((r,h), welftwgt(r,t0)*pop0(r)
         *   ((ev0(r,h) + sum(gov, evf0(r,gov)) + evs0(r))/(pop0(r)))**(1-epsw(t0)))/(1-epsw(t0)))
         /  sum(r,pop0(r)) ;
) ;

obj.l = sw0 ;
