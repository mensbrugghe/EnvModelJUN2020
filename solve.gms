rs(rr) = no ;

ifGbl = 0 ;

for(riter=1 to nriter by 1, loop(rr,

*  Loop over region rr

   rs(rr) = yes ;
*
*  Fix PE prices and update import prices
*
   pe.fx(s,i,d,tsim)  = pe.l(s,i,d,tsim) ;

*  Fix global flows

   xw.fx(s,i,d,tsim)  = xw.l(s,i,d,tsim) ;
   xtt.fx(r,img,tsim) = xtt.l(r,img,tsim) ;
   pdt.fx(r,img,tsim) = pdt.l(r,img,tsim) ;

*  Exogenize global trust

   trustY.fx(tsim) = trustY.l(tsim) ;

*  Exogenize remittance inflows

   remit.fx(s,l,d,tsim)$(not rs(d)) = remit.l(s,l,d,tsim) ;

*  Endogenize imports

   xw.lo(s,i,d,tsim)$(xwFlag(s,i,d) and rs(d)) = -inf ;
   xw.up(s,i,d,tsim)$(xwFlag(s,i,d) and rs(d)) = +inf ;

*  Endogenize exports

   xw.lo(s,i,d,tsim)$(xwFlag(s,i,d) and rs(s) and omegaw(s,i) ne inf) = -inf ;
   xw.up(s,i,d,tsim)$(xwFlag(s,i,d) and rs(s) and omegaw(s,i) ne inf) = +inf ;

*  Endogenize pd and xtt

   pdt.lo(r,i,tsim)$(rs(r) and xdtFlag(r,i) ne 0) = -inf ;
   pdt.up(r,i,tsim)$(rs(r) and xdtFlag(r,i) ne 0) = +inf ;
   xtt.lo(r,img,tsim)$(rs(r) and xttFlag(r,img) ne 0) = -inf ;
   xtt.up(r,img,tsim)$(rs(r) and xttFlag(r,img) ne 0) = +inf ;

*  solve

*  !!!! Need to change this so that it solves using NLP

   solve %1 using mcp ;

   put screen ;
   if (%1.solvestat eq 1,
      put // "Solved iteration ", riter:<2:0, " out of ", nriter:2:0,
             " iteration(s) for region ", rr.tl, " in year ", years(tsim):4:0 // ;
   else
      execute_unload "%odir%\%SIMNAME%.gdx" ; ;
      put // "Failed to solve for iteration ", riter:<2:0, " out of ", nriter:2:0,
             " iteration(s) for region ", rr.tl, " in year ", years(tsim):4:0 // ;
      Abort$(1) "Solution failure" ;
   ) ;
   putclose screen ;

   rs(r) = no ;

)) ;
*
*  --- include all regions again
*
rs(r) = yes ;
ifGbl = 1 ;
*
*  --- release bounds on variables fixed in individual solves
*      and introduce original lower bounds
*
pe.lo(s,i,d,tsim)$xwFlag(s,i,d) = 0.001*pe.l(s,i,d,tsim) ;
pe.up(s,i,d,tsim)$xwFlag(s,i,d) = +inf ;

xw.lo(s,i,d,tsim)$xwFlag(s,i,d) = -inf;
xw.up(s,i,d,tsim)$xwFlag(s,i,d) = +inf;

pdt.lo(r,i,tsim)$xdtFlag(r,i) = -inf ;
pdt.up(r,i,tsim)$xdtFlag(r,i) = +inf ;

xtt.lo(r,img,tsim)$xttFlag(r,img) = -inf ;
xtt.up(r,img,tsim)$xttFlag(r,img) = +inf ;

trustY.lo(tsim)$trustY0 = -inf ;
trustY.up(tsim)$trustY0 = +inf ;

remit.lo(s,l,d,tsim)$remit0(s,l,d) = -inf ;
remit.up(s,l,d,tsim)$remit0(s,l,d) = +inf ;

if(ifMCP,
   solve %1 using mcp ;
else
   solve %1 using nlp maximizing obj ;
) ;

put screen ;
if (%1.solvestat eq 1,
   put // "Solved global model in year ", years(tsim):4:0 // ;
else
   execute_unload "%odir%\%SIMNAME%.gdx" ; ;
   put // "Failed to solve global model in year ", years(tsim):4:0 // ;
   Abort$(1) "Solution failure" ;
) ;
putclose screen ;

*  Update substituted out variables

if(ifSUB,
   $$onDotL

   pp.l(r,a,i,tsim)   = M_PP(r,a,i,tsim) ;

   pa.l(r,i,aa,tsim)  = M_PA(r,i,aa,tsim) ;
   pd.l(r,i,aa,tsim)  = M_PD(r,i,aa,tsim) ;
   pm.l(r,i,aa,tsim)  = M_PM(r,i,aa,tsim) ;

   pwe.l(s,i,d,tsim) = M_PWE(s,i,d,tsim) ;
   pwm.l(s,i,d,tsim) = M_PWM(s,i,d,tsim) ;
   pdm.l(s,i,d,tsim) = M_PDM(s,i,d,tsim) ;

$iftheni "%MRIO_MODULE%" == "ON"
   pdma.l(s,i,d,aa,tsim) = M_PDMA(s,i,d,aa,tsim) ;
$endif

   pwmg.l(s,i,d,tsim) = M_PWMG(s,i,d,tsim) ;
   xwmg.l(s,i,d,tsim)$xwmg0(s,i,d) = M_XWMG(s,i,d,tsim) ;
   xmgm.l(img,s,i,d,tsim)$xmgm0(img,s,i,d) = M_XMGM(img,s,i,d,tsim) ;

   pfp.l(r,f,a,tsim) = M_PFP(r,f,a,tsim) ;

   emiTotETS.l(r,em,aets,tsim) = M_EmiTotETS(r,em,aets,tsim) ;

   $$offDotL
) ;

*  Update income and price elasticities

omegaad.fx(r,h)
      = (sum(k$xcFlag(r,k,h), (betaad.l(r,k,h,tsim)-alphaad.l(r,k,h,tsim))
      *     log(xc.l(r,k,h,tsim)*xc0(r,k,h)/(pop0(r)*pop.l(r,tsim)) - gammac.l(r,k,h,tsim)))
      - power(1+exp(u.l(r,h,tsim)*u0(r,h)),2)*exp(-u.l(r,h,tsim)*u0(r,h)))
      $(%utility% eq AIDADS)

      + 1$(%utility% ne AIDADS)
      ;

omegaad.fx(r,h) = 1/omegaad.l(r,h) ;

etah.l(r,k,h,tsim)$xcFlag(r,k,h)

   = (muc0(r,k,h)*muc.l(r,k,h,tsim)/((pc.l(r,k,h,tsim)*xc.l(r,k,h,tsim)
   *                      pc0(r,k,h)*xc0(r,k,h))/(yd.l(r,tsim)*yd0(r))))$(%utility% eq ELES)

   + ((muc.l(r,k,h,tsim)*muc0(r,k,h) - (betaad.l(r,k,h,tsim)-alphaad.l(r,k,h,tsim))*omegaad.l(r,h))
   / (hshr0(r,k,h)*hshr.l(r,k,h,tsim)))$(%utility% eq AIDADS or %utility% eq LES)

   + ((eh.l(r,k,h,tsim)*bh.l(r,k,h,tsim)
   - sum(kp$xcFlag(r,kp,h), hshr0(r,kp,h)*hshr.l(r,kp,h,tsim)*eh.l(r,kp,h,tsim)*bh.l(r,kp,h,tsim)))
   / sum(kp$xcFlag(r,kp,h), hshr0(r,kp,h)*hshr.l(r,kp,h,tsim)*eh.l(r,kp,h,tsim))
   - (bh.l(r,k,h,tsim)-1)
   + sum(kp$xcFlag(r,kp,h), hshr0(r,kp,h)*hshr.l(r,kp,h,tsim)*bh.l(r,kp,h,tsim)))$(%utility% eq CDE)
   ;

epsh.l(r,k,kp,h,tsim)$(xcFlag(r,k,h) and xcFlag(r,kp,h))

   = (-muc0(r,k,h)*muc.l(r,k,h,tsim)*pc.l(r,kp,h,tsim)*pop.l(r,tsim)*gammac.l(r,kp,h,tsim)
   *  pc0(r,kp,h)/(pc.l(r,k,h,tsim)*xc.l(r,k,h,tsim)*pc0(r,k,h)*xc0(r,k,h))
   - kron(k,kp)*(1 - pop.l(r,tsim)*gammac.l(r,k,h,tsim)*pop0(r)
   / (xc.l(r,k,h,tsim)*xc0(r,k,h))))$(%utility% eq ELES)

   + ((muc.l(r,kp,h,tsim)*muc0(r,kp,h)-kron(k,kp))
   *  (muc.l(r,k,h,tsim)*muc0(r,k,h)*supy0(r,h)*supy.l(r,h,tsim))
   /  (hshr0(r,k,h)*hshr.l(r,k,h,tsim)*((yd.l(r,tsim)-(savh.l(r,h,tsim)*(savh0(r,h)/yd0(r))))/pop.l(r,tsim))
   *  (yd0(r)/pop0(r))) - (hshr0(r,kp,h)*hshr.l(r,kp,h,tsim)*etah.l(r,k,h,tsim)))
   $(%utility% eq AIDADS or %utility% eq LES)

   + (hshr0(r,kp,h)*hshr.l(r,kp,h,tsim)*(-bh.l(r,kp,h,tsim)
   - (eh.l(r,k,h,tsim)*bh.l(r,k,h,tsim)
   - sum(k1$xcFlag(r,k1,h), hshr0(r,k1,h)*hshr.l(r,k1,h,tsim)*eh.l(r,k1,h,tsim)*bh.l(r,k1,h,tsim)))
   /  sum(k1$xcFlag(r,k1,h), hshr0(r,k1,h)*hshr.l(r,k1,h,tsim)*eh.l(r,k1,h,tsim)))
   + kron(k,kp)*(bh.l(r,k,h,tsim)-1))$(%utility% eq CDE)
   ;

* display epsh.l ;
