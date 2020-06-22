px.l(r,a,tsim)          = px.l(r,a,tsim-1) ;
uc.l(r,a,v,tsim)        = uc.l(r,a,v,tsim-1) ;
pxv.l(r,a,v,tsim)       = pxv.l(r,a,v,tsim-1) ;
xpx.l(r,a,v,tsim)       = rwork(r)*xpx.l(r,a,v,tsim-1) ;
xghg.l(r,a,v,tsim)      = rwork(r)*xghg.l(r,a,v,tsim-1) ;
nd1.l(r,a,tsim)         = rwork(r)*nd1.l(r,a,tsim-1) ;
va.l(r,a,v,tsim)        = rwork(r)*va.l(r,a,v,tsim-1) ;
pxp.l(r,a,v,tsim)       = pxp.l(r,a,v,tsim-1) ;
lab1.l(r,a,tsim)        = rwork(r)*lab1.l(r,a,tsim-1) ;
kef.l(r,a,v,tsim)       = rwork(r)*kef.l(r,a,v,tsim-1) ;
nd2.l(r,a,tsim)         = rwork(r)*nd2.l(r,a,tsim-1) ;
va1.l(r,a,v,tsim)       = rwork(r)*va1.l(r,a,v,tsim-1) ;
va2.l(r,a,v,tsim)       = rwork(r)*va2.l(r,a,v,tsim-1) ;
xf.l(r,f,a,tsim)        = rwork(r)*xf.l(r,f,a,tsim-1) ;
pva.l(r,a,v,tsim)       = pva.l(r,a,v,tsim-1) ;
pva1.l(r,a,v,tsim)      = pva1.l(r,a,v,tsim-1) ;
pva2.l(r,a,v,tsim)      = pva2.l(r,a,v,tsim-1) ;
kf.l(r,a,v,tsim)        = rwork(r)*kf.l(r,a,v,tsim-1) ;
xnrg.l(r,a,v,tsim)      = rwork(r)*xnrg.l(r,a,v,tsim-1) ;
pkef.l(r,a,v,tsim)      = pkef.l(r,a,v,tsim-1) ;
ksw.l(r,a,v,tsim)       = rwork(r)*ksw.l(r,a,v,tsim-1) ;
pkf.l(r,a,v,tsim)       = pkf.l(r,a,v,tsim-1) ;
ks.l(r,a,v,tsim)        = rwork(r)*ks.l(r,a,v,tsim-1) ;
xwat.l(r,a,tsim)        = rwork(r)*xwat.l(r,a,tsim-1) ;
pksw.l(r,a,v,tsim)      = pksw.l(r,a,v,tsim-1) ;
kv.l(r,a,v,tsim)        = rwork(r)*kv.l(r,a,v,tsim-1) ;
kslo.l(r,a,tsim)        = rwork(r)*kslo.l(r,a,tsim-1) ;
lab2.l(r,a,tsim)        = rwork(r)*lab2.l(r,a,tsim-1) ;
pks.l(r,a,v,tsim)       = pks.l(r,a,v,tsim-1) ;
plab1.l(r,a,tsim)       = plab1.l(r,a,tsim-1) ;
plab2.l(r,a,tsim)       = plab2.l(r,a,tsim-1) ;
labb.l(r,wb,a,tsim)     = rwork(r)*labb.l(r,wb,a,tsim-1) ;
plabb.l(r,wb,a,tsim)    = plabb.l(r,wb,a,tsim-1) ;
pnd1.l(r,a,tsim)        = pnd1.l(r,a,tsim-1) ;
pnd2.l(r,a,tsim)        = pnd2.l(r,a,tsim-1) ;
pwat.l(r,a,tsim)        = pwat.l(r,a,tsim-1) ;

if(ifNRGNest,
   xnely.l(r,a,v,tsim)     = rwork(r)*xnely.l(r,a,v,tsim-1) ;
   xolg.l(r,a,v,tsim)      = rwork(r)*xolg.l(r,a,v,tsim-1) ;
   xaNRG.l(r,a,NRG,v,tsim) = rwork(r)*xaNRG.l(r,a,NRG,v,tsim-1) ;

   paNRG.l(r,a,NRG,v,tsim) = paNRG.l(r,a,NRG,v,tsim-1) ;
   polg.l(r,a,v,tsim)      = polg.l(r,a,v,tsim-1) ;
   pnely.l(r,a,v,tsim)     = pnely.l(r,a,v,tsim-1) ;
) ;

pnrg.l(r,a,v,tsim)       = pnrg.l(r,a,v,tsim-1) ;
lambdae.l(r,e,a,v,tsim)  = lambdae.l(r,e,a,v,tsim-1)*power(1 + 0.01*aeei(r,e,a,v,tsim), gap(tsim)) ;
pnrg.l(r,a,v,tsim)$(vOld(v) and xnrgFlag(r,a)) =
      ((sum(e, (alpha_eio(r,e,a,v,tsim)*shr0_eio(r,e,a))
   *     (pa.l(r,e,a,tsim-1)/lambdae.l(r,e,a,v,tsim))**(1-sigmae(r,a,v))))**(1/(1-sigmae(r,a,v))))
   $(not ifNRGNest)

   +  (((alpha_NRGB(r,a,"ELY",v,tsim)*shr0_NRGB(r,a,"ELY"))
   *    paNRG.l(r,a,"ELY",v,tsim)**(1-sigmae(r,a,v))
   +   (alpha_nely(r,a,v,tsim)*shr0_nely(r,a))
   *    pnely.l(r,a,v,tsim)**(1-sigmae(r,a,v)))**(1/(1-sigmae(r,a,v))))
   $ifNRGNest ;

loop((vNew,vOld),
   pnrg.l(r,a,vNew,tsim)$xnrgFlag(r,a) = pnrg.l(r,a,vOld,tsim) ;
) ;

pnrg.l(r,a,v,tsim)$(xnrgFlag(r,a)) =
      ((sum(e, (alpha_eio(r,e,a,v,tsim)*shr0_eio(r,e,a))
   *     (pa.l(r,e,a,tsim-1)/lambdae.l(r,e,a,v,tsim))**(1-sigmae(r,a,v))))**(1/(1-sigmae(r,a,v))))
   $(not ifNRGNest)

   +  (((alpha_NRGB(r,a,"ELY",v,tsim)*shr0_NRGB(r,a,"ELY"))
   *    paNRG.l(r,a,"ELY",v,tsim)**(1-sigmae(r,a,v))
   +   (alpha_nely(r,a,v,tsim)*shr0_nely(r,a))
   *    pnely.l(r,a,v,tsim)**(1-sigmae(r,a,v)))**(1/(1-sigmae(r,a,v))))
   $ifNRGNest ;

xa.l(r,i,aa,tsim)        = rwork(r)*xa.l(r,i,aa,tsim-1) ;
xpv.l(r,a,v,tsim)        = rwork(r)*xpv.l(r,a,v,tsim-1) ;
xp.l(r,a,tsim)           = sum(v, xpv.l(r,a,v,tsim)) ;
x.l(r,a,i,tsim)          = rwork(r)*x.l(r,a,i,tsim-1) ;
p.l(r,a,i,tsim)          = p.l(r,a,i,tsim-1) ;
pp.l(r,a,i,tsim)         = pp.l(r,a,i,tsim-1) ;
ps.l(r,i,tsim)           = ps.l(r,i,tsim-1) ;
xpow.l(r,elyc,tsim)      = rwork(r)*xpow.l(r,elyc,tsim-1) ;
ppow.l(r,elyc,tsim)      = ppow.l(r,elyc,tsim-1) ;
ppowndx.l(r,elyc,tsim)   = ppowndx.l(r,elyc,tsim-1) ;
xpb.l(r,pb,elyc,tsim)    = rwork(r)*xpb.l(r,pb,elyc,tsim-1) ;
ppb.l(r,pb,elyc,tsim)    = ppb.l(r,pb,elyc,tsim-1) ;
ppbndx.l(r,pb,elyc,tsim) = ppbndx.l(r,pb,elyc,tsim-1) ;

phtaxpb.l(r,pb,elyc,tsim) = phtaxpb.l(r,pb,elyc,tsim-1) ;
chiphpb.l(r,tsim)         = chiphpb.l(r,tsim-1) ;

yqtf.l(r,tsim)           = yqtf.l(r,tsim-1) ;
trustY.l(tsim)           = trustY.l(tsim-1) ;
yqht.l(r,tsim)           = yqht.l(r,tsim-1) ;
remit.l(s,l,r,tsim)      = remit.l(s,l,r,tsim-1) ;
ODAOut.l(r,tsim)         = ODAOut.l(r,tsim-1) ;
ODAGbl.l(tsim)           = ODAGbl.l(tsim-1) ;
ODAIn.l(r,tsim)          = ODAIn.l(r,tsim-1) ;

yh.l(r,tsim)             = rwork(r)*yh.l(r,tsim-1) ;
deprY.l(r,tsim)          = rwork(r)*deprY.l(r,tsim-1) ;
yd.l(r,tsim)             = rwork(r)*yd.l(r,tsim-1) ;
yc.l(r,h,tsim)           = rwork(r)*yc.l(r,h,tsim-1) ;
supy.l(r,h,tsim)         = rwork(r)*supy.l(r,h,tsim-1) ;
xc.l(r,k,h,tsim)         = rwork(r)*xc.l(r,k,h,tsim-1) ;
zcons.l(r,k,h,tsim)      = rwork(r)*zcons.l(r,k,h,tsim-1) ;
hshr.l(r,k,h,tsim)       = hshr.l(r,k,h,tsim-1) ;
u.l(r,h,tsim)            = rwork(r)*u.l(r,h,tsim-1) ;
pc.l(r,k,h,tsim)         = pc.l(r,k,h,tsim-1) ;
xcnnrg.l(r,k,h,tsim)     = rwork(r)*xcnnrg.l(r,k,h,tsim-1) ;
pcnnrg.l(r,k,h,tsim)     = pcnnrg.l(r,k,h,tsim-1) ;
xcnrg.l(r,k,h,tsim)      = rwork(r)*xcnrg.l(r,k,h,tsim-1) ;
pcnrg.l(r,k,h,tsim)      = pcnrg.l(r,k,h,tsim-1) ;
xcnely.l(r,k,h,tsim)     = rwork(r)*xcnely.l(r,k,h,tsim-1) ;
pcnely.l(r,k,h,tsim)     = pcnely.l(r,k,h,tsim-1) ;
xcolg.l(r,k,h,tsim)      = rwork(r)*xcolg.l(r,k,h,tsim-1) ;
pcolg.l(r,k,h,tsim)      = pcolg.l(r,k,h,tsim-1) ;
xacNRG.l(r,k,h,NRG,tsim) = rwork(r)*xacNRG.l(r,k,h,NRG,tsim-1) ;
pacNRG.l(r,k,h,NRG,tsim) = pacNRG.l(r,k,h,NRG,tsim-1) ;
xaac.l(r,i,h,tsim)       = rwork(r)*xaac.l(r,i,h,tsim-1) ;
xawc.l(r,i,h,tsim)       = rwork(r)*xawc.l(r,i,h,tsim-1) ;
paacc.l(r,i,h,tsim)      = paacc.l(r,i,h,tsim-1) ;
paac.l(r,i,h,tsim)       = paac.l(r,i,h,tsim-1) ;
pawc.l(r,i,h,tsim)       = pawc.l(r,i,h,tsim-1) ;
pah.l(r,i,h,tsim)        = pah.l(r,i,h,tsim-1) ;
savh.l(r,h,tsim)         = rwork(r)*savh.l(r,h,tsim-1) ;
ygov.l(r,gy,tsim)        = rwork(r)*ygov.l(r,gy,tsim-1) ;
ntmY.l(r,tsim)           = rwork(r)*ntmY.l(r,tsim-1) ;
pfd.l(r,fd,tsim)         = pfd.l(r,fd,tsim-1) ;
yfd.l(r,fd,tsim)         = rwork(r)*yfd.l(r,fd,tsim-1) ;
cpi.l(r,h,cpindx,tsim)   = cpi.l(r,h,cpindx,tsim-1) ;
ev.l(r,h,tsim)           = rwork(r)*ev.l(r,h,tsim-1) ;
evf.l(r,fdc,tsim)        = rwork(r)*evf.l(r,fdc,tsim-1) ;
evs.l(r,tsim)            = rwork(r)*evs.l(r,tsim-1) ;
xat.l(r,i,tsim)          = rwork(r)*xat.l(r,i,tsim-1) ;
xdt.l(r,i,tsim)          = rwork(r)*xdt.l(r,i,tsim-1) ;
xmt.l(r,i,tsim)          = rwork(r)*xmt.l(r,i,tsim-1) ;
pat.l(r,i,tsim)          = pat.l(r,i,tsim-1) ;
pa.l(r,i,aa,tsim)        = pa.l(r,i,aa,tsim-1) ;
pd.l(r,i,aa,tsim)        = pd.l(r,i,aa,tsim-1) ;
xd.l(r,i,aa,tsim)        = rwork(r)*xd.l(r,i,aa,tsim-1) ;
pm.l(r,i,aa,tsim)        = pm.l(r,i,aa,tsim-1) ;
xm.l(r,i,aa,tsim)        = rwork(r)*xm.l(r,i,aa,tsim-1) ;
xw.l(s,i,d,tsim)         = rwork(d)*xw.l(s,i,d,tsim-1) ;
pdt.l(r,i,tsim)          = pdt.l(r,i,tsim-1) ;
xet.l(r,i,tsim)          = rwork(r)*xet.l(r,i,tsim-1) ;
xs.l(r,i,tsim)           = rwork(r)*xs.l(r,i,tsim-1) ;
pe.l(s,i,d,tsim)         = pe.l(s,i,d,tsim-1) ;
pet.l(r,i,tsim)          = pet.l(r,i,tsim-1) ;
pwe.l(s,i,d,tsim)        = pwe.l(s,i,d,tsim-1) ;
pwm.l(s,i,d,tsim)        = pwm.l(s,i,d,tsim-1) ;

$iftheni "%MRIO_MODULE%" == "ON"
   xwa.l(s,i,d,aa,tsim)  = rwork(d)*xwa.l(s,i,d,aa,tsim-1) ;
   pdma.l(s,i,d,aa,tsim) = pdma.l(s,i,d,aa,tsim-1) ;
   pma.l(r,i,aa,tsim)    = pma.l(r,i,aa,tsim-1) ;
$else
   pdm.l(s,i,d,tsim)      = pdm.l(s,i,d,tsim-1) ;
   pmt.l(r,i,tsim)        = pmt.l(r,i,tsim-1) ;
$endif

xwmg.l(s,i,d,tsim)      = rwork(d)*xwmg.l(s,i,d,tsim-1) ;
xmgm.l(img,s,i,d,tsim)  = rwork(d)*xmgm.l(img,s,i,d,tsim-1) ;
pwmg.l(s,i,d,tsim)      = pwmg.l(s,i,d,tsim-1) ;
xtmg.l(img,tsim)        = xtmg.l(img,tsim-1) ;
xtt.l(r,i,tsim)         = rwork(r)*xtt.l(r,i,tsim-1) ;
ptmg.l(img,tsim)        = ptmg.l(img,tsim-1) ;

pf.l(r,f,a,tsim)        = pf.l(r,f,a,tsim-1) ;

ldz.l(r,l,z,tsim)       = (popT(r,"P1564",tsim)/popT(r,"P1564",tsim-1))*ldz.l(r,l,z,tsim-1) ;
awagez.l(r,l,z,tsim)    = awagez.l(r,l,z,tsim-1) ;
urbPrem.l(r,l,tsim)     = urbPrem.l(r,l,tsim-1) ;
migr.l(r,l,tsim)        = migr.l(r,l,tsim-1) ;
lsz.l(r,l,z,tsim)       = (popT(r,"P1564",tsim)/popT(r,"P1564",tsim-1))*lsz.l(r,l,z,tsim-1) ;
ewagez.l(r,l,z,tsim)    = ewagez.l(r,l,z,tsim-1) ;
uez.l(r,l,z,tsim)       = uez.l(r,l,z,tsim-1) ;
rwage.l(r,l,z,tsim)     = rwage.l(r,l,z,tsim-1) ;
errW.l(r,l,z,tsim)      = errW.l(r,l,z,tsim-1) ;
twage.l(r,l,tsim)       = twage.l(r,l,tsim-1) ;

pk.l(r,a,v,tsim)        = pk.l(r,a,v,tsim-1) ;
trent.l(r,tsim)         = trent.l(r,tsim-1) ;
rtrent.l(r,tsim)        = rtrent.l(r,tsim-1) ;
kxRat.l(r,a,v,tsim)     = kxRat.l(r,a,v,tsim-1) ;
rrat.l(r,a,tsim)        = rrat.l(r,a,tsim-1) ;
arent.l(r,tsim)         = arent.l(r,tsim-1) ;
if(ifCal,
   chiaps.l(r,h,tsim)   = chiaps.l(r,h,tsim-1) ;
) ;
aps.l(r,h,tsim)         = (chiaps.l(r,h,tsim)/aps0(r,h))*(trent.l(r,tsim)/trent.l(r,tsim-1))**etaaps(r,h) ;

ptland.l(r,tsim)        = ptland.l(r,tsim-1) ;
ptlandndx.l(r,tsim)     = ptlandndx.l(r,tsim-1) ;
xlb.l(r,lb,tsim)        = xlb.l(r,lb,tsim-1) ;
plb.l(r,lb,tsim)        = plb.l(r,lb,tsim-1) ;
plbndx.l(r,lb,tsim)     = plbndx.l(r,lb,tsim-1) ;
xnlb.l(r,tsim)          = xnlb.l(r,tsim-1) ;
pnlb.l(r,tsim)          = pnlb.l(r,tsim-1) ;
pnlbndx.l(r,tsim)       = pnlbndx.l(r,tsim-1) ;

$iftheni.RD "%simtype%" == "RcvDyn"
$iftheni.DEPL "%DEPL_MODULE%" == "ON"
prat.l(r,a,tsim)$ifDepl(r,a)    = prat.l(r,a,tsim-1) ;
omegar.l(r,a,tsim)$ifDepl(r,a)  = omegar.l(r,a,tsim-1) ;
dscRate.l(r,a,tsim)$ifDsc(r,a)  = dscRate.l(r,a,tsim-1) ;
extRate.l(r,a,tsim)$ifDepl(r,a) = extRate.l(r,a,tsim-1) ;

extr.l(r,a,tsim)$ifDepl(r,a)    = rwork(r)*extr.l(r,a,tsim-1) ;

cumExt.l(r,a,tsim)$ifDepl(r,a)
   = ((extr.l(r,a,tsim)*extr0(r,a) - extr.l(r,a,tsim-1)*extr0(r,a))
   / ((extr.l(r,a,tsim)/extr.l(r,a,tsim-1))**(1/gap(tsim))-1))/cumext0(r,a) ;

res.l(r,a,tsim)$ifDepl(r,a)
   = (res.l(r,a,tsim-1)*res0(r,a) - cumExt.l(r,a,tsim)*cumExt0(r,a)
   + ((1 - power((1-dscRate.l(r,a,tsim)),gap(tsim)))*ytdres.l(r,a,tsim-1)*ytdRes0(r,a))$ifDsc(r,a))
   /  res0(r,a) ;

ytdRes.l(r,a,tsim)$ifDsc(r,a)   = power((1 - dscRate.l(r,a,tsim)), gap(tsim))*ytdres.l(r,a,tsim-1) ;
resGap.l(r,a,tsim)$ifDepl(r,a)  = resGap.l(r,a,tsim-1) ;
resp.l(r,a,tsim)$ifDepl(r,a)    = (res.l(r,a,tsim)*res0(r,a) - resGap.l(r,a,tsim))/resp0(r,a) ;
xfPot.l(r,a,tsim)$ifDepl(r,a)   = (extRate.l(r,a,tsim)*resP.l(r,a,tsim)*resp0(r,a)
                                + (resGap.l(r,a,tsim) / gap(tsim))$(not ifResPFlag(r,a)))/xfPot0(r,a) ;
$endif.DEPL
$endif.RD

etanrs.l(r,a,tsim)      = etanrsx(r,a,"lo") + (etanrsx(r,a,"hi") - etanrsx(r,a,"lo"))
                        *  sigmoid(kink*(sum(nrs, xf.l(r,nrs,a,tsim)/xf.l(r,nrs,a,tsim-1))-1)) ;
xfNot.l(r,a,tsim)       = rwork(r)*xfNot.l(r,a,tsim-1) ;
xfs.l(r,nrs,a,tsim)     = rwork(r)*xfs.l(r,nrs,a,tsim-1) ;

$iftheni "%DEPL_MODULE%" == "ON"
   xfGap.l(r,a,tsim)    = xfGap.l(r,a,tsim-1) ;
   xfs.l(r,nrs,a,t)$xf0(r,nrs,a) = xf.l(r,nrs,a,t) ;
$endif

pfp.l(r,f,a,tsim)       = pfp.l(r,f,a,tsim-1) ;
pkp.l(r,a,v,tsim)       = pkp.l(r,a,v,tsim-1) ;

savg.l(r,tsim)          = savg.l(r,tsim-1) ;

rsg.l(r,tsim)           = rsg.l(r,tsim-1) ;
kappah.l(r,tsim)        = kappah.l(r,tsim-1) ;
xfd.l(r,fd,tsim)        = rwork(r)*xfd.l(r,fd,tsim-1) ;

kstocke.l(r,tsim)       = rwork(r)*kstocke.l(r,tsim-1) ;
ror.l(r,tsim)           = ror.l(r,tsim-1) ;
rorc.l(r,tsim)          = rorc.l(r,tsim-1) ;
rore.l(r,tsim)          = rore.l(r,tsim-1) ;
devRoR.l(r,tsim)        = devRoR.l(r,tsim-1) ;
grK.l(r,tsim)           = grK.l(r,tsim-1) ;
rorg.l(tsim)            = rorg.l(tsim-1) ;
savf.l(r,tsim)          = savf.l(r,tsim-1) ;
savfRat.l(r,tsim)       = savfRat.l(r,tsim-1) ;
pmuv.l(tsim)            = pmuv.l(tsim-1) ;
pfact.l(r,tsim)         = pfact.l(r,tsim-1) ;
pwfact.l(tsim)          = pwfact.l(tsim-1) ;
pwgdp.l(tsim)           = pwgdp.l(tsim-1) ;
pwsav.l(tsim)           = pwsav.l(tsim-1) ;
psave.l(r,tsim)         = psave.l(r,tsim-1) ;
chisave.l(tsim)         = chisave.l(tsim-1) ;
pw.l(a,tsim)            = pw.l(a,tsim-1) ;
wchinrs.l(a,tsim)       = wchinrs.l(a,tsim-1) ;
*pwgdp.l(tsim)          = pwgdp.l(tsim-1) ;

gdpmp.l(r,tsim)         = rwork(r)*gdpmp.l(r,tsim-1) ;
rgdpmp.l(r,tsim)        = rwork(r)*rgdpmp.l(r,tsim-1) ;
pgdpmp.l(r,tsim)        = pgdpmp.l(r,tsim-1) ;
rgdppc.l(r,tsim)        = (rwork(r)/(popT(r,"PTOTL",tsim)/popT(r,"PTOTL",tsim-1)))
                        *  rgdppc.l(r,tsim-1) ;
grrgdppc.l(r,tsim)      = grrgdppc.l(r,tsim-1) ;

rfdshr.l(r,fd,tsim)     = rfdshr.l(r,fd,tsim-1) ;
nfdshr.l(r,fd,tsim)     = nfdshr.l(r,fd,tsim-1) ;

emi.l(r,em,is,aa,tsim)  = emi.l(r,em,is,aa,tsim-1) ;
emitax.l(r,em,aa,tsim)  = emiTax.l(r,em,aa,tsim-1) ;
emiTot.l(r,em,tsim)     = emiTot.l(r,em,tsim-1) ;
emiGbl.l(em,tsim)       = emiGbl.l(em,tsim-1) ;

kstock.l(r,tsim)        = rwork(r)*kstock.l(r,tsim-1) ;
tkaps.l(r,tsim)         = rwork(r)*tkaps.l(r,tsim-1) ;
rore.l(r,tsim)          = (rorc.l(r,tsim)*rorc0(r)*(kstocke.l(r,tsim)/kstock.l(r,tsim))**(-epsRor(r,tsim)))/rore0(r) ;
tls.l(r,tsim)           = (popT(r,"P1564",tsim)/popT(r,"P1564",tsim-1))*tls.l(r,tsim-1) ;
ls.l(r,l,tsim)          = (popT(r,"P1564",tsim)/popT(r,"P1564",tsim-1))*ls.l(r,l,tsim-1) ;
tland.l(r,tsim)         = tland.l(r,tsim-1) ;
pop.l(r,tsim)           = (popT(r,"PTOTL",tsim)/popT(r,"PTOTL",tsim-1))*pop.l(r,tsim-1) ;

skillprem.l(r,l,tsim)   = skillprem.l(r,l,tsim-1) ;

sw.l(tsim) = ((sum((r,h), welfwgt(r,tsim)*pop0(r)*pop.l(r,tsim)
           *   (ev0(r,h)*ev.l(r,h,tsim)/(pop0(r)*pop.l(r,tsim)))**(1-epsw(tsim)))/(1-epsw(tsim)))
           /  sum(r,pop0(r)*pop.l(r,tsim)))/sw0 ;

swt.l(tsim) = ((sum((r,h), welftwgt(r,tsim)*pop0(r)*pop.l(r,tsim)
            *   ((ev0(r,h)*ev.l(r,h,tsim) + sum(gov, evf0(r,gov)*evf.l(r,gov,tsim)))
            /    (pop0(r)*pop.l(r,tsim)))**(1-epsw(tsim)))/(1-epsw(tsim)))
            /  sum(r,pop0(r)*pop.l(r,tsim)))/swt0 ;
swt2.l(tsim) = swt2.l(tsim-1) ;
obj.l = sw.l(tsim) ;

$ontext
$ondotl
if(years(tsim) eq 2022,
   put screen ; put / ; put "INITVAR:" / ;
   loop((r,i,aa)$(sameas(r,"OPC") and sameas(i,"OIL-C") and sameas(aa,"PFD-a")),
      put "XD:  ", xd.l(r,i,aa,tsim-1), xd.l(r,i,aa,tsim) / ;
      put "PD:  ", m_PD(r,i,aa,tsim-1), m_PD(r,i,aa,tsim) / ;
      put "PA:  ", pa.l(r,i,aa,tsim-1), pa.l(r,i,aa,tsim) / ;
      put "XA:  ", xa.l(r,i,aa,tsim-1), xa.l(r,i,aa,tsim) / ;
   ) ;
   Abort "Temp" ;
) ;
$offdotl
$offtext

if(0,
   invGFact.l(r,tsim)      = invGFact.l(r,tsim-1) ;
else
   invGFact.l(r,tsim)      = 1/((sum(inv, xfd.l(r,inv,tsim)/xfd.l(r,inv,tsim-1)))**(1/gap(tsim))
                           - 1 + depr(r,tsim)) ;
) ;
