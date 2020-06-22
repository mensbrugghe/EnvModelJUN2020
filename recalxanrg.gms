$ontext

$batinclude "recalxanrgn.gms" xnrg  xa    pnrg    paa   aeio    sigmae  a   lambdae
                                1    2      3      4      5        6    7     8

Arguments
   1     CES aggregate volume
   2     Component volume (has vintage)
   3     CES aggregate price
   4     Component price
   5     Component share parameter
   6     Elasticity of substitution
   7     Sectors
   8     Component tech progress

$offtext

$setargs tvol vol tprice price shrParm elast index prodfact
*          1   2     3     4      5      6     7      8

loop((r,%index%),

   tvol = sum(v, %tvol%.l(r,%index%,v,tsim-1)) ;

   if(tvol ne 0,

      tprice = sum(v, %tprice%.l(r,%index%,v,tsim-1) * %tvol%.l(r,%index%,v,tsim-1)) / tvol ;

      loop(e,

         vol = %vol%.l(r,e,%index%,tsim-1) ;

         if(vol ne 0,

            price = %price%.l(r,e,%index%,tsim-1) ;

            %shrParm%(r,e,%index%,vOld,tsim)
               = (%prodfact%.l(r,e,%index%,vOld,tsim-1)**(1-%elast%(r,%index%,vOld)))
               * (vol/tvol) * (price/tprice)**%elast%(r,%index%,vOld) ;
         ) ;
      ) ;
   ) ;
) ;
