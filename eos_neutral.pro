;------------------------------------------------------------
;------------------------------------------------------------
;------------------------------------------------------------
;
; NAME: EOS
;
; PURPOSE:
;       computes rho  (in situ volumic mass) 
;
; CATEGORY:
;       calculation
;
; CALLING SEQUENCE:
;       tableau=eos(t,s)
;
; INPUTS:
;       t : temperature
;       s : salinity
;
; REFERENCE:
;	Compute the neutral volumic mass (Kg/m3) from known potential 
;       temperature and salinity fields using McDougall and Jackett 2005
;       equation of state.
;              potential temperature         t        deg celsius
;              salinity                      s        psu
;              nutral density                rho      kg/m**3
;
;         Check value: rho(35,20) = 1024.59416751197 kg/m**3 
;          t = 20 deg celcius, s=35 psu
;
;       McDougall and Jackett, J. Mar Res., 2005
;
;   MODIFICATION HISTORY:
;-      Gurvan Madec (04/14/2005) 
;------------------------------------------------------------
;------------------------------------------------------------
;------------------------------------------------------------
FUNCTION eos_neutral, t, s
;
@common
;
; mask t and s fields
   zt=t*tmask
   zs=s*tmask 
	; neutral density
	;
	;   ... square root salinity
     	
	            zsr= sqrt( abs( zs ) )
                                ; Numerator
                                ; T-Polynome:                    T^3                       T^2                         T                      cst
	            zr1= ( ( -4.3159255086706703e-4*zt+8.1157118782170051e-2 )*zt+2.2280832068441331e-1 )*zt+1002.3063688892480
    ; S-T Polynome:                  S^2                        T S                     S
                zr2= ( -1.7052298331414675e-7*zs-3.1710675488863952e-3*zt-1.0304537539692924e-4 )*zs
                
                                ;Denominator
                                ; T-Polynome:                     T^4                      T^3                       T^2                         T                      cst
                zr3= ( ( (-2.3850178558212048e-9*zt -1.6212552470310961e-7 )*zt+7.8717799560577725e-5 )*zt+4.3907692647825900e-5 )*zt+     1.0
                                ; S T-Polynome:                     T^3 S                  T S                       S
                zr4= ( ( -2.2744455733317707e-9*zt*zt+6.0399864718597388e-6)*zt-5.1268124398160734e-4 )*zs
                                ; S T-Polynome:                     T^2 S^3/2               S^3/2
                zr5= ( -1.3409379420216683e-9*zt*zt-3.6138532339703262e-5)*zs*zsr
	;
	;   ... masked neutral density
	            zrho= ( zr1 + zr2 ) / ( zr3 + zr4 + zr5 ) * tmask
return, zrho 
end
