$PBExportHeader$uo_lotescorrelequipo.sru
forward
global type uo_lotescorrelequipo from nonvisualobject
end type
end forward

global type uo_lotescorrelequipo from nonvisualobject
end type
global uo_lotescorrelequipo uo_lotescorrelequipo

type variables
long		loco_ultcor, 	loco_ultcom, 	loco_ulcore, 	loco_ulcove, 	loco_ulcocl, loco_comlin, loco_nropal
long 		loco_inicor, 	loco_inicom, 	loco_incore, 	loco_incocl, 	loco_comcor, il_correcompa
String	loco_dwcomp
end variables

forward prototypes
public function boolean existe (integer ai_planta, integer ai_especie, string as_computador, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_planta, integer ai_especie, string as_computador, boolean ab_mensaje, transaction at_transaccion);Boolean lb_retorno = TRUE

IF ai_especie <> 99 THEN
	
	SELECT 	loco_ultcor,  loco_ultcom,  loco_ulcore,  loco_ulcove, 
				loco_ulcocl,  loco_inicor,  loco_inicom,  loco_incore, 
				loco_incocl,  IsNull(loco_comcor, 0), 	   loco_comlin
		INTO :loco_ultcor, :loco_ultcom, :loco_ulcore, :loco_ulcove, 
			  :loco_ulcocl, :loco_inicor, :loco_inicom, :loco_incore, 
			  :loco_incocl, :loco_comcor, :loco_comlin
	   FROM dbo.spro_lotescorrelequipo
	  WHERE plde_codigo = :ai_planta
  		 AND espe_codigo = 99
		 AND equi_nombre = :as_computador
	  USING at_Transaccion;
ELSE
	
	SELECT 	IsNull(loco_comcor, 0) + 1, loco_comlin, loco_nropal, loco_dwcomp
		 INTO :loco_comcor, :loco_comlin, :loco_nropal, :loco_dwcomp
		 FROM dbo.spro_correlcompequipo
		WHERE plde_codigo = :ai_planta
		  AND equi_nombre = :as_computador
		USING at_Transaccion;
	
END IF			
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Correlativos por Equipo")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		IF ai_especie <> 99 THEN
			MessageBox("Atención", "Correlativos para (" + String(ai_Planta, '0000') + " - " + &
							String(ai_especie, '00') + " - "  + String(as_computador) +   &
							"), no ha sido creado en tabla respectiva.~r~r" + &
							"Ingrese o seleccione otro(s).")
		ELSE
			MessageBox("Atención", "Correlativos de Compactos para (" + &
							String(ai_Planta, '0000') + " - " + String(as_computador) +   &
							"), no ha sido creado en tabla respectiva.~r~r" + &
							"Ingrese o seleccione otro(s).")
		END IF
	END IF
ELSE
	il_correcompa	=	loco_comcor + (loco_comlin * 1000000)// + 1
END IF

RETURN lb_Retorno
end function

on uo_lotescorrelequipo.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_lotescorrelequipo.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

