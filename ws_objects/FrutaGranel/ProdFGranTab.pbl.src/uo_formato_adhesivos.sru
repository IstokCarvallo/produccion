$PBExportHeader$uo_formato_adhesivos.sru
forward
global type uo_formato_adhesivos from nonvisualobject
end type
end forward

global type uo_formato_adhesivos from nonvisualobject
end type
global uo_formato_adhesivos uo_formato_adhesivos

type variables
String	ls_formato, is_impresoracomp
end variables

forward prototypes
public function boolean existe (long al_planta, integer ai_especie, string as_equipo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (long al_planta, integer ai_especie, string as_equipo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_retorno	=	True


SELECT IsNull(loco_dwfrmt, ''), loco_impcom
  INTO :ls_formato, :is_impresoracomp
  FROM dbo.spro_lotescorrelequipo
 WHERE equi_nombre	=:as_equipo
   AND plde_codigo	=:al_planta
	AND espe_codigo	=:ai_especie
 USING at_transaccion;
 
 IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Correlativos de Lotes por Equipo")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 OR ls_formato = "" THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "PC no tiene asignado un formato de Adhesivo.~r~r"+&
									  "Favor ingrese formato correspondiente en el mantenedor de correlativos de lotes.")	
	END IF
END IF

RETURN lb_Retorno
end function

on uo_formato_adhesivos.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_formato_adhesivos.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

