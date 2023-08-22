$PBExportHeader$uo_entidades.sru
forward
global type uo_entidades from nonvisualobject
end type
end forward

global type uo_entidades from nonvisualobject
end type
global uo_entidades uo_entidades

type variables
Integer	planta, faena
String	rut
Long  	codigo
end variables

forward prototypes
public function boolean existeembaladora (long al_embaladora, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existeembaladora (long al_embaladora, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_retorno	=	True

select plde_codigo,enpa_rutper,faen_codigo,enpa_codper
  into :planta, :rut, :faena, :codigo
  from dbo.spro_entidadespacking  
 where enpa_codper	=	:al_embaladora
 using at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Entidades de Packing")
	lb_Retorno	=	False
	
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Embaladora" + String(al_embaladora, '00000') + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
END IF

RETURN lb_Retorno
end function

on uo_entidades.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_entidades.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

