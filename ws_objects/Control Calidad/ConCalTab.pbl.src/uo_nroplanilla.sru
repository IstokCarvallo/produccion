$PBExportHeader$uo_nroplanilla.sru
$PBExportComments$Objecto Usuario que valida la Existencia de Tipos de Transporte.
forward
global type uo_nroplanilla from nonvisualobject
end type
end forward

global type uo_nroplanilla from nonvisualobject
end type
global uo_nroplanilla uo_nroplanilla

type variables
Long NUMERO
end variables

forward prototypes
public function boolean existe (long al_planilla, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (long al_planilla, boolean ab_mensaje, transaction at_transaccion);
SELECT cpde_numero
  INTO :numero
  FROM dba.ctlcalplandestinosenc
  WHERE cpde_numero =: al_planilla
  AND   clie_codigo =: gi_CodExport
  Using	At_Transaccion;

IF At_Transaccion.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Transaccion, "Lectura de Tabla de destinos")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Número de Planilla " + String(al_planilla) + &
						", no ha sido~r" + &
						"ingresado en tabla respectiva.~r~r" + &
						"Ingrese o seleccione otro Número.")
	END IF	
	RETURN False
END IF

RETURN True
end function

on uo_nroplanilla.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_nroplanilla.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

