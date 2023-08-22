$PBExportHeader$uo_ctlcaltipoinspec.sru
$PBExportComments$Objeto de Usuario de Validación de Tipos de Inspección
forward
global type uo_ctlcaltipoinspec from nonvisualobject
end type
end forward

global type uo_ctlcaltipoinspec from nonvisualobject
end type
global uo_ctlcaltipoinspec uo_ctlcaltipoinspec

type variables
Integer	Codigo
String	Descripcion
end variables

forward prototypes
public function boolean existe (integer ai_tipoinspec, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_tipoinspec, boolean ab_mensaje, transaction at_transaccion);  SELECT ccti_codigo,   
         ccti_descrip  
    INTO :codigo,   
         :descripcion  
    FROM dbo.ctlcaltiposinspeccion  
   WHERE ccti_codigo = :ai_tipoinspec
	Using	at_transaccion;

IF at_transaccion.SqlCode = -1 THEN
	F_ErrorBaseDatos(at_transaccion,"Lectura de Tabla de Tipo Inspección")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	IF ab_mensaje	=	True THEN
		MessageBox("Atención", "Código de Tipo de Inspección (" + String(ai_tipoinspec, '000') + "), no ha sido~r" + &
		"ingresado en tabla respectiva.~r~rIngrese o seleccione otro Código.")
	END IF
	RETURN False
END IF

RETURN True
end function

on uo_ctlcaltipoinspec.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_ctlcaltipoinspec.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

