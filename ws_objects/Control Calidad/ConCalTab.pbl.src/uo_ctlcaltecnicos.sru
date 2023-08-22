$PBExportHeader$uo_ctlcaltecnicos.sru
$PBExportComments$Objeto de Técnicos con controles básicos a la entidad
forward
global type uo_ctlcaltecnicos from nonvisualobject
end type
end forward

global type uo_ctlcaltecnicos from nonvisualobject
end type
global uo_ctlcaltecnicos uo_ctlcaltecnicos

type variables
Integer	ii_zona_codigo, ii_cctc_codigo
String	is_cctc_nombre,is_cctc_abrevi
end variables

forward prototypes
public function boolean ofp_recupera_ctlcalagronomos (transaction at_trans, integer ai_zona_codigo, integer ai_cctc_codigo, boolean ab_conmensaje)
end prototypes

public function boolean ofp_recupera_ctlcalagronomos (transaction at_trans, integer ai_zona_codigo, integer ai_cctc_codigo, boolean ab_conmensaje);SELECT zona_codigo, cctc_codigo, cctc_nombre, cctc_abrevi
	INTO	:ii_zona_codigo, :ii_cctc_codigo, :is_cctc_nombre, :is_cctc_abrevi
	FROM	dba.ctlcaltecnicos
	WHERE	:ai_zona_codigo in (0,zona_codigo)	
	AND	cctc_codigo	=	:ai_cctc_codigo
	Using	at_trans;

IF At_Trans.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Trans,"Lectura de CtlCalTécnicos")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	IF ab_conmensaje	=	True THEN
		MessageBox("Atención", "Código de Técnico (" + String(ai_zona_codigo, '000') + &
		" / " + String(ai_cctc_codigo, '0000') + "), no ha sido~r" + &
		"ingresado en tabla respectiva.~r~rIngrese o Seleccione otro Código.")
	END IF
	RETURN False
END IF

RETURN True
end function

on uo_ctlcaltecnicos.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_ctlcaltecnicos.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

