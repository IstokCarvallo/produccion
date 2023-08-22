$PBExportHeader$uo_ctlcalagronomos.sru
$PBExportComments$Objeto de Exportadores con controles básicos a la entidad
forward
global type uo_ctlcalagronomos from nonvisualobject
end type
end forward

global type uo_ctlcalagronomos from nonvisualobject
end type
global uo_ctlcalagronomos uo_ctlcalagronomos

type variables
Integer	ii_zona_codigo, ii_ccag_codigo
String	is_ccag_nombre,is_ccag_abrevi
end variables

forward prototypes
public function boolean ofp_recupera_ctlcalagronomos (transaction at_trans, integer ai_zona_codigo, integer ai_ccag_codigo, boolean ab_conmensaje)
end prototypes

public function boolean ofp_recupera_ctlcalagronomos (transaction at_trans, integer ai_zona_codigo, integer ai_ccag_codigo, boolean ab_conmensaje);SELECT zona_codigo, ccag_codigo, ccag_nombre, ccag_abrevi
	INTO	:ii_zona_codigo, :ii_ccag_codigo, :is_ccag_nombre, :is_ccag_abrevi
	FROM	dbo.ctlcalagronomos
	WHERE	:ai_zona_codigo in (0,zona_codigo)	
	AND	ccag_codigo	=	:ai_ccag_codigo
	Using	at_trans;

IF At_Trans.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Trans,"Lectura de CtlCalAgronomos")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	IF ab_conmensaje	=	True THEN
		MessageBox("Atención", "Código de Agrónomo (" + String(ai_zona_codigo, '000') + &
		" / " + String(ai_ccag_codigo, '0000') + "), no ha sido~r" + &
		"ingresado en tabla respectiva.~r~rIngrese o Seleccione otro Código.")
	END IF
	RETURN False
END IF

RETURN True
end function

on uo_ctlcalagronomos.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_ctlcalagronomos.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

