$PBExportHeader$uo_ctlcalinspectores.sru
$PBExportComments$Objeto de Usuario de Validación de Inspectores.
forward
global type uo_ctlcalinspectores from nonvisualobject
end type
end forward

global type uo_ctlcalinspectores from nonvisualobject
end type
global uo_ctlcalinspectores uo_ctlcalinspectores

type variables
Integer	CodigoInspector, TipoInspeccion
String	NombreInspector, AbreviInspector
end variables

forward prototypes
public function boolean existe (transaction at_trans, integer ai_inspector, boolean ab_conmensaje)
public function boolean tipoinspeccion (integer ai_tipo, integer ai_inspector, boolean ab_mensaje, transaction at_trans)
end prototypes

public function boolean existe (transaction at_trans, integer ai_inspector, boolean ab_conmensaje);SELECT ccin_codigo, ccin_nombre, ccin_abrevi
	INTO	:CodigoInspector, :NombreInspector, :AbreviInspector
	FROM	dbo.ctlcalinspectores
	WHERE	ccin_codigo	=	:ai_inspector
	Using	at_trans;

IF At_Trans.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Trans,"Lectura de Tabla de Inspectores")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	IF ab_conmensaje	=	True THEN
		MessageBox("Atención", "Código de Inspector (" + String(ai_inspector, '0000') + "), no ha sido~r" + &
		"ingresado en tabla respectiva.~r~rIngrese o seleccione otro Código.")
	END IF
	RETURN False
END IF

RETURN True
end function

public function boolean tipoinspeccion (integer ai_tipo, integer ai_inspector, boolean ab_mensaje, transaction at_trans);SELECT ccin_codigo, ccin_nombre, ccin_abrevi, ccin_tipins
	INTO	:CodigoInspector, :NombreInspector, :AbreviInspector, :TipoInspeccion
	FROM	dbo.ctlcalinspectores
	WHERE	ccin_tipins =  :ai_tipo 
	AND   ccin_codigo	=	:ai_inspector
	Using	at_trans;

IF At_Trans.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Trans,"Lectura de Tabla de Inspectores")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	IF ab_mensaje	THEN
		MessageBox("Atención", "Código de Inspector (" + String(ai_inspector, '0000') + "), ~r" + &
		"no corresponde a tipo Inspección (" + String(ai_tipo) + ").~r~rIngrese o seleccione otro Código.")
	END IF
	RETURN False
END IF

RETURN True
end function

on uo_ctlcalinspectores.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_ctlcalinspectores.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

