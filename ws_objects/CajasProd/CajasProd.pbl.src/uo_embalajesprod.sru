$PBExportHeader$uo_embalajesprod.sru
$PBExportComments$Objeto de Validación de Embalajesprod
forward
global type uo_embalajesprod from nonvisualobject
end type
end forward

global type uo_embalajesprod from nonvisualobject
end type
global uo_embalajesprod uo_embalajesprod

type variables
Integer	CodEnvase,TipoEnvase,CodEtiq
String	Codigo, Nombre, Abrevi 
end variables

forward prototypes
public function boolean existe (integer ai_cliente, string as_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_cliente, string as_codigo, boolean ab_mensaje, transaction at_transaccion);SELECT	emba_codigo, emba_abrevi, emba_nombre, enva_tipoen,
		   etiq_codigo
	INTO	:Codigo,:Abrevi,:Nombre,:TipoEnvase,:CodEtiq
	FROM	dbo.embalajesprod
	WHERE	clie_codigo =	:ai_cliente
	AND	emba_codigo =	:as_Codigo	
	Using	At_Transaccion;

IF At_Transaccion.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Transaccion,"Lectura de Tabla de Embalajes")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Embalaje " + as_Codigo + ", no ha sido~r" + &
		"ingresado en tabla respectiva.~r~rIngrese o seleccione otro Código.")
	END IF
	
	RETURN False
END IF

RETURN True
end function

on uo_embalajesprod.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_embalajesprod.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

