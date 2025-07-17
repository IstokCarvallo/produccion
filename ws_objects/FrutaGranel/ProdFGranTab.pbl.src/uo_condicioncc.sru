$PBExportHeader$uo_condicioncc.sru
$PBExportComments$Objeto de Validación de Condiciones Control de Calidad.
forward
global type uo_condicioncc from nonvisualobject
end type
end forward

global type uo_condicioncc from nonvisualobject
end type
global uo_condicioncc uo_condicioncc

type variables
Integer	Codigo
String		Nombre
end variables

forward prototypes
public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	cocc_codigo, cocc_nombre
	INTO	:Codigo, :Nombre
	FROM	dbo.spro_condicioncc
	WHERE	cocc_codigo	=	:ai_Codigo
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Condición Control de Calidad")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100  THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Condición " + String(ai_Codigo, '00') + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

on uo_condicioncc.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_condicioncc.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

