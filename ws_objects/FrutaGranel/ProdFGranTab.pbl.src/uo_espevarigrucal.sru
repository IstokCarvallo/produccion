$PBExportHeader$uo_espevarigrucal.sru
$PBExportComments$Objeto de Validación de Grupo según Especie y Variedad
forward
global type uo_espevarigrucal from nonvisualobject
end type
end forward

global type uo_espevarigrucal from nonvisualobject
end type
global uo_espevarigrucal uo_espevarigrucal

type variables
Integer	Especie, Variedad, Dano
String	Nombre, Codigo
end variables

forward prototypes
public function boolean existe (integer ai_especie, integer ai_variedad, string as_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_especie, integer ai_variedad, string as_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	espe_codigo, vari_codigo, evdc_grucal, evdc_despor, evdc_codano
	INTO	:Especie, :Variedad, :Codigo, :Nombre, :Dano
	FROM	dba.spro_espevarigrucal
	WHERE	espe_codigo	=	:ai_Especie
	AND	vari_codigo	=	:ai_Variedad
	AND	evdc_grucal	=	:as_Codigo
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla espevarigrucal")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código Distribución de Calibre  " + as_Codigo + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
	lb_Retorno	=	False
	
END IF

RETURN lb_Retorno
end function

on uo_espevarigrucal.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_espevarigrucal.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

