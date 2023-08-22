$PBExportHeader$uo_especiecatego.sru
$PBExportComments$Objeto de Validación de Categoria según Especie y Variedad
forward
global type uo_especiecatego from nonvisualobject
end type
end forward

global type uo_especiecatego from nonvisualobject
end type
global uo_especiecatego uo_especiecatego

type variables
Integer	Especie, Variedad, Codigo, Dano
String	Nombre
end variables

forward prototypes
public function boolean existe (integer ai_especie, integer ai_variedad, integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_especie, integer ai_variedad, integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	espe_codigo, vari_codigo, cate_codigo, esca_despor, esca_codano
	INTO	:Especie, :Variedad, :Codigo, :Nombre, :Dano
	FROM	dbo.spro_especiecatego
	WHERE	espe_codigo	=	:ai_Especie
	AND	vari_codigo	=	:ai_Variedad
	AND	cate_codigo	=	:ai_Codigo
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla EspecieCatego")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código Color Cubrimiento  " + String(ai_Codigo, '00') + &
						", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
	lb_Retorno	=	False
	
END IF

RETURN lb_Retorno
end function

on uo_especiecatego.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_especiecatego.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

