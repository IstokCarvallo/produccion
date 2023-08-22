$PBExportHeader$uo_prodestadocert.sru
$PBExportComments$Objeto de Validación de Estados de Certificacion
forward
global type uo_prodestadocert from nonvisualobject
end type
end forward

global type uo_prodestadocert from nonvisualobject
end type
global uo_prodestadocert uo_prodestadocert

type variables
Long	Codigo
String	Nombre, Abrevi

end variables

forward prototypes
public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT		prec_codigo, prec_nombre
	INTO		:Codigo,:Nombre
	FROM		dbo.spro_prodestadocertific
	WHERE	prec_codigo	=	:ai_Codigo
	USING	at_Transaccion; 
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla spro_prodestadocertific")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Protocolo " + String(ai_Codigo) + &
					", no ha sido Ingresado.~r~rIngrese o seleccione otro Código.")	
	END IF
END IF

RETURN lb_Retorno
end function

on uo_prodestadocert.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_prodestadocert.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

