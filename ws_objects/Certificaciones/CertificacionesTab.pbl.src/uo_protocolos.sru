$PBExportHeader$uo_protocolos.sru
$PBExportComments$Objeto de Validación de protocolos de Certificacion
forward
global type uo_protocolos from nonvisualobject
end type
end forward

global type uo_protocolos from nonvisualobject
end type
global uo_protocolos uo_protocolos

type variables
Long	Codigo
String	Nombre, Abrevi

end variables

forward prototypes
public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT		prot_codigo, prot_nombre, prot_abrevi
	INTO		:Codigo,:Nombre,:Abrevi
	FROM		dbo.cert_protocolo
	WHERE	prot_codigo	=	:ai_Codigo
	USING	at_Transaccion; 
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Protocolos")
	
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

on uo_protocolos.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_protocolos.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

