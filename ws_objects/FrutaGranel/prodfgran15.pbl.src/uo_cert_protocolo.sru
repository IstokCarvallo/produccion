$PBExportHeader$uo_cert_protocolo.sru
$PBExportComments$Objeto de Validación de Transportístas
forward
global type uo_cert_protocolo from nonvisualobject
end type
end forward

global type uo_cert_protocolo from nonvisualobject
end type
global uo_cert_protocolo uo_cert_protocolo

type variables
Integer	prot_codigo
String	prot_nombre, prot_abrevi
end variables

forward prototypes
public function boolean existe (long al_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (long al_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT prot_codigo, prot_nombre, prot_abrevi
  INTO :prot_codigo, :prot_nombre, :prot_abrevi
  FROM dba.cert_protocolo
 WHERE prot_codigo	=	:al_Codigo
 USING at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Cert Protocolos")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Protocolo " + String(al_Codigo) + &
				", no ha sido Ingresado.~r~rIngrese o seleccione otro Código.")	
	END IF
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

on uo_cert_protocolo.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_cert_protocolo.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

