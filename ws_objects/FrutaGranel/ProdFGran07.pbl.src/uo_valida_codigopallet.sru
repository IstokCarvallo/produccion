$PBExportHeader$uo_valida_codigopallet.sru
forward
global type uo_valida_codigopallet from nonvisualobject
end type
end forward

global type uo_valida_codigopallet from nonvisualobject
end type
global uo_valida_codigopallet uo_valida_codigopallet

type variables
Integer	copa_codigo
String	copa_descri
Decimal	copa_anchos, copa_largos
end variables

forward prototypes
public function boolean existe (integer ai_copa_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_copa_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno	=	True

SELECT  copa_codigo,  copa_descri,  copa_anchos,  copa_largos
  INTO :copa_codigo, :copa_descri, :copa_anchos, :copa_largos
  FROM dbo.codigopallet
 WHERE copa_codigo	=	:ai_copa_codigo
 USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla codigopallet")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Tipo de Pallet " + String(ai_copa_codigo) + &
				", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
	lb_Retorno	=	False
END IF


RETURN lb_Retorno
end function

on uo_valida_codigopallet.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_valida_codigopallet.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

