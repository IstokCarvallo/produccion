$PBExportHeader$uo_tipopallet.sru
$PBExportComments$Objeto de Validación de tipo de Pallet
forward
global type uo_tipopallet from nonvisualobject
end type
end forward

global type uo_tipopallet from nonvisualobject
end type
global uo_tipopallet uo_tipopallet

type variables
Integer	Codigo,Cajas,Altura

end variables

forward prototypes
public function boolean existe (integer ai_cliente, string as_embalaje, string as_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_cliente, string as_embalaje, string as_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

  SELECT tpem_codigo,tpem_cancaj,tpem_altura  
    INTO :Codigo,:Cajas,:Altura  
    FROM dbo.tipopallemba  
   WHERE clie_codigo = :ai_cliente  AND  
         emba_codigo = :as_embalaje AND  
         tpem_codigo = :as_codigo   
   USING	at_Transaccion;      
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla de Tipo de Pallet")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Tipo de Pallet " + String(as_Codigo) + &
					", no ha sido Ingresado.~r~rIngrese o seleccione otro Código.")	
	END IF
END IF

RETURN lb_Retorno
end function

on uo_tipopallet.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_tipopallet.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

