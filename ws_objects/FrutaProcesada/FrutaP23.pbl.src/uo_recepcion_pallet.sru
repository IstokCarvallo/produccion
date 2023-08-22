$PBExportHeader$uo_recepcion_pallet.sru
$PBExportComments$Objeto de Validación de Existencia de Pallet.
forward
global type uo_recepcion_pallet from nonvisualobject
end type
end forward

global type uo_recepcion_pallet from nonvisualobject
end type
global uo_recepcion_pallet uo_recepcion_pallet

type variables
Date	 	FechaRec
Integer	TipoEn,PlantaOri,Condicion,Planta
Long		Numero,nrores
end variables

forward prototypes
public function boolean existe (integer ai_cliente, integer ai_planta, long al_numero, boolean ab_mensaje, transaction at_transaccion)
public function boolean recepcion (integer ai_planta, long al_numero, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_cliente, integer ai_planta, long al_numero, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True
  
SELECT 	rfpe_numero
  INTO	:Numero
  FROM  	dbo.recfruproced
  WHERE 	clie_codigo	=	:ai_Cliente 
  AND	  	plde_codigo	= 	:ai_Planta 
  AND   	paen_numero =  :al_Numero
  USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla recfruproced")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	
	lb_Retorno	=	False
	
	IF ab_Mensaje THEN
		MessageBox("Atención", "Número de Pallet " + String(al_Numero, '00000000') + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Número.")
	END IF	
END IF

RETURN lb_Retorno
end function

public function boolean recepcion (integer ai_planta, long al_numero, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT 	rfpe_fecrec,rfpe_tipoen,rfpe_ptaori,frre_codigo,rfpe_nrores,plde_codigo
  INTO	:FechaRec,:TipoEn,:PlantaOri,:Condicion,:nrores,:Planta
  FROM  	dbo.recfruprocee
  WHERE  plde_codigo	= 	:ai_Planta 
  AND   	rfpe_numero =  :al_Numero
  USING at_Transaccion;
  
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla recfruproced")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	
	lb_Retorno	=	False
	
	IF ab_Mensaje THEN
		MessageBox("Atención", "Número de Recepción " + String(al_Numero, '00000000') + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Número.")
	END IF	
END IF

RETURN lb_Retorno
end function

on uo_recepcion_pallet.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_recepcion_pallet.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

