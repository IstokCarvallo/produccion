$PBExportHeader$uo_tipopallet.sru
$PBExportComments$Objeto de Validación de Tipo de Pallet por Envase
forward
global type uo_tipopallet from nonvisualobject
end type
end forward

global type uo_tipopallet from nonvisualobject
end type
global uo_tipopallet uo_tipopallet

type variables
Integer  TipoEnvase, Envase, ALtura, Cajas
String	TipoPallet
end variables

forward prototypes
public function boolean existe (integer ai_tipo, integer ai_envase, string as_tipopallet, boolean ab_mensaje, transaction at_transaccion)
public function boolean existe_porembalaje (integer ai_cliente, string as_embalaje, string as_tipopallet, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_tipo, integer ai_envase, string as_tipopallet, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno	=	True

SELECT	enva_tipoen, enva_codigo, tpen_codigo, tpen_altura, tpen_cancaj
	INTO	:TipoEnvase, :Envase, :TipoPallet, :ALtura, :Cajas
	FROM	dbo.tipopallenvase
	WHERE	enva_tipoen	=	:ai_tipo
	AND   enva_codigo =  :ai_envase
	AND	tpen_codigo =	:as_TipoPallet
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Tipo de Pallet por Envase")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Tipo de Pallet " + as_TipoPallet + &
				", no ha sido registrado para envase seleccionado.~r~rIngrese o seleccione otro Código.")
	END IF
	lb_Retorno	=	False
END IF


RETURN lb_Retorno
end function

public function boolean existe_porembalaje (integer ai_cliente, string as_embalaje, string as_tipopallet, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno	=	True

  SELECT tpem_altura, tpem_cancaj, tpem_codigo
	INTO :ALtura, :Cajas, :TipoPallet
	FROM dbo.tipopallemba   
	WHERE	clie_codigo	=	:ai_cliente
		AND   emba_codigo =  :as_embalaje
		AND	tpem_codigo =	:as_TipoPallet
		USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Tipo de Pallet por Envase")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Tipo de Pallet " + as_TipoPallet + &
				", no ha sido registrado para envase seleccionado.~r~rIngrese o seleccione otro Código.")
	END IF
	lb_Retorno	=	False
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

