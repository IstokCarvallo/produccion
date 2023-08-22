$PBExportHeader$uo_centrocostos.sru
$PBExportComments$Objeto de Validación de Centros de Costos
forward
global type uo_centrocostos from nonvisualobject
end type
end forward

global type uo_centrocostos from nonvisualobject
end type
global uo_centrocostos uo_centrocostos

type variables

Integer	Codpred, CodCCosto, CodEspe, CodVari
String   CentroCosto
Long		Codprod
end variables

forward prototypes
public function boolean existe (transaction at_transaccion, boolean ab_mensaje, long al_codprod, integer ai_codpred, integer ai_codcc)
end prototypes

public function boolean existe (transaction at_transaccion, boolean ab_mensaje, long al_codprod, integer ai_codpred, integer ai_codcc);Boolean	lb_Retorno	=	True

SELECT	prod_codigo, prbr_codpre, prcc_codigo, prcc_nombre, espe_codigo, vari_codigo
	INTO	:codprod, :codpred, :CodCCosto, :CentroCosto, :CodEspe, :CodVari
	FROM	dbo.prodcuarteles
	WHERE	prod_codigo	=	:al_codprod
	AND   prbr_codpre =  :ai_codpred
	AND   prcc_codigo =  :ai_codcc
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla ProdCuarteles")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Centro Costo " + String(ai_codcc, '00') + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF	
END IF

RETURN lb_Retorno
end function

on uo_centrocostos.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_centrocostos.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

