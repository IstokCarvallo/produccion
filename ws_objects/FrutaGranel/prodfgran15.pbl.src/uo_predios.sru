$PBExportHeader$uo_predios.sru
$PBExportComments$Objeto de Validación de Productores
forward
global type uo_predios from nonvisualobject
end type
end forward

global type uo_predios from nonvisualobject
end type
global uo_predios uo_predios

type variables
Integer	Codpred, Comuna, Ano_Prop, Ano_Usu, TipoDominio
String   Predio, DeslindeNorte, DeslindeSur, DeslindeOriente, DeslindePoniente,&
			Propietario, Fojas_Prop, Rol_Prop, Ins_Prop, Conservador, Fojas_Usu, Rol_Usu, Notario
DateTime	FechaArrend
Long     Codprod
end variables

forward prototypes
private function boolean existe (integer ai_codpred, long al_codprod, boolean ab_mensaje, transaction at_transaccion)
public function boolean existe (transaction at_transaccion, boolean ab_mensaje, long al_codprod, integer ai_codpred)
end prototypes

private function boolean existe (integer ai_codpred, long al_codprod, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno	=	True

IF Not IsNull(al_CodProd) OR Not IsNull(ai_CodPred) THEN
	SELECT	prod_codigo, prbr_codpre, prbr_predio, comu_codigo, prbr_desnor,
				prbr_dessur, prbr_desori, prbr_despon, prbr_anomde, prbr_fojas,
				prbr_nrorol, prbr_numero, prbr_conser, prbr_anoins, prbr_fojusu, 
				prbr_rolusu, prbr_anousu, prbr_notari, prbr_fecarr, prbr_tipdom
		INTO	:codprod, :codpred, :predio, :comuna, :DeslindeNorte,
				:DeslindeSur, :DeslindeOriente, :DeslindePoniente, 
				:Propietario, :Fojas_Prop, :Rol_Prop, :Ins_Prop, :Conservador,
				:Ano_Prop, :Fojas_Usu, :Rol_Usu, :Ano_Usu, :Notario, 
				:FechaArrend, :TipoDominio
		FROM	dbo.prodbienraiz
		WHERE	prod_codigo	=	:al_codprod
		AND   prbr_codpre =  :ai_codpred
		USING	at_Transaccion;
	
	IF at_Transaccion.SQLCode = -1 THEN
		F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla ProdBienRaiz")
		
		lb_Retorno	=	False
	ELSEIF at_Transaccion.SQLCode = 100 THEN
		IF ab_Mensaje THEN
			MessageBox("Atención", "Código de Predio " + String(ai_codpred, '00') + &
						", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
		END IF
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean existe (transaction at_transaccion, boolean ab_mensaje, long al_codprod, integer ai_codpred);Boolean	lb_Retorno	=	True

IF Not IsNull(al_CodProd) OR Not IsNull(ai_CodPred) THEN
	SELECT	prod_codigo, prbr_codpre, prbr_predio, comu_codigo, prbr_desnor,
				prbr_dessur, prbr_desori, prbr_despon, prbr_anomde, prbr_fojas,
				prbr_nrorol, prbr_numero, prbr_conser, prbr_anoins, prbr_fojusu, 
				prbr_rolusu, prbr_anousu, prbr_notari, prbr_fecarr, prbr_tipdom
		INTO	:codprod, :codpred, :predio, :comuna, :DeslindeNorte,
				:DeslindeSur, :DeslindeOriente, :DeslindePoniente, 
				:Propietario, :Fojas_Prop, :Rol_Prop, :Ins_Prop, :Conservador,
				:Ano_Prop, :Fojas_Usu, :Rol_Usu, :Ano_Usu, :Notario, 
				:FechaArrend, :TipoDominio
		FROM	dbo.prodbienraiz
		WHERE	prod_codigo	=	:al_codprod
		AND   prbr_codpre =  :ai_codpred
		USING	at_Transaccion;
	
	IF at_Transaccion.SQLCode = -1 THEN
		F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla ProdBienRaiz")
		
		lb_Retorno	=	False
	ELSEIF at_Transaccion.SQLCode = 100 THEN
		IF ab_Mensaje THEN
			MessageBox("Atención", "Código de Predio " + String(ai_codpred, '00') + &
						", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
		END IF
	END IF
END IF

RETURN lb_Retorno
end function

on uo_predios.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_predios.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

