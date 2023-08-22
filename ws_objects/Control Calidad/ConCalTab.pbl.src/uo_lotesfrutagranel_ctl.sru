$PBExportHeader$uo_lotesfrutagranel_ctl.sru
$PBExportComments$Objeto de Validación de Lotes Fruta Granel
forward
global type uo_lotesfrutagranel_ctl from nonvisualobject
end type
end forward

global type uo_lotesfrutagranel_ctl from nonvisualobject
end type
global uo_lotesfrutagranel_ctl uo_lotesfrutagranel_ctl

type variables
Integer		Planta, Especie, Numero, Variedad, Predio, &
				CentroCosto, Ducha, PeriodoFrio, PreFrio, CondicCCal, &
				Categoria, LlenaOptimo, Encarpado, EmpolPerfo, TotalBulto
Decimal{3}	TotalNeto, KilosPromed
Long			NroFolio, SaldoExistencia, Productor
DateTime	FechaCosecha, FechaCCal, HoraCCal
String			TipoFrio, Observac, NomPlanta, NomEspecie

uo_Especie		iuo_Especie
uo_Plantadesp	iuo_Planta
end variables

forward prototypes
public function boolean existe (integer ai_planta, integer ai_especie, integer ai_numero, boolean ab_mensaje, transaction at_transaccion)
public function boolean existeordenproceso (integer ai_plantaproc, integer ai_tipoorden, integer al_orden, integer ai_plantalote, integer ai_especie, integer ai_lote, boolean ab_mensaje, transaction at_transaccion)
public function boolean obtienesaldocamara (integer ai_planta, integer ai_camara, integer ai_tipoenvase, integer ai_envase, ref long al_saldo, boolean ab_mensaje, transaction at_transaccion)
public function long saldoexistencia (integer ai_especie, integer ai_variedad, long al_productor, string as_tratamiento, integer ai_periodo, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_planta, integer ai_especie, integer ai_numero, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

If Not iuo_Planta.Existe(ai_planta, True, Sqlca) Then
	lb_Retorno	=	False
Else
	If Not iuo_Especie.Existe(ai_Especie, True, Sqlca)	Then
		lb_Retorno	=	False
	Else
			SELECT	lote_pltcod, lote_espcod, lote_codigo, vari_codigo, prod_codigo,
				prbr_codpre, prcc_codigo, lote_ducha, frio_tipofr, pefr_codigo,
				fgcc_prefri, cocc_codigo, cate_codigo, fgcc_feccos, fgcc_nrofol,
				fgcc_feccon, fgcc_horcon, fgcc_llenop, fgcc_encarp, fgcc_empper,
				fgcc_observ, lote_totbul, lote_totnet, lote_kilpro
		INTO	:Planta, :Especie, :Numero, :Variedad, :Productor, :Predio,
				:CentroCosto, :Ducha, :TipoFrio, :PeriodoFrio, :PreFrio, 
				:CondicCCal, :Categoria, :FechaCosecha, :NroFolio, :FechaCCal,
				:HoraCCal, :LlenaOptimo, :Encarpado, :EmpolPerfo, :Observac,
				:TotalBulto, :TotalNeto, :KilosPromed
		FROM	dba.spro_lotesfrutagranel
		WHERE	lote_pltcod	=	:ai_Planta
		AND	lote_espcod	=	:ai_Especie
		AND	lote_codigo	=	:ai_Numero
		USING at_Transaccion;
	
		IF at_Transaccion.SQLCode = -1 Then
			F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Lotes Fruta Granel")
			
			lb_Retorno	=	False
		ElseIf at_Transaccion.SQLCode = 100 Then
			If ab_Mensaje Then
				MessageBox("Atención", "Número de Lote " + String(ai_Numero, '0000') + &
							", no ha sido registrado para Planta '" + Trim(NomPlanta) + &
						" y Especie '" + Trim(NomEspecie) + "'.~r~r" + &
						"Ingrese o seleccione otro Lote.")	
			End If
			lb_Retorno	=	False
		End If
	End If
End If

Return lb_Retorno
end function

public function boolean existeordenproceso (integer ai_plantaproc, integer ai_tipoorden, integer al_orden, integer ai_plantalote, integer ai_especie, integer ai_lote, boolean ab_mensaje, transaction at_transaccion);/*
	Chequea que Lote esté considerado en Orden de Proceso
	mediante la revisión de los lotes Despachados para Proceso
*/
Integer	li_Existe
Boolean	lb_Retorno

SELECT	Count(*)
	INTO	:li_Existe
	FROM	dba.spro_movtofrutagranenca en, dba.spro_movtofrutagrandeta de
	WHERE	en.plde_codigo	=	:ai_PlantaProc
	AND	en.defg_tipdoc	=	:ai_TipoOrden
	AND	en.defg_docrel	=	:al_Orden
	AND	de.plde_codigo	=	en.plde_codigo
	AND	de.tpmv_codigo	=	en.tpmv_codigo
	AND	de.mfge_numero	=	en.mfge_numero
	AND	de.lote_pltcod	=	:ai_PlantaLote
	AND	de.lote_espcod	=	:ai_Especie
	AND	de.lote_codigo	=	:ai_Lote
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Encabezado y " + &
							"Detalle de Movimiento Fruta Granel")
ELSEIF li_Existe = 0 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Lote no fue registrado en despacho de~r" + &
					"Frigorífico a Orden de Proceso especificada." + &
					"~r~rIngrese o seleccione otro Lote.")	
	END IF				
ELSE
	lb_Retorno	=	True
END IF

RETURN True
end function

public function boolean obtienesaldocamara (integer ai_planta, integer ai_camara, integer ai_tipoenvase, integer ai_envase, ref long al_saldo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno	=	True


SELECT caex_canbul
	INTO	:al_Saldo
	FROM	dba.spro_camaraexistefg
	WHERE	plde_codigo =	:ai_Planta
	AND	cama_codigo =	:ai_Camara
	AND	lote_pltcod =	:Planta
	AND	lote_espcod =	:Especie
	AND	lote_codigo =	:Numero
	AND	enva_tipoen =	:ai_TipoEnvase
	AND	enva_codigo =	:ai_Envase
	USING at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura Existencia en Cámaras")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 OR Isnull(al_Saldo) OR al_Saldo = 0 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención","No Existe Saldo en la Cámara "+String(ai_Camara,'00')+&
						" para Lote "+String(Planta,'0000')+String(Especie,'00')+String(Numero,'0000')+&
						" y Envase "+String(ai_TipoEnvase,'0')+'-'+String(ai_Envase,'000'))
	END IF
	
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
					
end function

public function long saldoexistencia (integer ai_especie, integer ai_variedad, long al_productor, string as_tratamiento, integer ai_periodo, transaction at_transaccion);
//Se saca la sgte opción por no estar completamente definida su utilidad
// SELECT	Sum(cam.caex_canbul ) - IsNull(cam.caex_bulcom,0)
ai_especie = ai_especie * 1
SELECT	Sum(isnull(cam.caex_canbul,0))
	INTO	:SaldoExistencia
	FROM	dba.spro_lotesfrutagrandeta lfd,dba.spro_lotesfrutagranel lfg,
			dba.spro_camaraexistefg cam
	WHERE	lfg.lote_espcod =	:ai_Especie
	AND	lfg.vari_codigo =	:ai_Variedad
	AND	lfg.prod_codigo =	:al_Productor
	AND	lfg.frio_tipofr =	:as_Tratamiento
	AND	lfg.pefr_codigo =	:ai_Periodo
	AND	lfg.lote_pltcod = lfd.lote_pltcod
	AND	lfg.lote_espcod = lfd.lote_espcod
	AND	lfg.lote_codigo = lfd.lote_codigo
   AND   lfd.lote_pltcod = cam.lote_pltcod
   AND   lfd.lote_espcod = cam.lote_espcod
   AND   lfd.lote_codigo = cam.lote_codigo
   AND   lfd.enva_tipoen = cam.enva_tipoen
   AND   lfd.enva_codigo = cam.enva_codigo
   AND   lfd.lotd_kilpro > 0
	USING at_Transaccion;
	
	IF at_Transaccion.SQLCode = -1 THEN
		F_ErrorBaseDatos(at_Transaccion, "Lectura de Existencia de Bultos")
	
		SaldoExistencia = 0
	END IF

RETURN SaldoExistencia
end function

on uo_lotesfrutagranel_ctl.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_lotesfrutagranel_ctl.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;iuo_Especie	=	Create uo_Especie
iuo_Planta	=	Create uo_Plantadesp
end event

event destructor;Destroy iuo_Especie
Destroy iuo_Planta	
end event

