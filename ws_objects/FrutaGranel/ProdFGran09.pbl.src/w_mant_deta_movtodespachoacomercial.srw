$PBExportHeader$w_mant_deta_movtodespachoacomercial.srw
$PBExportComments$Ventana que genera recepción de lotes en Fruta Comercial
forward
global type w_mant_deta_movtodespachoacomercial from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_movtodespachoacomercial from w_mant_detalle_csd
integer width = 2450
integer height = 1996
end type
global w_mant_deta_movtodespachoacomercial w_mant_deta_movtodespachoacomercial

type variables
uo_plantadesp			iuo_PltaLote
uo_especie				iuo_Especie
uo_lotesfrutagranel	iuo_Lote
uo_productores			iuo_Productor
uo_camarasfrigo		iuo_Camara
uo_tipomovtofruta		iuo_TipoMovto

str_variedad			istr_Variedad
str_envase				istr_Envase

DataWindowChild		idwc_Camara, idwc_Planta, idwc_Especie
DataWindowChild		idwc_tipoenvase  //agrego lm
Dec{3}					id_KilosPromed
end variables

forward prototypes
public subroutine capturakilospromedio (string as_columna, string as_valor)
public function boolean hayexistencia (string as_columna, string as_valor, integer ai_cantidad)
public function boolean duplicado (string as_columna, string as_valor)
public subroutine buscaenvase ()
public subroutine buscalote ()
end prototypes

public subroutine capturakilospromedio (string as_columna, string as_valor);Integer	li_PlantaLote, li_EspecieLote,  &
			li_tipoenva, li_envase
Long     ll_NumeroLote

IF istr_mant.argumento[2] <> '2' THEN
   //dw_1.AcceptText()
	li_PlantaLote	=	dw_1.Object.lote_pltcod[il_Fila]
	li_EspecieLote	=	dw_1.Object.lote_espcod[il_Fila]
	ll_NumeroLote	=	dw_1.Object.lote_codigo[il_Fila]
	li_tipoenva    =  dw_1.Object.enva_tipoen[il_Fila]
	li_envase		=  dw_1.Object.enva_codigo[il_Fila]
	
	CHOOSE CASE as_Columna
			
		CASE "lote_pltcod"
			li_PlantaLote	=	Integer(as_Valor)
			
		CASE "lote_espcod"
			li_EspecieLote	=	Integer(as_Valor)
			
		CASE "lote_codigo"
			ll_NumeroLote	=	Long(as_Valor)
			
		CASE "enva_tipoen"
			li_TipoEnva		=	Integer(as_Valor)
	
		CASE "enva_codigo"
			li_envase		=	Integer(as_Valor)
			
	END CHOOSE
	
	SELECT	Isnull(lotd_kilpro,0)
		INTO	:id_KilosPromed
		FROM	dba.spro_lotesfrutagrandeta
		WHERE	lote_pltcod	=	:li_PlantaLote
		AND	lote_espcod	=	:li_EspecieLote
		AND	lote_codigo	=	:ll_NumeroLote
		AND   enva_tipoen =  :li_TipoEnva
		AND   enva_codigo =  :li_Envase;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura tabla de Detalle Lote Fruta Granel")
		
	END IF
END IF	

RETURN
end subroutine

public function boolean hayexistencia (string as_columna, string as_valor, integer ai_cantidad);Boolean	lb_Retorno = True
Integer	li_Planta, li_Camara, li_PlantaLote, li_EspecieLote,  &
			li_tipoenva, li_envase
Long     ll_saldo, ll_NumeroLote

IF istr_mant.argumento[2] <> '2' THEN

	li_Planta		=	dw_1.Object.plde_codigo[il_Fila]
	li_Camara		=	dw_1.Object.cama_codigo[il_Fila]
	li_PlantaLote	=	dw_1.Object.lote_pltcod[il_Fila]
	li_EspecieLote	=	dw_1.Object.lote_espcod[il_Fila]
	ll_NumeroLote	=	dw_1.Object.lote_codigo[il_Fila]
	li_tipoenva    =  dw_1.Object.enva_tipoen[il_Fila]
	li_envase		=  dw_1.Object.enva_codigo[il_Fila]
	
	CHOOSE CASE as_Columna
			
		CASE "plde_codigo"
			li_Planta		=	Integer(as_Valor)
			
		CASE "cama_codigo"
			li_Camara		=	Integer(as_Valor)
			
		CASE "lote_pltcod"
			li_PlantaLote	=	Integer(as_Valor)
			
		CASE "lote_espcod"
			li_EspecieLote	=	Integer(as_Valor)
			
		CASE "lote_codigo"
			ll_NumeroLote	=	Long(as_Valor)
			
		CASE "enva_tipoen"
			li_TipoEnva		=	Integer(as_Valor)
	
		CASE "enva_codigo"
			li_envase		=	Integer(as_Valor)
			
	END CHOOSE
	
	SELECT	IsNull(Sum(caex_canbul), 0)
		INTO	:ll_Saldo
		FROM	dba.spro_camaraexistefg
		WHERE	plde_codigo	=	:li_Planta
		AND	cama_codigo	=	:li_Camara
		AND	lote_pltcod	=	:li_PlantaLote
		AND	lote_espcod	=	:li_EspecieLote
		AND	lote_codigo	=	:ll_NumeroLote
		AND   enva_tipoen =  :li_TipoEnva
		AND   enva_codigo =  :li_Envase;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura tabla de Existencia Frigorífico")
		
		lb_Retorno	=	False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Lote en Cámara especificada.~r~r" + &
						"Ingrese o seleccione otros antecedentes.")
		
		lb_Retorno	=	False
	ELSEIF Not IsNull(ai_Cantidad) AND ai_Cantidad > 0 AND ai_Cantidad > ll_Saldo THEN
		MessageBox("Atención", "No hay Existencia suficiente del Lote~r" + &
						"en Cámara especificada. Saldo = " +String(ll_saldo)+"~r~r" + &
						"Ingrese o seleccione otros antecedentes.")
		
		lb_Retorno	=	False
	ELSE
		IF ll_Saldo>0 THEN
			CapturaKilosPromedio(as_columna,as_valor)
			dw_1.Object.mfgd_bulent[il_Fila] =  ll_Saldo
			dw_1.Object.mfgd_kgnent[il_Fila]	=	ll_Saldo * id_KilosPromed
		END IF
	END IF
END IF	
RETURN lb_Retorno
end function

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
String	ls_Camara, ls_Planta, ls_Especie, ls_Lote, ls_TiEnvase, ls_Envase

ls_Camara	=	String(dw_1.Object.cama_codigo[il_Fila])
ls_Planta	=	String(dw_1.Object.lote_pltcod[il_Fila])
ls_Especie	=	String(dw_1.Object.lote_espcod[il_Fila])
ls_Lote		=	String(dw_1.Object.lote_codigo[il_Fila])
ls_TiEnvase	=	String(dw_1.Object.enva_tipoen[il_Fila])
ls_Envase	=	String(dw_1.Object.enva_codigo[il_Fila])

CHOOSE CASE as_Columna
	CASE "cama_codigo"
		ls_Camara	=	as_Valor

	CASE "lote_pltcod"
		ls_Planta	=	as_Valor

	CASE "lote_espcod"
		ls_Especie	=	as_Valor

	CASE "lote_codigo"
		ls_Lote		=	as_Valor

	CASE "enva_tipoen"
		ls_TiEnvase	=	as_Valor

	CASE "enva_codigo"
		ls_Envase	=	as_Valor

END CHOOSE

ll_Fila	=	dw_1.Find("cama_codigo = " + ls_Camara + " AND " + &
							"lote_pltcod = " + ls_Planta + " AND " + &
							"lote_espcod = " + ls_Especie + " AND " + &
							"lote_codigo = " + ls_Lote + " AND " + &
							"enva_tipoen = " + ls_TiEnvase + " AND " + &
							"enva_codigo = " + ls_Envase , &
							1, dw_1.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	MessageBox("Error", "Registro ya fue ingresada anteriormente", Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine buscaenvase ();str_busqueda	lstr_busq
String			ls_Nula

SetNull(ls_Nula)

lstr_busq.argum[1]	=	String(dw_1.Object.enva_tipoen[il_Fila])

OpenWithParm(w_busc_envases, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	dw_1.SetColumn("enva_codigo")
	dw_1.Object.enva_codigo[il_Fila]	=	Integer(ls_Nula)
	dw_1.Object.enva_nombre[il_Fila]	=	ls_Nula
	dw_1.SetFocus()
ELSE
	IF Duplicado("enva_codigo",lstr_busq.argum[2]) THEN
		dw_1.SetItem(il_Fila, "enva_codigo", Integer(ls_Nula))
	ELSE	
		dw_1.Object.enva_codigo[il_Fila]	=	Integer(lstr_busq.argum[2])
		dw_1.Object.enva_nombre[il_Fila]	=	lstr_busq.argum[3]
		
		ExisteEnvase(dw_1.object.enva_tipoen[il_fila], &
						 dw_1.object.enva_codigo[il_fila], istr_Envase)
				
		IF iuo_TipoMovto.Sentido = 2 THEN
			IF Not HayExistencia("enva_codigo", lstr_busq.argum[2], &
				dw_1.Object.mfgd_bulent[il_Fila]) THEN
				dw_1.SetItem(il_Fila, "enva_codigo", Integer(ls_Nula))
			END IF
		END IF
	END IF	
END IF

RETURN
end subroutine

public subroutine buscalote ();str_busqueda	lstr_busq
String			ls_Nula

SetNull(ls_Nula)

lstr_busq.argum[1]	=	String(dw_1.Object.plde_codigo[il_Fila])
lstr_busq.argum[2]	=	String(dw_1.Object.lote_espcod[il_Fila])

IF istr_mant.Argumento[2]='21' THEN
	lstr_busq.argum[3]	=	"21"
   lstr_busq.argum[4]	=	istr_mant.argumento[9]
	lstr_busq.argum[5]   =  istr_mant.argumento[6]
	lstr_busq.argum[6]   =  istr_mant.argumento[8]
	lstr_busq.argum[13]	=	istr_mant.argumento[13]
	
	OpenWithParm(w_busc_spro_lotefrutagranel, lstr_busq)

	lstr_busq	= Message.PowerObjectParm
	
	IF lstr_busq.argum[3] = "" THEN
		dw_1.SetColumn("lote_codigo")
		dw_1.SetFocus()
	ELSEIF Duplicado("lote_codigo", lstr_busq.argum[3]) THEN
		dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
	ELSE
		dw_1.Object.lote_codigo[il_Fila] =	Integer(lstr_busq.argum[3])
		dw_1.Object.vari_nombre[il_Fila] =	lstr_busq.argum[13]
		dw_1.Object.prod_nombre[il_Fila] =	lstr_busq.argum[6]
	
		IF iuo_Lote.Existe(iuo_PltaLote.Codigo, iuo_Especie.Codigo, &
							Integer(lstr_busq.argum[3]), True, sqlca) THEN
		  IF iuo_TipoMovto.Sentido = 2 THEN
			IF Not HayExistencia("lote_codigo", lstr_busq.argum[3], &
				dw_1.Object.mfgd_bulent[il_Fila]) THEN
				dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
			ELSE
				CapturaKilosPromedio("","")
			END IF
		  END IF				
		END IF					
   END IF
   RETURN
END IF	

IF istr_mant.Argumento[2]='2' THEN
	lstr_busq.argum[3]	=	""
   lstr_busq.argum[4]	=	""//istr_mant.argumento[9]
	lstr_busq.argum[5]   =  ""//istr_mant.argumento[6]
	lstr_busq.argum[6]   =  ""//istr_mant.argumento[8]

	OpenWithParm(w_busc_spro_lotefrutagranel, lstr_busq)

	lstr_busq	= Message.PowerObjectParm
	
	IF lstr_busq.argum[3] = "" THEN
		dw_1.SetColumn("lote_codigo")
		
		dw_1.SetFocus()
	ELSEIF Duplicado("lote_codigo", lstr_busq.argum[3]) THEN
		dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
	ELSE
		dw_1.Object.lote_codigo[il_Fila] =	Integer(lstr_busq.argum[3])
		dw_1.Object.vari_nombre[il_Fila] =	lstr_busq.argum[13]
		dw_1.Object.prod_nombre[il_Fila] =	lstr_busq.argum[6]
	
		IF iuo_Lote.Existe(iuo_PltaLote.Codigo, iuo_Especie.Codigo, &
							Integer(lstr_busq.argum[3]), True, sqlca) THEN
		  IF iuo_TipoMovto.Sentido = 2 THEN
			IF Not HayExistencia("lote_codigo", lstr_busq.argum[3], &
				dw_1.Object.mfgd_bulent[il_Fila]) THEN
				dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
			END IF
		  END IF				
		END IF					
   END IF
   RETURN
END IF

IF istr_mant.Argumento[2]='22' OR  istr_mant.Argumento[2]='30' THEN
	lstr_busq.argum[3]	=	String(dw_1.Object.cama_codigo[il_Fila])
   lstr_busq.argum[4]   =  "0"
	
	OpenWithParm(w_busc_lotesfrutagranel_existencia, lstr_busq)

	lstr_busq	= Message.PowerObjectParm
	
	IF lstr_busq.argum[3] = "" THEN
		dw_1.SetColumn("lote_codigo")
		dw_1.SetFocus()
	ELSEIF Duplicado("lote_codigo", lstr_busq.argum[3]) THEN
		dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
	ELSE
		dw_1.Object.lote_pltcod[il_Fila] =	Integer(lstr_busq.argum[1])
		dw_1.Object.lote_codigo[il_Fila] =	Integer(lstr_busq.argum[3])
		dw_1.Object.vari_nombre[il_Fila] =	lstr_busq.argum[5]
		dw_1.Object.prod_nombre[il_Fila] =	lstr_busq.argum[7]
		dw_1.Object.enva_tipoen[il_Fila] =	integer(lstr_busq.argum[10])
		dw_1.Object.enva_codigo[il_Fila] =	integer(lstr_busq.argum[11])
		dw_1.Object.enva_nombre[il_Fila] =	lstr_busq.argum[12]
		dw_1.Object.cama_codigo[il_Fila] =  integer(lstr_busq.argum[9])
	   
		IF Duplicado("lote_codigo", lstr_busq.argum[3]) THEN
			dw_1.SetItem(il_Fila, "lote_pltcod", Integer(istr_mant.argumento[1]))
			dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
			dw_1.SetItem(il_Fila, "vari_nombre", ls_Nula)
			dw_1.SetItem(il_Fila, "prod_nombre", ls_Nula)
			dw_1.SetItem(il_Fila, "enva_tipoen", Integer(ls_Nula))
			dw_1.SetItem(il_Fila, "enva_codigo", Integer(ls_Nula))
			dw_1.SetItem(il_Fila, "enva_nombre", ls_Nula)
		ELSE
			iuo_PltaLote.Existe(Integer(lstr_busq.argum[1]), True, sqlca)
			IF iuo_Lote.Existe(iuo_PltaLote.Codigo, iuo_Especie.Codigo, &
									 Integer(lstr_busq.argum[3]), True, sqlca) THEN
			  IF iuo_TipoMovto.Sentido = 2 THEN
				IF Not HayExistencia("lote_codigo", lstr_busq.argum[3], &
											Long(lstr_busq.argum[8])) THEN
					dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
				ELSE
					CapturaKilosPromedio("","")
					
					dw_1.Object.mfgd_bulent[il_Fila] =  long(lstr_busq.argum[8])
					dw_1.Object.mfgd_kgnent[il_Fila]	=	long(lstr_busq.argum[8]) * id_KilosPromed
				END IF
			  END IF				
			END IF
		END IF	
   END IF
   RETURN
END IF

IF istr_mant.Argumento[2]='23' OR istr_mant.Argumento[2]='36'THEN
	lstr_busq.argum[3]	=	String(dw_1.Object.cama_codigo[il_Fila])
   lstr_busq.argum[4]   =  istr_mant.argumento[6]
	lstr_busq.argum[5]	=  istr_mant.Argumento[2]
	
	OpenWithParm(w_busc_lotesfrutagranel_existencia, lstr_busq)

	lstr_busq	= Message.PowerObjectParm
	
	IF lstr_busq.argum[3] = "" THEN
		dw_1.SetColumn("lote_codigo")
		dw_1.SetFocus()
	ELSEIF Duplicado("lote_codigo", lstr_busq.argum[3]) THEN
		dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
	ELSE
		dw_1.Object.lote_pltcod[il_Fila] =	Integer(lstr_busq.argum[1])
		dw_1.Object.lote_codigo[il_Fila] =	Integer(lstr_busq.argum[3])
		dw_1.Object.vari_nombre[il_Fila] =	lstr_busq.argum[5]
		dw_1.Object.prod_nombre[il_Fila] =	lstr_busq.argum[7]
		dw_1.Object.enva_tipoen[il_Fila] =	integer(lstr_busq.argum[10])
		dw_1.Object.enva_codigo[il_Fila] =	integer(lstr_busq.argum[11])
		dw_1.Object.enva_nombre[il_Fila] =	lstr_busq.argum[12]
		dw_1.Object.cama_codigo[il_Fila] =  integer(lstr_busq.argum[9])
	   
		IF Duplicado("lote_codigo", lstr_busq.argum[3]) THEN
			dw_1.SetItem(il_Fila, "lote_pltcod", Integer(istr_mant.argumento[1]))
			dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
			dw_1.SetItem(il_Fila, "vari_nombre", ls_Nula)
			dw_1.SetItem(il_Fila, "prod_nombre", ls_Nula)
			dw_1.SetItem(il_Fila, "enva_tipoen", Integer(ls_Nula))
			dw_1.SetItem(il_Fila, "enva_codigo", Integer(ls_Nula))
			dw_1.SetItem(il_Fila, "enva_nombre", ls_Nula)
		ELSE	
			iuo_PltaLote.Existe(Integer(lstr_busq.argum[1]), True, sqlca)
			IF iuo_Lote.Existe(iuo_PltaLote.Codigo, iuo_Especie.Codigo, &
									 Integer(lstr_busq.argum[3]), True, sqlca) THEN
			  IF iuo_TipoMovto.Sentido = 2 THEN
				IF Not HayExistencia("lote_codigo", lstr_busq.argum[3], &
				   long(lstr_busq.argum[8])) THEN
					dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
				ELSE
					CapturaKilosPromedio("","")

					dw_1.Object.mfgd_bulent[il_Fila] =  long(lstr_busq.argum[8])
					dw_1.Object.mfgd_kgnent[il_Fila]	=	long(lstr_busq.argum[8]) * id_KilosPromed
				END IF
			  END IF				
			END IF
		END IF	
   END IF
   RETURN
END IF
end subroutine

on w_mant_deta_movtodespachoacomercial.create
call super::create
end on

on w_mant_deta_movtodespachoacomercial.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;///Agrega L.M.
IF istr_mant.Argumento[2]='21' THEN	
   dw_1.Object.lote_espcod[il_Fila]					=	integer(istr_mant.Argumento[8])
	dw_1.Object.lote_espcod.Protect 				=	1
	dw_1.Object.lote_espcod.BackGround.Color 	=	RGB(192,192,192)
	dw_1.Object.prod_nombre[il_fila]					=	istr_mant.argumento[7]
	dw_1.Object.prod_nombre.Protect 				=	1
	dw_1.Object.prod_nombre.BackGround.Color 	=	RGB(192,192,192)
	dw_1.Object.vari_nombre[il_fila]					=	istr_mant.argumento[10]
	dw_1.Object.vari_nombre.Protect 				=	1
	dw_1.Object.vari_nombre.BackGround.Color 	=	RGB(192,192,192)
END IF	
//
IF istr_mant.argumento[2] = '2' OR istr_mant.argumento[2] = '30' THEN
	dw_1.Object.lote_pltcod.Protect 				=	0
	dw_1.Object.lote_pltcod.BackGround.Color	=	RGB(255,255,255)
ELSEIF iuo_TipoMovto.Sentido = 2 THEN
	dw_1.Object.lote_pltcod.Protect 				=	0
	dw_1.Object.lote_pltcod.BackGround.Color	=	RGB(255,255,255)
ELSE
	dw_1.Object.lote_pltcod.Protect 				= 	1
	dw_1.Object.lote_pltcod.BackGround.Color	=	RGB(192,192,192)
END IF	

ias_campo[1]	=	String(dw_1.object.plde_coorde[il_fila])
ias_campo[2]	=	String(dw_1.object.cama_codigo[il_fila])
ias_campo[3]	=	String(dw_1.object.lote_pltcod[il_fila])
ias_campo[4]	=	String(dw_1.object.lote_espcod[il_fila])
ias_campo[5]	=	String(dw_1.object.lote_codigo[il_fila])
ias_campo[6]	=	String(dw_1.object.mfgd_bulent[il_fila])
ias_campo[7]	=	String(dw_1.object.mfgd_kgnent[il_fila])
ias_campo[8]	=	String(dw_1.object.enva_tipoen[il_fila])
ias_campo[9]	=	String(dw_1.object.enva_codigo[il_fila])
ias_campo[13]	=	String(dw_1.Object.clie_codigo[il_fila])

IF istr_mant.Agrega THEN
	
	dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
	dw_1.Object.plde_coorde[il_Fila]	=	Integer(istr_Mant.Argumento[1])
	
	dw_1.Object.tpmv_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[2])
	dw_1.Object.mfge_numero[il_Fila]	=	Integer(istr_Mant.Argumento[3])
	
	dw_1.object.lote_pltcod[il_fila]	=	Integer(istr_Mant.Argumento[1])
	dw_1.object.lote_espcod[il_fila]	=	gstr_ParamPlanta.CodigoEspecie
	
	iuo_PltaLote.Existe(Integer(istr_Mant.Argumento[1]), True, sqlca)
	iuo_Especie.Existe(gstr_ParamPlanta.CodigoEspecie, True, sqlca)
	
ELSEIF Not istr_mant.Agrega AND Not istr_mant.Borra THEN
	
	dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
	
	iuo_PltaLote.Existe(dw_1.object.lote_pltcod[il_fila], True, sqlca)
	
	iuo_Especie.Existe(dw_1.object.lote_espcod[il_fila], True, sqlca)
	
	iuo_Lote.Existe(iuo_PltaLote.Codigo, iuo_Especie.Codigo, &
						dw_1.object.lote_codigo[il_fila], True, sqlca)
	
	IF ExisteVariedad_gr(iuo_Especie.Codigo, iuo_Lote.Variedad, istr_Variedad) THEN
		dw_1.Object.vari_nombre[il_fila] = istr_variedad.Nombre
	END IF	
	
	IF iuo_Productor.Existe(iuo_Lote.Productor, True, sqlca) THEN
		dw_1.Object.prod_nombre[il_fila] = iuo_Productor.Nombre
	END IF
END IF
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.object.plde_coorde[il_fila]	=	Integer(ias_campo[1])
	dw_1.object.cama_codigo[il_fila]	=	Integer(ias_campo[2])
	dw_1.object.lote_pltcod[il_fila]		=	Integer(ias_campo[3])
	dw_1.object.lote_espcod[il_fila]	=	Integer(ias_campo[4])
	dw_1.object.lote_codigo[il_fila]	=	Long(ias_campo[5])
	dw_1.object.mfgd_bulent[il_fila]	=	Integer(ias_campo[6])
	dw_1.object.mfgd_kgnent[il_fila]	=	Dec(ias_campo[7])
	dw_1.Object.clie_codigo[il_fila]	=	Integer(ias_campo[13])
END IF
end event

event ue_antesguardar;Integer	li_Contador, li_row, li_suma
String	ls_Mensaje, ls_Columna[]

dw_1.Object.clie_codigo[il_fila]	=	Integer(Istr_Mant.Argumento[1])

IF IsNull(dw_1.Object.cama_codigo[il_Fila]) THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nNúmero de Cámara"
	ls_Columna[li_Contador]	=	"cama_codigo"
END IF

IF IsNull(dw_1.Object.lote_pltcod[il_Fila]) OR &
	dw_1.Object.lote_pltcod[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nPlanta de Lote"
	ls_Columna[li_Contador]	=	"lote_pltcod"
END IF

IF IsNull(dw_1.Object.lote_espcod[il_Fila]) OR &
	dw_1.Object.lote_espcod[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nEspecie"
	ls_Columna[li_Contador]	=	"lote_espcod"
END IF

IF IsNull(dw_1.Object.lote_codigo[il_Fila]) OR &
	dw_1.Object.lote_codigo[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nNúmero de Lote"
	ls_Columna[li_Contador]	=	"lote_codigo"
END IF

IF IsNull(dw_1.Object.enva_tipoen[il_Fila]) OR &
	dw_1.Object.enva_tipoen[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nTipo de envase"
	ls_Columna[li_Contador]	=	"enva_tipoen"
END IF

IF IsNull(dw_1.Object.enva_codigo[il_Fila]) OR &
	dw_1.Object.enva_codigo[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nEnvase"
	ls_Columna[li_Contador]	=	"enva_codigo"
END IF

IF IsNull(dw_1.Object.mfgd_bulent[il_Fila]) OR &
	dw_1.Object.mfgd_bulent[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCantidad de Bultos"
	ls_Columna[li_Contador]	=	"mfgd_bulent"
END IF

IF li_Contador > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_Columna[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF

IF Dec(dw_1.Object.mfgd_kgnent[il_fila]) > 100000 THEN
	Messagebox("Error de Consistencia","Los kilos netos superan lo permitido." + &
				  "~r~rDebe modificar la cantidad de bultos", StopSign!, Ok!)
	 Message.DoubleParm = -1
END IF
end event

event ue_nuevo();call super::ue_nuevo;///Agrega L.M.
IF istr_mant.Argumento[2]='21' THEN
   dw_1.Object.lote_espcod[il_Fila]				=	integer(istr_mant.Argumento[8])
	dw_1.Object.prod_nombre[il_fila]				=	istr_mant.argumento[7]
	dw_1.Object.vari_nombre[il_fila]				=	istr_mant.argumento[10]
ELSE
   dw_1.Object.lote_espcod[il_Fila]	=	gstr_ParamPlanta.CodigoEspecie
	iuo_Especie.Existe(gstr_ParamPlanta.CodigoEspecie, True, sqlca)

END IF

dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
dw_1.Object.plde_coorde[il_Fila]	=	Integer(istr_Mant.Argumento[1])
dw_1.Object.tpmv_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[2])
dw_1.Object.mfge_numero[il_Fila]	=	Integer(istr_Mant.Argumento[3])
dw_1.Object.lote_pltcod[il_Fila]	=	Integer(istr_Mant.Argumento[1])

IF istr_mant.argumento[2] = '2' THEN
	dw_1.Object.lote_pltcod.Protect =  0
	dw_1.Object.lote_pltcod.BackGround.Color = RGB(255,255,255)
ELSEIF istr_mant.argumento[2] = '23' OR istr_mant.argumento[2] = '36' OR &
	    istr_mant.argumento[2] = '22' OR istr_mant.argumento[2] = '30' THEN
	dw_1.Object.lote_pltcod.Protect =  0
	dw_1.Object.lote_pltcod.BackGround.Color = RGB(255,255,255)	
ELSE
	dw_1.Object.lote_pltcod.Protect 				= 	1
	dw_1.Object.lote_pltcod.BackGround.Color	=	RGB(192,192,192)
END IF

iuo_PltaLote.Existe(Integer(istr_Mant.Argumento[1]), True, sqlca)
end event

event open;/* 
	Argumentos
		istr_Mant.Argumento[01]	=	Código Planta
		istr_Mant.Argumento[02]	=	Tipo de Movimiento
		istr_Mant.Argumento[03]	=	Número de Despacho
		istr_Mant.Argumento[04]	=	Sentido del Movimiento => 2 = Despacho
		istr_Mant.Argumento[13]	=	Cliente
*/
x	= 100
y	= 220

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

iuo_PltaLote	=	Create uo_plantadesp
iuo_Especie		=	Create uo_especie
iuo_Lote			=	Create uo_lotesfrutagranel
iuo_Productor	=	Create uo_productores
iuo_Camara		=	Create uo_camarasfrigo
iuo_TipoMovto	=	Create uo_tipomovtofruta

dw_1.GetChild("cama_codigo", idwc_Camara)
idwc_Camara.SetTransObject(sqlca)
idwc_Camara.Retrieve(Integer(istr_Mant.Argumento[1]))
idwc_Camara.SetSort("cama_nombre A")
idwc_Camara.Sort()

dw_1.GetChild("lote_pltcod", idwc_Planta)
idwc_Planta.SetTransObject(sqlca)
idwc_Planta.Retrieve()
idwc_Planta.SetSort("plde_nombre A")
idwc_Planta.Sort()

dw_1.GetChild("lote_espcod", idwc_Especie)
idwc_Especie.SetTransObject(sqlca)
idwc_Especie.Retrieve()
idwc_Especie.SetSort("espe_nombre A")
idwc_Especie.Sort()

dw_1.GetChild("enva_tipoen", idwc_TipoEnvase)
idwc_TipoEnvase.SetTransObject(sqlca)
idwc_TipoEnvase.Retrieve()
idwc_TipoEnvase.SetSort("tien_nombre A")
idwc_TipoEnvase.Sort()
idwc_TipoEnvase.SetFilter("tien_usoenv = 1")
idwc_TipoEnvase.Filter()

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)

iuo_TipoMovto.Existe(Integer(istr_mant.Argumento[2]),True,SQLCA)

IF iuo_TipoMovto.Sentido = 2 THEN
	dw_1.Modify("titulo.Text = 'DETALLE LOTES A DESPACHO'")
ELSE
	dw_1.Modify("titulo.Text = 'DETALLE LOTES A RECEPCION'")
END IF
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_movtodespachoacomercial
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_movtodespachoacomercial
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_movtodespachoacomercial
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_movtodespachoacomercial
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_movtodespachoacomercial
integer x = 2222
integer y = 408
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_movtodespachoacomercial
integer x = 2222
integer y = 232
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_movtodespachoacomercial
integer x = 2222
integer y = 580
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_movtodespachoacomercial
integer x = 0
integer y = 0
integer width = 2153
integer height = 1884
string dataobject = "dw_mant_movtofrutagraneldeta_despacho"
end type

event dw_1::itemchanged;String  ls_Columna, ls_Nula

SetNull(ls_Nula)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna
	CASE "cama_codigo"
		IF Not iuo_Camara.Existe(Integer(istr_Mant.Argumento[1]), &
						Integer(Data), True, sqlca) THEN
			This.SetItem(il_Fila, ls_Columna, integer(ias_campo[2]))
			
			RETURN 1
		ELSEIF Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, integer(ias_campo[2]))
			
			RETURN 1
		ELSE
			IF iuo_TipoMovto.Sentido = 2 THEN
				IF Not HayExistencia(ls_Columna, Data, &
				This.Object.mfgd_bulent[il_Fila]) THEN
					This.SetItem(il_Fila, ls_Columna, integer(ias_campo[2]))
					
					RETURN 1
				END IF
			END IF
		END IF

	CASE "lote_pltcod"
		IF Not iuo_PltaLote.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(il_Fila, ls_Columna, integer(ias_campo[3]))
			
			RETURN 1
		ELSEIF Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, integer(ias_campo[3]))
			
			RETURN 1
		ELSE
			IF iuo_TipoMovto.Sentido = 2 THEN
				IF Not HayExistencia(ls_Columna, Data, &
				This.Object.mfgd_bulent[il_Fila]) THEN
					This.SetItem(il_Fila, ls_Columna, integer(ias_campo[3]))
					
					RETURN 1
				END IF
			END IF
		END IF

	CASE "lote_espcod"
		IF Not iuo_Especie.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(il_Fila, ls_Columna, integer(ias_campo[4]))
			
			RETURN 1
		ELSEIF Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, integer(ias_campo[4]))
			
			RETURN 1
		ELSE
			IF iuo_TipoMovto.Sentido = 2 THEN
				IF Not HayExistencia(ls_Columna, Data, &
				This.Object.mfgd_bulent[il_Fila]) THEN
					This.SetItem(il_Fila, ls_Columna, integer(ias_campo[4]))
					
					RETURN 1
				END IF
			END IF
		END IF

	CASE "lote_codigo"
		IF Not iuo_Lote.Existe(iuo_PltaLote.Codigo, &
										iuo_Especie.Codigo, &
										Integer(Data), True, sqlca) THEN
			This.SetItem(il_Fila, ls_Columna, Long(ias_campo[5]))
			
			RETURN 1
		ELSEIF Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, Long(ias_campo[5]))
			
			RETURN 1
		ELSE
			IF iuo_lote.categoria = 100 AND istr_mant.Argumento[2] = "36" THEN
				MessageBox("Atención","El lote ingresado es de categoria a proceso.")
				This.SetItem(il_Fila, ls_Columna, Long(ias_campo[5]))
				RETURN 1
			END IF	
			IF iuo_lote.totalneto > 0 THEN
			   IF istr_mant.argumento[2] = "23" THEN
              IF iuo_Lote.Productor <> Integer(istr_mant.argumento[6]) THEN
  					  MessageBox("Atención", "No existe Lote para Productor a Devolución.~r~r" + &
						           "Ingrese o seleccione otros antecedentes.")
		  		  	  This.SetItem(il_Fila, ls_Columna, Long(ias_campo[5]))
						RETURN 1
				  END IF
			   END IF 
				
				IF iuo_TipoMovto.Sentido = 2 THEN
					IF Not HayExistencia(ls_Columna, Data, &
						This.Object.mfgd_bulent[il_Fila]) THEN
						This.SetItem(il_Fila, ls_Columna, Long(ias_campo[5]))
						
						RETURN 1
					ELSE
						CapturaKilosPromedio("","")
					END IF
				END IF
	
				ExisteVariedad_gr(iuo_Especie.Codigo, iuo_Lote.Variedad, istr_Variedad)
				iuo_Productor.Existe(iuo_Lote.Productor, True, sqlca)
				
				This.SetItem(il_Fila, "prod_nombre", iuo_Productor.Nombre)
				This.SetItem(il_Fila, "vari_nombre", istr_Variedad.Nombre)
			ELSE
				MessageBox("Atención", "No existe Lote como Definitivo.~r~r" + &
						     "Ingrese o seleccione otros antecedentes.")
			  	This.SetItem(il_Fila, ls_Columna, Long(ias_campo[5]))
						
				RETURN 1
			END IF	
		END IF

	CASE "enva_tipoen"
		IF Not ExisteEnvase(Integer(Data), 0, istr_Envase) OR &
			Duplicado(ls_Columna, Data) OR istr_Envase.UsoEnvase <> 1 THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ias_campo[8]))

			RETURN 1
		ELSE
			IF iuo_TipoMovto.Sentido = 2 THEN
				IF Not HayExistencia(ls_Columna, Data, &
					This.Object.mfgd_bulent[il_Fila]) THEN
					This.SetItem(il_Fila, ls_Columna, Integer(ias_campo[8]))
					
					RETURN 1
				ELSE
					CapturaKilosPromedio(ls_Columna,Data)
				END IF
			END IF
		END IF

	CASE "enva_codigo"
		istr_Envase.TipoEnvase = this.Object.enva_tipoen[il_fila]
		IF Not ExisteEnvase(istr_Envase.TipoEnvase, Integer(Data), istr_Envase) OR &
			Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ias_campo[9]))
			
			RETURN 1
		ELSE
			dw_1.Object.enva_nombre[il_Fila]	=	istr_Envase.Nombre
			IF iuo_TipoMovto.Sentido = 2 THEN
				IF Not HayExistencia(ls_Columna, Data, &
					This.Object.mfgd_bulent[il_Fila]) THEN
					This.SetItem(il_Fila, ls_Columna, Integer(ias_campo[9]))
					
					RETURN 1
				ELSE
					CapturaKilosPromedio(ls_Columna,Data)
				END IF
			END IF
		END IF
		
	CASE "mfgd_bulent"
		IF Integer(data) > 9999 OR Integer(data) < 0 THEN
			This.Setitem(row,"mfgd_bulent", Integer(ls_Nula))
			RETURN 1
		ELSE
			IF iuo_TipoMovto.Sentido = 2 THEN
				IF Not HayExistencia(ls_Columna, "0", integer(Data)) THEN
					This.SetItem(il_Fila, ls_Columna, Integer(ias_campo[6]))
					This.SetItem(il_Fila,"mfgd_kgnent",dec(ias_campo[7]))
					RETURN 1
				ELSE
					This.Object.mfgd_kgnent[il_Fila]	=	Integer(Data) * id_KilosPromed
				END IF
			END IF
		END IF


END CHOOSE
end event

event dw_1::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name
	CASE "b_buscalotes"
		BuscaLote()

	CASE "b_buscaenvase"
		BuscaEnvase()

END CHOOSE
end event

