$PBExportHeader$w_mant_deta_movtofrutagranel_despacho.srw
$PBExportComments$Mantención de Detalle Despacho de Fruta Granel Interplanta y Devolución a Productor
forward
global type w_mant_deta_movtofrutagranel_despacho from w_mant_detalle_csd
end type
type dw_lotes from datawindow within w_mant_deta_movtofrutagranel_despacho
end type
type pb_tarjas from picturebutton within w_mant_deta_movtofrutagranel_despacho
end type
type dw_2 from datawindow within w_mant_deta_movtofrutagranel_despacho
end type
type dw_tarjas from uo_dw within w_mant_deta_movtofrutagranel_despacho
end type
end forward

global type w_mant_deta_movtofrutagranel_despacho from w_mant_detalle_csd
integer width = 2679
integer height = 2312
string title = "DETALLE DE LOTES A DESPACHO"
boolean controlmenu = true
dw_lotes dw_lotes
pb_tarjas pb_tarjas
dw_2 dw_2
dw_tarjas dw_tarjas
end type
global w_mant_deta_movtofrutagranel_despacho w_mant_deta_movtofrutagranel_despacho

type variables
uo_plantadesp				iuo_PltaLote
uo_especie					iuo_Especie
uo_lotesfrutagranel		iuo_Lote
uo_productores				iuo_Productor
uo_camarasfrigo			iuo_Camara
uo_tipomovtofruta			iuo_TipoMovto
uo_validacionesvaciado	iuo_valida

str_variedad				istr_Variedad
str_envase				istr_Envase

DataWindowChild		idwc_Camara, idwc_Planta, idwc_Especie
DataWindowChild		idwc_tipoenvase  //agrego lm
Dec{3}					id_KilosPromed


end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
public subroutine capturakilospromedio (string as_columna, string as_valor)
public subroutine buscaenvase ()
public function boolean hayexistencia (string as_columna, string as_valor, integer ai_cantidad)
public subroutine buscalote ()
public subroutine cambiatarjas (integer ai_fila)
end prototypes

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

public subroutine capturakilospromedio (string as_columna, string as_valor);Integer		li_PlantaLote, li_EspecieLote, li_filas, li_bultos
Long     	ll_NumeroLote
Integer 		li_Cliente, li_Planta, li_lote, li_Especie, li_Envase, li_TipoEnva, li_TipOrden, li_Numero, li_fila
Long			ll_Bins, ll_cantidad
String		ls_Calidad
Decimal		ld_taraenvase
DataStore	lds_calidades

If istr_mant.argumento[2] <> '2' Then
   If as_Columna = "tarjas" Then
		id_KilosPromed	=	0
		li_bultos		=	0
		FOR li_filas = 1 TO dw_tarjas.RowCount()
			If Not iuo_valida.binsduplicados(dw_1.Object.clie_codigo[il_fila],  dw_1.Object.plde_codigo[il_fila], &
														dw_tarjas.Object.fgmb_nrotar[li_filas], TRUE, sqlca) Then
				Return 
			End If
			
			ll_bins		=	iuo_valida.Bins
			li_lote			=	iuo_valida.Lote
			li_especie	=	iuo_valida.Especie
					
			If NOT iuo_valida.cargabins(dw_1.Object.clie_codigo[il_fila],  dw_1.Object.plde_codigo[il_fila], ll_Bins, TRUE, sqlca) Then
				Return 
			End If
			
			li_TipoEnva	=	iuo_valida.TipoEnva
			li_Envase		=	iuo_valida.Envase
			ls_Calidad 	=	iuo_valida.Calidad
			
			lds_calidades					=	CREATE DataStore
			lds_calidades.DataObject	=	"dw_mues_spro_calidadenvases"
			lds_calidades.SetTransObject(sqlca)
			lds_calidades.Retrieve(li_TipoEnva, li_envase)
			
			li_fila 			= 	lds_calidades.Find("cale_calida ='" + ls_Calidad + "'", 1, lds_calidades.RowCount()) 
			ld_taraenvase 	= 	lds_calidades.GetItemDecimal(li_fila, "cale_pesoen")
			
			id_KilosPromed = 	id_KilosPromed + (dw_tarjas.Object.fgmb_kilbru[li_fila] - ld_taraenvase)
			li_bultos			=	li_bultos + dw_tarjas.Object.fgmb_canbul[li_fila]
		NEXT
		
		dw_1.Object.mfgd_bulent[il_fila]	=	li_bultos
		dw_1.Object.mfgd_kgnent[il_fila]	=	id_KilosPromed
		
	Else
		li_PlantaLote	=	dw_1.Object.lote_pltcod[il_Fila]
		li_EspecieLote	=	dw_1.Object.lote_espcod[il_Fila]
		ll_NumeroLote	=	dw_1.Object.lote_codigo[il_Fila]
		li_tipoenva    	=  dw_1.Object.enva_tipoen[il_Fila]
		li_envase			=  dw_1.Object.enva_codigo[il_Fila]
	
		CHOOSE Case as_Columna
			Case "lote_pltcod"
				li_PlantaLote	=	Integer(as_Valor)
	
			Case "lote_espcod"
				li_EspecieLote	=	Integer(as_Valor)
	
			Case "lote_codigo"
				ll_NumeroLote	=	Long(as_Valor)
	
			Case "enva_tipoen"
				li_TipoEnva		=	Integer(as_Valor)
	
			Case "enva_codigo"
				li_envase		=	Integer(as_Valor)
	
		End CHOOSE
		
		SELECT	Isnull(lotd_kilpro,0)
			INTO	:id_KilosPromed
			FROM	dbo.spro_lotesfrutagrandeta
			WHERE	lote_pltcod	=	:li_PlantaLote
			AND	lote_espcod	=	:li_EspecieLote
			AND	lote_codigo	=	:ll_NumeroLote
			AND   enva_tipoen =  :li_TipoEnva
			AND   enva_codigo =  :li_Envase;
		
		If sqlca.SQLCode = -1 Then
			F_ErrorBaseDatos(sqlca, "Lectura tabla de Detalle Lote Fruta Granel")
		End If
	End If
End If	
end subroutine

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
		FROM	dbo.spro_camaraexistefg
		WHERE	plde_codigo	=	:li_Planta
		AND	cama_codigo	=	:li_Camara
		AND	lote_pltcod	=	:li_PlantaLote
		AND	lote_espcod	=	:li_EspecieLote
		AND	lote_codigo	=	:ll_NumeroLote
		AND   IsNull(:li_TipoEnva, -1) in (enva_tipoen, -1)
		AND   IsNull(:li_Envase, -1) in (enva_codigo, -1);
	
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
				
		dw_2.Reset()
		dw_tarjas.RowsMove(1, dw_tarjas.RowCount(), Primary!, dw_2, dw_2.RowCount() + 1, Primary!)
		
		dw_2.Retrieve(li_PlantaLote, li_EspecieLote, ll_NumeroLote,li_tipoenva,li_envase)
		
		dw_2.RowsMove(1, dw_2.RowCount(), Primary!, dw_tarjas, dw_tarjas.RowCount() + 1 , Primary!)
			
		IF ll_Saldo > 0 THEN
		ELSE
					
			CapturaKilosPromedio(as_columna,as_valor)
			dw_1.Object.mfgd_bulent[il_Fila] =  ll_Saldo
			dw_1.Object.mfgd_kgnent[il_Fila]	=	ll_Saldo * id_KilosPromed
		END IF
		
	END IF
END IF	
RETURN lb_Retorno
end function

public subroutine buscalote ();str_busqueda	lstr_busq
String			ls_Nula

SetNull(ls_Nula)

lstr_busq.argum[1]	=	String(dw_1.Object.plde_codigo[il_Fila])
lstr_busq.argum[2]	=	String(dw_1.Object.lote_espcod[il_Fila])
lstr_busq.argum[16]	=	String(dw_1.Object.clie_codigo[il_Fila])

If istr_mant.Argumento[2]='21' Then
	lstr_busq.argum[3]	=	"21"
   	lstr_busq.argum[4]	=	istr_mant.argumento[9]
	lstr_busq.argum[5]   	=  istr_mant.argumento[6]
	lstr_busq.argum[6]   	=  istr_mant.argumento[8]

	OpenWithParm(w_busc_spro_lotefrutagranel, lstr_busq)

	lstr_busq	= Message.PowerObjectParm
	
	If lstr_busq.argum[3] = "" Then
		dw_1.SetColumn("lote_codigo")
		dw_1.SetFocus()
	ElseIf Duplicado("lote_codigo", lstr_busq.argum[3]) Then
		dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
	Else
		dw_1.Object.lote_codigo[il_Fila] 	=	Integer(lstr_busq.argum[3])
		dw_1.Object.vari_nombre[il_Fila] 	=	lstr_busq.argum[13]
		dw_1.Object.prod_nombre[il_Fila]	=	lstr_busq.argum[6]
	
		If iuo_Lote.Existe(iuo_PltaLote.Codigo, iuo_Especie.Codigo, &
							Integer(lstr_busq.argum[3]), True, sqlca) Then
		  If iuo_TipoMovto.Sentido = 2 Then
			If Not HayExistencia("lote_codigo", lstr_busq.argum[3], &
				dw_1.Object.mfgd_bulent[il_Fila]) Then
				dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
			Else
				CapturaKilosPromedio("","")
			End If
		  End If				
		End If					
   End If
   Return
End If	

If istr_mant.Argumento[2]='2' Then
	lstr_busq.argum[3]	=	""
   lstr_busq.argum[4]	=	""//istr_mant.argumento[9]
	lstr_busq.argum[5]   =  ""//istr_mant.argumento[6]
	lstr_busq.argum[6]   =  ""//istr_mant.argumento[8]

	OpenWithParm(w_busc_spro_lotefrutagranel, lstr_busq)

	lstr_busq	= Message.PowerObjectParm
	
	If lstr_busq.argum[3] = "" Then
		dw_1.SetColumn("lote_codigo")
		dw_1.Object.vari_nombre[il_Fila] = ls_Nula
		dw_1.Object.prod_nombre[il_Fila] = ls_Nula

		dw_1.SetFocus()
	ElseIf Duplicado("lote_codigo", lstr_busq.argum[3]) Then
		dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
	Else
		dw_1.Object.lote_codigo[il_Fila] =	Integer(lstr_busq.argum[3])
		dw_1.Object.vari_nombre[il_Fila] =	lstr_busq.argum[13]
		dw_1.Object.prod_nombre[il_Fila] =	lstr_busq.argum[6]
	
		If iuo_Lote.Existe(iuo_PltaLote.Codigo, iuo_Especie.Codigo, &
							Integer(lstr_busq.argum[3]), True, sqlca) Then
		  If iuo_TipoMovto.Sentido = 2 Then
			If Not HayExistencia("lote_codigo", lstr_busq.argum[3], &
				dw_1.Object.mfgd_bulent[il_Fila]) Then
				dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
			End If
		  End If				
		End If					
   End If
   Return
End If	

If istr_mant.Argumento[2]='22' OR  istr_mant.Argumento[2]='30' Then
	lstr_busq.argum[3]	=	String(dw_1.Object.cama_codigo[il_Fila])
   lstr_busq.argum[4]   =  "0"
	
	OpenWithParm(w_busc_lotesfrutagranel_existencia, lstr_busq)

	lstr_busq	= Message.PowerObjectParm
	
	If lstr_busq.argum[3] = "" Then
		dw_1.SetColumn("lote_codigo")
		dw_1.SetFocus()
	ElseIf Duplicado("lote_codigo", lstr_busq.argum[3]) Then
		dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
	Else
		dw_1.Object.lote_codigo[il_Fila] =	Integer(lstr_busq.argum[3])
		dw_1.Object.vari_nombre[il_Fila] =	lstr_busq.argum[5]
		dw_1.Object.prod_nombre[il_Fila] =	lstr_busq.argum[7]
		dw_1.Object.enva_tipoen[il_Fila] =	integer(lstr_busq.argum[10])
		dw_1.Object.enva_codigo[il_Fila] =	integer(lstr_busq.argum[11])
		dw_1.Object.enva_nombre[il_Fila] =	lstr_busq.argum[12]
		dw_1.Object.cama_codigo[il_Fila] =  integer(lstr_busq.argum[9])
	   
		If Duplicado("lote_codigo", lstr_busq.argum[3]) Then
			dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
			dw_1.SetItem(il_Fila, "vari_nombre", ls_Nula)
			dw_1.SetItem(il_Fila, "prod_nombre", ls_Nula)
			dw_1.SetItem(il_Fila, "enva_tipoen", Integer(ls_Nula))
			dw_1.SetItem(il_Fila, "enva_codigo", Integer(ls_Nula))
			dw_1.SetItem(il_Fila, "enva_nombre", ls_Nula)
		Else
			If iuo_Lote.Existe(iuo_PltaLote.Codigo, iuo_Especie.Codigo, &
									 Integer(lstr_busq.argum[3]), True, sqlca) Then
			  If iuo_TipoMovto.Sentido = 2 Then
				If Not HayExistencia("lote_codigo", lstr_busq.argum[3], &
					dw_1.Object.mfgd_bulent[il_Fila]) Then
					dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
				Else
					CapturaKilosPromedio("","")
					
					dw_1.Object.mfgd_bulent[il_Fila] =  long(lstr_busq.argum[8])
					dw_1.Object.mfgd_kgnent[il_Fila]	=	long(lstr_busq.argum[8]) * id_KilosPromed
				End If
			  End If				
			End If
		End If	
   End If
   Return
End If

If istr_mant.Argumento[2]='23' Then
	lstr_busq.argum[3]	=	String(dw_1.Object.cama_codigo[il_Fila])
   lstr_busq.argum[4]   =  istr_mant.argumento[6]
	
	OpenWithParm(w_busc_lotesfrutagranel_existencia, lstr_busq)

	lstr_busq	= Message.PowerObjectParm
	
	If lstr_busq.argum[3] = "" Then
		dw_1.SetColumn("lote_codigo")
		dw_1.SetFocus()
	ElseIf Duplicado("lote_codigo", lstr_busq.argum[3]) Then
		dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
	Else
		dw_1.Object.lote_codigo[il_Fila] 	=	Integer(lstr_busq.argum[3])
		dw_1.Object.vari_nombre[il_Fila] 	=	lstr_busq.argum[5]
		dw_1.Object.prod_nombre[il_Fila]	=	lstr_busq.argum[7]
		dw_1.Object.enva_tipoen[il_Fila] 	=	integer(lstr_busq.argum[10])
		dw_1.Object.enva_codigo[il_Fila] 	=	integer(lstr_busq.argum[11])
		dw_1.Object.enva_nombre[il_Fila]	=	lstr_busq.argum[12]
		dw_1.Object.cama_codigo[il_Fila]	=  integer(lstr_busq.argum[9])
					
		dw_2.Reset()
		dw_tarjas.RowsMove(1, dw_tarjas.RowCount(), Primary!, dw_2, dw_2.RowCount() + 1, Primary!)
		
		dw_2.Retrieve(iuo_PltaLote.Codigo, iuo_Especie.Codigo, &
						 Integer(lstr_busq.argum[3]),integer(lstr_busq.argum[10]),integer(lstr_busq.argum[11]))
		
		dw_2.RowsMove(1, dw_2.RowCount(), Primary!, dw_tarjas, dw_tarjas.RowCount() + 1, Primary!)
	   
		If Duplicado("lote_codigo", lstr_busq.argum[3]) Then
			dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
			dw_1.SetItem(il_Fila, "vari_nombre", ls_Nula)
			dw_1.SetItem(il_Fila, "prod_nombre", ls_Nula)
			dw_1.SetItem(il_Fila, "enva_tipoen", Integer(ls_Nula))
			dw_1.SetItem(il_Fila, "enva_codigo", Integer(ls_Nula))
			dw_1.SetItem(il_Fila, "enva_nombre", ls_Nula)
		Else	
			If iuo_Lote.Existe(iuo_PltaLote.Codigo, iuo_Especie.Codigo, Integer(lstr_busq.argum[3]), True, sqlca) Then
			  If iuo_TipoMovto.Sentido = 2 Then
				If Not HayExistencia("lote_codigo", lstr_busq.argum[3], &
					dw_1.Object.mfgd_bulent[il_Fila]) Then
					dw_1.SetItem(il_Fila, "lote_codigo", Integer(ls_Nula))
				Else
					CapturaKilosPromedio("","")
					dw_1.Object.mfgd_bulent[il_Fila] =  long(lstr_busq.argum[8])
					dw_1.Object.mfgd_kgnent[il_Fila]	=	long(lstr_busq.argum[8]) * id_KilosPromed
				End If
			  End If				
			End If
		End If	
   End If
   Return
End If
end subroutine

public subroutine cambiatarjas (integer ai_fila);str_mant				lstr_mant
Integer				li_fila
uo_calicosechero	luo_Calidad

luo_Calidad	=	Create uo_calicosechero

lstr_mant.Argumento[01]	=	String(dw_1.Object.lote_pltcod[ai_fila])
lstr_mant.Argumento[02]	=	String(dw_1.Object.lote_espcod[ai_fila])
lstr_mant.Argumento[03]	=	String(dw_1.Object.lote_codigo[ai_fila])
lstr_mant.Argumento[04]	=	String(dw_1.Object.enva_tipoen[ai_fila])
lstr_mant.Argumento[05]	=	String(dw_1.Object.enva_codigo[ai_fila])
lstr_mant.dw					=	dw_tarjas

OpenWithParm(w_seleccion_tarjas, lstr_mant)

lstr_mant	= Message.PowerObjectParm		

If lstr_mant.Respuesta = 1 Then
	dw_1.Object.mfgd_kgnent[ai_fila]	=	0
	dw_1.Object.mfgd_bulent[ai_fila]	=	0
	
	For li_fila = 1 To dw_tarjas.RowCount()
		
		iuo_valida.binsduplicados(dw_1.Object.clie_codigo[il_fila],  dw_1.Object.plde_codigo[il_fila],  dw_Tarjas.Object.fgmb_nrotar[li_fila], False, Sqlca)
		iuo_valida.cargabins(dw_1.Object.clie_codigo[il_fila],  dw_1.Object.plde_codigo[il_fila], iuo_valida.Bins, True, Sqlca)
		luo_Calidad.Existe(iuo_valida.TipoEnva, iuo_valida.Envase, iuo_valida.Calidad, False, Sqlca)	

		dw_1.Object.mfgd_kgnent[ai_fila]	=	dw_1.Object.mfgd_kgnent[ai_fila] + (dw_tarjas.Object.fgmb_kilbru[li_fila] - luo_Calidad.Peso)
		dw_1.Object.mfgd_bulent[ai_fila]	=	dw_1.Object.mfgd_bulent[ai_fila] + dw_tarjas.Object.fgmb_canbul[li_fila]
	Next
	
End If
end subroutine

on w_mant_deta_movtofrutagranel_despacho.create
int iCurrent
call super::create
this.dw_lotes=create dw_lotes
this.pb_tarjas=create pb_tarjas
this.dw_2=create dw_2
this.dw_tarjas=create dw_tarjas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_lotes
this.Control[iCurrent+2]=this.pb_tarjas
this.Control[iCurrent+3]=this.dw_2
this.Control[iCurrent+4]=this.dw_tarjas
end on

on w_mant_deta_movtofrutagranel_despacho.destroy
call super::destroy
destroy(this.dw_lotes)
destroy(this.pb_tarjas)
destroy(this.dw_2)
destroy(this.dw_tarjas)
end on

event ue_recuperadatos;call super::ue_recuperadatos;IF istr_mant.Argumento[2]='21' THEN
   	dw_1.Object.lote_espcod[il_Fila]				=	integer(istr_mant.Argumento[8])
	dw_1.Object.prod_nombre[il_fila]				=	istr_mant.argumento[7]
	dw_1.Object.vari_nombre[il_fila]				=	istr_mant.argumento[10]
	
	dw_1.Object.lote_espcod.Protect 					=	1
	dw_1.Object.prod_nombre.Protect 				=	1
	dw_1.Object.vari_nombre.Protect 					=	1
	
	dw_1.Object.lote_espcod.Color 	=	RGB(255,255,255)
	dw_1.Object.prod_nombre.Color 	=	RGB(255,255,255)
	dw_1.Object.vari_nombre.Color 	=	RGB(255,255,255)
	
	dw_1.Object.lote_espcod.BackGround.Color 	=	553648127
	dw_1.Object.prod_nombre.BackGround.Color 	=	553648127
	dw_1.Object.vari_nombre.BackGround.Color 	=	553648127
END IF	

dw_1.Object.lote_pltcod.Protect 					=	0
dw_1.Object.lote_pltcod.Color						=	0
dw_1.Object.lote_pltcod.BackGround.Color		=	RGB(255,255,255)

ias_campo[1]											=	String(dw_1.object.plde_coorde[il_fila])
ias_campo[2]											=	String(dw_1.object.cama_codigo[il_fila])
ias_campo[3]											=	String(dw_1.object.lote_pltcod[il_fila])
ias_campo[4]											=	String(dw_1.object.lote_espcod[il_fila])
ias_campo[5]											=	String(dw_1.object.lote_codigo[il_fila])
ias_campo[6]											=	String(dw_1.object.mfgd_bulent[il_fila])
ias_campo[7]											=	String(dw_1.object.mfgd_kgnent[il_fila])
ias_campo[8]											=	String(dw_1.object.enva_tipoen[il_fila])
ias_campo[9]											=	String(dw_1.object.enva_codigo[il_fila])
ias_campo[10]											=	String(dw_1.object.clie_codigo[il_fila])

IF istr_mant.Agrega THEN
	dw_1.Object.clie_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[16])
	dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
	dw_1.Object.plde_coorde[il_Fila]	=	Integer(istr_Mant.Argumento[1])
	dw_1.Object.tpmv_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[2])
	dw_1.Object.mfge_numero[il_Fila]	=	Integer(istr_Mant.Argumento[3])
	dw_1.object.lote_pltcod[il_fila]	=	Integer(istr_Mant.Argumento[1])
	dw_1.object.lote_espcod[il_fila]	=	gstr_ParamPlanta.CodigoEspecie
	
	iuo_PltaLote.Existe(Integer(istr_Mant.Argumento[1]), True, sqlca)
	iuo_Especie.Existe(gstr_ParamPlanta.CodigoEspecie, True, sqlca)
	
ELSEIF Not istr_mant.Agrega AND Not istr_mant.Borra THEN
	iuo_PltaLote.Existe(dw_1.object.lote_pltcod[il_fila], True, sqlca)
	iuo_Especie.Existe(dw_1.object.lote_espcod[il_fila], True, sqlca)
	iuo_Lote.Existe(iuo_PltaLote.Codigo, iuo_Especie.Codigo, dw_1.object.lote_codigo[il_fila], True, sqlca)
	ExisteVariedad_gr(iuo_Especie.Codigo, iuo_Lote.Variedad, istr_Variedad)
	iuo_Productor.Existe(iuo_Lote.Productor, True, sqlca)
END IF

dw_tarjas.Visible	=	True
pb_tarjas.Enabled	=	True

TriggerEvent("Resize")

IF IsNull(dw_1.Object.lote_codigo[il_fila]) THEN
	dw_tarjas.SetFilter("lote_codigo = 0")
ELSE
	dw_tarjas.SetFilter("lote_codigo = " + String(dw_1.Object.lote_codigo[il_fila]))
END IF

dw_tarjas.Filter()
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.object.plde_coorde[il_fila]	=	Integer(ias_campo[1])
	dw_1.object.cama_codigo[il_fila]	=	Integer(ias_campo[2])
	dw_1.object.lote_pltcod[il_fila]	=	Integer(ias_campo[3])
	dw_1.object.lote_espcod[il_fila]	=	Integer(ias_campo[4])
	dw_1.object.lote_codigo[il_fila]	=	Long(ias_campo[5])
	dw_1.object.mfgd_bulent[il_fila]	=	Integer(ias_campo[6])
	dw_1.object.mfgd_kgnent[il_fila]	=	Dec(ias_campo[7])
	dw_1.object.clie_codigo[il_fila] =  Integer(ias_campo[10])

END IF
end event

event ue_antesguardar;Integer	li_Contador, li_row, li_suma
String	ls_Mensaje, ls_Columna[]

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

event ue_nuevo;call super::ue_nuevo;//Agrega L.M.
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
	dw_1.Object.lote_pltcod.Color 	= 	0
	dw_1.Object.lote_pltcod.BackGround.Color 	= 	RGB(255,255,255)
	
ELSE
	dw_1.Object.lote_pltcod.Protect 				= 	1
	dw_1.Object.lote_pltcod.Color					=	RGB(255,255,255)
	dw_1.Object.lote_pltcod.BackGround.Color	=	553648127
END IF

iuo_PltaLote.Existe(Integer(istr_Mant.Argumento[1]), True, sqlca)

IF IsNull(dw_1.Object.lote_codigo[il_fila]) THEN
	dw_tarjas.SetFilter("lote_codigo = 0")
ELSE
	dw_tarjas.SetFilter("lote_codigo = " + String(dw_1.Object.lote_codigo[il_fila]))
END IF

dw_tarjas.Filter()
end event

event open;/* 
	Argumentos
	istr_Mant.Argumento[1]	=	Código Planta
	istr_Mant.Argumento[2]	=	Tipo de Movimiento
	istr_Mant.Argumento[3]	=	Número de Despacho
	istr_Mant.Argumento[4]	=	Sentido del Movimiento => 2 = Despacho
	istr_mant.Argumento[16] =  Cliente
*/

x	= 100
y	= 220

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_lotes.SetTransObject(SQLCa)

iuo_PltaLote		=	Create uo_plantadesp
iuo_Especie		=	Create uo_especie
iuo_Lote			=	Create uo_lotesfrutagranel
iuo_Productor	=	Create uo_productores
iuo_Camara		=	Create uo_camarasfrigo
iuo_TipoMovto	=	Create uo_tipomovtofruta
iuo_valida		=	Create uo_validacionesvaciado

dw_1.GetChild("cama_codigo", idwc_Camara)
idwc_Camara.SetTransObject(sqlca)
idwc_Camara.Retrieve(Integer(istr_Mant.Argumento[1]))
idwc_Camara.SetSort("cama_nombre A")
idwc_Camara.Sort()

dw_1.GetChild("lote_pltcod", idwc_Planta)
idwc_Planta.SetTransObject(sqlca)
idwc_Planta.Retrieve(gi_codexport)
idwc_Planta.SetSort("plde_nombre A")
idwc_Planta.Sort()

dw_1.GetChild("lote_espcod", idwc_Especie)
idwc_Especie.SetTransObject(sqlca)
idwc_Especie.Retrieve(gstr_parempresa.empr_codexp)
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

dw_2.SetTransObject(sqlca)

dw_tarjas.SetTransObject(sqlca)
istr_mant.dw2.ShareData(dw_tarjas)

iuo_TipoMovto.Existe(Integer(istr_mant.Argumento[2]),True,SQLCA)

IF iuo_TipoMovto.Sentido = 2 THEN
	dw_1.Modify("titulo.Text = 'DETALLE LOTES A DESPACHO'")
	
	dw_1.Object.enva_tipoen.Protect				=	1
	dw_1.Object.enva_codigo.Protect				=	1
	dw_1.Object.enva_nombre.Protect			=	1
	dw_1.Object.mfgd_bulent.Protect				=	1
	
	dw_1.Object.enva_tipoen.Color		=	RGB(255,255,255)
	dw_1.Object.enva_codigo.Color	=	RGB(255,255,255)
	dw_1.Object.enva_nombre.Color	=	RGB(255,255,255)
	dw_1.Object.mfgd_bulent.Color	=	RGB(255,255,255)
	
	dw_1.Object.enva_tipoen.BackGround.Color		=	553648127
	dw_1.Object.enva_codigo.BackGround.Color	=	553648127
	dw_1.Object.enva_nombre.BackGround.Color	=	553648127
	dw_1.Object.mfgd_bulent.BackGround.Color	=	553648127
ELSE
	dw_1.Modify("titulo.Text = 'DETALLE LOTES A RECEPCION'")
END IF
end event

event resize;Integer		li_posic_x, li_posic_y, li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

This.Width			=	dw_1.Width + 600
If dw_tarjas.Visible Then 
	This.Height	 		= dw_1.Height + dw_Tarjas.Height + 270
Else
	This.Height			=	dw_1.Height + 300
End If

dw_1.x				=	78
dw_1.y				=	100	

li_posic_x			=	This.WorkSpaceWidth() - 400
li_posic_y			=	108

pb_acepta.width	=	li_Ancho
pb_acepta.height	=	li_Alto
pb_acepta.x			=	li_posic_x
pb_acepta.y			=	li_posic_y

pb_cancela.x		=	pb_acepta.x
pb_cancela.y		=	pb_acepta.y + li_Siguiente
pb_cancela.width	=	li_Ancho
pb_cancela.height	=	li_Alto

pb_salir.x			=	pb_acepta.x
pb_salir.y			=	pb_cancela.y + li_Siguiente
pb_salir.width		=	li_Ancho
pb_salir.height		=	li_Alto

dw_tarjas.y			= dw_1.y + dw_1.Height + 32
pb_tarjas.x			= pb_acepta.x
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_movtofrutagranel_despacho
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_movtofrutagranel_despacho
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_movtofrutagranel_despacho
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_movtofrutagranel_despacho
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_movtofrutagranel_despacho
integer x = 2245
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_movtofrutagranel_despacho
integer x = 2245
integer y = 232
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_movtofrutagranel_despacho
integer x = 2245
integer y = 584
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_movtofrutagranel_despacho
integer x = 59
integer y = 96
integer width = 2080
integer height = 1480
string dataobject = "dw_mant_movtofrutagraneldeta_despacho"
end type

event dw_1::itemchanged;String  ls_Columna, ls_Nula

SetNull(ls_Nula)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna
	CASE "cama_codigo"
		If Not iuo_Camara.Existe(Integer(istr_Mant.Argumento[1]), &
						Integer(Data), True, sqlca) Then
			This.SetItem(il_Fila, ls_Columna, integer(ias_campo[2]))
			
			Return 1
		ElseIf Duplicado(ls_Columna, Data) Then
			This.SetItem(il_Fila, ls_Columna, integer(ias_campo[2]))
			
			Return 1
		Else
			If iuo_TipoMovto.Sentido = 2 Then
				If Not HayExistencia(ls_Columna, Data, &
				This.Object.mfgd_bulent[il_Fila]) Then
					This.SetItem(il_Fila, ls_Columna, integer(ias_campo[2]))
					
					Return 1
				End If
			End If
		End If

	CASE "lote_pltcod"
		If Not iuo_PltaLote.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(il_Fila, ls_Columna, integer(ias_campo[3]))
			
			Return 1
		ElseIf Duplicado(ls_Columna, Data) Then
			This.SetItem(il_Fila, ls_Columna, integer(ias_campo[3]))
			
			Return 1
		Else
			If iuo_TipoMovto.Sentido = 2 Then
				If Not HayExistencia(ls_Columna, Data, &
				This.Object.mfgd_bulent[il_Fila]) Then
					This.SetItem(il_Fila, ls_Columna, integer(ias_campo[3]))
					
					Return 1
				End If
			End If
		End If

	CASE "lote_espcod"
		dw_1.Object.vari_nombre[Row] 	=	ls_nula
		dw_1.Object.prod_nombre[Row] 	=	ls_nula
		dw_1.Object.enva_tipoen[Row] 	=	Integer(ls_nula)
		dw_1.Object.enva_codigo[Row] 	=	Integer(ls_nula)
		dw_1.Object.enva_nombre[Row] 	=	ls_nula
		dw_1.Object.cama_codigo[Row] 	=  Integer(ls_nula)
		dw_1.Object.mfgd_bulent[Row] 	=  Integer(ls_nula)
		dw_1.Object.mfgd_kgnent[Row]	=	Integer(ls_nula)
		
		If Not iuo_Especie.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(il_Fila, ls_Columna, integer(ias_campo[4]))
			
			Return 1
		ElseIf Duplicado(ls_Columna, Data) Then
			This.SetItem(il_Fila, ls_Columna, integer(ias_campo[4]))
			
			Return 1
		Else
			If iuo_TipoMovto.Sentido = 2 Then				
				If Not HayExistencia(ls_Columna, Data, &
				This.Object.mfgd_bulent[il_Fila]) Then
					This.SetItem(il_Fila, ls_Columna, integer(ias_campo[4]))
					
					Return 1
				End If
			End If
		End If
		dw_tarjas.Visible	=	True
		
		Parent.TriggerEvent("Resize")

	CASE "lote_codigo"
		If Not iuo_Lote.Existe(This.Object.lote_pltcod[Row], This.Object.lote_espcod[Row], Long(Data), True, Sqlca) Then
			This.SetItem(il_Fila, ls_Columna, Long(ias_campo[5]))
			dw_1.Object.lote_codigo[Row] 	=	Long(ls_nula)
			dw_1.Object.vari_nombre[Row] 	=	ls_nula
			dw_1.Object.prod_nombre[Row] 	=	ls_nula
			dw_1.Object.enva_tipoen[Row] 	=	Long(ls_nula)
			dw_1.Object.enva_codigo[Row] 	=	Long(ls_nula)
			dw_1.Object.enva_nombre[Row] 	=	ls_nula
			dw_1.Object.cama_codigo[Row] 	=  Long(ls_nula)
			dw_1.Object.mfgd_bulent[Row] 	=  Long(ls_nula)
			dw_1.Object.mfgd_kgnent[Row]	=	Long(ls_nula)
			Return 1
			
		ElseIf Duplicado(ls_Columna, Data) Then
			dw_1.Object.lote_codigo[Row] 	=	Long(ls_nula)
			dw_1.Object.vari_nombre[Row] 	=	ls_nula
			dw_1.Object.prod_nombre[Row] 	=	ls_nula
			dw_1.Object.enva_tipoen[Row] 	=	Long(ls_nula)
			dw_1.Object.enva_codigo[Row] 	=	Long(ls_nula)
			dw_1.Object.enva_nombre[Row] 	=	ls_nula
			dw_1.Object.cama_codigo[Row] 	=  Long(ls_nula)
			dw_1.Object.mfgd_bulent[Row] 	=  Long(ls_nula)
			dw_1.Object.mfgd_kgnent[Row]	=	Long(ls_nula)
			Return 1
			
		Else
			If iuo_lote.totalneto > 0 Then
			   If istr_mant.argumento[2] = "23" Then
					If iuo_Lote.Productor <> Integer(istr_mant.argumento[6]) Then
						  MessageBox("Atención", "No existe Lote para Productor a Devolución.~r~r" + &
										  "Ingrese o seleccione otros antecedentes.")
						  This.SetItem(il_Fila, ls_Columna, Long(ias_campo[5]))
							Return 1
				  	End If
			   End If 
	
				If iuo_TipoMovto.Sentido = 2 Then
					If Not HayExistencia(ls_Columna, Data, &
						This.Object.mfgd_bulent[il_Fila]) Then
						This.SetItem(il_Fila, ls_Columna, Long(ias_campo[5]))
						
						Return 1
					Else
						If dw_lotes.Retrieve(This.Object.lote_pltcod[Row], This.Object.lote_espcod[Row], Long(Data), &
													This.Object.cama_codigo[Row]) > 0 Then
							dw_1.Object.lote_codigo[Row] 	=	dw_lotes.Object.lote_codigo[1]
							dw_1.Object.vari_nombre[Row] 	=	dw_lotes.Object.vari_nombre[1]
							dw_1.Object.prod_nombre[Row] 	=	dw_lotes.Object.prod_nombre[1]
							dw_1.Object.enva_tipoen[Row] 	=	dw_lotes.Object.enva_tipoen[1]
							dw_1.Object.enva_codigo[Row] 	=	dw_lotes.Object.enva_codigo[1]
							dw_1.Object.enva_nombre[Row] 	=	dw_lotes.Object.enva_nombre[1]
							dw_1.Object.cama_codigo[Row] 	=  dw_lotes.Object.cama_codigo[1]
							
							CapturaKilosPromedio("lote_codigo",Data)
							dw_1.Object.mfgd_bulent[Row] 	=  dw_lotes.Object.caex_canbul[1]
							dw_1.Object.mfgd_kgnent[Row]	=	dw_lotes.Object.caex_canbul[1] * id_KilosPromed
							
						Else
							MessageBox("Alerta", "El lote no posee existencia en la cámara indicada")
							dw_1.Object.lote_codigo[Row] 	=	Long(ls_nula)
							dw_1.Object.vari_nombre[Row] 	=	ls_nula
							dw_1.Object.prod_nombre[Row] 	=	ls_nula
							dw_1.Object.enva_tipoen[Row] 	=	Long(ls_nula)
							dw_1.Object.enva_codigo[Row] 	=	Long(ls_nula)
							dw_1.Object.enva_nombre[Row] 	=	ls_nula
							dw_1.Object.cama_codigo[Row] 	=  Long(ls_nula)
							dw_1.Object.mfgd_bulent[Row] 	=  Long(ls_nula)
							dw_1.Object.mfgd_kgnent[Row]	=	Long(ls_nula)
							
							Return 1
							
						End If
					End If
				End If
	
				ExisteVariedad_gr(iuo_Especie.Codigo, iuo_Lote.Variedad, istr_Variedad)
				iuo_Productor.Existe(iuo_Lote.Productor, True, sqlca)
				
				This.SetItem(il_Fila, "prod_nombre", iuo_Productor.Nombre)
				This.SetItem(il_Fila, "vari_nombre", istr_Variedad.Nombre)
				
				dw_tarjas.SetFilter("lote_codigo = " + Data)
				dw_tarjas.Filter()

			Else
				MessageBox("Atención", "No existe Lote como Definitivo.~r~r" + &
						     "Ingrese o seleccione otros antecedentes.")
			  	This.SetItem(il_Fila, ls_Columna, Long(ias_campo[5]))
						
				Return 1
			End If	
		End If

	CASE "enva_tipoen"
		If Not ExisteEnvase(Integer(Data), 0, istr_Envase) OR &
			Duplicado(ls_Columna, Data) OR istr_Envase.UsoEnvase <> 1 Then
			This.SetItem(il_Fila, ls_Columna, Integer(ias_campo[8]))

			Return 1
		Else
			If iuo_TipoMovto.Sentido = 2 Then
				If Not HayExistencia(ls_Columna, Data, &
					This.Object.mfgd_bulent[il_Fila]) Then
					This.SetItem(il_Fila, ls_Columna, Integer(ias_campo[8]))
					
					Return 1
				Else
					CapturaKilosPromedio(ls_Columna,Data)
				End If
			End If
		End If

	CASE "enva_codigo"
		istr_Envase.TipoEnvase = this.Object.enva_tipoen[il_fila]
		If Not ExisteEnvase(istr_Envase.TipoEnvase, Integer(Data), istr_Envase) OR &
			Duplicado(ls_Columna, Data) Then
			This.SetItem(il_Fila, ls_Columna, Integer(ias_campo[9]))
			
			Return 1
		Else
			dw_1.Object.enva_nombre[il_Fila]	=	istr_Envase.Nombre
			If iuo_TipoMovto.Sentido = 2 Then
				If Not HayExistencia(ls_Columna, Data, &
					This.Object.mfgd_bulent[il_Fila]) Then
					This.SetItem(il_Fila, ls_Columna, Integer(ias_campo[9]))
					
					Return 1
				Else
					CapturaKilosPromedio(ls_Columna,Data)
				End If
			End If
		End If
		
	CASE "mfgd_bulent"
		If Integer(data) > 9999 OR Integer(data) < 0 Then
			This.Setitem(Row,"mfgd_bulent", Integer(ls_Nula))
			Return 1
		Else
			If iuo_TipoMovto.Sentido = 2 Then
				If Not HayExistencia(ls_Columna, "0", integer(Data)) Then
					This.SetItem(il_Fila, ls_Columna, Integer(ias_campo[6]))
					This.SetItem(il_Fila,"mfgd_kgnent",dec(ias_campo[7]))
					Return 1
				Else
					This.Object.mfgd_kgnent[il_Fila]	=	Integer(Data) * id_KilosPromed
				End If
			End If
		End If


End CHOOSE
end event

event dw_1::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name
	CASE "b_buscalotes"
		BuscaLote()

	CASE "b_buscaenvase"
		BuscaEnvase()

	CASE "b_tarjas"
		cambiatarjas(Row)
		CapturaKilosPromedio("tarjas", "0")
END CHOOSE
end event

event dw_1::rowfocuschanged;call super::rowfocuschanged;
IF IsNull(dw_1.Object.lote_codigo[CurrentRow]) THEN
	dw_tarjas.SetFilter("lote_codigo = 0")
	
ELSE
	dw_tarjas.SetFilter("lote_codigo = " + String(dw_1.Object.lote_codigo[CurrentRow]))
	
END IF

dw_tarjas.Filter()
end event

type dw_lotes from datawindow within w_mant_deta_movtofrutagranel_despacho
boolean visible = false
integer x = 2185
integer y = 836
integer width = 274
integer height = 184
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_lotes_existencia_mod"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type pb_tarjas from picturebutton within w_mant_deta_movtofrutagranel_despacho
string tag = "Carga Tarjas"
integer x = 2245
integer y = 1764
integer width = 302
integer height = 244
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\apuntes.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\apuntes-bn.png"
alignment htextalign = left!
end type

event clicked;CambiaTarjas(dw_1.GetRow())
end event

type dw_2 from datawindow within w_mant_deta_movtofrutagranel_despacho
boolean visible = false
integer x = 2185
integer y = 1096
integer width = 274
integer height = 184
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_movtograndeta_lotes_disp"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_tarjas from uo_dw within w_mant_deta_movtofrutagranel_despacho
integer x = 59
integer y = 1644
integer width = 2080
integer height = 524
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_spro_movtograndeta_tarjas"
end type

