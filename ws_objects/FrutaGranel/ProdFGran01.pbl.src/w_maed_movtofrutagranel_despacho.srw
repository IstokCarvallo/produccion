$PBExportHeader$w_maed_movtofrutagranel_despacho.srw
$PBExportComments$Despacho de Fruta Granel Interplanta y Devolución a Productor
forward
global type w_maed_movtofrutagranel_despacho from w_mant_encab_deta_csd
end type
type tab_1 from tab within w_maed_movtofrutagranel_despacho
end type
type tp_1 from userobject within tab_1
end type
type dw_detalle from uo_dw within tp_1
end type
type tp_1 from userobject within tab_1
dw_detalle dw_detalle
end type
type tp_2 from userobject within tab_1
end type
type dw_envases from uo_dw within tp_2
end type
type tp_2 from userobject within tab_1
dw_envases dw_envases
end type
type tab_1 from tab within w_maed_movtofrutagranel_despacho
tp_1 tp_1
tp_2 tp_2
end type
type cb_guia from commandbutton within w_maed_movtofrutagranel_despacho
end type
type dw_exideta from datawindow within w_maed_movtofrutagranel_despacho
end type
type dw_exiencab from datawindow within w_maed_movtofrutagranel_despacho
end type
type dw_granpesa from datawindow within w_maed_movtofrutagranel_despacho
end type
type dw_error from uo_dw within w_maed_movtofrutagranel_despacho
end type
type dw_exismovtodetanulos from datawindow within w_maed_movtofrutagranel_despacho
end type
type dw_exidetaborra from datawindow within w_maed_movtofrutagranel_despacho
end type
type dw_movtobins from datawindow within w_maed_movtofrutagranel_despacho
end type
type dw_5 from datawindow within w_maed_movtofrutagranel_despacho
end type
type productoresxlote from structure within w_maed_movtofrutagranel_despacho
end type
type str_envase from structure within w_maed_movtofrutagranel_despacho
end type
type str_productores_envases from structure within w_maed_movtofrutagranel_despacho
end type
end forward

type productoresxlote from structure
	integer		prod_codigo[]
	string		prod_rut[]
	string		prod_nombre[]
	integer		lote_codigo[]
	integer		lote_espcod[]
	integer		lote_pltcod[]
end type

type str_envase from structure
	integer		tipenv
	integer		codenv
	string		calcos
	integer		cantid
end type

type str_productores_envases from structure
	long		productor[]
	long		guiasii[]
	integer		tipomovto[]
	integer		numero[]
	integer		cliente[]
end type

global type w_maed_movtofrutagranel_despacho from w_mant_encab_deta_csd
integer width = 5330
integer height = 2052
string title = "DESPACHO DE FRUTA GRANEL"
string menuname = ""
event type long ue_despuesguardar ( )
event ue_despuesborrar ( )
tab_1 tab_1
cb_guia cb_guia
dw_exideta dw_exideta
dw_exiencab dw_exiencab
dw_granpesa dw_granpesa
dw_error dw_error
dw_exismovtodetanulos dw_exismovtodetanulos
dw_exidetaborra dw_exidetaborra
dw_movtobins dw_movtobins
dw_5 dw_5
end type
global w_maed_movtofrutagranel_despacho w_maed_movtofrutagranel_despacho

type variables
w_mant_deta_movtofrutagranel_despacho	iw_mantencion_1
w_mant_deta_movtoenvadeta					iw_mantencion_2

DataWindowChild   								idwc_PltaDest, idwc_Motivo, idwc_Transp, idwc_Camion, &
														idwc_PltaOrig, idwc_PltaODdw1,idwc_Pltadw1,idwc_cliente
DataWindow										dw_3, dw_4

str_variedad										istr_variedad
str_categoria									istr_categoria

uo_cliente        								iuo_cliente
uo_productores									iuo_Productores
uo_plantadesp									iuo_PltaDestino
uo_motivodespacho							iuo_MotiDespacho
uo_transportista								iuo_Transport
uo_camiones									iuo_Camion
uo_tipodoctoplanta							iuo_TipoDocto
uo_FechaMovto									iuo_FechaMovto
uo_tipomovtofruta								iuo_TipoMovtoFruta
uo_tipomovtofruta								iuo_TipoMovtoEnva
nvuo_traspaso_interplanta					iuo_traspaso
uo_bins											iuo_bins
uo_calicosechero  								iuo_calicosechero
uo_grabatablabitacora						iuo_grabatablabitacora
uo_Mail											iuo_Mail

Long    	 										il_NumFruta = 0, il_NumEnva =  0, li_retorno, il_coderror
Boolean											ib_Modifica, ib_AutoCommit, ib_ConectadoExistencia, ib_ConectadoExistencia2
String												is_rut, 			is_columna,		is_rutemp,		is_sermed, is_chofer, is_error, is_correo
Integer											ii_Cantidad, 	il_conexiste, 	il_coneccion, 	il_coneccion2, il_zonal
DateTime										idt_FechaSistema

String												is_enva_tipoen[], is_enva_codigo[], is_cale_calida[], &
													is_cantidad[], 	is_pesone[], 		is_cale_nombre[], &
													is_prodenv[], 		is_prodbaspal[]

Transaction										sqlexi

Private:
str_Productores_Envases						wstr_Prod_Enva

end variables

forward prototypes
public subroutine buscaproductor ()
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean revisacamara (integer ai_plantadest)
public function boolean revisaenvases ()
public subroutine habilitaingreso ()
public function boolean noexistechofer (string rut)
public subroutine buscacamion ()
public function boolean existedespacho (integer ai_cliente, integer ai_planta, integer ai_tipomovto, long al_numero)
public function boolean lotesdestarados (integer ai_cliente, string as_lote)
public function boolean revisaultcorrel (integer ai_cliente, integer ai_plantadest)
public function boolean conexionexistencia ()
public function boolean cargaenvases ()
public subroutine determina_productoresenvase (integer ai_tipomovto)
public function boolean conexionexistencia_2 ()
public subroutine cargamovenv ()
public function boolean datos_correo ()
end prototypes

event type long ue_despuesguardar();integer	li_bodega, li_fila, li_cliente, li_planta,li_enva_codigo, li_enva_tipoen, li_bodevirtual, li_devcorreo,&
			li_bodzonal, li_lote_pltcod, li_lote_espcod, li_lote_codigo, li_corte
Long 		ll_fila, ll_numero, li_secuencia = 1, li_filarecep, ll_filarecep, li_secuenciarece = 1,&
			ll_numerorecep, ll_fila_nueva, ll_fila_nea, ll_count, ll_docrel, ll_numnuevoini, ll_numnuevofin,&
			ll_prod_codigo, ll_numdesp, ll_fil_newdesp, ll_fildesp, ll_newdespacho, ll_numerodespa, li_secdespacho, &
			ll_bodedest, li_secuenciadesp
String	ls_item_codigo, ls_productor, ls_prod, ls_calidad, ls_correozonal, ls_correo, ls_texto, ls_asunto, &
			ls_error, serrormsg, ls_correodestino
Boolean	lb_AutoCommit, lb_Retorno
Date		ld_mesprocori, ld_mesprocdest

If ib_Conectadoexistencia Then
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	ll_docrel = dw_2.Object.mfge_numero[1]
	
	If Not luo_existencia.paramexistencia(True, sqlexi) Then
		Message.DoubleParm = -1
		li_retorno = 2 
		RETURN 2
	Else
		li_bodevirtual = luo_existencia.bodvirtual
		li_bodzonal		= luo_existencia.bodzonal
		ls_correo 		= luo_existencia.correo
		ld_mesprocori 	= luo_existencia.Mesproceso
	End If		
	
	luo_existencia.existeregistro(ll_docrel,li_bodzonal,1,3,True,sqlexi,dw_2.Object.mfge_fecmov[1])
		
	If luo_existencia.count <> 0 Then
		li_retorno = 2 
		is_error = 'Registro ya Existe'
		Return 2
	End If	
	
	If Not luo_existencia.correlativobode(3,li_bodzonal,li_bodzonal,True,sqlexi) Then
		Message.DoubleParm = -1
		li_retorno = 2 
		RETURN 2
	Else
		ll_numero = luo_existencia.numero
	End If	
		
	If isnull(ll_numero) Then
		ll_numnuevoini = Long(String(li_bodzonal)+''+'0001')
		ll_numnuevofin = Long(String(li_bodzonal)+''+'9999')
		
		INSERT INTO dbo.correlbode(bode_codigo,mden_tipdoc,cobo_bodadm,cobo_inicia,cobo_final,cobo_actual)
		VALUES(:li_bodzonal,3,:li_bodzonal,:ll_numnuevoini,:ll_numnuevofin,:ll_numnuevoini)
		USING sqlexi;
		
		If sqlexi.SQLCode = -1 Then
			F_ErrorBaseDatos(sqlexi,"Correlbode")
			Message.DoubleParm = -1
			sqlexi.AutoCommit	=	ib_AutoCommit
			li_retorno = 2 
			RETURN 2
		End If
		ll_numero = ll_numnuevoini - 1
	End If	
	
	ll_numero = ll_numero + 1
	
	li_lote_pltcod = dw_3.Object.lote_pltcod[1]
	li_lote_espcod = dw_3.Object.lote_espcod[1]
	li_lote_codigo = dw_3.Object.lote_codigo[1]
	
	SELECT prod_codigo
	INTO :ll_prod_codigo
	FROM dbo.spro_lotesfrutagranel
	WHERE lote_pltcod = :li_lote_pltcod
		AND	lote_espcod = :li_lote_espcod
		AND	lote_codigo = :li_lote_codigo;
	
	ls_productor = String(ll_prod_codigo,'000000')
	
	If Not luo_existencia.Existecliente(ls_productor,True,sqlexi) Then
		Message.DoubleParm = -1
		is_error = 'Cliente '+ls_productor+' No Existe, Revise Tabla de Clientes en Existencia'
		li_retorno = 2 
		RETURN 2
	Else	
		ls_productor = luo_existencia.prod
	End If
	
	If luo_existencia.prod = '' OR isnull(luo_existencia.prod) Then
		ls_productor = luo_existencia.prdgen
	End If	

	ll_fila_nea = dw_exiencab.InsertRow(0)
	
	dw_exiencab.Object.mden_tipdoc[ll_fila_nea] 	= 	3
	dw_exiencab.Object.mden_numero[ll_fila_nea] 	= 	ll_numero 
	dw_exiencab.Object.tpdo_codigo[ll_fila_nea] 	= 	2
	dw_exiencab.Object.mden_fecmov[ll_fila_nea] 	= 	dw_2.Object.mfge_fecmov[1]
	dw_exiencab.Object.tpmv_tipomv[ll_fila_nea] 	= 	2
	dw_exiencab.Object.mden_estaci[1] = gstr_us.computador
	dw_exiencab.Object.mden_fecdig[1] = Date(Today())
	dw_exiencab.Object.mden_hordig[1] = Time(Now())
	If istr_mant.argumento[2] = '23' Then
		dw_exiencab.Object.tpmv_codigo[ll_fila_nea] 	= 	8
		dw_exiencab.Object.mden_observ[ll_fila_nea] 	= 	'Despacho Por Devolución al Productor'
		dw_exiencab.Object.clpr_rut[ll_fila_nea]	 	= 	ls_productor
	Else
		dw_exiencab.Object.tpmv_codigo[ll_fila_nea] 	= 	2
		dw_exiencab.Object.mden_observ[ll_fila_nea] 	= 	'Despacho a Bodega Zonal Interplanta'
	End If	
	dw_exiencab.Object.mden_tipana[ll_fila_nea] 	= 	4
	dw_exiencab.Object.bode_codigo[ll_fila_nea] 	= 	li_bodzonal
	
	If Not iuo_PltaDestino.Existe(dw_2.Object.plde_coorde[1], True, sqlca) Then
		li_retorno = 2 
		RETURN 2
	End If
	
	luo_existencia.bodega_administradora(iuo_PltaDestino.PackVirtual,True,sqlexi)

	dw_exiencab.Object.mden_bodest[ll_fila_nea] 	= 	luo_existencia.bodeAdmi	
	dw_exiencab.Object.mden_docrel[ll_fila_nea] 	= 	dw_2.Object.mfge_numero[1]
	dw_exiencab.Object.mden_fecdre[ll_fila_nea] 	= 	dw_2.Object.mfge_fecmov[1]
	dw_exiencab.Object.mden_estado[ll_fila_nea] 	= 	1
	dw_exiencab.Object.mden_pcopda[ll_fila_nea] =	1
	
	FOR li_fila = 1 TO dw_4.RowCount()
		ll_fila				=	dw_exideta.InsertRow(0)
		
		li_enva_codigo 	= 	dw_4.Object.enva_codigo[li_fila]
		li_enva_tipoen 	= 	dw_4.Object.enva_tipoen[li_fila]
		ls_calidad			=  dw_4.Object.cale_calida[li_fila]
		
		If Not iuo_calicosechero.Existe(li_enva_tipoen,li_enva_codigo,ls_calidad,True,sqlca) Then
			Message.DoubleParm = -1
			is_error = 'Problema con los Envases, Reviselos en Existencia'
			li_retorno = 2 
			RETURN 2
		Else
			ls_item_codigo = iuo_calicosechero.item 
		End If	
		
		If ls_item_codigo = '' OR IsNull(ls_item_codigo) Then
			ls_item_codigo = luo_existencia.itgene
		End If
	
		dw_exideta.Object.mden_tipdoc[ll_fila] 	= 	3
		dw_exideta.Object.mden_numero[ll_fila] 	= 	ll_numero
		dw_exideta.Object.mdde_secuen[ll_fila] 	= 	li_secuencia 
		dw_exideta.Object.tpmv_tipomv[ll_fila] 	= 	2
		
		If istr_mant.argumento[2] = '22' Then
			dw_exideta.Object.tpmv_codigo[ll_fila] 	= 	2
		Else
			dw_exideta.Object.tpmv_codigo[ll_fila] 	= 	8
		End If
		
		dw_exideta.Object.item_codigo[ll_fila] 	= 	ls_item_codigo
		dw_exideta.Object.mdde_identi[ll_fila] 	= 	''
		dw_exideta.Object.mdde_fecmov[ll_fila] 	= 	dw_2.Object.mfge_fecmov[1]
		dw_exideta.Object.bode_codigo[ll_fila] 	= 	li_bodzonal
		dw_exideta.Object.mdde_cantid[ll_fila] 	= 	dw_4.Object.fgme_cantid[li_fila]
					
		li_secuencia = li_secuencia + 1
	
	NEXT	
	////////////envía correo si se ejecuta un despacho con fercha menor al mes de proceso///////////////	
	If ld_mesprocori > dw_2.Object.mfge_fecmov[1] Then
		ls_texto	 = "No es posible actualizar movto. despacho Inter Planta Existencia, por modIficación anterior al mes de proceso"
		ls_asunto = "ModIfica Fruta Granel Despacho Movto. Nº "+String(dw_2.Object.mfge_numero[1])
			is_error = 'No es posible actualizar movto. recepción Inter Planta Existencia, por modIficación anterior al mes de proceso'
		li_retorno = 2 
		li_corte = 1
		Return 2
	End If
	
	If li_corte <> 1 Then
		If dw_exiencab.Rowcount() > 0 OR li_secuencia > 1 Then
			lb_AutoCommit		=	sqlexi.AutoCommit
			sqlexi.AutoCommit	=	False
		
			If dw_exiencab.Update(True, False) = 1 Then
				If dw_exideta.Update(True, False) = 1 Then
					Commit;
					
					If sqlexi.SQLCode <> 0 Then
						F_ErrorBaseDatos(sqlexi, This.Title)
						
						RollBack;
						li_retorno = 2	
						Messagebox("Existencia","Grabación de Datos NO se realizó")
					Else
						li_retorno = 1						
						dw_exiencab.ResetUpdate()
						dw_exideta.ResetUpdate()
						Messagebox("Existencia","Grabación de Datos realizada Satisfactoriamente")
					End If
				Else
					RollBack;
					li_retorno = 2
					Messagebox("Existencia","Grabación de Datos NO se realizó")
				End If
			Else
				F_ErrorBaseDatos(sqlexi, This.Title)
				RollBack;
				li_retorno = 2	
				Messagebox("Existencia","Grabación de Datos NO se realizó")
			End If
				
			dw_exiencab.Reset()
			dw_exideta.Reset()
		End If
		
		DISCONNECT USING sqlexi;
		
		//envío correo a encargados de bodega movimiento despacho
		ls_texto	 = "Se despachó a planta "+String(dw_2.Object.plde_coorde[1])+' desde planta '+String(dw_2.Object.plde_codigo[1])+".~nEl Nº despacho generado en existencia es "+String(ll_numero)
		ls_asunto = "Despacho interplanta Nº "+String(ll_docrel)
		
		iuo_Mail.of_Send({ls_correo},ls_asunto,ls_texto,0)
		
	Else
		dw_exideta.Reset()
		dw_exiencab.Reset()
	End If	
End If


end event

event ue_despuesborrar();Long 		ll_numero, respuesta, ll_nueva, ll_fila, ll_filote, ll_filaenva, ll_docrel, il_numeroenva
Boolean lb_AutoCommit
Integer	li_bodevirtual, li_bodzonal, li_bodecomercial, li_cliente, li_planta, li_tipomov, &
		ll_recepcion, li_lote, li_categoria 	
String ls_correo		
Date	ld_mesprocori, ld_fecha

IF ib_Conectadoexistencia = TRUE THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	ll_docrel = dw_2.Object.mfge_numero[1]
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
	ELSE
		li_bodevirtual = luo_existencia.bodvirtual
		li_bodzonal		= luo_existencia.bodzonal
		ls_correo 		= luo_existencia.correo
		ld_mesprocori 	= luo_existencia.Mesproceso
	END IF		
		
	IF luo_existencia.Mesproceso > dw_2.Object.mfge_fecmov[1] THEN
		Message.DoubleParm = -1
		RETURN 
	END IF	
	
	li_cliente 		= dw_2.Object.clie_codigo[1]
	li_planta 		= dw_2.Object.plde_codigo[1] 
	li_tipomov		= dw_2.Object.tpmv_codigo[1]
	ll_recepcion 	= dw_2.Object.mfge_numero[1]
		
	FOR ll_filaenva = 1 TO dw_4.RowCount()
	
		il_numeroenva = dw_2.Object.mfge_numero[1]
		ld_fecha		= dw_2.Object.mfge_fecmov[1]
			
		IF NOT luo_existencia.maximo_porfecha(3,li_bodzonal,il_numeroenva,1,ld_fecha,True,sqlexi) THEN
			Message.DoubleParm = -1
			Return
		ELSE
			ll_numero = luo_existencia.numero
		END IF
			
		IF NOT isnull(ll_numero) THEN
			//Return
				
		IF NOT luo_existencia.actualizaexistencia(2,3,li_bodzonal,ll_numero,il_numeroenva,True,sqlexi) THEN
			Message.DoubleParm = -1
			Return
		END IF	
		
		dw_exidetaborra.Retrieve(3,ll_numero)
		
		FOR ll_fila = 1 TO dw_exidetaborra.RowCount()
			ll_nueva = dw_exismovtodetanulos.InsertRow(0)
			
			dw_exismovtodetanulos.Object.mden_tipdoc[ll_nueva] = dw_exidetaborra.Object.mden_tipdoc[ll_fila]   
			dw_exismovtodetanulos.Object.mden_numero[ll_nueva] = dw_exidetaborra.Object.mden_numero[ll_fila]  
			dw_exismovtodetanulos.Object.mdde_secuen[ll_nueva] = dw_exidetaborra.Object.mdde_secuen[ll_fila]  
			dw_exismovtodetanulos.Object.tpmv_tipomv[ll_nueva] = dw_exidetaborra.Object.tpmv_tipomv[ll_fila]  
			dw_exismovtodetanulos.Object.tpmv_codigo[ll_nueva] = dw_exidetaborra.Object.tpmv_codigo[ll_fila]  
			dw_exismovtodetanulos.Object.item_codigo[ll_nueva] = dw_exidetaborra.Object.item_codigo[ll_fila]  
			dw_exismovtodetanulos.Object.item_armado[ll_nueva] = dw_exidetaborra.Object.item_armado[ll_fila]  
			dw_exismovtodetanulos.Object.mdde_identi[ll_nueva] = dw_exidetaborra.Object.mdde_identi[ll_fila]  
			dw_exismovtodetanulos.Object.mdde_fecmov[ll_nueva] = dw_exidetaborra.Object.mdde_fecmov[ll_fila]  
			dw_exismovtodetanulos.Object.bode_codigo[ll_nueva] = dw_exidetaborra.Object.bode_codigo[ll_fila]  
			dw_exismovtodetanulos.Object.ocen_numero[ll_nueva] = dw_exidetaborra.Object.mdde_ocompr[ll_fila]  
			dw_exismovtodetanulos.Object.ccon_codigo[ll_nueva] = dw_exidetaborra.Object.ccon_codigo[ll_fila]  
			dw_exismovtodetanulos.Object.exic_codigo[ll_nueva] = dw_exidetaborra.Object.exic_codigo[ll_fila]  
			dw_exismovtodetanulos.Object.mdde_cantid[ll_nueva] = dw_exidetaborra.Object.mdde_cantid[ll_fila]  
			dw_exismovtodetanulos.Object.mdde_canarm[ll_nueva] = dw_exidetaborra.Object.mdde_canarm[ll_fila]  
			dw_exismovtodetanulos.Object.mdde_prunmo[ll_nueva] = dw_exidetaborra.Object.mdde_prunmo[ll_fila]  
			dw_exismovtodetanulos.Object.mdde_totmor[ll_nueva] = dw_exidetaborra.Object.mdde_totmor[ll_fila]  
			dw_exismovtodetanulos.Object.mdde_totpes[ll_nueva] = dw_exidetaborra.Object.mdde_totpes[ll_fila]  
			dw_exismovtodetanulos.Object.mdde_facpes[ll_nueva] = dw_exidetaborra.Object.mdde_facpes[ll_fila]  
			dw_exismovtodetanulos.Object.mdde_facmor[ll_nueva] = dw_exidetaborra.Object.mdde_facmor[ll_fila]  
	
		NEXT
		
		DELETE FROM dbo.exismovtodeta 
			WHERE	mden_numero = :ll_numero
			AND	mden_tipdoc = 3
			AND	bode_codigo = :li_bodzonal
			USING sqlexi;
		
		IF sqlexi.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlexi,"exismovtodeta")
			Message.DoubleParm = -1
			sqlexi.AutoCommit	=	ib_AutoCommit
			RETURN 
		END IF
		
		IF dw_exismovtodetanulos.Rowcount() > 0 THEN
			lb_AutoCommit		=	sqlexi.AutoCommit
			sqlexi.AutoCommit	=	False
			
			IF dw_exismovtodetanulos.Update(True, False) = 1 THEN
				
				dw_exismovtodetanulos.ResetUpdate()
	
			ELSE
				F_ErrorBaseDatos(sqlexi, This.Title)
					
				RollBack;
				Return
			END IF
			
			sqlexi.AutoCommit	=	lb_AutoCommit
		
		END IF
	END IF		
	NEXT
END IF

end event

public subroutine buscaproductor ();//Str_busqueda	lstr_busq
//
//OpenWithParm(w_busc_productores, lstr_busq)
//
//lstr_busq	= Message.PowerObjectParm
//
//IF lstr_busq.argum[1] = "" THEN
//	dw_2.SetColumn("prod_codigo")
//	dw_2.SetFocus()
//ELSE
//	dw_2.Object.prod_codigo[il_fila]	=	Long(lstr_busq.argum[1])
//	dw_2.Object.prod_nombre[il_fila]	=	lstr_busq.argum[2]
//	istr_Mant.Argumento[6]				=	lstr_busq.argum[1]
//	istr_mant.argumento[8]				=	lstr_busq.argum[2]
//	
//	dw_2.SetFocus()
//END IF
//
//RETURN
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect				=	0
	dw_2.Object.mfge_numero.Protect			=	0
	dw_2.Object.plde_coorde.Protect				=	0
	dw_2.Object.tran_codigo.Protect				=	0
	dw_2.Object.cami_patent.Protect				=	0
	dw_2.Object.cami_patcar.Protect				=	0
	dw_2.Object.mfge_rutcho.Protect				=	0
	dw_2.Object.mfge_chofer.Protect				=	0
	dw_2.Object.mfge_fecmov.Protect				=	0
	dw_2.Object.refg_horasa.Protect				=	0
	dw_2.Object.prod_codigo.Protect				=	0
	dw_2.Object.moti_codigo.Protect				=	0

	dw_2.Object.clie_codigo.Color					=	0
	dw_2.Object.mfge_numero.Color				=	0
	dw_2.Object.plde_coorde.Color				=	0
	dw_2.Object.tran_codigo.Color					=	0
	dw_2.Object.cami_patent.Color				=	0
	dw_2.Object.cami_patcar.Color				=	0
	dw_2.Object.mfge_rutcho.Color				=	0
	dw_2.Object.mfge_chofer.Color				=	0
	dw_2.Object.mfge_fecmov.Color				=	0
	dw_2.Object.refg_horasa.Color					=	0
	dw_2.Object.prod_codigo.Color				=	0
	dw_2.Object.moti_codigo.Color					=	0
	
	dw_2.Object.clie_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.mfge_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.plde_coorde.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patent.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patcar.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.mfge_rutcho.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.mfge_chofer.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.mfge_fecmov.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.refg_horasa.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.prod_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.moti_codigo.BackGround.Color		=	RGB(255,255,255)
	
	dw_2.Object.b_camion.Visible           	=  1
	IF integer(istr_mant.Argumento[2]) = 23 THEN dw_2.Object.b_buscaproductor.Visible      =  1
ELSE
	dw_2.Object.clie_codigo.Protect				=	1
	dw_2.Object.mfge_numero.Protect			=	1
	dw_2.Object.plde_coorde.Protect				=	1
	dw_2.Object.tran_codigo.Protect				=	1
	dw_2.Object.cami_patent.Protect				=	1
	dw_2.Object.cami_patcar.Protect				=	1
	dw_2.Object.mfge_rutcho.Protect				=	1
	dw_2.Object.mfge_chofer.Protect				=	1
	dw_2.Object.mfge_fecmov.Protect				=	1
	dw_2.Object.refg_horasa.Protect				=	1
	dw_2.Object.prod_codigo.Protect				=	1
	dw_2.Object.moti_codigo.Protect				=	1

	dw_2.Object.clie_codigo.Color					=	RGB(255,255,255)
	dw_2.Object.mfge_numero.Color				=	RGB(255,255,255)
	dw_2.Object.plde_coorde.Color				=	RGB(255,255,255)
	dw_2.Object.tran_codigo.Color					=	RGB(255,255,255)
	dw_2.Object.cami_patent.Color				=	RGB(255,255,255)
	dw_2.Object.cami_patcar.Color				=	RGB(255,255,255)
	dw_2.Object.mfge_rutcho.Color				=	RGB(255,255,255)
	dw_2.Object.mfge_chofer.Color				=	RGB(255,255,255)
	dw_2.Object.mfge_fecmov.Color				=	RGB(255,255,255)
	dw_2.Object.refg_horasa.Color					=	RGB(255,255,255)
	dw_2.Object.prod_codigo.Color				=	RGB(255,255,255)
	dw_2.Object.moti_codigo.Color					=	RGB(255,255,255)
	
	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
	dw_2.Object.mfge_numero.BackGround.Color	=	553648127
	dw_2.Object.plde_coorde.BackGround.Color		=	553648127
	dw_2.Object.tran_codigo.BackGround.Color		=	553648127
	dw_2.Object.cami_patent.BackGround.Color		=	553648127
	dw_2.Object.cami_patcar.BackGround.Color		=	553648127
	dw_2.Object.mfge_rutcho.BackGround.Color	=	553648127
	dw_2.Object.mfge_chofer.BackGround.Color	=	553648127
	dw_2.Object.mfge_fecmov.BackGround.Color	=	553648127
	dw_2.Object.refg_horasa.BackGround.Color		=	553648127
	dw_2.Object.prod_codigo.BackGround.Color		=	553648127
	dw_2.Object.moti_codigo.BackGround.Color		=	553648127
	
	dw_2.Object.b_camion.Visible           	=  0
	IF integer(istr_mant.Argumento[2]) = 23 THEN 	dw_2.Object.b_buscaproductor.Visible      =  0
END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean			lb_Retorno
Integer			li_planta, li_TipoMovto
Long				ll_Numero

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

IF NOT Borrando THEN
	IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		dw_2.SetItem(1,"mfge_usuari",gstr_us.nombre)
		dw_2.SetItem(1,"mfge_comput",gstr_us.computador)
		dw_2.SetItem(1,"mfge_horacr",F_FechaHora())
	ELSEIF dw_2.GetItemStatus(1, 0, Primary!) = DataModified! THEN
		dw_2.SetItem(1,"mfge_usumod",gstr_us.nombre)
		dw_2.SetItem(1,"mfge_commod",gstr_us.computador)
		dw_2.SetItem(1,"mfge_horact",F_FechaHora())
	END IF
END IF

IF Borrando THEN
	IF dw_4.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF dw_3.Update(True, False) = 1 THEN
				IF dw_5.Update(True, False) = 1 THEN
					IF dw_2.Update(True, False) = 1 THEN
					
						IF Long(dw_2.Object.mfge_guisii[1]) > 0 THEN
							iuo_traspaso.ii_planta 		= 	dw_2.Object.plde_codigo[1]
							iuo_traspaso.ii_tipo			=	dw_2.Object.tpmv_codigo[1]
							iuo_traspaso.ii_movto		=	dw_2.Object.mfge_numero[1]
							iuo_traspaso.ii_cliente		=	dw_2.Object.clie_codigo[1]
							iuo_traspaso.ii_plantades 	=	dw_2.Object.plde_coorde[1]
							iuo_traspaso.ii_sentido 	= 	3
							iuo_traspaso.ii_ManAut		=	1
							iuo_traspaso.il_guiasii		=	dw_2.Object.mfge_guisii[1]
							
							iuo_traspaso.TriggerEvent("traspasadatos")
						END IF
						IF Message.DoubleParm = 0 THEN
							Commit;
					
							IF sqlca.SQLCode <> 0 THEN
								F_ErrorBaseDatos(sqlca, This.Title)
							ELSE
								lb_Retorno	=	True
						
								dw_1.ResetUpdate()
								dw_2.ResetUpdate()
								dw_3.ResetUpdate()
								dw_4.ResetUpdate()
								dw_5.ResetUpdate()
							END IF
						ELSE
							F_ErrorBaseDatos(sqlca, This.Title)
							RollBack;
						END IF
					ELSE
						F_ErrorBaseDatos(sqlca, This.Title)
						RollBack;
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
			
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
		
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
	
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)

		RollBack;
	END IF
ELSE
	li_Planta		=	dw_2.Object.plde_codigo[1]
	li_TipoMovto	=	dw_2.Object.tpmv_codigo[1]
	ll_Numero		=	dw_2.Object.mfge_numero[1]

	IF dw_2.Update(True, False) = 1 THEN
		IF dw_3.Update(True, False) = 1 THEN
			IF dw_1.Update(True, False) = 1 THEN
				IF dw_4.Update(True, False) = 1 THEN
					IF dw_5.Update(True, False) = 1 THEN
						Commit;
						
						IF sqlca.SQLCode <> 0 THEN
							F_ErrorBaseDatos(sqlca, This.Title)
						ELSE
							IF li_TipoMovto = 30 AND NOT istr_mant.solo_consulta THEN
								DECLARE GeneraRecepcion PROCEDURE FOR dbo.FGran_GeneraTraspasoInterno  
									@Planta		=	:li_Planta,   
									@TipoMovto	=	:li_TipoMovto,   
									@Numero		=	:ll_Numero  ;
					
								EXECUTE GeneraRecepcion ;
								
								IF sqlca.SQLCode = -1 THEN
									//F_ErrorBaseDatos(sqlca, This.Title)
									RollBack ;
								ELSE
									istr_mant.solo_consulta = TRUE
									pb_eli_det.Enabled = FALSE
									pb_ins_det.Enabled = FALSE
								END IF
								Close GeneraRecepcion ;
							END IF
							
							lb_Retorno	=	True
				
							dw_1.ResetUpdate()
							dw_2.ResetUpdate()
							dw_3.ResetUpdate()
							dw_4.ResetUpdate()
							dw_5.ResetUpdate()
						END IF
					ELSE
						F_ErrorBaseDatos(sqlca, This.Title)
				
						RollBack;
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
			
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
		
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
	
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)

		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	ib_AutoCommit

RETURN lb_Retorno
end function

public function boolean revisacamara (integer ai_plantadest);Long		ll_Numero
Boolean	lb_Retorno = True

SELECT	cama_codigo	INTO	:ll_Numero
	FROM	dbo.camarasbode
	WHERE	plde_codigo	=	:ai_Plantadest
	AND	cama_codigo	=	0;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Cámaras Frigoríficas.")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Planta de Destino no posee Cámara 0 - Patio - asociada. Ingrese o Seleccione Otro Código.")
	lb_Retorno	=	False
END IF

RETURN lb_retorno
end function

public function boolean revisaenvases ();Long ll_Fila, ll_FilaBusc, ll_Filaarr,arr_envases[500,4], ll_sumBultos, ll_Row, ll_FilaUlt
Integer  li_TipoEnva, li_Envase, li_TipoEnvaSig, li_EnvaseSig, li_cont
String   ls_mensaje
Boolean  lb_ya_ingresado = False

ll_Fila = 1
ll_FilaArr = 1

Do While  ll_Fila<= dw_3.RowCount()
	
	ll_sumBultos = 0
	lb_ya_ingresado = False
	
   li_Tipoenva =	dw_3.Object.enva_tipoen[ll_Fila]
	li_envase	=	dw_3.Object.enva_codigo[ll_Fila]
		
	FOR ll_Row=1 To UpperBound(arr_envases)
		IF arr_envases[ll_row,1] = li_TipoEnva AND arr_envases[ll_row,2] = li_Envase THEN
			lb_ya_ingresado = TRUE
			ll_row = UpperBound(arr_envases)
		ELSE
			IF arr_envases[ll_row,1] = 0 THEN ll_row = UpperBound(arr_envases)
		END IF
	NEXT	
	
	IF lb_ya_ingresado = False THEN
		
		ll_SumBultos = dw_3.Object.mfgd_bulent[ll_Fila]
		
		FOR ll_FilaBusc = (ll_fila + 1) TO dw_3.RowCount()
			li_TipoEnvaSig =	dw_3.Object.enva_tipoen[ll_FilaBusc]
			li_EnvaseSig	=	dw_3.Object.enva_codigo[ll_FilaBusc]
			IF li_TipoEnva = li_TipoEnvaSig And li_Envase=li_EnvaseSig THEN
				ll_SumBultos = ll_SumBultos + dw_3.Object.mfgd_bulent[ll_FilaBusc]
			END IF	
		NEXT	
		
		arr_envases[ll_FilaArr,1] = li_TipoEnva
		arr_envases[ll_FilaArr,2] = li_Envase
		arr_envases[ll_FilaArr,3] = ll_SumBultos
		arr_envases[ll_FilaArr,4] = 0
		ll_FilaArr++
	END IF
	ll_Fila++
LOOP	

ll_Fila    = 1
ll_FilaUlt = ll_FilaArr

DO WHILE  ll_Fila<= dw_4.RowCount()
	
	ll_sumBultos = 0
	lb_ya_ingresado = False
	
   li_Tipoenva =	dw_4.Object.enva_tipoen[ll_Fila]
	li_envase	=	dw_4.Object.enva_codigo[ll_Fila]
		
	FOR ll_Row=1 To UpperBound(arr_envases)
		IF arr_envases[ll_Row,1] = li_TipoEnva AND arr_envases[ll_Row,2] = li_Envase THEN
			lb_ya_ingresado	=	TRUE
			ll_FilaArr			=	ll_Row
			ll_row				=	UpperBound(arr_envases)
		ELSE
			IF arr_envases[ll_Row,1] = 0 THEN ll_row	=	UpperBound(arr_envases)
		END IF
	NEXT	
	
	IF lb_ya_ingresado THEN
		
		IF arr_envases[ll_FilaArr,4] = 0 THEN
			ll_SumBultos = dw_4.Object.fgme_cantid[ll_Fila]
			
			FOR ll_FilaBusc = (ll_fila + 1) TO dw_4.RowCount()
				li_TipoEnvaSig	=	dw_4.Object.enva_tipoen[ll_FilaBusc]
				li_EnvaseSig	=	dw_4.Object.enva_codigo[ll_FilaBusc]
				IF li_TipoEnva = li_TipoEnvaSig And li_Envase=li_EnvaseSig THEN
					ll_SumBultos = ll_SumBultos + dw_4.Object.fgme_cantid[ll_FilaBusc]
				END IF	
			NEXT	
			
			arr_envases[ll_FilaArr,4] = ll_SumBultos
		END IF	
	ELSE
		arr_envases[ll_FilaUlt,1] = li_TipoEnva
		arr_envases[ll_FilaUlt,2] = li_Envase
		arr_envases[ll_FilaUlt,3] = 0
      arr_envases[ll_FilaUlt,4] = dw_4.Object.fgme_cantid[ll_Fila]
		ll_FilaUlt++
	END IF
	ll_Fila++
LOOP	

FOR ll_Fila=1 TO UpperBound(arr_envases)
	IF arr_envases[ll_Fila,3]<>arr_envases[ll_Fila,4] THEN
   	li_cont ++
	   ls_mensaje = 	ls_mensaje + "~nTipo Envase " + String(arr_envases[ll_Fila,1]) 
		ls_mensaje = 	ls_mensaje + "  y Envase " + String(arr_envases[ll_Fila,2]) 
		ls_mensaje = 	ls_mensaje + "  Bultos (" + String(arr_envases[ll_Fila,3])
		ls_mensaje = 	ls_mensaje + ") (" + String(arr_envases[ll_Fila,4]) + ")"
	   IF arr_envases[ll_Fila,1] = 0 THEN ll_Fila = UpperBound(arr_envases)	
   END IF		
NEXT

IF li_cont > 0 AND NOT gstr_paramplanta.palletdebins THEN
	MessageBox("Error de Consistencia", "No Existe Concordancia de Bultos en :" + ls_mensaje + ".", StopSign!, Ok!)
	RETURN FALSE
END IF


RETURN TRUE

end function

public subroutine habilitaingreso ();Boolean	lb_Estado = True
Date		ld_Fecha
Time		lt_Hora

IF dw_2.Object.mfge_fecmov[1] = ld_Fecha OR IsNull(dw_2.Object.mfge_fecmov[1]) THEN
	lb_Estado = False
END IF

IF dw_2.Object.refg_horasa[1] = lt_Hora OR IsNull(dw_2.Object.refg_horasa[1]) THEN
	lb_Estado = False
END IF

IF dw_2.Object.moti_codigo[1] = 0 OR IsNull(dw_2.Object.moti_codigo[1]) THEN
	lb_Estado = False
END IF

IF UpperBound(istr_Mant.Argumento[]) < 2 THEN
	lb_Estado = False
	tab_1.tp_1.Enabled	=	lb_Estado
	tab_1.tp_2.Enabled	=	lb_Estado
	pb_ins_det.Enabled	=	lb_Estado
	Return 
END IF

IF istr_Mant.Argumento[2] = "22" OR istr_Mant.Argumento[2] = "30" THEN
	IF dw_2.Object.plde_coorde[1] = 0 OR IsNull(dw_2.Object.plde_coorde[1]) THEN
		lb_Estado = False
	END IF
ELSEIF dw_2.Object.prod_codigo[1] = 0 OR IsNull(dw_2.Object.prod_codigo[1]) THEN
	lb_Estado = False
END IF
	
IF istr_Mant.Argumento[2] <> "30"	THEN
	IF dw_2.Object.tran_codigo[1] = 0 OR IsNull(dw_2.Object.tran_codigo[1]) THEN
		lb_Estado = False
	END IF
	
	IF dw_2.Object.cami_patent[1] = "" OR IsNull(dw_2.Object.cami_patent[1]) THEN
		lb_Estado = False
	END IF
	
	IF dw_2.Object.mfge_rutcho[1] = "" OR IsNull(dw_2.Object.mfge_rutcho[1]) THEN
		lb_Estado = False
	END IF
	
	IF dw_2.Object.mfge_chofer[1] = "" OR IsNull(dw_2.Object.mfge_chofer[1]) THEN
		lb_Estado = False
	END IF
END IF

tab_1.tp_1.Enabled	=	lb_Estado
tab_1.tp_2.Enabled	=	lb_Estado
pb_ins_det.Enabled	=	lb_Estado
end subroutine

public function boolean noexistechofer (string rut);
String ls_nombre,ls_paterno,ls_materno

SELECT 	clpr_nombre,clpr_apepat,clpr_apemat
INTO		:ls_nombre, :ls_paterno , :ls_materno
FROM 		dbo.clienprove
WHERE		clpr_nrorut =:rut;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClienProve")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Rut No ha sido Generado. Ingrese Otro.")
	Return True
END IF

is_chofer	= 	ls_nombre + " " + ls_paterno + " " + ls_materno
Return False
end function

public subroutine buscacamion ();Str_busqueda	lstr_busq

IF istr_Mant.Argumento[2] = "22" OR istr_Mant.Argumento[2] = "30" THEN
   lstr_busq.argum[1] = '2'
ELSE
	lstr_busq.argum[1] = '1'
END IF

OpenWithParm(w_busc_camiones, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_2.SetColumn("cami_patent")
	dw_2.SetFocus()
ELSE
	dw_2.Object.cami_clasifi[il_fila]	=	Integer(lstr_busq.argum[1])
	dw_2.Object.cami_patent[il_fila]		=	lstr_busq.argum[2]
	dw_2.Object.cami_patcar[il_fila]		=	lstr_busq.argum[6]
	dw_2.Object.mfge_rutcho[il_fila]		=	lstr_busq.argum[5]
	is_rut 										= 	lstr_busq.argum[5]
	dw_2.Object.mfge_chofer[il_fila]		=	lstr_busq.argum[4]
	HabilitaIngreso()
	dw_2.SetFocus()
END IF

RETURN
end subroutine

public function boolean existedespacho (integer ai_cliente, integer ai_planta, integer ai_tipomovto, long al_numero);Integer	li_Contador
Long		ll_Numero
Boolean	lb_Retorno = True

SELECT	mfge_numero
	INTO	:ll_Numero
	FROM	dbo.spro_movtofrutagranenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfge_numero	=	:al_Numero 
	AND   clie_codigo =  :ai_cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Despachos Fruta Granel")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	istr_mant.Argumento[3]	=	String(al_Numero)
	This.TriggerEvent("ue_recuperadatos")
ELSE
	MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean lotesdestarados (integer ai_cliente, string as_lote);Integer li_codigo, li_plt, li_esp, li_lot
String  ls_nombre
Boolean lb_Retorno=True

li_plt = Integer(Mid(as_lote,1,4))
li_esp = Integer(Mid(as_lote,5,2))
li_lot = Integer(Mid(as_lote,7,4))

SELECT lote_codigo
  INTO :li_codigo
  FROM dbo.spro_lotesfrutagranel
 WHERE lote_pltcod = :li_plt
	AND lote_espcod = :li_esp
	AND lote_codigo = :li_lot
	AND lote_totnet > 0;
//	AND clie_codigo = :ai_cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Lotes Fruta Granel")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	lb_Retorno	=	False
END IF	
RETURN lb_retorno

end function

public function boolean revisaultcorrel (integer ai_cliente, integer ai_plantadest);Long		ll_Planta
Boolean	lb_Retorno = True

SELECT	plde_codigo	INTO	:ll_Planta
	FROM	dbo.spro_correltipomovto
	WHERE	plde_codigo	=	:ai_Plantadest
	AND	tpmv_tipcor =  1;
//	AND   clie_codigo =  :ai_cliente;

	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Correlativos.")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","No han sido creado ultimos correlativos para Planta de Destino. Ingrese o Seleccione Otro Código.")
	lb_Retorno	=	False
END IF

RETURN lb_retorno
end function

public function boolean conexionexistencia ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta

IF ib_ConectadoExistencia THEN
	DISCONNECT USING sqlexi;
END IF

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

SELECT 	cone_nomodb,	cone_nomser,	cone_nombas,
			cone_nodbms,	cone_nomusu,	cone_passwo  
	INTO 	:ls_nomodb,		:ls_nomser,		:ls_nombas,
			:ls_nodbms,		:ls_Usuario,	:ls_Password
	FROM dbo.prodconectividad   
	WHERE cone_codigo = :il_coneccion;

sqlexi.ServerName	=	ls_nomser
sqlexi.DataBase		=	ls_nombas
sqlexi.Dbms			= 	ls_nodbms
sqlexi.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'

CONNECT USING sqlexi;

is_base = ls_nombas

IF sqlexi.SQLCode = 0 THEN
	ib_ConectadoExistencia	=	True
ELSE
	ib_ConectadoExistencia	=	False
END IF

RETURN ib_ConectadoExistencia
end function

public function boolean cargaenvases ();Long 			ll_filas, ll_Fila, ll_rectbp, ll_tibapa[], ll_cant_tibapa[], ll_tibapaant, ll_totalbultos
Integer		li_filas, li_desde, li_hasta, li_UseFind, li_recenv
String 		ls_enva_tipoen[], ls_enva_codigo[], ls_cale_calida[], ls_cantidad[], &
				ls_pesone[], ls_cale_nombre[], ls_prodenv[], ls_prodbaspal[], ls_prod_nombre[]
Boolean		lb_flag, lb_flgtpb
DataWindow	dw_9Mirror, dw_binsmirror

dw_9Mirror		=	Create DataWindow
dw_binsmirror	=	Create DataWindow

dw_granpesa.SetTransObject(sqlca)
dw_movtobins.SetTransObject(sqlca)

dw_granpesa.Reset()

FOR ll_filas = 1 TO dw_3.RowCount()
	dw_granpesa.Retrieve(dw_3.Object.lote_pltcod[ll_filas], dw_3.Object.lote_espcod[ll_filas], dw_3.Object.lote_codigo[ll_filas])													
NEXT

If dw_5.RowCount() > 0 Then
	dw_5.SetFilter("")
	dw_5.Filter()
	
	dw_9Mirror		=	dw_granpesa//dw_5
	dw_binsmirror	=	dw_movtobins
	
	dw_9Mirror.SetSort("bins_numero asc")
	dw_binsmirror.SetSort("mfgp_tibapa asc, bins_numero asc")
	dw_9Mirror.Sort()
	dw_binsmirror.Sort()
Else	
	dw_9Mirror		=	dw_granpesa
	dw_binsmirror	=	dw_movtobins
	
	dw_9Mirror.SetSort("mfgp_tibapa asc, bins_numero asc")
	dw_binsmirror.SetSort("mfgp_tibapa asc, bins_numero asc")
	dw_9Mirror.Sort()
	dw_binsmirror.Sort()
End If

If dw_9Mirror.RowCount() > 0 Then
	FOR ll_Fila = 1 TO dw_9Mirror.RowCount()
		
		//INICIO Control de envases
		iuo_bins.Existe(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], dw_9Mirror.Object.bins_numero[ll_Fila], sqlca, TRUE)
		
		lb_flag	=	False
		If UpperBound(ls_enva_tipoen[]) = 0 Then
			ls_enva_tipoen[1]		=	String(iuo_bins.enva_tipoen)
			ls_enva_codigo[1]		=	String(iuo_bins.enva_codigo)
			ls_cale_calida[1]		=	iuo_bins.cale_calida
			ls_cantidad[1]			=	String(dw_9Mirror.Object.fgmb_canbul[ll_Fila])
			ls_pesone[1]			=	String(iuo_bins.cale_pesoen)
			ls_cale_nombre[1]		=	iuo_bins.cale_nombre
			ls_prodenv[1]			=	String(dw_9Mirror.Object.prod_codigo[ll_Fila])			
		Else
			
			FOR li_recenv = LowerBound(ls_enva_tipoen[]) TO UpperBound(ls_enva_tipoen[])
				If ls_enva_tipoen[li_recenv] = String(iuo_bins.enva_tipoen) Then
					If ls_enva_codigo[li_recenv] = String(iuo_bins.enva_codigo) Then
						If ls_cale_calida[li_recenv] = String(iuo_bins.cale_calida) Then
							If ls_prodenv[li_recenv] = String(dw_9Mirror.Object.prod_codigo[ll_Fila]) Then
								ls_cantidad[li_recenv]	=	String(Integer(ls_cantidad[li_recenv]) )//+ 1)
								lb_flag						=	True
								EXIT
							End If
						End If
					End If
				End If
			NEXT
			
			If NOT lb_flag Then
				li_recenv								=	li_recenv + 1
				ls_enva_tipoen[li_recenv]		=	String(iuo_bins.enva_tipoen)
				ls_enva_codigo[li_recenv]		=	String(iuo_bins.enva_codigo)
				ls_cale_calida[li_recenv]			=	iuo_bins.cale_calida
				ls_cantidad[li_recenv]				=	String(dw_9Mirror.Object.fgmb_canbul[ll_Fila])
				ls_pesone[li_recenv]				=	String(iuo_bins.cale_pesoen)
				ls_cale_nombre[li_recenv]		=	iuo_bins.cale_nombre
				ls_prodenv[li_recenv] 			= 	String(dw_9Mirror.Object.prod_codigo[ll_Fila])
			End If
		End If
	NEXT
End If

FOR ll_fila = 1 TO UpperBound(ls_cantidad[])
	ll_totalbultos	=	ll_totalbultos + Long(ls_cantidad[ll_fila])
NEXT
If ll_totalbultos > 0 Then dw_2.Object.mfge_totbul[1]	=	ll_totalbultos

//ESTO ESTABA COMENTADO!!!!
//DESCOMENTADO SOLO PARA PROBAR TRANSFERENCIA DE BASESPALLET
//FOR ll_Fila = 1 TO dw_BinsMirror.RowCount()
//			ll_tibapa[ll_Fila] = dw_BinsMirror.Object.mfgp_tibapa[ll_Fila] 
//NEXT
//If UpperBound(ll_tibapa[]) > 0 Then
//	FOR ll_Fila = 1 TO UpperBound(ll_tibapa[])
//		If ll_tibapa[ll_Fila] > 0 Then
//			//INICIO Control de base pallets
//			iuo_bins.Existe(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], &
//								 ll_tibapa[ll_Fila], sqlca, TRUE)
//			
//			lb_flag	=	False
//			If UpperBound(ls_enva_tipoen[]) = 0 Then
//				ls_enva_tipoen[1]	=	String(iuo_bins.enva_tipoen)
//				ls_enva_codigo[1]	=	String(iuo_bins.enva_codigo)
//				ls_cale_calida[1]	=	iuo_bins.cale_calida
//				ls_cantidad[1]		=	String(ll_cant_tibapa[ll_fila])//"1"
//				ls_pesone[1]		=	String(iuo_bins.cale_pesoen)
//				ls_cale_nombre[1]	=	iuo_bins.cale_nombre
//				ls_prodenv[1] 		= 	ls_prodbaspal[ll_Fila]
//				
//			Else
//				FOR li_recenv = LowerBound(ls_enva_tipoen[]) TO UpperBound(ls_enva_tipoen[])
//					If ls_enva_tipoen[li_recenv] = String(iuo_bins.enva_tipoen) Then
//						If ls_enva_codigo[li_recenv] = String(iuo_bins.enva_codigo) Then
//							If ls_cale_calida[li_recenv] = String(iuo_bins.cale_calida) Then
//								If ls_prodenv[li_recenv] = ls_prodbaspal[ll_Fila] Then
//								
//									ls_cantidad[li_recenv]	=	String(Integer(ls_cantidad[li_recenv]) + ll_cant_tibapa[ll_fila])
//									lb_flag						=	True
//									EXIT
//								End If
//							End If
//						End If
//					End If
//				NEXT
//				
//				If NOT lb_flag Then
//					li_recenv							=	li_recenv + 1
//					ls_enva_tipoen[li_recenv]		=	String(iuo_bins.enva_tipoen)
//					ls_enva_codigo[li_recenv]		=	String(iuo_bins.enva_codigo)
//					ls_cale_calida[li_recenv]		=	iuo_bins.cale_calida
//					ls_cantidad[li_recenv]			=	'1'//String(ll_cant_tibapa[ll_fila])
//					ls_pesone[li_recenv]				=	String(iuo_bins.cale_pesoen)
//					ls_cale_nombre[li_recenv]		=	iuo_bins.cale_nombre
//					ls_prodenv[li_recenv] 			= 	ls_prodbaspal[ll_Fila]
//				End If
//				
//			End If
//			//FIN Control de envases
//			
//		End If
//	NEXT
//End If
// Hasta Aca 

//Asignación de envases recepcionados
is_enva_tipoen[]	=	ls_enva_tipoen[]
is_enva_codigo[]	=	ls_enva_codigo[]
is_cale_calida[]		=	ls_cale_calida[]
is_cantidad[]		=	ls_cantidad[]
is_pesone[]			=	ls_pesone[]
is_cale_nombre[]	=	ls_cale_nombre[]
is_prodenv[]		=	ls_prodenv[]

CargaMovEnv()

Return True
end function

public subroutine determina_productoresenvase (integer ai_tipomovto);str_Productores_Envases	lstr_Prod_Enva
DataWindow	ldw_envase
Long			ll_Fila, ll_GuiaSII, ll_Fila_Lote
Integer		li_Secuencia, li_Cliente, li_ClieAnt
Long			ll_Productor,ll_ProdAnt

//Inicializa la Estructura de Instancia
wstr_Prod_Enva	= lstr_Prod_Enva

ldw_envase	=	dw_4

FOR ll_Fila	=	1 TO ldw_Envase.RowCount()
	ll_Productor	=	ldw_Envase.Object.prod_codigo[ll_Fila]
   li_Cliente		=	ldw_Envase.Object.clie_codigo[ll_Fila]
	
	IF (ll_Productor <> ll_ProdAnt) THEN
		ll_ProdAnt	=	ll_Productor
		li_ClieAnt	=	li_Cliente
		li_Secuencia ++
		
		IF ai_TipoMovto = 41 THEN
			ll_Fila_Lote	=	dw_3.Find(" prod_codigo = "+String(ll_Productor),1,dw_3.RowCount())
			
			IF ll_Fila_Lote > 0 THEN
				ll_GuiaSII	=	dw_3.Object.lote_guisii[ll_Fila_Lote]
			ELSE
				ll_GuiaSII	=	0
			END IF
		END IF
		
		wstr_Prod_Enva.GuiaSII[li_Secuencia]	=	ll_GuiaSII
		wstr_Prod_Enva.Cliente[li_Secuencia]	=	li_Cliente
		wstr_Prod_Enva.Productor[li_Secuencia]	=	ll_Productor
		wstr_Prod_Enva.TipoMovto[li_Secuencia]	=	ai_TipoMovto
	END IF
NEXT

RETURN
end subroutine

public function boolean conexionexistencia_2 ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta

//IF ib_ConectadoExistencia2 THEN
//	DISCONNECT USING sqlexi2;
//END IF
//
//ls_Usuario				=	sqlca.UserId
//ls_Password				=	sqlca.DbPass
//
//SELECT 	cone_nomodb,	cone_nomser,	cone_nombas,
//			cone_nodbms,	cone_nomusu,	cone_passwo  
//	INTO 	:ls_nomodb,		:ls_nomser,		:ls_nombas,
//			:ls_nodbms,		:ls_Usuario,	:ls_Password
//	FROM dbo.prodconectividad   
//	WHERE cone_codigo = :il_coneccion2;
//
//sqlexi2.ServerName	=	ls_nomser
//sqlexi2.DataBase	=	ls_nombas
//sqlexi2.Dbms			= 	ls_nodbms
//sqlexi2.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
//								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
//CONNECT USING sqlexi2;
//
//IF sqlexi2.SQLCode = 0 THEN
//	ib_ConectadoExistencia2	=	True
//ELSE
//	ib_ConectadoExistencia2	=	False
//END IF

RETURN ib_ConectadoExistencia2

end function

public subroutine cargamovenv ();Integer			li_rectipenv, li_filadw_5
uo_productores	iuo_prod

iuo_prod			=	Create uo_productores

IF dw_3.RowCount() < 1 THEN RETURN

dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, dw_4.RowCount(), Delete!)

FOR li_rectipenv = LowerBound(is_enva_tipoen) TO UpperBound(is_enva_tipoen)
	
	IF Integer(is_enva_tipoen[li_rectipenv]) > 0 THEN
		
		li_filadw_5 									= 	dw_4.InsertRow(0)
		dw_4.Object.prod_codigo[li_filadw_5]	=	Long(is_prodenv[li_rectipenv])
		
		iuo_prod.Existe(Long(is_prodenv[li_rectipenv]), True, SQLCa)
		
		dw_4.Object.prod_nombre[li_filadw_5]	=	iuo_prod.nombre
		dw_4.Object.plde_codigo[li_filadw_5]		=	dw_2.Object.plde_codigo[1]
		dw_4.Object.enva_tipoen[li_filadw_5]	=	Integer(is_enva_tipoen[li_rectipenv])
		dw_4.Object.enva_codigo[li_filadw_5]	=	Integer(is_enva_codigo[li_rectipenv])
		dw_4.Object.cale_calida[li_filadw_5]		=	is_cale_calida[li_rectipenv]
		dw_4.Object.fgme_cantid[li_filadw_5]	=	Integer(is_cantidad[li_rectipenv])
		dw_4.Object.fgme_pesone[li_filadw_5]	=	Dec(is_pesone[li_rectipenv])
		dw_4.Object.fgme_conenv[li_filadw_5]	=	1
		dw_4.Object.clie_codigo[li_filadw_5]		=	dw_2.Object.clie_codigo[1]
		dw_4.Object.cale_nombre[li_filadw_5]	=	is_cale_nombre[li_rectipenv]
		dw_4.Object.fgme_sentid[li_filadw_5]	=	Integer(istr_Mant.Argumento[4])

	END IF
NEXT
end subroutine

public function boolean datos_correo ();Integer li_codigo

SELECT 	empr_codplt
INTO		:li_codigo
FROM 		dbo.parempresa;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla parempresa")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

SELECT 	isnull(plde_correo,'')
INTO		:is_correo
FROM 		dbo.plantadesp
WHERE 	plde_codigo = :li_codigo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla parempresa")
	Return True
ELSEIF is_correo = '' THEN
	MessageBox("Atención","Falta Correo en Tabla Planta.")
	Return True
END IF

Return False
end function

event open;/* 
	Argumentos
	istr_Mant.Argumento[1]	=	Código Planta
	istr_Mant.Argumento[2]	=	Tipo de Movimiento
	istr_Mant.Argumento[3]	=	Número de Despacho
	istr_Mant.Argumento[4]	=	Sentido del Movimiento => 2 = Despacho
	istr_Mant.Argumento[7]	=	Envases con Fruta Solamente.
	istr_Mant.Argumento[6]	=	Código Productor
	lstr_Mant.Argumento[8]  =  Nombre Productor
	istr_Mant.Argumento[16]	=	Código Cliente
*/

DataWindowChild	ldwc_Camara, ldwc_TipoEnvase, ldwc_PltaLote, ldwc_Especie

x												= 0
y												= 0
This.Height									= 2500
im_menu										= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

dw_3	=	tab_1.tp_1.dw_detalle
dw_4	=	tab_1.tp_2.dw_envases

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	Message.StringParm
istr_Mant.Argumento[3]	=	""
istr_Mant.Argumento[4]	=	"2"
istr_Mant.Argumento[7]	=	"2"

If istr_Mant.Argumento[2] = "22" Then
	This.Title	=	"DESPACHO INTER PLANTA"
	dw_2.Object.mfge_guisii.Protect	=	1
	dw_2.Object.mfge_guisii.Color		=	RGB(255,255,255)
	dw_2.Object.mfge_guisii.BackGround.Color	=	553648127
ElseIf istr_Mant.Argumento[2] = "23" Then
	This.Title	=	"DEVOLUCION A PRODUCTOR"
Else
	This.Title	=	"TRASPASO PLANTA VIRTUAL"
End If

dw_1.GetChild("plde_codigo", idwc_Pltadw1)
idwc_Pltadw1.SetTransObject(sqlca)
idwc_Pltadw1.Retrieve(gi_codexport)

dw_1.GetChild("plde_coorde", idwc_PltaODdw1)
idwc_PltaODdw1.SetTransObject(sqlca)
idwc_PltaODdw1.Retrieve(gi_codexport)

//Cliente
dw_2.GetChild("clie_codigo", idwc_Cliente)
idwc_Cliente.SetTransObject(sqlca)
idwc_Cliente.Retrieve() 
idwc_Cliente.InsertRow(0)

dw_2.GetChild("plde_codigo", idwc_PltaOrig)
idwc_PltaOrig.SetTransObject(sqlca)

If idwc_PltaOrig.Retrieve(gi_codexport) = 0 Then
	MessageBox("Atención", "Falta Registrar Plantas")
	idwc_PltaOrig.InsertRow(0)
Else
	idwc_PltaOrig.SetSort("plde_nombre A")
	idwc_PltaDest.Sort()
End If

dw_2.GetChild("plde_coorde", idwc_PltaDest)
idwc_PltaDest.SetTransObject(sqlca)

If idwc_PltaDest.Retrieve(gi_codexport) = 0 Then
	MessageBox("Atención", "Falta Registrar Plantas")
	idwc_PltaDest.InsertRow(0)
Else
	idwc_PltaDest.SetFilter("plde_codigo <> " + String(gstr_ParamPlanta.CodigoPlanta))
	idwc_PltaDest.Filter()
	idwc_PltaDest.SetSort("plde_nombre A")
	idwc_PltaDest.Sort()
End If

dw_2.GetChild("moti_codigo", idwc_Motivo)
idwc_Motivo.SetTransObject(sqlca)

If idwc_Motivo.Retrieve() = 0 Then
	MessageBox("Atención", "Falta Registrar Motivos de Despacho")
	idwc_Motivo.InsertRow(0)
Else
	idwc_Motivo.SetSort("moti_nombre A")
	idwc_Motivo.Sort()
End If

dw_2.GetChild("tran_codigo", idwc_Transp)
idwc_Transp.SetTransObject(sqlca)

If idwc_Transp.Retrieve() = 0 Then
	MessageBox("Atención", "Falta Registrar Transportístas")
	idwc_Transp.InsertRow(0)
Else
	idwc_Transp.SetFilter("tran_tiptra <> 2")
	idwc_Transp.Filter()
	idwc_Transp.SetSort("tran_nombre A")
	idwc_Transp.Sort()
End If

dw_2.GetChild("cami_patent", idwc_Camion)
idwc_Camion.SetTransObject(sqlca)

If idwc_Camion.Retrieve(2) = 0 Then
	MessageBox("Atención", "Falta Registrar Camiones con Flete Propio")
	idwc_Camion.InsertRow(0)
Else
	idwc_Camion.SetSort("cami_codigo A")
	idwc_Camion.Sort()
End If

dw_3.GetChild("cama_codigo", ldwc_Camara)
ldwc_Camara.SetTransObject(sqlca)

If ldwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 Then
	MessageBox("Atención", "Falta Registrar Cámaras para Planta " + &
					String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	ldwc_Camara.InsertRow(0)
Else
	ldwc_Camara.SetSort("cama_nombre A")
	ldwc_Camara.Sort()
End If

dw_3.GetChild("lote_pltcod", ldwc_PltaLote)
ldwc_PltaLote.SetTransObject(sqlca)
ldwc_PltaLote.Retrieve(gi_codexport)

dw_3.GetChild("lote_espcod", ldwc_Especie)
ldwc_Especie.SetTransObject(sqlca)
ldwc_Especie.Retrieve(gstr_parempresa.empr_codexp)

dw_4.GetChild("enva_tipoen", ldwc_TipoEnvase)
ldwc_TipoEnvase.SetTransObject(sqlca)

If ldwc_TipoEnvase.Retrieve() = 0 Then
	MessageBox("Atención", "Falta Registrar Envases ")
	ldwc_TipoEnvase.InsertRow(0)
Else
	ldwc_TipoEnvase.SetSort("tien_nombre A")
	ldwc_TipoEnvase.Sort()
End If

dw_4.GetChild("cama_codigo", ldwc_Camara)
ldwc_Camara.SetTransObject(sqlca)

If ldwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 Then
	MessageBox("Atención", "Falta Registrar Cámaras para Planta " + String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	ldwc_Camara.InsertRow(0)
Else
	ldwc_Camara.SetSort("cama_nombre A")
	ldwc_Camara.Sort()	
End If

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_granpesa.SetTransObject(sqlca)

dw_3.ModIfy("datawindow.message.title='Error '+ is_titulo")
dw_3.SetRowFocusIndicator(Hand!)
dw_3.ModIfy("DataWindow.Footer.Height = 88")

dw_4.ModIfy("datawindow.message.title='Error '+ is_titulo")
dw_4.SetRowFocusIndicator(Hand!)
dw_4.ModIfy("DataWindow.Footer.Height = 88")

istr_Mant.dw				=	dw_1
istr_Mant.solo_consulta =	False

pb_nuevo.PostEvent(Clicked!)

iuo_cliente       				=	Create uo_cliente
iuo_Productores			=	Create uo_productores
iuo_PltaDestino				=	Create uo_plantadesp
iuo_MotiDespacho			=	Create uo_motivodespacho
iuo_Transport				=	Create uo_transportista
iuo_Camion					=	Create uo_camiones
iuo_TipoDocto				=	Create uo_tipodoctoplanta
iuo_FechaMovto			=	Create uo_FechaMovto

iuo_tipomovtofruta			=	Create uo_tipomovtofruta
iuo_TipoMovtoEnva		=	Create uo_tipomovtofruta
iuo_traspaso				=	Create nvuo_traspaso_interplanta
iuo_calicosechero			=  Create uo_calicosechero  
iuo_bins						=	Create uo_bins
iuo_Mail						=	Create uo_Mail
end event

event ue_borra_detalle;call super::ue_borra_detalle;SetPointer(HourGlass!)

ib_Borrar				=	True
Message.DoubleParm	=	0
istr_mant.Borra		=	True
istr_mant.Agrega		=	False

w_main.SetMicroHelp("Validando la eliminación de detalle...")

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF tab_1.SelectedTab = 1 THEN
	istr_Mant.dw	=	dw_3
	istr_mant.dw2	=	dw_5
	
	OpenWithParm(iw_mantencion_1, istr_Mant)
	
	istr_Mant	=	Message.PowerObjectParm
	
	IF istr_mant.Respuesta = 1 THEN
		IF dw_3.DeleteRow(0) = 1 THEN
			ib_Borrar	=	False
			
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_Borrar	=	False
			
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
		IF dw_3.RowCount() = 0 THEN 
			HabilitaEncab(True)
			
			pb_eli_det.Enabled			=	False
			dw_2.Object.mfge_totbul[1]	=	0
			dw_2.Object.mfge_tpneto[1]	=	0
		ELSE
			dw_2.Object.mfge_totbul[1]	=	Round(dw_3.Object.total_bultos[1], 0)
			dw_2.Object.mfge_tpneto[1]	=	Round(dw_3.Object.total_kilos[1], 3)
		END IF
	END IF
ELSE
	istr_Mant.dw	=	dw_4
	
	OpenWithParm(iw_mantencion_2, istr_Mant)
	
	istr_Mant	=	Message.PowerObjectParm
	
	IF istr_mant.Respuesta = 1 THEN
		IF dw_4.DeleteRow(0) = 1 THEN
			ib_Borrar	=	False
			
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_Borrar	=	False
			
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
		IF dw_4.RowCount() = 0 THEN 
			HabilitaEncab(True)
			pb_eli_det.Enabled	=	False
		END IF
	END IF
END IF

istr_mant.Borra	 = False
end event

event ue_nuevo_detalle;istr_mant.Borra	=	False
istr_mant.Agrega	=	True

Habilitaencab(FALSE)

If tab_1.SelectedTab = 1 Then
	istr_mant.dw	=	dw_3
	istr_mant.dw2	=	dw_5
	
	OpenWithParm(iw_mantencion_1, istr_mant)

	If istr_mant.respuesta = 1 Then
		If dw_3.RowCount() > 0 Then
			dw_2.Object.mfge_totbul[1]		=	Round(dw_3.Object.total_bultos[1], 0)
			dw_2.Object.mfge_tpneto[1]	=	Round(dw_3.Object.total_kilos[1], 3)
			pb_eli_det.Enabled				=	True
		Else
			dw_2.Object.mfge_totbul[1]		=	0
			dw_2.Object.mfge_tpneto[1]	=	0
			pb_eli_det.Enabled				=	False
		End If
	End If
	CargaEnvases()
Else
	istr_mant.dw	=	dw_4

	OpenWithParm(iw_mantencion_2, istr_mant)

	If dw_4.RowCount() > 0 Then
		pb_eli_det.Enabled			=	True
	Else
		pb_eli_det.Enabled			=	False
	End If
End If

If dw_3.RowCount() > 0 AND dw_4.RowCount() > 0 Then HabilitaEncab(False)

If dw_3.RowCount() > 0 AND dw_4.RowCount() > 0 AND Not pb_eliminar.Enabled Then
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
End If

If tab_1.SelectedTab = 1 Then
	dw_3.SetRow(il_Fila)
	dw_3.SelectRow(il_Fila, True)
Else
	dw_4.SetRow(il_Fila)
	dw_4.SelectRow(il_Fila, True)
End If
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta

DO
	dw_2.SetRedraw(False)

	ll_fila_e	=	dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), &
											Long(istr_Mant.Argumento[3]), dw_2.Object.clie_codigo[1])
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			IF dw_2.RowCount() > 0 AND Integer(istr_Mant.Argumento[2]) = 23 THEN
				istr_mant.argumento[6]	= String(dw_2.Object.prod_codigo[1])
				istr_mant.argumento[8]	= String(dw_2.Object.prod_nombre[1])
			END IF	
			
			IF dw_3.Retrieve(Integer(istr_mant.argumento[1]),  Integer(istr_mant.argumento[2]), &
									Long(istr_mant.argumento[3]), dw_2.Object.clie_codigo[1]) = -1 OR &
				dw_1.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]), &
									Long(istr_mant.argumento[3]),1, dw_2.Object.clie_codigo[1]) = -1 OR &
				dw_4.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]), &
									Long(istr_mant.argumento[3]), 2, dw_2.Object.clie_codigo[1]) = -1 OR &
				dw_5.Retrieve(dw_2.Object.clie_codigo[1], 	  Integer(istr_Mant.Argumento[1]), &
									Integer(istr_Mant.Argumento[2]),Long(istr_Mant.Argumento[3])) = -1 Then
				respuesta = MessageBox(	"Error en Base de Datos", &
												"No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				tab_1.tp_1.Enabled		=	True
				tab_1.tp_2.Enabled		=	True
				pb_eliminar.Enabled		=	True
				pb_grabar.Enabled			=	True
				pb_imprimir.Enabled		=	True
				cb_guia.Enabled 			=	True
				pb_ins_det.Enabled		=	Not istr_mant.solo_consulta
				
				IF dw_2.Object.tpmv_codigo[1] = 30 THEN istr_mant.solo_consulta = True

				HabilitaEncab(False)				
				pb_eli_det.Enabled	=	Not istr_mant.solo_consulta
				
				dw_1.SetRow(1)
				dw_1.SelectRow(1,True)
				dw_1.SetFocus()
			End If
		Loop While respuesta = 1

		If respuesta = 2 Then Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_maed_movtofrutagranel_despacho.create
int iCurrent
call super::create
this.tab_1=create tab_1
this.cb_guia=create cb_guia
this.dw_exideta=create dw_exideta
this.dw_exiencab=create dw_exiencab
this.dw_granpesa=create dw_granpesa
this.dw_error=create dw_error
this.dw_exismovtodetanulos=create dw_exismovtodetanulos
this.dw_exidetaborra=create dw_exidetaborra
this.dw_movtobins=create dw_movtobins
this.dw_5=create dw_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
this.Control[iCurrent+2]=this.cb_guia
this.Control[iCurrent+3]=this.dw_exideta
this.Control[iCurrent+4]=this.dw_exiencab
this.Control[iCurrent+5]=this.dw_granpesa
this.Control[iCurrent+6]=this.dw_error
this.Control[iCurrent+7]=this.dw_exismovtodetanulos
this.Control[iCurrent+8]=this.dw_exidetaborra
this.Control[iCurrent+9]=this.dw_movtobins
this.Control[iCurrent+10]=this.dw_5
end on

on w_maed_movtofrutagranel_despacho.destroy
call super::destroy
destroy(this.tab_1)
destroy(this.cb_guia)
destroy(this.dw_exideta)
destroy(this.dw_exiencab)
destroy(this.dw_granpesa)
destroy(this.dw_error)
destroy(this.dw_exismovtodetanulos)
destroy(this.dw_exidetaborra)
destroy(this.dw_movtobins)
destroy(this.dw_5)
end on

event ue_modifica_detalle;istr_mant.agrega	=	False
istr_mant.borra	=	False

IF tab_1.SelectedTab = 1 THEN
	IF dw_3.RowCount() > 0 THEN
		istr_mant.dw		=	dw_3
		istr_mant.dw2		=	dw_5
		
		OpenWithParm(iw_mantencion_1, istr_mant)
		
		IF istr_mant.respuesta = 1 THEN
			dw_2.Object.mfge_totbul[1]	=	Round(dw_3.Object.total_bultos[1], 0)
			dw_2.Object.mfge_tpneto[1]	=	Round(dw_3.Object.total_kilos[1], 3)
		END IF
	END IF
	CargaEnvases()
ELSE
	IF dw_4.RowCount() > 0 THEN
		istr_mant.dw		=	dw_4
		
		OpenWithParm(iw_mantencion_2, istr_mant)
	END IF
END IF
end event

event ue_nuevo;is_rut = ""

Call Super::ue_nuevo

dw_3.Reset()
dw_4.Reset()
dw_5.Reset()

dw_2.Object.clie_codigo[1]		=  gi_codexport
dw_2.Object.plde_codigo[1]		=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.tpmv_codigo[1]	=	Integer(istr_Mant.Argumento[2])
istr_mant.Argumento[16]   		= String (gi_codexport)

idt_FechaSistema					=	F_FechaHora()

dw_2.Object.mfge_fecmov[1]	=	Date(idt_FechaSistema)
dw_2.Object.refg_horasa[1]	=	Time(idt_FechaSistema)
dw_2.Object.defg_tipdoc[1]	=	2  //Guía de Despacho

cb_guia.Enabled  =	False

istr_mant.solo_consulta = FALSE
end event

event ue_antesguardar;Long		ll_Fila, ll_row, ll_Productor, ll_Fila_Busca, li_filaalter, ll_tarja
Integer	li_Secuencia, li_Planta, li_TipoMovto, li_TipoMovtoEnva, li_Cliente
Boolean  lb_Actualiza_Fruta = FALSE, lb_Actualiza_Envase = FALSE
String	ls_lote

If dw_3.RowCount() > 0 Then
	FOR ll_row = 1 TO dw_3.RowCount()
		ls_lote = String(dw_3.Object.lote_pltcod[ll_row],"0000") + &
					 String(dw_3.Object.lote_espcod[ll_row],"00") + &
					 String(dw_3.Object.lote_codigo[ll_row],"0000")
					 
		If Not lotesdestarados(dw_2.Object.clie_codigo[1],ls_lote) Then
			Messagebox("Error de Consistencia","El lote " + ls_lote + " no ha sido destarado" +&
															"~r~rPor favor ingrese otro lote")
			Message.DoubleParm = -1
			Return
		End If
	NEXT
End If

If NOT RevisaEnvases() Then
	Message.DoubleParm = -1
	Return
End If	

If dw_3.RowCount() > 0 Then
	If dw_3.Object.total_bultos[dw_3.RowCount()] > 9999 Then
		Messagebox("Error de Consistencia","Total de bultos supera lo permitido")
		Message.DoubleParm = -1
		Return
	End If 
End If

If dw_4.RowCount() > 0 Then
	If dw_4.Object.total_envases[dw_4.RowCount()] > 9999 Then
		Messagebox("Error de Consistencia","Total de envases supera lo permitido")
		Message.DoubleParm = -1
		Return
	End If
	
	If dw_4.Object.total_pesoenv[dw_4.RowCount()] > 99999 Then
		Messagebox("Error de Consistencia","Total del peso de los envases supera lo permitido")
		Message.DoubleParm = -1
		Return
	End If
End If
	
ib_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

li_Planta			=	dw_2.Object.plde_codigo[1]
li_TipoMovto	=	dw_2.Object.tpmv_codigo[1]
li_Cliente        	=  dw_2.Object.clie_codigo[1]


If li_TipoMovto = 22 OR li_TipoMovto = 30 Then
	li_TipoMovtoEnva = 62
Else
	li_TipoMovtoEnva = 61
End If	

If dw_2.GetItemStatus(1, 0, Primary!) = NewModIfied! Then

 	If il_NumEnva=0 Then
		iuo_TipoMovtoEnva.BloqueaCorrel()	
		il_NumEnva = iuo_TipoMovtoEnva.UltimoCorrelativo(4,li_TipoMovtoEnva,li_Planta) 
	
		If il_NumEnva = 0 OR IsNull(il_NumEnva) Then
			Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
			Message.DoubleParm = -1
			Return
		Else
			lb_Actualiza_Envase = TRUE
		End If
	Else
		il_NumEnva	=	il_NumEnva	+	1
	End If
	
	If il_NumFruta=0 Then
		iuo_TipoMovtoFruta.bloqueacorrel()
		il_NumFruta = iuo_TipoMovtoFruta.UltimoCorrelativo(1,li_TipoMovto,li_Planta) 
	
		If il_NumFruta = 0 OR IsNull(il_NumFruta) Then
			Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
			Message.DoubleParm = -1
			Return
		Else
			lb_Actualiza_Fruta = TRUE	
		End If
	Else
		il_NumFruta	=	il_NumFruta	+	1
   End If

	dw_2.Object.mfge_numero[1]	=	il_NumFruta
	
	If dw_4.RowCount() > 0 Then
		dw_2.Object.prod_codigo[1]	=	dw_4.Object.prod_codigo[1]
	End If
	
	If dw_3.RowCount() > 0 Then
		dw_2.Object.espe_codigo[1]	=	dw_3.Object.lote_espcod[1]
	End If
	
	Determina_ProductoresEnvase(li_TipoMovtoEnva)

	//Resetea Movto de Envase en caso de errores previos en la grabación.
	dw_1.Reset()
	
	FOR ll_Productor = 1 TO UpperBound(wstr_Prod_Enva.Productor)
		ll_Fila	=	dw_1.InsertRow(0)

		dw_1.Object.clie_codigo[ll_Fila]		=	dw_2.Object.clie_codigo[1]
		dw_1.Object.plde_codigo[ll_Fila]		=	dw_2.Object.plde_codigo[1]
		dw_1.Object.tpmv_codigo[ll_Fila]		=	li_TipoMovtoEnva
		dw_1.Object.meen_numero[ll_Fila]	=	il_NumEnva
		dw_1.Object.plde_coorde[ll_Fila]		=	dw_2.Object.plde_coorde[1]
		dw_1.Object.prod_codigo[ll_Fila]		=	Long(wstr_Prod_Enva.Productor[ll_Productor])
		
		dw_1.Object.tpmv_codrec[ll_Fila]		=	dw_2.Object.tpmv_codigo[1]
		dw_1.Object.mfge_numero[ll_Fila]	=	il_NumFruta
		dw_1.Object.meen_modulo[ll_Fila]	=	1
		
		dw_1.Object.tran_codigo[ll_Fila]	=	dw_2.Object.tran_codigo[1]
		dw_1.Object.cami_clasIfi[ll_Fila]=	dw_2.Object.cami_clasIfi[1]
		dw_1.Object.cami_patent[ll_Fila]	=	dw_2.Object.cami_patent[1]
		dw_1.Object.cami_patcar[ll_Fila]	=	dw_2.Object.cami_patcar[1]
		dw_1.Object.meen_rutcho[ll_Fila]	=	dw_2.Object.mfge_rutcho[1]
		dw_1.Object.meen_chofer[ll_Fila]	=	dw_2.Object.mfge_chofer[1]
		
		dw_1.Object.meen_guisii[ll_Fila]		=	dw_2.Object.mfge_guisii[1]//wstr_Prod_Enva.GuiaSII[ll_Productor]
		dw_1.Object.meen_fecmov[ll_Fila]	=	dw_2.Object.mfge_fecmov[1]
		
		il_NumEnva ++
	NEXT
	il_NumEnva --
	
   //Preguntar el Momento de Actualización
	If lb_Actualiza_Fruta  Then iuo_TipoMovtoFruta.Actualiza_Correlativo(1,li_Planta,li_TipoMovto,il_NumFruta) 
   If lb_Actualiza_Envase Then iuo_TipoMovtoEnva.Actualiza_Correlativo(4,li_Planta,li_TipoMovtoEnva,il_NumEnva) 
   ///////////////////////////////////////
	
Else
   il_NumFruta						=	dw_2.Object.mfge_numero[1]	
End If

istr_mant.Argumento[3]			=	String(dw_2.Object.mfge_numero[1])

If dw_3.RowCount() > 0 Then
	dw_2.Object.mfge_totbul[1] = 	dw_3.Object.total_bultos[dw_3.RowCount()]
	dw_2.Object.mfge_tpneto[1] = 	dw_3.Object.total_kilos[dw_3.RowCount()]
Else
	dw_2.Object.mfge_totbul[1] = 	0
	dw_2.Object.mfge_tpneto[1] = 	0
End If	
	
SELECT IsNull(Max(mfgd_secuen), 0) + 1
  INTO :li_Secuencia
  FROM dbo.spro_movtofrutagrandeta 
 WHERE plde_codigo	=	:li_Planta
   AND tpmv_codigo	=	:li_TipoMovto
   AND mfge_numero	=	:il_NumFruta
   AND clie_codigo =  :li_cliente;
	
FOR ll_Fila = 1 TO dw_3.RowCount()
	If dw_3.GetItemStatus(ll_Fila, 0, Primary!) = NewModIfied! Then
		dw_3.Object.clie_codigo[ll_fila] =  dw_2.Object.clie_codigo[1]
		dw_3.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_3.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
		dw_3.Object.mfge_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]
		dw_3.Object.mfgd_secuen[ll_Fila]	=	li_Secuencia
		
		li_Secuencia ++
	End If
NEXT
dw_5.SetFilter("")
dw_5.Filter()

li_Secuencia	=	1

FOR li_filaalter = 1 TO dw_5.RowCount()
	If dw_5.GetItemStatus(li_filaalter, 0, Primary!) = NewModIfied! Then
		ll_Fila											=	dw_3.find("lote_codigo = " + String(dw_5.Object.lote_codigo[li_filaalter]), 1, dw_3.RowCount())
		If ll_Fila > 0 Then
			dw_5.Object.clie_codigo[li_filaalter]	=  dw_2.Object.clie_codigo[1]
			dw_5.Object.plde_codigo[li_filaalter]	=	dw_2.Object.plde_codigo[1]
			dw_5.Object.tpmv_codigo[li_filaalter]	=	dw_2.Object.tpmv_codigo[1]
			dw_5.Object.mfge_numero[li_filaalter]	=	dw_2.Object.mfge_numero[1]
			dw_5.Object.mfgd_secuen[li_filaalter]	=	dw_3.Object.mfgd_secuen[ll_Fila]
		Else
			dw_5.Object.clie_codigo[li_filaalter]	=  dw_2.Object.clie_codigo[1]
			dw_5.Object.plde_codigo[li_filaalter]	=	dw_2.Object.plde_codigo[1]
			dw_5.Object.tpmv_codigo[li_filaalter]	=	dw_2.Object.tpmv_codigo[1]
			dw_5.Object.mfge_numero[li_filaalter]	=	dw_2.Object.mfge_numero[1]
			dw_5.Object.mfgd_secuen[li_filaalter]	=	li_Secuencia
			li_Secuencia ++
		End If
	End If
NEXT

FOR ll_Fila = 1 TO dw_4.RowCount()
	If dw_4.GetItemStatus(ll_Fila, 0, Primary!) = NewModIfied! Then
		
		ll_Productor	=	dw_4.Object.prod_codigo[ll_Fila]
		
		ll_Fila_Busca	=	dw_1.Find("plde_codigo	= "	+	String(li_Planta) 			+ " and " + &
											 "tpmv_codigo 	= "	+	String(li_TipoMovtoEnva) 	+ " and " + &
											 "clie_codigo 	= "	+	String(li_Cliente) 			+ " and " + &											 
											 "prod_codigo 	= "	+	String(ll_Productor),1,dw_4.RowCount())
		
		If ll_Fila_Busca > 0 Then						 
			dw_4.Object.plde_codigo[ll_Fila]		=	dw_1.Object.plde_codigo[ll_Fila_Busca]
			dw_4.Object.tpmv_codigo[ll_Fila]		=	li_TipoMovtoEnva
			dw_4.Object.meen_numero[ll_Fila]		=	dw_1.Object.meen_numero[ll_Fila_Busca]
			dw_4.Object.clie_codigo[ll_Fila]		=	li_Cliente
		End If
	End If
NEXT

cb_guia.Enabled  =	True
end event

event ue_borrar;Integer		li_Cliente
IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar		=	True
ib_AutoCommit	=	sqlca.AutoCommit

w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount() > 0 THEN
	dw_3.RowsMove(1, dw_3.RowCount(), Primary!, dw_3, 1, Delete!)
END IF

IF dw_4.RowCount() > 0 THEN
	dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
END IF

IF dw_5.RowCount() > 0 THEN
	dw_5.RowsMove(1, dw_5.RowCount(), Primary!, dw_5, 1, Delete!)
END IF

dw_2.Object.mfge_estmov[1] = 4

IF dw_1.DeleteRow(0) = 1 THEN
	ib_Borrar	=	False
	w_main.SetMicroHelp("Borrando Registro...")
		
	IF wf_actualiza_db(True) THEN
		w_main.SetMicroHelp("Registro Borrado...")
		
		//Crea conexion hacia existencia
		li_Cliente	=	dw_2.Object.clie_codigo[1]
		sqlexi		=	CREATE Transaction
		
		SELECT 	clie_conexi, cone_codigo
		INTO   	:il_conexiste, :il_coneccion
		FROM dbo.clientesprod
		WHERE clie_codigo = :li_Cliente; 
		
//		IF il_conexiste = 1 THEN
//			IF Conexionexistencia() THEN
//				dw_exideta.SetTransObject(sqlexi)
//				dw_exiencab.SetTransObject(sqlexi)	
//				dw_exismovtodetanulos.SetTransObject(sqlexi)
//				dw_exidetaborra.SetTransObject(sqlexi)
//				TriggerEvent("ue_despuesborrar")
//			ELSE
//				MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)
//			END IF
//		END IF	
		
		
		MessageBox("Eliminación Exitosa", "Eliminación realizada con exito", Exclamation!)
		SetPointer(Arrow!)
	ELSE
		w_main.SetMicroHelp("Registro no Borrado...")
	END IF
ELSE
	ib_Borrar	=	False

	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF
end event

event resize;call super::resize;
tab_1.x								=	dw_1.x
tab_1.y								=	dw_1.y
tab_1.Width							=	dw_1.Width
tab_1.Height						=	dw_1.Height

tab_1.tp_1.dw_detalle.x			= 27
tab_1.tp_1.dw_detalle.y			= 36
tab_1.tp_1.dw_detalle.height	= tab_1.height - 180
tab_1.tp_1.dw_detalle.width	= tab_1.width - 92

tab_1.tp_2.dw_envases.x		= 27
tab_1.tp_2.dw_envases.y		= 36
tab_1.tp_2.dw_envases.height	= tab_1.height - 180
tab_1.tp_2.dw_envases.width	= tab_1.width - 92

cb_guia.x 							=  pb_salir.x
cb_guia.y 							=  pb_salir.y + pb_salir.Height
end event

event ue_seleccion;call super::ue_seleccion;Str_Busqueda	lstr_busq
Date 				ld_FechaInicio

lstr_Busq.Argum[1]	=	istr_Mant.Argumento[1]
lstr_Busq.Argum[2]	=	istr_Mant.Argumento[2]
lstr_Busq.Argum[3]	=	'3'
lstr_Busq.Argum[4]	=	String(ld_FechaInicio)
lstr_Busq.Argum[10]	=	String(dw_2.Object.clie_codigo[1])

OpenWithParm(w_busc_spro_movtofrutagranenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	istr_Mant.Argumento[1]	=	lstr_Busq.Argum[1]
	istr_Mant.Argumento[2]	=	lstr_Busq.Argum[2]
	istr_Mant.Argumento[3]	=	lstr_Busq.Argum[3]
	
	This.TriggerEvent("ue_recuperadatos")
END IF
end event

event ue_guardar;Integer	li_Cliente
String	ls_numero
Date		ld_fecha

If il_NumFruta > 0 Then
	il_NumFruta = 0
	il_NumEnva	= 0
End If

If dw_1.AcceptText() = -1 Then Return

SetPointer(HourGlass!)
w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0
TriggerEvent("ue_antesguardar")
If Message.DoubleParm = -1 Then Return

ls_numero = String(dw_2.Object.mfge_numero[1])
ld_fecha = dw_2.Object.mfge_fecmov[1]

If wf_actualiza_db(False) Then
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled			= 	True
	pb_imprimir.Enabled			= 	True
	
	li_Cliente = dw_2.Object.clie_codigo[1]
		
	SELECT clie_conexi, cone_codigo
	INTO :il_conexiste, :il_coneccion
	FROM dbo.clientesprod
	WHERE clie_codigo = :li_Cliente;
	
//	If il_conexiste = 1 AND (istr_Mant.Argumento[2] = "22" OR istr_Mant.Argumento[2] = "23") Then
//		Datos_correo()
//				
//		sqlexi							=	CREATE Transaction
//		iuo_grabatablabitacora	=	Create uo_grabatablabitacora
//		
//		If Conexionexistencia() Then
//			dw_exideta.SetTransObject(sqlexi)
//			dw_exiencab.SetTransObject(sqlexi)
//			dw_exismovtodetanulos.SetTransObject(sqlexi)
//			dw_exidetaborra.SetTransObject(sqlexi)
//			
//			TriggerEvent("ue_despuesborrar")
//			TriggerEvent("ue_despuesguardar")
//			
//			Disconnect Using sqlexi;
//			
//			If li_retorno = 2 Then
//				iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
//																'VerIficar Datos en Base de Existencia '+is_base,is_base,1,is_error,1,il_coderror,&
//																is_correo,'Problema Control de Envases Despacho Interplanta Granel '+is_base,'Movimiento N° '+ls_numero+' Mantenedor '+&
//																This.Title+' con problemas, usuario '+gstr_us.Nombre+' Computador '+gstr_us.computador +', Error '+is_error)
//				li_retorno = 0	
//			Else	
//				iuo_grabatablabitacora.actualizaestado(ls_numero,ld_fecha,This.Title)	
//			End If
//		Else
//			MessageBox("Atención", "No puede Despachar.~r~rFalló Conexion con Existencia.", Exclamation!, Ok!)
//			
//			iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
//													'VerIficar Conexión a Base '+is_base,is_base,1,'No Se Pudo Conectar el Sistema a Base de Datos de Existencia',1,il_coderror,&
//													is_correo,'Problema Control de Envases Despacho Interplanta Granel '+is_base,'Movimiento N° '+ls_numero+' Con problemas, Error en Conexión con Base de Existencia '+&
//													' Mantenedor '+This.Title+' con problemas, usuario '+gstr_us.Nombre+' Computador '+gstr_us.computador +', Error '+is_error)
//			MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)
//			Return 
//		End If
//	End If
Else
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	Return
End If

If Long(dw_2.Object.mfge_guisii[1]) > 0 Then
	iuo_traspaso.ii_planta 		= 	dw_2.Object.plde_codigo[1]
	iuo_traspaso.ii_tipo			=	dw_2.Object.tpmv_codigo[1]
	iuo_traspaso.ii_movto			=	dw_2.Object.mfge_numero[1]
	iuo_traspaso.ii_cliente		=	dw_2.Object.clie_codigo[1]
	iuo_traspaso.ii_plantades 	=	dw_2.Object.plde_coorde[1]
	iuo_traspaso.ii_sentido 		= 	1
	iuo_traspaso.ii_ManAut		=	1
	iuo_traspaso.il_guiasii		=	dw_2.Object.mfge_guisii[1]
	
	iuo_traspaso.TriggerEvent("ue_traspasadatos")
End If
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "ANEXO GUIA DE DESPACHO"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_movtofrutagranel_despacho"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]), &
                          long(istr_mant.argumento[3]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutagranel_despacho
boolean visible = false
integer x = 41
integer y = 888
integer width = 3223
integer height = 164
boolean titlebar = false
string title = ""
string dataobject = "dw_mant_movtoenvaenca_recepfruta"
boolean hscrollbar = false
boolean vscrollbar = false
boolean border = true
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutagranel_despacho
integer x = 78
integer y = 36
integer width = 3077
integer height = 856
integer taborder = 10
string dataobject = "dw_mant_movtofrutagranel_despacho"
end type

event dw_2::itemchanged;call super::itemchanged;String	ls_Nula
String	ls_Columna
Date		ld_Fecha
Integer	li_cliente

ls_Columna	=	dwo.Name
SetNull(ls_Nula)

CHOOSE Case ls_Columna
	Case "clie_codigo"
		If NOT iuo_cliente.Existe(Integer(Data), TRUE, sqlca) Then
			This.SetItem(1,"clie_codigo",Integer(ls_Nula))
			This.SetFocus()
			Return 1
		Else
			istr_mant.Argumento[16] = Data
		End If	
		
	Case "mfge_numero"
		If NOT ExisteDespacho(dw_2.Object.clie_codigo[1],gstr_ParamPlanta.CodigoPlanta, Integer(istr_mant.argumento[2]), Integer(data)) Then
			This.SetItem(1,"mfge_numero",Long(ls_Nula))
			This.SetFocus()
			Return 1
		End If

	Case "plde_coorde"
		If Not iuo_PltaDestino.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(1, ls_Columna, integer(ls_Nula))
			Return 1
		ElseIf iuo_PltaDestino.Codigo = gstr_ParamPlanta.CodigoPlanta Then
			MessageBox("Atención", "No puede Despachar a la Planta de Origen.~r~r" + &
						"Ingrese o seleccione otra Planta de Destino.", Exclamation!, Ok!)
			This.SetItem(1, ls_Columna, integer(ls_Nula))
			Return 1
		Else
 			If istr_Mant.Argumento[2] = "30" Then
				If NOT revisacamara(Integer(data)) Then
					This.SetItem(1, ls_Columna, integer(ls_Nula))
					Return 1
				ElseIf Not revisaultcorrel(dw_2.Object.clie_codigo[1],Integer(data)) Then
					This.SetItem(1, ls_Columna, integer(ls_Nula))
					Return 1
				End If 	
			End If	
		End If

	Case "moti_codigo"
		If Not iuo_MotiDespacho.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(1, ls_Columna, integer(ls_Nula))
			
			Return 1
		End If

	Case "tran_codigo"
		If Not iuo_Transport.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(1, ls_Columna, integer(ls_Nula))
			
			Return 1
		End If

	Case "cami_patent"
    If istr_Mant.Argumento[2] = "22" OR istr_Mant.Argumento[2] = "30" Then
		If Not iuo_Camion.Existe(2, Data, True, sqlca) Then
			This.SetItem(1, ls_Columna, ls_Nula)

			Return 1
		Else
			This.Object.cami_clasIfi[1]	=	2
			This.Object.cami_patcar[1]	=	iuo_Camion.PateCarro
			This.Object.mfge_rutcho[1]	=	iuo_Camion.RutChofer
			is_rut 							= 	iuo_Camion.RutChofer
			This.Object.mfge_chofer[1]	=	iuo_Camion.Chofer
			
		End If
	Else	
		If Not iuo_Camion.Existe(1, Data, True, sqlca) Then
			This.SetItem(1, ls_Columna, ls_Nula)

			Return 1
		Else
			This.Object.cami_clasIfi[1]	=	1
			This.Object.cami_patcar[1]	=	iuo_Camion.PateCarro
			This.Object.mfge_rutcho[1]	=	iuo_Camion.RutChofer
			is_rut 							= 	iuo_Camion.RutChofer
			This.Object.mfge_chofer[1]	=	iuo_Camion.Chofer
		End If
	End If	
	
	Case "defg_tipdoc"
		If Not iuo_TipoDocto.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(1, ls_Columna, integer(ls_Nula))
			Return 1
		End If
		
		Case "mfge_rutcho"
		is_rut = F_verrut(data, True)
		If is_rut = "" Then
			dw_2.SetItem(1, "mfge_rutcho", ls_Nula)
			dw_2.SetItem(1, "mfge_chofer", ls_Nula)
			Return 1
		Else
			
			If NoExisteChofer(Data) Then
				dw_2.SetItem(1, "mfge_rutcho", ls_Nula)
				dw_2.SetItem(1, "mfge_chofer", ls_Nula)
				Return 1
			Else	
				dw_2.SetItem(1, "mfge_chofer",is_chofer)
			End If
		End If	

	Case "mfge_fecmov"
		ld_Fecha	=	Date(data)	
		
		If NOT iuo_FechaMovto.Valida_FechaMovto(ld_Fecha) Then
			This.Object.mfge_fecmov[1]	=	Date(ls_Nula)
			Return 1
		End If
		
	Case "prod_codigo"
		If Not iuo_Productores.Existe(Long(Data), True, sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_Nula))
			Return 1
		Else
			dw_2.SetItem(1, "prod_nombre", iuo_Productores.nombre)
				
			istr_mant.argumento[6]	=	String(iuo_Productores.Codigo)
			istr_mant.argumento[8]	=	iuo_Productores.nombre
		End If
		
	Case 'mfge_trasva'
		If Data = '0' Then This.SetItem(Row, 'mfge_cantra', Integer(ls_Nula))
End CHOOSE

HabilitaIngreso()
end event

event dw_2::doubleclicked;//
end event

event dw_2::itemfocuschanged;call super::itemfocuschanged;IF is_rut <> "" THEN
	IF Len(is_rut) < 10 THEN
		This.Object.mfge_rutcho.Format = '@@@@@@'
	ELSE
		This.Object.mfge_rutcho.Format = '@@@.@@@.@@@-@'
	END IF
	
//	IF dwo.Name <> "mfge_rutcho" THEN
//		This.SetItem(1, "mfge_rutcho", is_rut)
//	END IF
END IF

HabilitaIngreso()
end event

event dw_2::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name	
	CASE "b_buscaproductor"
		buscaproductor()		

	CASE "b_camion"
		buscacamion()
		IF istr_Mant.Argumento[2] = "22" OR istr_Mant.Argumento[2] = "30" THEN
			iuo_Camion.Existe(2, This.Object.cami_patent[row], True, sqlca)
		ELSE
			iuo_Camion.Existe(1, This.Object.cami_patent[row], True, sqlca)
		END IF
		
END CHOOSE
end event

event dw_2::itemerror;RETURN 1
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutagranel_despacho
integer x = 4864
integer y = 256
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(TRUE)

tab_1.tp_1.Enabled	=	False
tab_1.tp_2.Enabled	=	False
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutagranel_despacho
integer x = 4864
integer y = 432
integer taborder = 80
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutagranel_despacho
integer x = 4864
integer y = 616
integer taborder = 100
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutagranel_despacho
integer x = 4864
integer y = 796
integer taborder = 110
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutagranel_despacho
integer x = 4864
integer y = 976
integer taborder = 120
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutagranel_despacho
integer x = 4864
integer y = 1488
integer taborder = 30
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutagranel_despacho
integer x = 4864
integer y = 1664
integer taborder = 40
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutagranel_despacho
integer x = 4864
integer y = 72
integer taborder = 50
end type

type tab_1 from tab within w_maed_movtofrutagranel_despacho
event create ( )
event destroy ( )
integer x = 55
integer y = 1016
integer width = 4773
integer height = 972
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean fixedwidth = true
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tp_1 tp_1
tp_2 tp_2
end type

on tab_1.create
this.tp_1=create tp_1
this.tp_2=create tp_2
this.Control[]={this.tp_1,&
this.tp_2}
end on

on tab_1.destroy
destroy(this.tp_1)
destroy(this.tp_2)
end on

event selectionchanged;IF NewIndex = 1 THEN
	IF dw_3.RowCount() > 0 THEN
		IF dw_2.Object.tpmv_codigo[1]<>30 AND NOT istr_mant.solo_consulta THEN
			pb_eli_det.Enabled	=	True
		END IF
		il_Fila 					=	1
		
		dw_3.SelectRow(0,False)
		dw_4.SelectRow(0,False)
		dw_3.SetRow(il_Fila)
		dw_3.SelectRow(il_Fila, True)
	ELSE
		pb_eli_det.Enabled	=	False
	END IF
ELSE
	IF dw_4.RowCount() > 0 THEN
		IF dw_2.Object.tpmv_codigo[1]<>30 AND NOT istr_mant.solo_consulta THEN
			pb_eli_det.Enabled	=	True
		END IF	
		il_Fila 					=	1
		
		dw_3.SelectRow(0,False)
		dw_4.SelectRow(0,False)
		dw_4.SetRow(il_Fila)
		dw_4.SelectRow(il_Fila, True)
	ELSE
		pb_eli_det.Enabled	=	False
	END IF
END IF
end event

type tp_1 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 4736
integer height = 844
boolean enabled = false
long backcolor = 16777215
string text = "Detalle de Fruta"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Table!"
long picturemaskcolor = 553648127
dw_detalle dw_detalle
end type

on tp_1.create
this.dw_detalle=create dw_detalle
this.Control[]={this.dw_detalle}
end on

on tp_1.destroy
destroy(this.dw_detalle)
end on

type dw_detalle from uo_dw within tp_1
integer x = 18
integer y = 36
integer width = 4672
integer height = 780
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_movtofrutagraneldeta_despacho"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_despacho.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutagranel_despacho.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_despacho.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event getfocus;call super::getfocus;IF il_fila > 0 THEN This.SelectRow(il_fila, True)

RETURN 0
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = getrow()
	This.SelectRow(0,False)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);call super::ue_seteafila;il_fila	= This.GetRow()

This.SelectRow(0, False)
This.SelectRow(il_fila, True)

RETURN 0
end event

type tp_2 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 4736
integer height = 844
boolean enabled = false
long backcolor = 16777215
string text = "Detalle de Envases       "
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "ArrangeTables!"
long picturemaskcolor = 553648127
dw_envases dw_envases
end type

on tp_2.create
this.dw_envases=create dw_envases
this.Control[]={this.dw_envases}
end on

on tp_2.destroy
destroy(this.dw_envases)
end on

type dw_envases from uo_dw within tp_2
integer x = 37
integer y = 36
integer width = 4672
integer height = 780
integer taborder = 10
string dataobject = "dw_mues_movtoenvadeta_desp"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_despacho.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutagranel_despacho.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_despacho.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event getfocus;call super::getfocus;IF il_fila > 0 THEN This.SelectRow(il_fila, True)

RETURN 0
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = getrow()
	This.SelectRow(0,False)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);call super::ue_seteafila;il_fila	= This.GetRow()

This.SelectRow(0, False)
This.SelectRow(il_fila, True)

RETURN 0
end event

type cb_guia from commandbutton within w_maed_movtofrutagranel_despacho
integer x = 4859
integer y = 1256
integer width = 302
integer height = 112
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Guía SII"
end type

event clicked;Str_mant		lstr_mant
Long			ll_Fila

lstr_Mant.Argumento[1]	=	String(dw_2.Object.plde_codigo[1])
lstr_Mant.Argumento[2]	=	String(dw_2.Object.tpmv_codigo[1])
lstr_Mant.Argumento[3]	=	String(dw_2.Object.mfge_numero[1])
lstr_Mant.Argumento[5]	=  String(dw_2.Object.clie_codigo[1])
lstr_Mant.Argumento[6]	=	'4'

OpenWithParm(w_emis_guia_despacho, lstr_Mant)

lstr_Mant = Message.PowerObjectParm

IF IsNull(lstr_Mant) THEN RETURN


//Normaliza Nro. Guia despacho
For ll_Fila = 1 To dw_1.RowCount()
	dw_1.Object.meen_guisii[ll_Fila] = Long(lstr_Mant.Argumento[1])
Next

Parent.TriggerEvent("ue_guardar")

IF lstr_Mant.Respuesta = 1 THEN
	istr_mant.solo_consulta = TRUE
	Parent.TriggerEvent("ue_recuperadatos")
END IF
end event

type dw_exideta from datawindow within w_maed_movtofrutagranel_despacho
boolean visible = false
integer x = 3328
integer y = 200
integer width = 187
integer height = 156
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

event dberror;String	ls_Tipo, ls_Mensaje

Str_ErrorBaseDatos	lstr_ErrBD

CHOOSE CASE buffer
	CASE delete!
		ls_Tipo = "Borrando"
		
	CASE primary!
		DwItemStatus Stat
		
		Stat	=	This.getitemstatus(Row, 0, Buffer)
		
		IF Stat = New! OR Stat = NewModified! THEN
			ls_Tipo	=	"Agregando"
		ELSE
			ls_Tipo	=	"Actualizando"
		END IF
		
END CHOOSE

lstr_ErrBD.Titulo	=	"Error " + ls_Tipo + " registro " + String(row)
lstr_ErrBD.Numero	=	SqlDbCode
lstr_ErrBD.Texto	=	SqlErrText
is_error				=  SqlErrText
il_coderror			=	SqlDbCode

ls_Mensaje	=	"Error " + ls_Tipo + " registro " + String(row)
ls_Mensaje	+=	"~r~nNúmero de Error Base Datos: " + String(SqlDbCode)
ls_Mensaje	+=	"~r~nMensaje de Error Base Datos:~r~n~r~n" + SqlErrText

lstr_ErrBD.MensajePantalla	=	ls_Mensaje

//OpenWithParm(w_ErrorBaseDatos, lstr_ErrBD)

This.SetFocus()
This.SetRow(row)
This.ScrollToRow(row)

RETURN 1
end event

type dw_exiencab from datawindow within w_maed_movtofrutagranel_despacho
boolean visible = false
integer x = 3506
integer y = 60
integer width = 187
integer height = 144
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_exismovtoenca_analisis"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

event dberror;String	ls_Tipo, ls_Mensaje

Str_ErrorBaseDatos	lstr_ErrBD

CHOOSE CASE buffer
	CASE delete!
		ls_Tipo = "Borrando"
		
	CASE primary!
		DwItemStatus Stat
		
		Stat	=	This.getitemstatus(Row, 0, Buffer)
		
		IF Stat = New! OR Stat = NewModified! THEN
			ls_Tipo	=	"Agregando"
		ELSE
			ls_Tipo	=	"Actualizando"
		END IF
		
END CHOOSE

lstr_ErrBD.Titulo	=	"Error " + ls_Tipo + " registro " + String(row)
lstr_ErrBD.Numero	=	SqlDbCode
lstr_ErrBD.Texto	=	SqlErrText
is_error				=  SqlErrText
il_coderror			=	SqlDbCode

ls_Mensaje	=	"Error " + ls_Tipo + " registro " + String(row)
ls_Mensaje	+=	"~r~nNúmero de Error Base Datos: " + String(SqlDbCode)
ls_Mensaje	+=	"~r~nMensaje de Error Base Datos:~r~n~r~n" + SqlErrText

lstr_ErrBD.MensajePantalla	=	ls_Mensaje

//OpenWithParm(w_ErrorBaseDatos, lstr_ErrBD)

This.SetFocus()
This.SetRow(row)
This.ScrollToRow(row)

RETURN 1
end event

type dw_granpesa from datawindow within w_maed_movtofrutagranel_despacho
boolean visible = false
integer x = 3328
integer y = 64
integer width = 187
integer height = 144
integer taborder = 140
boolean bringtotop = true
string dataobject = "dw_cargabultoslote"
end type

event retrievestart;Return 2
end event

type dw_error from uo_dw within w_maed_movtofrutagranel_despacho
boolean visible = false
integer x = 3141
integer y = 64
integer width = 187
integer height = 148
integer taborder = 11
boolean bringtotop = true
boolean vscrollbar = false
end type

type dw_exismovtodetanulos from datawindow within w_maed_movtofrutagranel_despacho
boolean visible = false
integer x = 1952
integer y = 2176
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_inserta_exismovtodetanulos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exidetaborra from datawindow within w_maed_movtofrutagranel_despacho
boolean visible = false
integer x = 494
integer y = 2188
integer width = 686
integer height = 400
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta_borra"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_movtobins from datawindow within w_maed_movtofrutagranel_despacho
boolean visible = false
integer x = 3141
integer y = 204
integer width = 197
integer height = 152
integer taborder = 21
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_movtofrutagranpesa_porlote"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event retrievestart;Return 2
end event

type dw_5 from datawindow within w_maed_movtofrutagranel_despacho
boolean visible = false
integer x = 3515
integer y = 204
integer width = 187
integer height = 152
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_movtograndeta_tarjas"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

