$PBExportHeader$w_maed_movtofrutacomenca_reproceso.srw
$PBExportComments$Recepción - Despacho de Fruta Comercial.
forward
global type w_maed_movtofrutacomenca_reproceso from w_mant_encab_deta_csd
end type
type tab_1 from tab within w_maed_movtofrutacomenca_reproceso
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
type tab_1 from tab within w_maed_movtofrutacomenca_reproceso
tp_1 tp_1
tp_2 tp_2
end type
type dw_6 from datawindow within w_maed_movtofrutacomenca_reproceso
end type
type dw_5 from datawindow within w_maed_movtofrutacomenca_reproceso
end type
type cb_guisii from commandbutton within w_maed_movtofrutacomenca_reproceso
end type
type cb_capturadatos from commandbutton within w_maed_movtofrutacomenca_reproceso
end type
type dw_exideta from datawindow within w_maed_movtofrutacomenca_reproceso
end type
type dw_exiencab from datawindow within w_maed_movtofrutacomenca_reproceso
end type
end forward

global type w_maed_movtofrutacomenca_reproceso from w_mant_encab_deta_csd
integer width = 3625
integer height = 2092
string menuname = ""
event ue_imprimir ( )
event ue_carga_detalle pbm_custom27
event ue_despuesguardar ( )
tab_1 tab_1
dw_6 dw_6
dw_5 dw_5
cb_guisii cb_guisii
cb_capturadatos cb_capturadatos
dw_exideta dw_exideta
dw_exiencab dw_exiencab
end type
global w_maed_movtofrutacomenca_reproceso w_maed_movtofrutacomenca_reproceso

type variables
w_mant_deta_movtofrutacomdeta			iw_mantencion_1
w_mant_deta_movtoenvadeta				iw_mantencion_2


DataWindowChild   						idwc_PltaDest, 	idwc_Transp, 	idwc_Camion, &
												idwc_TipoMovto, 	idwc_cliente, 	idwc_productores
DataWindow									dw_3, dw_4
str_variedad								istr_variedad
str_categoria								istr_categoria

uo_plantadesp								iuo_PltaDestino
uo_Productores								iuo_Productor
uo_transportista							iuo_Transport
uo_camiones									iuo_Camion
uo_tipodoctoplanta						iuo_TipoDocto
uo_clienprove								iuo_ClienProve
uo_lotesfrutacomer						iuo_LotesFrutaComer
uo_tipomovtofruta							iuo_TipoMovtoFruta
uo_tipomovtofruta							iuo_TipoMovtoEnva
uo_clientesprod							iuo_clientes
uo_calicosechero  						iuo_calicosechero

Long     									il_NumFruta = 0, il_NumEnva = 0, il_conexiste, il_coneccion
Boolean										ib_Modifica, ib_AutoCommit, ib_Predefinido, ib_ConectadoExistencia
Integer										ii_TipoMovto, ii_Movto, ii_cliente
String										is_rut, is_columna, is_rutcliente, is_archivo

Transaction		sqlexi
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public function boolean existeorden (integer ai_planta, integer ai_tipo, long al_numero)
public subroutine buscaorden ()
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean existemovimientodoc (integer ai_tipodoc, long al_docrel)
public subroutine habilitaingreso (string as_columna)
public subroutine buscacamion ()
public function boolean existemovimiento (integer ai_planta, integer ai_tipomovto, long al_numero)
public subroutine modificaencab (integer is_movto)
public function boolean retiraprod (long al_productor, integer ai_especie, integer ai_variedad, integer ai_categoria, date ad_fechalot)
public subroutine agregaclientea_dws ()
public function boolean valida_password ()
public function boolean conexionexistencia ()
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "MOVIMIENTO DE FRUTA COMERCIAL"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_movtofrutacomenca_movtos"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]),&
                          Long(istr_mant.argumento[3]))

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

event ue_carga_detalle;w_main.SetMicroHelp("Cargando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

Long		ll_Nuevo, ll_NumeroLote, ll_TotalBultos,ll_Productor
String	ls_datos, ls_Patente, ls_PatCarro, ls_Calida, ls_Cliente, ls_RutChofer, ls_Chofer, &
			ls_GrupoCal, ls_TipoFrio, ls_Calibre
Integer	li_retorno = 2, li_archivo, li_Tipo, li_Planta, li_PlantaCoorde, li_Transporte, &
			 li_Camion, li_Camara, li_Pltcod, li_EspCod, li_SecLote, &
			li_TipoEnvase, li_Envase, li_ConEnv, li_Cantidad, li_Categoria, li_Servicio, &
			li_DiasGr, li_Turno, li_Variedad, li_PeriodoFrio, li_Secuencia, li_Null
Dec{3}	ld_Kilos, ld_PesoNeto, ld_KilProm
Dec{2}	ld_Bultos
Date		ld_FechaMovto, ld_FechaLote

SetNull(li_Null)

li_archivo	= FileOpen(is_archivo)

IF li_archivo < 0 THEN
	li_retorno = MessageBox("Error","Archivo no pudo ser abierto.", Exclamation!,RetryCancel!)
	Message.DoubleParm = li_retorno
	RETURN 1
ELSE
	SetPointer(HourGlass!)

	dw_3.SetRedraw(False)
	dw_4.SetRedraw(False)

	DO WHILE FileRead(li_archivo, ls_datos) >= 0
		li_Tipo				=	Integer(Mid(ls_datos,1,1))

		IF li_Tipo	=	1	THEN
			li_Planta			=	Integer(Mid(ls_datos,2,4))
			ld_FechaMovto		=	Date(Mid(ls_datos,9,2) + "/" + Mid(ls_datos,11,2) + "/" + &
										Mid(ls_datos,13,4))

			ll_Productor		=	long(Mid(ls_datos,19,4))
			IF ll_Productor = 0 THEN
				ll_Productor = li_Null
			END IF

			li_PlantaCoorde	=	Integer(Mid(ls_datos,23,4))
			li_Transporte		=	Integer(Mid(ls_datos,27,4))
			ls_Cliente			=	Trim(Mid(ls_datos,31,10))
			li_Camion			=	Integer(Mid(ls_datos,41,1))
			ls_Patente			=	Trim(Mid(ls_datos,42,10))
			ls_PatCarro			=	Trim(Mid(ls_datos,52,10))
			ls_RutChofer		=	Trim(Mid(ls_datos,70,10))
			ls_Chofer			=	Trim(Mid(ls_datos,80,40))
			ll_TotalBultos		=	Long(Mid(ls_datos,120,4))
			ld_PesoNeto			=	Dec(Mid(ls_datos,124,12))

			// Encabezado Movimento Fruta Comercial
			dw_2.SetItem(1, "plde_codigo", li_Planta)
			dw_2.SetItem(1, "tpmv_codigo", Integer(istr_Mant.Argumento[2]))
			dw_2.SetItem(1, "prod_codigo", ll_Productor)
			dw_2.SetItem(1, "plde_coorde", li_PlantaCoorde)
			dw_2.SetItem(1, "tran_codigo", li_Transporte)
			dw_2.SetItem(1, "clpr_rut", ls_Cliente)
			dw_2.SetItem(1, "cami_clasifi", li_Camion)
			dw_2.SetItem(1, "cami_patent", ls_Patente)
			dw_2.SetItem(1, "cami_patcar", ls_PatCarro)
			dw_2.SetItem(1, "mfco_rutcho", ls_RutChofer)
			dw_2.SetItem(1, "mfco_chofer", ls_Chofer)
			dw_2.SetItem(1, "mfco_fecmov", ld_FechaMovto)
			dw_2.SetItem(1, "mfco_totbul", ll_TotalBultos)
			dw_2.SetItem(1, "mfco_tpneto", ld_PesoNeto)

		ELSEIF	li_Tipo	=	2	THEN
			li_PlantaCoorde	=	Integer(Mid(ls_datos,2,4))
			li_Camara			=	Integer(Mid(ls_datos,6,4))
			li_PltCod			=	Integer(Mid(ls_datos,10,4))
			li_EspCod			=	Integer(Mid(ls_datos,14,2))
			ll_NumeroLote		=	Long(Mid(ls_datos,16,8))
			li_SecLote			=	Integer(Mid(ls_datos,24,2))
			ld_Bultos			=	Dec(Mid(ls_datos,26,9))
			ld_Kilos				=	Dec(Mid(ls_datos,35,12))
			ld_KilProm			=	Dec(Mid(ls_datos,47,12))

			ll_Nuevo				=	dw_3.InsertRow(0)

			// Detalle de Movimiento Fruta Comercial
			dw_3.SetItem(ll_Nuevo, "plde_coorde", li_PlantaCoorde)
			dw_3.SetItem(ll_Nuevo, "cama_codigo", li_Camara)
			dw_3.SetItem(ll_Nuevo, "lofc_pltcod", li_PltCod)
			dw_3.SetItem(ll_Nuevo, "lofc_espcod", li_EspCod)
			dw_3.SetItem(ll_Nuevo, "lofc_lotefc", ll_NumeroLote)
			dw_3.SetItem(ll_Nuevo, "lfcd_secuen", li_SecLote)
			dw_3.SetItem(ll_Nuevo, "mfcd_bulent", ld_Bultos)
			dw_3.SetItem(ll_Nuevo, "mfcd_kgnent", ld_Kilos)
			dw_3.SetItem(ll_Nuevo, "mfcd_kilrom", ld_KilProm)

		ELSEIF	li_Tipo	=	3	THEN
			ll_Productor		=	long(Mid(ls_datos,2,4))
			li_Categoria		=	Integer(Mid(ls_datos,6,3))
			li_Servicio			=	Integer(Mid(ls_datos,9,2))
			li_DiasGr			=	Integer(Mid(ls_datos,11,3))
			ls_GrupoCal			=	Trim(Mid(ls_datos,14,3))
			ll_TotalBultos		=	Long(Mid(ls_datos,17,4))
			ld_Kilos				=	Dec(Mid(ls_datos,21,12))

			IF NOT iuo_LotesFrutaComer.ExisteEncab(Integer(istr_Mant.Argumento[1]), &
												li_EspCod, ll_NumeroLote, False, SqlCa) THEN

				ll_Nuevo				=	dw_5.InsertRow(0)

				// Encabezado de Lote Fruta Comercial
				dw_5.SetItem(ll_Nuevo, "lofc_espcod", li_EspCod)
				dw_5.SetItem(ll_Nuevo, "lofc_lotefc", ll_NumeroLote)
				dw_5.SetItem(ll_Nuevo, "prod_codigo", ll_Productor)
				dw_5.SetItem(ll_Nuevo, "cate_codigo", li_Categoria)
				dw_5.SetItem(ll_Nuevo, "sepl_codigo", li_Servicio)
				dw_5.SetItem(ll_Nuevo, "lofc_diagra", li_DiasGr)
				dw_5.SetItem(ll_Nuevo, "lofc_grucal", ls_GrupoCal)
				dw_5.SetItem(ll_Nuevo, "lofc_totbul", ll_TotalBultos)
				dw_5.SetItem(ll_Nuevo, "lofc_totkil", ld_Kilos)
			END IF

		ELSEIF	li_Tipo	=	4	THEN
			li_Secuencia		=	Integer(Mid(ls_datos,2,2))
			li_Turno				=	Integer(Mid(ls_datos,4,1))
			ld_FechaLote		=	Date(Mid(ls_datos,5,2) + "/" + Mid(ls_datos,7,2) + "/" + &
										Mid(ls_datos,9,4))

			ll_Productor		=	long(Mid(ls_datos,13,4))
			li_Variedad			=	Integer(Mid(ls_datos,17,4))
			ls_TipoFrio			=	Trim(Mid(ls_datos,21,2))
			li_PeriodoFrio		=	Integer(Mid(ls_datos,23,2))
			li_TipoEnvase		=	Integer(Mid(ls_datos,25,1))
			li_Envase			=	Integer(Mid(ls_datos,26,3))
			ls_Calibre			=	Trim(Mid(ls_datos,29,4))
			ls_GrupoCal			=	Trim(Mid(ls_datos,33,4))
			ld_Bultos			=	Dec(Mid(ls_datos,37,9))
			ld_PesoNeto			=	Dec(Mid(ls_datos,46,12))
			ld_KilProm			=	Dec(Mid(ls_datos,58,12))

			IF NOT iuo_LotesFrutaComer.ExisteEncab(Integer(istr_Mant.Argumento[1]), &
												li_EspCod, ll_NumeroLote, False, SqlCa) THEN

				ll_Nuevo				=	dw_6.InsertRow(0)

				// Detalle de Lote Fruta Comercial
				dw_6.SetItem(ll_Nuevo, "lofc_espcod", li_EspCod)
				dw_6.SetItem(ll_Nuevo, "lofc_lotefc", ll_NumeroLote)
				dw_6.SetItem(ll_Nuevo, "lfcd_secuen", li_Secuencia)
				dw_6.SetItem(ll_Nuevo, "lfcd_nturno", li_Turno)
				dw_6.SetItem(ll_Nuevo, "lfcd_fecham", ld_FechaLote)
				dw_6.SetItem(ll_Nuevo, "prod_codigo", ll_Productor)
				dw_6.SetItem(ll_Nuevo, "vari_codigo", li_Variedad)
				dw_6.SetItem(ll_Nuevo, "frio_tipofr", ls_TipoFrio)
				dw_6.SetItem(ll_Nuevo, "pefr_codigo", li_PeriodoFrio)
				dw_6.SetItem(ll_Nuevo, "enva_tipoen", li_TipoEnvase)
				dw_6.SetItem(ll_Nuevo, "enva_codigo", li_Envase)
				dw_6.SetItem(ll_Nuevo, "refe_calibr", ls_Calibre)
				dw_6.SetItem(ll_Nuevo, "refe_gcalib", ls_GrupoCal)
				dw_6.SetItem(ll_Nuevo, "lfcd_bultos", ld_Bultos)
				dw_6.SetItem(ll_Nuevo, "lfcd_kilnet", ld_PesoNeto)
				dw_6.SetItem(ll_Nuevo, "lfcd_kilpro", ld_KilProm)
			END IF

		ELSEIF	li_Tipo	=	5	THEN
			li_TipoEnvase		=	Integer(Mid(ls_datos,2,1))
			li_Envase			=	Integer(Mid(ls_datos,3,3))
			li_ConEnv			=	Integer(Mid(ls_datos,6,1))
			ls_Calida			=	Trim(Mid(ls_datos,7,4))
			li_Cantidad			=	Integer(Mid(ls_datos,12,4))
			ld_PesoNeto			=	Dec(Mid(ls_datos,16,8))

			ll_Nuevo				=	dw_4.InsertRow(0)

			// Detalle de Movimiento Envase
			dw_4.SetItem(ll_Nuevo, "enva_tipoen", li_TipoEnvase)
			dw_4.SetItem(ll_Nuevo, "enva_codigo", li_Envase)
			dw_4.SetItem(ll_Nuevo, "fgme_conenv", li_ConEnv)
			dw_4.SetItem(ll_Nuevo, "cale_calida", ls_Calida)
			dw_4.SetItem(ll_Nuevo, "fgme_sentid", Integer(istr_Mant.Argumento[4]))
			dw_4.SetItem(ll_Nuevo, "fgme_cantid", li_Cantidad)
			dw_4.SetItem(ll_Nuevo, "fgme_pesone", ld_PesoNeto)
		END IF
	LOOP
	dw_3.SetRedraw(True)
	dw_4.SetRedraw(True)
END IF

FileClose(li_archivo)

Message.DoubleParm = li_retorno

RETURN 0

SetPointer(Arrow!)
end event

event ue_despuesguardar();integer	li_fila, li_cliente, li_planta,li_enva_codigo, li_enva_tipoen, li_bodzonal, li_lote_pltcod, &
			li_lote_espcod, li_lote_codigo, li_bodedestino
Long 		ll_fila, ll_numero, li_secuencia = 1, ll_fila_nea, ll_docrel, ll_numnuevoini, ll_numnuevofin,&
			ll_prod_codigo
String	ls_item_codigo, ls_productor, ls_prod, ls_calidad
Boolean	lb_AutoCommit, lb_Retorno

IF ib_Conectadoexistencia THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	ll_docrel = dw_1.Object.meen_numero[1]
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
	ELSE
		li_bodedestino = luo_existencia.BodDestino
		li_bodzonal		= luo_existencia.bodzonal
	END IF		
	
	luo_existencia.existeencabezado(ll_docrel,li_bodzonal,1,3,True,sqlexi)
		
	IF luo_existencia.count <> 0 THEN
		Return 
	END IF	
	
	IF Not luo_existencia.correlativobode(3,li_bodzonal,li_bodzonal,True,sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
	ELSE
		ll_numero = luo_existencia.numero
	END IF	
		
	IF isnull(ll_numero) THEN
		ll_numnuevoini = Long(String(li_bodzonal)+''+'0001')
		ll_numnuevofin = Long(String(li_bodzonal)+''+'9999')
		
		INSERT INTO dba.correlbode(bode_codigo,mden_tipdoc,cobo_bodadm,cobo_inicia,cobo_final,cobo_actual)
		VALUES(:li_bodzonal,3,:li_bodedestino,:ll_numnuevoini,:ll_numnuevofin,:ll_numnuevoini)
		USING sqlexi;
		
		IF sqlexi.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlexi,"Correlbode")
			Message.DoubleParm = -1
			sqlexi.AutoCommit	=	ib_AutoCommit
			RETURN 
		END IF
		ll_numero = ll_numnuevoini - 1
	END IF	
	
	ll_numero = ll_numero + 1
	
	li_lote_pltcod = dw_3.Object.lofc_pltcod[1]
	li_lote_espcod = dw_3.Object.lofc_espcod[1]
	li_lote_codigo = dw_3.Object.lofc_lotefc[1]
	
	ls_productor = String(dw_1.Object.prod_codigo[1],'000000')
	
	IF ls_productor = '' OR isnull(ls_productor) OR ls_productor = '000000' THEN 	
		SELECT prod_codigo
		INTO :ll_prod_codigo
		FROM dba.spro_lotesfrutacomenc
		WHERE lofc_pltcod = :li_lote_pltcod
		AND	lofc_espcod = :li_lote_espcod
		AND	lofc_lotefc = :li_lote_codigo;
			
		ls_productor = String(ll_prod_codigo,'000000')	
	END IF 		
	
	IF Not luo_existencia.Existecliente(ls_productor,True,sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
	ELSE	
		ls_productor = luo_existencia.prod
	END IF
	
	IF luo_existencia.prod = '' OR isnull(luo_existencia.prod) THEN
		ls_productor = luo_existencia.prdgen
	END IF	

	ll_fila_nea = dw_exiencab.InsertRow(0)
	
	dw_exiencab.Object.mden_tipdoc[ll_fila_nea] 	= 	3
	dw_exiencab.Object.mden_numero[ll_fila_nea] 	= 	ll_numero 
	dw_exiencab.Object.tpdo_codigo[ll_fila_nea] 	= 	2
	dw_exiencab.Object.mden_fecmov[ll_fila_nea] 	= 	dw_2.Object.mfco_fecmov[1]
	dw_exiencab.Object.tpmv_tipomv[ll_fila_nea] 	= 	2
	dw_exiencab.Object.tpmv_codigo[ll_fila_nea] 	= 	2
	dw_exiencab.Object.mden_tipana[ll_fila_nea] 	= 	4
	dw_exiencab.Object.bode_codigo[ll_fila_nea] 	= 	li_bodzonal
	dw_exiencab.Object.mden_bodest[ll_fila_nea] 	= 	li_bodedestino
	dw_exiencab.Object.clpr_rut[ll_fila_nea]	 	= 	ls_productor
	dw_exiencab.Object.mden_docrel[ll_fila_nea] 	= 	dw_4.Object.meen_numero[1]
	dw_exiencab.Object.mden_fecdre[ll_fila_nea] 	= 	dw_2.Object.mfco_fecmov[1]
	dw_exiencab.Object.mden_observ[ll_fila_nea] 	= 	'Despacho de Bodega Zonal'
	dw_exiencab.Object.mden_estado[ll_fila_nea] 	= 	1
	dw_exiencab.Object.mden_pcopda[ll_fila_nea] 	=	1
	
	FOR li_fila = 1 TO dw_4.RowCount()
		ll_fila				=	dw_exideta.InsertRow(0)
		
		li_enva_codigo 	= 	dw_4.Object.enva_codigo[li_fila]
		li_enva_tipoen 	= 	dw_4.Object.enva_tipoen[li_fila]
		ls_calidad			=  dw_4.Object.cale_calida[li_fila]
		
		IF Not iuo_calicosechero.Existe(li_enva_tipoen,li_enva_codigo,ls_calidad,True,sqlca) THEN
			Message.DoubleParm = -1
			RETURN 
		ELSE
			ls_item_codigo = iuo_calicosechero.item
		END IF	
		
		IF ls_item_codigo = '' OR IsNull(ls_item_codigo) THEN
			ls_item_codigo = luo_existencia.itgene
		END IF
	
		dw_exideta.Object.mden_tipdoc[ll_fila]	= 3
		dw_exideta.Object.mden_numero[ll_fila] = ll_numero
		dw_exideta.Object.mdde_secuen[ll_fila] = li_secuencia 
		dw_exideta.Object.tpmv_tipomv[ll_fila] = 2
		dw_exideta.Object.tpmv_codigo[ll_fila] = 2
		dw_exideta.Object.item_codigo[ll_fila] = ls_item_codigo
		dw_exideta.Object.mdde_identi[ll_fila] = ''
		dw_exideta.Object.mdde_fecmov[ll_fila] = dw_2.Object.mfco_fecmov[1]
		dw_exideta.Object.bode_codigo[ll_fila] = li_bodzonal
		dw_exideta.Object.mdde_cantid[ll_fila] = dw_4.Object.fgme_cantid[li_fila]
					
		li_secuencia = li_secuencia + 1
	
	NEXT	
	
	IF dw_exiencab.Rowcount() > 0 THEN
		lb_AutoCommit			=	sqlexi.AutoCommit
		sqlexi.AutoCommit		=	False
		
		IF dw_exiencab.Update(True, False) = 1 THEN
			IF dw_exideta.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlexi.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlexi, This.Title)
					
					RollBack;
					dw_exideta.Reset()
					dw_exiencab.Reset()
				ELSE
					lb_Retorno	=	True
					
					dw_exiencab.ResetUpdate()
					dw_exideta.ResetUpdate()
				END IF
			ELSE
				F_ErrorBaseDatos(sqlexi, This.Title)
				
				RollBack;
				dw_exideta.Reset()
				dw_exiencab.Reset()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlexi, This.Title)
			
			RollBack;
			dw_exideta.Reset()
			dw_exiencab.Reset()
		END IF
		Messagebox("Existencia","Grabación de Datos realizada Satisfactoriamente")
		sqlexi.AutoCommit		=	lb_AutoCommit
		dw_exideta.Reset()
		dw_exiencab.Reset()
	
	END IF
	DISCONNECT USING sqlexi;
	ib_Conectadoexistencia = False
END IF

end event

public subroutine habilitaencab (boolean habilita);
IF Habilita THEN
	
	dw_2.Object.mfco_numero.Protect						=	0
	dw_2.Object.mfco_numero.BackGround.Color			=	RGB(255,255,255)
	dw_2.Object.mfco_fecmov.Protect						=	0
	dw_2.Object.mfco_fecmov.BackGround.Color			=	RGB(255,255,255)
	
	dw_2.Object.clie_codigo.Protect						=	0
	dw_2.Object.clie_codigo.BackGround.Color			=	RGB(255,255,255)
	dw_2.Object.plde_codigo.Protect						=	0
	dw_2.Object.plde_codigo.BackGround.Color			=	RGB(255,255,255)
   
	IF integer(istr_mant.argumento[4]) = 1 THEN
      	dw_2.Object.mfco_horaen.Protect				=	0
  		dw_2.Object.mfco_horaen.BackGround.Color		=	RGB(255,255,255)
   ELSE		  
		dw_2.Object.mfco_horasa.Protect					=	0
		dw_2.Object.mfco_horasa.BackGround.Color		=	RGB(255,255,255)
	END IF	

   IF integer(istr_mant.argumento[2]) = 2 THEN
		dw_2.Object.plde_coorde.Protect					=	0
		dw_2.Object.plde_coorde.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.tran_codigo.Protect					=	0
		dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
//		dw_2.Object.mfco_guisii.Protect					=	0
//		dw_2.Object.mfco_guisii.BackGround.Color		=	RGB(255,255,255)
	END IF	

	IF integer(istr_mant.argumento[2]) = 22 THEN
		
		dw_2.Object.moti_codigo.Protect					=	0
		dw_2.Object.moti_codigo.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.plde_coorde.Protect					=	0
		dw_2.Object.plde_coorde.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.tran_codigo.Protect					=	0
		dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
//		dw_2.Object.mfco_guisii.Protect					=	0
//		dw_2.Object.mfco_guisii.BackGround.Color		=	RGB(255,255,255)

	END IF
	
	IF integer(istr_mant.argumento[2]) = 23 THEN
		
		dw_2.Object.moti_codigo.Protect					=	0
		dw_2.Object.moti_codigo.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.prod_codigo.Protect					=	0
		dw_2.Object.prod_codigo.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.tran_codigo.Protect					=	0
		dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
//		dw_2.Object.mfco_guisii.Protect					=	0
//		dw_2.Object.mfco_guisii.BackGround.Color		=	RGB(255,255,255)

	END IF
	
	IF integer(istr_mant.argumento[2]) = 34 THEN
		
		dw_2.Object.moti_codigo.Protect					=	0
		dw_2.Object.moti_codigo.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.tran_codigo.Protect					=	0
		dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
//		dw_2.Object.mfco_guisii.Protect					=	0
//		dw_2.Object.mfco_guisii.BackGround.Color		=	RGB(255,255,255)

	END IF
		
	dw_2.Object.cami_patent.Protect						=	0
	dw_2.Object.cami_patent.BackGround.Color			=	RGB(255,255,255)
	dw_2.Object.cami_patcar.Protect						=	0
	dw_2.Object.cami_patcar.BackGround.Color			=	RGB(255,255,255)
	dw_2.Object.mfco_rutcho.Protect						=	0
	dw_2.Object.mfco_rutcho.BackGround.Color			=	RGB(255,255,255)
	dw_2.Object.mfco_chofer.Protect						=	0
	dw_2.Object.mfco_chofer.BackGround.Color			=	RGB(255,255,255)
	
	IF integer(istr_mant.argumento[2]) <> 25 THEN dw_2.Object.b_camion.visible	=	1
	IF integer(istr_mant.argumento[2]) = 25 THEN dw_2.Object.b_docrel.visible	=	1
ELSE
	
	dw_2.Object.mfco_numero.Protect						=	1
	dw_2.Object.mfco_numero.BackGround.Color			=	rgb(166,180,210)
	dw_2.Object.mfco_fecmov.Protect						=	1
	dw_2.Object.mfco_fecmov.BackGround.Color			=	rgb(166,180,210)
	
	dw_2.Object.clie_codigo.Protect						=	1
	dw_2.Object.clie_codigo.BackGround.Color			=	rgb(166,180,210)
	dw_2.Object.plde_codigo.Protect						=	1
	dw_2.Object.plde_codigo.BackGround.Color			=	rgb(166,180,210)

	IF integer(istr_mant.argumento[4]) = 1 THEN	
		dw_2.Object.mfco_horaen.Protect					=	1
		dw_2.Object.mfco_horaen.BackGround.Color		=	rgb(166,180,210)
	ELSE	
		dw_2.Object.mfco_horasa.Protect					=	1
		dw_2.Object.mfco_horasa.BackGround.Color		=	rgb(166,180,210)
	END IF
	
	IF integer(istr_mant.argumento[2]) = 2 THEN
		dw_2.Object.plde_coorde.Protect					=	1
		dw_2.Object.plde_coorde.BackGround.Color		=	rgb(166,180,210)
		dw_2.Object.tran_codigo.Protect					=	1
		dw_2.Object.tran_codigo.BackGround.Color		=	rgb(166,180,210)
		dw_2.Object.mfco_guisii.Protect					=	1
		dw_2.Object.mfco_guisii.BackGround.Color		=	rgb(166,180,210)
	END IF	

	IF integer(istr_mant.argumento[2]) = 22 THEN
		
		dw_2.Object.moti_codigo.Protect					=	1
		dw_2.Object.moti_codigo.BackGround.Color		=	rgb(166,180,210)
		dw_2.Object.plde_coorde.Protect					=	1
		dw_2.Object.plde_coorde.BackGround.Color		=	rgb(166,180,210)
		dw_2.Object.tran_codigo.Protect					=	1
		dw_2.Object.tran_codigo.BackGround.Color		=	rgb(166,180,210)
		dw_2.Object.mfco_guisii.Protect					=	1
		dw_2.Object.mfco_guisii.BackGround.Color		=	rgb(166,180,210)

	END IF
	
	IF integer(istr_mant.argumento[2]) = 23 THEN
		
		dw_2.Object.moti_codigo.Protect					=	1
		dw_2.Object.moti_codigo.BackGround.Color		=	rgb(166,180,210)
		dw_2.Object.prod_codigo.Protect					=	1
		dw_2.Object.prod_codigo.BackGround.Color		=	rgb(166,180,210)
		dw_2.Object.tran_codigo.Protect					=	1
		dw_2.Object.tran_codigo.BackGround.Color		=	rgb(166,180,210)
		dw_2.Object.mfco_guisii.Protect					=	1
		dw_2.Object.mfco_guisii.BackGround.Color		=	rgb(166,180,210)

	END IF
	
	IF integer(istr_mant.argumento[2]) = 25 THEN
		dw_2.Object.plde_coorde.Protect					=	1
		dw_2.Object.plde_coorde.BackGround.Color		=	rgb(166,180,210)
		dw_2.Object.mfco_docrel.Protect					=	1
		dw_2.Object.mfco_docrel.BackGround.Color		=	rgb(166,180,210)
	END IF
	
	IF integer(istr_mant.argumento[2]) = 34 THEN
		
		dw_2.Object.moti_codigo.Protect					=	1
		dw_2.Object.moti_codigo.BackGround.Color		=	rgb(166,180,210)
		dw_2.Object.tran_codigo.Protect					=	1
		dw_2.Object.tran_codigo.BackGround.Color		=	rgb(166,180,210)
		dw_2.Object.mfco_guisii.Protect					=	1
		dw_2.Object.mfco_guisii.BackGround.Color		=	rgb(166,180,210)

	END IF
	
	dw_2.Object.cami_patent.Protect						=	1
	dw_2.Object.cami_patent.BackGround.Color			=	rgb(166,180,210)
	dw_2.Object.cami_patcar.Protect						=	1
	dw_2.Object.cami_patcar.BackGround.Color			=	rgb(166,180,210)
	dw_2.Object.mfco_rutcho.Protect						=	1
	dw_2.Object.mfco_rutcho.BackGround.Color			=	rgb(166,180,210)
	dw_2.Object.mfco_chofer.Protect						=	1
	dw_2.Object.mfco_chofer.BackGround.Color			=	rgb(166,180,210)

	dw_2.Object.b_camion.visible							=	0
	dw_2.Object.b_docrel.visible							=	0
	
END IF
end subroutine

public function boolean existeorden (integer ai_planta, integer ai_tipo, long al_numero);Integer	li_especie
Boolean	lb_Retorno = True

SELECT	espe_codigo
	INTO	:li_especie
	FROM	dba.spro_doctointernopack
	WHERE	plde_codigo	=	:ai_Planta
	AND	dinp_tipdoc	=	:ai_Tipo
	AND	dinp_numero	=	:al_Numero;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Orden Interno Packing")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Número de Orden No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	istr_mant.argumento[5] = string(li_especie)
END IF

RETURN lb_Retorno
end function

public subroutine buscaorden ();Str_busqueda	lstr_busq
Integer			li_ClasifCamion

lstr_busq.argum[1] = String(dw_2.Object.mfco_tipdoc[1])

OpenWithParm(w_busqueda_doctointernopack, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_2.SetColumn("mfco_docrel")
	dw_2.SetFocus()
ELSE
	dw_2.Object.mfco_docrel[1]	=	Long(lstr_busq.argum[3])
   istr_Mant.Argumento[5]	   =	lstr_busq.argum[4]
	
	IF ExisteMovimientoDoc(dw_2.Object.mfco_tipdoc[1],Long(lstr_busq.argum[3])) THEN
		Triggerevent("ue_recuperadatos")
	END IF 
END IF

RETURN
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

IF NOT Borrando THEN
	IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		dw_2.SetItem(1,"mfco_usuari",gstr_us.nombre)
		dw_2.SetItem(1,"mfco_comput",gstr_us.computador)
		dw_2.SetItem(1,"mfco_horacr",F_FechaHora())
	ELSEIF dw_2.GetItemStatus(1, 0, Primary!) = DataModified! THEN
		dw_2.SetItem(1,"mfco_usumod",gstr_us.nombre)
		dw_2.SetItem(1,"mfco_commod",gstr_us.computador)
		dw_2.SetItem(1,"mfco_horact",F_FechaHora())
	END IF
END IF

IF Borrando THEN
	IF dw_4.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF dw_3.Update(True, False) = 1 THEN
				IF dw_2.Update(True, False) = 1 THEN
					Commit;
			
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
					ELSE
						lb_Retorno	=	True
			
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
						dw_3.ResetUpdate()
						dw_4.ResetUpdate()
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
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_5.Update(True, False) = 1 THEN
			IF dw_6.Update(True, False) = 1 THEN
				IF dw_3.Update(True, False) = 1 THEN
					IF dw_1.Update(True, False) = 1 THEN
						IF dw_4.Update(True, False) = 1 THEN
							Commit;
			
							IF sqlca.SQLCode <> 0 THEN
								F_ErrorBaseDatos(sqlca, This.Title)
							ELSE
								lb_Retorno	=	True
			
								dw_1.ResetUpdate()
								dw_2.ResetUpdate()
								dw_3.ResetUpdate()
								dw_4.ResetUpdate()
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
END IF

sqlca.AutoCommit	=	ib_AutoCommit

RETURN lb_Retorno
end function

public function boolean existemovimientodoc (integer ai_tipodoc, long al_docrel);Integer li_tipomov, li_planta
Long    ll_numero

li_planta  = Integer(istr_mant.argumento[1])
li_tipomov = Integer(istr_mant.argumento[2])

SELECT mfco_numero INTO :ll_Numero
  FROM dba.spro_movtofrutacomenca
 WHERE plde_codigo = :li_planta
   AND tpmv_codigo = :li_tipomov
	AND mfco_tipdoc = :ai_tipodoc
	AND mfco_docrel = :al_docrel;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento Fruta Comerial")
   RETURN FALSE
ELSEIF sqlca.SQLCode = 0 THEN
	istr_mant.Argumento[3]	=	String(ll_Numero)
	RETURN TRUE
END IF

RETURN FALSE

end function

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date		ld_Fecha
Time		lt_Hora


IF as_Columna <> "mfco_fecmov" AND &
	(dw_2.Object.mfco_fecmov[1] = ld_Fecha OR IsNull(dw_2.Object.mfco_fecmov[1])) THEN
	lb_Estado = False
END IF

IF integer(istr_Mant.Argumento[4]) = 1 THEN
	IF as_Columna <> "mfco_horaen" AND &
		(dw_2.Object.mfco_horaen[1] = lt_Hora OR IsNull(dw_2.Object.mfco_horaen[1])) THEN
		lb_Estado = False
	END IF
ELSE
	IF as_Columna <> "mfco_horasa" AND &
		(dw_2.Object.mfco_horasa[1] = lt_Hora OR IsNull(dw_2.Object.mfco_horasa[1])) THEN
		lb_Estado = False
	END IF
END IF

IF integer(istr_mant.argumento[2]) <> 25 THEN
	IF integer(istr_mant.argumento[2]) = 2 THEN
		IF as_Columna <> "plde_coorde" AND &
			(dw_2.Object.plde_coorde[1] = 0 OR IsNull(dw_2.Object.plde_coorde[1])) THEN
			lb_Estado = False
		END IF
		IF as_Columna <> "tran_codigo" AND &
			IsNull(dw_2.Object.tran_codigo[1]) THEN
			lb_Estado = False
		END IF
	END IF
	
	IF integer(istr_mant.argumento[2]) = 22 THEN
		
		IF as_Columna <> "moti_codigo" AND &
			(dw_2.Object.moti_codigo[1] = 0 OR IsNull(dw_2.Object.moti_codigo[1])) THEN
			lb_Estado = False
		END IF
		IF as_Columna <> "plde_coorde" AND &
			(dw_2.Object.plde_coorde[1] = 0 OR IsNull(dw_2.Object.plde_coorde[1])) THEN
			lb_Estado = False
		END IF
		IF as_Columna <> "tran_codigo" AND &
			IsNull(dw_2.Object.tran_codigo[1]) THEN
			lb_Estado = False
		END IF
	END IF
	
	
	IF integer(istr_mant.argumento[2]) = 23 THEN
		
		IF as_Columna <> "moti_codigo" AND &
			(dw_2.Object.moti_codigo[1] = 0 OR IsNull(dw_2.Object.moti_codigo[1])) THEN
			lb_Estado = False
		END IF
		IF as_Columna <> "prod_codigo" AND &
			(dw_2.Object.prod_codigo[1] = 0 OR IsNull(dw_2.Object.prod_codigo[1])) THEN
			lb_Estado = False
		END IF
		IF as_Columna <> "tran_codigo" AND &
			IsNull(dw_2.Object.tran_codigo[1]) THEN
			lb_Estado = False
		END IF
	END IF
	
	IF integer(istr_mant.argumento[2]) = 34 THEN
		
		IF as_Columna <> "moti_codigo" AND &
			(dw_2.Object.moti_codigo[1] = 0 OR IsNull(dw_2.Object.moti_codigo[1])) THEN
			lb_Estado = False
		END IF
		IF as_Columna <> "tran_codigo" AND &
			IsNull(dw_2.Object.tran_codigo[1]) THEN
			lb_Estado = False
		END IF
	END IF
	
	IF as_Columna <> "cami_patent" AND &
		(dw_2.Object.cami_patent[1] = "" OR IsNull(dw_2.Object.cami_patent[1])) THEN
		lb_Estado = False
	END IF
		
	IF as_Columna <> "mfco_chofer" AND &
		(dw_2.Object.mfco_chofer[1] = "" OR IsNull(dw_2.Object.mfco_chofer[1])) THEN
		lb_Estado = False
	END IF
ELSE
	IF as_Columna <> "plde_coorde" AND &
		(dw_2.Object.plde_coorde[1] = 0 OR IsNull(dw_2.Object.plde_coorde[1])) THEN
		lb_Estado = False
	END IF
	
	IF as_Columna <> "mfco_docrel" AND &
		(dw_2.Object.mfco_docrel[1] = 0 OR IsNull(dw_2.Object.mfco_docrel[1])) THEN
		lb_Estado = False
	END IF
END IF

IF as_Columna = "tipo_ingdat" THEN Return

tab_1.tp_1.Enabled	=	lb_Estado

IF (istr_mant.argumento[2]<>"25") THEN
	tab_1.tp_2.Enabled	=	lb_Estado
END IF

IF lb_estado THEN
	pb_ins_det.Enabled	=	NOT istr_mant.solo_consulta
END IF	




end subroutine

public subroutine buscacamion ();Str_busqueda	lstr_busq
Integer			li_ClasifCamion

IF Integer(istr_Mant.Argumento[2]) = 2 OR Integer(istr_Mant.Argumento[2]) = 22 THEN
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
	dw_2.Object.cami_clasifi[1]			=	Integer(lstr_busq.argum[1])
	dw_2.Object.cami_patent[1]		=	lstr_busq.argum[2]
	dw_2.Object.cami_patcar[1]		=	lstr_busq.argum[6]
	dw_2.Object.mfco_rutcho[1]		=	lstr_busq.argum[5]
	dw_2.Object.mfco_chofer[1]		=	lstr_busq.argum[4]
	is_rut  = lstr_busq.argum[5]
	dw_2.SetFocus()
END IF

RETURN
end subroutine

public function boolean existemovimiento (integer ai_planta, integer ai_tipomovto, long al_numero);Long		ll_Numero
Integer	li_cliente
Boolean	lb_Retorno = True

SELECT	mfco_numero, clie_codigo
	INTO	:ll_Numero, :li_cliente
	FROM	dba.spro_movtofrutacomenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfco_numero	=	:al_Numero;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento Fruta Comerial")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	IF ii_cliente <> li_cliente THEN
		MessageBox("Advertencia","El movimiento ingresado no pertenece al cliente " + String(ii_cliente) + ". Se hará un cambio automático de este.")
		ii_cliente = li_cliente
		dw_2.Object.clie_codigo[1] = li_cliente
	END IF
	istr_mant.Argumento[3]	=	String(al_Numero)
	
	This.TriggerEvent("ue_recuperadatos")
ELSE
	MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine modificaencab (integer is_movto);
dw_2.SetReDraw(False)

IF Integer(istr_Mant.Argumento[4]) = 1 THEN


	cb_guisii.Enabled = FALSE
	cb_guisii.visible = TRUE
	
	IF is_Movto	=	2 THEN
		
		This.Title	=	"Recepción Otras Plantas"
		cb_capturadatos.Visible	=	True
		cb_capturadatos.Enabled	=	True
		
		dw_2.Object.mfco_horasa.Visible			=	False
		dw_2.Modify("planta_t.Text = 'Planta Origen'")

		//No Solicita Motivo 
		dw_2.Object.moti_codigo.Protect				=	1
		dw_2.Object.moti_codigo.BackGround.Color	= rgb(166,180,210)
		
		//No Solicita Productor 
		dw_2.Object.prod_codigo.visible				=	0
		dw_2.Object.prod_codigo.Protect				=	1
		dw_2.Object.prod_codigo.BackGround.Color	= rgb(166,180,210)
		dw_2.Object.prod_codigo_t.visible			=	0
				
		//Doc. Relacionado Guía de Recepción
		dw_2.Setitem(1,"mfco_tipdoc",1)
		dw_2.Object.mfco_tipdoc.Protect				=	1
		dw_2.Object.mfco_tipdoc.BackGround.Color	= rgb(166,180,210)
		
		//Invisible Doc. Relacionado
		dw_2.Object.mfco_docrel.visible				=	0
		dw_2.Object.mfco_docrel_t.visible			=	0
		
	END IF
ELSE
	
	IF is_Movto	=	22 THEN
		
		This.Title	=	"Despacho Otras Plantas"		
		dw_2.Object.mfco_horaen.Visible	=	False
		dw_2.Modify("planta_t.Text = 'Planta Destino'")

		//Si Solicita Motivo 
		dw_2.Object.moti_codigo.Protect				=	0
		dw_2.Object.moti_codigo.BackGround.Color	= RGB(255,255,255)
		
		//No Solicita Productor 
		dw_2.Object.prod_codigo.visible				=	0
		dw_2.Object.prod_codigo.Protect				=	1
		dw_2.Object.prod_codigo.BackGround.Color	= rgb(166,180,210)
		dw_2.Object.prod_codigo_t.visible			=	0
				
		//Doc. Relacionado Guía de Recepción
		dw_2.Setitem(1,"mfco_tipdoc",2)
		dw_2.Object.mfco_tipdoc.Protect				=	1
		dw_2.Object.mfco_tipdoc.BackGround.Color	= rgb(166,180,210)
		
		//Invisible Doc. Relacionado
		dw_2.Object.mfco_docrel.visible				=	0
		dw_2.Object.mfco_docrel_t.visible			=	0
		
	END IF

	IF is_Movto	=	23 THEN
		This.Title	=	"Devolución a Productores"
		
		//Si Solicita Motivo 
		dw_2.Object.moti_codigo.Protect				=	0
		dw_2.Object.moti_codigo.BackGround.Color	= RGB(255,255,255)
		
		//Si Solicita Productor 
		dw_2.Object.prod_codigo.visible				=	1
		dw_2.Object.prod_codigo.Protect				=	0
		dw_2.Object.prod_codigo.BackGround.Color	= RGB(255,255,255)
		dw_2.Object.prod_codigo_t.visible			=	1
		
		//No Solicita Planta Destino
		dw_2.Object.plde_coorde.visible				=	0
		dw_2.Object.plde_coorde.Protect				=	1
		dw_2.Object.plde_coorde.BackGround.Color	= rgb(166,180,210)
		dw_2.Object.planta_t.visible					=	0
				
		//Doc. Relacionado Guía de Recepción
		dw_2.Setitem(1,"mfco_tipdoc",2)
		dw_2.Object.mfco_tipdoc.Protect				=	1
		dw_2.Object.mfco_tipdoc.BackGround.Color	= rgb(166,180,210)
		
		//Invisible Doc. Relacionado
		dw_2.Object.mfco_docrel.visible				=	0
		dw_2.Object.mfco_docrel_t.visible			=	0
		
	END IF
	
	IF is_Movto	=	25 THEN
		
		This.Title	=	"Traspaso Re - Proceso"		
		dw_2.Object.mfco_horaen.Visible	=	False
		
		dw_2.Modify("planta_t.Text = 'Planta Destino'")

		//Noi Solicita Motivo 
		dw_2.Object.moti_codigo.visible				=	0
		dw_2.Object.moti_codigo.Protect				=	1
		dw_2.Object.moti_codigo.BackGround.Color	= rgb(166,180,210)
		
		//No Solicita Productor 
		dw_2.Object.prod_codigo.visible				=	0
		dw_2.Object.prod_codigo.Protect				=	1
		dw_2.Object.prod_codigo.BackGround.Color	= rgb(166,180,210)
		dw_2.Object.prod_codigo_t.visible			=	0
				
		//Doc. Relacionado 
		dw_2.Setitem(1,"mfco_tipdoc",5)
		dw_2.Object.mfco_tipdoc.Protect				=	1
		dw_2.Object.mfco_tipdoc.BackGround.Color	= rgb(166,180,210)
		
		//visible Doc. Relacionado
		dw_2.Object.mfco_docrel.visible				=	1
		dw_2.Object.mfco_docrel.Protect				=	0
		dw_2.Object.mfco_docrel.BackGround.Color	= RGB(255,255,255)
		dw_2.Object.mfco_docrel_t.visible			=	1
		
		dw_2.Object.plde_coorde.Protect				=	0
		dw_2.Object.plde_coorde.BackGround.Color	= RGB(255,255,255)
		
		//Invisible 
		dw_2.Object.mfco_guisii.visible				=	0
		dw_2.Object.mfco_guisii_t.visible			=	0
		dw_2.Object.tran_codigo.visible				=	0
		dw_2.Object.cami_patent.visible				=	0
		dw_2.Object.cami_patcar.visible				=	0
		dw_2.Object.mfco_rutcho.visible				=	0
		dw_2.Object.mfco_chofer.visible				=	0
		dw_2.Object.t_7.visible							=	0
		dw_2.Object.tran_codigo_t.visible			=	0
		dw_2.Object.cami_patent_t.visible			=	0
		dw_2.Object.cami_patcar_t.visible			=	0
		dw_2.Object.t_4.visible							=	0
		dw_2.Object.mfge_chofer_t.visible			=	0
		cb_guisii.Enabled = FALSE
		cb_guisii.visible = FALSE
	
	END IF
	
	IF is_Movto	=	34 THEN
		
		This.Title	=	"Mermas"
		
		//Si Solicita Motivo 
		dw_2.Object.moti_codigo.Protect				=	0
		dw_2.Object.moti_codigo.BackGround.Color	= RGB(255,255,255)
		
		//No Solicita Productor 
		dw_2.Object.prod_codigo.visible				=	0
		dw_2.Object.prod_codigo.Protect				=	1
		dw_2.Object.prod_codigo.BackGround.Color	= rgb(166,180,210)
		dw_2.Object.prod_codigo_t.visible			=	0
		
		//No Solicita Planta Destino
		dw_2.Object.plde_coorde.visible				=	0
		dw_2.Object.plde_coorde.Protect				=	1
		dw_2.Object.plde_coorde.BackGround.Color	= rgb(166,180,210)
		dw_2.Object.planta_t.visible					=	0
				
		//Doc. Relacionado Guía de Recepción
		dw_2.Setitem(1,"mfco_tipdoc",2)
		dw_2.Object.mfco_tipdoc.Protect				=	1
		dw_2.Object.mfco_tipdoc.BackGround.Color	= rgb(166,180,210)
		
		//Invisible Doc. Relacionado
		dw_2.Object.mfco_docrel.visible				=	0
		dw_2.Object.mfco_docrel_t.visible			=	0
	
		
	END IF
END IF
dw_2.SetReDraw(TRUE)
end subroutine

public function boolean retiraprod (long al_productor, integer ai_especie, integer ai_variedad, integer ai_categoria, date ad_fechalot);long ll_Productor

SELECT   prod_codigo
INTO		:ll_productor
FROM		dba.spro_pararetiroprod as prp
WHERE prp.prod_codigo	=: al_productor
	AND prp.espe_codigo	=: ai_especie
	AND prp.vari_codigo 	=: ai_variedad
	AND prp.cate_codigo =:	ai_categoria
	AND prp.prep_fecini	<:	ad_fechalot;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_lotesfrutacomdeta")

	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN
	
	RETURN False
END IF

RETURN True

end function

public subroutine agregaclientea_dws ();Integer li_i

FOR li_i = 1 TO dw_1.RowCount()
	dw_1.object.clie_codigo[li_i] 	=	dw_2.Object.clie_codigo[1]
NEXT

FOR li_i = 1 TO dw_2.RowCount()
	dw_2.object.clie_codigo[li_i] 	= 	dw_2.Object.clie_codigo[1]
NEXT

FOR li_i = 1 TO dw_3.RowCount()
	dw_3.object.clie_codigo[li_i] 	= 	dw_2.Object.clie_codigo[1]
NEXT

FOR li_i = 1 TO dw_4.RowCount()
	dw_4.object.clie_codigo[li_i] 	= 	dw_2.Object.clie_codigo[1]
NEXT
end subroutine

public function boolean valida_password ();Boolean 	lb_retorno = TRUE
str_mant	lstr_mant

lstr_mant.Argumento[1]	=	"Comercial"
lstr_mant.Argumento[2]	=	gstr_paramplanta.Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN lb_retorno = FALSE

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
	FROM dba.prodconectividad   
	WHERE cone_codigo = :il_coneccion;

sqlexi.ServerName	=	ls_nomser
sqlexi.DataBase	=	ls_nombas
sqlexi.Dbms			= 	ls_nodbms
sqlexi.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
CONNECT USING sqlexi;

IF sqlexi.SQLCode = 0 THEN
	ib_ConectadoExistencia	=	True
ELSE
	ib_ConectadoExistencia	=	False
END IF

RETURN ib_ConectadoExistencia

end function

event open;/* 
Argumentos
istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Tipo de Movimiento
istr_Mant.Argumento[3]	=	Número de Movimiento
istr_Mant.Argumento[4]	=	Sentido del Movimiento
istr_Mant.Argumento[5]	=	Codigo Especie para Movimiento Traspaso Re-Proceso
istr_Mant.Argumento[6]	=	Codigo Productor // 
istr_Mant.argumento[16] =  Codigo Cliente
*/

DataWindowChild	ldwc_Camara, ldwc_TipoEnvase, ldwc_PltaLote, ldwc_Especie
String				ls_Movto
Integer				li_TipoMovto, li_Movto

dw_2.SetReDraw(FALSE)

x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

dw_3	=	tab_1.tp_1.dw_detalle
dw_4	=	tab_1.tp_2.dw_envases

ls_Movto	= " "//Message.StringParm

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[6]	=	""

IF ls_Movto = "" THEN
	ib_predefinido	=	False
ELSE
	istr_Mant.Argumento[2]	=	"19"//Mid(ls_Movto,2,2)
	istr_Mant.Argumento[4]	=	"6"//Mid(ls_Movto,1,2)

	li_TipoMovto				=	Integer(istr_Mant.Argumento[4])
	li_Movto						=	Integer(istr_Mant.Argumento[2])
	ib_Predefinido				=	True

	IF IsNull(li_TipoMovto) = False THEN
		SELECT tpmv_codigo
			INTO :ii_Movto
			FROM	dba.spro_tipomovtofruta
			WHERE	tpmv_codigo = :li_Movto ;

		IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de Tabla Tipos de Movimiento de Fruta")
			
		ELSEIF sqlca.SQLCode = 100 THEN
			MessageBox("Atención", "Código de Tipo Movimiento de Fruta no ha sido definido", &
				Exclamation!, OK!)
				
		END IF	
	END IF
END IF

pb_nuevo.PostEvent(Clicked!)

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)

dw_2.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(sqlca)
idwc_productores.Retrieve()

dw_2.GetChild("plde_coorde", idwc_PltaDest)
idwc_PltaDest.SetTransObject(sqlca)

IF idwc_PltaDest.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Plantas")
	idwc_PltaDest.InsertRow(0)
ELSE
	IF integer(istr_mant.argumento[2])<>25 THEN
		idwc_PltaDest.SetFilter("plde_codigo <> " + &
										String(gstr_ParamPlanta.CodigoPlanta))
		idwc_PltaDest.Filter()
	END IF	
	idwc_PltaDest.SetSort("plde_nombre A")
	idwc_PltaDest.Sort()
END IF

dw_2.GetChild("tran_codigo", idwc_Transp)
idwc_Transp.SetTransObject(sqlca)

IF idwc_Transp.Retrieve() < 1 THEN
	MessageBox("Atención", "Falta Registrar Transportistas")
	idwc_Transp.InsertRow(0)
ELSE
	idwc_Transp.SetSort("tran_nombre A")
	idwc_Transp.Sort()
END IF

dw_3.GetChild("cama_codigo", ldwc_Camara)
ldwc_Camara.SetTransObject(sqlca)

IF ldwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	MessageBox("Atención", "Falta Registrar Cámaras para Planta " + &
					String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	ldwc_Camara.InsertRow(0)
ELSE
	ldwc_Camara.SetSort("cama_nombre A")
	ldwc_Camara.Sort()
END IF

dw_3.GetChild("lote_pltcod", ldwc_PltaLote)
ldwc_PltaLote.SetTransObject(sqlca)
ldwc_PltaLote.Retrieve()

dw_3.GetChild("lofc_espcod", ldwc_Especie)
ldwc_Especie.SetTransObject(sqlca)
ldwc_Especie.Retrieve(gstr_parempresa.empr_codexp)

dw_4.GetChild("enva_tipoen", ldwc_TipoEnvase)
ldwc_TipoEnvase.SetTransObject(sqlca)

IF ldwc_TipoEnvase.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Envases ")
	ldwc_TipoEnvase.InsertRow(0)
ELSE
	ldwc_TipoEnvase.SetSort("tien_nombre A")
	ldwc_TipoEnvase.Sort()
END IF

dw_4.GetChild("cama_codigo", ldwc_Camara)
ldwc_Camara.SetTransObject(sqlca)

IF ldwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	MessageBox("Atención", "Falta Registrar Cámaras para Planta " + &
					String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	ldwc_Camara.InsertRow(0)
ELSE
	ldwc_Camara.SetSort("cama_nombre A")
	ldwc_Camara.Sort()
END IF

dw_1.SetTransObject(sqlca) // Encabezado Movimiento de Envase
dw_2.SetTransObject(sqlca) // Encabezado Movimiento de Fruta Comercial
dw_3.SetTransObject(sqlca) // Detalle de Movimiento de Fruta Comercial
dw_4.SetTransObject(sqlca) // Detalle de Movimiento de Envase
dw_5.SetTransObject(sqlca) // Encabezado Lotes Fruta Comercial
dw_6.SetTransObject(sqlca) // Detalle de Lotes Fruta Comercial

dw_3.Modify("datawindow.message.title='Error '+ is_titulo")
dw_3.SetRowFocusIndicator(Hand!)
dw_3.Modify("DataWindow.Footer.Height = 88")

dw_4.Modify("datawindow.message.title='Error '+ is_titulo")
dw_4.SetRowFocusIndicator(Hand!)
dw_4.Modify("DataWindow.Footer.Height = 88")

istr_Mant.dw				=	dw_1
istr_Mant.solo_consulta =	False

iuo_PltaDestino			=	Create uo_plantadesp
iuo_Productor				=	Create uo_Productores
iuo_Transport				=	Create uo_transportista
iuo_Camion					=	Create uo_camiones
iuo_TipoDocto				=	Create uo_tipodoctoplanta
iuo_ClienProve				=	Create uo_clienprove
iuo_LotesFrutaComer		=	Create uo_lotesfrutacomer
iuo_clientes				=	Create uo_clientesprod
iuo_TipoMovtoFruta		=	Create uo_tipomovtofruta		
iuo_TipoMovtoEnva			=  Create uo_tipomovtofruta		
iuo_calicosechero			=  Create uo_calicosechero  

end event

event ue_borra_detalle;call super::ue_borra_detalle;SetPointer(HourGlass!)
str_mant		lstr_mant

ib_Borrar				=	True
Message.DoubleParm	=	0
istr_mant.Borra		=	True
istr_mant.Agrega		=	False

w_main.SetMicroHelp("Validando la eliminación de detalle...")

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF tab_1.SelectedTab = 1 THEN
	istr_Mant.dw	=	dw_3
	
	IF NOT gstr_paramplanta.binsabins AND NOT gstr_paramplanta.bultobins THEN
		OpenWithParm(iw_mantencion_1, istr_mant)
	ELSE
		lstr_Mant.dw	=	dw_3
		lstr_Mant.Argumento[1]		=	String(dw_2.Object.plde_codigo[1])
		lstr_Mant.Argumento[2]		=	istr_Mant.Argumento[9]
		lstr_Mant.Argumento[3]		=	istr_Mant.Argumento[10]
		lstr_Mant.Argumento[4]		=	'2'
		lstr_Mant.Argumento[9]		=	'7'
		lstr_Mant.Argumento[10]		=	String(dw_2.Object.mfco_guisii[1])
		lstr_Mant.Argumento[11]		=	'devolucion'
		lstr_Mant.Argumento[13]		=	String(dw_2.Object.mfco_fecmov[1], 'dd/mm/yyyy')
		lstr_Mant.Argumento[14]		=	String(dw_3.GetRow())
		lstr_Mant.Argumento[20]		=	String(dw_2.Object.tipo_ingdat[1])

		OpenWithParm(w_mant_deta_movtofrutcomer_despachoventa, lstr_mant)
	END IF
	
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
			dw_2.Object.mfco_totbul[1]	=	0
			dw_2.Object.mfco_tpneto[1]	=	0
		ELSE
			dw_2.Object.mfco_totbul[1]	=	Round(dw_3.Object.total_bultos[1], 0)
			dw_2.Object.mfco_tpneto[1]	=	Round(dw_3.Object.total_kilos[1], 3)
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

event ue_nuevo_detalle;call super::ue_nuevo_detalle;str_mant		lstr_mant

istr_mant.Borra	=	False
istr_mant.Agrega	=	True

habilitaencab(False)

IF tab_1.SelectedTab = 1 THEN
	istr_mant.dw	=	dw_3

	istr_Mant.Argumento[7] = "0"  //Envases con Fruta 
	istr_mant.argumento[9]   = String(dw_2.Object.mfco_tipdoc[1])	
	istr_mant.argumento[10]  = String(dw_2.Object.mfco_docrel[1])
	istr_mant.argumento[13]  = String(dw_2.Object.mfco_fecmov[1])

	IF NOT gstr_paramplanta.binsabins AND NOT gstr_paramplanta.bultobins THEN
		OpenWithParm(iw_mantencion_1, istr_mant)
	ELSE
		istr_mant.Argumento[1]		=	String(dw_2.Object.plde_codigo[1])
		istr_mant.Argumento[2]		=	istr_Mant.Argumento[2]
		istr_mant.Argumento[3]		=	istr_Mant.Argumento[10]
		istr_mant.Argumento[4]		=	'2'
		istr_mant.Argumento[9]		=	'7'
		istr_mant.Argumento[10]		=	String(dw_2.Object.mfco_guisii[1])
		istr_mant.Argumento[11]		=	'devolucion'
		istr_mant.Argumento[13]		=	String(dw_2.Object.mfco_fecmov[1], 'dd/mm/yyyy')
		istr_mant.Argumento[20]		=	String(dw_2.Object.tipo_ingdat[1])
		
		OpenWithParm(w_mant_deta_movtofrutcomer_despachoventa, istr_mant)
	END IF
	
	IF dw_3.RowCount() > 0 THEN
		dw_2.Object.mfco_totbul[1]	=	Round(dw_3.Object.total_bultos[1], 0)
		dw_2.Object.mfco_tpneto[1]	=	Round(dw_3.Object.total_kilos[1], 3)
		pb_eli_det.Enabled			=	True
	ELSE
		dw_2.Object.mfco_totbul[1]	=	0
		dw_2.Object.mfco_tpneto[1]	=	0
		pb_eli_det.Enabled			=	False
	END IF
ELSE
	istr_mant.dw	=	dw_4
	istr_mant.Argumento[16]			= String(dw_2.Object.clie_codigo[1])
	
	OpenWithParm(iw_mantencion_2, istr_mant)

	IF dw_4.RowCount() > 0 THEN
		pb_eli_det.Enabled			=	True
	ELSE
		pb_eli_det.Enabled			=	False
	END IF
END IF

IF dw_3.RowCount() > 0 AND dw_4.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_3.RowCount() > 0 AND dw_4.RowCount() > 0 AND Not pb_eliminar.Enabled THEN
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
ELSEIF istr_mant.argumento[2]="25" AND dw_3.RowCount() > 0 AND Not pb_eliminar.Enabled THEN
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
END IF

IF tab_1.SelectedTab = 1 THEN
	dw_3.SetRow(il_Fila)
	dw_3.SelectRow(il_Fila, True)
ELSE
	dw_4.SetRow(il_Fila)
	dw_4.SelectRow(il_Fila, True)
END IF
end event

event ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta, ll_Numero
Integer	li_TipoMovto, li_planta, li_dw3

li_Planta			=	Integer(istr_Mant.Argumento[1])
li_TipoMovto	=	Integer(istr_Mant.Argumento[2])
ll_Numero		=	Long(istr_Mant.Argumento[3])
ii_cliente			= dw_2.Object.clie_codigo[1]

DO
	dw_2.SetRedraw(False)

	ll_fila_e	=	dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), ii_cliente,&
										  Integer(istr_Mant.Argumento[2]), &
										  Long(istr_Mant.Argumento[3]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila_e < 1 THEN 
			TriggerEvent("ue_nuevo")
			RETURN  
	ELSE
		IF NOT gstr_paramplanta.binsabins AND NOT gstr_paramplanta.bultobins THEN
			li_dw3= dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), &
										  Integer(istr_Mant.Argumento[2]), &
										  Long(istr_Mant.Argumento[3]))
		ELSE
			li_dw3= dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), &
										  Integer(istr_Mant.Argumento[2]), &
										  Long(istr_Mant.Argumento[3]),&
										  ii_cliente)
		END IF					
		DO
			IF li_dw3 = -1 OR &
				dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  li_TipoMovto, &
								  ll_Numero) = -1 OR &
				dw_4.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  li_TipoMovto, &
								  ll_Numero, &
								  Integer(istr_Mant.Argumento[4]), ii_cliente) = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", &
												"No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				
				tab_1.tp_1.Enabled	=	True
				
            IF istr_mant.argumento[2]<>"25" THEN
					tab_1.tp_2.Enabled	=	True
				ELSEIF istr_mant.argumento[2]="25" AND dw_2.Object.mfco_estmov[1]<>1 THEN
					tab_1.tp_2.Enabled	=	TRUE
					pb_eliminar.Enabled	=	FALSE
					pb_ins_det.Enabled	=	FALSE
					pb_eli_det.Enabled	=	FALSE
				END IF
				
				IF dw_2.Object.mfco_estmov[1]<>1 THEN
					istr_Mant.solo_consulta =	TRUE
					IF isnull(dw_2.Object.mfco_guisii[1]) OR dw_2.Object.mfco_guisii[1]=0 THEN
						cb_guisii.Enabled = TRUE
					ELSE
						cb_guisii.Enabled = FALSE
					END IF	
				ELSE
					istr_Mant.solo_consulta =	FALSE
				END IF	
				
				pb_eliminar.Enabled	=	True//NOT istr_Mant.solo_consulta
				pb_grabar.Enabled	=	True
				pb_imprimir.Enabled	=	True
				pb_ins_det.Enabled	=	NOT istr_Mant.solo_consulta
				pb_eli_det.Enabled	=	NOT istr_Mant.solo_consulta
				
				IF li_TipoMovto = 25 THEN
					IF not existeorden(dw_2.Object.plde_codigo[1],dw_2.Object.mfco_tipdoc[1],dw_2.Object.mfco_docrel[1]) THEN
						messagebox("Error de Relación","Documento Relacionado no existe")
					END IF	
				END IF	
												
				HabilitaEncab(False)
								
				dw_1.SetRow(1)
				dw_1.SelectRow(1,True)
				dw_1.SetFocus()
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_maed_movtofrutacomenca_reproceso.create
int iCurrent
call super::create
this.tab_1=create tab_1
this.dw_6=create dw_6
this.dw_5=create dw_5
this.cb_guisii=create cb_guisii
this.cb_capturadatos=create cb_capturadatos
this.dw_exideta=create dw_exideta
this.dw_exiencab=create dw_exiencab
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
this.Control[iCurrent+2]=this.dw_6
this.Control[iCurrent+3]=this.dw_5
this.Control[iCurrent+4]=this.cb_guisii
this.Control[iCurrent+5]=this.cb_capturadatos
this.Control[iCurrent+6]=this.dw_exideta
this.Control[iCurrent+7]=this.dw_exiencab
end on

on w_maed_movtofrutacomenca_reproceso.destroy
call super::destroy
destroy(this.tab_1)
destroy(this.dw_6)
destroy(this.dw_5)
destroy(this.cb_guisii)
destroy(this.cb_capturadatos)
destroy(this.dw_exideta)
destroy(this.dw_exiencab)
end on

event ue_modifica_detalle;str_mant		lstr_mant

istr_mant.agrega	=	False
istr_mant.borra	=	False

IF tab_1.SelectedTab = 1 THEN
	IF dw_3.RowCount() > 0 THEN
	istr_mant.dw		=	dw_3

	istr_Mant.Argumento[7] 		= 	"0"  //Envases con Fruta 
	istr_mant.argumento[9]   	=	String(dw_2.Object.mfco_tipdoc[1])	
	istr_mant.argumento[10]  	=	String(dw_2.Object.mfco_docrel[1])
	
	IF NOT gstr_paramplanta.binsabins AND NOT gstr_paramplanta.bultobins THEN
		OpenWithParm(iw_mantencion_1, istr_mant)
	ELSE
		istr_mant.Argumento[1]		=	String(dw_2.Object.plde_codigo[1])
		istr_mant.Argumento[2]		=	istr_Mant.Argumento[9]
		istr_mant.Argumento[3]		=	istr_Mant.Argumento[10]
		istr_mant.Argumento[4]		=	'2'
		istr_mant.Argumento[9]		=	'7'
		istr_mant.Argumento[10]		=	String(dw_2.Object.mfco_guisii[1])
		istr_mant.Argumento[11]		=	'devolucion'
		istr_mant.Argumento[12]		=	'0'
		istr_mant.Argumento[13]		=	String(dw_2.Object.mfco_fecmov[1], 'dd/mm/yyyy')
		istr_mant.Argumento[20]		=	String(dw_2.Object.tipo_ingdat[1])
		
		OpenWithParm(w_mant_deta_movtofrutcomer_despachoventa, istr_mant)
	END IF

		dw_2.Object.mfco_totbul[1]	=	Round(dw_3.Object.total_bultos[1], 0)
		dw_2.Object.mfco_tpneto[1]	=	Round(dw_3.Object.total_kilos[1], 3)
	ELSE
		dw_2.Object.mfco_totbul[1]	=	0
		dw_2.Object.mfco_tpneto[1]	=	0
	END IF
ELSE
	IF dw_4.RowCount() > 0 THEN
		istr_mant.dw		=	dw_4

		OpenWithParm(iw_mantencion_2, istr_mant)
	END IF
END IF
end event

event ue_nuevo;is_rut 			= ""
is_rutcliente	= ""

Call Super::ue_nuevo

dw_3.Reset()
dw_4.Reset()

iuo_clientes.existe(gi_codexport, false, sqlca)

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.tpmv_codigo[1]	=	Integer(istr_Mant.Argumento[2])
dw_2.Object.mfco_fecmov[1]	=	Date(Mid(String(Today()),1,10))

dw_2.object.clie_codigo[1] 		= 	iuo_clientes.Codigo
ii_cliente								=	iuo_clientes.Codigo
modificaencab(integer(istr_mant.Argumento[2]))

IF istr_Mant.Argumento[4] = "1" THEN
	dw_2.Object.mfco_horasa[1]			=	Time(Mid(String(Today(),'dd/mm/yyyy hh:mm:ss'),12,8))
	dw_2.Object.mfco_horaen.Protect	=	0
	dw_2.Object.mfco_horaen.Visible	=	1
	dw_2.Object.mfco_horasa.Protect	=	1
	dw_2.Object.mfco_horasa.Visible	=	0
ELSE
	dw_2.Object.mfco_horasa[1]			=	Time(Mid(String(Today(),'dd/mm/yyyy hh:mm:ss'),12,8))
	dw_2.Object.mfco_horasa.Protect	=	0
	dw_2.Object.mfco_horasa.Visible	=	1
	dw_2.Object.mfco_horaen.Protect	=	1
	dw_2.Object.mfco_horaen.Visible	=	0
END IF

dw_2.Object.tipo_ingdat[1]		=	1

istr_Mant.Argumento[6] = ""
dw_2.Object.mfco_estmov[1]	=	1
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_Fila 
Integer	li_Secuencia, li_Planta, li_TipoMovto, li_TipoMovtoEnva
Boolean  lb_Actualiza_Envase = False, lb_Actualiza_Fruta = False

ib_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

li_Planta						=	dw_2.Object.plde_codigo[1]
istr_Mant.Argumento[1]	= 	String(li_Planta)
li_TipoMovto				=	dw_2.Object.tpmv_codigo[1]
istr_Mant.Argumento[2] 	= 	String(li_TipoMovto)

IF Integer(istr_Mant.Argumento[4]) = 1 THEN
	IF li_TipoMovto = 2 THEN
		li_TipoMovtoEnva 	=	42
	END IF
ELSE
	IF li_TipoMovto = 22 OR li_TipoMovto = 34 THEN
		li_TipoMovtoEnva 	=	62
	ELSEIF li_TipoMovto = 23 THEN
		li_TipoMovtoEnva 	=	61
	ELSEIF li_TipoMovto = 25 OR li_TipoMovto = 26 THEN
		li_TipoMovtoEnva	=	65
	END IF
END IF

IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
	
		IF il_NumEnva=0 THEN
		
		   iuo_TipoMovtoEnva.bloqueacorrel()	
		   il_NumEnva = iuo_TipoMovtoEnva.UltimoCorrelativo(5,li_TipoMovtoEnva,li_Planta) 
	
		   IF il_NumEnva = 0 OR IsNull(il_NumEnva) THEN
			  Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
			  Message.DoubleParm = -1
		     RETURN
		   ELSE
			  lb_Actualiza_Envase = TRUE
		   END IF
	    END IF
	
	   IF il_NumFruta=0 THEN
		  iuo_TipoMovtoFruta.bloqueacorrel()
		  il_NumFruta = iuo_TipoMovtoFruta.UltimoCorrelativo(2,li_TipoMovto,li_Planta) 
	
		  IF il_NumFruta = 0 OR IsNull(il_NumFruta) THEN
			 Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
			 Message.DoubleParm = -1
			 RETURN
		  ELSE
			 lb_Actualiza_Fruta = TRUE	
		  END IF
      END IF


	dw_2.Object.mfco_numero[1]		=	il_NumFruta
	dw_2.Object.mfco_estmov[1]		=	3
	
	istr_Mant.Argumento[3] 				= 	String(il_NumFruta)
	
	IF li_TipoMovto = 25 THEN
		dw_2.Object.mfco_estmov[1]	= 1
	END IF	
	
	ll_Fila	=	dw_1.InsertRow(0)

	dw_1.Object.plde_codigo[ll_Fila]		=	dw_2.Object.plde_codigo[1]
	dw_1.Object.tpmv_codigo[ll_Fila]		=	li_TipoMovtoEnva
	dw_1.Object.meen_numero[ll_Fila]	=  il_NumEnva
	dw_1.Object.plde_coorde[ll_Fila]		=	dw_2.Object.plde_coorde[1]
	dw_1.Object.prod_codigo[ll_Fila]		=	dw_2.Object.prod_codigo[1]

	dw_1.Object.meen_modulo[ll_Fila]	=	2
	
	dw_1.Object.meen_guisii[ll_Fila]		=	dw_2.Object.mfco_guisii[1]
	dw_1.Object.meen_fecmov[ll_Fila]	=	dw_2.Object.mfco_fecmov[1]
	dw_1.Object.tran_codigo[ll_Fila]		=	dw_2.Object.tran_codigo[1]
	dw_1.Object.cami_clasifi[ll_Fila]		=	dw_2.Object.cami_clasifi[1]
	dw_1.Object.cami_patent[ll_Fila]		=	dw_2.Object.cami_patent[1]
	dw_1.Object.cami_patcar[ll_Fila]		=	dw_2.Object.cami_patcar[1]
	dw_1.Object.meen_rutcho[ll_Fila]		=	dw_2.Object.mfco_rutcho[1]
	dw_1.Object.meen_chofer[ll_Fila]		=	dw_2.Object.mfco_chofer[1]
	
	dw_1.Object.tpmv_codrec[ll_fila] 		=  li_TipoMovto
	dw_1.Object.mfge_numero[ll_fila] 	=  il_NumFruta
	
 	//Preguntar el Momento de Actualización
	IF lb_Actualiza_Fruta  THEN iuo_TipoMovtoFruta.Actualiza_Correlativo(2,li_Planta,li_TipoMovto,il_NumFruta) 
  	IF lb_Actualiza_Envase THEN iuo_TipoMovtoEnva.Actualiza_Correlativo(4,li_Planta,li_TipoMovtoEnva,il_NumEnva) 
   ///////////////////////////////////////
	
ELSE
	
	il_NumFruta						=	dw_2.Object.mfco_numero[1]
	istr_Mant.Argumento[3] 		= 	String(il_NumFruta)
	
END IF

SELECT	IsNull(Max(mfcd_secuen), 0) + 1
	INTO	:li_Secuencia
	FROM	dba.spro_movtofrutacomdeta
	WHERE	plde_codigo	=	:li_Planta
	AND	tpmv_codigo	=	:li_TipoMovto
	AND	mfco_numero	=	:il_NumFruta ;

FOR ll_Fila = 1 TO dw_3.RowCount()
	IF dw_3.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_3.Object.plde_codigo[ll_Fila]		=	dw_2.Object.plde_codigo[1]
		dw_3.Object.tpmv_codigo[ll_Fila]		=	dw_2.Object.tpmv_codigo[1]
		dw_3.Object.mfco_numero[ll_Fila]	=	dw_2.Object.mfco_numero[1]
		dw_3.Object.mfcd_secuen[ll_Fila]		=	li_Secuencia

		li_Secuencia ++
	END IF
NEXT

FOR ll_Fila = 1 TO dw_4.RowCount()
	IF dw_4.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_4.Object.plde_codigo[ll_Fila]		=	dw_2.Object.plde_codigo[1]
		dw_4.Object.tpmv_codigo[ll_Fila]		=	li_TipoMovtoEnva
		dw_4.Object.meen_numero[ll_Fila]	=	dw_1.Object.meen_numero[1]
		
	END IF
NEXT

FOR ll_Fila = 1 TO dw_5.RowCount()
	IF dw_5.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_5.Object.lofc_pltcod[ll_Fila]		=	dw_2.Object.plde_codigo[1]
	END IF
NEXT

FOR ll_Fila = 1 TO dw_6.RowCount()
	IF dw_6.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_6.Object.lofc_pltcod[ll_Fila]		=	dw_2.Object.plde_codigo[1]
	END IF
NEXT

IF li_tipoMovto<>25 THEN
	IF dw_2.Object.mfco_guisii[1]=0 OR isnull(dw_2.Object.mfco_guisii[1]) THEN
		cb_guisii.Enabled 	= TRUE
		cb_guisii.visible 	= TRUE
	END IF
END IF

agregaclientea_dws()
end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar		=	True
ib_AutoCommit	=	sqlca.AutoCommit

w_main.SetMicroHelp("Validando la eliminación...")

IF NOT valida_password() THEN
	MessageBox("Error", "No es posible anular el movimiento, ya que no posee el password correspondiente")
	Return
END IF

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount() > 0 THEN
	dw_3.RowsMove(1, dw_3.RowCount(), Primary!, dw_3, 1, Delete!)
END IF

IF dw_4.RowCount() > 0 THEN
	dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
END IF

IF dw_1.DeleteRow(0) = 1 THEN
		ib_Borrar	=	False
		
		w_main.SetMicroHelp("Anulando Registro...")
		
		dw_2.Object.mfco_estmov[1]	=	0
		
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_Borrar	=	False
	
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF
end event

event ue_seleccion;call super::ue_seleccion;Str_Busqueda	lstr_Busq

lstr_Busq.Argum[1] = istr_Mant.Argumento[1]		// Código Planta
lstr_Busq.Argum[2] = istr_Mant.Argumento[2]		// Tipo de Movimiento
lstr_Busq.Argum[3] = ''									// Estado Movimiento
lstr_Busq.Argum[4] = ''  								// Fecha Inicio Movimiento

OpenWithParm(w_busc_movtofrutacomenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	istr_Mant.Argumento[1]		=	lstr_Busq.Argum[1]
	istr_Mant.Argumento[2]		=	lstr_Busq.Argum[2]
	istr_Mant.Argumento[3]		=	lstr_Busq.Argum[3]
	ii_cliente							=	Integer(lstr_Busq.Argum[12])
	dw_2.Object.clie_codigo[1]	=	ii_cliente
	
	This.TriggerEvent("ue_recuperadatos")
END IF
end event

event ue_guardar;call super::ue_guardar;Integer	li_cliente

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
	
	li_Cliente = dw_2.Object.clie_codigo[1]
	
	SELECT clie_conexi, cone_codigo
	INTO :il_conexiste, :il_coneccion
	FROM dba.clientesprod
	WHERE clie_codigo = :li_Cliente;
	
	IF il_conexiste = 1 THEN
		sqlexi	=	CREATE Transaction
		
		IF Conexionexistencia() THEN
			dw_exideta.SetTransObject(sqlexi)
			dw_exiencab.SetTransObject(sqlexi)		
			TriggerEvent("ue_despuesguardar")
		ELSE
			MessageBox("Atención", "No puede Despachar.~r~r" + &
						"Falló Conexion con Existencia.", Exclamation!, Ok!)

			RETURN 
		END IF
	END IF	
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF


IF il_NumFruta>0 AND dw_2.GetItemStatus(1, 0, Primary!) = NotModified! THEN
	il_NumFruta = 0
	il_NumEnva	= 0
END IF



end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutacomenca_reproceso
boolean visible = false
integer x = 197
integer y = 2172
integer width = 3049
integer height = 844
string title = ""
string dataobject = "dw_mant_movtoenvaenca_comercial"
boolean hscrollbar = false
boolean vscrollbar = false
boolean resizable = true
boolean livescroll = false
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutacomenca_reproceso
integer x = 37
integer y = 32
integer width = 3095
integer height = 860
integer taborder = 10
string dataobject = "dw_mant_movtofrutacomenca_movtos"
end type

event dw_2::itemchanged;String	ls_Nula
String	ls_Columna
Integer	li_ClasifCamion
long ll_prod_cod

ls_Columna = GetColumnName()
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
	CASE "clie_codigo"
		IF NOT iuo_clientes.Existe(Integer(data), true, sqlca) THEN
			This.SetItem(1,"mfco_numero",Long(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSE
			RETURN 0
			ii_cliente	=	Integer(data)
		END IF
		
	CASE "mfco_numero"
		IF NOT ExisteMovimiento(gstr_ParamPlanta.CodigoPlanta, &
									   Integer(istr_Mant.Argumento[2]), &
									   Long(Data)) THEN
			This.SetItem(1,"mfco_numero",Long(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSE
			RETURN 0
		END IF

	CASE "plde_coorde"
		IF Not iuo_PltaDestino.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, ls_Nula)
			
			RETURN 1
		ELSE
			IF integer(istr_mant.argumento[2])<>25 THEN
				IF iuo_PltaDestino.Codigo = gstr_ParamPlanta.CodigoPlanta THEN
					MessageBox("Atención", "No puede Despachar a la Planta de Origen.~r~r" + &
						"Ingrese o seleccione otra Planta de Destino.", Exclamation!, Ok!)
			
					RETURN 1
				END IF
			END IF	
		END IF

	CASE "prod_codigo"
		
		ll_prod_cod = long(data)
		IF Not iuo_Productor.Existe(ll_prod_cod, True, sqlca) THEN
			This.SetItem(1, ls_Columna, integer(ls_Nula))
			
			RETURN 1
		ELSE
			istr_Mant.Argumento[6] = Data
		END IF
		
	CASE "tran_codigo"
		IF Not iuo_Transport.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, ls_Nula)
			
			RETURN 1
		END IF

	CASE "cami_patent"
		
		IF Integer(istr_Mant.Argumento[2]) = 2 OR Integer(istr_Mant.Argumento[2]) = 22 THEN
			li_ClasifCamion	=	2
		ELSE
			li_ClasifCamion	=	1
		END IF

		IF Not iuo_Camion.Existe(li_ClasifCamion, Data, False, sqlca) THEN
			This.Object.cami_patcar[1]		=	ls_Nula
			This.Object.mfco_rutcho[1]		=	ls_Nula
			This.Object.mfco_chofer[1]		=	ls_Nula
		ELSE
			This.Object.cami_patcar[1]		=	iuo_Camion.PateCarro
			This.Object.mfco_rutcho[1]		=	iuo_Camion.RutChofer
			This.Object.mfco_chofer[1]		=	iuo_Camion.Chofer
			This.Object.cami_clasifi[1]	=	iuo_Camion.Clasificacion
			is_rut  = iuo_Camion.RutChofer
		END IF

	CASE "defg_tipdoc"
		IF Not iuo_TipoDocto.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, ls_Nula)
			
			RETURN 1
		END IF

	CASE "mfco_docrel"
		IF NOT ExisteOrden(gstr_ParamPlanta.CodigoPlanta, &
								 dw_2.Object.mfco_tipdoc[1], Long(Data)) THEN
			This.SetItem(1,"mfco_docrel",Long(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSEIF ExisteMovimientoDoc(dw_2.Object.mfco_tipdoc[1], Long(Data)) THEN
					Parent.Triggerevent("ue_recuperadatos")
		END IF

	CASE "mfco_rutcho"
		is_rut = F_verrut(data, True)
		IF is_rut = "" THEN
			dw_2.SetItem(1, "mfco_rutcho", ls_Nula)
			RETURN 1
		ELSE
			This.SetItem(1, "mfco_rutcho", is_rut)
		END IF

	
END CHOOSE

HabilitaIngreso(ls_Columna)
end event

event dw_2::doubleclicked;//
end event

event dw_2::itemfocuschanged;call super::itemfocuschanged;IF is_rut <> "" THEN
	IF Len(is_rut) < 10 THEN
		This.Object.mfco_rutcho.Format = '@@@@@@'
	ELSE
		This.Object.mfco_rutcho.Format = '@@@.@@@.@@@-@'
	END IF
	
	IF dwo.Name <> "mfco_rutcho" THEN
		This.SetItem(1, "mfco_rutcho", is_rut)
	END IF
END IF

IF is_rutcliente <> "" THEN
	IF Len(is_rutcliente ) < 10 THEN
		This.Object.clpr_rut.Format = '@@@@@@'
	ELSE
		This.Object.clpr_rut.Format = '@@@.@@@.@@@-@'
	END IF
	
	IF dwo.Name <> "clpr_rut" THEN
		This.SetItem(1, "clpr_rut", is_rutcliente )
	END IF
END IF
end event

event dw_2::clicked;CHOOSE CASE dwo.Name
	CASE "b_camion"
		BuscaCamion()
		
	CASE "b_docrel"
		Buscaorden()

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutacomenca_reproceso
integer x = 3287
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(TRUE)
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutacomenca_reproceso
integer x = 3287
integer taborder = 70
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutacomenca_reproceso
integer x = 3287
integer y = 632
integer taborder = 80
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutacomenca_reproceso
integer x = 3287
integer taborder = 90
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutacomenca_reproceso
integer x = 3287
integer taborder = 100
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutacomenca_reproceso
integer x = 3287
integer y = 1504
integer taborder = 30
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutacomenca_reproceso
integer x = 3287
integer y = 1680
integer taborder = 40
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutacomenca_reproceso
integer x = 3287
integer taborder = 50
end type

type tab_1 from tab within w_maed_movtofrutacomenca_reproceso
event create ( )
event destroy ( )
integer x = 41
integer y = 928
integer width = 3095
integer height = 964
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 30586022
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
		IF dw_2.Object.tpmv_codigo[1] <> 25 THEN
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
		IF dw_2.Object.tpmv_codigo[1] <> 25 THEN
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
integer width = 3058
integer height = 836
boolean enabled = false
long backcolor = 30586022
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
integer x = 37
integer y = 36
integer width = 2981
integer height = 776
integer taborder = 11
string dataobject = "dw_mues_movtofrutacomdeta_movto"
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

event doubleclicked;call super::doubleclicked;IF Row > 0 THEN
	il_fila = row
	This.SetRow(il_fila)
	w_maed_movtofrutacomenca.TriggerEvent("ue_modifica_detalle")
END IF
RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutacomenca.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutacomenca.PostEvent("ue_seteafila")

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
integer width = 3058
integer height = 836
boolean enabled = false
long backcolor = 12632256
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
integer width = 2981
integer height = 776
integer taborder = 10
string dataobject = "dw_mues_movtoenvadeta"
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

event doubleclicked;call super::doubleclicked;w_maed_movtofrutacomenca.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutacomenca.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutacomenca.PostEvent("ue_seteafila")

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

type dw_6 from datawindow within w_maed_movtofrutacomenca_reproceso
boolean visible = false
integer x = 41
integer y = 2272
integer width = 3054
integer height = 368
integer taborder = 70
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle Lote"
string dataobject = "dw_gene_spro_lotesfrutacomdeta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_maed_movtofrutacomenca_reproceso
boolean visible = false
integer x = 18
integer y = 2148
integer width = 3054
integer height = 260
integer taborder = 10
boolean bringtotop = true
boolean titlebar = true
string title = "Encabezado Lote"
string dataobject = "dw_gene_spro_lotesfrutacomenc"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type cb_guisii from commandbutton within w_maed_movtofrutacomenca_reproceso
integer x = 3227
integer y = 1204
integer width = 279
integer height = 112
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Guía SII"
end type

event clicked;Str_mant lstr_mant

IF Isnull(dw_2.Object.mfco_guisii[1]) OR dw_2.Object.mfco_guisii[1] = 0 THEN
	
//	lstr_mant.argumento[1] = istr_mant.argumento[1]
//	lstr_mant.argumento[2] = istr_mant.argumento[2]
//	lstr_mant.argumento[3] = istr_mant.argumento[3]
//	lstr_mant.argumento[4] = istr_mant.argumento[4]
	lstr_Mant.Argumento[1]	=	String(dw_2.Object.plde_codigo[1])
	lstr_Mant.Argumento[2]	=	String(dw_2.Object.tpmv_codigo[1])
	lstr_Mant.Argumento[3]	=	String(dw_2.Object.mfco_numero[1])
	lstr_Mant.Argumento[5]	=  String(dw_2.Object.clie_codigo[1])
	
	OpenWithParm(w_emis_guia_despacho_venta, lstr_Mant)
	
	lstr_Mant = Message.PowerObjectParm
	
	IF IsNull(lstr_Mant) THEN RETURN
	
	IF lstr_Mant.Respuesta = 1 THEN
		istr_mant.solo_consulta = TRUE
		parent.TriggerEvent("ue_recuperadatos")
	END IF
END IF
end event

type cb_capturadatos from commandbutton within w_maed_movtofrutacomenca_reproceso
boolean visible = false
integer x = 3227
integer y = 1204
integer width = 279
integer height = 112
integer taborder = 110
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Captura"
end type

event clicked;String	ls_directorio
Integer	li_valida, li_opcion = 1
dwitemstatus stat

ib_ok	= True

IF dw_3.AcceptText() = -1 THEN li_opcion = -1
IF dw_3.ModifiedCount() > 0 THEN 
	li_opcion = 0
END IF

CHOOSE CASE li_opcion
	CASE -1
		ib_ok = False
	CASE 0
		CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
			CASE 1
				Message.DoubleParm = 0
				This.triggerevent("ue_guardar")
				IF message.doubleparm = -1 THEN ib_ok = False
				RETURN
			CASE 3
				ib_ok	= False
				RETURN
		END CHOOSE
END CHOOSE

IF ib_ok = False THEN RETURN

pb_nuevo.TriggerEvent(Clicked!)

DO
	li_valida	= GetFileOpenName("Carga de Archivo", ls_directorio, is_archivo, "emb", &
											"Archivos Fruta Comercial (*.FCM), *.FCM,Todos los Archivos (*.*), *.*")
	IF li_valida = 0 THEN
		pb_salir.SetFocus()
		RETURN
	ELSEIF li_valida = -1 THEN
		MessageBox("Error de Apertura","Ocurrió un error al utilizar el archivo",Information!,Ok!)
		Message.DoubleParm = 1
	ELSE
		is_archivo	= ls_directorio

		Message.DoubleParm = 2
		Parent.TriggerEvent("ue_carga_detalle")

		tab_1.tp_1.Enabled	=	True
		tab_1.tp_2.Enabled	=	True
	END IF
	
LOOP WHILE Message.DoubleParm = 1

IF dw_4.RowCount() > 0 THEN
	pb_grabar.Enabled = True
ELSE
	pb_grabar.Enabled = False
END IF
end event

type dw_exideta from datawindow within w_maed_movtofrutacomenca_reproceso
boolean visible = false
integer x = 3250
integer y = 2028
integer width = 974
integer height = 668
integer taborder = 80
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta"
boolean minbox = true
boolean maxbox = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
borderstyle borderstyle = stylelowered!
end type

type dw_exiencab from datawindow within w_maed_movtofrutacomenca_reproceso
boolean visible = false
integer x = 4407
integer y = 2204
integer width = 1650
integer height = 580
integer taborder = 90
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_exismovtoenca_analisis"
boolean minbox = true
boolean maxbox = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

