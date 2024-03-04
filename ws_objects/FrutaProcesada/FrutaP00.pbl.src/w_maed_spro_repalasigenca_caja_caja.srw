$PBExportHeader$w_maed_spro_repalasigenca_caja_caja.srw
$PBExportComments$Ventana de Repaletizaje.
forward
global type w_maed_spro_repalasigenca_caja_caja from w_mant_encab_deta_csd
end type
type pb_nvopdest from picturebutton within w_maed_spro_repalasigenca_caja_caja
end type
type pb_nvoporg from picturebutton within w_maed_spro_repalasigenca_caja_caja
end type
type pb_acepta from picturebutton within w_maed_spro_repalasigenca_caja_caja
end type
type dw_4 from uo_dw within w_maed_spro_repalasigenca_caja_caja
end type
type pb_cancela from picturebutton within w_maed_spro_repalasigenca_caja_caja
end type
type dw_7 from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type dw_8 from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type dw_9 from uo_dw within w_maed_spro_repalasigenca_caja_caja
end type
type dw_10 from uo_dw within w_maed_spro_repalasigenca_caja_caja
end type
type ids_cuadracajas from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type ids_palletencabhisto from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type ids_palletfrutahisto from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type ids_palletfrutanuevo from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type dw_5 from uo_dw within w_maed_spro_repalasigenca_caja_caja
end type
type ids_palletencabupdate from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type ids_alpalletfruta from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type ids_alpalletencab from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type ids_inspecpaldet from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type ids_despafrigoen from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type ids_despafrigodet from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type ids_palletencabhisto_des from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type ids_palletfrutahisto_des from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type ids_recepcionenca from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type ids_recepciondeta from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type ids_palletencabnuevo from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type pb_recupera from picturebutton within w_maed_spro_repalasigenca_caja_caja
end type
type dw_compacto from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type dw_3 from uo_dw within w_maed_spro_repalasigenca_caja_caja
end type
type dw_6 from uo_dw within w_maed_spro_repalasigenca_caja_caja
end type
type dw_spro_cajasprod from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type dw_11 from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type dw_12 from datawindow within w_maed_spro_repalasigenca_caja_caja
end type
type pb_agrupa from picturebutton within w_maed_spro_repalasigenca_caja_caja
end type
end forward

global type w_maed_spro_repalasigenca_caja_caja from w_mant_encab_deta_csd
boolean visible = false
integer width = 3831
integer height = 2084
string title = "REPALLETIZAJE CAJA A CAJA"
string menuname = ""
boolean minbox = false
event ue_recuperapallet_destino ( )
event ue_recuperapallet_origen ( )
event ue_despuesgrabar ( )
pb_nvopdest pb_nvopdest
pb_nvoporg pb_nvoporg
pb_acepta pb_acepta
dw_4 dw_4
pb_cancela pb_cancela
dw_7 dw_7
dw_8 dw_8
dw_9 dw_9
dw_10 dw_10
ids_cuadracajas ids_cuadracajas
ids_palletencabhisto ids_palletencabhisto
ids_palletfrutahisto ids_palletfrutahisto
ids_palletfrutanuevo ids_palletfrutanuevo
dw_5 dw_5
ids_palletencabupdate ids_palletencabupdate
ids_alpalletfruta ids_alpalletfruta
ids_alpalletencab ids_alpalletencab
ids_inspecpaldet ids_inspecpaldet
ids_despafrigoen ids_despafrigoen
ids_despafrigodet ids_despafrigodet
ids_palletencabhisto_des ids_palletencabhisto_des
ids_palletfrutahisto_des ids_palletfrutahisto_des
ids_recepcionenca ids_recepcionenca
ids_recepciondeta ids_recepciondeta
ids_palletencabnuevo ids_palletencabnuevo
pb_recupera pb_recupera
dw_compacto dw_compacto
dw_3 dw_3
dw_6 dw_6
dw_spro_cajasprod dw_spro_cajasprod
dw_11 dw_11
dw_12 dw_12
pb_agrupa pb_agrupa
end type
global w_maed_spro_repalasigenca_caja_caja w_maed_spro_repalasigenca_caja_caja

type variables
DataWindowChild	idwc_planta, idwc_espdest, idwc_esporig, idwc_vardest, idwc_varorig, &
						idwc_catdest, idwc_catorig, idwc_tipopaldest, idwc_tipopalorig, idwc_embdest, &
						idwc_emborig, idwc_proddest, idwc_prodorig, idwc_caldest, idwc_calorig, idwc_inspec,&
						idwc_variedesdeta, idwc_calrot

str_envase					istr_envase
str_calibreenvase			istr_calibre
Str_mant2					istr_mant2

uo_exportadores			iuo_exportador
uo_tipopallet			   	iuo_tipopallet
uo_productores				iuo_productor
uo_spro_palletencab		iuo_spro_palletencab
uo_lotescorrelequipo		iuo_correl
uo_controlventanas		iuo_ControlVentanas
uo_Clientesprod			iuo_Clientes

String							is_ultimacol, is_palletorig, is_palletdest, is_marca, is_marcao = 'M',&
								is_embalaje, is_empr_oficin, is_Computador
Boolean						ib_Modifica, ib_AutoCommit, ib_ExistePalletDestino = False, &
                                    ib_detalle
Boolean						ib_inspeccion_origen = FALSE, ib_inspeccion_destino = FALSE
integer                 		il_inspeccion, il_EstadoPallet, il_cantidad, il_existerepa, il_destino, il_varirotu, ii_planta   
Long							il_numero, il_count, il_existe, il_pallet, ii_nuinpe, il_NroCaja, il_cambiafila, &
								il_palcompac, il_caja
Date							id_fechai
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public function boolean habilitapallet ()
public function boolean existedetallepallet (long al_pallet)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine asigna_palletnuevo ()
public function boolean existepallet (long al_pallet, boolean ab_mensaje)
public function boolean controla_totalcajas (integer ai_traspa)
public function boolean atributos_pallet ()
public subroutine captura_total_cajas ()
public subroutine traspasa_cajas (integer ai_certifica)
public function boolean integridad ()
public function integer buscarepallet ()
public function boolean existerepalletizado ()
public function integer buscapallets ()
public subroutine grabahistoria_store (long numero, integer tipo)
public subroutine grabaalturas (long numero, integer tipo)
public subroutine grabahistoria_storealtura (long numero, integer tipo)
public subroutine traspasa_cajaspalfruta_encab ()
public subroutine traspasa_cajas_nuevas ()
public function boolean cuadrar_cajas (integer al_tipo)
public subroutine traspasa_cajas_altura (integer tipo)
public function integer buscapalletorigen ()
public subroutine buscacamara ()
public subroutine camaraexiste ()
public function boolean datosinpeccion (long codigo)
public function boolean procesoaltura (integer numero)
public function boolean noexistecliente (integer ai_codigo)
public function boolean variedadrotula (integer codigo, integer especie)
public function boolean existeembalaje (string as_embalaje)
public function boolean existevariedad (integer ai_variedad)
public function boolean existecalibre (string as_calibre, long ai_fila)
public function boolean existevarirotula (integer ai_variedad)
public function boolean existe_prodrotula (long al_codigo)
public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente)
public function boolean existe_transitorio (long al_pallet)
public function boolean pallet_nrosag (long al_pallet, integer al_sag)
end prototypes

event ue_recuperapallet_destino();Long	ll_fila_d, ll_fila_e, respuesta
Integer li_tipoenva, li_envase, li_cliente, li_tipopal, li_especie, li_lugar
string ls_embalaje, ls_tpem_codigo

DO
	dw_3.SetRedraw(False)
	dw_3.Reset()

	If dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), Long(istr_Mant.Argumento[4])) =  -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	Else
		DO
			If dw_3.RowCount()>0 Then
				li_cliente = integer(istr_Mant.Argumento[1])
				ls_embalaje =  dw_3.Object.emba_codigo[1]
				dw_3.GetChild("tpem_codigo", idwc_tipopaldest)
				idwc_tipopaldest.SetTransObject(SQLCA)
				If idwc_tipopaldest.Retrieve(li_cliente,ls_embalaje) = 0 Then
					idwc_tipopaldest.InsertRow(0)
				End If
				
				ls_tpem_codigo = dw_3.Object.tpem_codigo[1]
				
				If iuo_tipopallet.Existe(li_cliente, ls_embalaje, ls_tpem_codigo,True,SqlCa) Then
					dw_3.SetItem(1, "tota_ccajas", iuo_tipopallet.Cajas)
				End If
				li_especie = dw_3.Object.Espe_codigo[1]
				HabilitaEncab(FALSE)
			End If 	
			
			li_tipopal = dw_2.Object.repe_tipopa[1]
			
			dw_4.GetChild("vari_codigo", idwc_vardest)
			idwc_vardest.SetTransObject(SqlCa)
			If idwc_vardest.Retrieve(li_especie) = 0 Then
				idwc_vardest.InsertRow(0)
			End If
			
			dw_4.GetChild("pafr_calibr", idwc_calrot)
			idwc_calrot.SetTransObject(SqlCa)
			If idwc_calrot.Retrieve(li_especie) = 0 Then
				idwc_calrot.InsertRow(0)
			End If
			
			dw_4.GetChild("pafr_calrot", idwc_calorig)
			idwc_calorig.SetTransObject(SqlCa)
			If idwc_calorig.Retrieve(li_especie) = 0 Then
				idwc_calorig.InsertRow(0)
			End If
			
			If dw_4.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), Long(istr_Mant.Argumento[4]),li_tipopal) = -1 Then
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			Else				
				pb_nvopdest.Enabled	=	True
				pb_nvoporg.Enabled	=  True
				
				If il_existerepa = 1 Then
					pb_nvoporg.Enabled	=	False
					pb_acepta.Enabled		=	False
					pb_nvopdest.Enabled 	=  False
					li_lugar = dw_2.Object.repe_lugarp[1]
					If li_lugar = 3 Then
						pb_recupera.Enabled  =  True
						il_cambiafila = 1
						dw_3.Object.b_siguiente.Visible = 1
						dw_3.Object.b_anterior.Visible = 1
					End If	
				End If
				
				dw_4.SetRow(1)
				dw_4.SetFocus()
			End If
		LOOP WHILE respuesta = 1

		If respuesta = 2 Then Close(This)
	End If
	dw_3.SetRedraw(True)
	dw_4.SetRedraw(True)
LOOP WHILE respuesta = 1

If respuesta = 2 Then Close(This)
end event

event ue_recuperapallet_origen();Long	ll_fila_d, ll_fila_e, respuesta, ll_fila, ll_fila_find, ll_secuencia, ll_pafr_ccajas, ll_pafr_ccajas1
Integer li_tipoenva, li_envase, li_cliente
string ls_embalaje, ls_tpem_codigo
DO
	dw_5.SetRedraw(False)
	dw_5.Reset()
	
	dw_5.GetChild("tpem_codigo", idwc_tipopalorig)
	idwc_tipopalorig.SetTransObject(SQLCA)
	IF idwc_tipopalorig.Retrieve(li_cliente,'Z') = 0 THEN
		idwc_tipopalorig.InsertRow(0)
	END If

	IF dw_5.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), &
						  Long(istr_Mant.Argumento[5])) =  -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSE
		DO
			IF dw_5.RowCount()>0 THEN
				li_cliente = integer(istr_mant.argumento[1])
				ls_embalaje = dw_5.Object.emba_codigo[1]
				dw_5.GetChild("tpem_codigo", idwc_tipopalorig)
				idwc_tipopalorig.SetTransObject(SQLCA)
				IF idwc_tipopalorig.Retrieve(li_cliente,ls_embalaje) = 0 THEN
					idwc_tipopalorig.InsertRow(0)
				END If
				
				ls_tpem_codigo = dw_5.Object.tpem_codigo[1]
				
				IF iuo_tipopallet.Existe(li_cliente, ls_embalaje, ls_tpem_codigo,True,SqlCa) THEN
					dw_5.SetItem(1, "tota_ccajas", iuo_tipopallet.Cajas)
				END IF
				Habilitaencab(FALSE)
			END IF 	
			
			IF dw_6.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), &
						  Long(istr_Mant.Argumento[5])) = -1 THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			ELSE
				FOR ll_fila_d = 1 TO dw_6.RowCount() 
					ll_pafr_ccajas = dw_6.Object.pafr_ccajas[ll_fila_d]
					IF ll_pafr_ccajas = 0 OR Isnull(ll_pafr_ccajas) THEN
						dw_6.Object.pafr_ccajas[ll_fila_d] = dw_6.Object.cajas[ll_fila_d] 
					END IF
					ll_secuencia = dw_6.Object.pafr_secuen[ll_fila_d]
					FOR ll_fila = 1 TO dw_10.RowCount()
						ll_fila_find = dw_10.Find("paen_numero = " + String(dw_6.Object.paen_numero[ll_fila_d])+ " and " + &
							"pafr_secuen = " + String(ll_secuencia),ll_fila, ll_fila)
						
						IF ll_fila_find <> 0 THEN
							ll_pafr_ccajas1 = dw_10.Object.pafr_ccajas[ll_fila_find]
							IF ll_pafr_ccajas1 = 0 THEN
								dw_6.Object.pafr_ccajas[ll_fila_d] = 0
								dw_6.Object.cajas[ll_fila_d] 		  = 0
							ELSE	
								dw_6.Object.pafr_ccajas[ll_fila_d] = dw_6.Object.pafr_ccajas[ll_fila_d] - dw_10.Object.caja_traspa[ll_fila_find]
								dw_6.Object.cajas[ll_fila_d] 		  = dw_6.Object.cajas[ll_fila_d] - dw_10.Object.caja_traspa[ll_fila_find]	
							END IF
						END IF	
					NEXT	
				NEXT	
				
				pb_nvoporg.Enabled	=	True
				pb_acepta.Enabled		=	True
				
				IF il_existerepa = 1 THEN
					pb_nvoporg.Enabled	=	False
					pb_acepta.Enabled		=	False
					pb_nvopdest.Enabled 	=  False
				END IF	
				
				dw_6.SetRow(1)
				dw_6.SetFocus()
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_5.SetRedraw(True)
	dw_5.SetRedraw(True)
LOOP WHILE respuesta = 1

IF dw_6.RowCount() > 0 THEN
	dw_5.Object.paen_ccajas[1] = dw_6.Object.total_cajas[1]
END IF	

IF respuesta = 2 THEN Close(This)
end event

event ue_despuesgrabar();Long 		ll_numero, ll_find, ll_pallet, ll_fila_new, ll_fila, ll_fila_enca, ll_fila2, ll_nuevo1,&
			ll_fila1, ll_despacho, ll_fila_det, ll_nuevo2, ll_fila_rece, ll_nuevo, ll_nuevo3, ll_new,&
			ll_recepcion, ll_new2, ll_fila_rece2, ll_file, ll_altfila, ll_FilaRep, ll_FilaRepdet,&
			ll_FilaRep_rese
Integer 	li_planta, li_cliente, fila_find

ids_alpalletfruta.Reset() 
ids_alpalletencab.Reset() 

ids_despafrigoen.SetTransObject(sqlca)
ids_despafrigodet.SetTransObject(sqlca)
ids_recepcionenca.SetTransObject(sqlca)
ids_recepciondeta.SetTransObject(sqlca)	
ids_palletencabhisto_des.SetTransObject(sqlca) 		
ids_palletfrutahisto_des.SetTransObject(sqlca)
ids_alpalletencab.SetTransObject(sqlca)
ids_alpalletfruta.SetTransObject(sqlca)

li_planta	= Integer(dw_2.Object.plde_codigo[1])
li_cliente 	= integer(istr_Mant.Argumento[1])

///////////////////////////////////////RECEPCION////////////////////////////////////////////////
//Recupera numero de Recepción	
SELECT Isnull(Max(rfpe_numero),0) +1
	INTO :ll_recepcion
	FROM dbo.recfruprocee
	WHERE plde_codigo = :li_planta;	

//Graba y carga recepcion
FOR ll_fila_rece = 1 TO ids_palletencabnuevo.RowCount()
	
	ll_FilaRep_rese	=	ids_recepcionenca.Find("rfpe_numero = " + String(ll_recepcion) + &
				" AND plde_codigo = " + String(ids_palletencabnuevo.Object.plde_codigo[ll_fila_rece]) ,&
				1, ids_palletencabnuevo.RowCount())
		
	IF ll_FilaRep_rese = 0 THEN
		ll_new = ids_recepcionenca.InsertRow(0)
		
		ids_recepcionenca.Object.plde_Codigo[ll_new] = ids_palletencabnuevo.Object.plde_codigo[ll_fila_rece] 
		ids_recepcionenca.Object.rfpe_numero[ll_new] = ll_recepcion 
		ids_recepcionenca.Object.clie_codigo[ll_new] = ids_palletencabnuevo.Object.clie_codigo[ll_fila_rece] 
		ids_recepcionenca.Object.rfpe_fecrec[ll_new] = Today()
		ids_recepcionenca.Object.rfpe_tipoen[ll_new] = 5 
		ids_recepcionenca.Object.rfpe_ptaori[ll_new] = ids_palletencabnuevo.Object.plde_codigo[ll_fila_rece] 
		ids_recepcionenca.Object.frre_codigo[ll_new] = 2
		ids_recepcionenca.Object.rfpe_pcopda[ll_new] = 1
		ids_recepcionenca.Object.rfpe_horrec[ll_new] = Now()
		ids_recepcionenca.Object.rfpe_fecing[ll_new] = Today()
		ids_recepcionenca.Object.rfpe_horing[ll_new] = Now()
	END IF	
NEXT	

FOR ll_fila_rece2 = 1 TO ids_palletfrutanuevo.RowCount()
	ll_FilaRepdet	=	ids_recepciondeta.Find("plde_codigo = " + String(ids_palletfrutanuevo.Object.plde_codigo[ll_fila_rece2]) + &
					 " AND clie_codigo = " + String(ids_palletfrutanuevo.Object.clie_codigo[ll_fila_rece2]) + &
					 " AND paen_numero = " + String(ids_palletfrutanuevo.Object.paen_numero[ll_fila_rece2]), + &
					1, ids_palletfrutanuevo.RowCount())
	
	IF ll_FilaRepdet = 0 THEN
		ll_new2 = ids_recepciondeta.InsertRow(0)
		
		ids_recepciondeta.Object.plde_Codigo[ll_new2] = ids_palletfrutanuevo.Object.plde_codigo[ll_fila_rece2] 
		ids_recepciondeta.Object.rfpe_numero[ll_new2] = ll_recepcion 
		ids_recepciondeta.Object.clie_codigo[ll_new2] = ids_palletfrutanuevo.Object.clie_codigo[ll_fila_rece2] 
		ids_recepciondeta.Object.paen_numero[ll_new2] = ids_palletfrutanuevo.Object.paen_numero[ll_fila_rece2] 
		ids_recepciondeta.Object.rfpe_pcopda[ll_new2] = 1
	END IF	
NEXT		

IF ids_recepcionenca.Update() = 1 THEN
	IF ids_recepciondeta.Update() = 1 THEN
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
		ELSE
			ids_recepcionenca.ResetUpdate()
			ids_recepciondeta.ResetUpdate()
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)

		RollBack;											
	END IF	
ELSE
	F_ErrorBaseDatos(sqlca, This.Title)

	RollBack;											
END IF	

///////////////////////////////////////////////HISTORICAS//////////////////////////////////////////////////////////////////////////////////
//Recupera numero de proceso historico
SELECT	IsNull(Max(altu_numero), 0) + 1
	INTO	:ll_numero
	FROM	dbo.alpalletencab
	WHERE	plde_codigo	=	:li_Planta
	AND 	clie_codigo = :li_cliente
	AND	altu_numero < 99999999;

//Graba y carga datos historicos
FOR ll_fila2 = 1 TO ids_palletencabhisto.RowCount()
	ll_fila_enca	= ids_palletencabhisto_des.InsertRow(0)

	ids_palletencabhisto_des.SetItem(ll_fila_enca,"clie_codigo",ids_palletencabhisto.Object.clie_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"paen_numero",ids_palletencabhisto.Object.paen_numero[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"plde_codigo",ids_palletencabhisto.Object.plde_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"pahi_tipopa",ids_palletencabhisto.Object.pahi_tipopa[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"tpem_codigo",ids_palletencabhisto.Object.tpem_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"espe_codigo",ids_palletencabhisto.Object.espe_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"vari_codigo",ids_palletencabhisto.Object.vari_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"tiem_codigo",ids_palletencabhisto.Object.tiem_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"emba_codigo",ids_palletencabhisto.Object.emba_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"cate_codigo",ids_palletencabhisto.Object.cate_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"etiq_codigo",ids_palletencabhisto.Object.etiq_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"stat_codigo",ids_palletencabhisto.Object.stat_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"trat_codigo",ids_palletencabhisto.Object.trat_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"frio_codigo",ids_palletencabhisto.Object.frio_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"cond_codigo",ids_palletencabhisto.Object.cond_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"dest_codigo",ids_palletencabhisto.Object.dest_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"pahi_fecemb",ids_palletencabhisto.Object.pahi_fecemb[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"pahi_cosecha",ids_palletencabhisto.Object.pahi_cosecha[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"paen_altura",ids_palletencabhisto.Object.paen_altura[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"paen_ccajas",ids_palletencabhisto.Object.cajas_traspa[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"tmvp_codigo",ids_palletencabhisto.Object.tmvp_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"paen_fecini",ids_palletencabhisto.Object.paen_fecini[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"paen_horain",ids_palletencabhisto.Object.paen_horain[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"cama_codigo",ids_palletencabhisto.Object.cama_codigo[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"pahi_calle",ids_palletencabhisto.Object.pahi_calle[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"pahi_base",ids_palletencabhisto.Object.pahi_base[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"pahi_posici",ids_palletencabhisto.Object.pahi_posici[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"pahi_estado",ids_palletencabhisto.Object.pahi_estado[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"pahi_inspec",ids_palletencabhisto.Object.pahi_inspec[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"pahi_concal",ids_palletencabhisto.Object.pahi_inspec[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"pahi_pexpor",ids_palletencabhisto.Object.pahi_pexpor[ll_fila2])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"pahi_pmixto",ids_palletencabhisto.Object.pahi_pmixto[1])
	ids_palletencabhisto_des.SetItem(ll_fila_enca,"pahi_proces",ll_numero)
NEXT

FOR ll_fila_new = 1 TO ids_palletfrutahisto.RowCount()	
	ll_fila	= 	ids_palletfrutahisto_des.InsertRow(0)   
		
	ids_palletfrutahisto_des.SetItem(ll_fila,"clie_codigo",ids_palletfrutahisto.Object.clie_codigo[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"paen_numero",ids_palletfrutahisto.Object.paen_numero[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"espe_codigo",ids_palletfrutahisto.Object.espe_codigo[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"vari_codigo",ids_palletfrutahisto.Object.vari_codigo[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"emba_codigo",ids_palletfrutahisto.Object.emba_codigo[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"prod_codigo",ids_palletfrutahisto.Object.prod_codigo[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"cond_codigo",ids_palletfrutahisto.Object.cond_codigo[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"etiq_codigo",ids_palletfrutahisto.Object.etiq_codigo[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"plde_codigo",ids_palletfrutahisto.Object.plde_codigo[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafh_calibr",ids_palletfrutahisto.Object.pafh_calibr[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafh_secuen",ids_palletfrutahisto.Object.pafh_secuen[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafh_ccajas",ids_palletfrutahisto.Object.pafh_ccajas[ll_fila_new])//ids_palletfrutahisto.Object.cajas_traspa[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafh_nrlote",ids_palletfrutahisto.Object.pafh_nrlote[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafh_proces",ll_numero)
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafr_fecing",ids_palletfrutahisto.Object.pafr_fecing[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafr_fecemb",ids_palletfrutahisto.Object.pafr_fecemb[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafr_copack",ids_palletfrutahisto.Object.pafr_copack[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafr_tipdoc",5)
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafr_huert1",ids_palletfrutahisto.Object.pafr_huert1[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafr_cuart2",ids_palletfrutahisto.Object.pafr_cuart2[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafr_huert2",ids_palletfrutahisto.Object.pafr_huert2[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafr_cuart1",ids_palletfrutahisto.Object.pafr_cuart1[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafr_docrel",ids_palletfrutahisto.Object.pafr_docrel[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafr_varrot",ids_palletfrutahisto.Object.pafr_varrot[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafr_rotpak",ids_palletfrutahisto.Object.pafr_rotpak[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafr_fecrot",ids_palletfrutahisto.Object.pafr_fecrot[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafr_embrea",ids_palletfrutahisto.Object.pafr_embrea[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"cate_codigo",ids_palletfrutahisto.Object.cate_codigo[ll_fila_new])
	ids_palletfrutahisto_des.SetItem(ll_fila,"pafr_catrot",ids_palletfrutahisto.Object.pafr_catrot[ll_fila_new])
NEXT		

ll_nuevo = ids_alpalletencab.InsertRow(0)
ids_alpalletencab.Object.plde_codigo[ll_nuevo]	=	dw_2.Object.plde_codigo[1]
ids_alpalletencab.Object.altu_numero[ll_nuevo]	=	ll_numero
ids_alpalletencab.Object.clie_codigo[ll_nuevo]	=	dw_2.Object.clie_codigo[1]
ids_alpalletencab.Object.altu_fecmov[ll_nuevo]	=	Today()
ids_alpalletencab.Object.altu_observ[ll_nuevo] = 	'Reembalaje'

FOR  ll_altfila = 1 TO dw_10.RowCount()
	
	ll_pallet = dw_10.Object.paen_numero[ll_altfila]
	
	fila_find =  ids_alpalletfruta.Find("paen_numero = " + String(ll_pallet)+&
					" AND altu_numero = " + String(ll_numero),1, ids_alpalletfruta.RowCount())
					
	IF fila_find = 0 THEN
		ll_nuevo2 = ids_alpalletfruta.InsertRow(0)
		ids_alpalletfruta.Object.plde_codigo[ll_nuevo2] =	dw_10.Object.plde_codigo[ll_altfila]
		ids_alpalletfruta.Object.altu_numero[ll_nuevo2] =	ll_numero
		ids_alpalletfruta.Object.clie_codigo[ll_nuevo2] =	dw_10.Object.clie_codigo[ll_altfila]
		ids_alpalletfruta.Object.paen_numero[ll_nuevo2] =	ll_pallet
		ids_alpalletfruta.Object.alpf_fecmov[ll_nuevo2] =	Today()
	END IF
NEXT

IF ids_palletencabhisto_des.Update() = 1 THEN
	IF ids_palletfrutahisto_des.Update() = 1 THEN
		IF ids_alpalletencab.Update() = 1 THEN
			IF ids_alpalletfruta.Update() = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					ids_palletencabhisto_des.ResetUpdate()
					ids_palletfrutahisto_des.ResetUpdate()
					ids_alpalletencab.ResetUpdate()
					ids_alpalletfruta.ResetUpdate()
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

//////////////////////////////////////////DESPACHO//////////////////////////////////////////////////////
//Recupera numero de despacho
SELECT Isnull(Max(defe_numero),0) +1
	INTO :ll_despacho
	FROM dbo.despafrigoen
	WHERE plde_codigo = :li_planta;

//Carga y graba datos de despacho
FOR ll_fila1 = 1 TO dw_9.RowCount() 
	ll_FilaRep	=	ids_despafrigoen.Find("defe_numero = " + String(ll_despacho) + &
					 " AND plde_codigo = " + String(dw_9.Object.plde_codigo[ll_Fila1]) ,1, dw_10.RowCount())
	
	IF ll_FilaRep = 0 THEN
		ll_nuevo1 = ids_despafrigoen.InsertRow(0)
		
		ids_despafrigoen.Object.clie_codigo[ll_nuevo1] =   dw_9.Object.clie_codigo[ll_fila1] 
		ids_despafrigoen.Object.defe_numero[ll_nuevo1] =   ll_despacho
		ids_despafrigoen.Object.plde_codigo[ll_nuevo1] =   dw_9.Object.plde_codigo[ll_fila1]
		ids_despafrigoen.Object.defe_fecdes[ll_nuevo1] =   Today()
		ids_despafrigoen.Object.defe_tiposa[ll_nuevo1] =   3
		ids_despafrigoen.Object.defe_horade[ll_nuevo1] =   Time(Now())
		ids_despafrigoen.Object.defe_estado[ll_nuevo1] =   8
		ids_despafrigoen.Object.defe_pcopda[ll_nuevo1] =   1
		ids_despafrigoen.Object.defe_glosag[ll_nuevo1] =   'Despacho Por Reembalaje ' + String(ll_numero) 
		ids_despafrigoen.Object.defe_proces[ll_nuevo1] =   ll_numero    
	END IF	
NEXT

FOR  ll_fila_det = 1 TO dw_10.RowCount()
	
	ll_FilaRepdet	=	ids_despafrigodet.Find("plde_codigo = " + String(dw_10.Object.plde_codigo[ll_fila_det]) + &
					 " AND clie_codigo = " + String(dw_10.Object.clie_codigo[ll_fila_det]) + &
					 " AND paen_numero = " + String(dw_10.Object.paen_numero[ll_fila_det]), + &
					1, ids_despafrigodet.RowCount())
					
	IF ll_FilaRepdet = 0 THEN				
	
		ll_nuevo2 = ids_despafrigodet.InsertRow(0)
		
		ids_despafrigodet.Object.plde_codigo[ll_nuevo2] =   dw_10.Object.plde_codigo[ll_fila_det]    
		ids_despafrigodet.Object.defe_numero[ll_nuevo2] =   ll_despacho    
		ids_despafrigodet.Object.clie_codigo[ll_nuevo2] =   dw_10.Object.clie_codigo[ll_fila_det]    
		ids_despafrigodet.Object.paen_numero[ll_nuevo2] =   dw_10.Object.paen_numero[ll_fila_det]    
		ids_despafrigodet.Object.defe_pcopda[ll_nuevo2] =   1
	END IF	
NEXT	

IF ids_despafrigoen.Update() = 1 THEN
	IF ids_despafrigodet.Update() = 1 THEN
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
		ELSE
			ids_despafrigoen.ResetUpdate()
			ids_despafrigodet.ResetUpdate()
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)

		RollBack;											
	END IF	
ELSE
	F_ErrorBaseDatos(sqlca, This.Title)

	RollBack;											
END IF
end event

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect 	=	0
	dw_2.Object.repe_fecrep.Protect	=	0
	dw_2.Object.repe_respon.Protect	=	0
	dw_2.Object.repe_tipopa.Protect	=	0
	dw_2.Object.repe_lugarp.Protect	=	0
	dw_2.Object.repe_observ.Protect	=	0
	dw_2.Object.repe_numero.Protect	=	0
	
	dw_2.Object.clie_codigo.Color 		= 0
	dw_2.Object.repe_fecrep.Color		= 0
	//dw_2.Object.repe_respon.Color	= 0
	//dw_2.Object.repe_tipopa.Color 	= 0
	//dw_2.Object.repe_lugarp.Color 	= 0
	//dw_2.Object.repe_observ.Color 	= 0
	//dw_2.Object.repe_numero.Color 	= 0
	
	dw_2.Object.clie_codigo.BackGround.Color 		= RGB(255,255,255)	
	dw_2.Object.repe_fecrep.BackGround.Color		= RGB(255,255,255)
	dw_2.Object.repe_respon.BackGround.Color	= RGB(255,255,255)
	dw_2.Object.repe_tipopa.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.repe_lugarp.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.repe_observ.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.repe_numero.BackGround.Color 	= RGB(255,255,255)
ELSE
	dw_2.Object.clie_codigo.Protect 	=	1
	dw_2.Object.repe_fecrep.Protect	=	1
	dw_2.Object.repe_respon.Protect	=	1
	dw_2.Object.repe_tipopa.Protect	=	1
	dw_2.Object.repe_lugarp.Protect	=	1
	dw_2.Object.repe_observ.Protect	=	1
	dw_2.Object.repe_numero.Protect	=	1

	dw_2.Object.clie_codigo.Color 		= RGB(255,255,255)	
	dw_2.Object.repe_fecrep.Color		= RGB(255,255,255)
	//dw_2.Object.repe_respon.Color	= RGB(255,255,255)
	//dw_2.Object.repe_tipopa.Color 	= RGB(255,255,255)
	//dw_2.Object.repe_lugarp.Color 	= RGB(255,255,255)
	//dw_2.Object.repe_observ.Color 	= RGB(255,255,255)
	//dw_2.Object.repe_numero.Color 	= RGB(255,255,255)
	
	dw_2.Object.clie_codigo.BackGround.Color 		= 553648127
	dw_2.Object.repe_fecrep.BackGround.Color 	= 553648127
	dw_2.Object.repe_respon.BackGround.Color	= 553648127
	dw_2.Object.repe_tipopa.BackGround.Color 	= 553648127
	dw_2.Object.repe_lugarp.BackGround.Color 	= 553648127
	dw_2.Object.repe_observ.BackGround.Color 	= 553648127
	dw_2.Object.repe_numero.BackGround.Color 	= 553648127
END IF
end subroutine

public function boolean habilitapallet ();Date		ld_fecha, ld_repe_fecrep
boolean  lb_retorno = true
Integer 	li_cliente, li_repe_respon, li_repe_tipopa, li_repe_lugarp

dw_2.AcceptText()
li_cliente = dw_2.Object.clie_codigo[1]
If IsNull(li_cliente) OR li_cliente = 0 Then
	lb_retorno = False
End If	

ld_repe_fecrep = dw_2.Object.repe_fecrep[1]
If IsNull(ld_repe_fecrep) OR dw_2.Object.repe_fecrep[1] = ld_fecha Then
	lb_retorno = False
End If	

li_repe_respon = dw_2.Object.repe_respon[1]
If IsNull(li_repe_respon) OR li_repe_respon = 0 Then
	lb_retorno = False
End If	

li_repe_tipopa = dw_2.Object.repe_tipopa[1]
If IsNull(li_repe_tipopa) OR li_repe_tipopa = 0 Then
	lb_retorno = False
End If	

li_repe_lugarp = dw_2.Object.repe_lugarp[1]
If IsNull(li_repe_lugarp) OR li_repe_lugarp = 0 Then
	lb_retorno = False
End If	

If lb_retorno Then
	dw_3.Object.paen_tipopa.Protect		=	0

	dw_3.Object.paen_numero.Protect	=	0
	dw_3.Object.paen_numero.Color 		=	0
	dw_3.Object.paen_numero.BackGround.Color = RGB(255,255,255)
	
	dw_3.Object.tpem_codigo.Protect					=	0
	dw_3.Object.tpem_codigo.Color					=	0
	dw_3.Object.tpem_codigo.BackGround.Color	=	RGB(255,255,255)
End If

Return lb_retorno
end function

public function boolean existedetallepallet (long al_pallet);Integer li_cantidad, li_exportador, li_planta

li_exportador	=	Integer(istr_Mant.Argumento[1])
li_planta		=	Integer(istr_Mant.Argumento[2])

SELECT Count(*) 
	INTO :li_cantidad
	FROM dbo.palletfruta
	WHERE clie_codigo = :li_exportador
	AND plde_codigo = :li_planta
	AND paen_numero = :al_pallet;
 
IF isnull(li_cantidad) THEN li_cantidad = 0

SELECT max(pafr_secuen)
	INTO :il_caja
	FROM dbo.palletfruta
	WHERE clie_codigo = :li_exportador
	AND plde_codigo = :li_planta
	AND paen_numero = :al_pallet;
 
IF li_cantidad > 0 THEN RETURN TRUE
 
RETURN FALSE
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_Retorno, lb_autocommit
long ll_fila, pallet, Numero, ll_tota_ccajas
integer cliente,planta, li_planta, li_tiporepa, li_movto

DwItemStatus	Estadol

FOR ll_fila = 1 TO ids_palletfrutahisto.RowCount()		 
	Estadol	= ids_palletfrutahisto.GetItemStatus(ll_fila, 0, Primary!)
	cliente  = ids_palletfrutahisto.getitemnumber(1,"clie_codigo")
	planta   = ids_palletfrutahisto.getitemnumber(1,"plde_codigo")
	pallet   = ids_palletfrutahisto.getitemnumber(1,"paen_numero")
NEXT

FOR ll_fila = 1 TO ids_palletfrutahisto.RowCount()		 
	Estadol	= ids_palletfrutahisto.GetItemStatus(ll_fila, 0, Primary!)
	cliente  = ids_palletfrutahisto.getitemnumber(ll_fila,"clie_codigo")
	planta   = ids_palletfrutahisto.getitemnumber(ll_fila,"plde_codigo")
	pallet   = ids_palletfrutahisto.getitemnumber(ll_fila,"paen_numero")
NEXT

If Not dw_2.uf_check_required(0) Then Return False

If Not dw_1.uf_validate(0) Then Return False

If NOT Borrando Then
	If dw_2.GetItemStatus(1, 0, Primary!) = NewModIfied! Then
		dw_2.SetItem(1,"repe_usuari",gstr_us.nombre)
		dw_2.SetItem(1,"repe_comput",gstr_us.computador)
		dw_2.SetItem(1,"REPE_HORACT",F_FechaHora())
		dw_2.SetItem(1,"REPE_FECACT",Today())
	End If
End If

lb_autocommit		= SqlCa.AutoCommit
SqlCa.Autocommit	= False

If Borrando Then
	If ids_palletencabnuevo.Update(True, False) = 1 Then
		If ids_palletfrutanuevo.Update(True, False) = 1 Then
			If dw_10.Update(True,False) = 1 Then
				If dw_2.Update(True, False) = 1 Then
					If dw_1.Update(True, False) = 1 Then
						If ids_palletencabhisto.Update() = 1 Then
							If ids_palletfrutahisto.Update() = 1 Then 
								If ids_alpalletfruta.Update() = 1 Then 
									If ids_alpalletencab.Update() = 1 Then
										If ids_inspecpaldet.Update() = 1 Then
											Commit;
				
											If sqlca.SQLCode <> 0 Then
												F_ErrorBaseDatos(sqlca, This.Title)
											Else
												lb_Retorno	=	True
								
												dw_1.ResetUpdate()
												dw_2.ResetUpdate()
												dw_10.ResetUpdate()
												ids_palletencabnuevo.ResetUpdate()
												ids_palletfrutanuevo.ResetUpdate()
												ids_palletencabhisto.ResetUpdate()
												ids_palletfrutahisto.ResetUpdate()
												ids_alpalletfruta.ResetUpdate()
												ids_alpalletencab.ResetUpdate()
												ids_inspecpaldet.ResetUpdate()
											End If
										Else
											F_ErrorBaseDatos(sqlca, This.Title)
					
											RollBack;
										End If	
									Else
										F_ErrorBaseDatos(sqlca, This.Title)
					
										RollBack;
									End If
								Else
									F_ErrorBaseDatos(sqlca, This.Title)
					
									RollBack;
								End If	
							Else
								F_ErrorBaseDatos(sqlca, This.Title)
					
								RollBack;	
							End If
						Else
							F_ErrorBaseDatos(sqlca, This.Title)
					
							RollBack;	
						End If	
					Else
						F_ErrorBaseDatos(sqlca, This.Title)
				
						RollBack;
					End If
				Else
					F_ErrorBaseDatos(sqlca, This.Title)
		
					RollBack;
				End If
			Else
				F_ErrorBaseDatos(sqlca, This.Title)
	
				RollBack;
			End If
		Else
			F_ErrorBaseDatos(sqlca, This.Title)
	
			RollBack;
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)

		RollBack;
	End If
Else
	If ids_palletencabnuevo.Update(True, False) = 1 Then
		If ids_palletencabupdate.Update(True, False) = 1 Then
			If ids_palletfrutanuevo.Update(True, False) = 1 Then
				If dw_10.Update(True, False) = 1 Then
					If dw_9.Update(True,False) = 1 Then
						If ids_palletencabhisto.Update(True, False) = 1 Then
							If ids_palletfrutahisto.Update(True, False) = 1 Then
								If dw_2.Update(True, False) = 1 Then
									If dw_1.Update(True, False) = 1 Then 
										If ids_alpalletencab.Update(True, False) = 1 Then 
											If ids_alpalletfruta.Update(True, False) = 1 Then 
												If ids_inspecpaldet.Update() = 1 Then
													Commit;
												
													If sqlca.SQLCode <> 0 Then
														F_ErrorBaseDatos(sqlca, This.Title)
													Else
														lb_Retorno	=	True
										
														dw_1.ResetUpdate()
														dw_2.ResetUpdate()
														ids_palletencabnuevo.ResetUpdate()
														ids_palletencabupdate.ResetUpdate()
														ids_palletfrutanuevo.ResetUpdate()
														dw_9.ResetUpdate()
														dw_10.ResetUpdate()
														ids_palletencabhisto.ResetUpdate()
														ids_palletfrutahisto.ResetUpdate()
														ids_alpalletfruta.ResetUpdate()
														ids_alpalletencab.ResetUpdate()
														ids_inspecpaldet.ResetUpdate()
													End If
												Else
													F_ErrorBaseDatos(sqlca, This.Title)
													RollBack;											
												End If		
											Else
												F_ErrorBaseDatos(sqlca, This.Title)
												RollBack;											
											End If	
										Else	
											F_ErrorBaseDatos(sqlca, This.Title)
											RollBack;
										End If	
									Else
										F_ErrorBaseDatos(sqlca, This.Title)
										RollBack;
									End If
								Else
									F_ErrorBaseDatos(sqlca, This.Title)
									RollBack;	
								End If	
							Else
								F_ErrorBaseDatos(sqlca, This.Title)
								RollBack;
							End If		
						Else
							F_ErrorBaseDatos(sqlca, This.Title)
							RollBack;
						End If	
					Else
						F_ErrorBaseDatos(sqlca, This.Title)
						RollBack;
					End If
				Else
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				End If
			Else
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			End If
		Else
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;											
		End If		
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	End If
End If
	
Numero = Long(il_numero)
li_planta = Integer(istr_mant.argumento[2])
li_movto = 4

/*retorna tabla parametros a la normalidad*/
UPDATE dbo.parempresa SET
	empr_oficin = :is_empr_oficin;

/*actualiza numero actual en correlativos */
UPDATE dbo.CORRELMOVIMIENTOS SET
	como_actual = :numero
	WHERE plde_codigo = :li_planta
	AND	como_tipomv = :li_movto;

sqlca.AutoCommit	=	lb_AutoCommit

Return lb_Retorno
end function

public subroutine asigna_palletnuevo ();Long 		ll_repe_nrosag, ll_fila
Integer	li_inspecnuevo, li_inspeccion, li_tiporepa

IF NOT ib_ExistePalletDestino THEN
	dw_3.Object.plde_codigo[1]		=	dw_5.Object.plde_codigo[1]
	dw_3.Object.espe_codigo[1]	=	dw_5.Object.espe_codigo[1]
	dw_3.Object.vari_codigo[1]		=	dw_5.Object.vari_codigo[1]
	dw_3.Object.cate_codigo[1]		=	dw_5.Object.cate_codigo[1]
	dw_3.Object.cama_codigo[1]	=	dw_5.Object.cama_codigo[1]
	dw_3.Object.frio_codigo[1]		=	dw_5.Object.frio_codigo[1]
	dw_3.Object.etiq_codigo[1]		=	dw_5.Object.etiq_codigo[1]
	dw_3.Object.paen_estado[1]	=	1
	dw_3.Object.paen_fecini[1] 	=  dw_5.Object.paen_fecini[1]
	dw_3.Object.paen_fecemb[1]	=	dw_5.Object.paen_fecemb[1]
	dw_3.Object.paen_altura[1] 	=  dw_5.Object.paen_altura[1]
	dw_3.Object.paen_concal[1] 	=  dw_5.Object.paen_concal[1]
	dw_3.Object.stat_codigo[1] 	=  dw_5.Object.stat_codigo[1]
	dw_3.Object.paen_pexpor[1] 	=  dw_5.Object.paen_pexpor[1]
	dw_3.Object.emba_codigo[1] 	=  dw_5.Object.emba_codigo[1]
	dw_3.Object.trat_codigo[1] 		=  dw_5.Object.trat_codigo[1]
	dw_3.Object.dest_codigo[1] 	=  dw_5.Object.dest_codigo[1]
	dw_3.Object.cond_codigo[1] 	=  dw_5.Object.cond_codigo[1]
	dw_3.Object.paen_cosecha[1]	=  dw_5.Object.paen_cosecha[1]
	dw_3.Object.copa_codigo[1] 	=  dw_5.Object.copa_codigo[1]
END IF
end subroutine

public function boolean existepallet (long al_pallet, boolean ab_mensaje);integer li_cliente, li_planta, li_calidad
long ll_pallet
boolean lb_Retorno = True

li_cliente = Integer(istr_Mant.Argumento[1])
li_planta = Integer(istr_Mant.Argumento[2])
setnull(is_embalaje)
setnull(il_inspeccion)
setnull(il_EstadoPallet)

SELECT emba_codigo, paen_inspec, paen_estado, paen_concal   
	INTO :is_embalaje,:il_inspeccion,:il_EstadoPallet,:li_calidad 
	FROM dbo.palletencab  
	WHERE clie_codigo = :li_cliente  
	AND paen_numero = :al_Pallet 
	AND plde_codigo = :li_planta;

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_palletencab")
	lb_Retorno	=	False
ElseIf sqlca.SQLCode = 100 Then
	lb_Retorno	=	False
	If ab_Mensaje Then
		MessageBox("Atención", "Número de Pallet " + String(al_Pallet, '00000000') + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Número.")
	End If
End If

If li_calidad = 3 OR li_calidad = 2 Then
	MessageBox("Atención", "Número de Pallet " + String(al_Pallet, '00000000') + &
					", Ha sido Objetado o Rechazado.")
End If

Return lb_Retorno
end function

public function boolean controla_totalcajas (integer ai_traspa);Long	ll_Fila, ll_Total_CajasEnc, ll_Total_CajasDet, 	ll_Total_CajasTra
String ls_tpem_codigo

dw_3.AcceptText()
dw_4.AcceptText()
dw_6.AcceptText()
	
ll_Total_CajasEnc	=	Integer(dw_3.Object.tpem_codigo[1])

ll_Fila	=	dw_4.RowCount()

IF  ll_Fila > 0 THEN
	ll_Total_CajasDet	=	dw_4.Object.total_cajas[ll_Fila]
END IF

ll_Fila	=	dw_6.RowCount()

IF  ll_Fila > 0 THEN
	IF ai_traspa = 1 THEN
		ll_Total_CajasTra	=	dw_6.Object.pafr_ccajas[ll_Fila]
	ELSE
		ll_Total_CajasTra	=	dw_6.Object.total_traspa[ll_Fila]
	END IF	
END IF

IF ((ll_Total_CajasDet + ll_Total_CajasTra) > ll_Total_CajasEnc) THEN
	ls_tpem_codigo = dw_3.object.tpem_codigo[1]
	IF isnull(ls_tpem_codigo) THEN
		RETURN True
	ELSE
		RETURN False
	END IF	
ELSE
	RETURN True
END IF
end function

public function boolean atributos_pallet ();Boolean		lb_estado = False

dw_3.AcceptText()

IF ib_ExistePalletDestino THEN
	IF ib_inspeccion_Origen <>  ib_inspeccion_Destino AND ib_inspeccion_Destino THEN
		RETURN True
	END IF
	lb_estado = True
ELSE
	lb_estado = True
END IF

RETURN lb_estado
end function

public subroutine captura_total_cajas ();Integer li_repe_tipopa

IF dw_4.RowCount() > 0 THEN
	
	dw_3.Object.paen_ccajas[1] = dw_4.Object.total_cajas[1]
ELSE
	dw_3.Object.paen_ccajas[1] = 0
END IF

IF dw_6.RowCount() > 0 THEN
	
	li_repe_tipopa = dw_2.Object.repe_tipopa[1]
	IF li_repe_tipopa = 1 THEN
		dw_5.Object.paen_ccajas[1] = dw_6.Object.total_cajas[1] - dw_6.Object.total_traspa[1]
	ELSE
		dw_5.Object.paen_ccajas[1] = dw_6.Object.total_cajas[1]
	END IF	
ELSE
	dw_5.Object.paen_ccajas[1] = 0
END IF
end subroutine

public subroutine traspasa_cajas (integer ai_certifica);Long	ll_Fila, ll_BFilaRep, ll_BFilaPD, ll_Nuevo, ll_NuevoDet, ll_pallet, ll_Fila2, ll_NuevoDet2,&
		ll_Nuevo2, ll_caja_traspa, ll_pafr_ccajas, ll_paen_numero, ll_prodrot, ll_predrot, ll_cuarrot, ll_rotpak
Integer li_Secuen, li_cliente, li_planta, li_repe_tipopa, li_fumigacion, li_condicion,li_respuesta,&
		li_condicion_ori, li_condicion_des, li_secuencia

dw_1.AcceptText()
dw_4.AcceptText()
dw_6.AcceptText()

li_cliente = dw_2.Object.clie_codigo[1]

If dw_4.RowCount() > 0 Then
//	ll_prodrot  = dw_4.Object.pafr_prdrot[1]
//	ll_predrot  = dw_4.Object.pafr_huert4[1]
//	ll_cuarrot  = dw_4.Object.pafr_cuart4[1]
//	ll_rotpak	= dw_4.Object.pafr_rotpak[1]
	ll_prodrot  = 999999
	ll_predrot  = 99999
	ll_cuarrot  = 99999
	ll_rotpak	= 99999
Else
	ll_prodrot  = 999999
	ll_predrot  = 99999
	ll_cuarrot  = 99999
	ll_rotpak	= 99999
End If

FOR ll_Fila = 1 TO dw_6.RowCount()
	ll_BFilaRep	=	dw_1.Find("paen_nroori = " + String(dw_6.Object.paen_numero[ll_Fila]) + &
					 " AND pafr_secori = " + String(dw_6.Object.pafr_secuen[ll_Fila]) + &
					 " AND paen_numero = " + String(dw_3.Object.paen_numero[1]), + &
					1, dw_1.RowCount())

	If ll_BFilaRep > 0 Then
		
		ll_BFilaPD	=	dw_4.Find("paen_numero = " + String(dw_3.Object.paen_numero[1]) + &
					 " AND pafr_secuen = " + String(dw_1.Object.pafr_secuen[ll_BFilaRep]), &
					 1, dw_4.RowCount())
										 
    	ll_caja_traspa = dw_6.Object.caja_traspa[ll_Fila]
//		If ll_caja_traspa	> 0 Then
//			If ll_BFilaPD > 0 Then
//				dw_1.Object.pafr_ccajas[ll_BFilaRep]	=	dw_1.Object.pafr_ccajas[ll_BFilaRep] + dw_6.Object.caja_traspa[ll_Fila]
//			//	dw_4.Object.pafr_ccajas[ll_BFilaPD]		=	dw_4.Object.pafr_ccajas[ll_BFilaPD] + dw_6.Object.caja_traspa[ll_Fila]
//				dw_6.Object.pafr_ccajas[ll_Fila]			=	dw_6.Object.pafr_ccajas[ll_Fila] - dw_6.Object.caja_traspa[ll_Fila]
//				dw_6.Object.caja_traspa[ll_Fila]    	=  0	
//			End If
//		End If
	End If
	//Else
		
		ll_caja_traspa = dw_6.Object.caja_traspa[ll_Fila]
		If ll_caja_traspa	> 0 Then
			
//			If dw_4.RowCount() > 0 Then
//				li_secuen = dw_4.Object.pafr_secuen[dw_4.RowCount()] + 1
//			Else
//				li_secuen = 1
//			End If	
			
			If dw_4.RowCount() > 0 Then
				FOR li_secuencia = 1 TO dw_4.RowCount()
					If dw_4.Object.pafr_secuen[li_secuencia] < 9999 Then
						li_secuen = dw_4.Object.pafr_secuen[li_secuencia] + 1
					End If	
				NEXT	
			End If
			
			If li_secuen = 0 Then li_secuen = 1
			
			dw_6.GetChild("pafr_calibr", idwc_calorig)
			dw_4.GetChild("pafr_calibr", idwc_caldest)
			
			If idwc_calorig.Retrieve(0) = 0 Then
				idwc_calorig.InsertRow(0)
			End If
			
			If idwc_caldest.Retrieve(dw_6.Object.espe_codigo[ll_Fila]) = 0 Then
				idwc_caldest.InsertRow(0)
			End If
			
			dw_4.GetChild("vari_codigo", idwc_variedesdeta)
			idwc_variedesdeta.SetTransObject(SqlCa)
			If idwc_variedesdeta.Retrieve(dw_6.Object.espe_codigo[ll_Fila]) = 0 Then
				idwc_variedesdeta.InsertRow(0)
			End If
			
			dw_4.GetChild("pafr_calrot", idwc_calrot)
			idwc_calrot.SetTransObject(SqlCa)
			If idwc_calrot.Retrieve(dw_6.Object.espe_codigo[ll_Fila]) = 0 Then
				idwc_calrot.InsertRow(0)
			End If
						
			ll_Nuevo	=	dw_4.InsertRow(0)
			
			dw_4.Object.clie_codigo[ll_Nuevo]	=	dw_6.Object.clie_codigo[ll_Fila]
			dw_4.Object.plde_codigo[ll_Nuevo]	=	dw_6.Object.plde_codigo[ll_Fila]
			dw_4.Object.paen_numero[ll_Nuevo]	=	dw_3.Object.paen_numero[1]
			
			If dw_6.Object.pafr_secuen[ll_Fila] > 9999 Then
				dw_4.Object.pafr_secuen[ll_Nuevo]	=	dw_6.Object.pafr_secuen[ll_Fila]   //ll_Nuevo
			Else
				dw_4.Object.pafr_secuen[ll_Nuevo]	=	li_secuen
			End If	
			
			dw_4.Object.espe_codigo[ll_Nuevo]	=	dw_6.Object.espe_codigo[ll_Fila]
			dw_4.Object.vari_codigo[ll_Nuevo]	=	dw_6.Object.vari_codigo[ll_Fila]
			dw_4.Object.vari_nombre[ll_Nuevo]	=	dw_6.Object.vari_nombre[ll_Fila]
			dw_4.Object.pafr_varrot[ll_Nuevo]	=	dw_6.Object.pafr_varrot[ll_Fila]
			dw_4.Object.prod_codigo[ll_Nuevo]	=	dw_6.Object.prod_codigo[ll_Fila]
			
			//If ll_prodrot 	= 999999 Then
				dw_4.Object.pafr_prdrot[ll_Nuevo]	=	dw_6.Object.pafr_prdrot[ll_Fila]
				dw_4.Object.pafr_huert4[ll_Nuevo]	=	dw_6.Object.pafr_huert4[ll_Fila]
         		dw_4.Object.pafr_cuart4[ll_Nuevo]	=	dw_6.Object.pafr_cuart4[ll_Fila]
				dw_4.Object.pafr_rotpak[ll_Nuevo]	=	dw_6.Object.pafr_rotpak[ll_Fila]
				
				If li_repe_tipopa <> 7 Then
					dw_4.Object.pafr_huert2[ll_Nuevo]	=	dw_6.Object.pafr_huert2[ll_Fila]
					dw_4.Object.pafr_huert3[ll_Nuevo]	=	dw_6.Object.pafr_huert3[ll_Fila]
					dw_4.Object.pafr_cuart2[ll_Nuevo]	=	dw_6.Object.pafr_cuart2[ll_Fila]
					dw_4.Object.pafr_cuart3[ll_Nuevo]	=	dw_6.Object.pafr_cuart3[ll_Fila]
					dw_4.Object.pafr_barra1[ll_Nuevo]	=	dw_6.Object.pafr_barra1[ll_Fila]
					dw_4.Object.pafr_barra2[ll_Nuevo]	=	dw_6.Object.pafr_barra2[ll_Fila]
					dw_4.Object.pafr_barra3[ll_Nuevo]	=	dw_6.Object.pafr_barra3[ll_Fila]
					dw_4.Object.pafr_barra4[ll_Nuevo]	=	dw_6.Object.pafr_barra4[ll_Fila]
				End If
					
//			Else
//				If li_cliente = gi_CodExport Then
//					dw_4.Object.pafr_prdrot[ll_Nuevo]	=	ll_prodrot
//					dw_4.Object.pafr_huert4[ll_Nuevo]	=	ll_predrot
//					dw_4.Object.pafr_cuart4[ll_Nuevo]	=	ll_cuarrot
//					dw_4.Object.pafr_rotpak[ll_Nuevo]	=  ll_rotpak
//				Else	
//					dw_4.Object.pafr_prdrot[ll_Nuevo]	=	dw_6.Object.pafr_prdrot[ll_Fila]
//					dw_4.Object.pafr_huert4[ll_Nuevo]	=	dw_6.Object.pafr_huert4[ll_Fila]
//         		dw_4.Object.pafr_cuart4[ll_Nuevo]	=	dw_6.Object.pafr_cuart4[ll_Fila]
//					dw_4.Object.pafr_rotpak[ll_Nuevo]	=	dw_6.Object.pafr_rotpak[ll_Fila]
//				End If	
//			End If	
			
			dw_4.Object.emba_codigo[ll_Nuevo]	=	dw_6.Object.emba_codigo[ll_Fila]
			dw_4.Object.etiq_codigo[ll_Nuevo]	=	dw_6.Object.etiq_codigo[ll_Fila]
	     	dw_4.Object.pafr_huert1[ll_Nuevo]	=	dw_6.Object.pafr_huert1[ll_Fila]
         	dw_4.Object.pafr_cuart1[ll_Nuevo]	=	dw_6.Object.pafr_cuart1[ll_Fila]
			dw_4.Object.pafr_nrlote[ll_Nuevo]	=	dw_6.Object.pafr_nrlote[ll_Fila]
			dw_4.Object.pafr_calibr[ll_Nuevo]   =	dw_6.Object.pafr_calibr[ll_Fila]
			dw_4.Object.pafr_fecemb[ll_Nuevo]	=	dw_6.Object.pafr_fecemb[ll_Fila]
			dw_4.Object.pafr_ccajas[ll_Nuevo]	=	dw_6.Object.caja_traspa[ll_Fila]
			dw_4.Object.prod_nombre[ll_Nuevo]	=	dw_6.Object.prod_nombre[ll_Fila]
			dw_4.Object.cond_codigo[ll_Nuevo]	=	dw_6.Object.cond_codigo[ll_Fila]
			dw_4.Object.pafr_cjssal[ll_Nuevo]	=	dw_4.Object.pafr_ccajas[ll_Nuevo]
			dw_4.Object.pafr_fecing[ll_Nuevo]	=	dw_6.Object.pafr_fecing[ll_Fila]
			dw_4.Object.pafr_copack[ll_nuevo]   = 	dw_6.Object.pafr_copack[ll_Fila]
			dw_4.Object.pafr_docrel[ll_nuevo]   = 	dw_6.Object.pafr_docrel[ll_Fila]
			dw_4.Object.pafr_calrot[ll_nuevo]   = 	dw_6.Object.pafr_calrot[ll_Fila]
			dw_4.Object.pafr_fecrot[ll_nuevo]   = 	dw_6.Object.pafr_fecrot[ll_Fila]
			dw_4.Object.pafr_embrea[ll_nuevo]   = 	dw_6.Object.pafr_embrea[ll_Fila]
			dw_4.Object.cama_nombre[ll_nuevo]   = 	dw_6.Object.cama_nombre[ll_Fila]
			dw_4.Object.cate_codigo[ll_nuevo]   = 	dw_6.Object.cate_codigo[ll_Fila]
			dw_4.Object.pafr_catrot[ll_nuevo]   = 	dw_6.Object.pafr_catrot[ll_Fila]
			
			If isnull(dw_6.Object.pafr_nroori[ll_Fila]) OR &
					dw_6.Object.pafr_nroori[ll_Fila] = 0 Then
					
				dw_4.Object.pafr_nroori[ll_nuevo] = dw_6.Object.paen_numero[ll_Fila]
			Else	
				dw_4.Object.pafr_nroori[ll_nuevo] = dw_6.Object.pafr_nroori[ll_Fila]
			End If
			
			If isnull(dw_6.Object.pafr_secori[ll_Fila]) OR &
						dw_6.Object.pafr_secori[ll_Fila] = 0 Then
						
				dw_4.Object.pafr_secori[ll_nuevo] = dw_6.Object.pafr_secuen[ll_Fila]
			Else	
				dw_4.Object.pafr_secori[ll_nuevo] = dw_6.Object.pafr_secori[ll_Fila]
			End If
			
			dw_4.accepttext()

			//Inserta los Pallet			
			ll_NuevoDet	=	dw_1.InsertRow(0)
			li_repe_tipopa = dw_2.Object.repe_tipopa[1]
			If li_repe_tipopa = 1 OR li_repe_tipopa = 3 OR li_repe_tipopa = 7 Then
				dw_1.Object.plde_codigo[ll_NuevoDet]	=	dw_6.Object.plde_codigo[ll_Fila]
				dw_1.Object.clie_codigo[ll_NuevoDet]	=	dw_6.Object.clie_codigo[ll_Fila]
				dw_1.Object.repe_secuen[ll_NuevoDet]	=  ll_NuevoDet
				
				If li_repe_tipopa = 3 OR li_repe_tipopa = 7 Then
					dw_1.Object.paen_numero[ll_NuevoDet]	=	dw_3.Object.paen_numero[1]
					dw_1.Object.paen_nroori[ll_NuevoDet]	=	dw_5.Object.paen_numero[1]
					dw_1.Object.plde_codigo[ll_NuevoDet]	=	dw_6.Object.plde_codigo[ll_Fila]
					dw_1.Object.pafr_ccajas[ll_NuevoDet]	=	dw_6.Object.caja_traspa[ll_Fila]
					dw_1.Object.repd_totcao[ll_NuevoDet]	=	dw_6.Object.pafr_ccajas[ll_Fila]
				ElseIf li_repe_tipopa = 1 Then
					dw_1.Object.paen_numero[ll_NuevoDet]	=	dw_5.Object.paen_numero[1]
					dw_1.Object.paen_nroori[ll_NuevoDet]	=	dw_3.Object.paen_numero[1]
					dw_1.Object.plde_codigo[ll_NuevoDet]	=	dw_6.Object.plde_codigo[ll_Fila]
					dw_1.Object.pafr_ccajas[ll_NuevoDet]	=	dw_6.Object.caja_traspa[ll_Fila]
					dw_1.Object.repd_totcao[ll_NuevoDet]	=	dw_6.Object.pafr_ccajas[ll_Fila]
				End If
				
				dw_1.Object.plde_codigo[ll_NuevoDet]	=	dw_6.Object.plde_codigo[ll_Fila]
				dw_1.Object.pafr_secuen[ll_NuevoDet]	=	dw_6.Object.pafr_secuen[ll_Fila]
				dw_1.Object.pafr_fecemb[ll_NuevoDet]	=	dw_6.Object.pafr_fecemb[ll_Fila]
				dw_1.Object.espe_codigo[ll_NuevoDet]	=	dw_6.Object.espe_codigo[ll_Fila]
				dw_1.Object.vari_codigo[ll_NuevoDet]	=	dw_6.Object.vari_codigo[ll_Fila]
				dw_1.Object.pafr_varrot[ll_NuevoDet]	=	dw_6.Object.pafr_varrot[ll_Fila]
				dw_1.Object.prod_codigo[ll_NuevoDet]	=	dw_6.Object.prod_codigo[ll_Fila]
				dw_1.Object.emba_codigo[ll_NuevoDet]	=	dw_6.Object.emba_codigo[ll_Fila]
				dw_1.Object.etiq_codigo[ll_NuevoDet]	=	dw_6.Object.etiq_codigo[ll_Fila]
				dw_1.Object.pafr_calibr[ll_NuevoDet]	=	dw_6.Object.pafr_calibr[ll_Fila]
				dw_1.Object.pafr_secori[ll_NuevoDet]	=	dw_6.Object.pafr_secuen[ll_Fila]
				dw_1.Object.repd_marcad[ll_NuevoDet]	=	is_marca
				dw_1.Object.repd_marcao[ll_NuevoDet]	=	is_marcao
				dw_1.Object.pafr_fecing[ll_NuevoDet]	=	dw_6.Object.pafr_fecing[ll_Fila]
			Else
				dw_1.Object.plde_codigo[ll_NuevoDet]	=	dw_6.Object.plde_codigo[ll_Fila]
				dw_1.Object.clie_codigo[ll_NuevoDet]	=	dw_4.Object.clie_codigo[ll_nuevo]
				dw_1.Object.paen_numero[ll_NuevoDet]	=	dw_5.Object.paen_numero[1]
				dw_1.Object.repe_secuen[ll_NuevoDet]	=  ll_NuevoDet
				dw_1.Object.pafr_secuen[ll_NuevoDet]	=	dw_4.Object.pafr_secuen[ll_nuevo]
				dw_1.Object.pafr_fecemb[ll_NuevoDet]	=	dw_4.Object.pafr_fecemb[ll_nuevo]
				dw_1.Object.espe_codigo[ll_NuevoDet]	=	dw_4.Object.espe_codigo[ll_nuevo]
				dw_1.Object.vari_codigo[ll_NuevoDet]	=	dw_4.Object.vari_codigo[ll_nuevo]
				dw_1.Object.pafr_varrot[ll_NuevoDet]	=	dw_4.Object.pafr_varrot[ll_nuevo]
				dw_1.Object.prod_codigo[ll_NuevoDet]	=	dw_4.Object.prod_codigo[ll_nuevo]
				dw_1.Object.emba_codigo[ll_NuevoDet]	=	dw_4.Object.emba_codigo[ll_nuevo]
				dw_1.Object.etiq_codigo[ll_NuevoDet]	=	dw_4.Object.etiq_codigo[ll_nuevo]
				dw_1.Object.pafr_calibr[ll_NuevoDet]	=	dw_4.Object.pafr_calibr[ll_nuevo]
				dw_1.Object.pafr_ccajas[ll_NuevoDet]	=	dw_6.Object.caja_traspa[ll_Fila]
				dw_1.Object.paen_nroori[ll_NuevoDet]	=	dw_3.Object.paen_numero[1]
				dw_1.Object.pafr_secori[ll_NuevoDet]	=	dw_6.Object.pafr_secuen[ll_Fila]
				dw_1.Object.repd_totcao[ll_NuevoDet]	=	dw_4.Object.total_cajas[1] - dw_6.Object.caja_traspa[ll_Fila] 
				dw_1.Object.repd_marcad[ll_NuevoDet]	=	is_marca
				dw_1.Object.repd_marcao[ll_NuevoDet]	=	is_marcao
				dw_1.Object.pafr_fecing[ll_NuevoDet]	=	dw_6.Object.pafr_fecing[ll_Fila]
				
			End If	
							
			li_planta  = dw_3.Object.plde_codigo[1]
			li_cliente = dw_3.Object.clie_codigo[1]
			ll_pallet  = dw_3.Object.paen_numero[1]
			
			SELECT count(*) 
				INTO :il_count
				FROM dbo.palletencab  
				WHERE clie_codigo = :li_cliente  
				AND  paen_numero = :ll_Pallet 
				AND  plde_codigo = :li_planta;
			
			li_repe_tipopa = dw_2.Object.repe_tipopa[1]
			
			dw_6.Object.cajas[ll_Fila]					=	dw_6.Object.pafr_ccajas[ll_Fila] - dw_6.Object.caja_traspa[ll_Fila]
			dw_6.Object.pafr_ccajas[ll_Fila]			=	dw_6.Object.pafr_ccajas[ll_Fila] - dw_6.Object.caja_traspa[ll_Fila]
						
			If li_repe_tipopa = 1 OR li_repe_tipopa = 2 OR li_repe_tipopa = 7 Then
				dw_5.Object.paen_estado[1]            =  1
			Else
				dw_6.Object.caja_traspa[ll_Fila]     	=	0
				dw_5.Object.paen_estado[1]             =	3
			End If
			
			ll_paen_numero = dw_3.Object.paen_numero[1]
			If ExistePallet(ll_paen_numero,False) Then
				dw_1.Object.repd_tipood[ll_NuevoDet]	= 1
			Else	
				dw_1.Object.repd_tipood[ll_NuevoDet]	= 2
			End If	
		
			ll_pafr_ccajas = dw_6.Object.pafr_ccajas[ll_Fila]
			If ll_pafr_ccajas = 0 OR IsNull(ll_pafr_ccajas) Then
				dw_1.Object.repd_marcao[ll_NuevoDet]	=	'E'
			End If
		End If
	//End If
NEXT

end subroutine

public function boolean integridad ();Boolean	lb_Retorno = True
Integer	li_especie, li_variedad, li_espe_codigo, li_paen_pmixto, li_repe_tipopa, li_categoria
String	ls_embalaje
Long		ll_Busca, ll_Fila, ll_fila_e, ll_caja_traspa


dw_5.AcceptText()
dw_6.AcceptText()

li_especie		= dw_5.Object.espe_codigo[1]
li_espe_codigo = dw_3.Object.espe_codigo[1]
	
IF li_especie <> li_espe_codigo THEN
	MessageBox('ERROR DE CONSISTENCIA', 'Esta intentando juntar dos especies distintas', STOPSIGN!)
	lb_retorno	= False
END IF

li_paen_pmixto = dw_3.Object.paen_pmixto[1]

IF li_paen_pmixto <> 1 THEN
	FOR ll_Fila = 1 TO dw_6.RowCount()	
		ll_caja_traspa = dw_6.Object.caja_traspa[ll_fila]
		IF ll_caja_traspa	> 0 THEN
			ls_Embalaje		=	dw_6.Object.emba_codigo[ll_Fila]
			li_variedad		=	dw_6.Object.vari_codigo[ll_Fila]
			li_categoria	=	dw_6.Object.vari_codigo[ll_Fila]
			
			IF dw_4.RowCount() > 0 THEN
				/*
				Embalaje
				*/	
				li_repe_tipopa = dw_2.Object.repe_tipopa[1]
				IF li_repe_tipopa <> 7 THEN  
					IF dw_4.Find("emba_codigo = '" + ls_embalaje + "'", 1, dw_4.RowCount()) = 0 AND dw_3.object.paen_pmixto[1] <> 1 THEN
						MessageBox('ERROR DE CONSISTENCIA', 'El Embalaje que se esta ingresando no esta contenido en el pallet Destino.')
						lb_Retorno = False
					END IF
					/*
					Variedad
					*/
					IF dw_4.Find('vari_codigo = ' + String(li_variedad), 1, dw_4.RowCount()) = 0 AND dw_3.object.paen_pmixto[1] <> 1 THEN
						MessageBox('ERROR DE CONSISTENCIA', 'La Variedad que se esta ingresando no esta contenida en el pallet Destino.')
						lb_Retorno = False
					END IF		
				END IF
			END IF
		END IF
	NEXT 
END IF	

Return lb_retorno
end function

public function integer buscarepallet ();str_busqueda lstr_busq

open(w_busqueda_spro_repalasigenca)

lstr_busq = message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	istr_Mant.Argumento[1] = lstr_busq.argum[1]
	istr_Mant.Argumento[2] = lstr_busq.argum[2]
	istr_Mant.Argumento[3] = lstr_busq.argum[3]
	TriggerEvent("ue_recuperadatos")
	il_existerepa = 1
END IF	

Return 1

end function

public function boolean existerepalletizado ();Integer li_cantidad, li_exportador, li_planta
long     ll_numero

li_exportador	=	Integer(istr_Mant.Argumento[1])
li_planta			=	Integer(istr_Mant.Argumento[2])
ll_numero     	=	Long(istr_Mant.Argumento[3])

SELECT Count(*) INTO :li_cantidad
	FROM dbo.repalletenca
	WHERE clie_codigo = :li_exportador
	AND plde_codigo    = :li_planta
	AND repe_numero  = :ll_numero;
 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla repalletenca")

	RETURN FALSE
ELSE	
 	IF isnull(li_cantidad) THEN li_cantidad = 0
	 
	IF li_cantidad > 0 THEN 
		RETURN TRUE
	ELSE
		RETURN FALSE
	END IF	
END IF

end function

public function integer buscapallets ();string ls_null,ls_Estado[3] = {'Existencia','Despachado','Repalletizado'}
Integer li_repe_tipopa
Long ll_repe_nrosag
str_busqueda lstr_busq
SetNull(ls_null)

openWithParm(w_busqueda_pallets_repal,istr_mant)

lstr_busq = message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	istr_Mant.Argumento[1] = lstr_busq.argum[1]
	istr_Mant.Argumento[2] = lstr_busq.argum[2]
	istr_Mant.Argumento[4] = lstr_busq.argum[3]	
		
	IF ExistePallet(Long(istr_Mant.Argumento[4]),False) THEN
		
		li_repe_tipopa = dw_2.Object.repe_tipopa[1]
		
		IF li_repe_tipopa = 3 THEN
			MessageBox("Atención", "El pallet debe ser nuevo.")
			dw_3.SetItem(1, 'paen_numero', Long(ls_Null))
			Return 1
		END IF	
		
		ib_detalle = true
		li_repe_tipopa = dw_2.Object.repe_tipopa[1]
		IF li_repe_tipopa = 6 AND ib_detalle THEN
			
			MessageBox("Atención", "Número de pallet " + String(Long(lstr_busq.argum[3]), '00000000') + &
					", ya se encuentra ingresado. Para Cambio de Folio, el pallet destino debe ser nuevo. " + &
					"~r~rIngrese otro número de pallet.")
			dw_3.SetItem(1, "paen_numero", Long(ls_Null))
			istr_Mant.Argumento[4]	=	""
			RETURN 1
		END IF	
		
		IF il_EstadoPallet > 1 AND ib_detalle THEN
			MessageBox("Atención", "Número de pallet " + String(Long(lstr_busq.argum[3]), '00000000') + &
							", " + ls_Estado[il_EstadoPallet] + &
							"~r~rIngrese otro número de pallet.")
			dw_3.SetItem(1, "paen_numero", Long(ls_Null))
			istr_Mant.Argumento[4]	=	""
			RETURN 1
		END IF	
		
		IF il_inspeccion >= 1 THEN
			ib_inspeccion_destino =  TRUE
			MessageBox("Atención", "Trabajará con un pallet Inspeccionado.")
		END If		
		
		IF il_inspeccion = 5 THEN
			MessageBox("Atención", "El Pallet se Encuentra con Inspección Pendiente.")
				dw_3.SetItem(1, "paen_numero", Long(ls_Null))
				Return 1
		END IF
				
		ll_repe_nrosag = dw_2.Object.repe_nrosag[1]
		
		IF ll_repe_nrosag > 0 THEN
			IF IsNull(il_inspeccion) THEN il_inspeccion = 0
			IF il_inspeccion = 0 THEN
				MessageBox("Atención", "El pallet debe estar Inspeccionado.")
				dw_3.SetItem(1, "paen_numero", Long(ls_Null))
				Return 1
			END IF
		END  IF
		
		TriggerEvent("ue_recuperapallet_destino")				
		
		dw_3.Object.paen_estado[1]			=	1
		is_marca = 'M'
		
		dw_5.Object.paen_numero.Protect	=	0
		dw_5.Object.paen_numero.BackGround.Color = RGB(255,255,255)
		
		dw_5.Object.paen_tipopa.Protect	=	0
		dw_5.Object.paen_tipopa.BackGround.Color = RGB(255,255,255)
		
		is_palletdest = Mid(lstr_busq.argum[3],len(lstr_busq.argum[3]) - 6, 7)
		
		IF ib_detalle THEN
			ib_ExistePalletDestino	=	True
		ELSE
			ib_ExistePalletDestino	=	FaLSe
		END IF	
		
		dw_5.SetColumn("paen_numero")
		dw_5.SetFocus()
		
		dw_3.Object.paen_numero.Protect	=	1
		dw_3.Object.paen_numero.BackGround.Color = RGB(166,180,210)

	END IF	
END IF	

Return 1
end function

public subroutine grabahistoria_store (long numero, integer tipo);Long  ll_fila, ll_fila_d, ll_pallet, ll_find, ll_fila_d2, ll_pallet2, ll_find2
Integer li_cliente, li_planta

li_cliente 	= dw_5.Object.clie_codigo[1]
li_planta 	= dw_5.Object.plde_codigo[1]
ll_pallet 	= dw_5.Object.paen_numero[1]
ll_pallet2 	= dw_3.Object.paen_numero[1]
	
ll_find	=	ids_palletencabhisto.Find ( "clie_codigo = " + String(li_cliente) + &
								" AND paen_numero = " + String(ll_pallet2) + &
								" AND plde_codigo = " + String(li_planta), 1, ids_palletencabhisto.RowCount() )
								
ll_find2	=	ids_palletencabhisto.Find ( "clie_codigo = " + String(li_cliente) + &
								" AND paen_numero = " + String(ll_pallet) + &
								" AND plde_codigo = " + String(li_planta), 1, ids_palletencabhisto.RowCount() )
								
IF tipo = 1 THEN 
	IF il_existe > 0 THEN
		IF ll_find = 0 THEN
			ll_fila	= ids_palletencabhisto.InsertRow(0)
				
			ids_palletencabhisto.SetItem(ll_fila,"clie_codigo",dw_3.Object.clie_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"paen_numero",dw_3.Object.paen_numero[1])
			ids_palletencabhisto.SetItem(ll_fila,"plde_codigo",dw_3.Object.plde_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_tipopa",dw_3.Object.paen_tipopa[1])
			ids_palletencabhisto.SetItem(ll_fila,"tpem_codigo",dw_3.Object.tpem_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"espe_codigo",dw_3.Object.espe_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"vari_codigo",dw_3.Object.vari_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"tiem_codigo",dw_3.Object.tiem_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"emba_codigo",dw_3.Object.emba_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"cate_codigo",dw_3.Object.cate_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"etiq_codigo",dw_3.Object.etiq_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"stat_codigo",dw_3.Object.stat_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"trat_codigo",dw_3.Object.trat_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"frio_codigo",dw_3.Object.frio_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"cond_codigo",dw_3.Object.cond_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"dest_codigo",dw_3.Object.dest_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_fecemb",dw_3.Object.paen_fecemb[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_cosecha",dw_3.Object.paen_cosecha[1])
			ids_palletencabhisto.SetItem(ll_fila,"paen_altura",dw_3.Object.paen_altura[1])
			ids_palletencabhisto.SetItem(ll_fila,"paen_ccajas",dw_3.Object.paen_ccajas[1])
			ids_palletencabhisto.SetItem(ll_fila,"tmvp_codigo",dw_3.Object.tmvp_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"paen_fecini",dw_3.Object.paen_fecini[1])
			ids_palletencabhisto.SetItem(ll_fila,"paen_horain",dw_3.Object.paen_horain[1])
			ids_palletencabhisto.SetItem(ll_fila,"cama_codigo",dw_3.Object.cama_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_calle",dw_3.Object.paen_calle[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_base",dw_3.Object.paen_base[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_posici",dw_3.Object.paen_posici[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_estado",dw_3.Object.paen_estado[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_inspec",dw_3.Object.paen_inspec[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_concal",dw_3.Object.paen_concal[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_pexpor",dw_3.Object.paen_pexpor[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_pmixto",dw_3.Object.paen_pmixto[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_proces",numero)
		END IF		
	END IF
	
	IF ll_find2 = 0 THEN 
		ll_fila	= ids_palletencabhisto.InsertRow(0)
	
		ids_palletencabhisto.SetItem(ll_fila,"clie_codigo",dw_5.Object.clie_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"paen_numero",dw_5.Object.paen_numero[1])
		ids_palletencabhisto.SetItem(ll_fila,"plde_codigo",dw_5.Object.plde_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_tipopa",dw_5.Object.paen_tipopa[1])
		ids_palletencabhisto.SetItem(ll_fila,"tpem_codigo",dw_5.Object.tpem_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"espe_codigo",dw_5.Object.espe_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"vari_codigo",dw_5.Object.vari_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"tiem_codigo",dw_5.Object.tiem_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"emba_codigo",dw_5.Object.emba_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"cate_codigo",dw_5.Object.cate_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"etiq_codigo",dw_5.Object.etiq_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"stat_codigo",dw_5.Object.stat_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"trat_codigo",dw_5.Object.trat_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"frio_codigo",dw_5.Object.frio_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"cond_codigo",dw_5.Object.cond_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"dest_codigo",dw_5.Object.dest_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_fecemb",dw_5.Object.paen_fecemb[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_cosecha",dw_5.Object.paen_cosecha[1])
		ids_palletencabhisto.SetItem(ll_fila,"paen_altura",dw_5.Object.paen_altura[1])
		ids_palletencabhisto.SetItem(ll_fila,"paen_ccajas",dw_5.Object.paen_ccajas[1])
		ids_palletencabhisto.SetItem(ll_fila,"tmvp_codigo",dw_5.Object.tmvp_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"paen_fecini",dw_5.Object.paen_fecini[1])
		ids_palletencabhisto.SetItem(ll_fila,"paen_horain",dw_5.Object.paen_horain[1])
		ids_palletencabhisto.SetItem(ll_fila,"cama_codigo",dw_5.Object.cama_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_calle",dw_5.Object.paen_calle[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_base",dw_5.Object.paen_base[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_posici",dw_5.Object.paen_posici[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_estado",dw_5.Object.paen_estado[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_inspec",dw_5.Object.paen_inspec[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_concal",dw_5.Object.paen_concal[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_pexpor",dw_5.Object.paen_pexpor[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_pmixto",dw_5.Object.paen_pmixto[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_proces",numero)
		ids_palletencabhisto.SetItem(ll_fila,"cajas_traspa",dw_6.Object.total_traspa[1])
	END IF			
ELSE
	IF ll_find2 = 0 THEN 
		FOR ll_fila_d = 1 TO dw_6.RowCount()	
			ll_fila	= 	ids_palletfrutahisto.InsertRow(0)   
			ids_palletfrutahisto.SetItem(ll_fila,"clie_codigo",dw_6.Object.clie_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"paen_numero",dw_6.Object.paen_numero[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"espe_codigo",dw_6.Object.espe_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"vari_codigo",dw_6.Object.vari_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"emba_codigo",dw_6.Object.emba_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"prod_codigo",dw_6.Object.prod_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"cond_codigo",dw_6.Object.cond_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"etiq_codigo",dw_6.Object.etiq_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"plde_codigo",dw_6.Object.plde_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafh_calibr",dw_6.Object.pafr_calibr[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafh_secuen",dw_6.Object.pafr_secuen[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafh_ccajas",dw_6.Object.pafr_ccajas[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafh_nrlote",dw_6.Object.pafr_nrlote[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafh_proces",numero)
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_fecing",dw_6.Object.pafr_fecing[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_fecemb",dw_6.Object.pafr_fecemb[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_copack",dw_6.Object.pafr_copack[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_tipdoc",3)
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_huert2",dw_6.Object.pafr_huert2[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_huert1",dw_6.Object.pafr_huert1[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_cuart1",dw_6.Object.pafr_cuart1[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_cuart2",dw_6.Object.pafr_cuart2[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_docrel",dw_6.Object.pafr_docrel[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_varrot",dw_6.Object.pafr_varrot[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"cajas_traspa",dw_6.Object.caja_traspa[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_huert4",dw_6.Object.pafr_huert4[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_cuart4",dw_6.Object.pafr_cuart4[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_rotpak",dw_6.Object.pafr_rotpak[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_prdrot",dw_6.Object.pafr_prdrot[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_calrot",dw_6.Object.pafr_calrot[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_fecrot",dw_6.Object.pafr_fecrot[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_embrea",dw_6.Object.pafr_embrea[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"cate_codigo",dw_6.Object.cate_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_catrot",dw_6.Object.pafr_catrot[ll_fila_d])
		NEXT
	END IF
	
	IF il_existe > 0 THEN
		IF ll_find = 0 THEN
			FOR ll_fila_d = 1 TO dw_4.RowCount()	
				ll_fila	= 	ids_palletfrutahisto.InsertRow(0)   
					
				ids_palletfrutahisto.SetItem(ll_fila,"clie_codigo",dw_4.Object.clie_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"paen_numero",dw_4.Object.paen_numero[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"espe_codigo",dw_4.Object.espe_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"vari_codigo",dw_4.Object.vari_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"emba_codigo",dw_4.Object.emba_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"prod_codigo",dw_4.Object.prod_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"cond_codigo",dw_4.Object.cond_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"etiq_codigo",dw_4.Object.etiq_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"plde_codigo",dw_4.Object.plde_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafh_calibr",dw_4.Object.pafr_calibr[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafh_secuen",dw_4.Object.pafr_secuen[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafh_ccajas",dw_4.Object.pafr_ccajas[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafh_nrlote",dw_4.Object.pafr_nrlote[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafh_proces",numero)
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_fecing",dw_4.Object.pafr_fecing[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_fecemb",dw_4.Object.pafr_fecemb[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_copack",dw_4.Object.pafr_copack[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_tipdoc",3)
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_huert1",dw_4.Object.pafr_huert1[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_cuart1",dw_4.Object.pafr_cuart1[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_huert2",dw_4.Object.pafr_huert2[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_cuart2",dw_4.Object.pafr_cuart2[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_docrel",dw_4.Object.pafr_docrel[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_varrot",dw_4.Object.pafr_varrot[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_huert4",dw_4.Object.pafr_huert4[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_cuart4",dw_4.Object.pafr_cuart4[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_prdrot",dw_4.Object.pafr_prdrot[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_calrot",dw_4.Object.pafr_calrot[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_rotpak",dw_4.Object.pafr_rotpak[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_fecrot",dw_4.Object.pafr_fecrot[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_embrea",dw_4.Object.pafr_embrea[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"cate_codigo",dw_4.Object.cate_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_catrot",dw_4.Object.pafr_catrot[ll_fila_d])
			NEXT
		END IF
	END IF	
END IF

RETURN
end subroutine

public subroutine grabaalturas (long numero, integer tipo);Long ll_fila3,ll_fila2, fila_find, ll_numero, ll_nuevo
Integer li_repe_tipopa

li_repe_tipopa = dw_2.Object.repe_tipopa[1]

IF li_repe_tipopa = 1 OR li_repe_tipopa = 3 OR li_repe_tipopa = 7 THEN
	ll_numero = dw_5.Object.paen_numero[1]
ELSE	
	ll_numero = dw_3.Object.paen_numero[1]
END IF

fila_find =  ids_alpalletfruta.Find("paen_numero = " + String(ll_numero),&
				1, ids_alpalletfruta.RowCount())

IF fila_find = 0 THEN
	IF tipo = 0 THEN
		ll_nuevo = ids_alpalletfruta.InsertRow(0)
		
		ids_alpalletfruta.Object.plde_codigo[ll_nuevo] =	dw_2.Object.plde_codigo[1]
		ids_alpalletfruta.Object.altu_numero[ll_nuevo] =	numero
		ids_alpalletfruta.Object.clie_codigo[ll_nuevo] =	dw_2.Object.clie_codigo[1]
		
		li_repe_tipopa = dw_2.Object.repe_tipopa[1]
		IF li_repe_tipopa = 1 OR li_repe_tipopa = 3 OR li_repe_tipopa = 7 THEN
			ids_alpalletfruta.Object.paen_numero[ll_nuevo] =	dw_5.Object.paen_numero[1]
		ELSE	
			ids_alpalletfruta.Object.paen_numero[ll_nuevo] =	dw_3.Object.paen_numero[1]
		END IF	
		ids_alpalletfruta.Object.alpf_fecmov[ll_nuevo] =	dw_2.Object.repe_fecrep[1]
	ELSE
		ids_alpalletencab.Object.plde_codigo[1]	=	dw_2.Object.plde_codigo[1]
		ids_alpalletencab.Object.altu_numero[1]	=	numero
		ids_alpalletencab.Object.clie_codigo[1]	=	dw_2.Object.clie_codigo[1]
		ids_alpalletencab.Object.altu_fecmov[1]	=	dw_2.Object.repe_fecrep[1]
		ids_alpalletencab.Object.altu_observ[1] = 	'Repalletizaje'
	END IF
END IF	

IF li_repe_tipopa = 1 OR li_repe_tipopa = 3 OR li_repe_tipopa = 7 THEN
	ll_numero = dw_3.Object.paen_numero[1]
ELSE	
	ll_numero = dw_5.Object.paen_numero[1]
END IF

IF il_existe > 0 THEN
	fila_find =  ids_alpalletfruta.Find("paen_numero = " + String(ll_numero),&
				1, ids_alpalletfruta.RowCount())
	
	IF fila_find = 0 THEN
		IF tipo = 0 THEN
			ll_nuevo = ids_alpalletfruta.InsertRow(0)
			
			ids_alpalletfruta.Object.plde_codigo[ll_nuevo] =	dw_2.Object.plde_codigo[1]
			ids_alpalletfruta.Object.altu_numero[ll_nuevo] =	numero
			ids_alpalletfruta.Object.clie_codigo[ll_nuevo] =	dw_2.Object.clie_codigo[1]
			IF dw_4.RowCount() = 0 THEN
				li_repe_tipopa = dw_2.Object.repe_tipopa[1]
				IF li_repe_tipopa = 1 OR li_repe_tipopa = 3 OR li_repe_tipopa = 7 THEN
					ids_alpalletfruta.Object.paen_numero[ll_nuevo] =	dw_3.Object.paen_numero[1]
				ELSE	
					ids_alpalletfruta.Object.paen_numero[ll_nuevo] =	dw_5.Object.paen_numero[1]
				END IF	
			ELSE
				IF dw_2.Object.repe_tipopa[1] = 1 OR li_repe_tipopa = 3 OR li_repe_tipopa = 7 THEN
					ids_alpalletfruta.Object.paen_numero[ll_nuevo] =	dw_3.Object.paen_numero[1]
				ELSE
					ids_alpalletfruta.Object.paen_numero[ll_nuevo] =	dw_5.Object.paen_numero[1]
				END IF	
			END IF
			ids_alpalletfruta.Object.alpf_fecmov[ll_nuevo] =	dw_2.Object.repe_fecrep[1]
		END IF
	END IF	
END IF	
end subroutine

public subroutine grabahistoria_storealtura (long numero, integer tipo);Long  ll_fila, ll_fila_d, ll_pallet, ll_find, ll_fila_d2, ll_find2, ll_find3, ll_pallet2
Integer li_cliente, li_planta

li_cliente 	= dw_5.Object.clie_codigo[1]
li_planta 	= dw_5.Object.plde_codigo[1]
ll_pallet 	= dw_3.Object.paen_numero[1]
ll_pallet2 	= dw_5.Object.paen_numero[1]
	
ll_find	=	ids_palletencabhisto.Find ( "clie_codigo = " + String(li_cliente) + &
						" AND paen_numero = " + String(ll_pallet) + &
						" AND plde_codigo = " + String(li_planta), 1, ids_palletencabhisto.RowCount() )

ll_find2	=	ids_palletencabhisto.Find ( "clie_codigo = " + String(li_cliente) + &
						" AND paen_numero = " + String(ll_pallet2) + &
						" AND plde_codigo = " + String(li_planta), 1, ids_palletencabhisto.RowCount() )
								
IF tipo = 1 THEN  
	
	IF ll_find2 = 0 THEN
		ll_fila	= ids_palletencabhisto.InsertRow(0)
		
		ids_palletencabhisto.SetItem(ll_fila,"clie_codigo",dw_5.Object.clie_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"paen_numero",dw_5.Object.paen_numero[1])
		ids_palletencabhisto.SetItem(ll_fila,"plde_codigo",dw_5.Object.plde_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_tipopa",dw_5.Object.paen_tipopa[1])
		ids_palletencabhisto.SetItem(ll_fila,"tpem_codigo",dw_5.Object.tpem_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"espe_codigo",dw_5.Object.espe_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"vari_codigo",dw_5.Object.vari_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"tiem_codigo",dw_5.Object.tiem_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"emba_codigo",dw_5.Object.emba_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"cate_codigo",dw_5.Object.cate_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"etiq_codigo",dw_5.Object.etiq_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"stat_codigo",dw_5.Object.stat_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"trat_codigo",dw_5.Object.trat_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"frio_codigo",dw_5.Object.frio_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"cond_codigo",dw_5.Object.cond_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"dest_codigo",dw_5.Object.dest_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_fecemb",dw_5.Object.paen_fecemb[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_cosecha",dw_5.Object.paen_cosecha[1])
		ids_palletencabhisto.SetItem(ll_fila,"paen_altura",dw_5.Object.paen_altura[1])
		ids_palletencabhisto.SetItem(ll_fila,"paen_ccajas",dw_5.Object.paen_ccajas[1])
		ids_palletencabhisto.SetItem(ll_fila,"tmvp_codigo",dw_5.Object.tmvp_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"paen_fecini",dw_5.Object.paen_fecini[1])
		ids_palletencabhisto.SetItem(ll_fila,"paen_horain",dw_5.Object.paen_horain[1])
		ids_palletencabhisto.SetItem(ll_fila,"cama_codigo",dw_5.Object.cama_codigo[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_calle",dw_5.Object.paen_calle[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_base",dw_5.Object.paen_base[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_posici",dw_5.Object.paen_posici[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_estado",dw_5.Object.paen_estado[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_inspec",dw_5.Object.paen_inspec[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_concal",dw_5.Object.paen_concal[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_pexpor",dw_5.Object.paen_pexpor[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_pmixto",dw_5.Object.paen_pmixto[1])
		ids_palletencabhisto.SetItem(ll_fila,"pahi_proces",numero)
		ids_palletencabhisto.SetItem(ll_fila,"cajas_traspa",dw_6.Object.total_traspa[1])	
	END IF

	IF il_existe > 0 THEN
		IF ll_find = 0 THEN
			ll_fila	= ids_palletencabhisto.InsertRow(0)
			
			ids_palletencabhisto.SetItem(ll_fila,"clie_codigo",dw_3.Object.clie_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"paen_numero",dw_3.Object.paen_numero[1])
			ids_palletencabhisto.SetItem(ll_fila,"plde_codigo",dw_3.Object.plde_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_tipopa",dw_3.Object.paen_tipopa[1])
			ids_palletencabhisto.SetItem(ll_fila,"tpem_codigo",dw_3.Object.tpem_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"espe_codigo",dw_3.Object.espe_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"vari_codigo",dw_3.Object.vari_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"tiem_codigo",dw_3.Object.tiem_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"emba_codigo",dw_3.Object.emba_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"cate_codigo",dw_3.Object.cate_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"etiq_codigo",dw_3.Object.etiq_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"stat_codigo",dw_3.Object.stat_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"trat_codigo",dw_3.Object.trat_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"frio_codigo",dw_3.Object.frio_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"cond_codigo",dw_3.Object.cond_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"dest_codigo",dw_3.Object.dest_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_fecemb",dw_3.Object.paen_fecemb[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_cosecha",dw_3.Object.paen_cosecha[1])
			ids_palletencabhisto.SetItem(ll_fila,"paen_altura",dw_3.Object.paen_altura[1])
			ids_palletencabhisto.SetItem(ll_fila,"paen_ccajas",dw_3.Object.paen_ccajas[1])
			ids_palletencabhisto.SetItem(ll_fila,"tmvp_codigo",dw_3.Object.tmvp_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"paen_fecini",dw_3.Object.paen_fecini[1])
			ids_palletencabhisto.SetItem(ll_fila,"paen_horain",dw_3.Object.paen_horain[1])
			ids_palletencabhisto.SetItem(ll_fila,"cama_codigo",dw_3.Object.cama_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_calle",dw_3.Object.paen_calle[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_base",dw_3.Object.paen_base[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_posici",dw_3.Object.paen_posici[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_estado",dw_3.Object.paen_estado[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_inspec",dw_3.Object.paen_inspec[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_concal",dw_3.Object.paen_concal[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_pexpor",dw_3.Object.paen_pexpor[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_pmixto",dw_3.Object.paen_pmixto[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_proces",numero)
		END IF
	END IF
ELSE
	IF ll_find2 = 0 THEN
		FOR ll_fila_d = 1 TO dw_6.RowCount()	
			ll_fila	= 	ids_palletfrutahisto.InsertRow(0)   
			
			ids_palletfrutahisto.SetItem(ll_fila,"clie_codigo",dw_6.Object.clie_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"paen_numero",dw_6.Object.paen_numero[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"espe_codigo",dw_6.Object.espe_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"vari_codigo",dw_6.Object.vari_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"emba_codigo",dw_6.Object.emba_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"prod_codigo",dw_6.Object.prod_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"cond_codigo",dw_6.Object.cond_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"etiq_codigo",dw_6.Object.etiq_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"plde_codigo",dw_6.Object.plde_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafh_calibr",dw_6.Object.pafr_calibr[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafh_secuen",dw_6.Object.pafr_secuen[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafh_ccajas",dw_6.Object.pafr_ccajas[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafh_nrlote",dw_6.Object.pafr_nrlote[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafh_proces",numero)
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_fecing",dw_6.Object.pafr_fecing[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_fecemb",dw_6.Object.pafr_fecemb[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_copack",dw_6.Object.pafr_copack[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_tipdoc",3)
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_huert1",dw_6.Object.pafr_huert1[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_cuart1",dw_6.Object.pafr_cuart1[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_huert2",dw_6.Object.pafr_huert2[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_cuart2",dw_6.Object.pafr_cuart2[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_docrel",dw_6.Object.pafr_docrel[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_varrot",dw_6.Object.pafr_varrot[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"cajas_traspa",dw_6.Object.caja_traspa[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_huert4",dw_6.Object.pafr_huert4[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_cuart4",dw_6.Object.pafr_cuart4[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_rotpak",dw_6.Object.pafr_rotpak[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_calrot",dw_6.Object.pafr_calrot[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_prdrot",dw_6.Object.pafr_prdrot[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_fecrot",dw_6.Object.pafr_fecrot[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_embrea",dw_6.Object.pafr_embrea[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"cate_codigo",dw_6.Object.cate_codigo[ll_fila_d])
			ids_palletfrutahisto.SetItem(ll_fila,"pafr_catrot",dw_6.Object.pafr_catrot[ll_fila_d])
		NEXT	
	END IF	
	
	IF il_existe > 0 THEN
		IF ll_find = 0 THEN
			FOR ll_fila_d2 = 1 TO dw_4.RowCount()	
				ll_fila	= 	ids_palletfrutahisto.InsertRow(0)   
				
				ids_palletfrutahisto.SetItem(ll_fila,"clie_codigo",dw_4.Object.clie_codigo[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"paen_numero",dw_4.Object.paen_numero[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"espe_codigo",dw_4.Object.espe_codigo[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"vari_codigo",dw_4.Object.vari_codigo[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"emba_codigo",dw_4.Object.emba_codigo[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"prod_codigo",dw_4.Object.prod_codigo[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"cond_codigo",dw_4.Object.cond_codigo[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"etiq_codigo",dw_4.Object.etiq_codigo[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"plde_codigo",dw_4.Object.plde_codigo[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafh_calibr",dw_4.Object.pafr_calibr[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafh_secuen",dw_4.Object.pafr_secuen[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafh_ccajas",dw_4.Object.pafr_ccajas[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafh_nrlote",dw_4.Object.pafr_nrlote[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafh_proces",numero)
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_fecing",dw_4.Object.pafr_fecing[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_fecemb",dw_4.Object.pafr_fecemb[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_copack",dw_4.Object.pafr_copack[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_tipdoc",3)
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_huert1",dw_4.Object.pafr_huert1[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_cuart1",dw_4.Object.pafr_cuart1[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_huert2",dw_4.Object.pafr_huert2[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_cuart2",dw_4.Object.pafr_cuart2[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_docrel",dw_4.Object.pafr_docrel[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_varrot",dw_4.Object.pafr_varrot[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_huert4",dw_4.Object.pafr_huert1[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_cuart4",dw_4.Object.pafr_cuart1[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_calrot",dw_4.Object.pafr_calrot[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_prdrot",dw_4.Object.pafr_prdrot[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_rotpak",dw_4.Object.pafr_rotpak[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_fecrot",dw_4.Object.pafr_fecrot[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_embrea",dw_4.Object.pafr_embrea[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"cate_codigo",dw_4.Object.cate_codigo[ll_fila_d2])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_catrot",dw_4.Object.pafr_catrot[ll_fila_d2])
			NEXT
		END IF
	END IF	
END IF

RETURN
end subroutine

public subroutine traspasa_cajaspalfruta_encab ();Long	ll_Fila, ll_BFilaRep, ll_BFilaPD, ll_Nuevo, ll_NuevoDet, ll_fila1, ll_nuevo1, ll_find
Integer li_Secuen, li_repe_tipopa

dw_10.SetTransObject(sqlca)
dw_9.SetTransObject(sqlca)

li_repe_tipopa = dw_2.Object.repe_tipopa[1]

FOR ll_fila = 1 TO dw_6.Rowcount() 
	ll_Nuevo = dw_10.InsertRow(0)
	
	dw_10.Object.clie_codigo[ll_Nuevo]	=	dw_6.Object.clie_codigo[ll_Fila]
	dw_10.Object.plde_codigo[ll_Nuevo]	=	dw_6.Object.plde_codigo[ll_Fila]
	dw_10.Object.paen_numero[ll_Nuevo]	=	dw_6.Object.paen_numero[ll_Fila]
	dw_10.Object.pafr_secuen[ll_Nuevo]	=	dw_6.Object.pafr_secuen[ll_Fila]
	dw_10.Object.espe_codigo[ll_Nuevo]	=	dw_6.Object.espe_codigo[ll_Fila]
	dw_10.Object.vari_codigo[ll_Nuevo]	=	dw_6.Object.vari_codigo[ll_Fila]
	dw_10.Object.vari_nombre[ll_Nuevo]	=	dw_6.Object.vari_nombre[ll_Fila]
	dw_10.Object.pafr_varrot[ll_Nuevo]	=	dw_6.Object.pafr_varrot[ll_Fila]
	dw_10.Object.prod_codigo[ll_Nuevo]	=	dw_6.Object.prod_codigo[ll_Fila]
	dw_10.Object.pafr_prdrot[ll_Nuevo]	=	dw_6.Object.pafr_prdrot[ll_Fila]
	dw_10.Object.emba_codigo[ll_Nuevo]	=	dw_6.Object.emba_codigo[ll_Fila]
	dw_10.Object.etiq_codigo[ll_Nuevo]	=	dw_6.Object.etiq_codigo[ll_Fila]
	dw_10.Object.pafr_huert1[ll_Nuevo]	=	dw_6.Object.pafr_huert1[ll_Fila]
	dw_10.Object.pafr_cuart1[ll_Nuevo]	=	dw_6.Object.pafr_cuart1[ll_Fila]
	//dw_10.Object.pafr_huert2[ll_Nuevo]	=	dw_6.Object.pafr_huert2[ll_Fila]
	dw_10.Object.pafr_cuart2[ll_Nuevo]	=	dw_6.Object.pafr_cuart2[ll_Fila]
	dw_10.Object.pafr_huert4[ll_Nuevo]	=	dw_6.Object.pafr_huert4[ll_Fila]
	dw_10.Object.pafr_cuart4[ll_Nuevo]	=	dw_6.Object.pafr_cuart4[ll_Fila]
	dw_10.Object.pafr_nrlote[ll_Nuevo]	=	dw_6.Object.pafr_nrlote[ll_Fila]
	dw_10.Object.pafr_calibr[ll_Nuevo]   =	dw_6.Object.pafr_calibr[ll_Fila]
	dw_10.Object.pafr_fecemb[ll_Nuevo]	=	dw_6.Object.pafr_fecemb[ll_Fila]
	dw_10.Object.prod_nombre[ll_Nuevo]	=	dw_6.Object.prod_nombre[ll_Fila]
	dw_10.Object.cond_codigo[ll_Nuevo]	=	dw_6.Object.cond_codigo[ll_Fila]
	dw_10.Object.pafr_fecing[ll_Nuevo]	=	dw_6.Object.pafr_fecing[ll_Fila]
	dw_10.Object.pafr_copack[ll_nuevo]  =  dw_6.Object.pafr_copack[ll_Fila]
	dw_10.Object.pafr_ccajas[ll_nuevo]	=	dw_6.Object.pafr_ccajas[ll_Fila] - dw_6.Object.caja_traspa[ll_Fila]
	dw_10.Object.cajas[ll_nuevo]  		=	dw_6.Object.pafr_ccajas[ll_Fila] - dw_6.Object.caja_traspa[ll_Fila]
	dw_10.Object.pafr_docrel[ll_nuevo]  =  dw_6.Object.pafr_docrel[ll_Fila]
	dw_10.Object.caja_traspa[ll_nuevo]   = dw_6.Object.caja_traspa[ll_Fila]
	dw_10.Object.pafr_calrot[ll_nuevo]   = dw_6.Object.pafr_calrot[ll_Fila]
	dw_10.Object.pafr_rotpak[ll_nuevo]   = dw_6.Object.pafr_rotpak[ll_Fila]
	dw_10.Object.pafr_fecrot[ll_nuevo]   = dw_6.Object.pafr_fecrot[ll_Fila]
	dw_10.Object.pafr_embrea[ll_nuevo]   = dw_6.Object.pafr_embrea[ll_Fila]
	dw_10.Object.cama_nombre[ll_nuevo]   = dw_6.Object.cama_nombre[ll_Fila]
	dw_10.Object.cate_codigo[ll_nuevo]   = dw_6.Object.cate_codigo[ll_Fila]
	dw_10.Object.pafr_catrot[ll_nuevo]   = dw_6.Object.pafr_catrot[ll_Fila]
	
	IF li_repe_tipopa <> 7 THEN
		dw_10.Object.pafr_huert2[ll_Nuevo]	=	dw_6.Object.pafr_huert2[ll_Fila]
		dw_10.Object.pafr_huert3[ll_Nuevo]	=	dw_6.Object.pafr_huert3[ll_Fila]
		dw_10.Object.pafr_cuart2[ll_Nuevo]	=	dw_6.Object.pafr_cuart2[ll_Fila]
		dw_10.Object.pafr_cuart3[ll_Nuevo]	=	dw_6.Object.pafr_cuart3[ll_Fila]
		dw_10.Object.pafr_barra1[ll_Nuevo]	=	dw_6.Object.pafr_barra1[ll_Fila]
		dw_10.Object.pafr_barra2[ll_Nuevo]	=	dw_6.Object.pafr_barra2[ll_Fila]
		dw_10.Object.pafr_barra3[ll_Nuevo]	=	dw_6.Object.pafr_barra3[ll_Fila]
		dw_10.Object.pafr_barra4[ll_Nuevo]	=	dw_6.Object.pafr_barra4[ll_Fila]
	END IF	
NEXT

FOR ll_fila1 = 1 TO dw_5.RowCount()
	ll_nuevo1 = dw_9.InsertRow(0)
	
	dw_9.Object.clie_codigo[ll_nuevo1] =   dw_5.Object.clie_codigo[ll_fila1]    
	dw_9.Object.paen_numero[ll_nuevo1] =   dw_5.Object.paen_numero[ll_fila1]    
	dw_9.Object.plde_codigo[ll_nuevo1] =   dw_5.Object.plde_codigo[ll_fila1]    
	dw_9.Object.paen_tipopa[ll_nuevo1] =   dw_5.Object.paen_tipopa[ll_fila1]    
	dw_9.Object.tpem_codigo[ll_nuevo1] =   dw_5.Object.tpem_codigo[ll_fila1]    
	dw_9.Object.espe_codigo[ll_nuevo1] =   dw_5.Object.espe_codigo[ll_fila1]    
	dw_9.Object.vari_codigo[ll_nuevo1] =   dw_5.Object.vari_codigo[ll_fila1]    
	dw_9.Object.tiem_codigo[ll_nuevo1] =   dw_5.Object.tiem_codigo[ll_fila1]    
	dw_9.Object.cate_codigo[ll_nuevo1] =   dw_5.Object.cate_codigo[ll_fila1]    
	dw_9.Object.etiq_codigo[ll_nuevo1] =   dw_5.Object.etiq_codigo[ll_fila1]    
	dw_9.Object.stat_codigo[ll_nuevo1] =   dw_5.Object.stat_codigo[ll_fila1]    
	dw_9.Object.trat_codigo[ll_nuevo1] =   dw_5.Object.trat_codigo[ll_fila1]    
	dw_9.Object.frio_codigo[ll_nuevo1] =   dw_5.Object.frio_codigo[ll_fila1]    
	dw_9.Object.cond_codigo[ll_nuevo1] =   dw_5.Object.cond_codigo[ll_fila1]    
	dw_9.Object.dest_codigo[ll_nuevo1] =   dw_5.Object.dest_codigo[ll_fila1]    
	dw_9.Object.emba_codigo[ll_nuevo1] =   dw_5.Object.emba_codigo[ll_fila1]    
	dw_9.Object.paen_fecemb[ll_nuevo1] =   dw_5.Object.paen_fecemb[ll_fila1]    
	dw_9.Object.paen_cosecha[ll_nuevo1]=   dw_5.Object.paen_cosecha[ll_fila1]    
	dw_9.Object.paen_altura[ll_nuevo1] =   dw_5.Object.paen_altura[ll_fila1]    
	dw_9.Object.paen_ccajas[ll_nuevo1] =   dw_5.Object.paen_ccajas[ll_fila1]    
	dw_9.Object.tmvp_codigo[ll_nuevo1] =   dw_5.Object.tmvp_codigo[ll_fila1]    
	dw_9.Object.paen_fecini[ll_nuevo1] =   dw_5.Object.paen_fecini[ll_fila1]    
	dw_9.Object.paen_horain[ll_nuevo1] =   dw_5.Object.paen_horain[ll_fila1]    
	dw_9.Object.cama_codigo[ll_nuevo1] =   dw_5.Object.cama_codigo[ll_fila1]    
	dw_9.Object.paen_calle[ll_nuevo1]  =   dw_5.Object.paen_calle[ll_fila1]    
	dw_9.Object.paen_base[ll_nuevo1]   =   dw_5.Object.paen_base[ll_fila1]    
	dw_9.Object.paen_posici[ll_nuevo1] =   dw_5.Object.paen_posici[ll_fila1] 
	dw_9.Object.paen_inspec[ll_nuevo1] =   dw_5.Object.paen_inspec[ll_fila1]
		
	IF li_repe_tipopa = 3 THEN
		dw_9.Object.paen_estado[ll_nuevo1] =  3    
	ELSE
		dw_9.Object.paen_estado[ll_nuevo1] =  1    
	END IF	
	
	dw_9.Object.paen_concal[ll_nuevo1] =   dw_5.Object.paen_concal[ll_fila1] 
	dw_9.Object.paen_pexpor[ll_nuevo1] =   dw_5.Object.paen_pexpor[ll_fila1] 
	dw_9.Object.paen_pmixto[ll_nuevo1] =   dw_5.Object.paen_pmixto[ll_fila1]  
	dw_9.Object.paen_varrot[ll_nuevo1] =   dw_5.Object.paen_varrot[ll_fila1] 
	dw_9.Object.paen_nrasda[ll_nuevo1] =   dw_5.Object.paen_nrasda[ll_fila1] 
	dw_9.Object.prod_codigo[ll_nuevo1] =   dw_5.Object.prod_codigo[ll_fila1] 
	dw_9.Object.paen_calibr[ll_nuevo1] =   dw_5.Object.paen_calibr[ll_fila1] 
	dw_9.Object.copa_codigo[ll_nuevo1] =   dw_5.Object.copa_codigo[ll_fila1] 
	dw_9.Object.paen_huert1[ll_nuevo1] =   dw_5.Object.paen_huert1[ll_fila1] 
	dw_9.Object.paen_huert2[ll_nuevo1] =   dw_5.Object.paen_huert2[ll_fila1] 
	dw_9.Object.paen_huert3[ll_nuevo1] =   dw_5.Object.paen_huert3[ll_fila1] 
	dw_9.Object.paen_huert4[ll_nuevo1] =   dw_5.Object.paen_huert4[ll_fila1] 
	dw_9.Object.paen_cuart1[ll_nuevo1] =   dw_5.Object.paen_cuart1[ll_fila1] 
	dw_9.Object.paen_cuart2[ll_nuevo1] =   dw_5.Object.paen_cuart2[ll_fila1] 
	dw_9.Object.paen_cuart3[ll_nuevo1] =   dw_5.Object.paen_cuart3[ll_fila1] 
	dw_9.Object.paen_cuart4[ll_nuevo1] =   dw_5.Object.paen_cuart4[ll_fila1] 
	dw_9.Object.paen_barra1[ll_nuevo1] =   dw_5.Object.paen_barra1[ll_fila1] 
	dw_9.Object.paen_barra2[ll_nuevo1] =   dw_5.Object.paen_barra2[ll_fila1] 
	dw_9.Object.paen_barra3[ll_nuevo1] =   dw_5.Object.paen_barra3[ll_fila1] 
	dw_9.Object.paen_barra4[ll_nuevo1] =   dw_5.Object.paen_barra4[ll_fila1] 
	dw_9.Object.paen_pcopda[ll_nuevo1] =   dw_5.Object.paen_pcopda[ll_fila1] 
 	dw_9.Object.paen_ccajas[ll_nuevo1] =   dw_5.Object.paen_ccajas[ll_fila1] - dw_6.Object.total_traspa[1]
NEXT

end subroutine

public subroutine traspasa_cajas_nuevas ();Long	ll_Fila, ll_BFilaRep, ll_BFilaPD, ll_Nuevo, ll_NuevoDet, ll_fila1, ll_nuevo1, ll_find, &
ll_pallet, ll_cont, ll_fil, ll_nuevo2
Integer li_Secuen, li_cliente, li_planta, li_secuencia, fila_find, ll_filactualiza, li_tiporepa

ids_palletfrutanuevo.SetTransObject(sqlca)
ids_palletencabnuevo.SetTransObject(sqlca)

li_tiporepa = dw_2.Object.repe_tipopa[1]

//IF dw_4.RowCount() > 0 THEN
//	li_secuen = dw_4.Object.pafr_secuen[dw_4.RowCount()] + 1
//ELSE
//	li_secuen = 1
//END IF

FOR ll_fila = 1 TO dw_4.Rowcount() 
	
//	IF dw_4.RowCount() > 0 THEN
//		FOR li_secuencia = 1 TO dw_4.RowCount()
//			IF dw_4.Object.pafr_secuen[li_secuencia] < 999 THEN
//				li_secuen = dw_4.Object.pafr_secuen[li_secuencia] + 1
//			END IF	
//		NEXT	
//	ELSE
//		li_secuen = 1
//	END IF
	
	ll_Nuevo = ids_palletfrutanuevo.InsertRow(0)
	
	ids_palletfrutanuevo.Object.clie_codigo[ll_Nuevo]	=	dw_4.Object.clie_codigo[ll_Fila]
	ids_palletfrutanuevo.Object.plde_codigo[ll_Nuevo]	=	dw_4.Object.plde_codigo[ll_Fila]
	ids_palletfrutanuevo.Object.paen_numero[ll_Nuevo]	=	dw_4.Object.paen_numero[ll_Fila]
	
	//IF dw_4.Object.pafr_secuen[ll_Fila] > 999 THEN
		ids_palletfrutanuevo.Object.pafr_secuen[ll_Nuevo]	=	dw_4.Object.pafr_secuen[ll_Fila]
	//ELSE
	//	ids_palletfrutanuevo.Object.pafr_secuen[ll_Nuevo]	=	li_secuen
	//END IF	
	
	ids_palletfrutanuevo.Object.espe_codigo[ll_Nuevo]	=	dw_4.Object.espe_codigo[ll_Fila]
	ids_palletfrutanuevo.Object.vari_codigo[ll_Nuevo]	=	dw_4.Object.vari_codigo[ll_Fila]
	ids_palletfrutanuevo.Object.vari_nombre[ll_Nuevo]	=	dw_4.Object.vari_nombre[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_varrot[ll_Nuevo]	=	dw_4.Object.pafr_varrot[ll_Fila]
	ids_palletfrutanuevo.Object.prod_codigo[ll_Nuevo]	=	dw_4.Object.prod_codigo[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_prdrot[ll_Nuevo]	=	dw_4.Object.pafr_prdrot[ll_Fila]
	ids_palletfrutanuevo.Object.emba_codigo[ll_Nuevo]	=	dw_4.Object.emba_codigo[ll_Fila]
	ids_palletfrutanuevo.Object.etiq_codigo[ll_Nuevo]	=	dw_4.Object.etiq_codigo[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_huert1[ll_Nuevo]	=	dw_4.Object.pafr_huert1[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_cuart1[ll_Nuevo]	=	dw_4.Object.pafr_cuart1[ll_Fila]
	//ids_palletfrutanuevo.Object.pafr_huert2[ll_Nuevo]	=	dw_4.Object.pafr_huert2[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_cuart2[ll_Nuevo]	=	dw_4.Object.pafr_cuart2[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_huert4[ll_Nuevo]	=	dw_4.Object.pafr_huert4[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_cuart4[ll_Nuevo]	=	dw_4.Object.pafr_cuart4[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_nrlote[ll_Nuevo]	=	dw_4.Object.pafr_nrlote[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_calibr[ll_Nuevo]   =	dw_4.Object.pafr_calibr[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_fecemb[ll_Nuevo]	=	dw_4.Object.pafr_fecemb[ll_Fila]
	ids_palletfrutanuevo.Object.prod_nombre[ll_Nuevo]	=	dw_4.Object.prod_nombre[ll_Fila]
	ids_palletfrutanuevo.Object.cond_codigo[ll_Nuevo]	=	dw_4.Object.cond_codigo[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_cjssal[ll_Nuevo]	=	dw_4.Object.pafr_ccajas[ll_fila]
	ids_palletfrutanuevo.Object.pafr_fecing[ll_Nuevo]	=	dw_4.Object.pafr_fecing[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_copack[ll_nuevo]  =  dw_4.Object.pafr_copack[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_ccajas[ll_nuevo]	=	dw_4.Object.pafr_ccajas[ll_Fila] 
	ids_palletfrutanuevo.Object.pafr_docrel[ll_nuevo]	=	dw_4.Object.pafr_docrel[ll_Fila] 
	ids_palletfrutanuevo.Object.pafr_calrot[ll_nuevo]	=	dw_4.Object.pafr_calrot[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_rotpak[ll_nuevo]	=	dw_4.Object.pafr_rotpak[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_fecrot[ll_nuevo]	=	dw_4.Object.pafr_fecrot[ll_Fila] 
	ids_palletfrutanuevo.Object.pafr_embrea[ll_nuevo]	=	dw_4.Object.pafr_embrea[ll_Fila]
	ids_palletfrutanuevo.Object.cama_nombre[ll_nuevo]	=	dw_4.Object.cama_nombre[ll_Fila]
	ids_palletfrutanuevo.Object.cate_codigo[ll_nuevo]	=	dw_4.Object.cate_codigo[ll_Fila]
	ids_palletfrutanuevo.Object.pafr_catrot[ll_nuevo]	=	dw_4.Object.pafr_catrot[ll_Fila]
	
	IF li_tiporepa <> 7 THEN
		ids_palletfrutanuevo.Object.pafr_huert2[ll_Nuevo]	=	dw_4.Object.pafr_huert2[ll_Fila]
		ids_palletfrutanuevo.Object.pafr_huert3[ll_Nuevo]	=	dw_4.Object.pafr_huert3[ll_Fila]
		ids_palletfrutanuevo.Object.pafr_cuart2[ll_Nuevo]	=	dw_4.Object.pafr_cuart2[ll_Fila]
		ids_palletfrutanuevo.Object.pafr_cuart3[ll_Nuevo]	=	dw_4.Object.pafr_cuart3[ll_Fila]
		ids_palletfrutanuevo.Object.pafr_barra1[ll_Nuevo]	=	dw_4.Object.pafr_barra1[ll_Fila]
		ids_palletfrutanuevo.Object.pafr_barra2[ll_Nuevo]	=	dw_4.Object.pafr_barra2[ll_Fila]
		ids_palletfrutanuevo.Object.pafr_barra3[ll_Nuevo]	=	dw_4.Object.pafr_barra3[ll_Fila]
		ids_palletfrutanuevo.Object.pafr_barra4[ll_Nuevo]	=	dw_4.Object.pafr_barra4[ll_Fila]
	END IF	
//	IF isnull(dw_4.Object.pafr_nroori[ll_Fila]) OR &
//					dw_4.Object.pafr_nroori[ll_Fila] = 0 THEN
//					
//		ids_palletfrutanuevo.Object.pafr_nroori[ll_nuevo] = dw_4.Object.paen_numero[ll_Fila]
//	ELSE	

		ids_palletfrutanuevo.Object.pafr_nroori[ll_nuevo] = dw_4.Object.pafr_nroori[ll_Fila]
//	END IF
	
//	IF isnull(dw_4.Object.pafr_secori[ll_Fila]) OR &
//				dw_4.Object.pafr_secori[ll_Fila] = 0 THEN
//				
//		ids_palletfrutanuevo.Object.pafr_secori[ll_nuevo] = dw_4.Object.pafr_secuen[ll_Fila]
//	ELSE	
		ids_palletfrutanuevo.Object.pafr_secori[ll_nuevo] = dw_4.Object.pafr_secori[ll_Fila]
//	END IF
		
	
NEXT

ll_pallet = dw_3.Object.paen_numero[1] 
li_cliente = dw_2.Object.clie_codigo[1]
li_planta = dw_2.Object.plde_codigo[1]

SELECT count(*)
	INTO :ll_cont
	FROM dbo.palletencab
	WHERE paen_numero = :ll_pallet 
	AND	clie_codigo = :li_cliente
	AND	plde_codigo = :li_planta;

IF ll_cont > 0 THEN
	FOR ll_nuevo2 = 1 TO dw_3.RowCount()
		ids_palletencabupdate.Object.paen_ccajas[ll_nuevo2] =   dw_3.Object.paen_ccajas[1] + dw_6.Object.total_traspa[1]
		ids_palletencabupdate.Object.paen_numero[ll_nuevo2] =   dw_3.Object.paen_numero[1] 
		ids_palletencabupdate.Object.clie_codigo[ll_nuevo2] =   dw_3.Object.clie_codigo[1] 
		ids_palletencabupdate.Object.plde_codigo[ll_nuevo2] =   dw_3.Object.plde_codigo[1]
		ids_palletencabupdate.Object.paen_inspec[ll_nuevo2] =   dw_3.Object.paen_inspec[1]
		ids_palletencabupdate.Object.emba_codigo[ll_nuevo2] =   dw_3.Object.emba_codigo[1]
		ids_palletencabupdate.Object.vari_codigo[ll_nuevo2] =   dw_3.Object.vari_codigo[1]
		ids_palletencabupdate.Object.paen_varrot[ll_nuevo2] =   dw_3.Object.paen_varrot[1]
	NEXT	
	
	ids_palletencabupdate.ResetUpdate()
	
	FOR ll_fil = 1 TO ids_palletencabupdate.RowCount()
		IF il_cantidad <> 0 OR ids_palletencabupdate.RowCount() > 0  THEN
			ids_palletencabupdate.SetItemStatus(ll_fil,"paen_ccajas", Primary!, DataModified!) 
			ids_palletencabupdate.SetItemStatus(ll_fil,"clie_codigo", Primary!, DataModified!)
			ids_palletencabupdate.SetItemStatus(ll_fil,"plde_codigo", Primary!, DataModified!)
			ids_palletencabupdate.SetItemStatus(ll_fil,"paen_numero", Primary!, DataModified!)
			ids_palletencabupdate.SetItemStatus(ll_fil,"paen_inspec", Primary!, DataModified!)
			ids_palletencabupdate.SetItemStatus(ll_fil,"emba_codigo", Primary!, DataModified!)
			ids_palletencabupdate.SetItemStatus(ll_fil,"vari_codigo", Primary!, DataModified!)
			ids_palletencabupdate.SetItemStatus(ll_fil,"paen_varrot", Primary!, DataModified!)
		END IF
	NEXT	
ELSE	
		
	FOR ll_fila1 = 1 TO dw_3.RowCount()
		ll_nuevo1 = ids_palletencabnuevo.InsertRow(0)
		
		ids_palletencabnuevo.Object.clie_codigo[ll_nuevo1] =   dw_3.Object.clie_codigo[ll_fila1]    
		ids_palletencabnuevo.Object.paen_numero[ll_nuevo1] =   dw_3.Object.paen_numero[ll_fila1]    
		ids_palletencabnuevo.Object.plde_codigo[ll_nuevo1] =   dw_5.Object.plde_codigo[1]    
		ids_palletencabnuevo.Object.paen_tipopa[ll_nuevo1] =   dw_3.Object.paen_tipopa[ll_fila1]    
		ids_palletencabnuevo.Object.tpem_codigo[ll_nuevo1] =   dw_3.Object.tpem_codigo[ll_fila1]    
		ids_palletencabnuevo.Object.espe_codigo[ll_nuevo1] =   dw_3.Object.espe_codigo[ll_fila1]    
		ids_palletencabnuevo.Object.vari_codigo[ll_nuevo1] =   dw_3.Object.vari_codigo[ll_fila1]    
		ids_palletencabnuevo.Object.tiem_codigo[ll_nuevo1] =   dw_3.Object.tiem_codigo[ll_fila1]    
		ids_palletencabnuevo.Object.cate_codigo[ll_nuevo1] =   dw_3.Object.cate_codigo[ll_fila1]    
		ids_palletencabnuevo.Object.etiq_codigo[ll_nuevo1] =   dw_3.Object.etiq_codigo[ll_fila1]    
		ids_palletencabnuevo.Object.stat_codigo[ll_nuevo1] =   dw_3.Object.stat_codigo[ll_fila1]    
		ids_palletencabnuevo.Object.trat_codigo[ll_nuevo1] =   dw_3.Object.trat_codigo[ll_fila1]    
		ids_palletencabnuevo.Object.frio_codigo[ll_nuevo1] =   dw_3.Object.frio_codigo[ll_fila1]    
		ids_palletencabnuevo.Object.cond_codigo[ll_nuevo1] =   dw_3.Object.cond_codigo[ll_fila1]    
		ids_palletencabnuevo.Object.dest_codigo[ll_nuevo1] =   dw_3.Object.dest_codigo[ll_fila1]    
		ids_palletencabnuevo.Object.emba_codigo[ll_nuevo1] =   dw_3.Object.emba_codigo[ll_fila1]    
		ids_palletencabnuevo.Object.paen_fecemb[ll_nuevo1] =   dw_3.Object.paen_fecemb[ll_fila1]    
		ids_palletencabnuevo.Object.paen_cosecha[ll_nuevo1]=   dw_3.Object.paen_cosecha[ll_fila1]    
		ids_palletencabnuevo.Object.paen_altura[ll_nuevo1] =   dw_3.Object.paen_altura[ll_fila1]    
		ids_palletencabnuevo.Object.paen_ccajas[ll_nuevo1] =   dw_3.Object.paen_ccajas[ll_fila1]    
		ids_palletencabnuevo.Object.tmvp_codigo[ll_nuevo1] =   dw_3.Object.tmvp_codigo[ll_fila1]    
		ids_palletencabnuevo.Object.paen_fecini[ll_nuevo1] =   dw_3.Object.paen_fecini[ll_fila1]    
		ids_palletencabnuevo.Object.paen_horain[ll_nuevo1] =   dw_3.Object.paen_horain[ll_fila1]    
		ids_palletencabnuevo.Object.cama_codigo[ll_nuevo1] =   dw_3.Object.cama_codigo[ll_fila1]    
		ids_palletencabnuevo.Object.paen_calle[ll_nuevo1]  =   dw_3.Object.paen_calle[ll_fila1]    
		ids_palletencabnuevo.Object.paen_base[ll_nuevo1]   =   dw_3.Object.paen_base[ll_fila1]    
		ids_palletencabnuevo.Object.paen_posici[ll_nuevo1] =   dw_3.Object.paen_posici[ll_fila1] 
		ids_palletencabnuevo.Object.paen_estado[ll_nuevo1] =   1 
		ids_palletencabnuevo.Object.paen_inspec[ll_nuevo1] =   dw_3.Object.paen_inspec[ll_fila1] 
		ids_palletencabnuevo.Object.paen_concal[ll_nuevo1] =   dw_3.Object.paen_concal[ll_fila1] 
		ids_palletencabnuevo.Object.paen_pexpor[ll_nuevo1] =   dw_3.Object.paen_pexpor[ll_fila1] 
		ids_palletencabnuevo.Object.paen_pmixto[ll_nuevo1] =   dw_3.Object.paen_pmixto[ll_fila1]  
		ids_palletencabnuevo.Object.paen_varrot[ll_nuevo1] =   dw_3.Object.paen_varrot[ll_fila1] 
		ids_palletencabnuevo.Object.paen_nrasda[ll_nuevo1] =   dw_3.Object.paen_nrasda[ll_fila1] 
		ids_palletencabnuevo.Object.prod_codigo[ll_nuevo1] =   dw_3.Object.prod_codigo[ll_fila1] 
		ids_palletencabnuevo.Object.paen_calibr[ll_nuevo1] =   dw_3.Object.paen_calibr[ll_fila1] 
		ids_palletencabnuevo.Object.copa_codigo[ll_nuevo1] =   dw_3.Object.copa_codigo[ll_fila1] 
		ids_palletencabnuevo.Object.paen_huert1[ll_nuevo1] =   dw_3.Object.paen_huert1[ll_fila1] 
		ids_palletencabnuevo.Object.paen_huert2[ll_nuevo1] =   dw_3.Object.paen_huert2[ll_fila1] 
		ids_palletencabnuevo.Object.paen_huert3[ll_nuevo1] =   dw_3.Object.paen_huert3[ll_fila1] 
		ids_palletencabnuevo.Object.paen_huert4[ll_nuevo1] =   dw_3.Object.paen_huert4[ll_fila1] 
		ids_palletencabnuevo.Object.paen_cuart1[ll_nuevo1] =   dw_3.Object.paen_cuart1[ll_fila1] 
		ids_palletencabnuevo.Object.paen_cuart2[ll_nuevo1] =   dw_3.Object.paen_cuart2[ll_fila1] 
		ids_palletencabnuevo.Object.paen_cuart3[ll_nuevo1] =   dw_3.Object.paen_cuart3[ll_fila1] 
		ids_palletencabnuevo.Object.paen_cuart4[ll_nuevo1] =   dw_3.Object.paen_cuart4[ll_fila1] 
		ids_palletencabnuevo.Object.paen_barra1[ll_nuevo1] =   dw_3.Object.paen_barra1[ll_fila1] 
		ids_palletencabnuevo.Object.paen_barra2[ll_nuevo1] =   dw_3.Object.paen_barra2[ll_fila1] 
		ids_palletencabnuevo.Object.paen_barra3[ll_nuevo1] =   dw_3.Object.paen_barra3[ll_fila1] 
		ids_palletencabnuevo.Object.paen_barra4[ll_nuevo1] =   dw_3.Object.paen_barra4[ll_fila1] 
		ids_palletencabnuevo.Object.paen_pcopda[ll_nuevo1] =   dw_3.Object.paen_pcopda[ll_fila1] 
		ids_palletencabnuevo.Object.paen_ccajas[ll_nuevo1] =   dw_3.Object.paen_ccajas[ll_fila1] 
		ids_palletencabnuevo.Object.tmvp_codigo[ll_nuevo1]	=   3 
		
	NEXT
END IF
end subroutine

public function boolean cuadrar_cajas (integer al_tipo);Integer li_cantidad, li_exportador, li_planta
long  fila, fila_find, fila2,  ll_suma, ll_suma1, ll_pallet, ll_fila_caj
Boolean lb_retorno

li_exportador	=	Integer(istr_Mant.Argumento[1])
li_planta		=	Integer(istr_Mant.Argumento[2])

IF al_tipo = 1 THEN 

	FOR fila = 1 TO dw_6.RowCount() 
		ll_pallet = dw_6.Object.paen_numero[fila]
	
		SELECT Sum(pafr_ccajas) 
			INTO :li_cantidad
			FROM dbo.palletfruta
			WHERE clie_codigo = :li_exportador
			AND plde_codigo = :li_planta
			AND paen_numero = :ll_pallet;
		 
		IF isnull(li_cantidad) THEN li_cantidad = 0
		 
		IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca, "Lectura de Tabla palletfruta")
		END IF
		
		fila_find = 0
		fila_find =  ids_cuadracajas.Find("paen_numero = " + String(dw_6.Object.paen_numero[fila]),&
				1, ids_cuadracajas.RowCount())
					
		IF fila_find = 0 THEN
			fila2 = ids_cuadracajas.InsertRow(0)
			ids_cuadracajas.Object.paen_numero[fila2] = dw_6.Object.paen_numero[fila] 
			ids_cuadracajas.Object.cajas[fila2] = dw_6.Object.cajas[fila] - dw_6.Object.caja_traspa[fila]
			ids_cuadracajas.Object.pafr_ccajas[fila2] = dw_6.Object.cajas[fila] - dw_6.Object.caja_traspa[fila]
		ELSE
			IF fila2 = 0 THEN
				ll_suma =  ids_cuadracajas.Object.pafr_ccajas[fila_find] - dw_6.Object.caja_traspa[fila] 
				ll_suma1 =  ids_cuadracajas.Object.cajas[fila_find] - dw_6.Object.caja_traspa[fila] 
				ids_cuadracajas.Object.cajas[fila_find] = ll_suma 
				ids_cuadracajas.Object.pafr_ccajas[fila_find] = ll_suma1 
			ELSE	
				ll_suma  =  dw_6.Object.cajas[fila] - dw_6.Object.caja_traspa[fila] 
				ll_suma1 =  dw_6.Object.cajas[fila] - dw_6.Object.caja_traspa[fila] 
				ids_cuadracajas.Object.cajas[fila2] = ll_suma + ids_cuadracajas.Object.pafr_ccajas[fila2]
				ids_cuadracajas.Object.pafr_ccajas[fila2] = ll_suma1 +ids_cuadracajas.Object.pafr_ccajas[fila2]
			END IF	
		END IF			
	NEXT	
ELSE
	FOR ll_fila_caj = 1 TO ids_cuadracajas.RowCount()
		IF ids_cuadracajas.Object.pafr_ccajas[ll_fila_caj] = 0 THEN
			lb_retorno = True
		ELSE
			il_pallet = ids_cuadracajas.Object.paen_numero[ll_fila_caj] 
			Return False
		END IF
	NEXT	
END IF

Return lb_retorno

end function

public subroutine traspasa_cajas_altura (integer tipo);Long	ll_Fila, ll_BFilaRep, ll_BFilaPD, ll_Nuevo, ll_NuevoDet, ll_fila1, ll_nuevo1, ll_find, ll_fila10,&
	ll_pallet, ll_fil, ll_caja_traspa, ll_nuevo1act, ll_prodrot, ll_predrot, ll_cuarrot, ll_rotpak, ll_busca
Integer li_Secuen, fila_find, li_cliente, li_planta, li_inspeccion, li_inspecnuevo, li_tipopa, &
			li_tiporepa, li_secuencia, ll_filactualiza

ids_palletfrutanuevo.SetTransObject(sqlca)
ids_palletencabnuevo.SetTransObject(sqlca)

li_cliente		= dw_2.Object.clie_codigo[1]
li_planta 		= dw_2.Object.plde_codigo[1]
ll_pallet 		= dw_3.Object.paen_numero[1]
li_tipopa 		= dw_3.Object.paen_tipopa[1]
li_tiporepa	= dw_2.Object.repe_tipopa[1]

//IF dw_4.RowCount() > 0 THEN
//	ll_prodrot  = dw_4.Object.pafr_prdrot[1]
//	ll_predrot  = dw_4.Object.pafr_huert4[1]
//	ll_cuarrot  = dw_4.Object.pafr_cuart4[1]
//	ll_rotpak	= dw_4.Object.pafr_rotpak[1]
//ELSE
	ll_prodrot 	= 999999
	ll_predrot  = 99999
	ll_cuarrot  = 99999
	ll_rotpak	= 99999
//END IF

SELECT count(*) 
	INTO :il_cantidad
	FROM dbo.palletencab
	WHERE clie_codigo = :li_cliente
	AND plde_codigo = :li_planta
	AND paen_numero = :ll_pallet;
 
IF li_tiporepa  = 2 OR li_tiporepa  = 1  THEN
	IF ids_palletfrutanuevo.RowCount() > 0  THEN
		FOR ll_busca = 1 TO ids_palletfrutanuevo.RowCount()
			IF ids_palletfrutanuevo.Object.pafr_secuen[ll_busca] < 99999 THEN
				li_secuen = ids_palletfrutanuevo.Object.pafr_secuen[ll_busca] + 1
			END IF	
		NEXT	
	END IF
END IF	

IF li_tiporepa  = 7 THEN
	FOR ll_fila = 1 TO dw_4.Rowcount() 
		
		ll_BFilaPD	=	ids_palletfrutanuevo.Find("paen_numero = " + String(dw_4.Object.paen_numero[1]) + &
					 " AND pafr_secuen = " + String(dw_4.Object.pafr_secuen[ll_fila]), &
					 1, ids_palletfrutanuevo.RowCount())
		
		IF ll_BFilaPD = 0 THEN
			ll_Nuevo = ids_palletfrutanuevo.InsertRow(0)
			
			ids_palletfrutanuevo.Object.clie_codigo[ll_Nuevo]		=	dw_4.Object.clie_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.plde_codigo[ll_Nuevo]		=	dw_4.Object.plde_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.paen_numero[ll_Nuevo]	=	dw_4.Object.paen_numero[1]
			ids_palletfrutanuevo.Object.pafr_secuen[ll_Nuevo]	=	dw_4.Object.pafr_secuen[ll_Fila]
			ids_palletfrutanuevo.Object.espe_codigo[ll_Nuevo]	=	dw_4.Object.espe_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.vari_codigo[ll_Nuevo]		=	dw_4.Object.vari_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.vari_nombre[ll_Nuevo]	=	dw_4.Object.vari_nombre[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_varrot[ll_Nuevo]		=	dw_4.Object.pafr_varrot[ll_Fila]
			ids_palletfrutanuevo.Object.prod_codigo[ll_Nuevo]	=	dw_4.Object.prod_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_prdrot[ll_Nuevo]		=	dw_4.Object.pafr_prdrot[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_huert4[ll_Nuevo] 	=	dw_4.Object.pafr_huert4[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_cuart4[ll_Nuevo] 	=	dw_4.Object.pafr_cuart4[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_rotpak[ll_Nuevo] 	=	dw_4.Object.pafr_rotpak[ll_Fila]
			ids_palletfrutanuevo.Object.emba_codigo[ll_Nuevo]	=	dw_4.Object.emba_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.etiq_codigo[ll_Nuevo]		=	dw_4.Object.etiq_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_huert1[ll_Nuevo]		=	dw_4.Object.pafr_huert1[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_cuart1[ll_Nuevo]		=	dw_4.Object.pafr_cuart1[ll_Fila]
		//	ids_palletfrutanuevo.Object.pafr_huert2[ll_Nuevo]	=	dw_4.Object.pafr_huert2[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_cuart2[ll_Nuevo]		=	dw_4.Object.pafr_cuart2[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_nrlote[ll_Nuevo]		=	dw_4.Object.pafr_nrlote[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_calibr[ll_Nuevo]  		=	dw_4.Object.pafr_calibr[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_fecemb[ll_Nuevo]	=	dw_4.Object.pafr_fecemb[ll_Fila]
			ids_palletfrutanuevo.Object.prod_nombre[ll_Nuevo]	=	dw_4.Object.prod_nombre[ll_Fila]
			ids_palletfrutanuevo.Object.cond_codigo[ll_Nuevo]	=	dw_4.Object.cond_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_cjssal[ll_Nuevo]		=	dw_4.Object.pafr_ccajas[ll_fila] 
			ids_palletfrutanuevo.Object.pafr_fecing[ll_Nuevo]		=	dw_4.Object.pafr_fecing[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_copack[ll_nuevo]  	=  dw_4.Object.pafr_copack[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_ccajas[ll_nuevo]		=	dw_4.Object.pafr_ccajas[ll_Fila] 
			ids_palletfrutanuevo.Object.pafr_docrel[ll_nuevo]		=	dw_4.Object.pafr_docrel[ll_Fila] 
			ids_palletfrutanuevo.Object.pafr_calrot[ll_nuevo]		=	dw_4.Object.pafr_calrot[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_fecrot[ll_nuevo]		=	dw_4.Object.pafr_fecrot[ll_Fila] 
			ids_palletfrutanuevo.Object.pafr_embrea[ll_nuevo]	=	dw_4.Object.pafr_embrea[ll_Fila]
			ids_palletfrutanuevo.Object.cama_nombre[ll_nuevo]	=	dw_4.Object.cama_nombre[ll_Fila]
			ids_palletfrutanuevo.Object.cate_codigo[ll_nuevo]		=	dw_4.Object.cate_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_catrot[ll_nuevo]		=	dw_4.Object.pafr_catrot[ll_Fila]
						
		//	dw_6.Object.caja_traspa[ll_fila] = 0
			
			IF isnull(dw_4.Object.pafr_nroori[ll_Fila]) OR dw_4.Object.pafr_nroori[ll_Fila] = 0 THEN
				ids_palletfrutanuevo.Object.pafr_nroori[ll_nuevo] = dw_4.Object.paen_numero[ll_Fila]
			ELSE	
				ids_palletfrutanuevo.Object.pafr_nroori[ll_nuevo] = dw_4.Object.pafr_nroori[ll_Fila]
			END IF
			
			IF isnull(dw_4.Object.pafr_secori[ll_Fila]) OR dw_4.Object.pafr_secori[ll_Fila] = 0 THEN
				ids_palletfrutanuevo.Object.pafr_secori[ll_nuevo] = dw_4.Object.pafr_secuen[ll_Fila]
			ELSE	
				ids_palletfrutanuevo.Object.pafr_secori[ll_nuevo] = dw_4.Object.pafr_secori[ll_Fila]
			END IF
		END IF
	NEXT
	
	FOR ll_fila = 1 TO dw_6.Rowcount() 
		dw_6.Object.caja_traspa[ll_fila] = 0
	NEXT	

END IF	

IF li_tiporepa  <> 7 THEN

	FOR ll_fila = 1 TO dw_6.Rowcount() 
		ll_caja_traspa = dw_6.Object.caja_traspa[ll_fila]
		IF ll_caja_traspa > 0 THEN
				
	//		IF li_secuen = 0 THEN
	//			li_secuen = 1
	//			IF ids_palletfrutanuevo.RowCount() > 0 THEN
	//				FOR li_secuencia = 1 TO ids_palletfrutanuevo.RowCount()
	//					IF ids_palletfrutanuevo.Object.pafr_secuen[li_secuencia] < 9999 THEN
	//						li_secuen = ids_palletfrutanuevo.Object.pafr_secuen[ids_palletfrutanuevo.RowCount()] + 1
	//					END IF	
	//				NEXT	
	//			END IF
	//		END IF	
			
			ll_Nuevo = ids_palletfrutanuevo.InsertRow(0)
			
			ids_palletfrutanuevo.Object.clie_codigo[ll_Nuevo]		=	dw_6.Object.clie_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.plde_codigo[ll_Nuevo]		=	dw_6.Object.plde_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.paen_numero[ll_Nuevo]	=	dw_3.Object.paen_numero[1]
			
			IF li_secuen = 0 THEN
				IF dw_4.RowCount() > 0 THEN
					li_secuen = dw_4.Object.Pafr_secuen[dw_4.RowCount()]	
				ELSE	
					li_secuen = 1
				END IF	
			END IF	
			
			IF dw_6.Object.pafr_secuen[ll_Fila] > 99999 THEN
				ids_palletfrutanuevo.Object.pafr_secuen[ll_Nuevo]	=	dw_6.Object.pafr_secuen[ll_Fila]
			ELSE
				ids_palletfrutanuevo.Object.pafr_secuen[ll_Nuevo]	=	li_secuen
				li_secuen  ++
			END IF	
			
			ids_palletfrutanuevo.Object.espe_codigo[ll_Nuevo]	=	dw_6.Object.espe_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.vari_codigo[ll_Nuevo]		=	dw_6.Object.vari_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.vari_nombre[ll_Nuevo]	=	dw_6.Object.vari_nombre[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_varrot[ll_Nuevo]		=	dw_6.Object.pafr_varrot[ll_Fila]
			ids_palletfrutanuevo.Object.prod_codigo[ll_Nuevo]	=	dw_6.Object.prod_codigo[ll_Fila]
			
			//IF ll_prodrot = 999999 THEN
				ids_palletfrutanuevo.Object.pafr_prdrot[ll_Nuevo]	=	dw_6.Object.pafr_prdrot[ll_Fila]
				ids_palletfrutanuevo.Object.pafr_huert4[ll_Nuevo] 	=	dw_6.Object.pafr_huert4[ll_Fila]
				ids_palletfrutanuevo.Object.pafr_cuart4[ll_Nuevo] 	=	dw_6.Object.pafr_cuart4[ll_Fila]
				ids_palletfrutanuevo.Object.pafr_rotpak[ll_Nuevo] 	=	dw_6.Object.pafr_rotpak[ll_Fila]
	//		ELSE	
	//			IF li_cliente = gi_CodExport THENyyyy
	//				ids_palletfrutanuevo.Object.pafr_prdrot[ll_Nuevo]	=	ll_prodrot
	//				ids_palletfrutanuevo.Object.pafr_huert4[ll_Nuevo] 	=	ll_predrot
	//				ids_palletfrutanuevo.Object.pafr_cuart4[ll_Nuevo] 	=	ll_cuarrot
	//				ids_palletfrutanuevo.Object.pafr_rotpak[ll_Nuevo] 	=	ll_rotpak
	//			ELSE
	//				ids_palletfrutanuevo.Object.pafr_prdrot[ll_Nuevo]	=	dw_6.Object.pafr_prdrot[ll_Fila]
	//				ids_palletfrutanuevo.Object.pafr_huert4[ll_Nuevo] 	=	dw_6.Object.pafr_huert4[ll_Fila]
	//				ids_palletfrutanuevo.Object.pafr_cuart4[ll_Nuevo] 	=	dw_6.Object.pafr_cuart4[ll_Fila]
	//				ids_palletfrutanuevo.Object.pafr_rotpak[ll_Nuevo] 	=	dw_6.Object.pafr_rotpak[ll_Fila]
	//			END IF	
	//		END IF	
				
			ids_palletfrutanuevo.Object.emba_codigo[ll_Nuevo]	=	dw_6.Object.emba_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.etiq_codigo[ll_Nuevo]	=	dw_6.Object.etiq_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_huert1[ll_Nuevo]	=	dw_6.Object.pafr_huert1[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_cuart1[ll_Nuevo]	=	dw_6.Object.pafr_cuart1[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_huert2[ll_Nuevo]	=	dw_6.Object.pafr_huert2[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_cuart2[ll_Nuevo]	=	dw_6.Object.pafr_cuart2[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_nrlote[ll_Nuevo]	=	dw_6.Object.pafr_nrlote[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_calibr[ll_Nuevo]  =	dw_6.Object.pafr_calibr[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_fecemb[ll_Nuevo]	=	dw_6.Object.pafr_fecemb[ll_Fila]
			ids_palletfrutanuevo.Object.prod_nombre[ll_Nuevo]	=	dw_6.Object.prod_nombre[ll_Fila]
			ids_palletfrutanuevo.Object.cond_codigo[ll_Nuevo]	=	dw_6.Object.cond_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_cjssal[ll_Nuevo]	=	dw_6.Object.caja_traspa[ll_fila] 
			ids_palletfrutanuevo.Object.pafr_fecing[ll_Nuevo]	=	dw_6.Object.pafr_fecing[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_copack[ll_nuevo]  =  dw_6.Object.pafr_copack[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_ccajas[ll_nuevo]	=	dw_6.Object.caja_traspa[ll_Fila] 
			ids_palletfrutanuevo.Object.pafr_docrel[ll_nuevo]	=	dw_6.Object.pafr_docrel[ll_Fila] 
			ids_palletfrutanuevo.Object.pafr_calrot[ll_nuevo]	=	dw_6.Object.pafr_calrot[ll_Fila]
			
			ids_palletfrutanuevo.Object.pafr_fecrot[ll_nuevo]	=	dw_6.Object.pafr_fecrot[ll_Fila] 
			ids_palletfrutanuevo.Object.pafr_embrea[ll_nuevo]	=	dw_6.Object.pafr_embrea[ll_Fila]
			
			ids_palletfrutanuevo.Object.pafr_huert2[ll_Nuevo]	=	dw_6.Object.pafr_huert2[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_huert3[ll_Nuevo]	=	dw_6.Object.pafr_huert3[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_cuart2[ll_Nuevo]	=	dw_6.Object.pafr_cuart2[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_cuart3[ll_Nuevo]	=	dw_6.Object.pafr_cuart3[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_barra1[ll_Nuevo]	=	dw_6.Object.pafr_barra1[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_barra2[ll_Nuevo]	=	dw_6.Object.pafr_barra2[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_barra3[ll_Nuevo]	=	dw_6.Object.pafr_barra3[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_barra4[ll_Nuevo]	=	dw_6.Object.pafr_barra4[ll_Fila]
			ids_palletfrutanuevo.Object.cama_nombre[ll_Nuevo]	=	dw_6.Object.cama_nombre[ll_Fila]
			ids_palletfrutanuevo.Object.cate_codigo[ll_Nuevo]	=	dw_6.Object.cate_codigo[ll_Fila]
			ids_palletfrutanuevo.Object.pafr_catrot[ll_Nuevo]	=	dw_6.Object.pafr_catrot[ll_Fila]
			
			IF li_tiporepa  = 2 AND li_tiporepa  = 1 THEN
				li_secuen = li_secuen + 1
			END IF	
			
			dw_6.Object.caja_traspa[ll_fila] = 0
			
			IF isnull(dw_6.Object.pafr_nroori[ll_Fila]) OR dw_6.Object.pafr_nroori[ll_Fila] = 0 THEN
				ids_palletfrutanuevo.Object.pafr_nroori[ll_nuevo] = dw_6.Object.paen_numero[ll_Fila]
			ELSE	
				ids_palletfrutanuevo.Object.pafr_nroori[ll_nuevo] = dw_6.Object.pafr_nroori[ll_Fila]
			END IF
			
			IF isnull(dw_6.Object.pafr_secori[ll_Fila]) OR dw_6.Object.pafr_secori[ll_Fila] = 0 THEN
				ids_palletfrutanuevo.Object.pafr_secori[ll_nuevo] = dw_6.Object.pafr_secuen[ll_Fila]
			ELSE	
				ids_palletfrutanuevo.Object.pafr_secori[ll_nuevo] = dw_6.Object.pafr_secori[ll_Fila]
			END IF
		END IF
	NEXT
END IF		

FOR ll_fila1 = 1 TO dw_3.RowCount()

	fila_find =  ids_palletencabnuevo.Find("paen_numero = " + String(dw_3.Object.paen_numero[ll_fila1]),&
							1, ids_palletencabnuevo.RowCount())
	IF fila_find = 0 THEN
		IF il_cantidad = 0 THEN
			ll_nuevo1 = ids_palletencabnuevo.InsertRow(0)
			
			ids_palletencabnuevo.Object.clie_codigo[ll_nuevo1] 		=   dw_5.Object.clie_codigo[ll_fila1]    
			ids_palletencabnuevo.Object.paen_numero[ll_nuevo1]	=   dw_3.Object.paen_numero[ll_fila1]    
			ids_palletencabnuevo.Object.plde_codigo[ll_nuevo1] 		=   dw_5.Object.plde_codigo[1]    
			
			IF li_tiporepa = 1 OR li_tiporepa = 2 OR li_tiporepa = 7 THEN
				ids_palletencabnuevo.Object.paen_tipopa[ll_nuevo1] =   dw_3.Object.paen_tipopa[ll_fila1]    
				ids_palletencabnuevo.Object.tpem_codigo[ll_nuevo1] =   dw_3.Object.tpem_codigo[ll_fila1]
			ELSE
				ids_palletencabnuevo.Object.paen_tipopa[ll_nuevo1]	=   dw_5.Object.paen_tipopa[ll_fila1]    
				ids_palletencabnuevo.Object.tpem_codigo[ll_nuevo1]	=   dw_5.Object.tpem_codigo[ll_fila1] 
			END IF
			
			ids_palletencabnuevo.Object.espe_codigo[ll_nuevo1]	=   dw_5.Object.espe_codigo[ll_fila1]    
			ids_palletencabnuevo.Object.vari_codigo[ll_nuevo1] 	=   dw_5.Object.vari_codigo[ll_fila1]    
			ids_palletencabnuevo.Object.tiem_codigo[ll_nuevo1] 	=   dw_5.Object.tiem_codigo[ll_fila1]    
			ids_palletencabnuevo.Object.cate_codigo[ll_nuevo1] 	=   dw_5.Object.cate_codigo[ll_fila1]    
			ids_palletencabnuevo.Object.etiq_codigo[ll_nuevo1] 	=   dw_5.Object.etiq_codigo[ll_fila1]    
			ids_palletencabnuevo.Object.stat_codigo[ll_nuevo1] 	=   dw_5.Object.stat_codigo[ll_fila1]    
			ids_palletencabnuevo.Object.trat_codigo[ll_nuevo1] 	=   dw_5.Object.trat_codigo[ll_fila1]    
			ids_palletencabnuevo.Object.frio_codigo[ll_nuevo1] 	=   dw_5.Object.frio_codigo[ll_fila1]    
			ids_palletencabnuevo.Object.cond_codigo[ll_nuevo1] =   dw_5.Object.cond_codigo[ll_fila1]    
			ids_palletencabnuevo.Object.dest_codigo[ll_nuevo1] 	=   dw_5.Object.dest_codigo[ll_fila1]    
			ids_palletencabnuevo.Object.emba_codigo[ll_nuevo1] =   dw_5.Object.emba_codigo[ll_fila1]    
			ids_palletencabnuevo.Object.paen_fecemb[ll_nuevo1] =   dw_5.Object.paen_fecemb[ll_fila1]    
			ids_palletencabnuevo.Object.paen_cosecha[ll_nuevo1]=   dw_5.Object.paen_cosecha[ll_fila1]    
			ids_palletencabnuevo.Object.paen_altura[ll_nuevo1] =   dw_5.Object.paen_altura[ll_fila1]
			ids_palletencabnuevo.Object.paen_ccajas[ll_nuevo1] =   dw_4.Object.total_cajas[1]//dw_6.Object.total_traspa[1]
			ids_palletencabnuevo.Object.tmvp_codigo[ll_nuevo1] =   dw_5.Object.tmvp_codigo[ll_fila1] 
			ids_palletencabnuevo.Object.paen_fecini[ll_nuevo1] =   dw_5.Object.paen_fecini[ll_fila1]    
			ids_palletencabnuevo.Object.paen_horain[ll_nuevo1] =   dw_5.Object.paen_horain[ll_fila1]    
			ids_palletencabnuevo.Object.cama_codigo[ll_nuevo1] =   dw_5.Object.cama_codigo[ll_fila1]    
			ids_palletencabnuevo.Object.paen_calle[ll_nuevo1]  =   dw_5.Object.paen_calle[ll_fila1]    
			ids_palletencabnuevo.Object.paen_base[ll_nuevo1]   =   dw_5.Object.paen_base[ll_fila1]    
			ids_palletencabnuevo.Object.paen_posici[ll_nuevo1] =   dw_5.Object.paen_posici[ll_fila1] 
			ids_palletencabnuevo.Object.paen_estado[ll_nuevo1] =  1 
			ids_palletencabnuevo.Object.paen_inspec[ll_nuevo1] =   dw_3.Object.paen_inspec[ll_fila1] 
			ids_palletencabnuevo.Object.paen_concal[ll_nuevo1] =   dw_5.Object.paen_concal[ll_fila1] 
			ids_palletencabnuevo.Object.paen_pexpor[ll_nuevo1] =   dw_5.Object.paen_pexpor[ll_fila1] 
			ids_palletencabnuevo.Object.paen_pmixto[ll_nuevo1] =   dw_5.Object.paen_pmixto[ll_fila1]  
			ids_palletencabnuevo.Object.paen_varrot[ll_nuevo1] =   dw_5.Object.paen_varrot[ll_fila1] 
			ids_palletencabnuevo.Object.paen_nrasda[ll_nuevo1] =   dw_5.Object.paen_nrasda[ll_fila1] 
			ids_palletencabnuevo.Object.prod_codigo[ll_nuevo1] =   dw_5.Object.prod_codigo[ll_fila1] 
			ids_palletencabnuevo.Object.paen_calibr[ll_nuevo1] =   dw_5.Object.paen_calibr[ll_fila1] 
			ids_palletencabnuevo.Object.copa_codigo[ll_nuevo1] =   dw_5.Object.copa_codigo[ll_fila1] 
			ids_palletencabnuevo.Object.paen_huert1[ll_nuevo1] =   dw_5.Object.paen_huert1[ll_fila1] 
			ids_palletencabnuevo.Object.paen_huert2[ll_nuevo1] =   dw_5.Object.paen_huert2[ll_fila1] 
			ids_palletencabnuevo.Object.paen_huert3[ll_nuevo1] =   dw_5.Object.paen_huert3[ll_fila1] 
			ids_palletencabnuevo.Object.paen_huert4[ll_nuevo1] =   dw_5.Object.paen_huert4[ll_fila1] 
			ids_palletencabnuevo.Object.paen_cuart1[ll_nuevo1] =   dw_5.Object.paen_cuart1[ll_fila1] 
			ids_palletencabnuevo.Object.paen_cuart2[ll_nuevo1] =   dw_5.Object.paen_cuart2[ll_fila1] 
			ids_palletencabnuevo.Object.paen_cuart3[ll_nuevo1] =   dw_5.Object.paen_cuart3[ll_fila1] 
			ids_palletencabnuevo.Object.paen_cuart4[ll_nuevo1] =   dw_5.Object.paen_cuart4[ll_fila1] 
			ids_palletencabnuevo.Object.paen_barra1[ll_nuevo1] =   dw_5.Object.paen_barra1[ll_fila1] 
			ids_palletencabnuevo.Object.paen_barra2[ll_nuevo1] =   dw_5.Object.paen_barra2[ll_fila1] 
			ids_palletencabnuevo.Object.paen_barra3[ll_nuevo1] =   dw_5.Object.paen_barra3[ll_fila1] 
			ids_palletencabnuevo.Object.paen_barra4[ll_nuevo1] =   dw_5.Object.paen_barra4[ll_fila1] 
			ids_palletencabnuevo.Object.paen_pcopda[ll_nuevo1] =   dw_5.Object.paen_pcopda[ll_fila1] 
			ids_palletencabnuevo.Object.tmvp_codigo[ll_nuevo1]	=  3 
			
		ELSE
			ll_nuevo1 = ids_palletencabupdate.InsertRow(0)
			ids_palletencabupdate.Object.clie_codigo[ll_nuevo1] =   dw_3.Object.clie_codigo[ll_fila1]    
			ids_palletencabupdate.Object.paen_numero[ll_nuevo1] =   dw_3.Object.paen_numero[ll_fila1]    
			ids_palletencabupdate.Object.plde_codigo[ll_nuevo1] =   dw_3.Object.plde_codigo[ll_fila1]    
			ids_palletencabupdate.Object.paen_tipopa[ll_nuevo1] =   dw_3.Object.paen_tipopa[ll_fila1]    
			ids_palletencabupdate.Object.tpem_codigo[ll_nuevo1] =   dw_3.Object.tpem_codigo[ll_fila1]    
			ids_palletencabupdate.Object.espe_codigo[ll_nuevo1] =   dw_3.Object.espe_codigo[ll_fila1]    
			ids_palletencabupdate.Object.vari_codigo[ll_nuevo1] =   dw_3.Object.vari_codigo[ll_fila1]    
			ids_palletencabupdate.Object.tiem_codigo[ll_nuevo1] =   dw_3.Object.tiem_codigo[ll_fila1]    
			ids_palletencabupdate.Object.cate_codigo[ll_nuevo1] =   dw_3.Object.cate_codigo[ll_fila1]    
			ids_palletencabupdate.Object.etiq_codigo[ll_nuevo1] =   dw_3.Object.etiq_codigo[ll_fila1]    
			ids_palletencabupdate.Object.stat_codigo[ll_nuevo1] =   dw_3.Object.stat_codigo[ll_fila1]    
			ids_palletencabupdate.Object.trat_codigo[ll_nuevo1] =   dw_3.Object.trat_codigo[ll_fila1]    
			ids_palletencabupdate.Object.frio_codigo[ll_nuevo1] =   dw_3.Object.frio_codigo[ll_fila1]    
			ids_palletencabupdate.Object.cond_codigo[ll_nuevo1] =   dw_3.Object.cond_codigo[ll_fila1]    
			ids_palletencabupdate.Object.dest_codigo[ll_nuevo1] =   dw_3.Object.dest_codigo[ll_fila1]    
			ids_palletencabupdate.Object.emba_codigo[ll_nuevo1] =   dw_3.Object.emba_codigo[ll_fila1]    
			ids_palletencabupdate.Object.paen_fecemb[ll_nuevo1] =   dw_3.Object.paen_fecemb[ll_fila1]    
			ids_palletencabupdate.Object.paen_cosecha[ll_nuevo1]=   dw_3.Object.paen_cosecha[ll_fila1]    
			ids_palletencabupdate.Object.paen_altura[ll_nuevo1] =   dw_3.Object.paen_altura[ll_fila1]
			ids_palletencabupdate.Object.paen_ccajas[ll_nuevo1] =   dw_4.Object.total_cajas[1]//dw_3.Object.paen_ccajas[ll_fila1] + dw_6.Object.total_traspa[1]
			ids_palletencabupdate.Object.tmvp_codigo[ll_nuevo1] =   dw_3.Object.tmvp_codigo[ll_fila1] 
			ids_palletencabupdate.Object.paen_fecini[ll_nuevo1] =   dw_3.Object.paen_fecini[ll_fila1]    
			ids_palletencabupdate.Object.paen_horain[ll_nuevo1] =   dw_3.Object.paen_horain[ll_fila1]    
			ids_palletencabupdate.Object.cama_codigo[ll_nuevo1] =   dw_3.Object.cama_codigo[ll_fila1]    
			ids_palletencabupdate.Object.paen_calle[ll_nuevo1]  =   dw_3.Object.paen_calle[ll_fila1]    
			ids_palletencabupdate.Object.paen_base[ll_nuevo1]   =   dw_3.Object.paen_base[ll_fila1]    
			ids_palletencabupdate.Object.paen_posici[ll_nuevo1] =   dw_3.Object.paen_posici[ll_fila1] 
			ids_palletencabupdate.Object.paen_estado[ll_nuevo1] =  1 
			ids_palletencabupdate.Object.paen_inspec[ll_nuevo1] =   dw_3.Object.paen_inspec[ll_fila1] 
			ids_palletencabupdate.Object.paen_concal[ll_nuevo1] =   dw_3.Object.paen_concal[ll_fila1] 
			ids_palletencabupdate.Object.paen_pexpor[ll_nuevo1] =   dw_3.Object.paen_pexpor[ll_fila1] 
			ids_palletencabupdate.Object.paen_pmixto[ll_nuevo1] =   dw_3.Object.paen_pmixto[ll_fila1]  
			ids_palletencabupdate.Object.paen_varrot[ll_nuevo1] =   dw_3.Object.paen_varrot[ll_fila1] 
			ids_palletencabupdate.Object.paen_nrasda[ll_nuevo1] =   dw_3.Object.paen_nrasda[ll_fila1] 
			ids_palletencabupdate.Object.prod_codigo[ll_nuevo1] =   dw_3.Object.prod_codigo[ll_fila1] 
			ids_palletencabupdate.Object.paen_calibr[ll_nuevo1] =   dw_3.Object.paen_calibr[ll_fila1] 
			ids_palletencabupdate.Object.copa_codigo[ll_nuevo1] =   dw_3.Object.copa_codigo[ll_fila1] 
			ids_palletencabupdate.Object.paen_huert1[ll_nuevo1] =   dw_3.Object.paen_huert1[ll_fila1] 
			ids_palletencabupdate.Object.paen_huert2[ll_nuevo1] =   dw_3.Object.paen_huert2[ll_fila1] 
			ids_palletencabupdate.Object.paen_huert3[ll_nuevo1] =   dw_3.Object.paen_huert3[ll_fila1] 
			ids_palletencabupdate.Object.paen_huert4[ll_nuevo1] =   dw_3.Object.paen_huert4[ll_fila1] 
			ids_palletencabupdate.Object.paen_cuart1[ll_nuevo1] =   dw_3.Object.paen_cuart1[ll_fila1] 
			ids_palletencabupdate.Object.paen_cuart2[ll_nuevo1] =   dw_3.Object.paen_cuart2[ll_fila1] 
			ids_palletencabupdate.Object.paen_cuart3[ll_nuevo1] =   dw_3.Object.paen_cuart3[ll_fila1] 
			ids_palletencabupdate.Object.paen_cuart4[ll_nuevo1] =   dw_3.Object.paen_cuart4[ll_fila1] 
			ids_palletencabupdate.Object.paen_barra1[ll_nuevo1] =   dw_3.Object.paen_barra1[ll_fila1] 
			ids_palletencabupdate.Object.paen_barra2[ll_nuevo1] =   dw_3.Object.paen_barra2[ll_fila1] 
			ids_palletencabupdate.Object.paen_barra3[ll_nuevo1] =   dw_3.Object.paen_barra3[ll_fila1] 
			ids_palletencabupdate.Object.paen_barra4[ll_nuevo1] =   dw_3.Object.paen_barra4[ll_fila1] 
			ids_palletencabupdate.Object.paen_pcopda[ll_nuevo1] =   dw_3.Object.paen_pcopda[ll_fila1] 
			ids_palletencabupdate.Object.tmvp_codigo[ll_nuevo1] =   3 
			ids_palletencabupdate.Object.paen_inspec[ll_nuevo1] =   dw_3.Object.paen_inspec[ll_fila1]
			
		END IF	
	ELSE
		ll_nuevo1act = ids_palletencabupdate.InsertRow(0)
		IF dw_6.RowCount() > 0 THEN
			ids_palletencabupdate.Object.paen_ccajas[ll_nuevo1act] =   dw_3.Object.paen_ccajas[1] + dw_6.Object.total_traspa[1]
		ELSE
			ids_palletencabupdate.Object.paen_ccajas[ll_nuevo1act] =   dw_3.Object.paen_ccajas[1] 
		END IF
		ids_palletencabupdate.Object.paen_numero[ll_nuevo1act] =   dw_3.Object.paen_numero[1] 
		ids_palletencabupdate.Object.clie_codigo[ll_nuevo1act] =   dw_3.Object.clie_codigo[1] 
		ids_palletencabupdate.Object.plde_codigo[ll_nuevo1act] =   dw_3.Object.plde_codigo[1]
		ids_palletencabupdate.Object.tpem_codigo[ll_nuevo1act] =   dw_3.Object.tpem_codigo[1] 
		ids_palletencabupdate.Object.paen_inspec[ll_nuevo1act] =   dw_3.Object.paen_inspec[1]
		ids_palletencabupdate.Object.paen_tipopa[ll_nuevo1act] =   dw_3.Object.paen_tipopa[1]
		ids_palletencabupdate.Object.emba_codigo[ll_nuevo1act] =   dw_3.Object.emba_codigo[1]
		ids_palletencabupdate.Object.vari_codigo[ll_nuevo1act] =   dw_3.Object.vari_codigo[1]
		ids_palletencabupdate.Object.paen_varrot[ll_nuevo1act] =   dw_3.Object.paen_varrot[1]
	END IF		
NEXT

ids_palletencabupdate.ResetUpdate()

FOR ll_fil = 1 TO ids_palletencabupdate.RowCount()
	IF il_cantidad <> 0 OR ids_palletencabupdate.RowCount() > 0  THEN
		ids_palletencabupdate.SetItemStatus(ll_fil,"paen_ccajas", Primary!, DataModified!) 
		ids_palletencabupdate.SetItemStatus(ll_fil,"tpem_codigo", Primary!, DataModified!) 
		ids_palletencabupdate.SetItemStatus(ll_fil,"paen_inspec", Primary!, DataModified!)
		ids_palletencabupdate.SetItemStatus(ll_fil,"paen_tipopa", Primary!, DataModified!)
		ids_palletencabupdate.SetItemStatus(ll_fil,"emba_codigo", Primary!, DataModified!)
		ids_palletencabupdate.SetItemStatus(ll_fil,"vari_codigo", Primary!, DataModified!)
		ids_palletencabupdate.SetItemStatus(ll_fil,"paen_varrot", Primary!, DataModified!)
	END IF
NEXT	
end subroutine

public function integer buscapalletorigen ();string ls_null,ls_Estado[3] = {'Existencia','Despachado','Repalletizado'}
Integer li_repe_tipopa
Long 		ll_repe_nrosag
str_busqueda lstr_busq
SetNull(ls_null)

openWithParm(w_busqueda_pallets_repal,istr_mant)

lstr_busq = message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	istr_Mant.Argumento[1] = lstr_busq.argum[1]
	istr_Mant.Argumento[2] = lstr_busq.argum[2]
	istr_Mant.Argumento[5] = lstr_busq.argum[3]	
	
	IF ExistePallet(Long(istr_Mant.Argumento[5]),False) THEN
		
		ib_detalle = true
		
		li_repe_tipopa = dw_2.Object.repe_tipopa[1]
		
		IF li_repe_tipopa = 6 AND ib_detalle THEN
			
			MessageBox("Atención", "Número de pallet " + String(Long(lstr_busq.argum[3]), '00000000') + &
					", ya se encuentra ingresado. Para Cambio de Folio, el pallet destino debe ser nuevo. " + &
					"~r~rIngrese otro número de pallet.")
			dw_3.SetItem(1, "paen_numero", Long(ls_Null))
			istr_Mant.Argumento[4]	=	""
			RETURN 1
		END IF	
		
		IF il_EstadoPallet > 1 AND ib_detalle THEN
			MessageBox("Atención", "Número de pallet " + String(Long(lstr_busq.argum[3]), '00000000') + &
							", " + ls_Estado[il_EstadoPallet] + &
							"~r~rIngrese otro número de pallet.")
			istr_Mant.Argumento[4]	=	""
			RETURN 1
		END IF	
		
		IF il_inspeccion >= 1 THEN
			ib_inspeccion_destino =  TRUE
			MessageBox("Atención", "Trabajará con un pallet Inspeccionado.")
		END If		
		
		IF il_inspeccion = 5 THEN
			MessageBox("Atención", "El Pallet se Encuentra con Inspección Pendiente.")
			dw_3.SetItem(1, "paen_numero", Long(ls_Null))
			Return 1
		END IF
		
		ll_repe_nrosag = dw_2.Object.repe_nrosag[1]
		
		If ll_repe_nrosag > 0 Then 
			If IsNull(il_inspeccion) Then il_inspeccion = 0
			If il_inspeccion = 0 Then
				MessageBox("Atención", "El pallet debe estar Inspeccionado.")
				dw_3.SetItem(1, "paen_numero", Long(ls_Null))
				Return 1
			End If
		End If
		
		TriggerEvent("ue_recuperapallet_origen")				
		
		dw_3.Object.paen_estado[1]			=	1
		is_marca = 'M'
		
		dw_5.Object.paen_numero.Protect	=	0
		dw_5.Object.paen_numero.BackGround.Color = RGB(255,255,255)
		
		dw_5.Object.paen_tipopa.Protect	=	0
		dw_5.Object.paen_tipopa.BackGround.Color = RGB(255,255,255)
		
		is_palletdest = Mid(lstr_busq.argum[3],len(lstr_busq.argum[3]) - 6, 7)
		
		IF ib_detalle THEN
			ib_ExistePalletDestino	=	False
		ELSE
			ib_ExistePalletDestino	=	True
		END IF	
		
		Asigna_PalletNuevo()
		
		dw_5.SetColumn("paen_numero")
		dw_5.SetFocus()
		
		dw_3.Object.paen_numero.Protect	=	1
		dw_3.Object.paen_numero.Color		= RGB(255,255,255)
		dw_3.Object.paen_numero.BackGround.Color = 553648127
		
		IF dw_5.Rowcount() > 0 THEN
			OpenWithParm(w_mant_deta_repa_agrupado, istr_mant)
		 END IF

	END IF	
END IF	

Return 1
end function

public subroutine buscacamara ();
end subroutine

public subroutine camaraexiste ();integer li_planta, li_count

li_planta = Integer(istr_Mant.Argumento[2])

SELECT count(*)
	INTO :li_count
	FROM dbo.camarasbode  
	WHERE plde_codigo = :li_planta 
	AND  cama_codigo = 0;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla camarasbode")
END IF	

IF li_count = 0 THEN
	INSERT INTO dbo.camarasbode(plde_codigo,cama_codigo,cama_nombre,cama_cancal,cama_canbas,cama_canpos)
	VALUES (:li_planta,0,'Patio',99,99,0);
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla camarasbode")
	END IF
END IF

end subroutine

public function boolean datosinpeccion (long codigo);Integer li_cantidad, li_exportador, li_planta, li_destino
long     ll_numero
date		ld_fechai

li_exportador	=	Integer(istr_Mant.Argumento[1])
li_planta		=	Integer(istr_Mant.Argumento[2])

SELECT Distinct max(inpe_numero),inpe_fechai,dest_codigo
	INTO :ii_nuinpe, :id_fechai, :il_destino
	FROM dbo.inspecpalenc
	WHERE clie_codigo = :li_exportador
	AND plde_codigo   = :li_planta
	AND inpe_numero   = :codigo
	Group by inpe_fechai,dest_codigo;
 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla inspecpalenc")

	RETURN FALSE
ELSE	
 	RETURN TRUE
	
END IF

end function

public function boolean procesoaltura (integer numero);Integer li_cantidad, li_exportador, li_planta, li_destino
long     ll_count
date		ld_fechai

li_exportador	=	Integer(istr_Mant.Argumento[1])
li_planta		=	Integer(istr_Mant.Argumento[2])

SELECT Count(*)
	INTO :ll_count
	FROM dbo.alpalletencab
	WHERE clie_codigo = :li_exportador
	AND plde_codigo   = :li_planta
	AND altu_numero   = :numero;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla alpalletencab")

	RETURN FALSE
ELSEIF ll_count > 0 THEN	
 	RETURN FALSE
END IF	 

RETURN TRUE

end function

public function boolean noexistecliente (integer ai_codigo);String	ls_nombre

SELECT	clie_nombre
	INTO	:ls_nombre  
	FROM	dbo.clientesprod
	WHERE	clie_codigo =	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "Cliente no Existe. Ingrese otro.")
	RETURN True
END IF

IF F_ValidaCliente(ai_codigo) THEN
	istr_mant.Argumento[3]	=	String(ai_codigo)
	RETURN False
ELSE
	RETURN True
END IF

end function

public function boolean variedadrotula (integer codigo, integer especie);
SELECT vari_relaci
	INTO :il_varirotu
	FROM dbo.variedades
	WHERE espe_codigo = :especie
	AND vari_codigo    = :codigo;
 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla variedades")

	RETURN FALSE
ELSE	
 	RETURN True
END IF


end function

public function boolean existeembalaje (string as_embalaje);integer li_cliente, li_count

li_cliente = Integer(istr_Mant.Argumento[1])

SELECT count(*)
	INTO :li_count
	FROM dbo.embalajesprod 
	WHERE emba_codigo = :as_embalaje 
	AND  clie_codigo = :li_cliente;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla embalajesprod")
	Return False
END IF	

IF li_count = 0 THEN
	MessageBox('Atención', 'El Embalaje Ingresado no Existe o Pertenece a otro Cliente.')
	Return False	
END IF

Return True

end function

public function boolean existevariedad (integer ai_variedad);integer li_cliente, li_count, li_especie

li_cliente = Integer(istr_Mant.Argumento[1])
li_especie = dw_5.Object.espe_codigo[1]

SELECT count(*)
	INTO :li_count
	FROM dbo.variedades 
	WHERE vari_codigo = :ai_variedad 
	AND espe_codigo = :li_especie;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla variedades")
	Return False
END IF	

IF li_count = 0 THEN
	MessageBox('Atención', 'la Variedad Ingresada no Existe o Pertenece a otra Especie.')
	Return False	
END IF

Return True

end function

public function boolean existecalibre (string as_calibre, long ai_fila);integer li_count, li_especie, li_variedad
String	ls_calibre

li_especie = dw_5.Object.espe_codigo[1]
li_variedad = dw_4.Object.vari_codigo[ai_fila]

ls_calibre = MID((TRIM(as_calibre)+'   '),1,3)

SELECT   count(*)  
	INTO  :li_count
	FROM  dbo.variecalibre  
   WHERE  espe_codigo = :li_especie  
   AND  vari_codigo = :li_variedad   
   AND  vaca_calibr = :ls_calibre;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla calibresenvase")
	Return False
END IF	

IF li_count = 0 THEN
	MessageBox('Atención', 'El calibre Ingresado no Existe o Pertenece a otra Especie.')
	Return False	
END IF

Return True

end function

public function boolean existevarirotula (integer ai_variedad);integer li_cliente, li_count, li_especie

li_cliente = Integer(istr_Mant.Argumento[1])
li_especie = dw_5.Object.espe_codigo[1]

SELECT count(*)
	INTO :li_count
	FROM dbo.variedades 
	WHERE vari_relaci = :ai_variedad 
	AND espe_codigo = :li_especie;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla variedades")
	Return True
END IF	

IF li_count = 0 THEN
	MessageBox('Atención', 'la Variedad Rotulada Ingresada no Existe o Pertenece a otra Especie.')
	Return True	
END IF

Return False

end function

public function boolean existe_prodrotula (long al_codigo);integer li_cliente, li_count

li_cliente = Integer(istr_Mant.Argumento[1])

SELECT count(*)
	INTO :li_count
	FROM dbo.productores as pro,dbo.productoresclientes as cli 
	WHERE pro.prod_codigo = :al_codigo
	AND	pro.prod_codigo = cli.prod_codigo
	AND	cli.clie_codigo = :li_cliente;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Productores")
	Return True
END IF	

IF li_count = 0 THEN
	MessageBox('Atención', 'El Productor rotulado no existe en tabla productores o pertenece a otro cliente.')
	Return True	
END IF

Return False

end function

public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente);IF il_NroCaja < 1 OR IsNull(il_NroCaja) THEN

	IF NOT iuo_correl.Existe(ii_Planta,99, is_Computador, TRUE, sqlca) THEN
		SetNull(il_NroCaja)
		RETURN FALSE
	ELSE
		il_NroCaja	=	iuo_correl.il_correcompa
	END IF

ELSE
	il_NroCaja = il_NroCaja + 1
END IF

//dw_1.Object.capr_numero[1]	=	il_NroCaja

RETURN True
end function

public function boolean existe_transitorio (long al_pallet);integer	li_cliente, li_planta
long 		ll_cont
boolean	lb_Retorno = False

li_cliente = Integer(istr_Mant.Argumento[1])
li_planta = Integer(istr_Mant.Argumento[2])

SELECT count(*)
	INTO :ll_cont
	FROM dbo.recfruproced_trans as red,dbo.recfruprocee_trans as  rec 
	WHERE red.clie_codigo = :li_cliente  
	AND red.paen_numero = :al_Pallet 
	AND red.plde_codigo = :li_planta
	AND red.clie_codigo = rec.clie_codigo
	AND red.plde_codigo = rec.plde_codigo
	AND red.rfpe_numero = rec.rfpe_numero
	AND rec.rfpe_estado <> 2;

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla palletencab")
	lb_Retorno	=	True
ElseIf ll_cont > 0 Then
	lb_Retorno	= True
	MessageBox("Atención", "Número de Pallet " + String(al_Pallet, '00000000') + &
					", Esta en Estado Transitorio.~r~rIngrese o Seleccione Otro Número.")
End If

RETURN lb_Retorno
end function

public function boolean pallet_nrosag (long al_pallet, integer al_sag);integer li_cliente, li_planta, li_calidad
long 		ll_cont
boolean	lb_Retorno = False

li_cliente = Integer(istr_Mant.Argumento[1])
li_planta = Integer(istr_Mant.Argumento[2])

SELECT count(*)
	INTO :ll_cont
	FROM dbo.repalletenca as ren,dbo.repalletdeta as ret
	WHERE ret.clie_codigo = ren.clie_codigo
	AND  ret.plde_codigo = ren.plde_codigo
	AND ret.repe_numero = ren.repe_numero
	AND ret.clie_codigo = :li_cliente
	AND ret.plde_codigo = :li_planta
	AND ren.repe_nrosag = :al_sag
	AND (ret.paen_numero = :al_pallet 
	OR ret.paen_nroori = :al_pallet);
	
If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla repalletenca")
	lb_Retorno	=	True
ElseIf ll_cont > 0 Then
	MessageBox("Atención", "Número de Pallet " + String(al_Pallet, '00000000') + &
					", Asignado Mismo Número SAG en Otro Repa.~r~rIngrese o Seleccione Otro Número.")
	lb_Retorno	= True	
End If

Return lb_Retorno
end function

on w_maed_spro_repalasigenca_caja_caja.create
int iCurrent
call super::create
this.pb_nvopdest=create pb_nvopdest
this.pb_nvoporg=create pb_nvoporg
this.pb_acepta=create pb_acepta
this.dw_4=create dw_4
this.pb_cancela=create pb_cancela
this.dw_7=create dw_7
this.dw_8=create dw_8
this.dw_9=create dw_9
this.dw_10=create dw_10
this.ids_cuadracajas=create ids_cuadracajas
this.ids_palletencabhisto=create ids_palletencabhisto
this.ids_palletfrutahisto=create ids_palletfrutahisto
this.ids_palletfrutanuevo=create ids_palletfrutanuevo
this.dw_5=create dw_5
this.ids_palletencabupdate=create ids_palletencabupdate
this.ids_alpalletfruta=create ids_alpalletfruta
this.ids_alpalletencab=create ids_alpalletencab
this.ids_inspecpaldet=create ids_inspecpaldet
this.ids_despafrigoen=create ids_despafrigoen
this.ids_despafrigodet=create ids_despafrigodet
this.ids_palletencabhisto_des=create ids_palletencabhisto_des
this.ids_palletfrutahisto_des=create ids_palletfrutahisto_des
this.ids_recepcionenca=create ids_recepcionenca
this.ids_recepciondeta=create ids_recepciondeta
this.ids_palletencabnuevo=create ids_palletencabnuevo
this.pb_recupera=create pb_recupera
this.dw_compacto=create dw_compacto
this.dw_3=create dw_3
this.dw_6=create dw_6
this.dw_spro_cajasprod=create dw_spro_cajasprod
this.dw_11=create dw_11
this.dw_12=create dw_12
this.pb_agrupa=create pb_agrupa
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_nvopdest
this.Control[iCurrent+2]=this.pb_nvoporg
this.Control[iCurrent+3]=this.pb_acepta
this.Control[iCurrent+4]=this.dw_4
this.Control[iCurrent+5]=this.pb_cancela
this.Control[iCurrent+6]=this.dw_7
this.Control[iCurrent+7]=this.dw_8
this.Control[iCurrent+8]=this.dw_9
this.Control[iCurrent+9]=this.dw_10
this.Control[iCurrent+10]=this.ids_cuadracajas
this.Control[iCurrent+11]=this.ids_palletencabhisto
this.Control[iCurrent+12]=this.ids_palletfrutahisto
this.Control[iCurrent+13]=this.ids_palletfrutanuevo
this.Control[iCurrent+14]=this.dw_5
this.Control[iCurrent+15]=this.ids_palletencabupdate
this.Control[iCurrent+16]=this.ids_alpalletfruta
this.Control[iCurrent+17]=this.ids_alpalletencab
this.Control[iCurrent+18]=this.ids_inspecpaldet
this.Control[iCurrent+19]=this.ids_despafrigoen
this.Control[iCurrent+20]=this.ids_despafrigodet
this.Control[iCurrent+21]=this.ids_palletencabhisto_des
this.Control[iCurrent+22]=this.ids_palletfrutahisto_des
this.Control[iCurrent+23]=this.ids_recepcionenca
this.Control[iCurrent+24]=this.ids_recepciondeta
this.Control[iCurrent+25]=this.ids_palletencabnuevo
this.Control[iCurrent+26]=this.pb_recupera
this.Control[iCurrent+27]=this.dw_compacto
this.Control[iCurrent+28]=this.dw_3
this.Control[iCurrent+29]=this.dw_6
this.Control[iCurrent+30]=this.dw_spro_cajasprod
this.Control[iCurrent+31]=this.dw_11
this.Control[iCurrent+32]=this.dw_12
this.Control[iCurrent+33]=this.pb_agrupa
end on

on w_maed_spro_repalasigenca_caja_caja.destroy
call super::destroy
destroy(this.pb_nvopdest)
destroy(this.pb_nvoporg)
destroy(this.pb_acepta)
destroy(this.dw_4)
destroy(this.pb_cancela)
destroy(this.dw_7)
destroy(this.dw_8)
destroy(this.dw_9)
destroy(this.dw_10)
destroy(this.ids_cuadracajas)
destroy(this.ids_palletencabhisto)
destroy(this.ids_palletfrutahisto)
destroy(this.ids_palletfrutanuevo)
destroy(this.dw_5)
destroy(this.ids_palletencabupdate)
destroy(this.ids_alpalletfruta)
destroy(this.ids_alpalletencab)
destroy(this.ids_inspecpaldet)
destroy(this.ids_despafrigoen)
destroy(this.ids_despafrigodet)
destroy(this.ids_palletencabhisto_des)
destroy(this.ids_palletfrutahisto_des)
destroy(this.ids_recepcionenca)
destroy(this.ids_recepciondeta)
destroy(this.ids_palletencabnuevo)
destroy(this.pb_recupera)
destroy(this.dw_compacto)
destroy(this.dw_3)
destroy(this.dw_6)
destroy(this.dw_spro_cajasprod)
destroy(this.dw_11)
destroy(this.dw_12)
destroy(this.pb_agrupa)
end on

event open;/*
Argumentos :	[1]	=>	Código de Exportador
					[2]	=>	Código de Planta
					[3]	=>	Número de Repalletizaje
					[4]	=>	Número de Pallet Destino
					[5]	=>	Número de Pallet Origen
*/
x	= 0
y	= 0

iuo_tipopallet				=	Create uo_tipopallet
iuo_spro_palletencab		=	Create uo_spro_palletencab
iuo_exportador				=	Create uo_exportadores
iuo_productor				=  Create uo_productores
iuo_ControlVentanas		=	Create uo_controlventanas
iuo_Clientes					=	Create uo_Clientesprod
il_existerepa 				=	2
ii_nuinpe 					=	0

This.Height	= 2500
This.Width  	= 3600
im_menu		= m_principal

This.Icon										=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)	// Detalle de Repaletizado
dw_2.SetTransObject(sqlca) 	// Encabezado de Repaletizado
dw_3.SetTransObject(sqlca)	// Encabezado de Pallet Destino
dw_4.SetTransObject(sqlca)	// Detalle de Pallet Destino
dw_5.SetTransObject(sqlca) 	// Encabezado de Pallet Origen
dw_6.SetTransObject(sqlca)	 // Detalle de Pallet Origen
dw_7.SetTransObject(sqlca) 	// Encabezado de Pallet Origen
dw_8.SetTransObject(sqlca) 	// Encabezado de Pallet Origen

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_6.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_6
istr_mant.dw2						=	dw_10

istr_mant2.dw						=	ids_palletencabupdate
istr_mant2.dw2						=	ids_palletfrutanuevo
istr_mant2.dw3						=	dw_4
istr_mant2.dw4						=	ids_palletencabnuevo

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

dw_2.GetChild("plde_codigo", idwc_planta)

dw_3.GetChild("espe_codigo", idwc_espdest)
dw_3.GetChild("vari_codigo", idwc_vardest)
dw_3.GetChild("cate_codigo", idwc_catdest)
dw_3.GetChild("tpem_codigo", idwc_tipopaldest)

dw_4.GetChild("prod_codigo", idwc_proddest)
dw_4.GetChild("emba_codigo", idwc_embdest)
dw_4.GetChild("pafr_calibr", idwc_caldest)
dw_4.GetChild("vari_codigo", idwc_variedesdeta)

dw_5.GetChild("espe_codigo", idwc_esporig)
dw_5.GetChild("vari_codigo", idwc_varorig)
dw_5.GetChild("cate_codigo", idwc_catorig)
dw_5.GetChild("tpem_codigo", idwc_tipopalorig)

dw_6.GetChild("prod_codigo", idwc_prodorig)
dw_6.GetChild("emba_codigo", idwc_emborig)

dw_6.GetChild("pafr_calibr", idwc_calorig)
dw_4.GetChild("pafr_calibr", idwc_caldest)

idwc_planta.SetTransObject(SqlCa)
idwc_espdest.SetTransObject(SqlCa)
idwc_vardest.SetTransObject(SqlCa)
idwc_catdest.SetTransObject(SqlCa)
idwc_tipopaldest.SetTransObject(SqlCa)
idwc_proddest.SetTransObject(SqlCa)
idwc_embdest.SetTransObject(SqlCa)
idwc_caldest.SetTransObject(SqlCa)
idwc_esporig.SetTransObject(SqlCa)
idwc_varorig.SetTransObject(SqlCa)
idwc_catorig.SetTransObject(SqlCa)
idwc_tipopalorig.SetTransObject(SqlCa)
idwc_prodorig.SetTransObject(SqlCa)
idwc_emborig.SetTransObject(SqlCa)
idwc_calorig.SetTransObject(SqlCa)
idwc_variedesdeta.SetTransObject(SqlCa)

idwc_planta.Retrieve(1)

dw_3.GetChild("nuro_inpecc", idwc_inspec)
idwc_inspec.SetTransObject(SqlCa)
idwc_inspec.Retrieve(gi_CodExport, gi_CodPlanta)

IF idwc_espdest.Retrieve() = 0 THEN
	idwc_espdest.InsertRow(0)
END IF

IF idwc_calorig.Retrieve(0) = 0 THEN
	idwc_calorig.InsertRow(0)
END IF

IF idwc_caldest.Retrieve(0) = 0 THEN
	idwc_caldest.InsertRow(0)
END IF

IF idwc_vardest.Retrieve(0) = 0 THEN
	idwc_vardest.InsertRow(0)
END IF

IF idwc_variedesdeta.Retrieve(0) = 0 THEN
	idwc_variedesdeta.InsertRow(0)
END IF

IF idwc_catdest.Retrieve() = 0 THEN
	idwc_catdest.InsertRow(0)
END IF

idwc_tipopaldest.InsertRow(0)

IF idwc_esporig.Retrieve() = 0 THEN
	idwc_esporig.InsertRow(0)
END IF

IF idwc_varorig.Retrieve(0) = 0 THEN
	idwc_vardest.InsertRow(0)
END IF

IF idwc_catorig.Retrieve() = 0 THEN
	idwc_catorig.InsertRow(0)
END IF

idwc_tipopalorig.InsertRow(0)

istr_Mant.Argumento[1]	=	String(gi_CodExport)		// Exportador
istr_Mant.Argumento[2]	=	String(gi_CodPlanta)	     // Planta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

buscar	= "Productor:Nprod_codigo,Variedad:Nvari_codigo,Embalaje:Semba_codigo"
ordenar	= "Productor:prod_codigo,Variedad:vari_codigo,Embalaje:emba_codigo"

dw_3.Object.paen_numero.Protect	=	1
dw_3.Object.paen_tipopa.Protect		=	1
dw_3.Object.tpem_codigo.Protect		=	1

dw_3.Object.paen_numero.Color		= Rgb(255,255,255)
dw_3.Object.tpem_codigo.Color 		= Rgb(255,255,255)

dw_3.Object.paen_numero.BackGround.Color	= 553648127
dw_3.Object.tpem_codigo.BackGround.Color 	= 553648127

dw_3.Object.fumi_numero[1] = 0

dw_5.Object.paen_numero.Protect	=	1
dw_5.Object.paen_tipopa.Protect		=	1
dw_5.Object.tpem_codigo.Protect		=	1

dw_5.Object.paen_numero.Color		= Rgb(255,255,255)
dw_5.Object.tpem_codigo.Color 		= Rgb(255,255,255)

dw_5.Object.paen_numero.BackGround.Color	= 553648127
dw_5.Object.tpem_codigo.BackGround.Color 	= 553648127

dw_5.Modify("paen_numero_t.Text = 'Nro.Pallet Origen'")

dw_3.Object.b_siguiente.Visible = 0
dw_3.Object.b_anterior.Visible = 0

// Para actualizar en el historico
//ids_palletencabhisto 				= Create DataStore 
//ids_palletfrutahisto					= Create DataStore
//ids_alpalletfruta 						= Create DataStore 
//ids_alpalletencab					= Create DataStore
//ids_cuadracajas						= Create DataStore
//ids_palletencabnuevo				= Create DataStore
//ids_palletfrutanuevo					= Create DataStore
//ids_palletencabupdate				= Create DataStore
//ids_inspecpaldet						= Create DataStore
//ids_despafrigoen						= Create DataStore
//ids_despafrigodet					= Create DataStore
//ids_recepcionenca					= Create DataStore
//ids_recepciondeta					= Create DataStore
//ids_palletencabhisto_des 			= Create DataStore
//ids_palletfrutahisto_des 			= Create DataStore

//ids_palletencabhisto.DataObject 	= "dw_mant_palletencabhisto"
ids_palletfrutahisto.DataObject 	= "dw_mant_palletfrutahisto"//detalle historico de pallet
ids_palletencabhisto.SetTransObject(sqlca)//encabezado historico del pallet
ids_palletfrutahisto.SetTransObject(sqlca)//detalle historico de pallet
ids_palletencabupdate.SetTransObject(sqlca)//Actualiza pallet encabezados destinos

ids_alpalletfruta.DataObject = "dw_mues_alpalletfruta1"//detalle alturas
ids_alpalletencab.DataObject = "dw_mant_alpalletencab1"//encabezado alturas
ids_inspecpaldet.DataObject = "dw_mues_inspecpaldet"//detalle inspeccion
ids_alpalletfruta.SetTransObject(sqlca)
ids_alpalletencab.SetTransObject(sqlca)
ids_inspecpaldet.SetTransObject(sqlca)
ids_despafrigoen.SetTransObject(sqlca)
ids_despafrigodet.SetTransObject(sqlca)
ids_recepcionenca.SetTransObject(sqlca)
ids_recepciondeta.SetTransObject(sqlca)	
ids_palletencabhisto_des.SetTransObject(sqlca) 		
ids_palletfrutahisto_des.SetTransObject(sqlca)

//ids_cuadracajas.DataObject 			= "dw_mues_palletfruta_repaletizaje_origen"
//ids_palletencabnuevo.DataObject 	= "dw_mant_palletencab_repaletizaje2"
//ids_palletfrutanuevo.DataObject 		= "dw_mues_palletfruta_repaletizaje2"
//ids_palletencabupdate.DataObject 	= ""

ids_cuadracajas.SetTransObject(SqlCa)//dw ocupada solo para llevar cuenta de cajas en completar pallet
ids_palletencabnuevo.SetTransObject(SqlCa)//Encabezado de pallet destino nuevo
ids_palletfrutanuevo.SetTransObject(SqlCa)//detalle de pallet destino nuevo

ids_despafrigoen.DataObject			=	"dw_mant_despafrigoen_repa"
ids_despafrigodet.DataObject			=	"dw_mues_despafrigode"
ids_recepcionenca.DataObject			=	"dw_mant_recepcionenca_repa"
ids_recepciondeta.DataObject			=	"dw_mant_recepciondeta_repa"
ids_palletencabhisto_des.DataObject	=	"dw_mant_palletencabhisto"
ids_palletfrutahisto_des.DataObject	=	"dw_mant_palletfrutahisto"

CamaraExiste()
end event

event ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, ll_PalletNuevo
	dw_2.SetRedraw(False)
	dw_2.Reset()
	dw_3.SetRedraw(False)
	dw_3.Reset()

	IF dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), &
						  Integer(istr_Mant.Argumento[2]), &
						  Long(istr_Mant.Argumento[3])) =  -1 OR &						  
		 dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
						  Integer(istr_Mant.Argumento[2]), &
						  Long(istr_Mant.Argumento[3]))  = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		IF dw_1.RowCount() = 0 THEN
			Return 
		ELSE
			IF dw_2.Object.repe_tipopa[1] = 3 OR dw_2.Object.repe_tipopa[1] = 7 THEN
				istr_Mant.Argumento[4] = string(dw_1.object.paen_numero[1])
			ELSE
				istr_Mant.Argumento[4] = string(dw_1.object.paen_nroori[1])
			END IF	
			il_existerepa = 1
			TriggerEvent("ue_recuperapallet_destino")			
			ib_ExistePalletDestino	=	True
			pb_imprimir.Enabled		=	True
					
			HabilitaEncab(False)
			
			dw_1.SetRow(1)
			dw_1.SelectRow(1,True)
			dw_1.SetFocus()
		END IF
	END IF
		dw_2.SetRedraw(True)
		dw_3.SetRedraw(True)

IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo;Long		ll_modif1, ll_modif2 
Integer	li_cliente

li_cliente = Integer(Istr_mant.argumento[1])

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not ib_ok THEN RETURN

dw_1.Reset()
dw_2.Reset()
dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()

ii_nuinpe = 0
il_destino = 0

ids_palletencabhisto.Reset() 
ids_palletfrutahisto.Reset() 
ids_alpalletfruta.Reset() 
ids_alpalletencab.Reset() 
ids_palletencabnuevo.Reset()
ids_palletfrutanuevo.Reset()
ids_palletencabupdate.Reset()
dw_9.Reset()
dw_10.Reset()
ids_cuadracajas.Reset()
ids_palletencabhisto_des.Reset()
ids_palletencabhisto_des.Reset()
ids_despafrigoen.Reset()
ids_despafrigodet.Reset()
ids_recepcionenca.Reset()
ids_recepciondeta.Reset()

ids_palletencabhisto.SetTransObject(sqlca)
ids_palletfrutahisto.SetTransObject(sqlca)
ids_alpalletfruta.SetTransObject(sqlca)
ids_alpalletencab.SetTransObject(sqlca)
ids_palletencabnuevo.SetTransObject(sqlca)
ids_palletfrutanuevo.SetTransObject(sqlca)
ids_palletencabupdate.SetTransObject(sqlca)
dw_9.SetTransObject(sqlca)
dw_10.SetTransObject(sqlca)
ids_cuadracajas.SetTransObject(sqlca)
ids_palletencabhisto_des.SetTransObject(sqlca)
ids_palletencabhisto_des.SetTransObject(sqlca)
ids_despafrigoen.SetTransObject(sqlca)
ids_despafrigodet.SetTransObject(sqlca)
ids_recepcionenca.SetTransObject(sqlca)
ids_recepciondeta.SetTransObject(sqlca)

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled		=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True
dw_3.Enabled				=	True
pb_nvoporg.Enabled		=	False
pb_acepta.Enabled		=	False
pb_nvopdest.Enabled 		=  False

dw_2.SetRedraw(False)
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

//dw_3.GetChild("tpem_codigo", idwc_tipopaldest)
//idwc_tipopaldest.SetTransObject(SQLCA)
//IF idwc_tipopaldest.Retrieve(li_cliente,'Z') = 0 THEN
//	idwc_tipopaldest.InsertRow(0)
//END IF

dw_3.SetRedraw(False)
dw_3.InsertRow(0)
dw_3.SetRedraw(True)

dw_5.SetRedraw(False)
dw_5.InsertRow(0)
dw_5.SetRedraw(True)

dw_2.SetFocus()

HabilitaEncab(True)

pb_cancela.visible 		= FALSE
ib_ExistePalletDestino 	= FALSE
ib_inspeccion_origen  	= FALSE
ib_inspeccion_destino 	= FALSE

dw_3.Object.paen_numero.Protect 	= 1
dw_3.Object.paen_tipopa.Protect 		= 1
dw_3.Object.tpem_codigo.Protect 		= 1

dw_5.Object.paen_numero.Protect	= 1
dw_5.Object.paen_tipopa.Protect 		= 1
dw_5.Object.tpem_codigo.Protect 		= 1

dw_3.Object.paen_numero.Color 	= RGB(255,255,255)
dw_3.Object.tpem_codigo.Color 	= RGB(255,255,255)

dw_5.Object.paen_numero.Color 	= RGB(255,255,255)
dw_5.Object.tpem_codigo.Color 	= RGB(255,255,255)

dw_3.Object.paen_numero.BackGround.Color 	= 553648127
dw_3.Object.tpem_codigo.BackGround.Color 	= 553648127

dw_5.Object.paen_numero.BackGround.Color	= 553648127
dw_5.Object.tpem_codigo.BackGround.Color 	= 553648127

dw_3.GetChild("tpem_codigo", idwc_tipopaldest)
idwc_tipopaldest.SetTransObject(SqlCa)
idwc_tipopaldest.Retrieve(gi_CodExport,'X')
idwc_tipopaldest.InsertRow(0)

pb_nvopdest.Enabled 	= FALSE
pb_nvoporg.Enabled	= False

dw_2.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))
dw_2.SetItem(1, "repe_fecrep", DateTime(Today()))
dw_2.SetItem(1, "repe_factur", Integer(1))

dw_2.SetItem(1, "clie_codigo", Integer(istr_Mant.Argumento[1]))
dw_3.SetItem(1, "clie_codigo", Integer(istr_Mant.Argumento[1]))
dw_5.SetItem(1, "clie_codigo", Integer(istr_Mant.Argumento[1]))
iuo_Clientes.Existe(Integer(istr_Mant.Argumento[1]), False, Sqlca)

is_palletorig = ""
is_palletdest = ""

dw_2.SetColumn("repe_tipopa")
istr_Mant.Argumento[3] = ""

dw_3.Object.b_siguiente.Visible = 0
dw_3.Object.b_anterior.Visible = 0
end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0, li_alto1, &
			li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

dw_2.width				= this.workspacewidth() - 400
maximo	= dw_2.width

dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)

ids_palletencabnuevo.x = 3170
ids_palletencabnuevo.y = 20
ids_palletencabnuevo.width = 1120
ids_palletencabnuevo.height =  1756

dw_3.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_3.width				= this.workspacewidth() - 400

li_alto1 					= ROUND((This.WorkSpaceHeight() - dw_2.height - dw_3.height - dw_5.height)/2,0)

dw_4.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_4.y					= dw_2.Height + dw_3.Height
dw_4.height				= li_alto1
dw_4.width				= this.workspacewidth() - 400

dw_5.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_5.y					= dw_2.Height + dw_3.Height + dw_4.height
dw_5.width				= this.workspacewidth() - 400

dw_6.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_6.y					= dw_2.Height + dw_3.Height + dw_4.height + dw_5.height
dw_6.height				= li_alto1
dw_6.width				= this.workspacewidth() - 400

li_posic_x				= This.WorkSpaceWidth() - 300
li_posic_y				= 350

IF pb_buscar.Visible THEN
	pb_buscar.x				= li_posic_x
	pb_buscar.y				= li_posic_y
	pb_buscar.width		= li_Ancho
	pb_buscar.height		= li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				= li_posic_x
	pb_nuevo.y				= li_posic_y
	pb_nuevo.width		= li_Ancho
	pb_nuevo.height		= li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				= li_posic_x
	pb_grabar.y				= li_posic_y
	pb_grabar.width		= li_Ancho
	pb_grabar.height		= li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			= li_posic_x
	pb_imprimir.y			= li_posic_y
	pb_imprimir.width		= li_Ancho
	pb_imprimir.height	= li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_salir.Visible THEN
	pb_salir.x				= li_posic_x
	pb_salir.y				= li_posic_y
	pb_salir.width			= li_Ancho
	pb_salir.height		= li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_nvopdest.Visible THEN
	pb_nvopdest.x			= li_posic_x
	pb_nvopdest.y			= li_posic_y
	pb_nvopdest.width		= li_Ancho	
	pb_nvopdest.height	= li_Alto
	li_posic_y += li_Siguiente
End If

IF pb_agrupa.Visible THEN
	pb_agrupa.x				= li_posic_x //- 60
	pb_agrupa.y				= li_posic_y
	pb_agrupa.width		= li_Ancho	
//	pb_agrupa.height		= li_Alto
	li_posic_y += li_Siguiente
End If

IF pb_acepta.Visible THEN
	pb_acepta.x				= li_posic_x
	pb_acepta.y				= li_posic_y
	pb_acepta.width		= li_Ancho	
	pb_acepta.height		= li_Alto
	li_posic_y += li_Siguiente
End If

IF pb_cancela.Visible THEN
	pb_cancela.x			= li_posic_x
	pb_cancela.y			= li_posic_y
	pb_cancela.width		= li_Ancho
	pb_cancela.height		= li_Alto
	li_posic_y += li_Siguiente
End If

IF pb_nvoporg.Visible THEN
	pb_nvoporg.x			= li_posic_x
	pb_nvoporg.y			= li_posic_y
	pb_nvoporg.width		= li_Ancho
	pb_nvoporg.height		= li_Alto
	li_posic_y += li_Siguiente
End If

IF pb_recupera.Visible THEN
	pb_recupera.x			= li_posic_x
	pb_recupera.y			= li_posic_y
	pb_recupera.width		= li_Ancho
	pb_recupera.height	= li_Alto
	li_posic_y += li_Siguiente
End If

IF pb_eliminar.Visible THEN
	pb_eliminar.x			= li_posic_x
	pb_eliminar.y			= li_posic_y
	pb_eliminar.width		= li_Ancho
	pb_eliminar.height	= li_Alto
	li_posic_y += li_Siguiente
End If

end event

event ue_borra_detalle;IF dw_4.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_4.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

 IF dw_4.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_Fila, ll_NumeroRepal, ll_PalletDestino, ll_totalcajas, ll_NumeroRepal2, &
			ll_actual, ll_fin, ll_fila_rep, ll_Fila10, ll_Fila11, ll_fila12, ll_filaenca,&
			ll_paen_ccajas, ll_tota_ccajas, ll_fila_enca, ll_paen_ccajas2, ll_cont, ll_numpro,&
			ll_fila_his, ll_fila_fru, ll_fila_alt, ll_fila_adet, ll_fila_enca2, ll_fila_repa, ll_con_sag
Integer	li_Exportador, li_Planta, li_TipoMovto, li_Secuencia, li_tiporepa, li_movto, &
			li_paen_tipopa, li_repe_tipopa, li_totalcajas
String	ls_tpem_codigo

ib_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

li_Exportador		=	dw_3.Object.clie_codigo[1]
li_Planta				=	dw_2.Object.plde_codigo[1]
ll_NumeroRepal	=	dw_2.Object.repe_numero[1]

dw_9.SetTransObject(sqlca)//encabezado pallet origen
dw_10.SetTransObject(sqlca)//Detalle pallet origen

ls_tpem_codigo = dw_3.Object.tpem_codigo[1]
li_paen_tipopa = dw_3.Object.paen_tipopa[1]

FOR  ll_con_sag = 1 TO dw_3.RowCount()
	IF dw_3.Object.paen_inspec[ll_con_sag] = 1 THEN
		IF isnull(dw_2.Object.repe_nrosag[1]) OR dw_2.Object.repe_nrosag[1] = 0 THEN
			messagebox("Advertencia","Falta el ingreso del Número SAG.")
			Message.DoubleParm = -1
			sqlca.AutoCommit 	=	ib_AutoCommit	
			Return
		END IF
	END IF	
NEXT

IF dw_4.RowCount() > 0 THEN
	li_totalcajas  = dw_4.Object.total_cajas[1]
	IF li_paen_tipopa = 1 THEN
	
		IF Integer(ls_tpem_codigo) <> li_totalcajas THEN
			messagebox("Advertencia","Altura Destino no Cuadra con Total Cajas Pallet.")
			Message.DoubleParm = -1
			sqlca.AutoCommit 	=	ib_AutoCommit	
			Return
		END IF
	
	END IF	
END IF

IF isnull(ls_tpem_codigo) and li_paen_tipopa = 1 THEN
   messagebox("Falta de Datos","Falta Seleccionar un Tipo de Pallet en Pallet Destino.")
   Message.DoubleParm = -1
   sqlca.AutoCommit 	=	ib_AutoCommit	
   Return
END IF

li_repe_tipopa = dw_2.Object.repe_tipopa[1]

IF li_repe_tipopa = 3 THEN
	FOR ll_cont = 1 TO ids_palletencabnuevo.RowCount()
		IF IsNull(ids_palletencabnuevo.Object.espe_codigo[ll_cont]) OR ids_palletencabnuevo.Object.espe_codigo[ll_cont]= 0 THEN
			messagebox("Advertencia","Problemas con los datos del Pallet Nuevo(Especie).")
   		Message.DoubleParm = -1
   		sqlca.AutoCommit 	=	ib_AutoCommit
		END IF	
		IF IsNull(ids_palletencabnuevo.Object.vari_codigo[ll_cont]) OR ids_palletencabnuevo.Object.vari_codigo[ll_cont]= 0 THEN
			messagebox("Advertencia","Problemas con los datos del Pallet Nuevo(variedad).")
   		Message.DoubleParm = -1
   		sqlca.AutoCommit 	=	ib_AutoCommit
		END IF	
	NEXT
END IF

IF li_repe_tipopa = 3 THEN
	IF cuadrar_cajas(2) = False THEN
		messagebox("Error","Falta cuadrar Cajas. "+'Pallet '+String(il_pallet,'00000000'))
		Message.DoubleParm = -1
		sqlca.AutoCommit 	=	ib_AutoCommit	
		Return
	ELSE
		FOR ll_filaenca = 1 TO dw_9.RowCount()
			dw_9.Object.paen_ccajas[ll_filaenca] = 0
		NEXT	
	END IF	
END IF
	
IF dw_4.RowCount() > 0 THEN
	ll_totalcajas = dw_4.Object.total_cajas[1]
ELSE
	ll_totalcajas = 0
END IF

ll_paen_ccajas = dw_3.Object.paen_ccajas[1]

IF ll_totalcajas < ll_paen_ccajas THEN
	dw_3.SetItem(1,"paen_tipopa",2)
ELSEIF ll_totalcajas = Long(ls_tpem_codigo) THEN
	dw_3.SetItem(1,"paen_tipopa",1)
ELSEIF li_paen_tipopa = 1 AND ll_totalcajas > ll_paen_ccajas THEN
	MessageBox("Atención","El total de cajas supera al tipo pallet seleccionado.")
	Message.DoubleParm = -1
	sqlca.AutoCommit 	=	ib_AutoCommit	
	RETURN
END IF	

dw_3.SetItem(1,"tmvp_codigo",3)

IF dw_6.RowCount() > 0 THEN
	ll_totalcajas = dw_6.Object.total_cajas[1]
ELSE
	ll_totalcajas = 0
END IF

IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
	
	li_tiporepa = dw_2.Object.repe_tipopa[1]
	
	li_movto = 4
	SELECT max(repe_numero) 
		INTO  :ll_NumeroRepal
		FROM dbo.repalletenca
		WHERE plde_codigo = :li_planta
		AND  repe_numero <> 99999999;

	SELECT como_inicia, como_actual, como_termin
		INTO	:ll_NumeroRepal2, :ll_actual, :ll_fin
		FROM dbo.correlmovimientos
		WHERE plde_codigo = :li_planta
		AND	como_tipomv = :li_movto;
	
	IF ll_actual >= ll_fin THEN
			MessageBox("Advertencia","No Quedan Correlativos Disponibles, Proceda por Mantención.")
			Message.DoubleParm = -1
			Return 
	END IF	
	
	ll_fin = ll_fin - 3
	
	IF ll_actual >= ll_fin THEN 
		MessageBox("Advertencia","Quedan Menos de 3 Correlativos, Proceda por Mantención 'Correlativos'")
	END IF	
	
	IF sqlca.SQLCode = -1 THEN
			F_errorbasedatos(sqlca,"Lectura tabla CORRELMOVIMIENTOS")
	END IF
	
	IF Isnull(ll_NumeroRepal) OR String(ll_NumeroRepal) = '' or ll_NumeroRepal < ll_NumeroRepal2 THEN
		ll_NumeroRepal = ll_NumeroRepal2
	END IF	
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla repalletenca")
	ELSEIF sqlca.SQLCode = 0 THEN
		ll_NumeroRepal++
	END IF
	
	dw_2.Object.repe_numero[1]	=	ll_NumeroRepal
	il_numero = ll_NumeroRepal
END IF

istr_Mant.Argumento[3]	=	String(dw_2.Object.repe_numero[1])
istr_Mant.Argumento[4]	=	String(dw_3.Object.paen_numero[1])

ll_PalletDestino			=	dw_3.Object.paen_numero[1]

captura_total_cajas()

FOR ll_Fila = 1 TO dw_4.RowCount()
	IF dw_4.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
		dw_4.Object.paen_numero[ll_Fila]	=	dw_3.Object.paen_numero[1]
	END IF	
NEXT

FOR ll_Fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
		dw_1.Object.repe_numero[ll_Fila]	=	dw_2.Object.repe_numero[1]
	END IF	
NEXT

IF li_repe_tipopa = 1 OR li_repe_tipopa = 2 OR li_repe_tipopa = 7 THEN
	FOR ll_fila_enca = 1 TO dw_9.RowCount() 
		ll_paen_ccajas2 = dw_9.Object.paen_ccajas[ll_fila_enca]
		IF ll_paen_ccajas2 = 0 THEN
			dw_9.Object.paen_estado[ll_fila_enca] = 3 
		ELSE	
			dw_9.Object.paen_estado[ll_fila_enca] = 1
		END IF	
	NEXT	
END IF	

dw_9.ResetUpdate()
FOR ll_Fila10 = 1 TO dw_9.RowCount()
	dw_9.SetItemStatus(ll_Fila10,"paen_ccajas", Primary!, DataModified!) 
	dw_9.SetItemStatus(ll_Fila10,"paen_estado", Primary!, DataModified!) 
	dw_9.SetItemStatus(ll_Fila10,"paen_tipopa", Primary!, DataModified!)
	dw_9.SetItemStatus(ll_Fila10,"tpem_codigo", Primary!, DataModified!)
NEXT

dw_10.ResetUpdate()
FOR ll_Fila11 = 1 TO dw_10.RowCount()
	dw_10.SetItemStatus(ll_Fila11,"pafr_ccajas", Primary!, DataModified!)
	dw_10.SetItemStatus(ll_Fila11,"cajas", Primary!, DataModified!)
NEXT

dw_2.Object.repe_numero.Protect	=	1
dw_2.Object.repe_numero.BackGround.Color = RGB(166,180,210)
dw_4.Object.pafr_calibr.Protect				=	1
dw_4.Object.emba_codigo.Protect				=	1	
dw_4.Object.pafr_ccajas.Protect				=	1

//IF dw_4.RowCount() > 0 THEN
//	IF li_repe_tipopa <> 1 AND li_repe_tipopa <> 2 AND li_repe_tipopa <> 7 THEN
//		traspasa_cajas_nuevas()
//	END IF
//	
//	IF li_repe_tipopa = 1 OR li_repe_tipopa = 2 OR li_repe_tipopa = 7 THEN 
//		traspasa_cajas_altura(1)
//	END IF
//	
//END IF	

SELECT	IsNull(Max(altu_numero), 0) + 1
	INTO	:ll_numpro
	FROM	dbo.alpalletencab
	WHERE	plde_codigo	= :li_Planta
	AND 	clie_codigo = :li_Exportador
	AND	altu_numero < 99999999;
/*
Bloquea Tablas
*/	
SELECT empr_oficin 
	INTO :is_empr_oficin
	FROM dbo.parempresa;	
	
UPDATE dbo.parempresa SET
	empr_oficin = Mid(is_empr_oficin,1,4);
/**/

IF ids_palletencabhisto.Object.pahi_proces[1] > ll_numpro THEN
	ll_numpro = ids_palletencabhisto.Object.pahi_proces[1]
END IF	

FOR ll_fila_his = 1 TO	ids_palletencabhisto.RowCount()
	ids_palletencabhisto.SetItem(ll_fila_his,"pahi_proces",ll_numpro)
NEXT	

FOR ll_fila_fru = 1 TO	ids_palletfrutahisto.RowCount()
	ids_palletfrutahisto.SetItem(ll_fila_fru,"pafh_proces",ll_numpro)
NEXT

FOR ll_fila_alt = 1 TO	ids_alpalletencab.RowCount()
	ids_alpalletencab.SetItem(ll_fila_alt,"altu_numero",ll_numpro)
NEXT	

FOR ll_fila_adet = 1 TO	ids_alpalletfruta.RowCount()
	ids_alpalletfruta.SetItem(ll_fila_adet,"altu_numero",ll_numpro)
NEXT

FOR ll_fila_repa = 1 TO	dw_2.RowCount()
	dw_2.SetItem(ll_fila_repa,"repe_proces",ll_numpro)
NEXT

end event

event ue_guardar;Integer	li_cliente, li_planta
Long		ll_numero

SetPointer(HourGlass!)

If dw_4.AcceptText() = -1 Then Return
w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0
TriggerEvent("ue_antesguardar")

If Message.DoubleParm = -1 Then Return

If wf_actualiza_db(False) Then
	w_main.SetMicroHelp("Información Grabada.")
	
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
	
	li_planta 		= dw_2.Object.plde_codigo[1]
	li_cliente		= dw_2.Object.clie_codigo[1]
	ll_numero 	= dw_2.Object.repe_numero[1]
	
	DECLARE Actualiza_condicion PROCEDURE FOR dbo.Fproc_actualiza_condicion	
			  @cliente = :li_cliente,
			  @planta  = :li_planta,
			  @numero  = :ll_numero;
	EXECUTE Actualiza_condicion;
	
	Commit;
			
	If sqlca.SQLCode = -1 Then
		F_ErrorBaseDatos(sqlca, "Lectura Procedimiento Fproc_actualiza_condicion")
		Return
	End If	
	
	If dw_2.Object.repe_tipopa[1] = 7 Then TriggerEvent("ue_despuesgrabar")
	TriggerEvent("ue_nuevo")
		
	If dw_2.Object.repe_tipopa[1] = 6 Then		
		dw_3.SetColumn("paen_numero")
		ib_ExistePalletDestino = False
		
		pb_nvoporg.TriggerEvent(Clicked!)
		pb_nvopdest.TriggerEvent(Clicked!)
		
		dw_3.Object.tpem_codigo.Protect	=	1
		dw_3.Object.tpem_codigo.Color 	= RGB(255,255,255)
		dw_3.Object.tpem_codigo.BackGround.Color = 553648127
		dw_3.Object.nuro_inpecc.Protect	=	1
		dw_3.Object.nuro_inpecc.Color 	= RGB(255,255,255)
		dw_3.Object.nuro_inpecc.BackGround.Color = 553648127
	
		dw_5.Object.paen_numero.Protect	=	1
		dw_5.Object.paen_numero.Color 		= RGB(255,255,255)
		dw_5.Object.paen_numero.BackGround.Color = 553648127
		dw_5.Object.paen_tipopa.Protect	=	1
//		dw_5.Object.paen_tipopa.Color 	= RGB(255,255,255)
		dw_5.Object.paen_tipopa.BackGround.Color = 553648127
		dw_5.Object.tpem_codigo.Protect	=	1
		dw_5.Object.tpem_codigo.Color 	= RGB(255,255,255)
		dw_5.Object.tpem_codigo.BackGround.Color = 553648127
		pb_nvoporg.Enabled	=	False
		pb_nvopdest.Enabled	=	False
		pb_acepta.Enabled	=	False
		pb_acepta.Visible    	=  True
		pb_cancela.Visible   	=  False
		
	Else	
		dw_5.SetColumn("paen_numero")	
		pb_nvoporg.TriggerEvent(Clicked!)
		
		dw_3.Object.tpem_codigo.Protect	=	1
		dw_3.Object.tpem_codigo.Color 	= 553648127
		dw_3.Object.tpem_codigo.BackGround.Color = 553648127
	
		dw_5.Object.paen_numero.Protect	=	0
		dw_5.Object.paen_numero.Color 		= 0
		dw_5.Object.paen_numero.BackGround.Color = RGB(255,255,255)
		dw_5.Object.paen_tipopa.Protect	=	1
		dw_5.Object.paen_tipopa.BackGround.Color = 553648127
		dw_5.Object.tpem_codigo.Protect	=	1
		dw_5.Object.tpem_codigo.Color 	= RGB(255,255,255)
		dw_5.Object.tpem_codigo.BackGround.Color = 553648127
		dw_3.Object.nuro_inpecc.Protect	=	1
		dw_3.Object.nuro_inpecc.Color 	= RGB(255,255,255)
		dw_3.Object.nuro_inpecc.BackGround.Color = 553648127
	
		pb_nvoporg.Enabled	=	False
		pb_nvopdest.Enabled	=	False
		pb_acepta.Enabled	=	False
		pb_acepta.Visible    	=  True
		pb_cancela.Visible   	=  False
	End If	
Else
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	Return
End If
end event

event ue_imprimir;Integer li_repe_tipopa
SetPointer(HourGlass!)

Long		fila
date  ld_fecha

istr_info.titulo	= "REPALLETIZAJE"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

li_repe_tipopa = dw_2.Object.repe_tipopa[1]

IF li_repe_tipopa = 3 THEN
	vinf.dw_1.DataObject = "dw_info_repalasgenca_completapal"
ELSEIF li_repe_tipopa = 1	THEN
	vinf.dw_1.DataObject = "dw_info_repalasgenca_rebaje"
ELSEIF li_repe_tipopa = 2	THEN	
	vinf.dw_1.DataObject = "dw_info_repalasgenca_levanta"
ELSEIF li_repe_tipopa = 7	THEN	
	vinf.dw_1.DataObject = "dw_info_repalasgenca_reembalaje"
END IF

vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],ld_fecha,ld_fecha,&
		                           dw_2.Object.repe_numero[1], dw_2.Object.repe_numero[1])

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_borrar;//
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_repalasigenca_caja_caja
string tag = "Detalle Repalletizaje"
boolean visible = false
integer x = 3479
integer y = 2320
integer width = 192
integer height = 132
boolean titlebar = false
string title = "Detalle de Repaletizado"
string dataobject = "dw_mues_spro_repalasigdeta"
boolean hscrollbar = false
boolean vscrollbar = false
boolean border = true
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_repalasigenca_caja_caja
string tag = "Encabezado Repalletizaje"
integer x = 5
integer y = 0
integer width = 3113
integer height = 392
integer taborder = 10
string dataobject = "dw_mant_spro_repalasigenca"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Fecha, ls_null
Integer  li_exportador, li_existe

SetNull(ls_null)

ls_Columna = dwo.name

CHOOSE Case ls_Columna
	Case "repe_fecrep"
		ls_Fecha	=	Data
		This.SetItem(1, ls_Columna, DateTime(Date(Mid(ls_Fecha,1,10))))

   Case "repe_tipopa"
		If Data = "4" OR Data = "5" OR Data = "6" Then
			Messagebox("Advertencia","Tipo de Repalletizado No Disponible En Esta Opción!")
			This.SetItem(row,"repe_tipopa",Integer(ls_null))
			Return 1
		End If	
		
		If Data <> "7" Then
			dw_2.Object.repe_mercaj[row] = 0
			dw_2.Object.repe_merkil[row] = 0
			pb_agrupa.Visible 			  = False
		Else
			pb_agrupa.Visible 			  = True
		End If
		
		If Data = '3' Then
			This.Object.esta_codigo[Row] = 12
		ElseIf Data = '2' Then
			This.Object.esta_codigo[Row] = 11
		Else
			This.Object.esta_codigo[Row] = 13
		End If
		
		This.Object.repe_fecpro[Row] = DateTime(Today(), Now())
		
	Case "clie_codigo"
		li_exportador = this.Object.clie_codigo[row]
		
		iuo_Clientes.Existe(Integer(data), False, Sqlca)
		If NoExisteCliente(Integer(data)) Then
			This.SetItem(row,"clie_codigo",li_exportador)
			Return 1
		Else
			istr_mant.argumento[1]	= data
			dw_3.SetItem(1, "clie_codigo", Integer(istr_Mant.Argumento[1]))
			dw_5.SetItem(1, "clie_codigo", Integer(istr_Mant.Argumento[1]))
		End If			
		
	Case "plde_codigo"
		istr_mant.argumento[2]	= data
		dw_3.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))
		dw_5.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))
		
		camaraexiste()
	Case "repe_numero"
		istr_mant.argumento[3] = data
		If ExisteRepalletizado() Then
			Parent.TriggerEvent("ue_recuperadatos")
			il_existerepa = 1
		Else
			THIS.SetItem(row,"repe_numero",long(ls_null))
			il_existerepa = 2
			Return 1		
		End If	
		
End Choose

HabilitaPallet() 
end event

event dw_2::itemerror;call super::itemerror;RETURN 1
end event

event dw_2::clicked;
CHOOSE CASE dwo.name
		
	CASE "buscarepa"
		BuscaRepallet()

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_repalasigenca_caja_caja
integer x = 3323
integer y = 32
integer taborder = 60
long backcolor = 553648127
end type

event pb_nuevo::clicked;call super::clicked;ids_palletfrutahisto.Reset()
ids_palletencabhisto.Reset()
ids_alpalletfruta.Reset()
ids_alpalletencab.Reset()
ids_cuadracajas.Reset()
ids_palletfrutanuevo.Reset()
ids_palletencabnuevo.Reset()
ids_palletencabupdate.Reset()
ids_inspecpaldet.Reset()
dw_9.Reset()
dw_10.Reset()
il_existerepa = 2
pb_nvoporg.Enabled	=	False
pb_acepta.Enabled		=	False
pb_nvopdest.Enabled 	=  False
pb_recupera.Enabled  =  False
ii_nuinpe = 0
il_destino = 0
dw_3.Object.nuro_inpecc.Protect	=	1
dw_3.Object.nuro_inpecc.BackGround.Color = RGB(166,180,210)

end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3680
integer y = 1556
integer taborder = 100
boolean enabled = true
long backcolor = 553648127
end type

event pb_eliminar::clicked;call super::clicked;Parent.TriggerEvent("ue_borra_detalle")
end event

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_repalasigenca_caja_caja
integer x = 3323
integer y = 264
integer taborder = 110
long backcolor = 553648127
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_repalasigenca_caja_caja
integer x = 3323
integer y = 528
integer taborder = 120
long backcolor = 553648127
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_repalasigenca_caja_caja
integer x = 3328
integer y = 752
integer taborder = 130
long backcolor = 553648127
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3703
integer y = 1108
integer taborder = 150
long backcolor = 553648127
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3470
integer y = 1580
integer taborder = 160
long backcolor = 553648127
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3543
integer y = 1288
integer taborder = 140
boolean enabled = false
long backcolor = 553648127
end type

type pb_nvopdest from picturebutton within w_maed_spro_repalasigenca_caja_caja
string tag = "Nuevo Pallet Destino"
integer x = 3355
integer y = 968
integer width = 302
integer height = 244
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Regla1.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Regla1-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Integer li_repe_tipopa, li_totalcajas, li_tipopa, li_cliente
String ls_altura, ls_nula, ls_alturaori

li_repe_tipopa	= dw_2.Object.repe_tipopa[1]
ls_altura			= dw_3.Object.tpem_codigo[1]
ls_alturaori 		= dw_5.Object.tpem_codigo[1]

If dw_4.RowCount() > 0 Then
	li_totalcajas  = dw_4.Object.total_cajas[1]
	li_tipopa		= dw_3.Object.paen_tipopa[1]
	
	If li_tipopa = 1 Then
		If Integer(ls_altura) <> li_totalcajas Then
			MessageBox("Advertencia","Altura Destino no Cuadra con Total Cajas Pallet.")
			Return 1
		End If
	End If
Else
	Return 1
End If

If MessageBox("Atención", "Desea Asignar datos",Exclamation!, YesNO!, 2) = 1 Then
	dw_3.SetReDraw(False)
	If dw_6.RowCount() > 0 Then
		Asigna_palletnuevo()
	End If
	
	If li_repe_tipopa <> 1 AND li_repe_tipopa <> 2 AND li_repe_tipopa <> 7 Then
		Traspasa_Cajas_Nuevas()
	End If
	
	If li_repe_tipopa = 1 OR li_repe_tipopa = 2 OR li_repe_tipopa = 7 Then 
		traspasa_cajas_altura(1)
	End If
	
	ii_nuinpe = 0
	dw_3.SetReDraw(True)
Else
	ids_palletfrutahisto.Reset()
	ids_palletencabhisto.Reset()
	ids_alpalletfruta.Reset()
	ids_alpalletencab.Reset()
	ids_palletencabnuevo.Reset()
	ids_palletencabupdate.Reset()
	ids_palletfrutanuevo.Reset()
	ids_cuadracajas.Reset()
	dw_9.Reset()
	dw_10.Reset()
	dw_7.reset()
	dw_8.reset()
	dw_5.Reset()
	dw_1.Reset()
	il_existerepa = 2
	ii_nuinpe = 0
End If	

dw_3.Reset()
dw_3.SetRedraw(False)
li_cliente = Integer(istr_Mant.Argumento[1])
dw_3.GetChild("tpem_codigo", idwc_tipopaldest)
idwc_tipopaldest.SetTransObject(SQLCA)

//If idwc_tipopaldest.Retrieve(li_cliente,'Z') = 0 Then 
idwc_tipopaldest.InsertRow(0)

dw_3.InsertRow(0)

dw_3.SetRedraw(True)
dw_4.Reset()

dw_3.SetItem(1, "clie_codigo", Integer(istr_Mant.Argumento[1]))
dw_3.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))
dw_3.Object.paen_numero.Protect	=	0
dw_3.Object.paen_numero.Color		=	0
dw_3.Object.paen_numero.BackGround.Color	= RGB(255,255,255)

dw_3.Object.paen_tipopa.Protect		=	0
dw_3.Object.paen_tipopa.Color		= 	0
dw_3.Object.paen_tipopa.BackGround.Color		= RGB(255,255,255)

dw_5.Reset()
dw_5.SetRedraw(False)
dw_5.InsertRow(0)
dw_5.SetRedraw(True)
dw_6.Reset()
dw_5.SetItem(1, "clie_codigo", Integer(istr_Mant.Argumento[1]))
dw_5.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))

pb_grabar.Enabled = True

is_palletdest = ""
ii_nuinpe = 0

dw_3.SetColumn("paen_numero")
dw_3.SetFocus()

end event

type pb_nvoporg from picturebutton within w_maed_spro_repalasigenca_caja_caja
string tag = "Nuevo Pallet Origen"
integer x = 3337
integer y = 1424
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
string picturename = "\Desarrollo 17\Imagenes\Botones\Regla1.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Regla1-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Integer	li_totalcajas, li_tipopa
String 	ls_alturaori

IF dw_6.RowCount() > 0 THEN
	li_totalcajas  = dw_6.Object.total_cajas[1]
	li_tipopa		= dw_5.Object.paen_tipopa[1]
	ls_alturaori   = dw_5.Object.tpem_codigo[1]
	
	IF li_tipopa = 1 THEN
		IF Integer(ls_alturaori) <> li_totalcajas THEN
			messagebox("Error","Altura Origen no Cuadra con Total Cajas Pallet.")
			RETURN 1
		END IF
	END IF
END IF

dw_5.Reset()
dw_5.SetRedraw(False)
dw_5.InsertRow(0)
dw_5.SetRedraw(True)
dw_6.Reset()
dw_5.SetItem(1, "clie_codigo", Integer(istr_Mant.Argumento[1]))
dw_5.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))
dw_5.SetItem(1, "paen_tipopa", 2)
dw_5.Object.paen_numero.Protect				=	0
dw_5.Object.paen_numero.Color					=	0
dw_5.Object.paen_numero.BackGround.Color	= RGB(255,255,255)

is_palletorig = ""

dw_5.SetColumn("paen_numero")
dw_5.SetFocus()
end event

type pb_acepta from picturebutton within w_maed_spro_repalasigenca_caja_caja
string tag = "Traspasa"
integer x = 3333
integer y = 1160
integer width = 302
integer height = 244
integer taborder = 70
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Integer	li_CertIfica, li_cliente, li_planta, li_repe_tipopa, li_paen_tipopa, li_inspecnuevo,&
			li_inspeccion, li_tiporepa, li_totalcajas, li_tipopa, li_total_cajas, li_total_traspa, li_inpecnumero,&
			li_fumigacion, li_condicion, li_respuesta, li_condicion_ori, li_condicion_des, li_traspasa
String	ls_Tipopa, ls_tpem_codigo, ls_alturaori
Long		ll_Cajas, ll_NumeroRepal, ll_pallet, ll_paen_numero,ll_total_traspa, ll_total_cajas, ll_cont,&
			ll_numpro,ll_fila1, ll_nuevo1, fila_find, ll_numeroPall, contador, ll_coninpe, ll_numimpe, ll_cajexiste, ll_numcaja
			
dw_6.AcceptText()

li_tiporepa = dw_2.Object.repe_tipopa[1]

li_cliente = integer(istr_Mant.Argumento[1])
li_planta	= Integer(dw_2.Object.plde_codigo[1])
ll_pallet   = dw_3.Object.paen_numero[1]

If IsNull(dw_3.Object.cond_codigo[1]) Then
	li_condicion_des = 0
Else	
	li_condicion_des = dw_3.Object.cond_codigo[1]
End If

If IsNull(dw_5.Object.cond_codigo[1]) Then
	li_condicion_des = 0
Else	
	li_condicion_ori = dw_5.Object.cond_codigo[1]
End If

If li_condicion_ori <> li_condicion_des Then
	li_respuesta = MessageBox("Advertencia", "Esta Mezclando Condiciones, Desea Continuar", Exclamation!, YesNo!, 2)
	If li_respuesta = 2 Then
		Return 1
	End If	
End If	

If li_tiporepa <> 3 Then
	If dw_6.RowCount() > 0 Then
		li_totalcajas  = dw_6.Object.total_cajas[1] - dw_6.Object.total_traspa[1]
		li_tipopa		= dw_5.Object.paen_tipopa[1]
		ls_alturaori   = dw_5.Object.tpem_codigo[1]
		li_total_cajas = dw_6.Object.total_cajas[1]
		li_total_traspa= dw_6.Object.total_traspa[1]
		
		If li_tipopa = 1 Then
			If li_total_cajas <> li_total_traspa Then
				If Integer(ls_alturaori) <> li_totalcajas  Then
					MessageBox("Advertencia","Altura Origen no Cuadra con Total Cajas Pallet.")
					Return 1
				End If
			End If	
		End If
	End If
End If

li_paen_tipopa = dw_3.object.paen_tipopa[1]
If li_paen_tipopa = 1 Then

	ls_tpem_codigo = dw_3.object.tpem_codigo[1]
	If li_paen_tipopa = 1 and IsNull(ls_tpem_codigo) Then
		MessageBox("Atención", "Seleccione Tipo de pallet  destino")
		Return 1					  
	End If	
	
	ll_paen_numero = dw_3.Object.paen_numero[1]
	If IsNull(ll_paen_numero) OR ll_paen_numero = 0 Then
		MessageBox("Falta de Datos","Falta Número de Pallet en Destino.")
		Return 1
	End If
End If	

If NOT Integridad() Then Return 1

dw_3.Object.paen_numero.Protect	=	1
dw_3.Object.paen_numero.Color 		=	Rgb(255,255,255)
dw_3.Object.paen_numero.BackGround.Color = 553648127
					
SELECT count(*) 
	INTO :il_existe
	FROM dbo.palletencab  
	WHERE clie_codigo = :li_cliente  
	AND  paen_numero = :ll_Pallet 
	AND  plde_codigo = :li_planta;
		
li_inspecnuevo = dw_3.Object.paen_inspec[1]
li_inspeccion  = dw_5.Object.paen_inspec[1]
li_inpecnumero = dw_3.Object.nuro_inpecc[1]

FOR ll_cajexiste = 1 TO dw_6.RowCount()
	
	li_traspasa = dw_6.Object.caja_traspa[ll_cajexiste]
	ll_numcaja  = dw_6.Object.pafr_secuen[ll_cajexiste]
	
	If li_traspasa > 0 Then
		
		SELECT count(*) 
		INTO :ll_cont
		FROM dbo.palletfruta  
		WHERE clie_codigo = :li_cliente  
		AND paen_numero = :ll_Pallet 
		AND plde_codigo = :li_planta
		AND pafr_secuen = :ll_numcaja
		AND pafr_secuen > 9999;
	
		If ll_cont > 0 Then
			MessageBox("Atención","Caja "+String(ll_numcaja)+ " No se Puede Usar en Este Pallet, Porque ya Existio en él.")
			Return 1
		End If
	End If	
NEXT	

If li_tiporepa = 3 OR li_tiporepa = 7 OR li_tiporepa = 1 OR li_tiporepa = 2 Then
	If IsNull(li_inspecnuevo) Then
		If li_inspeccion = 0 Then
			dw_3.Object.paen_inspec[1] = 0
		Else	
			If dw_5.Object.paen_inspec[1] = 3 Then
				dw_3.Object.paen_inspec[1] = 0
			Else	
				dw_3.Object.paen_inspec[1] = dw_5.Object.paen_inspec[1]
			End If	
		End If	
	ElseIf li_inspecnuevo = 0 Then	
		dw_3.Object.paen_inspec[1] = 0
	Else		
		dw_3.Object.paen_inspec[1] = dw_5.Object.paen_inspec[1]
	End If
Else
	dw_3.Object.paen_inspec[1] = dw_5.Object.paen_inspec[1]
End If	

If dw_3.Object.paen_inspec[1] = 0 Then
	dw_3.Object.nuro_inpecc[1] = 0
	ids_inspecpaldet.Reset()
	dw_3.Object.nuro_inpecc.Protect	=	1
	dw_3.Object.nuro_inpecc.Color 	= RGB(255,255,255)
	dw_3.Object.nuro_inpecc.BackGround.Color = 553648127
Else
	dw_3.Object.nuro_inpecc.Protect	=	0
	dw_3.Object.nuro_inpecc.Color 	= 0
	dw_3.Object.nuro_inpecc.BackGround.Color = RGB(255,255,255)
	
	If IsNull(ii_nuinpe) OR ii_nuinpe = 0 Then
		MessageBox("Falta de Datos","Falta Número de Inspección.")
		If IsNull(dw_2.Object.repe_nrosag[1]) OR dw_2.Object.repe_nrosag[1] = 0 Then
   		MessageBox("Falta de Datos","Falta Número de SAG.")
		End If	
		Return 1
	Else	
		If IsNull(dw_2.Object.repe_nrosag[1]) OR dw_2.Object.repe_nrosag[1] = 0 Then
   		MessageBox("Falta de Datos","Falta Número de SAG.")
			Return 1
		End If	
		
		If li_tiporepa = 3 OR li_tiporepa  = 7 OR li_tiporepa  = 1 OR li_tiporepa  = 2 Then
			ll_numeroPall = dw_3.Object.paen_numero[1]
		End If
		
		SELECT count(*)
			INTO :contador
			FROM dbo.inspecpaldet
			WHERE paen_numero = :ll_numeroPall
			AND	inpe_numero = :ii_nuinpe
			AND	plde_codigo = :li_planta
			AND	clie_codigo = :li_cliente;
			
		If Contador = 0 Then
			fila_find =  ids_inspecpaldet.Find("paen_numero = " + String(ll_numeroPall),&
				1, ids_inspecpaldet.RowCount())
			
			If fila_find = 0 Then
				ll_nuevo1 = ids_inspecpaldet.InsertRow(0)
				
				ids_inspecpaldet.Object.clie_codigo[ll_nuevo1] =   dw_3.Object.clie_codigo[1]    
				ids_inspecpaldet.Object.paen_numero[ll_nuevo1] =   ll_numeroPall   
				ids_inspecpaldet.Object.plde_codigo[ll_nuevo1] =   dw_5.Object.plde_codigo[1]    
				ids_inspecpaldet.Object.inpe_tipoin[ll_nuevo1] =   1    
				ids_inspecpaldet.Object.inpe_numero[ll_nuevo1] =   ii_nuinpe    
				ids_inspecpaldet.Object.inpe_secuen[ll_nuevo1] =   90
				ids_inspecpaldet.Object.dest_codigo[ll_nuevo1] =   il_destino    
				ids_inspecpaldet.Object.inpd_fechai[ll_nuevo1] =   id_fechai  
				ids_inspecpaldet.Object.inpd_fechai[ll_nuevo1] =   id_fechai 
				
				SELECT count(*)
				INTO :ll_coninpe
				FROM dbo.inspecpalenc WHERE
					inpe_tipoin = 1 AND
					inpe_numero = :ii_nuinpe AND
					clie_codigo = :li_cliente AND
					plde_codigo = :li_planta AND
					inpe_secuen = 90;
				
				If ll_coninpe = 0 OR IsNull(ll_coninpe) Then
					INSERT INTO dbo.inspecpalenc(inpe_tipoin,inpe_numero,clie_codigo,plde_codigo,inpe_secuen,   
						dest_codigo,inpe_fechai,espe_codigo,vari_codigo,emba_codigo,   
						tpem_codigo,inpe_todpal,inpe_calibr,inpe_solnom,inpe_fecini,   
						inpe_fecter,inpe_proces,inpe_estado,tran_fechat,inpe_desdet,inpe_fecres,inpe_dessec)
					SELECT inpe_tipoin,inpe_numero,clie_codigo,plde_codigo,90,   
						dest_codigo,inpe_fechai,espe_codigo,vari_codigo,emba_codigo,   
						tpem_codigo,inpe_todpal,inpe_calibr,inpe_solnom,inpe_fecini,   
						inpe_fecter,inpe_proces,inpe_estado,tran_fechat,inpe_desdet,inpe_fecres,inpe_dessec 
					FROM dbo.inspecpalenc WHERE
						inpe_tipoin = 1 AND
						inpe_numero = :ii_nuinpe AND
						clie_codigo = :li_cliente AND
						plde_codigo = :li_planta AND
						inpe_secuen = 1;
				End If	
				
			End If
		End If 	
	End If	
End If

li_cliente = dw_2.Object.clie_codigo[1]

li_repe_tipopa = dw_2.Object.repe_tipopa[1]		
If li_repe_tipopa = 1 Then
	SELECT	IsNull(Max(pahi_proces), 0) + 1
		INTO	:ll_numpro
		FROM	dbo.palletencabhisto
		WHERE	plde_codigo	=	:li_Planta
		AND 	clie_codigo = :li_cliente
		AND	pahi_proces < 99999999;
	
	If NOT procesoaltura(ll_numpro) Then
		ll_numpro = ll_numpro +1
	End If
	
	grabahistoria_store(ll_numpro,0)	
	grabaalturas(ll_numpro,1)
End If

li_repe_tipopa = dw_2.Object.repe_tipopa[1]
If li_repe_tipopa = 1 Then
	SELECT	IsNull(Max(pahi_proces), 0) + 1
		INTO	:ll_numpro
		FROM	dbo.palletencabhisto
		WHERE	plde_codigo	=	:li_Planta
		AND 	clie_codigo = :li_cliente
		AND pahi_proces < 99999999;
	
	If NOT procesoaltura(ll_numpro) Then
		ll_numpro = ll_numpro +1
	End If
	grabahistoria_storealtura(ll_numpro,1)	
	grabaalturas(ll_numpro,0)
	dw_2.Object.repe_proces[1] = ll_numpro
End If	

li_repe_tipopa = dw_2.Object.repe_tipopa[1]
If li_repe_tipopa = 2 Then  
	SELECT	IsNull(Max(pahi_proces), 0) + 1
		INTO	:ll_numpro
		FROM	dbo.palletencabhisto
		WHERE	plde_codigo	=	:li_Planta
		AND 	clie_codigo = :li_cliente
		AND pahi_proces < 99999999;
	
	If NOT procesoaltura(ll_numpro) Then
		ll_numpro = ll_numpro +1
	End If	
	grabahistoria_store(ll_numpro,0)	
	grabaalturas(ll_numpro,1)
	dw_2.Object.repe_proces[1] = ll_numpro
End If

li_repe_tipopa = dw_2.Object.repe_tipopa[1]
If li_repe_tipopa = 2 Then
	SELECT	IsNull(Max(pahi_proces), 0) + 1
		INTO	:ll_numpro
		FROM	dbo.palletencabhisto
		WHERE	plde_codigo	=	:li_Planta
		AND clie_codigo = :li_cliente
		AND pahi_proces < 99999999;
	
	If NOT procesoaltura(ll_numpro) Then
		ll_numpro = ll_numpro +1
	End If
	grabahistoria_storealtura(ll_numpro,1)	
	grabaalturas(ll_numpro,0)	
	dw_2.Object.repe_proces[1] = ll_numpro
End If

li_repe_tipopa = dw_2.Object.repe_tipopa[1]
If dw_2.Object.repe_tipopa[1] = 3 Then  
	SELECT	IsNull(Max(pahi_proces), 0) + 1
		INTO	:ll_numpro
		FROM	dbo.palletencabhisto
		WHERE	plde_codigo	=	:li_Planta
		AND clie_codigo = :li_cliente
		AND pahi_proces < 99999999;
	
	If NOT procesoaltura(ll_numpro) Then
		ll_numpro = ll_numpro +1
	End If
	grabahistoria_store(ll_numpro,0)
	grabaalturas(ll_numpro,1)
	dw_2.Object.repe_proces[1] = ll_numpro
End If

li_repe_tipopa = dw_2.Object.repe_tipopa[1]
If li_repe_tipopa = 3 Then
	SELECT	IsNull(Max(pahi_proces), 0) + 1
		INTO	:ll_numpro
		FROM	dbo.palletencabhisto
		WHERE	plde_codigo	=	:li_Planta
		AND clie_codigo = :li_cliente
		AND pahi_proces < 99999999;
	
	If NOT procesoaltura(ll_numpro) Then
		ll_numpro = ll_numpro +1
	End If
	grabahistoria_storealtura(ll_numpro,1)	
	grabaalturas(ll_numpro,0)
	dw_2.Object.repe_proces[1] = ll_numpro
End If

li_repe_tipopa = dw_2.Object.repe_tipopa[1]
If li_repe_tipopa = 7 Then  
	SELECT	IsNull(Max(pahi_proces), 0) + 1
		INTO	:ll_numpro
		FROM	dbo.palletencabhisto
		WHERE	plde_codigo	=	:li_Planta
		AND clie_codigo = :li_cliente
		AND pahi_proces < 99999999;
	
	If NOT procesoaltura(ll_numpro) Then
		ll_numpro = ll_numpro +1
	End If
	grabahistoria_store(ll_numpro,0)	
	grabaalturas(ll_numpro,1)
	dw_2.Object.repe_proces[1] = ll_numpro
End If

li_repe_tipopa = dw_2.Object.repe_tipopa[1]
If li_repe_tipopa = 7 Then
	SELECT	IsNull(Max(pahi_proces), 0) + 1
		INTO	:ll_numpro
		FROM	dbo.palletencabhisto
		WHERE	plde_codigo	=	:li_Planta
		AND 	clie_codigo = :li_cliente
		AND pahi_proces < 99999999;
	
	If NOT procesoaltura(ll_numpro) Then
		ll_numpro = ll_numpro +1
	End If
	grabahistoria_storealtura(ll_numpro,1)
	grabaalturas(ll_numpro,0)
	dw_2.Object.repe_proces[1] = ll_numpro
End If

li_paen_tipopa = dw_3.object.paen_tipopa[1]
ls_tpem_codigo = dw_3.object.tpem_codigo[1]
If li_paen_tipopa = 1 and IsNull(ls_tpem_codigo) Then
	MessageBox("Atención", "Seleccione Tipo de pallet  destino")
	Return 1					  
End If	

ll_paen_numero = dw_3.Object.paen_numero[1]
If IsNull(ll_paen_numero) OR ll_paen_numero = 0 Then
   MessageBox("Falta de Datos","Falta Número de Pallet en Destino.")
   Return 1
End If

li_repe_tipopa = dw_2.Object.repe_tipopa[1]
If li_repe_tipopa <> 6 Then
	If dw_6.RowCount() >0 Then
		ll_total_traspa = dw_6.Object.total_traspa[dw_6.RowCount()]
		If ll_total_traspa >0 Then
			ll_paen_numero = dw_3.Object.paen_numero[1]
			If ExistePallet(ll_paen_numero, False) Then
				
			End If

			If Controla_TotalCajas(0) Then
				If dw_2.Object.repe_tipopa[1] = 3 Then
					cuadrar_cajas(1)
				End If	
				
				traspasa_cajaspalfruta_encab()
				Traspasa_Cajas(li_CertIfica)
				
				li_repe_tipopa = dw_2.Object.repe_tipopa[1]
				If li_repe_tipopa = 1 OR li_repe_tipopa = 2 OR li_repe_tipopa = 7 Then 
					traspasa_cajas_altura(1)
				End If
				
//				traspasa_cajaspalfruta_encab()
//				Traspasa_Cajas(li_CertIfica)
				pb_grabar.Enabled		=	True
				pb_nvoporg.Enabled	=  True
				pb_nvopdest.Enabled	=	False
				captura_total_cajas()
				dw_6.GroupCalc()
				ls_Tipopa	=	dw_5.Object.tpem_codigo[1]
			
				SELECT tpem_cancaj  
					INTO	 :ll_Cajas 
					FROM 	dbo.TIPOPALLEMBA  
					WHERE  clie_codigo = :li_cliente
					AND	 emba_codigo = :is_embalaje
					AND	 tpem_codigo = :ls_Tipopa;
			
				If dw_6.RowCount() > 0 Then 
					ll_total_cajas = dw_6.Object.total_cajas[1]
					If ll_total_cajas	< ll_Cajas Then
						dw_5.Object.paen_tipopa[1]	=	2
					End If
				Else
					dw_5.Object.paen_tipopa[1]	=	2
				End If
			Else
				MessageBox("Atención", "Total de cajas del detalle del pallet destino" + &
						  "~rno puede ser mayor a cajas total de pallet.")
			End If
		End If
	Else	
		MessageBox("Atención", "El pallet origen no posee detalle a traspasar. " + &
					  "~rElija un nuevo pallet origen.")
		Return 1			  
	End If	
End If	

li_repe_tipopa = dw_2.Object.repe_tipopa[1]
If li_repe_tipopa = 7 Then  
	dw_4.Object.pafr_calibr.Protect	=	0
	dw_4.Object.pafr_calrot.Protect	=	0
	dw_4.Object.emba_codigo.Protect	=	0	
	dw_4.Object.pafr_ccajas.Protect	=	0
Else
	dw_4.Object.pafr_calibr.Protect	=	1
	dw_4.Object.emba_codigo.Protect	=	1	
	dw_4.Object.pafr_ccajas.Protect	=	1
	dw_4.Object.pafr_calrot.Protect	=	1
End If

pb_grabar.Enabled = False
pb_nvopdest.Enabled = True
end event

type dw_4 from uo_dw within w_maed_spro_repalasigenca_caja_caja
string tag = "Detalle de Pallet Destino"
integer x = 5
integer y = 616
integer width = 3113
integer height = 520
integer taborder = 0
string title = "Detalle de Pallet Destino"
string dataobject = "dw_mues_palletfruta_repaletizaje"
boolean hscrollbar = true
boolean border = false
boolean livescroll = true
end type

event itemchanged;call super::itemchanged;String	ls_Columna, ls_null
Long 		ll_fila, ll_fila1, ll_fila2, ll_paen_nroori, ll_paen_numero, ll_pafr_secuen, ll_pafr_secuen1
Integer li_cliente

SetNull(ls_Null)
dw_4.AcceptText()
ids_palletencabupdate.AcceptText()

li_cliente = dw_2.Object.clie_codigo[1]

IF dw_2.Object.repe_tipopa[1] = 7 THEN
	ls_Columna = dwo.name
	CHOOSE CASE ls_Columna
			
		CASE "emba_codigo"
			
			MessageBox("Atención", "Verifique Tipo pallet.")
			
			IF NOT existeembalaje(data) THEN
				This.SetItem(Row, 'emba_codigo', String(ls_Null))
				Return 1
			END IF	
			
			dw_3.GetChild("tpem_codigo", idwc_tipopaldest)
			idwc_tipopaldest.SetTransObject(SqlCa)
			idwc_tipopaldest.Retrieve(li_cliente,Data)
			
			IF il_cantidad = 0 THEN
				FOR ll_fila1 = 1 TO ids_palletencabnuevo.RowCount()
					ll_paen_numero = ids_palletencabnuevo.Object.paen_numero[ll_fila1]
					ll_paen_nroori = dw_4.Object.paen_numero[Row]
					IF ll_paen_numero = ll_paen_nroori THEN
						ids_palletencabnuevo.Object.emba_codigo[ll_fila1] = data
					END IF
				NEXT
			END IF	
			
			FOR ll_fila1 = 1 TO ids_palletencabupdate.RowCount()
				ll_paen_numero = ids_palletencabupdate.Object.paen_numero[ll_fila1]
				ll_paen_nroori = dw_4.Object.paen_numero[Row]
				IF ll_paen_numero = ll_paen_nroori THEN
					ids_palletencabupdate.Object.emba_codigo[ll_fila1] = data
				END IF
			NEXT
			
			FOR ll_fila2 = 1 TO ids_palletfrutanuevo.RowCount()
				ll_paen_nroori = ids_palletfrutanuevo.Object.paen_numero[ll_fila2]
				ll_paen_numero = dw_4.Object.paen_numero[Row]
				ll_pafr_secuen = ids_palletfrutanuevo.Object.pafr_secuen[ll_fila2]
				ll_pafr_secuen1 = dw_4.Object.pafr_secuen[Row]
				IF ll_paen_nroori = ll_paen_numero AND &
					ll_pafr_secuen = ll_pafr_secuen1 THEN
					ids_palletfrutanuevo.Object.emba_codigo[ll_fila2] = data
				END IF
			NEXT
			
			dw_3.Object.emba_codigo[1] = Data 
			
			FOR ll_fila = 1 TO dw_1.RowCount()
				ll_paen_nroori = dw_1.Object.paen_numero[ll_fila]
				ll_paen_numero = dw_3.Object.paen_numero[1]
				IF ll_paen_nroori = ll_paen_numero THEN
					dw_1.Object.emba_codigo[ll_fila] = data
				END IF
			NEXT	
			
		CASE "pafr_calibr"	
			
			IF NOT existecalibre(data,row) THEN
				This.SetItem(Row, 'pafr_calibr', ls_Null)
				Return 1
			END IF	
			
			FOR ll_fila = 1 TO dw_1.RowCount()
				ll_paen_nroori = dw_1.Object.paen_numero[ll_fila]
				ll_paen_numero = dw_3.Object.paen_numero[1]
				IF ll_paen_nroori = ll_paen_numero THEN
					dw_1.Object.pafr_calibr[ll_fila] = dw_4.Object.pafr_calibr[Row] 
				END IF
			NEXT	
			
			FOR ll_fila2 = 1 TO ids_palletfrutanuevo.RowCount()
				ll_paen_nroori = ids_palletfrutanuevo.Object.paen_numero[ll_fila2]
				ll_paen_numero = dw_4.Object.paen_numero[Row]
				ll_pafr_secuen = ids_palletfrutanuevo.Object.pafr_secuen[ll_fila2]
				ll_pafr_secuen1 = dw_4.Object.pafr_secuen[Row]
				IF ll_paen_nroori = ll_paen_numero AND &
					ll_pafr_secuen = ll_pafr_secuen1 THEN
					ids_palletfrutanuevo.Object.pafr_calibr[ll_fila2] = data
				END IF
			NEXT
			
			CASE "pafr_calrot"
//				FOR ll_fila = 1 TO dw_1.RowCount()
//					ll_paen_nroori = dw_1.Object.paen_numero[ll_fila]
//					ll_paen_numero = dw_3.Object.paen_numero[1]
//					IF ll_paen_nroori = ll_paen_numero THEN
//						dw_1.Object.pafr_calrot[ll_fila] = dw_4.Object.pafr_calrot[Row]
//					END IF
//				NEXT	
			
				FOR ll_fila2 = 1 TO ids_palletfrutanuevo.RowCount()
					ll_paen_nroori = ids_palletfrutanuevo.Object.paen_numero[ll_fila2]
					ll_paen_numero = dw_4.Object.paen_numero[Row]
					ll_pafr_secuen = ids_palletfrutanuevo.Object.pafr_secuen[ll_fila2]
					ll_pafr_secuen1 = dw_4.Object.pafr_secuen[Row]
					IF ll_paen_nroori = ll_paen_numero AND &
						ll_pafr_secuen = ll_pafr_secuen1 THEN
						ids_palletfrutanuevo.Object.pafr_calrot[ll_fila2] = data
					END IF
				NEXT
				
		CASE "vari_codigo"
			
			IF variedadrotula(Integer(data),dw_4.Object.espe_codigo[row]) THEN
				dw_4.Object.pafr_varrot[row] = il_varirotu
			ELSE	
				dw_4.Object.pafr_varrot[row] = Integer(Data)
			END IF
			
			IF NOT existevariedad(Integer(data)) THEN
				This.SetItem(Row, 'vari_codigo', Integer(ls_Null))
				Return 1
			END IF	
			
			IF il_cantidad = 0 THEN
				FOR ll_fila1 = 1 TO ids_palletencabnuevo.RowCount()
					ll_paen_numero = ids_palletencabnuevo.Object.paen_numero[ll_fila1]
					ll_paen_nroori = dw_4.Object.paen_numero[Row]
					IF ll_paen_numero = ll_paen_nroori THEN
						ids_palletencabnuevo.Object.vari_codigo[ll_fila1] = Integer(data)
						ids_palletencabnuevo.Object.paen_varrot[ll_fila1] = il_varirotu
					END IF
				NEXT
			END IF	
			
			FOR ll_fila1 = 1 TO ids_palletencabupdate.RowCount()
				ll_paen_numero = ids_palletencabupdate.Object.paen_numero[ll_fila1]
				ll_paen_nroori = dw_4.Object.paen_numero[Row]
				IF ll_paen_numero = ll_paen_nroori THEN
					ids_palletencabupdate.Object.vari_codigo[ll_fila1] = Integer(data)
					ids_palletencabupdate.Object.paen_varrot[ll_fila1] = il_varirotu
				END IF
			NEXT
			
			FOR ll_fila = 1 TO dw_1.RowCount()
				ll_paen_nroori = dw_1.Object.paen_numero[ll_fila]
				ll_paen_numero = dw_3.Object.paen_numero[1]
				IF ll_paen_nroori = ll_paen_numero THEN
					dw_1.Object.vari_codigo[ll_fila] = dw_4.Object.vari_codigo[Row] 
					dw_1.Object.pafr_varrot[ll_fila] = il_varirotu 
					
				END IF
			NEXT	
			
			FOR ll_fila2 = 1 TO ids_palletfrutanuevo.RowCount()
				ll_paen_nroori = ids_palletfrutanuevo.Object.paen_numero[ll_fila2]
				ll_paen_numero = dw_4.Object.paen_numero[Row]
				ll_pafr_secuen = ids_palletfrutanuevo.Object.pafr_secuen[ll_fila2]
				ll_pafr_secuen1 = dw_4.Object.pafr_secuen[Row]
				IF ll_paen_nroori = ll_paen_numero AND &
					ll_pafr_secuen = ll_pafr_secuen1 THEN
					ids_palletfrutanuevo.Object.vari_codigo[ll_fila2] = Integer(data)
					ids_palletfrutanuevo.Object.pafr_varrot[ll_fila2] = il_varirotu
				END IF
			NEXT
			
			dw_3.Object.vari_codigo[1] = Integer(data) 
			dw_3.Object.paen_varrot[1] = il_varirotu
			
		CASE "pafr_ccajas"	
			dw_3.Object.paen_ccajas[1] = dw_4.Object.total_cajas[1] 
			
			FOR ll_fila = 1 TO dw_1.RowCount() 
				ll_paen_nroori = dw_1.Object.paen_numero[ll_fila]
				ll_paen_numero = dw_3.Object.paen_numero[1]
				IF ll_paen_nroori = ll_paen_numero THEN
					dw_1.Object.pafr_ccajas[ll_fila] = dw_4.Object.total_cajas[Row] 
					dw_1.Object.emba_codigo[ll_fila] = dw_3.Object.emba_codigo[1] 
					dw_1.Object.pafr_calibr[ll_fila] = dw_4.Object.pafr_calibr[Row] 
				END IF
			NEXT	
			
			FOR ll_fila1 = 1 TO ids_palletencabupdate.RowCount()
				ll_paen_numero = ids_palletencabupdate.Object.paen_numero[ll_fila1]
				ll_paen_nroori = dw_4.Object.paen_numero[Row]
				IF ll_paen_numero = ll_paen_nroori THEN
					ids_palletencabupdate.Object.paen_ccajas[ll_fila1] = dw_4.Object.total_cajas[Row] 
				END IF
			NEXT
			
			FOR ll_fila2 = 1 TO ids_palletfrutanuevo.RowCount()
				IF ids_palletfrutanuevo.Object.paen_numero[ll_fila2] = dw_4.Object.paen_numero[Row] AND &
					ids_palletfrutanuevo.Object.pafr_secuen[ll_fila2] = dw_4.Object.pafr_secuen[Row] THEN
					ids_palletfrutanuevo.Object.pafr_ccajas[ll_fila2] = Integer(data)
					ids_palletfrutanuevo.Object.pafr_cjssal[ll_fila2] = Integer(data)
				END IF
			NEXT
			
			IF il_cantidad = 0 THEN
				FOR ll_fila1 = 1 TO ids_palletencabnuevo.RowCount()
					IF ids_palletencabnuevo.Object.paen_numero[ll_fila1] = dw_4.Object.paen_numero[Row] THEN
						ids_palletencabnuevo.Object.paen_ccajas[ll_fila1] = dw_4.Object.total_cajas[Row]
					END IF
				NEXT
			ELSE
				FOR ll_fila1 = 1 TO ids_palletencabupdate.RowCount()
					IF ids_palletencabupdate.Object.paen_numero[ll_fila1] = dw_4.Object.paen_numero[Row] THEN
						ids_palletencabupdate.Object.paen_ccajas[ll_fila1] = dw_4.Object.total_cajas[Row] 
					END IF
				NEXT
			END IF
	
		CASE "pafr_varrot"
			
			IF ExisteVariRotula(Integer(data)) THEN
				This.SetItem(Row, 'pafr_varrot', Integer(ls_Null)) 
				Return 1
			END IF	
			
			FOR ll_fila2 = 1 TO ids_palletfrutanuevo.RowCount()
				ll_paen_nroori = ids_palletfrutanuevo.Object.paen_numero[ll_fila2]
				ll_paen_numero = dw_4.Object.paen_numero[Row]
				ll_pafr_secuen = ids_palletfrutanuevo.Object.pafr_secuen[ll_fila2]
				ll_pafr_secuen1 = dw_4.Object.pafr_secuen[Row]
				IF ll_paen_nroori = ll_paen_numero AND &
					ll_pafr_secuen = ll_pafr_secuen1 THEN
					ids_palletfrutanuevo.Object.pafr_varrot[ll_fila2] = Integer(data)
				END IF
			NEXT
		
		CASE "pafr_prdrot"
			IF existe_prodrotula(Long(data)) THEN
				This.SetItem(Row, 'pafr_prdrot', Long(ls_Null)) 
				Return 1
			END IF	
			
			FOR ll_fila2 = 1 TO ids_palletfrutanuevo.RowCount()
				ll_paen_nroori = ids_palletfrutanuevo.Object.paen_numero[ll_fila2]
				ll_paen_numero = dw_4.Object.paen_numero[Row]
				ll_pafr_secuen = ids_palletfrutanuevo.Object.pafr_secuen[ll_fila2]
				ll_pafr_secuen1 = dw_4.Object.pafr_secuen[Row]
				IF ll_paen_nroori = ll_paen_numero AND &
					ll_pafr_secuen = ll_pafr_secuen1 THEN
					ids_palletfrutanuevo.Object.pafr_prdrot[ll_fila2] = Long(data)
				END IF
			NEXT
				
	END CHOOSE
END IF	

//ls_Columna = dwo.name
//CHOOSE CASE ls_Columna
//
//	CASE "pafr_prdrot"
//		IF existe_prodrotula(Long(data)) THEN
//			This.SetItem(Row, 'pafr_prdrot', Long(ls_Null)) 
//			Return 1
//	END IF	
//				
//END CHOOSE
end event

event itemerror;call super::itemerror;Return 1 
end event

event losefocus;call super::losefocus;dw_4.AcceptText()
ids_palletencabupdate.AcceptText()
end event

type pb_cancela from picturebutton within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3333
integer y = 1264
integer width = 302
integer height = 244
integer taborder = 90
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Cancelar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Cancelar-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;
pb_nvoporg.Enabled	=  TRUE
pb_nvopdest.Enabled	=	TRUE
captura_total_cajas()
end event

type dw_7 from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 2715
integer y = 2448
integer width = 192
integer height = 132
integer taborder = 150
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_alpalletencab1"
end type

type dw_8 from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3287
integer y = 2192
integer width = 192
integer height = 132
integer taborder = 150
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_alpalletfruta1"
end type

type dw_9 from uo_dw within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3095
integer y = 2704
integer width = 192
integer height = 132
integer taborder = 11
boolean bringtotop = true
string title = "encabezado origen"
string dataobject = "dw_mant_palletencab_repaletizaje1"
boolean vscrollbar = false
borderstyle borderstyle = stylebox!
end type

type dw_10 from uo_dw within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 2903
integer y = 2448
integer width = 192
integer height = 132
integer taborder = 11
boolean bringtotop = true
string title = "detalle actualiza"
string dataobject = "dw_mues_palletfruta_repaletizaje_origen"
boolean vscrollbar = false
borderstyle borderstyle = stylebox!
end type

type ids_cuadracajas from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3099
integer y = 2320
integer width = 192
integer height = 132
integer taborder = 40
boolean bringtotop = true
string title = "cuadraCajas"
string dataobject = "dw_mues_palletfruta_repaletizaje_origen"
end type

type ids_palletencabhisto from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3287
integer y = 2444
integer width = 192
integer height = 132
integer taborder = 150
boolean bringtotop = true
string title = "encaba histo"
string dataobject = "dw_mant_palletencabhisto"
end type

type ids_palletfrutahisto from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3095
integer y = 2576
integer width = 192
integer height = 132
integer taborder = 160
boolean bringtotop = true
string title = "fruta histo"
string dataobject = "dw_mant_palletfrutahisto"
end type

type ids_palletfrutanuevo from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3287
integer y = 2576
integer width = 192
integer height = 132
integer taborder = 150
boolean bringtotop = true
string title = "palletfrutanuevo"
string dataobject = "dw_mues_palletfruta_repaletizaje2"
end type

type dw_5 from uo_dw within w_maed_spro_repalasigenca_caja_caja
string tag = "Encabezado Pallet Origen"
integer x = 5
integer y = 1144
integer width = 3113
integer height = 220
integer taborder = 30
string title = "Pallet Origen"
string dataobject = "dw_mant_palletencab_repaletizaje"
boolean vscrollbar = false
boolean border = false
end type

event itemchanged;String	ls_Columna, ls_Null, ls_Fecha, ls_Estado[3] = {'Existencia','Despachado','Repalletizado'},&
			ls_embalaje
Boolean  lb_detalle
integer 	li_cliente, li_planta, li_paen_tipopa, li_tiporepa, li_categoria
Long		ll_NumeroRepal, ll_paen_numero, ll_repe_nrosag, ll_fila
		
SetNull(ls_Null)
li_cliente 		= integer(istr_mant.argumento[1])
ls_Columna 		= dwo.name
li_planta 			= dw_2.Object.plde_codigo[1]

CHOOSE Case ls_Columna
	Case "paen_numero"
		If NOT isnull(dw_2.Object.repe_nrosag[1]) Then
			If pallet_nrosag(Long(data),dw_2.Object.repe_nrosag[1]) Then
				This.SetItem(1, ls_Columna, Long(ls_Null))
				Return 1	
			End If
		End If
		
		is_palletdest = ""
		is_palletorig = Mid(data,len(Data) - 6, 7)
		
		ll_paen_numero = dw_3.Object.paen_numero[1]
		If ll_paen_numero = Long(data) Then
			 	MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
						", fue ingresado como pallet destino. ~r~rIngrese otro número de pallet.")
			This.SetItem(1, ls_Columna, Long(ls_Null))
			istr_Mant.Argumento[5]	=	""
			Return 1
		Else
			dw_2.Object.clie_codigo.Protect	= 1
			dw_2.Object.clie_codigo.Color 		= RGB(255,255,255)
			dw_2.Object.clie_codigo.BackGround.Color = 553648127
			
			istr_Mant.Argumento[5]	=	Data
			ib_inspeccion_Origen 	=  False
			If ExistePallet(Long(istr_Mant.Argumento[5]),TRUE) Then
				If il_EstadoPallet = 3 Then
					MessageBox("Atención", "Pallet ya fue Repalletizado")	
					This.SetItem(Row, ls_Columna, Long(ls_Null))
					Return 1
				ElseIf il_EstadoPallet = 2 Then		
					MessageBox("Atención", "Pallet ya fue Despachado")	
					This.SetItem(Row, ls_Columna, Long(ls_Null))
					Return 1
				End If
				
				lb_detalle = existedetallepallet(Long(istr_Mant.Argumento[5]))
				
				If il_inspeccion >= 1 Then
					ib_inspeccion_Origen 	=  TRUE
				Else
					If ib_inspeccion_destino Then
						MessageBox("Atención", "Trabajará con un pallet NO inspeccionado")										
					End If	
				End If
				
				If il_inspeccion = 5 Then
					MessageBox("Atención", "El Pallet se Encuentra con Inspección PEndiente.")
					This.SetItem(Row, ls_Columna, Long(ls_Null))
					Return 1
				End If
				
				If il_inspeccion = 1 Then MessageBox("Atención", "El Pallet Ingresado se Encuentra Inspeccionado.")
				
				ll_repe_nrosag = dw_2.Object.repe_nrosag[1]
				If ll_repe_nrosag > 0 Then 
					If IsNull(il_inspeccion) Then il_inspeccion = 0
					If il_inspeccion = 0 Then
						MessageBox("Atención", "El pallet debe estar Inspeccionado.")
						This.SetItem(Row, ls_Columna, Long(ls_Null))
						Return 1
					End If
				End If
					
				If il_EstadoPallet  = 2 Then
					If lb_detalle Then
						MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
										", " + ls_Estado[il_EstadoPallet] + &
										"~r~rIngrese otro número de pallet")
					Else
						MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
										", No posee Detalle de Fruta."  + &
										"~r~rIngrese otro número de pallet")										
					End If					
					This.SetItem(1, ls_Columna, Long(ls_Null))
					istr_Mant.Argumento[5]	=	""
					Return 1
				ElseIf Atributos_Pallet() Then
					
					Parent.TriggerEvent("ue_recuperapallet_origen")
					
					ib_ExistePalletDestino = ExistePallet(dw_3.object.paen_numero[1],FALSE)
					Asigna_PalletNuevo()
					
					ls_embalaje = dw_5.object.emba_codigo[row]
					dw_5.GetChild("tpem_codigo", idwc_tipopaldest)
					idwc_tipopaldest.SetTransObject(SqlCa)
					idwc_tipopaldest.Retrieve(li_cliente,ls_embalaje)
					
					dw_5.GetChild("tpem_codigo", idwc_tipopalorig)
					idwc_tipopalorig.SetTransObject(SQLCA)
					If idwc_tipopalorig.Retrieve(li_cliente,ls_embalaje) = 0 Then
						idwc_tipopalorig.InsertRow(0)
					End If
					
					ls_embalaje = dw_5.object.emba_codigo[row]
					dw_3.GetChild("tpem_codigo", idwc_tipopaldest)
					idwc_tipopaldest.SetTransObject(SqlCa)
					idwc_tipopaldest.Retrieve(li_cliente,ls_embalaje)
													  
					This.Object.paen_numero.Protect	=	1
					This.Object.paen_numero.Color	= RGB(255,255,255)
					This.Object.paen_numero.BackGround.Color = 553648127
					li_paen_tipopa = dw_5.Object.paen_tipopa[1]
					li_tiporepa = dw_2.Object.repe_tipopa[1]
					
					If li_paen_tipopa = 1 Then
						dw_5.Object.paen_tipopa.Protect	=	0
						dw_5.Object.paen_tipopa.BackGround.Color = RGB(255,255,255)
						dw_5.Object.tpem_codigo.Protect	=	0
						dw_5.Object.tpem_codigo.BackGround.Color = RGB(255,255,255)
					Else
						dw_5.Object.paen_tipopa.Protect	=	1
						dw_5.Object.paen_tipopa.BackGround.Color = 553648127
						dw_5.Object.tpem_codigo.Protect	=	1
						dw_5.Object.tpem_codigo.BackGround.Color = 553648127
					End If	
					
					dw_3.SetFocus()
					dw_3.SetColumn("tpem_codigo")
				Else
					MessageBox("Atención", "Atributos pallet destino y pallet origenson distintos.~r~rIngrese otro número de pallet.")
					This.SetItem(1, ls_Columna, Long(ls_Null))
					istr_Mant.Argumento[5]	=	""					
					Return 1
				End If
			Else
				MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
								", no ha sido registrado.~r~rIngrese o seleccione otro número.")
				This.SetItem(1, ls_Columna, Long(ls_Null))
				istr_Mant.Argumento[5]	=	""				
				Return 1
			End If
 		End If
		 		 
		FOR ll_Fila = 1 TO dw_6.RowCount()	
			li_categoria	=	dw_6.Object.cate_codigo[ll_Fila]
			If dw_4.RowCount() > 0 Then
				If dw_4.Find('cate_codigo = ' + String(li_categoria), 1, dw_4.RowCount()) = 0  Then
					MessageBox('ATENCIÓN', 'El Pallet Seleccionado Tiene Categorías Distintas al Pallet Destino.')
					//Return 1
				End If		
			End If
		NEXT  
			
		FOR ll_fila = 1 TO dw_6.RowCount()
			If isnull(dw_6.Object.pafr_huert1[ll_fila])	OR isnull(dw_6.Object.pafr_cuart1[ll_fila]) Then
				MessageBox("Atención", "Pallet Con Predio o Cuartel Nulo.")
				dw_3.GetChild("nuro_inpecc", idwc_inspec)
				idwc_inspec.SetTransObject(SqlCa)
				idwc_inspec.Retrieve(li_cliente, li_planta)
				Return 1
			End If	
		NEXT	
		 
	 	If This.Rowcount() > 0 Then
			If il_caja > 9999 Then
				OpenWithParm(w_mant_deta_repa_agrupado, istr_mant)
			End If
	 	End If	
		 
		dw_3.GetChild("nuro_inpecc", idwc_inspec)
		idwc_inspec.SetTransObject(SqlCa)
		idwc_inspec.Retrieve(li_cliente, li_planta)
	
	Case "paen_tipopa"
		If data = "1" Then
			dw_5.Object.tpem_codigo.Protect	=	0
			dw_5.Object.tpem_codigo.Color 	= 0
			dw_5.Object.tpem_codigo.BackGround.Color = RGB(255,255,255)
		Else
			dw_5.Object.tpem_codigo[1] = ls_Null

			dw_5.Object.tpem_codigo.Protect	=	1
			dw_5.Object.tpem_codigo.Color	= Rgb(255,255,255)
			dw_5.Object.tpem_codigo.BackGround.Color = 553648127
		End If	
		
		dw_5.Object.paen_numero.Protect	=	0
		dw_5.Object.paen_numero.Color 		=	0
		dw_5.Object.paen_numero.BackGround.Color = RGB(255,255,255)
		dw_5.SetFocus()
	
	Case "tpem_codigo"
		ls_embalaje = dw_5.object.emba_codigo[row]
		If iuo_tipopallet.Existe(li_cliente,ls_embalaje, &
										 Data,True,SqlCa) Then
			This.SetItem(1, "tota_ccajas", iuo_tipopallet.Cajas)
			This.SetItem(1, "paen_altura", iuo_tipopallet.Altura)
		Else
			This.SetItem(1, ls_Columna, ls_Null)
			Return 1
		End If
		
End CHOOSE
end event

event itemerror;RETURN 1
end event

event losefocus;call super::losefocus;accepttext()

IF is_palletorig <> "" THEN
	is_palletorig	=	String(long(Mid( is_palletorig, 1, Len(is_palletorig) - 1)), "0000000") + &
					     Mid(is_palletorig, Len(is_palletorig))
	
	This.SetItem(il_fila, "paen_numero", long(is_palletorig))
END IF 	
end event

event itemfocuschanged;call super::itemfocuschanged;IF dwo.Name = "paen_numero" THEN
	This.Object.paen_numero.EditMask.Mask = "#######"
END IF

end event

event clicked;call super::clicked;CHOOSE CASE dwo.name
		
	CASE "buscapallet"
		IF dw_5.Object.paen_numero.Protect = "1"  THEN RETURN 1
		buscapalletorigen()
		
END CHOOSE
end event

type ids_palletencabupdate from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 2907
integer y = 2320
integer width = 192
integer height = 132
integer taborder = 170
boolean bringtotop = true
string title = "Actualiza encabezado"
string dataobject = "dw_mant_palletencab_repaletizaje2"
end type

type ids_alpalletfruta from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 2715
integer y = 2704
integer width = 192
integer height = 132
integer taborder = 140
boolean bringtotop = true
string title = "alpalletfruta"
string dataobject = "dw_mues_alpalletfruta1"
end type

type ids_alpalletencab from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 2715
integer y = 2576
integer width = 192
integer height = 132
integer taborder = 90
boolean bringtotop = true
string title = "allpalletencab"
string dataobject = "dw_mant_alpalletencab1"
end type

type ids_inspecpaldet from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 2903
integer y = 2576
integer width = 192
integer height = 132
integer taborder = 40
boolean bringtotop = true
string title = "inspecpaldet"
string dataobject = "dw_mues_inspecpaldet"
end type

type ids_despafrigoen from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3287
integer y = 2704
integer width = 192
integer height = 132
integer taborder = 160
boolean bringtotop = true
string title = "despachoenca"
string dataobject = "dw_mant_despafrigoen_repa"
end type

type ids_despafrigodet from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 2903
integer y = 2704
integer width = 192
integer height = 132
integer taborder = 170
boolean bringtotop = true
string title = "despachodeta"
string dataobject = "dw_mues_despafrigode"
end type

type ids_palletencabhisto_des from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 2907
integer y = 2192
integer width = 192
integer height = 132
integer taborder = 180
boolean bringtotop = true
string title = "historiaenca"
string dataobject = "dw_mant_palletencabhisto"
end type

type ids_palletfrutahisto_des from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3095
integer y = 2448
integer width = 192
integer height = 132
integer taborder = 190
boolean bringtotop = true
string title = "historiadeta"
string dataobject = "dw_mant_palletfrutahisto"
end type

type ids_recepcionenca from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 2715
integer y = 2320
integer width = 192
integer height = 132
integer taborder = 50
boolean bringtotop = true
string title = "Rececepcionenca"
string dataobject = "dw_mant_recepcionenca_repa"
end type

type ids_recepciondeta from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 2715
integer y = 2192
integer width = 192
integer height = 132
integer taborder = 170
boolean bringtotop = true
string title = "recepciondeta"
string dataobject = "dw_mant_recepciondeta_repa"
end type

type ids_palletencabnuevo from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3090
integer y = 2180
integer width = 192
integer height = 132
integer taborder = 140
boolean bringtotop = true
string title = "Pallet Nuevo"
string dataobject = "dw_mant_palletencab_repaletizaje2"
end type

type pb_recupera from picturebutton within w_maed_spro_repalasigenca_caja_caja
string tag = "Compactos"
boolean visible = false
integer x = 3653
integer y = 984
integer width = 302
integer height = 244
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\apuntes.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\apuntes-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Integer		li_Reimprime, li_cliente, li_planta, li_especie, li_variedad, li_etiqueta, li_condicion,&
				li_linea, li_mercado, li_imprime
Long			ll_fila, ll_filadet, ll_Ncajas, ll_pallet, ll_productor, ll_secuencia, ll_Fila1, ll_cajas, ll_ncaja, ll_filnew,&
				ll_inicial, ll_final, ll_cont, ll_prodrot, ll_fill, ll_nrocaja, gl_packing
String		ls_calibre, ls_embalaje, ls_dw, ls_fecha
Boolean		lb_AutoCommit, lb_Retorno


li_Reimprime	=	MessageBox("Atención","Desea Imprimir Adhesivos Compactos?",Exclamation!, OKCancel!, 2)

dw_11.SetTransObject(sqlca)
dw_12.SetTransObject(sqlca)

IF li_Reimprime <> 1 THEN Return

iuo_correl	=	Create uo_lotescorrelequipo

il_NroCaja = 0

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", &
				"ComputerName", RegString!,is_Computador)
				
dw_compacto.SetTransObject(sqlca)
dw_spro_cajasprod.SetTransObject(sqlca)

OpenWithParm(w_info_compactorepa, istr_mant)

istr_mant	       = Message.PowerObjectParm

li_mercado = Integer(istr_mant.argumento[17])

IF li_mercado = -1 THEN Return

iuo_correl					=	Create	uo_lotescorrelequipo

li_cliente = dw_2.Object.clie_codigo[1]
li_planta  = dw_2.Object.plde_codigo[1]
ii_planta  = li_planta
ll_pallet  = dw_3.Object.paen_numero[1]

SELECT loco_dwcomp
INTO	:ls_dw
FROM dbo.spro_correlcompequipo
WHERE plde_codigo = :li_planta
AND	equi_nombre = :is_Computador;

dw_compacto.DataObject = ls_dw
dw_compacto.SetTransObject(sqlca)

SELECT Count(*),max(capr_numero)
INTO :ll_cont,:ll_final
FROM dbo.spro_cajasprod
WHERE clie_codigo = :li_cliente
AND	plde_codigo = :li_planta
AND	capr_numpal = :ll_pallet;

SELECT Count(*),min(capr_numero)
INTO :ll_cont,:ll_inicial
FROM dbo.spro_cajasprod
WHERE clie_codigo = :li_cliente
AND	plde_codigo = :li_planta
AND	capr_numpal = :ll_pallet;

dw_11.Retrieve(li_cliente,ll_pallet,li_planta)

IF ll_cont = 0 THEN

	IF li_Reimprime = 1 THEN
		IF BuscaNuevoCorrelativo(li_planta,li_cliente) = True THEN
		END IF
	END IF
	
	ll_inicial = iuo_correl.il_correcompa
	
	ll_ncaja = iuo_correl.il_correcompa
		
	IF li_Reimprime = 1 THEN
		FOR ll_filadet = 1 TO dw_11.RowCount()
			FOR 	ll_fila = 1 TO dw_11.Object.pafr_ccajas[ll_filadet]
				
				li_cliente 	 = dw_11.Object.clie_codigo[ll_filadet]
				ll_pallet	 = dw_11.Object.paen_numero[ll_filadet]
				li_especie	 = dw_11.Object.espe_codigo[ll_filadet]
				li_variedad  = dw_11.Object.vari_codigo[ll_filadet]
				ls_embalaje  = dw_11.Object.emba_codigo[ll_filadet]
				ll_productor = dw_11.Object.prod_codigo[ll_filadet]
				li_condicion = dw_11.Object.cond_codigo[ll_filadet]
				li_etiqueta  = dw_11.Object.etiq_codigo[ll_filadet]
				li_planta	 = dw_11.Object.plde_codigo[ll_filadet]
				ls_calibre   = dw_11.Object.pafr_calibr[ll_filadet]
				ll_secuencia = dw_11.Object.pafr_secuen[ll_filadet]
				ll_prodrot 	 = dw_11.Object.pafr_prdrot[ll_filadet]
				
				ll_filnew = dw_spro_cajasprod.InsertRow(0)
				
				dw_spro_cajasprod.Object.clie_codigo[ll_filnew] = li_cliente  
				dw_spro_cajasprod.Object.plde_codigo[ll_filnew] = li_planta  
				dw_spro_cajasprod.Object.capr_numero[ll_filnew] = ll_ncaja   
				dw_spro_cajasprod.Object.espe_codigo[ll_filnew] = li_especie  
				dw_spro_cajasprod.Object.vari_codigo[ll_filnew] = li_variedad  
				dw_spro_cajasprod.Object.prod_codigo[ll_filnew] = ll_productor  
				dw_spro_cajasprod.Object.prod_predio[ll_filnew] = dw_11.Object.pafr_huert1[ll_filadet]  
				dw_spro_cajasprod.Object.prod_huerto[ll_filnew] = dw_11.Object.pafr_huert1[ll_filadet]  
				dw_spro_cajasprod.Object.prod_cuarte[ll_filnew] = dw_11.Object.pafr_cuart1[ll_filadet]  
				dw_spro_cajasprod.Object.emba_codigo[ll_filnew] = ls_embalaje  
				dw_spro_cajasprod.Object.etiq_codigo[ll_filnew] = li_etiqueta  
				dw_spro_cajasprod.Object.capr_fecemb[ll_filnew] = dw_11.Object.pafr_fecemb[ll_filadet]  
				dw_spro_cajasprod.Object.capr_calibr[ll_filnew] = ls_calibre  
				dw_spro_cajasprod.Object.capr_numpal[ll_filnew] = ll_pallet  
				dw_spro_cajasprod.Object.capr_estado[ll_filnew] = 1  
				dw_spro_cajasprod.Object.capr_varrot[ll_filnew] = dw_11.Object.pafr_varrot[ll_filadet]  
				dw_spro_cajasprod.Object.cate_codigo[ll_filnew] = dw_11.Object.cate_codigo[ll_filadet]  
				dw_spro_cajasprod.Object.capr_cespak[ll_filnew] = dw_11.Object.pafr_copack[ll_filadet] 
				dw_spro_cajasprod.Object.capr_docrel[ll_filnew] = dw_11.Object.pafr_docrel[ll_filadet] 
				dw_spro_cajasprod.Object.capr_hordig[ll_filnew] = Now()  
				dw_spro_cajasprod.Object.capr_fecdig[ll_filnew] = Today()  
				//dw_spro_cajasprod.Object.capr_nrlote[ll_filnew] = dw_11.Object.pafr_nrlote[ll_filadet]
				dw_spro_cajasprod.Object.capr_lineas[ll_filnew] = iuo_correl.loco_comlin  
				dw_spro_cajasprod.Object.capr_pcline[ll_filnew] = is_computador
				dw_spro_cajasprod.Object.capr_prdrot[ll_filnew] = ll_prodrot
				dw_spro_cajasprod.Object.cate_codigo[ll_filnew] = dw_11.Object.cate_codigo[ll_filadet] 
				dw_spro_cajasprod.Object.capr_catrot[ll_filnew] = dw_11.Object.pafr_catrot[ll_filadet] 
				
				lb_autocommit = SQLCA.Autocommit
				SQLCA.Autocommit = FALSE
				
				IF dw_spro_cajasprod.Update(True, False) = 1 THEN
					Commit;
						
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, 'Compacto')
						RollBack;
						Return
					ELSE								
						dw_spro_cajasprod.ResetUpdate()
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, 'Compacto')
					RollBack;
					Return
				END IF
				ll_Ncaja ++
			NEXT	
		NEXT
			
		ll_final = ll_ncaja - 1
		ll_Fila1 = dw_12.Retrieve(li_cliente,li_planta,ll_inicial,ll_final,li_mercado)
		
		IF ll_Fila1 = -1 THEN
			MessageBox( "Error en Base de Datos", &
							"Se ha producido un error en Base " + &
							"de Datos : ~n" + sqlca.SQLErrText, StopSign!, OK!)
		ELSEIF ll_Fila1 = 0 THEN
			MessageBox( "No Existe información", "No Existe información para este informe.",StopSign!, OK!)
		END IF
				
		FOR ll_fill = 1 TO ll_Fila1
			ll_nrocaja = dw_12.Object.capr_numero[ll_fill]
			gl_packing = dw_12.Object.plde_codigo[ll_fill]
			
			dw_compacto.Retrieve(li_cliente,li_planta,ll_nrocaja,ll_nrocaja,li_mercado)
			
			dw_compacto.Object.Ole_1.Object.BarCode	=	Integer(dw_11.Object.clie_codbor[1])
			IF dw_compacto.Object.Ole_1.Object.BarCode = 20 THEN
				dw_compacto.Object.Ole_1.Object.Text 	= 	'00' + &
																String(dw_11.Object.zona_codigo[1],'00') + &
																String(dw_11.Object.plde_codigo[1],'0000') + &
																String(dw_11.Object.capr_numero[1],'0000000000')
			ELSEIF dw_compacto.Object.Ole_1.Object.BarCode = 88 THEN
				ls_fecha									=	String(dw_11.Object.pafr_fecemb[1])
				ls_fecha									=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)
				dw_compacto.Object.Ole_1.Object.Text 	= 	"01" + dw_11.Object.emba_nrogs1[1] + "10" + ls_fecha + "\F" + &
																"21" + String(gl_packing, "0000") +  &
																String(ll_nrocaja, '00000000')
			END IF
			dw_compacto.AcceptText()
			dw_compacto.Print()
		NEXT	
	END IF
ELSE
	ll_Fila1 = dw_12.Retrieve(li_cliente,li_planta,ll_inicial,ll_final,li_mercado)
		
	IF ll_Fila1 = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de Datos : ~n" + sqlca.SQLErrText, StopSign!, OK!)
	ELSEIF ll_Fila1 = 0 THEN
		MessageBox( "No Existe información", "No Existe información para este informe.", StopSign!, OK!)
	END IF
			
	FOR ll_fill = 1 TO ll_Fila1
		
		ll_nrocaja = dw_12.Object.capr_numero[ll_fill]
		gl_packing = dw_12.Object.plde_codigo[ll_fill]
		
		dw_compacto.Retrieve(li_cliente,li_planta,ll_nrocaja,ll_nrocaja,li_mercado)
		
		dw_compacto.Object.Ole_1.Object.BarCode	=	Integer(dw_11.Object.clie_codbor[1])
		IF dw_compacto.Object.Ole_1.Object.BarCode = 20 THEN
			dw_compacto.Object.Ole_1.Object.Text 	= 	'00' + &
															String(dw_11.Object.zona_codigo[1],'00') + &
															String(dw_11.Object.plde_codigo[1],'0000') + &
															String(dw_11.Object.capr_numero[1],'0000000000')
		ELSEIF dw_compacto.Object.Ole_1.Object.BarCode = 88 THEN
		   ls_fecha									=	String(dw_11.Object.pafr_fecemb[1])
			ls_fecha									=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)
			dw_compacto.Object.Ole_1.Object.Text 	= 	"01" + dw_11.Object.emba_nrogs1[1] + "10" + ls_fecha + "\F" + &
															"21" + String(gl_packing, "0000") + '0' + &
															String(ll_nrocaja, '0000000')
		END IF
		dw_compacto.AcceptText()
		dw_compacto.Print()
	NEXT
END IF	
end event

type dw_compacto from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3858
integer y = 2300
integer width = 192
integer height = 132
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod_uvas_prod"
end type

type dw_3 from uo_dw within w_maed_spro_repalasigenca_caja_caja
string tag = "Encabezado Pallet Destino"
integer x = 5
integer y = 384
integer width = 3113
integer height = 228
integer taborder = 20
string title = "Pallet Destino"
string dataobject = "dw_mant_palletencab_repaletizaje_des"
boolean vscrollbar = false
boolean border = false
end type

event itemchanged;String		ls_Columna, ls_Null, ls_Estado[3] = {'Existencia','Despachado','Repalletizado'}, ls_embalaje
integer 	li_Cliente, li_Planta, li_repe_tipopa, li_tipopa
long		ll_NumeroRepal, ll_paen_numero, ll_repe_nrosag, ll_numero, ll_find, ll_pallet

SetNull(ls_Null)

li_cliente 	= Integer(istr_mant.argumento[1])
ls_Columna	= dwo.name

li_planta = dw_2.Object.plde_codigo[1]

Choose Case ls_Columna
	Case "paen_numero"
		If Not ExistePallet(Long(Data), False) Then
			If Not IsNull(This.Object.paen_tipopa[Row]) Then
				If Integer(istr_Mant.Argumento[1]) = 81 Then
					//iuo_Clientes.Genera_Pallet = 1 or iuo_Clientes.Genera_Pucho = 1
					If Not iuo_ControlVentanas.ValidaRango(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), &
							Long(Data), This.Object.paen_tipopa[Row], True, Sqlca) Then
						This.SetItem(Row, ls_Columna, Integer(ls_Null))
						Return 1
					End If
				End If
			End If
		End If
		
		If Not IsNull(dw_2.Object.repe_nrosag[1]) Then
			If pallet_nrosag(Long(data),dw_2.Object.repe_nrosag[1]) Then
				This.SetItem(1, ls_Columna, Long(ls_Null))
				Return 1	
			End If	
		End If
				
		If Existe_Transitorio(Long(data)) Then
			This.SetItem(1, ls_Columna, Long(ls_Null))
			Return 1
		End If	
		
		ll_pallet = Long(data)
		ll_find	=	ids_palletencabupdate.Find ( "paen_numero = " + String(ll_pallet),1, ids_palletencabupdate.RowCount())
		
		If ll_find > 0 Then
			 MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
							", Ya fue Constituido en este Repalletizado . " + &
							"~r~rIngrese otro número de pallet.")
			This.SetItem(1, ls_Columna, Long(ls_Null))
			istr_Mant.Argumento[4]	=	""
			Return 1
		End If					
		
		ll_find	=	ids_palletencabnuevo.Find ( "paen_numero = " + String(ll_pallet),1, ids_palletencabnuevo.RowCount())
		
		If ll_find > 0 Then
			 MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
							", Ya fue Constituido en este Repalletizado . " + &
							"~r~rIngrese otro número de pallet.")
			This.SetItem(1, ls_Columna, Long(ls_Null))
			istr_Mant.Argumento[4]	=	""
			Return 1
		End If		
			
		ib_Detalle = TRUE
		is_palletdest = ""
		
		ll_paen_numero = dw_5.Object.paen_numero[1]
		
		If ll_paen_numero = Long(data) Then
		   MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
							", fue ingresado como pallet origen. " + &
							"~r~rIngrese otro número de pallet.")
			This.SetItem(1, ls_Columna, Long(ls_Null))
			istr_Mant.Argumento[4]	=	""
			Return 1
		Else
			dw_2.Object.clie_codigo.Protect 				= 1
			dw_2.Object.clie_codigo.Color 					= RGB(255,255,255)
			dw_2.Object.clie_codigo.BackGround.Color = 553648127
			
			istr_Mant.Argumento[4]	=	Data
			ib_inspeccion_destino   =  FALSE
			If ExistePallet(Long(istr_Mant.Argumento[4]),False) Then
				
				ib_detalle = existedetallepallet(Long(istr_Mant.Argumento[4]))
				
				li_repe_tipopa = dw_2.Object.repe_tipopa[1]
				If li_repe_tipopa = 6 AND ib_detalle Then
					
					MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
							", ya se encuentra ingresado. Para Cambio de Folio, el pallet destino debe ser nuevo. " + &
							"~r~rIngrese otro número de pallet.")
					This.SetItem(1, ls_Columna, Long(ls_Null))
					istr_Mant.Argumento[4]	=	""
					Return 1
				End If	
				
				If il_EstadoPallet > 1 AND ib_detalle Then
					MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
									", " + ls_Estado[il_EstadoPallet] + &
									"~r~rIngrese otro número de pallet.")
					This.SetItem(1, ls_Columna, Long(ls_Null))
					istr_Mant.Argumento[4]	=	""
					Return 1
				End If
				
				If il_inspeccion = 5 Then
					MessageBox("Atención", "El Pallet se Encuentra con Inspección PEndiente.")
						This.SetItem(Row, ls_Columna, Long(ls_Null))
						Return 1
				End If
				
				If il_inspeccion >= 1 Then
					ib_inspeccion_destino =  TRUE
					MessageBox("Atención", "Trabajará con un pallet Inspeccionado.")
				End If	
				
				ll_repe_nrosag = dw_2.Object.repe_nrosag[1]
				If ll_repe_nrosag > 0 Then
					If IsNull(il_inspeccion) Then il_inspeccion = 0
					If il_inspeccion = 0 Then
						MessageBox("Atención", "El pallet debe estar Inspeccionado.")
						This.SetItem(Row, ls_Columna, Long(ls_Null))
						Return 1
					End If
				End If
				
				li_repe_tipopa = dw_2.Object.repe_tipopa[1]
				
				If li_repe_tipopa = 3 Then
					MessageBox("Atención", "El pallet debe ser nuevo.")
					This.SetItem(Row, ls_Columna, Long(ls_Null))
					Return 1
				End If		
				
				Parent.TriggerEvent("ue_recuperapallet_destino")				
				
				dw_3.Object.paen_estado[1]			=	1
				is_marca = 'M'
				
				li_tipopa = dw_3.Object.paen_tipopa[1] 
				
				If li_tipopa = 1 Then 
					dw_3.Object.tpem_codigo.Protect	=	0
					dw_3.Object.tpem_codigo.Color 	= 	0
					dw_3.Object.tpem_codigo.BackGround.Color = RGB(255,255,255)
				End If	
				
				dw_5.Object.paen_numero.Protect	=	0
				dw_5.Object.paen_numero.Color 		= 	0
				dw_5.Object.paen_numero.BackGround.Color = RGB(255,255,255)
				
				dw_5.Object.paen_tipopa.Protect	=	0
				dw_5.Object.paen_tipopa.Color 	= 	0
				dw_5.Object.paen_tipopa.BackGround.Color = RGB(255,255,255)
				
				is_palletdest = Mid(data,len(Data) - 6, 7)
				
				If ib_detalle Then
					ib_ExistePalletDestino	=	True
				Else
					ib_ExistePalletDestino	=	FALSe
				End If	
				
				dw_5.SetColumn("paen_numero")
				dw_5.SetFocus()
				
				ll_numero = dw_3.Object.paen_numero[1]
				
				SELECT Distinct max(inpe_numero)
					INTO :ii_nuinpe
					FROM dbo.inspecpaldet
					WHERE clie_codigo = :li_cliente
					AND plde_codigo = :li_planta
					AND paen_numero = :ll_numero
					AND IsNull(inpd_nroanu,0) = 0
					AND IsNull(inpd_frecha,'19000101') ='19000101';
					 
				If sqlca.SQLCode = -1 Then F_ErrorBaseDatos(sqlca, "Lectura de Tabla inspecpaldet")
				If ii_nuinpe <> 0 Then dw_3.Object.nuro_inpecc[1] = ii_nuinpe				
			Else
				ib_ExistePalletDestino	=	False
				is_marca = 'A'
				dw_4.Reset()
				dw_3.SetItem(1, "clie_codigo", Integer(istr_Mant.Argumento[1]))
			    This.SetItem(1, ls_Columna, Long(istr_Mant.Argumento[4]))
				pb_nvopdest.Enabled	=	True
				is_palletdest = Mid(data,len(Data) - 7, 8)						
			End If
		End If
		
	Case "tpem_codigo"
		ls_embalaje = dw_5.Object.emba_codigo[1]
		If IsNull(ls_embalaje) Then ls_embalaje = dw_3.Object.emba_codigo[1]
		If iuo_tipopallet.Existe(li_cliente,ls_embalaje, Data,True,SqlCa) Then
       
			This.SetItem(1, "tota_ccajas", iuo_tipopallet.Cajas)
			This.SetItem(1, "paen_altura", iuo_tipopallet.Altura)
		Else
			This.SetItem(1, ls_Columna, ls_Null)
			Return 1
		End If
		
	Case "paen_tipopa"
		If Not ExistePallet(This.Object.paen_numero[Row], False) Then
			If Not IsNull(This.Object.paen_numero[Row]) Then 
				If Integer(istr_Mant.Argumento[1]) = 81 Then
				//iuo_Clientes.Genera_Pallet = 1 or iuo_Clientes.Genera_Pucho = 1
					If Not iuo_ControlVentanas.ValidaRango(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), &
							This.Object.paen_numero[Row], Integer(Data), True, Sqlca) Then
						This.SetItem(Row, ls_Columna, Integer(ls_Null))
						Return 1
					End If
				End If
			End If
		End If
		
		If data = "1" Then
			dw_3.Object.tpem_codigo.Protect	=	0
			dw_3.Object.tpem_codigo.Color 	=	0
			dw_3.Object.tpem_codigo.BackGround.Color = RGB(255,255,255)
		Else
			dw_3.Object.tpem_codigo[1] = ls_Null

			dw_3.Object.tpem_codigo.Protect	=	1
			dw_3.Object.tpem_codigo.BackGround.Color = RGB(255,255,255)
			dw_3.Object.tpem_codigo.BackGround.Color = 553648127
		End If	
		
		pb_nvopdest.Enabled	=	True
		
		dw_5.Object.paen_numero.Protect	=	0
		dw_5.Object.paen_numero.Color 		=	0
		dw_5.Object.paen_numero.BackGround.Color = RGB(255,255,255)
		dw_5.SetFocus()
		
	Case "nuro_inpecc"
		datosinpeccion(Long(data)) 
		
End Choose
end event

event losefocus;call super::losefocus;accepttext()

IF is_palletdest <> "" THEN
	is_palletdest	=	String(long(Mid( is_palletdest, 1, Len(is_palletdest) - 1)), "0000000") + &
					      Mid(is_palletdest, Len(is_palletdest))
	
	this.SetItem(il_fila, "paen_numero", long(is_palletdest))
END IF 	
end event

event itemfocuschanged;call super::itemfocuschanged;IF dwo.Name = "paen_numero" THEN
	This.Object.paen_numero.EditMask.Mask = "#######"
END IF

end event

event clicked;call super::clicked;

CHOOSE CASE dwo.name
		
	CASE "buscapallet"
		IF dw_3.Object.paen_numero.Protect = "1"  THEN RETURN 1
		Buscapallets()
		
END CHOOSE
end event

event itemerror;RETURN 1
end event

event buttonclicked;call super::buttonclicked;String	ls_Columna
Integer	li_tipopal, li_cliente, li_planta
Long		ll_pallet, ll_pallet2

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna	
	CASE "b_anterior"
		IF il_cambiafila > 1 THEN
			il_cambiafila --
		ELSE
			il_cambiafila = 1
		END IF
		
		li_tipopal 	= dw_2.Object.repe_tipopa[1]
		
		IF dw_2.Object.repe_tipopa[1] = 3 OR dw_2.Object.repe_tipopa[1] = 7 THEN
			dw_1.SetSort("paen_numero Asc")
			dw_1.Sort()
			ll_pallet	= dw_1.Object.paen_numero[il_cambiafila]	
		ELSE
			dw_1.SetSort("paen_nroori Asc")
			dw_1.Sort()
			ll_pallet	= dw_1.object.paen_nroori[il_cambiafila]
		END IF	
				
		li_cliente	= dw_1.Object.clie_codigo[il_cambiafila]	
		li_planta	= dw_1.Object.plde_codigo[il_cambiafila]
		
		IF il_palcompac <> ll_pallet THEN 
			dw_4.Retrieve(li_cliente,li_planta,ll_pallet,li_tipopal) 
			dw_3.Retrieve(li_cliente,li_planta,ll_pallet)
			pb_recupera.Enabled = True
			il_palcompac = ll_pallet
		ELSE
			pb_recupera.Enabled = False
		END IF	
		
		
	CASE "b_siguiente"
		
		IF dw_1.RowCount() = il_cambiafila THEN
			il_cambiafila = dw_1.RowCount()
		ELSE
			il_cambiafila ++
		END IF
		
		li_tipopal 	= dw_2.Object.repe_tipopa[1]
		
		IF dw_2.Object.repe_tipopa[1] = 3 OR dw_2.Object.repe_tipopa[1] = 7 THEN
			dw_1.SetSort("paen_numero Asc")
			dw_1.Sort()
			ll_pallet	= dw_1.Object.paen_numero[il_cambiafila]
		ELSE
			dw_1.SetSort("paen_nroori Asc")
			dw_1.Sort()
			ll_pallet	= dw_1.object.paen_nroori[il_cambiafila]
		END IF	
		
		li_cliente	= dw_1.Object.clie_codigo[il_cambiafila]	
		li_planta	= dw_1.Object.plde_codigo[il_cambiafila]
		
		IF il_palcompac <> ll_pallet THEN 
			dw_4.Retrieve(li_cliente,li_planta,ll_pallet,li_tipopal) 
			dw_3.Retrieve(li_cliente,li_planta,ll_pallet)
			pb_recupera.Enabled = True
			il_palcompac = ll_pallet
		ELSE
			pb_recupera.Enabled = False
		END IF	
END CHOOSE
end event

type dw_6 from uo_dw within w_maed_spro_repalasigenca_caja_caja
string tag = "Detalle de Pallet Origen"
integer x = 5
integer y = 1376
integer width = 3113
integer height = 532
integer taborder = 40
string title = "Detalle de Pallet Origen"
string dataobject = "dw_mues_palletfruta_repaletizaje_origen"
boolean hscrollbar = true
boolean livescroll = true
end type

event itemchanged;call super::itemchanged;String	ls_Columna, ls_Null
Integer	li_Cajas, li_repe_tipopa

SetNull(ls_Null)

ls_Columna = dwo.name

CHOOSE CASE ls_Columna

	CASE "caja_traspa"
		li_Cajas	=	This.Object.pafr_ccajas[Row]
			IF Integer(Data) > li_Cajas THEN
				MessageBox("Atención", "Cantidad de Cajas a Traspasar" + &
							  "~rno puede ser mayor a Cajas del Pallet.")
				This.SetItem(Row, ls_Columna, Long(ls_Null))
				RETURN 1
			END IF
		li_repe_tipopa = dw_2.Object.repe_tipopa[1]		
		IF li_repe_tipopa = 1 THEN
			dw_6.Object.cajas[1] = This.Object.pafr_ccajas[Row] - Integer(Data)
		END IF	
END CHOOSE
end event

event itemerror;call super::itemerror;RETURN 1
end event

event losefocus;call super::losefocus;dw_6.AcceptText()

end event

type dw_spro_cajasprod from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3858
integer y = 2480
integer width = 192
integer height = 132
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_cajasprod_compacto"
end type

type dw_11 from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3287
integer y = 2320
integer width = 192
integer height = 132
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_palletfruta_pallet_bultos"
end type

type dw_12 from datawindow within w_maed_spro_repalasigenca_caja_caja
boolean visible = false
integer x = 3479
integer y = 2192
integer width = 192
integer height = 132
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_adhesivo"
end type

type pb_agrupa from picturebutton within w_maed_spro_repalasigenca_caja_caja
string tag = "Compactos"
integer x = 3346
integer y = 1704
integer width = 270
integer height = 92
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Agrupa"
alignment htextalign = left!
end type

event clicked;Long ll_fila, ll_paen_nroori, ll_paen_numero

IF dw_4.RowCount() = 0 THEN
	MessageBox("Atención", "Falta Traspaso de Datos.")
	Return
END IF	

istr_mant2.argumento[1]  = string(dw_3.Object.paen_numero[1])
istr_mant2.argumento[2]  = string(dw_3.Object.emba_codigo[1])
OpenWithParm(w_mant_deta_agrupado_reembalaje, istr_mant2)

istr_mant2	       = Message.PowerObjectParm

dw_3.Object.emba_codigo[1] = dw_4.Object.emba_codigo[1] 
			
FOR ll_fila = 1 TO dw_1.RowCount()
	ll_paen_nroori = dw_1.Object.paen_numero[ll_fila]
	ll_paen_numero = dw_3.Object.paen_numero[1]
	IF ll_paen_nroori = ll_paen_numero THEN
		dw_1.Object.emba_codigo[ll_fila] = dw_3.Object.emba_codigo[1]
	END IF
NEXT	

FOR ll_fila = 1 TO dw_1.RowCount()
	ll_paen_nroori = dw_1.Object.paen_numero[ll_fila]
	ll_paen_numero = dw_3.Object.paen_numero[1]
	IF ll_paen_nroori = ll_paen_numero THEN
		dw_1.Object.pafr_calibr[ll_fila] = dw_4.Object.pafr_calibr[1]  
	END IF
NEXT	

FOR ll_fila = 1 TO dw_1.RowCount()
	ll_paen_nroori = dw_1.Object.paen_numero[ll_fila]
	ll_paen_numero = dw_3.Object.paen_numero[1]
	IF ll_paen_nroori = ll_paen_numero THEN
		dw_1.Object.vari_codigo[ll_fila] = dw_4.Object.vari_codigo[1]
		dw_1.Object.pafr_varrot[ll_fila] = dw_4.Object.pafr_varrot[1]
	END IF
NEXT	

end event

