$PBExportHeader$w_maed_recfruprocee_particular_nuevo.srw
forward
global type w_maed_recfruprocee_particular_nuevo from w_mant_encab_deta_csd
end type
type pb_recupera from picturebutton within w_maed_recfruprocee_particular_nuevo
end type
type pb_captura from picturebutton within w_maed_recfruprocee_particular_nuevo
end type
type dw_3 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
type dw_4 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
type dw_5 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
type dw_6 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
type dw_7 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
type dw_8 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
type dw_9 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
type dw_11 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
type dw_12 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
type dw_13 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
type dw_14 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
type dw_10 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
type dw_15 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
type dw_16 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
type dw_17 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
type dw_18 from datawindow within w_maed_recfruprocee_particular_nuevo
end type
end forward

global type w_maed_recfruprocee_particular_nuevo from w_mant_encab_deta_csd
integer width = 3771
integer height = 2160
string title = "RECEPCION DE PALLETS"
string menuname = ""
event ue_validaregistro ( )
event ue_despuesguardar ( )
event ue_cargarchivoplano ( )
pb_recupera pb_recupera
pb_captura pb_captura
dw_3 dw_3
dw_4 dw_4
dw_5 dw_5
dw_6 dw_6
dw_7 dw_7
dw_8 dw_8
dw_9 dw_9
dw_11 dw_11
dw_12 dw_12
dw_13 dw_13
dw_14 dw_14
dw_10 dw_10
dw_15 dw_15
dw_16 dw_16
dw_17 dw_17
dw_18 dw_18
end type
global w_maed_recfruprocee_particular_nuevo w_maed_recfruprocee_particular_nuevo

type variables
w_mant_deta_palletfruta_nuevo iw_mantencion

DataWindowChild	dw_puerto, dw_planta, dw_ptaori, dw_fruta, idwc_patente, idwc_categorias,& 
		idwc_status, idwc_condicion, idwc_tratamiento, idwc_tipofrio, idwc_destino,&
		dw_especie, dw_etiqueta, dw_cliente, dw_condiciones, dw_emba
Integer 	ii_recepcion, ii_cliente, ii_planta, ii_estado, ii_controlaaceso,ii_borra=0,&
			ii_Evento, ii_yaexiste, ii_varrotu
Boolean	ib_existe_folio, ib_primera_entrada, ib_conectado,ib_ConectadoExistencia,&
			ib_Allow_Inserts, ib_Allow_Updates
Long     il_pallet, il_folio, il_numero, il_NroCaja
Date     id_mespro 
String   is_Archivo, is_mensaje, is_ultimacol

Transaction	sqlexis, sqlconec

uo_pallet				iuo_pallet
uo_especie				iuo_especie
uo_variedades			iuo_variedades
uo_embalajesprod		iuo_embalajesprod
uo_etiquetas			iuo_etiquetas
uo_tipofrio				iuo_tipofrio	
uo_status				iuo_status	
uo_tipopallet			iuo_tipopallet	
uo_condicion			iuo_condicion	
uo_codigopallet		iuo_codigopallet	
uo_destinos				iuo_destinos
uo_productores			iuo_productores	
uo_calibre				iuo_calibre
uo_categoria         iuo_categoria
uo_tratamiento       iuo_tratamiento
uo_frutarecepcion		iuo_frutarecepcion
uo_patente				iuo_patente

DataStore	ids_palletfruta_fechaI
end variables

forward prototypes
public function boolean noexisteproductor (string as_columna, string as_valor)
public subroutine cuentatarjas ()
public function boolean noexistecliente (integer ai_codigo)
public subroutine buscaproductor ()
public subroutine habilitaingreso ()
public function long buscanuevofolio (integer cliente, integer planta)
public function boolean noexisteembarque (string as_columna, string as_valor)
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean noexisteplanta (string columna)
public function boolean existefolio (string as_columna, string as_valor)
public subroutine buscaembarque ()
public subroutine eliminapallet (long pallet)
public function boolean conexionexistencia ()
public function integer cajaspallet (string as_embalaje)
public function integer buscabodega (integer bodega)
public function boolean folio_valido (integer bodega, integer tipdoc, long folio)
public subroutine graba_distribucproducc ()
public function boolean chequeaitems (integer tipo, long numero)
public function boolean wf_grabaexistencia ()
public function integer bodeparam ()
public function boolean noexistepallet (integer ai_bodega, long al_pallet)
public subroutine capturarecepciones (integer ai_cliente, integer ai_planta, long al_numero)
public function boolean wf_actualiza_trans (boolean borrando)
public function long buscafoliorecfruprocee_trans (integer ai_planta)
public subroutine existe_cargaregistro ()
public function string buscdescfruta (integer fruta)
public function boolean coneccionbase ()
public function boolean existespro_palletencab (integer ai_cliente, integer ai_planta, long al_pallet)
public function boolean duplicado (string campo)
public subroutine buscavariedad ()
public subroutine buscaembalaje ()
public subroutine cuentacajas ()
public function boolean existevariedadrotu (integer ai_especie, integer ai_variedad)
end prototypes

event ue_validaregistro();Integer	li_cont, li_cont1, li_cont2
String	ls_mensaje, ls_colu[], ls_frio,ls_tipo
Long     ll_Fila,ll_Fildet

IF dw_1.RowCount() >0 THEN
	
	FOR ll_Fila = 1 to dw_1.RowCount()
		IF iuo_pallet.existe(dw_1.Object.clie_codigo[ll_fila],dw_1.Object.paen_numero[ll_fila],False,sqlca,dw_1.Object.plde_codigo[ll_fila]) THEN
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nPallet : " + String(dw_1.Object.paen_numero[ll_fila]) 
			ls_colu[li_cont]	= "paen_numero"
		END IF
		
		IF dw_5.Retrieve(dw_2.Object.clie_codigo[1],dw_1.Object.paen_numero[ll_fila],dw_2.object.plde_codigo[1]) > 0 THEN
			
			IF NOT Isnull(dw_5.Object.espe_codigo[1]) THEN
				IF NOT iuo_especie.existe(dw_5.Object.espe_codigo[1],False,sqlca) THEN
					li_cont1 ++
					ls_mensaje 			= ls_mensaje + "~nEspecie, Correspondiente a Pallet : " + String(dw_1.Object.paen_numero[ll_Fila]) 
					ls_colu[li_cont1]	= "espe_codigo"
				END IF
			END IF
			IF NOT IsNull(dw_5.Object.vari_codigo[1]) THEN
				IF NOT iuo_variedades.existe(dw_5.Object.espe_codigo[1],dw_5.Object.vari_codigo[1],False,sqlca) THEN
					li_cont1 ++
					ls_mensaje 			= ls_mensaje + "~nVariedad, Correspondiente a Pallet : " + String(dw_1.Object.paen_numero[ll_Fila]) 
					ls_colu[li_cont1]	= "vari_codigo"
				END IF
			END IF
			IF NOT IsNull(dw_5.Object.emba_codigo[1]) THEN
				IF NOT iuo_embalajesprod.existe(dw_2.Object.clie_codigo[1],dw_5.Object.emba_codigo[1],False,sqlca) THEN
					li_cont1 ++
					ls_mensaje 			= ls_mensaje + "~nEmbalaje, Correspondiente a Pallet : " + String(dw_1.Object.paen_numero[ll_Fila]) 
					ls_colu[li_cont1]	= "emba_codigo"
				END IF
			END IF
			IF NOT Isnull(dw_5.Object.cate_codigo[1]) THEN
				IF NOT iuo_categoria.existe(dw_5.Object.cate_codigo[1],False,sqlca) THEN
					li_cont1 ++
					ls_mensaje 			= ls_mensaje + "~nCategoria, Correspondiente a Pallet : " + String(dw_1.Object.paen_numero[ll_Fila]) 
					ls_colu[li_cont1]	= "cate_codigo"
				END IF
			END IF
			IF NOT Isnull(dw_5.Object.trat_codigo[1]) THEN
				IF NOT iuo_tratamiento.existe(dw_5.Object.trat_codigo[1],False,sqlca) THEN
					li_cont1 ++
					ls_mensaje 			= ls_mensaje + "~nTratamiento, Correspondiente a Pallet : " + String(dw_1.Object.paen_numero[ll_Fila]) 
					ls_colu[li_cont1]	= "trat_codigo"
				END IF
			END IF
			IF NOT Isnull(dw_5.Object.etiq_codigo[1]) THEN
				IF NOT iuo_etiquetas.existe(dw_5.Object.etiq_codigo[1],False,sqlca) THEN
					li_cont1 ++
					ls_mensaje 			= ls_mensaje + "~nEtiqueta, Correspondiente a Pallet : " + String(dw_1.Object.paen_numero[ll_Fila]) 
					ls_colu[li_cont1]	= "etiq_codigo"
				END IF
			END IF
			//ls_frio = dw_5.Object.frio_codigo[ll_fila]
			IF NOT Isnull(dw_5.Object.frio_codigo[1]) THEN
				IF NOT iuo_tipofrio.existe(dw_5.Object.frio_codigo[1],False,sqlca) THEN
					li_cont1 ++
					ls_mensaje 			= ls_mensaje + "~nTipo Frio, Correspondiente a Pallet : " + String(dw_1.Object.paen_numero[ll_Fila]) 
					ls_colu[li_cont1]	= "frio_codigo"
				END IF
			END IF
			IF NOT Isnull(dw_5.Object.stat_codigo[1]) THEN
				IF NOT iuo_status.existe(dw_5.Object.stat_codigo[1],False,sqlca) THEN
					li_cont1 ++
					ls_mensaje 			= ls_mensaje + "~nStatus, Correspondiente a Pallet : " + String(dw_1.Object.paen_numero[ll_fila]) 
					ls_colu[li_cont1]	= "stat_codigo"
				END IF
			END IF
			//ls_tipo = dw_5.Object.tpem_codigo[ll_fila]
			IF NOT Isnull(dw_5.Object.tpem_codigo[1]) THEN
				IF NOT iuo_tipopallet.existe(dw_2.Object.clie_codigo[1],dw_5.Object.emba_codigo[1],dw_5.Object.tpem_codigo[1],False,sqlca) THEN
					li_cont1 ++
					ls_mensaje 			= ls_mensaje + "~nTipo Pallet: " +String(dw_5.Object.tpem_codigo[1]) + ", Correspondiente a Pallet : " + String(dw_1.Object.paen_numero[ll_Fila]) 
					ls_colu[li_cont1]	= "tpem_codigo"
				END IF
			END IF
			IF NOT Isnull(dw_5.Object.cond_codigo[1]) THEN
				IF NOT iuo_condicion.existe(dw_5.Object.cond_codigo[1],False,sqlca) THEN
					li_cont1 ++
					ls_mensaje 			= ls_mensaje + "~nCondición, Correspondiente a Pallet : " + String(dw_1.Object.paen_numero[ll_Fila]) 
					ls_colu[li_cont1]	= "cond_codigo"
				END IF
			END IF
			IF NOT isnull(dw_5.Object.copa_codigo[1]) THEN
				IF NOT iuo_codigopallet.existe(dw_5.Object.copa_codigo[1],False,sqlca) THEN
					li_cont1 ++
					ls_mensaje 			= ls_mensaje + "~nCódigo Pallet : " +String(dw_5.Object.copa_codigo[1]) + ", Correspondiente a Pallet : " + String(dw_1.Object.paen_numero[ll_Fila]) 
					ls_colu[li_cont1]	= "copa_codigo"
				END IF
			END IF
			IF NOT Isnull(dw_5.Object.dest_codigo[1]) THEN
				IF NOT iuo_destinos.existe(dw_5.Object.dest_codigo[1],False,sqlca) THEN
					li_cont1 ++
					ls_mensaje 			= ls_mensaje + "~nDestino : " + String(dw_5.Object.dest_codigo[1]) + ", Correspondiente a Pallet : " + String(dw_1.Object.paen_numero[ll_Fila]) 
					ls_colu[li_cont1]	= "dest_codigo"
				END IF
			END IF
			
			IF dw_6.Retrieve(dw_2.Object.clie_codigo[1],dw_1.Object.paen_numero[ll_fila],dw_2.Object.plde_codigo[1]) > 0 THEN
				  
				FOR ll_Fildet = 1 TO dw_6.RowCount()
					 IF NOT Isnull(dw_6.Object.prod_codigo[ll_Fildet]) THEN
						 IF NOT iuo_productores.existe(dw_6.Object.prod_codigo[ll_Fildet],False,sqlca) THEN
							 li_cont2 ++
							 ls_mensaje 			= ls_mensaje + "~nProductor, Correspondiente a Pallet : " + String(dw_1.Object.paen_numero[ll_fila]) 
							 ls_colu[li_cont2]	= "prod_codigo"
						 END IF
					 END IF
					 IF NOT Isnull(dw_6.Object.pafr_calibr[ll_Fildet]) THEN
						 IF NOT iuo_calibre.existe(dw_5.Object.espe_codigo[1],dw_5.Object.vari_codigo[1],dw_6.Object.pafr_calibr[ll_Fildet],False,sqlca) THEN
							 li_cont2 ++
							 ls_mensaje 			= ls_mensaje + "~nCalibre, Correspondiente a Pallet : " + String(dw_1.Object.paen_numero[ll_fila]) 
							 ls_colu[li_cont2]	= "pafr_calibr"
						 END IF
					END IF
				NEXT
			END IF
		END IF
	NEXT
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Código de " + ls_mensaje + " Ya existe en tabla respectiva.", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
	This.TriggerEvent("ue_nuevo")
ELSEIF li_cont1 > 0 THEN
	MessageBox("Error de Consistencia", " " + ls_mensaje + " No existe en tabla respectiva.", StopSign!, Ok!)
	Message.DoubleParm = -1
	This.TriggerEvent("ue_nuevo")
ELSEIF li_cont2 > 0 THEN
	MessageBox("Error de Consistencia", " " + ls_mensaje + " No existe en tabla respectiva.", StopSign!, Ok!)
	Message.DoubleParm = -1
	This.TriggerEvent("ue_nuevo")
END IF
dw_5.Reset()
dw_6.Reset()


end event

event ue_despuesguardar();Long    ll_FilasProd, ll_Cantidad, ll_Fila, ll_nrodoc, ll_Fildet
String  ls_emba
Integer li_TipoPallet,li_tipdoc,li_bodega, li_bodadm
Date    ld_fecrec, ld_fecemb

ld_Fecrec	=  dw_2.Object.rfpe_fecrec[1]
li_bodega   =  dw_2.Object.rfpe_ptaori[1]
li_tipdoc	=	3

IF ConexionExistencia() THEN
		
	dw_10.SetTransObject(sqlca)
	dw_7.SetTransObject(sqlexis)
	dw_8.SetTransObject(sqlexis)
	dw_9.SetTransObject(sqlexis)
	
	ll_FilasProd = dw_10.Retrieve(dw_2.Object.clie_codigo[1],dw_2.Object.rfpe_ptaori[1],dw_2.Object.rfpe_numero[1]) 	
	
	//buscabodega(li_bodega)
	//li_bodega = (buscabodega(li_bodega))
	IF ll_FilasProd <> 0 THEN
	
		IF  dw_10.Object.paen_fecemb[1] >= id_mespro THEN
		
			dw_7.InsertRow(0)
			
			dw_7.Object.repe_tipori[1]   = dw_2.Object.rfpe_tipoen[1]
			dw_7.Object.bode_codigo[1]   = /*buscabodega(li_bodega)*/dw_2.Object.rfpe_ptaori[1]
				
				
				FOR ll_Fila = 1 TO ll_FilasProd
					
					IF NoExistePallet(dw_2.Object.rfpe_ptaori[1],dw_10.Object.paen_numero[ll_fila]) THEN
						ll_Fildet = dw_8.InsertRow(0)
						
						li_TipoPallet							=	dw_10.Object.paen_tipopa[ll_fila]
						dw_8.Object.repd_nropal[ll_Fildet]	=	dw_10.Object.paen_numero[ll_fila]
						dw_8.Object.clpr_rut[ll_Fildet]		=	String(dw_10.Object.prod_codigo[ll_fila], &
																		'000000')
						dw_8.Object.espe_codigo[ll_Fildet]	=	dw_10.Object.espe_codigo[ll_fila]
						dw_8.Object.vari_codigo[ll_Fildet]	=	dw_10.Object.vari_codigo[ll_fila]
						dw_8.Object.emba_codigo[ll_Fildet]	=	dw_10.Object.emba_codigo[ll_fila]
						dw_8.Object.repd_ccajas[ll_Fildet]	=	dw_10.Object.pafr_ccajas[ll_fila]
						dw_8.Object.etiq_codigo[ll_Fildet]	=	dw_10.Object.etiq_codigo[ll_fila]
				
						ls_emba										=	dw_10.Object.emba_codigo[ll_fila]
						
						IF li_TipoPallet = 1 THEN
							dw_8.Object.repd_cajpal[ll_Fildet]	=	dw_10.Object.paen_ccajas[ll_fila]
						ELSE
							ll_cantidad									=	CajasPallet(ls_emba)
							dw_8.Object.repd_cajpal[ll_Fildet]	=	ll_cantidad
						END IF
					END IF
				NEXT
				
			IF dw_7.GetItemStatus(1,0,Primary!) = New! or dw_7.GetItemStatus(1,0,Primary!) = NewModified! THEN
				SELECT	Max(mden_numero)
					INTO	:ll_nrodoc
					FROM	"dba"."EXISMOVTOENCA"
					WHERE	mden_tipdoc	= :li_tipdoc
					AND	bode_codigo = :li_bodega 
				  USING  sqlexis;
					
				IF IsNull(ll_nrodoc) THEN ll_nrodoc = 0
				
				ll_nrodoc ++
				
				IF NOT Folio_valido(li_bodega, li_tipdoc, ll_nrodoc) THEN
					MessageBox("Error de Consistencia", "No existen folios disponibles para generar documento.", StopSign!, Ok!)
					Message.DoubleParm = -1
					RETURN
				ELSE
					ll_nrodoc = il_folio 
					dw_7.Object.mden_numero[1]	= ll_nrodoc
				END IF
			
				IF dw_8.RowCount() > 0 THEN
					dw_9.InsertRow(0)
					dw_9.Object.mden_tipdoc[1]	=	li_tipdoc
					dw_9.Object.mden_numero[1]	=	ll_nrodoc
					dw_9.Object.mden_fecmov[1]	=	dw_10.Object.paen_fecemb[1]
					dw_9.Object.tpmv_tipomv[1]	=	2
					dw_9.Object.tpmv_codigo[1]	=	7
					dw_9.Object.mden_tipana[1]	=	0
					dw_9.Object.bode_codigo[1]	=	dw_7.Object.bode_codigo[1]
				
					li_bodadm	=	buscabodega(li_bodega)
					dw_9.Object.mden_bodest[1]	=	li_bodadm
					
					FOR ll_fila = 1 TO dw_8.RowCount()
						dw_8.Object.mden_tipdoc[ll_fila]	= li_tipdoc
						dw_8.Object.mden_numero[ll_fila]	= ll_nrodoc
					NEXT
					dw_7.Object.repe_fecrec[1]   = dw_10.Object.paen_fecemb[1]
				ELSE
					MessageBox("Error de Consistencia", "El Proceso de Rebaje Ya fue Realizado para estos Pallet.", StopSign!, Ok!)
					Message.DoubleParm = -1
				END IF
			
				IF Message.DoubleParm = 0 THEN
					wf_grabaexistencia()
					graba_distribucproducc()
				END IF	
			END IF
		ELSE
		 MessageBox("Error de Consistencia", "Fecha No Correponde a Mes de Proceso, No Se Realizara Rebaje por Producción.", StopSign!, Ok!)
		 Message.DoubleParm = -1
		 RETURN
		END IF
	ELSE
		MessageBox("Error de Consistencia", "No Existe Informacion para rebajar en Existencia.", StopSign!, Ok!)
		 Message.DoubleParm = -1
		 RETURN
	END IF	
ELSE
MessageBox("Sin Conexión", "No Existe Conexión a Existencia.", StopSign!, Ok!)	
RETURN
		
END IF


	
	
	
end event

event ue_cargarchivoplano();w_main.SetMicroHelp("Cargando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

Integer     li_tipoen,li_ptaori,li_tracod,li_ticcod, li_tipopa,li_retorno,li_tabla,li_planta,li_cliente,li_especie,&
				li_variedad,li_tiemcod,li_categoria,li_etiqueta,li_status,li_tratamiento,li_condicion,li_destino,li_altura,li_tmpv,&
				li_estado,li_inspec,li_concal,li_pexpor,li_pmixto,li_varrot,li_copa,li_espe,li_vari,li_cond,li_etiq,li_plde,li_secuen,&
				li_lote,li_copack,li_variro,li_clie,li_plat,li_secu,li_dest,li_tipoin,li_esp,li_var,li_todpal,li_tipoin1,&
				li_clie1,li_secu1,li_dest1,li_count,li_filaCaja,li_predio
Date        ld_fecrec, ld_fecact,ld_fecemb,ld_feccos,ld_fecini,ld_fechaemb,ld_fechai,ld_fechaa,ld_frecha,ld_fechaing
String      ls_mensaje, ls_registro, ls_embalaje, ls_calibre,ls_patent,ls_chofer,ls_tpemcod,ls_frio,ls_nrasda,ls_emba,&
				ls_calib,ls_calrot,ls_emb,ls_tpem,ls_calidad, ls_copa, ls_cean14, ls_regcap
Long			ll_guides,ll_fildet,ll_numpal,ll_filas,ll_secuen,ll_fila,ll_pallet,ll_cajas,ll_productor,ll_filpal,ll_pallet1,ll_correl,&
				ll_prod,ll_totcaja,ll_prdrot,ll_huerto1,ll_cuarte1,ll_filpafr,ll_pal1,ll_nroanu,ll_Filinp,ll_Filadet,ll_numero,ll_numero1,&
				ll_NumCaja,ll_Embala, ll_Selecc, ll_Pesado, ll_Numgia,  ll_Docrel
Time			ld_horact,ld_horini

ll_Filas	= FileOpen(is_archivo)

IF ll_Filas < 0 THEN
	li_Retorno				= MessageBox("Error","Archivo no pudo ser abierto.", Exclamation!, &
												RetryCancel!)
									
	Message.DoubleParm	= li_Retorno
ELSEIF ll_Filas = 0 THEN
		MessageBox("Atención", "Archivo no tiene Filas.")
		Message.DoubleParm	=	2
	 ELSE
	   SetPointer(HourGlass!)
END IF
	 
DO WHILE  FileRead(ll_Filas, ls_Registro)>=1 
	ll_secuen++
		
	li_tabla		 =	Integer(Mid(ls_Registro, 1, 1))
		
	IF li_tabla = 1 THEN
		li_planta    = Integer(Mid(ls_Registro, 2, 4))
		ll_correl	 =	Long(Mid(ls_Registro, 6, 8))
		li_cliente   = Integer(Mid(ls_Registro, 14, 3))
		ld_fecrec    = Date(String(Mid(ls_Registro, 17, 2)+'-'+Mid(ls_Registro, 19, 2)+'-'+Mid(ls_Registro, 21, 4) ))
		li_tipoen	 = Integer(Mid(ls_Registro, 25, 2))
		li_ptaori    = Integer(Mid(ls_Registro, 27, 4))
		ll_guides    = Long(Mid(ls_Registro, 31, 8))
		li_tracod    = Integer(Mid(ls_Registro, 39, 4))
		li_ticcod    = Integer(Mid(ls_Registro, 43, 2))
		ls_patent    = Mid(ls_Registro, 45, 20)
		ls_chofer	 =	Mid(ls_Registro, 65, 50)
		ld_fecact    = Date(String(Mid(ls_Registro, 115, 2)+'-'+Mid(ls_Registro, 117, 2)+'-'+Mid(ls_Registro, 119, 4) ))
		ld_horact    = Time(String(Mid(ls_Registro, 123, 2)+':'+Mid(ls_Registro, 125, 2)+':'+Mid(ls_Registro, 127, 2) ))
		
		ll_fila =	dw_2.InsertRow(0)
		   
		
		   dw_2.SetItem(ll_fila,"plde_codigo",li_planta)
			dw_2.SetItem(ll_fila,"clie_codigo",li_cliente)
			dw_2.SetItem(ll_fila,"rfpe_fecrec",ld_fecrec)			
			dw_2.SetItem(ll_fila,"rfpe_tipoen",li_tipoen )
			dw_2.SetItem(ll_fila,"rfpe_ptaori",li_ptaori )
			dw_2.SetItem(ll_fila,"rfpe_nrores",ll_guides)
			dw_2.SetItem(ll_fila,"rfpe_guides",ll_guides)
			dw_2.SetItem(ll_fila,"tran_codigo",li_tracod) 
			dw_2.SetItem(ll_fila,"tica_codigo",li_ticcod)
			dw_2.SetItem(ll_fila,"rfpe_patent",ls_patent)
			dw_2.SetItem(ll_fila,"rfpe_chofer",ls_chofer)
			dw_2.SetItem(ll_fila,"rfpe_fecact",ld_fecact)
			dw_2.SetItem(ll_fila,"rfpe_horact",ld_horact )
			dw_2.SetItem(ll_fila,"rfpe_horrec",Now())
			dw_3.InsertRow(0)	
			/* Obtiene Numero Automático para Recfruprocee_trans (recepción transitoria) */
			//dw_3.Object.rfpe_numero[1]  = BuscaFolioRecfruprocee_Trans(li_planta)
			dw_3.Object.rfpe_numero[1]  = ll_correl
			dw_3.SetItem(ll_fila,"plde_codigo",li_planta)
			dw_3.SetItem(ll_fila,"clie_codigo",li_cliente)
			dw_3.SetItem(ll_fila,"rfpe_fecrec",ld_fecrec)			
			dw_3.SetItem(ll_fila,"rfpe_tipoen",li_tipoen )
			dw_3.SetItem(ll_fila,"rfpe_ptaori",li_ptaori )
			dw_3.SetItem(ll_fila,"rfpe_nrores",ll_guides)
			dw_3.SetItem(ll_fila,"rfpe_guides",ll_guides)
			dw_3.SetItem(ll_fila,"tran_codigo",li_tracod) 
			dw_3.SetItem(ll_fila,"tica_codigo",li_ticcod)
			dw_3.SetItem(ll_fila,"rfpe_patent",ls_patent)
			dw_3.SetItem(ll_fila,"rfpe_chofer",ls_chofer)
			dw_3.SetItem(ll_fila,"rfpe_fecact",ld_fecact)
			dw_3.SetItem(ll_fila,"rfpe_horact",ld_horact )
			
	END IF
	
	IF li_tabla = 2 THEN
			
			ll_pallet  = Long(Mid(ls_Registro, 17, 8))
									
			ll_fildet =	dw_1.InsertRow(0)
			
			dw_1.SetItem(ll_fildet,"plde_codigo",li_planta)
			dw_1.SetItem(ll_fildet,"clie_codigo",li_cliente)
			dw_1.SetItem(ll_fildet,"paen_numero",ll_pallet)
			dw_4.InsertRow(0)
			dw_4.Object.rfpe_numero[ll_fildet] = dw_3.Object.rfpe_numero[1]
			dw_4.SetItem(ll_fildet,"plde_codigo",li_planta)
			dw_4.SetItem(ll_fildet,"clie_codigo",li_cliente)
			dw_4.SetItem(ll_fildet,"paen_numero",ll_pallet)
			
	END IF
	
	IF li_tabla	=	3 THEN
		
			ll_numpal		=	Long(Mid(ls_Registro, 5, 8))
			li_tipopa	 	=	Integer(Mid(ls_Registro, 17, 1))
			ls_tpemcod 	 	=	Mid(ls_Registro, 18, 5)
			li_especie	 	=	Integer(Mid(ls_Registro, 23, 2))
			li_variedad	 	=	Integer(Mid(ls_Registro, 25, 4))
			li_tiemcod	 	=	Integer(Mid(ls_Registro, 29, 2))
			li_categoria 	= 	Integer(Mid(ls_Registro, 31, 3))
			li_etiqueta		=  Integer(Mid(ls_Registro, 34, 4))
			li_status		=	Integer(Mid(ls_Registro, 38, 2))
			li_tratamiento =  Integer(Mid(ls_Registro, 40, 2))
			ls_frio			=  Mid(ls_Registro, 42, 1)
			li_condicion	=  Integer(Mid(ls_Registro, 43, 1))
			li_destino		=	Integer(Mid(ls_Registro, 44, 3))
			ls_embalaje		=	Mid(ls_Registro, 47, 10)
			ld_fecemb      =  Date(String(Mid(ls_Registro, 57, 2)+'-'+Mid(ls_Registro, 59, 2)+'-'+Mid(ls_Registro, 61, 4) ))
		   ld_feccos		=	Date(String(Mid(ls_Registro, 65, 2)+'-'+Mid(ls_Registro, 67, 2)+'-'+Mid(ls_Registro, 69, 4) ))
			li_altura		=	Integer(Mid(ls_Registro, 73, 4))
			ll_cajas			=	Long(Mid(ls_Registro, 77, 7))
			li_tmpv        =  Integer(Mid(ls_Registro, 84, 2))
			ld_fecini		=	Date(String(Mid(ls_Registro, 86, 2)+'-'+Mid(ls_Registro, 88, 2)+'-'+Mid(ls_Registro, 90, 4) ))
			ld_horini		=	Time(String(Mid(ls_Registro, 94, 2)+':'+Mid(ls_Registro, 96, 2)+':'+Mid(ls_Registro, 98, 2) ))
			li_estado		=	Long(Mid(ls_Registro, 109, 1))
			li_inspec		=	Long(Mid(ls_Registro, 110, 1))
			li_concal		=	Long(Mid(ls_Registro, 111, 1))
			li_pexpor		=	Integer(Mid(ls_Registro, 112, 1))
			li_pmixto		=	Integer(Mid(ls_Registro, 113, 1))
			li_varrot		=	Integer(Mid(ls_Registro, 114, 4))
			ls_nrasda		=	Mid(ls_Registro, 118, 16)
			ls_copa			=	Mid(ls_Registro, 134, 3)
			IF NOT IsNull(ls_copa) AND ls_copa <>"" THEN
				li_copa		=	Integer(ls_copa)
			ELSE
				SetNull(li_copa)
			END IF
			
			li_count = 0
//			Select count() 
//			into :li_count from dba.PALLETENCAB_TRANS
//			where plde_codigo = :li_planta
//			And	clie_codigo = :li_cliente
//			And 	paen_numero = :ll_numpal;
			
			IF li_count = 0 OR Isnull(li_count) THEN
				ll_filpal =	dw_5.InsertRow(0)
				
				dw_5.SetItem(ll_filpal,"clie_codigo",li_cliente)
				dw_5.SetItem(ll_filpal,"paen_numero",ll_numpal)
				dw_5.Setitem(ll_filpal,"plde_codigo",li_planta)
				dw_5.SetItem(ll_filpal,"paen_tipopa",li_tipopa)
				dw_5.SetItem(ll_filpal,"tpem_codigo",Trim(ls_tpemcod))
				dw_5.Setitem(ll_filpal,"espe_codigo",li_especie)
				dw_5.SetItem(ll_filpal,"vari_codigo",li_variedad)
				dw_5.SetItem(ll_filpal,"tiem_codigo",li_tiemcod)
				dw_5.Setitem(ll_filpal,"cate_codigo",li_categoria)
				dw_5.SetItem(ll_filpal,"etiq_codigo",li_etiqueta)
				dw_5.SetItem(ll_filpal,"stat_codigo",li_status)
				dw_5.Setitem(ll_filpal,"trat_codigo",li_tratamiento)
				dw_5.SetItem(ll_filpal,"frio_codigo",ls_frio)
				dw_5.SetItem(ll_filpal,"cond_codigo",li_condicion)
				dw_5.Setitem(ll_filpal,"dest_codigo",li_destino)
				dw_5.SetItem(ll_filpal,"emba_codigo",Trim(ls_embalaje))
				dw_5.SetItem(ll_filpal,"paen_fecemb",ld_fecemb)
				dw_5.SetItem(ll_filpal,"paen_cosecha",ld_feccos)
				dw_5.Setitem(ll_filpal,"paen_altura",li_altura)
				dw_5.Setitem(ll_filpal,"paen_ccajas",ll_cajas)
				dw_5.Setitem(ll_filpal,"tmvp_codigo",li_tmpv)
				dw_5.Setitem(ll_filpal,"paen_fecini",ld_fecini)
				dw_5.Setitem(ll_filpal,"paen_horain",ld_horini)
				dw_5.Setitem(ll_filpal,"paen_estado",1)
				dw_5.Setitem(ll_filpal,"paen_inspec",li_inspec)
				dw_5.Setitem(ll_filpal,"paen_concal",li_concal)
				dw_5.Setitem(ll_filpal,"paen_pexpor",li_pexpor)
				dw_5.Setitem(ll_filpal,"paen_pmixto",li_pmixto)
				dw_5.Setitem(ll_filpal,"paen_varrot",li_varrot)
				dw_5.Setitem(ll_filpal,"paen_nrasda",ls_nrasda)
				dw_5.Setitem(ll_filpal,"copa_codigo",li_copa)
				
				
				ii_planta    = li_planta
				ii_cliente   = li_cliente
				il_numero	 =	ll_numpal
			END IF
			
			
	END IF
		
	IF li_tabla	=	4	THEN
						
			ll_pallet1	=	Long(Mid(ls_Registro, 5, 8))
			li_espe		=	Integer(Mid(ls_Registro, 13, 2))
			li_vari		=	Integer(Mid(ls_Registro, 15, 4))
			ls_emba		=	Trim((Mid(ls_Registro, 19, 10)))
			ll_prod		=	Long(Mid(ls_Registro, 29, 5))
			li_cond		=	Integer(Mid(ls_Registro, 34, 1))
			li_etiq		=	Integer(Mid(ls_Registro, 35, 4))
			li_plde		=	Integer(Mid(ls_Registro, 39, 4))
			ls_calib		=	Mid(ls_Registro, 43, 3)
			li_secuen	=	Integer(Mid(ls_Registro, 46, 2))
			ll_totcaja	=	Integer(Mid(ls_Registro, 48, 7))
			li_lote		=	Integer(Mid(ls_Registro, 55, 4))
			li_copack	=	Integer(Mid(ls_Registro, 59, 4))
			li_variro	=	Integer(Mid(ls_Registro, 63, 4))
			ll_prdrot	=	Integer(Mid(ls_Registro, 67, 5))
			ls_calrot	=	Mid(ls_Registro, 72, 3)
			ll_huerto1	=	Integer(Mid(ls_Registro, 75, 5))
			ll_cuarte1	=	Integer(Mid(ls_Registro, 80, 5))
			ld_fechaemb =	Date(String(Mid(ls_Registro, 85, 2)+'-'+Mid(ls_Registro, 87, 2)+'-'+Mid(ls_Registro, 89, 4) ))
			ld_fechaing =	Date(String(Mid(ls_Registro, 93, 2)+'-'+Mid(ls_Registro,95, 2)+'-'+Mid(ls_Registro, 97, 4) ))
			
			li_count = 0
			Select count() 
			into :li_count from dba.PALLETFRUTA_TRANS
			where plde_codigo = :li_planta
			And	clie_codigo = :li_cliente
			And 	paen_numero = :ll_numpal
			And	espe_codigo = :li_espe
			And	vari_codigo = :li_vari
			And	emba_codigo = :ls_emba
			And	prod_codigo = :ll_prod
			And	cond_codigo = :li_cond
			And	etiq_codigo = :li_etiq
			And	pafr_calibr = :ls_calib
			And	pafr_secuen = :li_secuen;
			
			IF li_count = 0 OR Isnull(li_count)  THEN
			
				ll_filpafr =	dw_6.InsertRow(0)
				
				dw_6.SetItem(ll_filpafr,"clie_codigo",li_cliente)
				dw_6.SetItem(ll_filpafr,"paen_numero",ll_pallet1)
				dw_6.Setitem(ll_filpafr,"espe_codigo",li_espe)
				dw_6.SetItem(ll_filpafr,"vari_codigo",li_vari)
				dw_6.SetItem(ll_filpafr,"emba_codigo",ls_emba)
				dw_6.Setitem(ll_filpafr,"prod_codigo",ll_prod)
				dw_6.SetItem(ll_filpafr,"cond_codigo",li_cond)
				dw_6.SetItem(ll_filpafr,"etiq_codigo",li_etiq)
				dw_6.Setitem(ll_filpafr,"plde_codigo",li_plde)
				dw_6.SetItem(ll_filpafr,"pafr_calibr",ls_calib)
				dw_6.SetItem(ll_filpafr,"pafr_secuen",li_secuen)
				dw_6.Setitem(ll_filpafr,"pafr_ccajas",ll_totcaja)
				dw_6.SetItem(ll_filpafr,"pafr_nrlote",li_lote)
				dw_6.SetItem(ll_filpafr,"pafr_copack",li_copack)
				dw_6.Setitem(ll_filpafr,"pafr_varrot",li_variro)
				dw_6.SetItem(ll_filpafr,"pafr_prdrot",ll_prdrot)
				dw_6.SetItem(ll_filpafr,"pafr_calrot",ls_calrot)
				dw_6.Setitem(ll_filpafr,"pafr_huert1",ll_huerto1)
				dw_6.Setitem(ll_filpafr,"pafr_cuart1",ll_cuarte1)
				dw_6.Setitem(ll_filpafr,"pafr_fecemb",ld_fechaemb)
				dw_6.Setitem(ll_filpafr,"pafr_fecing",ld_fechaing )
			END IF
	END IF
		
	IF li_tabla	=	5 THEN
			li_tipoin	=	Integer(Mid(ls_Registro, 2, 1))
			ll_numero	=	Long(Mid(ls_Registro, 3, 8))
			li_clie		=	Integer(Mid(ls_Registro, 11, 3))
			li_plat		=	Integer(Mid(ls_Registro, 14, 4))
			li_secu		=	Integer(Mid(ls_Registro, 18, 2))
			li_dest		=	Integer(Mid(ls_Registro, 20, 3))
			ld_fechai	=	Date(String(Mid(ls_Registro, 23, 2)+'-'+Mid(ls_Registro, 25, 2)+'-'+Mid(ls_Registro, 27, 4) ))
			li_esp		=	Integer(Mid(ls_Registro, 31, 2))
			li_var		=	Integer(Mid(ls_Registro, 33, 4))
			ls_emb		=	Trim(Mid(ls_Registro, 37, 10))
			ls_tpem		=	Trim(Mid(ls_Registro, 47, 5))
			li_todpal	=	Integer(Mid(ls_Registro, 52, 1))
			ls_calidad	=	Mid(ls_Registro, 53, 3)
			
			li_count = 0
			Select count() 
			into :li_count from dba.INSPECPALENC_TRANS
			where plde_codigo = :li_plat
			And	clie_codigo = :li_clie
			And 	inpe_numero = :ll_numero
			And	inpe_tipoin = :li_tipoin
			And	inpe_secuen = :li_secu;
			
			
			IF li_count = 0 OR Isnull(li_count) THEN
				ll_Filinp	=	dw_11.InsertRow(0)
				
				dw_11.SetItem(ll_Filinp,"inpe_tipoin",li_tipoin)
				dw_11.SetItem(ll_Filinp,"inpe_numero",ll_numero)
				dw_11.Setitem(ll_Filinp,"clie_codigo",li_clie)
				dw_11.SetItem(ll_Filinp,"plde_codigo",li_plat)
				dw_11.SetItem(ll_Filinp,"inpe_secuen",li_secu)
				dw_11.Setitem(ll_Filinp,"dest_codigo",li_dest)
				dw_11.SetItem(ll_Filinp,"inpe_fechai",ld_fechai)
				dw_11.SetItem(ll_Filinp,"espe_codigo",li_esp)
				dw_11.Setitem(ll_Filinp,"vari_codigo",li_var)
				dw_11.SetItem(ll_Filinp,"emba_codigo",ls_emb)
				dw_11.SetItem(ll_Filinp,"tpem_codigo",ls_tpem)
				dw_11.Setitem(ll_Filinp,"inpe_todpal",li_todpal)
				dw_11.Setitem(ll_Filinp,"inpe_calibr",ls_calidad)
			END IF
	END IF
		
	IF li_tabla	=	6 THEN
			li_tipoin1	=	Integer(Mid(ls_Registro, 2, 1))
			ll_numero1	=	Long(Mid(ls_Registro, 3, 8))
			li_clie1		=	Integer(Mid(ls_Registro, 11, 3))
			li_plat		=	Integer(Mid(ls_Registro, 14, 4))
			li_secu1		=	Integer(Mid(ls_Registro, 18, 2))
			ll_pal1     =  Long(Mid(ls_Registro, 20, 8))
			li_dest1		=	Integer(Mid(ls_Registro, 28, 3))
			ld_fechai	=	Date(String(Mid(ls_Registro, 31, 2)+'-'+Mid(ls_Registro, 33, 2)+'-'+Mid(ls_Registro, 35, 4) ))
			ll_nroanu	=	Integer(Mid(ls_Registro, 39, 8))
			ld_fechaa	=	Date(String(Mid(ls_Registro, 47, 2)+'-'+Mid(ls_Registro, 49, 2)+'-'+Mid(ls_Registro, 51, 4) ))
			ld_frecha	=	Date(String(Mid(ls_Registro, 55, 2)+'-'+Mid(ls_Registro, 57, 2)+'-'+Mid(ls_Registro, 59, 4) ))
			
			li_count = 0
			Select count() 
			into :li_count from dba.INSPECPALDET_TRANS
			where plde_codigo = :li_plat
			And	clie_codigo = :li_clie
			And 	inpe_numero = :ll_numero
			And	inpe_tipoin = :li_tipoin
			And	inpe_secuen = :li_secu
			And	paen_numero = :ll_pal1;
			
			
			IF li_count = 0 OR Isnull(li_count) THEN
			
				ll_Filadet	=	dw_12.InsertRow(0)
				
				dw_12.SetItem(ll_Filadet,"inpe_tipoin",li_tipoin1)
				dw_12.SetItem(ll_Filadet,"inpe_numero",ll_numero1)
				dw_12.Setitem(ll_Filadet,"clie_codigo",li_clie1)
				dw_12.SetItem(ll_Filadet,"plde_codigo",li_plat)
				dw_12.SetItem(ll_Filadet,"inpe_secuen",li_secu1)
				dw_12.SetItem(ll_Filadet,"paen_numero",ll_pal1)
				dw_12.Setitem(ll_Filadet,"dest_codigo",li_dest)
				dw_12.SetItem(ll_Filadet,"inpd_fechai",ld_fechai)
				dw_12.SetItem(ll_Filadet,"inpd_nroanu",li_esp)
				dw_12.Setitem(ll_Filadet,"inpd_fechaa",li_var)
				dw_12.SetItem(ll_Filadet,"inpd_frecha",ls_emb)
			END IF
	END IF	
	
	IF li_tabla	=	7 THEN
			li_plde		=	Integer(Mid(ls_Registro, 5, 4))  
			ll_NumCaja	=	Long(Mid(ls_Registro, 9, 8))		
			li_espe		=	Integer(Mid(ls_Registro, 17, 2))
			li_vari		=	Integer(Mid(ls_Registro, 19, 4))			
			ll_prod		=	Long(Mid(ls_Registro, 23, 5))
			li_predio	=	Integer(Mid(ls_Registro, 28, 3))
			ll_huerto1	=	Integer(Mid(ls_Registro, 31, 3))			
			ll_cuarte1	=	Integer(Mid(ls_Registro, 34, 3))			
			ls_emba		=	Trim((Mid(ls_Registro, 37, 10)))
			li_etiq		=	Integer(Mid(ls_Registro, 47, 3))			
			ld_fechaemb =	Date(String(Mid(ls_Registro, 50, 2)+'-'+Mid(ls_Registro, 52, 2)+'-'+Mid(ls_Registro, 54, 4) ))
			ls_calib		=	Mid(ls_Registro, 58, 3)			
			ll_embala	=	Long(Mid(ls_Registro, 61, 8))
			ll_selecc	=	Long(Mid(ls_Registro, 69, 8))
			ll_pesado	=	Long(Mid(ls_Registro, 77, 8))		
			ls_cean14	=	Trim(Mid(ls_Registro, 85, 14))
			ll_pallet1	=	Long(Mid(ls_Registro, 99, 8))					
			ls_regcap	=	Trim(Mid(ls_Registro, 107, 100))
			li_estado	=	Integer(Mid(ls_Registro, 207, 1))			
			li_variro	=	Integer(Mid(ls_Registro, 208, 4))
			ll_Numgia	=	Long(Mid(ls_Registro, 212, 8))			
			li_categoria=	Integer(Mid(ls_Registro, 220, 3))
			li_copack	=	Integer(Mid(ls_Registro, 223, 4))
			ll_Docrel	=	Long(Mid(ls_Registro, 227, 8))			
			ld_horact   =  Time(String(Mid(ls_Registro, 235, 2)+':'+Mid(ls_Registro, 237, 2)+':'+Mid(ls_Registro, 239, 2) ))						
			ld_fechaing =	Date(String(Mid(ls_Registro, 241, 2)+'-'+Mid(ls_Registro,243, 2)+'-'+Mid(ls_Registro, 245, 4) ))					
			li_lote		=	Integer(Mid(ls_Registro, 249, 4))
							
			li_count = 0
			
			Select count() 
			into :li_count from dba.SPRO_CAJASPRODPALLET
			where clie_codigo = :li_cliente
			AND	plde_codigo = :li_planta
			AND	capr_numero	= :ll_NumCaja
			AND	capr_numpal = :ll_numpal;
			 
			IF li_count = 0 OR Isnull(li_count)  THEN			
				li_filaCaja =	dw_14.InsertRow(0)				
				dw_14.SetItem(li_filaCaja,"clie_codigo",li_cliente)
				dw_14.Setitem(li_filaCaja,"plde_codigo",li_plde)
				dw_14.Setitem(li_filaCaja,"capr_numero",ll_NumCaja)
				dw_14.Setitem(li_filaCaja,"espe_codigo",li_espe)
				dw_14.SetItem(li_filaCaja,"vari_codigo",li_vari)
				dw_14.Setitem(li_filaCaja,"prod_codigo",ll_prod)
				dw_14.Setitem(li_filaCaja,"prod_predio",li_predio)
				dw_14.Setitem(li_filaCaja,"prod_huerto",ll_huerto1)
				dw_14.Setitem(li_filaCaja,"prod_cuarte",ll_cuarte1)
				dw_14.SetItem(li_filaCaja,"emba_codigo",ls_emba)
				dw_14.SetItem(li_filaCaja,"etiq_codigo",li_etiq)
				dw_14.Setitem(li_filaCaja,"capr_fecemb",ld_fechaemb)
				dw_14.SetItem(li_filaCaja,"capr_calibr",ls_calib)				
				dw_14.SetItem(li_filaCaja,"capr_embala",ll_embala)				
				dw_14.SetItem(li_filaCaja,"capr_selecc",ll_selecc)				
				dw_14.SetItem(li_filaCaja,"capr_pesado",ll_pesado)				
				dw_14.SetItem(li_filaCaja,"capr_cean14",ls_cean14)												
				dw_14.SetItem(li_filaCaja,"capr_numpal",ll_pallet1)
				dw_14.SetItem(li_filaCaja,"capr_regcap",ls_regcap)
				dw_14.SetItem(li_filaCaja,"capr_estado",li_estado)
				dw_14.SetItem(li_filaCaja,"capr_varrot",li_variro)
				dw_14.SetItem(li_filaCaja,"capr_numgia",ll_Numgia)						
				dw_14.SetItem(li_filaCaja,"cate_codigo",li_categoria)
				dw_14.SetItem(li_filaCaja,"capr_cespak",li_copack)				
				dw_14.SetItem(li_filaCaja,"capr_docrel",ll_Docrel)
				dw_14.SetItem(li_filaCaja,"capr_hordig",ld_horact)
				dw_14.SetItem(li_filaCaja,"capr_fecdig",ld_fechaing)
				dw_14.SetItem(li_filaCaja,"capr_nrlote",li_lote)
			END IF
	END IF
			
LOOP
	
SetPointer(Arrow!)

dw_1.SetRedraw(True)
Message.DoubleParm = li_retorno
end event

public function boolean noexisteproductor (string as_columna, string as_valor);String	ls_nombre
Integer	li_cliente
Long		ll_product

li_cliente	=	dw_2.Object.clie_codigo[1]
ll_product	=	dw_2.Object.prod_codigo[1]

CHOOSE CASE as_Columna
	CASE "clie_codigo"
		li_cliente	=	Integer(as_valor)

	CASE "prod_codigo"
		ll_product	=	Long(as_valor)
		
END CHOOSE

IF IsNull(li_cliente) = False AND li_cliente > 0 AND &
	IsNull(ll_product) = False AND ll_product > 0 THEN
	SELECT	prod_nombre INTO :ls_nombre
		FROM	dba.productores
		WHERE	prod_codigo	=	:ll_product ;
				
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Productores")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención ", "Código de Productor no ha sido ingresado." + &
						"~n~nIngrese o Seleccione otro.", &
						Exclamation!, OK!) 
		RETURN True
	ELSE
		dw_1.SetItem(1, "prod_codigo", ls_nombre)
		RETURN False
	END IF
ELSE
	RETURN False
END IF
end function

public subroutine cuentatarjas ();Long 		I,ll_tra=0,ll_def=0
Integer	li_Tarjas, li_Tardef

FOR I=1 TO dw_1.Rowcount()
	IF dw_1.Object.paen_tipopa[I]=1 THEN
		ll_def ++
	ELSE
		ll_tra ++
	END IF
NEXT

li_Tarjas = dw_2.Object.rfpe_tarjas[1]
li_Tardef = dw_2.Object.rfpe_tardef[1]

istr_mant.argumento[10]	= String( li_Tarjas + li_Tardef)
istr_mant.argumento[11]	= String(dw_2.Object.rfpe_tardef[1])
istr_mant.argumento[12]	= String(dw_2.Object.rfpe_tarjas[1])
		
RETURN
end subroutine

public function boolean noexistecliente (integer ai_codigo);String	ls_nombre

SELECT	clie_nombre
	INTO	:ls_nombre  
   FROM	dba.clientesprod  
   WHERE	clie_codigo =	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "Cliente no Existe. Ingrese otro.")
	RETURN True
END IF

IF F_ValidaCliente(ai_codigo) THEN
	dw_2.GetChild("plde_codigo", dw_planta)
	dw_2.GetChild("rfpe_ptaori", dw_ptaori)
	dw_planta.SetTransObject(sqlca)
	dw_ptaori.SetTransObject(sqlca)
	istr_mant.Argumento[3]	=	String(ai_codigo)
	dw_planta.Retrieve(1)
	dw_ptaori.Retrieve()
	RETURN False
ELSE
	RETURN True
END IF

end function

public subroutine buscaproductor ();Str_busqueda	lstr_busq

dw_2.Modify("buscaproductor.border = 5")

lstr_busq.Argum[1]	=	istr_mant.Argumento[3]

OpenWithParm(w_busc_productores, istr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[3] <> "" THEN
	dw_2.setItem(1, "prod_codigo", lstr_busq.argum[3])
	dw_2.setItem(1, "prod_nombre", lstr_busq.argum[4])
ELSE
	dw_2.SetColumn("prod_codigo")
	dw_2.SetFocus()
END IF

dw_2.Modify("buscaproductor.border = 6")
end subroutine

public subroutine habilitaingreso ();Date		ld_fecha
Integer	li_tarjas,li_tardef
Boolean	lb_estado = True
String	ls_patent, ls_chofer

dw_2.AcceptText()

li_tarjas	=	dw_2.Object.rfpe_tarjas[1]
li_tardef	=	dw_2.Object.rfpe_tardef[1]
ls_patent	=	dw_2.Object.rfpe_patent[1]
ls_chofer	=	dw_2.Object.rfpe_chofer[1]

IF IsNull(li_tarjas) THEN li_tarjas = 0
IF IsNull(li_tardef) THEN li_tardef = 0

//IF IsNull(dw_2.Object.rfpe_numero[1]) OR dw_2.Object.rfpe_numero[1] = 0 OR &
IF IsNull(dw_2.Object.rfpe_nrores[1]) OR dw_2.Object.rfpe_nrores[1] = 0 OR &
	IsNull(dw_2.Object.tica_codigo[1]) OR dw_2.Object.tica_codigo[1] = 0 OR &
	IsNull(ls_patent) OR ls_patent = "" OR &
	IsNull(dw_2.Object.tran_codigo[1]) OR dw_2.Object.tran_codigo[1] = 0 OR &
	IsNull(ls_chofer) OR ls_chofer = "" OR &
	li_tarjas + li_tardef = 0 THEN
	lb_estado = False
END IF

CHOOSE CASE dw_2.Object.rfpe_tipoen[1]
	CASE 1, 2
		IF IsNull(dw_2.Object.rfpe_ptaori[1]) OR dw_2.Object.rfpe_ptaori[1] = 0 THEN
			lb_estado = False
		END IF
	
	 CASE 3
		IF IsNull(dw_2.Object.puer_codigo[1]) OR dw_2.Object.puer_codigo[1] = 0 OR &
			IsNull(dw_2.Object.embq_codigo[1]) OR dw_2.Object.embq_codigo[1] = "" THEN
			lb_estado = False
		END IF			
END CHOOSE	

pb_ins_det.Enabled = lb_estado
end subroutine

public function long buscanuevofolio (integer cliente, integer planta);Integer	li_planta, li_tipoins, li_movto
Long		ll_numero,ll_numero2, ll_fin, ll_actual

li_planta	=	planta

li_movto = 1

SELECT Max(rfpe_numero)
  INTO :ll_numero
  FROM DBA.RECFRUPROCEE
 WHERE plde_codigo = :li_planta;
 
Select como_inicia, como_actual, como_termin
Into	:ll_numero2, :ll_actual, :ll_fin
from DBA.CORRELMOVIMIENTOS 
Where plde_codigo = :li_planta
and	COMO_TIPOMV = :li_movto;

IF ll_actual >= ll_fin THEN
	Return 0
END IF	

ll_fin = ll_fin - 3

IF ll_actual >= ll_fin THEN 
	MessageBox("Advertencia","Quedan menos de 3 Correlativos, Proceda por Mantención 'Correlativos'")
END IF	

IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla CORRELMOVIMIENTOS")
END IF

IF Isnull(ll_numero) OR String(ll_numero) = '' or ll_numero < ll_numero2 THEN
	ll_numero = ll_numero2
END IF	


IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla RecFruProcee")
ELSEIF sqlca.SQLCode = 0 THEN
	ll_numero++
END IF

RETURN ll_numero















end function

public function boolean noexisteembarque (string as_columna, string as_valor);String	ls_nombre, ls_Codigo
Integer	li_Cliente, li_Puerto
Date		ld_fzarpe

ls_Codigo	=	dw_2.Object.embq_codigo[1]
li_Cliente	=	dw_2.Object.clie_codigo[1]

CHOOSE CASE as_Columna
	CASE "embq_codigo"
		ls_codigo	=	as_valor

END CHOOSE

IF IsNull(ls_codigo) = False AND ls_codigo <> "" THEN
	SELECT	embq_nomnav, embq_fzarpe, embq_ptoori INTO :ls_nombre, :ld_fzarpe, :li_Puerto
		FROM	dba.embarqueprod
		WHERE	embq_codigo	=	:ls_Codigo
		AND   clie_codigo =	:li_Cliente ;
				
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Embarqueprod")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención ", "Código de Embarque no ha sido ingresado." + &
						"~n~nIngrese o Seleccione otro.", &
						Exclamation!, OK!) 
		RETURN True
	ELSE
		istr_mant.argumento[24]	=	ls_codigo
		dw_2.SetItem(1, "embq_nomnav", ls_nombre)
		dw_2.SetItem(1, "puer_codigo", li_Puerto)	
		RETURN False
	END IF
ELSE
	RETURN False
END IF

end function

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.SetTabOrder("clie_codigo",10)
	dw_2.SetTabOrder("rfpe_numero",20)
	dw_2.SetTabOrder("plde_codigo",30)
	dw_2.SetTabOrder("rfpe_fecrec",70)
	dw_2.SetTabOrder("rfpe_horrec",80)
	
	dw_2.Modify("clie_codigo.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("rfpe_numero.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("rfpe_fecrec.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("rfpe_horrec.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.SetColumn("rfpe_numero")
	dw_2.SetFocus()
ELSE
	dw_2.SetTabOrder("clie_codigo",0)
	dw_2.SetTabOrder("rfpe_numero",0)
	dw_2.SetTabOrder("plde_codigo",0)
	dw_2.SetTabOrder("rfpe_fecrec",0)
	dw_2.SetTabOrder("rfpe_horrec",0)
	dw_2.Modify("clie_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_numero.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_fecrec.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_horrec.BackGround.Color = " + String(RGB(166,180,210)))

END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno
Long numero
Integer li_planta, li_movto

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_18.Update(True, False) = 1 THEN
			IF dw_17.Update(True, False) = 1 THEN
				IF dw_1.Update(True, False) = 1 THEN
					Commit;
					
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
						
						RollBack;
					ELSE
						lb_Retorno	=	True
						
						dw_2.ResetUpdate()
						dw_18.ResetUpdate()
						dw_17.ResetUpdate()
						dw_1.ResetUpdate()
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
	END IF
ELSE
	IF dw_17.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF dw_2.Update(True, False) = 1 THEN
				IF dw_18.Update(True, False) = 1 THEN
					Commit;
					
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
						
						RollBack;
					ELSE
						lb_Retorno	=	True
						
						dw_17.ResetUpdate()
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
						dw_18.ResetUpdate()
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
	END IF
	IF ids_palletfruta_fechaI.Update()=1 THEN		
			UPDATE dba.RECFRUPROCEE SET
				 rfpe_guides = 0
				 WHERE rfpe_tarjas = 999
				 AND   rfpe_nrores = 999
				 AND   rfpe_tardef = 999;
			
			
			Commit;
		
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
			ELSE
				lb_Retorno	=	True
			END IF
	ELSE 	
		F_ErrorBaseDatos(sqlca, This.Title)
	
		RollBack;
	END IF
END IF

Numero = Long(istr_mant.argumento[2])
li_planta = Integer(istr_mant.argumento[1])

li_movto = 1

/*actualiza numero actual en correlativos */
update DBA.CORRELMOVIMIENTOS set
COMO_ACTUAl = :numero
where plde_codigo = :li_planta
And	como_tipomv = :li_movto;

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean noexisteplanta (string columna);Integer	li_cliente, li_planta, li_tipo

li_cliente	=	Integer(istr_mant.argumento[3])
li_planta	=	Integer(columna)
li_tipo		=	Integer(istr_mant.argumento[13])

SELECT	plde_codigo
	INTO	:li_planta 
   FROM	dba.plantadesp  
   WHERE plde_codigo	=	:li_planta
	AND	plde_tipopl	=	:li_tipo ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla PLANTADESP")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Codigo Planta/Packing no Existe. Ingrese otro")
	RETURN True
END IF

istr_mant.argumento[15]	=	String(li_planta)

RETURN False
end function

public function boolean existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe,li_cliente, li_tipoen
Long		ll_nfolio

li_planta	=	dw_2.Object.plde_codigo[1]
ll_nfolio 	=	dw_2.Object.rfpe_numero[1]
li_cliente	=  dw_2.Object.clie_codigo[1]

CHOOSE CASE as_columna
	CASE "plde_codigo"
		li_planta	=	Integer(as_valor)
		
	CASE "rfpe_numero"
		ll_nfolio 	=	Long(as_valor)
		
	CASE "clie_codigo"
		li_cliente 	=	Integer(as_valor)	
		
END CHOOSE

SELECT  	rfpe_tipoen
	INTO	:li_tipoen
	FROM	dba.RECFRUPROCEE
	WHERE	plde_codigo	=	:li_planta
	AND	rfpe_numero	=	:ll_nfolio ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla RecFruProcee")
	RETURN False
ELSEIF sqlca.SQLCode = 0 THEN
	    istr_mant.argumento[1]	= String(li_planta)
	    istr_mant.argumento[2]	= String(ll_nfolio)
       istr_mant.argumento[3]	= String(li_cliente) 
	    istr_mant.argumento[20]= String(li_tipoen)
	    istr_mant.argumento[4]	= String(dw_2.Object.rfpe_tarjas[1])
	    dw_2.SetItem(1, "clie_codigo",li_cliente)
	    dw_2.SetItem(1, "plde_codigo",li_planta)
	    This.TriggerEvent("ue_recuperadatos")
		 IF li_tipoen = 1 THEN
		    dw_ptaori.Setfilter("plde_tipopl=2")
		    dw_ptaori.Filter()
	    ELSE
		    dw_ptaori.Setfilter("plde_tipopl=1")
		    dw_ptaori.Filter()
	    END IF
		 ib_existe_folio	=	True
	    RETURN False
	ELSE
	    IF IsNull(ll_nfolio) THEN
   		 istr_mant.argumento[1]	= String(li_planta)
		    istr_mant.argumento[2]	= String(ll_nfolio)
		    istr_mant.argumento[3]	= String(li_cliente)
			 ib_existe_folio	=	False
		    RETURN False
	    ELSE
		    MessageBox("Atención","Número de Documento No ha sido generado. Ingrese Otro.")
 			 ib_existe_folio	=	False
		    RETURN True
	    END IF
    END IF

end function

public subroutine buscaembarque ();Str_busqueda	lstr_busq

dw_2.Modify("buscaembarque.border = 5")

istr_busq.Argum[1]	=	String(dw_2.Object.clie_codigo[1])

OpenWithParm(w_busc_embarques, istr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.Argum[1] <> "" THEN
	dw_2.setItem(1, "embq_codigo", lstr_busq.argum[1])
	dw_2.setItem(1, "embq_nomnav", lstr_busq.argum[2])
	dw_2.setItem(1, "puer_codigo", Integer(lstr_busq.argum[6]))	
	istr_mant.argumento[24]	=	lstr_busq.argum[1]
ELSE
	dw_2.SetColumn("embq_codigo")
	dw_2.SetFocus()
END IF

dw_2.Modify("buscaembarque.border = 6")


HabilitaIngreso()
end subroutine

public subroutine eliminapallet (long pallet);Integer 	li_cliente,li_planta
Long		ll_palet,ll_palet1,ll_palet2,ll_palet3,ll_palet4,ll_palet5,ll_palet6

li_planta	=	Integer(istr_mant.argumento[1])
li_cliente	=	Integer(istr_mant.argumento[3])

SELECT count(*) INTO :ll_palet1
FROM dba.despafrigode
WHERE clie_codigo	=	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

SELECT count(*) INTO :ll_palet2
FROM dba.repalletdeta
WHERE clie_codigo = 	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

SELECT count(*) INTO :ll_palet3
FROM dba.inspecpaldet
WHERE clie_codigo = 	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

SELECT count(*) INTO :ll_palet4
FROM dba.fumigadet
WHERE clie_codigo = 	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

SELECT count(*) INTO	:ll_palet5
FROM dba.reetidet
WHERE clie_codigo = 	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

SELECT count(*) INTO :ll_palet6
FROM dba.alpalletfruta
WHERE clie_codigo = 	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

IF IsNull(ll_palet1) THEN	ll_palet1	=	0
IF IsNull(ll_palet2) THEN	ll_palet2 	= 	0
IF IsNull(ll_palet3) THEN	ll_palet3 	= 	0
IF IsNull(ll_palet4) THEN	ll_palet4 	= 	0
IF IsNull(ll_palet5) THEN	ll_palet5 	= 	0
IF IsNull(ll_palet6) THEN	ll_palet6 	= 	0

ll_palet		=	ll_palet1+ll_palet2+ll_palet3+ll_palet4+ll_palet5+ll_palet6

IF ll_palet	=	0 THEN
	
ELSE
	MessageBox( "Error", "Pallet con Relación en Otras Tablas, Solo Elimina en Recepción.")	
END IF

RETURN
end subroutine

public function boolean conexionexistencia ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta

DISCONNECT USING sqlexis;

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

  SELECT cone_nomodb,cone_nomser,cone_nombas,
         cone_nodbms,cone_nomusu,cone_passwo  
    INTO :ls_nomodb,:ls_nomser,:ls_nombas,
	      :ls_nodbms,:ls_Usuario,:ls_Password
    FROM dba.prodconectividad   
   WHERE cone_codigo = 95;

sqlexis.ServerName	=	ls_nomser
sqlexis.DataBase	   =	ls_nombas
sqlexis.Dbms			= 	ls_nodbms
sqlexis.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'

CONNECT USING sqlexis;

IF sqlexis.SQLCode = 0 THEN
	ib_ConectadoExistencia	=	True
ELSE
	ib_ConectadoExistencia	=	False
END IF

RETURN ib_ConectadoExistencia







end function

public function integer cajaspallet (string as_embalaje);Integer	li_CajasPallet, li_Cliente

li_Cliente = dw_2.Object.clie_codigo[1]

  SELECT	emba_cajpal
	INTO	:li_CajasPallet
	FROM	"dba"."embalajesprod"
	WHERE	emba_codigo	=	:as_Embalaje AND
	      clie_codigo =  :li_Cliente
	USING sqlca;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla de Embalajesprod")
	
	li_CajasPallet	=	0
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Embalaje " + as_Embalaje + " no ha sido Creado en " + &
					"tabla respectiva.~r~rAvise a Encargado de Sistema.")
					
	li_CajasPallet	=	0
ELSEIF li_CajasPallet = 0 THEN
	MessageBox("Atención", "Embalaje " + as_Embalaje + " no tiene cantidad de Cajas " + &
					"por Pallet.~r~rAvise a Encargado de Sistema.")
END IF

RETURN li_CajasPallet
end function

public function integer buscabodega (integer bodega);Integer	li_bodega, li_bodadm, li_administradora

li_administradora	=	bodeparam()

li_bodega			=	bodega

  SELECT bode_admini INTO :li_bodadm
	FROM dba.bodegas
	WHERE bode_codigo = :li_bodega
	USING sqlexis ;

IF IsNull(li_bodadm) OR li_bodadm = 0 THEN
	li_bodadm	=	li_administradora
END IF

RETURN li_bodadm


end function

public function boolean folio_valido (integer bodega, integer tipdoc, long folio);Long		ll_inicia, ll_final, li_bodadm

li_bodadm	=	buscabodega(bodega)

SELECT	cobo_inicia, cobo_final
	INTO	:ll_inicia, :ll_final
	FROM	"dba"."CORRELBODE"
	WHERE	bode_codigo	=	:bodega
	AND	mden_tipdoc	=	:tipdoc
	AND   cobo_bodadm =  :li_bodadm
	USING sqlexis;

IF folio < ll_inicia THEN
	il_folio	=	ll_inicia
	RETURN True
ELSEIF folio > ll_inicia THEN
	il_folio	=	folio
	RETURN True
ELSEIF folio <= ll_final THEN
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine graba_distribucproducc ();IF Message.DoubleParm = -1 THEN RETURN

Long		ll_numero
Integer	li_tipdoc, li_mensaje, li_filas
Date		ld_Fecha

li_tipdoc	=	dw_7.Object.mden_tipdoc[1]
ll_numero	=	dw_7.Object.mden_numero[1]
ld_Fecha		=	dw_7.Object.repe_fecrec[1]


IF ChequeaItems(li_tipdoc,ll_numero) THEN
	DECLARE Produc_Diaria PROCEDURE FOR dba.exis_distribucproducc :li_tipdoc, :ll_numero, :ld_Fecha  
	USING sqlexis;
	EXECUTE Produc_Diaria ;

	IF sqlexis.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlexis, This.Title + '~r~rSE ANULARÁ LA CARGA.')
		DECLARE Anula_Produc_Diaria_1 PROCEDURE FOR dba.exis_Anularebajeprod :li_tipdoc, :ll_numero  
		USING sqlexis;
		EXECUTE Anula_Produc_Diaria_1 ;
	ELSE
		MessageBox("Proceso Terminado","Rebaje Producción Realizado, se Generó Guía Nº:" + String(ll_numero) , exclamation!)	
	END IF
	
ELSE
		DECLARE Anula_Produc_Diaria_2 PROCEDURE FOR dba.exis_Anularebajeprod :li_tipdoc, :ll_numero  
		USING sqlexis;
		EXECUTE Anula_Produc_Diaria_2 ;
END IF
	


end subroutine

public function boolean chequeaitems (integer tipo, long numero);Date		ld_fecha
Integer	li_tipdoc
Long		ll_numero, fila
String 	ls_numero

li_tipdoc			=	tipo
ll_numero			=	numero
istr_info.titulo	= "INFORME CHEQUEA REBAJE"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_infcheqrebajeprod"
vinf.dw_1.SetTransObject(sqlexis)

fila = vinf.dw_1.Retrieve(li_tipdoc,ll_numero,dw_7.Object.repe_fecrec[1])

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlexis.SQLErrText, StopSign!, Ok!)
	RETURN False				
ELSEIF fila = 0 THEN
	CLOSE (vinf)
	RETURN true
ELSEIF vinf.dw_1.Object.stkactu[1] = 6001 THEN
	MessageBox("Error ", "No Existe Estandar, opción: Componentes por Especie.", &
										StopSign!, Ok!)
	DECLARE Anula_Produc_Diaria_1 PROCEDURE FOR dba.exis_Anularebajeprod :li_tipdoc, :ll_numero USING sqlexis ;
	EXECUTE Anula_Produc_Diaria_1 ;
	RETURN False
	
ELSEIF vinf.dw_1.Object.cantidad[1] = 6002 THEN
		MessageBox("Error ", "No Existe Estandar, opción: Componentes por Variedad.", &
										StopSign!, Ok!)
		DECLARE Anula_Produc_Diaria_2 PROCEDURE FOR dba.exis_Anularebajeprod :li_tipdoc, :ll_numero  USING sqlexis;
		EXECUTE Anula_Produc_Diaria_2 ;
		RETURN False
		
ELSEIF vinf.dw_1.Object.diferencia[1] = 6003 THEN
		MessageBox("Error ", "No Existe Estandar, opción: Caja Etiquetada por Embalaje.", &
										StopSign!, Ok!)
		DECLARE Anula_Produc_Diaria_3 PROCEDURE FOR dba.exis_Anularebajeprod :li_tipdoc, :ll_numero USING sqlexis ;
		EXECUTE Anula_Produc_Diaria_3 ;
		RETURN False
		
ELSE
	F_Membrete(vinf.dw_1)
	
	ls_numero = String(ll_numero)
	

	vinf.dw_1.Modify("documento.text = '" + ls_numero + "'")
   vinf.dw_1.Object.item_codigo.EditMask.Mask = 'XX.XXX'
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF	
	SetPointer(Arrow!)
	
	RETURN True
END IF
end function

public function boolean wf_grabaexistencia ();IF dw_9.Update() = 1 THEN
	IF dw_7.Update() = 1 THEN
		IF dw_8.Update() = 1 THEN
			Commit;
				
			IF sqlexis.sqlcode <> 0 THEN
				F_ErrorBaseDatos(sqlexis, This.Title)
				RETURN False
			ELSE
				RETURN True
			END IF 
		END IF 
	END IF 
ELSE
	Rollback;
	
	IF sqlexis.sqlcode <> 0 THEN F_ErrorBaseDatos(sqlexis, This.Title)
		RETURN False
END IF

RETURN True
end function

public function integer bodeparam ();Integer li_administradora

SELECT expa_bodega,expa_mespro
  INTO :li_administradora,:id_mespro  
  FROM dba.existeparam  
 WHERE expa_identi = 1 
 USING sqlexis ;
  
 IF sqlexis.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlexis, "Lectura Existeparam ")
	li_administradora = 0
 END IF

RETURN li_administradora
end function

public function boolean noexistepallet (integer ai_bodega, long al_pallet);long ll_existe
  
  SELECT Distinct det.repd_nropal
    INTO :ll_existe  
    FROM dba.receproddeta as det,   
         dba.receprodenca as enc 
   WHERE det.mden_tipdoc = enc.mden_tipdoc  and  
         det.mden_numero = enc.mden_numero  and  
         enc.bode_codigo = :ai_bodega  AND  
         det.repd_nropal = :al_pallet    
    USING sqlexis   ;
	 
IF sqlexis.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlexis, "Lectura Tabla receproddeta")
	RETURN True
ELSEIF sqlexis.SQLCode = 100 THEN
	RETURN True

END IF


RETURN False


end function

public subroutine capturarecepciones (integer ai_cliente, integer ai_planta, long al_numero);Long ll_Fila_e, ll_Fila_d, ll_Fila, ll_Null, respuesta, ll_fila2

SetNull(ll_Null)

dw_1.Reset() 
dw_2.Reset()
dw_2.InsertRow(2)
ll_fila_e	= dw_3.Retrieve(ai_Planta,al_Numero,ai_Cliente) 
	
IF ll_fila_e = -1 THEN
	respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
ELSE
	
   	ll_fila_d	= dw_4.Retrieve(ai_Planta,al_Numero,ai_Cliente)

		IF ll_fila_d = -1 THEN
			respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
		ELSE                        
   		//dw_4.RowsCopy(1, dw_4.RowCount(), Primary!, dw_1, 1, Primary!)
			FOR ll_Fila = 1 TO dw_4.RowCount()
				ll_fila2=dw_1.InsertRow(0)
								
				dw_1.Object.plde_codigo[ll_fila2] = dw_4.Object.plde_codigo[ll_Fila] 
				dw_1.Object.rfpe_numero[ll_fila2] = ll_Null 
				dw_1.Object.clie_codigo[ll_fila2] = dw_4.Object.clie_codigo[ll_Fila]  
				dw_1.Object.paen_numero[ll_fila2] = dw_4.Object.paen_numero[ll_Fila]  
				dw_1.Object.paen_tipopa[ll_fila2] = dw_4.Object.paen_tipopa[ll_Fila]  
				dw_1.Object.emba_codigo[ll_fila2] = dw_4.Object.emba_codigo[ll_Fila]  
				dw_1.Object.cate_codigo[ll_fila2] = dw_4.Object.cate_codigo[ll_Fila]  
				dw_1.Object.stat_codigo[ll_fila2] = dw_4.Object.stat_codigo[ll_Fila]  
				dw_1.Object.paen_ccajas[ll_fila2] = dw_4.Object.paen_ccajas[ll_Fila]  
				dw_1.Object.espe_codigo[ll_fila2] = dw_4.Object.espe_codigo[ll_Fila]  
				dw_1.Object.tpem_codigo[ll_fila2] = dw_4.Object.tpem_codigo[ll_Fila]     
				dw_1.Object.vari_codigo[ll_fila2] = dw_4.Object.vari_codigo[ll_Fila]      
				dw_1.Object.tiem_codigo[ll_fila2] = dw_4.Object.tiem_codigo[ll_Fila]      
				dw_1.Object.etiq_codigo[ll_fila2] = dw_4.Object.etiq_codigo[ll_Fila]  
				dw_1.Object.trat_codigo[ll_fila2] = dw_4.Object.trat_codigo[ll_Fila]  
				dw_1.Object.frio_codigo[ll_fila2] = dw_4.Object.frio_codigo[ll_Fila]  
				dw_1.Object.cond_codigo[ll_fila2] = dw_4.Object.cond_codigo[ll_Fila]  
				dw_1.Object.paen_fecemb[ll_Fila2] = dw_4.Object.paen_fecemb[ll_Fila]  
				dw_1.Object.paen_cosecha[ll_fila2] =dw_4.Object.paen_cosecha[ll_Fila]   
				dw_1.Object.paen_altura[ll_fila2] = dw_4.Object.paen_altura[ll_Fila]  
				dw_1.Object.copa_codigo[ll_fila2] = dw_4.Object.copa_codigo[ll_Fila] 
			NEXT
	   END IF

	  // dw_3.RowsCopy(1, dw_3.RowCount(), Primary!, dw_2, 1, Primary!)
	  
	  dw_2.Object.rfpe_numero[1] = ll_Null
	  dw_2.Object.plde_codigo[1] = dw_3.Object.plde_codigo[1]
     dw_2.Object.inpr_numero[1] = dw_3.Object.inpr_numero[1]
	  dw_2.Object.rfpe_fecrec[1] = dw_3.Object.rfpe_fecrec[1]
	  dw_2.Object.rfpe_tarjas[1] = dw_3.Object.rfpe_tarjas[1]
	  dw_2.Object.rfpe_nrores[1] = dw_3.Object.rfpe_nrores[1]
	  dw_2.Object.rfpe_tipoen[1] = dw_3.Object.rfpe_tipoen[1]
	  dw_2.Object.rfpe_guides[1] = dw_3.Object.rfpe_guides[1]
	  dw_2.Object.rfpe_ptaori[1] = dw_3.Object.rfpe_ptaori[1]
	  dw_2.Object.puer_codigo[1] = dw_3.Object.puer_codigo[1]
	  dw_2.Object.embq_codigo[1] = dw_3.Object.embq_codigo[1]
	  dw_2.Object.prod_codigo[1] = dw_3.Object.prod_codigo[1]
	  dw_2.Object.rfpe_nomres[1] = dw_3.Object.rfpe_nomres[1]
	  dw_2.Object.clie_codigo[1] = dw_3.Object.clie_codigo[1]
	  dw_2.Object.rfpe_tardef[1] = dw_3.Object.rfpe_tardef[1]
	  dw_2.Object.tran_codigo[1] = dw_3.Object.tran_codigo[1]
	  dw_2.Object.rfpe_patent[1] = dw_3.Object.rfpe_patent[1]
	  dw_2.Object.rfpe_chofer[1] = dw_3.Object.rfpe_chofer[1]
	  dw_2.Object.tica_codigo[1] = dw_3.Object.tica_codigo[1]
	  dw_2.Object.rfpe_fecact[1] = dw_3.Object.rfpe_fecact[1]
	  dw_2.Object.rfpe_horact[1] = dw_3.Object.rfpe_horact[1]
	  dw_2.Object.frre_codigo[1] = dw_3.Object.frre_codigo[1] 
	  dw_2.Object.rfpe_horrec[1] = Now()
		

      pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
		pb_imprimir.Enabled	= True
		pb_ins_det.Enabled	= True

		IF ll_fila_d > 0 THEN
			pb_eli_det.Enabled	= True
			dw_1.SetRow(1)
			dw_1.SelectRow(1,True)
			dw_1.SetFocus()
			HabilitaEncab(False)
				
			istr_mant.Argumento[4]	=	String(dw_2.Object.rfpe_tarjas[1])
			istr_mant.Argumento[11]	=	String(dw_2.Object.rfpe_tardef[1])
			istr_mant.argumento[21] =  String(dw_2.Object.rfpe_ptaori[1])
			istr_mant.argumento[30] =  String(dw_2.Object.rfpe_nrores[1])
		END IF
		
	END IF
end subroutine

public function boolean wf_actualiza_trans (boolean borrando);
Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

		IF dw_5.Update(True, False) = 1 THEN
			IF dw_6.Update(True, False) = 1 THEN
				IF dw_3.Update(True, False) = 1 THEN
					IF dw_4.Update(True, False) = 1 THEN
						IF dw_11.Update(True, False) = 1 THEN
							IF dw_12.Update(True, False) = 1 THEN
								IF dw_14.Update(True, False) = 1 THEN
									Commit;
										
									IF sqlca.SQLCode <> 0 THEN
										F_ErrorBaseDatos(sqlca, This.Title)
										RollBack;
									ELSE
										lb_Retorno	=	True
																
										dw_5.ResetUpdate()
										dw_6.ResetUpdate()
										dw_3.ResetUpdate()
										dw_4.ResetUpdate()
										dw_11.ResetUpdate() 
										dw_12.ResetUpdate()
										dw_14.ResetUpdate()
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
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno


end function

public function long buscafoliorecfruprocee_trans (integer ai_planta);Integer	li_planta, li_cliente
Long		ll_numero
Boolean	lb_nulo

//li_cliente	=	cliente	
li_planta	=	ai_planta

SELECT Max(rfpe_numero)
  INTO :ll_numero
  FROM DBA.RECFRUPROCEE_TRANS
 WHERE plde_codigo = :li_planta
 USING sqlca ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla RecFruProcee_trans")
ELSEIF sqlca.SQLCode = 0 THEN
	
		 lb_nulo = IsNull(ll_numero)	
	
	    IF lb_nulo THEN
			 ll_numero=li_planta*10000
		 ELSE
			 ll_numero++
		 END IF
ELSE
	ll_numero=li_planta*10000
END IF

RETURN ll_numero
end function

public subroutine existe_cargaregistro ();Long ll_Numero, ll_Existe
Integer li_Planta, li_Cliente

IF dw_2.RowCount() > 0 THEN
	
	li_Planta 	= dw_2.Object.plde_codigo[1]
	ll_Numero 	= dw_3.Object.rfpe_numero[1] 
	li_Cliente 	= dw_2.Object.clie_codigo[1]
	
	SELECT rfpe_numero  
	  INTO :ll_Existe  
	  FROM dba.recfruprocee_trans 
	 WHERE plde_codigo = :li_Planta  AND  
			 clie_codigo = :li_Cliente AND  
			 rfpe_numero = :ll_Numero   ;
	
	IF ll_Existe > 0 THEN
		
		DELETE FROM dba.recfruproced_trans 
		 WHERE plde_codigo = :li_Planta AND  
				 rfpe_numero = :ll_Existe  AND  
				 clie_codigo = :li_Cliente ;
		 
		DELETE FROM dba.recfruprocee_trans  
		 WHERE plde_codigo = :li_Planta AND  
				 rfpe_numero = :ll_Existe  AND  
				 clie_codigo = :li_Cliente ;
		   
	END IF
 
  END IF
COMMIT ;

end subroutine

public function string buscdescfruta (integer fruta);Integer	li_codigo
String	ls_descri, ls_abrevi

SELECT frre_codigo,frre_descri,frre_abrevi  
INTO :li_codigo,:ls_descri,:ls_abrevi  
FROM dba.frutarecibida  
WHERE frre_codigo = :fruta;

		
RETURN ls_descri

end function

public function boolean coneccionbase ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta


DISCONNECT USING sqlconec;

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

  SELECT cone_nomodb,cone_nomser,cone_nombas,
         cone_nodbms,cone_nomusu,cone_passwo  
    INTO :ls_nomodb,:ls_nomser,:ls_nombas,
	      :ls_nodbms,:ls_Usuario,:ls_Password
    FROM dba.prodconectividad   
   WHERE cone_codigo = 90;

sqlconec.ServerName	=	ls_nomser
sqlconec.DataBase	   =	ls_nombas
sqlconec.Dbms			= 	ls_nodbms
sqlconec.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'

CONNECT USING sqlconec;

IF sqlconec.SQLCode = 0 THEN
	ib_Conectado	=	True
ELSE
	ib_Conectado	=	False
END IF

RETURN ib_Conectado

end function

public function boolean existespro_palletencab (integer ai_cliente, integer ai_planta, long al_pallet);Boolean 	lb_Retorno
Long		ll_Cantid


lb_Retorno	= False

SELECT Count(*)
	INTO	:ll_Cantid
	FROM	dba.spro_palletencab
	WHERE	clie_codigo	= 	:ai_Cliente
	AND	paen_numero = 	:al_Pallet
	AND	plde_codigo	=	:ai_Planta;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Spro_PalletEncab")
	lb_retorno = True
ELSEIF ll_Cantid > 0 THEN	
		//MessageBox("Atención","No se puede Eliminar el Pallet es Ingreso Caja a Caja. Ingrese Otro.", Exclamation!, Ok!)
		lb_retorno = True		
END IF
		
RETURN lb_Retorno
end function

public function boolean duplicado (string campo);Long		ll_fila
String	ls_codigo,ls_cliente,ls_numero


ls_codigo	=	String(dw_2.Object.plde_codigo[1])
ls_numero	= 	campo
ls_cliente	= 	String(dw_2.Object.clie_codigo[1])


	ll_fila	= dw_17.Find("plde_codigo = " + ls_codigo + " AND clie_codigo = " + ls_cliente + " AND paen_numero =	" + campo, 1, dw_17.RowCount())

IF ll_fila > 0 THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine buscavariedad ();Str_busqueda	lstr_busq

dw_17.Modify("buscavariedad.border = 0")
dw_17.Modify("buscavariedad.border = 5")

lstr_busq.argum[1]	=	istr_mant.Argumento[1]
lstr_busq.argum[2]	=	String(dw_17.GetItemNumber(1, "espe_codigo"))

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[4] <> '' THEN
	istr_mant.argumento[4]	=	lstr_busq.argum[4]
	istr_mant.argumento[5]	=	lstr_busq.argum[5]
	
	dw_17.setItem(1, "vari_codigo", Integer(lstr_busq.argum[4]))
	dw_17.setItem(1, "vari_nombre", lstr_busq.argum[5])
	dw_17.SetColumn("vari_codigo")
	dw_17.SetFocus()
END IF

dw_17.Modify("buscavariedad.border = 0")
dw_17.Modify("buscavariedad.border = 6")
end subroutine

public subroutine buscaembalaje ();Str_busqueda		lstr_busq

dw_17.Modify("buscaembalaje.border = 0")
dw_17.Modify("buscaembalaje.border = 5")

lstr_busq.Argum[1]	=	istr_mant.Argumento[1]

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.Argum[2] <> "" THEN
	istr_mant.Argumento[7]	=	lstr_busq.Argum[2]
	istr_mant.Argumento[8]	=	lstr_busq.Argum[3]
	
	dw_17.setItem(1, "emba_codigo", lstr_busq.Argum[2])
	dw_17.setItem(1, "emba_nombre", lstr_busq.Argum[3])
	dw_17.SetItem(1, "tiem_codigo", Integer(lstr_busq.Argum[5]))
	
	dw_emba.Retrieve(Integer(istr_mant.Argumento[1]), lstr_busq.Argum[2])
ELSE
	dw_17.SetColumn("emba_codigo")
	dw_17.SetFocus()
END IF

dw_17.Modify("buscaembalaje.border = 0")
dw_17.Modify("buscaembalaje.border = 6")
end subroutine

public subroutine cuentacajas ();Long	I
istr_mant.Argumento[11]	= string(dw_17.GetItemNumber(1, "paen_ccajas"))
For I=1 to dw_1.RowCount()
	istr_mant.argumento[11]     =	String(Long(istr_mant.argumento[11]) - dw_1.GetitemNumber(I,"pafr_ccajas"))
Next
IF	Long(istr_mant.argumento[11])<0 THEN  istr_mant.argumento[11]='0'

RETURN
end subroutine

public function boolean existevariedadrotu (integer ai_especie, integer ai_variedad);String	ls_nombre

SELECT	vari_relaci
	INTO	:ii_varrotu  
   FROM	dba.variedades  
   WHERE	espe_codigo =	:ai_especie
	AND	vari_codigo = 	:ai_variedad;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Variedades")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "Variedad no existe no Existe. Ingrese otra.")
	RETURN True
END IF
Return False


end function

event open;//	Argumentos Mantenedor
//	istr_mant.argumento[1]		= 	Código de Planta
//	istr_mant.argumento[2]		= 	Número de Folio Recepción
//	istr_mant.argumento[3]		= 	Código de Exportador
//	istr_mant.argumento[4]		= 	Cantidad de Tarjas Transitorias
//	istr_mant.argumento[5]		= 	Tipo de packing
// istr_mant.argumento[6]  	= 	Número de Pallet
// istr_mant.argumento[7]  	= 	Parámetro de solo consulta 1=consulta, "":otro
// istr_mant.argumento[8]  	= 	Especie
//	istr_mant.argumento[11]		= 	Cantidad de Tarjas Definitivas
// istr_mant.argumento[20] 	= 	Tipo de Recepción
// istr_mant.argumento[21]  	= 	Packing Origen
// istr_mant.argumento[30]    =  Guia de despacho
// istr_mant.argumento[31]    =  Tipo de Recepción 1=Packing, -1=Otro
// istr_mant.argumento[33]    =  Recepción Transmitida
// istr_mant.argumento[34]    =  Número Recepción Transmitida
// istr_mant.argumento[35]    =  Permite determinar si el botón de grabar se activa desde ue_nuevo
// istr_mant.argumento[36]    =  Permite determinar si el botón de grabar se activa desde ue_borra_detalle

Integer li_codigo, li_cliente


/* Si Código de parempresa es igual a 1 se efectua la transacción para REBAJE DE EXISTENCIA*/
  SELECT empr_codgen,empr_coacce
    INTO :li_codigo,:ii_controlaaceso  
    FROM dba.parempresa  
	 USING sqlca;

IF Not f_validafechatempo(today()) THEN
   Messagebox('Atención','Aclare Fecha Actual Temporada con Informática')
END IF

IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("rfpe_ptaori", dw_ptaori)
dw_2.GetChild("puer_codigo", dw_puerto)

dw_planta.SetTransObject(sqlca)
dw_ptaori.SetTransObject(sqlca)
dw_puerto.SetTransObject(sqlca)

dw_planta.Retrieve(1)
dw_ptaori.Retrieve(gi_codplanta)
dw_ptaori.Setfilter("plde_tipopl=2")
dw_ptaori.Filter()
dw_puerto.Retrieve(900)

dw_ptaori.SetSort("plde_nombre")
dw_ptaori.Sort( )

dw_2.GetChild("frre_codigo", dw_fruta)
dw_fruta.SetTransObject(sqlca)
dw_fruta.Retrieve()
dw_2.Object.frre_codigo[1] = 1
x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_11.SetTransObject(sqlca)
dw_12.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

dw_17.SetTransObject(sqlca)
dw_17.GetChild("clie_codigo", dw_cliente)
dw_17.GetChild("plde_codigo", dw_planta)
dw_17.GetChild("espe_codigo", dw_especie)
dw_17.GetChild("etiq_codigo", dw_etiqueta)

dw_17.GetChild("cate_codigo", idwc_categorias)
dw_17.GetChild("stat_codigo", idwc_status)
dw_17.GetChild("cond_codigo", idwc_condicion)
dw_17.GetChild("trat_codigo", idwc_tratamiento)
dw_17.GetChild("frio_codigo", idwc_tipofrio)
dw_17.GetChild("dest_codigo", idwc_destino)

dw_cliente.SetTransObject(sqlca)
dw_planta.SetTransObject(sqlca)
dw_especie.SetTransObject(sqlca)
dw_etiqueta.SetTransObject(sqlca)

idwc_categorias.SetTransObject(sqlca)
idwc_status.SetTransObject(sqlca)
idwc_condicion.SetTransObject(sqlca)
idwc_tratamiento.SetTransObject(sqlca)
idwc_tipofrio.SetTransObject(sqlca)
idwc_destino.SetTransObject(sqlca)

dw_cliente.Retrieve(gi_codexport)
dw_planta.Retrieve(1)
dw_especie.Retrieve()
dw_etiqueta.Retrieve()

dw_17.GetChild("tpem_codigo", dw_emba)
dw_emba.SetTransObject(sqlca)
if dw_emba.Retrieve(gi_codexport, 'U006') = 0 then
   dw_emba.Insertrow(0)
end if

idwc_categorias.Retrieve()
idwc_status.Retrieve()
idwc_condicion.Retrieve()
idwc_tratamiento.Retrieve()
idwc_tipofrio.Retrieve()
idwc_destino.Retrieve()
//dw_17.InsertRow(0)

istr_mant.dw				= dw_1
istr_mant.solo_consulta = False

buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"

istr_mant.argumento[1]	=	String(gi_codplanta)
istr_mant.argumento[3]	=	String(gi_codexport)
istr_mant.argumento[4]  =	'0'
istr_mant.argumento[5]	=	'dw_mues_recfruproced'
istr_mant.argumento[6]	= 	'1'
istr_mant.argumento[11] =	'0'
istr_mant.argumento[13] =	'2'
istr_mant.argumento[16] =  '1'
istr_mant.argumento[20] =	'1'
istr_mant.argumento[21] =	''
istr_mant.argumento[24] =	''
istr_mant.argumento[31] =	'1'
istr_mant.argumento[32] =	''
istr_mant.argumento[33] =	''
istr_mant.argumento[34] =	''
istr_mant.argumento[35] =	'0'
istr_mant.argumento[36] =	'0'
istr_mant.argumento[39] =	''				// Tipo de Entrada
istr_mant.argumento[40] =	''				// Tipo de Entrada InterPlanta 2 - 6

iuo_especie				=	CREATE	uo_especie				
iuo_variedades			=	CREATE	uo_variedades			
iuo_embalajesprod		=  CREATE	uo_embalajesprod		
iuo_etiquetas			=	CREATE	uo_etiquetas			
iuo_tipofrio			=	CREATE	uo_tipofrio					
iuo_status				=	CREATE	uo_status				
iuo_tipopallet			=	CREATE	uo_tipopallet			
iuo_condicion			=	CREATE	uo_condicion				
iuo_codigopallet		=	CREATE	uo_codigopallet			
iuo_destinos			=	CREATE	uo_destinos				
iuo_productores		=	CREATE	uo_productores				
iuo_calibre				=	CREATE	uo_calibre	
iuo_categoria			=	CREATE	uo_categoria
iuo_tratamiento		=	CREATE	uo_tratamiento
iuo_frutarecepcion	=	CREATE	uo_frutarecepcion	
iuo_patente				=	CREATE	uo_patente

sqlexis					=	CREATE Transaction
sqlconec					=	CREATE Transaction

IF ii_controlaaceso = 1 THEN
	IF NOT coneccionbase() THEN
		MessageBox("Sin Conexión", "No Existe Conexión a Base Control de Acceso.", StopSign!, Ok!)	
	ELSE
		dw_2.Object.rfpe_patent.Dddw.Name				=	'dw_mues_controlacceso'
		dw_2.Object.rfpe_patent.Dddw.DisplayColumn	=	'ctac_patent'
		dw_2.Object.rfpe_patent.Dddw.DataColumn		=	'ctac_patent'
		dw_2.Object.rfpe_patent.Dddw.AllowEdit			=  True
		dw_2.Object.rfpe_patent.Dddw.HScrollBar		=  True
		dw_2.Object.rfpe_patent.Dddw.VScrollBar		=  True
		dw_2.Object.rfpe_patent.Dddw.Case 				= 	"Upper"
		dw_2.Modify("rfpe_patent.dddw.Limit=10")
		dw_2.Modify("rfpe_patent.Dddw.PercentWidth=200")
	
		dw_2.GetChild("rfpe_patent", idwc_patente)
		idwc_patente.SetTransObject(sqlconec)
		idwc_patente.Retrieve()
	END IF	
END IF	
//DISCONNECT USING sqlconec;

IF li_codigo = 1 THEN	
	IF NOT ConexionExistencia() THEN
		MessageBox("Sin Conexión", "No Existe Conexión a Existencia.", StopSign!, Ok!)	
		RETURN
	END IF
END IF

pb_nuevo.PostEvent(Clicked!)

ib_primera_entrada = True

gb_Repalletizado = False

iuo_pallet       = CREATE   uo_pallet

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							
// Para guardar fecha de ingreso en la Palletfruta
ids_palletfruta_fechaI					=	CREATE	DataStore
ids_palletfruta_fechaI.DataObject	=	'dw_mues_palletfruta_pafrfecing'
ids_palletfruta_fechaI.SetTransObject(sqlca)

dw_13.SetTransObject(sqlca)
dw_14.SetTransObject(sqlca)
dw_15.SetTransObject(sqlca)
dw_16.SetTransObject(sqlca)
dw_18.SetTransObject(sqlca)



end event

event ue_borra_detalle;call super::ue_borra_detalle;Long		pallet
Integer	li_Cliente,li_Planta

IF dw_1.rowcount() < 1 THEN
	dw_1.SetFocus()
	RETURN
END IF

pallet=dw_1.getitemNumber(dw_1.GetRow(),"paen_numero")

li_Cliente	= dw_1.GetItemNumber(dw_1.GetRow(), "clie_codigo")
li_Planta	= dw_1.GetItemNumber(dw_1.GetRow(), "plde_codigo")

IF ExisteSpro_palletencab(li_Cliente,li_Planta,pallet) THEN
	MessageBox("Atención","No se puede borrar actual registro. Pallet Caja a Caja")	
	RETURN
END IF

IF dw_1.rowcount() < 1 THEN
	dw_1.SetFocus()
	RETURN
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

THIS.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False
istr_mant.argumento[46]=	String(dw_17.Object.paen_fecemb[1])		// Fecha de Embalaje

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		IF Messagebox('Borrar','Elimina Detalle de Pallet ?', question!, yesno!, 2) = 1 THEN
			istr_mant.argumento[36] = '0'
			//pb_grabar.TriggerEvent(Clicked!)
			This.TriggerEvent("ue_guardar")
		END IF 
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_1.RowCount() = 0 THEN 
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;date fecha
fecha=(dw_17.Object.paen_fecemb[1])


istr_mant.borra	= False
istr_mant.agrega	= True
cuentacajas()
istr_mant.argumento[14]=   String(dw_17.Object.espe_codigo[1])
istr_mant.argumento[15]=   String(dw_17.Object.vari_codigo[1])
istr_mant.argumento[17]=   String(dw_17.Object.paen_numero[1])
istr_mant.argumento[18]=	String(dw_17.Object.cond_codigo[1])
istr_mant.argumento[45]=	String(dw_2.Object.rfpe_fecrec[1])		// Fecha de Ingreso
istr_mant.argumento[46]=	String(dw_17.Object.paen_fecemb[1])		// Fecha de Embalaje

OpenWithParm(w_mant_deta_palletfruta_nuevo, istr_mant)

IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, ll_fila_pal, ll_fila_dr, ll_fila, ll_numero, ll_fila_det,&
	ll_fila_enc
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
		 	ll_fila_d	= dw_18.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]))
			ii_evento = ll_fila_d
			IF ll_fila_d > 0 THEN
				ll_numero = dw_18.Object.paen_numero[1]
			END IF
			
			ll_fila_enc	= dw_17.Retrieve(Integer(istr_mant.argumento[3]),ll_numero,Integer(istr_mant.argumento[1]))
			ll_fila_det	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),ll_numero,Integer(istr_mant.argumento[1]))
			
			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE                                                    
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_imprimir.Enabled	= True
				pb_ins_det.Enabled	= True

				IF ll_fila_d > 0 THEN
					pb_eli_det.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
					
					istr_mant.Argumento[4]	=	String(dw_2.Object.rfpe_tarjas[1])
					istr_mant.Argumento[11]	=	String(dw_2.Object.rfpe_tardef[1])
					istr_mant.argumento[21] =  String(dw_2.Object.rfpe_ptaori[1])
					istr_mant.argumento[30] =  String(dw_2.Object.rfpe_nrores[1])
					
					IF Isnull(dw_2.Object.rfpe_fecing[1]) OR Isnull(dw_2.Object.rfpe_horing[1]) OR &
						Isnull(dw_2.Object.rfpe_sucuco[1]) OR dw_2.Object.rfpe_sucuco[1] = 0 THEN
						dw_2.Object.rfpe_fecing.Visible = 0
						dw_2.Object.rfpe_horing.Visible = 0
						dw_2.Object.rfpe_sucuco.Visible = 0
						dw_2.Object.t_19.Visible = 0
						dw_2.Object.t_20.Visible = 0
						dw_2.Object.t_21.Visible = 0
					ELSE
						dw_2.Object.rfpe_fecing.Visible = 1
						dw_2.Object.rfpe_horing.Visible = 1
						dw_2.Object.rfpe_sucuco.Visible = 1
						dw_2.Object.t_19.Visible = 1
						dw_2.Object.t_20.Visible = 1
						dw_2.Object.t_21.Visible = 1
					END IF
					
					istr_mant.argumento[40] = ''
					IF dw_2.Object.rfpe_tipoen[1] = 2 OR dw_2.Object.rfpe_tipoen[1] =	6 THEN
						istr_mant.argumento[40] = '1'
					END IF					
					
					IF ll_fila_d = dw_2.Object.rfpe_tarjas[1] + dw_2.Object.rfpe_tardef[1] THEN
						pb_ins_det.Enabled = False
					END IF
				ELSE
//	   		pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_maed_recfruprocee_particular_nuevo.create
int iCurrent
call super::create
this.pb_recupera=create pb_recupera
this.pb_captura=create pb_captura
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_6=create dw_6
this.dw_7=create dw_7
this.dw_8=create dw_8
this.dw_9=create dw_9
this.dw_11=create dw_11
this.dw_12=create dw_12
this.dw_13=create dw_13
this.dw_14=create dw_14
this.dw_10=create dw_10
this.dw_15=create dw_15
this.dw_16=create dw_16
this.dw_17=create dw_17
this.dw_18=create dw_18
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_recupera
this.Control[iCurrent+2]=this.pb_captura
this.Control[iCurrent+3]=this.dw_3
this.Control[iCurrent+4]=this.dw_4
this.Control[iCurrent+5]=this.dw_5
this.Control[iCurrent+6]=this.dw_6
this.Control[iCurrent+7]=this.dw_7
this.Control[iCurrent+8]=this.dw_8
this.Control[iCurrent+9]=this.dw_9
this.Control[iCurrent+10]=this.dw_11
this.Control[iCurrent+11]=this.dw_12
this.Control[iCurrent+12]=this.dw_13
this.Control[iCurrent+13]=this.dw_14
this.Control[iCurrent+14]=this.dw_10
this.Control[iCurrent+15]=this.dw_15
this.Control[iCurrent+16]=this.dw_16
this.Control[iCurrent+17]=this.dw_17
this.Control[iCurrent+18]=this.dw_18
end on

on w_maed_recfruprocee_particular_nuevo.destroy
call super::destroy
destroy(this.pb_recupera)
destroy(this.pb_captura)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_6)
destroy(this.dw_7)
destroy(this.dw_8)
destroy(this.dw_9)
destroy(this.dw_11)
destroy(this.dw_12)
destroy(this.dw_13)
destroy(this.dw_14)
destroy(this.dw_10)
destroy(this.dw_15)
destroy(this.dw_16)
destroy(this.dw_17)
destroy(this.dw_18)
end on

event ue_nuevo;Integer li_codigo
Boolean	lb_Consulta

HabilitaEncab(True)
istr_mant.argumento[35] =	'0'
istr_mant.argumento[36] =	'0'
ib_ok	= True

DwItemStatus	lis_Estado

lb_Consulta	= istr_mant.Solo_Consulta

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			IF dw_1.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

lis_Estado	=	dw_17.GetItemStatus(1, 0, Primary!)

IF IsNull(lis_Estado) OR lis_Estado = NewModified! OR  lb_Consulta THEN
	dw_1.Reset()
	dw_3.Reset()
	dw_4.Reset()
	dw_10.reset()
	dw_17.reset()
	dw_17.InsertRow(0)
	dw_18.reset()
	
	/* Si Código de parempresa es igual a 1 se efectua la transacción para existencia*/
	  SELECT empr_codgen  
		 INTO :li_codigo  
		 FROM dba.parempresa  
		 USING sqlca;
		
	IF li_codigo = 1 THEN	
		dw_8.reset()
		dw_9.reset()
		dw_7.reset()
	END IF
	pb_eli_det.Enabled		= False
	pb_ins_det.Enabled		= False
	pb_grabar.Enabled			= False
	pb_eliminar.Enabled		= False
	pb_imprimir.Enabled		= False
	istr_mant.solo_consulta	= False
	dw_2.Enabled				= True
	
	dw_2.SetRedraw(False)
	dw_2.Reset()
	dw_2.InsertRow(0)
	dw_2.SetItem(1, "clie_codigo", gi_codexport)
	dw_2.SetItem(1, "plde_codigo", gi_codplanta)
	dw_2.SetItem(1, "tica_codigo", 1)
	dw_2.SetItem(1, "frre_codigo", 1)
	dw_2.SetItem(1, "rfpe_horrec",Now())
	dw_17.SetItem(1, "clie_codigo", gi_codexport)
	dw_17.SetItem(1, "plde_codigo", gi_codplanta)
	
	dw_2.Modify("rfpe_tipoen.BackGround.Color = " + String(RGB(166,180,210)))
	
	dw_ptaori.SetSort("plde_nombre")
	dw_ptaori.Sort( )
	
	/*se reasigna tipo de recepción como Packing*/
	istr_mant.argumento[31]='1'
	/*se limpia argumento de recepción transmitida*/
	istr_mant.argumento[33] = ''
	//
	dw_2.SetRedraw(True)
	Return
END IF
		
	dw_17.SetRedraw(False)
	dw_17.Reset()
	dw_17.InsertRow(0)
	dw_17.SetRedraw(True)
	dw_17.SetItem(1, "clie_codigo", gi_codexport)
	dw_17.SetItem(1, "plde_codigo", gi_codplanta)
	dw_18.Reset()
	
	dw_1.Reset()

	pb_eli_det.Enabled		=	True
	pb_ins_det.Enabled		=	True
	pb_grabar.Enabled			=	True
	pb_eliminar.Enabled		=	True
	pb_imprimir.Enabled		=	True
	
	IF IsNull(lis_Estado) OR lis_Estado = New! OR  lb_Consulta THEN
		dw_2.SetFocus()
	ELSE
		dw_17.SetFocus()
	END IF






end event

event ue_seleccion;call super::ue_seleccion;istr_busq.argum[1]	= istr_mant.argumento[3]	
istr_busq.argum[2]	= istr_mant.argumento[1]	

OpenWithParm(w_busc_recfruprocee, istr_busq)

istr_busq	       = Message.PowerObjectParm

IF istr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= istr_busq.argum[2]
	istr_mant.argumento[2]	= istr_busq.argum[5]
	istr_mant.argumento[3]	= istr_busq.argum[1]
	istr_mant.argumento[4]  = istr_busq.argum[7]
   istr_mant.argumento[20] = istr_busq.argum[17]
	ib_existe_folio	=	True
	This.TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_modifica_detalle;	Cuentacajas()
	
	IF dw_1.RowCount() > 0 THEN
		istr_mant.argumento[1]	=	String(dw_17.getitemNumber(1,"clie_codigo"))
		istr_mant.argumento[2]	=	String(dw_17.getitemNumber(1,"paen_numero"))
		istr_mant.argumento[3]	=	String(dw_17.getitemNumber(1,"espe_codigo"))
		istr_mant.argumento[45]=	String(dw_2.Object.rfpe_fecrec[1])		// Fecha de Ingreso
		istr_mant.argumento[46]=	String(dw_17.Object.paen_fecemb[1])		// Fecha de Embalaje
		
		istr_mant.agrega 			= False
		istr_mant.solo_consulta	= False

		OpenWithParm(iw_mantencion, istr_mant)
	END IF

end event

event ue_antesguardar;Long 		ll_nuevofolio, ll_fila, ll_numero, ll_fila_det, fila_find
Integer	li_fillas, ll_filas, ll_fila_d, ll_fila_g, li_Secuencia, li_cliente,&
	li_planta, li_paen_mixto
Date ld_Fecha_Ing

IF ExistevariedadRotu(dw_17.Object.espe_codigo[1],dw_17.Object.vari_codigo[1]) THEN
	dw_17.Object.paen_varrot[1] = dw_17.Object.vari_codigo[1]
ELSE	
	dw_17.Object.paen_varrot[1] = ii_varrotu
END IF	

li_cliente	=	dw_17.Object.clie_codigo[1]
li_planta	=	dw_17.Object.plde_codigo[1]
ll_numero	=	dw_17.Object.paen_numero[1]

SELECT	IsNull(Max(pafr_secuen), 0)
	INTO	:li_Secuencia
	FROM	dba.palletfruta
	WHERE	clie_codigo	=	:li_cliente
	AND	plde_codigo =	:li_planta
	AND	paen_numero	=	:ll_numero;

FOR ll_fila = 1 TO dw_1.RowCount()
		dw_1.Object.clie_codigo[ll_fila]	=	dw_17.Object.clie_codigo[1]
		dw_1.Object.plde_codigo[ll_fila]	=	dw_17.Object.plde_codigo[1]
		dw_1.Object.paen_numero[ll_fila]	=	dw_17.Object.paen_numero[1]
		dw_1.Object.espe_codigo[ll_fila]	=	dw_17.Object.espe_codigo[1]
		dw_1.Object.cond_codigo[ll_fila]	=	dw_17.Object.cond_codigo[1]
		dw_1.Object.etiq_codigo[ll_fila]	=	dw_17.Object.etiq_codigo[1]
		dw_1.Object.pafr_fecemb[ll_fila]	=	dw_17.Object.paen_fecemb[1]
		
		dw_1.Object.pafr_prdrot[ll_fila]	=	dw_1.Object.prod_codigo[1]
		dw_1.Object.pafr_calrot[ll_fila]	=	dw_1.Object.pafr_calibr[1]
		dw_1.Object.pafr_varrot[ll_fila]	=  dw_17.Object.paen_varrot[1]
		
		IF IsNull(dw_17.Object.paen_pmixto[1]) THEN
			li_paen_mixto = 0
		ELSE
			li_paen_mixto = dw_17.Object.paen_pmixto[1]
		END IF
		
		IF li_paen_mixto <> 1 THEN
			dw_1.Object.vari_codigo[ll_fila]	=	dw_17.Object.vari_codigo[1]
			dw_1.Object.emba_codigo[ll_fila]	=	dw_17.Object.emba_codigo[1]
			dw_1.Object.pafr_varrot[ll_fila]	=	dw_17.Object.vari_codigo[1]
		END IF
	
	IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		li_Secuencia ++
		dw_1.Object.pafr_secuen[ll_Fila] =	li_Secuencia
	END IF
	
NEXT


fila_find =  dw_18.Find("paen_numero = " + String(dw_17.Object.paen_numero[1]),&
			1, dw_18.RowCount())
			
IF fila_find = 0 THEN 			
	ll_fila_det = dw_18.InsertRow(0)
	dw_18.Object.clie_codigo[ll_fila_det]	=	dw_17.Object.clie_codigo[1]
	dw_18.Object.plde_codigo[ll_fila_det]	=	dw_17.Object.plde_codigo[1]
	dw_18.Object.paen_numero[ll_fila_det]	=	dw_17.Object.paen_numero[1]
	
END IF

IF dw_2.GetNextModified(0, Primary!) > 0 THEN
	dw_2.SetItem(1, "rfpe_fecact", Today())
	dw_2.SetItem(1, "rfpe_horact", Now())
END IF

ll_nuevofolio = dw_2.Object.rfpe_numero[1]

	/*
	Se actualiza tabla recfruprocee a objeto de bloquearla hasta que termine la grabación
	del ingreso
	*/
	UPDATE dba.RECFRUPROCEE SET
			 rfpe_guides = 999
			 WHERE rfpe_tarjas = 999
			 AND   rfpe_nrores = 999
			 AND   rfpe_tardef = 999;
			 
	IF isnull(dw_2.Object.rfpe_numero[1]) OR dw_2.Object.rfpe_numero[1] = 0 THEN
		ll_nuevofolio=Buscanuevofolio(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]))
	END IF
	
	IF Long(ll_nuevofolio) = 0  THEN
		MessageBox("Advertencia","No Quedan Correlativos Disponibles, Proceda por Mantención.")
		Message.DoubleParm = -1
		Return 
	END IF
	
	IF isnull(dw_2.Object.rfpe_numero[1]) OR dw_2.Object.rfpe_numero[1] = 0 THEN
		dw_2.Object.rfpe_numero[1]	= ll_nuevofolio
   	dw_2.SetItem(1, "rfpe_numero",ll_nuevofolio)
	END IF	

	istr_mant.argumento[2]	= String(ll_nuevofolio)
	IF fila_find = 0 THEN
		FOR li_fillas = 1 TO dw_18.RowCount()
			 dw_18.Object.rfpe_numero[li_fillas]	= ll_nuevofolio
		NEXT
	END IF	

// Para grabar Fechas de Embalaje y Recepcion a la Palletfruta
ids_palletfruta_fechaI.Reset() 
dw_13.Reset()

IF istr_mant.argumento[13] = '2'  OR  istr_mant.argumento[13] = '1' THEN
	For ll_filas	=	1	To	dw_1.RowCount()
		IF	dw_13.Retrieve(dw_2.Object.clie_codigo[1],dw_1.Object.paen_numero[ll_filas],dw_2.Object.plde_codigo[1]) > 0 THEN
			FOR ll_fila_d = 1 TO dw_13.RowCount()
				ll_fila_g	=	ids_palletfruta_fechaI.InsertRow(0)
				ids_palletfruta_fechaI.SetItem(ll_fila_g,'clie_codigo',dw_13.Object.clie_codigo[ll_fila_d])				
				ids_palletfruta_fechaI.SetItem(ll_fila_g,'paen_numero',dw_13.Object.paen_numero[ll_fila_d])						
				ids_palletfruta_fechaI.SetItem(ll_fila_g,'espe_codigo',dw_13.Object.espe_codigo[ll_fila_d])				
				ids_palletfruta_fechaI.SetItem(ll_fila_g,'vari_codigo',dw_13.Object.vari_codigo[ll_fila_d])				
				ids_palletfruta_fechaI.SetItem(ll_fila_g,'emba_codigo',dw_13.Object.emba_codigo[ll_fila_d])						
				ids_palletfruta_fechaI.SetItem(ll_fila_g,'prod_codigo',dw_13.Object.prod_codigo[ll_fila_d])				
				ids_palletfruta_fechaI.SetItem(ll_fila_g,'cond_codigo',dw_13.Object.cond_codigo[ll_fila_d])				
				ids_palletfruta_fechaI.SetItem(ll_fila_g,'etiq_codigo',dw_13.Object.etiq_codigo[ll_fila_d])						
				ids_palletfruta_fechaI.SetItem(ll_fila_g,'plde_codigo',dw_13.Object.plde_codigo[ll_fila_d])				
				ids_palletfruta_fechaI.SetItem(ll_fila_g,'pafr_calibr',dw_13.Object.pafr_calibr[ll_fila_d])				
				ids_palletfruta_fechaI.SetItem(ll_fila_g,'pafr_secuen',dw_13.Object.pafr_secuen[ll_fila_d])		
				
				ld_Fecha_Ing	=	dw_13.GetItemDate(ll_fila_d,'pafr_fecing')
				// Para guardar la fecha de Ingreso cuando es Packing
				IF istr_mant.argumento[13] = '2' THEN
					ld_Fecha_Ing	= dw_2.Object.rfpe_fecrec[1] 
				END IF
				
				// Para guardar la fecha de Ingreso cuando traspaso Inperplanta
				IF  istr_mant.argumento[13] = '1'  THEN
					IF Isnull(ld_Fecha_Ing) OR ld_Fecha_Ing = Date('1900-01-01') THEN
						ld_Fecha_Ing	= dw_2.Object.rfpe_fecrec[1]
					END IF
				END IF
						
				ids_palletfruta_fechaI.SetItem(ll_fila_g,'pafr_fecing',ld_Fecha_Ing)				
			NEXT
		END IF
	NEXT
	
	DwItemStatus	Estadol 
	ids_palletfruta_fechaI.ResetUpdate()
	
	FOR ll_filas 	=	1 TO 	ids_palletfruta_fechaI.RowCount()
		Estadol	=	ids_palletfruta_fechaI.GetItemStatus(ll_filas, 0, Primary!)
		//ids_palletfruta_fechaI.SetItemStatus(ll_filas,0,  Primary!, DataModified!	)	
		ids_palletfruta_fechaI.SetItemStatus(ll_filas,'pafr_fecing',  Primary!, DataModified!	)	
		Estadol	=	ids_palletfruta_fechaI.GetItemStatus(ll_filas, 0, Primary!)
	NEXT
END IF

dw_13.Reset()

end event

event ue_borrar;call super::ue_borrar;//Long ll_fila, Pallet
//
//IF dw_1.rowcount() >0 THEN
//	FOR ll_fila=1 TO dw_1.rowcount()
//		 Pallet = dw_1.object.paen_numero[ll_fila]
//		 rebajaTamlot(Pallet)
//	NEXT
//END IF

end event

event ue_validaborrar;call super::ue_validaborrar;//
end event

event resize;//
Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
ELSE
	dw_1.width	=	This.WorkSpaceWidth() - 400
	maximo		=	dw_1.width
END IF

dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					= 37

dw_17.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
//dw_17.y					= 37

dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					= 64 + dw_2.Height
dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41

li_posic_x				= This.WorkSpaceWidth() - 250
li_posic_y				=300

IF pb_buscar.Visible THEN
	pb_buscar.x				= li_posic_x
	pb_buscar.y				= li_posic_y
	pb_buscar.width		= 156
	pb_buscar.height		= 133
	li_visible ++
	li_posic_y += 165
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				= li_posic_x
	pb_nuevo.y				= li_posic_y
	pb_nuevo.width			= 156
	pb_nuevo.height		= 133
	li_visible ++
	li_posic_y += 165
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			= li_posic_x
	pb_eliminar.y			= li_posic_y
	pb_eliminar.width		= 156
	pb_eliminar.height	= 133
	li_visible ++
	li_posic_y += 165
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				= li_posic_x
	pb_grabar.y				= li_posic_y
	pb_grabar.width		= 156
	pb_grabar.height		= 133
	li_visible ++
	li_posic_y += 165
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			= li_posic_x
	pb_imprimir.y			= li_posic_y
	pb_imprimir.width		= 156
	pb_imprimir.height	= 133
	li_visible ++
	li_posic_y += 165
END IF

IF pb_recupera.Visible THEN
	pb_recupera.x			= li_posic_x
	pb_recupera.y			= li_posic_y
	pb_recupera.width		= 156
	pb_recupera.height	= 133
	li_visible ++
	li_posic_y += 165
END IF

IF pb_captura.Visible THEN
	pb_captura.x			= li_posic_x
	pb_captura.y			= li_posic_y
	pb_captura.width		= 156
	pb_captura.height	= 133
	li_visible ++
	li_posic_y += 165
END IF

IF pb_salir.Visible THEN
	pb_salir.x				= li_posic_x
	pb_salir.y				= li_posic_y
	pb_salir.width			= 156
	pb_salir.height		= 133
	li_visible ++
	li_posic_y += 165
END IF


pb_ins_det.x			= li_posic_x
pb_ins_det.y			= 1300
pb_ins_det.width		= 156
pb_ins_det.height		= 133

pb_eli_det.x			= li_posic_x
pb_eli_det.y			= pb_ins_det.y + 160
pb_eli_det.width		= 156
pb_eli_det.height		= 133









end event

event ue_guardar;Integer li_Planta, li_Cliente, li_administradora, li_bodeadmin, li_codigo, li_Estado
Long    ll_Pallet, ll_Fila, ll_numero

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN 	RETURN

/*  INICIO  TRANSMISION AUTOMÁTICA (RECEPCIÓN INTERPLANTA)    */	

IF istr_mant.argumento[33] =	'1' THEN
	
	TriggerEvent("ue_validaregistro")
	
	IF Message.DoubleParm = -1  THEN RETURN
		FOR ll_Fila = 1 TO dw_1.RowCount()
			
			li_Planta  = dw_1.Object.plde_codigo[ll_Fila]
			li_cliente = dw_1.Object.clie_codigo[ll_Fila]
			ll_Pallet  = dw_1.Object.paen_numero[ll_Fila]
					
			DECLARE GrabaPallet PROCEDURE FOR dba.FProc_CargaRecepciones_Transmitidas
			@Cliente =:li_Cliente,
			@Planta  =:li_Planta,
			@Pallet = :ll_Pallet;
						
			EXECUTE GrabaPallet;
		 NEXT
		 
		 IF sqlca.sqlcode < 0 THEN
				F_ErrorBaseDatos(sqlca,"El proceso Carga Pallet Por Recepción Transmitida " +&
						" no se ejecutó Exitosamente")
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
				dw_3.ResetUpdate()
				dw_4.ResetUpdate()
				dw_5.ResetUpdate()
				dw_6.ResetUpdate()
				dw_11.ResetUpdate()
				dw_12.ResetUpdate()	
				dw_14.ResetUpdate()
				Close GrabaPallet;
				Return		  
		 ELSE
			
			 ll_numero = dw_3.Object.rfpe_numero[1]
			 
			 
	//		 DELETE FROM dba.recfruproced_trans  
	//			WHERE clie_codigo = :li_Cliente AND  
	//					plde_codigo = :li_Planta  AND  
	//					rfpe_numero = :ll_Numero ;
						
			 UPDATE dba.recfruprocee_trans  SET
				RFPE_ESTADO = 2
				WHERE clie_codigo = :li_Cliente AND  
						plde_codigo = :li_Planta  AND  
						rfpe_numero = :ll_Numero ;			
			
			
			 Close GrabaPallet;
			 MessageBox("Atención", "El proceso Carga Pallet Por Recepción Transmitida " +&
									" se ejecutó Exitosamente")								
	
		 END IF	
	END IF
	istr_mant.argumento[33] = ''
	
	IF wf_actualiza_db(False) THEN
		w_main.SetMicroHelp("Información Grabada.")
		pb_eliminar.Enabled	= True
		pb_imprimir.Enabled	= True
		
	/*  FINAL  TRANSMISION AUTOMÁTICA (RECEPCIÓN INTERPLANTA)    */		
		
	/*  INICIO  REBAJE A EXISTENCIA    */	
	/* Si Código de parempresa es igual a 1 se efectua la transacción para existencia*/
	  SELECT empr_codgen  
		 INTO :li_codigo  
		 FROM dba.parempresa  
		 USING sqlca;
		
		IF li_codigo = 1 THEN	
			IF istr_mant.argumento[31]  = '1' AND istr_mant.argumento[35] =	'1' AND istr_mant.argumento[36] = '1' THEN
				IF dw_2.Object.rfpe_ptaori[1] > 0 THEN
					li_administradora = BodeParam()
					IF li_administradora<> 0 THEN
						li_bodeadmin = BuscaBodega(dw_2.Object.rfpe_ptaori[1])
						IF li_administradora = li_bodeadmin  THEN /*SI EL TIPO DE ENTRADA ES PACKING , SE REALIZA EL REBAJE A EXISTENCIA*/
							TriggerEvent("ue_despuesguardar")
						END IF
					END IF
				END IF
			END IF
		END IF
	ELSE
		w_main.SetMicroHelp("No se puede Grabar información.")
		Message.DoubleParm = -1
		RETURN
	END IF
	
/* FINAL REBAJE A EXISTENCIA    */
 
	
	

end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
Date		ld_desde, ld_hasta
String   ls_recepcion, ls_descri

str_info	lstr_info

SELECT pate_inicio,pate_termin
INTO   :ld_desde,:ld_hasta
FROM dba.paramtemporada
WHERE pate_tempor=1;

lstr_info.titulo	= "RECEPCION DE PALLETS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_recepcion_pallet"
//vinf.dw_1.DataObject = "dw_info_recfruproced"

vinf.dw_1.SetTransObject(sqlca)

istr_mant.argumento[30] = String(dw_2.Object.rfpe_numero[1])

ls_descri = buscdescfruta(dw_2.Object.frre_codigo[1])

ls_recepcion = "Recepcion " +String(dw_2.Object.rfpe_numero[1])
ls_recepcion = ls_recepcion + "                       Fecha "+String(dw_2.Object.rfpe_fecrec[1])
ls_recepcion = ls_recepcion + "                       Guia "+String(dw_2.Object.rfpe_nrores[1])
ls_recepcion = ls_recepcion + "                       En Planta "+String(dw_2.Object.plde_codigo[1])

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]), &
ld_desde,ld_hasta,0,Long(istr_mant.argumento[30]),0,0)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
  	vinf.dw_1.Modify("guia.text = '" + ls_recepcion + "'")
	vinf.dw_1.Modify("fruta.text = '" + ls_descri + "'")  
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_recfruprocee_particular_nuevo
integer x = 27
integer y = 1380
integer width = 3333
integer height = 616
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_palletfruta_nuevo"
boolean livescroll = false
end type

event dw_1::dragdrop;call super::dragdrop;dw_1.Object.objetname.Moveable = 0 
end event

event dw_1::dwnkey;call super::dwnkey;This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!
		Message.DoubleParm = 0
		
		Parent.TriggerEvent("ue_validaregistro")
		
		IF Message.DoubleParm = -1 THEN
			This.SetRedraw(True)
			RETURN -1
		ELSEIF Key = KeyDownArrow! AND il_fila = dw_1.RowCount() THEN
			Parent.TriggerEvent("ue_nuevo_detalle")
		END IF
		
	CASE KeyTab!
		IF is_ultimacol = This.GetColumnName() AND il_fila = dw_1.RowCount() THEN
			Message.DoubleParm = 0
			
			Parent.TriggerEvent("ue_validaregistro")
			
			IF Message.DoubleParm = -1 THEN
				This.SetRedraw(True)
				RETURN -1
			END IF
		END IF

END CHOOSE

This.SetRedraw(True)
This.SetColumn(1)
This.SetFocus()

RETURN 0
end event

event dw_1::ue_seteafila;il_Fila	=	This.GetRow()
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_recfruprocee_particular_nuevo
integer x = 37
integer y = 12
integer width = 3333
integer height = 1344
string dataobject = "dw_mant_recfruprocee_particular_nuevo"
end type

event dw_2::itemchanged;call super::itemchanged;String	ls_columna, ls_nula
Date		ld_nula

DataWIndowChild	dw_calibres

SetNull(ls_nula)
SetNull(ld_nula)

ls_columna = GetColumnName()

CHOOSE CASE ls_columna
	CASE "clie_codigo"
		dw_17.SetItem(1, "clie_codigo", Integer(Data))
		
	CASE "plde_codigo"
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(1, ls_columna, Integer(ls_nula))
			RETURN 1
		ELSE
			dw_17.SetItem(1, "plde_codigo", Integer(data))
		END IF
		
	CASE "rfpe_numero"
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(1, ls_columna, Integer(ls_nula))
			RETURN 1
		END IF
		
	CASE "embq_codigo"
		IF NoExisteEmbarque(ls_columna, data) THEN
			This.SetItem(1, ls_columna, ls_nula)
			RETURN 1
		END IF
		istr_mant.argumento[24]	=	data
	CASE "prod_codigo"
		IF NoExisteProductor(ls_columna, data) THEN
			This.SetItem(1, ls_columna, Integer(ls_nula))
			RETURN 1
		END IF
		
	CASE "rfpe_tarjas"
		istr_mant.argumento[4]	= data
		
	CASE "rfpe_tardef"
		istr_mant.argumento[11]	= data

	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, gi_codexport)
			RETURN 1
		END IF	
		
	CASE "rfpe_tipoen"
		IF (data='1') THEN
			istr_mant.argumento[31] = '1'
			istr_mant.argumento[13] = '2'
			dw_ptaori.Setfilter("plde_tipopl=2")
			dw_ptaori.Filter()
		ELSE
			istr_mant.argumento[31] = '-1'
		     IF (data='2') OR (data='6') THEN
			   istr_mant.argumento[13] = '1'
			   dw_ptaori.Setfilter("plde_tipopl=1")
			   dw_ptaori.Filter()
		     END IF
		   	istr_mant.argumento[20] = data
		END IF
		istr_mant.argumento[40] = ''
	   IF (data='2') OR (data='6') THEN
				istr_mant.argumento[40] = '1'
		END IF
		
	CASE "rfpe_ptaori"
		IF NoexistePlanta(data) THEN
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		ELSE 
			istr_mant.argumento[21]=data
		END IF
		
		
	CASE "rfpe_fecrec"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, ls_Columna, ld_nula)
			RETURN 1
		END IF

	CASE "rfpe_nrores"
		istr_mant.argumento[30]	= data
		
	CASE "frre_codigo"
		IF iuo_frutarecepcion.existe(Integer(Data),TRUE,sqlca) THEN
			istr_mant.argumento[38]=data
		ELSE 
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF	
	
	CASE "rfpe_patent"
		IF ib_conectado = True THEN
			IF iuo_patente.existe(data,True,sqlconec) THEN
				dw_2.Object.rfpe_fecing.Visible = 1
				dw_2.Object.rfpe_horing.Visible = 1
				dw_2.Object.rfpe_sucuco.Visible = 1
				dw_2.Object.t_19.Visible = 1
				dw_2.Object.t_20.Visible = 1
				dw_2.Object.t_21.Visible = 1
				dw_2.Object.rfpe_fecing[Row] = iuo_patente.FechaIng
				dw_2.Object.rfpe_horing[Row] = iuo_patente.HoraIng
				dw_2.Object.rfpe_sucuco[Row] = iuo_patente.Sucursal
			ELSE
				dw_2.Object.rfpe_fecing[Row] = Date(ls_Nula)
				dw_2.Object.rfpe_horing[Row] = Time(ls_Nula)
				dw_2.Object.rfpe_sucuco[Row] = Integer(ls_Nula)
				dw_2.Object.rfpe_fecing.Visible = 0
				dw_2.Object.rfpe_horing.Visible = 0
				dw_2.Object.rfpe_sucuco.Visible = 0
				dw_2.Object.t_19.Visible = 0
				dw_2.Object.t_20.Visible = 0
				dw_2.Object.t_21.Visible = 0
			END IF	
		END IF	
	END CHOOSE

HabilitaIngreso()
end event

event dw_2::clicked;call super::clicked;CHOOSE CASE dwo.name
	CASE "buscaembarque"
		BuscaEmbarque()
		
	CASE "buscaproductor"
		BuscaProductor()
		
END CHOOSE
end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_recfruprocee_particular_nuevo
integer x = 3442
integer y = 252
end type

event pb_nuevo::clicked;ib_primera_entrada = True
ib_existe_folio	 =	False
dw_2.Object.rfpe_fecing.Visible = 0
dw_2.Object.rfpe_horing.Visible = 0
dw_2.Object.rfpe_sucuco.Visible = 0

istr_mant.argumento[40] = ''

call super:: clicked


end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_recfruprocee_particular_nuevo
integer x = 3442
integer y = 432
end type

event pb_eliminar::clicked;ii_borra = 1

call Super::Clicked
end event

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_recfruprocee_particular_nuevo
integer x = 3442
integer y = 612
end type

event pb_grabar::clicked;ib_primera_entrada = False

istr_mant.argumento[35] =	'1'
istr_mant.argumento[36] =	'1'

call super:: clicked

ib_existe_folio = True
end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_recfruprocee_particular_nuevo
integer x = 3442
integer y = 792
end type

event pb_imprimir::clicked;Integer		li_TipoEntrada, li_Reimprime
Long			ll_FilaPallet, ll_FilaCajas, ll_NroPallet, ll_NroCajas

li_TipoEntrada	=	dw_2.Object.rfpe_tipoen[1]

IF li_TipoEntrada =  7 THEN
	
	li_Reimprime	=	MessageBox("Atención","Desea Re-Imprimir Adhesivos Compactos?",Exclamation!, OKCancel!, 2)
	
	IF li_Reimprime = 1 THEN
		
		FOR ll_FilaPallet = 1 TO dw_1.RowCount()
			
		    ll_NroPallet	=	dw_1.Object.paen_numero[ll_FilaPallet]
		    dw_15.Reset()
			 dw_15.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]),ll_NroPallet)
			 FOR ll_FilaCajas = 1 TO dw_15.RowCount()
			 		
				  il_NroCaja	=	dw_15.Object.capr_numero[ll_FilaCajas]
			     dw_16.Reset()
				  
				  Parent.TriggerEvent("ue_imprimir2")
				  
				  
			 NEXT
	   NEXT
	ELSE
		
		CALL SUPER::Clicked
	END IF
ELSE
		
	CALL Super::Clicked
	
END IF
end event

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_recfruprocee_particular_nuevo
integer x = 3442
integer y = 1332
end type

event pb_salir::clicked;istr_mant.argumento[33] = '0'
istr_mant.argumento[35] = '0'
istr_mant.argumento[36] = '0'
IF dw_1.RowCount() > 0 THEN
   //pb_grabar.TriggerEvent(clicked!)
	This.TriggerEvent("ue_guardar")
	DISCONNECT USING sqlexis;
	DISCONNECT USING sqlconec;
END IF

call super:: clicked
end event

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_recfruprocee_particular_nuevo
integer x = 3442
integer y = 1600
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_recfruprocee_particular_nuevo
integer x = 3442
integer y = 1776
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_recfruprocee_particular_nuevo
integer x = 3442
integer y = 72
end type

type pb_recupera from picturebutton within w_maed_recfruprocee_particular_nuevo
string tag = "Busca Recepciones Transmitidas"
integer x = 3447
integer y = 972
integer width = 155
integer height = 132
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo\Bmp\RESCATAE.BMP"
string disabledname = "\Desarrollo\Bmp\RESCATAD.BMP"
alignment htextalign = left!
end type

event clicked;Long ll_Null, ll_Fila

SetNull(ll_Null)

istr_busq.argum[1]	= istr_mant.argumento[3]	
istr_busq.argum[2]	= istr_mant.argumento[1]

istr_busq.argum[5]   = ""
istr_busq.argum[6]   = ""
istr_busq.argum[7]   = ""
istr_busq.argum[8]   = '1' /*Recepción Transmitida*/
dw_3.Reset()
dw_4.Reset()


OpenWithParm(w_busc_recfruprocee_trans, istr_busq)

istr_busq	       = Message.PowerObjectParm

IF istr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= istr_busq.argum[2]
	istr_mant.argumento[34]	= istr_busq.argum[5]
	istr_mant.argumento[3]	= istr_busq.argum[1]
	istr_mant.argumento[4]  = istr_busq.argum[7]
	istr_mant.argumento[33] = istr_busq.argum[8] 
	istr_mant.argumento[31] =	''
	dw_ptaori.Setfilter("plde_tipopl=1")
	dw_ptaori.Filter()
	CapturaRecepciones(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]),Long(istr_mant.argumento[34]))
ELSE
	istr_mant.argumento[33] = ''
	istr_mant.argumento[34] = ''
	
	pb_buscar.SetFocus()
END IF
end event

type pb_captura from picturebutton within w_maed_recfruprocee_particular_nuevo
integer x = 3447
integer y = 1152
integer width = 155
integer height = 132
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\buscaarc.bmp"
alignment htextalign = left!
end type

event clicked;String	ls_directorio, ls_archivo
Integer	li_valida = 0, li_opcion = 1, li_cliente, li_planta, li_estado
Long		ll_numero

dw_2.Reset()
dw_1.Reset()
FileClose(li_valida)

DO
	li_valida		= GetFileOpenName("Carga Archivo Plano de Transmisión de Recepciones", ls_directorio, ls_archivo, "", &
										"Cajas (*.TXT), (*.TXT),Todos los Archivos (*.*), *.*")

IF li_valida = 0 THEN
		pb_salir.SetFocus()
		dw_2.InsertRow(0)
		RETURN
	ELSEIF li_valida = -1 THEN
		MessageBox("Error de Apertura","Ocurrió un error al utilizar el archivo",Information!,Ok!)
		Message.DoubleParm = 1
	ELSE
		is_archivo        = ls_directorio
		ls_archivo			= ls_directorio
		
		Message.DoubleParm = 2
		Parent.TriggerEvent("ue_cargarchivoplano")
		
		ll_Numero 	= dw_3.Object.rfpe_numero[1] 
		
		SELECT  rfpe_estado 
		INTO :li_estado
		FROM dba.recfruprocee_trans
		WHERE clie_codigo = :ii_Cliente AND  
			plde_codigo = :ii_Planta  AND  
			rfpe_numero = :ll_Numero ;	
	
		IF Isnull(li_estado) OR  li_estado <> 2 THEN

		//	existe_cargaregistro()
			IF wf_actualiza_Trans(False) THEN
				istr_mant.argumento[33] = '1'
				FileClose(li_valida)
			ELSE
				MessageBox("Error de Captura","Carga de Pallet ya fue Realizada, Verifique",Information!,Ok!)
				Message.DoubleParm = -1
				FileClose(li_valida)
				Parent.TriggerEvent("ue_nuevo")
			END IF
							
			IF Message.DoubleParm = 2 THEN 
				MessageBox("Atención", "No se cargó archivo exitosamente," + is_mensaje + ".", StopSign!, Ok!)
				FileClose(li_valida)
			END IF
	ELSE
		MessageBox("Error de Captura","Carga de Pallet ya fue Realizada, Verifique",Information!,Ok!)
			Message.DoubleParm = -1
			FileClose(li_valida)
			Parent.TriggerEvent("ue_nuevo")
			
	END IF	
			
END IF
	
LOOP WHILE Message.DoubleParm = 1
end event

type dw_3 from datawindow within w_maed_recfruprocee_particular_nuevo
boolean visible = false
integer y = 2048
integer width = 352
integer height = 296
integer taborder = 100
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_recfruprocee_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_maed_recfruprocee_particular_nuevo
boolean visible = false
integer x = 425
integer y = 2056
integer width = 206
integer height = 204
integer taborder = 110
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_recfruproced_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_maed_recfruprocee_particular_nuevo
boolean visible = false
integer x = 667
integer y = 2020
integer width = 306
integer height = 224
integer taborder = 120
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_palletencab_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_6 from datawindow within w_maed_recfruprocee_particular_nuevo
boolean visible = false
integer x = 1038
integer y = 2040
integer width = 279
integer height = 228
integer taborder = 130
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_palletfruta_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_7 from datawindow within w_maed_recfruprocee_particular_nuevo
boolean visible = false
integer x = 1330
integer y = 2088
integer width = 219
integer height = 136
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_receprodenca"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_8 from datawindow within w_maed_recfruprocee_particular_nuevo
boolean visible = false
integer x = 1600
integer y = 2088
integer width = 219
integer height = 136
integer taborder = 150
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_receproddeta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_9 from datawindow within w_maed_recfruprocee_particular_nuevo
boolean visible = false
integer x = 1856
integer y = 2088
integer width = 219
integer height = 136
integer taborder = 160
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_exismovtoenca"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_11 from datawindow within w_maed_recfruprocee_particular_nuevo
boolean visible = false
integer x = 2569
integer y = 2012
integer width = 302
integer height = 232
integer taborder = 160
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspec_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_12 from datawindow within w_maed_recfruprocee_particular_nuevo
boolean visible = false
integer x = 2880
integer y = 2068
integer width = 261
integer height = 204
integer taborder = 170
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspecdet_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_13 from datawindow within w_maed_recfruprocee_particular_nuevo
boolean visible = false
integer x = 3182
integer y = 2060
integer width = 334
integer height = 184
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_palletfruta"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_14 from datawindow within w_maed_recfruprocee_particular_nuevo
boolean visible = false
integer x = 1513
integer y = 2040
integer width = 279
integer height = 228
integer taborder = 170
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_cajasprod_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_10 from datawindow within w_maed_recfruprocee_particular_nuevo
boolean visible = false
integer x = 1833
integer y = 2416
integer width = 251
integer height = 124
integer taborder = 160
boolean bringtotop = true
string title = "none"
string dataobject = "dw_rebajeexistenciarecepcion"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_15 from datawindow within w_maed_recfruprocee_particular_nuevo
boolean visible = false
integer x = 2002
integer y = 1872
integer width = 686
integer height = 400
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_cajasprod"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_16 from datawindow within w_maed_recfruprocee_particular_nuevo
boolean visible = false
integer x = 453
integer y = 1492
integer width = 686
integer height = 400
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_17 from datawindow within w_maed_recfruprocee_particular_nuevo
event add_row pbm_custom20
event del_all_rows pbm_custom18
event del_row pbm_custom19
integer x = 37
integer y = 652
integer width = 3333
integer height = 708
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_palletencab_nuevo"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event add_row;Long		Row

IF Not ib_Allow_Inserts THEN
	RETURN 0
END IF

Row	=	This.InsertRow(0)

This.ScrollToRow(Row)
This.SetRow(Row)
This.SetFocus()

RETURN 0
end event

event del_all_rows;Int		Cnt
Long		Row

IF Not ib_Allow_Updates THEN RETURN -1

Cnt	=	This.RowCount()

FOR Row = Cnt TO 1 Step -1
	This.DeleteRow(Row)
NEXT

RETURN 0
end event

event del_row;/*
Borra la Fila actual, pero chequea primero
*/
Long		Row
Window	Win

IF Not ib_Allow_Updates THEN RETURN -1

Win	=	Parent
Row	=	This.GetRow()

IF Row <= 0 THEN
	MessageBox(Win.Title, "No hay registro para borrar.")
	RETURN -1
END IF

IF MessageBox(Win.Title, "Para borrar el registro actual seleccione " + &
					"~nCancelar para aborta.", Question!, OkCancel!, 1) <> 1 THEN 
	RETURN -1
ELSE
	This.DeleteRow(Row)
END IF
end event

event buttonclicked;String	ls_Columna
Long		ll_fila, ll_fila_enc, ll_numero, ll_fila_det, fila_find

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "b_primero"
		fila_find = dw_18.RowCount()
		IF fila_find > 0 THEN
			ll_numero = dw_18.Object.paen_numero[1]
			ll_fila_enc = dw_17.Retrieve(Integer(istr_mant.argumento[3]),ll_numero,Integer(istr_mant.argumento[1]))
			ll_fila_det	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),ll_numero,Integer(istr_mant.argumento[1]))
		END IF
	CASE "b_anterior"
		fila_find =  dw_18.Find("paen_numero = " + String(dw_17.Object.paen_numero[1]),&
				1, dw_18.RowCount())
				
		IF fila_find > 1 THEN
			ll_fila = fila_find - 1 
			ll_numero = dw_18.Object.paen_numero[ll_fila]
			ll_fila_enc = dw_17.Retrieve(Integer(istr_mant.argumento[3]),ll_numero,Integer(istr_mant.argumento[1]))
			ll_fila_det	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),ll_numero,Integer(istr_mant.argumento[1]))
		END IF
		
	CASE "b_siguiente"
		fila_find =  dw_18.Find("paen_numero = " + String(dw_17.Object.paen_numero[1]),&
				1, dw_18.RowCount())
		IF fila_find < ii_evento THEN
			ll_fila = fila_find + 1 
			ll_numero = dw_18.Object.paen_numero[ll_fila]
			ll_fila_enc = dw_17.Retrieve(Integer(istr_mant.argumento[3]),ll_numero,Integer(istr_mant.argumento[1]))
			ll_fila_det	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),ll_numero,Integer(istr_mant.argumento[1]))
		END IF
		
	CASE "b_ultimo"
		fila_find = dw_18.RowCount()
		IF fila_find > 0 THEN
			ll_numero = dw_18.Object.paen_numero[dw_18.RowCount()]
			ll_fila_enc = dw_17.Retrieve(Integer(istr_mant.argumento[3]),ll_numero,Integer(istr_mant.argumento[1]))
			ll_fila_det	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),ll_numero,Integer(istr_mant.argumento[1]))
		END IF
		
END CHOOSE
end event

event itemchanged;Long		ll_null
String	ls_columna, ls_asda

SetNull(ll_null)
ls_columna = GetColumnName()

CHOOSE CASE ls_columna

	CASE "paen_numero"
				
		IF Duplicado(Data) THEN
			dw_17.SetItem(1, "paen_numero", ll_null)
			RETURN 1
		ELSE
			istr_mant.argumento[17] = String(Data)
		END IF
		
	CASE "vari_codigo"
		istr_mant.argumento[15] = String(Data)
		
	CASE "espe_codigo"
		istr_mant.argumento[14] = String(Data)
	
	CASE "Etiq_codigo"
		istr_mant.argumento[16] = String(Data)
		
	CASE "cond_codigo"
		istr_mant.argumento[18] = String(Data)
		
	CASE "rfpe_ptaori"
		istr_mant.argumento[19] = String(Data)	
	
	CASE "puer_codigo"
		IF Isnull(istr_mant.argumento[19]) THEN
			istr_mant.argumento[19] = String(Data)	
		END IF	
		
END CHOOSE


IF ii_yaexiste = 1 THEN
	ii_yaexiste = 0
	dw_2.SetItem(1, "paen_numero", ll_null)
	RETURN 1
END IF	


end event

event clicked;IF dw_17.Object.paen_pmixto[1]	=	1 AND Integer(dw_17.Object.paen_pmixto.Protect)	=	1 THEN RETURN

CHOOSE CASE dwo.name
		
	CASE "buscavariedad"
		buscavariedad()
		
	CASE "buscaembalaje"
		buscaembalaje()
		
END CHOOSE
end event

event itemerror;RETURN 1
end event

event losefocus;AcceptText()

RETURN 0
end event

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

ls_Mensaje	=	"Error " + ls_Tipo + " registro " + String(row)
ls_Mensaje	+=	"~r~nNúmero de Error Base Datos: " + String(SqlDbCode)
ls_Mensaje	+=	"~r~nMensaje de Error Base Datos:~r~n~r~n" + SqlErrText

lstr_ErrBD.MensajePantalla	=	ls_Mensaje

OpenWithParm(w_ErrorBaseDatos, lstr_ErrBD)

This.SetFocus()
This.SetRow(row)
This.ScrollToRow(row)

RETURN 1
end event

event sqlpreview;DwItemStatus	Estado

IF Row = 0 THEN RETURN 0

CHOOSE CASE Buffer
	CASE Delete!
		This.TriggerEvent("on_delete")
		
	CASE Primary!, Filter!
		Estado	=	This.GetItemStatus(Row, 0, Buffer)
		
		CHOOSE CASE Estado
			CASE New!, NewModified!
				This.TriggerEvent("on_insert")
				
			CASE DataModified!
				This.TriggerEvent("on_update")
				
		END CHOOSE
END CHOOSE
	
IF Message.ReturnValue = 1 THEN
	RETURN 1
ELSE
	RETURN 0
END IF
end event

type dw_18 from datawindow within w_maed_recfruprocee_particular_nuevo
integer x = 1248
integer y = 1388
integer width = 1486
integer height = 588
integer taborder = 60
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_recfruproced"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
end type

