$PBExportHeader$w_maed_recfruprocee_particular_palletenc.srw
forward
global type w_maed_recfruprocee_particular_palletenc from w_mant_encab_deta_csd
end type
type pb_recupera from picturebutton within w_maed_recfruprocee_particular_palletenc
end type
type pb_captura from picturebutton within w_maed_recfruprocee_particular_palletenc
end type
type dw_3 from datawindow within w_maed_recfruprocee_particular_palletenc
end type
type dw_4 from datawindow within w_maed_recfruprocee_particular_palletenc
end type
type dw_5 from datawindow within w_maed_recfruprocee_particular_palletenc
end type
type dw_6 from datawindow within w_maed_recfruprocee_particular_palletenc
end type
type dw_7 from datawindow within w_maed_recfruprocee_particular_palletenc
end type
type dw_8 from datawindow within w_maed_recfruprocee_particular_palletenc
end type
type dw_9 from datawindow within w_maed_recfruprocee_particular_palletenc
end type
type dw_10 from datawindow within w_maed_recfruprocee_particular_palletenc
end type
type dw_11 from datawindow within w_maed_recfruprocee_particular_palletenc
end type
type dw_12 from datawindow within w_maed_recfruprocee_particular_palletenc
end type
type tab_palletenc from tab within w_maed_recfruprocee_particular_palletenc
end type
type tp_recepcion from userobject within tab_palletenc
end type
type dw_2_share from uo_dw within tp_recepcion
end type
type tp_recepcion from userobject within tab_palletenc
dw_2_share dw_2_share
end type
type tab_palletenc from tab within w_maed_recfruprocee_particular_palletenc
tp_recepcion tp_recepcion
end type
type dw_55 from uo_dw within w_maed_recfruprocee_particular_palletenc
end type
type dw_44 from uo_dw within w_maed_recfruprocee_particular_palletenc
end type
type dw_33 from uo_dw within w_maed_recfruprocee_particular_palletenc
end type
type dw_palletfruta from uo_dw within w_maed_recfruprocee_particular_palletenc
end type
type dw_13 from uo_dw within w_maed_recfruprocee_particular_palletenc
end type
end forward

global type w_maed_recfruprocee_particular_palletenc from w_mant_encab_deta_csd
integer width = 3707
integer height = 2860
string title = "RECEPCION DE PALLETS"
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowstate windowstate = maximized!
event ue_imprimir ( )
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
dw_10 dw_10
dw_11 dw_11
dw_12 dw_12
tab_palletenc tab_palletenc
dw_55 dw_55
dw_44 dw_44
dw_33 dw_33
dw_palletfruta dw_palletfruta
dw_13 dw_13
end type
global w_maed_recfruprocee_particular_palletenc w_maed_recfruprocee_particular_palletenc

type variables
w_mant_deta_palletfruta			iw_mantencion

DataWindowChild	dw_puerto, dw_planta, dw_ptaori, dw_fruta
Integer 	ii_recepcion
Boolean	ib_existe_folio, ib_primera_entrada, ib_encabezado_activo
Long     il_pallet, il_folio
Date     id_mespro 
String   is_Archivo, is_mensaje

Transaction		sqlexis

Boolean	ib_ConectadoExistencia

Str_mant					istr_mant2
Datastore 				ids_palletfruta[]
Long						il_paen_numero[]		
uo_palletencab			iuo_palletencab[]

uo_pallet					iuo_pallet
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
public subroutine cuentacajas ()
public function boolean agrega_palletencab ()
public subroutine carga_istr ()
public subroutine limpiatab ()
public function boolean grabapallets ()
public function integer grabapalletfruta ()
end prototypes

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

event ue_validaregistro();Integer	li_cont, li_cont1, li_cont2
String	ls_mensaje, ls_colu[], ls_frio,ls_tipo
Long     ll_Fila,ll_Fildet

IF dw_1.RowCount() >0 THEN
	
	FOR ll_Fila = 1 to dw_1.RowCount()
		IF iuo_pallet.existe(dw_1.Object.clie_codigo[ll_fila],dw_1.Object.paen_numero[ll_fila],False,sqlca) THEN
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
	
	buscabodega(li_bodega)
	
	IF  dw_10.Object.paen_fecemb[1] >= id_mespro THEN
	
		dw_7.InsertRow(0)
		
		dw_7.Object.repe_tipori[1]   = dw_2.Object.rfpe_tipoen[1]
		dw_7.Object.bode_codigo[1]   = dw_2.Object.rfpe_ptaori[1]
			
			
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
			
					ls_emba									=	dw_10.Object.emba_codigo[ll_fila]
					
					IF li_TipoPallet = 1 THEN
						dw_8.Object.repd_cajpal[ll_Fildet]	=	dw_10.Object.paen_ccajas[ll_fila]
					ELSE
						ll_cantidad								=	CajasPallet(ls_emba)
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
    MessageBox("Error de Consistencia", "Fecha no correponde a mes de proceso, No se realizara Rebaje por Producción.", StopSign!, Ok!)
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
				li_clie1,li_secu1,li_dest1
Date        ld_fecrec, ld_fecact,ld_fecemb,ld_feccos,ld_fecini,ld_fechaemb,ld_fechai,ld_fechaa,ld_frecha,ld_fechaing
String      ls_mensaje, ls_registro, ls_embalaje, ls_calibre,ls_patent,ls_chofer,ls_tpemcod,ls_frio,ls_nrasda,ls_emba,&
				ls_calib,ls_calrot,ls_emb,ls_tpem,ls_calidad, ls_copa
Long			ll_guides,ll_fildet,ll_numpal,ll_filas,ll_secuen,ll_fila,ll_pallet,ll_cajas,ll_productor,ll_filpal,ll_pallet1,ll_correl,&
				ll_prod,ll_totcaja,ll_prdrot,ll_huerto1,ll_cuarte1,ll_filpafr,ll_pal1,ll_nroanu,ll_Filinp,ll_Filadet,ll_numero,ll_numero1
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
			ld_fechaing 	 =	Date(String(Mid(ls_Registro, 93, 2)+'-'+Mid(ls_Registro,95, 2)+'-'+Mid(ls_Registro, 97, 4) ))
			
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
tab_palletenc.tp_recepcion.dw_2_share.AcceptText()

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

public function long buscanuevofolio (integer cliente, integer planta);Integer	li_planta, li_cliente
Long		ll_numero
Boolean	lb_nulo

//li_cliente	=	cliente	
li_planta	=	planta

SELECT Max(rfpe_numero)
  INTO :ll_numero
  FROM DBA.RECFRUPROCEE
 WHERE plde_codigo = :li_planta;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla RecFruProcee")
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
	dw_2.Modify("clie_codigo.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("rfpe_numero.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.SetColumn("rfpe_numero")
	dw_2.SetFocus()
ELSE
	dw_2.SetTabOrder("clie_codigo",0)
	dw_2.SetTabOrder("rfpe_numero",0)
	dw_2.SetTabOrder("plde_codigo",0)
	dw_2.Modify("clie_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_numero.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(RGB(166,180,210)))

END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update() = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
	ELSEIF dw_2.Update() = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	ELSE
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
		ELSE
			lb_Retorno	=	True
		END IF
	END IF
ELSE
	IF dw_2.Update() = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
	ELSEIF dw_1.Update() = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	ELSE
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
	END IF
END IF

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
	
	   /* Rebaja cantidad de cajas en tabla ctlcallotes
				RebajaTamlot(pallet)
	
		DELETE FROM dba.palletfruta  
		WHERE ( clie_codigo	=	:li_cliente ) AND  
				( plde_codigo 	= 	:li_planta ) AND  
				( paen_numero 	= 	:pallet );
				
		IF SQLCA.sqlcode	=	0 then
				COMMIT;*/
				
//				DELETE FROM dba.palletencab
//				WHERE ( clie_codigo	=	:li_cliente ) AND  
//						( plde_codigo 	=	:li_planta ) AND  
//						( paen_numero 	=	:pallet );
//				IF SQLCA.sqlcode	=	0 THEN
//					MessageBox( "Eliminar"," Pallet "+ String(pallet) + " Eliminado "&
//					+" Correctamente!")
//					COMMIT;
//				ELSE 
//					MessageBox( "Error", "No se Pudo Eliminar el Pallet "& 
//					+"valida.")
//					ROLLBACK;
//				END IF
		//ELSE
			//MessageBox( "Error", &
			//"No se Pudo Eliminar el Pallet "& 
					//+"valida.")
					//ROLLBACK;		
		//END IF

ELSE
	MessageBox( "Error", "Pallet con Relación en Otras Tablas, Solo Elimina en Recepción.")	
	
END IF

RETURN
end subroutine

public function boolean conexionexistencia ();String	ls_Nombre, ls_Usuario, ls_Password

IF ib_ConectadoExistencia THEN
	DISCONNECT USING sqlexis;
END IF

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass
ls_nombre				=	ProfileString(gstr_apl.ini, gs_Base, "NombreOdbcExisten", "")
sqlexis.Dbms			=	sqlca.Dbms
sqlexis.ServerName	=	sqlca.ServerName
sqlexis.DataBase		=	ProFileString(gstr_apl.ini, gs_base, "DatabaseExisten", "Existenc")
sqlexis.DbParm			= "Connectstring='DSN=" + ls_nombre + ";UID=" + ls_Usuario + &
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

li_bodega	=	bodega

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
  
  SELECT det.repd_nropal
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

public subroutine capturarecepciones (integer ai_cliente, integer ai_planta, long al_numero);Long ll_Fila_e, ll_Fila_d, ll_Fila, ll_Null, respuesta

SetNull(ll_Null)

dw_1.Reset() 
dw_2.Reset()
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
   		dw_4.RowsCopy(1, dw_4.RowCount(), Primary!, dw_1, 1, Primary!)
			FOR ll_Fila = 1 TO dw_1.RowCount()
				 dw_1.Object.rfpe_numero[ll_Fila] = ll_Null
			NEXT
	   END IF

	   dw_3.RowsCopy(1, dw_3.RowCount(), Primary!, dw_2, 1, Primary!)
	   dw_2.Object.rfpe_numero[1] = ll_Null

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
   WHERE frre_codigo = :fruta   ;

		
RETURN ls_descri

end function

public subroutine cuentacajas ();Long	li_fila, li_cajas_dwpalletfruta

istr_mant.Argumento[11]	= string(iuo_palletencab[tab_palletenc.selectedTab - 1].retornacajas())

For li_fila = 1 to dw_palletfruta.RowCount()
//	li_cajas_dw1					=	dw_1.GetitemNumber(li_fila, "pafr_ccajas")
	li_cajas_dwpalletfruta					=	dw_palletfruta.GetitemNumber(li_fila, "pafr_ccajas")
	istr_mant.argumento[11]     =	String(Long(istr_mant.argumento[11]) - li_cajas_dwpalletfruta)
Next

IF	Long(istr_mant.argumento[11])<0 THEN  istr_mant.argumento[11]='0'

RETURN
end subroutine

public function boolean agrega_palletencab ();Boolean lb_Creado
Integer li_filaspae, li_filaspaf, li_filas, li_cliente, li_planta
Long ll_paen_numero, ll_palletencontrado

IF istr_mant2.argumento[30] = '1' THEN//agrega uno nuevo en blanco.

	li_filaspae 			= 	UpperBound(il_paen_numero)
	li_cliente				= 	Integer(istr_mant2.argumento[1])
	li_planta				= 	Integer(istr_mant2.argumento[2])
	ll_paen_numero 	= 	Long(istr_mant2.argumento[3])	
	FOR li_filas =  1 to li_filaspae
		IF ll_paen_numero = 	il_paen_numero[li_filas] THEN
			lb_Creado = true
		END IF
	NEXT
	
	SELECT paen_numero
	INTO :ll_palletencontrado
	FROM dba.palletencab
	WHERE plde_codigo 	=: li_planta and
			clie_codigo 		=: li_cliente and
			paen_numero 	=: ll_paen_numero;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura Tabla palletencab")
		Return False
	ELSEIF sqlca.SQLCode = 0 THEN
		lb_Creado = TRUE
	END IF
	
	IF lb_Creado THEN 
		Messagebox('Error', 'La tarja ' + istr_mant2.argumento[3] + ' ya ha sido ingresada')
		Return NOT lb_Creado
	END IF
	
	li_filaspae = li_filaspae + 1
	
	il_paen_numero[li_filaspae]	 					= 	ll_paen_numero
	iuo_palletencab[li_filaspae] 						= 	Create uo_palletencab
	iuo_palletencab[li_filaspae].Text				=	String(ll_paen_numero) 
	
	ids_palletfruta[li_filaspae]						= 	Create DataStore
	ids_palletfruta[li_filaspae].dataObject 		= 	'dw_mues_palletfruta'
	ids_palletfruta[li_filaspae].ShareData(dw_palletfruta)
	
	tab_palletenc.OpenTabWithParm(iuo_palletencab[li_filaspae], istr_mant2, 0)
	tab_palletenc.Control[li_filaspae + 1].Text	=	String(ll_paen_numero)
	
	iuo_palletencab[li_filaspae].dw_1.InsertRow(0)
	tab_palletenc.SelectTab(li_filaspae + 1)
	iuo_palletencab[li_filaspae].dw_1.SetFocus()
	dw_palletfruta.Title = ''
	
	li_filas 	=	dw_1.InsertRow(0)
	dw_1.Object.plde_codigo[li_filas] 				= 	li_planta
	dw_1.Object.clie_codigo[li_filas] 				= 	li_cliente
//	dw_1.Object.rfpe_numero[li_filas] 			= 	li_rfpenumero
	dw_1.Object.paen_numero[li_filas] 			= 	ll_paen_numero
	
ELSE //agrega un nuevo pallet desde la dw donde estan creados.
	
	LimpiaTab()
	
	FOR li_filaspae = 1 to dw_1.RowCount()
		li_cliente												= 	dw_1.Object.clie_codigo[li_filaspae]
		li_planta												= 	dw_1.Object.plde_codigo[li_filaspae]
		ll_paen_numero 									= 	dw_1.Object.paen_numero[li_filaspae]
		
		istr_mant2.Argumento [1]						=	String(li_cliente)
		istr_mant2.Argumento [2]						=	String(li_planta)
		istr_mant2.Argumento [3]						=	String(ll_paen_numero)
		
		il_paen_numero[li_filaspae]	 					= 	ll_paen_numero
		iuo_palletencab[li_filaspae] 						= 	Create uo_palletencab
		iuo_palletencab[li_filaspae].Text				=	String(ll_paen_numero) 
		
		ids_palletfruta[li_filaspae]						= 	Create DataStore
		ids_palletfruta[li_filaspae].dataObject 		= 	'dw_mues_palletfruta'
		ids_palletfruta[li_filaspae].SetTransObject(sqlca)
		ids_palletfruta[li_filaspae].Retrieve(li_cliente, ll_paen_numero, li_planta)
		ids_palletfruta[li_filaspae].ShareData(dw_palletfruta)

		tab_palletenc.OpenTabWithParm(iuo_palletencab[li_filaspae], istr_mant2, 0)
		tab_palletenc.Control[li_filaspae + 1].Text	=	String(ll_paen_numero)
		
		iuo_palletencab[li_filaspae].dw_1.SetTransObject(sqlca)
		iuo_palletencab[li_filaspae].dw_1.Retrieve(li_cliente, ll_paen_numero, li_planta)		
		iuo_palletencab[li_filaspae].dw_1.Object.Paen_numero[1] = ll_paen_numero
		
		tab_palletenc.SelectTab(li_filaspae + 1)
		iuo_palletencab[li_filaspae].dw_1.SetFocus()
		dw_palletfruta.Title = ''
	NEXT
END IF

RETURN TRUE
end function

public subroutine carga_istr ();istr_mant3 = iuo_palletencab[tab_palletenc.selectedTab - 1].retornaestructura()
end subroutine

public subroutine limpiatab ();Integer li_filaspae

IF UpperBound(iuo_palletencab) > 0 THEN
	tab_palletenc.SetRedraw(FALSE)
	For li_filaspae = UpperBound(iuo_palletencab) to 1 STEP -1
		tab_palletenc.CloseTab(iuo_palletencab[li_filaspae])
		SetNull(il_paen_numero[li_filaspae])
		Destroy ids_palletfruta[li_filaspae]
		Destroy iuo_palletencab[li_filaspae]
	NEXT
	tab_palletenc.SetRedraw(TRUE)
END IF
end subroutine

public function boolean grabapallets ();Integer 	li_filas, li_Retrieve_palletencab, li_Retrieve_palletfruta, li_Update_iuo, li_Update_ids, li_rows, li_descuentanulls
Boolean	lb_retorno
String 	ls_PalletSinDetalle

lb_retorno = FALSE

//Validar que cada encab tenga un deta.

ls_PalletSinDetalle 			= 	''
FOR li_filas = 1 to UpperBound(ids_palletfruta)
	IF NOT IsNull(ids_palletfruta[li_filas]) THEN
		IF ids_palletfruta[li_filas].RowCount() < 1 THEN
			ls_PalletSinDetalle 	= 	ls_PalletSinDetalle + ',~r' + String(il_paen_numero[li_filas]) 
		END IF
	END IF
NEXT

IF LEN(ls_PalletSinDetalle) > 0 THEN
	ls_PalletSinDetalle	= 	RIGHT(ls_PalletSinDetalle, LEN(ls_PalletSinDetalle) - 2)
	MessageBox("Error","Falta el detalle de los siguientes Pallets:~r" + ls_PalletSinDetalle, Exclamation!)
	Return lb_retorno
END IF

//Realizar Update para todos los pallets
FOR li_filas = 1 to UpperBound(ids_palletfruta)
	IF NOT IsNull(ids_palletfruta[li_filas]) THEN
		iuo_Palletencab[li_filas].dw_1.SetTransObject(SQLCA)
		li_Retrieve_PalletEncab = iuo_Palletencab[li_filas].dw_1.Update(True, False)
		
		IF li_Retrieve_PalletEncab = 1 THEN
			li_Update_iuo ++
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
		END IF
		
		dw_13.SetTransObject(SQLCA)
		ids_palletfruta[li_filas].ShareData(dw_13)
		li_Retrieve_PalletFruta =	GrabaPalletFruta()
		
		IF li_Retrieve_PalletFruta = 1 THEN 
			li_Update_ids ++
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
		END IF
	ELSE
		li_descuentanulls ++
	END IF
NEXT

//Validar que se hayan hecho todos los updates.
IF li_update_iuo = (UpperBound(ids_palletfruta) - li_descuentanulls) THEN
	IF li_update_ids = (UpperBound(ids_palletfruta) - li_descuentanulls) THEN
		
		FOR li_rows = 1 TO UpperBound(ids_palletFruta)
			ids_palletFruta[li_rows].ResetUpdate()
			iuo_Palletencab[li_rows].dw_1.ResetUpdate()
		NEXT
		
		lb_retorno 	=	TRUE
		COMMIT;
	ELSE
		MessageBox("Error", "No se Grabaron todos los detalles de los pallets", StopSign!)
		ROLLBACK;
	END IF
ELSE
	MessageBox("Error", "No se Grabaron todos los encabezados de los pallets", StopSign!)
	ROLLBACK;
END IF

Return lb_retorno
end function

public function integer grabapalletfruta ();Boolean 				lb_retorno
Integer				li_filas, li_existe, li_Correctos
Long  					li_clie_codigo, li_paen_numero, li_espe_codigo, li_vari_codigo, li_prod_codigo, li_cond_codigo, li_etiq_codigo, li_plde_codigo
Long					li_pafr_secuen, li_pafr_ccajas, li_pafr_nrlote, li_pafr_copack, li_pafr_varrot, li_pafr_prdrot, li_pafr_huert1, li_pafr_cuart1
String 				ls_emba_codigo, ls_pafr_calibr, ls_pafr_calrot
Date					ld_pafr_fecemb, ld_pafr_fecing
dwItemStatus		ldwis_estado


FOR li_filas = 1 to dw_13.RowCount()

	ldwis_estado = dw_13.GetItemStatus(li_filas, 0, Primary!)
	IF ldwis_estado = NotModified! Then
		li_Correctos ++
	ELSE
		li_clie_codigo		=	dw_13.Object.clie_codigo[li_filas]
		li_paen_numero	=	dw_13.Object.paen_numero[li_filas]
		li_espe_codigo		=	dw_13.Object.espe_codigo[li_filas]
		li_vari_codigo		=	dw_13.Object.vari_codigo[li_filas]
		li_prod_codigo		=	dw_13.Object.prod_codigo[li_filas]
		li_cond_codigo		=	dw_13.Object.cond_codigo[li_filas]
		li_etiq_codigo		=	dw_13.Object.etiq_codigo[li_filas]
		li_plde_codigo		=	dw_13.Object.plde_codigo[li_filas]
		li_pafr_secuen		=	dw_13.Object.pafr_secuen[li_filas]	
		li_pafr_ccajas		=	dw_13.Object.pafr_ccajas[li_filas]
		li_pafr_nrlote		=	dw_13.Object.pafr_nrlote[li_filas]
		li_pafr_copack		=	dw_13.Object.pafr_copack[li_filas]
		li_pafr_varrot		=	dw_13.Object.pafr_varrot[li_filas]
		li_pafr_prdrot		=	dw_13.Object.pafr_prdrot[li_filas]
		li_pafr_huert1		=	dw_13.Object.pafr_huert1[li_filas]
		li_pafr_cuart1		=	dw_13.Object.pafr_cuart1[li_filas]
		ls_emba_codigo	=	dw_13.Object.emba_codigo[li_filas]
		ls_pafr_calibr		=	dw_13.Object.pafr_calibr[li_filas]
		ls_pafr_calrot		=	dw_13.Object.pafr_calrot[li_filas]
		ld_pafr_fecemb		=	dw_13.Object.pafr_fecemb[li_filas]
		
		ld_pafr_fecing		=	dw_13.Object.pafr_fecing[li_filas]
		// Para guardar la fecha de Ingreso cuando es Packing
		IF istr_mant.argumento[13] = '2' AND istr_mant.argumento[39] <> '' THEN
			 ld_pafr_fecing = Date(istr_mant2.argumento[39])
		END IF
		
		// Para guardar la fecha de Ingreso cuando traspaso Inperplanta
		IF istr_mant.argumento[13] = '1' AND istr_mant.argumento[39] <> '' THEN
			 IF Isnull(ld_pafr_fecing) OR ld_pafr_fecing = Date('1900-01-01') THEN 
				ld_pafr_fecing  = Date(istr_mant2.argumento[39])
			END IF
		END IF
	
		SELECT Max(pafr_secuen)
		INTO :li_existe
		FROM dba.palletFruta
		WHERE clie_codigo =: li_clie_codigo and
				plde_codigo =: li_plde_codigo and
				paen_numero =: li_paen_numero;
				
		IF sqlca.SQLCode = -1 THEN
			F_errorbasedatos(sqlca,"Lectura tabla PalletFruta")
		ELSEIF ( li_existe = 0 OR ISNull(li_existe) ) OR ( li_existe < li_pafr_secuen ) THEN
			INSERT INTO dba.palletfruta  
				( clie_codigo, paen_numero, espe_codigo, vari_codigo,   
				emba_codigo, prod_codigo, cond_codigo,   
				etiq_codigo, plde_codigo, pafr_calibr,   
				pafr_secuen, pafr_ccajas, pafr_nrlote,   
				pafr_copack, pafr_varrot, pafr_prdrot,   
				pafr_calrot, pafr_fecemb, pafr_huert1,   
				pafr_cuart1,pafr_fecing)  
			VALUES ( :li_clie_codigo, :li_paen_numero, :li_espe_codigo, :li_vari_codigo,   
				:ls_emba_codigo, :li_prod_codigo, :li_cond_codigo,   
				:li_etiq_codigo, :li_plde_codigo, :ls_pafr_calibr,   
				:li_pafr_secuen, :li_pafr_ccajas, :li_pafr_nrlote,   
				:li_pafr_copack, :li_pafr_varrot, :li_pafr_prdrot,   
				:ls_pafr_calrot, :ld_pafr_fecemb, :li_pafr_huert1,   
				:li_pafr_cuart1,:ld_pafr_fecing);
				
		ELSEIF li_existe >= li_pafr_secuen THEN
			
			UPDATE dba.palletfruta  
			SET clie_codigo 	= :li_clie_codigo,   
				paen_numero 	= :li_paen_numero,   
				espe_codigo 	= :li_espe_codigo,   
				vari_codigo	 	= :li_vari_codigo,   
				emba_codigo 	= :ls_emba_codigo,   
				prod_codigo 	= :li_prod_codigo,   
				cond_codigo 	=  :li_cond_codigo,   
				etiq_codigo 		= :li_etiq_codigo,   
				plde_codigo 	= :li_plde_codigo,   
				pafr_calibr 		= :ls_pafr_calibr,   
				pafr_secuen 	= :li_pafr_secuen,   
				pafr_ccajas 		= :li_pafr_ccajas,   
				pafr_nrlote 		= :li_pafr_nrlote,   
				pafr_copack 	= :li_pafr_copack,   
				pafr_varrot 		= :li_pafr_varrot,   
				pafr_prdrot 		= :li_pafr_prdrot,   
				pafr_calrot 		= :ls_pafr_calrot,   
				pafr_fecemb 	= :ld_pafr_fecemb,   
				pafr_huert1 	= :li_pafr_huert1,   
				pafr_cuart1 	= :li_pafr_cuart1,
				pafr_fecing		= :ld_pafr_fecing;
				
		END IF
		IF sqlca.SQLCode = -1 THEN
			F_errorbasedatos(sqlca,"Grabación en Tabla PalletFruta")
		ELSEIF sqlca.SQLCode = 0 THEN
			li_Correctos ++
		END IF
	END IF
NEXT
IF li_Correctos = dw_13.RowCount() THEN
	Return 1
ELSE
	Return -1
END IF
end function

event open;//	Argumentos Mantenedor
//	istr_mant.argumento[1]		= 	Código de Planta
//	istr_mant.argumento[2]		= 	Número de Folio Recepción
//	istr_mant.argumento[3]		= 	Código de Exportador
//	istr_mant.argumento[4]		= 	Cantidad de Tarjas Transitorias
//	istr_mant.argumento[5]		= 	Tipo de packing
// istr_mant.argumento[6]  		= 	Número de Pallet
// istr_mant.argumento[7]  		= 	Parámetro de solo consulta 1=consulta, "":otro
//	istr_mant.argumento[11]	= 	Cantidad de Tarjas Definitivas
// istr_mant.argumento[20] 	= 	Tipo de Recepción
// istr_mant.argumento[21]  	= 	Packing Origen
// istr_mant.argumento[30]    	=  Guia de despacho
// istr_mant.argumento[31]    	=  Tipo de Recepción 1=Packing, -1=Otro
// istr_mant.argumento[33]    	=  Recepción Transmitida
// istr_mant.argumento[34]    	=  Número Recepción Transmitida
// istr_mant.argumento[35]    	=  Permite determinar si el botón de grabar se activa desde ue_nuevo
// istr_mant.argumento[36]    	=  Permite determinar si el botón de grabar se activa desde ue_borra_detalle

Integer li_codigo


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
//This.Height	= 2020
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


istr_mant.dw				= dw_1
istr_mant.solo_consulta = False

buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"

istr_mant.argumento[1]		=	String(gi_codplanta)
istr_mant.argumento[3]		=	String(gi_codexport)
istr_mant2.argumento[1] 	= String(gi_codexport)
istr_mant2.argumento[2] 	=	String(gi_codplanta)
istr_mant.argumento[5]		=	'dw_mues_recfruproced'
istr_mant.argumento[6]		= 	'1'
istr_mant.argumento[13] 	=	'2'
istr_mant.argumento[20] 	=	'1'
istr_mant.argumento[21] 	=	''
istr_mant.argumento[24] 	=	''
istr_mant.argumento[4]  	=	'0'
istr_mant.argumento[11] 	=	'0'
istr_mant.argumento[31] 	=	'1'
istr_mant.argumento[32] 	=	''
istr_mant.argumento[33] 	=	''
istr_mant.argumento[34] 	=	''
istr_mant.argumento[35] 	=	'0'
istr_mant.argumento[36] 	=	'0'
istr_mant.argumento[39] =	''				// Tipo de Entrada


iuo_especie				=	CREATE	uo_especie				
iuo_variedades			=	CREATE	uo_variedades			
iuo_embalajesprod	=  CREATE	uo_embalajesprod		
iuo_etiquetas			=	CREATE	uo_etiquetas			
iuo_tipofrio				=	CREATE	uo_tipofrio					
iuo_status				=	CREATE	uo_status				
iuo_tipopallet			=	CREATE	uo_tipopallet			
iuo_condicion			=	CREATE	uo_condicion				
iuo_codigopallet		=	CREATE	uo_codigopallet			
iuo_destinos				=	CREATE	uo_destinos				
iuo_productores		=	CREATE	uo_productores				
iuo_calibre				=	CREATE	uo_calibre	
iuo_categoria			=	CREATE	uo_categoria
iuo_tratamiento		=	CREATE	uo_tratamiento
iuo_frutarecepcion		=	CREATE	uo_frutarecepcion		

sqlexis					=	CREATE Transaction

/* Si Código de parempresa es igual a 1 se efectua la transacción para REBAJE DE EXISTENCIA*/
  SELECT empr_codgen  
    INTO :li_codigo  
    FROM dba.parempresa  
	 USING sqlca;
	
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

dw_2.ShareData(tab_palletenc.tp_recepcion.dw_2_share)

IF gi_CodExport = 300 THEN tab_palletenc.tp_recepcion.dw_2_share.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

tab_palletenc.tp_recepcion.dw_2_share.GetChild("plde_codigo", dw_planta)
tab_palletenc.tp_recepcion.dw_2_share.GetChild("rfpe_ptaori", dw_ptaori)
tab_palletenc.tp_recepcion.dw_2_share.GetChild("puer_codigo", dw_puerto)

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

tab_palletenc.tp_recepcion.dw_2_share.GetChild("frre_codigo", dw_fruta)
dw_fruta.SetTransObject(sqlca)
dw_fruta.Retrieve()
tab_palletenc.tp_recepcion.dw_2_share.Object.frre_codigo[1] = 1

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							
//dw_2.Object.rfpe_tarjas[1] = 2
end event

event ue_borra_detalle;call super::ue_borra_detalle;Long		pallet
Integer 	li_tab, li_planta, li_cliente, li_recep

IF dw_1.rowcount() < 2 THEN
	dw_1.SetFocus()
	RETURN
END IF

li_tab	=	tab_palletenc.SelectedTab - 1
pallet	=	dw_1.getitemNumber(li_tab, "paen_numero")

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

//OpenWithParm(iw_mantencion, istr_mant)
//
//istr_mant = Message.PowerObjectParm
//
//IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		IF Messagebox('Borrar','Elimina Detalle de Pallet ?', question!, yesno!, 2) = 1 THEN
			istr_mant.argumento[36] = '0'
			//pb_grabar.TriggerEvent(Clicked!)
			This.TriggerEvent("ue_guardar")
			istr_mant.argumento[1]	=	String(tab_palletenc.tp_recepcion.dw_2_share.Object.plde_codigo[1])
			istr_mant.argumento[2]	=	String(tab_palletenc.tp_recepcion.dw_2_share.Object.rfpe_numero[1])
			istr_mant.argumento[3]	=	String(tab_palletenc.tp_recepcion.dw_2_share.Object.clie_codigo[1])
			pb_nuevo.TriggerEvent(Clicked!)
			This.TriggerEvent("ue_recuperadatos")
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
//END IF
//
istr_mant.borra	 = False
end event

event ue_nuevo_detalle;Long			il_tarjas, il_tardef
Boolean 		lb_Respuesta
date 			fecha
Integer		li_i

il_tarjas	=	dw_2.Object.rfpe_tarjas[1]
il_tardef	=	dw_2.Object.rfpe_tardef[1]

IF IsNull(il_tarjas) THEN il_tarjas = 0
IF IsNull(il_tardef) THEN il_tardef = 0

IF dw_palletfruta.Title <> 'Detalle del pallet'  THEN//Agrega Pallets
	IF dw_1.RowCount() >= (il_tarjas + il_tardef) THEN
		MessageBox("Atención", "No puede ingresar más Tarjas.")	
	ELSE
		Cuentatarjas()
		istr_mant.agrega	=	False
		istr_mant.argumento[6]=""
		
		istr_mant2.dw = dw_1
		
		lb_respuesta = FALSE
		
		DO WHILE NOT lb_respuesta
			
			OpenWithParm(w_mues_pidepallet, istr_mant2)
			istr_mant2 = Message.PowerObjectParm	
			
			IF istr_mant2.Respuesta = 1 THEN
				istr_mant2.argumento[30] 	=	'1'
				lb_respuesta = Agrega_Palletencab()
			ELSE
				lb_respuesta = TRUE
			END IF
			
		LOOP
		//
		
		IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
		
		IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
			pb_eliminar.Enabled	= TRUE
			pb_grabar.Enabled		= TRUE	
		END IF
		
		dw_1.SetRow(il_fila)
		dw_1.SelectRow(il_fila,True)
		
	END IF
ELSE
	IF 	tab_palletenc.SelectedTab = 1 THEN
		MessageBox("Error", "No se puede Ingresar directamente el detalle de la Recepción.~rSolo ingrese el detalle de los Pallets", Exclamation!)
	ELSE
		fecha								=	(iuo_palletencab[tab_palletenc.SelectedTab - 1].dw_1.Object.paen_fecemb[1])
	
		istr_mant.borra					=	FALSE
		istr_mant.agrega				=	TRUE
		
		CuentaCajas()
		IF Integer(istr_mant.argumento[11]) = 0 THEN
			MessageBox("Error", "Ya se han ingresado todas las cajas para este pallet", Exclamation!)
		ELSE
			carga_istr()
			istr_mant3.argumento[16]	=	'2'
			ids_palletfruta[tab_palletenc.SelectedTab - 1].ShareData(dw_palletfruta)
			istr_mant3.dw 					=	dw_palletfruta
			istr_mant3.agrega				= 	TRUE
			istr_mant3.solo_consulta		= 	FALSE
			istr_mant3.argumento[21] 	=	String(iuo_palletencab[tab_palletenc.SelectedTab - 1].dw_1.Object.plde_codigo[1])
			
			IF istr_mant.argumento[13]		=	'1'	OR istr_mant.argumento[13] 	=	'2'  THEN
				istr_mant.argumento[39] 	=	String(dw_2.Object.rfpe_fecrec[1])
			END IF
			
			OpenWithParm(iw_mantencion, istr_mant3)
			
			IF dw_palletfruta.RowCount() > 0 THEN HabilitaEncab(False)
			
			FOR li_i = 1 TO dw_palletfruta.RowCount()
				dw_PalletFruta.Object.pafr_secuen[li_i] 	= 	li_i
				dw_PalletFruta.Object.pafr_copack[li_i] 	= 	tab_palletenc.tp_recepcion.dw_2_share.Object.rfpe_ptaori[1]
				dw_PalletFruta.Object.pafr_varrot[li_i]	=	dw_PalletFruta.Object.vari_codigo[li_i]
				dw_PalletFruta.Object.pafr_prdrot[li_i]	=	dw_PalletFruta.Object.prod_codigo[li_i]
				dw_PalletFruta.Object.pafr_calrot[li_i]	=	dw_PalletFruta.Object.pafr_calibr[li_i]
				
				IF IsNull(dw_PalletFruta.Object.pafr_copack[li_i]) THEN
					dw_PalletFruta.Object.pafr_copack[li_i] = -1
				END IF
				
			NEXT
			
			IF dw_palletfruta.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
				pb_eliminar.Enabled		=	TRUE
				pb_grabar.Enabled		= 	TRUE
			END IF
			
			dw_palletfruta.SetRow(il_fila)
			dw_palletfruta.SelectRow(il_fila,True)
		END IF
	END IF
END IF

end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE 
				
				istr_mant2.argumento[30] = '2'
				
//				dw_1.Reset()
//				dw_1.SetTransObject(SQLCA)
				Agrega_PalletEncab()
				
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
					
					istr_mant.Argumento[4]		=	String(dw_2.Object.rfpe_tarjas[1])
					istr_mant.Argumento[11]	=	String(dw_2.Object.rfpe_tardef[1])
					istr_mant.argumento[21] 	=  String(dw_2.Object.rfpe_ptaori[1])
					istr_mant.argumento[30] 	=  String(dw_2.Object.rfpe_nrores[1])
					
					
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

on w_maed_recfruprocee_particular_palletenc.create
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
this.dw_10=create dw_10
this.dw_11=create dw_11
this.dw_12=create dw_12
this.tab_palletenc=create tab_palletenc
this.dw_55=create dw_55
this.dw_44=create dw_44
this.dw_33=create dw_33
this.dw_palletfruta=create dw_palletfruta
this.dw_13=create dw_13
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
this.Control[iCurrent+10]=this.dw_10
this.Control[iCurrent+11]=this.dw_11
this.Control[iCurrent+12]=this.dw_12
this.Control[iCurrent+13]=this.tab_palletenc
this.Control[iCurrent+14]=this.dw_55
this.Control[iCurrent+15]=this.dw_44
this.Control[iCurrent+16]=this.dw_33
this.Control[iCurrent+17]=this.dw_palletfruta
this.Control[iCurrent+18]=this.dw_13
end on

on w_maed_recfruprocee_particular_palletenc.destroy
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
destroy(this.dw_10)
destroy(this.dw_11)
destroy(this.dw_12)
destroy(this.tab_palletenc)
destroy(this.dw_55)
destroy(this.dw_44)
destroy(this.dw_33)
destroy(this.dw_palletfruta)
destroy(this.dw_13)
end on

event ue_nuevo;Integer li_codigo

HabilitaEncab(True)
istr_mant.argumento[35] =	'0'
istr_mant.argumento[36] =	'0'
ib_ok	= True

CHOOSE CASE wf_modifica()
	CASE -1
		ib_ok = False
	CASE 1
		Message.DoubleParm = 0
		This.TriggerEvent("ue_guardar")
		IF message.DoubleParm = -1 THEN ib_ok = False
	CASE 3
		ib_ok	= False
		RETURN
END CHOOSE

IF ib_ok = False THEN RETURN

dw_1.Reset()
dw_3.Reset()
dw_4.Reset()
dw_10.reset()
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

LimpiaTab()
tab_palletenc.SelectedTab 	=	 1
tab_palletenc.tp_recepcion.dw_2_share.SetRedraw(False)
tab_palletenc.tp_recepcion.dw_2_share.Reset()
tab_palletenc.tp_recepcion.dw_2_share.InsertRow(0)
dw_2.ShareData(tab_palletenc.tp_recepcion.dw_2_share)
tab_palletenc.tp_recepcion.dw_2_share.SetItem(1, "clie_codigo", gi_codexport)
tab_palletenc.tp_recepcion.dw_2_share.SetItem(1, "plde_codigo", gi_codplanta)
tab_palletenc.tp_recepcion.dw_2_share.SetItem(1, "tica_codigo", 1)
tab_palletenc.tp_recepcion.dw_2_share.SetItem(1, "frre_codigo", 1)
tab_palletenc.tp_recepcion.dw_2_share.SetRedraw(True)

dw_ptaori.SetSort("plde_nombre")
dw_ptaori.Sort( )

/*se reasigna tipo de recpción como Packing*/
istr_mant.argumento[31]='1'
/*se limpia argumento de recepción transmitida*/
istr_mant.argumento[33] = ''
//
dw_2.SetRedraw(True)

dw_2.SetFocus()

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
	ib_existe_folio	=	True
	This.TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
	istr_mant.argumento[7]=""
	istr_mant.argumento[6]=String(dw_1.GetitemNumber(dw_1.GetRow(),"paen_numero"))
//	OpenWithParm(w_maed_palletencab_consulta, istr_mant)
	OpenWithParm(w_maed_palletencab_recepcion, istr_mant)
   istr_mant.argumento[6]=""

END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long 		ll_nuevofolio
Integer	li_fillas

IF dw_2.GetNextModified(0, Primary!) > 0 THEN
	dw_2.SetItem(1, "rfpe_fecact", Today())
	dw_2.SetItem(1, "rfpe_horact", Now())
END IF

IF Not ib_primera_entrada AND Not ib_existe_folio  AND grabapallets() THEN
	/*
	Se actualiza tabla recfruprocee a objeto de bloquearla hasta que termine la grabación
	del ingreso
	*/
	UPDATE dba.RECFRUPROCEE SET
			 rfpe_guides = 999
			 WHERE rfpe_tarjas = 999
			 AND   rfpe_nrores = 999
			 AND   rfpe_tardef = 999;

	ll_nuevofolio=Buscanuevofolio(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]))
	dw_2.Object.rfpe_numero[1]	= ll_nuevofolio
     dw_2.SetItem(1, "rfpe_numero",ll_nuevofolio)

	istr_mant.argumento[2]	= String(ll_nuevofolio)
	
	FOR li_fillas = 1 TO dw_1.RowCount()
		 dw_1.Object.rfpe_numero[li_fillas]	= ll_nuevofolio
	NEXT		


END IF
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

event ue_validaborrar;call super::ue_validaborrar;//Long ll_fila, Pallet
//
//IF dw_1.rowcount() >0 THEN
//	FOR ll_fila=1 TO dw_1.rowcount()
//		 Pallet = dw_1.object.paen_numero[ll_fila]
//		 rebajaTamlot(Pallet)
//	NEXT
//END IF
end event

event resize;////
//Integer	maximo, li_posic_x, li_posic_y, li_visible = 0
//
//IF dw_2.width > il_AnchoDw_1 THEN
//	maximo		=	dw_2.width
//ELSE
//	dw_1.width	=	This.WorkSpaceWidth() - 400
//	maximo		=	dw_1.width
//END IF
//
//dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
//dw_2.y					= 37
//
//dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
//dw_1.y					= 64 + dw_2.Height
//dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41
//
//gb_1.x 					= This.WorkSpaceWidth() - 310
//gb_1.y 					= 0
//gb_1.width				= 275
//
//li_posic_x				= This.WorkSpaceWidth() - 250
//li_posic_y				= gb_1.y + 88
//
//IF pb_buscar.Visible THEN
//	pb_buscar.x				= li_posic_x
//	pb_buscar.y				= li_posic_y
//	pb_buscar.width		= 156
//	pb_buscar.height		= 133
//	li_visible ++
//	li_posic_y += 165
//END IF
//
//IF pb_nuevo.Visible THEN
//	pb_nuevo.x				= li_posic_x
//	pb_nuevo.y				= li_posic_y
//	pb_nuevo.width			= 156
//	pb_nuevo.height		= 133
//	li_visible ++
//	li_posic_y += 165
//END IF
//
//IF	pb_eliminar.Visible THEN
//	pb_eliminar.x			= li_posic_x
//	pb_eliminar.y			= li_posic_y
//	pb_eliminar.width		= 156
//	pb_eliminar.height	= 133
//	li_visible ++
//	li_posic_y += 165
//END IF
//
//IF pb_grabar.Visible THEN
//	pb_grabar.x				= li_posic_x
//	pb_grabar.y				= li_posic_y
//	pb_grabar.width		= 156
//	pb_grabar.height		= 133
//	li_visible ++
//	li_posic_y += 165
//END IF
//
//IF pb_imprimir.Visible THEN
//	pb_imprimir.x			= li_posic_x
//	pb_imprimir.y			= li_posic_y
//	pb_imprimir.width		= 156
//	pb_imprimir.height	= 133
//	li_visible ++
//	li_posic_y += 165
//END IF
//
//IF pb_recupera.Visible THEN
//	pb_recupera.x			= li_posic_x
//	pb_recupera.y			= li_posic_y
//	pb_recupera.width		= 156
//	pb_recupera.height	= 133
//	li_visible ++
//	li_posic_y += 165
//END IF
//
//IF pb_captura.Visible THEN
//	pb_captura.x			= li_posic_x
//	pb_captura.y			= li_posic_y
//	pb_captura.width		= 156
//	pb_captura.height	= 133
//	li_visible ++
//	li_posic_y += 165
//END IF
//
//IF pb_salir.Visible THEN
//	pb_salir.x				= li_posic_x
//	pb_salir.y				= li_posic_y
//	pb_salir.width			= 156
//	pb_salir.height		= 133
//	li_visible ++
//	li_posic_y += 165
//END IF
//
//gb_1.height				= 170 * li_visible + 70 /*  (Según Botones Visibles)  */
//gb_2.x 					= gb_1.x
//gb_2.y 					= 1420
//gb_2.width				= 275
//gb_2.height				= 170 * 2 + 65 /*  (2 Botones)  */
//
//pb_ins_det.x			= li_posic_x
//pb_ins_det.y			= gb_2.y + 70
//pb_ins_det.width		= 156
//pb_ins_det.height		= 133
//
//pb_eli_det.x			= li_posic_x
//pb_eli_det.y			= pb_ins_det.y + 160
//pb_eli_det.width		= 156
//pb_eli_det.height		= 133
end event

event ue_guardar;Integer li_Planta, li_Cliente, li_administradora, li_bodeadmin, li_codigo
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
    ELSE
		
		 ll_numero = Long(istr_mant.argumento[34])
		 
		 DELETE FROM dba.recfruproced_trans  
			WHERE clie_codigo = :li_Cliente AND  
					plde_codigo = :li_Planta  AND  
					rfpe_numero = :ll_Numero ;
					
		 DELETE FROM dba.recfruprocee_trans  
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
		IF istr_mant.argumento[31]  = '1' AND istr_mant.argumento[35] =	'1' AND istr_mant.argumento[36] = '1'THEN
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

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_recfruprocee_particular_palletenc
boolean visible = false
integer x = 0
integer y = 1736
integer width = 3355
integer height = 492
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_recfruproced"
boolean hscrollbar = false
boolean vscrollbar = false
boolean livescroll = false
end type

event dw_1::dragdrop;call super::dragdrop;dw_1.Object.objetname.Moveable = 0 
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_recfruprocee_particular_palletenc
boolean visible = false
integer x = 23
integer y = 1176
integer width = 3209
integer height = 556
string dataobject = "dw_mant_recfruprocee_particular_palltenc"
end type

event dw_2::itemchanged;call super::itemchanged;//String	ls_columna, ls_nula
//Date		ld_nula
//
//DataWIndowChild	dw_calibres
//
//SetNull(ls_nula)
//SetNull(ld_nula)
//
//ls_columna = GetColumnName()
//
//CHOOSE CASE ls_columna
//	CASE "plde_codigo"
//		IF ExisteFolio(ls_columna, data) THEN
//			This.SetItem(1, ls_columna, Integer(ls_nula))
//			RETURN 1
//		END IF
//		
//	CASE "rfpe_numero"
//		IF ExisteFolio(ls_columna, data) THEN
//			This.SetItem(1, ls_columna, Integer(ls_nula))
//			RETURN 1
//		END IF
//		
//	CASE "embq_codigo"
//		IF NoExisteEmbarque(ls_columna, data) THEN
//			This.SetItem(1, ls_columna, ls_nula)
//			RETURN 1
//		END IF
//		istr_mant.argumento[24]	=	data
//	CASE "prod_codigo"
//		IF NoExisteProductor(ls_columna, data) THEN
//			This.SetItem(1, ls_columna, Integer(ls_nula))
//			RETURN 1
//		END IF
//		
//	CASE "rfpe_tarjas"
//		istr_mant.argumento[4]	= data
//		
//	CASE "rfpe_tardef"
//		istr_mant.argumento[11]	= data
//
//	CASE "clie_codigo"
//		IF NoExisteCliente(Integer(data)) THEN
//			This.SetItem(Row, ls_Columna, gi_codexport)
//			RETURN 1
//		END IF	
//		
//	CASE "rfpe_tipoen"
//		IF (data='1') THEN
//			istr_mant.argumento[31] = '1'
//			istr_mant.argumento[13] = '2'
//			dw_ptaori.Setfilter("plde_tipopl=2")
//			dw_ptaori.Filter()
//		ELSE
//			istr_mant.argumento[31] = '-1'
//		   IF (data='2') OR (data='6') THEN
//			   istr_mant.argumento[13] = '1'
//			   dw_ptaori.Setfilter("plde_tipopl=1")
//			   dw_ptaori.Filter()
//		   END IF
//		istr_mant.argumento[20] = data
//	  END IF
//		
//	CASE "rfpe_ptaori"
//		IF NoexistePlanta(data) THEN
//			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
//			RETURN 1
//		ELSE 
//			istr_mant.argumento[21]=data
//		END IF
//		
//		
//	CASE "rfpe_fecrec"
//		IF Not f_validafechatempo(date(data)) THEN
//			This.SetItem(Row, ls_Columna, ld_nula)
//			RETURN 1
//		END IF
//		
//	CASE "rfpe_nrores"
//		istr_mant.argumento[30]	= data
//		
//	CASE "frre_codigo"
//		IF iuo_frutarecepcion.existe(Integer(Data),TRUE,sqlca) THEN
//			istr_mant.argumento[38]=data
//		ELSE 
//			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
//			RETURN 1
//		END IF	
//	
//		
//END CHOOSE
//
//HabilitaIngreso()
end event

event dw_2::clicked;call super::clicked;CHOOSE CASE dwo.name
	CASE "buscaembarque"
		BuscaEmbarque()
		
	CASE "buscaproductor"
		BuscaProductor()
		
END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_recfruprocee_particular_palletenc
integer x = 3374
integer y = 252
end type

event pb_nuevo::clicked;call super::clicked;ib_primera_entrada = True
ib_existe_folio	 =	False

call super:: clicked
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_recfruprocee_particular_palletenc
integer x = 3374
integer y = 432
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_recfruprocee_particular_palletenc
integer x = 3374
integer y = 612
end type

event pb_grabar::clicked;ib_primera_entrada = False

istr_mant.argumento[35] =	'1'
istr_mant.argumento[36] =	'1'

call super:: clicked

ib_existe_folio = True
end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_recfruprocee_particular_palletenc
integer x = 3374
integer y = 792
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_recfruprocee_particular_palletenc
integer x = 3374
integer y = 1332
end type

event pb_salir::clicked;istr_mant.argumento[33] = '0'
istr_mant.argumento[35] = '0'
istr_mant.argumento[36] = '0'
IF dw_1.RowCount() > 0 THEN
   //pb_grabar.TriggerEvent(clicked!)
	This.TriggerEvent("ue_guardar")
	DISCONNECT USING sqlexis;
END IF

call super:: clicked
end event

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_recfruprocee_particular_palletenc
integer x = 3374
integer y = 1600
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_recfruprocee_particular_palletenc
integer x = 3374
integer y = 1776
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_recfruprocee_particular_palletenc
integer x = 3374
integer y = 72
end type

type pb_recupera from picturebutton within w_maed_recfruprocee_particular_palletenc
string tag = "Busca Recepciones Transmitidas"
integer x = 3374
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

type pb_captura from picturebutton within w_maed_recfruprocee_particular_palletenc
integer x = 3374
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
Integer	li_valida = 0, li_opcion = 1

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
		existe_cargaregistro()
		IF wf_actualiza_Trans(False) THEN
			istr_mant.argumento[33] = '1'
		ELSE
			MessageBox("Error de Captura","Carga de Pallet ya fue Realizada, Verifique",Information!,Ok!)
		   Message.DoubleParm = -1
			Parent.TriggerEvent("ue_nuevo")
	   END IF
						
		IF Message.DoubleParm = 2 THEN 
			MessageBox("Atención", "No se cargó archivo exitosamente," + is_mensaje + ".", StopSign!, Ok!)
		   FileClose(li_valida)
		END IF
			
	END IF
	
LOOP WHILE Message.DoubleParm = 1
end event

type dw_3 from datawindow within w_maed_recfruprocee_particular_palletenc
boolean visible = false
integer x = 55
integer y = 2040
integer width = 279
integer height = 140
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_recfruprocee_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_maed_recfruprocee_particular_palletenc
boolean visible = false
integer x = 357
integer y = 2040
integer width = 279
integer height = 140
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_recfruproced_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_maed_recfruprocee_particular_palletenc
boolean visible = false
integer x = 663
integer y = 2040
integer width = 279
integer height = 140
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_palletencab_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_6 from datawindow within w_maed_recfruprocee_particular_palletenc
boolean visible = false
integer x = 969
integer y = 2040
integer width = 279
integer height = 140
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_palletfruta_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_7 from datawindow within w_maed_recfruprocee_particular_palletenc
boolean visible = false
integer x = 2414
integer y = 2040
integer width = 219
integer height = 136
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_receprodenca"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_8 from datawindow within w_maed_recfruprocee_particular_palletenc
boolean visible = false
integer x = 2683
integer y = 2040
integer width = 219
integer height = 136
integer taborder = 150
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_receproddeta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_9 from datawindow within w_maed_recfruprocee_particular_palletenc
boolean visible = false
integer x = 2939
integer y = 2040
integer width = 219
integer height = 136
integer taborder = 160
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_exismovtoenca"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_10 from datawindow within w_maed_recfruprocee_particular_palletenc
boolean visible = false
integer x = 2057
integer y = 2040
integer width = 251
integer height = 124
integer taborder = 150
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_rebaje_produccionporrecepcion"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_11 from datawindow within w_maed_recfruprocee_particular_palletenc
boolean visible = false
integer x = 59
integer y = 2196
integer width = 274
integer height = 204
integer taborder = 160
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspec_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_12 from datawindow within w_maed_recfruprocee_particular_palletenc
boolean visible = false
integer x = 398
integer y = 2200
integer width = 261
integer height = 204
integer taborder = 170
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspecdet_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type tab_palletenc from tab within w_maed_recfruprocee_particular_palletenc
integer x = 59
integer y = 8
integer width = 3136
integer height = 1264
integer taborder = 50
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
boolean multiline = true
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
boolean pictureonright = true
integer selectedtab = 1
tp_recepcion tp_recepcion
end type

on tab_palletenc.create
this.tp_recepcion=create tp_recepcion
this.Control[]={this.tp_recepcion}
end on

on tab_palletenc.destroy
destroy(this.tp_recepcion)
end on

event selectionchanging;IF NewIndex = 1 THEN
	dw_palletfruta.ShareDataoff()
ELSEIF UpperBound(ids_palletfruta) > 0 THEN
	dw_palletfruta.ShareDataoff()
	ids_palletfruta[NewIndex - 1].ShareData(dw_palletfruta)
END IF

IF OldIndex > 0 THEN 
	This.Control[OldIndex].PictureName 	= 	""
ELSE
END IF
IF NewIndex > 0 THEN 
	This.Control[NewIndex].PictureName 	= 	"EditStops!"
END IF

IF OldIndex = 1 THEN
	THIS.tp_recepcion.dw_2_share.AcceptText()
ELSEIF OldIndex > 1 THEN
	iuo_palletencab[OldIndex - 1].dw_1.AcceptText()
END IF
end event

event getfocus;dw_palletfruta.Title = ''
end event

type tp_recepcion from userobject within tab_palletenc
integer x = 18
integer y = 112
integer width = 3099
integer height = 1136
long backcolor = 33543637
string text = "Recepción"
long tabtextcolor = 33554432
long tabbackcolor = 30586022
string picturename = "EditStops!"
long picturemaskcolor = 553648127
dw_2_share dw_2_share
end type

on tp_recepcion.create
this.dw_2_share=create dw_2_share
this.Control[]={this.dw_2_share}
end on

on tp_recepcion.destroy
destroy(this.dw_2_share)
end on

type dw_2_share from uo_dw within tp_recepcion
integer x = 5
integer y = 4
integer width = 2743
integer height = 908
integer taborder = 11
string dataobject = "dw_mant_recfruprocee_particular_palltenc"
boolean vscrollbar = false
boolean border = false
end type

event clicked;call super::clicked;CHOOSE CASE dwo.name
	CASE "buscaembarque"
		BuscaEmbarque()
		
	CASE "buscaproductor"
		BuscaProductor()
		
END CHOOSE
end event

event itemchanged;call super::itemchanged;String	ls_columna, ls_nula
Date		ld_nula

DataWIndowChild	dw_calibres

SetNull(ls_nula)
SetNull(ld_nula)

ls_columna = GetColumnName()

CHOOSE CASE ls_columna
	CASE "plde_codigo"
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(1, ls_columna, Integer(ls_nula))
			RETURN 1
		ELSE
			istr_mant2.argumento[2] = data
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
		ELSE
			istr_mant2.argumento[1] = data
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
	
		
END CHOOSE

HabilitaIngreso()
end event

event getfocus;call super::getfocus;dw_palletfruta.Title = ''
end event

type dw_55 from uo_dw within w_maed_recfruprocee_particular_palletenc
integer x = 9
integer y = 2848
integer width = 2825
integer height = 220
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_inspecpalenc_informada"
boolean vscrollbar = false
end type

type dw_44 from uo_dw within w_maed_recfruprocee_particular_palletenc
integer x = 9
integer y = 3076
integer width = 3109
integer height = 220
integer taborder = 21
boolean bringtotop = true
string dataobject = "dw_mues_inspecpaldet_informada"
boolean vscrollbar = false
end type

type dw_33 from uo_dw within w_maed_recfruprocee_particular_palletenc
integer x = 9
integer y = 3304
integer width = 3360
integer height = 220
integer taborder = 21
boolean bringtotop = true
string dataobject = "dw_mues_recfruproced"
boolean vscrollbar = false
end type

type dw_palletfruta from uo_dw within w_maed_recfruprocee_particular_palletenc
integer x = 169
integer y = 1280
integer width = 2898
integer height = 704
integer taborder = 11
boolean bringtotop = true
boolean titlebar = true
string dataobject = "dw_mues_palletfruta"
end type

event getfocus;call super::getfocus;This.Title = 'Detalle del pallet'
end event

type dw_13 from uo_dw within w_maed_recfruprocee_particular_palletenc
boolean visible = false
integer x = 5
integer y = 1980
integer width = 3547
integer height = 432
integer taborder = 11
boolean bringtotop = true
boolean titlebar = true
string dataobject = "dw_pf_test"
boolean hscrollbar = true
end type

