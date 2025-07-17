$PBExportHeader$w_maed_movtofrutagranel_recepcion_planta.srw
$PBExportComments$Recepción de Fruta Granel de Otras Plantas
forward
global type w_maed_movtofrutagranel_recepcion_planta from w_mant_encab_deta_csd
end type
type cb_capturadatos from commandbutton within w_maed_movtofrutagranel_recepcion_planta
end type
type dw_loteenc from uo_dw within w_maed_movtofrutagranel_recepcion_planta
end type
type dw_lotedet from uo_dw within w_maed_movtofrutagranel_recepcion_planta
end type
type dw_pesaje from uo_dw within w_maed_movtofrutagranel_recepcion_planta
end type
type dw_spro_bins from datawindow within w_maed_movtofrutagranel_recepcion_planta
end type
type dw_exiencab from datawindow within w_maed_movtofrutagranel_recepcion_planta
end type
type dw_exideta from datawindow within w_maed_movtofrutagranel_recepcion_planta
end type
type dw_exismovtodetanulos from datawindow within w_maed_movtofrutagranel_recepcion_planta
end type
type dw_exidetaborra from datawindow within w_maed_movtofrutagranel_recepcion_planta
end type
type dw_bins from datawindow within w_maed_movtofrutagranel_recepcion_planta
end type
type tab_1 from tab within w_maed_movtofrutagranel_recepcion_planta
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
type tab_1 from tab within w_maed_movtofrutagranel_recepcion_planta
tp_1 tp_1
tp_2 tp_2
end type
type productoresxlote from structure within w_maed_movtofrutagranel_recepcion_planta
end type
type str_envase from structure within w_maed_movtofrutagranel_recepcion_planta
end type
type str_pesaje from structure within w_maed_movtofrutagranel_recepcion_planta
end type
type str_productores_envases from structure within w_maed_movtofrutagranel_recepcion_planta
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

type str_pesaje from structure
	datetime		fechahora[]
	decimal { 4 }		pesaje[]
	decimal { 4 }		total
	boolean		agrega
	boolean		modifica
	str_puertacomm		puerta
	datawindow		dw
	string		argum[]
end type

type str_productores_envases from structure
	long		productor[]
	long		guiasii[]
	integer		tipomovto[]
	integer		numero[]
	integer		cliente[]
end type

global type w_maed_movtofrutagranel_recepcion_planta from w_mant_encab_deta_csd
integer width = 4475
integer height = 2044
string title = "RECEPCION INTER PLANTA"
string menuname = ""
event ue_carga_detalle pbm_custom27
event type long ue_despuesguardar ( )
event ue_despuesborrar ( )
cb_capturadatos cb_capturadatos
dw_loteenc dw_loteenc
dw_lotedet dw_lotedet
dw_pesaje dw_pesaje
dw_spro_bins dw_spro_bins
dw_exiencab dw_exiencab
dw_exideta dw_exideta
dw_exismovtodetanulos dw_exismovtodetanulos
dw_exidetaborra dw_exidetaborra
dw_bins dw_bins
tab_1 tab_1
end type
global w_maed_movtofrutagranel_recepcion_planta w_maed_movtofrutagranel_recepcion_planta

type variables
w_mant_deta_movtofrutagranel_despacho	iw_mantencion_1
w_mant_deta_movtoenvadeta_recepfruta	iw_mantencion_2

DataWindowChild   							idwc_PltaDest, idwc_Transp, idwc_Camion,idwc_planta,&
                           								idwc_plantadw1,idwc_pltaOrigdw1,idwc_cliente

DataWindow									dw_3, dw_4

str_variedad										istr_variedad
str_categoria									istr_categoria

uo_plantadesp									iuo_PltaDestino
uo_transportista								iuo_Transport
uo_camiones									iuo_Camion
uo_tipodoctoplanta							iuo_TipoDocto
uo_tipomovtofruta								iuo_TipoMovtoFruta
uo_tipomovtofruta								iuo_TipoMovtoEnva
nvuo_traspaso_interplanta					iuo_traspaso
uo_calicosechero  								iuo_calicosechero
uo_grabatablabitacora						iuo_grabatablabitacora
uo_Mail											iuo_Mail

Long     											il_NumFruta=0, il_NumEnva=0, li_retorno, il_coderror
Boolean											ib_Modifica, ib_AutoCommit, ib_ConectadoExistencia
String												is_rut, is_columna,is_rutemp,is_archivo, is_chofer, is_error, is_correo
DateTime										idt_FechaSistema
Date     											id_FechaSistema
Time												it_FechaSistema
Integer											il_conexiste, il_coneccion

Transaction										sqlexi

Private:
str_Productores_Envases						wstr_Prod_Enva
str_pesaje              							wstr_pesaje
str_pesaje              							wstr_pesajeCarro
str_puertacomm		      					istr_puertacomm
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public function boolean revisaenvases ()
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine habilitaingreso (string as_columna)
public function boolean noexistechofer (string rut)
public subroutine buscacamion ()
public subroutine buscanombres (integer ai_especie, integer ai_variedad, long al_productor, integer ai_tipoenva, integer ai_envase, string as_calidad, integer ai_fila, datawindow dw_a)
public function boolean noexistecliente (integer cliente)
public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, long al_numero, integer ai_cliente)
public function integer existeenvaencab (integer ai_fila)
public function integer existemovtoencab (integer ai_fila)
public function integer existemovtodeta (integer ai_fila)
public function integer existeenvadeta (integer ai_fila)
public function integer existemovtopesa (integer ai_fila)
public function integer existeloteenca (integer ai_fila)
public function integer existelotedeta (integer ai_fila)
public function integer existemovtobins (integer ai_fila)
public function integer existebins (integer ai_fila)
public function boolean conexionexistencia ()
public subroutine determina_productoresenvase (integer ai_tipomovto)
public function boolean datos_correo ()
public subroutine productoreslotes (ref string productores[])
public subroutine wf_cargabruto ()
end prototypes

event ue_carga_detalle;Integer		li_retorn, li_estado, li_control
Long			ll_filas
Boolean		lb_retorno = True

w_main.SetMicroHelp("Cargando Datos...")
SetPointer(HourGlass!)

dw_2.Reset()

IF iuo_traspaso.dw_movtoenca.RowsCopy(1, iuo_traspaso.dw_movtoenca.RowCount(), Primary!, dw_2, 1, Primary!) = -1 OR &
   iuo_traspaso.dw_movtodeta.RowsCopy(1, iuo_traspaso.dw_movtodeta.RowCount(), Primary!,dw_3, 1, Primary!) = -1 OR &
   iuo_traspaso.dw_granpesa.RowsCopy(1, iuo_traspaso.dw_granpesa.RowCount(), Primary!, dw_pesaje, 1, Primary!) = -1 OR &
   iuo_traspaso.dw_envaenca.RowsCopy(1, iuo_traspaso.dw_envaenca.RowCount(), Primary!,dw_1, 1, Primary!) = -1 OR &
   iuo_traspaso.dw_envadeta.RowsCopy(1, iuo_traspaso.dw_envadeta.RowCount(), Primary!,dw_4, 1, Primary!) = -1 OR &
   iuo_traspaso.dw_movtoBinsTrans.RowsCopy(1, iuo_traspaso.dw_movtoBinsTrans.RowCount(), Primary!,dw_spro_bins, 1, Primary!) = -1 OR &
   iuo_traspaso.dw_loteenca.RowsCopy(1, iuo_traspaso.dw_loteenca.RowCount(), Primary!,dw_loteenc, 1, Primary!) = -1 OR &
   iuo_traspaso.dw_lotedeta.RowsCopy(1, iuo_traspaso.dw_lotedeta.RowCount(), Primary!,dw_lotedet, 1, Primary!) = -1 THEN//OR &
//   iuo_traspaso.dw_bins.RowsCopy(1, iuo_traspaso.dw_bins.RowCount(), Primary!,&
//												 dw_bins, 1, Primary!) = -1 THEN
	MessageBox('Error', 'AL traspasar datos')
	lb_retorno = False
ELSE
	
	FOR ll_filas = 1 TO dw_spro_bins.RowCount()
		dw_spro_bins.Object.plde_codigo[ll_filas]	=	gstr_ParamPlanta.CodigoPlanta
	NEXT
	
	dw_2.Object.defg_docrel[1] = 	dw_2.Object.mfge_numero[1]
	
	SetNull(li_estado)
	SetNull(id_FechaSistema)
	SetNull(it_FechaSistema)
	dw_2.Object.mfge_numero[1]	=	li_estado
	dw_2.Object.refg_fecsal[1]		=	id_FechaSistema
	dw_2.Object.refg_horasa[1]	=	it_FechaSistema
	
	dw_2.Object.plde_codigo[1]		=	gstr_ParamPlanta.CodigoPlanta
	dw_2.Object.tpmv_codigo[1]	=	Integer(istr_Mant.Argumento[2])
	dw_2.Object.defg_tipdoc[1] 	=  22  				// Guía de Recepción
	dw_2.Object.clie_codigo[1] 		=  gi_codexport 	// Cliente
	
	idt_FechaSistema					=	F_FechaHora()
	
	id_FechaSistema					=	Date(idt_FechaSistema)
	it_FechaSistema					=	Time(Mid(String(idt_FechaSistema),10))
	
	dw_2.Object.mfge_fecmov[1]	=	id_FechaSistema
	dw_2.Object.refg_horaen[1]	=	it_FechaSistema
	
END IF

IF lb_retorno THEN 
	HabilitaEncab(False)
	HabilitaIngreso("mfge_chofer")
	pb_eli_det.Enabled	=	True
	pb_grabar.Enabled	=	True
	tab_1.Enabled			=	True
	tab_1.tp_1.Enabled	=	True
	tab_1.tp_2.Enabled	=	True
	RETURN 1
ELSE
	RETURN -1
END IF

SetPointer(Arrow!)
end event

event type long ue_despuesguardar();integer	li_enva_codigo, li_enva_tipoen, li_bodevirtual, li_devcorreo,&
			li_bodzonal, li_pldori, li_lote_pltcod,li_lote_espcod,li_lote_codigo
Long 		li_filarecep, ll_filarecep, li_secuenciarece = 1,&
			ll_numerorecep, ll_fila_nueva, ll_fila_nea, ll_docrel, li_secuenciadesp = 1, ll_prod_codigo
String	ls_item_codigo, ls_productor, ls_calidad, ls_correo, ls_texto, ls_asunto, &
			ls_error, serrormsg
Boolean	lb_AutoCommit, lb_Retorno
Date		ld_mesprocori

IF ib_ConectadoExistencia  THEN
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	ll_docrel = dw_2.Object.mfge_numero[1]
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
		li_retorno = 2
		is_error = 'Error en Parámetros de Existencia'
		RETURN 2
	ELSE
		li_bodevirtual = luo_existencia.bodvirtual
		li_bodzonal		= luo_existencia.bodzonal
		/*cambio para eliminar bodegas virtuales*/
		li_bodevirtual = luo_existencia.bodzonal
		/*--------------------------------------*/
		ls_correo 		= luo_existencia.correo
		ld_mesprocori 	= luo_existencia.Mesproceso
	END IF
	
	luo_existencia.existeregistro(ll_docrel,li_bodzonal,1,1,True,sqlexi,dw_2.Object.mfge_fecmov[1])
		
	IF luo_existencia.count <> 0 THEN
		li_retorno = 2 
		is_error = 'Registro ya Existe'
		Return 2
	END IF	
	
	IF Not luo_existencia.correlativobode(1,li_bodzonal,li_bodzonal,True,sqlexi) THEN
		Message.DoubleParm = -1
		li_retorno = 2 
		is_error = 'Error con los Correlativos de Bodega'
		RETURN 2
	ELSE
		ll_numerorecep = luo_existencia.numero
	END IF	
	
	ll_numerorecep = 0
	
	IF Not luo_existencia.correlativobode(1,li_bodzonal,li_bodzonal,True,sqlexi) THEN
		Message.DoubleParm = -1
		li_retorno = 2 
		is_error = 'Error con los Correlativos de Bodega'
		RETURN 2
	ELSE
		ll_numerorecep = luo_existencia.numero
	END IF	
	
	IF isnull(ll_numerorecep) THEN
		ll_numerorecep = 0
	END IF	
		
	ll_numerorecep 	= 	ll_numerorecep + 1
	
	ll_fila_nueva 		= 	dw_exiencab.InsertRow(0)
	
	IF Not iuo_PltaDestino.Existe(dw_3.Object.lote_pltcod[1], True, sqlca) THEN
		li_retorno = 2 
		is_error = 'Planta de Destino no Existe'
		RETURN 2
	END IF
	
	luo_existencia.bodega_administradora(iuo_PltaDestino.PackVirtual,True,sqlexi)
	
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
	
	IF Not luo_existencia.Existecliente(ls_productor,True,sqlexi) THEN
		Message.DoubleParm = -1
		li_retorno = 2 
		is_error = 'Cliente '+ls_productor+' No Existe, Revise Tabla de Clientes en Existencia'
		RETURN 2
	ELSE	
		ls_productor = luo_existencia.prod
	END IF
	
	IF luo_existencia.prod = '' OR isnull(luo_existencia.prod) THEN
		ls_productor = luo_existencia.prdgen
	END IF	
	
	dw_exiencab.Object.mden_tipdoc[ll_fila_nueva] 	= 	1
	dw_exiencab.Object.mden_numero[ll_fila_nueva] 	= 	ll_numerorecep 
	dw_exiencab.Object.tpdo_codigo[ll_fila_nueva] 	= 	2
	dw_exiencab.Object.mden_fecmov[ll_fila_nueva] 	= 	dw_2.Object.mfge_fecmov[1]
	dw_exiencab.Object.tpmv_tipomv[ll_fila_nueva] 	= 	1
	dw_exiencab.Object.tpmv_codigo[ll_fila_nueva] 	= 	2
	dw_exiencab.Object.mden_tipana[ll_fila_nueva] 	= 	4
	dw_exiencab.Object.bode_codigo[ll_fila_nueva] 	= 	li_bodzonal
	dw_exiencab.Object.mden_bodest[ll_fila_nueva] 	= 	luo_existencia.bodeAdmi
	//dw_exiencab.Object.clpr_rut[ll_fila_nueva]	 	= 	ls_productor
	dw_exiencab.Object.mden_docrel[ll_fila_nueva] 	= 	dw_2.Object.mfge_numero[1]
	dw_exiencab.Object.mden_fecdre[ll_fila_nueva] 	= 	dw_2.Object.mfge_fecmov[1]
	dw_exiencab.Object.mden_observ[ll_fila_nueva] 	= 	'Recepción Desde bodega Virtual Interplanta'
	dw_exiencab.Object.mden_estado[ll_fila_nueva] 	= 	1
	dw_exiencab.Object.mden_pcopda[ll_fila_nueva] 	= 	1
	dw_exiencab.Object.mden_gdprod[ll_fila_nueva] 	= 	dw_2.Object.mfge_guisii[1]
	
	FOR li_filarecep = 1 TO dw_4.RowCount()
		ll_filarecep		=	dw_exideta.InsertRow(0)
		
		li_enva_codigo	=	dw_4.Object.enva_codigo[li_filarecep]
		li_enva_tipoen =	dw_4.Object.enva_tipoen[li_filarecep]
		ls_calidad		=  dw_4.Object.cale_calida[li_filarecep]
		
		IF Not iuo_calicosechero.Existe(li_enva_tipoen,li_enva_codigo,ls_calidad,True,sqlca) THEN
			Message.DoubleParm = -1
			li_retorno = 2 
			is_error = 'Problema con los Envases'
			RETURN 2
		ELSE
			ls_item_codigo = iuo_calicosechero.item 
		END IF	
		
		IF ls_item_codigo = '' OR IsNull(ls_item_codigo) THEN
			ls_item_codigo = luo_existencia.itgene
		END IF
		
		dw_exideta.Object.mden_tipdoc[ll_filarecep] 	= 	1
		dw_exideta.Object.mden_numero[ll_filarecep] 	= 	ll_numerorecep
		dw_exideta.Object.mdde_secuen[ll_filarecep] 	= 	li_secuenciadesp 
		dw_exideta.Object.tpmv_tipomv[ll_filarecep] 	=	1
		dw_exideta.Object.tpmv_codigo[ll_filarecep] 	=	2
		dw_exideta.Object.item_codigo[ll_filarecep]	=	ls_item_codigo
		dw_exideta.Object.mdde_identi[ll_filarecep] 	=	''
		dw_exideta.Object.mdde_fecmov[ll_filarecep] 	= 	dw_2.Object.mfge_fecmov[1]
		dw_exideta.Object.bode_codigo[ll_filarecep] 	= 	li_bodzonal
		dw_exideta.Object.mdde_cantid[ll_filarecep] 	= 	dw_4.Object.fgme_cantid[li_filarecep]
		li_secuenciadesp 										= 	li_secuenciadesp + 1
	
	NEXT
	
	IF ld_mesprocori > dw_2.Object.mfge_fecmov[1] THEN
		ls_texto	 = "No es posible actualizar movto. recepción Inter Planta Existencia, por modificación anterior al mes de proceso"
		ls_asunto = "Modifica Fruta Granel Despacho Movto. Nº "+String(dw_2.Object.mfge_numero[1])
	
		li_retorno = 2
		is_error = 'No es posible actualizar movto. recepción Inter Planta Existencia, por modificación anterior al mes de proceso'
		Return 2
	END IF
	
	IF dw_exiencab.Rowcount() > 0 THEN
		lb_AutoCommit		=	sqlexi.AutoCommit
		sqlexi.AutoCommit	=	False
	
		IF dw_exiencab.Update(True, False) = 1 THEN
			IF dw_exideta.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlexi.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlexi, This.Title)
					
					RollBack;
					li_retorno = 2	
					Messagebox("Existencia","Grabación de Datos NO se realizó")
				ELSE
					li_retorno = 1						
					dw_exiencab.ResetUpdate()
					dw_exideta.ResetUpdate()
					Messagebox("Existencia","Grabación de Datos realizada Satisfactoriamente")
					
				END IF
			ELSE
								
				RollBack;
				li_retorno = 2
				Messagebox("Existencia","Grabación de Datos NO se realizó")
			END IF
		ELSE
			F_ErrorBaseDatos(sqlexi, This.Title)
		
			RollBack;
			li_retorno = 2	
			Messagebox("Existencia","Grabación de Datos NO se realizó")
		END IF
			
		dw_exiencab.Reset()
		dw_exideta.Reset()
	END IF
	DISCONNECT USING sqlexi;
	ib_Conectadoexistencia = False
	
	//envío correo a encargados de bodega movimiento recepcion
	ls_texto	 = "Se Recepcionó desde planta "+String(dw_3.Object.lote_pltcod[1])+' en planta '+String(dw_2.Object.plde_codigo[1])+".~nEl Nº Recepción generado en existencia es "+String(ll_numerorecep)
		
	ls_asunto = "Despacho interplanta Nº "+String(ll_docrel)
	iuo_Mail.Of_Send({ls_correo},ls_asunto,ls_texto,0)
	
END IF

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
	
	ll_docrel = Long(Istr_Mant.Argumento[3])//dw_2.Object.mfge_numero[1]
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
	ELSE
		li_bodevirtual = luo_existencia.bodvirtual
		li_bodzonal		= luo_existencia.bodzonal
		ls_correo 		= luo_existencia.correo
		ld_mesprocori 	= luo_existencia.Mesproceso
	END IF		
		
	IF luo_existencia.Mesproceso > Date(Istr_Mant.Argumento[20]) THEN //dw_2.Object.mfge_fecmov[1] THEN
		Message.DoubleParm = -1
		RETURN 
	END IF	
	
	li_cliente 			= Integer(Istr_Mant.Argumento[16])//dw_2.Object.clie_codigo[1]
	li_planta 			= Long(Istr_Mant.Argumento[1])//dw_2.Object.plde_codigo[1] 
	li_tipomov		= Integer(Istr_Mant.Argumento[1])//dw_2.Object.tpmv_codigo[1]
	ll_recepcion 	= Long(Istr_Mant.Argumento[3])//dw_2.Object.mfge_numero[1]
		
	FOR ll_filaenva = 1 TO dw_4.RowCount()
	
		il_numeroenva 	= Long(Istr_Mant.Argumento[3])//dw_2.Object.mfge_numero[1]
		ld_fecha			= Date(Istr_Mant.Argumento[20])//dw_2.Object.mfge_fecmov[1]
		
		IF NOT luo_existencia.maximo_porfecha(1,li_bodzonal,il_numeroenva,1,ld_fecha,True,sqlexi) THEN
			Message.DoubleParm = -1
			Return
		ELSE
			ll_numero = luo_existencia.numero
		END IF
				
		IF NOT isnull(ll_numero) THEN
			//Return
				
		IF NOT luo_existencia.actualizaexistencia(2,1,li_bodzonal,ll_numero,il_numeroenva,True,sqlexi) THEN
			Message.DoubleParm = -1
			Return
		END IF	
		
		dw_exidetaborra.Retrieve(1,ll_numero)
		
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
			AND	mden_tipdoc = 1
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

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.mfge_numero.Protect			=	0
	dw_2.Object.tran_codigo.Protect				=	0
	dw_2.Object.cami_patent.Protect				=	0
	dw_2.Object.cami_patcar.Protect				=	0
	dw_2.Object.mfge_rutcho.Protect				=	0
	dw_2.Object.defg_docrel.Protect				=	0
	dw_2.Object.mfge_fecmov.Protect				=	0
	dw_2.Object.refg_horaen.Protect				=	0

	dw_2.Object.mfge_numero.Color			=	0
	dw_2.Object.tran_codigo.Color				=	0	
	dw_2.Object.cami_patent.Color			=	0
	dw_2.Object.cami_patcar.Color			=	0
	dw_2.Object.mfge_rutcho.Color			=	0
	dw_2.Object.defg_docrel.Color				=	0	
	dw_2.Object.mfge_fecmov.Color			=	0
	dw_2.Object.refg_horaen.Color			=	0
	
	dw_2.Object.mfge_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patent.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patcar.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.mfge_rutcho.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.defg_docrel.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.mfge_fecmov.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.refg_horaen.BackGround.Color		=	RGB(255,255,255)
ELSE
	dw_2.Object.mfge_numero.Protect			=	1
	dw_2.Object.tran_codigo.Protect				=	1
	dw_2.Object.cami_patent.Protect				=	1
	dw_2.Object.cami_patcar.Protect				=	1
	dw_2.Object.mfge_rutcho.Protect				=	1
	dw_2.Object.defg_docrel.Protect				=	1
	dw_2.Object.mfge_fecmov.Protect				=	1
	dw_2.Object.refg_horaen.Protect				=	1

	dw_2.Object.mfge_numero.Color			=	RGB(255,255,255)
	dw_2.Object.tran_codigo.Color				=	RGB(255,255,255)
	dw_2.Object.cami_patent.Color			=	RGB(255,255,255)
	dw_2.Object.cami_patcar.Color			=	RGB(255,255,255)
	dw_2.Object.mfge_rutcho.Color			=	RGB(255,255,255)
	dw_2.Object.defg_docrel.Color				=	RGB(255,255,255)
	dw_2.Object.mfge_fecmov.Color			=	RGB(255,255,255)
	dw_2.Object.refg_horaen.Color			=	RGB(255,255,255)
	
	dw_2.Object.mfge_numero.BackGround.Color	=	553648127
	dw_2.Object.tran_codigo.BackGround.Color		=	553648127
	dw_2.Object.cami_patent.BackGround.Color		=	553648127
	dw_2.Object.cami_patcar.BackGround.Color		=	553648127
	dw_2.Object.mfge_rutcho.BackGround.Color	=	553648127
	dw_2.Object.defg_docrel.BackGround.Color		=	553648127
	dw_2.Object.mfge_fecmov.BackGround.Color	=	553648127
	dw_2.Object.refg_horaen.BackGround.Color		=	553648127
END IF
end subroutine

public function boolean revisaenvases ();Long ll_Fila, ll_FilaBusc, ll_Filaarr,arr_envases[500,4], ll_sumBultos, ll_Row, ll_FilaUlt
Integer  li_TipoEnva, li_Envase, li_TipoEnvaSig, li_EnvaseSig, li_cont
String   ls_mensaje
Boolean  lb_ya_ingresado = False

ll_Fila = 1
ll_FilaArr = 1

Do While  ll_Fila<= dw_3.RowCount()
	
	ll_sumBultos 		= 0
	lb_ya_ingresado 	= False
	
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

IF li_cont > 0 AND (gstr_paramplanta.binsabins OR gstr_paramplanta.bultobins) THEN
	MessageBox("Error de Consistencia", "No Existe Concordancia de Bultos en :" + ls_mensaje + ".", StopSign!, Ok!)
	RETURN FALSE
END IF


RETURN TRUE

end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_Retorno

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
				IF dw_loteenc.Update(True, False) = 1 THEN
					IF dw_lotedet.Update(True, False) = 1 THEN
						IF dw_pesaje.Update(True, False) = 1 THEN
							IF dw_spro_bins.Update(True, False) = 1 THEN
								IF dw_bins.Update(True, False) = 1 THEN
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
											dw_loteenc.ResetUpdate()
											dw_lotedet.ResetUpdate()
											dw_pesaje.ResetUpdate()
											dw_spro_bins.ResetUpdate()
											dw_bins.ResetUpdate()
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
		IF dw_loteenc.Update(True, False) = 1 THEN
			IF dw_lotedet.Update(True, False) = 1 THEN
				IF dw_3.Update(True, False) = 1 THEN
					IF dw_1.Update(True, False) = 1 THEN
						IF dw_4.Update(True, False) = 1 THEN
							IF dw_pesaje.Update(True, False) = 1 THEN
								IF dw_spro_bins.Update(True, False) = 1 THEN
									IF dw_bins.Update(True, False) = 1 THEN
										Commit;
								
										IF sqlca.SQLCode <> 0 THEN
											F_ErrorBaseDatos(sqlca, This.Title)
										ELSE
											lb_Retorno	=	True
								
											dw_1.ResetUpdate()
											dw_2.ResetUpdate()
											dw_3.ResetUpdate()
											dw_4.ResetUpdate()
											dw_loteenc.ResetUpdate()
											dw_lotedet.ResetUpdate()
											dw_pesaje.ResetUpdate()
											dw_spro_bins.ResetUpdate()
											dw_bins.ResetUpdate()
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

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date		ld_Fecha
Time		lt_Hora

dw_2.AcceptText()

IF as_Columna <> "mfge_fecmov" AND &
	(dw_2.Object.mfge_fecmov[1] = ld_Fecha OR IsNull(dw_2.Object.mfge_fecmov[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "refg_horaen" AND &
	(dw_2.Object.refg_horaen[1] = lt_Hora OR IsNull(dw_2.Object.refg_horaen[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "tran_codigo" AND &
	IsNull(dw_2.Object.tran_codigo[1]) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "cami_patent" AND &
	(dw_2.Object.cami_patent[1] = "" OR IsNull(dw_2.Object.cami_patent[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "mfge_chofer" AND &
	(dw_2.Object.mfge_chofer[1] = "" OR IsNull(dw_2.Object.mfge_chofer[1])) THEN
	lb_Estado = False
END IF
	
tab_1.tp_1.Enabled	=	lb_Estado
tab_1.tp_2.Enabled	=	lb_Estado
pb_ins_det.Enabled	=	lb_Estado

IF dw_3.RowCount()>0 Or dw_4.RowCount()>0 THEN
	pb_eli_det.Enabled	=	lb_Estado
	pb_grabar.Enabled		=	lb_estado
END IF	

end subroutine

public function boolean noexistechofer (string rut);String ls_nombre,ls_paterno,ls_materno

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

lstr_busq.argum[1] = '1'

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
	is_rut = lstr_busq.argum[5]
	dw_2.Object.mfge_chofer[il_fila]		=	lstr_busq.argum[4]
	dw_2.SetFocus()
END IF

RETURN
end subroutine

public subroutine buscanombres (integer ai_especie, integer ai_variedad, long al_productor, integer ai_tipoenva, integer ai_envase, string as_calidad, integer ai_fila, datawindow dw_a);String ls_Variedad, ls_productor, ls_Envase, ls_Calidad

IF ai_variedad <> 0 THEN

	SELECT vari_nombre INTO :ls_Variedad
	  FROM dbo.variedades
	 WHERE espe_codigo = :ai_especie
	   AND vari_codigo = :ai_variedad;
		
   IF sqlca.SQLCode = -1 THEN
	  F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Variedades")
	ELSEIF sqlca.SQLCode = 0 THEN
		dw_a.SetItem(ai_fila,"vari_nombre",ls_variedad)
	END IF	
	
END IF

IF al_productor <> 0 THEN

	SELECT prod_nombre INTO :ls_productor
	  FROM dbo.productores
	 WHERE prod_codigo = :al_productor;
		
   IF sqlca.SQLCode = -1 THEN
	  F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Productor")
	ELSEIF sqlca.SQLCode = 0 THEN
		dw_a.SetItem(ai_fila,"prod_nombre",ls_productor)
	END IF	

END IF

IF ai_envase <> 0 THEN

	SELECT enva_nombre INTO :ls_Envase
	  FROM dbo.envases
	 WHERE enva_tipoen = :ai_tipoenva
	   AND enva_codigo = :ai_envase;
		
   IF sqlca.SQLCode = -1 THEN
	  F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Envases")
	ELSEIF sqlca.SQLCode = 0 THEN
		dw_a.SetItem(ai_fila,"enva_nombre",ls_Envase)
	END IF	
	
END IF

IF as_calidad <> "" THEN

	SELECT cale_nombre INTO :ls_Calidad
	  FROM dbo.spro_calicosechero
	 WHERE enva_tipoen = :ai_tipoenva
	   AND enva_codigo = :ai_envase
		AND cale_calida = :as_calidad;
		
   IF sqlca.SQLCode = -1 THEN
	  F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Calidades")
	ELSEIF sqlca.SQLCode = 0 THEN
		dw_a.SetItem(ai_fila,"cale_nombre",ls_Calidad)
	END IF	
	
END IF
end subroutine

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT 	clie_nombre
INTO		:ls_nombre
FROM 		dbo.clientesprod
WHERE		clie_codigo =:cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClientesProd")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

Return False
end function

public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, long al_numero, integer ai_cliente);Long		ll_Numero
Boolean	lb_Retorno = True

SELECT	mfge_numero
	INTO	:ll_Numero
	FROM	dbo.spro_movtofrutagranenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfge_numero	=	:al_Numero
	AND   clie_codigo =  :ai_cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Recepción Fruta Granel")
	
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

public function integer existeenvaencab (integer ai_fila);Integer	li_cliente, li_planta, li_tipo, li_numero, li_cuenta

li_cliente	= 	dw_1.Object.clie_codigo[ai_fila]
li_planta 	= 	dw_1.Object.plde_codigo[ai_fila]
li_tipo 		= 	dw_1.Object.tpmv_codigo[ai_fila]
li_numero 	= 	dw_1.Object.meen_numero[ai_fila]

SELECT count(*)
INTO :li_cuenta
FROM dbo.spro_movtoenvaenca
WHERE clie_codigo = :li_cliente
  AND plde_codigo = :li_planta
  AND tpmv_codigo = :li_tipo
  AND meen_numero = :li_numero
  USING sqlca;
  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtoenvaenca")
	Return -1
ELSEIF li_cuenta > 0 THEN
	Return 1
ELSE
	Return 0
END IF
end function

public function integer existemovtoencab (integer ai_fila);Integer	li_cliente, li_planta, li_tipo, li_numero, li_cuenta

li_cliente	= 	dw_2.Object.clie_codigo[ai_fila]
li_planta 	= 	dw_2.Object.plde_codigo[ai_fila]
li_tipo 		= 	dw_2.Object.tpmv_codigo[ai_fila]
li_numero 	= 	dw_2.Object.mfge_numero[ai_fila]

SELECT count(*)
INTO :li_cuenta
FROM dbo.spro_movtofrutagranenca
WHERE clie_codigo = :li_cliente
  AND plde_codigo = :li_planta
  AND tpmv_codigo = :li_tipo
  AND mfge_numero = :li_numero
  USING sqlca;
  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagranenca")
	Return -1
ELSEIF li_cuenta > 0 THEN
	Return 1
ELSE
	Return 0
END IF
end function

public function integer existemovtodeta (integer ai_fila);Integer	li_cliente, li_planta, li_tipo, li_numero, li_secuencia, li_cuenta

li_cliente	= 	dw_3.Object.clie_codigo[ai_fila]
li_planta 	= 	dw_3.Object.plde_codigo[ai_fila]
li_tipo 		= 	dw_3.Object.tpmv_codigo[ai_fila]
li_numero 	= 	dw_3.Object.mfge_numero[ai_fila]
li_secuencia=	dw_3.Object.mfgd_secuen[ai_fila]

SELECT count(*)
INTO :li_cuenta
FROM dbo.spro_movtofrutagrandeta
WHERE clie_codigo = :li_cliente
  AND plde_codigo = :li_planta
  AND tpmv_codigo = :li_tipo
  AND mfge_numero = :li_numero
  AND mfgd_secuen = :li_secuencia
  USING sqlca;
  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagrandeta")
	Return -1
ELSEIF li_cuenta > 0 THEN
	Return 1
ELSE
	Return 0
END IF
end function

public function integer existeenvadeta (integer ai_fila);Integer	li_cliente, li_planta, li_tipo, li_numero, li_cuenta
Integer	li_envase, li_tipoen, li_conenv, li_sentid
String 	ls_calida

li_cliente	= 	dw_4.Object.clie_codigo[ai_fila]
li_planta 	= 	dw_4.Object.plde_codigo[ai_fila]
li_tipo 		= 	dw_4.Object.tpmv_codigo[ai_fila]
li_numero 	= 	dw_4.Object.meen_numero[ai_fila]
li_tipoen	= 	dw_4.Object.enva_tipoen[ai_fila]
li_envase 	= 	dw_4.Object.enva_codigo[ai_fila]
li_conenv	=	dw_4.Object.fgme_conenv[ai_fila]
li_sentid	=	dw_4.Object.fgme_sentid[ai_fila]
ls_calida	=	dw_4.Object.cale_calida[ai_fila]

SELECT count(*)
INTO :li_cuenta
FROM dbo.spro_movtoenvadeta
WHERE clie_codigo = :li_cliente
  AND plde_codigo = :li_planta
  AND tpmv_codigo = :li_tipo
  AND meen_numero = :li_numero
  AND enva_tipoen = :li_tipoen
  AND enva_codigo = :li_envase
  AND fgme_conenv = :li_conenv
  AND fgme_sentid = :li_sentid
  AND cale_calida = :ls_calida
  USING sqlca;
  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtoenvadeta")
	Return -1
ELSEIF li_cuenta > 0 THEN
	Return 1
ELSE
	Return 0
END IF
end function

public function integer existemovtopesa (integer ai_fila);Integer	li_cliente, li_planta, li_tipo, li_numero, li_nropes, li_secuencia, li_cuenta

li_cliente	= 	dw_pesaje.Object.clie_codigo[ai_fila]
li_planta 	= 	dw_pesaje.Object.plde_codigo[ai_fila]
li_tipo 		= 	dw_pesaje.Object.tpmv_codigo[ai_fila]
li_numero 	= 	dw_pesaje.Object.mfge_numero[ai_fila]
li_nropes	= 	dw_pesaje.Object.mfgp_nropes[ai_fila]
li_secuencia=	dw_pesaje.Object.mfgp_secuen[ai_fila]

SELECT count(*)
INTO :li_cuenta
FROM dbo.spro_movtofrutagranpesa
WHERE clie_codigo = :li_cliente
  AND plde_codigo = :li_planta
  AND tpmv_codigo = :li_tipo
  AND mfge_numero = :li_numero
  AND mfgp_nropes = :li_nropes
  AND mfgp_secuen = :li_secuencia
  USING sqlca;
  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagranpesa")
	Return -1
ELSEIF li_cuenta > 0 THEN
	Return 1
ELSE
	Return 0
END IF
end function

public function integer existeloteenca (integer ai_fila);Integer	li_planta, li_especie, li_lote, li_cuenta

li_planta	= 	dw_loteenc.Object.lote_pltcod[ai_fila]
li_especie 	= 	dw_loteenc.Object.lote_espcod[ai_fila]
li_lote 		= 	dw_loteenc.Object.lote_codigo[ai_fila]

SELECT count(*)
INTO :li_cuenta
FROM dbo.spro_lotesfrutagranel
WHERE lote_pltcod = :li_planta
  AND lote_espcod = :li_especie
  AND lote_codigo = :li_lote
  USING sqlca;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_lotesfrutagranel")
	Return -1
ELSEIF li_cuenta > 0 THEN
	Return 1
ELSE
	Return 0
END IF
end function

public function integer existelotedeta (integer ai_fila);Integer	li_planta, li_especie, li_lote, li_tipoen, li_envase, li_cuenta

li_planta	= 	dw_lotedet.Object.lote_pltcod[ai_fila]
li_especie 	= 	dw_lotedet.Object.lote_espcod[ai_fila]
li_lote 		= 	dw_lotedet.Object.lote_codigo[ai_fila]
li_tipoen	= 	dw_lotedet.Object.enva_tipoen[ai_fila]
li_envase	=	dw_lotedet.Object.enva_codigo[ai_fila]

SELECT count(*)
INTO :li_cuenta
FROM dbo.spro_lotesfrutagrandeta
WHERE lote_pltcod = :li_planta
  AND lote_espcod = :li_especie
  AND lote_codigo = :li_lote
  AND enva_tipoen = :li_tipoen
  AND enva_codigo = :li_envase
  USING sqlca;
  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_lotesfrutagrandeta")
	Return -1
ELSEIF li_cuenta > 0 THEN
	Return 1
ELSE
	Return 0
END IF
end function

public function integer existemovtobins (integer ai_fila);Integer	li_cliente, li_planta, li_especie, li_cuenta
Long		ll_tarja, ll_bins, ll_lote, ll_lotplt

li_cliente	= 	dw_spro_bins.Object.clie_codigo[ai_fila]
li_planta 	= 	dw_spro_bins.Object.plde_codigo[ai_fila]
ll_tarja		= 	dw_spro_bins.Object.fgmb_nrotar[ai_fila]
ll_bins 		= 	dw_spro_bins.Object.bins_numero[ai_fila]
ll_lotplt 		= 	dw_spro_bins.Object.lote_pltcod[ai_fila]					//LRBB 16.ene.2014, agrega lote planta a las busqueda en tabla
li_especie	= 	dw_spro_bins.Object.lote_espcod[ai_fila]
ll_lote	 	= 	dw_spro_bins.Object.lote_codigo[ai_fila]

SELECT count(*)
INTO :li_cuenta
FROM dbo.spro_MovtoBins
WHERE clie_codigo = :li_cliente
  AND plde_codigo = :li_planta
  AND fgmb_nrotar = :ll_tarja
  AND bins_numero = :ll_bins
  AND lote_pltcod    = :ll_lotplt								//LRBB 16.ene.2014, es parte de la llave de la tabla
  AND lote_espcod = :li_especie
  AND lote_codigo = :ll_lote
  USING sqlca;
  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_MovtoBins")
	Return -1
ELSEIF li_cuenta > 0 THEN
	Return 1
ELSE
	Return 0
END IF
end function

public function integer existebins (integer ai_fila);Integer	li_cliente, li_planta, li_cuenta
Long		ll_numero

li_cliente	= 	dw_bins.Object.clie_codigo[ai_fila]
li_planta 	= 	dw_bins.Object.plde_codigo[ai_fila]
ll_numero	= 	dw_bins.Object.bins_numero[ai_fila]

SELECT count(*)
INTO :li_cuenta
FROM dbo.spro_Bins
WHERE clie_codigo = :li_cliente
  AND plde_codigo = :li_planta
  AND bins_numero = :ll_numero
  USING sqlca;
  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_Bins")
	Return -1
ELSEIF li_cuenta > 0 THEN
	Return 1
ELSE
	Return 0
END IF
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

public subroutine determina_productoresenvase (integer ai_tipomovto);str_Productores_Envases	lstr_Prod_Enva
DataWindow	ldw_envase
Long			ll_Fila, ll_GuiaSII, ll_Fila_Lote
Integer		li_Secuencia, li_Cliente, li_ClieAnt
Long			ll_Productor,ll_ProdAnt

//Inicializa la Estructura de Instancia
wstr_Prod_Enva	= lstr_Prod_Enva

IF ai_TipoMovto = 41 THEN
	ldw_envase	=	dw_4
ELSE
	ldw_envase	=	dw_4
END IF

FOR ll_Fila	=	1 TO ldw_Envase.RowCount()
	ldw_Envase.Object.clie_codigo[ll_Fila]	=		dw_2.Object.clie_codigo[1]
	ll_Productor									=		ldw_Envase.Object.prod_codigo[ll_Fila]
   li_Cliente										=		ldw_Envase.Object.clie_codigo[ll_Fila]
	
	IF (ll_Productor <> ll_ProdAnt) OR (li_Cliente <> li_ClieAnt) THEN
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
		
		wstr_Prod_Enva.Productor[li_Secuencia]	=	ll_Productor
		wstr_Prod_Enva.GuiaSII[li_Secuencia]	=	ll_GuiaSII
		wstr_Prod_Enva.TipoMovto[li_Secuencia]	=	ai_TipoMovto
		wstr_Prod_Enva.Cliente[li_Secuencia]	=	li_Cliente
	END IF
NEXT

RETURN
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

SELECT 	plde_correo
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

public subroutine productoreslotes (ref string productores[]);Long		ll_Fila
Integer	li_Secuencia
String	ls_productor, ls_ProdAnt, ls_Nula[]
productoresxlote	istr_pxl

Productores	=	ls_Nula

FOR ll_Fila = 1 TO dw_3.RowCount()
	ls_Productor	=	dw_3.Object.prod_rut[ll_Fila]
	
	istr_pxl.prod_rut[ll_fila]			=	dw_3.Object.prod_rut[ll_Fila]
	istr_pxl.prod_codigo[ll_fila]		=	dw_3.Object.prod_codigo[ll_Fila]
	istr_pxl.prod_nombre[ll_fila]	=	dw_3.Object.prod_nombre[ll_Fila]
	istr_pxl.lote_codigo[ll_fila]		=	dw_3.Object.lote_codigo[ll_Fila]
	istr_pxl.lote_pltcod[ll_fila]		=	dw_3.Object.lote_pltcod[ll_Fila]
	istr_pxl.lote_espcod[ll_fila]		=	dw_3.Object.lote_espcod[ll_Fila]

	IF ls_Productor <> ls_ProdAnt THEN
		li_Secuencia ++
		Productores[li_Secuencia]	=	ls_Productor
	
		ls_ProdAnt	=	ls_Productor
	END IF
NEXT

//Integer li_filas_dw6, li_filas_dw4, li_bulto
//
//dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
//
//FOR li_filas_dw6 = 1 TO dw_6.RowCount()
//	
//	li_filas_dw4	=	dw_4.find( 'enva_tipoen = ' + String(dw_6.Object.enva_tipoen[li_filas_dw6]) + ' and ' +&
//									   'enva_codigo = ' + String(dw_6.Object.enva_codigo[li_filas_dw6]) + ' and ' +&
//										 + ' and ' +&, 1, dw_2.RowCount() )
//										
//	IF li_filas_dw4 < 1 THEN
//		li_filas_dw4	=	dw_4.InsertRow(0)
//		dw_4.Object.lote_pltcod[li_filas_dw4]	=	Integer(istr_mant.Argumento[1])
//		dw_4.Object.lote_espcod[li_filas_dw4]	=	Integer(istr_mant.Argumento[5])
//		dw_4.Object.lote_codigo[li_filas_dw4]	=	dw_6.Object.lote_codigo[li_filas_dw6]
//		dw_4.Object.enva_tipoen[li_filas_dw4]	=	dw_6.Object.enva_tipoen[li_filas_dw6]
//		dw_4.Object.enva_codigo[li_filas_dw4]	=	dw_6.Object.enva_codigo[li_filas_dw6]
//	END IF
//	
//	li_bulto 											=	dw_4.Object.lotd_totbul[li_filas_dw4]
//	
//	IF IsNull(li_bulto) THEN li_bulto = 0
//	
//	dw_4.Object.lotd_totbul[li_filas_dw4] 	=	li_bulto + 1
//	
//NEXT

RETURN
end subroutine

public subroutine wf_cargabruto ();Integer	li_fila, li_especie
Decimal	ld_Bruto, ld_totbut
Long		ll_planta, ll_lote
uo_bins	luo_bins

luo_bins	= Create uo_bins

For li_fila = 1 To dw_pesaje.RowCount()
	If luo_bins.Existe(dw_pesaje.Object.clie_codigo[li_fila], dw_pesaje.Object.plde_codigo[li_fila],  dw_pesaje.Object.bins_numero[li_fila], sqlca, False) THEN
		ld_bruto = ld_bruto + luo_bins.cale_pesoen
	End If
Next

dw_2.Object.refg_tkenen[1]	=	ld_bruto
//ld_totbut							=	dw_2.Object.mfge_tpneto[1] + ld_bruto
dw_2.Object.refg_tkbent[1]	=	dw_2.Object.mfge_tpneto[1] + ld_bruto //ld_totbut

Destroy luo_bins
end subroutine

event open;/* 
	Argumentos
		istr_Mant.Argumento[1]	=	Código Planta
		istr_Mant.Argumento[2]	=	Tipo de Movimiento
		istr_Mant.Argumento[3]	=	Número de Despacho
		istr_Mant.Argumento[4]	=	Sentido del Movimiento => 2 = Despacho
		istr_Mant.Argumento[7]	=	Sentido del Movimiento => 2 = Despacho
		istr_Mant.Argumento[16] = Cliente
		istr_Mant.Argumento[20] = 	Fecha Mov
		
		
*/

DataWindowChild	ldwc_Camara, ldwc_TipoEnvase, ldwc_PltaLote, ldwc_Especie

x				= 0
y				= 0
This.Height	= 2520
im_menu		= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

dw_3	=	tab_1.tp_1.dw_detalle
dw_4	=	tab_1.tp_2.dw_envases

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	"2"
istr_Mant.Argumento[3]	=	""
istr_Mant.Argumento[4]	=	"1"
istr_Mant.Argumento[7]	=	""
istr_Mant.Argumento[16]	=	String(gi_codexport)

//cliente
dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve() 

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)

IF idwc_planta.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
ELSE
	idwc_planta.SetSort("plde_nombre A")
	idwc_planta.Sort()
END IF

dw_2.GetChild("plde_coorde", idwc_PltaDest)
idwc_PltaDest.SetTransObject(sqlca)

IF idwc_PltaDest.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Plantas")
	idwc_PltaDest.InsertRow(0)
ELSE
	idwc_PltaDest.SetFilter("plde_codigo <> " + String(gstr_ParamPlanta.CodigoPlanta))
	idwc_PltaDest.Filter()
	idwc_PltaDest.SetSort("plde_nombre A")
	idwc_PltaDest.Sort()
END IF

dw_1.GetChild("plde_codigo", idwc_plantadw1)
idwc_plantadw1.SetTransObject(sqlca)

IF idwc_plantadw1.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Plantas")
	idwc_plantadw1.InsertRow(0)
ELSE
	idwc_plantadw1.SetSort("plde_nombre A")
	idwc_plantadw1.Sort()
END IF

dw_1.GetChild("plde_coorde", idwc_pltaOrigdw1)
idwc_pltaOrigdw1.SetTransObject(sqlca)

IF idwc_pltaOrigdw1.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Plantas")
	idwc_pltaOrigdw1.InsertRow(0)
ELSE
	idwc_pltaOrigdw1.SetFilter("plde_codigo <> " + String(gstr_ParamPlanta.CodigoPlanta))
	idwc_pltaOrigdw1.Filter()
	idwc_pltaOrigdw1.SetSort("plde_nombre A")
	idwc_pltaOrigdw1.Sort()
END IF


dw_2.GetChild("tran_codigo", idwc_Transp)
idwc_Transp.SetTransObject(sqlca)

IF idwc_Transp.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Transportístas")
	idwc_Transp.InsertRow(0)
ELSE
	idwc_Transp.SetSort("tran_nombre A")
	idwc_Transp.Sort()
END IF

dw_2.GetChild("cami_patent", idwc_Camion)
idwc_Camion.SetTransObject(sqlca)

IF idwc_Camion.Retrieve(2) = 0 THEN
	MessageBox("Atención", "Falta Registrar Camiones con Flete Propio")
	idwc_Camion.InsertRow(0)
ELSE
	idwc_Camion.SetSort("cami_codigo A")
	idwc_Camion.Sort()
END IF

dw_3.GetChild("cama_codigo", ldwc_Camara)
ldwc_Camara.SetTransObject(sqlca)
IF ldwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	MessageBox("Atención", "Falta Registrar Cámaras para Planta " + String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	ldwc_Camara.InsertRow(0)
ELSE
	ldwc_Camara.SetSort("cama_nombre A")
	ldwc_Camara.Sort()
END IF

dw_3.GetChild("lote_pltcod", ldwc_PltaLote)
ldwc_PltaLote.SetTransObject(sqlca)
ldwc_PltaLote.Retrieve()

dw_3.GetChild("lote_espcod", ldwc_Especie)
ldwc_Especie.SetTransObject(sqlca)
IF ldwc_Especie.Retrieve() = 0 THEN
	ldwc_Especie.InsertRow(0)
ELSE
	ldwc_Especie.SetSort("lote_espcod A")
	ldwc_Especie.Sort()
END IF


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
	MessageBox("Atención", "Falta Registrar Cámaras para Planta " + String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	ldwc_Camara.InsertRow(0)
ELSE
	ldwc_Camara.SetSort("cama_nombre A")
	ldwc_Camara.Sort()
END IF

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_loteenc.SetTransObject(sqlca)
dw_lotedet.SetTransObject(sqlca)
dw_pesaje.SetTransObject(sqlca)
dw_spro_bins.SetTransObject(sqlca)
dw_bins.SetTransObject(sqlca)

dw_3.Modify("datawindow.message.title='Error '+ is_titulo")
dw_3.SetRowFocusIndicator(Hand!)
dw_3.Modify("DataWindow.Footer.Height = 88")

dw_4.Modify("datawindow.message.title='Error '+ is_titulo")
dw_4.SetRowFocusIndicator(Hand!)
dw_4.Modify("DataWindow.Footer.Height = 88")

istr_Mant.dw				=	dw_1
istr_Mant.dw2				=	dw_4
istr_Mant.solo_consulta  =	False

pb_nuevo.PostEvent(Clicked!)

iuo_PltaDestino				=	Create uo_plantadesp
iuo_Transport				=	Create uo_transportista
iuo_Camion					=	Create uo_camiones
iuo_TipoDocto				=	Create uo_tipodoctoplanta
iuo_traspaso				=	Create nvuo_traspaso_interplanta
iuo_tipomovtofruta			=	Create uo_tipomovtofruta
iuo_TipoMovtoEnva		=	Create uo_tipomovtofruta
iuo_Calicosechero			= 	Create uo_calicosechero
iuo_Mail						=	Create uo_Mail
end event

event ue_borra_detalle;call super::ue_borra_detalle;//IF dw_1.rowcount() < 1 THEN RETURN

str_mant_envases	lstr_mant			//LRBB 16.ene.2014, se cambio por estructura local


SetPointer(HourGlass!)

ib_Borrar				=	True
Message.DoubleParm	=	0
istr_mant.Borra		=	True
istr_mant.Agrega		=	False

w_main.SetMicroHelp("Validando la eliminación de detalle...")

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF tab_1.SelectedTab  <> 1 THEN											//LRBB 16.ene.2014, se cambio por estructura local
	lstr_mant.argumento				=	istr_mant.argumento
	lstr_mant.dw						=	istr_mant.dw
	lstr_mant.dw2						=	istr_mant.dw2
	lstr_mant.Agrega					=	istr_mant.Agrega
	lstr_mant.Borra					     =	istr_mant.Borra
	lstr_mant.Solo_Consulta			=	istr_mant.Solo_Consulta
	lstr_mant.respuesta				=	istr_mant.respuesta
	lstr_mant.tipo						=	istr_mant.tipo
	lstr_mant.usuariosoloconsulta	=	istr_mant.usuariosoloconsulta
	
END IF

IF tab_1.SelectedTab = 1 THEN
	istr_Mant.dw	=	dw_3
	
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
	lstr_Mant.dw	=	dw_4
	
	OpenWithParm(iw_mantencion_2, lstr_Mant)		//LRBB 16.ene.2014, se cambio por estructura local
	
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

event ue_nuevo_detalle;call super::ue_nuevo_detalle;Integer	li_tabpage, li_filas, li_prods
Boolean	lb_estado, lb_existe
str_mant_envases	lstr_mant


li_tabpage	=	tab_1.SelectedTab

istr_mant.Borra	=	False
istr_mant.Agrega	=	True

lb_estado			=	istr_mant.Solo_Consulta

IF li_tabpage <> 1 THEN
	lstr_mant.argumento				=	istr_mant.argumento
	lstr_mant.dw						=	istr_mant.dw
	lstr_mant.dw2						=	istr_mant.dw2
	lstr_mant.Agrega					=	istr_mant.Agrega
	lstr_mant.Borra					=	istr_mant.Borra
	lstr_mant.Solo_Consulta			=	istr_mant.Solo_Consulta
	lstr_mant.respuesta				=	istr_mant.respuesta
	lstr_mant.tipo						=	istr_mant.tipo
	lstr_mant.usuariosoloconsulta	=	istr_mant.usuariosoloconsulta
	
	FOR li_filas = 1 TO dw_loteenc.RowCount()
		lb_existe =	False
		
		FOR li_prods = 1 TO UpperBound(lstr_mant.Productores)
			IF Integer(lstr_mant.Productores[li_prods]) = dw_loteenc.Object.prod_codigo[li_filas] THEN
				lb_existe = True
			END IF
		NEXT
		
		IF NOT lb_existe THEN
			lstr_mant.Productores[li_prods]	=	String(dw_loteenc.Object.prod_codigo[li_filas])
		END IF
	NEXT
END IF

IF tab_1.SelectedTab = 1 THEN
	istr_mant.dw	=	dw_3
	
	OpenWithParm(iw_mantencion_1, istr_mant)

	IF dw_3.RowCount() > 0 THEN
		dw_2.Object.mfge_totbul[1]	=	Round(dw_3.Object.total_bultos[1], 0)
		dw_2.Object.mfge_tpneto[1]	=	Round(dw_3.Object.total_kilos[1], 3)
		pb_eli_det.Enabled			=	True
	ELSE
		dw_2.Object.mfge_totbul[1]	=	0
		dw_2.Object.mfge_tpneto[1]	=	0
		pb_eli_det.Enabled			=	False
	END IF
ELSE
	lstr_mant.dw	=	dw_4
	
	OpenWithParm(iw_mantencion_2, lstr_mant)

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
END IF

IF tab_1.SelectedTab = 1 THEN
	dw_3.SetRow(il_Fila)
	dw_3.SelectRow(il_Fila, True)
ELSE
	dw_4.SetRow(il_Fila)
	dw_4.SelectRow(il_Fila, True)
END IF
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta

DO
	dw_2.SetRedraw(False)

	ll_fila_e	=	dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), &
											Integer(istr_Mant.Argumento[2]), &
											Long(istr_Mant.Argumento[3]),&
											Integer(istr_Mant.Argumento[16]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			IF dw_3.Retrieve(Integer(istr_mant.argumento[1]), &
									Integer(istr_mant.argumento[2]), &
									Long(istr_mant.argumento[3]),&
									Integer(istr_Mant.Argumento[16])) = -1 OR &
				dw_1.Retrieve(Integer(istr_mant.argumento[1]), &
									Integer(istr_mant.argumento[2]), &
									Long(istr_mant.argumento[3]),&
									Integer(istr_Mant.Argumento[16])) = -1 OR &
				dw_4.Retrieve(Integer(istr_mant.argumento[1]), &
									Integer(istr_mant.argumento[2]), &
									Long(istr_mant.argumento[3]),&
									Integer(istr_Mant.Argumento[16])) = -1 OR &
				dw_pesaje.Retrieve(Long(istr_mant.argumento[1]),&
						 	     	Long(istr_mant.argumento[2]),&
								     Long(istr_mant.argumento[3]),&
											Integer(istr_Mant.Argumento[16])) = -1  OR &
				dw_spro_bins.Retrieve(Long(istr_mant.argumento[1]),&
						 	     	Long(istr_mant.argumento[2]),&
								     Long(istr_mant.argumento[3]),&
											Integer(istr_Mant.Argumento[16])) = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", &
												"No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled	=	True
				pb_grabar.Enabled		=	True
				pb_imprimir.Enabled	=	True
				pb_ins_det.Enabled	=	True
				
				istr_Mant.Argumento[20] = String(dw_2.Object.mfge_fecmov[1])

				HabilitaEncab(False)
				
				pb_eli_det.Enabled	=	True
				
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

on w_maed_movtofrutagranel_recepcion_planta.create
int iCurrent
call super::create
this.cb_capturadatos=create cb_capturadatos
this.dw_loteenc=create dw_loteenc
this.dw_lotedet=create dw_lotedet
this.dw_pesaje=create dw_pesaje
this.dw_spro_bins=create dw_spro_bins
this.dw_exiencab=create dw_exiencab
this.dw_exideta=create dw_exideta
this.dw_exismovtodetanulos=create dw_exismovtodetanulos
this.dw_exidetaborra=create dw_exidetaborra
this.dw_bins=create dw_bins
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_capturadatos
this.Control[iCurrent+2]=this.dw_loteenc
this.Control[iCurrent+3]=this.dw_lotedet
this.Control[iCurrent+4]=this.dw_pesaje
this.Control[iCurrent+5]=this.dw_spro_bins
this.Control[iCurrent+6]=this.dw_exiencab
this.Control[iCurrent+7]=this.dw_exideta
this.Control[iCurrent+8]=this.dw_exismovtodetanulos
this.Control[iCurrent+9]=this.dw_exidetaborra
this.Control[iCurrent+10]=this.dw_bins
this.Control[iCurrent+11]=this.tab_1
end on

on w_maed_movtofrutagranel_recepcion_planta.destroy
call super::destroy
destroy(this.cb_capturadatos)
destroy(this.dw_loteenc)
destroy(this.dw_lotedet)
destroy(this.dw_pesaje)
destroy(this.dw_spro_bins)
destroy(this.dw_exiencab)
destroy(this.dw_exideta)
destroy(this.dw_exismovtodetanulos)
destroy(this.dw_exidetaborra)
destroy(this.dw_bins)
destroy(this.tab_1)
end on

event ue_modifica_detalle;Integer	li_tabpage, li_filas, li_prods
Boolean	lb_estado, lb_existe
str_mant_envases	lstr_mant

li_tabpage			=	tab_1.SelectedTab

istr_mant.agrega	= 	False
istr_mant.borra	= 	False

lb_estado			=	istr_mant.Solo_Consulta

IF li_tabpage <> 1 THEN
	lstr_mant.argumento				=	istr_mant.argumento
	lstr_mant.dw						=	istr_mant.dw
	lstr_mant.dw2						=	istr_mant.dw2
	lstr_mant.Agrega					=	istr_mant.Agrega
	lstr_mant.Borra					=	istr_mant.Borra
	lstr_mant.Solo_Consulta			=	istr_mant.Solo_Consulta
	lstr_mant.respuesta				=	istr_mant.respuesta
	lstr_mant.tipo						=	istr_mant.tipo
	lstr_mant.usuariosoloconsulta	=	istr_mant.usuariosoloconsulta

	FOR li_filas = 1 TO dw_loteenc.RowCount()
		lb_existe =	False

		FOR li_prods = 1 TO UpperBound(lstr_mant.Productores)
			IF Integer(lstr_mant.Productores[li_prods]) = dw_loteenc.Object.prod_codigo[li_filas] THEN
				lb_existe = True

			END IF
		NEXT

		IF NOT lb_existe THEN
			lstr_mant.Productores[li_prods]	=	String(dw_loteenc.Object.prod_codigo[li_filas])

		END IF
	NEXT

END IF

IF li_tabpage = 1 THEN
	IF dw_3.RowCount() > 0 THEN
		istr_mant.dw		=	dw_3

		OpenWithParm(iw_mantencion_1, istr_mant)

		dw_2.Object.mfge_totbul[1]	=	Round(dw_3.Object.total_bultos[1], 0)
		dw_2.Object.mfge_tpneto[1]	=	Round(dw_3.Object.total_kilos[1], 3)

	ELSE
		dw_2.Object.mfge_totbul[1]	=	0
		dw_2.Object.mfge_tpneto[1]	=	0

	END IF

ELSE
	IF dw_4.RowCount() > 0 THEN
		lstr_mant.dw	=	dw_4
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'1'
		OpenWithParm(iw_mantencion_2, lstr_mant)

	END IF

	lstr_mant.Solo_Consulta	=	lb_estado

END IF
end event

event ue_nuevo;call super::ue_nuevo;is_rut = ""

Call Super::ue_nuevo

dw_3.Reset()
dw_4.Reset()
dw_bins.Reset()
dw_exideta.Reset()
dw_exidetaborra.Reset()
dw_exismovtodetanulos.Reset()
dw_exiencab.Reset()
dw_lotedet.Reset()
dw_loteenc.Reset()
dw_pesaje.Reset()
dw_spro_bins.Reset()

//dw_2.Object.sepl_codigo.visible	=	0
//dw_2.Object.t_3.visible				=	0

dw_2.Object.plde_codigo[1]		=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.tpmv_codigo[1]	=	Integer(istr_Mant.Argumento[2])
dw_2.Object.defg_tipdoc[1] 	=  22  // Guía de Recepción
dw_2.Object.clie_codigo[1] 		=  gi_codexport //Cliente

idt_FechaSistema					=	F_FechaHora()

id_FechaSistema					=	Date(idt_FechaSistema)
it_FechaSistema					=	Time(Mid(String(idt_FechaSistema),10))

dw_2.Object.mfge_fecmov[1]	=	id_FechaSistema
dw_2.Object.refg_horaen[1]	=	it_FechaSistema
end event

event ue_antesguardar;Long		ll_Fila, ll_Productor, li_findprod
Integer	li_Secuencia, li_Planta, li_cliente, li_TipoMovto, li_TipoMovtoEnva, li_control, li_estado
Boolean     lb_Actualiza_Fruta = FALSE, lb_Actualiza_Envase = FALSE, lb_retorno = TRUE

If dw_3.RowCount() > 0 Then
	If dw_3.Object.total_bultos[dw_3.RowCount()] > 9999 Then
		Messagebox("Error de Consistencia","La cantidad de bultos no es permitida")
		Message.DoubleParm = -1
		Return
	End If 
End If

If dw_4.RowCount() > 0 Then
	If dw_4.Object.total_pesoenv[dw_4.RowCount()] > 99999 Then
		Messagebox("Error de Consistencia","El peso de envases supera lo permitido")
		Message.DoubleParm = -1
		Return
	End If
	
	If dw_4.Object.total_envases[dw_4.RowCount()] > 9999 Then
		Messagebox("Error de Consistencia","La cantidad de envases supera lo permitido")
		Message.DoubleParm = -1
		Return
	End If
End If

If NOT revisaenvases() Then
	Message.DoubleParm = -1
	Return
End If	

ib_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

li_cliente			=	dw_2.Object.clie_codigo[1]
li_Planta			=	dw_2.Object.plde_codigo[1]
li_TipoMovto	=	dw_2.Object.tpmv_codigo[1]
li_TipoMovtoEnva  =  42

If dw_2.GetItemStatus(1, 0, Primary!) = NewModIfied! Then
	If il_NumEnva=0 Then
		
		iuo_TipoMovtoEnva.bloqueacorrel()	
		il_NumEnva = iuo_TipoMovtoEnva.UltimoCorrelativo(4,li_TipoMovtoEnva,li_Planta) 
	
		If il_NumEnva = 0 OR IsNull(il_NumEnva) Then
			Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
			Message.DoubleParm = -1
			Return
		Else
			lb_Actualiza_Envase = TRUE
		End If
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
   End If	
		
	dw_2.Object.mfge_numero[1]	=	il_NumFruta
	dw_2.Object.mfge_estmov[1]	=	3

	Determina_ProductoresEnvase(li_TipoMovtoEnva)
	
	//Resetea Movto de Envase en caso de errores previos en la grabación.
	dw_1.Reset()
	
	FOR ll_Productor = 1 TO UpperBound(wstr_Prod_Enva.Productor)
		ll_Fila										=	dw_1.InsertRow(0)
		
		dw_1.Object.plde_codigo[ll_Fila]		=	dw_2.Object.plde_codigo[1]
		dw_1.Object.tpmv_codigo[ll_Fila]		=	li_TipoMovtoEnva
		dw_1.Object.meen_numero[ll_Fila]		=	il_NumEnva
		dw_1.Object.meen_modulo[ll_Fila]		=	1
//		dw_1.Object.plde_coorde[ll_Fila]		=	dw_2.Object.lote_pakori[1]
		dw_1.Object.prod_codigo[ll_Fila]		=	Long(wstr_Prod_Enva.Productor[ll_Productor])
		dw_1.Object.meen_guisii[ll_Fila]		=	wstr_Prod_Enva.GuiaSII[ll_Productor]
		dw_1.Object.meen_fecmov[ll_Fila]		=	dw_2.Object.mfge_fecmov[1]
		
		dw_1.Object.tpmv_codrec[ll_Fila]		=	dw_2.Object.tpmv_codigo[1]
		dw_1.Object.mfge_numero[ll_Fila]		=	il_NumFruta
		
		dw_1.Object.tran_codigo[ll_Fila]		=	dw_2.Object.tran_codigo[1]
		dw_1.Object.cami_clasIfi[ll_Fila]	=	dw_2.Object.cami_clasIfi[1]
		dw_1.Object.cami_patent[ll_Fila]		=	dw_2.Object.cami_patent[1]
		dw_1.Object.cami_patcar[ll_Fila]		=	dw_2.Object.cami_patcar[1]
		dw_1.Object.meen_rutcho[ll_Fila]		=	dw_2.Object.mfge_rutcho[1]
		dw_1.Object.meen_chofer[ll_Fila]		=	dw_2.Object.mfge_chofer[1]
		dw_1.Object.clie_codigo[ll_Fila]		=	dw_2.Object.clie_codigo[1]

		il_NumEnva++
	NEXT
	//Descuenta último Correlativo Envases acumulado.
	il_NumEnva --
	//Preguntar el Momento de Actualización
	If lb_Actualiza_Fruta  Then iuo_TipoMovtoFruta.Actualiza_Correlativo(1,li_Planta,li_TipoMovto,il_NumFruta) 
   If lb_Actualiza_Envase Then iuo_TipoMovtoEnva.Actualiza_Correlativo(4,li_Planta,li_TipoMovtoEnva,il_NumEnva) 
   ///////////////////////////////////////
Else
	il_NumFruta	=	dw_2.Object.mfge_numero[1]	
End If

SELECT	IsNull(Max(mfgd_secuen), 0) + 1
	INTO	:li_Secuencia
	FROM	dbo.spro_movtofrutagrandeta
	WHERE	plde_codigo	=	:li_Planta
	AND	tpmv_codigo	=	:li_TipoMovto
	AND	mfge_numero	=	:il_NumFruta;
	
FOR ll_Fila = 1 TO dw_3.RowCount()
	If dw_3.GetItemStatus(1, 0, Primary!) = NewModIfied! Then
		dw_3.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_3.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
		dw_3.Object.mfge_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]
		dw_3.Object.mfgd_secuen[ll_Fila]	=	li_Secuencia
		
		li_Secuencia ++
	End If
NEXT

FOR ll_Fila = dw_4.RowCount() TO 1 Step -1
	If ll_fila > 0 Then
		
		If dw_4.Object.enva_tipoen[ll_Fila] < 1 OR IsNull(dw_4.Object.enva_tipoen[ll_Fila]) Then
			dw_4.deleteRow(ll_Fila)
		Else
			li_findprod = dw_1.Find("prod_codigo = " + String(dw_4.Object.prod_codigo[ll_Fila]), 1, dw_1.RowCount())
			
			dw_4.Object.plde_codigo[ll_Fila]	=	dw_1.Object.plde_codigo[li_findprod]
			dw_4.Object.tpmv_codigo[ll_Fila]	=	dw_1.Object.tpmv_codigo[li_findprod]
			dw_4.Object.meen_numero[ll_Fila]	=	dw_1.Object.meen_numero[li_findprod]
			dw_4.Object.clie_codigo[ll_Fila]	=	dw_1.Object.clie_codigo[li_findprod]	
		End If
	End If
NEXT

FOR ll_fila = 1 TO dw_bins.RowCount()
	dw_bins.Object.clie_codigo[ll_Fila]	=	dw_1.Object.clie_codigo[1]	
NEXT

/***************************************************************************/
/*******Controles para evitar errores al insertar datos ya existentes*******/
/***************************************************************************/
/******Se opta por actualizar los datos que existian con anterioridad*******/
/*para que sean congruentes con los datos nuevos que vengan en la recepción*/
/***************************************************************************/

FOR ll_fila = 1 TO dw_1.RowCount()
	li_control = ExisteEnvaEncab(ll_fila)
	Choose Case li_control
		Case 0
			li_estado = dw_1.SetItemStatus(ll_fila, 0, Primary!, NewModIfied!)
		Case 1
			li_estado = dw_1.SetItemStatus(ll_fila, 0, Primary!, DataModIfied!)
		Case Else
			li_estado = -1
	End Choose
			
	If li_estado = -1 Then lb_retorno = False
NEXT

FOR ll_fila = 1 TO dw_2.RowCount()
	li_control = ExisteMovtoEncab(ll_fila)
	Choose Case li_control
		Case 0
			li_estado = dw_2.SetItemStatus(ll_fila, 0, Primary!, NewModIfied!)
		Case 1
			li_estado = dw_2.SetItemStatus(ll_fila, 0, Primary!, DataModIfied!)
		Case Else
			li_estado = -1
	End Choose
	
	If li_estado = -1 Then lb_retorno = False
NEXT

FOR ll_fila = 1 TO dw_3.RowCount()
	li_control = ExisteMovtoDeta(ll_fila)
	Choose Case li_control
		Case 0
			li_estado = dw_3.SetItemStatus(ll_fila, 0, Primary!, NewModIfied!)
		Case 1
			li_estado = dw_3.SetItemStatus(ll_fila, 0, Primary!, DataModIfied!)
		Case Else
			li_estado = -1
	End Choose
	
	If li_estado = -1 Then lb_retorno = False
NEXT

FOR ll_fila = 1 TO dw_4.RowCount()
	li_control = ExisteEnvaDeta(ll_fila)
	Choose Case li_control
		Case 0
			li_estado = dw_4.SetItemStatus(ll_fila, 0, Primary!, NewModIfied!)
		Case 1
			li_estado = dw_4.SetItemStatus(ll_fila, 0, Primary!, DataModIfied!)
		Case Else
			li_estado = -1
	End Choose
	
	If li_estado = -1 Then lb_retorno = False
NEXT

FOR ll_fila = 1 TO dw_loteenc.RowCount()
	li_control = ExisteLoteEnca(ll_fila)
	Choose Case li_control
		Case 0
			li_estado = dw_loteenc.SetItemStatus(ll_fila, 0, Primary!, NewModIfied!)
		Case 1
			li_estado = dw_loteenc.SetItemStatus(ll_fila, 0, Primary!, DataModIfied!)
		Case Else
			li_estado = -1
	End Choose
	
	If li_estado = -1 Then lb_retorno = False
NEXT

FOR ll_fila = 1 TO dw_lotedet.RowCount()
	li_control = ExisteLoteDeta(ll_fila)
	Choose Case li_control
		Case 0
			li_estado = dw_lotedet.SetItemStatus(ll_fila, 0, Primary!, NewModIfied!)
		Case 1
			li_estado = dw_lotedet.SetItemStatus(ll_fila, 0, Primary!, DataModIfied!)
		Case Else
			li_estado = -1
	End Choose
	
	If li_estado = -1 Then lb_retorno = False
NEXT

FOR ll_fila = 1 TO dw_pesaje.RowCount()
	dw_pesaje.Object.mfge_numero[ll_fila] = il_NumFruta
	dw_pesaje.Object.tpmv_codigo[ll_fila] = li_TipoMovto
	dw_pesaje.Object.plde_codigo[ll_fila] = dw_2.Object.plde_codigo[1]
	
	li_control = ExisteMovtoPesa(ll_fila)
	Choose Case li_control
		Case 0
			li_estado = dw_pesaje.SetItemStatus(ll_fila, 0, Primary!, NewModIfied!)
		Case 1
			li_estado = dw_pesaje.SetItemStatus(ll_fila, 0, Primary!, DataModIfied!)
		Case Else
			li_estado = -1
	End Choose
	
	If li_estado = -1 Then lb_retorno = False
NEXT

FOR ll_fila = 1 TO dw_spro_bins.RowCount()
	dw_spro_bins.Object.mfge_numero[ll_fila] = il_NumFruta
	
	li_control = ExisteMovtoBins(ll_fila)
	Choose Case li_control
		Case 0
			li_estado = dw_spro_bins.SetItemStatus(ll_fila, 0, Primary!, NewModIfied!)
		Case 1
			li_estado = dw_spro_bins.SetItemStatus(ll_fila, 0, Primary!, DataModIfied!)
		Case Else
			li_estado = -1
	End Choose
	
	If li_estado = -1 Then lb_retorno = False
NEXT

FOR ll_fila = 1 TO dw_bins.RowCount()
	li_control = ExisteBins(ll_fila)
	Choose Case li_control
		Case 0
			li_estado = dw_bins.SetItemStatus(ll_fila, 0, Primary!, NewModIfied!)
		Case 1
			li_estado = dw_bins.SetItemStatus(ll_fila, 0, Primary!, DataModIfied!)
		Case Else
			li_estado = -1
	End Choose
	li_control	=	dw_bins.Find("clie_codigo = "	+ String(dw_bins.Object.clie_codigo[ll_fila]) + " AND " + &
										 "plde_codigo = " + String(dw_bins.Object.plde_codigo[ll_fila]) + " AND " + &
										 "bins_numero = " + String(dw_bins.Object.bins_numero[ll_fila]), &
										 1, dw_bins.RowCount())

	If li_control > 0 AND li_control <> ll_fila Then
		dw_bins.DeleteRow(li_control)
		If li_control < ll_fila Then ll_fila --
	End If
		
	If li_estado = -1 Then lb_retorno = False
NEXT

If NOT lb_retorno Then Message.DoubleParm = -1
end event

event ue_borrar;Integer	li_Cliente
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

IF dw_Pesaje.RowCount() > 0 THEN
	dw_Pesaje.RowsMove(1, dw_Pesaje.RowCount(), Primary!, dw_Pesaje, 1, Delete!)
END IF

IF dw_spro_bins.RowCount() > 0 THEN
	dw_spro_bins.RowsMove(1, dw_spro_bins.RowCount(), Primary!, dw_spro_bins, 1, Delete!)
END IF

IF dw_2.DeleteRow(0) = 1 AND dw_1.DeleteRow(0) = 1 THEN
		ib_Borrar	=	False
		
		w_main.SetMicroHelp("Borrando Registro...")
		
		IF wf_actualiza_db(True) THEN
			
			//Crea conexion hacia existencia
			li_Cliente = Long(istr_Mant.Argumento[16])
			sqlexi		=	CREATE Transaction
			
			SELECT 	clie_conexi, cone_codigo
			INTO   	:il_conexiste, :il_coneccion
			FROM dbo.clientesprod
			WHERE clie_codigo = :li_Cliente;			
			
			IF il_conexiste = 1 THEN
				IF Conexionexistencia() THEN
					dw_exideta.SetTransObject(sqlexi)
					dw_exiencab.SetTransObject(sqlexi)	
					dw_exismovtodetanulos.SetTransObject(sqlexi)
					dw_exidetaborra.SetTransObject(sqlexi)
					TriggerEvent("ue_despuesborrar")
				ELSE
					MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)
				END IF
			END IF	
						
			w_main.SetMicroHelp("Registro Borrado...")
			
			MessageBox("Eliminación Exitosa", "Eliminación realizada con exito", Exclamation!)
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

event resize;call super::resize;Tab_1.x								=	dw_1.x
Tab_1.y								=	dw_1.y
Tab_1.Width						=	dw_1.Width
Tab_1.Height						=	dw_1.Height

Tab_1.Tp_1.dw_detalle.x			= 27
Tab_1.Tp_1.dw_detalle.y			= 36
Tab_1.Tp_1.dw_detalle.height		= Tab_1.height - 180
Tab_1.Tp_1.dw_detalle.width		= Tab_1.width - 92

Tab_1.Tp_2.dw_envases.x			= 27
Tab_1.Tp_2.dw_envases.y			= 36
Tab_1.Tp_2.dw_envases.height	= Tab_1.height - 180
Tab_1.Tp_2.dw_envases.width		= Tab_1.width - 92

cb_capturadatos.x 							=  pb_salir.x
cb_capturadatos.y 							=  pb_salir.y + pb_salir.Height + 10
end event

event ue_seleccion;call super::ue_seleccion;IF ib_ok = False THEN RETURN

Str_Busqueda	lstr_busq
Integer li_cliente

li_cliente = dw_2.Object.clie_codigo[1]

lstr_busq.argum[1] = String(dw_2.Object.plde_codigo[1])
lstr_busq.argum[2] = '2'								// Movimiento de Recepción
lstr_busq.argum[3] = ''									// Cualquier estado
lstr_busq.argum[4] = String(idt_FechaSistema)   // Desde Fecha de Inicio Ducha
lstr_busq.argum[10] = String(dw_2.Object.clie_codigo[1])  // Cliente

OpenWithParm(w_busc_spro_movtofrutagranenca_recepcion, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	
	istr_mant.Argumento[1]	=	lstr_busq.argum[1]
	istr_mant.Argumento[2]	=	lstr_busq.argum[2]
	istr_mant.Argumento[3]	=	lstr_busq.argum[3]
	
	This.TriggerEvent("ue_recuperadatos")
END IF
end event

event ue_guardar;Integer	li_Cliente
String		ls_numero
Date		ld_fecha

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

ls_numero = String(dw_2.Object.mfge_numero[1])
ld_fecha = dw_2.Object.mfge_fecmov[1]

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	
	li_Cliente = Long(istr_Mant.Argumento[16])
		
	SELECT clie_conexi, cone_codigo
	INTO :il_conexiste, :il_coneccion
	FROM dbo.clientesprod
	WHERE clie_codigo = :li_Cliente;
	
	IF il_conexiste = 1  THEN
		sqlexi		=	CREATE Transaction
		iuo_grabatablabitacora			=	Create uo_grabatablabitacora
		Datos_correo()
		//sqlexi2	=	CREATE Transaction
		
		IF Conexionexistencia() THEN
			dw_exideta.SetTransObject(sqlexi)
			dw_exiencab.SetTransObject(sqlexi)
			
			dw_exismovtodetanulos.SetTransObject(sqlexi)
			dw_exidetaborra.SetTransObject(sqlexi)
			TriggerEvent("ue_despuesborrar")
			TriggerEvent("ue_despuesguardar")
			Disconnect Using sqlexi;
			
			IF li_retorno = 2 THEN
				
				iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
				'Verificar Datos en Base de Existencia '+is_base,is_base,1,is_error,1,il_coderror,&
				is_correo,'Problema Control de Envases Recepción Interplanta Granel '+is_base,'Movimiento N° '+ls_numero+' Mantenedor '+This.Title+' con problemas, usuario '+gstr_us.Nombre+' Computador '+gstr_us.computador +', Error '+is_error)
				li_retorno = 0	
			ELSE	
				iuo_grabatablabitacora.actualizaestado(ls_numero,ld_fecha,This.Title)
			END IF
		ELSE
			MessageBox("Atención", "No puede Despachar.~r~r" + &
						"Falló Conexion con Existencia.", Exclamation!, Ok!)
			
			iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
			'Verificar Conexión a Base '+is_base,is_base,1,'No Se Pudo Conectar el Sistema a Base de Datos de Existencia',1,il_coderror,&
			is_correo,'Problema Control de Envases Recepción Interplanta Granel '+is_base,'Movimiento N° '+ls_numero+' Con problemas, Error en Conexión con Base de Existencia '+' Mantenedor '+This.Title+' con problemas, usuario '+gstr_us.Nombre+' Computador '+gstr_us.computador +', Error '+is_error)
			MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)			
		END IF
	END IF	

	pb_eliminar.Enabled		= True
	pb_imprimir.Enabled		= True
	iuo_traspaso.lb_LocalRemoto = False
//	iuo_traspaso.TriggerEvent("ue_borrar")
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

event ue_imprimir;Long	ll_modIf
Date	ld_FechaRecepcion

If Not istr_mant.Solo_Consulta Then
	Choose Case wf_modIfica()
		Case -1
			ib_ok = False
		Case 0
			ll_modIf	=	dw_1.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_2.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_3.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_4.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_pesaje.GetNextModIfied(0, Primary!)
		
			If dw_3.RowCount() > 0 and ll_modIf > 0 Then
				Choose Case MessageBox("Grabar registro(s)","Desea Grabar la información antes de Imprimir ?", Question!, YesNoCancel!)
					Case 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						If message.DoubleParm = -1 Then ib_ok = False
					Case 3
						ib_ok	= False
						Return
				End Choose
			End If
	End Choose
End If

If Not ib_ok Then Return

SetPointer(HourGlass!)

Long		fila
Integer li_estado, li_Kilos

istr_info.titulo	= "GUIA DE RECEPCION FRUTA GRANEL"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

li_estado					=	dw_2.object.mfge_estmov[1]
ld_FechaRecepcion	=	dw_2.object.mfge_fecmov[1]

If li_estado=1 Then
   vinf.dw_1.DataObject = 	"dw_info_guia_recepcion_Transitoria"
Else
	vinf.dw_1.DataObject = 	"dw_info_guia_recepcion_definitiva_interplanta"
	li_Kilos 				= 	0
End If

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(dw_2.Object.plde_codigo[1], dw_2.Object.tpmv_codigo[1],dw_2.Object.mfge_numero[1], dw_2.Object.clie_codigo[1],  ld_FechaRecepcion, ld_FechaRecepcion)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.",  StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If li_estado=1 Then
		vinf.dw_1.ModIfy('t_9.Text	= "Recepción InterPlanta Transitoria"')
	Else
		vinf.dw_1.ModIfy('t_9.Text	= "Recepción InterPlanta Definitiva"')
	End If
	
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutagranel_recepcion_planta
boolean visible = false
integer x = 14
integer y = 2540
integer width = 3799
integer height = 868
boolean enabled = false
string title = ""
string dataobject = "dw_mant_movtoenvaenca_origen"
boolean livescroll = false
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutagranel_recepcion_planta
integer x = 46
integer y = 32
integer width = 3282
integer height = 804
integer taborder = 10
string dataobject = "dw_mant_movtofrutagranenca_origen"
end type

event dw_2::itemchanged;String	ls_Nula
String	ls_Columna

ls_Columna = GetColumnName()
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
	CASE "espe_codigo"
		IF Not manbin_especie(This.Object.plde_codigo[row], Integer(Data), True, sqlca) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF
		
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(row,"clie_codigo",Integer(ls_Nula))
			RETURN 1
		ELSE
			istr_mant.argumento[16] = String(data)
		END IF
		
	CASE "mfge_numero"
		IF NOT ExisteRecepcion(gstr_ParamPlanta.CodigoPlanta, 2, Integer(data),Integer(istr_mant.argumento[16])) THEN
			This.SetItem(1,"mfge_numero",Long(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF

	CASE "plde_coorde"
		IF Not iuo_PltaDestino.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, ls_Nula)
			
			RETURN 1
		ELSEIF iuo_PltaDestino.Codigo = gstr_ParamPlanta.CodigoPlanta THEN
			MessageBox("Atención", "No puede Despachar a la Planta de Origen.~r~r" + &
						"Ingrese o seleccione otra Planta de Destino.", Exclamation!, Ok!)
			
			RETURN 1
		ELSE
			istr_Mant.Argumento[11] = Data
		END IF

	CASE "tran_codigo"
		IF Not iuo_Transport.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, ls_Nula)
			
			RETURN 1
		END IF

	CASE "cami_patent"
		IF Not iuo_Camion.Existe(1, Data, True, sqlca) THEN
			This.SetItem(1, ls_Columna, ls_Nula)
			
			RETURN 1
		ELSE
			This.Object.cami_patcar[1]	=	iuo_Camion.PateCarro
			This.Object.mfge_rutcho[1]	=	iuo_Camion.RutChofer
			is_rut = iuo_Camion.RutChofer
			This.Object.mfge_chofer[1]	=	iuo_Camion.Chofer
		END IF

	CASE "defg_tipdoc"
		IF Not iuo_TipoDocto.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, ls_Nula)
			
			RETURN 1
		END IF

		
	CASE "mfge_rutcho"
		
		is_rut = F_verrut(data, True)
		IF is_rut = "" THEN
			dw_2.SetItem(row, "mfge_rutcho", ls_Nula)
			dw_2.SetItem(row, "mfge_chofer", ls_Nula)
			RETURN 1
		ELSE
			IF NoExisteChofer(Data) THEN
				dw_2.SetItem(row, "mfge_rutcho", ls_Nula)
				dw_2.SetItem(row, "mfge_chofer", ls_Nula)
				RETURN 1
			ELSE	
				dw_2.SetItem(row, "mfge_chofer",is_chofer)
			END IF
		END IF	
		
	CASE "refg_diagra"
			IF Integer(data) > 999 OR Integer(data) < 0 THEN
				This.SetItem(row, "refg_diagra", Integer(ls_Nula))
				RETURN 1
			END IF	

END CHOOSE

HabilitaIngreso(ls_Columna)
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
end event

event dw_2::buttonclicked;call super::buttonclicked;
CHOOSE CASE dwo.Name
	CASE "b_camion"
		buscacamion()
		iuo_Camion.Existe(2, This.Object.cami_patent[row], True, sqlca)
CASE "buscacamion"
		buscacamion()
END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutagranel_recepcion_planta
integer x = 3991
integer y = 304
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(TRUE)
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutagranel_recepcion_planta
integer x = 3991
integer y = 556
integer taborder = 70
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutagranel_recepcion_planta
integer x = 3991
integer y = 596
integer taborder = 80
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutagranel_recepcion_planta
integer x = 3982
integer y = 1060
integer taborder = 90
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutagranel_recepcion_planta
integer x = 3991
integer y = 1312
integer taborder = 100
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutagranel_recepcion_planta
integer x = 3991
integer y = 1468
integer taborder = 30
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutagranel_recepcion_planta
integer x = 3995
integer y = 1644
integer taborder = 40
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutagranel_recepcion_planta
integer x = 3991
integer y = 52
integer taborder = 50
end type

type cb_capturadatos from commandbutton within w_maed_movtofrutagranel_recepcion_planta
integer x = 3991
integer y = 908
integer width = 302
integer height = 112
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Captura"
end type

event clicked;String				ls_directorio
Integer			li_valida, li_opcion = 1
dwitemstatus	stat

ib_ok	= True

pb_nuevo.TriggerEvent(Clicked!)

If IsValid(iuo_traspaso) Then Destroy iuo_traspaso;

iuo_traspaso				=	Create nvuo_traspaso_interplanta

iuo_traspaso.ii_sentido	=	2
iuo_traspaso.w_parent	=	Parent
iuo_traspaso.ii_planta 	= 	gstr_ParamPlanta.CodigoPlanta//dw_2.Object.plde_codigo[1]
iuo_traspaso.ii_tipo		=	22
iuo_traspaso.ii_cliente	=	dw_2.Object.clie_codigo[1]

iuo_traspaso.ii_ManAut = Messagebox("Modo de Carga", "Selección de metodo de carga. ~n~r"+&
													"Presione [SI] para Archivos Planos, [NO] para Tablas Temporales", Question!, YesNo!, 2)

iuo_traspaso.Triggerevent("ue_traspasadatos")

Istr_Mant.Argumento[20] = String(dw_2.Object.mfge_fecmov[1])

//LRBB 16.ene.2014 cheque parametros de planta y especie, cuando es captura de datos desde transferencias
If Not manbin_especie(dw_2.Object.plde_codigo[1], dw_2.Object.espe_codigo[1], True, sqlca) Then Return 1

wf_CargaBruto()

//dw_2.Object.tpmv_codigo[1] = Integer(istr_Mant.Argumento[2])
end event

type dw_loteenc from uo_dw within w_maed_movtofrutagranel_recepcion_planta
boolean visible = false
integer x = 3721
integer y = 56
integer width = 165
integer height = 128
integer taborder = 11
boolean bringtotop = true
string title = "enca lote"
string dataobject = "dw_mues_spro_lotesfrutagranel_origen"
boolean vscrollbar = false
boolean border = false
borderstyle borderstyle = styleshadowbox!
end type

type dw_lotedet from uo_dw within w_maed_movtofrutagranel_recepcion_planta
boolean visible = false
integer x = 3552
integer y = 236
integer width = 165
integer height = 128
integer taborder = 11
boolean bringtotop = true
string title = "deta lote"
string dataobject = "dw_mues_spro_lotesfrutagradet_origen"
boolean vscrollbar = false
boolean border = false
borderstyle borderstyle = styleshadowbox!
end type

type dw_pesaje from uo_dw within w_maed_movtofrutagranel_recepcion_planta
boolean visible = false
integer x = 3730
integer y = 236
integer width = 165
integer height = 128
integer taborder = 11
boolean bringtotop = true
string title = "Pesaje Bins"
string dataobject = "dw_pesaje_romana_rec_interplanta"
boolean vscrollbar = false
boolean border = false
borderstyle borderstyle = styleshadowbox!
end type

type dw_spro_bins from datawindow within w_maed_movtofrutagranel_recepcion_planta
boolean visible = false
integer x = 3525
integer y = 64
integer width = 165
integer height = 128
integer taborder = 70
boolean bringtotop = true
string title = "Movimiento Bins"
string dataobject = "dw_spro_movtobins_origen"
boolean border = false
borderstyle borderstyle = styleshadowbox!
end type

type dw_exiencab from datawindow within w_maed_movtofrutagranel_recepcion_planta
boolean visible = false
integer x = 1531
integer y = 3028
integer width = 1650
integer height = 580
integer taborder = 80
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

type dw_exideta from datawindow within w_maed_movtofrutagranel_recepcion_planta
boolean visible = false
integer x = 242
integer y = 3112
integer width = 974
integer height = 668
integer taborder = 70
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

type dw_exismovtodetanulos from datawindow within w_maed_movtofrutagranel_recepcion_planta
boolean visible = false
integer x = 4722
integer y = 1952
integer width = 686
integer height = 400
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_inserta_exismovtodetanulos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exidetaborra from datawindow within w_maed_movtofrutagranel_recepcion_planta
boolean visible = false
integer x = 3406
integer y = 236
integer width = 165
integer height = 128
integer taborder = 80
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta_borra"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

type dw_bins from datawindow within w_maed_movtofrutagranel_recepcion_planta
boolean visible = false
integer x = 3323
integer y = 64
integer width = 174
integer height = 128
integer taborder = 30
string title = "Bins"
string dataobject = "dw_mues_spro_bins_origen"
boolean border = false
borderstyle borderstyle = styleshadowbox!
end type

type tab_1 from tab within w_maed_movtofrutagranel_recepcion_planta
event create ( )
event destroy ( )
integer x = 41
integer y = 860
integer width = 3799
integer height = 972
integer taborder = 20
boolean bringtotop = true
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
		pb_eli_det.Enabled	=	True
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
		pb_eli_det.Enabled	=	True
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
integer width = 3762
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
integer x = 23
integer y = 12
integer width = 3726
integer height = 812
integer taborder = 11
string dataobject = "dw_mues_movtofrutagraneldeta_origen"
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

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_recepcion_planta.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutagranel_recepcion_planta.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_recepcion_planta.PostEvent("ue_seteafila")

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
integer width = 3762
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
integer x = 14
integer y = 24
integer width = 3639
integer height = 812
integer taborder = 10
string dataobject = "dw_mues_movtoenvadeta_origen"
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

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_recepcion_planta.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutagranel_recepcion_planta.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_recepcion_planta.PostEvent("ue_seteafila")

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

