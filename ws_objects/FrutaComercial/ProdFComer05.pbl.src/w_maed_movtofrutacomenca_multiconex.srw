$PBExportHeader$w_maed_movtofrutacomenca_multiconex.srw
$PBExportComments$Recepción - Despacho de Fruta Comercial.
forward
global type w_maed_movtofrutacomenca_multiconex from w_mant_encab_deta_csd
end type
type dw_6 from datawindow within w_maed_movtofrutacomenca_multiconex
end type
type dw_5 from datawindow within w_maed_movtofrutacomenca_multiconex
end type
type cb_guisii from commandbutton within w_maed_movtofrutacomenca_multiconex
end type
type dw_pesaje from datawindow within w_maed_movtofrutacomenca_multiconex
end type
type dw_movtobins from datawindow within w_maed_movtofrutacomenca_multiconex
end type
type dw_bins from datawindow within w_maed_movtofrutacomenca_multiconex
end type
type cb_capturadatos from commandbutton within w_maed_movtofrutacomenca_multiconex
end type
type dw_exideta from datawindow within w_maed_movtofrutacomenca_multiconex
end type
type dw_exiencab from datawindow within w_maed_movtofrutacomenca_multiconex
end type
type tab_1 from tab within w_maed_movtofrutacomenca_multiconex
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
type tab_1 from tab within w_maed_movtofrutacomenca_multiconex
tp_1 tp_1
tp_2 tp_2
end type
type productoresxlote from structure within w_maed_movtofrutacomenca_multiconex
end type
type str_envase from structure within w_maed_movtofrutacomenca_multiconex
end type
type str_productores_envases from structure within w_maed_movtofrutacomenca_multiconex
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

global type w_maed_movtofrutacomenca_multiconex from w_mant_encab_deta_csd
integer width = 5166
string menuname = ""
windowstate windowstate = maximized!
event ue_imprimir ( )
event ue_carga_detalle pbm_custom27
event type long ue_despuesguardar ( )
event ue_despuesguardar2 ( )
dw_6 dw_6
dw_5 dw_5
cb_guisii cb_guisii
dw_pesaje dw_pesaje
dw_movtobins dw_movtobins
dw_bins dw_bins
cb_capturadatos cb_capturadatos
dw_exideta dw_exideta
dw_exiencab dw_exiencab
tab_1 tab_1
end type
global w_maed_movtofrutacomenca_multiconex w_maed_movtofrutacomenca_multiconex

type variables
w_mant_deta_movtofrutacomdeta_multiconex	iw_mantencion_1
w_mant_deta_movtoenvadeta						iw_mantencion_2


DataWindowChild   								idwc_PltaDest, 		idwc_Transp, 	idwc_Camion, &
														idwc_TipoMovto, 	idwc_cliente, 	idwc_productores
DataWindow										dw_3, dw_4
str_variedad											istr_variedad
str_categoria										istr_categoria

uo_plantadesp										iuo_PltaDestino
uo_Productores										iuo_Productor
uo_transportista									iuo_Transport
uo_camiones										iuo_Camion
uo_tipodoctoplanta								iuo_TipoDocto
uo_clienprove										iuo_ClienProve
uo_lotesfrutacomer								iuo_LotesFrutaComer
uo_tipomovtofruta									iuo_TipoMovtoFruta
uo_tipomovtofruta									iuo_TipoMovtoEnva
uo_clientesprod									iuo_clientes
nvuo_traspaso_interplanta_comercial			iuo_traspaso
uo_calicosechero  									iuo_calicosechero
uo_envases											iuo_envases
uo_spro_lotesfrutacomenc						iuo_Lotes
uo_grabatablabitacora							iuo_grabatablabitacora
 

Long     												il_NumFruta = 0, il_NumEnva = 0, il_coneccion, il_conexiste, il_coneccion2, li_retorno, il_coderror
Boolean												ib_Modifica, ib_AutoCommit, ib_Predefinido, ib_ConectadoExistencia
Integer												ii_TipoMovto, ii_Movto, ii_cliente, ii_bodedestino
String													is_rut, is_columna, is_rutcliente, is_archivo, is_movto, is_error, is_correo

DateTime											idt_FechaSistema
Date 													id_FechaSistema
Time													it_FechaSistema

Transaction											sqlexi//, sqlexi2

Private:
str_Productores_Envases	wstr_Prod_Enva
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
public function boolean retiraprod (long al_productor, integer ai_especie, integer ai_variedad, integer ai_categoria, date ad_fechalot)
public subroutine agregaclientea_dws ()
public function boolean valida_password ()
public function integer existebins (integer ai_fila)
public function integer existeenvadeta (integer ai_fila)
public function integer existeenvaencab (integer ai_fila)
public function integer existelotedeta (integer ai_fila)
public function integer existeloteenca (integer ai_fila)
public function integer existemovtobins (integer ai_fila)
public function integer existemovtodeta (integer ai_fila)
public function integer existemovtoencab (integer ai_fila)
public function boolean conexionexistencia ()
public subroutine carga_envases ()
public subroutine determina_productoresenvase (integer ai_tipomovto)
public function boolean conexionexistencia_2 ()
public subroutine modificaencab (integer as_movto)
public function boolean datos_correo ()
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
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_carga_detalle;Integer		li_fila, li_estado, li_control
Boolean		lb_retorno = True

w_main.SetMicroHelp("Cargando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

dw_2.Reset()

IF iuo_traspaso.dw_movtoenca.RowsCopy(1, iuo_traspaso.dw_movtoenca.RowCount(), Primary!, dw_2, 1, Primary!) 			= -1 OR &
   iuo_traspaso.dw_movtodeta.RowsCopy(1, iuo_traspaso.dw_movtodeta.RowCount(), Primary!, dw_3, 1, Primary!) 			= -1 OR &
    iuo_traspaso.dw_envaenca.RowsCopy(1, iuo_traspaso.dw_envaenca.RowCount(),  Primary!, dw_1, 1, Primary!) 			= -1 OR &
    iuo_traspaso.dw_envadeta.RowsCopy(1, iuo_traspaso.dw_envadeta.RowCount(),  Primary!, dw_4, 1, Primary!) 			= -1 OR &
    iuo_traspaso.dw_loteenca.RowsCopy(1, iuo_traspaso.dw_loteenca.RowCount(),  Primary!, dw_5, 1, Primary!) 			= -1 OR &
    iuo_traspaso.dw_lotedeta.RowsCopy(1, iuo_traspaso.dw_lotedeta.RowCount(),  Primary!, dw_6, 1, Primary!) 			= -1 OR &
   iuo_traspaso.dw_movtobins.RowsCopy(1, iuo_traspaso.dw_movtobins.RowCount(), Primary!, dw_movtobins, 1, Primary!) 			= -1 THEN
	MessageBox("Advertencia", "Han ocurrido errores en la carga de datos desde el objeto")
	lb_retorno = False
	
ELSE
	SetNull(li_estado)
	SetNull(id_FechaSistema)
	SetNull(it_FechaSistema)
	
	dw_2.Object.mfco_docrel[1] = 	dw_2.Object.mfco_numero[1]
	dw_2.Object.mfco_numero[1]	=	li_estado
	dw_2.Object.plde_coorde[1]	=	iuo_traspaso.ii_planta
//	dw_2.Object.refg_fecsal[1]	=	id_FechaSistema
	dw_2.Object.mfco_horasa[1]	=	it_FechaSistema
	
	dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
	dw_2.Object.tpmv_codigo[1]	=	Integer(istr_Mant.Argumento[2])
	dw_2.Object.mfco_tipdoc[1] =  22  			// Guía de Recepción
	dw_2.Object.clie_codigo[1] =  gi_codexport// Cliente
	
	idt_FechaSistema				=	F_FechaHora()
	
	id_FechaSistema				=	Date(idt_FechaSistema)
	it_FechaSistema				=	Time(Mid(String(idt_FechaSistema),10))
	
	dw_2.Object.mfco_fecmov[1]	=	id_FechaSistema
	dw_2.Object.mfco_horaen[1]	=	it_FechaSistema
	
END IF

IF lb_retorno THEN 
	HabilitaIngreso("mfco_fecmov")
	RETURN 1
ELSE
	RETURN -1
END IF



SetPointer(Arrow!)
end event

event type long ue_despuesguardar();integer	li_bodega, li_fila, li_cliente, li_planta,li_enva_codigo, li_enva_tipoen, li_bodevirtual,&
			li_bodzonal, li_lote_pltcod, li_lote_espcod, li_lote_codigo, li_bodedestino, li_comercial,&
			li_secuenciadesp, li_devcorreo, li_corte
Long 		ll_fila, ll_numero, li_secuencia = 1, li_filarecep, ll_filarecep, li_secuenciarece = 1,&
			ll_numerorecep, ll_fila_nueva, ll_fila_nea, ll_count, ll_docrel, ll_numnuevoini, ll_numnuevofin,&
			ll_prod_codigo, ll_bodedest
String	ls_item_codigo, ls_productor, ls_prod, ls_calidad, ls_correodestino, ls_correo, ls_texto, ls_asunto, ls_error, &
			serrormsg, ls_correozonal
Boolean	lb_AutoCommit, lb_Retorno
Date		ld_mesprocori, ld_mesprocdest

IF ib_Conectadoexistencia THEN
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	ll_docrel = dw_2.Object.mfco_numero[1]
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
		li_retorno = 2	
		is_error = 'Problema con los Parámetros de Existencia'
		RETURN 2
	ELSE
		li_bodzonal		= luo_existencia.bodzonal
		li_comercial	= luo_existencia.BodDestino
		ls_correo 		= luo_existencia.correo
		ld_mesprocori 	= luo_existencia.Mesproceso
	END IF	
		
	luo_existencia.existeencabezado(ll_docrel,li_bodzonal,1,3,True,sqlexi)
		
	IF luo_existencia.count <> 0 THEN
		li_retorno = 2	
		is_error = 'Encabezado en Existencia ya Existe'
		Return 2
	END IF	
	
	IF Not luo_existencia.correlativobode(3,li_bodzonal,li_bodzonal,True,sqlexi) THEN
		Message.DoubleParm = -1
		li_retorno = 2	
		is_error = 'Problema con los Correlativos de Existencia'
		RETURN 2
	ELSE
		ll_numero = luo_existencia.numero
	END IF	
		
	IF isnull(ll_numero) THEN
		ll_numnuevoini = Long(String(li_bodzonal)+''+'0001')
		ll_numnuevofin = Long(String(li_bodzonal)+''+'9999')
		
		INSERT INTO dbo.correlbode(bode_codigo,mden_tipdoc,cobo_bodadm,cobo_inicia,cobo_final,cobo_actual)
		VALUES(:li_bodzonal,3,:li_bodzonal,:ll_numnuevoini,:ll_numnuevofin,:ll_numnuevoini)
		USING sqlexi;
		
		IF sqlexi.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlexi,"Correlbode")
			Message.DoubleParm = -1
			sqlexi.AutoCommit	=	ib_AutoCommit
			li_retorno = 2	
			RETURN 2
		END IF
		ll_numero = ll_numnuevoini - 1
	END IF	
	
	ll_numero = ll_numero + 1
	
	ls_productor = String(dw_1.Object.prod_codigo[1],'000000')
	
	IF Not luo_existencia.Existecliente(ls_productor,True,sqlexi) THEN
		Message.DoubleParm = -1
		li_retorno = 2	
		is_error = 'Problema con el Productor en Existencia'
		RETURN 2
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
	
	IF Not iuo_PltaDestino.Existe(dw_2.Object.plde_coorde[1], True, sqlca) THEN
		li_retorno = 2	
		is_error = 'Planta de Destino NO existe'
		RETURN 2
	END IF	
	dw_exiencab.Object.mden_bodest[ll_fila_nea] 	= 	iuo_PltaDestino.paccomercial
	dw_exiencab.Object.clpr_rut[ll_fila_nea]	 	= 	ls_productor
	dw_exiencab.Object.mden_docrel[ll_fila_nea] 	= 	dw_2.Object.mfco_numero[1]
	dw_exiencab.Object.mden_fecdre[ll_fila_nea] 	= 	dw_2.Object.mfco_fecmov[1]
	dw_exiencab.Object.mden_observ[ll_fila_nea] 	= 	'Despacho Interplanta Comercial'
	dw_exiencab.Object.mden_estado[ll_fila_nea] 	= 	1
	dw_exiencab.Object.mden_pcopda[ll_fila_nea] 	=	1
	dw_exiencab.Object.mden_estaci[ll_fila_nea] 	= 	gstr_us.computador
	dw_exiencab.Object.mden_fecdig[ll_fila_nea] 	= 	Date(Today())
	dw_exiencab.Object.mden_hordig[ll_fila_nea] 	= 	Time(Now())
	
	FOR li_fila = 1 TO dw_4.RowCount()
		ll_fila				=	dw_exideta.InsertRow(0)
		
		li_enva_codigo 	= 	dw_4.Object.enva_codigo[li_fila]
		li_enva_tipoen 	= 	dw_4.Object.enva_tipoen[li_fila]
		ls_calidad			=  dw_4.Object.cale_calida[li_fila]
		
		IF Not iuo_calicosechero.Existe(li_enva_tipoen,li_enva_codigo,ls_calidad,True,sqlca) THEN
			Message.DoubleParm = -1
			li_retorno = 2	
			is_error = 'Problema con los Envases en Existencia'
			RETURN 2
		ELSE
			ls_item_codigo = iuo_calicosechero.item
		END IF	
		
		IF ls_item_codigo = '' OR IsNull(ls_item_codigo) THEN
			ls_item_codigo = luo_existencia.itgene
		END IF
	
		dw_exideta.Object.mden_tipdoc[ll_fila] 	= 	3
		dw_exideta.Object.mden_numero[ll_fila] 	= 	ll_numero
		dw_exideta.Object.mdde_secuen[ll_fila] 	= 	li_secuencia 
		dw_exideta.Object.tpmv_tipomv[ll_fila] 	= 	2
		dw_exideta.Object.tpmv_codigo[ll_fila] 	= 	2
		dw_exideta.Object.item_codigo[ll_fila] 	= 	ls_item_codigo
		dw_exideta.Object.mdde_identi[ll_fila] 	= 	''
		dw_exideta.Object.mdde_fecmov[ll_fila] 	= 	dw_2.Object.mfco_fecmov[1]
		dw_exideta.Object.bode_codigo[ll_fila] 	= 	li_bodzonal
		dw_exideta.Object.mdde_cantid[ll_fila] 	= 	dw_4.Object.fgme_cantid[li_fila]
					
		li_secuencia = li_secuencia + 1
	
	NEXT	
	
	IF luo_existencia.bodega_zonal(li_bodzonal,sqlexi,True) THEN
		ls_correozonal = luo_existencia.correozonal
	END IF	
	
	////////***correo si es que mes de proceso es mayor a fecha movto***//////////////////
	IF ld_mesprocori > dw_2.Object.mfco_fecmov[1] THEN
		ls_texto	 = "No es posible actualizar movto. despacho en Existencia, por modificación anterior al mes de proceso"
		ls_asunto = "Modifica Fruta Comercial Despacho Movto. Nº "+String(ll_docrel)
//		li_devcorreo =  Send_mail("smtp.rioblanco.cl","<frutacomercial@rioblanco.cl>","<"+ls_correo+">","<"+ls_correozonal+">","",ls_asunto,ls_texto,"",ls_error)
	
		IF (li_devcorreo<0) THEN
			messagebox("Error No" + string(li_devcorreo),sErrorMsg)
		END IF
		li_corte = 1
	END IF	
	
	IF dw_exiencab.Rowcount() > 0 OR li_secuencia > 1 THEN
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
	
		sqlexi.AutoCommit		=	lb_AutoCommit
		dw_exideta.Reset()
		dw_exiencab.Reset()
		ib_Conectadoexistencia = False
		DISCONNECT USING sqlexi;
		//envío correo a encargados de bodega
		ls_texto	 = "Se despachó a planta "+String(dw_2.Object.plde_coorde[1])+' desde planta '+String(dw_2.Object.plde_codigo[1])+".~nEl Nº despacho generado en existencia es "+String(ll_numero)
			
		ls_asunto = "Despacho interplanta Nº "+String(ll_docrel)
//		li_devcorreo =  Send_mail("smtp.rioblanco.cl","<frutaComerciall@rioblanco.cl>","<"+ls_correo+">","<"+ls_correozonal+">","",ls_asunto,ls_texto,"",ls_error)
		
		IF (li_devcorreo<0) THEN
			messagebox("Error No" + string(li_devcorreo),sErrorMsg)
		END IF
	END IF	
END IF	


end event

event ue_despuesguardar2();integer	li_bodega, li_fila, li_cliente, li_planta,li_enva_codigo, li_enva_tipoen, li_bodevirtual,&
			li_bodzonal, li_lote_pltcod, li_lote_espcod, li_lote_codigo, li_bodedestino, li_comercial,&
			li_secuenciadesp, li_devcorreo, li_corte
Long 		ll_fila, ll_numero, li_secuencia = 1, li_filarecep, ll_filarecep, li_secuenciarece = 1,&
			ll_numerorecep, ll_fila_nueva, ll_fila_nea, ll_count, ll_docrel, ll_numnuevoini, ll_numnuevofin,&
			ll_prod_codigo, ll_bodedest
String	ls_item_codigo, ls_productor, ls_prod, ls_calidad, ls_correodestino, ls_correo, ls_texto, ls_asunto, ls_error, serrormsg
Boolean	lb_AutoCommit, lb_Retorno
Date		ld_mesprocori, ld_mesprocdest

IF ib_Conectadoexistencia THEN
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	ll_docrel = dw_2.Object.mfco_numero[1]
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
	ELSE
		li_bodzonal		= luo_existencia.bodzonal
		li_comercial	= luo_existencia.BodDestino
		ls_correo 		= luo_existencia.correo
		ld_mesprocori 	= luo_existencia.Mesproceso
	END IF	
		
	luo_existencia.existeencabezado(ll_docrel,li_comercial,1,1,True,sqlexi)
		
	IF luo_existencia.count <> 0 THEN
		Return 
	END IF	
	
	IF Not luo_existencia.correlativobode(1,li_comercial,li_bodzonal,True,sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
	ELSE
		ll_numero = luo_existencia.numero
	END IF	
		
	IF isnull(ll_numero) THEN
		ll_numnuevoini = Long(String(li_comercial)+''+'0001')
		ll_numnuevofin = Long(String(li_comercial)+''+'9999')
		
		INSERT INTO dbo.correlbode(bode_codigo,mden_tipdoc,cobo_bodadm,cobo_inicia,cobo_final,cobo_actual)
		VALUES(:li_comercial,3,:li_bodzonal,:ll_numnuevoini,:ll_numnuevofin,:ll_numnuevoini)
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
	
//	li_lote_pltcod = dw_3.Object.lofc_pltcod[1]
//	li_lote_espcod = dw_3.Object.lofc_espcod[1]
//	li_lote_codigo = dw_3.Object.lofc_lotefc[1]
//		
//	SELECT prod_codigo
//	INTO :ll_prod_codigo
//	FROM dbo.spro_lotesfrutagranel
//	WHERE lote_pltcod = :li_lote_pltcod
//		AND	lote_espcod = :li_lote_espcod
//		AND	lote_codigo = :li_lote_codigo;
	
	ls_productor = String(dw_4.Object.prod_codigo[1],'000000')
	
	IF Not luo_existencia.Existecliente(ls_productor,True,sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
	ELSE	
		ls_productor = luo_existencia.prod
	END IF
	
	IF luo_existencia.prod = '' OR isnull(luo_existencia.prod) THEN
		ls_productor = luo_existencia.prdgen
	END IF	
	
	IF Not iuo_PltaDestino.Existe(dw_2.Object.plde_coorde[1], True, sqlca) THEN
		RETURN 
	END IF

	ll_fila_nueva 		= 	dw_exiencab.InsertRow(0)

	dw_exiencab.Object.mden_tipdoc[ll_fila_nueva] 	= 	1
	dw_exiencab.Object.mden_numero[ll_fila_nueva] 	= 	ll_numero
	dw_exiencab.Object.tpdo_codigo[ll_fila_nueva] 	= 	2
	dw_exiencab.Object.mden_fecmov[ll_fila_nueva] 	= 	dw_2.Object.mfco_fecmov[1]
	dw_exiencab.Object.tpmv_tipomv[ll_fila_nueva] 	= 	1
	dw_exiencab.Object.tpmv_codigo[ll_fila_nueva] 	= 	2
	dw_exiencab.Object.mden_tipana[ll_fila_nueva] 	= 	3
	dw_exiencab.Object.bode_codigo[ll_fila_nueva] 	= 	li_comercial
	dw_exiencab.Object.mden_bodest[ll_fila_nueva] 	= 	iuo_PltaDestino.paccomercial
	dw_exiencab.Object.clpr_rut[ll_fila_nueva]	 	= 	ls_productor
	dw_exiencab.Object.mden_docrel[ll_fila_nueva] 	= 	dw_2.Object.mfco_numero[1]
	dw_exiencab.Object.mden_fecdre[ll_fila_nueva] 	= 	dw_2.Object.mfco_fecmov[1]
	dw_exiencab.Object.mden_observ[ll_fila_nueva] 	= 	'Recepción Desde bodega Virtual Interplanta'
	dw_exiencab.Object.mden_estado[ll_fila_nueva] 	= 	1
	dw_exiencab.Object.mden_pcopda[ll_fila_nueva] 	= 	1
	
	FOR li_filarecep = 1 TO dw_4.RowCount()
		ll_filarecep		=	dw_exideta.InsertRow(0)
		
		li_enva_codigo	=	dw_4.Object.enva_codigo[li_filarecep]
		li_enva_tipoen =	dw_4.Object.enva_tipoen[li_filarecep]
		ls_calidad		=  dw_4.Object.cale_calida[li_filarecep]
	
		IF Not iuo_calicosechero.Existe(li_enva_tipoen,li_enva_codigo,ls_calidad,True,sqlca) THEN
			Message.DoubleParm = -1
			RETURN 
		ELSE
			ls_item_codigo = iuo_calicosechero.item 
		END IF	
	
		IF ls_item_codigo = '' OR IsNull(ls_item_codigo) THEN
			ls_item_codigo = luo_existencia.itgene
		END IF
		
		dw_exideta.Object.mden_tipdoc[ll_filarecep] 	= 	1
		dw_exideta.Object.mden_numero[ll_filarecep] 	= 	ll_numero
		dw_exideta.Object.mdde_secuen[ll_filarecep] 	= 	li_secuenciadesp 
		dw_exideta.Object.tpmv_tipomv[ll_filarecep] 	=	1
		dw_exideta.Object.tpmv_codigo[ll_filarecep] 	=	3
		dw_exideta.Object.item_codigo[ll_filarecep]	=	ls_item_codigo
		dw_exideta.Object.mdde_identi[ll_filarecep] 	=	''
		dw_exideta.Object.mdde_fecmov[ll_filarecep] 	= 	dw_2.Object.mfco_fecmov[1]
		dw_exideta.Object.bode_codigo[ll_filarecep] 	= 	li_comercial
		dw_exideta.Object.mdde_cantid[ll_filarecep] 	= 	dw_4.Object.fgme_cantid[li_filarecep]
		li_secuenciadesp 										= 	li_secuenciadesp + 1
	
	NEXT
	
	////////////envía correo si se ejecuta un despacho con fercha menor al mes de proceso///////////////	
	IF ld_mesprocori > dw_2.Object.mfco_fecmov[1] THEN
		ls_texto	 = "No es posible actualizar movto. despacho Inter Planta Existencia, por modificación anterior al mes de proceso"
		ls_asunto = "Modifica Fruta Granel Despacho Movto. Nº "+String(dw_2.Object.mfge_numero[1])
//		li_devcorreo =  Send_mail("smtp.rioblanco.cl","<frutagranel@rioblanco.cl>","<"+ls_correo+">","","",ls_asunto,ls_texto,"",ls_error)
	
		IF (li_devcorreo<0) THEN
			messagebox("Error No" + string(li_devcorreo),sErrorMsg)
		END IF
		dw_exideta.Reset()
		dw_exiencab.Reset()
		Return 
	END IF
	
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
	
	//envío correo a encargados de bodega
	
	ls_texto	 = "Se Recepcionó en planta "+String(dw_2.Object.plde_codigo[1])+' desde planta '+String(dw_2.Object.plde_coorde[1])+".~nEl Nº Recepción generado en existencia es "+String(ll_numero)
		
	ls_asunto = "Despacho interplanta Nº "+String(ll_docrel)
//	li_devcorreo =  Send_mail("smtp.rioblanco.cl","<frutaComerciall@rioblanco.cl>","<"+ls_correo+">","","",ls_asunto,ls_texto,"",ls_error)
	
	IF (li_devcorreo<0) THEN
		messagebox("Error No" + string(li_devcorreo),sErrorMsg)
	END IF
END IF
end event

public subroutine habilitaencab (boolean habilita);If Habilita Then	
	dw_2.Object.mfco_numero.Protect						=	0
	dw_2.Object.mfco_fecmov.Protect						=	0
	dw_2.Object.clie_codigo.Protect							=	0
	dw_2.Object.plde_codigo.Protect							=	0
	dw_2.Object.mfco_numero.Color							=	0
	dw_2.Object.mfco_fecmov.Color							=	0
	dw_2.Object.clie_codigo.Color								=	0
	dw_2.Object.plde_codigo.Color							=	0
	dw_2.Object.mfco_numero.BackGround.Color			=	RGB(255,255,255)
	dw_2.Object.mfco_fecmov.BackGround.Color			=	RGB(255,255,255)
	dw_2.Object.clie_codigo.BackGround.Color				=	RGB(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color			=	RGB(255,255,255)
   
	If integer(istr_mant.argumento[4]) = 1 Then
      	dw_2.Object.mfco_horaen.Protect					=	0
		dw_2.Object.mfco_horaen.Color						=	0
  		dw_2.Object.mfco_horaen.BackGround.Color		=	RGB(255,255,255)
   Else		  
		dw_2.Object.mfco_horasa.Protect					=	0
		dw_2.Object.mfco_horasa.Color						=	0
		dw_2.Object.mfco_horasa.BackGround.Color		=	RGB(255,255,255)
	End If	

   If integer(istr_mant.argumento[2]) = 2 Then
		dw_2.Object.plde_coorde.Protect					=	0
		dw_2.Object.tran_codigo.Protect					=	0
		dw_2.Object.plde_coorde.Color					=	0
		dw_2.Object.tran_codigo.Color					=	0
		dw_2.Object.plde_coorde.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
	End If	

	If integer(istr_mant.argumento[2]) = 22 Then
		dw_2.Object.moti_codigo.Protect					=	0
		dw_2.Object.plde_coorde.Protect					=	0
		dw_2.Object.tran_codigo.Protect					=	0
		dw_2.Object.moti_codigo.Color		=	0
		dw_2.Object.plde_coorde.Color		=	0
		dw_2.Object.tran_codigo.Color		=	0

		dw_2.Object.moti_codigo.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.plde_coorde.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
	End If
	
	If integer(istr_mant.argumento[2]) = 23 Then
		
		dw_2.Object.moti_codigo.Protect					=	0
		dw_2.Object.prod_codigo.Protect					=	0
		dw_2.Object.tran_codigo.Protect					=	0
		dw_2.Object.moti_codigo.Color		=	0
		dw_2.Object.prod_codigo.Color		=	0
		dw_2.Object.tran_codigo.Color		=	0
		
		dw_2.Object.moti_codigo.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.prod_codigo.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
	End If
	
	If integer(istr_mant.argumento[2]) = 34 Then
		dw_2.Object.moti_codigo.Protect					=	0
		dw_2.Object.tran_codigo.Protect					=	0
		dw_2.Object.moti_codigo.Color		=	0
		dw_2.Object.tran_codigo.Color		=	0
		
		dw_2.Object.moti_codigo.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
	End If
		
	dw_2.Object.cami_patent.Protect						=	0
	dw_2.Object.cami_patcar.Protect						=	0
	dw_2.Object.mfco_rutcho.Protect					=	0
	dw_2.Object.mfco_chofer.Protect					=	0
	dw_2.Object.cami_patent.Color			=	0
	dw_2.Object.cami_patcar.Color			=	0
	dw_2.Object.mfco_rutcho.Color			=	0
	dw_2.Object.mfco_chofer.Color			=	0
	
	dw_2.Object.cami_patent.BackGround.Color			=	RGB(255,255,255)
	dw_2.Object.cami_patcar.BackGround.Color			=	RGB(255,255,255)
	dw_2.Object.mfco_rutcho.BackGround.Color			=	RGB(255,255,255)
	dw_2.Object.mfco_chofer.BackGround.Color			=	RGB(255,255,255)
	
	If integer(istr_mant.argumento[2]) <> 25 Then dw_2.Object.b_camion.visible	=	1
	If integer(istr_mant.argumento[2]) = 25 Then dw_2.Object.b_docrel.visible	=	1
Else
	
	dw_2.Object.mfco_numero.Protect						=	1
	dw_2.Object.mfco_fecmov.Protect						=	1
	dw_2.Object.mfco_numero.Color			=	RGB(255,255,255)
	dw_2.Object.mfco_fecmov.Color			=	RGB(255,255,255)
	
	dw_2.Object.mfco_numero.BackGround.Color			=	553648127
	dw_2.Object.mfco_fecmov.BackGround.Color			=	553648127
	
	dw_2.Object.clie_codigo.Protect						=	1
	dw_2.Object.clie_codigo.Color			=	RGB(255,255,255)
	dw_2.Object.clie_codigo.BackGround.Color			=	553648127
	dw_2.Object.plde_codigo.Protect						=	1
	dw_2.Object.plde_codigo.Color			=	RGB(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color			=	553648127

	If integer(istr_mant.argumento[4]) = 1 Then	
		dw_2.Object.mfco_horaen.Protect					=	1
		dw_2.Object.mfco_horaen.Color			=	RGB(255,255,255)
		dw_2.Object.mfco_horaen.BackGround.Color		=	553648127
	Else	
		dw_2.Object.mfco_horasa.Protect					=	1
		dw_2.Object.mfco_horasa.Color			=	RGB(255,255,255)
		dw_2.Object.mfco_horasa.BackGround.Color		=	553648127
	End If
	
	If integer(istr_mant.argumento[2]) = 2 Then
		dw_2.Object.plde_coorde.Protect					=	1
		dw_2.Object.plde_coorde.Color			=	RGB(255,255,255)
		dw_2.Object.plde_coorde.BackGround.Color		=	553648127
		dw_2.Object.tran_codigo.Protect					=	1
		dw_2.Object.tran_codigo.Color			=	RGB(255,255,255)
		dw_2.Object.tran_codigo.BackGround.Color		=	553648127
		dw_2.Object.mfco_guisii.Protect					=	1
		dw_2.Object.mfco_guisii.Color			=	RGB(255,255,255)
		dw_2.Object.mfco_guisii.BackGround.Color		=	553648127
	End If	

	If integer(istr_mant.argumento[2]) = 22 Then
		dw_2.Object.moti_codigo.Protect					=	1
		dw_2.Object.moti_codigo.Color			=	RGB(255,255,255)
		dw_2.Object.moti_codigo.BackGround.Color		=	553648127
		dw_2.Object.plde_coorde.Protect					=	1
		dw_2.Object.plde_coorde.Color			=	RGB(255,255,255)
		dw_2.Object.plde_coorde.BackGround.Color		=	553648127
		dw_2.Object.tran_codigo.Protect					=	1
		dw_2.Object.tran_codigo.Color			=	RGB(255,255,255)
		dw_2.Object.tran_codigo.BackGround.Color		=	553648127
		dw_2.Object.mfco_guisii.Protect					=	1
		dw_2.Object.mfco_guisii.Color			=	RGB(255,255,255)
		dw_2.Object.mfco_guisii.BackGround.Color		=	553648127

	End If
	
	If integer(istr_mant.argumento[2]) = 23 Then
		dw_2.Object.moti_codigo.Protect					=	1
		dw_2.Object.moti_codigo.Color			=	RGB(255,255,255)
		dw_2.Object.moti_codigo.BackGround.Color		=	553648127
		dw_2.Object.prod_codigo.Protect					=	1
		dw_2.Object.prod_codigo.Color			=	RGB(255,255,255)
		dw_2.Object.prod_codigo.BackGround.Color		=	553648127
		dw_2.Object.tran_codigo.Protect					=	1
		dw_2.Object.tran_codigo.Color			=	RGB(255,255,255)
		dw_2.Object.tran_codigo.BackGround.Color		=	553648127
		dw_2.Object.mfco_guisii.Protect					=	1
		dw_2.Object.mfco_guisii.Color			=	RGB(255,255,255)
		dw_2.Object.mfco_guisii.BackGround.Color		=	553648127

	End If
	
	If integer(istr_mant.argumento[2]) = 25 Then
		dw_2.Object.plde_coorde.Protect					=	1
		dw_2.Object.plde_coorde.Color			=	RGB(255,255,255)
		dw_2.Object.plde_coorde.BackGround.Color		=	553648127
		dw_2.Object.mfco_docrel.Protect					=	1
		dw_2.Object.mfco_docrel.Color			=	RGB(255,255,255)
		dw_2.Object.mfco_docrel.BackGround.Color		=	553648127
	End If
	
	If integer(istr_mant.argumento[2]) = 34 Then
		dw_2.Object.moti_codigo.Protect					=	1
		dw_2.Object.moti_codigo.Color			=	RGB(255,255,255)
		dw_2.Object.moti_codigo.BackGround.Color	=	553648127
		dw_2.Object.tran_codigo.Protect					=	1
		dw_2.Object.tran_codigo.Color			=	RGB(255,255,255)
		dw_2.Object.tran_codigo.BackGround.Color	=	553648127
		dw_2.Object.mfco_guisii.Protect					=	1
		dw_2.Object.mfco_guisii.Color			=	RGB(255,255,255)
		dw_2.Object.mfco_guisii.BackGround.Color		=	553648127
	End If
	
	dw_2.Object.cami_patent.Protect						=	1
	dw_2.Object.cami_patent.Color			=	RGB(255,255,255)
	dw_2.Object.cami_patent.BackGround.Color			=	553648127
	dw_2.Object.cami_patcar.Protect						=	1
	dw_2.Object.cami_patcar.Color			=	RGB(255,255,255)
	dw_2.Object.cami_patcar.BackGround.Color			=	553648127
	dw_2.Object.mfco_rutcho.Protect						=	1
	dw_2.Object.mfco_rutcho.Color			=	RGB(255,255,255)
	dw_2.Object.mfco_rutcho.BackGround.Color			=	553648127
	dw_2.Object.mfco_chofer.Protect						=	1
	dw_2.Object.mfco_chofer.Color			=	RGB(255,255,255)
	dw_2.Object.mfco_chofer.BackGround.Color			=	553648127

	dw_2.Object.b_camion.visible							=	0
	dw_2.Object.b_docrel.visible							=	0
	
End If
end subroutine

public function boolean existeorden (integer ai_planta, integer ai_tipo, long al_numero);Integer	li_especie
Boolean	lb_Retorno = True

SELECT	espe_codigo
	INTO	:li_especie
	FROM	dbo.spro_doctointernopack
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
		IF dw_3.Update(True, False) = 1 THEN
			IF dw_1.Update(True, False) = 1 THEN
				IF dw_6.Update(True, False) = 1 THEN
					IF dw_5.Update(True, False) = 1 THEN
						IF dw_2.Update(True, False) = 1 THEN
							IF dw_bins.Update(True, False) = 1 THEN
								IF dw_movtobins.Update(True, False) = 1 THEN
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
										dw_6.ResetUpdate()
										dw_bins.ResetUpdate()
										dw_movtobins.ResetUpdate()
										
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
		IF dw_bins.Update(True, False) = 1 THEN
			IF dw_5.Update(True, False) = 1 THEN
				IF dw_6.Update(True, False) = 1 THEN
					IF dw_1.Update(True, False) = 1 THEN
						IF dw_3.Update(True, False) = 1 THEN
							IF dw_4.Update(True, False) = 1 THEN
//								IF dw_movtobins.Update(True, False) = 1 THEN
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
										dw_6.ResetUpdate()
										dw_bins.ResetUpdate()
										
									END IF
//								ELSE
//									F_ErrorBaseDatos(sqlca, This.Title)
//							
//									RollBack;
//								END IF
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

public function boolean existemovimientodoc (integer ai_tipodoc, long al_docrel);Integer li_tipomov, li_planta
Long    ll_numero

li_planta  = Integer(istr_mant.argumento[1])
li_tipomov = Integer(istr_mant.argumento[2])

SELECT mfco_numero INTO :ll_Numero
  FROM dbo.spro_movtofrutacomenca
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

IF UpperBound(istr_Mant.Argumento) < 4 THEN Return

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
	dw_2.Object.cami_clasifi[1]	=	Integer(lstr_busq.argum[1])
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
	FROM	dbo.spro_movtofrutacomenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfco_numero	=	:al_Numero;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento Fruta Comercial")
	
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

public function boolean retiraprod (long al_productor, integer ai_especie, integer ai_variedad, integer ai_categoria, date ad_fechalot);long ll_Productor

SELECT   prod_codigo
INTO		:ll_productor
FROM		dbo.spro_pararetiroprod as prp
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

public function integer existebins (integer ai_fila);Integer	li_cliente, li_planta, li_numero, li_cuenta

li_cliente	= 	dw_bins.Object.clie_codigo[ai_fila]
li_planta 	= 	dw_bins.Object.plde_codigo[ai_fila]
li_numero	= 	dw_bins.Object.bins_numero[ai_fila]

SELECT count(*)
INTO :li_cuenta
FROM dbo.spro_Bins
WHERE clie_codigo = :li_cliente
  AND plde_codigo = :li_planta
  AND bins_numero = :li_numero
  USING sqlca;
  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_Bins")
	Return -1
ELSEIF li_cuenta > 0 THEN
	dw_bins.RowsDiscard(ai_fila, ai_fila, Primary!)
	Return ai_fila - 1
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
	dw_4.RowsDiscard(ai_fila, ai_fila, Primary!)
	Return ai_fila - 1
ELSE
	Return 0
END IF
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
	dw_1.RowsDiscard(ai_fila, ai_fila, Primary!)
	Return ai_fila - 1
ELSE
	Return 0
END IF
end function

public function integer existelotedeta (integer ai_fila);Integer	li_planta, li_especie, li_lote, li_secuen, li_cuenta

li_planta	= 	dw_6.Object.lofc_pltcod[ai_fila]
li_especie 	= 	dw_6.Object.lofc_espcod[ai_fila]
li_lote 		= 	dw_6.Object.lofc_lotefc[ai_fila]
li_secuen	= 	dw_6.Object.lfcd_secuen[ai_fila]

SELECT count(*)
INTO :li_cuenta
FROM dbo.spro_lotesfrutacomdeta
WHERE lofc_pltcod = :li_planta
  AND lofc_espcod = :li_especie
  AND lofc_lotefc = :li_lote
  AND lfcd_secuen = :li_secuen
  USING sqlca;
  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_lotesfrutacomdeta")
	Return -1
ELSEIF li_cuenta > 0 THEN
	dw_6.RowsDiscard(ai_fila, ai_fila, Primary!)
	Return ai_fila - 1
ELSE
	Return 0
END IF
end function

public function integer existeloteenca (integer ai_fila);Integer	li_planta, li_especie, li_lote, li_cuenta

li_planta	= 	dw_5.Object.lofc_pltcod[ai_fila]
li_especie 	= 	dw_5.Object.lofc_espcod[ai_fila]
li_lote 		= 	dw_5.Object.lofc_lotefc[ai_fila]

SELECT count(*)
INTO :li_cuenta
FROM dbo.spro_lotesfrutacomenc
WHERE lofc_pltcod = :li_planta
  AND lofc_espcod = :li_especie
  AND lofc_lotefc = :li_lote
  USING sqlca;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_lotesfrutacomenc")
	Return -1
ELSEIF li_cuenta > 0 THEN
	dw_6.RowsDiscard(ai_fila, ai_fila, Primary!)
	Return ai_fila - 1
ELSE
	Return 0
END IF
end function

public function integer existemovtobins (integer ai_fila);Integer	li_cliente, li_planta, li_tarja, li_bins, li_especie, li_lote, li_cuenta

li_cliente	= 	dw_movtobins.Object.clie_codigo[ai_fila]
li_planta 	= 	dw_movtobins.Object.plde_codigo[ai_fila]
li_tarja		= 	dw_movtobins.Object.fgmb_nrotar[ai_fila]
li_bins 		= 	dw_movtobins.Object.bins_numero[ai_fila]
li_especie	= 	dw_movtobins.Object.lote_espcod[ai_fila]
li_lote	 	= 	dw_movtobins.Object.lote_codigo[ai_fila]

SELECT count(*)
INTO :li_cuenta
FROM dbo.spro_MovtoBins
WHERE clie_codigo = :li_cliente
  AND plde_codigo = :li_planta
  AND fgmb_nrotar = :li_tarja
  AND bins_numero = :li_bins
  AND lote_espcod = :li_especie
  AND lote_codigo = :li_lote
  USING sqlca;
  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_MovtoBins")
	Return -1
ELSEIF li_cuenta > 0 THEN
	dw_2.RowsDiscard(ai_fila, ai_fila, Primary!)
	Return ai_fila - 1
ELSE
	Return 0
END IF
end function

public function integer existemovtodeta (integer ai_fila);Integer	li_cliente, li_planta, li_tipo, li_numero, li_secuencia, li_cuenta

li_cliente	= 	dw_3.Object.clie_codigo[ai_fila]
li_planta 	= 	dw_3.Object.plde_codigo[ai_fila]
li_tipo 		= 	dw_3.Object.tpmv_codigo[ai_fila]
li_numero 	= 	dw_3.Object.mfco_numero[ai_fila]
li_secuencia=	dw_3.Object.mfcd_secuen[ai_fila]

SELECT count(*)
INTO :li_cuenta
FROM dbo.spro_movtofrutacomdeta
WHERE clie_codigo = :li_cliente
  AND plde_codigo = :li_planta
  AND tpmv_codigo = :li_tipo
  AND mfco_numero = :li_numero
  AND mfcd_secuen = :li_secuencia
  USING sqlca;
  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutacomdeta")
	Return -1
ELSEIF li_cuenta > 0 THEN
	dw_3.RowsDiscard(ai_fila, ai_fila, Primary!)
	Return ai_fila - 1
ELSE
	Return 0
END IF
end function

public function integer existemovtoencab (integer ai_fila);Integer	li_cliente, li_planta, li_tipo, li_numero, li_cuenta

li_cliente	= 	dw_2.Object.clie_codigo[ai_fila]
li_planta 	= 	dw_2.Object.plde_codigo[ai_fila]
li_tipo 		= 	dw_2.Object.tpmv_codigo[ai_fila]
li_numero 	= 	dw_2.Object.mfco_numero[ai_fila]

SELECT count(*)
INTO :li_cuenta
FROM dbo.spro_movtofrutacomenca
WHERE clie_codigo = :li_cliente
  AND plde_codigo = :li_planta
  AND tpmv_codigo = :li_tipo
  AND mfco_numero = :li_numero
  USING sqlca;
  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutacomenca")
	Return -1
ELSEIF li_cuenta > 0 THEN
	dw_2.RowsDiscard(ai_fila, ai_fila, Primary!)
	Return ai_fila - 1
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
sqlexi.DataBase	=	ls_nombas
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

public subroutine carga_envases ();Long			ll_Filas, ll_productor, ll_cantidad, ll_nueva
DataWindow	ldw_3
Integer		li_tipo, li_enva
String		ls_calidad

ldw_3				=	Create DataWindow
ldw_3				=	dw_3

ldw_3.SetSort("prod_codigo asc, enva_tipoen asc, enva_codigo asc, cale_calida asc")
ldw_3.Sort()

IF ldw_3.RowCount() < 1 THEN RETURN

ll_Filas			=	1
ll_productor	=	ldw_3.Object.prod_codigo[ll_Filas]
li_tipo			=	ldw_3.Object.enva_tipoen[ll_Filas]
li_enva			=	ldw_3.Object.enva_codigo[ll_Filas]
ls_calidad		=	ldw_3.Object.cale_calida[ll_Filas]

dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, dw_4.DeletedCount() + 1, Delete!) 

FOR ll_Filas = 1 TO ldw_3.RowCount()
	IF ll_productor	=	ldw_3.Object.prod_codigo[ll_Filas] AND &
		li_tipo			=	ldw_3.Object.enva_tipoen[ll_Filas] AND &
		li_enva			=	ldw_3.Object.enva_codigo[ll_Filas] AND &
		ls_calidad		=	ldw_3.Object.cale_calida[ll_Filas] THEN
		
		ll_cantidad		=	ldw_3.Object.mfcd_bulent[ll_Filas] + ll_cantidad
		
	ELSE
		
		IF NOT iuo_calicosechero.Existe(li_tipo, li_enva, ls_calidad, True, SQLCa) THEN 
			dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, dw_4.DeletedCount() + 1, Delete!)
			Return
			
		END IF
		IF NOT iuo_envases.Existe(li_tipo, li_enva, True, Sqlca) THEN
			dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, dw_4.DeletedCount() + 1, Delete!)
			Return
			
		END IF
		IF NOT iuo_productor.Existe(ll_productor, True, SQLCa) THEN
			dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, dw_4.DeletedCount() + 1, Delete!)
			Return
			
		END IF
		
		ll_nueva										=	dw_4.InsertRow(0)
		
		dw_4.Object.enva_tipoen[ll_nueva]	=	li_tipo
		dw_4.Object.enva_codigo[ll_nueva]	=	li_enva
		dw_4.Object.cale_calida[ll_nueva]	=	ls_calidad
		dw_4.Object.fgme_sentid[ll_nueva]	=	Integer(istr_Mant.Argumento[4])
		dw_4.Object.fgme_cantid[ll_nueva]	=	ll_cantidad
		dw_4.Object.fgme_pesone[ll_nueva]	=	iuo_calicosechero.Peso
		dw_4.Object.enva_nombre[ll_nueva]	=	iuo_envases.Nombre
		dw_4.Object.cale_nombre[ll_nueva]	=	iuo_calicosechero.nombre
		dw_4.Object.clie_codigo[ll_nueva]	=	dw_2.Object.clie_codigo[1] 
		dw_4.Object.prod_codigo[ll_nueva]	=	ll_productor
		dw_4.Object.prod_nombre[ll_nueva]	=	iuo_productor.Nombre
		
		ll_productor								=	ldw_3.Object.prod_codigo[ll_Filas]
		li_tipo										=	ldw_3.Object.enva_tipoen[ll_Filas]
		li_enva										=	ldw_3.Object.enva_codigo[ll_Filas]
		ls_calidad									=	ldw_3.Object.cale_calida[ll_Filas]
		ll_cantidad									=	ldw_3.Object.mfcd_bulent[ll_Filas]
	END IF
NEXT

IF ll_nueva <= ldw_3.Rowcount() THEN

	IF NOT iuo_calicosechero.Existe(li_tipo, li_enva, ls_calidad, True, SQLCa) THEN 
		dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, dw_4.DeletedCount() + 1, Delete!)
		Return
	END IF
	IF NOT iuo_envases.Existe(li_tipo, li_enva, True, Sqlca) THEN
		dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, dw_4.DeletedCount() + 1, Delete!)
		Return
	END IF
	IF NOT iuo_productor.Existe(ll_productor, True, SQLCa) THEN
		dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, dw_4.DeletedCount() + 1, Delete!)
		Return
	END IF
	
	ll_nueva										=	dw_4.InsertRow(0)
	
	dw_4.Object.plde_codigo[ll_nueva]	=	dw_2.Object.plde_codigo[1]
	dw_4.Object.tpmv_codigo[ll_nueva]	=  dw_2.Object.tpmv_codigo[1] 
	dw_4.Object.enva_tipoen[ll_nueva]	=	li_tipo
	dw_4.Object.enva_codigo[ll_nueva]	=	li_enva
	dw_4.Object.cale_calida[ll_nueva]	=	ls_calidad
	dw_4.Object.fgme_sentid[ll_nueva]	=	Integer(istr_Mant.Argumento[4])
	dw_4.Object.fgme_cantid[ll_nueva]	=	ll_cantidad
	dw_4.Object.fgme_pesone[ll_nueva]	=	iuo_calicosechero.Peso
	dw_4.Object.enva_nombre[ll_nueva]	=	iuo_envases.Nombre
	dw_4.Object.cale_nombre[ll_nueva]	=	iuo_calicosechero.nombre
	dw_4.Object.clie_codigo[ll_nueva]		=	dw_2.Object.clie_codigo[1] 
	dw_4.Object.prod_codigo[ll_nueva]	=	ll_productor
	dw_4.Object.prod_nombre[ll_nueva]	=	iuo_productor.Nombre
END IF
end subroutine

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

//IF ib_ConectadoExistencia THEN
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
//sqlexi2.DataBase		=	ls_nombas
//sqlexi2.Dbms			= 	ls_nodbms
//sqlexi2.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
//								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
//CONNECT USING sqlexi2;
//
//IF sqlexi2.SQLCode = 0 THEN
//	ib_ConectadoExistencia	=	True
//ELSE
//	ib_ConectadoExistencia	=	False
//END IF
//
RETURN ib_ConectadoExistencia

end function

public subroutine modificaencab (integer as_movto);dw_2.SetReDraw(False)

If Integer(istr_Mant.Argumento[4]) = 1 Then
	cb_guisii.Enabled = FALSE
	cb_guisii.visible = TRUE
	
	If as_Movto	=	2 Then		
		This.Title	=	"Recepción Otras Plantas"
		cb_capturadatos.Visible	=	True
		cb_capturadatos.Enabled	=	True
		
		dw_2.Object.mfco_horasa.Visible			=	False
		dw_2.ModIfy("planta_t.Text = 'Planta Origen'")

		//No Solicita Motivo 
		dw_2.Object.moti_codigo.Protect				=	1
		dw_2.Object.moti_codigo.Color				= 0
		dw_2.Object.moti_codigo.BackGround.Color	= 553648127
		
		//No Solicita Productor 
		dw_2.Object.prod_codigo.visible				=	0
		dw_2.Object.prod_codigo.Protect				=	1
		dw_2.Object.prod_codigo.Color				= 0
		dw_2.Object.prod_codigo.BackGround.Color	= 553648127
		dw_2.Object.prod_codigo_t.visible			=	0
				
		//Doc. Relacionado Guía de Recepción
		dw_2.Setitem(1,"mfco_tipdoc",1)
		dw_2.Object.mfco_tipdoc.Protect				=	1
		dw_2.Object.mfco_tipdoc.Color				= 0
		dw_2.Object.mfco_tipdoc.BackGround.Color	= 553648127
		
		//Invisible Doc. Relacionado
		dw_2.Object.mfco_docrel.visible				=	0
		dw_2.Object.mfco_docrel_t.visible			=	0
	End If
Else
	If as_Movto	=	22 Then
		This.Title	=	"Despacho Otras Plantas"		
		dw_2.Object.mfco_horaen.Visible	=	False
		dw_2.ModIfy("planta_t.Text = 'Planta Destino'")

		//Si Solicita Motivo 
		dw_2.Object.moti_codigo.Protect				=	0
		dw_2.Object.moti_codigo.Color				=	0
		dw_2.Object.moti_codigo.BackGround.Color	= RGB(255,255,255)
		
		//No Solicita Productor 
		dw_2.Object.prod_codigo.visible				=	0
		dw_2.Object.prod_codigo.Protect				=	1
		dw_2.Object.prod_codigo.Color				= RGB(255,255,255)
		dw_2.Object.prod_codigo.BackGround.Color	= 553648127
		dw_2.Object.prod_codigo_t.visible			=	0
				
		//Doc. Relacionado Guía de Recepción
		dw_2.Setitem(1,"mfco_tipdoc",2)
		dw_2.Object.mfco_tipdoc.Protect				=	1
		dw_2.Object.mfco_tipdoc.Color				=	RGB(255,255,255)
		dw_2.Object.mfco_tipdoc.BackGround.Color	= 553648127
		
		//Invisible Doc. Relacionado
		dw_2.Object.mfco_docrel.visible				=	0
		dw_2.Object.mfco_docrel_t.visible			=	0
	End If

	If as_Movto	=	23 Then
		This.Title	=	"Devolución a Productores"
		
		//Si Solicita Motivo 
		dw_2.Object.moti_codigo.Protect				=	0
		dw_2.Object.moti_codigo.Color				= 0
		dw_2.Object.moti_codigo.BackGround.Color	= RGB(255,255,255)
		
		//Si Solicita Productor 
		dw_2.Object.prod_codigo.visible				=	1
		dw_2.Object.prod_codigo.Protect				=	0
		dw_2.Object.prod_codigo.Color				=  0
		dw_2.Object.prod_codigo.BackGround.Color	= RGB(255,255,255)
		dw_2.Object.prod_codigo_t.visible			=	1
		
		//No Solicita Planta Destino
		dw_2.Object.plde_coorde.visible				=	0
		dw_2.Object.plde_coorde.Protect				=	1
		dw_2.Object.mfco_tipdoc.Color				=	RGB(255,255,255)
		dw_2.Object.plde_coorde.BackGround.Color	= 553648127
		dw_2.Object.planta_t.visible					=	0
				
		//Doc. Relacionado Guía de Recepción
		dw_2.Setitem(1,"mfco_tipdoc",2)
		dw_2.Object.mfco_tipdoc.Protect				=	1
		dw_2.Object.mfco_tipdoc.Color				=	RGB(255,255,255)
		dw_2.Object.mfco_tipdoc.BackGround.Color	= 553648127
		
		//Invisible Doc. Relacionado
		dw_2.Object.mfco_docrel.visible				=	0
		dw_2.Object.mfco_docrel_t.visible			=	0
	End If
	
	If as_Movto	=	25 Then
		This.Title	=	"Traspaso Re - Proceso"		
		dw_2.Object.mfco_horaen.Visible	=	False
		
		dw_2.ModIfy("planta_t.Text = 'Planta Destino'")

		//Noi Solicita Motivo 
		dw_2.Object.moti_codigo.visible				=	0
		dw_2.Object.moti_codigo.Protect				=	1
		dw_2.Object.moti_codigo.Color				=	RGB(255,255,255)
		dw_2.Object.moti_codigo.BackGround.Color	= 553648127
		
		//No Solicita Productor 
		dw_2.Object.prod_codigo.visible				=	0
		dw_2.Object.prod_codigo.Protect				=	1
		dw_2.Object.prod_codigo.Color				=	RGB(255,255,255)
		dw_2.Object.prod_codigo.BackGround.Color	= 553648127
		dw_2.Object.prod_codigo_t.visible			=	0
				
		//Doc. Relacionado 
		dw_2.Setitem(1,"mfco_tipdoc",5)
		dw_2.Object.mfco_tipdoc.Protect				=	1
		dw_2.Object.mfco_tipdoc.Color				=	RGB(255,255,255)
		dw_2.Object.mfco_tipdoc.BackGround.Color	= 553648127
		
		
		//productor 1 
		dw_2.Object.prod_codigo_1.visible				=	1
		dw_2.Object.prod_codigo_1.Protect				=	1
		dw_2.Object.prod_codigo_1.Color				=	RGB(255,255,255)
		dw_2.Object.prod_codigo_1.BackGround.Color= 553648127
		dw_2.Object.prod_codigo_t.visible				=	0
		dw_2.Object.productor.visible						=	1
		
		//visible Doc. Relacionado
		dw_2.Object.mfco_docrel.visible				=	1
		dw_2.Object.mfco_docrel.Protect				=	0
		dw_2.Object.mfco_docrel.Color				=	0
		dw_2.Object.mfco_docrel.BackGround.Color	= RGB(255,255,255)
		dw_2.Object.mfco_docrel_t.visible			=	1
		
		dw_2.Object.plde_coorde.Protect				=	0
		dw_2.Object.plde_coorde.Color				=	0
		dw_2.Object.plde_coorde.BackGround.Color	= RGB(255,255,255)
		
		//Invisible 
		dw_2.Object.planta_t.visible					=	0
		dw_2.Object.plde_coorde.visible				=	0
		dw_2.Object.mfco_guisii.visible				=	0
		dw_2.Object.mfco_guisii_t.visible				=	0
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
		dw_2.Object.tipo_ingdat.visible				=  0
		dw_2.Object.t_6.visible							=	0
	End If
	
	If as_Movto	=	34 Then
		This.Title	=	"Mermas"
		//Si Solicita Motivo 
		dw_2.Object.moti_codigo.Protect				=	0
		dw_2.Object.moti_codigo.Color	= 0
		dw_2.Object.moti_codigo.BackGround.Color	= RGB(255,255,255)
		
		//No Solicita Productor 
		dw_2.Object.prod_codigo.visible				=	0
		dw_2.Object.prod_codigo.Protect				=	1
		dw_2.Object.prod_codigo.Color	= RGB(255,255,255)
		dw_2.Object.prod_codigo.BackGround.Color	= 553648127
		dw_2.Object.prod_codigo_t.visible			=	0
		
		//No Solicita Planta Destino
		dw_2.Object.plde_coorde.visible				=	0
		dw_2.Object.plde_coorde.Protect				=	1
		dw_2.Object.plde_coorde.Color	= RGB(255,255,255)
		dw_2.Object.plde_coorde.BackGround.Color	= 553648127
		dw_2.Object.planta_t.visible					=	0
				
		//Doc. Relacionado Guía de Recepción
		dw_2.Setitem(1,"mfco_tipdoc",2)
		dw_2.Object.mfco_tipdoc.Protect				=	1
		dw_2.Object.mfco_tipdoc.Color	= RGB(255,255,255)
		dw_2.Object.mfco_tipdoc.BackGround.Color	= 553648127
		
		//Invisible Doc. Relacionado
		dw_2.Object.mfco_docrel.visible				=	0
		dw_2.Object.mfco_docrel_t.visible			=	0
	End If
End If

dw_2.SetReDraw(TRUE)
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

event open;/*
Argumentos

istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Tipo de Movimiento
istr_Mant.Argumento[3]	=	Número de Movimiento
istr_Mant.Argumento[4]	=	Sentido del Movimiento
istr_Mant.Argumento[5]	=	Codigo Especie para Movimiento Traspaso Re-Proceso
istr_Mant.Argumento[6]	=	Codigo Productor
istr_Mant.argumento[16] =  Codigo Cliente
*/

DataWindowChild	ldwc_Camara, ldwc_TipoEnvase, ldwc_PltaLote, ldwc_Especie
String				ls_Movto
Integer			li_TipoMovto, li_Movto

dw_2.SetReDraw(FALSE)

x												= 	0
y												= 	0
This.Height									= 	2520
im_menu										= 	m_principal

This.ParentWindow().ToolBarVisible	= 	True
im_menu.Item[1].Item[6].Enabled		= 	True
im_menu.Item[7].Visible					= 	True

dw_3											=	tab_1.tp_1.dw_detalle
dw_4											=	tab_1.tp_2.dw_envases

is_movto										= 	Message.StringParm

istr_Mant.Argumento[1]					=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[6]					=	""

IF is_movto = "" THEN
	ib_predefinido	=	False
ELSE
	istr_Mant.Argumento[2]	=	Mid(is_movto,2,2)
	istr_Mant.Argumento[4]	=	Mid(is_movto,1,1)

	li_TipoMovto				=	Integer(istr_Mant.Argumento[4])
	li_Movto						=	Integer(istr_Mant.Argumento[2])
	ib_Predefinido				=	True
	
	IF IsNull(li_TipoMovto) = False THEN
		SELECT tpmv_codigo
			INTO :ii_Movto
			FROM	dbo.spro_tipomovtofruta
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
idwc_productores.Retrieve(-1)

dw_2.GetChild("plde_coorde", idwc_PltaDest)
idwc_PltaDest.SetTransObject(sqlca)

IF idwc_PltaDest.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Plantas")
	idwc_PltaDest.InsertRow(0)
	
ELSE
	IF integer(istr_mant.argumento[2]) <> 25 THEN
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

dw_1.SetTransObject(sqlca) 			// Encabezado Movimiento de Envase
dw_2.SetTransObject(sqlca) 			// Encabezado Movimiento de Fruta Comercial
dw_3.SetTransObject(sqlca) 			// Detalle de Movimiento de Fruta Comercial
dw_4.SetTransObject(sqlca) 			// Detalle de Movimiento de Envase
dw_5.SetTransObject(sqlca) 			// Encabezado Lotes Fruta Comercial
dw_6.SetTransObject(sqlca) 			// Detalle de Lotes Fruta Comercial
dw_pesaje.SetTransObject(sqlca) 		// Movimientos de Pesaje
dw_movtobins.SetTransObject(sqlca) 	// Movimientos de Bins
dw_bins.SetTransObject(sqlca) 		// Tabla Maestra de Bins

dw_3.Modify("datawindow.message.title='Error '+ is_titulo")
dw_3.SetRowFocusIndicator(Hand!)
dw_3.Modify("DataWindow.Footer.Height = 88")

dw_4.Modify("datawindow.message.title='Error '+ is_titulo")
dw_4.SetRowFocusIndicator(Hand!)
dw_4.Modify("DataWindow.Footer.Height = 88")

istr_Mant.dw				=	dw_1
istr_Mant.solo_consulta 	=	False

iuo_PltaDestino				=	Create uo_plantadesp
iuo_Productor				=	Create uo_Productores
iuo_Transport				=	Create uo_transportista
iuo_Camion					=	Create uo_camiones
iuo_TipoDocto				=	Create uo_tipodoctoplanta
iuo_ClienProve				=	Create uo_clienprove
iuo_LotesFrutaComer		=	Create uo_lotesfrutacomer
iuo_clientes					=	Create uo_clientesprod
iuo_TipoMovtoFruta		=	Create uo_tipomovtofruta		
iuo_TipoMovtoEnva		=  Create uo_tipomovtofruta
iuo_calicosechero			=  Create uo_calicosechero
iuo_envases					=	Create uo_envases
iuo_Lotes					=	Create uo_spro_lotesfrutacomenc
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
	istr_Mant.dw		=	dw_3
	
	IF NOT gstr_paramplanta.binsabins THEN
		OpenWithParm(iw_mantencion_1, istr_mant)
	ELSE

		lstr_Mant.Argumento[1]	=	String(dw_2.Object.plde_codigo[1])
		lstr_Mant.Argumento[2]	=	istr_Mant.Argumento[9]
		lstr_Mant.Argumento[3]	=	istr_Mant.Argumento[10]
		lstr_Mant.Argumento[4]	=	'2'
		lstr_Mant.Argumento[9]	=	'7'
		lstr_Mant.Argumento[10]	=	String(dw_2.Object.mfco_guisii[1])
		lstr_Mant.Argumento[11]	=	'devolucion'
		lstr_Mant.Argumento[13]	=	String(dw_2.Object.mfco_fecmov[1], 'dd/mm/yyyy')

		OpenWithParm(w_mant_deta_movtofrutcomer_despachoventa, istr_mant)
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
	
	istr_Mant		=	Message.PowerObjectParm
	
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

habilitaencab( False)

IF tab_1.SelectedTab = 1 THEN
	istr_mant.dw	=	dw_3

	istr_mant.Argumento[7] 		= 	"0"  //Envases con Fruta 
	istr_mant.argumento[9]   	= 	String(dw_2.Object.mfco_tipdoc[1])	
	istr_mant.argumento[10]  	= 	String(dw_2.Object.mfco_docrel[1])
	istr_mant.argumento[13]  	= 	String(dw_2.Object.mfco_fecmov[1])
	istr_mant.Argumento[40] 	=	'O'

	OpenWithParm(iw_mantencion_1, istr_mant)

	IF dw_3.RowCount() > 0 THEN
		dw_2.Object.mfco_totbul[1]	=	Round(dw_3.Object.total_bultos[1], 0)
		dw_2.Object.mfco_tpneto[1]	=	Round(dw_3.Object.total_kilos[1], 3)
		pb_eli_det.Enabled			=	True
		Carga_Envases()
		
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

	ll_fila_e	=	dw_2.Retrieve(Integer(istr_Mant.Argumento[1]),Integer(istr_Mant.Argumento[2]), &
										  Long(istr_Mant.Argumento[3]),ii_cliente)
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila_e < 1 THEN 
			TriggerEvent("ue_nuevo")
			RETURN  
	ELSE
		IF NOT gstr_paramplanta.binsabins THEN
			li_dw3= dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), &
										  Long(istr_Mant.Argumento[3]), ii_cliente)
		ELSE
			li_dw3= dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), &
										  Long(istr_Mant.Argumento[3]),ii_cliente)
		END IF					
		DO
			IF li_dw3 = -1 OR &
				dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), li_TipoMovto, ll_Numero, ii_cliente) = -1 OR &
				dw_4.Retrieve(Integer(istr_Mant.Argumento[1]), li_TipoMovto, ll_Numero, ii_cliente) = -1 THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			ELSE
			Tab_1.tp_1.Enabled	=	True
				
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
				If dw_2.Object.mfco_guiemi[1] = 0 Or dw_2.Object.mfco_guiemi[1] = 3 Then
//						IF isnull(dw_2.Object.mfco_guisii[1]) OR dw_2.Object.mfco_guisii[1]=0 THEN
					cb_guisii.Enabled 	= TRUE
				ELSE
					cb_guisii.Enabled 	= FALSE
				END IF	
			ELSE
				istr_Mant.solo_consulta =	FALSE
			END IF	
				
				pb_eliminar.Enabled	=	True//NOT istr_Mant.solo_consulta
				pb_grabar.Enabled		=	True
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

on w_maed_movtofrutacomenca_multiconex.create
int iCurrent
call super::create
this.dw_6=create dw_6
this.dw_5=create dw_5
this.cb_guisii=create cb_guisii
this.dw_pesaje=create dw_pesaje
this.dw_movtobins=create dw_movtobins
this.dw_bins=create dw_bins
this.cb_capturadatos=create cb_capturadatos
this.dw_exideta=create dw_exideta
this.dw_exiencab=create dw_exiencab
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_6
this.Control[iCurrent+2]=this.dw_5
this.Control[iCurrent+3]=this.cb_guisii
this.Control[iCurrent+4]=this.dw_pesaje
this.Control[iCurrent+5]=this.dw_movtobins
this.Control[iCurrent+6]=this.dw_bins
this.Control[iCurrent+7]=this.cb_capturadatos
this.Control[iCurrent+8]=this.dw_exideta
this.Control[iCurrent+9]=this.dw_exiencab
this.Control[iCurrent+10]=this.tab_1
end on

on w_maed_movtofrutacomenca_multiconex.destroy
call super::destroy
destroy(this.dw_6)
destroy(this.dw_5)
destroy(this.cb_guisii)
destroy(this.dw_pesaje)
destroy(this.dw_movtobins)
destroy(this.dw_bins)
destroy(this.cb_capturadatos)
destroy(this.dw_exideta)
destroy(this.dw_exiencab)
destroy(this.tab_1)
end on

event ue_modifica_detalle;str_mant		lstr_mant

lstr_mant.agrega	=	False
lstr_mant.borra	=	False

IF tab_1.SelectedTab = 1 THEN
	IF dw_3.RowCount() > 0 THEN
		lstr_mant.dw		=	dw_3

		lstr_Mant.Argumento[7] 		= 	"0"  //Envases con Fruta 
		lstr_mant.argumento[2]   	=	String(dw_2.Object.mfco_tipdoc[1])	
		lstr_mant.argumento[9]   	=	String(dw_2.Object.mfco_tipdoc[1])	
		lstr_mant.argumento[10]  	=	String(dw_2.Object.mfco_docrel[1])
		istr_mant.Argumento[40] 	=	'O'

		IF NOT gstr_paramplanta.binsabins THEN
			OpenWithParm(iw_mantencion_1, lstr_mant)
		ELSE

			lstr_Mant.Argumento[1]	=	String(dw_2.Object.plde_codigo[1])
			lstr_Mant.Argumento[2]	=	istr_Mant.Argumento[9]
			lstr_Mant.Argumento[3]	=	istr_Mant.Argumento[10]
			lstr_Mant.Argumento[4]	=	'2'
			lstr_Mant.Argumento[9]	=	'7'
			lstr_Mant.Argumento[10]	=	String(dw_2.Object.mfco_guisii[1])
			lstr_Mant.Argumento[11]	=	'devolucion'
			lstr_Mant.Argumento[12]	=	'0'
			lstr_Mant.Argumento[13]	=	String(dw_2.Object.mfco_fecmov[1], 'dd/mm/yyyy')
	
			OpenWithParm(w_mant_deta_movtofrutcomer_despachoventa, lstr_mant)
		END IF
		
		dw_2.Object.mfco_totbul[1]	=	Round(dw_3.Object.total_bultos[1], 0)
		dw_2.Object.mfco_tpneto[1]	=	Round(dw_3.Object.total_kilos[1], 3)
		Carga_Envases()
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

event ue_nuevo;is_rut 		= ""
is_rutcliente	= ""

Call Super::ue_nuevo

dw_3.Reset()
dw_4.Reset()

iuo_clientes.existe(gi_codexport, false, sqlca)

dw_2.Object.plde_codigo[1]				=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.tpmv_codigo[1]				=	Integer(istr_Mant.Argumento[2])
dw_2.Object.mfco_fecmov[1]				=	Date(Mid(String(Today()),1,10))

dw_2.Object.mfco_guisii.Protect				=	1
dw_2.Object.mfco_guisii.Color					=	RGB(255,255,255)
dw_2.Object.mfco_guisii.BackGround.Color	=	553648127

dw_2.object.clie_codigo[1] 			= 	iuo_clientes.Codigo
istr_mant.Argumento[20]					=	'1'
ii_cliente									=	iuo_clientes.Codigo

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

istr_Mant.Argumento[6] 					= ""
dw_2.Object.mfco_estmov[1]				=	1
il_NumFruta									=	0
il_NumEnva									=	0
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_Fila, ll_Productor
Integer	li_Secuencia, li_Planta, li_TipoMovto, li_TipoMovtoEnva, li_control, li_estado, li_numeroenva
Boolean  lb_Actualiza_Envase = False, lb_Actualiza_Fruta = False, lb_retorno = True

ib_AutoCommit				=	sqlca.AutoCommit
sqlca.AutoCommit			=	False
li_Planta					=	dw_2.Object.plde_codigo[1]
istr_Mant.Argumento[1]	= 	String(li_Planta)
li_TipoMovto				=	dw_2.Object.tpmv_codigo[1]
istr_Mant.Argumento[2] 	= 	String(li_TipoMovto)

IF Integer(istr_Mant.Argumento[4]) = 1 THEN
	IF li_TipoMovto = 2 THEN
		li_TipoMovtoEnva 	=	42
	END IF
ELSE
	IF li_TipoMovto 		=	22 OR li_TipoMovto = 34 THEN
		li_TipoMovtoEnva 	=	62
		
	ELSEIF li_TipoMovto 	= 	23 THEN
		li_TipoMovtoEnva 	=	61
		
	ELSEIF li_TipoMovto 	= 	25 OR li_TipoMovto = 26 THEN
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

	dw_2.Object.mfco_numero[1]			=	il_NumFruta
	dw_2.Object.mfco_estmov[1]			=	3
	
	istr_Mant.Argumento[3] 				= 	String(il_NumFruta)
	
	IF li_TipoMovto = 25 THEN
		dw_2.Object.mfco_estmov[1]		= 	1
		
	END IF
	
	Determina_ProductoresEnvase(li_TipoMovtoEnva)

	//Resetea Movto de Envase en caso de errores previos en la grabación.
	dw_1.Reset()
	
	FOR ll_Productor = 1 TO UpperBound(wstr_Prod_Enva.Productor)
		
		ll_Fila									=	dw_1.InsertRow(0)
		
		dw_1.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_1.Object.tpmv_codigo[ll_Fila]	=	li_TipoMovtoEnva
		dw_1.Object.meen_numero[ll_Fila]	=  il_NumEnva
		dw_1.Object.clie_codigo[ll_Fila]	=	dw_2.Object.clie_codigo[1]
		dw_1.Object.plde_coorde[ll_Fila]	=	dw_2.Object.plde_coorde[1]
		dw_1.Object.prod_codigo[ll_Fila]	=	Long(wstr_Prod_Enva.Productor[ll_Productor])
		
		dw_1.Object.meen_modulo[ll_Fila]	=	2
		
		dw_1.Object.meen_guisii[ll_Fila]	=	wstr_Prod_Enva.GuiaSII[ll_Productor]
		dw_1.Object.meen_fecmov[ll_Fila]	=	dw_2.Object.mfco_fecmov[1]
		dw_1.Object.tran_codigo[ll_Fila]	=	dw_2.Object.tran_codigo[1]
		dw_1.Object.cami_clasifi[ll_Fila]=	dw_2.Object.cami_clasifi[1]
		dw_1.Object.cami_patent[ll_Fila]	=	dw_2.Object.cami_patent[1]
		dw_1.Object.cami_patcar[ll_Fila]	=	dw_2.Object.cami_patcar[1]
		dw_1.Object.meen_rutcho[ll_Fila]	=	dw_2.Object.mfco_rutcho[1]
		dw_1.Object.meen_chofer[ll_Fila]	=	dw_2.Object.mfco_chofer[1]
		
		dw_1.Object.tpmv_codrec[ll_fila] =  li_TipoMovto
		dw_1.Object.mfge_numero[ll_fila] =  il_NumFruta
		
		il_NumEnva ++
	NEXT
	
	il_NumEnva --
	
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
	FROM	dbo.spro_movtofrutacomdeta
	WHERE	plde_codigo	=	:li_Planta
	AND	tpmv_codigo	=	:li_TipoMovto
	AND	mfco_numero	=	:il_NumFruta ;

FOR ll_Fila = 1 TO dw_3.RowCount()
	IF dw_3.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_3.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_3.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
		dw_3.Object.mfco_numero[ll_Fila]	=	dw_2.Object.mfco_numero[1]
		dw_3.Object.mfcd_secuen[ll_Fila]	=	li_Secuencia

		li_Secuencia ++
	END IF
NEXT

FOR ll_Fila = 1 TO dw_4.RowCount()
	IF dw_4.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_4.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_4.Object.tpmv_codigo[ll_Fila]	=	li_TipoMovtoEnva
		dw_4.Object.meen_numero[ll_Fila]	=	dw_1.Object.meen_numero[dw_1.Find("prod_codigo = " + String(dw_4.Object.prod_codigo[ll_Fila]), 1, dw_1.RowCount())]
		
	END IF
NEXT

FOR ll_Fila = 1 TO dw_5.RowCount()
	IF dw_5.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		If Not iuo_Lotes.Existe(dw_5.Object.lofc_pltcod[ll_Fila], dw_5.Object.lofc_espcod[ll_Fila], dw_5.Object.lofc_lotefc[ll_Fila], False, Sqlca) Then
//			dw_5.Object.lofc_pltcod[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		Else
			If istr_Mant.Argumento[4] = '1' Then 
				dw_5.Deleterow(ll_Fila)
			Else
//				dw_5.Object.lofc_pltcod[ll_Fila]	=	dw_2.Object.plde_codigo[1]
			End if
		End If
	END IF
NEXT

FOR ll_Fila = 1 TO dw_6.RowCount()
	IF dw_6.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
//		dw_6.Object.lofc_pltcod[ll_Fila]	=	dw_2.Object.plde_codigo[1]
	END IF
NEXT

IF li_tipoMovto<>25 THEN
	IF dw_2.Object.mfco_guisii[1] = 0 OR isnull(dw_2.Object.mfco_guisii[1]) THEN
		cb_guisii.Enabled 	= TRUE
		cb_guisii.visible 	= TRUE
	END IF
END IF

IF is_movto = '102' THEN
	FOR ll_fila = 1 TO dw_bins.RowCount()
		IF isnull(dw_bins.Object.clie_codigo[ll_fila]) THEN 
			dw_bins.Reset()
			ll_fila = dw_bins.RowCount()
		END IF	
	NEXT	
END IF	

agregaclientea_dws()

/***************************************************/
IF integer(istr_mant.Argumento[2]) = 22 THEN Return
/***************************************************/

////////////////////////////////////////////////////////////////////////////
//Controles para evitar errores al insertar datos ya existentes.//
////////////////////////////////////////////////////////////////////////////
//Se opta por actualizar los datos que existian con anterioridad
//para que sean congruentes con los datos nuevos que vengan en la recepción.
////////////////////////////////////////////////////////////////////////////
FOR ll_fila = 1 TO dw_1.RowCount()
	dw_1.Object.tpmv_codigo[ll_fila] = li_TipoMovtoEnva
//	dw_1.Object.meen_numero[ll_fila] = il_NumEnva
	dw_1.Object.plde_codigo[ll_fila] = li_Planta
	
	li_control = ExisteEnvaEncab(ll_fila)
	CHOOSE CASE li_control
		CASE 0
			li_estado = dw_1.SetItemStatus(ll_fila, 0, Primary!, NewModified!)
		CASE -1
			li_estado = -1
		CASE ELSE
			ll_fila = li_control
	END CHOOSE
			
	IF li_estado = -1 THEN lb_retorno = False
NEXT

FOR ll_fila = 1 TO dw_2.RowCount()
	dw_2.Object.plde_codigo[ll_fila] = li_Planta
	
	li_control = ExisteMovtoEncab(ll_fila)
	CHOOSE CASE li_control
		CASE 0
			li_estado = dw_2.SetItemStatus(ll_fila, 0, Primary!, NewModified!)
		CASE -1
			li_estado = -1
		CASE ELSE
			ll_fila = li_control
	END CHOOSE
	
	IF li_estado = -1 THEN lb_retorno = False
NEXT

FOR ll_fila = 1 TO dw_3.RowCount()
	dw_3.Object.plde_codigo[ll_fila] = li_Planta
//	dw_3.Object.lofc_pltcod[ll_fila] = li_Planta
	
	li_control = ExisteMovtoDeta(ll_fila)
	CHOOSE CASE li_control
		CASE 0
			li_estado = dw_3.SetItemStatus(ll_fila, 0, Primary!, NewModified!)
		CASE -1
			li_estado = -1
		CASE ELSE
			ll_fila = li_control
	END CHOOSE
	
	IF li_estado = -1 THEN lb_retorno = False
NEXT

FOR ll_fila = 1 TO dw_4.RowCount()
	dw_4.Object.tpmv_codigo[ll_fila] = li_TipoMovtoEnva
	dw_4.Object.plde_codigo[ll_fila] = li_Planta
	
	SetNull(li_numeroenva)
	
	FOR li_control = 1 TO dw_1.RowCount()
		IF dw_1.Object.Prod_codigo[li_control] = dw_4.Object.Prod_codigo[ll_fila] THEN
			li_numeroenva	=	dw_1.Object.meen_numero[li_control]
		END IF
	NEXT
	
	IF IsNull(li_numeroenva) THEN li_numeroenva = il_NumEnva
	dw_4.Object.meen_numero[ll_fila] =	li_numeroenva
	li_control 								=	ExisteEnvaDeta(ll_fila)
	CHOOSE CASE li_control
		CASE 0
			li_estado = dw_4.SetItemStatus(ll_fila, 0, Primary!, NewModified!)
		CASE -1
			li_estado = -1
		CASE ELSE
			ll_fila = li_control
	END CHOOSE
	
	IF li_estado = -1 THEN lb_retorno = False
NEXT

FOR ll_fila = 1 TO dw_5.RowCount()
//	dw_5.Object.lofc_pltcod[ll_fila] = li_Planta
	
	li_control = ExisteLoteEnca(ll_fila)
	CHOOSE CASE li_control
		CASE 0
			li_estado = dw_5.SetItemStatus(ll_fila, 0, Primary!, NewModified!)
		CASE -1
			li_estado = -1
		CASE ELSE
			ll_fila = li_control
	END CHOOSE
	
	IF li_estado = -1 THEN lb_retorno = False
NEXT

FOR ll_fila = 1 TO dw_6.RowCount()
	dw_6.Object.lfcd_tipdoc[ll_fila] = dw_2.Object.tpmv_codigo[1]
	dw_6.Object.lfcd_docrel[ll_fila] = dw_2.Object.mfco_numero[1]
//	dw_6.Object.lofc_pltcod[ll_fila] = li_Planta
	
	li_control = ExisteLoteDeta(ll_fila)
	CHOOSE CASE li_control
		CASE 0
			li_estado = dw_6.SetItemStatus(ll_fila, 0, Primary!, NewModified!)
		CASE -1
			li_estado = -1
		CASE ELSE
			ll_fila = li_control
	END CHOOSE
	
	IF li_estado = -1 THEN lb_retorno = False
NEXT

FOR ll_fila = 1 TO dw_movtobins.RowCount()
	dw_movtobins.Object.mfge_numero[ll_fila] = il_NumFruta
	dw_movtobins.Object.plde_codigo[ll_fila] = li_Planta

	li_control = ExisteMovtoBins(ll_fila)
	CHOOSE CASE li_control
		CASE 0
			li_estado = dw_movtobins.SetItemStatus(ll_fila, 0, Primary!, NewModified!)
		CASE -1
			li_estado = -1
		CASE ELSE
			ll_fila = li_control
	END CHOOSE
	
	IF li_estado = -1 THEN lb_retorno = False
NEXT

FOR ll_fila = 1 TO dw_bins.RowCount()
	dw_bins.Object.plde_codigo[ll_fila] = li_Planta
	
	li_control = ExisteBins(ll_fila)
	CHOOSE CASE li_control
		CASE 0
			li_estado = dw_bins.SetItemStatus(ll_fila, 0, Primary!, NewModified!)
		CASE -1
			li_estado = -1
		CASE ELSE
			ll_fila = li_control
	END CHOOSE
	
	IF li_estado = -1 THEN lb_retorno = False
NEXT

IF NOT lb_retorno THEN Message.DoubleParm = -1
end event

event ue_borrar;If dw_2.RowCount() < 1 Then Return

SetPointer(HourGlass!)

ib_borrar			=	True
ib_AutoCommit	=	sqlca.AutoCommit

w_main.SetMicroHelp("Validando la eliminación...")

If Not Valida_Password() Then
	MessageBox("Error", "No es posible anular el movimiento, ya que no posee el password correspondiente")
	Return
End If

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

If Message.DoubleParm = -1 Then Return

If dw_3.RowCount() > 0 Then dw_3.RowsMove(1, dw_3.RowCount(), Primary!, dw_3, 1, Delete!)
If dw_4.RowCount() > 0 Then dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
If dw_1.RowCount() > 0 Then dw_1.RowsMove(1, dw_1.RowCount(), Primary!, dw_1, 1, Delete!)

//If dw_1.DeleteRow(0) = 1 Then
ib_Borrar	=	False
w_main.SetMicroHelp("Anulando Registro...")

dw_2.Object.mfco_estmov[1]	=	0

If wf_actualiza_db(True) Then
	w_main.SetMicroHelp("Registro Borrado...")			
	This.TriggerEvent("ue_nuevo")
	SetPointer(Arrow!)
Else
	w_main.SetMicroHelp("Registro no Borrado...")
End If			
//Else
//	ib_Borrar	=	False
//	MessageBox(This.Title,"No se puede borrar actual registro.")
//End If
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
	ii_cliente						=	Integer(lstr_Busq.Argum[12])
	dw_2.Object.clie_codigo[1]	=	ii_cliente
	
	This.TriggerEvent("ue_recuperadatos")
END IF
end event

event ue_guardar;Integer	li_Cliente, li_planta
String	ls_numero
Date		ld_fecha

IF il_NumFruta>0 AND dw_2.GetItemStatus(1, 0, Primary!) = NotModified! THEN
	il_NumFruta = 0
	il_NumEnva	= 0
END IF

ls_numero = String(dw_2.Object.mfco_numero[1])
ld_fecha = dw_2.Object.mfco_fecmov[1]

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	IF dw_2.Object.mfco_retenv[1] = 1 THEN
		IF is_movto = '222' THEN
			IF gi_admenvase <> 1 THEN
				li_Cliente = dw_2.Object.clie_codigo[1]
				iuo_grabatablabitacora			=	Create uo_grabatablabitacora
				Datos_correo()
				
				SELECT clie_conexi, cone_codigo
				INTO :il_conexiste, :il_coneccion
				FROM dbo.clientesprod
				WHERE clie_codigo = :li_Cliente;
				
				IF il_conexiste = 1 THEN
					sqlexi	=	CREATE Transaction
					
					IF Conexionexistencia() THEN
						dw_exideta.SetTransObject(sqlexi)
						dw_exiencab.SetTransObject(sqlexi)	
						
						IF is_movto = '222' THEN
							TriggerEvent("ue_despuesguardar")
							IF li_retorno = 2 THEN
								iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
									'Verificar Datos en Base de Existencia '+is_base,is_base,1,is_error,1,il_coderror,&
								is_correo,'Problema Control de Envases Despacho Interplanta Comercial','Movimiento N° '+ls_numero+' Con problemas, Error '+is_error)
								is_error = ''	
							END IF		
						END IF
						
						Disconnect Using sqlexi;
					ELSE
						MessageBox("Atención", "No puede Despachar.~r~r" + &
									"Falló Conexion con Existencia.", Exclamation!, Ok!)
						
						iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
						'Verificar Conexión a Base '+is_base,is_base,1,'No Se Pudo Conectar el Sistema a Base de Datos de Existencia',1,il_coderror,&
						is_correo,'Problema Control de Envases Despacho Interplanta Comercial','Movimiento N° '+ls_numero+' Con problemas, Error en Conexión con Base de Existencia')
						MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)
						
						RETURN 
					END IF
				END IF	
			END IF	
		END IF	
	END IF	
	
	w_main.SetMicroHelp("Información Grabada.")
	IF integer(istr_mant.Argumento[2]) = 22 THEN
		pb_eliminar.Enabled			= 	True
		pb_imprimir.Enabled			= 	True
		
		iuo_traspaso					=	Create nvuo_traspaso_interplanta_comercial
		
		iuo_traspaso.ii_planta 		= 	dw_2.Object.plde_codigo[1]
		iuo_traspaso.ii_tipo			=	dw_2.Object.tpmv_codigo[1]
		iuo_traspaso.ii_movto		=	dw_2.Object.mfco_numero[1]
		iuo_traspaso.ii_cliente		=	dw_2.Object.clie_codigo[1]
		iuo_traspaso.ii_plantades 	=	dw_2.Object.plde_coorde[1]	
		iuo_traspaso.ii_sentido 	= 	1//Despacho
	
		iuo_traspaso.Triggerevent("traspasadatos")
		
		Destroy iuo_traspaso;
		GarbageCollect()
	ELSE
	//	li_planta = dw_2.Object.plde_codigo[1]
		
		iuo_traspaso.lb_LocalRemoto = False
		iuo_traspaso.TriggerEvent("ue_borrar")
	END IF
	
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm 			= 	-1
	RETURN
END IF
end event

event resize;call super::resize;Tab_1.x			=	dw_1.x
Tab_1.y			=	dw_1.y
Tab_1.Width	=	dw_1.Width
Tab_1.Height	=	dw_1.Height

Tab_1.Tp_1.dw_detalle.Width		= dw_1.Width - 120
Tab_1.Tp_1.dw_detalle.Height		= dw_1.Height - 200

Tab_1.Tp_2.dw_envases.Width	= dw_1.Width - 120
Tab_1.Tp_2.dw_envases.Height	= dw_1.Height - 200


cb_capturadatos.x	=	pb_Salir.x
cb_capturadatos.y	=	pb_Salir.y + 255

cb_guisii.x				=	cb_capturadatos.x

If Not cb_capturadatos.Visible Then
	cb_guisii.y				=	cb_capturadatos.y
Else
	cb_guisii.y				=	cb_capturadatos.y + cb_capturadatos.Height + 10
End If

pb_eli_det.y			=	dw_1.y + dw_1.Height - 255
pb_ins_det.y			=	pb_eli_det.y - 255

end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutacomenca_multiconex
boolean visible = false
integer x = 55
integer y = 892
integer width = 4658
integer height = 1200
boolean titlebar = false
string title = "Enca enva"
string dataobject = "dw_mant_movtoenvaenca_origen"
boolean hscrollbar = false
boolean vscrollbar = false
boolean livescroll = false
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutacomenca_multiconex
integer x = 37
integer y = 32
integer width = 3054
integer height = 952
integer taborder = 10
string dataobject = "dw_mant_movtofrutacomenca_movtos_origen"
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
			ii_cliente	=	Integer(data)
//			This.SetItem(1,"clpr_rut",iuo_clientes.clie_nrorut)
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
			istr_Mant.Argumento[6]	= Data
		END IF
		
	CASE "tran_codigo"
		IF Not iuo_Transport.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, ls_Nula)
			
			RETURN 1
		END IF

	CASE "cami_patent"
		
		IF Integer(istr_Mant.Argumento[2]) = 2 OR Integer(istr_Mant.Argumento[2]) = 22 THEN
			li_ClasifCamion					=	2
		ELSE
			li_ClasifCamion					=	1
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
			is_rut  								= 	iuo_Camion.RutChofer
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
		
	Case 'mfco_trasva'
		If Data = '0' Then This.SetItem(Row, 'mfco_cantra', Integer(ls_Nula))	
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


HabilitaIngreso(dwo.Name)
end event

event dw_2::clicked;CHOOSE CASE dwo.Name
	CASE "b_camion"
		BuscaCamion()
		HabilitaIngreso('cami_patent')
		
	CASE "b_docrel"
		Buscaorden()

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutacomenca_multiconex
integer x = 4736
integer y = 252
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(TRUE)
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutacomenca_multiconex
integer x = 4736
integer y = 428
integer taborder = 70
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutacomenca_multiconex
integer x = 4736
integer y = 612
integer taborder = 80
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutacomenca_multiconex
integer x = 4736
integer y = 792
integer taborder = 90
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutacomenca_multiconex
integer x = 4736
integer y = 1096
integer taborder = 100
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutacomenca_multiconex
integer x = 4736
integer y = 1484
integer taborder = 30
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutacomenca_multiconex
integer x = 4736
integer y = 1660
integer taborder = 40
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutacomenca_multiconex
integer x = 4736
integer y = 68
integer taborder = 50
end type

type dw_6 from datawindow within w_maed_movtofrutacomenca_multiconex
boolean visible = false
integer x = 3429
integer y = 452
integer width = 174
integer height = 140
integer taborder = 70
boolean bringtotop = true
string title = "Deta Lote"
string dataobject = "dw_gene_spro_lotesfrutacomdeta_origen"
end type

type dw_5 from datawindow within w_maed_movtofrutacomenca_multiconex
boolean visible = false
integer x = 3218
integer y = 456
integer width = 174
integer height = 140
integer taborder = 10
boolean bringtotop = true
string title = "Enca Lote"
string dataobject = "dw_gene_spro_lotesfrutacomenc_origen"
end type

type cb_guisii from commandbutton within w_maed_movtofrutacomenca_multiconex
integer x = 4736
integer y = 1980
integer width = 302
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

lstr_Mant.Argumento[1]	=	String(dw_2.Object.plde_codigo[1])
lstr_Mant.Argumento[2]	=	String(dw_2.Object.tpmv_codigo[1])
lstr_Mant.Argumento[3]	=	String(dw_2.Object.mfco_numero[1])
lstr_Mant.Argumento[5]	=  String(dw_2.Object.clie_codigo[1])
lstr_Mant.Argumento[6]	=  '3'

OpenWithParm(w_emis_guia_despacho_venta, lstr_Mant)

lstr_Mant = Message.PowerObjectParm

IF IsNull(lstr_Mant) THEN RETURN

IF lstr_Mant.Respuesta = 1 THEN
	istr_mant.solo_consulta = TRUE
	Parent.TriggerEvent("ue_recuperadatos")
END IF
end event

type dw_pesaje from datawindow within w_maed_movtofrutacomenca_multiconex
boolean visible = false
integer x = 3424
integer y = 260
integer width = 174
integer height = 140
integer taborder = 80
boolean bringtotop = true
string title = "Gran Pesa"
string dataobject = "dw_pesaje_romana_origen"
end type

type dw_movtobins from datawindow within w_maed_movtofrutacomenca_multiconex
boolean visible = false
integer x = 3671
integer y = 456
integer width = 174
integer height = 140
integer taborder = 90
boolean bringtotop = true
string title = "Mvto Bins"
string dataobject = "dw_spro_movtobins_origen"
end type

type dw_bins from datawindow within w_maed_movtofrutacomenca_multiconex
boolean visible = false
integer x = 3657
integer y = 264
integer width = 174
integer height = 140
integer taborder = 100
boolean bringtotop = true
string title = "Bins"
string dataobject = "dw_mues_spro_bins_origen"
end type

type cb_capturadatos from commandbutton within w_maed_movtofrutacomenca_multiconex
boolean visible = false
integer x = 4736
integer y = 1872
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
boolean enabled = false
string text = "Captura"
end type

event clicked;String	ls_directorio
Integer	li_valida, li_opcion = 1
dwitemstatus stat

ib_ok	= True

IF IsValid(iuo_traspaso) THEN
	Destroy iuo_traspaso;
END IF

iuo_traspaso				=	Create nvuo_traspaso_interplanta_comercial
iuo_traspaso.ii_sentido	=	2
iuo_traspaso.w_parent	=	Parent
iuo_traspaso.ii_planta 	= 	dw_2.Object.plde_codigo[1]
iuo_traspaso.ii_tipo		=	22
iuo_traspaso.ii_cliente	=	dw_2.Object.clie_codigo[1]

pb_nuevo.TriggerEvent(Clicked!)

iuo_traspaso.ii_ManAut 	= 	Messagebox("Modo de Carga", "Selección de metodo de carga. ~n~r"+&
																	  "Presione [SI] para Archivos Planos, "+&
																	  "[NO] para Tablas Temporales", Question!, YesNo!, 2)
iuo_traspaso.Triggerevent("traspasadatos")

tab_1.Enabled			=	True
tab_1.tp_1.Enabled	=	True
tab_1.tp_2.Enabled	=	True


end event

type dw_exideta from datawindow within w_maed_movtofrutacomenca_multiconex
boolean visible = false
integer x = 3895
integer y = 256
integer width = 174
integer height = 152
integer taborder = 80
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta"
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

type dw_exiencab from datawindow within w_maed_movtofrutacomenca_multiconex
boolean visible = false
integer x = 3218
integer y = 260
integer width = 174
integer height = 140
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_exismovtoenca_analisis"
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

type tab_1 from tab within w_maed_movtofrutacomenca_multiconex
event create ( )
event destroy ( )
integer x = 37
integer y = 1008
integer width = 4613
integer height = 1128
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
integer width = 4576
integer height = 1000
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
integer x = 37
integer y = 36
integer width = 4507
integer height = 1012
integer taborder = 11
string dataobject = "dw_mues_movtofrutacomdeta_ventas_origen"
boolean hscrollbar = true
boolean border = false
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
	w_maed_movtofrutacomenca_multiconex.TriggerEvent("ue_modifica_detalle")
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
integer width = 4576
integer height = 1000
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
integer width = 4096
integer height = 1012
integer taborder = 10
string dataobject = "dw_mues_movtoenvadeta_origen"
boolean hscrollbar = true
boolean border = false
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
	w_maed_movtofrutacomenca_multiconex.TriggerEvent("ue_modifica_detalle")
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

