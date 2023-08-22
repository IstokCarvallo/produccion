$PBExportHeader$w_maed_movtofrutacomercial.srw
$PBExportComments$Recepción de Fruta Granel de Huerto
forward
global type w_maed_movtofrutacomercial from w_mant_encab_deta_csd
end type
type cb_guia from commandbutton within w_maed_movtofrutacomercial
end type
type tab_1 from tab within w_maed_movtofrutacomercial
end type
type tp_1 from userobject within tab_1
end type
type dw_detafruta from uo_dw within tp_1
end type
type tp_1 from userobject within tab_1
dw_detafruta dw_detafruta
end type
type tp_2 from userobject within tab_1
end type
type dw_envrec from uo_dw within tp_2
end type
type tp_2 from userobject within tab_1
dw_envrec dw_envrec
end type
type tp_3 from userobject within tab_1
end type
type dw_envdes from uo_dw within tp_3
end type
type tp_3 from userobject within tab_1
dw_envdes dw_envdes
end type
type tab_1 from tab within w_maed_movtofrutacomercial
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
end type
type dw_exideta from datawindow within w_maed_movtofrutacomercial
end type
type dw_exiencab from datawindow within w_maed_movtofrutacomercial
end type
type dw_exidetaborra from datawindow within w_maed_movtofrutacomercial
end type
type dw_exismovtodetanulos from datawindow within w_maed_movtofrutacomercial
end type
type str_productores_envases from structure within w_maed_movtofrutacomercial
end type
end forward

type str_productores_envases from structure
	integer		productor[]
	long		guiasii[]
	integer		tipomovto[]
	integer		Numero[]
end type

global type w_maed_movtofrutacomercial from w_mant_encab_deta_csd
integer width = 5120
integer height = 3304
string title = "DESPACHO RETIRO VENTAS"
string menuname = ""
windowstate windowstate = maximized!
event ue_validapassword ( )
event type long ue_despuesguardar ( )
event ue_despuesborrar ( )
cb_guia cb_guia
tab_1 tab_1
dw_exideta dw_exideta
dw_exiencab dw_exiencab
dw_exidetaborra dw_exidetaborra
dw_exismovtodetanulos dw_exismovtodetanulos
end type
global w_maed_movtofrutacomercial w_maed_movtofrutacomercial

type variables
DataWindowChild		idwc_Transp,idwc_Camion,idwc_Variedad,idwc_Predio,idwc_Camara, idwc_cliente
DataWindow			dw_3,dw_5,dw_7

uo_transportista			iuo_Transport
uo_camiones				iuo_Camion
uo_especie					iuo_Especie
uo_Productores				iuo_Productor
uo_pesoestanespe			iuo_PesoEstanEspe
uo_fechaMovto				iuo_FechaMovto

uo_tipomovtofruta			iuo_TipoMovtoFruta
uo_tipomovtofruta			iuo_TipoMovtoEnva
uo_plantadesp				iuo_PltaDestino	
uo_calicosechero  			iuo_calicosechero
uo_clientesprod			iuo_cliente

uo_clienprove				iuo_clprv
uo_bins						iuo_bins
uo_grabatablabitacora	iuo_grabatablabitacora

Long     						il_NumFruta=0, il_NumEnva=0,il_Productor, li_retorno, il_coderror
Boolean						ib_AutoCommit, ib_Salida, ib_Destare, ib_Frigo=False, ib_cierraventa = FALSE, ib_ConectadoExistencia
String							is_rut, is_rutprod, is_RutProductor, is_NombreProductor, is_rutclie, is_error, is_correo
Integer						ii_Cantidad, il_coneccion, il_conexiste 
DateTime					idt_FechaSistema

w_mant_deta_movtofrutcomer_despachoventa		iw_mantencion_1, iw_mantencion_3
w_mant_deta_movtoenvadeta_despa_comer		iw_mantencion_2

Private:
str_Productores_Envases		wstr_Prod_Enva

Transaction						sqlexi
end variables

forward prototypes
public subroutine productoreslotes (ref string productores[])
protected function integer wf_modifica ()
public subroutine determina_productoresenvase (integer ai_tipomovto)
public subroutine habilitaencab (boolean habilita)
public subroutine habilitafrigo ()
public subroutine habilitafrigorifico ()
public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, integer al_numero)
public function boolean buscagarantia (long al_orden, integer ai_tipo)
public subroutine datosorden ()
public subroutine captura_totales ()
public subroutine buscaorden ()
public subroutine buscacamion ()
public function boolean existeorden (long al_orden, integer ai_tipo)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine habilitagrabacion (string as_columna)
public subroutine destare (boolean ab_actualiza)
public subroutine habilitaingreso (string as_columna)
public subroutine habilitasalida ()
public function boolean actualizaorden ()
public subroutine captura_entrada ()
public function boolean valida_password ()
public subroutine buscacliente ()
public function boolean conexionexistencia ()
public function boolean datos_correo ()
end prototypes

event ue_validapassword();Boolean 	lb_retorno = TRUE
str_mant	lstr_mant

lstr_mant.Argumento[1]	=	"Comercial"
lstr_mant.Argumento[2]	=	gstr_paramplanta.Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

event type long ue_despuesguardar();integer	li_bodega, li_fila, li_cliente, li_planta,li_enva_codigo, li_enva_tipoen,&
			li_bodzonal, li_lote_pltcod, li_lote_espcod, li_lote_codigo, li_bodedestino,li_secuenciarece, &
			li_filarecep, li_DevZon
Long 		ll_fila, ll_numero, li_secuencia = 1, ll_fila_nueva, ll_fila_nea, ll_count, &
			ll_docrel, ll_numnuevoini, ll_numnuevofin, ll_prod_codigo,ll_numerorecep, ll_filarecep, &
			ll_lfcd_secuen, ll_general, ll_bins
String	ls_item_codigo, ls_productor, ls_prod, ls_calidad, ls_productorant
Boolean	lb_AutoCommit, lb_Retorno

IF ib_Conectadoexistencia THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
//	dw_3.Retrieve(Integer(istr_mant.argumento[1]),&
//							     Integer(istr_mant.argumento[2]),&
//							     Long(istr_mant.argumento[3]),&
//								  Integer(istr_Mant.Argumento[30]))
//								  
//	dw_3.SetSort("prod_codigo asc")							  
//	dw_3.Sort()								  
	
	ll_docrel = dw_2.Object.mfco_numero[1]
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
		li_retorno = 2	
		is_error = 'Problema con los Parametros de Empresa en Existencia'
		RETURN 2
	ELSE
		li_bodedestino = 	luo_existencia.BodDestino
		li_bodzonal		= 	luo_existencia.bodzonal
		li_DevZon		=	luo_existencia.DevZon
	END IF	
	
	IF luo_existencia.Mesproceso > dw_2.Object.mfco_fecmov[1] THEN
		Message.DoubleParm = -1
		li_retorno = 2	
		is_error = 'Mes de proceso Mayor a Fecha de Movimiento'		
		Return 2
	END IF
	
	luo_existencia.existeencabezado(ll_docrel,li_bodzonal,1,3,True,sqlexi)
	
	IF luo_existencia.count <> 0 THEN
		li_retorno = 2	
			is_error = 'Problema Enzabezado ya Existe'
		Return 2
	END IF
		
	//FOR ll_general = 1 TO dw_3.RowCount()
				
		IF Not luo_existencia.correlativobode(3,li_bodzonal,li_bodzonal,True,sqlexi) THEN
			Message.DoubleParm = -1
			li_retorno = 2	
			RETURN 2 
		ELSE
			ll_numero = luo_existencia.numero
		END IF	
			
		IF isnull(ll_numero) THEN
			ll_numnuevoini = Long(String(li_bodzonal)+''+'0001')
			ll_numnuevofin = Long(String(li_bodzonal)+''+'9999')
			
			INSERT INTO dba.correlbode(bode_codigo,mden_tipdoc,cobo_bodadm,cobo_inicia,cobo_final,cobo_actual)
			VALUES(:li_bodzonal,3,:li_bodzonal,:ll_numnuevoini,:ll_numnuevofin,:ll_numnuevoini)
			USING sqlexi;
			COMMIT;
			
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
		
//		li_lote_pltcod = dw_3.Object.lofc_pltcod[ll_general]
//		li_lote_espcod = dw_3.Object.lofc_espcod[ll_general]
//		li_lote_codigo = dw_3.Object.lofc_lotefc[ll_general]
//		ll_lfcd_secuen	= dw_3.Object.lfcd_secuen[ll_general]
//			
//		SELECT prod_codigo, bins_numero
//		INTO :ll_prod_codigo, :ll_bins
//		FROM dba.spro_lotesfrutacomdeta
//		WHERE lofc_pltcod = :li_lote_pltcod
//		AND	lofc_espcod = :li_lote_espcod
//		AND	lofc_lotefc = :li_lote_codigo
//		AND	lfcd_secuen = :ll_lfcd_secuen;
		
		ls_productor = dw_2.Object.clpr_rut[1]//String(dw_2.Object.clpr_rut[1],'000000')
		
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
		
		IF ls_productorant <> ls_productor THEN
			dw_exiencab.Object.mden_tipdoc[ll_fila_nea] 	= 	3
			dw_exiencab.Object.mden_numero[ll_fila_nea] 	= 	ll_numero 
			dw_exiencab.Object.tpdo_codigo[ll_fila_nea] 	= 	2
			dw_exiencab.Object.mden_fecmov[ll_fila_nea] 	= 	dw_2.Object.mfco_fecmov[1]
			dw_exiencab.Object.tpmv_tipomv[ll_fila_nea] 	= 	2
						
//			IF isnull(dw_2.Object.mfco_retenv[1]) OR  dw_2.Object.mfco_retenv[1] = 0 THEN
//				dw_exiencab.Object.tpmv_codigo[ll_fila_nea] 	= 	2
//				dw_exiencab.Object.mden_bodest[ll_fila_nea] 	= 	li_bodzonal
//			ELSE
				dw_exiencab.Object.tpmv_codigo[ll_fila_nea] 	= 	8
				dw_exiencab.Object.mden_bodest[ll_fila_nea] 	= 	li_bodzonal
				dw_exiencab.Object.clpr_rut[ll_fila_nea]	 	= 	ls_productor
		//	END IF
			
			dw_exiencab.Object.mden_tipana[ll_fila_nea] 	= 	4
			dw_exiencab.Object.bode_codigo[ll_fila_nea] 	= 	li_bodzonal
			dw_exiencab.Object.mden_docrel[ll_fila_nea] 	= 	dw_2.Object.mfco_numero[1]
			dw_exiencab.Object.mden_fecdre[ll_fila_nea] 	= 	dw_2.Object.mfco_fecmov[1]
			dw_exiencab.Object.mden_observ[ll_fila_nea] 	= 	'Traspaso Cta. Cte. Envases Venta Directa'
			dw_exiencab.Object.mden_estado[ll_fila_nea] 	= 	1
			dw_exiencab.Object.mden_pcopda[ll_fila_nea] 	=	1
			dw_exiencab.Object.mden_estaci[ll_fila_nea] 	= 	gstr_us.computador
			dw_exiencab.Object.mden_fecdig[ll_fila_nea] 	= 	Date(Today())
			dw_exiencab.Object.mden_hordig[ll_fila_nea] 	= 	Time(Now())
		ELSE
			ll_numero = ll_numero - 1
		END IF 
			
		FOR li_fila = 1 TO dw_7.RowCount()
			ll_fila				=	dw_exideta.InsertRow(0)
			
			li_enva_codigo 	= 	dw_7.Object.enva_codigo[li_fila]
			li_enva_tipoen 	= 	dw_7.Object.enva_tipoen[li_fila]
			ls_calidad			= 	dw_7.Object.cale_calida[li_fila]
//			iuo_bins.existe(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],ll_bins,sqlexi,True)
//			
//			ls_calidad			=  iuo_bins.cale_calida
			
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
		
			dw_exideta.Object.mden_tipdoc[ll_fila] 	= 	3
			dw_exideta.Object.mden_numero[ll_fila] 	= 	ll_numero
			dw_exideta.Object.mdde_secuen[ll_fila] 	= 	li_secuencia 
			dw_exideta.Object.tpmv_tipomv[ll_fila] 	= 	2
			
//			IF isnull(dw_2.Object.mfco_retenv[1]) OR  dw_2.Object.mfco_retenv[1] = 0 THEN
//				dw_exideta.Object.tpmv_codigo[ll_fila] 	= 	2
//			ELSE
				dw_exideta.Object.tpmv_codigo[ll_fila] 	= 	8
			//END IF
			
			dw_exideta.Object.item_codigo[ll_fila] 	= 	ls_item_codigo
			dw_exideta.Object.mdde_identi[ll_fila] 	= 	''
			dw_exideta.Object.mdde_fecmov[ll_fila] 	= 	dw_2.Object.mfco_fecmov[1]
			dw_exideta.Object.bode_codigo[ll_fila] 	= 	li_bodzonal
			dw_exideta.Object.mdde_cantid[ll_fila] 	= 	dw_7.Object.fgme_cantid[li_fila]
						
			li_secuencia = li_secuencia + 1
		NEXT	
	
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
			
		sqlexi.AutoCommit		=	lb_AutoCommit
		dw_exideta.Reset()
		dw_exiencab.Reset()
		ib_Conectadoexistencia = False
		DISCONNECT USING sqlexi;
	
	END IF	
	sqlexi.AutoCommit		=	lb_AutoCommit
	dw_exideta.Reset()
	dw_exiencab.Reset()
	ib_Conectadoexistencia = False
	DISCONNECT USING sqlexi;
END IF	


end event

event ue_despuesborrar();Long 		ll_numero, respuesta, ll_nueva, ll_fila, ll_docrel
Boolean lb_AutoCommit
Integer	li_bodecomercial, li_bodzonal, li_devcorreo
String	ls_correozonal, ls_correo, ls_texto, ls_asunto, ls_error, sErrorMsg

IF ib_Conectadoexistencia = TRUE THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
		sqlexi.AutoCommit	=	ib_AutoCommit
		RETURN 
	ELSE
		li_bodecomercial	= luo_existencia.BodDestino
		li_bodzonal			= luo_existencia.bodzonal
	END IF	
	
	ll_docrel = dw_2.Object.mfco_numero[1]
	
	IF luo_existencia.Mesproceso > dw_2.Object.mfco_fecmov[1] THEN
		Message.DoubleParm = -1
			
		IF luo_existencia.numeromaximo(3,li_bodzonal,ll_docrel,1,True,sqlexi) THEN
			ll_numero = luo_existencia.numero
		END IF
		
		IF isnull(ll_numero) THEN
			ll_numero = 0
		END IF	
				
		IF luo_existencia.bodega_zonal(li_bodzonal,sqlexi,True) THEN
			ls_correozonal = luo_existencia.correozonal
		END IF	
		
		ls_correo = luo_existencia.correo
		
		IF ll_numero = 0 THEN
			ls_texto	 = "No es posible actualizar movto. en Existencia, por modificación anterior al mes de proceso(No presenta movimiento en bodegas)"
		ELSE	
			ls_texto	 = "No es posible actualizar movto. en Existencia Nº "+String(ll_numero)+"; por modificación anterior al mes de proceso"
		END IF
		
		ls_asunto = "Modifica Fruta Comercial Despacho Venta Directa Movto. Nº "+String(ll_docrel)
	//	li_devcorreo =  Send_mail("smtp.rioblanco.cl","<frutacomercial@rioblanco.cl>","<"+ls_correo+">","<"+ls_correozonal+">","",ls_asunto,ls_texto,"",ls_error)
		
		IF (li_devcorreo<0) THEN
			messagebox("Error No" + string(li_devcorreo),sErrorMsg)
		END IF
		RETURN 
	END IF
	
	IF NOT luo_existencia.numeromaximo(3,li_bodzonal,ll_docrel,1,True,sqlexi) THEN
		Message.DoubleParm = -1
		Return
	ELSE
		ll_numero = luo_existencia.numero
	END IF
			
	IF isnull(ll_numero) THEN
		Return
	END IF	
	
	IF NOT luo_existencia.actualizaexistencia(2,3,li_bodzonal,ll_numero,ll_docrel,True,sqlexi) THEN
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
	
	DELETE FROM dba.exismovtodeta 
		WHERE	mden_numero = :ll_numero
		AND	mden_tipdoc = 3
		AND	bode_codigo = :li_bodzonal
		USING sqlexi;
		COMMIT;
	
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
			Commit;

		ELSE
			F_ErrorBaseDatos(sqlexi, This.Title)
				
			RollBack;
			Return
		END IF
		
		sqlexi.AutoCommit	=	lb_AutoCommit
	
	END IF

END IF
end event

public subroutine productoreslotes (ref string productores[]);Long		ll_Fila
Integer	li_Secuencia
String	ls_productor, ls_ProdAnt, ls_Nula[]

Productores	=	ls_Nula

FOR ll_Fila = 1 TO dw_3.RowCount()
	ls_Productor	=	dw_3.Object.prod_rut[ll_Fila]
	
	IF ls_Productor <> ls_ProdAnt THEN
		li_Secuencia ++
		Productores[li_Secuencia]	=	ls_Productor
	
		ls_ProdAnt	=	ls_Productor
	END IF
NEXT

RETURN
end subroutine

protected function integer wf_modifica ();IF dw_3.AcceptText() = -1 THEN RETURN -1
IF dw_5.AcceptText() = -1 THEN RETURN -1
IF dw_7.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF (dw_3.ModifiedCount() + dw_3.DeletedCount()) > 0 THEN RETURN 0
IF (dw_5.ModifiedCount() + dw_5.DeletedCount()) > 0 THEN RETURN 0
IF (dw_7.ModifiedCount() + dw_7.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0

RETURN 1
end function

public subroutine determina_productoresenvase (integer ai_tipomovto);str_Productores_Envases	lstr_Prod_Enva
DataWindow	ldw_envase
Long			ll_Fila, ll_GuiaSII, ll_Fila_Lote,ll_Productor
Integer		li_ProdAnt, li_Secuencia

//Inicializa la Estructura de Instancia
wstr_Prod_Enva	= lstr_Prod_Enva

IF ai_TipoMovto = 64 THEN
	ldw_envase	=	dw_5
ELSE
	ldw_envase	=	dw_7
END IF

FOR ll_Fila	=	1 TO ldw_Envase.RowCount()
	ll_Productor	=	ldw_Envase.Object.prod_codigo[ll_Fila]
	
	IF ll_Productor <> li_ProdAnt THEN
		li_ProdAnt	=	ll_Productor
		li_Secuencia ++
		
//		IF ai_TipoMovto = 64 THEN
//			ll_Fila_Lote	=	dw_3.Find("prod_codigo = "+String(li_Productor),1,dw_3.RowCount())
//			
//			IF ll_Fila_Lote > 0 THEN
//				ll_GuiaSII	=	dw_3.Object.lote_guisii[ll_Fila_Lote]
//			ELSE
				ll_GuiaSII	=	0
//			END IF
//		END IF
		
		wstr_Prod_Enva.Productor[li_Secuencia]	=	ll_Productor
		wstr_Prod_Enva.GuiaSII[li_Secuencia]	=	ll_GuiaSII
		wstr_Prod_Enva.TipoMovto[li_Secuencia]	=	64
	END IF
NEXT

RETURN
end subroutine

public subroutine habilitaencab (boolean habilita);If Habilita Then
	dw_2.Object.mfco_numero.Protect				=	0
	dw_2.Object.clie_codigo.Protect					=	0
	dw_2.Object.mfco_docrel.Protect					=	0
	dw_2.Object.mfco_tipval.Protect					=	0
	dw_2.Object.tran_codigo.Protect					=	0
	dw_2.Object.cami_patent.Protect					=	0
	dw_2.Object.cami_patcar.Protect					=	0
	dw_2.Object.mfco_rutcho.Protect					=	0
	dw_2.Object.mfco_chofer.Protect					=	0
	dw_2.Object.mfco_totbul.Protect					=	0	
	dw_2.Object.clpr_rut.Protect						=	0
	dw_2.Object.mfco_tkbent.Protect					=	0
	dw_2.Object.mfco_tkbenc.Protect					=	0
	dw_2.Object.mfco_fecmov.Protect					=	0
	dw_2.Object.mfco_horaen.Protect					=	0
	
	dw_2.Object.mfco_numero.Color	=	0
	dw_2.Object.clie_codigo.Color		=	0
	dw_2.Object.mfco_docrel.Color	=	0
	dw_2.Object.mfco_tipval.Color		=	0	
	dw_2.Object.tran_codigo.Color		=	0
	dw_2.Object.cami_patent.Color	=	0
	dw_2.Object.cami_patcar.Color	=	0
	dw_2.Object.mfco_rutcho.Color	=	0
	dw_2.Object.mfco_chofer.Color	=	0
	dw_2.Object.mfco_totbul.Color		=	0	
	dw_2.Object.clpr_rut.Color			=	0
	dw_2.Object.mfco_tkbent.Color	=	0
	dw_2.Object.mfco_tkbenc.Color	=	0
	dw_2.Object.mfco_fecmov.Color	=	0
	dw_2.Object.mfco_horaen.Color	=	0	

	dw_2.Object.mfco_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.clie_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.mfco_docrel.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.mfco_tipval.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patent.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patcar.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.mfco_rutcho.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.mfco_chofer.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.mfco_totbul.BackGround.Color		=	RGB(255,255,255)	
	dw_2.Object.clpr_rut.BackGround.Color			=	RGB(255,255,255)	
	dw_2.Object.mfco_tkbent.BackGround.Color		=	RGB(255,255,255)	
	dw_2.Object.mfco_tkbenc.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.mfco_fecmov.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.mfco_horaen.BackGround.Color	=	RGB(255,255,255)	
	
	dw_2.Object.mfco_totbul.Protect					=	1
	dw_2.Object.mfco_tkbsal.Protect					=	1
	dw_2.Object.mfco_tkbsac.Protect					=	1
	dw_2.Object.mfco_horasa.Protect					=	1
	dw_2.Object.fecha_sal.Protect						=	1

	dw_2.Object.mfco_totbul.Color		=	RGB(255,255,255)		
	dw_2.Object.mfco_tkbsal.Color		=	RGB(255,255,255)	
	dw_2.Object.mfco_tkbsac.Color	=	RGB(255,255,255)	
	dw_2.Object.mfco_horasa.Color	=	RGB(255,255,255)	
	dw_2.Object.fecha_sal.Color		=	RGB(255,255,255)
	
	dw_2.Object.mfco_totbul.BackGround.Color		=	553648127	
	dw_2.Object.mfco_tkbsal.BackGround.Color		=	553648127
	dw_2.Object.mfco_tkbsac.BackGround.Color	=	553648127
	dw_2.Object.mfco_horasa.BackGround.Color	=	553648127
	dw_2.Object.fecha_sal.BackGround.Color		=	553648127
	
	dw_2.Object.buscacamion.visible					=  1
	tab_1.tp_3.Enabled									=	False
ELSE
	dw_2.Object.mfco_numero.Protect				=	1
	dw_2.Object.clie_codigo.Protect					=	1
	dw_2.Object.mfco_docrel.Protect					=	1	
	dw_2.Object.clpr_rut.Protect						=	1
	dw_2.Object.mfco_tipval.Protect					=	1
	dw_2.Object.tran_codigo.Protect					=	1
	dw_2.Object.cami_patent.Protect					=	1
	dw_2.Object.cami_patcar.Protect					=	1
	dw_2.Object.mfco_rutcho.Protect					=	1
	dw_2.Object.mfco_chofer.Protect					=	1
	
	dw_2.Object.mfco_numero.Color	=	RGB(255,255,255)
	dw_2.Object.clie_codigo.Color		=	RGB(255,255,255)
	dw_2.Object.mfco_docrel.Color	=	RGB(255,255,255)
	dw_2.Object.clpr_rut.Color			=	RGB(255,255,255)
	dw_2.Object.mfco_tipval.Color		=	RGB(255,255,255)
	dw_2.Object.tran_codigo.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patent.Color	=	RGB(255,255,255)
	dw_2.Object.cami_patcar.Color	=	RGB(255,255,255)
	dw_2.Object.mfco_rutcho.Color	=	RGB(255,255,255)
	dw_2.Object.mfco_chofer.Color	=	RGB(255,255,255)
	
	dw_2.Object.mfco_numero.BackGround.Color	=	553648127
	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
	dw_2.Object.mfco_docrel.BackGround.Color		=	553648127
	dw_2.Object.clpr_rut.BackGround.Color			=	553648127
	dw_2.Object.mfco_tipval.BackGround.Color		=	553648127
	dw_2.Object.tran_codigo.BackGround.Color		=	553648127
	dw_2.Object.cami_patent.BackGround.Color		=	553648127
	dw_2.Object.cami_patcar.BackGround.Color		=	553648127
	dw_2.Object.mfco_rutcho.BackGround.Color	=	553648127
	dw_2.Object.mfco_chofer.BackGround.Color	=	553648127
	
	
	dw_2.Object.b_orden.visible						=  0
   	dw_2.Object.buscacamion.visible					=  0

	If dw_2.Object.mfco_estmov[1] = 3 Then
		dw_2.Object.mfco_tkbent.Protect					=	1
		dw_2.Object.mfco_tkbenc.Protect					=	1
		dw_2.Object.mfco_fecmov.Protect					=	1
		dw_2.Object.mfco_horaen.Protect					=	1
		dw_2.Object.mfco_totbul.Protect					=	1

		dw_2.Object.mfco_tkbent.Color	=	RGB(255,255,255)	
		dw_2.Object.mfco_tkbenc.Color	=	RGB(255,255,255)	
		dw_2.Object.mfco_fecmov.Color	=	RGB(255,255,255)	
		dw_2.Object.mfco_horaen.Color	=	RGB(255,255,255)	
		dw_2.Object.mfco_totbul.Color		=	RGB(255,255,255)	
		
		dw_2.Object.mfco_tkbent.BackGround.Color		=	553648127	
		dw_2.Object.mfco_tkbenc.BackGround.Color	=	553648127	
		dw_2.Object.mfco_fecmov.BackGround.Color	=	553648127	
		dw_2.Object.mfco_horaen.BackGround.Color	=	553648127	
		dw_2.Object.mfco_totbul.BackGround.Color		=	553648127	
		
		dw_2.Object.mfco_tipval.Protect					=	0
		dw_2.Object.tran_codigo.Protect					=	0
		dw_2.Object.cami_patent.Protect					=	0
		dw_2.Object.cami_patcar.Protect					=	0
		dw_2.Object.mfco_rutcho.Protect					=	0
		dw_2.Object.mfco_chofer.Protect					=	0
		dw_2.Object.mfco_totbul.Protect					=	0
		dw_2.Object.clpr_rut.Protect						=	0	
		dw_2.Object.mfco_tkbent.Protect					=	0
		dw_2.Object.mfco_tkbenc.Protect					=	0
		dw_2.Object.mfco_fecmov.Protect					=	0
		dw_2.Object.mfco_horaen.Protect					=	0
		
		dw_2.Object.mfco_tipval.Color		=	0
		dw_2.Object.tran_codigo.Color		=	0	
		dw_2.Object.cami_patent.Color	=	0
		dw_2.Object.cami_patcar.Color	=	0
		dw_2.Object.mfco_rutcho.Color	=	0
		dw_2.Object.mfco_chofer.Color	=	0
		dw_2.Object.mfco_totbul.Color		=	0
		dw_2.Object.clpr_rut.Color			=	0	
		dw_2.Object.mfco_tkbent.Color	=	0
		dw_2.Object.mfco_tkbenc.Color	=	0
		dw_2.Object.mfco_fecmov.Color	=	0
		dw_2.Object.mfco_horaen.Color	=	0
		
		dw_2.Object.mfco_tipval.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.cami_patent.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.cami_patcar.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.mfco_rutcho.BackGround.Color	=	RGB(255,255,255)
		dw_2.Object.mfco_chofer.BackGround.Color	=	RGB(255,255,255)
		dw_2.Object.mfco_totbul.BackGround.Color		=	RGB(255,255,255)	
		dw_2.Object.clpr_rut.BackGround.Color			=	RGB(255,255,255)	
		dw_2.Object.mfco_tkbent.BackGround.Color		=	RGB(255,255,255)	
		dw_2.Object.mfco_tkbenc.BackGround.Color	=	RGB(255,255,255)	
		dw_2.Object.mfco_fecmov.BackGround.Color	=	RGB(255,255,255)	
		dw_2.Object.mfco_horaen.BackGround.Color	=	RGB(255,255,255)	
		
		dw_2.Object.mfco_totbul.Protect					=	1
		dw_2.Object.mfco_tkbsal.Protect					=	1
		dw_2.Object.mfco_tkbsac.Protect					=	1
		dw_2.Object.mfco_horasa.Protect					=	1
		dw_2.Object.fecha_sal.Protect						=	1
		
		dw_2.Object.mfco_totbul.Color		=	RGB(255,255,255)				
		dw_2.Object.mfco_tkbsal.Color		=	RGB(255,255,255)	
		dw_2.Object.mfco_tkbsac.Color	=	RGB(255,255,255)	
		dw_2.Object.mfco_horasa.Color	=	RGB(255,255,255)	
		dw_2.Object.fecha_sal.Color		=	RGB(255,255,255)	

		dw_2.Object.mfco_totbul.BackGround.Color		=	553648127			
		dw_2.Object.mfco_tkbsal.BackGround.Color		=	553648127
		dw_2.Object.mfco_tkbsac.BackGround.Color	=	553648127
		dw_2.Object.mfco_horasa.BackGround.Color	=	553648127
		dw_2.Object.fecha_sal.BackGround.Color		=	553648127
		
		dw_2.Object.buscacamion.visible					=  1
	End If
End If

If dw_2.Object.mfco_estmov[1] = 4 And dw_2.Object.mfco_guiemi[1] <> 2 Then cb_guia.Enabled	=	False

If istr_mant.Argumento[9] = '9' Then
	dw_2.Object.mfco_rutcho.Protect					=	1
	dw_2.Object.mfco_chofer.Protect					=	1
	dw_2.Object.clpr_rut.Protect						=	1
	dw_2.Object.tran_codigo.Protect					=	1
	dw_2.Object.cami_patent.Protect					=	1
	dw_2.Object.cami_patcar.Protect					=	1
	
	dw_2.Object.mfco_rutcho.Color	=	RGB(255,255,255)	
	dw_2.Object.mfco_chofer.Color	=	RGB(255,255,255)	
	dw_2.Object.clpr_rut.Color			=	RGB(255,255,255)	
	dw_2.Object.tran_codigo.Color		=	RGB(255,255,255)	
	dw_2.Object.cami_patent.Color	=	RGB(255,255,255)	
	dw_2.Object.cami_patcar.Color	=	RGB(255,255,255)	
	
	dw_2.Object.mfco_rutcho.BackGround.Color	=	553648127
	dw_2.Object.mfco_chofer.BackGround.Color	=	553648127
	dw_2.Object.clpr_rut.BackGround.Color			=	553648127
	dw_2.Object.tran_codigo.BackGround.Color		=	553648127
	dw_2.Object.cami_patent.BackGround.Color		=	553648127
	dw_2.Object.cami_patcar.BackGround.Color		=	553648127
	dw_2.Object.BuscaCliente.Enabled					=	False
	dw_2.Object.BuscaCamion.Enabled				=	False
	dw_2.Object.refg_tkbsal_t.Text						=	'Boleta'
	cb_guia.Enabled										=	False
	cb_guia.Visible											=	False
Else
	dw_2.Object.mfco_guisii.Protect					=	1
	dw_2.Object.mfco_guisii.Color						=	RGB(255,255,255)	
	dw_2.Object.mfco_guisii.BackGround.Color		=	553648127
End If
end subroutine

public subroutine habilitafrigo ();
end subroutine

public subroutine habilitafrigorifico ();
dw_2.Object.mfco_tkbent.Protect					=	1
dw_2.Object.mfco_tkbent.BackGround.Color		=	rgb(166,180,210)
dw_2.Object.mfco_tkbenc.Protect					=	1
dw_2.Object.mfco_tkbenc.BackGround.Color	=	rgb(166,180,210)
dw_2.Object.mfco_fecmov.Protect					=	1
dw_2.Object.mfco_fecmov.BackGround.Color	=	rgb(166,180,210)
dw_2.Object.mfco_horaen.Protect					=	1
dw_2.Object.mfco_horaen.BackGround.Color	=	rgb(166,180,210)

tab_1.tp_3.Enabled			=	True
tab_1.tp_1.Enabled			=	True

//dw_2.Object.mfco_estmov[1] = 2

ib_Frigo							=	True

dw_2.Object.destare.Text	=	'Salida'

tab_1.SelectTab(1)

end subroutine

public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, integer al_numero);Long		ll_Numero
Boolean	lb_Retorno = True
Integer	li_cliente

li_cliente	=	dw_2.Object.clie_codigo[1]

SELECT	mfco_numero
	INTO	:ll_Numero
	FROM	dbo.spro_movtofrutacomenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfco_numero	=	:al_Numero 
	AND 	clie_codigo	=	:li_cliente;
	
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

public function boolean buscagarantia (long al_orden, integer ai_tipo);Integer	li_Contador, li_planta, li_tiporet, li_garantia
Long		ll_Numero, ll_productor
String   ls_rut, ls_obser
Boolean	lb_Retorno = True

li_planta = dw_2.Object.plde_codigo[1]

IF integer(istr_mant.argumento[2]) = 32 THEN

	SELECT	re.oret_numero, vt.clpr_rut, vt.odfc_observ, vt.odfc_tipret, vt.odfc_garant
		INTO	:ll_Numero, :ls_rut, :ls_obser, :li_tiporet, :li_garantia
		FROM	dbo.spro_ordenventacomenca vt, dbo.spro_ordenretiroventaenc re
	  WHERE	re.plde_codigo	=	:li_Planta
		 AND	re.oret_numero	=	:al_orden
		 AND  re.plde_codigo =  vt.plde_codigo
		 AND  re.odfc_numero =  vt.odfc_numero;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Orden de Retiro")
	
		lb_Retorno	=	False
	ELSEIF sqlca.SQLCode = 0 THEN

		dw_2.SetItem(1,"garantia",li_garantia)
		
		IF li_tiporet = 2 THEN
			SELECT prod_codigo INTO :ll_productor
			  FROM dbo.clienprove
			  WHERE clpr_rut = :ls_rut;
			 IF sqlca.SQLCode = -1 THEN
 				F_ErrorBaseDatos(sqlca, "Lectura de Tabla Clientes")
				istr_mant.argumento[5] = ""	 
			ELSEIF sqlca.SQLCode = 0 THEN
				istr_mant.argumento[5] = String(ll_productor)
			END IF	
		ELSE
			istr_mant.argumento[5] = ""
		END IF

	ELSE
		MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
		lb_Retorno	=	False
	END IF
	
ELSE
	
	SELECT	odfc_numero, clpr_rut, odfc_observ, odfc_tipret, odfc_garant
		INTO	:ll_Numero, :ls_rut, :ls_obser, :li_tiporet, :li_garantia
		FROM	dbo.spro_ordenventacomenca 
	  WHERE	plde_codigo	=	:li_Planta
		 AND	odfc_numero	=	:al_orden;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Orden de Venta")
	
		lb_Retorno	=	False
	ELSEIF sqlca.SQLCode = 0 THEN
		
		IF li_tiporet = 2 THEN

			dw_2.SetItem(1,"garantia",li_garantia)
			 
			SELECT prod_codigo INTO :ll_productor
			  FROM dbo.clienprove
			  WHERE clpr_rut = :ls_rut;
			 IF sqlca.SQLCode = -1 THEN
 				F_ErrorBaseDatos(sqlca, "Lectura de Tabla Clientes")
				istr_mant.argumento[5] = "" 
			ELSEIF sqlca.SQLCode = 0 THEN
				istr_mant.argumento[5] = String(ll_productor)
			END IF
		ELSE
			MessageBox("Atención","Número de Correlativo No es del Tipo Orden de Venta Parcial. Ingrese Otro.")
   		lb_Retorno	=	False
		END IF	
	ELSE
		MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
		lb_Retorno	=	False
	END IF
	
END IF

RETURN lb_Retorno
end function

public subroutine datosorden ();//
//IF istr_mant.Argumento[2] = "35" THEN
//end if	
end subroutine

public subroutine captura_totales ();Long			ll_Total_Bultos, ll_Fila
Decimal{3}	ld_Total_PesoEnv_Ent,ld_Total_PesoEnv_Sal, ld_Total_Neto,&
				ld_Total_KBE, ld_Total_KBS, ld_KBE_Camion, ld_KBE_Carro,&
				ld_KBS_Camion, ld_KBS_Carro, ld_TaraCamion, ld_TaraCarro,ld_KBR_Romana

dw_2.AcceptText()
dw_3.AcceptText()
dw_5.AcceptText()
dw_7.AcceptText()

ll_Fila	=	dw_3.RowCount()

IF ll_Fila > 0 THEN
	dw_3.GroupCalc()
	ll_Total_Bultos		=	dw_3.GetItemNumber(ll_Fila,"total_bultos")
	ld_KBR_Romana	=	dw_3.GetItemNumber(ll_Fila,"total_romanas")
END IF

ll_Fila	=	dw_5.RowCount()

IF ll_Fila > 0 THEN
	dw_5.GroupCalc()
	ld_Total_PesoEnv_Ent	=	dw_5.Object.total_pesoenv[ll_Fila]
END IF

ll_Fila	=	dw_7.RowCount()

IF ll_Fila > 0 THEN
	dw_7.GroupCalc()
	ld_Total_PesoEnv_Sal	=	dw_7.Object.total_pesoenv[ll_Fila]
END IF

//Determina Peso Neto del Camión
IF ib_Salida THEN
	ld_KBE_Camion	=	dw_2.Object.mfco_tkbent[1]
	ld_KBE_Carro		=	dw_2.Object.mfco_tkbenc[1]

	IF Isnull(ld_KBE_Camion) THEN ld_KBE_Camion = 0
	IF Isnull(ld_KBE_Carro) THEN ld_KBE_Carro = 0

	ld_Total_KBE		=	ld_KBE_Camion + ld_KBE_Carro

	ld_KBS_Camion	=	dw_2.Object.mfco_tkbsal[1]
	ld_KBS_Carro		=	dw_2.Object.mfco_tkbsac[1]

	IF Isnull(ld_KBS_Camion) THEN ld_KBS_Camion = 0
	IF Isnull(ld_KBS_Carro) THEN ld_KBS_Carro = 0

	IF ld_KBS_Camion + ld_KBS_Carro > 0 AND &
		ld_KBS_Camion + ld_KBS_Carro < ld_Total_PesoEnv_Sal THEN
		MessageBox("Atención","Kilos de Salida no pueden ser menor a los Envases de Salida")
		RETURN
	ELSE
		ld_Total_KBS	=	ld_KBS_Camion + ld_KBS_Carro + ld_Total_PesoEnv_Sal 
	END IF

	ld_Total_Neto		=	ld_KBR_Romana - ld_Total_KBS //- ( ld_Total_KBE + ld_Total_PesoEnv_Ent )

	dw_2.Object.mfco_tkensa[1]	=	ld_Total_PesoEnv_Sal
	dw_2.Object.mfco_tpneto[1]	=	ld_Total_Neto
	
END IF

dw_2.Object.mfco_totbul[1]			=	ll_Total_Bultos
dw_2.Object.mfco_tkenen[1]			=	ld_Total_PesoEnv_Ent

IF ib_Destare THEN
	Destare(True)
END IF

RETURN
end subroutine

public subroutine buscaorden ();Long           ll_Nula
Str_Busqueda	lstr_busq

SetNull(ll_Nula)

lstr_busq.argum[1] = String(gstr_param.plde_codigo)
lstr_busq.argum[2] = ''
lstr_busq.argum[3] = ''									

IF integer(istr_mant.argumento[2]) = 35 THEN
	lstr_busq.argum[2] = '2'
	OpenWithParm(w_busc_ordenventacomercial, lstr_busq)
ELSE
	OpenWithParm(w_busc_ordenretiroventa, lstr_busq)
END IF


lstr_busq	=	Message.PowerObjectParm
IF lstr_busq.argum[2] <> "" THEN
	
	IF Not existeorden(long(lstr_busq.argum[2]),integer(lstr_busq.argum[1])) THEN
		dw_2.SetItem(1, "mfco_docrel", ll_Nula)
		RETURN
	ELSE
		dw_2.SetItem(1, "mfco_docrel", long(lstr_busq.argum[2]))
		istr_mant.argumento[10] = lstr_busq.argum[2]
	END IF

END IF
end subroutine

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
	dw_2.Object.mfco_rutcho[il_fila]		=	lstr_busq.argum[5]
	dw_2.Object.mfco_chofer[il_fila]		=	lstr_busq.argum[4]
	is_rut = lstr_busq.argum[5]
	dw_2.SetFocus()
END IF

RETURN
end subroutine

public function boolean existeorden (long al_orden, integer ai_tipo);Integer	li_Contador, li_planta, li_tiporet, li_garantia, li_estado
Long		ll_Numero, ll_numventa, ll_productor
String   ls_rut, ls_obser
Boolean	lb_Retorno = True

li_planta = dw_2.Object.plde_codigo[1]

IF integer(istr_mant.argumento[2]) = 32 THEN

	SELECT	re.oret_numero, vt.clpr_rut, vt.odfc_observ, vt.odfc_tipret, vt.odfc_garant,
	         vt.odfc_estado, vt.odfc_numero
		INTO	:ll_Numero, :ls_rut, :ls_obser, :li_tiporet, :li_garantia, :li_estado,
		      :ll_Numventa
		FROM	dbo.spro_ordenventacomenca vt, dbo.spro_ordenretiroventaenc re
	  WHERE	re.plde_codigo		=	:li_Planta
		 AND	re.oret_numero	=	:al_orden
		 AND  re.plde_codigo 		=  vt.plde_codigo
		 AND  re.odfc_numero 	=  vt.odfc_numero;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Orden de Retiro")
	
		lb_Retorno					=	False
	ELSEIF sqlca.SQLCode = 0 THEN
		
		IF li_estado = 0 THEN
			MessageBox("Atención","Orden de Venta ya se encuentra cerrada. Ingrese Otra.")
			RETURN False
		END IF	
		
		dw_2.SetItem(1,"clpr_rut",ls_rut)
		dw_2.SetItem(1,"garantia",li_garantia)
		
		ib_cierraventa = FALSE
		Istr_mant.argumento[12] = String(ll_NumVenta)
		IF li_tiporet = 2 THEN
			SELECT prod_codigo INTO :ll_productor
			  FROM dbo.clienprove
			  WHERE clpr_rut = :ls_rut;
			 IF sqlca.SQLCode = -1 THEN
 				F_ErrorBaseDatos(sqlca, "Lectura de Tabla Clientes")
				istr_mant.argumento[5] = ""	 
			ELSEIF sqlca.SQLCode = 0 THEN
				istr_mant.argumento[5] = String(ll_productor)
			END IF	
		ELSE
			istr_mant.argumento[5] = ""
			ib_cierraventa = TRUE
			
		END IF
		
		dw_2.SetItem(1,"mfco_observ",ls_obser)
	ELSE
		MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
		lb_Retorno	=	False
	END IF
	
ELSE
	
	SELECT	odfc_numero, clpr_rut, odfc_observ, odfc_tipret, odfc_garant
		INTO	:ll_Numero, :ls_rut, :ls_obser, :li_tiporet, :li_garantia
		FROM	dbo.spro_ordenventacomenca 
	  WHERE	plde_codigo		=	:li_Planta
		 AND	odfc_numero	=	:al_orden;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Orden de Venta")
	
		lb_Retorno				=	False
	ELSEIF sqlca.SQLCode = 0 THEN
		
		IF li_tiporet = 2 THEN
		   	dw_2.SetItem(1,"clpr_rut",ls_rut)
 		   	dw_2.SetItem(1,"mfco_observ",ls_obser)
			dw_2.SetItem(1,"garantia",li_garantia)
			 
			SELECT prod_codigo INTO :ll_productor
			  FROM dbo.clienprove
			  WHERE clpr_rut = :ls_rut;
			 IF sqlca.SQLCode = -1 THEN
 				F_ErrorBaseDatos(sqlca, "Lectura de Tabla Clientes")
				istr_mant.argumento[5] = "" 
			ELSEIF sqlca.SQLCode = 0 THEN
				istr_mant.argumento[5] = String(ll_productor)
			END IF
		ELSE
			MessageBox("Atención","Número de Correlativo No es del Tipo Orden de Venta Parcial. Ingrese Otro.")
   		lb_Retorno	=	False
		END IF	
	ELSE
		MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
		lb_Retorno	=	False
	END IF
	
END IF

RETURN lb_Retorno
end function

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
		
		IF dw_3.DeletedCount() > 0 THEN Borrando = True
	END IF
END IF

IF Borrando THEN
	IF dw_5.Update(True, False) = 1 THEN 					//Detalle Envase Recep
		IF dw_7.Update(True, False) = 1 THEN				//Detalle Envase Retirados
			IF dw_1.Update(True, False) = 1 THEN			//Encabezado de Envases
				IF dw_2.Update(True,False) = 1 THEN			//Encabezado Movimiento
					IF dw_3.Update(True,False) = 1 THEN		//Detalle Movimiento
						Commit;
							
						IF sqlca.SQLCode <> 0 THEN
							F_ErrorBaseDatos(sqlca, This.Title)
							RollBack;
						ELSE
							lb_Retorno	=	True
									
							dw_7.ResetUpdate()
							dw_5.ResetUpdate()
							dw_3.ResetUpdate()
							dw_2.ResetUpdate()
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
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN		 				//Encabezado Movimiento
		IF dw_3.Update(True, False) = 1 THEN					//Detalle de Movimiento
			IF dw_1.Update(True, False) = 1 THEN				//Encabezado de Envases
				IF dw_5.Update(True,False) = 1 THEN				//Envases Recepcionados
					IF dw_7.Update(True,False) = 1 THEN			//Envases Retirados
					   
						IF ib_cierraventa	 AND dw_2.Object.mfco_estmov[1] = 3 THEN
							IF  ActualizaOrden() THEN
								 Commit;
							ELSE
								RollBack;
							END IF	
						ELSE
							Commit;
						END IF
						
						
						IF sqlca.SQLCode <> 0 THEN
							F_ErrorBaseDatos(sqlca, This.Title)
						ELSE
							lb_Retorno	=	True
          					
							dw_7.ResetUpdate()
							dw_5.ResetUpdate()
							dw_3.ResetUpdate()
							dw_2.ResetUpdate()
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

public subroutine habilitagrabacion (string as_columna);Boolean	lb_Estado = True
Date		ld_Fecha
Time		lt_Hora
Datetime ldt_fechahora

IF as_Columna <> "fecha_sal" AND &
	(dw_2.Object.fecha_sal[1] = ldt_fechahora OR IsNull(dw_2.Object.fecha_sal[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "mfco_horasa" AND &
	(dw_2.Object.mfco_horasa[1] = lt_Hora OR IsNull(dw_2.Object.mfco_horasa[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "mfco_tkbsal" AND &
	(dw_2.Object.mfco_tkbent[1] = 0 OR IsNull(dw_2.Object.mfco_tkbent[1])) THEN
	lb_Estado = False
END IF

pb_Grabar.Enabled	=	lb_Estado
end subroutine

public subroutine destare (boolean ab_actualiza);Long			ll_Fila
Integer		li_Especie, li_TipoEnvase, li_Envase, li_TipoAnt, li_EnvaAnt,&
				li_TotBultos, li_Lote, li_LoteAnt, li_BultosLote
Date			ld_Fechamovto
Decimal{3}	ld_TotalNeto, ld_PesoEstandar[], ld_PesoMinimo[], ld_PesoMaximo[],&
				ld_TotalEstandar, ld_PesoDistrib[], ld_LoteDistrib, ld_NetoLoteEnvase,&
				ld_TotalDistrib, ld_Remanente, ld_Estandar

Double		ld_Factor

ld_FechaMovto	=	dw_2.Object.mfco_fecmov[1]
ld_TotalNeto	=	dw_2.Object.mfco_tpneto[1]

dw_3.SetSort("lofc_pltcod A, lofc_espcod A, lofc_lotefc A, enva_tipoen A, enva_codigo A")
dw_3.Sort()

FOR ll_Fila = 1 TO dw_3.RowCount()
	
	li_Especie		=	dw_3.Object.lofc_espcod[ll_fila]
	li_TipoEnvase	=	dw_3.Object.enva_tipoen[ll_Fila]
	li_Envase		=	dw_3.Object.enva_codigo[ll_Fila]
	
	IF iuo_PesoEstanEspe.Existe(li_Especie, li_TipoEnvase, li_Envase, ld_FechaMovto, True, SQLCA) THEN
		
		ld_PesoEstandar[ll_fila]	=	iuo_PesoEstanEspe.PesoDistrib
		ld_PesoMinimo[ll_fila]		=	iuo_PesoEstanEspe.PesoMinimo
		ld_PesoMaximo[ll_fila]		=	iuo_PesoEstanEspe.PesoMaximo
	ELSE
		ld_PesoEstandar[ll_fila]	=	0
		ld_PesoMinimo[ll_fila]		=	0
		ld_PesoMaximo[ll_fila]		=	0
	END IF
		
	li_TotBultos		=	dw_3.Object.mfcd_bulent[ll_Fila]
	ld_TotalEstandar	=	ld_TotalEstandar + Round(li_TotBultos*ld_PesoEstandar[ll_fila],3)

NEXT

FOR ll_fila=1 To dw_3.RowCount()
	IF ld_PesoEstandar[ll_fila]=0 THEN
		Messagebox("Error de Consistencia","Datos Elegidos no poseen Peso Estandar.")
		Return
	END IF
NEXT

FOR ll_fila=1 to dw_3.RowCount()
	ld_pesodistrib[ll_fila] = Round(((Round(li_TotBultos*ld_PesoEstandar[ll_fila],3)) * ld_TotalNeto)/ld_TotalEstandar,1)
   ld_totaldistrib         = ld_totaldistrib + (Round(((Round(li_TotBultos*ld_PesoEstandar[ll_fila],3)) * ld_TotalNeto)/ld_TotalEstandar,1))
NEXT

ld_Remanente = ld_TotalDistrib - ld_TotalNeto
	
IF ld_Remanente <> 0 THEN
	ld_pesodistrib[dw_3.RowCount()]	=	ld_pesodistrib[dw_3.RowCount()] - ld_Remanente
END IF


fOR ll_fila=1 to dw_3.RowCount()
//	dw_3.Object.mfcd_kilrom[ll_fila] = ld_pesodistrib[ll_fila]
NEXT

ib_destare = TRUE

pb_ins_det.Enabled 		=	False
pb_eli_det.Enabled 		= 	False
istr_mant.Solo_Consulta = 	True

end subroutine

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date		ld_Fecha
Time		lt_Hora

If istr_Mant.Argumento[9] <> '9' Then
	If as_Columna <> "mfco_fecmov" And &
		(dw_2.Object.mfco_fecmov[1] = ld_Fecha Or IsNull(dw_2.Object.mfco_fecmov[1])) Then
		lb_Estado = False
	End If
	
	If as_Columna <> "mfco_horaen" And &
		(dw_2.Object.mfco_horaen[1] = lt_Hora Or IsNull(dw_2.Object.mfco_horaen[1])) Then
		lb_Estado = False
	End If
	
	If as_Columna <> "tran_codigo" And IsNull(dw_2.Object.tran_codigo[1]) Then
		lb_Estado = False
	End If
		
	If as_Columna <> "cami_patent" And (dw_2.Object.cami_patent[1] = "" Or IsNull(dw_2.Object.cami_patent[1])) Then
		lb_Estado = False
	End If
		
	If as_Columna <> "mfco_chofer" And (dw_2.Object.mfco_chofer[1] = "" Or IsNull(dw_2.Object.mfco_chofer[1])) Then
		lb_Estado = False
	End If
End If

If as_Columna <> "mfco_tipval" And &
	(dw_2.Object.mfco_tipval[1] = 0 Or IsNull(dw_2.Object.mfco_tipval[1])) Then
	lb_Estado = False
End If

tab_1.tp_2.Enabled	=	lb_Estado
tab_1.tp_3.Enabled	=	lb_Estado

If dw_2.Object.mfco_estmov[1] > 3 Then
	pb_ins_det.Enabled	=	False
Else
	pb_ins_det.Enabled	=	lb_Estado
End If	

If integer(istr_mant.argumento[2]) = 35 Then
	pb_ins_det.Enabled	=	lb_Estado	
End If	

pb_grabar.Enabled		=	lb_Estado


If dw_2.Object.mfco_estmov[1] = 3 Then pb_grabar.Enabled	=	True
If dw_2.Object.mfco_estmov[1] = 3 Then cb_guia.Enabled	=	True

IF dw_2.Object.mfco_estmov[1] = 1 AND dw_2.Object.mfco_pcopda[1] = 2 THEN
	pb_grabar.Enabled	= 	TRUE
END IF

end subroutine

public subroutine habilitasalida ();
dw_2.Object.mfco_tkbsal.Protect				=	0
dw_2.Object.mfco_tkbsal.BackGround.Color	=	RGB(255,255,255)
dw_2.Object.mfco_tkbsac.Protect				=	0
dw_2.Object.mfco_tkbsac.BackGround.Color	=	RGB(255,255,255)
dw_2.Object.mfco_horasa.Protect				=	0
dw_2.Object.mfco_horasa.BackGround.Color	=	RGB(255,255,255)
dw_2.Object.fecha_sal.Protect					=	0
dw_2.Object.fecha_sal.BackGround.Color		=	RGB(255,255,255)

dw_2.Object.mfco_tkbent.Protect					=	1
dw_2.Object.mfco_tkbent.BackGround.Color		=	rgb(166,180,210)
dw_2.Object.mfco_tkbenc.Protect					=	1
dw_2.Object.mfco_tkbenc.BackGround.Color		=	rgb(166,180,210)
dw_2.Object.mfco_fecmov.Protect					=	1
dw_2.Object.mfco_fecmov.BackGround.Color		=	rgb(166,180,210)
dw_2.Object.mfco_horaen.Protect					=	1
dw_2.Object.mfco_horaen.BackGround.Color		=	rgb(166,180,210)

tab_1.tp_3.Enabled			=	True
tab_1.tp_1.Enabled			=	True
tab_1.tp_2.Enabled			=	True

//dw_2.Object.mfco_estmov[1] =  2

ib_Frigo							=  True
ib_Salida						=	True

dw_2.Object.destare.Text	=	'Destare'

idt_FechaSistema				=	F_FechaHora()

dw_2.Object.fecha_sal[1]	=	Date(Mid(String(idt_FechaSistema),1,10))
dw_2.Object.mfco_horasa[1]	=	Time(Mid(String(idt_FechaSistema),12,8))

//captura_totales()
captura_entrada()

tab_1.SelectTab(1)
end subroutine

public function boolean actualizaorden ();Integer li_planta
Long	  ll_Orden

li_planta = dw_2.Object.plde_codigo[1]
ll_orden  = Long(istr_mant.argumento[12])

UPDATE dbo.spro_ordenventacomenca
   SET odfc_estado = 0
 WHERE plde_codigo = :li_planta
   AND odfc_numero = :ll_orden;
	
	  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Orden de Venta")
	RETURN FALSE
END IF	
	
RETURN TRUE
end function

public subroutine captura_entrada ();Long			ll_Total_Bultos, ll_Fila
Decimal{3}	ld_Total_PesoEnv_Ent,ld_Total_PesoEnv_Sal, ld_Total_Neto,&
				ld_Total_KBE, ld_Total_KBS, ld_KBE_Camion, ld_KBE_Carro,&
				ld_KBS_Camion, ld_KBS_Carro, ld_TaraCamion, ld_TaraCarro,ld_KBR_Romana, &
				ld_valor_neto, ld_valor_bruto

dw_2.AcceptText()
dw_3.AcceptText()
ld_KBR_Romana = 0

IF ( dw_2.RowCount() + dw_3.RowCount() ) < 2 THEN Return

ll_Fila					=	dw_3.RowCount()
ld_Total_PesoEnv_Sal = 	dw_2.Object.mfco_tkensa[1]

IF ll_Fila > 0 THEN
	dw_3.GroupCalc()
	ll_Total_Bultos	=	dw_3.Object.total_bultos[ll_Fila]
	ld_KBR_Romana		=	dw_3.Object.total_romanas[ll_Fila]
	ld_Total_Neto		= 	dw_3.Object.total_kilos[ll_Fila]
	ld_valor_neto		=	dw_3.Object.total_neto[ll_Fila]
	ld_valor_bruto		=	dw_3.Object.total_bruto[ll_Fila]
END IF

dw_2.Object.mfco_tkbsal[1] 	= 	ld_KBR_Romana
dw_2.Object.mfco_tpneto[1]		=	ld_Total_Neto

dw_2.Object.peso_bruto[1]		=	ld_KBR_Romana
dw_2.Object.total_neto[1]		=	ld_valor_neto
dw_2.Object.total_iva[1]		=	ld_valor_bruto - ld_valor_neto
dw_2.Object.total_bruto[1]		=	ld_valor_bruto

RETURN
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

public subroutine buscacliente ();Str_busqueda	lstr_busq

lstr_busq.argum[1] = '1'

OpenWithParm(w_busc_clienprove, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF UpperBound(lstr_busq.argum) < 2 THEN Return

IF lstr_busq.argum[1] = "" THEN
	dw_2.SetColumn("cami_patent")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clpr_rut[il_fila]		=	lstr_busq.argum[1]
	dw_2.Object.clpr_nombre[il_fila]	=	lstr_busq.argum[2]
	is_rutclie								=	lstr_busq.argum[1]
	dw_2.SetFocus()
END IF

RETURN
end subroutine

public function boolean conexionexistencia ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta

IF ib_ConectadoExistencia THEN
	DISCONNECT USING sqlexi;
END IF

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

SELECT 	cone_nomodb,	cone_nomser,	cone_nombas,
			cone_nodbms,	cone_nomusu,	cone_passwo  
	INTO 	:ls_nomodb,		:ls_nomser,		:ls_nombas,
			:ls_nodbms,		:ls_Usuario,		:ls_Password
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

public function boolean datos_correo ();Integer li_codigo
String	ls_numero
Date		ld_fecha

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

ls_numero = String(dw_2.Object.mfge_numero[1])
ld_fecha = dw_2.Object.mfge_fecmov[1]

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

on w_maed_movtofrutacomercial.create
int iCurrent
call super::create
this.cb_guia=create cb_guia
this.tab_1=create tab_1
this.dw_exideta=create dw_exideta
this.dw_exiencab=create dw_exiencab
this.dw_exidetaborra=create dw_exidetaborra
this.dw_exismovtodetanulos=create dw_exismovtodetanulos
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_guia
this.Control[iCurrent+2]=this.tab_1
this.Control[iCurrent+3]=this.dw_exideta
this.Control[iCurrent+4]=this.dw_exiencab
this.Control[iCurrent+5]=this.dw_exidetaborra
this.Control[iCurrent+6]=this.dw_exismovtodetanulos
end on

on w_maed_movtofrutacomercial.destroy
call super::destroy
destroy(this.cb_guia)
destroy(this.tab_1)
destroy(this.dw_exideta)
destroy(this.dw_exiencab)
destroy(this.dw_exidetaborra)
destroy(this.dw_exismovtodetanulos)
end on

event open;/* Argumentos
istr_mant.argumento[1] 		= 	Código Planta
istr_mant.argumento[2] 		= 	Tipo de Movimiento 32 
istr_mant.argumento[3] 		= 	Número de Despacho o Destare
istr_mant.argumento[4] 		= 	Sentido del Movimiento de Envases // Depende Solicitud
istr_mant.argumento[5] 		= 	Cliente - Codigo Productor // 
istr_mant.argumento[6] 		= 	
istr_mant.argumento[7] 		= 	
istr_mant.argumento[8] 		= 	
istr_mant.argumento[9] 		=  7 Tipo Doc. orden Retiro - Vacío Indica Orden de Venta
istr_mant.argumento[10] 	=  Orden de Retiro 
istr_mant.argumento[11] 	=  Tipo de Movimiento: 32 Despachos O.Retiro - 35 Destare Cliente O.Vta
istr_mant.argumento[12] 	= 	Orden de Venta
istr_mant.argumento[13] 	= 	Fecha de Movimiento
istr_mant.argumento[30] 	= 	Cliente Movimiento
*/
str_busqueda lstr_busq

x												= 	0
y												= 	0
This.Height									= 	2520
im_menu										= 	m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

lstr_busq = Message.PowerObjectParm

istr_mant.argumento[1]		=	String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[2]		=	lstr_busq.argum[1]
istr_mant.argumento[4]		=	'2'
istr_mant.argumento[5]		=	""
istr_mant.argumento[7]		=	'0'
istr_mant.argumento[9]		=	lstr_busq.argum[2]	
istr_Mant.Argumento[30]	=	String(gi_CodExport)

IF integer(istr_mant.argumento[2]) = 32 THEN
	this.title = 'Despacho Venta Directa'
	istr_mant.argumento[6] = '3'
ELSE
	this.title = 'Venta a Servicio'
	istr_mant.argumento[6] = '1'
	cb_guia.visible=False
END IF	

dw_3	=	tab_1.tp_1.dw_detafruta
dw_5	=	tab_1.tp_2.dw_envrec
dw_7	=	tab_1.tp_3.dw_envdes

dw_2.GetChild("tran_codigo", idwc_Transp)
idwc_Transp.SetTransObject(sqlca)

IF idwc_Transp.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Transportistas")
	idwc_Transp.InsertRow(0)
ELSE
	idwc_Transp.SetSort("tran_nombre A")
	idwc_Transp.Sort()
END IF

dw_2.GetChild("clpr_rut", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)

IF idwc_cliente.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Clientes")
	idwc_cliente.InsertRow(0)
END IF

dw_3.GetChild("cama_codigo",idwc_Camara)
idwc_Camara.SetTransObject(SQLCA)
idwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 84")

dw_3.Modify("datawindow.message.title='Error '+ is_titulo")
dw_3.Modify("DataWindow.Footer.Height = 84")

dw_5.Modify("datawindow.message.title='Error '+ is_titulo")
dw_5.Modify("DataWindow.Footer.Height = 84")

dw_7.Modify("datawindow.message.title='Error '+ is_titulo")
dw_7.Modify("DataWindow.Footer.Height = 84")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.solo_consulta			=	False

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

iuo_Transport			=	Create uo_transportista
iuo_Camion				=	Create uo_camiones
iuo_Especie				=	Create uo_especie
iuo_PesoEstanEspe	=	Create uo_PesoEstanEspe
iuo_FechaMovto		=	Create uo_FechaMovto
iuo_tipomovtofruta		=	Create uo_tipomovtofruta
iuo_TipoMovtoEnva	=	Create uo_tipomovtofruta
iuo_clprv					=	Create uo_clienprove
iuo_PltaDestino			=	Create uo_plantadesp		
iuo_calicosechero		=  Create uo_calicosechero  
iuo_cliente				=	Create uo_clientesprod
iuo_bins					=	Create uo_bins
end event

event ue_borra_detalle;call super::ue_borra_detalle;Integer	li_tabpage, li_borra
Boolean	lb_estado
str_mant_envases	lstr_mant

li_tabpage	=	tab_1.SelectedTab

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

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
	
	//ProductoresLotes(lstr_mant.productores)
END IF

CHOOSE CASE li_tabpage
	CASE 1 
		IF Not ib_salida THEN

 			istr_mant.argumento[9]   =	String(dw_2.Object.mfco_tipdoc[1])	
			istr_mant.argumento[10]  = String(dw_2.Object.mfco_docrel[1])
			istr_mant.argumento[20]  =	String(dw_2.Object.tipo_ingdat[1])
			
			istr_mant.dw	=	dw_3

			IF gstr_paramplanta.binsabins THEN
				OpenWithParm(iw_mantencion_1, istr_mant)
			ELSE
				OpenWithParm(iw_mantencion_3, istr_mant)
			END IF
		ELSE
			RETURN
		END IF
		
	CASE 2
		IF Not ib_frigo THEN
			lstr_mant.dw	=	dw_5
			SetNull(lstr_mant.dw2)
			lstr_mant.Argumento[4]	=	'2'
			OpenWithParm(iw_mantencion_2, lstr_mant)
		ELSE
			RETURN
		END IF
		
	CASE 3
		IF Not ib_Salida THEN
			lstr_mant.dw	=	dw_7
			SetNull(lstr_mant.dw2)
			lstr_mant.Argumento[4]	=	'1'
			OpenWithParm(iw_mantencion_2, lstr_mant)
		ELSE
			RETURN
		END IF
		
END CHOOSE

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	CHOOSE CASE li_tabpage
		CASE 1
			IF dw_3.RowCount()>0 THEN li_borra	=	dw_3.DeleteRow(0)

		CASE 2
			IF dw_5.RowCount()>0 THEN	li_borra	=	dw_5.DeleteRow(0)
		CASE 3
			IF dw_7.RowCount()>0 THEN li_borra	=	dw_7.DeleteRow(0)
	END CHOOSE
 
	IF li_borra = 1 THEN
		
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	//IF dw_3.RowCount() = 0 THEN pb_eli_det.Enabled = False
END IF

istr_mant.borra	 = False

captura_totales()
end event

event ue_borrar;Integer	li_Cliente
IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

IF NOT valida_password() THEN
	MessageBox("Error", "No es posible anular el movimiento, ya que no posee el password correspondiente")
	Return
END IF

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)
IF dw_7.RowCount() > 0 THEN dw_7.RowsMove(1,dw_7.RowCount(),Primary!,dw_7,1,Delete!)
IF dw_5.RowCount() > 0 THEN dw_5.RowsMove(1,dw_5.RowCount(),Primary!,dw_5,1,Delete!)

dw_2.ResetUpdate()
dw_2.SetItem(1,"mfco_estmov",0)

	IF wf_actualiza_db(True) THEN
		IF dw_2.Object.mfco_retenv[1] = 1 THEN 
			IF gi_admenvase <> 1 THEN
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
						dw_exismovtodetanulos.SetTransObject(sqlexi)
						dw_exidetaborra.SetTransObject(sqlexi)
						TriggerEvent("ue_despuesborrar")
						Disconnect Using sqlexi;
					ELSE
						MessageBox("Atención", "No puede Despachar.~r~r" + &
									"Falló Conexion con Existencia.", Exclamation!, Ok!)
			
						RETURN 
					END IF
				END IF	
			END IF	
		END IF
		w_main.SetMicroHelp("Registro Borrado...")
		This.TriggerEvent("ue_nuevo")
		SetPointer(Arrow!)
	ELSE
		w_main.SetMicroHelp("Registro no Borrado...")
	END IF			
end event

event ue_guardar;Integer	li_cliente
String	ls_numero
Date		ld_fecha

IF dw_3.AcceptText() = -1 THEN RETURN
IF dw_7.AcceptText() = -1 THEN RETURN
IF dw_5.AcceptText() = -1 THEN RETURN
IF dw_1.AcceptText() = -1 THEN RETURN

ls_numero = String(dw_2.Object.mfco_numero[1])
ld_fecha = dw_2.Object.mfco_fecmov[1]

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
	
	IF dw_2.Object.mfco_retenv[1] = 1 THEN
	
		IF gi_admenvase <> 1 THEN
		
			li_Cliente = dw_2.Object.clie_codigo[1]
			
			SELECT clie_conexi, cone_codigo
			INTO  :il_conexiste, :il_coneccion
			FROM dba.clientesprod
			WHERE clie_codigo = :li_Cliente;
			
			IF il_conexiste = 1 THEN
				sqlexi	=	CREATE Transaction
				iuo_grabatablabitacora			=	Create uo_grabatablabitacora
				Datos_correo()
				IF Conexionexistencia() THEN
					dw_exideta.SetTransObject(sqlexi)
					dw_exiencab.SetTransObject(sqlexi)	
					dw_exismovtodetanulos.SetTransObject(sqlexi)
					dw_exidetaborra.SetTransObject(sqlexi)
				//	IF dw_2.Object.mfco_estmov[1] = 2 THEN
					TriggerEvent("ue_despuesborrar")		
					TriggerEvent("ue_despuesguardar")
					
					IF li_retorno = 2 THEN
						iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
							'Verificar Datos en Base de Existencia '+is_base,is_base,1,is_error,1,il_coderror,&
						is_correo,'Problema Control de Envases Despacho Venta Directa Comercial','Movimiento N° '+ls_numero+' Con problemas, Error '+is_error)
						is_error = ''	
					END IF		
				//	END IF	
				ELSE
					MessageBox("Atención", "No puede Despachar.~r~r" + &
								"Fallo Conexion con Existencia.", Exclamation!, Ok!)
					
					iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
					'Verificar Conexión a Base '+is_base,is_base,1,'No Se Pudo Conectar el Sistema a Base de Datos de Existencia',1,il_coderror,&
					is_correo,'Problema Control de Envases Despacho Venta Directa Comercial','Movimiento N° '+ls_numero+' Con problemas, Error en Conexión con Base de Existencia')
					MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)
		
					RETURN 
				END IF
			END IF	
		END IF	
	END IF	
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF

IF il_NumFruta > 0 AND dw_2.GetItemStatus(1, 0, Primary!) = NotModified! THEN
	il_NumFruta = 0
	il_NumEnva	= 0
END IF

IF dw_2.Object.mfco_estmov[1]	=	3 THEN
	cb_guia.Enabled	=	True
END IF
		
end event

event ue_modifica_detalle;Integer	li_tabpage
Boolean	lb_estado

str_mant_envases	lstr_mant

li_tabpage	=	tab_1.SelectedTab

istr_mant.agrega	= False
istr_mant.borra	= False

lb_estado			=	istr_mant.Solo_Consulta

If li_tabpage <> 1 Then
	lstr_mant.argumento				=	istr_mant.argumento
	lstr_mant.dw						=	istr_mant.dw
	lstr_mant.dw2						=	istr_mant.dw2
	lstr_mant.Agrega					=	istr_mant.Agrega
	lstr_mant.Borra						=	istr_mant.Borra
	lstr_mant.Solo_Consulta			=	istr_mant.Solo_Consulta
	lstr_mant.respuesta				=	istr_mant.respuesta
	lstr_mant.tipo						=	istr_mant.tipo
	lstr_mant.usuariosoloconsulta	=	istr_mant.usuariosoloconsulta
	
	//ProductoresLotes(lstr_mant.productores)
End If

Choose Case li_tabpage
	Case 1
		If ib_salida Then
  			istr_mant.Solo_Consulta	=	True
		End If
		
		If dw_3.RowCount() > 0 Then
			
			istr_mant.argumento[9]   	= 	String(dw_2.Object.mfco_tipdoc[1])	
			istr_mant.argumento[10] 	= 	String(dw_2.Object.mfco_docrel[1])
			istr_mant.Argumento[12]	=	String(dw_2.Object.mfco_tipval[1])
			istr_mant.argumento[13]  	= 	String(dw_2.Object.mfco_fecmov[1])
			istr_mant.argumento[20]  	= 	String(dw_2.Object.tipo_ingdat[1])
			istr_mant.dw					=	dw_3

			If gstr_paramplanta.binsabins Then
				OpenWithParm(iw_mantencion_1, istr_mant)
			Else
				OpenWithParm(iw_mantencion_3, istr_mant)
			End If
			
		End If
		
		istr_mant.Solo_Consulta	=	lb_estado

	Case 2
		
		If ib_Frigo Then
			lstr_mant.Solo_Consulta	=	True
		End If
		
		If dw_5.RowCount() > 0 Then
			lstr_mant.dw	=	dw_5
			SetNull(lstr_mant.dw2)
			lstr_mant.Argumento[4]	=	'2'
			OpenWithParm(iw_mantencion_2, lstr_mant)
		End If
		lstr_mant.Solo_Consulta	=	lb_estado
		
	Case 3	
		
		If ib_Salida Then
			lstr_mant.Solo_Consulta	=	True
		End If
		
		If dw_7.RowCount() > 0 Then
			lstr_mant.dw	=	dw_7
			SetNull(lstr_mant.dw2)
			lstr_mant.Argumento[4]	=	'1'
			OpenWithParm(iw_mantencion_2, lstr_mant)
		End If
		
End Choose

captura_totales()
end event

event ue_nuevo;Long		ll_modif

is_rut		=	''
is_rutprod	=	''

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif	=	dw_1.GetNextModified(0, Primary!)
			ll_modif	+=	dw_2.GetNextModified(0, Primary!)
			ll_modif	+=	dw_3.GetNextModified(0, Primary!)
			ll_modif	+=	dw_5.GetNextModified(0, Primary!)
			ll_modif	+=	dw_7.GetNextModified(0, Primary!)
		
			IF dw_1.RowCount() > 0 and ll_modif > 0 THEN
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

dw_1.Reset()
dw_3.Reset()
dw_5.Reset()
dw_7.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled		=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()

dw_2.Object.plde_codigo[1]		=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.clie_codigo[1]		=	gi_CodExport
dw_2.Object.tpmv_codigo[1]	=	integer(istr_mant.argumento[2])
dw_2.Object.mfco_estmov[1]	=	integer(istr_mant.argumento[6])
dw_2.Object.mfco_tipdoc[1] 	= 	integer(istr_mant.argumento[9])

idt_FechaSistema	=	F_FechaHora()

dw_2.Object.mfco_fecmov[1]		=	Date(Mid(String(idt_FechaSistema),1,10))
dw_2.Object.mfco_horaen[1]		=	Time(Mid(String(idt_FechaSistema),12,8))

dw_2.Object.tipo_ingdat[1]		=	1

dw_2.Object.destare.Visible		=	False
dw_2.Object.destare.Text		=	'Frigorífico'
cb_guia.Enabled					=	False

ib_Salida								=	False
ib_Destare							=	False
ib_Frigo								=	False
il_NumFruta							=	0
il_NumEnva							=	0

istr_Mant.Argumento[3]			=	''
istr_mant.Argumento[7]			=	'0'

tab_1.tp_2.Enabled				=	False
tab_1.tp_3.Enabled				=	False

pb_ins_det.Enabled				=	False

tab_1.SelectTab(1)

HabilitaEncab(True)		

is_rut 								= ""
dw_2.Object.clpr_rut[1] 		= ''
dw_2.Object.clpr_rut.Format 	= '@@@@@@@@@@'
end event

event ue_nuevo_detalle;Integer	li_tabpage
Boolean	lb_estado

str_mant_envases	lstr_mant

li_tabpage			=	tab_1.SelectedTab

istr_mant.Borra	=	False
istr_mant.Agrega	=	True
	
lb_estado			=	istr_mant.Solo_Consulta

IF li_tabpage <> 1 THEN
	
	lstr_mant.argumento				=	istr_mant.argumento
	lstr_mant.dw						=	istr_mant.dw
	lstr_mant.dw2						=	istr_mant.dw2
	lstr_mant.Agrega					=	istr_mant.Agrega
	lstr_mant.Borra						=	istr_mant.Borra
	lstr_mant.Solo_Consulta			=	istr_mant.Solo_Consulta
	lstr_mant.respuesta				=	istr_mant.respuesta
	lstr_mant.tipo						=	istr_mant.tipo
	lstr_mant.usuariosoloconsulta	=	istr_mant.usuariosoloconsulta
	
END IF

CHOOSE CASE li_tabpage
	CASE 1
		
		If dw_2.Object.plde_codigo[1] = 41 Then
			If dw_3.RowCount() > 20 Then
				MessageBox('Atencion', 'No se puede ingresar mas de 20 registros', Exclamation!, OK!)
				Return
			End If
		End If
		
		IF ib_Destare OR ib_Salida THEN
			istr_mant.Solo_Consulta	=	True
		END IF

		istr_mant.argumento[9]   	= 	String(dw_2.Object.mfco_tipdoc[1])	
		istr_mant.argumento[10]  	= 	String(dw_2.Object.mfco_docrel[1])
		istr_Mant.Argumento[11]	=	'venta'
		istr_mant.Argumento[12]	=	String(dw_2.Object.mfco_tipval[1])
		istr_mant.argumento[13]  	= 	String(dw_2.Object.mfco_fecmov[1])
		istr_mant.argumento[20]  	= 	String(dw_2.Object.tipo_ingdat[1])
		istr_mant.argumento[16]  	= 	String(dw_2.Object.clie_codigo[1])
		
		istr_mant.dw	=	dw_3

		IF gstr_paramplanta.binsabins THEN
			OpenWithParm(iw_mantencion_1, istr_mant)
		ELSE
			OpenWithParm(iw_mantencion_3, istr_mant)
		END IF
		
		istr_mant.Solo_Consulta	=	lb_estado
		captura_entrada()

	CASE 2
		
		IF Not ib_Destare THEN
			lstr_mant.dw	=	dw_5
			SetNull(lstr_mant.dw2)
			lstr_mant.Argumento[4]	=	'2'
			OpenWithParm(iw_mantencion_2, lstr_mant)
		END IF	
		lstr_mant.Solo_Consulta	=	lb_estado

	CASE 3
		
		IF ib_Destare Or ib_Salida THEN
			lstr_mant.Solo_Consulta	=	True
		END IF
		
		lstr_mant.dw	=	dw_7
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'1'
		OpenWithParm(iw_mantencion_2, lstr_mant)
		
END CHOOSE

IF dw_3.RowCount() > 0 AND dw_5.RowCount() > 0 and NOT ib_Salida THEN HabilitaEncab(False)

IF dw_3.RowCount() > 0 AND dw_5.RowCount() > 0 AND Not pb_eliminar.Enabled AND NOT ib_Salida THEN
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
	pb_eli_det.Enabled	=	True
END IF

CHOOSE CASE li_tabpage 
	CASE 1
		istr_mant	=	Message.PowerObjectParm
		
		dw_3.SetRow(il_Fila)
		dw_3.SelectRow(il_Fila, True)
	CASE 2
		
		dw_7.SetRow(il_Fila)
		dw_7.SelectRow(il_Fila, True)
	CASE 3
		
		dw_5.SetRow(il_Fila)
		dw_5.SelectRow(il_Fila, True)
END CHOOSE

captura_totales()
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta, ll_fila_env

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[01]),Integer(istr_mant.argumento[02]),&
										    Long(istr_mant.argumento[03]),Integer(istr_Mant.Argumento[30]))

	If ll_fila_e = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ElseIf ll_fila_e > 0 Then
		
		tab_1.tp_2.Enabled		=	True
		tab_1.tp_1.Enabled		=	True
		cb_guia.Enabled			=	False
		
		If dw_2.Object.mfco_estmov[1]	=	1 AND dw_2.Object.mfco_pcopda[1] = 1 Then
			dw_2.Object.destare.Visible		=	True
			tab_1.tp_1.Enabled				=	True
			cb_guia.Enabled					=	False
		End If
		
		If dw_2.Object.mfco_estmov[1]	=	2 Then
			tab_1.tp_1.Enabled				=	True
			tab_1.tp_3.Enabled				=	True
			tab_1.tp_3.Visible					=	True
			ib_Frigo								=	True
		  	dw_2.Object.destare.Text		=	'Salida'
			dw_2.Object.destare.Visible		=	True
   			istr_mant.Solo_Consulta			=	False
			cb_guia.Enabled					=	False
		End If

		If dw_2.Object.mfco_estmov[1]	=	3 Then
			tab_1.tp_1.Enabled				=	True
			tab_1.tp_3.Enabled				=	True
			tab_1.tp_3.Visible					=	True
			istr_mant.Solo_Consulta			=	False
			cb_guia.Enabled					=	True
		End If

		If dw_2.Object.mfco_estmov[1]	=	4 Then
			tab_1.tp_1.Enabled				=	True
			tab_1.tp_3.Enabled				=	True
			tab_1.tp_3.Visible					=	True
			//istr_mant.Solo_Consulta			=	True
			cb_guia.Enabled					=	True//False
		End If
		
		If dw_2.Object.mfco_guiemi[1]	=	0 Or dw_2.Object.mfco_guiemi[1]	=	2 Then
			cb_guia.Enabled					=	True
		ElseIf dw_2.Object.mfco_guiemi[1] =	1 Then
			istr_mant.Solo_Consulta			=	True
			cb_guia.Enabled					=	False
		Else
			cb_guia.Enabled					=	False
		End If


		iuo_Camion.Existe(1, dw_2.Object.cami_patent[1], False, sqlca)

		If dw_2.Object.mfco_estmov[1] = 1 AND dw_2.Object.mfco_pcopda[1] = 2 Then
			HabilitaEncab(True)
			istr_mant.Solo_Consulta			=	False
		Else
			HabilitaEncab(False)
		End If

		DO
			If dw_1.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]), Long(istr_mant.argumento[3])) = -1 OR &								  
				dw_3.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]), Long(istr_mant.argumento[3]), Integer(istr_Mant.Argumento[30])) = -1 OR &								  
				dw_5.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]), Long(istr_mant.argumento[3]),1, Integer(istr_Mant.Argumento[30])) = -1 OR &								  
				dw_7.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]), Long(istr_mant.argumento[3]),2, Integer(istr_Mant.Argumento[30])) = -1  Then
	
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.",  Information!, RetryCancel!)

			Else
				If dw_2.Object.mfco_estmov[1] = 2 Then	HabilitaFrigorIfico()

				//Para determinar Fuera de Rango
				//If dw_2.Object.mfco_estmov[1] = 3 Then Destare(False)
				//Comentado, ya que el estado primario de la venta es 3 y no se realizara destare.
				
				pb_imprimir.Enabled	= 	True
				pb_eliminar.Enabled  = 	Not istr_mant.Solo_Consulta
				pb_grabar.Enabled	= 	Not istr_mant.Solo_Consulta

				If dw_2.Object.mfco_estmov[1]	=	3 Then
					pb_grabar.Enabled	= 	TRUE
				End If

				pb_ins_det.Enabled	= 	Not istr_mant.Solo_Consulta
				pb_eli_det.Enabled	= 	Not istr_mant.Solo_Consulta
				dw_3.SetRow(1)
				dw_3.SelectRow(1,True)
				dw_3.SetFocus()
					
			End If
		LOOP WHILE respuesta = 1

		If respuesta = 2 Then Close(This)
	End If
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

If respuesta = 2 Then Close(This)

captura_entrada()
end event

event ue_antesguardar;Long			ll_Fila, ll_Fila_d, ll_Total_Bultos, ll_Productor, &
				ll_Envases_Fruta,	ll_Filas, ll_Lote, ll_Fila_Busca, ll_row
Integer		li_Secuencia, li_Planta, li_TipoMovto, &
				li_Lote_pltcod, li_Lote_espcod, li_Lote_codigo,&
				li_TipoEnvase, li_Envase, li_Lote_Ant, li_camara, li_TipoMovtoEnva, li_garantia
Decimal{3}	ld_Total_PesoEnv_Sal, ld_KBS_Camion, ld_KBS_Carro
Date			ld_Fecha
Time			lt_Hora
Boolean 		lb_Actualiza_Envase = False, lb_Actualiza_Fruta = False

Message.DoubleParm = 0

li_Planta				=	Integer(istr_mant.Argumento[1])
li_TipoMovto		=	Integer(istr_mant.Argumento[2])
li_TipoMovtoEnva  =  64//Despacho de envases
li_garantia       		=  dw_2.Object.garantia[1]

If ib_Frigo Then dw_2.Object.mfco_estmov[1] = 2

If dw_2.Object.mfco_estmov[1] = 1 AND dw_2.Object.mfco_pcopda[1] = 2 Then
	dw_2.Object.mfco_estmov[1] = 2
End If

If ib_frigo Then
	If dw_3.RowCount() > 0 Then
		ll_Total_Bultos	=	dw_3.Object.total_bultos[dw_3.RowCount()]
	Else
		ll_Total_Bultos	= 0
	End If
	
	If dw_7.RowCount() > 0 Then
		ll_Envases_Fruta	=	dw_7.Object.tot_bultos_fruta[dw_7.RowCount()]
	Else
		ll_Envases_Fruta	= 0
	End If
	
	If ll_Total_Bultos<>ll_envases_Fruta and li_TipoMovto=32 and li_garantia=0 and dw_2.Object.mfco_estmov[1] = 3 Then
		MessageBox("Atención","Cantidad de Bultos del Detalle Fruta son Distintos a los de Envases Retirados.")
		Message.DoubleParm = -1
		RETURN
	End If

End If

If ib_Destare Then
	If IsNull(dw_2.Object.mfco_tkbsal[1]) OR dw_2.Object.mfco_tkbsal[1] = 0 Then
		MessageBox("Atención","No se ha registrado la Tara de Salida del Camión.")
		Message.DoubleParm = -1
		RETURN
	End If

	ll_Fila	=	dw_7.RowCount()

	If ll_Fila > 0 Then
		dw_7.GroupCalc()
		ld_Total_PesoEnv_Sal	=	dw_7.Object.total_pesoenv[ll_Fila]
	End If

	ld_KBS_Camion	=	dw_2.Object.mfco_tkbsal[1]
	ld_KBS_Carro	=	dw_2.Object.mfco_tkbsac[1]

	If ld_KBS_Camion + ld_KBS_Carro > 0 AND &
		ld_KBS_Camion + ld_KBS_Carro < ld_Total_PesoEnv_Sal Then
		MessageBox("Atención","Kilos de Salida no pueden ser menor a los Envases de Salida")
		Message.DoubleParm = -1
		RETURN
	End If
	
	If ib_Destare Then
		//Pasa a Definitiva
		dw_2.Object.mfco_estmov[1]	=	3
		cb_guia.Enabled				=	True	
	End If
	
	lt_Hora								=	dw_2.Object.mfco_horasa[1]
	ld_Fecha								=	Date(dw_2.Object.fecha_sal[1])
	dw_2.Object.mfco_horasa[1]	=	lt_Hora
End If

ib_AutoCommit		=	SQLCA.AutoCommit
SQLCA.AutoCommit	=	False

If (istr_mant.Argumento[9] = '9') And Not IsNull(dw_2.Object.mfco_guisii[1]) And  dw_2.Object.mfco_guisii[1] > 0 Then
	dw_2.Object.mfco_estmov[1] = 4
	istr_mant.Solo_Consulta	=	True
End If

If dw_2.GetItemStatus(1, 0, Primary!) = NewModIfied! Then

		If il_NumEnva=0 Then
		
		   iuo_TipoMovtoEnva.bloqueacorrel()	
		   il_NumEnva = iuo_TipoMovtoEnva.UltimoCorrelativo(5,li_TipoMovtoEnva,li_Planta) 
	
		   If il_NumEnva = 0 OR IsNull(il_NumEnva) Then
			  Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
			  Message.DoubleParm = -1
		     RETURN
		   Else
			  lb_Actualiza_Envase = TRUE
		   End If
	    End If
	
	   If il_NumFruta=0 Then
		  iuo_TipoMovtoFruta.bloqueacorrel()
		  il_NumFruta = iuo_TipoMovtoFruta.UltimoCorrelativo(2,li_TipoMovto,li_Planta) 
	
		  If il_NumFruta = 0 OR IsNull(il_NumFruta) Then
			 Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
			 Message.DoubleParm = -1
			 RETURN
		  Else
			 lb_Actualiza_Fruta = TRUE	
		  End If
      End If
	
	dw_2.Object.mfco_numero[1]	=	il_NumFruta
   
	istr_mant.Argumento[3] 			=	String(il_NumFruta)
		
	lt_Hora								=	dw_2.Object.mfco_horaen[1]
	ld_Fecha								=	Date(dw_2.Object.mfco_fecmov[1])
	dw_2.Object.mfco_horaen[1]	=	lt_Hora
	
	//Estado para retiro clientes, lotes a servicio
	If li_TipoMovto = 35 Then dw_2.Object.mfco_estret[1]	=	1
	
	ll_Fila	=	dw_1.InsertRow(0)
	dw_1.Object.plde_codigo[ll_Fila]		=	li_Planta
	dw_1.Object.tpmv_codigo[ll_Fila]		=	li_TipoMovtoEnva   //Movimiento de Despacho Envase Comercial
	dw_1.Object.meen_numero[ll_Fila]	=	il_NumEnva
	dw_1.Object.tpmv_codrec[ll_Fila]		=	dw_2.Object.tpmv_codigo[1]
	dw_1.Object.mfge_numero[ll_Fila]	=	il_NumFruta
	dw_1.Object.meen_guisii[ll_Fila]		=	0
	dw_1.Object.meen_modulo[ll_Fila]	=	2
	dw_1.Object.meen_fecmov[ll_Fila]	=	dw_2.Object.mfco_fecmov[1]
	dw_1.Object.tran_codigo[ll_Fila]		=	dw_2.Object.tran_codigo[1]
	dw_1.Object.cami_clasIfi[ll_Fila]		=	dw_2.Object.cami_clasIfi[1]
	dw_1.Object.cami_patent[ll_Fila]		=	dw_2.Object.cami_patent[1]
	dw_1.Object.cami_patcar[ll_Fila]		=	dw_2.Object.cami_patcar[1]
	dw_1.Object.meen_rutcho[ll_Fila]		=	dw_2.Object.mfco_rutcho[1]
	dw_1.Object.meen_chofer[ll_Fila]		=	dw_2.Object.mfco_chofer[1]
	dw_1.Object.clpr_rut[ll_Fila]			=	dw_2.Object.clpr_rut[1]
	dw_1.Object.clie_codigo[ll_Fila]		=	dw_2.Object.clie_codigo[1]
	
 	//Preguntar el Momento de Actualización
	If lb_Actualiza_Fruta  Then iuo_TipoMovtoFruta.Actualiza_Correlativo(2,li_Planta,li_TipoMovto,il_NumFruta) 
  	If lb_Actualiza_Envase Then iuo_TipoMovtoEnva.Actualiza_Correlativo(4,li_Planta,li_TipoMovtoEnva,il_NumEnva) 
   ///////////////////////////////////////
	
Else
	il_NumFruta									=	dw_2.Object.mfco_numero[1]
	istr_mant.Argumento[3] 					=	String(dw_2.Object.mfco_numero[1])
End If


FOR ll_row=1 TO dw_3.RowCount()
	If dw_3.GetItemStatus(ll_row, 0, Primary!) = NewModIfied! Then
		dw_3.Object.tpmv_codigo[ll_row]		=	dw_2.Object.tpmv_codigo[1]
   	dw_3.Object.mfco_numero[ll_row]		=	il_NumFruta
		dw_3.Object.mfcd_secuen[ll_row]		= 	ll_row
		dw_3.Object.clie_codigo[ll_row]		=	dw_2.Object.clie_codigo[1]
	End If
NEXT	

FOR ll_Fila = 1 TO dw_5.RowCount()
	If dw_5.GetItemStatus(ll_Fila, 0, Primary!) = NewModIfied! Then
		dw_5.Object.plde_codigo[ll_Fila]		=	dw_1.Object.plde_codigo[1]
		dw_5.Object.tpmv_codigo[ll_Fila]		=	dw_1.Object.tpmv_codigo[1]
		dw_5.Object.meen_numero[ll_Fila]	=	dw_1.Object.meen_numero[1]

		dw_5.Object.fgme_sentid[ll_fila] 	=  1
		dw_5.Object.clie_codigo[ll_fila]		=	dw_2.Object.clie_codigo[1]
	End If
NEXT

If integer(istr_mant.argumento[2])=35 Then
	FOR ll_Fila=1 To dw_5.RowCount()
		If dw_5.GetItemStatus(ll_Fila, 0, Primary!) = NewModIfied! Then
			
			ll_Fila_d = dw_7.InsertRow(0)
			dw_7.Object.plde_codigo[ll_Fila_d]		=	dw_5.Object.plde_codigo[ll_Fila]
			dw_7.Object.tpmv_codigo[ll_Fila_d]		=	dw_5.Object.tpmv_codigo[ll_Fila]
			dw_7.Object.meen_numero[ll_Fila_d]	=	dw_5.Object.meen_numero[ll_Fila]
			dw_7.Object.fgme_conenv[ll_Fila_d]  	=  1
			dw_7.Object.enva_tipoen[ll_Fila_d]		=  dw_5.Object.enva_tipoen[ll_Fila]
			dw_7.Object.enva_codigo[ll_Fila_d]		=  dw_5.Object.enva_codigo[ll_Fila]
         	dw_7.Object.cale_calida[ll_Fila_d]			=  dw_5.Object.cale_calida[ll_Fila]
			dw_7.Object.fgme_sentid[ll_fila_d]	  	=  2
			dw_7.Object.fgme_cantid[ll_Fila_d]		=  dw_5.Object.fgme_cantid[ll_Fila]
			dw_7.Object.fgme_pesone[ll_Fila_d]		=  dw_5.Object.fgme_pesone[ll_Fila]
			dw_7.Object.clie_codigo[ll_Fila_d]			=	dw_2.Object.clie_codigo[1]
		
	   ElseIf dw_5.GetItemStatus(ll_Fila, 0, Primary!) = DataModIfied! Then

			ll_Fila_d	=	dw_7.Find("enva_tipoen = " + String(dw_5.Object.enva_tipoen[ll_Fila]) + &
									 	 " AND enva_codigo = " + String(dw_5.Object.enva_codigo[ll_Fila]) + &
										 " AND cale_calida = " + String(dw_5.Object.cale_calida[ll_Fila]), + & 
								       1, dw_7.RowCount())

			dw_7.Object.fgme_cantid[ll_Fila_d]		=	dw_5.Object.fgme_cantid[ll_fila]
			dw_7.Object.fgme_pesone[ll_Fila_d]		=	dw_5.Object.fgme_pesone[ll_fila]
			dw_7.Object.clie_codigo[ll_Fila_d]			=	dw_2.Object.clie_codigo[1]
		End If
	NEXT
Else
	FOR ll_Fila = 1 TO dw_7.RowCount()
	If dw_7.GetItemStatus(ll_Fila, 0, Primary!) = NewModIfied! Then
		
		dw_7.Object.plde_codigo[ll_Fila]		=	dw_1.Object.plde_codigo[1]
		dw_7.Object.tpmv_codigo[ll_Fila]		=	dw_1.Object.tpmv_codigo[1]
		dw_7.Object.meen_numero[ll_Fila]	=	dw_1.Object.meen_numero[1]
		dw_7.Object.clie_codigo[ll_Fila]		=	dw_2.Object.clie_codigo[1]
		dw_7.Object.fgme_sentid[ll_fila] 		=  2
	End If
NEXT

//Captura_totales()
Captura_Entrada()
End If	
end event

event ue_seleccion;IF ib_ok = False THEN RETURN

Str_Busqueda	lstr_busq

lstr_busq.argum[1] = String(gstr_param.plde_codigo)
lstr_busq.argum[2] = istr_mant.argumento[2]         	// Movimiento de Recepción
lstr_busq.argum[3] = ''                           			 	// Cualquier estado
lstr_busq.argum[4] = String(idt_FechaSistema)       	// Desde Fecha de Inicio 
lstr_busq.argum[5] = istr_mant.argumento[9]

OpenWithParm(w_busc_movtofrutacomenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	
	pb_eli_det.Enabled		=	False
	pb_ins_det.Enabled		=	False
	pb_grabar.Enabled		=	False
	pb_eliminar.Enabled		=	False
	pb_imprimir.Enabled		=	False
	dw_2.Enabled				=	True

	dw_2.SetFocus()

	idt_FechaSistema	=	F_FechaHora()

	dw_2.Object.destare.Visible	=	False

	dw_2.Object.destare.Text		=	'Frigorífico'
	cb_guia.Enabled					=	False
   
	ib_Salida								=	False
	ib_Destare							=	False
	ib_Frigo								=	False

	istr_Mant.Argumento[3]			=	''
	istr_mant.Argumento[7]			=	'0'

	IF integer(istr_mant.argumento[2]) = 35 THEN tab_1.tp_3.visible = False

	tab_1.SelectTab(1)
	istr_mant.argumento[3]	=	lstr_busq.argum[3]
	This.TriggerEvent("ue_recuperadatos")
END IF
end event

event ue_imprimir;Long	ll_modIf

ib_ok	=	True

If Not istr_mant.Solo_Consulta Then
	CHOOSE CASE wf_modIfica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modIf	=	dw_1.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_2.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_3.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_5.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_7.GetNextModIfied(0, Primary!)
		
			If dw_3.RowCount() > 0 and ll_modIf > 0 Then
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información antes de Imprimir ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						If message.DoubleParm = -1 Then ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				End CHOOSE
			End If
	End CHOOSE
End If

If Not ib_ok Then RETURN

SetPointer(HourGlass!)

Long		fila
Integer  li_estado, li_Kilos

istr_info.titulo	= "GUIA DE DESPACHO A VENTA FRUTA COMERCIAL"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

li_estado	=	dw_2.object.mfco_estmov[1]

If li_estado = 1 Then
   vinf.dw_1.DataObject = "dw_info_guia_venta_transitoria"
	Return
Else
	vinf.dw_1.DataObject = "dw_info_guia_venta_Definitiva"
End If

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), Integer(istr_mant.Argumento[2]),&
								  Long(istr_mant.Argumento[3]), li_estado, gstr_paramplanta.porcentajeiva)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

event closequery;IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			Message.ReturnValue = 1 
		CASE 0
			IF dw_3.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.triggerevent("ue_guardar")
						IF message.doubleparm = -1 THEN Message.ReturnValue = 1
						RETURN
					CASE 3
						Message.ReturnValue = 1
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF
end event

event ue_validaborrar;IF MessageBox("Anular Movimiento","Desea Anular el Movimiento ?", Question!, YesNo!) = 1 THEN
	Message.DoubleParm = 1
ELSE
	Message.DoubleParm = -1
END IF

RETURN





end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	pb_Eliminar.Enabled	=	True
	
	IF istr_mant.Solo_Consulta THEN
		dw_2.Enabled			=	False
//		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled		=	False
		pb_ins_det.Enabled	=	False
		pb_eli_det.Enabled	=	False
	ELSE
		dw_2.Enabled			=	True
		pb_Eliminar.Enabled	=	True
		pb_Grabar.Enabled		=	True
		pb_ins_det.Enabled	=	True
		pb_eli_det.Enabled	=	True
	END IF
ELSE
	IF istr_mant.Solo_Consulta THEN
		pb_ins_det.Enabled	=	False
	ELSE
		pb_ins_det.Enabled	=	True
	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

event resize;call super::resize;Tab_1.x			=	dw_1.x
Tab_1.y			=	dw_1.y
Tab_1.Width	=	dw_1.Width
Tab_1.Height	=	dw_1.Height

Tab_1.Tp_1.dw_detafruta.Width		= dw_1.Width - 120
Tab_1.Tp_1.dw_detafruta.Height		= dw_1.Height - 200

Tab_1.Tp_2.dw_envrec.Width	= dw_1.Width - 120
Tab_1.Tp_2.dw_envrec.Height	= dw_1.Height - 200

Tab_1.Tp_3.dw_envdes.Width	= dw_1.Width - 120
Tab_1.Tp_3.dw_envdes.Height	= dw_1.Height - 200

cb_guia.x				=	pb_Salir.x
cb_guia.y				=	pb_Salir.y + pb_Salir.Height

pb_eli_det.y				=	dw_1.y + dw_1.Height - 255
pb_ins_det.y			=	pb_eli_det.y - 255

end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutacomercial
boolean visible = false
integer x = 105
integer y = 2144
integer width = 3054
integer height = 920
string title = "Detalle de Movimiento de Envase"
string dataobject = "dw_mant_movtoenvaenca_comercial"
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutacomercial
integer x = 873
integer y = 28
integer width = 2939
integer height = 984
string dataobject = "dw_mant_movtofrutacomenca_venta"
boolean livescroll = true
end type

event dw_2::itemchanged;String		ls_Nula,	ls_Columna, ls_Dato
Date		ld_Fecha
Time		lt_Hora

ls_Columna = GetColumnName()
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
	CASE "clie_codigo"
		If Not iuo_cliente.existe(integer(data),True,Sqlca) Then
			This.Object.clie_codigo[1] = Integer(ls_Nula)
		Else
			istr_Mant.Argumento[30]	= Data
		End If
		
	CASE "mfco_numero"
		If NOT ExisteRecepcion(gstr_ParamPlanta.CodigoPlanta, Integer(istr_mant.argumento[2]), Integer(data)) Then
			This.SetItem(1,"mfco_numero",Long(ls_Nula))
			This.SetFocus()
			RETURN 1
		Else
			istr_mant.argumento[3] = Data
		End If
    
	CASE "mfco_fecmov"
		ld_Fecha	=	Date(Mid(data,1,10))
		
		If NOT iuo_FechaMovto.Valida_FechaMovto(ld_Fecha) Then
			This.SetItem(1,"mfco_fecmov",Date(ls_Nula))
			This.SetFocus()
			RETURN 1
		End If

   CASE "mfco_docrel"
		
		If Not existeorden(long(data),dw_2.Object.mfco_tipdoc[1]) Then
			This.SetItem(1, ls_Columna, long(ls_Nula))
			RETURN 1
		Else
			istr_mant.argumento[10] = Data
		End If
		

	CASE "tran_codigo"
		If Not iuo_Transport.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(1, ls_Columna, Integer(ls_Nula))
			RETURN 1
		End If

	CASE "cami_patent"
		If data <> "" AND Not iuo_Camion.Existe(1, Data, FALSE, sqlca) Then
					
			RETURN 0
		Else
			This.Object.cami_patcar[1]	=	iuo_Camion.PateCarro
			This.Object.mfco_rutcho[1]	=	iuo_Camion.RutChofer
			This.Object.mfco_chofer[1]	=	iuo_Camion.Chofer
			is_rut = iuo_Camion.RutChofer
		End If
		
	CASE "clpr_rut"
		is_rutclie = F_verrut(data, True)
		If is_rutclie = "" Then
			dw_2.SetItem(1, "clpr_rut", ls_Nula)
			RETURN 1
		Else
			If NOT iuo_clprv.existe(is_rutclie, true, sqlca) Then
				dw_2.SetItem(1, "clpr_rut", ls_Nula)
				RETURN 1
			Else
				dw_2.SetItem(1, "clpr_rut", is_rutclie)
				dw_2.SetItem(1, "clpr_nombre", iuo_clprv.RazonSocial)
			End If
		End If
		
	CASE "mfco_rutcho"
		is_rut = F_verrut(data, True)
		If is_rut = "" Then
			dw_2.SetItem(1, "mfco_rutcho", ls_Nula)
			RETURN 1
		Else
			dw_2.SetItem(1, "mfco_rutcho", is_rut)
		End If
		
	CASE "mfco_tkbsal", "mfco_tkbsac"
		captura_Totales()

	CASE "fecha_sal"
		ld_Fecha	=	Date(Mid(data,1,10))
		
		If NOT iuo_FechaMovto.Valida_FechaMovto(ld_Fecha) OR &
			ld_Fecha < This.Object.mfge_fecmov[row] Then
			MessageBox("Atención","Fecha de Salida no puede ser anterior a Fecha de Entrada")
			This.SetItem(1,"fecha_sal",SetNull(ld_Fecha))
			RETURN 1
		End If
		
	CASE "mfco_horasa"
		ld_Fecha	=	This.Object.fecha_sal[1]
	
		If Time(Mid(data,12,5)) < This.Object.refg_horaen[row] Then
			MessageBox("Atención","Hora de Salida no puede ser anterior a Hora de Entrada")
			This.SetItem(1,"refg_horasa",SetNull(lt_Hora))
			RETURN 1
		End If
	Case 'mfco_trasva'
		If Data = '0' Then This.SetItem(Row, 'mfco_cantra', Integer(ls_Nula))
		
End CHOOSE

HabilitaIngreso(ls_Columna)

If ib_Salida Then
	HabilitaGrabacion(ls_Columna)
End If
end event

event dw_2::buttonclicked;call super::buttonclicked;String	ls_Boton

ls_Boton = dwo.Name

CHOOSE CASE ls_Boton		
	CASE "buscacamion"
		buscacamion()
   		HabilitaIngreso('cami_patent')

	CASE "buscacliente"
		buscacliente()
   		HabilitaIngreso('clie_codigo')
	
	CASE "b_orden"
	 	buscaOrden()
		
	CASE "destare"
		If NOT ib_Frigo Then
         HabilitafrigorIfico()
		Else 
			If Not ib_Salida Then
				If dw_2.Object.mfco_estmov[1]=2 AND dw_2.Object.garantia[1]=0 AND dw_3.RowCount()>0 Then
					HabilitaSalida()
				   	//captura_totales()
					captura_entrada()
				ElseIf dw_2.Object.mfco_estmov[1]=2 AND dw_2.Object.garantia[1]=1 AND dw_3.RowCount()>0 Then
				      HabilitaSalida()
				      //captura_totales()
					captura_entrada()
				End If
			
			Else
				If dw_3.RowCount() >0  Then
				   If dw_7.RowCount() >0 Then
						Destare(True)
						//captura_totales()
						captura_entrada()
					Else
						MessageBox("Error de Datos","Falta Ingresos de Movimiento de Envases")
					End If	
				Else
					MessageBox("Error de Datos","Falta Ingresos de Detalle de Fruta.")
				End If
			End If
		End If
		
	CASE "camion"
		If not ib_Salida Then
			iuo_Camion.Existe(1, This.Object.cami_patent[1], FALSE, sqlca)
			If Isnull(iuo_Camion.TaraCamion) Then
				This.Object.mfco_tkbent[1]	=	0
			Else
				This.Object.mfco_tkbent[1]	=	iuo_Camion.TaraCamion
				//captura_totales()
				captura_entrada()
			End If
		End If
		
	CASE "carro"
		If not ib_Salida Then
			iuo_Camion.Existe(1, This.Object.cami_patent[1], FALSE, sqlca)
			If Isnull(iuo_Camion.TaraCarro) Then
				This.Object.mfco_tkbenc[1]	=	0
			Else
				This.Object.mfco_tkbenc[1]	=	iuo_Camion.TaraCarro
				//captura_totales()
				captura_entrada()
			End If
		End If		
End CHOOSE
end event

event dw_2::itemfocuschanged;call super::itemfocuschanged;//IF is_rut <> "" THEN
//	This.Object.mfco_rutcho.Format = '@@@.@@@.@@@-@'
//	
//	IF dwo.Name <> "mfco_rutcho" THEN
//		This.SetItem(1, "mfco_rutcho", is_rut)
//	END IF
//END IF

IF is_rutclie <> "" THEN
	This.Object.clpr_rut.Format = '@@@.@@@.@@@-@'
	
	IF dwo.Name <> "clpr_rut" THEN
		This.SetItem(1, "clpr_rut", is_rutclie)
	END IF
END IF
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutacomercial
integer x = 4741
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutacomercial
string tag = "Anula Movimiento"
integer x = 4741
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutacomercial
integer x = 4741
integer y = 632
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutacomercial
integer x = 4741
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutacomercial
integer x = 4741
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutacomercial
integer x = 4741
integer y = 1464
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutacomercial
integer x = 4741
integer y = 1636
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutacomercial
integer x = 4741
integer y = 96
end type

type cb_guia from commandbutton within w_maed_movtofrutacomercial
integer x = 4718
integer y = 1228
integer width = 302
integer height = 112
integer taborder = 70
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

event clicked;Str_mant lstr_mant

lstr_Mant.Argumento[1]	=	String(dw_2.Object.plde_codigo[1])
lstr_Mant.Argumento[2]	=	String(dw_2.Object.tpmv_codigo[1])
lstr_Mant.Argumento[3]	=	String(dw_2.Object.mfco_numero[1])
lstr_Mant.Argumento[5]	=  String(dw_2.Object.clie_codigo[1])
lstr_Mant.Argumento[6]	=	'3'

OpenWithParm(w_emis_guia_despacho_venta, lstr_Mant)

lstr_Mant = Message.PowerObjectParm

IF IsNull(lstr_Mant) THEN RETURN

IF lstr_Mant.Respuesta = 1 THEN
	istr_mant.solo_consulta = TRUE
	Parent.TriggerEvent("ue_recuperadatos")
END IF
end event

type tab_1 from tab within w_maed_movtofrutacomercial
integer x = 32
integer y = 1028
integer width = 4658
integer height = 944
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean fixedwidth = true
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
end type

on tab_1.create
this.tp_1=create tp_1
this.tp_2=create tp_2
this.tp_3=create tp_3
this.Control[]={this.tp_1,&
this.tp_2,&
this.tp_3}
end on

on tab_1.destroy
destroy(this.tp_1)
destroy(this.tp_2)
destroy(this.tp_3)
end on

type tp_1 from userobject within tab_1
string tag = "Detalle de Lotes Ingresados"
integer x = 18
integer y = 112
integer width = 4622
integer height = 816
long backcolor = 16777215
string text = "Detalle de Fruta"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "DatabaseProfile5!"
long picturemaskcolor = 536870912
dw_detafruta dw_detafruta
end type

on tp_1.create
this.dw_detafruta=create dw_detafruta
this.Control[]={this.dw_detafruta}
end on

on tp_1.destroy
destroy(this.dw_detafruta)
end on

type dw_detafruta from uo_dw within tp_1
integer x = 50
integer y = 36
integer width = 4553
integer height = 752
integer taborder = 10
string dataobject = "dw_mues_movtofrutacomdeta_ventas"
boolean hscrollbar = true
boolean livescroll = true
end type

event doubleclicked;call super::doubleclicked;IF dw_3.RowCount() > 0 THEN
   w_maed_movtofrutacomercial.TriggerEvent("ue_modifica_detalle")
END IF


RETURN 0
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutacomercial.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutacomercial.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

type tp_2 from userobject within tab_1
string tag = "Registro de Envases recepcionados con y sin fruta"
boolean visible = false
integer x = 18
integer y = 112
integer width = 4622
integer height = 816
long backcolor = 16777215
string text = "Envases Recibidos"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "CreateRuntime!"
long picturemaskcolor = 536870912
dw_envrec dw_envrec
end type

on tp_2.create
this.dw_envrec=create dw_envrec
this.Control[]={this.dw_envrec}
end on

on tp_2.destroy
destroy(this.dw_envrec)
end on

type dw_envrec from uo_dw within tp_2
integer x = 50
integer y = 40
integer width = 2615
integer height = 752
integer taborder = 11
string dataobject = "dw_mues_movtoenvadeta_recfruta_comer"
boolean hscrollbar = true
boolean livescroll = true
end type

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutacomercial.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event doubleclicked;call super::doubleclicked;IF dw_5.RowCount()>0 THEN
   w_maed_movtofrutacomercial.TriggerEvent("ue_modifica_detalle")
END IF

RETURN 0
end event

type tp_3 from userobject within tab_1
string tag = "Registro de Envases que retira para cosecha"
integer x = 18
integer y = 112
integer width = 4622
integer height = 816
boolean enabled = false
long backcolor = 16777215
string text = "Envases Retirados"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Inherit!"
long picturemaskcolor = 536870912
dw_envdes dw_envdes
end type

on tp_3.create
this.dw_envdes=create dw_envdes
this.Control[]={this.dw_envdes}
end on

on tp_3.destroy
destroy(this.dw_envdes)
end on

type dw_envdes from uo_dw within tp_3
integer x = 50
integer y = 40
integer width = 2615
integer height = 752
integer taborder = 21
string dataobject = "dw_mues_movtoenvadeta_recfruta_comer"
boolean hscrollbar = true
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutacomercial.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event doubleclicked;call super::doubleclicked;IF dw_7.RowCount()>0 THEN
   w_maed_movtofrutacomercial.TriggerEvent("ue_modifica_detalle")
END IF


RETURN 0
end event

type dw_exideta from datawindow within w_maed_movtofrutacomercial
boolean visible = false
integer x = 288
integer y = 2432
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

type dw_exiencab from datawindow within w_maed_movtofrutacomercial
boolean visible = false
integer x = 1193
integer y = 2200
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

type dw_exidetaborra from datawindow within w_maed_movtofrutacomercial
boolean visible = false
integer x = 3506
integer y = 2104
integer width = 686
integer height = 400
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta_borra"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exismovtodetanulos from datawindow within w_maed_movtofrutacomercial
boolean visible = false
integer x = 3255
integer y = 2492
integer width = 686
integer height = 400
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_inserta_exismovtodetanulos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

