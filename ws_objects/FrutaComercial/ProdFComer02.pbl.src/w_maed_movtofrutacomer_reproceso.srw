$PBExportHeader$w_maed_movtofrutacomer_reproceso.srw
$PBExportComments$Ventana de Recepción Fruta Comercial de Re-Proceso.
forward
global type w_maed_movtofrutacomer_reproceso from w_mant_encab_deta_csd
end type
type dw_3 from uo_dw within w_maed_movtofrutacomer_reproceso
end type
type dw_6 from datawindow within w_maed_movtofrutacomer_reproceso
end type
type dw_8 from datawindow within w_maed_movtofrutacomer_reproceso
end type
type tab_1 from tab within w_maed_movtofrutacomer_reproceso
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
type tab_1 from tab within w_maed_movtofrutacomer_reproceso
tp_1 tp_1
tp_2 tp_2
end type
type dw_exiencab from datawindow within w_maed_movtofrutacomer_reproceso
end type
type dw_exideta from datawindow within w_maed_movtofrutacomer_reproceso
end type
type dw_exidetaborra from datawindow within w_maed_movtofrutacomer_reproceso
end type
type dw_exismovtodetanulos from datawindow within w_maed_movtofrutacomer_reproceso
end type
type str_pesaje from structure within w_maed_movtofrutacomer_reproceso
end type
end forward

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

global type w_maed_movtofrutacomer_reproceso from w_mant_encab_deta_csd
integer width = 5038
integer height = 2452
string title = "INGRESO COMERCIAL DE RE-PROCESO"
string menuname = ""
windowstate windowstate = maximized!
event ue_despuesguardar ( )
event ue_despuesborrar ( )
dw_3 dw_3
dw_6 dw_6
dw_8 dw_8
tab_1 tab_1
dw_exiencab dw_exiencab
dw_exideta dw_exideta
dw_exidetaborra dw_exidetaborra
dw_exismovtodetanulos dw_exismovtodetanulos
end type
global w_maed_movtofrutacomer_reproceso w_maed_movtofrutacomer_reproceso

type variables
w_mant_deta_lotesfrutacomdeta		iw_mantencion
w_mant_deta_movtoenvadeta			iw_mantencion_2

DataWindowChild						idwc_planta, idwc_tipofrio, idwc_periodofrio, &
											idwc_especie, idwc_variedad, idwc_tipoenvase, &
											idwc_categoria, idwc_tipomovto, idwc_transportista, &
											idwc_plantaori, idwc_tipodocto

DataWindow							dw_4, dw_5

str_envase								istr_envase

uo_variedades							iuo_variedades
uo_categorias							iuo_categorias
uo_productores							iuo_productores
uo_productores							iuo_prodempresa
uo_spro_ordenproceso				iuo_spro_ordenproceso
uo_doctointernopack					iuo_doctointernopack
uo_plantadesp							iuo_planta
uo_transportista						iuo_transportista
uo_camiones							iuo_Camion
uo_tipodoctoplanta					iuo_TipoDocto
uo_tipomovtofruta						iuo_TipoMovtoFruta
uo_tipomovtofruta						iuo_TipoMovtoEnva
uo_clientesprod						iuo_cliente
uo_calicosechero  						iuo_calicosechero
//uo_camarasfrigo						iuo_camaras

Long					     				il_NumFruta=0, il_NumEnva=0, il_coneccion, il_conexiste, il_numeroenva, il_secuencia
Integer 									ii_tipool=0
String										is_rut, is_recepcion
Boolean									ib_Modifica, ib_AutoCommit, ib_ConectadoExistencia
Private:		
str_pesaje								wstr_pesaje

Transaction			sqlexi

end variables

forward prototypes
public subroutine buscacamion ()
public subroutine buscacliente ()
public function boolean existemovtorecep (long al_numero)
public function integer buscatotalenvases (integer ai_planta, integer ai_tipmov, long al_numero, integer ai_loteplanta, integer ai_loteespe, long al_lote, integer ai_tipoenva, integer ai_envase)
public function boolean revisaenvases ()
public subroutine totales ()
public subroutine habilitalote ()
public function boolean buscamovto (integer ai_tipdoc, long al_docrel)
public subroutine buscaprodrot ()
public subroutine buscaproductor ()
public function long nuevolote (integer al_revlote)
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean existemovimiento (long al_numero)
public function boolean existelote (long al_lote)
public subroutine habilitaingreso (string as_columna)
public function boolean borradetallemovto (integer ai_planta, integer ai_especie, long al_lote, integer ai_secuen)
public function integer tiporecepcion (string as_recepcion, date ad_fechamovto)
public function long entreganrolote ()
public subroutine buscareproceso ()
public function boolean retiraprod (long al_productor, integer ai_especie, integer ai_variedad, integer ai_categoria, date ad_fechalot)
public function boolean conexionexistencia ()
public function long nuevofoliolote ()
public subroutine imprime_tarja ()
end prototypes

event ue_despuesguardar();integer	li_bodega, li_fila, li_cliente, li_planta,li_enva_codigo, li_enva_tipoen, &
			li_bodecomercial, li_bodzonal
Long 		ll_fila, ll_numero, li_secuencia = 1, ll_numnuevoini, ll_numnuevofin, ll_count, ll_docrel, ll_fila_nea
String	ls_item_codigo, ls_productor, ls_prod, ls_calidad
Boolean	lb_AutoCommit, lb_Retorno

IF ib_Conectadoexistencia = TRUE THEN
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
	ELSE
		li_bodecomercial 	= luo_existencia.BodDestino
		li_bodzonal			= luo_existencia.bodzonal
	END IF		
			
	IF luo_existencia.Mesproceso > dw_2.Object.mfco_fecmov[1] THEN
		Message.DoubleParm = -1
		RETURN 
	END IF		
			
	ll_docrel = dw_2.Object.mfco_numero[1]

	luo_existencia.existeencabezado(ll_docrel,li_bodecomercial,1,1,True,sqlexi)
		
	IF luo_existencia.count = 0 THEN
		IF Not luo_existencia.correlativobode(1,li_bodecomercial,li_bodzonal,True,sqlexi) THEN
			Message.DoubleParm = -1
			RETURN 
		ELSE
			ll_numero = luo_existencia.numero
		END IF	
				
		IF isnull(ll_numero) THEN
			ll_numnuevoini = Long(String(li_bodecomercial)+''+'0001')
			ll_numnuevofin = Long(String(li_bodecomercial)+''+'9999')
			
			INSERT INTO dbo.correlbode(bode_codigo,mden_tipdoc,cobo_bodadm,cobo_inicia,cobo_final,cobo_actual)
			VALUES(:li_bodecomercial,1,:li_bodzonal,:ll_numnuevoini,:ll_numnuevofin,:ll_numnuevoini)
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
		
		ls_productor = String(dw_4.Object.prod_codigo[1],'000000')
				
		IF Not luo_existencia.Existecliente(ls_productor,True,sqlexi) THEN
			Message.DoubleParm = -1
			RETURN 
			ls_productor = luo_existencia.prod
		END IF
		
		IF luo_existencia.prod = '' OR isnull(luo_existencia.prod) THEN
			ls_productor = luo_existencia.prdgen
		END IF	
		
		ll_fila_nea = dw_exiencab.InsertRow(0)
		
		dw_exiencab.Object.mden_tipdoc[ll_fila_nea] = 1
		dw_exiencab.Object.mden_numero[ll_fila_nea] = ll_numero 
		dw_exiencab.Object.tpdo_codigo[ll_fila_nea] = 2
		dw_exiencab.Object.mden_fecmov[ll_fila_nea] = dw_2.Object.mfco_fecmov[1]
		dw_exiencab.Object.tpmv_tipomv[ll_fila_nea] = 1
		dw_exiencab.Object.tpmv_codigo[ll_fila_nea] = 2
		dw_exiencab.Object.mden_tipana[ll_fila_nea] = 3
		dw_exiencab.Object.bode_codigo[ll_fila_nea] = li_bodecomercial
		dw_exiencab.Object.mden_bodest[ll_fila_nea] = li_bodzonal
		dw_exiencab.Object.clpr_rut[ll_fila_nea]	  = ls_productor
		dw_exiencab.Object.mden_docrel[ll_fila_nea] = dw_2.Object.mfco_numero[1]
		dw_exiencab.Object.mden_fecdre[ll_fila_nea] = dw_2.Object.mfco_fecmov[1]
		dw_exiencab.Object.mden_observ[ll_fila_nea] = 'Traspaso Cta. Cte. Envases'
		dw_exiencab.Object.mden_estado[ll_fila_nea] = 1
		dw_exiencab.Object.mden_pcopda[ll_fila_nea] = 1
		dw_exiencab.Object.mden_estaci[ll_fila_nea] = gstr_us.computador
		dw_exiencab.Object.mden_fecdig[ll_fila_nea] = Date(Today())
		dw_exiencab.Object.mden_hordig[ll_fila_nea] = Time(Now())
		
		FOR li_fila = 1 TO dw_5.RowCount()
			ll_fila	=	dw_exideta.InsertRow(0)
			
			li_enva_codigo = dw_5.Object.enva_codigo[li_fila]
			li_enva_tipoen = dw_5.Object.enva_tipoen[li_fila]
			ls_calidad		= dw_5.Object.cale_calida[li_fila]
			
			IF Not iuo_calicosechero.Existe(li_enva_tipoen,li_enva_codigo,ls_calidad,True,sqlca) THEN
				Message.DoubleParm = -1
				RETURN 
			ELSE
				ls_item_codigo = iuo_calicosechero.item 
			END IF	
			
			IF ls_item_codigo = '' OR IsNull(ls_item_codigo) THEN
				ls_item_codigo = luo_existencia.itgene
			END IF
			
			dw_exideta.Object.mden_tipdoc[ll_fila] = 1
			dw_exideta.Object.mden_numero[ll_fila] = ll_numero
			dw_exideta.Object.mdde_secuen[ll_fila] = li_secuencia 
			dw_exideta.Object.tpmv_tipomv[ll_fila] = 1 
			dw_exideta.Object.tpmv_codigo[ll_fila] = 2 
			dw_exideta.Object.item_codigo[ll_fila] = ls_item_codigo
			dw_exideta.Object.mdde_identi[ll_fila] = ''
			dw_exideta.Object.mdde_fecmov[ll_fila] = dw_2.Object.mfco_fecmov[1]
			dw_exideta.Object.bode_codigo[ll_fila] = li_bodecomercial
			dw_exideta.Object.mdde_cantid[ll_fila] = dw_5.Object.fgme_cantid[li_fila]
			li_secuencia = li_secuencia + 1
		
		NEXT	
					
		IF Not luo_existencia.correlativobode(3,li_bodzonal,li_bodzonal,True,sqlexi) THEN
			Message.DoubleParm = -1
			RETURN 
		ELSE
			ll_numero = luo_existencia.numero
		END IF	
			
		IF isnull(ll_numero) THEN
			ll_numnuevoini = Long(String(li_bodecomercial)+''+'0001')
			ll_numnuevofin = Long(String(li_bodecomercial)+''+'9999')
			
			INSERT INTO dbo.correlbode(bode_codigo,mden_tipdoc,cobo_bodadm,cobo_inicia,cobo_final,cobo_actual)
			VALUES(:li_bodzonal,3,:li_bodzonal,:ll_numnuevoini,:ll_numnuevofin,:ll_numnuevoini)
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
		
		ls_productor = String(dw_6.Object.prod_codigo[1],'000000')
		
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
		dw_exiencab.Object.tpmv_codigo[ll_fila_nea] 	= 	3
		dw_exiencab.Object.mden_tipana[ll_fila_nea] 	= 	4
		dw_exiencab.Object.bode_codigo[ll_fila_nea] 	= 	li_bodzonal
		dw_exiencab.Object.mden_bodest[ll_fila_nea] 	= 	li_bodecomercial
		dw_exiencab.Object.clpr_rut[ll_fila_nea]	 	= 	ls_productor
		dw_exiencab.Object.mden_docrel[ll_fila_nea] 	= 	dw_2.Object.mfco_numero[1]
		dw_exiencab.Object.mden_fecdre[ll_fila_nea] 	= 	dw_2.Object.mfco_fecmov[1]
		dw_exiencab.Object.mden_observ[ll_fila_nea] 	= 	'Traspaso Cta. Cte. Envases'
		dw_exiencab.Object.mden_estado[ll_fila_nea] 	= 	1
		dw_exiencab.Object.mden_pcopda[ll_fila_nea] 	=	1
		dw_exiencab.Object.mden_estaci[ll_fila_nea]  =  gstr_us.computador
		dw_exiencab.Object.mden_fecdig[ll_fila_nea]  =  Date(Today())
		dw_exiencab.Object.mden_hordig[ll_fila_nea]  =  Time(Now())
		li_secuencia = 1
		
		FOR li_fila = 1 TO dw_5.RowCount()
			
			ll_fila			=	dw_exideta.InsertRow(0)
			
			li_enva_codigo = dw_5.Object.enva_codigo[li_fila]
			li_enva_tipoen = dw_5.Object.enva_tipoen[li_fila]
			ls_calidad		= dw_5.Object.cale_calida[li_fila]
			
			IF Not iuo_calicosechero.Existe(li_enva_tipoen,li_enva_codigo,ls_calidad,True,sqlca) THEN
				Message.DoubleParm = -1
				RETURN 
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
			dw_exideta.Object.tpmv_codigo[ll_fila] 	= 	3 
			dw_exideta.Object.item_codigo[ll_fila] 	= 	ls_item_codigo
			dw_exideta.Object.mdde_identi[ll_fila] 	= 	''
			dw_exideta.Object.mdde_fecmov[ll_fila] 	= 	dw_2.Object.mfco_fecmov[1]
			dw_exideta.Object.bode_codigo[ll_fila] 	= 	li_bodecomercial
			dw_exideta.Object.mdde_cantid[ll_fila] 	= 	dw_5.Object.fgme_cantid[li_fila]
			li_secuencia = li_secuencia + 1
			
		NEXT	
		
		IF dw_exiencab.Rowcount() > 0 THEN
			IF dw_exiencab.Update(True, False) = 1 THEN
				IF dw_exideta.Update(True, False) = 1 THEN
					Commit;
					
//					IF sqlexi.SQLCode <> 0 THEN
//						F_ErrorBaseDatos(sqlexi, This.Title)
//						
//					ELSE
//						lb_Retorno	=	True
						
						dw_exiencab.ResetUpdate()
						dw_exideta.ResetUpdate()
					//END IF
				ELSE
					F_ErrorBaseDatos(sqlexi, This.Title)
					
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqlexi, This.Title)
				
				RollBack;
			END IF
			
			Messagebox("Existencia","Grabación de Datos realizada Satisfactoriamente")
			dw_exiencab.Reset()
			dw_exideta.Reset()
			sqlexi.AutoCommit	=	lb_AutoCommit
			
		END IF
		DISCONNECT USING sqlexi;
		ib_Conectadoexistencia = False
	END IF
END IF









end event

event ue_despuesborrar();Long 		ll_numero, respuesta, ll_nueva, ll_fila
Boolean lb_AutoCommit
Integer	li_bodecomercial, li_bodzonal, li_devcorreo, li_tipdoc
String	ls_correo, ls_error, sErrorMsg, ls_correozonal, ls_texto, ls_asunto

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
	
	IF luo_existencia.Mesproceso > dw_2.Object.mfco_fecmov[1] THEN
		il_numeroenva = dw_2.Object.mfco_numero[1]
	
		IF luo_existencia.numeromaximo(1,li_bodecomercial,il_numeroenva,1,True,sqlexi) THEN
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
		
		ls_asunto = "Modifica Fruta Comercial Movto. Nº "+String(il_numeroenva)
	//	li_devcorreo =  Send_mail("smtp.rioblanco.cl","<frutacomercial@rioblanco.cl>","<"+ls_correo+">","<"+ls_correozonal+">","",ls_asunto,ls_texto,"",ls_error)
		
		IF (li_devcorreo<0) THEN
			messagebox("Error No" + string(li_devcorreo),sErrorMsg)
		END IF
		
		RETURN 
	 
	END IF	
	
	il_numeroenva = dw_2.Object.mfco_numero[1]
	
	IF NOT luo_existencia.numeromaximo(1,li_bodecomercial,il_numeroenva,1,True,sqlexi) THEN
		Message.DoubleParm = -1
		Return
	ELSE
		ll_numero = luo_existencia.numero
	END IF
			
	IF isnull(ll_numero) THEN
		Return
	END IF	
	
	IF NOT luo_existencia.actualizaexistencia(2,1,li_bodecomercial,ll_numero,il_numeroenva,True,sqlexi) THEN
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
	
	li_tipdoc = dw_exidetaborra.Object.mden_tipdoc[1] 
	
	DELETE FROM dbo.exismovtodeta 
		WHERE	mden_numero = :ll_numero
		AND	mden_tipdoc = :li_tipdoc
		AND	bode_codigo = :li_bodecomercial
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

	IF NOT luo_existencia.nummaxpbodega(3,li_bodzonal,li_bodecomercial,il_numeroenva,True,sqlexi) THEN
		F_ErrorBaseDatos(sqlexi,"exismovtoenca")
		Message.DoubleParm = -1
		Return
	ELSE	
		ll_numero = luo_existencia.numero
	END IF	

	IF isnull(ll_numero) THEN
		Return
	END IF	
		
	UPDATE dbo.exismovtoenca SET
		mden_estado = 2
		WHERE mden_tipdoc = :li_tipdoc
		AND bode_codigo = :li_bodecomercial
		AND mden_numero = :ll_numero
		AND mden_docrel = :il_numeroenva
		AND mden_estado = 1
		USING sqlexi;
		
	IF sqlexi.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlexi,"exismovtoenca")
		Message.DoubleParm = -1
		sqlexi.AutoCommit	=	ib_AutoCommit
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
	
	li_tipdoc = dw_exidetaborra.Object.mden_tipdoc[1]
	
	DELETE FROM dbo.exismovtodeta 
		WHERE	mden_tipdoc = :li_tipdoc 
		AND	mden_numero = :ll_numero
		AND	bode_codigo = :li_bodzonal
		USING sqlexi;
		
	UPDATE dbo.exismovtoenca SET
		mden_estado = 2
		WHERE mden_tipdoc = :li_tipdoc
		AND bode_codigo = :li_bodzonal
		AND mden_numero = :ll_numero
		AND mden_docrel = :il_numeroenva
		AND mden_estado = 1
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
	
	//DISCONNECT USING sqlexi;
	//ib_Conectadoexistencia = False
END IF




end event

public subroutine buscacamion ();Str_busqueda	lstr_busq

lstr_busq.argum[1] = String(dw_2.Object.cami_clasifi[1])

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
	dw_2.SetFocus()
END IF

RETURN
end subroutine

public subroutine buscacliente ();Str_busqueda	lstr_busq

lstr_busq.Argum[1]	=	'0'

OpenWithParm(w_busc_clienprove, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	dw_2.SetColumn("clpr_rut")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clpr_rut[1]			=	lstr_busq.argum[1]
	dw_2.Object.clpr_nombre[1]		=	lstr_busq.argum[2]
	dw_2.SetFocus()
END IF

RETURN
end subroutine

public function boolean existemovtorecep (long al_numero);Integer	li_TipoMovto, li_Planta, li_TipoDocto, li_Tipo
Long     ll_NroDocto

li_Planta		=	dw_2.Object.plde_codigo[1]
li_TipoMovto	=	Integer(istr_mant.argumento[2])

SELECT	mfco_tipdoc, mfco_docrel
	INTO	:li_TipoDocto, :ll_NroDocto
	FROM	dbo.spro_movtofrutacomenca
	WHERE	plde_codigo	=	:li_Planta
	AND	tpmv_codigo	=	:li_TipoMovto
	AND	mfco_numero	=	:al_Numero ;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_movtofrutacomenca")
	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN
	RETURN False
END IF

istr_Mant.Argumento[4] = String(li_TipoDocto)
istr_Mant.Argumento[5] = String(ll_NroDocto)

RETURN True
end function

public function integer buscatotalenvases (integer ai_planta, integer ai_tipmov, long al_numero, integer ai_loteplanta, integer ai_loteespe, long al_lote, integer ai_tipoenva, integer ai_envase);Decimal{2} ld_Bultos

  SELECT sum(lfd.lfcd_bultos) into :ld_Bultos
    FROM dbo.spro_movtofrutacomenca mfe, dbo.spro_movtofrutacomdeta mfd,
	      dbo.spro_lotesfrutacomdeta lfd
   WHERE ( mfe.plde_codigo = :ai_planta )
	  AND ( mfe.tpmv_codigo = :ai_tipmov )
	  AND ( mfe.mfco_numero = :al_numero )
	  AND ( mfe.plde_codigo = mfd.plde_codigo )
	  AND ( mfe.tpmv_codigo = mfd.tpmv_codigo )
	  AND ( mfe.mfco_numero = mfd.mfco_numero )
	  AND ( mfd.lofc_pltcod = lfd.lofc_pltcod )
	  AND ( mfd.lofc_espcod = lfd.lofc_espcod )
	  AND ( mfd.lofc_lotefc = lfd.lofc_lotefc )
	  AND ( mfd.lfcd_secuen = lfd.lfcd_secuen )
	  AND ( lfd.lofc_lotefc <> :al_lote )
	  AND ( lfd.enva_tipoen = :ai_tipoenva )
	  AND ( lfd.enva_codigo = :ai_envase );
	  
	IF SqlCa.SQLCode = -1 THEN
		F_ErrorBaseDatos(SqlCa, "Lectura de Tabla CalibresEnvase")
      RETURN 0
	END IF 
	  
	IF isnull(ld_Bultos) THEN ld_Bultos=0
	
RETURN ld_Bultos	  
end function

public function boolean revisaenvases ();Long     ll_Fila, ll_FilaBusc, ll_Filaarr,arr_envases[500,4], ll_Row, &
         ll_FilaUlt, ll_loteac, ll_numero
Integer  li_TipoEnva, li_Envase, li_TipoEnvaSig, li_EnvaseSig, li_cont, &
         li_planta, li_tipmov, li_lotepl, li_lotees
Dec{2}   ld_sumbultos, ld_BuscaBultos=0
String   ls_mensaje
Boolean  lb_ya_ingresado = False

li_planta = dw_2.Object.plde_codigo[1]
li_tipmov = dw_2.Object.tpmv_codigo[1]
ll_numero = dw_2.Object.mfco_numero[1]
li_lotepl = dw_3.Object.lofc_espcod[1]
li_lotees = dw_3.Object.lofc_pltcod[1]
ll_loteac = dw_3.Object.lofc_lotefc[1]

ll_Fila = 1
ll_FilaArr = 1

DO WHILE  ll_Fila<= dw_4.RowCount()
	
	ld_sumBultos = 0
	lb_ya_ingresado = False
	
   li_Tipoenva =	dw_4.Object.enva_tipoen[ll_Fila]
	li_envase	=	dw_4.Object.enva_codigo[ll_Fila]
		
	FOR ll_Row=1 To UpperBound(arr_envases)
		IF arr_envases[ll_row,1] = li_TipoEnva AND arr_envases[ll_row,2] = li_Envase THEN
			lb_ya_ingresado = TRUE
			ll_row = UpperBound(arr_envases)
		ELSE
			IF arr_envases[ll_row,1] = 0 THEN ll_row = UpperBound(arr_envases)
		END IF
	NEXT	
	
	IF lb_ya_ingresado = False THEN
		
		ld_SumBultos = dw_4.Object.lfcd_bultos[ll_Fila]
		
		FOR ll_FilaBusc = (ll_fila + 1) TO dw_4.RowCount()
			li_TipoEnvaSig =	dw_4.Object.enva_tipoen[ll_FilaBusc]
			li_EnvaseSig	=	dw_4.Object.enva_codigo[ll_FilaBusc]
			IF li_TipoEnva = li_TipoEnvaSig And li_Envase=li_EnvaseSig THEN
				ld_SumBultos = ld_SumBultos + dw_4.Object.lfcd_bultos[ll_FilaBusc]
			END IF	
		NEXT	
		
		ld_BuscaBultos = BuscaTotalEnvases(li_planta,li_tipmov,ll_numero, li_lotepl, &
		                li_lotees, ll_loteac, li_tipoenva, li_envase )
		
		ld_SumBultos = ld_SumBultos + ld_BuscaBultos
		
		arr_envases[ll_FilaArr,1] = li_TipoEnva
		arr_envases[ll_FilaArr,2] = li_Envase
		arr_envases[ll_FilaArr,3] = ld_SumBultos
		arr_envases[ll_FilaArr,4] = 0
		ll_FilaArr++
		
	END IF
	ll_Fila++

LOOP

ll_Fila = 1
DO WHILE  ll_Fila<= dw_5.RowCount()
	
	ld_sumBultos = 0
	lb_ya_ingresado = False
	
   li_Tipoenva =	dw_5.Object.enva_tipoen[ll_Fila]
	li_envase	=	dw_5.Object.enva_codigo[ll_Fila]
		
	FOR ll_Row=1 To UpperBound(arr_envases)
		IF arr_envases[ll_row,1] = li_TipoEnva AND arr_envases[ll_row,2] = li_Envase THEN
			lb_ya_ingresado = TRUE
			ll_row = UpperBound(arr_envases)
		ELSE
			IF arr_envases[ll_row,1] = 0 THEN ll_row = UpperBound(arr_envases)
		END IF
	NEXT	
	
	IF lb_ya_ingresado = False THEN
		
		ld_BuscaBultos = BuscaTotalEnvases(li_planta,li_tipmov,ll_numero, li_lotepl, &
		                 li_lotees, ll_loteac, li_tipoenva, li_envase )
		
		ld_SumBultos = ld_SumBultos + ld_BuscaBultos
		
		arr_envases[ll_FilaArr,1] = li_TipoEnva
		arr_envases[ll_FilaArr,2] = li_Envase
		arr_envases[ll_FilaArr,3] = ld_SumBultos
		arr_envases[ll_FilaArr,4] = 0
		ll_FilaArr++
		
	END IF
	ll_Fila++
LOOP

ll_Fila    = 1
ll_FilaUlt = ll_FilaArr

DO WHILE  ll_Fila<= dw_5.RowCount()
	
	ld_sumBultos = 0
	lb_ya_ingresado = False
	
   li_Tipoenva =	dw_5.Object.enva_tipoen[ll_Fila]
	li_envase	=	dw_5.Object.enva_codigo[ll_Fila]
		
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
			ld_SumBultos = dw_5.Object.fgme_cantid[ll_Fila]
			
			FOR ll_FilaBusc = (ll_fila + 1) TO dw_5.RowCount()
				li_TipoEnvaSig	=	dw_5.Object.enva_tipoen[ll_FilaBusc]
				li_EnvaseSig	=	dw_5.Object.enva_codigo[ll_FilaBusc]
				IF li_TipoEnva = li_TipoEnvaSig And li_Envase=li_EnvaseSig THEN
					ld_SumBultos = ld_SumBultos + dw_5.Object.fgme_cantid[ll_FilaBusc]
				END IF	
			NEXT	
			
			arr_envases[ll_FilaArr,4] = ld_SumBultos
		END IF	
	ELSE
		arr_envases[ll_FilaUlt,1] = li_TipoEnva
		arr_envases[ll_FilaUlt,2] = li_Envase
		arr_envases[ll_FilaUlt,3] = 0
      arr_envases[ll_FilaUlt,4] = dw_5.Object.fgme_cantid[ll_Fila]
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

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "No Existe Concordancia de Bultos en :" + ls_mensaje + ".", StopSign!, Ok!)
	RETURN FALSE
END IF


RETURN TRUE

end function

public subroutine totales ();Decimal{2}	ld_TotalBultos, ld_TotalKilos

dw_4.AcceptText()

IF dw_4.RowCount() = 0 THEN il_Fila = 0
IF dw_4.RowCount() < il_Fila THEN il_Fila = dw_4.RowCount()

IF il_Fila > 0 THEN
	ld_TotalBultos	= dw_4.Object.total_bulto[il_Fila]
	ld_TotalKilos	= dw_4.Object.total_kilos[il_Fila]
ELSE
	ld_TotalBultos	= 0
	ld_TotalKilos	= 0
END IF

dw_3.SetItem(1, "lofc_totbul", Long(ld_TotalBultos))
dw_3.SetItem(1, "lofc_totkil", ld_TotalKilos)
end subroutine

public subroutine habilitalote ();
If Istr_Mant.Argumento[12] = "1" AND (gstr_paramplanta.PoolVenta = 1 Or gstr_paramplanta.PoolVenta = 5) Then
   	dw_3.Object.lofc_lotefc.Protect		=	0 
	dw_3.Object.lofc_lotefc.Color 		=	0
	dw_3.Object.lofc_lotefc.BackGround.Color	 = RGB(255,255,255)
ElseIf Istr_Mant.Argumento[12] = "2" AND (gstr_paramplanta.PoolRetiro = 1 Or gstr_paramplanta.PoolRetiro = 4) Then
	dw_3.Object.lofc_lotefc.Protect		=	0
	dw_3.Object.lofc_lotefc.Color 		= 	0
	dw_3.Object.lofc_lotefc.BackGround.Color	 = RGB(255,255,255)
Else
   	dw_3.Object.lofc_lotefc.Protect					=	1
	dw_3.Object.lofc_lotefc.Color 					= 	RGB(255,255,255)
	dw_3.Object.lofc_lotefc.BackGround.Color	 = 553648127
End If

end subroutine

public function boolean buscamovto (integer ai_tipdoc, long al_docrel);Long     ll_nrodocto
Integer li_planta, li_tiponum, li_cliente

li_planta  = dw_2.Object.plde_codigo[1]
li_tiponum = Integer(istr_mant.argumento[2])
li_cliente = Integer(istr_mant.argumento[13])

SELECT	mfco_numero
	INTO	:ll_nrodocto
	FROM	dbo.spro_movtofrutacomenca
	WHERE	plde_codigo	=	:li_planta
	AND	tpmv_codigo	=	:li_tiponum
	AND	mfco_tipdoc	=	:ai_tipdoc
	AND   mfco_docrel =  :al_docrel
	AND 	clie_codigo	=	:li_cliente;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_movtofrutacomenca")

	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN

	RETURN False
END IF

IF ExisteMovimiento(ll_nrodocto) THEN
   istr_Mant.Argumento[3] = string(ll_nrodocto)
END IF	

RETURN True
end function

public subroutine buscaprodrot ();Str_Busqueda	lstr_Busq

OpenWithParm(w_busc_productores, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	dw_3.SetItem(1, "prod_codigo", Long(lstr_Busq.Argum[1]))
	dw_3.SetItem(1, "prod_nombre", lstr_Busq.Argum[2])

	HabilitaIngreso("prod_codigo")
	dw_3.SetColumn("prod_codigo")
	dw_3.SetFocus()
END IF
end subroutine

public subroutine buscaproductor ();Str_Busqueda	lstr_Busq

OpenWithParm(w_busc_productores, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	dw_2.SetItem(1, "prod_codigo", Long(lstr_Busq.Argum[1]))
	dw_2.SetItem(1, "prod_nombre", lstr_Busq.Argum[2])

	HabilitaIngreso("prod_codigo")
	dw_2.SetColumn("prod_codigo")
	dw_2.SetFocus()
END IF
end subroutine

public function long nuevolote (integer al_revlote);Integer 	li_planta, li_especie, Existelote=1
Long    	ll_lote=0
String	ls_nombrepc

li_planta  = Integer(istr_mant.Argumento[1])
li_especie = Integer(istr_mant.Argumento[7])
ls_nombrepc	=	gstr_us.computador

If Not IsNull(al_revlote) AND al_revlote > 0 Then
	  
	 SELECT Count(*) INTO :ExisteLote 
	   FROM dbo.spro_lotesfrutacomenc 
 	  WHERE lofc_pltcod = :li_planta
		 AND lofc_espcod = :li_especie
		 AND lofc_lotefc = :al_revlote;

End If

If ExisteLote = 0 OR isnull(existelote) Then
	ll_Lote = al_RevLote
	dw_3.Object.lofc_pltcod[1] = li_planta
	dw_3.Object.lofc_espcod[1] = li_especie
	
	iuo_prodempresa.existe(gstr_paramplanta.productorempresa,false,sqlca)
	dw_3.SetItem(1, "prod_codigo", gstr_paramplanta.productorempresa)
	dw_3.SetItem(1, "prod_nombre", iuo_prodempresa.nombre )

	dw_3.Object.lofc_totbul[1] = 0
	dw_3.Object.lofc_totkil[1] = 0
Else
	If istr_mant.Argumento[12] = "1" Then
		If gstr_paramplanta.PoolVenta = 5 OR gstr_paramplanta.PoolVenta = 1 Then
			SELECT loco_ultcom  into :ll_lote
			  FROM dbo.spro_lotescorrelequipo
			 WHERE espe_codigo = :li_especie
				AND plde_codigo = :li_planta
				AND Upper(equi_nombre) 	=	Upper(:ls_nombrepc);
		Else
			SELECT loco_inicom  into :ll_lote
			  FROM dbo.spro_lotescorrelequipo
			 WHERE espe_codigo = :li_especie
				AND plde_codigo = :li_planta
				AND Upper(equi_nombre) 	=	Upper(:ls_nombrepc);
		End If

	Else
		If gstr_paramplanta.PoolRetiro = 4 OR gstr_paramplanta.PoolRetiro = 1 Then
			SELECT loco_ulcore  into :ll_lote
			  FROM dbo.spro_lotescorrelequipo
			 WHERE espe_codigo = :li_especie
				AND plde_codigo = :li_planta
				AND Upper(equi_nombre) 	=	Upper(:ls_nombrepc);
		Else
			SELECT loco_incore  into :ll_lote
			  FROM dbo.spro_lotescorrelequipo
			 WHERE espe_codigo = :li_especie
				AND plde_codigo = :li_planta
				AND Upper(equi_nombre) 	=	Upper(:ls_nombrepc);
		End If
	End If
	
	dw_3.Object.lofc_pltcod[1] = li_planta
	dw_3.Object.lofc_espcod[1] = li_especie
	
	iuo_prodempresa.existe(gstr_paramplanta.productorempresa,false,sqlca)
	dw_3.SetItem(1, "prod_codigo", gstr_paramplanta.productorempresa)
	dw_3.SetItem(1, "prod_nombre", iuo_prodempresa.nombre )

	dw_3.Object.lofc_totbul[1] = 0
	dw_3.Object.lofc_totkil[1] = 0
	
	If isnull(ll_lote) Then ll_lote=0
	If (Istr_Mant.Argumento[12] = "1" AND (gstr_Paramplanta.PoolVenta = 1) OR &
													  (gstr_Paramplanta.PoolVenta = 5)) OR &
		(Istr_Mant.Argumento[12] = "2" AND (gstr_Paramplanta.PoolRetiro = 1) OR &
													  (gstr_Paramplanta.PoolRetiro = 4)) Then
		ll_lote = ll_lote + 1
	End If
	
	If (istr_Mant.Argumento[12] = "1" AND (gstr_paramplanta.PoolVenta = 1) OR &
													  (gstr_paramplanta.PoolVenta = 5)) Then
		UPDATE dbo.spro_lotescorrel SET loco_ultcom = :ll_lote
	 	  FROM dbo.spro_lotescorrelequipo
		 WHERE espe_codigo = :li_especie
			AND plde_codigo = :li_planta
			AND Upper(equi_nombre) 	=	Upper(:ls_nombrepc);
	ElseIf gstr_paramplanta.PoolRetiro = 1 OR gstr_paramplanta.PoolRetiro = 4 Then
		UPDATE dbo.spro_lotescorrel SET loco_ulcore = :ll_lote
	 	  FROM dbo.spro_lotescorrelequipo
		 WHERE espe_codigo = :li_especie
			AND plde_codigo = :li_planta
			AND Upper(equi_nombre) 	=	Upper(:ls_nombrepc);
	End If
	
	pb_grabar.enabled=false
	pb_eliminar.enabled=false

End If

RETURN ll_lote
end function

public subroutine habilitaencab (boolean habilita); If Habilita Then
	If istr_Mant.Argumento[8] = "1" Then
		dw_2.Object.plde_codigo.Protect		=	0
		dw_2.Object.clie_codigo.Protect		=	0
		dw_2.Object.mfco_numero.Protect	=	0
		dw_2.Object.mfco_fecmov.Protect		=	0
		dw_2.Object.mfco_tipdoc.Protect		=	0
		dw_2.Object.mfco_docrel.Protect		=	0

		dw_2.Object.plde_codigo.Color 	= 0
		dw_2.Object.clie_codigo.Color 		= 0
		dw_2.Object.mfco_numero.Color	= 0
		dw_2.Object.mfco_fecmov.Color 	= 0
		dw_2.Object.mfco_tipdoc.Color 	= 0
		dw_2.Object.mfco_docrel.Color 	= 0

		
		dw_2.Object.plde_codigo.BackGround.Color 	= RGB(255,255,255)
		dw_2.Object.clie_codigo.BackGround.Color 		= RGB(255,255,255)
		dw_2.Object.mfco_numero.BackGround.Color	= RGB(255,255,255)
		dw_2.Object.mfco_fecmov.BackGround.Color 	= RGB(255,255,255)
		dw_2.Object.mfco_tipdoc.BackGround.Color 	= RGB(255,255,255)
		dw_2.Object.mfco_docrel.BackGround.Color 	= RGB(255,255,255)
		
	Else
		dw_2.Object.mfco_numero.Protect	=	0
		dw_2.Object.mfco_fecmov.Protect		=	0
		dw_2.Object.mfco_horaen.Protect		=	0
		dw_2.Object.cami_clasIfi.Protect		=	0
		dw_2.Object.plde_coorde.Protect		=	0
		dw_2.Object.cami_patent.Protect		=	0
		dw_2.Object.cami_patcar.Protect		=	0
		dw_2.Object.mfco_guisii.Protect		=	0
		dw_2.Object.mfco_rutcho.Protect		=	0
		dw_2.Object.mfco_chofer.Protect		=	0
		dw_2.Object.cod_especie.Protect		=	0
		
		dw_2.Object.mfco_numero.Color	= 0
		dw_2.Object.mfco_fecmov.Color 	= 0
		dw_2.Object.mfco_horaen.Color 	= 0
		dw_2.Object.cami_clasIfi.Color 	= 0
		dw_2.Object.plde_coorde.Color 	= 0
		dw_2.Object.cami_patent.Color 	= 0
		dw_2.Object.cami_patcar.Color 	= 0
		dw_2.Object.mfco_guisii.Color 		= 0
		dw_2.Object.mfco_rutcho.Color 	= 0
		dw_2.Object.mfco_chofer.Color 	= 0
		dw_2.Object.cod_especie.Color 	= 0
		dw_3.Object.prod_codigo.Color 	= 	0
		
		dw_2.Object.mfco_numero.BackGround.Color	= RGB(255,255,255)
		dw_2.Object.mfco_fecmov.BackGround.Color 	= RGB(255,255,255)
		dw_2.Object.mfco_horaen.BackGround.Color 	= RGB(255,255,255)
		dw_2.Object.cami_clasIfi.BackGround.Color 	= RGB(255,255,255)
		dw_2.Object.plde_coorde.BackGround.Color 	= RGB(255,255,255)
		dw_2.Object.cami_patent.BackGround.Color 	= RGB(255,255,255)
		dw_2.Object.cami_patcar.BackGround.Color 	= RGB(255,255,255)
		dw_2.Object.mfco_guisii.BackGround.Color 		= RGB(255,255,255)
		dw_2.Object.mfco_rutcho.BackGround.Color 	= RGB(255,255,255)
		dw_2.Object.mfco_chofer.BackGround.Color 	= RGB(255,255,255)
		dw_2.Object.cod_especie.BackGround.Color 	= RGB(255,255,255)
	End If

	dw_2.SetFocus()
Else
	If istr_Mant.Argumento[8] = "1" Then
		dw_2.Object.plde_codigo.Protect		=	1
		dw_2.Object.clie_codigo.Protect		=	1
		dw_2.Object.mfco_numero.Protect	=	1
		dw_2.Object.mfco_fecmov.Protect		=	1
		dw_2.Object.mfco_tipdoc.Protect		=	1
		dw_2.Object.mfco_docrel.Protect		=	1
		
		dw_2.Object.plde_codigo.Color 	= RGB(255,255,255)
		dw_2.Object.clie_codigo.Color 		= RGB(255,255,255)
		dw_2.Object.mfco_numero.Color	= RGB(255,255,255)
		dw_2.Object.mfco_fecmov.Color 	= RGB(255,255,255)
		dw_2.Object.mfco_docrel.Color 	= RGB(255,255,255)
		
		dw_2.Object.plde_codigo.BackGround.Color 	= 553648127
		dw_2.Object.clie_codigo.BackGround.Color 		= 553648127
		dw_2.Object.mfco_numero.BackGround.Color	= 553648127
		dw_2.Object.mfco_fecmov.BackGround.Color 	= 553648127
		dw_2.Object.mfco_tipdoc.BackGround.Color 	= 553648127
		dw_2.Object.mfco_docrel.BackGround.Color 	= 553648127
	Else
		dw_2.Object.mfco_numero.Protect	=	1
		dw_2.Object.mfco_fecmov.Protect		=	1
		dw_2.Object.mfco_horaen.Protect		=	1
		dw_2.Object.cami_clasIfi.Protect		=	1
		dw_2.Object.plde_coorde.Protect		=	1
		dw_2.Object.cami_patent.Protect		=	1
		dw_2.Object.cami_patcar.Protect		=	1
		dw_2.Object.mfco_guisii.Protect		=	1
		dw_2.Object.mfco_rutcho.Protect		=	1
		dw_2.Object.mfco_tipdoc.Protect		=	1
		dw_2.Object.mfco_chofer.Protect		=	1
		dw_2.Object.cod_especie.Protect		=	1

		dw_2.Object.mfco_numero.Color	= RGB(255,255,255)
		dw_2.Object.mfco_fecmov.Color 	= RGB(255,255,255)
		dw_2.Object.mfco_horaen.Color 	= RGB(255,255,255)
		dw_2.Object.cami_clasIfi.Color 	= RGB(255,255,255)
		dw_2.Object.plde_coorde.Color 	= RGB(255,255,255)
		dw_2.Object.cami_patent.Color 	= RGB(255,255,255)
		dw_2.Object.cami_patcar.Color 	= RGB(255,255,255)
		dw_2.Object.mfco_guisii.Color 		= RGB(255,255,255)
		dw_2.Object.mfco_rutcho.Color 	= RGB(255,255,255)
		dw_2.Object.mfco_tipdoc.Color 	= RGB(255,255,255)
		dw_2.Object.mfco_chofer.Color 	= RGB(255,255,255)
		dw_2.Object.cod_especie.Color 	= RGB(255,255,255)
		
		dw_2.Object.mfco_numero.BackGround.Color	= 553648127
		dw_2.Object.mfco_fecmov.BackGround.Color 	= 553648127
		dw_2.Object.mfco_horaen.BackGround.Color 	= 553648127
		dw_2.Object.cami_clasIfi.BackGround.Color 	= 553648127
		dw_2.Object.plde_coorde.BackGround.Color 	= 553648127
		dw_2.Object.cami_patent.BackGround.Color 	= 553648127
		dw_2.Object.cami_patcar.BackGround.Color 	= 553648127
		dw_2.Object.mfco_guisii.BackGround.Color 		= 553648127
		dw_2.Object.mfco_rutcho.BackGround.Color 	= 553648127
		dw_2.Object.mfco_tipdoc.BackGround.Color 	= 553648127
		dw_2.Object.mfco_chofer.BackGround.Color 	= 553648127
		dw_2.Object.cod_especie.BackGround.Color 	= 553648127
	End If
End If
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean		lb_AutoCommit, lb_Retorno
DataStore	lds_detalleMovto
Integer		li_planta, li_especie
Long			ll_folio
String		ls_nombrepc
	
If Not dw_2.uf_check_required(0) Then RETURN False

If Not dw_1.uf_validate(0) Then RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If NOT Borrando Then
	If dw_2.GetItemStatus(1, 0, Primary!) = NewModIfied! Then
		dw_2.SetItem(1,"mfco_usuari",gstr_us.nombre)
		dw_2.SetItem(1,"mfco_comput",gstr_us.computador)
		dw_2.SetItem(1,"mfco_horacr",F_FechaHora())
	ElseIf dw_2.GetItemStatus(1, 0, Primary!) = DataModIfied! Then
		dw_2.SetItem(1,"mfco_usumod",gstr_us.nombre)
		dw_2.SetItem(1,"mfco_commod",gstr_us.computador)
		dw_2.SetItem(1,"mfco_horact",F_FechaHora())
	End If
End If

lds_detalleMovto					=	Create DataStore
lds_detalleMovto.DataObject	=	dw_1.DataObject

lds_detalleMovto.SetTransObject(SQLCA)

If dw_1.DeletedCount()>0 Then
	//Vacía Buffer Delete! de Detalle
	dw_1.RowsMove(1,dw_1.DeletedCount(),Delete!,lds_detallemovto,1,Primary!)
	
	//Inicializa los Flags a NotModIfied! (Estaban en NewModIfied! al mover las filas)
	lds_detalleMovto.ResetUpdate()
	
	//Pasa las filas al Buffer Delete! para forzar una eliminación previa del detalle	
	lds_detalleMovto.RowsMove(1,lds_detalleMovto.RowCount(),Primary!,lds_detalleMovto,1,Delete!)
	
End If

If Borrando Then
	If dw_5.Update(True, False) = 1 Then	
		If dw_8.Update(True, False) = 1 Then	
			If dw_1.Update(True, False) = 1 Then
				If dw_4.Update(True, False) = 1 Then
					If dw_3.Update(True, False) = 1 Then
						If dw_6.Update(True, False) = 1 Then
							If dw_2.Update(True, False) = 1 Then
								Commit;
			
								If sqlca.SQLCode <> 0 Then
									F_ErrorBaseDatos(sqlca, This.Title)
								Else
									lb_Retorno	=	True
			
									dw_1.ResetUpdate()
									dw_2.ResetUpdate()
									dw_3.ResetUpdate()
									dw_4.ResetUpdate()
									dw_5.ResetUpdate()
									dw_6.ResetUpdate()
									dw_8.ResetUpdate()
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
	li_planta  		= 	Integer(istr_mant.Argumento[1])
	li_especie		= 	Integer(istr_mant.Argumento[7])
	ll_folio			=	dw_3.Object.lofc_lotefc[1]
	ls_nombrepc		=	gstr_us.computador
	If dw_3.Update(True, False) = 1 Then
		If lds_detalleMovto.Update(True,False) = 1 Then
			If dw_4.Update(True, False) = 1 Then
				If dw_2.Update(True, False) = 1 Then
					If dw_1.Update(True, False) = 1 Then
						If dw_6.Update(True, False) = 1 Then
							If dw_5.Update(True, False) = 1 Then
								If dw_8.Update(True, False) = 1 Then
									UPDATE	dbo.spro_lotescorrelequipo
										SET	loco_ultcom		=	:ll_Folio
									 WHERE	plde_codigo		=	:li_planta
										AND	espe_codigo		=	:li_especie
										AND 	Upper(equi_nombre) 	=	Upper(:ls_nombrepc)
									 USING	sqlca;
	
									Commit;
				
									If sqlca.SQLCode <> 0 Then
										F_ErrorBaseDatos(sqlca, This.Title)
									Else
										lb_Retorno	=	True
			
										dw_1.ResetUpdate()
										dw_2.ResetUpdate()
										dw_3.ResetUpdate()
										dw_4.ResetUpdate()
										dw_5.ResetUpdate()
										dw_6.ResetUpdate()
										dw_8.ResetUpdate()
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

sqlca.AutoCommit	=	ib_AutoCommit

Destroy lds_detalleMovto

RETURN lb_Retorno
end function

public function boolean existemovimiento (long al_numero);Integer	li_tiponum, li_planta, li_tipodocto, li_Tipo, li_cliente
Long     ll_nrodocto

li_planta	=	dw_2.Object.plde_codigo[1]
li_tiponum	=	Integer(istr_mant.argumento[2])
li_Tipo		=	dw_2.Object.mfco_tipdoc[1]
li_cliente	=	dw_2.Object.clie_codigo[1]

SELECT	mfco_tipdoc, mfco_docrel
	INTO	:li_tipodocto, :ll_nrodocto
	FROM	dbo.spro_movtofrutacomenca
	WHERE	plde_codigo	=	:li_planta
	AND	tpmv_codigo	=	:li_tiponum
	AND	mfco_numero	=	:al_numero
	AND	mfco_tipdoc	=	:li_Tipo 
	AND 	clie_codigo	=	:li_cliente;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_movtofrutacomenca")
	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN
	RETURN False
END IF

istr_Mant.Argumento[4] = String(li_tipodocto)
istr_Mant.Argumento[5] = String(ll_nrodocto)

RETURN True
end function

public function boolean existelote (long al_lote);Integer	li_Cantidad, li_Planta, li_Especie, li_tipomovto, li_camara, li_tipoenva, li_envase,&
         li_tipodoc, li_secuencia, li_cliente
Long		ll_secuen, ll_numero, ll_lote, ll_docrel
Double   ld_bultos,ld_kilos

dw_2.AcceptText()

li_Planta		=	Integer(istr_Mant.Argumento[1])
li_Especie		=	Integer(istr_Mant.Argumento[7])
li_tipomovto   =  integer(istr_Mant.Argumento[2])
ll_numero 	 	=  integer(istr_Mant.Argumento[3])
li_cliente		=	dw_2.Object.clie_codigo[1]

If isnull(li_especie) or li_especie=0 Then
	MessageBox("Atención", "Primero debe Seleccionar Una Orden de Proceso o un Numero de Movimiento.")
	Return FALSE
End If		

If al_lote <> 0 Then
  SELECT	Max(lfcd_secuen)
	 INTO	:li_secuencia
	 FROM	dbo.spro_movtofrutacomdeta
	WHERE	lofc_pltcod	=	:li_Planta
	  and lofc_espcod =  :li_especie
	  and lofc_lotefc =  :al_lote
	  and tpmv_codigo =  :li_tipomovto
	  and mfco_numero =  :ll_numero;

 /**/
  SELECT	mfcd_bulent, mfcd_kgnent
	 INTO	:ld_bultos, :ld_kilos 
	 FROM	dbo.spro_movtofrutacomdeta
	WHERE	lofc_pltcod	=	:li_Planta
	  and lofc_espcod =  :li_especie
	  and lofc_lotefc =  :al_lote
	  and tpmv_codigo =  :li_tipomovto
	  and mfco_numero =  :ll_numero
	  and lfcd_secuen =  :li_secuencia;
  
	If SqlCa.SQLCode = -1 Then
		F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_lotesfrutacomdeta")
		Return False
	ElseIf SqlCa.SQLCode = 100 Then
		Return FALSE
	End If
	
	dw_3.Object.lofc_totbul[1] = ld_bultos
	dw_3.Object.lofc_totkil[1] = ld_Kilos
				 
	If dw_3.Retrieve(li_planta, li_Especie, al_lote) =  -1 Then
		MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.")
		Return FALSE
		
	 ElseIf dw_4.Retrieve(li_planta,li_especie, al_lote) = -1 AND &
			  dw_5.Retrieve(li_planta,li_tipomovto, ll_numero,1, li_cliente) = -1 Then
			  
				MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.")
				Return FALSE
	 Else
			If dw_4.Rowcount() > 0 Then
				dw_4.SetRedraw(False)
				
				pb_grabar.Enabled   = TRUE
				pb_imprimir.Enabled = TRUE
				
				li_tipodoc	=	dw_2.Object.mfco_tipdoc[1]
				ll_docrel	=	dw_2.Object.mfco_docrel[1]

				dw_4.SetFilter("")
				dw_4.Filter()

				dw_4.SetFilter("lfcd_tipdoc = " + String(li_tipodoc) + " And lfcd_docrel = " + String(ll_docrel))
				dw_4.Filter()
				
				il_fila = 1
				
				dw_4.SetRow(1)
				dw_4.SetFocus()
				dw_4.SetRedraw(TRUE)
				
			End If
			Return TRUE
	 End If
Else
	
  SELECT	Max(lofc_lotefc)
	 INTO	:ll_lote
	 FROM	dbo.spro_movtofrutacomdeta
	WHERE	plde_codigo =  :li_Planta
	  AND lofc_pltcod	=	:li_Planta
	  and lofc_espcod =  :li_especie
	  and tpmv_codigo =  :li_tipomovto
	  and mfco_numero =  :ll_numero;
	 
	 Istr_Mant.Argumento[6] = String(ll_lote)
	 Return TRUE	
End If
end function

public subroutine habilitaingreso (string as_columna);Date			ld_fecha
Time			lt_hora
Boolean		lb_estado = True

dw_2.AcceptText()
dw_3.AcceptText()
dw_2.AcceptText()
	
IF istr_Mant.Argumento[8] = "1" THEN

	IF as_Columna <> "plde_codigo" AND &
		(dw_2.Object.plde_codigo[1] = 0 OR IsNull(dw_2.Object.plde_codigo[1])) THEN
		lb_Estado = False
	END IF

	IF as_Columna <> "mfco_fecmov" AND &
		(dw_2.Object.mfco_fecmov[1] = ld_Fecha OR IsNull(dw_2.Object.mfco_fecmov[1])) THEN
		lb_Estado = False
	END IF
	
	IF as_Columna <> "mfco_tipdoc" AND &
		(dw_2.Object.mfco_tipdoc[1] = 0 OR IsNull(dw_2.Object.mfco_tipdoc[1])) THEN
		lb_Estado = False
	END IF
		
	IF as_Columna <> "mfco_docrel" AND &
		(dw_2.Object.mfco_docrel[1] = 0 OR IsNull(dw_2.Object.mfco_docrel[1])) THEN
		lb_Estado = False
	END IF

//	IF as_Columna <> "prod_codigo" AND &
//		(dw_3.Object.prod_codigo[1] = 0 OR IsNull(dw_3.Object.prod_codigo[1])) THEN
//		lb_Estado = False
//	END IF

	IF as_Columna <> "lofc_lotefc" AND &
		(dw_3.Object.lofc_lotefc[1] = 0 OR IsNull(dw_3.Object.lofc_lotefc[1])) THEN
		lb_Estado = False
	END IF
			
	
ELSE
	
	IF as_Columna <> "plde_codigo" AND &
		(dw_2.Object.plde_codigo[1] = 0 OR IsNull(dw_2.Object.plde_codigo[1])) THEN
		lb_Estado = False
	END IF
	
	IF as_Columna <> "mfco_fecmov" AND &
		(dw_2.Object.mfco_fecmov[1] = ld_Fecha OR IsNull(dw_2.Object.mfco_fecmov[1])) THEN
		lb_Estado = False
	END IF
	
	IF as_Columna <> "mfco_horaen" AND &
		(time(dw_2.Object.mfco_horaen[1]) = time(lt_Hora) OR IsNull(dw_2.Object.mfco_horaen[1])) THEN
		lb_Estado = False
	END IF

	IF as_Columna <> "cami_clasifi" AND &
		(dw_2.Object.cami_clasifi[1] = 0 OR IsNull(dw_2.Object.cami_clasifi[1])) THEN
		lb_Estado = False
	END IF

	IF as_Columna <> "cami_patent" AND &
		(dw_2.Object.cami_patent[1] = "" OR IsNull(dw_2.Object.cami_patent[1])) THEN
		lb_Estado = False
	END IF
	
	IF as_Columna <> "plde_coorde" AND &
		(dw_2.Object.plde_coorde[1] = 0 OR IsNull(dw_2.Object.plde_coorde[1])) THEN
		lb_Estado = False
	END IF

	IF as_Columna <> "cod_especie" AND &
		(dw_2.Object.cod_especie[1] = 0 OR IsNull(dw_2.Object.cod_especie[1])) THEN
		lb_Estado = False
	END IF
	
	IF as_Columna <> "prod_codigo" AND &
		(dw_3.Object.prod_codigo[1] = 0 OR IsNull(dw_3.Object.prod_codigo[1])) THEN
		lb_Estado = False
	END IF

	IF as_Columna <> "lofc_lotefc" AND &
		(dw_3.Object.lofc_lotefc[1] = 0 OR IsNull(dw_3.Object.lofc_lotefc[1])) THEN
		lb_Estado = False
	END IF
	
END IF

	
tab_1.tp_1.Enabled	=	lb_Estado
tab_1.tp_2.Enabled	=	lb_Estado

pb_ins_det.Enabled	=	lb_Estado

IF lb_estado THEN
	Habilitaencab(FALSE)
END IF	

end subroutine

public function boolean borradetallemovto (integer ai_planta, integer ai_especie, long al_lote, integer ai_secuen);Long ll_Fila, ll_borra

ll_Borra=0
ll_fila=1

Do While ll_fila<=dw_1.RowCount()
	
	IF  dw_1.Object.lofc_pltcod[ll_fila] = ai_planta  AND &
	    dw_1.Object.lofc_espcod[ll_fila] = ai_especie AND &
		 dw_1.Object.lofc_lotefc[ll_fila] = al_lote    AND &
		 dw_1.Object.lfcd_secuen[ll_fila] = ai_secuen  THEN
		 
		 IF dw_1.deleterow(ll_fila) = 1 THEN
			ll_borra++
		 END IF
		
	ELSE
		ll_fila++
	END IF	
	
LOOP

IF ll_Borra>0 THEN
	RETURN TRUE
ELSE
	RETURN FALSE
END IF
end function

public function integer tiporecepcion (string as_recepcion, date ad_fechamovto);Integer li_lote

IF as_Recepcion = '1' THEN
	CHOOSE CASE gstr_paramplanta.PoolVenta
		CASE 1
			li_Lote = 0
			
		CASE 2
			li_lote = f_SemanaFecha(datetime(ad_fechamovto))

		CASE 3
			li_lote = f_DiaFecha(datetime(ad_fechamovto))

		CASE 4
			li_lote = f_anoFecha(datetime(ad_fechamovto))
		
		CASE 5
			li_Lote = 0
			
	END CHOOSE
		
ELSEIF as_Recepcion = '2' THEN
	CHOOSE CASE gstr_paramplanta.PoolRetiro
		CASE 1
			li_Lote = 0
				
		CASE 2
			li_Lote = Integer(istr_Mant.Argumento[5])			
		
		CASE 3
			li_Lote = f_Semanafecha(datetime(ad_fechamovto))

		CASE 4
			li_Lote = 0	

	END CHOOSE
END IF			
 
RETURN li_Lote
end function

public function long entreganrolote ();Integer li_planta, li_especie, Existelote=1
Long    ll_lote=0, ll_ultlote

li_Planta	= Integer(Istr_Mant.Argumento[1])
li_Especie	= Integer(Istr_Mant.Argumento[7])

	SELECT ulc.loco_ultcom  into :ll_ultlote
	  FROM dbo.spro_lotescorrel ulc
	 WHERE ulc.espe_codigo = :li_especie
		AND ulc.plde_codigo = :li_planta;
	
	IF isnull(ll_ultlote) THEN ll_ultlote=0
	
	ll_ultlote = ll_ultlote + 1
	IF (is_Recepcion = "1") AND &
		(gstr_paramplanta.PoolVenta = 2 OR gstr_paramplanta.PoolVenta = 3) THEN
		ll_lote = ll_ultlote + Tiporecepcion(is_recepcion, iuo_doctointernopack.ld_fecha)
		
	ELSEIF (is_Recepcion = "2") AND (gstr_paramplanta.PoolRetiro = 2) THEN
			 ll_lote = ll_ultlote + Tiporecepcion(is_recepcion, iuo_doctointernopack.ld_fecha)
			 
		ELSE
			 ll_lote = Tiporecepcion(is_recepcion, iuo_doctointernopack.ld_fecha) 
	END IF
RETURN ll_lote

end function

public subroutine buscareproceso ();Str_Busqueda	lstr_Busq
Long				ll_lote
String			ls_Null

SetNull(ls_Null)

lstr_Busq.Argum[1]	=	istr_Mant.Argumento[4]	// Tipo de Proceso

OpenWithParm(w_busqueda_doctointernopack, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

IF lstr_Busq.Argum[2] <> "" THEN
	dw_2.SetItem(1, "mfco_tipdoc", Integer(lstr_Busq.Argum[2]))
	dw_2.SetItem(1, "mfco_docrel", Long(lstr_Busq.Argum[3]))

	IF iuo_doctointernopack.Existe(Integer(istr_Mant.Argumento[13]), &
											 Integer(istr_Mant.Argumento[1]), &
											 Integer(lstr_Busq.Argum[2]), &
											 Long(lstr_Busq.Argum[3]), True, SqlCa) THEN

		dw_2.SetItem(1, "espe_codigo", iuo_doctointernopack.Especie)
		dw_3.SetItem(1, "lofc_espcod", iuo_doctointernopack.Especie)
		dw_3.Enabled = True
	END IF

	istr_Mant.Argumento[4]	= lstr_Busq.Argum[2]
	istr_Mant.Argumento[5]	= lstr_Busq.Argum[3]
	istr_Mant.Argumento[7]	= String(iuo_doctointernopack.Especie)
   IF Integer(lstr_Busq.Argum[2])=5 THEN istr_mant.argumento[2] = "6"
	IF Integer(lstr_Busq.Argum[2])=6 THEN istr_mant.argumento[2] = "7"
	IF Integer(lstr_Busq.Argum[2])=8 THEN istr_mant.argumento[2] = "8"
	
	iuo_prodempresa.existe(gstr_paramplanta.productorempresa,false,sqlca)
		
	dw_3.SetItem(1, "prod_codigo", gstr_paramplanta.productorempresa)
	dw_3.SetItem(1, "prod_nombre", iuo_prodempresa.nombre )
	
	IF buscamovto(integer(istr_Mant.Argumento[4]),Long(lstr_busq.argum[5])) THEN	
		IF (istr_Mant.Argumento[12] = "1" AND gstr_paramplanta.PoolVenta <> 1 AND &
														 gstr_paramplanta.PoolVenta <> 5) OR &
			(istr_Mant.Argumento[12] = "2" AND gstr_Paramplanta.PoolRetiro <> 1 AND &
														 gstr_Paramplanta.PoolRetiro <> 4) THEN
			Istr_Mant.Argumento[6] = String(Nuevolote(0) + TipoRecepcion(istr_Mant.Argumento[11], &
															  Date(Mid(String(iuo_doctointernopack.ld_fecha),1,10))))
			dw_3.Object.lofc_lotefc[1] = Long(Istr_Mant.Argumento[6])	
		ELSE
			ExisteLote(0)
		END IF
		
		IF Existelote(Integer(Istr_Mant.Argumento[6])) AND istr_Mant.Argumento[6] <> "0" THEN
			TriggerEvent("ue_recuperadatos")
		ELSEIF (Istr_Mant.Argumento[12] = "1") AND &
				 gstr_paramplanta.PoolVenta = 5 OR gstr_paramplanta.PoolVenta = 1 THEN 
				 dw_3.Object.lofc_lotefc[1] = Integer("")
				 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
		ELSEIF (Istr_Mant.Argumento[12] = "2") AND &
				 gstr_paramplanta.PoolRetiro = 1 OR gstr_paramplanta.PoolRetiro = 4 THEN 
				 dw_3.Object.lofc_lotefc[1] = Integer("")
				 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
		ELSE
				 dw_3.Object.lofc_lotefc[1] = Integer(Istr_Mant.Argumento[6])
				 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
				 Habilitaencab(False)
				 dw_5.Retrieve(Integer(istr_Mant.Argumento[1]), &
									Integer(istr_Mant.Argumento[2]), &
										Long(istr_Mant.Argumento[3]),1)
				 
		END IF
	END IF
	istr_Mant.Argumento[5]	= lstr_Busq.Argum[5]
	istr_Mant.Argumento[7]	= String(iuo_doctointernopack.Especie)
END IF

dw_2.SetColumn("mfco_numero")
dw_2.SetFocus()



end subroutine

public function boolean retiraprod (long al_productor, integer ai_especie, integer ai_variedad, integer ai_categoria, date ad_fechalot);Long  ll_Productor

SELECT   prod_codigo
INTO		:ll_productor
FROM		dbo.spro_pararetiroprod as prp
WHERE		prp.prod_codigo =: al_productor
AND		prp.espe_codigo =: ai_especie
AND		prp.vari_codigo =: ai_variedad
AND      prp.cate_codigo =: ai_categoria
AND      prp.prep_fecini <: ad_fechalot;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_lotesfrutacomdeta")

	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN

	RETURN False
END IF

RETURN True

end function

public function boolean conexionexistencia ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta

IF ib_ConectadoExistencia THEN
	DISCONNECT USING sqlexi;
END IF

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

SELECT cone_nomodb,cone_nomser,cone_nombas,
		cone_nodbms,cone_nomusu,cone_passwo  
 INTO :ls_nomodb,:ls_nomser,:ls_nombas,
		:ls_nodbms,:ls_Usuario,:ls_Password
 FROM dbo.prodconectividad   
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

public function long nuevofoliolote ();Long 		ll_Folio
Integer	li_especie, ll_planta
String	ls_nombrepc

ll_planta	=	Long(istr_Mant.Argumento[1])
li_especie	=	Long(istr_mant.Argumento[7])
ls_nombrepc	=	gstr_us.computador

SELECT	loco_ultcom
  INTO	:ll_Folio
  FROM	dbo.spro_lotescorrelequipo
 WHERE	plde_codigo = 	:ll_Planta
	AND	espe_codigo = 	:li_especie
	AND 	Upper(equi_nombre) 	=	Upper(:ls_nombrepc)
 USING	sqlca;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Actualización Tabla Correlativos de Lotes")
	Message.DoubleParm = -1
	SQLCA.AutoCommit	=	ib_AutoCommit
	RETURN 0
END IF

ll_Folio	=	ll_Folio + 1


RETURN ll_Folio
end function

public subroutine imprime_tarja ();SetPointer(HourGlass!)

Long		fila, ll_lote
Integer  li_plantalote, li_espelote

istr_info.titulo	= "IDENTIFICACION FRUTA COMERCIAL"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_identificacion_fruta_comercial_1"
vinf.dw_1.SetTransObject(sqlca)

li_plantalote = dw_3.Object.lofc_pltcod[1]
li_espelote   = dw_3.Object.lofc_espcod[1]
ll_lote       = dw_3.Object.lofc_lotefc[1]

fila = vinf.dw_1.Retrieve(integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]), &
                          Long(istr_mant.argumento[3]), li_plantalote, li_espelote, ll_lote, &
								  dw_2.Object.clie_codigo[1],il_secuencia)

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
end subroutine

on w_maed_movtofrutacomer_reproceso.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_6=create dw_6
this.dw_8=create dw_8
this.tab_1=create tab_1
this.dw_exiencab=create dw_exiencab
this.dw_exideta=create dw_exideta
this.dw_exidetaborra=create dw_exidetaborra
this.dw_exismovtodetanulos=create dw_exismovtodetanulos
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_6
this.Control[iCurrent+3]=this.dw_8
this.Control[iCurrent+4]=this.tab_1
this.Control[iCurrent+5]=this.dw_exiencab
this.Control[iCurrent+6]=this.dw_exideta
this.Control[iCurrent+7]=this.dw_exidetaborra
this.Control[iCurrent+8]=this.dw_exismovtodetanulos
end on

on w_maed_movtofrutacomer_reproceso.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_6)
destroy(this.dw_8)
destroy(this.tab_1)
destroy(this.dw_exiencab)
destroy(this.dw_exideta)
destroy(this.dw_exidetaborra)
destroy(this.dw_exismovtodetanulos)
end on

event open;String				ls_Movto

x	= 0
y	= 0
This.Height	= 2520
im_menu		= m_principal

iuo_variedades			=	Create uo_variedades
iuo_categorias			=	Create uo_categorias
iuo_planta				=	Create uo_plantadesp
iuo_productores		=	Create uo_productores
iuo_prodempresa		=	Create uo_productores
iuo_doctointernopack	=	Create uo_doctointernopack
iuo_transportista		=	Create uo_transportista
iuo_Camion				=	Create uo_camiones
iuo_TipoDocto			=	Create uo_tipodoctoplanta
iuo_TipoMovtoFruta	=	Create uo_tipomovtofruta
iuo_TipoMovtoEnva	=  Create uo_tipomovtofruta		
iuo_cliente 				=	Create uo_clientesprod
iuo_calicosechero		=  Create uo_calicosechero

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

ls_Movto	= Message.StringParm

dw_4							=	tab_1.tp_1.dw_detalle
dw_5							=	tab_1.tp_2.dw_envases
is_Recepcion				=  Mid(ls_Movto,2,1)
istr_Mant.Argumento[12] =  Mid(ls_Movto,2,1)
istr_Mant.Argumento[8]	=	Mid(ls_Movto,1,1)

dw_4.ModIfy("datawindow.message.title='Error '+ is_titulo")
dw_4.SetRowFocusIndicator(Hand!)
dw_4.ModIfy("DataWindow.Footer.Height = 110")

dw_5.ModIfy("datawindow.message.title='Error '+ is_titulo")
dw_5.SetRowFocusIndicator(Hand!)
dw_5.ModIfy("DataWindow.Footer.Height = 110")

istr_Mant.dw						=	dw_4
istr_Mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

If istr_Mant.Argumento[8]	=	"2" Then
	This.Title						=	"Recepción Packing Particular"
	dw_2.DataObject			=	"dw_mant_movtofrutacomenca_recep"
	dw_4.DataObject			=	"dw_mues_lotesfrutacomdeta_recep"
	dw_2.Height					=	752
	dw_3.y						=	776
	istr_Mant.Argumento[2]	=	"3"	// Tipo Movto. (Recep. de Packing Particular)
Else
	istr_Mant.Argumento[2]	=	"6"	// Tipo Movto. (Recep. de Re-Proceso)
	istr_Mant.Argumento[4]	=	"5"	// Tipo de Proceso (Re-Proceso)
End If

istr_Mant.Argumento[13]	=	String(gi_CodExport)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_8.SetTransObject(sqlca)

dw_2.GetChild("plde_codigo", idwc_planta)
dw_2.GetChild("espe_codigo", idwc_especie)
dw_2.GetChild("frio_tipofr", idwc_tipofrio)
dw_2.GetChild("pefr_codigo", idwc_periodofrio)
dw_3.GetChild("cate_codigo", idwc_categoria)
dw_4.GetChild("enva_tipoen", idwc_tipoenvase)

idwc_planta.SetTransObject(SqlCa)
idwc_especie.SetTransObject(SqlCa)
idwc_tipofrio.SetTransObject(SqlCa)
idwc_periodofrio.SetTransObject(SqlCa)
idwc_categoria.SetTransObject(SqlCa)
idwc_tipoenvase.SetTransObject(SqlCa)

idwc_planta.Retrieve()

If idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 Then
	MessageBox("Atención", "Falta Registrar Especie")
	idwc_especie.InsertRow(0)
End If

If idwc_tipofrio.Retrieve() = 0 Then
	MessageBox("Atención", "Falta Registrar Tipo de Frío")
	idwc_tipofrio.InsertRow(0)
End If

If idwc_periodofrio.Retrieve() = 0 Then
	MessageBox("Atención", "Falta Registrar Periodo de Frío")
	idwc_periodofrio.InsertRow(0)
End If

If idwc_categoria.Retrieve() = 0 Then
	MessageBox("Atención", "Falta Registrar Categoría")
	idwc_categoria.InsertRow(0)
Else
	idwc_categoria.SetSort("cate_nombre A")
	idwc_categoria.Sort()

	idwc_categoria.SetFilter("cate_embala = 0")
	idwc_categoria.Filter()
End If

If idwc_tipoenvase.Retrieve() = 0 Then
	MessageBox("Atención", "Falta Registrar Tipo de Envase")
	idwc_tipoenvase.InsertRow(0)
Else
	idwc_TipoEnvase.SetSort("tien_nombre A")
	idwc_TipoEnvase.Sort()
	
	idwc_TipoEnvase.SetFilter("tien_usoenv = 1")
	idwc_TipoEnvase.Filter()
End If

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, 	This.Title, "Acceso a Aplicación", 1)

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)	// Planta
istr_Mant.Argumento[3]	=	""

dw_3.Object.prod_codigo.Protect					=	0
dw_3.Object.prod_codigo.Color 					= 	0
dw_3.Object.prod_codigo.BackGround.Color 	= 	RGB(255,255,255)
dw_3.Object.b_prodrot.Visible						=	1
		
buscar	= "Turno:Nlfcd_turno,Tipo Envase:Nenva_tipoen,Envase:Nenva_codigo,Calibre:Srefe_calibr"
ordenar	= "Turno:lfcd_turno,Tipo Envase:enva_tipoen,Envase:enva_codigo,Calibre:refe_calibr"

end event

event ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, ll_docrel
Integer  li_tipodoc


If integer(istr_mant.argumento[8]) = 1 Then

	DO
		dw_2.SetRedraw(False)
		dw_3.SetRedraw(False)
	   dw_4.SetRedraw(False)
		
		If dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), &
							  Integer(istr_Mant.Argumento[2]), &
							  Long(istr_Mant.Argumento[3]), &
							  Integer(istr_Mant.Argumento[13])) = -1 OR &
			dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), &
							  Integer(istr_Mant.Argumento[7]), &
							  Long(istr_Mant.Argumento[6])) =  -1 Then
			respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
		Else
	
			DO
				If dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
									  Integer(istr_Mant.Argumento[2]), &
									  Long(istr_Mant.Argumento[3]), &
							  		  Integer(istr_Mant.Argumento[13])) = -1  OR &
					dw_4.Retrieve(Integer(istr_Mant.Argumento[1]), &
									  Integer(istr_Mant.Argumento[7]), &
									  Long(istr_Mant.Argumento[6])) = -1  OR &
					dw_5.Retrieve(Integer(istr_Mant.Argumento[1]), &
									  Integer(istr_Mant.Argumento[2]), &
					 	  			  Long(istr_Mant.Argumento[3]),1, &
									  Integer(istr_Mant.Argumento[13])) = -1 OR &
					dw_8.Retrieve(Integer(istr_Mant.Argumento[1]), &
									  Integer(istr_Mant.Argumento[2]), &
					 	  			  Long(istr_Mant.Argumento[3]),2, &
							  		  Integer(istr_Mant.Argumento[13])) = -1 OR &					  
					dw_6.Retrieve(Integer(istr_Mant.Argumento[1]), &
									  Integer(istr_Mant.Argumento[2]), &
									  Long(istr_Mant.Argumento[3])) = -1 Then
						respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
														Information!, RetryCancel!)
						pb_grabar.Enabled		=	False
				Else
					
					tab_1.tp_1.Enabled 	=	True
					tab_1.tp_2.Enabled 	=	True
				
					pb_eliminar.Enabled	=	True
					pb_grabar.Enabled		=	True
					pb_imprimir.Enabled	=	True
					pb_ins_det.Enabled	=	True
		
					HabilitaEncab(False)
						
					pb_eli_det.Enabled	=	True
					If dw_4.RowCount() > 0 Then
						li_tipodoc	=	dw_2.Object.mfco_tipdoc[1]
						ll_docrel		=	dw_2.Object.mfco_docrel[1]

						dw_4.SetFilter("")
						dw_4.Filter()
					   If integer(istr_mant.argumento[8]) = 1 Then
						   dw_4.SetFilter("lfcd_tipdoc = " + String(li_tipodoc) + " And lfcd_docrel = " + String(ll_docrel))
						   dw_4.Filter()
						Else
							dw_4.SetFilter("If (isnull(lfcd_tipdoc) and isnull(lfcd_docrel),1,0) = 1 ")
						   dw_4.Filter()
						End If	
					End If
					
					If dw_5.RowCount() > 0 Then il_NumEnva	=	dw_5.Object.meen_numero[1]
					
					il_fila = 1				
					dw_4.SetRow(1)
					dw_4.SelectRow(1,True)
					dw_4.SetFocus()
					istr_Mant.Argumento[15]	=	String(dw_2.Object.vari_codigo[1])
				End If

			LOOP WHILE respuesta = 1
	
			If respuesta = 2 Then Close(This)
			
		End If
		dw_2.SetRedraw(True)
		dw_3.SetRedraw(True)
		dw_4.SetRedraw(TRUE)
	LOOP WHILE respuesta = 1
	
	If respuesta = 2 Then Close(This)
	
Else
	
	DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	dw_3.SetRedraw(False)
	dw_3.Reset()
   dw_4.SetRedraw(False)
	
	If dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), &
						  Integer(istr_Mant.Argumento[2]), &
						  Long(istr_Mant.Argumento[3])) = -1 Then
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	Else

	 	DO
			If dw_2.RowCount() > 0 Then

				If dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), &
						  Integer(istr_Mant.Argumento[7]), &
						  Long(istr_Mant.Argumento[6])) =  -1 Then
					respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
				Else						
					If dw_3.RowCount() = 0 Then
						dw_3.InsertRow(0)
					Else
						 dw_4.Retrieve(Integer(istr_Mant.Argumento[1]), &
											Integer(istr_Mant.Argumento[7]), &
											Long(istr_Mant.Argumento[6]))
						
						If dw_4.RowCount() > 0 Then
																	
							dw_4.SetFilter("")
							dw_4.Filter()
							dw_4.SetFilter("If (isnull(lfcd_tipdoc) and isnull(lfcd_docrel),1,0) = 1 ")
							dw_4.Filter()
						End If
						
						il_fila = 1				
						dw_4.SetRow(1)
						dw_4.SelectRow(1,True)
						dw_4.SetFocus()
					End If	
				End If
			End If
			
			If dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  Long(istr_Mant.Argumento[3])) = -1  OR &
				dw_5.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
				 	  			  Long(istr_Mant.Argumento[3]),1) = -1 OR &
				dw_8.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
				 	  			  Long(istr_Mant.Argumento[3]),2) = -1 OR &					  
				dw_6.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  Long(istr_Mant.Argumento[3])) = -1 Then
								  
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			Else
					tab_1.tp_1.Enabled 	=	True
					tab_1.tp_2.Enabled 	=	True				
					
					pb_eliminar.Enabled	=	True
					pb_grabar.Enabled		=	True
					pb_imprimir.Enabled	=	True
					pb_ins_det.Enabled	=	True
	
					HabilitaEncab(False)

					If dw_4.RowCount() > 0 Then
						li_tipodoc	=	dw_2.Object.mfco_tipdoc[1]
								
						dw_4.SetFilter("")
						dw_4.Filter()
						dw_4.SetFilter("If (isnull(lfcd_tipdoc) and isnull(lfcd_docrel),1,0) = 1 ")
						dw_4.Filter()
					End If
				
					pb_eli_det.Enabled	=	True
					il_fila = 1				
					dw_4.SetRow(1)
					dw_4.SelectRow(1,True)
					dw_4.SetFocus()
				
			End If
		LOOP WHILE respuesta = 1

		If respuesta = 2 Then Close(This)
	End If
	dw_2.SetRedraw(True)
	dw_3.SetRedraw(True)
	dw_4.SetRedraw(True)
LOOP WHILE respuesta = 1

If respuesta = 2 Then Close(This)
End If
end event

event ue_nuevo;
Long		ll_modif1, ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	 =	dw_4.GetNextModified(0, Primary!)
			ll_modif1	+=	dw_2.GetNextModified(0, Primary!)
			ll_modif1	+=	dw_5.GetNextModified(0, Primary!)
			
			IF dw_4.RowCount() > 0 AND ll_modif1 > 0 THEN
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

dw_4.SetFilter("")
dw_4.Filter()

dw_1.Reset()
dw_4.Reset()
dw_5.reset()
dw_6.reset()
dw_8.reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False

dw_2.Enabled				=	True
dw_3.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_3.SetRedraw(False)
dw_3.Reset()
dw_3.InsertRow(0)
dw_3.SetRedraw(True)

dw_2.SetFocus()

HabilitaEncab(True)

is_rut 			= ""

dw_2.Object.plde_codigo[1]		=	Integer(istr_Mant.Argumento[1])
dw_2.Object.clie_codigo[1]		=	gi_CodExport
dw_2.Object.tpmv_codigo[1]	=	Integer(istr_Mant.Argumento[2])
dw_2.Object.mfco_fecmov[1]	=	Date(Mid(String(Today()),1,10))

IF istr_Mant.Argumento[8]	=	"1" THEN
	dw_2.Object.mfco_tipdoc[1] 	= 	Integer(istr_Mant.Argumento[4])
ELSE
	dw_2.Object.cami_clasifi[1]	= 	2
	dw_2.Object.mfco_horaen[1] 	= 	Time(Now())
	dw_2.Object.cod_especie[1] 	= 	gstr_ParamPlanta.CodigoEspecie
	dw_3.Object.lofc_espcod[1] 	= 	gstr_ParamPlanta.CodigoEspecie
	dw_3.Object.clie_codigo[1] 		= 	Integer(istr_Mant.Argumento[13])
	istr_Mant.Argumento[7]			=	String(gstr_ParamPlanta.CodigoEspecie)
	dw_2.Object.mfco_tipdoc[1] 	= 	1
	
	dw_2.Object.mfco_tipdoc.Protect 					=	1
	dw_2.Object.mfco_tipdoc.Color 					=	RGB(255,255,255)
	dw_2.Object.mfco_tipdoc.BackGround.Color 	=	553648127
	
END IF

istr_mant.argumento[3] 		=	""
istr_mant.argumento[11] 	=	""

dw_3.Object.lofc_pltcod[1] =	Integer(istr_Mant.Argumento[1])

end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;istr_Mant.Borra		= False
istr_Mant.Agrega	= True

If tab_1.SelectedTab = 1 Then
	istr_Mant.Argumento[10]	=	String(dw_3.Object.prod_codigo[1])
   	istr_Mant.Argumento[11] 	=	String(dw_2.Object.mfco_fecmov[1],'dd/mm/yyyy')
	istr_Mant.Argumento[15]	= String(iuo_doctointernopack.Variedad)
		
	istr_mant.dw	=	dw_4
	
	OpenWithParm(iw_mantencion, istr_mant)

	If dw_4.RowCount() > 0 Then
		pb_eli_det.Enabled	=	True
	Else
		pb_eli_det.Enabled	=	False
	End If

	dw_4.SetRow(il_fila)
	dw_4.SelectRow(il_fila,True)
Else
	istr_mant.dw	=	dw_5
	istr_mant.argumento[15] 	= istr_mant.argumento[7]
	istr_mant.argumento[7]  	= "0"
	istr_mant.argumento[16]  	= string(iuo_cliente.Codigo)
	
	OpenWithParm(iw_mantencion_2, istr_mant)

   istr_mant.argumento[7] = istr_mant.argumento[15]
	If dw_5.RowCount() > 0 Then
		pb_eli_det.Enabled			=	True
	Else
		pb_eli_det.Enabled			=	False
	End If
	
	dw_5.SetRow(il_Fila)
	dw_5.SelectRow(il_Fila, True)
End If

If dw_4.RowCount() > 0 AND dw_5.RowCount() > 0 Then HabilitaEncab(False)

If dw_4.RowCount() > 0 AND dw_5.RowCount() > 0 AND Not pb_eliminar.Enabled Then
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
End If

istr_mant.Borra	=	False
istr_mant.Agrega	=	True


end event

event ue_borra_detalle();Integer li_planta, li_especie, li_secuencia, li_tipoen, li_envase
String  ls_calida
Long    ll_lote, ll_fila, ll_filaborra

SetPointer(HourGlass!)

ib_Borrar				=	True
Message.DoubleParm	=	0
istr_mant.Borra		=	True
istr_mant.Agrega		=	False

w_main.SetMicroHelp("Validando la eliminación de detalle...")

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF tab_1.SelectedTab = 1 THEN
	
	IF dw_4.rowcount() < 1 THEN RETURN
	
	istr_Mant.dw	=	dw_4
	
	li_planta  		=	dw_4.Object.lofc_pltcod[il_fila]
	li_especie 		=	dw_4.Object.lofc_espcod[il_fila]
	ll_lote    		=	dw_4.Object.lofc_lotefc[il_fila]
	li_secuencia	=	dw_4.Object.lfcd_secuen[il_fila]
	
	OpenWithParm(iw_mantencion, istr_mant)

	istr_mant = Message.PowerObjectParm

	IF istr_mant.respuesta = 1 THEN
		IF borradetallemovto(li_planta,li_especie,ll_lote,li_secuencia) THEN
			IF dw_4.DeleteRow(0) = 1 THEN
				ib_borrar = False
			
				W_main.SetMicroHelp("Borrando Registro...")
				il_fila = dw_4.GetRow()
				dw_4.SetRow(il_fila)
				dw_4.SelectRow(il_fila, True)
				SetPointer(Arrow!)
			ELSE
				ib_borrar = False
				MessageBox(This.Title,"No se puede borrar actual registro.")
			END IF
		
			IF dw_4.RowCount() = 0 THEN
				habilitaencab(True)
				pb_eli_det.Enabled = False
			END IF
		ELSE
			ib_borrar = False
			MessageBox(This.Title,"No se puede borrar registro Actual.")
		END IF
	END IF

	istr_mant.borra	= False

ELSE
	istr_Mant.dw	=	dw_5
	
	li_tipoen = dw_5.Object.enva_tipoen[il_fila]
	li_envase = dw_5.Object.enva_codigo[il_fila]
	ls_calida = dw_5.Object.cale_calida[il_fila]
	
	OpenWithParm(iw_mantencion_2, istr_Mant)
	
	istr_Mant	=	Message.PowerObjectParm
	
	IF istr_mant.Respuesta = 1 THEN
		
		ll_filaborra = dw_8.Find("enva_tipoen = " + String(li_tipoen) + " AND " + &
		                         "enva_codigo = " + String(li_envase) + " AND " + &
										 "cale_calida = '" + ls_calida + "'",1,dw_8.RowCount())
		 
		IF ll_filaborra > 0 THEN
			dw_8.DeleteRow(ll_filaborra)
		END IF
	
		IF dw_5.DeleteRow(0) = 1 THEN
			ib_Borrar	=	False
			
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_Borrar	=	False
			
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
		IF dw_5.RowCount() = 0 THEN 
			HabilitaEncab(True)
			pb_eli_det.Enabled	=	False
		END IF
	END IF
END IF

istr_mant.Borra	 = False
SetPointer(Arrow!)


end event

event ue_antesguardar;
Long		ll_Fila, ll_Nuevo, ll_FilaMov, ll_Numerolote, ll_TotalBulTos, ll_filaenv, ll_docrel
Integer	li_Exportador, li_Planta, li_TipoMovTo, li_Secuencia, li_especie, &
         li_TipoMovToEnva , li_tipodoc   
Dec{2}	ld_TotalKilos
Boolean	lb_Existe = False, lb_Actualiza_Fruta = False, lb_Actualiza_Envase = True

ib_AuToCommit		=	sqlca.AuToCommit
sqlca.AuToCommit	=	False

li_Planta			=	dw_2.Object.plde_codigo[1]
li_TipoMovTo		=	Integer(istr_Mant.ArgumenTo[2])
li_TipoMovToEnva 	=	43
li_especie			=	Integer(istr_Mant.ArgumenTo[7])
ll_Fila = 1

dw_4.SetRedraw(FALSE)

If NOT RevisaEnvases() Then
	Message.DoubleParm = -1
	dw_4.SetRedraw(TRUE)
	Return
End If

li_tipodoc	=	dw_2.Object.mfco_tipdoc[1]
ll_docrel		=	dw_2.Object.mfco_docrel[1]

dw_4.SetFilter("")
dw_4.Filter()

If dw_2.GetItemStatus(1, 0, Primary!) = NewModIfied! Then
	If il_NumEnva=0 Then
		iuo_TipoMovToEnva.bloqueacorrel()	
		il_NumEnva = iuo_TipoMovToEnva.UltimoCorrelativo(4,li_TipoMovToEnva,li_Planta) 

		If il_NumEnva = 0 OR IsNull(il_NumEnva) Then
		  Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
		  Message.DoubleParm = -1
		  dw_4.SetRedraw(TRUE)
		  Return
		Else
		  lb_Actualiza_Envase = TRUE
		End If
	 End If

	If il_NumFruta=0 Then
	  iuo_TipoMovToFruta.bloqueacorrel()
	  il_NumFruta = iuo_TipoMovToFruta.UltimoCorrelativo(2,li_TipoMovTo,li_Planta) 

	  If il_NumFruta = 0 OR IsNull(il_NumFruta) Then
		 Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
		 Message.DoubleParm = -1
		 dw_4.SetRedraw(TRUE)
		 Return
	  Else
		 lb_Actualiza_Fruta = TRUE	
	  End If
	End If	

	dw_2.Object.mfco_numero[1]	=	il_NumFruta
	istr_Mant.ArgumenTo[3] 			= 	String(il_NumFruta)
	dw_2.Object.tpmv_codigo[1] 	=  li_TipoMovTo
	dw_2.Object.mfco_estmov[1] 	=  1
	
	ll_Fila	=	dw_6.InsertRow(0)

	dw_6.Object.plde_codigo[ll_Fila]		=	dw_2.Object.plde_codigo[1]
	dw_6.Object.tpmv_codigo[ll_Fila]		=	li_TipoMovToEnva
	dw_6.Object.meen_numero[ll_Fila]	=  il_NumEnva
	
   If li_TipoMovTo = 3 Then
		dw_6.Object.plde_coorde[ll_Fila]	=  dw_2.Object.plde_coorde[1]
   Else
		dw_6.Object.plde_coorde[ll_Fila]	=  dw_2.Object.plde_codigo[1]
	End If	
	
	dw_6.Object.meen_fecmov[ll_Fila]	=	dw_2.Object.mfco_fecmov[1]
	//dw_6.Object.tran_codigo[ll_Fila]	=	0 Ahora acepta nulo
	dw_6.Object.meen_modulo[ll_Fila]	=	2
	dw_6.Object.tpmv_codrec[ll_fila] =  li_TipoMovTo
	dw_6.Object.mfge_numero[ll_fila] =  il_NumFruta
	
	//Preguntar el MomenTo de Actualización
	If lb_Actualiza_Fruta  Then iuo_TipoMovToFruta.Actualiza_Correlativo(2,li_Planta,li_TipoMovTo,il_NumFruta) 
	If lb_Actualiza_Envase Then iuo_TipoMovToEnva.Actualiza_Correlativo(4,li_Planta,li_TipoMovToEnva,il_NumEnva) 
	///////////////////////////////////////
Else
	il_NumFruta					=	dw_2.Object.mfco_numero[1]
	istr_Mant.ArgumenTo[3] 	= 	String(il_NumFruta)

End If

ll_numerolote = dw_3.Object.lofc_lotefc[1]

SELECT	IsNull(Max(lfcd_secuen), 0) + 1
	INTo	:li_Secuencia
	FROM	dbo.spro_lotesfrutacomdeta
	WHERE	lofc_pltcod	=	:li_Planta
	AND	lofc_espcod	=	:li_especie
	AND   lofc_lotefc =  :ll_Numerolote;

For ll_Fila = 1 To dw_4.RowCount()
	If dw_4.GetItemStatus(ll_Fila, 0, Primary!) = NewModIfied! Then
		dw_4.Object.lfcd_secuen[ll_Fila]	=	li_Secuencia
		dw_4.Object.lfcd_fecham[ll_Fila]	=	dw_2.Object.mfco_fecmov[1]
		dw_4.Object.lofc_pltcod[ll_Fila]	=	dw_3.Object.lofc_pltcod[1]
		dw_4.Object.lofc_lotefc[ll_Fila]		=	dw_3.Object.lofc_lotefc[1]
		dw_4.Object.clie_codigo[ll_Fila]	=	dw_2.Object.clie_codigo[1]
		dw_4.Object.lfcd_tipool[ll_Fila]		=	2
		
		If istr_Mant.ArgumenTo[8] = "1" Then
			dw_4.Object.lfcd_tipdoc[ll_Fila]	=	dw_2.Object.mfco_tipdoc[1]
			dw_4.Object.lfcd_docrel[ll_Fila]	=	dw_2.Object.mfco_docrel[1]
		End If		
		li_Secuencia ++
	End If
	
Next

SELECT	IsNull(Max(mfcd_secuen), 0) + 1
	INTo	:li_Secuencia
	FROM	dbo.spro_movTofrutacomdeta
	WHERE	plde_codigo	=	:li_Planta
	AND	tpmv_codigo	=	:li_TipoMovTo
	AND   mfco_numero =  :il_NumFruta;

For ll_Fila = 1 To dw_4.RowCount()
	If dw_4.GetItemStatus(ll_Fila, 0, Primary!) = NewModIfied! Then
		ll_Nuevo	=	dw_1.InsertRow(0)
		dw_1.Object.plde_codigo[ll_Nuevo]	=	dw_2.Object.plde_codigo[1]
		dw_1.Object.tpmv_codigo[ll_Nuevo]	=	dw_2.Object.tpmv_codigo[1]
		dw_1.Object.mfco_numero[ll_Nuevo]	=	dw_2.Object.mfco_numero[1]
		dw_1.Object.mfcd_secuen[ll_Nuevo]	=	li_Secuencia
		dw_1.Object.plde_coorde[ll_Nuevo]	=	dw_2.Object.plde_codigo[1]
		dw_1.Object.cama_codigo[ll_Nuevo]	=	0
		dw_1.Object.lofc_pltcod[ll_Nuevo]		=	dw_2.Object.plde_codigo[1]
		dw_1.Object.lofc_espcod[ll_Nuevo]	=	dw_3.Object.lofc_espcod[1]
		dw_1.Object.lofc_lotefc[ll_Nuevo]		=	dw_4.Object.lofc_lotefc[1]
		dw_1.Object.lfcd_secuen[ll_Nuevo]	=	dw_4.Object.lfcd_secuen[ll_Fila]
		dw_1.Object.mfcd_bulent[ll_Nuevo]	=	dw_4.Object.lfcd_bulTos[ll_Fila]
		dw_1.Object.mfcd_kgnent[ll_Nuevo]	=	dw_4.Object.lfcd_kilnet[ll_Fila]
		dw_1.Object.mfcd_kilrom[ll_Nuevo]	=	dw_4.Object.lfcd_kilpro[ll_Fila]
		dw_1.Object.clie_codigo[ll_Nuevo]		=	dw_2.Object.clie_codigo[1]
		
		li_Secuencia ++
	ElseIf dw_4.GetItemStatus(ll_Fila, 0, Primary!) = DataModIfied! Then
		ll_FilaMov	=	dw_1.Find("lofc_lotefc = " + String(dw_4.Object.lofc_lotefc[ll_Fila]) + &
								  " AND lfcd_secuen = " + String(dw_4.Object.lfcd_secuen[ll_Fila]), 1, dw_1.RowCount())

		dw_1.Object.mfcd_bulent[ll_FilaMov]	=	dw_4.Object.lfcd_bulTos[ll_Fila]
		dw_1.Object.mfcd_kgnent[ll_FilaMov]	=	dw_4.Object.lfcd_kilnet[ll_Fila]
		dw_1.Object.mfcd_kilrom[ll_FilaMov]	=	dw_4.Object.lfcd_kilpro[ll_Fila]
		dw_1.Object.mfcd_calibr[ll_FilaMov]	=	dw_4.Object.refe_gcalib[ll_Fila]
		dw_1.Object.vari_codigo[ll_FilaMov]	=	dw_4.Object.vari_codigo[ll_Fila]
	End If
Next

For ll_Fila = 1 To dw_5.RowCount()
	If dw_5.GetItemStatus(ll_fila, 0, Primary!) = NewModIfied! Then
		dw_5.Object.plde_codigo[ll_Fila]		=	dw_2.Object.plde_codigo[1]
		dw_5.Object.tpmv_codigo[ll_Fila]		=	li_TipoMovToEnva
		dw_5.Object.meen_numero[ll_Fila]	=	dw_6.Object.meen_numero[1]
		dw_5.Object.clie_codigo[ll_Fila]		=	dw_2.Object.clie_codigo[1]
	End If
	dw_5.Object.fgme_sentid[ll_Fila]	=	1
Next

For ll_Fila = 1 To dw_1.RowCount()
	ll_TotalBulTos							+=	Long(dw_1.Object.mfcd_bulent[ll_Fila])
	ld_TotalKilos							+=	Dec(dw_1.Object.mfcd_kgnent[ll_Fila])
Next

For ll_fila = 1 To dw_6.RowCount()
	dw_6.Object.clie_codigo[ll_Fila]	=	dw_2.Object.clie_codigo[1]
	dw_6.Object.prod_codigo[ll_Fila]	=	dw_3.Object.Prod_codigo[1]
Next

For ll_fila = 1 To dw_5.RowCount()
	dw_5.Object.clie_codigo[ll_Fila]	=	dw_2.Object.clie_codigo[1]
	dw_5.Object.prod_codigo[ll_Fila]	=	dw_3.Object.Prod_codigo[1]
Next

dw_2.Object.mfco_Totbul[1]	=	ll_TotalBulTos
dw_2.Object.mfco_tpneTo[1]	=	ld_TotalKilos

If integer(istr_mant.argumenTo[8]) = 1 Then
	dw_4.SetFilter("lfcd_tipdoc = " + String(li_tipodoc) + " And lfcd_docrel = " + String(ll_docrel))
	dw_4.Filter()
Else
	dw_4.SetFilter("If (isnull(lfcd_tipdoc) and isnull(lfcd_docrel),1,0) = 1 ")
	dw_4.Filter()
End If

dw_4.SetRedraw(TRUE)
end event

event ue_guardar;Integer	li_Cliente

IF dw_1.AcceptText() = -1 THEN RETURN
IF dw_3.AcceptText() = -1 THEN RETURN
IF dw_4.AcceptText() = -1 THEN RETURN
IF dw_5.AcceptText() = -1 THEN RETURN

IF dw_4.RowCount()<=0 THEN Message.DoubleParm = -1
SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
	
//	//Crea conexion hacia existencia
//	IF gi_admenvase <> 1 THEN
//		li_Cliente	=	dw_2.Object.clie_codigo[1]
//		
//		SELECT 	clie_conexi, cone_codigo
//		INTO   	:il_conexiste, :il_coneccion
//		FROM dbo.clientesprod
//		WHERE clie_codigo = :li_Cliente;
//		
//		IF il_conexiste = 1 THEN
//			sqlexi	=	CREATE Transaction
//			IF Conexionexistencia() THEN
//				dw_exideta.SetTransObject(sqlexi)
//				dw_exiencab.SetTransObject(sqlexi)	
//				dw_exismovtodetanulos.SetTransObject(sqlexi)
//				dw_exidetaborra.SetTransObject(sqlexi)
//				TriggerEvent("ue_despuesborrar")
//				TriggerEvent("ue_despuesguardar")
//			ELSE
//				MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)
//			END IF
//		END IF
//	END IF	
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

event ue_seleccion;call super::ue_seleccion;Str_Busqueda	lstr_Busq
Long  			ll_lote

lstr_Busq.Argum[1] = istr_Mant.Argumento[1]		// Código Planta
lstr_Busq.Argum[2] = istr_Mant.Argumento[2]		// Tipo de Movimiento
lstr_Busq.Argum[3] = ''									// Estado Movimiento
lstr_Busq.Argum[4] = ''  								// Fecha Inicio Movimiento

OpenWithParm(w_busc_movtofrutacomenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	istr_Mant.Argumento[1]	=	lstr_Busq.Argum[1]
	istr_Mant.Argumento[2]	=	lstr_Busq.Argum[2]
	istr_Mant.Argumento[3]	=	lstr_Busq.Argum[3]
	istr_Mant.Argumento[4]	=	lstr_Busq.Argum[10]
	istr_Mant.Argumento[5]	=	lstr_Busq.Argum[11]

	IF istr_Mant.Argumento[8]	=	"1" THEN
		IF iuo_doctointernopack.Existe(Integer(istr_Mant.Argumento[13]), &
												 Integer(istr_Mant.Argumento[1]), &
												 Integer(lstr_Busq.Argum[10]), &
												 Long(lstr_Busq.Argum[11]), True, SqlCa) THEN

			istr_Mant.Argumento[7]	= String(iuo_doctointernopack.Especie)

			iuo_prodempresa.existe(gstr_paramplanta.productorempresa,false,sqlca)
			
			dw_3.SetItem(1, "prod_codigo", gstr_paramplanta.productorempresa)
			dw_3.SetItem(1, "prod_nombre", iuo_prodempresa.nombre )
			dw_2.SetItem(1, "mfco_docrel", Integer(istr_Mant.Argumento[5]))
			dw_2.SetItem(1, "espe_codigo", iuo_doctointernopack.Especie)
			dw_3.SetItem(1, "lofc_espcod", iuo_doctointernopack.Especie)
			dw_3.Enabled = True
		ELSE
			dw_3.Enabled = False
		END IF
	END IF
	
	dw_2.SetItem(1, "mfco_docrel", Integer(istr_Mant.Argumento[5]))	
	IF iuo_spro_ordenproceso.Existe(Integer(istr_Mant.Argumento[1]), &
										  integer(istr_mant.Argumento[4]), &
										  Long(istr_Mant.Argumento[5]), True, SqlCa,gi_codexport)       THEN
		
		istr_Mant.Argumento[7]	= String(iuo_doctointernopack.Especie)
		HabilitaLote()
		iuo_spro_ordenproceso.CapturaReferencias(1,0,1,0,0,False,Sqlca)

	END IF
	
	IF (istr_Mant.Argumento[12] = "1" AND gstr_paramplanta.PoolVenta <> 1 AND &
													 gstr_paramplanta.PoolVenta <> 5) OR &
		(istr_Mant.Argumento[12] = "2" AND gstr_Paramplanta.PoolRetiro <> 1 AND &
													 gstr_Paramplanta.PoolRetiro <> 4) THEN
	
		Istr_Mant.Argumento[6] = String(Nuevolote(ll_lote) + TipoRecepcion(istr_Mant.Argumento[12], &
														  Date(Mid(String(iuo_doctointernopack.ld_fecha),1,10))))
		dw_3.Object.lofc_lotefc[1] = Long(Istr_Mant.Argumento[6])
	ELSE
		ExisteLote(0)
	END IF
	
	IF istr_Mant.Argumento[6] <> "0" AND Existelote(Integer(Istr_Mant.Argumento[6])) THEN
		TriggerEvent("ue_recuperadatos")
	ELSEIF (Istr_Mant.Argumento[12] = "1") AND &
			 gstr_paramplanta.PoolVenta = 5 OR gstr_paramplanta.PoolVenta = 1 THEN 
			 dw_3.Object.lofc_lotefc[1] = Integer("")
			 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
	ELSEIF (Istr_Mant.Argumento[12] = "2") AND &
			 gstr_paramplanta.PoolRetiro = 1 OR gstr_paramplanta.PoolRetiro = 4 THEN 
			 dw_3.Object.lofc_lotefc[1] = Integer("")
			 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
	ELSE
			 dw_3.Object.lofc_lotefc[1] = Integer(Istr_Mant.Argumento[6])
			 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
			 Habilitaencab(False)
			 dw_5.Retrieve(Integer(istr_Mant.Argumento[1]), &
								Integer(istr_Mant.Argumento[2]), &
									Long(istr_Mant.Argumento[3]),1)
			 
	END IF

	HabilitaLote()
END IF
end event

event ue_modifica_detalle;call super::ue_modifica_detalle;
istr_mant.agrega	=	False
istr_mant.borra	=	False

IF tab_1.SelectedTab = 1 THEN
	IF dw_4.RowCount() > 0 THEN	
		istr_Mant.Argumento[10]	=	String(dw_3.Object.prod_codigo[1])
		istr_Mant.Argumento[11] =	String(dw_2.Object.mfco_fecmov[1],'dd/mm/yyyy')
		
		istr_mant.dw	=	dw_4
		
		OpenWithParm(iw_mantencion, istr_mant)
	
		IF dw_4.RowCount() > 0 THEN
			pb_eli_det.Enabled	=	True
		ELSE
			pb_eli_det.Enabled	=	False
		END IF
	END IF
ELSE	
	IF dw_5.RowCount() > 0 THEN
		istr_mant.dw	=	dw_5
		istr_mant.argumento[15]	= istr_mant.argumento[7]
		istr_mant.argumento[7] 	= "0"
		istr_mant.argumento[16]	= string(iuo_cliente.Codigo)
		
		OpenWithParm(iw_mantencion_2, istr_mant)
	
		istr_mant.argumento[7] = istr_mant.argumento[15]
		
		IF dw_5.RowCount() > 0 THEN
			pb_eli_det.Enabled			=	True
		ELSE
			pb_eli_det.Enabled			=	False
		END IF
	END IF
END IF


end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila, ll_lote
Integer  li_plantalote, li_espelote

istr_info.titulo	= "IDENTIFICACION FRUTA COMERCIAL"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_identificacion_comercial_repro"
vinf.dw_1.SetTransObject(sqlca)

li_plantalote = dw_3.Object.lofc_pltcod[1]
li_espelote   = dw_3.Object.lofc_espcod[1]
ll_lote       = dw_3.Object.lofc_lotefc[1]

fila = vinf.dw_1.Retrieve(integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]), &
                          Long(istr_mant.argumento[3]), li_plantalote, li_espelote, ll_lote)

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

event ue_borrar();IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar		=	True
ib_AutoCommit	=	sqlca.AutoCommit

w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.RowCount() > 0 THEN
	dw_1.RowsMove(1, dw_1.RowCount(), Primary!, dw_1, 1, Delete!)
END IF

IF dw_4.RowCount() > 0 THEN
	dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
END IF

IF dw_5.RowCount() > 0 THEN
	dw_5.RowsMove(1, dw_5.RowCount(), Primary!, dw_5, 1, Delete!)
END IF

IF dw_8.RowCount() > 0 THEN
	dw_8.RowsMove(1, dw_8.RowCount(), Primary!, dw_8, 1, Delete!)
END IF

IF dw_2.DeleteRow(0) = 1 AND dw_3.DeleteRow(0) = 1 AND dw_6.DeleteRow(0) = 1 THEN
		ib_Borrar	=	False
		
		w_main.SetMicroHelp("Borrando Registro...")
		
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

event resize;call super::resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0


tab_1.Width          = This.WorkSpaceWidth() - 450

maximo					= tab_1.width

dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					= 37
dw_3.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_3.y					= dw_2.y + dw_2.Height

tab_1.x					= 37
tab_1.y					= 64 + dw_2.Height + dw_3.Height
tab_1.Height			= This.WorkSpaceHeight() - tab_1.y - 41

Tab_1.Tp_1.dw_detalle.Height =  tab_1.Height - 200
Tab_1.Tp_1.dw_detalle.Width  =  tab_1.Width  - 100

Tab_1.Tp_2.dw_envases.Height =  tab_1.Height - 200
Tab_1.Tp_2.dw_envases.Width  =  tab_1.Width  - 100
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutacomer_reproceso
boolean visible = false
integer x = 352
integer y = 160
integer width = 169
integer height = 148
boolean titlebar = false
string title = "Detalle de Movimiento "
string dataobject = "dw_mues_movtofrutacomdeta"
boolean hscrollbar = false
boolean vscrollbar = false
boolean border = true
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event dw_1::rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = getrow()
	This.SelectRow(0,False)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event type long dw_1::ue_seteafila(unsignedlong wparam, long lparam);il_fila	= This.GetRow()

This.SelectRow(0, False)
This.SelectRow(il_fila, True)

RETURN 0
end event

event dw_1::clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutacomer_reproceso
integer x = 873
integer y = 32
integer width = 2853
integer height = 252
integer taborder = 10
string dataobject = "dw_mant_movtofrutacomenca_reproceso"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Null, ls_Fecha
Long		ll_lote
SetNull(ls_Null)

ls_Columna = dwo.name

Choose Case ls_Columna
	Case "clie_codigo"
		If NOT iuo_cliente.Existe(Integer(data), TRUE, sqlca) Then
			dw_2.Object.clie_codigo[1]	=	Integer(ls_Null)
			Return 1
		Else
			istr_Mant.Argumento[13]	=	String(iuo_Cliente.Codigo)
		End If

	Case "plde_codigo"
		If Not iuo_planta.existe(integer(data),True,Sqlca) Then
			This.SetItem(1, "plde_codigo", integer(ls_Null))
			Return 1
		Else	
			istr_Mant.Argumento[1]	= Data
			This.SetItem(1, "mfco_docrel", Long(ls_Null))
			This.SetItem(1, "espe_codigo", Integer(ls_Null))
		End If

	Case "mfco_tipdoc"
		If istr_Mant.Argumento[8]	=	"1" Then
			istr_Mant.Argumento[4]	= Data
			This.SetItem(1, "mfco_docrel", Long(ls_Null))
			This.SetItem(1, "espe_codigo", Integer(ls_Null))
			If integer(data)=5 Then istr_mant.argumento[2] = "6"
			If integer(data)=6 Then istr_mant.argumento[2] = "7"
			If integer(data)=8 Then istr_mant.argumento[2] = "8"
		Else
			If Not iuo_TipoDocto.Existe(Integer(Data), True, sqlca) Then
				This.SetItem(1, ls_Columna, Integer(ls_Null))
				
				Return 1
			Else
				istr_Mant.Argumento[4]	= Data
			End If
		End If

	Case "mfco_docrel"
		If istr_Mant.Argumento[8]	=	"1" Then
			If iuo_doctointernopack.Existe(This.Object.clie_codigo[1], &
													 Integer(istr_Mant.Argumento[1]), &
													 This.Object.mfco_tipdoc[1], &
													 Long(Data), True, SqlCa) Then
				istr_Mant.Argumento[5]		= Data
				istr_Mant.Argumento[7]		= String(iuo_doctointernopack.Especie)
				istr_Mant.Argumento[15]	= String(iuo_doctointernopack.Variedad)
				
				HabilitaLote()
				
				This.SetItem(1, "espe_codigo", iuo_doctointernopack.Especie)
				
				iuo_prodempresa.existe(gstr_paramplanta.productorempresa, False, Sqlca)
		
				dw_3.SetItem(1, "prod_codigo", gstr_paramplanta.productorempresa)
				dw_3.SetItem(1, "prod_nombre", iuo_prodempresa.nombre )
				
				istr_Mant.Argumento[17]	=	String(iuo_ProdEmpresa.Codigo)
				istr_Mant.Argumento[18]	=	iuo_ProdEmpresa.Nombre
	
				dw_3.SetItem(1, "lofc_espcod", iuo_DoctoInternoPack.Especie)
				
				dw_3.Enabled = True
				
				If buscamovto(integer(istr_Mant.Argumento[4]),Long(data)) Then
					If (istr_Mant.Argumento[12] = "1" AND gstr_paramplanta.PoolVenta <> 1 AND &
																	 gstr_paramplanta.PoolVenta <> 5) OR &
						(istr_Mant.Argumento[12] = "2" AND gstr_Paramplanta.PoolRetiro <> 1 AND &
																	 gstr_Paramplanta.PoolRetiro <> 4) Then
						Istr_Mant.Argumento[6] = String(Nuevolote(ll_lote) + TipoRecepcion(istr_Mant.Argumento[12], &
														  Date(Mid(String(iuo_doctointernopack.ld_fecha),1,10))))
						dw_3.Object.lofc_lotefc[1] = Long(Istr_Mant.Argumento[6])
					Else
						ExisteLote(0)
					End If
	
					If Existelote(Integer(Istr_Mant.Argumento[6])) AND istr_Mant.Argumento[6] <> "0" Then
						Parent.TriggerEvent("ue_recuperadatos")
					ElseIf (Istr_Mant.Argumento[12] = "1") AND &
							 gstr_paramplanta.PoolVenta = 5 OR gstr_paramplanta.PoolVenta = 1 Then 
							 dw_3.Object.lofc_lotefc[1] = Integer("")
							 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
					ElseIf (Istr_Mant.Argumento[12] = "2") AND &
							 gstr_paramplanta.PoolRetiro = 1 OR gstr_paramplanta.PoolRetiro = 4 Then 
							 dw_3.Object.lofc_lotefc[1] = Integer("")
							 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
					Else
							 dw_3.Object.lofc_lotefc[1] = Integer(Istr_Mant.Argumento[6])
							 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
							 Habilitaencab(False)
							 dw_5.Retrieve(Integer(istr_Mant.Argumento[1]), &
												Integer(istr_Mant.Argumento[2]), &
													Long(istr_Mant.Argumento[3]),1, &
													This.Object.clie_codigo[1])
							 
					End If
				Else
					If (istr_Mant.Argumento[12] = "1" AND gstr_paramplanta.PoolVenta <> 1 AND &
																	 gstr_paramplanta.PoolVenta <> 5) OR &
						(istr_Mant.Argumento[12] = "2" AND gstr_Paramplanta.PoolRetiro <> 1 AND &
																	 gstr_Paramplanta.PoolRetiro <> 4) Then
							Istr_Mant.Argumento[6] = &
										String(Nuevolote(ll_lote) + TipoRecepcion(istr_Mant.Argumento[12], &
										Date(Mid(String(iuo_doctointernopack.ld_fecha),1,10))))
						If dw_3.Retrieve(Integer(Istr_Mant.Argumento[1]),Integer(Istr_Mant.Argumento[7]), &
											  Integer(Istr_Mant.Argumento[6])) <= 0 Then
							dw_3.InsertRow(0)
							dw_3.SetItem(1, "lofc_pltcod", Integer(Istr_Mant.Argumento[1]))						
							dw_3.SetItem(1, "lofc_espcod", iuo_doctointernopack.Especie)			
							dw_3.SetItem(1, "prod_codigo", gstr_paramplanta.productorempresa)
							dw_3.SetItem(1, "prod_nombre", iuo_prodempresa.nombre)
							dw_3.Object.lofc_lotefc[1] = Long(Istr_Mant.Argumento[6])
						End If
					Else
						ExisteLote(0)
					End If
	
					If (Istr_Mant.Argumento[12] = "1") AND &
							 gstr_paramplanta.PoolVenta = 5 OR gstr_paramplanta.PoolVenta = 1 Then 
							 dw_3.Object.lofc_lotefc[1] = nuevofoliolote()//Integer("")
							 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
					ElseIf (Istr_Mant.Argumento[12] = "2") AND &
							 gstr_paramplanta.PoolRetiro = 1 OR gstr_paramplanta.PoolRetiro = 4 Then 
							 dw_3.Object.lofc_lotefc[1] = nuevofoliolote()//Integer("")
							 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
					Else
							 dw_3.Object.lofc_lotefc[1] = long(Istr_Mant.Argumento[6])
							 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
							 Habilitaencab(False)
							 dw_5.Retrieve(Integer(istr_Mant.Argumento[1]), &
												Integer(istr_Mant.Argumento[2]), &
													Long(istr_Mant.Argumento[3]),1, &
													Long(istr_Mant.Argumento[13]))
							 
					End If
					
				End If	
			Else		
				This.SetItem(1, "espe_codigo", Integer(ls_Null))
				This.SetItem(1, ls_Columna, Long(ls_Null))
				dw_3.Enabled = False
				Return 1
			End If
		Else
			istr_Mant.Argumento[5]	= Data
		End If
		
	Case "mfco_numero"
		If istr_Mant.Argumento[8]	=	"1" Then
			If ExisteMovimiento(Long(Data)) Then
				istr_Mant.Argumento[3]	=	Data
				This.SetItem(1, "mfco_docrel", Integer(istr_Mant.Argumento[5]))	
				If iuo_doctointernopack.Existe(Integer(istr_Mant.Argumento[13]), &
													 	 Integer(istr_Mant.Argumento[1]), &
														 Integer(istr_mant.Argumento[4]), &
														 Long(istr_Mant.Argumento[5]), True, SqlCa)       Then
					
					istr_Mant.Argumento[7]	= String(iuo_doctointernopack.Especie)
					Habilitalote()
					This.SetItem(1, "espe_codigo", iuo_doctointernopack.Especie)
				End If
				
				If (istr_Mant.Argumento[12] = "1" AND gstr_paramplanta.PoolVenta <> 1 AND &
																 gstr_paramplanta.PoolVenta <> 5) OR &
					(istr_Mant.Argumento[12] = "2" AND gstr_Paramplanta.PoolRetiro <> 1 AND &
																 gstr_Paramplanta.PoolRetiro <> 4) Then
					Istr_Mant.Argumento[6] = String(Nuevolote(ll_lote) + TipoRecepcion(istr_Mant.Argumento[12], &
													  Date(Mid(String(iuo_doctointernopack.ld_fecha),1,10))))
					dw_3.Object.lofc_lotefc[1] = Long(Istr_Mant.Argumento[6])
				Else
					ExisteLote(0)
				End If
				
				If Istr_Mant.Argumento[6] <> "0" AND Existelote(Integer(Istr_Mant.Argumento[6])) Then
					Parent.TriggerEvent("ue_recuperadatos")
				ElseIf gstr_paramplanta.PoolVenta = 5 OR gstr_paramplanta.PoolVenta = 1 Then 
					 dw_3.Object.lofc_lotefc[1] = Integer("")
					 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
				 Else
					 dw_3.Object.lofc_lotefc[1] = Integer(Istr_Mant.Argumento[6])
					 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
					 Habilitaencab(False)
				End If
			
			Else
				This.SetItem(1, ls_Columna, Long(ls_Null))
				Return 1
			End If
		Else
			If ExisteMovtoRecep(Long(Data)) Then
				istr_Mant.Argumento[3]	=	Data

				If Existelote(0) Then
					Parent.TriggerEvent("ue_recuperadatos")
				Else
					If (istr_Mant.Argumento[12] = "1" AND gstr_paramplanta.PoolVenta <> 1 AND &
																	 gstr_paramplanta.PoolVenta <> 5) OR &
						 (istr_Mant.Argumento[12] = "2" AND gstr_Paramplanta.PoolRetiro <> 1 AND &
																	 gstr_Paramplanta.PoolRetiro <> 4) Then
						 Istr_Mant.Argumento[6] = String(Nuevolote(ll_lote) + TipoRecepcion(istr_Mant.Argumento[12], &
														  Date(Mid(String(dw_2.Object.mfco_fecmov[row]),1,10))))
						 dw_3.Object.lofc_lotefc[1] = Long(Istr_Mant.Argumento[6])
					Else
						ExisteLote(0)
					End If
					
				End If
			Else
				If (istr_Mant.Argumento[12] = "1" AND gstr_paramplanta.PoolVenta <> 1 AND &
																 gstr_paramplanta.PoolVenta <> 5) OR &
					(istr_Mant.Argumento[12] = "2" AND gstr_Paramplanta.PoolRetiro <> 1 AND &
																 gstr_Paramplanta.PoolRetiro <> 4) Then
					Istr_Mant.Argumento[6] = String(Nuevolote(ll_lote) + TipoRecepcion(istr_Mant.Argumento[12], &
													  Date(Mid(String(dw_2.Object.mfco_fecmov[row]),1,10))))
					dw_3.Object.lofc_lotefc[1] = Long(Istr_Mant.Argumento[6])
				Else
					ExisteLote(0)
				End If
				
			End If
		End If

	Case "mfco_fecmov"
		ls_Fecha	=	Data
		This.SetItem(1, ls_Columna, Date(Mid(ls_Fecha,1,10)))

	Case "plde_coorde"
		If Not iuo_Planta.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(1, ls_Columna, ls_Null)
			
			Return 1
		End If

	Case "tran_codigo"
		If Not iuo_Transportista.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(1, ls_Columna, ls_Null)
			
			Return 1
		End If

	Case "cami_patent"
		If Not iuo_Camion.Existe(This.Object.cami_clasIfi[1], Data, False, sqlca) Then
			This.Object.cami_patcar[1]		=	ls_Null
			This.Object.mfco_rutcho[1]		=	ls_Null
			This.Object.mfco_chofer[1]		=	ls_Null
		Else
			This.Object.cami_patcar[1]		=	iuo_Camion.PateCarro
			This.Object.mfco_rutcho[1]		=	iuo_Camion.RutChofer
			This.Object.mfco_chofer[1]		=	iuo_Camion.Chofer
			This.Object.cami_clasIfi[1]		=	iuo_Camion.ClasIficacion
		End If

	Case "mfco_rutcho"
		is_rut = F_verrut(data, True)
		If is_rut = "" Then
			dw_2.SetItem(1, "mfco_rutcho", ls_Null)
			Return 1
		Else
			This.SetItem(1, "mfco_rutcho", is_rut)
		End If

	Case "cod_especie"
		
		istr_Mant.Argumento[7]	= Data
		dw_3.SetItem(1, "lofc_espcod", Integer(Data))

End Choose

HabilitaIngreso(ls_columna)
end event

event dw_2::itemerror;call super::itemerror;RETURN 1
end event

event dw_2::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.name

	CASE "b_reproceso"
		BuscaReProceso()

	CASE "b_camion"
		BuscaCamion()

END CHOOSE
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

end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutacomer_reproceso
integer x = 4672
integer y = 316
integer taborder = 50
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutacomer_reproceso
integer x = 4672
integer y = 492
integer taborder = 60
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutacomer_reproceso
integer x = 4672
integer y = 680
integer taborder = 70
boolean enabled = true
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutacomer_reproceso
integer x = 4672
integer y = 856
integer taborder = 80
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutacomer_reproceso
integer x = 4672
integer y = 1036
integer taborder = 90
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutacomer_reproceso
integer x = 4672
integer y = 1424
integer taborder = 100
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutacomer_reproceso
integer x = 4672
integer y = 1596
integer taborder = 110
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutacomer_reproceso
integer x = 4672
integer y = 132
integer taborder = 40
end type

type dw_3 from uo_dw within w_maed_movtofrutacomer_reproceso
integer x = 873
integer y = 280
integer width = 2871
integer height = 264
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
string dataobject = "dw_mant_lotesfrutacomenc_proceso"
boolean vscrollbar = false
boolean border = false
end type

event itemchanged;call super::itemchanged;String	ls_Columna, ls_Null, ls_Fecha

SetNull(ls_Null)

ls_Columna = dwo.name

CHOOSE CASE ls_Columna

	CASE "plde_codigo"
		IF Not iuo_planta.existe(integer(data),True,Sqlca) THEN
			This.SetItem(1, "plde_codigo", integer(ls_Null))
		ELSE	
			istr_Mant.Argumento[1]	= Data
			This.SetItem(1, "mfco_docrel", Long(ls_Null))
			This.SetItem(1, "espe_codigo", Integer(ls_Null))
		END IF
		
	CASE "prod_codigo"
		IF iuo_productores.Existe(long(Data),True,SqlCa) THEN
			This.Object.prod_nombre[il_Fila]	=	iuo_productores.Nombre 
		ELSE
			This.Object.prod_codigo[il_Fila]	=	Integer(ls_Null)
			This.Object.prod_nombre[il_Fila]	=	ls_Null
			RETURN 1
		END IF
		
	CASE "mfco_tipdoc"
		IF istr_Mant.Argumento[8]	=	"1" THEN
			istr_Mant.Argumento[4]	= Data
			This.SetItem(1, "mfco_docrel", Long(ls_Null))
			This.SetItem(1, "espe_codigo", Integer(ls_Null))
			IF integer(data)=5 THEN istr_mant.argumento[2] = "6"
			IF integer(data)=6 THEN istr_mant.argumento[2] = "7"
			IF integer(data)=8 THEN istr_mant.argumento[2] = "8"
		ELSE
			IF Not iuo_TipoDocto.Existe(Integer(Data), True, sqlca) THEN
				This.SetItem(1, ls_Columna, Integer(ls_Null))
				
				RETURN 1
			ELSE
				istr_Mant.Argumento[4]	= Data
			END IF
		END IF

	CASE "mfco_docrel"
		IF istr_Mant.Argumento[8]	=	"1" THEN
			IF iuo_doctointernopack.Existe(Integer(istr_Mant.Argumento[13]), &
													 Integer(istr_Mant.Argumento[1]), &
													 This.Object.mfco_tipdoc[1], &
													 Long(Data), True, SqlCa) THEN
				istr_Mant.Argumento[5]	= Data
				istr_Mant.Argumento[7]	= String(iuo_doctointernopack.Especie)
				
				This.SetItem(1, "espe_codigo", iuo_doctointernopack.Especie)
				
				iuo_prodempresa.existe(gstr_paramplanta.productorempresa,false,sqlca)
		
				dw_3.SetItem(1, "prod_codigo", gstr_paramplanta.productorempresa)
				dw_3.SetItem(1, "prod_nombre", iuo_prodempresa.nombre )
	
				dw_3.SetItem(1, "lofc_espcod", iuo_doctointernopack.Especie)
				
				dw_3.Enabled = True
				
				IF buscamovto(integer(istr_Mant.Argumento[4]),Long(data)) THEN
					Istr_Mant.Argumento[6] = String(Tiporecepcion(Istr_Mant.Argumento[11], &
																				iuo_spro_ordenproceso.fechaorden))
					IF Existelote(Integer(Istr_Mant.Argumento[6])) THEN
						Parent.TriggerEvent("ue_recuperadatos")
					ELSE
						dw_3.Object.lofc_lotefc[1] = Integer(Istr_Mant.Argumento[6])
						dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
						Habilitaencab(False)
					END IF
				
				END IF
			ELSE
				This.SetItem(1, "espe_codigo", Integer(ls_Null))
				This.SetItem(1, ls_Columna, Long(ls_Null))
				dw_3.Enabled = False
				RETURN 1
			END IF
		ELSE
			istr_Mant.Argumento[5]	= Data
		END IF

	CASE "mfco_numero"
		IF istr_Mant.Argumento[8]	=	"1" THEN
			IF ExisteMovimiento(Long(Data)) THEN
				istr_Mant.Argumento[3]	=	Data
				IF iuo_doctointernopack.Existe(Integer(istr_Mant.Argumento[13]), &
													 	 Integer(istr_Mant.Argumento[1]), &
														 Integer(istr_mant.Argumento[4]), &
														 Long(istr_Mant.Argumento[5]), True, SqlCa)       THEN
					
					istr_Mant.Argumento[7]	= String(iuo_doctointernopack.Especie)
		
					This.SetItem(1, "espe_codigo", iuo_doctointernopack.Especie)
				END IF
				

				Istr_Mant.Argumento[6] = String(Tiporecepcion(Istr_Mant.Argumento[11], &
																			iuo_spro_ordenproceso.fechaorden))
				IF Existelote(Integer(Istr_Mant.Argumento[6])) THEN
					Parent.TriggerEvent("ue_recuperadatos")
				ELSE
					dw_3.Object.lofc_lotefc[1] = Integer(Istr_Mant.Argumento[6])
					dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
					Habilitaencab(False)
				END IF

			ELSE
				This.SetItem(1, ls_Columna, Long(ls_Null))
				RETURN 1
			END IF
		ELSE
			IF ExisteMovtoRecep(Long(Data)) THEN
				istr_Mant.Argumento[3]	=	Data

				IF Existelote(0) THEN
					Parent.TriggerEvent("ue_recuperadatos")
				END IF
				
			END IF
		END IF

	CASE "mfco_fecmov"
		ls_Fecha	=	Data
		This.SetItem(1, ls_Columna, Date(Mid(ls_Fecha,1,10)))

	CASE "plde_coorde"
		IF Not iuo_Planta.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, ls_Null)
			
			RETURN 1
		END IF

	CASE "tran_codigo"
		IF Not iuo_Transportista.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, ls_Null)
			
			RETURN 1
		END IF

	CASE "cami_patent"
		IF Not iuo_Camion.Existe(This.Object.cami_clasifi[1], Data, False, sqlca) THEN
			This.Object.cami_patcar[1]		=	ls_Null
			This.Object.mfco_rutcho[1]		=	ls_Null
			This.Object.mfco_chofer[1]		=	ls_Null
		ELSE
			This.Object.cami_patcar[1]		=	iuo_Camion.PateCarro
			This.Object.mfco_rutcho[1]		=	iuo_Camion.RutChofer
			This.Object.mfco_chofer[1]		=	iuo_Camion.Chofer
			This.Object.cami_clasifi[1]	=	iuo_Camion.Clasificacion
		END IF

	CASE "mfco_rutcho"
		is_rut = F_verrut(data, True)
		IF is_rut = "" THEN
			dw_2.SetItem(1, "mfco_rutcho", ls_Null)
			RETURN 1
		ELSE
			This.SetItem(1, "mfco_rutcho", is_rut)
		END IF

	CASE "cod_especie"
		
		istr_Mant.Argumento[7]	= Data
		dw_3.SetItem(1, "lofc_espcod", Integer(Data))
		
	CASE "lofc_lotefc"
		dw_4.reset()
		IF Existelote(long(data)) THEN
         istr_mant.argumento[6] = Data
	
		ELSEIF gstr_paramplanta.PoolVenta <> 1 THEN
			dw_3.SetItem(1,"lofc_lotefc",long(ls_null))
			Return 1
		END IF	

		

END CHOOSE

HabilitaIngreso(ls_columna)
end event

event buttonclicked;long ll_lote

CHOOSE CASE dwo.name

	CASE "b_nuevolote"
		ll_lote = dw_3.Object.lofc_lotefc[1]
		dw_3.Reset()
		dw_3.insertrow(0)
		ll_lote = Nuevolote(ll_lote)
      	dw_3.Object.lofc_lotefc[1] = ll_lote
		istr_Mant.Argumento[6]	=	String(ll_lote)

	CASE "b_prodrot"
		BuscaProdRot()

END CHOOSE
end event

event itemerror;call super::itemerror;RETURN 1
end event

type dw_6 from datawindow within w_maed_movtofrutacomer_reproceso
boolean visible = false
integer width = 169
integer height = 148
integer taborder = 70
boolean bringtotop = true
string title = "Encabezado de Envases"
string dataobject = "dw_mant_movtoenvaenca_comercial"
end type

type dw_8 from datawindow within w_maed_movtofrutacomer_reproceso
boolean visible = false
integer x = 178
integer width = 165
integer height = 148
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_movtoenvadeta"
end type

type tab_1 from tab within w_maed_movtofrutacomer_reproceso
event create ( )
event destroy ( )
integer x = 41
integer y = 604
integer width = 4553
integer height = 1180
integer taborder = 30
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
	IF dw_4.RowCount() > 0 THEN
		pb_eli_det.Enabled	=	True
		il_Fila 					=	1
		
		dw_4.SelectRow(0,False)
		dw_5.SelectRow(0,False)
		dw_4.SetRow(il_Fila)
		dw_4.SelectRow(il_Fila, True)
	ELSE
		pb_eli_det.Enabled	=	False
	END IF
ELSE
	IF dw_5.RowCount() > 0 THEN
		pb_eli_det.Enabled	=	True
		il_Fila 					=	1
		
		dw_4.SelectRow(0,False)
		dw_5.SelectRow(0,False)
		dw_5.SetRow(il_Fila)
		dw_5.SelectRow(il_Fila, True)
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
integer width = 4517
integer height = 1052
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
integer y = 32
integer width = 4466
integer height = 992
integer taborder = 21
string dataobject = "dw_mues_lotesfrutacomdeta_reproceso"
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

event doubleclicked;call super::doubleclicked;This.SetRow(row)
w_maed_movtofrutacomer_reproceso.TriggerEvent("ue_modifica_detalle")

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

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutacomer_reproceso.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutacomer_reproceso.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event buttonclicked;call super::buttonclicked;String	ls_columna

ls_columna	=	dwo.name

CHOOSE CASE ls_columna
	CASE "b_tarja"
		il_secuencia = This.Object.lfcd_secuen[row]
		Imprime_tarja()
		
END CHOOSE
end event

type tp_2 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 4517
integer height = 1052
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
integer y = 32
integer width = 4027
integer height = 992
integer taborder = 31
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

event doubleclicked;call super::doubleclicked;This.SetRow(row)
w_maed_movtofrutacomer_reproceso.TriggerEvent("ue_modifica_detalle")

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

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutacomer_reproceso.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutacomer_reproceso.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);call super::ue_seteafila;il_fila	= This.GetRow()

This.SelectRow(0, False)
This.SelectRow(il_fila, True)

RETURN 0
end event

type dw_exiencab from datawindow within w_maed_movtofrutacomer_reproceso
boolean visible = false
integer x = 791
integer y = 2964
integer width = 686
integer height = 400
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_exismovtoenca_analisis"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exideta from datawindow within w_maed_movtofrutacomer_reproceso
boolean visible = false
integer y = 160
integer width = 169
integer height = 148
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta"
end type

type dw_exidetaborra from datawindow within w_maed_movtofrutacomer_reproceso
boolean visible = false
integer x = 178
integer y = 160
integer width = 169
integer height = 148
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta_borra"
end type

type dw_exismovtodetanulos from datawindow within w_maed_movtofrutacomer_reproceso
boolean visible = false
integer x = 352
integer width = 169
integer height = 148
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_inserta_exismovtodetanulos"
end type

