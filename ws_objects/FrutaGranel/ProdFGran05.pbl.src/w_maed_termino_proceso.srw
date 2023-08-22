$PBExportHeader$w_maed_termino_proceso.srw
forward
global type w_maed_termino_proceso from w_mant_encab_deta_csd
end type
type dw_5 from datawindow within w_maed_termino_proceso
end type
type dw_4 from datawindow within w_maed_termino_proceso
end type
type dw_exideta from datawindow within w_maed_termino_proceso
end type
type dw_exiencab from datawindow within w_maed_termino_proceso
end type
type dw_3 from datawindow within w_maed_termino_proceso
end type
type dw_10 from datawindow within w_maed_termino_proceso
end type
type dw_7 from datawindow within w_maed_termino_proceso
end type
type dw_6 from datawindow within w_maed_termino_proceso
end type
end forward

global type w_maed_termino_proceso from w_mant_encab_deta_csd
string tag = "w_maed_termino_proceso"
integer width = 4741
integer height = 2352
string title = "Término de Proceso"
string menuname = ""
boolean center = true
event ue_despuesguardar ( )
dw_5 dw_5
dw_4 dw_4
dw_exideta dw_exideta
dw_exiencab dw_exiencab
dw_3 dw_3
dw_10 dw_10
dw_7 dw_7
dw_6 dw_6
end type
global w_maed_termino_proceso w_maed_termino_proceso

type variables
uo_spro_ordenproceso			iuo_ordenproceso	
uo_calicosechero  					iuo_calicosechero
uo_control_historico_proceso	iuo_historico

Boolean								ib_Modifica, ib_AutoCommit, ib_ConectadoExistencia
Integer								il_coneccion,il_conexiste
datawindowchild 	      			idwc_planta

Transaction							sqlexi
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine insertamovimientotraspaso (integer paso)
public subroutine devolucionbultos ()
public function boolean noexistecliente (integer al_codigo)
public function boolean buscalotesintraspaso ()
public function boolean conexionexistencia ()
public subroutine insertamovimientotraspasocomercial (integer paso)
public subroutine devolucionbultos_com ()
public function boolean historial ()
public function any existetermino (integer ai_fila)
public subroutine wf_bloqueacolumnas ()
end prototypes

event ue_despuesguardar();integer	li_bodega, li_fila, li_cliente, li_planta,li_enva_codigo, li_enva_tipoen, li_bodevirtual,&
			li_bodzonal, li_devcorreo, li_tipomov
Long 		ll_fila, ll_numero, li_secuencia = 1, li_filarecep, ll_filarecep, li_secuenciarece = 1,&
			ll_numerorecep, ll_fila_nueva, ll_fila_nea, ll_count, ll_docrel, ll_numnuevoini, ll_numnuevofin
String	ls_item_codigo, ls_productor, ls_prod, ls_calidad, ls_correozonal, ls_correo, ls_texto, ls_asunto, ls_error, serrormsg
Boolean	lb_AutoCommit, lb_Retorno
Date		ld_fechamov

IF ib_Conectadoexistencia THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	ll_docrel = dw_2.Object.orpr_numero[1]
	
	
	li_cliente 	= dw_2.Object.clie_codigo[1]
	li_planta 	= dw_2.Object.plde_codigo[1] 
	li_tipomov	= dw_2.Object.orpr_tipord[1]
	
	
	dw_7.Retrieve(li_planta,li_tipomov,ll_docrel,li_cliente)
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
	ELSE
		li_bodevirtual = luo_existencia.bodvirtual
		li_bodzonal		= luo_existencia.bodzonal
	END IF		
	
	ld_fechamov = dw_2.Object.orpr_fecpro[1]
	
	IF luo_existencia.Mesproceso > dw_2.Object.orpr_fecpro[1] THEN
		Message.DoubleParm = -1
		
		IF luo_existencia.numeromaximo(3,li_bodevirtual,ll_docrel,1,True,sqlexi) THEN
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
		
		ls_asunto = "Modifica Término de vaciado, Movto. Nº "+String(ll_docrel)
//		li_devcorreo =  Send_mail("smtp.rioblanco.cl","<frutaGranel@rioblanco.cl>","<"+ls_correo+">","<"+ls_correozonal+">","",ls_asunto,ls_texto,"",ls_error)
		
//		IF (li_devcorreo<0) THEN
//			messagebox("Error No" + string(li_devcorreo),sErrorMsg)
//		END IF
		
		RETURN 
	END IF

	luo_existencia.existeregistro(ll_docrel,li_bodevirtual,1,3,True,sqlexi,ld_fechamov)
		
	IF luo_existencia.count <> 0 THEN
		Return 
	END IF	
	
	IF Not luo_existencia.correlativobode(3,li_bodevirtual,li_bodzonal,True,sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
	ELSE
		ll_numero = luo_existencia.numero
	END IF	
		
	IF isnull(ll_numero) THEN
		ll_numnuevoini = Long(String(li_bodevirtual)+''+'0001')
		ll_numnuevofin = Long(String(li_bodevirtual)+''+'9999')
		
		INSERT INTO dbo.correlbode(bode_codigo,mden_tipdoc,cobo_bodadm,cobo_inicia,cobo_final,cobo_actual)
		VALUES(:li_bodevirtual,3,:li_bodzonal,:ll_numnuevoini,:ll_numnuevofin,:ll_numnuevoini)
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
	
	ls_productor = String(dw_2.Object.prod_codigo[1],'000000')
	
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
	dw_exiencab.Object.mden_fecmov[ll_fila_nea] 	= 	dw_2.Object.orpr_fecpro[1]
	dw_exiencab.Object.tpmv_tipomv[ll_fila_nea] 	= 	2
	dw_exiencab.Object.tpmv_codigo[ll_fila_nea] 	= 	2
	dw_exiencab.Object.mden_tipana[ll_fila_nea] 	= 	4
	dw_exiencab.Object.bode_codigo[ll_fila_nea] 	= 	li_bodevirtual
	dw_exiencab.Object.mden_bodest[ll_fila_nea] 	= 	li_bodzonal
	dw_exiencab.Object.clpr_rut[ll_fila_nea]	 	= 	ls_productor
	dw_exiencab.Object.mden_docrel[ll_fila_nea] 	= 	dw_2.Object.orpr_numero[1]
	dw_exiencab.Object.mden_fecdre[ll_fila_nea] 	= 	dw_2.Object.orpr_fecpro[1]
	dw_exiencab.Object.mden_observ[ll_fila_nea] 	= 	'Traspaso Cta. Cte. Envases'
	dw_exiencab.Object.mden_estado[ll_fila_nea] 	= 	1
	dw_exiencab.Object.mden_pcopda[ll_fila_nea] 	=	1
	dw_exiencab.Object.mden_estaci[ll_fila_nea] = gstr_us.computador
	dw_exiencab.Object.mden_fecdig[ll_fila_nea] = Date(Today())
	dw_exiencab.Object.mden_hordig[ll_fila_nea] = Time(Now())
	
	FOR li_fila = 1 TO dw_7.RowCount()
		//IF dw_4.Object.opvd_canbul[li_fila] > 0 THEN
			ll_fila				=	dw_exideta.InsertRow(0)
			
			li_enva_codigo 	= 	dw_7.Object.enva_codigo[li_fila]
			li_enva_tipoen 	= 	dw_7.Object.enva_tipoen[li_fila]
			ls_calidad			=  dw_7.Object.cale_calida[li_fila]
			
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
			dw_exideta.Object.tpmv_codigo[ll_fila] 	= 	2 
			dw_exideta.Object.item_codigo[ll_fila] 	= 	ls_item_codigo
			dw_exideta.Object.mdde_identi[ll_fila] 	= 	''
			dw_exideta.Object.mdde_fecmov[ll_fila] 	= 	dw_2.Object.orpr_fecpro[1]
			dw_exideta.Object.bode_codigo[ll_fila] 	= 	li_bodevirtual
			dw_exideta.Object.mdde_cantid[ll_fila] 	= 	dw_7.Object.envases[li_fila]
						
			li_secuencia = li_secuencia + 1
		//END IF
	NEXT	
	
	IF Not luo_existencia.correlativobode(3,li_bodzonal,li_bodzonal,True,sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
	ELSE
		ll_numerorecep = luo_existencia.numero
	END IF	
	
	IF isnull(ll_numerorecep) THEN
		ll_numerorecep = 0
	END IF	
	
	ll_numerorecep 	= 	ll_numerorecep + 1
	
	ll_fila_nueva 		= 	dw_exiencab.InsertRow(0)
	
	dw_exiencab.Object.mden_tipdoc[ll_fila_nueva] 	= 	3
	dw_exiencab.Object.mden_numero[ll_fila_nueva] 	= 	ll_numerorecep 
	dw_exiencab.Object.tpdo_codigo[ll_fila_nueva] 	= 	2
	dw_exiencab.Object.mden_fecmov[ll_fila_nueva] 	= 	dw_2.Object.orpr_fecpro[1]
	dw_exiencab.Object.tpmv_tipomv[ll_fila_nueva] 	= 	1
	dw_exiencab.Object.tpmv_codigo[ll_fila_nueva] 	= 	3
	dw_exiencab.Object.mden_tipana[ll_fila_nueva] 	= 	3
	dw_exiencab.Object.bode_codigo[ll_fila_nueva] 	= 	li_bodzonal
	dw_exiencab.Object.mden_bodest[ll_fila_nueva] 	= 	li_bodevirtual
	dw_exiencab.Object.clpr_rut[ll_fila_nueva]	 	= 	ls_productor
	dw_exiencab.Object.mden_docrel[ll_fila_nueva] 	= 	dw_2.Object.orpr_numero[1]
	dw_exiencab.Object.mden_fecdre[ll_fila_nueva] 	= 	dw_2.Object.orpr_fecpro[1]
	dw_exiencab.Object.mden_observ[ll_fila_nueva] 	= 	'Traspaso Cta. Cte. Envases'
	dw_exiencab.Object.mden_estado[ll_fila_nueva] 	= 	1
	dw_exiencab.Object.mden_pcopda[ll_fila_nueva] 	= 	1
	dw_exiencab.Object.mden_estaci[ll_fila_nueva] 	= gstr_us.computador
	dw_exiencab.Object.mden_fecdig[ll_fila_nueva] 	= Date(Today())
	dw_exiencab.Object.mden_hordig[ll_fila_nueva] 	= Time(Now())
	
	FOR li_filarecep = 1 TO dw_7.RowCount()
		//IF dw_4.Object.opvd_canbul[li_filarecep] > 0 THEN
			ll_filarecep		=	dw_exideta.InsertRow(0)
			
			li_enva_codigo	=	dw_7.Object.enva_codigo[li_filarecep]
			li_enva_tipoen =	dw_7.Object.enva_tipoen[li_filarecep]
			ls_calidad		=  dw_7.Object.cale_calida[li_filarecep]
			
			IF Not iuo_calicosechero.Existe(li_enva_tipoen,li_enva_codigo,ls_calidad,True,sqlca) THEN
				Message.DoubleParm = -1
				RETURN 
			ELSE
				ls_item_codigo = iuo_calicosechero.item 
			END IF	
			
			IF ls_item_codigo = '' OR IsNull(ls_item_codigo) THEN
				ls_item_codigo = luo_existencia.itgene
			END IF
			
			dw_exideta.Object.mden_tipdoc[ll_filarecep] 	= 	3
			dw_exideta.Object.mden_numero[ll_filarecep] 	= 	ll_numerorecep
			dw_exideta.Object.mdde_secuen[ll_filarecep] 	= 	li_secuenciarece 
			dw_exideta.Object.tpmv_tipomv[ll_filarecep] 	=	1
			dw_exideta.Object.tpmv_codigo[ll_filarecep] 	=	3 
			dw_exideta.Object.item_codigo[ll_filarecep]	=	ls_item_codigo
			dw_exideta.Object.mdde_identi[ll_filarecep] 	=	''
			dw_exideta.Object.mdde_fecmov[ll_filarecep] 	= 	dw_2.Object.orpr_fecpro[1]
			dw_exideta.Object.bode_codigo[ll_filarecep] 	= 	li_bodzonal
			dw_exideta.Object.mdde_cantid[ll_filarecep] 	= 	dw_7.Object.envases[li_filarecep]
			li_secuenciarece 										= 	li_secuenciarece + 1
//		END IF	
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
				ELSE
					lb_Retorno	=	True
					
					dw_exiencab.ResetUpdate()
					dw_exideta.ResetUpdate()
				END IF
			ELSE
				F_ErrorBaseDatos(sqlexi, This.Title)
				
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlexi, This.Title)
			
			RollBack;
		END IF
		Messagebox("Existencia","Grabación de Datos realizada Satisfactoriamente")
		sqlexi.AutoCommit		=	lb_AutoCommit
	
	END IF
	DISCONNECT USING sqlexi;
	ib_Conectadoexistencia = False
END IF


end event

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect				=	0
	dw_2.Object.orpr_tipord.Protect				=	0
	dw_2.Object.orpr_numero.Protect				=	0
	dw_2.Object.orpr_estado.Protect				=	0
	
	dw_2.Object.clie_codigo.Color		=	0
	dw_2.Object.orpr_tipord.Color		=	0	
	dw_2.Object.orpr_numero.Color	=	0
	dw_2.Object.orpr_estado.Color		=	0
	
	dw_2.Object.clie_codigo.BackGround.Color		=	RGB(255,255,255)	
	dw_2.Object.orpr_tipord.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.orpr_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.orpr_estado.BackGround.Color		=	RGB(255,255,255)
	
ELSE
	dw_2.Object.clie_codigo.Protect				=	1
	dw_2.Object.orpr_tipord.Protect				=	1
	dw_2.Object.orpr_numero.Protect				=	1
	dw_2.Object.orpr_estado.Protect				=	1

	dw_2.Object.clie_codigo.Color		=	RGB(255,255,255)	
	dw_2.Object.orpr_tipord.Color		=	0	
	dw_2.Object.orpr_numero.Color	=	RGB(255,255,255)
	dw_2.Object.orpr_estado.Color		=	0
	
	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
	dw_2.Object.orpr_tipord.BackGround.Color		=	553648127
	dw_2.Object.orpr_numero.BackGround.Color	=	553648127
	dw_2.Object.orpr_estado.BackGround.Color		=	553648127
END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean			lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False
//dw_2.Object.orpr_estado[1]
IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_3.Update(True, False) = 1 THEN
			IF dw_2.Update(True, False) = 1 THEN
				IF Historial() THEN
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
		IF dw_3.Update(True, False) = 1 THEN
			IF dw_1.Update(True, False) = 1 THEN
				IF dw_6.Update(True, False) = 1 THEN
					IF buscalotesintraspaso() THEN
						IF Historial() THEN
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

public subroutine insertamovimientotraspaso (integer paso);Long 		ll_Fila, ll_FilaNueva
Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[16])

IF dw_2.Object.orpr_tipord[1] = 9 THEN
	InsertaMovimientoTraspasoComercial(paso)
	RETURN
END IF

IF Paso=1 THEN
	FOR ll_fila = 1 TO dw_1.RowCount()
		ll_FilaNueva 									= 	dw_6.InsertRow(0)
		dw_6.Object.plde_codigo[ll_FilaNueva] 	= 	dw_1.Object.plde_codigo[ll_Fila]
		dw_6.Object.tpmv_codigo[ll_FilaNueva]	= 	dw_1.Object.tpmv_codigo[ll_Fila]
		dw_6.Object.tepr_numero[ll_FilaNueva]	= 	dw_1.Object.mfge_numero[ll_Fila]
		dw_6.Object.tepr_secuen[ll_FilaNueva]	= 	dw_1.Object.mfgd_secuen[ll_Fila]
		dw_6.Object.lote_pltcod[ll_FilaNueva] 	= 	dw_1.Object.lote_pltcod[ll_Fila]
		dw_6.Object.lote_espcod[ll_FilaNueva] 	= 	dw_1.Object.lote_espcod[ll_Fila]
		dw_6.Object.lote_codigo[ll_FilaNueva] 	= 	dw_1.Object.lote_codigo[ll_Fila]
		dw_6.Object.enva_tipoen[ll_FilaNueva] 	= 	dw_1.Object.enva_tipoen[ll_Fila]
		dw_6.Object.enva_codigo[ll_FilaNueva]	=	dw_1.Object.enva_codigo[ll_Fila]
		dw_6.Object.tepr_bultra[ll_FilaNueva] 	= 	dw_1.Object.mfgd_bulent[ll_Fila]
		dw_6.Object.clie_codigo[ll_FilaNueva] 	= 	li_Cliente
		
	NEXT
ELSE
	IF dw_6.RowCount() > 0 THEN
		dw_6.SetSort(	"plde_codigo asc, tpmv_codigo asc, tepr_numero asc, " + &
						  	"tepr_secuen asc, lote_pltcod asc, lote_espcod asc, " + &
						  	"lote_codigo asc, clie_codigo asc, enva_tipoen asc, " + &
						  	"enva_codigo asc")
		dw_6.Sort()
		
		FOR ll_Fila = 1 To dw_1.RowCount()
		
			ll_FilaNueva = dw_6.Find("plde_codigo 	= " 	+ string(dw_1.Object.plde_codigo[ll_fila]) + " AND " + &
			                         "tpmv_codigo 	= " 	+ string(dw_1.Object.tpmv_codigo[ll_fila]) + " AND " + &
											 "tepr_numero 	= " 	+ string(dw_1.Object.mfge_numero[ll_fila]) + " AND " + &
											 "tepr_secuen 	= " 	+ string(dw_1.Object.mfgd_secuen[ll_fila]) + " AND " + &
											 "lote_pltcod 	= " 	+ string(dw_1.Object.lote_pltcod[ll_fila]) + " AND " + &
                                  "lote_espcod 	= " 	+ string(dw_1.Object.lote_espcod[ll_fila]) + " AND " + &
											 "lote_codigo 	= " 	+ string(dw_1.Object.lote_codigo[ll_fila]) + " AND " + &
											 "clie_codigo 	= " 	+ string(li_Cliente) + " AND " + &
											 "enva_tipoen 	= " 	+ string(dw_1.Object.enva_tipoen[ll_fila]) + " AND " + &
											 "enva_codigo 	= " 	+ string(dw_1.Object.enva_codigo[ll_fila]), 1, dw_6.RowCount())
			
		  IF ll_FilaNueva > 0 THEN
			
			 dw_6.Object.tepr_bulvac[ll_FilaNueva] = dw_1.Object.mfgd_bulent[ll_Fila]
	 
		  END IF	
		NEXT
	END IF	
END IF	
end subroutine

public subroutine devolucionbultos ();Long		 	ll_FIla, ll_sumbul, ll_filabusq
Integer 		li_lotepl1,	li_lotees1,	li_loteco1,	li_tipoen1,	li_envase1, &
		 		li_lotepl2,	li_lotees2,	li_loteco2,	li_tipoen2,	li_envase2, li_Cliente
String		ls_orden

IF istr_mant.argumento[2] = '9' THEN 
	devolucionbultos_com()
ELSE
	ll_sumbul  	= 	0
	li_lotepl1 	= 	dw_4.Object.lote_pltcod[1]
	li_lotees1 	= 	dw_4.Object.lote_espcod[1]
	li_loteco1 	= 	dw_4.Object.lote_codigo[1]
	li_tipoen1 	= 	dw_4.Object.enva_tipoen[1]
	li_envase1 	= 	dw_4.Object.enva_codigo[1]
	ll_sumbul  	= 	dw_4.Object.opvd_canbul[1]
	li_Cliente	=	Integer(istr_mant.argumento[16])
	
	ls_orden		=	"lote_pltcod asc, lote_espcod asc, lote_codigo asc, " + &
						"clie_codigo asc, enva_tipoen asc, enva_codigo asc"

	dw_1.SetSort(ls_orden)

	dw_1.Sort()

	FOR ll_Fila = 2 TO dw_4.RowCount()
		li_lotepl2 		= 	dw_4.Object.lote_pltcod[ll_fila]
		li_lotees2 		= 	dw_4.Object.lote_espcod[ll_fila]
		li_loteco2 		= 	dw_4.Object.lote_codigo[ll_fila]
		li_tipoen2 		= 	dw_4.Object.enva_tipoen[ll_fila]
		li_envase2 		= 	dw_4.Object.enva_codigo[ll_fila]

		IF li_lotepl1	=	li_lotepl2 	AND 	&
			li_lotees1	=	li_lotees2 	AND 	&
			li_loteco1	=	li_loteco2 	AND 	&
			li_tipoen1 	=	li_tipoen2 	AND	&
			li_envase1	=	li_envase2 	THEN

			ll_sumbul  	=	ll_sumbul + dw_4.Object.opvd_canbul[ll_fila]

		ELSE
			
			ll_filabusq = dw_1.Find("lote_pltcod = " + String(li_lotepl1) + " AND " + &
											"lote_espcod = " + String(li_lotees1) + " AND " + &
											"lote_codigo = " + String(li_loteco1) + " AND " + &
											"clie_codigo = " + String(li_Cliente) + " AND " + &										
											"enva_tipoen = " + String(li_tipoen1) + " AND " + &
											"enva_codigo = " + String(li_envase1) , 1, dw_1.RowCount())

			IF ll_filabusq>0 THEN

				dw_1.Object.mfgd_bulent[ll_filabusq] = ll_sumbul

			END IF

			li_lotepl1 	= 	dw_4.Object.lote_pltcod[ll_fila]
			li_lotees1 	= 	dw_4.Object.lote_espcod[ll_fila]
			li_loteco1 	= 	dw_4.Object.lote_codigo[ll_fila]
			li_tipoen1 	= 	dw_4.Object.enva_tipoen[ll_fila]
			li_envase1 	= 	dw_4.Object.enva_codigo[ll_fila]
			ll_sumbul  	= 	dw_4.Object.opvd_canbul[ll_fila]
		END IF
	NEXT	
	
	ll_filabusq = dw_1.Find("lote_pltcod = " + String(li_lotepl1) + " AND " + &
									"lote_espcod = " + String(li_lotees1) + " AND " + &
									"lote_codigo = " + String(li_loteco1) + " AND " + &
									"clie_codigo = " + String(li_Cliente) + " AND " + &
									"enva_tipoen = " + String(li_tipoen1) + " AND " + &
									"enva_codigo = " + String(li_envase1) , 1, dw_1.RowCount())
										
	IF ll_filabusq>0 THEN
				
		dw_1.Object.mfgd_bulent[ll_filabusq] = ll_sumbul
			
	END IF
END IF
end subroutine

public function boolean noexistecliente (integer al_codigo);Integer	li_cliente

SELECT clie_codigo
INTO	:li_cliente
FROM	dbo.clientesprod
WHERE	clie_codigo	=	:al_codigo;
	
IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla ClientesProd" )
		Return True			
ELSEIF sqlca.SQLCode = 100 THEN
		messagebox("Atención","No Existe Código Seleccionado en Tabla ")			
		Return True						
END IF

Return False

end function

public function boolean buscalotesintraspaso ();Long 		ll_fila, ll_FilaNueva, ll_BultosOriginales
Integer	li_Cliente, li_plde_codigo, li_cama_codigo, li_lote_pltcod, li_lote_espcod, &
			li_lote_codigo, li_enva_tipoen, li_enva_codigo, li_lfcd_secuen
Boolean 	lb_retorno

lb_retorno	=	TRUE

FOR ll_fila = 1 to dw_4.RowCount()
	ll_BultosOriginales		= 	dw_4.Object.mfgd_bulent[ll_Fila]
	
	IF dw_4.Object.opvd_canbul[ll_Fila] = 0 THEN
		
		IF dw_2.Object.orpr_tipord[1] = 9 THEN
			li_plde_codigo 	= dw_4.Object.plde_codigo[ll_fila] 
			li_cama_codigo		= dw_4.Object.cama_codigo[ll_fila] 
			li_lote_pltcod 	= dw_4.Object.lote_pltcod[ll_fila] 
			li_lote_espcod 	= dw_4.Object.lote_espcod[ll_fila] 
			li_lote_codigo 	= dw_4.Object.lote_codigo[ll_fila] 
			li_enva_tipoen 	= dw_4.Object.enva_tipoen[ll_fila] 
			li_enva_codigo 	= dw_4.Object.enva_codigo[ll_fila] 
			li_lfcd_secuen		= dw_4.Object.lfcd_secuen[ll_fila] 
				
			UPDATE	dbo.spro_camaraexistecom
				SET	caex_canbul 	= :ll_BultosOriginales
			WHERE 	plde_codigo 	= :li_plde_codigo AND
						cama_codigo 	= :li_cama_codigo	AND
						lofc_pltcod 	= :li_lote_pltcod AND
						lofc_espcod 	= :li_lote_espcod AND
						lofc_lotefc 	= :li_lote_codigo;			
		ELSE
			li_plde_codigo		= dw_4.Object.plde_codigo[ll_fila] 
			li_cama_codigo		= dw_4.Object.cama_codigo[ll_fila] 
			li_lote_pltcod 	= dw_4.Object.lote_pltcod[ll_fila] 
			li_lote_espcod 	= dw_4.Object.lote_espcod[ll_fila] 
			li_lote_codigo 	= dw_4.Object.lote_codigo[ll_fila] 
			li_enva_tipoen 	= dw_4.Object.enva_tipoen[ll_fila] 
			li_enva_codigo 	= dw_4.Object.enva_codigo[ll_fila] 
				
			UPDATE	dbo.spro_camaraexistefg 
				SET	caex_canbul 	= :ll_BultosOriginales
			WHERE 	plde_codigo 	= :li_plde_codigo AND
						lote_pltcod		= :li_lote_pltcod AND
						lote_espcod		= :li_lote_espcod AND
						lote_codigo 	= :li_lote_codigo AND
						enva_tipoen		= :li_enva_tipoen AND
						enva_codigo		= :li_enva_codigo;
		END IF
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
			lb_Retorno	=	False
			ll_fila = dw_1.RowCount()
		ELSE
			lb_Retorno	=	True
		END IF
	END IF
NEXT

Return lb_retorno
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
			:ls_nodbms,		:ls_Usuario,		:ls_Password
	FROM dbo.prodconectividad   
	WHERE cone_codigo = :il_coneccion;

sqlexi.ServerName	=	ls_nomser
sqlexi.DataBase		=	ls_nombas
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

public subroutine insertamovimientotraspasocomercial (integer paso);Long 		ll_Fila, ll_FilaNueva, ll_paso
Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[16])

IF Paso=1 THEN
	FOR ll_fila = 1 To dw_1.RowCount()
		
		ll_FilaNueva 									= 	dw_6.InsertRow(0)
		dw_6.Object.plde_codigo[ll_FilaNueva] 	= 	dw_1.Object.plde_codigo[ll_Fila]
		dw_6.Object.tpmv_codigo[ll_FilaNueva]	= 	dw_1.Object.tpmv_codigo[ll_Fila]
		dw_6.Object.tepr_numero[ll_FilaNueva]	= 	dw_1.Object.mfco_numero[ll_Fila]
		dw_6.Object.tepr_secuen[ll_FilaNueva]	= 	dw_1.Object.mfcd_secuen[ll_Fila]
		dw_6.Object.lote_pltcod[ll_FilaNueva] 	= 	dw_1.Object.lofc_pltcod[ll_Fila]
		dw_6.Object.lote_espcod[ll_FilaNueva] 	= 	dw_1.Object.lofc_espcod[ll_Fila]
		dw_6.Object.lote_codigo[ll_FilaNueva] 	= 	dw_1.Object.lofc_lotefc[ll_Fila]
		dw_6.Object.enva_tipoen[ll_FilaNueva] 	= 	dw_1.Object.enva_tipoen[ll_Fila]
		dw_6.Object.enva_codigo[ll_FilaNueva]	=	dw_1.Object.enva_codigo[ll_Fila]
		dw_6.Object.tepr_bultra[ll_FilaNueva] 	= 	dw_1.Object.mfcd_bulent[ll_Fila]
		dw_6.Object.clie_codigo[ll_FilaNueva] 	= 	li_Cliente
		
	NEXT
ELSE
	IF dw_6.RowCount() > 0 THEN
		FOR ll_Fila = 1 To dw_1.RowCount()
			
			ll_FilaNueva = dw_6.Find("plde_codigo	= " 	+ 	string(dw_1.Object.plde_codigo[ll_fila]) 		+ " AND " + &
			                         			"tpmv_codigo 	= " 	+ 	string(dw_1.Object.tpmv_codigo[ll_fila]) 	+ " AND " + &
											 "tepr_numero 	= " 	+ 	string(dw_1.Object.mfco_numero[ll_fila]) 	+ " AND " + &
											 "tepr_secuen 	= " 	+ 	string(dw_1.Object.mfcd_secuen[ll_fila]) 	+ " AND " + &
											 "lote_pltcod 	= " 	+ 	string(dw_1.Object.lofc_pltcod[ll_fila]) 		+ " AND " + &
                                  				 "lote_espcod 	= " 	+ 	string(dw_1.Object.lofc_espcod[ll_fila]) 		+ " AND " + &
											 "lote_codigo 	= " 	+ 	string(dw_1.Object.lofc_lotefc[ll_fila]) 		+ " AND " + &
											 "clie_codigo 	= " 	+ 	string(li_Cliente) 									+ " AND " + &
											 "enva_tipoen 	= " 	+ 	string(dw_1.Object.enva_tipoen[ll_fila]) 		+ " AND " + &
											 "enva_codigo 	= " 	+ 	string(dw_1.Object.enva_codigo[ll_fila]), 1, dw_6.RowCount())
          	
		  	IF ll_FilaNueva > 0 THEN
				dw_6.Object.tepr_bulvac[ll_FilaNueva] = dw_1.Object.mfcd_bulent[ll_Fila]
	 	  	END IF	
				
		NEXT
	END IF	
END IF	
end subroutine

public subroutine devolucionbultos_com ();Long		 	ll_FIla, ll_sumbul, ll_filabusq
Integer 		li_lotepl1,	li_lotees1,	li_loteco1,	li_tipoen1,	li_envase1, &
		 		li_lotepl2,	li_lotees2,	li_loteco2,	li_tipoen2,	li_envase2, li_Cliente
String		ls_orden

ll_sumbul  	= 	0
li_lotepl1 	= 	dw_4.Object.lote_pltcod[1]
li_lotees1 	= 	dw_4.Object.lote_espcod[1]
li_loteco1 	= 	dw_4.Object.lote_codigo[1]
li_tipoen1 	= 	dw_4.Object.enva_tipoen[1]
li_envase1 	= 	dw_4.Object.enva_codigo[1]
ll_sumbul  	= 	dw_4.Object.opvd_canbul[1]
li_Cliente	=	Integer(istr_mant.argumento[16])

ls_orden		=	"lote_pltcod asc, lote_espcod asc, lote_codigo asc, " + &
				 	"clie_codigo asc, enva_tipoen asc, enva_codigo asc"
				 
dw_4.SetSort(ls_orden)

ls_orden		=	"lofc_pltcod asc, lofc_espcod asc, lofc_lotefc asc, " + &
				 	"clie_codigo asc, enva_tipoen asc, enva_codigo asc"
dw_1.SetSort(ls_orden)
				 
dw_4.Sort()
dw_1.Sort()

FOR ll_Fila = 2 TO dw_4.RowCount()
	li_lotepl2 		= 	dw_4.Object.lote_pltcod[ll_fila]
	li_lotees2 		= 	dw_4.Object.lote_espcod[ll_fila]
	li_loteco2 		= 	dw_4.Object.lote_codigo[ll_fila]
	li_tipoen2 		= 	dw_4.Object.enva_tipoen[ll_fila]
	li_envase2 		= 	dw_4.Object.enva_codigo[ll_fila]
	
	IF li_lotepl1	=	li_lotepl2 	AND 	&
		li_lotees1	=	li_lotees2 	AND 	&
		li_loteco1	=	li_loteco2 	AND 	&
		li_tipoen1 	=	li_tipoen2 	AND	&
		li_envase1	=	li_envase2 	THEN
		
		ll_sumbul  	=	ll_sumbul + dw_4.Object.opvd_canbul[ll_fila]
		
	ELSE
		
		ll_filabusq = dw_1.Find("lofc_pltcod 	= " + String(li_lotepl1) + " AND " + &
										"lofc_espcod 	= " + String(li_lotees1) + " AND " + &
										"lofc_lotefc 	= " + String(li_loteco1) + " AND " + &
										"clie_codigo 	= " + String(li_Cliente) + " AND " + &										
										"enva_tipoen 	= " + String(li_tipoen1) + " AND " + &
										"enva_codigo 	= " + String(li_envase1) , 1, dw_1.RowCount())
										
		IF ll_filabusq>0 THEN
			
			dw_1.Object.mfgd_bulent[ll_filabusq] = ll_sumbul
			
		END IF
		
		li_lotepl1 	= 	dw_4.Object.lote_pltcod[ll_fila]
		li_lotees1 	= 	dw_4.Object.lote_espcod[ll_fila]
		li_loteco1 	= 	dw_4.Object.lote_codigo[ll_fila]
		li_tipoen1 	= 	dw_4.Object.enva_tipoen[ll_fila]
		li_envase1 	= 	dw_4.Object.enva_codigo[ll_fila]
		ll_sumbul  	= 	dw_4.Object.opvd_canbul[ll_fila]
	END IF
NEXT	

ll_filabusq = dw_1.Find("lofc_pltcod 	= " + String(li_lotepl1) + " AND " + &
								"lofc_espcod 	= " + String(li_lotees1) + " AND " + &
								"lofc_lotefc 	= " + String(li_loteco1) + " AND " + &
								"clie_codigo 	= " + String(li_Cliente) + " AND " + &										
								"enva_tipoen 	= " + String(li_tipoen1) + " AND " + &
								"enva_codigo 	= " + String(li_envase1) , 1, dw_1.RowCount())
									
IF ll_filabusq>0 THEN
			
	dw_1.Object.mfgd_bulent[ll_filabusq] = ll_sumbul
		
END IF
						
end subroutine

public function boolean historial ();Boolean			lb_retorno	=	True
Integer			ll_fila
DwItemStatus	li_dwitemstatus
Integer			li_cliente, li_tipord, li_codmod, li_tipmov
Long				ll_planta, ll_proceso
Date 				ld_fecmov
Time				lt_hormov
String 			ls_pcname

ll_fila = 1
	
li_dwitemstatus	=	dw_2.GetItemStatus(ll_Fila, 0, Primary!)

IF li_dwitemstatus <> New! AND li_dwitemstatus <> NotModified! OR dw_4.DeletedCount() > 0 THEN
	li_cliente		=	Integer(istr_mant.argumento[16])
	ll_planta		=	dw_2.Object.plde_codigo[ll_fila]
	li_tipord		=	dw_2.Object.orpr_tipord[ll_fila]
	ll_proceso		=	dw_2.Object.orpr_numero[ll_fila]
	ld_fecmov		=	Date(f_fechahora())
	lt_hormov		=	Time(f_fechahora())
	ls_pcname		=	gstr_us.computador
			
	li_tipmov		=	4//4 termino proceso
	
	CHOOSE CASE li_dwitemstatus
		CASE NewModified!
			li_codmod	=	1
			
		CASE DataModified!
			li_codmod	=	2
			
	END CHOOSE

	IF dw_4.DeletedCount() > 0 THEN li_codmod	=	2
		
	lb_retorno	=	iuo_historico.InsertaHistoria(li_cliente, ll_planta, li_tipord, &
																ll_proceso, li_codmod, li_tipmov, &
																ld_fecmov,  lt_hormov, ls_pcname, &
																True, Sqlca)
END IF

IF NOT lb_retorno THEN Return lb_retorno

dw_4.RowsCopy(1, dw_4.DeletedCount(), Delete!, dw_10, 1, Primary!)

IF dw_10.RowCount() > 0 THEN		
	li_cliente		=	Integer(istr_mant.argumento[16])
	ll_planta		=	dw_10.Object.plde_codigo[ll_fila]
	li_tipord		=	dw_10.Object.orpr_tipord[ll_fila]
	ll_proceso		=	dw_10.Object.orpr_numero[ll_fila]
	ld_fecmov		=	Date(f_fechahora())
	lt_hormov		=	Time(f_fechahora())
	ls_pcname		=	gstr_us.computador
			
	li_tipmov		=	4//4 termino proceso
	li_codmod		=	3//1 creacion, 2 modificacion, 3 eliminacion
	
	lb_retorno	=	iuo_historico.InsertaHistoria(li_cliente, ll_planta, li_tipord, &
																ll_proceso, li_codmod, li_tipmov, &
																ld_fecmov,  lt_hormov, ls_pcname, &
																True, Sqlca)
	dw_10.Reset()
	
END IF

Return lb_retorno
end function

public function any existetermino (integer ai_fila);Integer	li_tipo, li_secuen, li_existe
Long		ll_planta, ll_numero


ll_planta	=	dw_6.Object.plde_codigo[ai_fila]
li_tipo		=	dw_6.Object.tpmv_codigo[ai_fila]
ll_numero=	dw_6.Object.tepr_numero[ai_fila]
li_secuen	=	dw_6.Object.tepr_secuen[ai_fila]

SELECT count(*) 
  INTO :li_existe
  FROM dbo.spro_terminoproceso
 WHERE :ll_planta	=	plde_codigo
   and :li_tipo	=	tpmv_codigo
   and :ll_numero	=	tepr_numero
   and :li_secuen	=	tepr_secuen;
	
IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_terminoproceso" )
		Return NewModified!
ELSEIF sqlca.SQLCode = 100 THEN
		Return NewModified!
END IF

Return DataModified!
end function

public subroutine wf_bloqueacolumnas ();
end subroutine

on w_maed_termino_proceso.create
int iCurrent
call super::create
this.dw_5=create dw_5
this.dw_4=create dw_4
this.dw_exideta=create dw_exideta
this.dw_exiencab=create dw_exiencab
this.dw_3=create dw_3
this.dw_10=create dw_10
this.dw_7=create dw_7
this.dw_6=create dw_6
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_5
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.dw_exideta
this.Control[iCurrent+4]=this.dw_exiencab
this.Control[iCurrent+5]=this.dw_3
this.Control[iCurrent+6]=this.dw_10
this.Control[iCurrent+7]=this.dw_7
this.Control[iCurrent+8]=this.dw_6
end on

on w_maed_termino_proceso.destroy
call super::destroy
destroy(this.dw_5)
destroy(this.dw_4)
destroy(this.dw_exideta)
destroy(this.dw_exiencab)
destroy(this.dw_3)
destroy(this.dw_10)
destroy(this.dw_7)
destroy(this.dw_6)
end on

event open;x				= 0
y				= 0

This.Height	= 2520
im_menu		= m_principal

Integer	li_resultado
String	ls_parametros, ls_nombre

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	"4"
istr_Mant.Argumento[16]	=	String(gi_Codexport)

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(gi_codexport)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)

istr_Mant.dw				=	dw_1
istr_Mant.solo_consulta =	False

pb_nuevo.PostEvent(Clicked!)

iuo_ordenproceso			=	Create uo_spro_ordenproceso
iuo_calicosechero			=  Create uo_calicosechero
iuo_historico				=	Create uo_control_historico_proceso
end event

event ue_nuevo;call super::ue_nuevo;Long		ll_modif1, ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
			ll_modif1	+=	dw_2.GetNextModified(0, Primary!)
			ll_modif1	+=	dw_4.GetNextModified(0, Primary!)
			
			IF dw_1.RowCount() > 0 AND ll_modif1 > 0 THEN
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

IF NOT ib_ok THEN RETURN

dw_1.Reset()
dw_4.Reset()
dw_3.Reset()
dw_5.Reset()
dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.SetRedraw(True)

dw_2.InsertRow(0)
dw_2.SetFocus()

dw_1.DataObject='dw_mant_termino_de_proceso_actualiza'
dw_4.DataObject='dw_mant_termino_de_proceso'
dw_6.DataObject='dw_mant_mues_terminoproceso'
dw_1.SetTransObject(SQLCA)
dw_4.SetTransObject(SQLCA)
dw_6.SetTransObject(SQLCA)

pb_grabar.Enabled				=	False
pb_imprimir.Enabled				=	False
dw_2.Enabled						=	True

dw_2.Object.plde_codigo[1]		=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.orpr_tipord[1]		=	4
dw_2.Object.orpr_fecpro[1]		=	Date(String(Today(),'dd/mm/yyyy'))
dw_2.Object.orpr_estado[1] 	=  1
dw_2.Object.clie_codigo[1] 		=  Integer(istr_mant.argumento[16])

istr_mant.argumento[1] 			=	String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[2] 			=	"4"
istr_mant.argumento[3]			=	""
istr_mant.argumento[4]			=	String(Date(String(Today(),'dd/mm/yyyy')))

HabilitaEncab(TRUE)

dw_2.SetColumn("orpr_numero")

end event

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta, ll_row

DO
	dw_2.SetRedraw(False)
	
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_mant.Argumento[1]), &
										  Integer(istr_mant.Argumento[2]), &
										  Integer(istr_mant.Argumento[3]), &
										  Integer(istr_mant.Argumento[16]))
										  
	ll_fila_e	=	dw_7.Retrieve(Integer(istr_mant.Argumento[1]), &
										  Integer(istr_mant.Argumento[2]), &
										  Integer(istr_mant.Argumento[3]), &
										  Integer(istr_mant.Argumento[16]))									  
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		
		DO
			ll_fila_d	=	dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
												  Integer(istr_mant.Argumento[2]), &
												  Integer(istr_mant.Argumento[3]), &
												  Integer(istr_mant.Argumento[16]))
												  
			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				ll_fila_e	=	dw_5.Retrieve(Integer(istr_mant.Argumento[1]), &
												  Integer(istr_mant.Argumento[2]), &
												  Integer(istr_mant.Argumento[3]), &
												  Integer(istr_mant.Argumento[16]))
				
				ll_fila_e	=	dw_4.Retrieve(Integer(istr_mant.Argumento[1]), &
												  Integer(istr_mant.Argumento[2]), &
												  Integer(istr_mant.Argumento[3]), &
												  Integer(istr_mant.Argumento[16]))
				
				IF ll_fila_e > 0 THEN
					ll_fila_e	=	dw_3.Retrieve(Integer(istr_mant.Argumento[1]), &
													  dw_4.Object.tpmv_codigo[1], &
													  dw_4.Object.mfge_numero[1],2, &
													  Integer(istr_mant.Argumento[16]))
				END IF
				
				pb_eli_det.Enabled	=	True
				pb_imprimir.Enabled	=	True
				pb_grabar.Enabled		=	True
				
				
				HabilitaEncab(False)
			END IF
			dw_2.SetRedraw(True)
			
		LOOP WHILE respuesta = 1
		IF respuesta = 2 THEN Close(This)
		
	END IF
	dw_2.SetRedraw(True)
	
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_antesguardar;call super::ue_antesguardar;Long 		ll_fila, ll_fila_e, ll_planta, ll_orden, ll_espcod, ll_lote, ll_tipoenv, &
         ll_enva, ll_bultos
Integer 	li_tipo, li_Cliente

ll_planta	=	gstr_ParamPlanta.CodigoPlanta
li_tipo  	=	Integer(istr_mant.argumento[2])
ll_orden		=	Long(istr_mant.argumento[3])
li_Cliente	=	Integer(istr_mant.argumento[16])

If dw_2.Object.orpr_estado[1] = 1 OR dw_2.Object.orpr_estado[1] = 4 OR dw_2.Object.orpr_estado[1] = 2 Then

	dw_2.Object.orpr_estado[1] = 2

	If dw_3.RowCount() = 0 Then
		FOR ll_fila=1 TO dw_5.rowcount()

			ll_fila_e = dw_3.Insertrow(0)

			dw_3.Object.clie_codigo[ll_fila_e] 	= 	li_Cliente
			dw_3.Object.plde_codigo[ll_fila_e] 	= 	dw_5.Object.plde_codigo[ll_fila]
			dw_3.Object.tpmv_codigo[ll_fila_e] 	= 	dw_4.Object.tpmv_codigo[1]
			dw_3.Object.meen_numero[ll_fila_e]	= 	dw_4.Object.mfge_numero[1]
			dw_3.Object.enva_tipoen[ll_fila_e] 	= 	dw_5.Object.enva_tipoen[ll_fila]
			dw_3.Object.enva_codigo[ll_fila_e] 	= 	dw_5.Object.enva_codigo[ll_fila]
			dw_3.Object.fgme_conenv[ll_fila_e] 	= 	1
			dw_3.Object.cale_calida[ll_fila_e] 	= 	dw_5.Object.cale_calida[ll_fila]
			dw_3.Object.fgme_sentid[ll_fila_e] 	= 	2
			dw_3.Object.fgme_cantid[ll_fila_e] 	= 	dw_5.Object.bultos[ll_fila]

		NEXT
	End If	

  	UPDATE dbo.spro_movtofrutagranenca  
  	   SET mfge_estmov = 2
   	WHERE ( dbo.spro_movtofrutagranenca.plde_codigo = :ll_planta ) AND
      	   ( dbo.spro_movtofrutagranenca.clie_codigo = :li_cliente ) AND  		
      	   ( dbo.spro_movtofrutagranenca.defg_tipdoc = :li_tipo ) AND  
         	( dbo.spro_movtofrutagranenca.defg_docrel = :ll_orden )  ;

	If sqlca.SQLCode = -1 Then

		F_ErrorBaseDatos(sqlca, "Actualización de Tabla de Movimiento de Fruta Encabezado")	
		Message.DoubleParm = -1
	End If	
	
	InsertaMovimientoTraspaso(1)
	DevolucionBultos()
	InsertaMovimientoTraspaso(2)

	FOR ll_fila = 1 TO dw_6.RowCount()
		dw_6.SetItemStatus(ll_fila, 0, Primary!, ExisteTermino(ll_fila))
	NEXT
End If
end event

event ue_seleccion;call super::ue_seleccion;Str_Busqueda	lstr_busq
String ls_Nula
SetNull(ls_nula)

lstr_busq.argum[01]	=	istr_mant.argumento[1]
lstr_busq.argum[15]	=	'4'
lstr_busq.argum[16]	=	istr_mant.argumento[16]


OpenWithParm(w_busc_spro_ordenproceso_traspaso, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" and lstr_busq.argum[1] <> "0" THEN
	IF iuo_ordenproceso.Existe(gstr_ParamPlanta.CodigoPlanta,&
							 Integer(istr_Mant.Argumento[2]), &
							 Integer(lstr_busq.argum[1]), True, Sqlca, &
							 integer(istr_mant.argumento[16])) THEN	

		istr_mant.argumento[3]	= lstr_busq.argum[1]
		TriggerEvent("ue_recuperadatos")

	ELSE
		dw_2.SetItem(1,"orpr_numero",long(ls_Nula))

	END IF
END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "TERMINO DE PROCESO"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

IF dw_2.Object.orpr_tipord[1] = 9 THEN
	vinf.dw_1.DataObject = "dw_info_termino_de_proceso_com"
ELSE
	vinf.dw_1.DataObject = "dw_info_termino_de_proceso"
END IF

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
								  Integer(istr_mant.Argumento[2]), &
								  Integer(istr_mant.Argumento[3]), &
								  Integer(istr_mant.Argumento[16]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event resize;call super::resize;//dw_4.x		= dw_1.x
//dw_4.y		= dw_1.y
//dw_4.Height	= dw_1.Height
//dw_4.Width	= dw_1.Width
end event

event ue_guardar;Integer	li_Cliente

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
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_termino_proceso
boolean visible = false
integer x = 265
integer y = 20
integer width = 206
integer height = 172
boolean titlebar = false
string title = "Detalle de Termino de Proceso"
string dataobject = "dw_mant_termino_de_proceso_actualiza"
boolean hscrollbar = false
boolean vscrollbar = false
boolean border = true
borderstyle borderstyle = stylebox!
end type

event dw_1::clicked;//
end event

event dw_1::doubleclicked;//
end event

event dw_1::getfocus;//
end event

event dw_1::itemfocuschanged;//
end event

event dw_1::rowfocuschanged;//
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_termino_proceso
integer x = 759
integer y = 56
integer width = 2843
integer height = 476
string dataobject = "dw_mant_termino_de_proceso_ordenenca"
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null
Integer	li_codigo, li_null
String	ls_columna

SetNull(ll_null)
SetNull(li_null)

ls_columna = GetColumnName()

CHOOSE CASE ls_columna

	CASE "clie_codigo"
		
			IF NoExisteCliente(Integer(Data)) THEN
				This.SetItem(row, ls_Columna, li_Null)
				RETURN 1
			END IF	
			
			istr_mant.argumento[16]	=	data		
		
	CASE "orpr_numero"	
		
		IF iuo_ordenproceso.Existe(gstr_ParamPlanta.CodigoPlanta,&
											Integer(istr_Mant.Argumento[2]), &
											Integer(data),True,Sqlca, &
											Integer(istr_mant.argumento[16])) THEN	
											
			IF iuo_ordenproceso.Estado = 1 THEN
				MessageBox("Error", "La Orden de proceso esta en Estado (1) ~"Vigente~".~r~n"+&
										  "Debe realizar el traspaso a proceso antes de terminar la orden.~r~n"+&
										  "Ingrese o Seleccione otra orden.")
				dw_2.SetItem(1,"orpr_numero",ll_Null)
				RETURN 1
			
			ELSEIF iuo_ordenproceso.Estado = 4 OR iuo_ordenproceso.Estado = 2 THEN
				
				istr_mant.argumento[3]	= data
				parent.TriggerEvent("ue_recuperadatos")
				
			ELSEIF iuo_ordenproceso.Estado = 5 THEN
				MessageBox("Error", "La Orden de proceso esta en Estado (5) ~"Cierre Web~".~r~n"+&
										  "Ingrese o Seleccione otra orden.")
				dw_2.SetItem(1,"orpr_numero",ll_Null)
				RETURN 1
				
			ELSEIF iuo_ordenproceso.Estado = 3 THEN
				MessageBox("Error", "La Orden de proceso esta en Estado (3) ~"Terminada~".~r~n"+&
										  "Ingrese o Seleccione otra orden.")
				dw_2.SetItem(1,"orpr_numero",ll_Null)
				RETURN 1
			END IF			
		ELSE
			dw_2.SetItem(1,"orpr_numero",ll_Null)
			RETURN 1
		END IF
		
	CASE "orpr_tipord"	
		istr_mant.argumento[2] = Data
		IF data = '9' THEN
			dw_1.DataObject='dw_mant_termino_de_proceso_actualiza_com'
			dw_4.DataObject='dw_mant_termino_de_proceso_com'
			dw_6.DataObject='dw_mant_mues_terminoproceso_com'
		ELSE
			dw_1.DataObject='dw_mant_termino_de_proceso_actualiza'
			dw_4.DataObject='dw_mant_termino_de_proceso'
			dw_6.DataObject='dw_mant_mues_terminoproceso'
		END IF
		dw_1.SetTransObject(SQLCA)
		dw_4.SetTransObject(SQLCA)
		dw_6.SetTransObject(SQLCA)

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_termino_proceso
integer x = 4398
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_termino_proceso
boolean visible = false
integer x = 4389
integer y = 540
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_termino_proceso
integer x = 4398
integer y = 700
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_termino_proceso
integer x = 4398
integer y = 880
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_termino_proceso
integer x = 4398
integer y = 1060
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_termino_proceso
boolean visible = false
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_termino_proceso
boolean visible = false
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_termino_proceso
integer x = 4398
integer y = 156
end type

type dw_5 from datawindow within w_maed_termino_proceso
boolean visible = false
integer x = 494
integer y = 4
integer width = 206
integer height = 172
integer taborder = 70
boolean bringtotop = true
string title = "dw_5"
string dataobject = "dw_mant_termino_de_proceso_agrenvase"
end type

type dw_4 from datawindow within w_maed_termino_proceso
integer x = 37
integer y = 656
integer width = 4325
integer height = 1192
integer taborder = 50
boolean titlebar = true
string title = "Detalle del Término de Proceso"
string dataobject = "dw_mant_termino_de_proceso"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exideta from datawindow within w_maed_termino_proceso
boolean visible = false
integer x = 46
integer y = 28
integer width = 206
integer height = 172
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta"
boolean livescroll = true
end type

type dw_exiencab from datawindow within w_maed_termino_proceso
boolean visible = false
integer x = 507
integer y = 212
integer width = 206
integer height = 172
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_exismovtoenca_analisis"
end type

type dw_3 from datawindow within w_maed_termino_proceso
boolean visible = false
integer x = 585
integer y = 2464
integer width = 3058
integer height = 632
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle de Envases"
string dataobject = "dw_mues_movtoenvadeta"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_10 from datawindow within w_maed_termino_proceso
boolean visible = false
integer y = 2744
integer width = 3319
integer height = 432
integer taborder = 30
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_termino_de_proceso"
boolean resizable = true
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_7 from datawindow within w_maed_termino_proceso
boolean visible = false
integer x = 279
integer y = 228
integer width = 206
integer height = 172
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_envases_proceso"
end type

type dw_6 from datawindow within w_maed_termino_proceso
boolean visible = false
integer x = 50
integer y = 224
integer width = 206
integer height = 172
integer taborder = 70
boolean bringtotop = true
string title = "dw_6"
string dataobject = "dw_mant_mues_terminoproceso"
boolean livescroll = true
end type

