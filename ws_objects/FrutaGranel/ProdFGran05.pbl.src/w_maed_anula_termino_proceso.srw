$PBExportHeader$w_maed_anula_termino_proceso.srw
forward
global type w_maed_anula_termino_proceso from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_anula_termino_proceso
end type
type dw_4 from datawindow within w_maed_anula_termino_proceso
end type
type dw_exidetaborra from datawindow within w_maed_anula_termino_proceso
end type
type dw_exismovtodetanulos from datawindow within w_maed_anula_termino_proceso
end type
type dw_10 from datawindow within w_maed_anula_termino_proceso
end type
end forward

global type w_maed_anula_termino_proceso from w_mant_encab_deta_csd
string tag = "w_maed_anula_termino_proceso"
integer width = 3707
integer height = 2296
string title = "Anulación del Término de Proceso"
string menuname = ""
event ue_despuesborrar ( )
dw_3 dw_3
dw_4 dw_4
dw_exidetaborra dw_exidetaborra
dw_exismovtodetanulos dw_exismovtodetanulos
dw_10 dw_10
end type
global w_maed_anula_termino_proceso w_maed_anula_termino_proceso

type variables
uo_spro_ordenproceso				iuo_ordenproceso
uo_control_historico_proceso	iuo_historico

Boolean								ib_Modifica, ib_AutoCommit, ib_ConectadoExistencia
Integer 								il_conexiste, il_coneccion 
Long									il_numeroenva

datawindowchild 	   		   idwc_planta

Transaction							sqlexi
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public subroutine insertamovimientotraspaso (integer paso)
public subroutine devolucionbultos ()
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean noexistecliente (integer al_codigo)
public function boolean conexionexistencia ()
public function boolean historial ()
end prototypes

event ue_despuesborrar();Long 		ll_numero, respuesta, ll_nueva, ll_fila
Boolean lb_AutoCommit
Integer	li_bodevirtual, li_bodzonal, li_devcorreo 	
String	ls_correozonal, ls_correo, ls_texto, ls_asunto, ls_error, serrormsg

IF ib_Conectadoexistencia = TRUE THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	il_numeroenva = dw_2.Object.orpr_numero[1]
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
		sqlexi.AutoCommit	=	ib_AutoCommit
		RETURN 
	ELSE
		li_bodevirtual = luo_existencia.bodvirtual
		li_bodzonal		= luo_existencia.bodzonal
	END IF
	
	IF luo_existencia.Mesproceso > dw_2.Object.orpr_fecpro[1] THEN
		Message.DoubleParm = -1
		
		IF luo_existencia.numeromaximo(3,li_bodevirtual,il_numeroenva,1,True,sqlexi) THEN
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
		
		ls_asunto = "Modifica anulación de término vaciado, Movto. Nº "+String(il_numeroenva)
//		li_devcorreo =  Send_mail("smtp.rioblanco.cl","<frutaGranel@rioblanco.cl>","<"+ls_correo+">","","",ls_asunto,ls_texto,"",ls_error)
		
//		IF (li_devcorreo<0) THEN
//			messagebox("Error No" + string(li_devcorreo),sErrorMsg)
//		END IF
		
		RETURN 
	END IF
	
	IF NOT luo_existencia.numeromaximo(3,li_bodevirtual,il_numeroenva,1,True,sqlexi) THEN
		Message.DoubleParm = -1
		Return
	ELSE
		ll_numero = luo_existencia.numero
	END IF
			
	IF isnull(ll_numero) THEN
		Return
	END IF	
	
	IF NOT luo_existencia.actualizaexistencia(2,3,li_bodevirtual,ll_numero,il_numeroenva,True,sqlexi) THEN
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
	AND	bode_codigo = :li_bodEvirtual
	USING sqlexi;
	
	IF sqlexi.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlexi,"exismovtodeta")
		Message.DoubleParm = -1
		sqlexi.AutoCommit	=	ib_AutoCommit
		RETURN 
	END IF
	
	UPDATE dbo.exismovtoenca SET
		mden_estado = 2
	WHERE mden_tipdoc = 3
	AND bode_codigo = :li_bodEvirtual
	AND mden_numero = :ll_numero
	AND mden_docrel = :il_NumeroEnva
	AND mden_estado = 1
	USING sqlexi;
	
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

	IF NOT luo_existencia.nummaxpbodega(3,li_bodzonal,li_bodevirtual,il_numeroenva,True,sqlexi) THEN
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
	WHERE mden_tipdoc = 3
	AND bode_codigo = :li_bodzonal
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
	
	DELETE FROM dbo.exismovtodeta 
	WHERE	mden_tipdoc = 3 
	AND	mden_numero = :ll_numero
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
	
	DISCONNECT USING sqlexi;
	ib_Conectadoexistencia = False
END IF




end event

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect				=	0
	dw_2.Object.orpr_tipord.Protect				=	0
	dw_2.Object.orpr_numero.Protect				=	0
	dw_2.Object.orpr_estado.Protect				=	0

	
	dw_2.Object.clie_codigo.Color					=	0
//	dw_2.Object.orpr_tipord.Color					=	0
	dw_2.Object.orpr_numero.Color				=	0
//	dw_2.Object.orpr_estado.Color					=	0
	
	dw_2.Object.clie_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.orpr_tipord.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.orpr_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.orpr_estado.BackGround.Color		=	RGB(255,255,255)
	
ELSE
	dw_2.Object.clie_codigo.Protect				=	1
	dw_2.Object.orpr_tipord.Protect				=	1
	dw_2.Object.orpr_numero.Protect				=	1
	dw_2.Object.orpr_estado.Protect				=	1
	pb_grabar.Enabled				=	True
	
	dw_2.Object.clie_codigo.Color					=	RGB(255,255,255)
//	dw_2.Object.orpr_tipord.Color					=	RGB(255,255,255)
	dw_2.Object.orpr_numero.Color				=	RGB(255,255,255)
//	dw_2.Object.orpr_estado.Color					=	RGB(255,255,255)
	
	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
	dw_2.Object.orpr_tipord.BackGround.Color		=	553648127
	dw_2.Object.orpr_numero.BackGround.Color	=	553648127
	dw_2.Object.orpr_estado.BackGround.Color		=	553648127
END IF
end subroutine

public subroutine insertamovimientotraspaso (integer paso);Long ll_Fila, ll_FilaNueva


IF Paso=1 THEN
	FOR ll_fila = 1 To dw_1.RowCount()
		
//		ll_FilaNueva = dw_6.InsertRow(0)
//		dw_6.Object.plde_codigo[ll_FilaNueva] = dw_1.Object.plde_codigo[ll_Fila]
//		dw_6.Object.tpmv_codigo[ll_FilaNueva] = dw_1.Object.tpmv_codigo[ll_Fila]
//		dw_6.Object.tepr_numero[ll_FilaNueva] = dw_1.Object.mfge_numero[ll_Fila]
//		dw_6.Object.tepr_secuen[ll_FilaNueva] = dw_1.Object.mfgd_secuen[ll_Fila]
//		dw_6.Object.lote_pltcod[ll_FilaNueva] = dw_1.Object.lote_pltcod[ll_Fila]
//		dw_6.Object.lote_espcod[ll_FilaNueva] = dw_1.Object.lote_espcod[ll_Fila]
//		dw_6.Object.lote_codigo[ll_FilaNueva] = dw_1.Object.lote_codigo[ll_Fila]
//		dw_6.Object.enva_tipoen[ll_FilaNueva] = dw_1.Object.enva_tipoen[ll_Fila]
//		dw_6.Object.enva_codigo[ll_FilaNueva] = dw_1.Object.enva_codigo[ll_Fila]
//		dw_6.Object.tepr_bultra[ll_FilaNueva] = dw_1.Object.mfgd_bulent[ll_Fila]
		
	NEXT
ELSE
//	IF dw_6.RowCount() > 0 THEN
//		FOR ll_Fila = 1 To dw_1.RowCount()
//		
//			ll_FilaNueva = dw_6.Find("plde_codigo = " + string(dw_1.Object.plde_codigo[ll_fila]) + "AND " + &
//			                         "tpmv_codigo = " + string(dw_1.Object.tpmv_codigo[ll_fila]) + "AND " + &
//											 "tepr_numero = " + string(dw_1.Object.mfge_numero[ll_fila]) + "AND " + &
//											 "tepr_secuen = " + string(dw_1.Object.mfgd_secuen[ll_fila]) + "AND " + &
//											 "lote_pltcod = " + string(dw_1.Object.lote_pltcod[ll_fila]) + "AND " + &
//                                  "lote_espcod = " + string(dw_1.Object.lote_espcod[ll_fila]) + "AND " + &
//											 "lote_codigo = " + string(dw_1.Object.lote_codigo[ll_fila]) + "AND " + &
//											 "enva_tipoen = " + string(dw_1.Object.enva_tipoen[ll_fila]) + "AND " + &
//											 "enva_codigo = " + string(dw_1.Object.enva_codigo[ll_fila]), 1, dw_6.RowCount())
//          
//		  IF ll_FilaNueva > 0 THEN
//			
//			 dw_6.Object.tepr_bulvac[ll_FilaNueva] = dw_1.Object.mfgd_bulent[ll_Fila]
//	 
//		  END IF	
//		NEXT
//	END IF	
END IF	
end subroutine

public subroutine devolucionbultos ();Long		ll_FIla, ll_filabusq
Integer 	li_lotepl, li_lotees, li_loteco, li_tipoen, li_envase, li_secuen, li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[16])

IF istr_mant.argumento[2] = '9' THEN
	FOR ll_Fila= 1 TO dw_4.RowCount()
		li_secuen 	= 	dw_4.Object.tepr_secuen[ll_fila]
		li_lotepl 		= 	dw_4.Object.lote_pltcod[ll_fila]
		li_lotees 		= 	dw_4.Object.lote_espcod[ll_fila]
		li_loteco 		= 	dw_4.Object.lote_codigo[ll_fila]
		li_tipoen 		= 	dw_4.Object.enva_tipoen[ll_fila]
		li_envase 	= 	dw_4.Object.enva_codigo[ll_fila]	
		
		ll_filabusq =  dw_1.Find("mfcd_secuen 	= " + String(li_secuen) + " AND " + &
										"lofc_pltcod		= " + String(li_lotepl) + " AND " + &
										"lofc_espcod 	= " + String(li_lotees) + " AND " + &
										"lofc_lotefc		= " + String(li_loteco) + " AND " + &
										"clie_codigo 		= " + String(li_Cliente), 1, dw_1.RowCount())
										
		IF ll_filabusq>0 THEN
			
			dw_1.Object.mfcd_bulent[ll_filabusq] = long(dw_4.Object.tepr_bultra[ll_fila])
			
		END IF	
		
	NEXT
ELSE
	FOR ll_Fila= 1 TO dw_4.RowCount()
		li_secuen = dw_4.Object.tepr_secuen[ll_fila]
		li_lotepl = dw_4.Object.lote_pltcod[ll_fila]
		li_lotees = dw_4.Object.lote_espcod[ll_fila]
		li_loteco = dw_4.Object.lote_codigo[ll_fila]
		li_tipoen = dw_4.Object.enva_tipoen[ll_fila]
		li_envase = dw_4.Object.enva_codigo[ll_fila]	
		
		ll_filabusq = dw_1.Find("mfgd_secuen 	= " + String(li_secuen) + " AND " + &
										"lote_pltcod 	= " + String(li_lotepl) + " AND " + &
										"lote_espcod 	= " + String(li_lotees) + " AND " + &
										"lote_codigo 	= " + String(li_loteco) + " AND " + &
										"clie_codigo 		= " + String(li_Cliente) + " AND " + &									
										"enva_tipoen 	= " + String(li_tipoen) + " AND " + &
										"enva_codigo 	= " + String(li_envase) , 1, dw_1.RowCount())
										
		IF ll_filabusq>0 THEN
			
			dw_1.Object.mfgd_bulent[ll_filabusq] = long(dw_4.Object.tepr_bultra[ll_fila])
			
		END IF	
		
	NEXT	
END IF
				
				
IF dw_4.RowCount() > 0 THEN dw_4.RowsMove(1,dw_4.RowCount(),Primary!,dw_4,1,Delete!)				
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean			lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

IF Borrando THEN
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
	IF dw_2.Update(True, False) = 1 THEN
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
END IF

sqlca.AutoCommit	=	ib_AutoCommit

RETURN lb_Retorno

end function

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

public function boolean conexionexistencia ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta

IF ib_ConectadoExistencia THEN
	DISCONNECT USING sqlexi;
END IF

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

SELECT  cone_nomodb,	cone_nomser,	cone_nombas,
		  cone_nodbms,	cone_nomusu,	cone_passwo  
  INTO :ls_nomodb,  :ls_nomser,    :ls_nombas,
		 :ls_nodbms,  :ls_Usuario,   :ls_Password
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
			
	li_tipmov		=	5//5 Anulación termino proceso
	
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
			
	li_tipmov		=	5//4 termino proceso
	li_codmod		=	3//1 creacion, 2 modificacion, 3 eliminacion
	
	lb_retorno	=	iuo_historico.InsertaHistoria(li_cliente, ll_planta, li_tipord, &
																ll_proceso, li_codmod, li_tipmov, &
																ld_fecmov,  lt_hormov, ls_pcname, &
																True, Sqlca)
	dw_10.Reset()
	
END IF

Return lb_retorno
end function

on w_maed_anula_termino_proceso.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_exidetaborra=create dw_exidetaborra
this.dw_exismovtodetanulos=create dw_exismovtodetanulos
this.dw_10=create dw_10
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.dw_exidetaborra
this.Control[iCurrent+4]=this.dw_exismovtodetanulos
this.Control[iCurrent+5]=this.dw_10
end on

on w_maed_anula_termino_proceso.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_exidetaborra)
destroy(this.dw_exismovtodetanulos)
destroy(this.dw_10)
end on

event open;x				= 0
y				= 0

This.Height	= 2520
im_menu		= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	"4"
istr_mant.argumento[16]	=	String(gi_codexport)

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(gi_codexport)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)

istr_Mant.dw				=	dw_1
istr_Mant.solo_consulta =	False

pb_nuevo.PostEvent(Clicked!)

iuo_ordenproceso			=	Create uo_spro_ordenproceso
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
			ll_modif2	=	dw_2.GetNextModified(0, Primary!)
			ll_modif1	=	dw_4.GetNextModified(0, Primary!)
			
//			IF ib_ModEncab OR dw_1.GetNextModified(0, Primary!) > 0 THEN
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

dw_1.Reset()
dw_4.Reset()
dw_3.Reset()

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.SetRedraw(True)

dw_2.InsertRow(0)
dw_2.SetFocus()

pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.orpr_tipord[1]	=	4
dw_2.Object.orpr_fecpro[1]	=	Date(String(Today(),'dd/mm/yyyy'))
dw_2.Object.orpr_estado[1] =  1
dw_2.Object.clie_codigo[1] =  Integer(istr_mant.argumento[16])

istr_mant.argumento[1] 		=	string(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[2] 		=	"4"
istr_mant.argumento[3]		=	""
istr_mant.argumento[4]		=	String(Date(String(Today(),'dd/mm/yyyy')))

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
				ll_fila_e	=	dw_4.Retrieve(Integer(istr_mant.Argumento[1]), &
												  Integer(istr_mant.Argumento[2]), &
												  Integer(istr_mant.Argumento[3]), &
												  Integer(istr_mant.Argumento[16]))
				
						
				IF ll_fila_e>0 THEN
					ll_fila_e	=	dw_3.Retrieve(Integer(istr_mant.Argumento[1]), &
													  dw_4.Object.tpmv_codigo[1], &
													  dw_4.Object.tepr_numero[1],2, &
													  Integer(istr_mant.Argumento[16]))
				END IF
				
				pb_eli_det.Enabled	=	True
				pb_imprimir.Enabled	=	True
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

event ue_antesguardar;call super::ue_antesguardar;Long 		ll_fila, ll_fila_e,ll_planta, ll_orden, ll_espcod, ll_lote, ll_tipoenv, ll_enva, ll_bultos
Integer	li_tipo, li_Cliente

Message.DoubleParm = 0

ll_planta		=	gstr_ParamPlanta.CodigoPlanta
li_tipo  		=	Integer(istr_mant.argumento[2])
ll_orden			=	Long(istr_mant.argumento[3])
li_Cliente		=	Integer(istr_mant.argumento[16])

il_numeroenva 	= 	dw_2.Object.orpr_numero[1]

IF dw_2.Object.orpr_estado[1]=3 THEN
	MessageBox("Atención","La orden de proceso ya se encuentra cerrada.")
	Message.DoubleParm = -1	
	RETURN
END IF	

IF dw_2.Object.orpr_estado[1]=1 THEN
	MessageBox("Atención","La orden de proceso se encuentra vigente.")
	Message.DoubleParm = -1	
	RETURN
END IF	

IF dw_2.Object.orpr_estado[1]=2 THEN

	dw_2.Object.orpr_estado[1]	=	1

	IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)

	IF istr_mant.argumento[2] = '9' THEN
		UPDATE dbo.spro_movtofrutacomenca  
			  SET mfco_estmov	= 1
			WHERE 	( dbo.spro_movtofrutacomenca.plde_codigo 	= :ll_planta ) AND 
						( dbo.spro_movtofrutacomenca.tpmv_codigo 	= 21 ) AND
						( dbo.spro_movtofrutacomenca.clie_codigo 	= :li_Cliente ) AND				
						( dbo.spro_movtofrutacomenca.mfco_tipdoc 	= :li_tipo ) AND  
						( dbo.spro_movtofrutacomenca.mfco_docrel 	= :ll_orden );
				
		IF sqlca.SQLCode = -1 THEN
		
			F_ErrorBaseDatos(sqlca, "Actualización de Tabla de Movimiento de Fruta Comercial Encabezado")	
			Message.DoubleParm = -1
			RETURN 
		END IF	
	ELSE
		UPDATE dbo.spro_movtofrutagranenca  
			  SET mfge_estmov = 1
			WHERE 	( dbo.spro_movtofrutagranenca.plde_codigo 	= :ll_planta ) AND 
						( dbo.spro_movtofrutagranenca.tpmv_codigo 	= 21 ) AND
						( dbo.spro_movtofrutagranenca.clie_codigo 	= :li_Cliente ) AND				
						( dbo.spro_movtofrutagranenca.defg_tipdoc 	= :li_tipo ) AND  
						( dbo.spro_movtofrutagranenca.defg_docrel 	= :ll_orden );
				
		IF sqlca.SQLCode = -1 THEN
		
			F_ErrorBaseDatos(sqlca, "Actualización de Tabla de Movimiento de Fruta Encabezado")	
			Message.DoubleParm = -1
			RETURN 
		END IF
	END IF
	   DevolucionBultos()
	
	
END IF
end event

event ue_seleccion;call super::ue_seleccion;Str_Busqueda	lstr_busq
String ls_Nula
SetNull(ls_nula)

lstr_busq.argum[1]	=	istr_mant.argumento[1]
lstr_busq.argum[16]	=	istr_mant.argumento[16]

OpenWithParm(w_busc_spro_ordenproceso_traspaso, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" and lstr_busq.argum[1]<> "0" THEN
	IF iuo_ordenproceso.Existe(gstr_ParamPlanta.CodigoPlanta,&
											Integer(istr_Mant.Argumento[2]), &
											Integer(lstr_busq.argum[1]),True,Sqlca,integer(istr_mant.argumento[16])) THEN	
											
			istr_mant.argumento[3]	= lstr_busq.argum[1]
			
			TriggerEvent("ue_recuperadatos")
			
		ELSE
			dw_2.SetItem(1,"orpr_numero",long(ls_Nula))
		END IF
END IF
		

		
end event

event resize;call super::resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

maximo	= dw_4.width

//IF dw_2.width > maximo THEN maximo = dw_2.width

//dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					= 37

dw_4.x					= 37 + Round((maximo - dw_4.width) / 2, 0)
dw_4.y					= 64 + dw_2.Height
dw_4.height				= This.WorkSpaceHeight() - dw_4.y - 41
//dw_4.width				= This.workspacewidth() - gb_1.width - 240


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
	
	//Crea conexion hacia existencia
	li_Cliente	=	dw_2.Object.clie_codigo[1]
	
//	SELECT 	clie_conexi, cone_codigo
//	INTO   	:il_conexiste, :il_coneccion
//	FROM dbo.clientesprod
//	WHERE clie_codigo = :li_Cliente;
//	
//	IF il_conexiste = 1 THEN
//		sqlexi	=	CREATE Transaction
//		IF Conexionexistencia() THEN
//			dw_exismovtodetanulos.SetTransObject(sqlexi)
//			dw_exidetaborra.SetTransObject(sqlexi)
//			TriggerEvent("ue_despuesborrar")
//		ELSE
//			MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)
//		END IF
//	END IF	
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF


end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_anula_termino_proceso
boolean visible = false
integer x = 2953
integer y = 80
integer width = 206
integer height = 168
boolean titlebar = false
string title = "Detalle de Termino de Proceso"
string dataobject = "dw_mant_termino_de_proceso_actualiza"
boolean hscrollbar = false
boolean vscrollbar = false
boolean border = true
boolean livescroll = false
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

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_anula_termino_proceso
integer x = 96
integer y = 56
integer width = 2839
integer height = 484
string dataobject = "dw_mant_termino_de_proceso_ordenenca"
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null
Integer	li_codigo, li_null
String	ls_columna

SetNull(li_null)
SetNull(ll_null)

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
											Integer(data),True,Sqlca,integer(istr_mant.argumento[16])) THEN	
											
			IF iuo_ordenproceso.Estado = 2 THEN
				
				istr_mant.argumento[3]	= data
				parent.TriggerEvent("ue_recuperadatos")
				
			ELSEIF iuo_ordenproceso.Estado = 5 THEN
				MessageBox("Error", "La Orden de proceso esta en Estado (5) ~"Cierre Web~".~r~n"+&
										  "Ingrese o Seleccione otra orden.")
				dw_2.SetItem(1,"orpr_numero",ll_Null)
				RETURN 1
				
			ELSEIF iuo_ordenproceso.Estado = 4 THEN
				MessageBox("Error", "La Orden de proceso esta en Estado (5) ~"Mod. Desde Packing~".~r~n"+&
										  "Ingrese o Seleccione otra orden.")
				dw_2.SetItem(1,"orpr_numero",ll_Null)
				RETURN 1
				
			ELSEIF iuo_ordenproceso.Estado = 3 THEN
				MessageBox("Error", "La Orden de proceso esta en Estado (3) ~"Cerrado~".~r~n"+&
										  "Ingrese o Seleccione otra orden.")
				dw_2.SetItem(1,"orpr_numero",ll_Null)
				RETURN 1
				
			ELSEIF iuo_ordenproceso.Estado = 1 THEN
				MessageBox("Error", "La Orden de proceso esta en Estado (1) ~"Pendiente~".~r~n"+&
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
			dw_4.DataObject='dw_mant_mues_terminoproceso_com'
		ELSE
			dw_1.DataObject='dw_mant_termino_de_proceso_actualiza'
			dw_4.DataObject='dw_mant_mues_terminoproceso'
		END IF
		dw_1.SetTransObject(SQLCA)
		dw_4.SetTransObject(SQLCA)

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_anula_termino_proceso
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_anula_termino_proceso
boolean visible = false
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_anula_termino_proceso
boolean enabled = true
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_anula_termino_proceso
boolean visible = false
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_anula_termino_proceso
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_anula_termino_proceso
boolean visible = false
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_anula_termino_proceso
boolean visible = false
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_anula_termino_proceso
end type

type dw_3 from datawindow within w_maed_anula_termino_proceso
boolean visible = false
integer x = 2958
integer y = 264
integer width = 192
integer height = 168
integer taborder = 50
boolean bringtotop = true
string title = "Detalle de Envases"
string dataobject = "dw_mues_movtoenvadeta"
end type

type dw_4 from datawindow within w_maed_anula_termino_proceso
integer x = 14
integer y = 676
integer width = 3163
integer height = 1192
integer taborder = 50
boolean titlebar = true
string title = "Detalle del Término de Proceso"
string dataobject = "dw_mant_mues_terminoproceso"
boolean hscrollbar = true
boolean vscrollbar = true
string icon = "OleGenReg!"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exidetaborra from datawindow within w_maed_anula_termino_proceso
boolean visible = false
integer x = 3054
integer y = 164
integer width = 219
integer height = 192
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta_borra"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exismovtodetanulos from datawindow within w_maed_anula_termino_proceso
boolean visible = false
integer x = 3072
integer y = 364
integer width = 229
integer height = 188
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_inserta_exismovtodetanulos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_10 from datawindow within w_maed_anula_termino_proceso
boolean visible = false
integer x = 2953
integer y = 452
integer width = 201
integer height = 160
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_mues_terminoproceso"
end type

