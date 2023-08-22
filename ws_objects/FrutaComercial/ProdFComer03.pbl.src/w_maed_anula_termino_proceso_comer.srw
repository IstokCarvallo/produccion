$PBExportHeader$w_maed_anula_termino_proceso_comer.srw
$PBExportComments$Anula proceso de Término fruta Comercial
forward
global type w_maed_anula_termino_proceso_comer from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_anula_termino_proceso_comer
end type
type dw_4 from datawindow within w_maed_anula_termino_proceso_comer
end type
type dw_exidetaborra from datawindow within w_maed_anula_termino_proceso_comer
end type
type dw_exismovtodetanulos from datawindow within w_maed_anula_termino_proceso_comer
end type
end forward

global type w_maed_anula_termino_proceso_comer from w_mant_encab_deta_csd
string tag = "w_maed_anula_termino_proceso"
integer width = 3648
integer height = 2232
string title = "Anulación del Término de Proceso"
string menuname = ""
event ue_despuesborrar ( )
dw_3 dw_3
dw_4 dw_4
dw_exidetaborra dw_exidetaborra
dw_exismovtodetanulos dw_exismovtodetanulos
end type
global w_maed_anula_termino_proceso_comer w_maed_anula_termino_proceso_comer

type variables
uo_doctointernopack		iuo_doctointerno
uo_clientesprod			iuo_cliente

Boolean						ib_Modifica, ib_AutoCommit, ib_ConectadoExistencia
Long				il_coneccion, il_conexiste, il_numeroenva

Transaction		sqlexi

datawindowchild 	      idwc_planta
end variables

forward prototypes
public subroutine insertamovimientotraspaso (integer paso)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine habilitaencab (boolean habilita)
public subroutine devolucionbultos ()
public function boolean conexionexistencia ()
end prototypes

event ue_despuesborrar();Long 		ll_numero, respuesta, ll_nueva, ll_fila
Boolean lb_AutoCommit
Integer	li_bodzonal, li_bodecomercial 	

IF ib_Conectadoexistencia = TRUE THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
		sqlexi.AutoCommit	=	ib_AutoCommit
		RETURN 
	ELSE
		li_bodecomercial = luo_existencia.BodDestino
		li_bodzonal		= luo_existencia.bodzonal
	END IF	
	
	IF NOT luo_existencia.numeromaximo(3,li_bodecomercial,il_numeroenva,1,True,sqlexi) THEN
		Message.DoubleParm = -1
		Return
	ELSE
		ll_numero = luo_existencia.numero
	END IF
			
	IF isnull(ll_numero) THEN
		Return
	END IF	
	
	IF NOT luo_existencia.actualizaexistencia(2,3,li_bodecomercial,ll_numero,il_numeroenva,True,sqlexi) THEN
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
	AND	bode_codigo = :li_bodecomercial
	USING sqlexi;
	
	IF sqlexi.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlexi,"exismovtodeta")
		Message.DoubleParm = -1
		sqlexi.AutoCommit	=	ib_AutoCommit
		RETURN 
	END IF
	
	UPDATE dba.exismovtoenca SET
		mden_estado = 2
	WHERE mden_tipdoc = 3
	AND bode_codigo = :li_bodecomercial
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

	IF NOT luo_existencia.nummaxpbodega(1,li_bodzonal,li_bodecomercial,il_numeroenva,True,sqlexi) THEN
		F_ErrorBaseDatos(sqlexi,"exismovtoenca")
		Message.DoubleParm = -1
		Return
	ELSE	
		ll_numero = luo_existencia.numero
	END IF	

	IF isnull(ll_numero) THEN
		Return
	END IF	
		
	UPDATE dba.exismovtoenca SET
	mden_estado = 2
	WHERE mden_tipdoc = 1
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
	
	DELETE FROM dba.exismovtodeta 
	WHERE	mden_tipdoc = 1 
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

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect				=	0
	dw_2.Object.clie_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.dinp_tipdoc.Protect				=	0
	dw_2.Object.dinp_tipdoc.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.dinp_numero.Protect				=	0
	dw_2.Object.dinp_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.dinp_estado.Protect				=	0
	dw_2.Object.dinp_estado.BackGround.Color	=	RGB(255,255,255)
	
ELSE
	dw_2.Object.clie_codigo.Protect				=	1
	dw_2.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.dinp_tipdoc.Protect				=	1
	dw_2.Object.dinp_tipdoc.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.dinp_numero.Protect				=	1
	dw_2.Object.dinp_numero.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.dinp_estado.Protect				=	1
	dw_2.Object.dinp_estado.BackGround.Color	=	RGB(166,180,210)
END IF
end subroutine

public subroutine devolucionbultos ();LONG ll_FIla, ll_filabusq,ll_loteco
Integer li_lotepl,li_lotees,li_tipoen,li_envase, li_secuen, li_lotese


FOR ll_Fila= 1 TO dw_4.RowCount()
	li_secuen = dw_4.Object.tepr_secuen[ll_fila]
	li_lotepl = dw_4.Object.lote_pltcod[ll_fila]
	li_lotees = dw_4.Object.lote_espcod[ll_fila]
	ll_loteco = dw_4.Object.lote_codigo[ll_fila]
	li_lotese = dw_4.Object.lote_secuen[ll_fila]
	li_tipoen = dw_4.Object.enva_tipoen[ll_fila]
	li_envase = dw_4.Object.enva_codigo[ll_fila]	
	
	ll_filabusq = dw_1.Find("mfcd_secuen = " + String(li_secuen) + " AND " + &
									"lofc_pltcod = " + String(li_lotepl) + " AND " + &
								   "lofc_espcod = " + String(li_lotees) + " AND " + &
								   "lofc_lotefc = " + String(ll_loteco) + " AND " + &
									"lfcd_secuen = " + String(li_lotese) + " AND " + &
								   "enva_tipoen = " + String(li_tipoen) + " AND " + &
								   "enva_codigo = " + String(li_envase) , 1, dw_1.RowCount())
									
	IF ll_filabusq>0 THEN
		
		dw_1.Object.mfcd_bulent[ll_filabusq] = long(dw_4.Object.tepr_bultra[ll_fila])
		
	END IF	
	
NEXT	
				
				
IF dw_4.RowCount() > 0 THEN dw_4.RowsMove(1,dw_4.RowCount(),Primary!,dw_4,1,Delete!)				
end subroutine

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

on w_maed_anula_termino_proceso_comer.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_exidetaborra=create dw_exidetaborra
this.dw_exismovtodetanulos=create dw_exismovtodetanulos
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.dw_exidetaborra
this.Control[iCurrent+4]=this.dw_exismovtodetanulos
end on

on w_maed_anula_termino_proceso_comer.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_exidetaborra)
destroy(this.dw_exismovtodetanulos)
end on

event open;x				= 0
y				= 0

This.Height	= 2020
im_menu		= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True


istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	"5"
istr_Mant.Argumento[10]	=	String(gstr_parempresa.empr_codexp)


dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve()


dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)


istr_Mant.dw				=	dw_1
istr_Mant.solo_consulta =	False

pb_nuevo.PostEvent(Clicked!)

iuo_doctointerno	= 	Create uo_doctointernopack
iuo_cliente			=	Create uo_clientesprod
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

IF Not ib_ok THEN RETURN

dw_1.Reset()
dw_4.Reset()
dw_3.Reset()

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.SetRedraw(True)

dw_2.InsertRow(0)
dw_2.SetFocus()

pb_grabar.Enabled			=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.dinp_tipdoc[1]	=	5
dw_2.Object.dinp_fecdoc[1]	=	Date(Today())
dw_2.Object.dinp_estado[1] =  1
dw_2.Object.clie_codigo[1]	=	Integer(istr_Mant.Argumento[10])

istr_mant.argumento[1] 		=	string(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[2] 		=	"5"
istr_mant.argumento[3]		=	""
istr_mant.argumento[4]		=	String(Date(String(Today(),'dd/mm/yyyy')))

HabilitaEncab(TRUE)

dw_2.SetColumn("clie_codigo")

end event

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta, ll_row

DO
	dw_2.SetRedraw(False)
	
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_Mant.Argumento[10]),&
										  Integer(istr_mant.Argumento[1]), &
										  Integer(istr_mant.Argumento[2]), &
										  Integer(istr_mant.Argumento[3]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		
		DO
			ll_fila_d	=	dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
												  Integer(istr_mant.Argumento[2]), &
												  Integer(istr_mant.Argumento[3]),&
												  Integer(istr_Mant.Argumento[10]))
												  
			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				ll_fila_e	=	dw_4.Retrieve(Integer(istr_mant.Argumento[1]), &
												  Integer(istr_mant.Argumento[2]), &
												  Integer(istr_mant.Argumento[3]))
				
						
				IF ll_fila_e>0 THEN
					ll_fila_e	=	dw_3.Retrieve(Integer(istr_mant.Argumento[1]), &
													  dw_4.Object.tpmv_codigo[1], &
													  dw_4.Object.tepr_numero[1],2,&
													  Integer(istr_Mant.Argumento[10]))
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

event ue_antesguardar;call super::ue_antesguardar;Long ll_fila, ll_fila_e,ll_planta, ll_orden, ll_espcod, ll_lote, ll_tipoenv, ll_enva, ll_bultos
Integer li_tipo

Message.DoubleParm = 0
ll_planta	=	gstr_ParamPlanta.CodigoPlanta
li_tipo  	=	Integer(istr_mant.argumento[2])
ll_orden		=	Long(istr_mant.argumento[3])

il_numeroenva 	= 	dw_2.Object.dinp_numero[1]

IF dw_4.RowCount() <= 0 THEN
	Message.DoubleParm = -1	
	RETURN
END IF	

IF dw_2.Object.dinp_estado[1]=3 THEN
	MessageBox("Atención","La orden de Re - Proceso ya se encuentra cerrada.")
	Message.DoubleParm = -1	
	RETURN
END IF	

IF dw_2.Object.dinp_estado[1]=1 THEN
	MessageBox("Atención","La orden de Re - Proceso se encuentra vigente.")
	Message.DoubleParm = -1	
	RETURN
END IF	

IF dw_2.Object.dinp_estado[1]=2 THEN

	dw_2.Object.dinp_estado[1]=1

	IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)

  	UPDATE dba.spro_movtofrutacomenca  
   	  SET mfco_estmov = 1
   	WHERE ( plde_codigo = :ll_planta ) AND 
		      ( tpmv_codigo = 25 ) AND
      	   ( mfco_tipdoc = :li_tipo ) AND  
         	( mfco_docrel = :ll_orden );
			
		IF sqlca.SQLCode = -1 THEN
		
   		F_ErrorBaseDatos(sqlca, "Actualización de Tabla de Movimiento de Fruta Encabezado")	
			Message.DoubleParm = -1
		   RETURN 
		END IF	
		
	 devolucionbultos()
	
	
END IF
end event

event ue_seleccion;call super::ue_seleccion;Str_Busqueda	lstr_busq
String ls_Nula
//SetNull(ls_nula)
//
//lstr_busq.argum[1]	=	istr_mant.argumento[1]
//
//OpenWithParm(w_busc_spro_ordenproceso_traspaso, lstr_busq)
//
//lstr_busq	=	Message.PowerObjectParm
//
//IF lstr_busq.argum[1] <> "" and lstr_busq.argum[1]<> "0" THEN
//	Str_Busqueda	lstr_busq
//String ls_Nula
//SetNull(ls_nula)
//
//lstr_busq.argum[1]	=	istr_mant.argumento[2]
//lstr_busq.argum[2]	=	""
//
//OpenWithParm(w_busqueda_doctointernopack, lstr_busq)
//
//lstr_busq	=	Message.PowerObjectParm
//
//IF lstr_busq.argum[2] <> "" THEN
//	
//	IF iuo_doctointerno.Existe(gstr_ParamPlanta.CodigoPlanta,&
//										Integer(istr_Mant.Argumento[2]), &
//										Integer(lstr_busq.argum[3]),True,Sqlca) THEN	
//											
//			istr_mant.argumento[3]	= lstr_busq.argum[3]
//			
//			TriggerEvent("ue_recuperadatos")
//			
//	ELSE
//		dw_2.SetItem(1,"dinp_numero",integer(ls_Nula))
//		RETURN 
//	END IF
//
//END IF	
//END IF
end event

event ue_imprimir();SetPointer(HourGlass!)

Long		fila

//istr_info.titulo	= "TERMINO DE PROCESO"
//istr_info.copias	= 1
//
//OpenWithParm(vinf,istr_info)
//
//vinf.dw_1.DataObject = "dw_info_termino_de_proceso"
//
//vinf.dw_1.SetTransObject(sqlca)
//
//fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
//								  Integer(istr_mant.Argumento[2]), &
//								  Integer(istr_mant.Argumento[3]))
//
//IF fila = -1 THEN
//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
//ELSEIF fila = 0 THEN
//	MessageBox( "No Existe información", "No existe información para este informe.", &
//					StopSign!, Ok!)
//ELSE
//	F_Membrete(vinf.dw_1)
//	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
//	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')
//
//	vinf.Visible	= True
//	vinf.Enabled	= True
//END IF

SetPointer(Arrow!)
end event

event ue_guardar;call super::ue_guardar;Integer		li_cliente


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
	
	SELECT 	clie_conexi, cone_codigo
	INTO   	:il_conexiste, :il_coneccion
	FROM dba.clientesprod
	WHERE clie_codigo = :li_Cliente;
	
	IF il_conexiste = 1 THEN
		sqlexi	=	CREATE Transaction
		IF Conexionexistencia() THEN
			dw_exismovtodetanulos.SetTransObject(sqlexi)
			dw_exidetaborra.SetTransObject(sqlexi)
			TriggerEvent("ue_despuesborrar")
		ELSE
			MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)
		END IF
	END IF	
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF


end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_anula_termino_proceso_comer
boolean visible = false
integer x = 50
integer y = 1320
integer width = 3095
integer height = 516
string title = "Detalle de Termino de Proceso Comercial"
string dataobject = "dw_mant_termino_procesocomer_actualiza"
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

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_anula_termino_proceso_comer
integer x = 192
integer y = 56
integer width = 2853
integer height = 336
string dataobject = "dw_mant_termino_procesocom_ordenenca"
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null
Integer	li_codigo
String	ls_columna


SetNull(ll_null)
ls_columna = GetColumnName()

CHOOSE CASE ls_columna
	CASE "clie_codigo"
		IF NOT iuo_cliente.Existe(Integer(data), TRUE, SQLCA) THEN
			dw_2.Object.clie_codigo[1] = ll_Null
			RETURN 1
		ELSE
			dw_2.Object.dinp_numero[1] = 	ll_Null
			istr_Mant.Argumento[10]		=	Data
		END IF
		
	CASE "dinp_numero"	
		
		IF iuo_doctointerno.Existe(Integer(istr_Mant.Argumento[10]),&
											Integer(gstr_ParamPlanta.CodigoPlanta),&
											Integer(istr_Mant.Argumento[2]), &
											Long(data),True,Sqlca) THEN	
											
			istr_mant.argumento[3]	= data
			
			parent.TriggerEvent("ue_recuperadatos")
			
		ELSE
			dw_2.SetItem(1,"dinp_numero",ll_Null)
			RETURN 1
		END IF
		
	CASE "dinp_tipdoc"	
		istr_mant.argumento[2] = Data
		

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_anula_termino_proceso_comer
integer x = 3296
integer y = 388
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_anula_termino_proceso_comer
boolean visible = false
integer x = 3296
integer y = 564
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_anula_termino_proceso_comer
integer x = 3296
integer y = 752
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_anula_termino_proceso_comer
boolean visible = false
integer x = 3296
integer y = 928
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_anula_termino_proceso_comer
integer x = 3296
integer y = 1108
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_anula_termino_proceso_comer
boolean visible = false
integer x = 3296
integer y = 1496
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_anula_termino_proceso_comer
boolean visible = false
integer x = 3296
integer y = 1668
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_anula_termino_proceso_comer
integer y = 204
end type

type dw_3 from datawindow within w_maed_anula_termino_proceso_comer
boolean visible = false
integer x = 69
integer y = 884
integer width = 3058
integer height = 424
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle de Envases"
string dataobject = "dw_mues_movtoenvadeta"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_maed_anula_termino_proceso_comer
integer x = 14
integer y = 420
integer width = 3214
integer height = 1448
integer taborder = 50
boolean titlebar = true
string title = "Detalle del Término de Proceso Comercial"
string dataobject = "dw_mant_mues_terminoproceso_comer"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exidetaborra from datawindow within w_maed_anula_termino_proceso_comer
boolean visible = false
integer x = 1710
integer y = 2000
integer width = 686
integer height = 400
integer taborder = 80
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta_borra"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exismovtodetanulos from datawindow within w_maed_anula_termino_proceso_comer
boolean visible = false
integer x = 2464
integer y = 1996
integer width = 686
integer height = 400
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_inserta_exismovtodetanulos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

