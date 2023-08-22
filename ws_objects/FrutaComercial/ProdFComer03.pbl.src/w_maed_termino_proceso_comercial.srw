$PBExportHeader$w_maed_termino_proceso_comercial.srw
forward
global type w_maed_termino_proceso_comercial from w_mant_encab_deta_csd
end type
type dw_5 from datawindow within w_maed_termino_proceso_comercial
end type
type dw_4 from datawindow within w_maed_termino_proceso_comercial
end type
type dw_exiencab from datawindow within w_maed_termino_proceso_comercial
end type
type dw_exideta from datawindow within w_maed_termino_proceso_comercial
end type
type dw_6 from datawindow within w_maed_termino_proceso_comercial
end type
type dw_3 from datawindow within w_maed_termino_proceso_comercial
end type
end forward

global type w_maed_termino_proceso_comercial from w_mant_encab_deta_csd
string tag = "w_maed_termino_proceso"
integer width = 4946
integer height = 2012
string title = "Término de Proceso"
string menuname = ""
event ue_despuesguardar ( )
dw_5 dw_5
dw_4 dw_4
dw_exiencab dw_exiencab
dw_exideta dw_exideta
dw_6 dw_6
dw_3 dw_3
end type
global w_maed_termino_proceso_comercial w_maed_termino_proceso_comercial

type variables
uo_doctointernopack		iuo_doctointerno
uo_clientesprod			iuo_cliente
uo_calicosechero  			iuo_calicosechero

Boolean						ib_Modifica, ib_AutoCommit, ib_ConectadoExistencia
Long         					ll_Numenva
Long							il_coneccion, il_conexiste

datawindowchild 	      idwc_planta

Transaction		sqlexi
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine insertamovimientotraspaso (integer paso)
public subroutine buscamovenvase (integer ai_planta, integer ai_tipomov, long al_numero, integer ai_movenva)
public subroutine devolucionbultos ()
public function boolean buscalotesintraspaso ()
public function boolean conexionexistencia ()
end prototypes

event ue_despuesguardar();integer	li_bodega, li_fila, li_cliente, li_planta,li_enva_codigo, li_enva_tipoen, li_bodzonal, li_Bodcomercial
Long 		ll_fila, ll_numero, li_secuencia = 1, li_filarecep, ll_filarecep, li_secuenciarece = 1,&
			ll_numerorecep, ll_fila_nueva, ll_fila_nea, ll_count, ll_docrel, ll_numnuevoini, ll_numnuevofin
String	ls_item_codigo, ls_productor, ls_prod, ls_calidad
Boolean	lb_AutoCommit, lb_Retorno

IF ib_Conectadoexistencia THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	ll_docrel = dw_2.Object.dinp_numero[1]
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
	ELSE
		li_Bodcomercial = luo_existencia.BodDestino
		li_bodzonal		 = luo_existencia.bodzonal
	END IF		
	
	luo_existencia.existeencabezado(ll_docrel,li_Bodcomercial,1,3,True,sqlexi)
		
	IF luo_existencia.count <> 0 THEN
		Return 
	END IF	
	
	IF Not luo_existencia.correlativobode(3,li_Bodcomercial,li_bodzonal,True,sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
	ELSE
		ll_numero = luo_existencia.numero
	END IF	
		
	IF isnull(ll_numero) THEN
		ll_numnuevoini = Long(String(li_Bodcomercial)+''+'0001')
		ll_numnuevofin = Long(String(li_Bodcomercial)+''+'9999')
		
		INSERT INTO dbo.correlbode(bode_codigo,mden_tipdoc,cobo_bodadm,cobo_inicia,cobo_final,cobo_actual)
		VALUES(:li_Bodcomercial,3,:li_bodzonal,:ll_numnuevoini,:ll_numnuevofin,:ll_numnuevoini)
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
	dw_exiencab.Object.mden_fecmov[ll_fila_nea] 	= 	dw_2.Object.dinp_fecdoc[1]
	dw_exiencab.Object.tpmv_tipomv[ll_fila_nea] 	= 	2
	dw_exiencab.Object.tpmv_codigo[ll_fila_nea] 	= 	2
	dw_exiencab.Object.mden_tipana[ll_fila_nea] 	= 	4
	dw_exiencab.Object.bode_codigo[ll_fila_nea] 	= 	li_Bodcomercial
	dw_exiencab.Object.mden_bodest[ll_fila_nea] 	= 	li_bodzonal
	dw_exiencab.Object.clpr_rut[ll_fila_nea]	 	= 	ls_productor
	dw_exiencab.Object.mden_docrel[ll_fila_nea] 	= 	dw_2.Object.dinp_numero[1]
	dw_exiencab.Object.mden_fecdre[ll_fila_nea] 	= 	dw_2.Object.dinp_fecdoc[1]
	dw_exiencab.Object.mden_observ[ll_fila_nea] 	= 	'Traspaso Cta. Cte. Envases'
	dw_exiencab.Object.mden_estado[ll_fila_nea] 	= 	1
	dw_exiencab.Object.mden_pcopda[ll_fila_nea] 	=	1
	dw_exiencab.Object.mden_estaci[ll_fila_nea] 	= 	gstr_us.computador
	dw_exiencab.Object.mden_fecdig[ll_fila_nea] 	= 	Date(Today())
	dw_exiencab.Object.mden_hordig[ll_fila_nea] 	= 	Time(Now())
	
	FOR li_fila = 1 TO dw_3.RowCount()
		IF dw_3.Object.fgme_cantid[li_fila] > 0 THEN
			ll_fila				=	dw_exideta.InsertRow(0)
			
			li_enva_codigo 	= 	dw_3.Object.enva_codigo[li_fila]
			li_enva_tipoen 	= 	dw_3.Object.enva_tipoen[li_fila]
			ls_calidad			=  dw_3.Object.cale_calida[li_fila]
			
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
			dw_exideta.Object.mdde_fecmov[ll_fila] 	= 	dw_2.Object.dinp_fecdoc[1]
			dw_exideta.Object.bode_codigo[ll_fila] 	= 	li_Bodcomercial
			dw_exideta.Object.mdde_cantid[ll_fila] 	= 	dw_3.Object.fgme_cantid[li_fila]
						
			li_secuencia = li_secuencia + 1
		END IF
	NEXT	
	
	IF Not luo_existencia.correlativobode(1,li_bodzonal,li_bodzonal,True,sqlexi) THEN
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
	
	dw_exiencab.Object.mden_tipdoc[ll_fila_nueva] 	= 	1
	dw_exiencab.Object.mden_numero[ll_fila_nueva] 	= 	ll_numerorecep 
	dw_exiencab.Object.tpdo_codigo[ll_fila_nueva] 	= 	2
	dw_exiencab.Object.mden_fecmov[ll_fila_nueva] 	= 	dw_2.Object.dinp_fecdoc[1]
	dw_exiencab.Object.tpmv_tipomv[ll_fila_nueva] 	= 	1
	dw_exiencab.Object.tpmv_codigo[ll_fila_nueva] 	= 	5
	dw_exiencab.Object.mden_tipana[ll_fila_nueva] 	= 	3
	dw_exiencab.Object.bode_codigo[ll_fila_nueva] 	= 	li_bodzonal
	dw_exiencab.Object.mden_bodest[ll_fila_nueva] 	= 	li_Bodcomercial
	dw_exiencab.Object.clpr_rut[ll_fila_nueva]	 	= 	ls_productor
	dw_exiencab.Object.mden_docrel[ll_fila_nueva] 	= 	dw_2.Object.dinp_numero[1]
	dw_exiencab.Object.mden_fecdre[ll_fila_nueva] 	= 	dw_2.Object.dinp_fecdoc[1]
	dw_exiencab.Object.mden_observ[ll_fila_nueva] 	= 	'Traspaso Cta. Cte. Envases'
	dw_exiencab.Object.mden_estado[ll_fila_nueva] 	= 	1
	dw_exiencab.Object.mden_pcopda[ll_fila_nueva] 	= 	1
	
	FOR li_filarecep = 1 TO dw_3.RowCount()
		IF dw_3.Object.fgme_cantid[li_filarecep] > 0 THEN
			ll_filarecep		=	dw_exideta.InsertRow(0)
			
			li_enva_codigo	=	dw_3.Object.enva_codigo[li_filarecep]
			li_enva_tipoen =	dw_3.Object.enva_tipoen[li_filarecep]
			ls_calidad		=  dw_3.Object.cale_calida[li_filarecep]
			
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
			dw_exideta.Object.mden_numero[ll_filarecep] 	= 	ll_numerorecep
			dw_exideta.Object.mdde_secuen[ll_filarecep] 	= 	li_secuenciarece 
			dw_exideta.Object.tpmv_tipomv[ll_filarecep] 	=	1
			dw_exideta.Object.tpmv_codigo[ll_filarecep] 	=	5 
			dw_exideta.Object.item_codigo[ll_filarecep]	=	ls_item_codigo
			dw_exideta.Object.mdde_identi[ll_filarecep] 	=	''
			dw_exideta.Object.mdde_fecmov[ll_filarecep] 	= 	dw_2.Object.dinp_fecdoc[1]
			dw_exideta.Object.bode_codigo[ll_filarecep] 	= 	li_bodzonal
			dw_exideta.Object.mdde_cantid[ll_filarecep] 	= 	dw_3.Object.fgme_cantid[li_filarecep]
			li_secuenciarece 										= 	li_secuenciarece + 1
		END IF	
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
	dw_2.Object.orpr_tipord.Protect				=	0
	dw_2.Object.orpr_tipord.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.orpr_numero.Protect				=	0
	dw_2.Object.orpr_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.orpr_estado.Protect				=	0
	dw_2.Object.orpr_estado.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.clie_codigo.Protect				=	0
	dw_2.Object.clie_codigo.BackGround.Color	=	RGB(255,255,255)
	
ELSE
	dw_2.Object.orpr_tipord.Protect				=	1
	dw_2.Object.orpr_tipord.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.orpr_numero.Protect				=	1
	dw_2.Object.orpr_numero.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.orpr_estado.Protect				=	1
	dw_2.Object.orpr_estado.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.clie_codigo.Protect				=	1
	dw_2.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)

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
				IF dw_6.Update(True, False) = 1 THEN
					IF buscalotesintraspaso() THEN
						Commit;
				
						IF sqlca.SQLCode <> 0 THEN
							F_ErrorBaseDatos(sqlca, This.Title)
						ELSE
							lb_Retorno	=	True
				
							dw_1.ResetUpdate()
							dw_2.ResetUpdate()
							dw_3.ResetUpdate()
							dw_6.ResetUpdate()
							dw_4.Retrieve(Integer(istr_mant.Argumento[1]), &
										  Integer(istr_mant.Argumento[2]), &
										  Integer(istr_mant.Argumento[3]), &
										  Integer(istr_mant.Argumento[7]))
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

public subroutine insertamovimientotraspaso (integer paso);Long ll_Fila, ll_FilaNueva


IF Paso=1 THEN
	FOR ll_fila = 1 To dw_1.RowCount()
		
		ll_FilaNueva = dw_6.InsertRow(0)
		dw_6.Object.plde_codigo[ll_FilaNueva] = dw_1.Object.plde_codigo[ll_Fila]
		dw_6.Object.tpmv_codigo[ll_FilaNueva] = dw_1.Object.tpmv_codigo[ll_Fila]
		dw_6.Object.tepr_numero[ll_FilaNueva] = dw_1.Object.mfco_numero[ll_Fila]
		dw_6.Object.tepr_secuen[ll_FilaNueva] = dw_1.Object.mfcd_secuen[ll_Fila]
		dw_6.Object.lote_pltcod[ll_FilaNueva] = dw_1.Object.lofc_pltcod[ll_Fila]
		dw_6.Object.lote_espcod[ll_FilaNueva] = dw_1.Object.lofc_espcod[ll_Fila]
		dw_6.Object.lote_codigo[ll_FilaNueva] = dw_1.Object.lofc_lotefc[ll_Fila]
		dw_6.Object.lote_secuen[ll_FilaNueva] = dw_1.Object.lfcd_secuen[ll_Fila]
		dw_6.Object.enva_tipoen[ll_FilaNueva] = dw_1.Object.enva_tipoen[ll_Fila]
		dw_6.Object.enva_codigo[ll_FilaNueva] = dw_1.Object.enva_codigo[ll_Fila]
		dw_6.Object.tepr_bultra[ll_FilaNueva] = dw_1.Object.mfcd_bulent[ll_Fila]
		
	NEXT
ELSE
	IF dw_6.RowCount() > 0 THEN
		FOR ll_Fila = 1 To dw_1.RowCount()
		
			ll_FilaNueva = dw_6.Find("plde_codigo = " + string(dw_1.Object.plde_codigo[ll_fila]) + "AND " + &
			                         "tpmv_codigo = " + string(dw_1.Object.tpmv_codigo[ll_fila]) + "AND " + &
											 "tepr_numero = " + string(dw_1.Object.mfco_numero[ll_fila]) + "AND " + &
											 "tepr_secuen = " + string(dw_1.Object.mfcd_secuen[ll_fila]) + "AND " + &
											 "lote_pltcod = " + string(dw_1.Object.lofc_pltcod[ll_fila]) + "AND " + &
                                  "lote_espcod = " + string(dw_1.Object.lofc_espcod[ll_fila]) + "AND " + &
											 "lote_codigo = " + string(dw_1.Object.lofc_lotefc[ll_fila]) + "AND " + &
											 "enva_tipoen = " + string(dw_1.Object.enva_tipoen[ll_fila]) + "AND " + &
											 "enva_codigo = " + string(dw_1.Object.enva_codigo[ll_fila]), 1, dw_6.RowCount())
          
		  IF ll_FilaNueva > 0 THEN
			
			 dw_6.Object.tepr_bulvac[ll_FilaNueva] = dw_1.Object.mfcd_bulent[ll_Fila]
	 
		  END IF	
		NEXT
	END IF	
END IF	
end subroutine

public subroutine buscamovenvase (integer ai_planta, integer ai_tipomov, long al_numero, integer ai_movenva);
SELECT meen_numero  INTO :ll_Numenva
  FROM dbo.spro_movtoenvaenca
 WHERE plde_codigo = :ai_planta
   AND tpmv_codigo = :ai_movenva
	AND tpmv_codrec = :ai_tipomov
	AND mfge_numero = :al_numero
	AND meen_modulo = 2;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento Envase Comercial")
END IF

IF isnull(ll_NumEnva) THEN ll_NumEnva=0

end subroutine

public subroutine devolucionbultos ();long		 	ll_FIla, ll_sumbul, ll_filabusq
Integer 		li_lotepl1,	li_lotees1,	li_loteco1,	li_tipoen1,	li_envase1, li_sec1, &
		 		li_lotepl2,	li_lotees2,	li_loteco2,	li_tipoen2,	li_envase2, li_sec2,li_Cliente
String		ls_orden

ll_sumbul  	= 	0
li_lotepl1 	= 	dw_4.Object.lofc_pltcod[1]
li_lotees1 	= 	dw_4.Object.lofc_espcod[1]
li_loteco1 	= 	dw_4.Object.lofc_lotefc[1]
li_tipoen1 	= 	dw_4.Object.enva_tipoen[1]
li_envase1 	= 	dw_4.Object.enva_codigo[1]
ll_sumbul  	= 	dw_4.Object.opvd_canbul[1]
li_sec1		=	dw_4.Object.lfcd_secuen[1]

li_Cliente	=	Integer(istr_mant.argumento[7])

ls_orden		=	"lote_pltcod asc, lote_espcod asc, lote_codigo asc, " + &
				 	"lfcd_secuen asc, clie_codigo asc, enva_tipoen asc, enva_codigo asc"
dw_4.SetSort(ls_orden)

ls_orden		=	"lofc_pltcod asc, lofc_espcod asc, lofc_lotefc asc, " + &
				 	"lfcd_secuen asc, clie_codigo asc, enva_tipoen asc, enva_codigo asc"
dw_1.SetSort(ls_orden)
				 
dw_4.Sort()
dw_1.Sort()

FOR ll_Fila = 2 TO dw_4.RowCount()
	li_lotepl2 		= 	dw_4.Object.lofc_pltcod[ll_fila]
	li_lotees2 		= 	dw_4.Object.lofc_espcod[ll_fila]
	li_loteco2 		= 	dw_4.Object.lofc_lotefc[ll_fila]
	li_tipoen2 		= 	dw_4.Object.enva_tipoen[ll_fila]
	li_envase2 		= 	dw_4.Object.enva_codigo[ll_fila]
	li_sec2			=	dw_4.Object.lfcd_secuen[ll_fila]
	
	IF li_lotepl1	=	li_lotepl2 	AND 	&
		li_lotees1	=	li_lotees2 	AND 	&
		li_loteco1	=	li_loteco2 	AND 	&
		li_tipoen1 	=	li_tipoen2 	AND	&
		li_envase1	=	li_envase2 	AND	&
		li_sec1		=	li_sec2 		THEN
		
		ll_sumbul  	=	ll_sumbul + dw_4.Object.opvd_canbul[ll_fila]
		
	ELSE
		
		ll_filabusq = dw_1.Find("lofc_pltcod 	= " + String(li_lotepl1) + " AND " + &
										"lofc_espcod 	= " + String(li_lotees1) + " AND " + &
										"lofc_lotefc 	= " + String(li_loteco1) + " AND " + &
										"clie_codigo 	= " + String(li_Cliente) + " AND " + &										
										"enva_tipoen 	= " + String(li_tipoen1) + " AND " + &
										"enva_codigo 	= " + String(li_envase1) + " AND " + &
										"lfcd_secuen 	= " + String(li_sec1) , 1, dw_1.RowCount())
										
		IF ll_filabusq>0 THEN
			
			dw_1.Object.mfgd_bulent[ll_filabusq] = ll_sumbul
			
		END IF
		
		li_lotepl1 	= 	dw_4.Object.lofc_pltcod[ll_fila]
		li_lotees1 	= 	dw_4.Object.lofc_espcod[ll_fila]
		li_loteco1 	= 	dw_4.Object.lofc_lotefc[ll_fila]
		li_tipoen1 	= 	dw_4.Object.enva_tipoen[ll_fila]
		li_envase1 	= 	dw_4.Object.enva_codigo[ll_fila]
		ll_sumbul  	= 	dw_4.Object.opvd_canbul[ll_fila]
		li_sec1		=	dw_4.Object.lfcd_secuen[1]
	END IF
NEXT	

ll_filabusq = dw_1.Find("lofc_pltcod 	= " + String(li_lotepl1) + " AND " + &
								"lofc_espcod 	= " + String(li_lotees1) + " AND " + &
								"lofc_lotefc 	= " + String(li_loteco1) + " AND " + &
								"clie_codigo 	= " + String(li_Cliente) + " AND " + &										
								"enva_tipoen 	= " + String(li_tipoen1) + " AND " + &
								"enva_codigo 	= " + String(li_envase1) + " AND " + &
								"lfcd_secuen 	= " + String(li_sec1) , 1, dw_1.RowCount())
									
IF ll_filabusq>0 THEN
			
	dw_1.Object.mfgd_bulent[ll_filabusq] = ll_sumbul
		
END IF
end subroutine

public function boolean buscalotesintraspaso ();Long 		ll_fila, ll_FilaNueva, ll_BultosOriginales
Integer	li_Cliente, li_plde_codigo, li_cama_codigo, li_lote_pltcod, li_lote_espcod, &
			li_lote_codigo, li_enva_tipoen, li_enva_codigo, li_lfcd_secuen
Boolean 	lb_retorno

lb_retorno	=	TRUE

FOR ll_fila = 1 to dw_4.RowCount()
	ll_BultosOriginales		= 	dw_4.Object.mfgd_bulent[ll_Fila]
	
	IF dw_4.Object.opvd_canbul[ll_Fila] = 0 THEN
		li_plde_codigo 	= dw_4.Object.plde_codigo[ll_fila] 
		li_cama_codigo		= dw_4.Object.cama_codigo[ll_fila] 
		li_lote_pltcod 	= dw_4.Object.lofc_pltcod[ll_fila] 
		li_lote_espcod 	= dw_4.Object.lofc_espcod[ll_fila] 
		li_lote_codigo 	= dw_4.Object.lofc_lotefc[ll_fila] 
		li_enva_tipoen 	= dw_4.Object.enva_tipoen[ll_fila] 
		li_enva_codigo 	= dw_4.Object.enva_codigo[ll_fila] 
		li_lfcd_secuen		= dw_4.Object.lfcd_secuen[ll_fila] 
			
		UPDATE	dbo.spro_camaraexistecom
			SET	caex_canbul 	= :ll_BultosOriginales
		WHERE 	plde_codigo 	= :li_plde_codigo AND
					cama_codigo 	= :li_cama_codigo	AND
					lofc_pltcod 	= :li_lote_pltcod AND
					lofc_espcod 	= :li_lote_espcod AND
					lofc_lotefc 	= :li_lote_codigo AND
					lfcd_secuen		= :li_lfcd_secuen;	
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

on w_maed_termino_proceso_comercial.create
int iCurrent
call super::create
this.dw_5=create dw_5
this.dw_4=create dw_4
this.dw_exiencab=create dw_exiencab
this.dw_exideta=create dw_exideta
this.dw_6=create dw_6
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_5
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.dw_exiencab
this.Control[iCurrent+4]=this.dw_exideta
this.Control[iCurrent+5]=this.dw_6
this.Control[iCurrent+6]=this.dw_3
end on

on w_maed_termino_proceso_comercial.destroy
call super::destroy
destroy(this.dw_5)
destroy(this.dw_4)
destroy(this.dw_exiencab)
destroy(this.dw_exideta)
destroy(this.dw_6)
destroy(this.dw_3)
end on

event open;call super::open;istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	"5"
istr_Mant.Argumento[7]	=	String(gstr_parempresa.empr_codexp)


dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve()

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)

istr_Mant.dw				=	dw_1
istr_Mant.solo_consulta =	False

pb_nuevo.PostEvent(Clicked!)

iuo_doctointerno	=	Create uo_doctointernopack
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

			IF dw_1.RowCount() > 0 and ll_modif1>0 THEN
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
dw_5.Reset()
dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.SetRedraw(True)

dw_2.InsertRow(0)
dw_2.SetFocus()

pb_grabar.Enabled				=	False
pb_imprimir.Enabled			=	False
dw_2.Enabled					=	True

istr_mant.argumento[1] 		=	string(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[2] 		=	"5"
istr_mant.argumento[3]		=	""
istr_mant.argumento[4]		=	String(Date(String(Today(),'dd/mm/yyyy')))

dw_2.Object.plde_codigo[1]	=	integer(istr_mant.argumento[1])
dw_2.Object.orpr_tipord[1]	=	integer(istr_mant.argumento[2])
dw_2.Object.orpr_fecpro[1]	=	Date(Today())
dw_2.Object.orpr_estado[1] =  1
dw_2.Object.clie_codigo[1]	=	integer(istr_Mant.Argumento[7])

HabilitaEncab(TRUE)

dw_2.SetColumn("clie_codigo")

end event

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta, ll_row

DO
	dw_2.SetRedraw(False)
	
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_Mant.Argumento[7]), &
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
												  Integer(istr_mant.Argumento[3]), &
												  Integer(istr_mant.Argumento[7]))
												  
			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				ll_fila_e	=	dw_4.Retrieve(Integer(istr_Mant.Argumento[1]), &
												  Integer(istr_mant.Argumento[2]), &
												  Integer(istr_mant.Argumento[3]), &
												  Integer(istr_mant.Argumento[7]))
				
				ll_fila_e	=	dw_5.Retrieve(Integer(istr_mant.Argumento[1]), &
												  Integer(istr_mant.Argumento[2]), &
												  Integer(istr_mant.Argumento[3]), &
												  Integer(istr_mant.Argumento[7]))
				
				IF ll_fila_d>0 THEN
					ll_fila_d	=	dw_3.Retrieve(Integer(istr_mant.Argumento[1]), &
													  dw_4.Object.tpmv_codigo[1], &
													  dw_4.Object.mfge_numero[1], 2, &
													  Integer(istr_mant.Argumento[7]))
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

event ue_antesguardar;call super::ue_antesguardar;Long ll_fila, ll_fila_e,ll_planta, ll_orden, ll_espcod, ll_lote, ll_tipoenv, ll_enva, ll_bultos
Integer li_tipo

ll_planta	=	gstr_ParamPlanta.CodigoPlanta
li_tipo  	=	Integer(istr_mant.argumento[2])
ll_orden		=	Long(istr_mant.argumento[3])

IF dw_2.Object.orpr_estado[1]	=	3 THEN
   Message.DoubleParm 			= -1
	
ELSEIF dw_2.Object.orpr_estado[1]	=	1 THEN
	dw_2.Object.orpr_estado[1]	=	2
	
   IF dw_4.RowCount() > 0 THEN 
		BuscaMovEnvase(dw_4.Object.plde_codigo[1],dw_4.Object.tpmv_codigo[1],&
								dw_4.Object.mfge_numero[1],dw_4.Object.tpmv_codigo[1])
		IF ll_NumEnva=0 THEN
			messagebox("Error de Conexión","No se puede Encontrar Movimiento de Envase")
			Message.DoubleParm = -1
			RETURN
		END IF
	ELSE
		messagebox("Error de Datos","No hay detalle de vaciado.")
		Message.DoubleParm = -1
		RETURN
	END IF							
	
	IF dw_3.RowCount()=0 THEN
		FOR ll_fila=1 TO dw_5.rowcount()
			ll_fila_e = dw_3.Insertrow(0)
		
			dw_3.Object.plde_codigo[ll_fila_e] 	= 	dw_4.Object.plde_codigo[1]
			dw_3.Object.clie_codigo[ll_fila_e] 	= 	dw_2.Object.clie_codigo[1]
			dw_3.Object.tpmv_codigo[ll_fila_e] 	= 	dw_4.Object.tpmv_codigo[1]
			dw_3.Object.meen_numero[ll_fila_e] 	= 	dw_4.Object.mfge_numero[1]
			dw_3.Object.enva_tipoen[ll_fila_e] 	= 	dw_5.Object.enva_tipoen[ll_fila]
			dw_3.Object.enva_codigo[ll_fila_e] 	= 	dw_5.Object.enva_codigo[ll_fila]
			dw_3.Object.fgme_conenv[ll_fila_e] 	= 	1
			dw_3.Object.cale_calida[ll_fila_e] 	= 	dw_5.Object.cale_calida[ll_fila]
			dw_3.Object.fgme_sentid[ll_fila_e] 	= 	2
			dw_3.Object.fgme_cantid[ll_fila_e] 	= 	dw_5.Object.bultos[ll_fila]
	
		NEXT
	END IF	

  	UPDATE dbo.spro_movtofrutacomenca  
   	  SET mfco_estmov = 2
   	WHERE ( dbo.spro_movtofrutacomenca.plde_codigo = :ll_planta ) AND  
      	   ( dbo.spro_movtofrutacomenca.mfco_tipdoc = :li_tipo ) AND  
         	( dbo.spro_movtofrutacomenca.mfco_docrel = :ll_orden )  ;
			
		IF sqlca.SQLCode = -1 THEN
		
   		F_ErrorBaseDatos(sqlca, "Actualización de Tabla de Movimiento de Fruta Encabezado")	
			Message.DoubleParm = -1
		
		END IF	
		
	insertamovimientotraspaso(1)	
   devolucionbultos()
	insertamovimientotraspaso(2)
END IF
end event

event ue_seleccion;call super::ue_seleccion;Str_Busqueda	lstr_busq
String ls_Nula
SetNull(ls_nula)

lstr_busq.argum[1]	=	istr_mant.argumento[2]
lstr_busq.argum[2]	=	""

OpenWithParm(w_busqueda_doctointernopack, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[2] <> "" THEN
	
	IF iuo_doctointerno.Existe(Integer(istr_Mant.Argumento[7]), &
										gstr_ParamPlanta.CodigoPlanta,&
										Integer(istr_Mant.Argumento[2]), &
										Integer(lstr_busq.argum[3]),True,Sqlca) THEN	
											
			istr_mant.argumento[3]	= lstr_busq.argum[3]
			
			TriggerEvent("ue_recuperadatos")
			
	ELSE
		dw_2.SetItem(1,"orpr_numero",integer(ls_Nula))
		RETURN 
	END IF

END IF		
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "TERMINO DE PROCESO"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_termino_procesocomercial"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
								  Integer(istr_mant.Argumento[2]), &
								  Integer(istr_mant.Argumento[3]))

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
	
//	li_Cliente	=	dw_2.Object.clie_codigo[1]
//	
//	SELECT 	clie_conexi, cone_codigo
//	INTO   	:il_conexiste, :il_coneccion
//	FROM dbo.clientesprod
//	WHERE clie_codigo = :li_Cliente;
//	
//	IF il_conexiste = 1 THEN
//		sqlexi	=	CREATE Transaction
//		IF Conexionexistencia() THEN
//			dw_exideta.SetTransObject(sqlexi)
//			dw_exiencab.SetTransObject(sqlexi)	
//			TriggerEvent("ue_despuesguardar")
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

event resize;call super::resize;dw_4.x		= dw_1.x
dw_4.y		= dw_1.y
dw_4.Width	= dw_1.Width
dw_4.Height	= dw_1.Height
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_termino_proceso_comercial
boolean visible = false
integer y = 516
integer width = 4224
integer height = 356
string title = "Detalle de Termino de Proceso"
string dataobject = "dw_mant_termino_procesocomer_actualiza"
boolean hscrollbar = false
boolean vscrollbar = false
boolean resizable = true
boolean livescroll = false
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

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_termino_proceso_comercial
integer x = 306
integer y = 72
integer width = 2857
integer height = 284
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
			istr_Mant.Argumento[7]		=	Data
		END IF
		
	CASE "orpr_numero"	
		
		IF iuo_doctointerno.Existe(Integer(istr_Mant.Argumento[7]), &
											gstr_ParamPlanta.CodigoPlanta,&
											Integer(istr_Mant.Argumento[2]), &
											Long(data),True,Sqlca) THEN	
											
			istr_mant.argumento[3]		= data
			
			parent.TriggerEvent("ue_recuperadatos")
			
		ELSE
			dw_2.Object.orpr_numero[1] = ll_Null
			RETURN 1
		END IF
		
	CASE "orpr_tipord"	
		istr_mant.argumento[2] = Data
		

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_termino_proceso_comercial
integer x = 4416
integer y = 264
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_termino_proceso_comercial
boolean visible = false
integer x = 4416
integer y = 440
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_termino_proceso_comercial
integer x = 4416
integer y = 628
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_termino_proceso_comercial
integer x = 4416
integer y = 804
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_termino_proceso_comercial
integer x = 4416
integer y = 984
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_termino_proceso_comercial
boolean visible = false
integer x = 4416
integer y = 1372
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_termino_proceso_comercial
boolean visible = false
integer x = 4416
integer y = 1544
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_termino_proceso_comercial
integer x = 4416
integer y = 80
end type

type dw_5 from datawindow within w_maed_termino_proceso_comercial
boolean visible = false
integer x = 3232
integer y = 8
integer width = 229
integer height = 156
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_termino_procesocom_agrenvase"
end type

type dw_4 from datawindow within w_maed_termino_proceso_comercial
integer x = 59
integer y = 420
integer width = 4238
integer height = 1432
integer taborder = 50
boolean titlebar = true
string title = "Detalle del Término de Proceso"
string dataobject = "dw_mant_termino_de_proceso_com"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exiencab from datawindow within w_maed_termino_proceso_comercial
boolean visible = false
integer x = 1056
integer y = 2052
integer width = 686
integer height = 400
integer taborder = 80
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_exismovtoenca_analisis"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exideta from datawindow within w_maed_termino_proceso_comercial
boolean visible = false
integer x = 1486
integer y = 1956
integer width = 686
integer height = 400
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_6 from datawindow within w_maed_termino_proceso_comercial
string tag = "guarda el movimiento para deshacer los cambios"
boolean visible = false
integer x = 3497
integer y = 100
integer width = 229
integer height = 156
integer taborder = 70
boolean bringtotop = true
string dataobject = "dw_mant_mues_terminoproceso_comer"
end type

type dw_3 from datawindow within w_maed_termino_proceso_comercial
boolean visible = false
integer x = 3237
integer y = 184
integer width = 229
integer height = 156
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_movtoenvadeta"
end type

