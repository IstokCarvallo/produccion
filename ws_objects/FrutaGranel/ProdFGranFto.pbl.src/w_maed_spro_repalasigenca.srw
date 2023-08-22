$PBExportHeader$w_maed_spro_repalasigenca.srw
$PBExportComments$Ventana de Repaletizaje.
forward
global type w_maed_spro_repalasigenca from w_mant_encab_deta_csd
end type
type dw_3 from uo_dw within w_maed_spro_repalasigenca
end type
type pb_nvopdest from picturebutton within w_maed_spro_repalasigenca
end type
type pb_nvoporg from picturebutton within w_maed_spro_repalasigenca
end type
type dw_5 from uo_dw within w_maed_spro_repalasigenca
end type
type cb_todos from commandbutton within w_maed_spro_repalasigenca
end type
type pb_acepta from picturebutton within w_maed_spro_repalasigenca
end type
type dw_6 from uo_dw within w_maed_spro_repalasigenca
end type
type dw_4 from uo_dw within w_maed_spro_repalasigenca
end type
type pb_cancela from picturebutton within w_maed_spro_repalasigenca
end type
end forward

global type w_maed_spro_repalasigenca from w_mant_encab_deta_csd
integer width = 3634
integer height = 2048
string title = "REPALLETIZAJE"
string menuname = ""
event ue_recuperapallet_destino ( )
event ue_recuperapallet_origen ( )
dw_3 dw_3
pb_nvopdest pb_nvopdest
pb_nvoporg pb_nvoporg
dw_5 dw_5
cb_todos cb_todos
pb_acepta pb_acepta
dw_6 dw_6
dw_4 dw_4
pb_cancela pb_cancela
end type
global w_maed_spro_repalasigenca w_maed_spro_repalasigenca

type variables
DataWindowChild	idwc_planta, idwc_espdest, idwc_esporig, idwc_vardest, idwc_varorig, &
						idwc_catdest, idwc_catorig, idwc_tipopaldest, idwc_tipopalorig, idwc_embdest, &
						idwc_emborig, idwc_proddest, idwc_prodorig, idwc_caldest, idwc_calorig

str_envase						istr_envase
str_calibreenvase				istr_calibre

uo_exportadores				iuo_exportador
uo_tipopallenvase				iuo_tipopallet
uo_productores					iuo_productor
uo_spro_palletencab			iuo_spro_palletencab

String							is_ultimacol, is_palletorig, is_palletdest, is_marca, is_marcao = 'M'
Boolean							ib_Modifica, ib_AutoCommit, ib_ExistePalletDestino = False, &
                           ib_detalle
Boolean							ib_inspeccion_origen = FALSE, ib_inspeccion_destino = FALSE

end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public function boolean habilitapallet ()
public function boolean existedetallepallet (long al_pallet)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine asigna_palletnuevo ()
public function boolean existepallet (long al_pallet, boolean ab_mensaje)
public function boolean modificafolio ()
public function boolean controla_totalcajas (integer ai_traspa)
public function long certifica ()
public subroutine agregatodos (boolean ab_todos)
public subroutine asigna_inspec_palletnuevo ()
public function boolean atributos_pallet ()
public subroutine captura_total_cajas ()
public subroutine traspasa_cajas (integer ai_certifica)
public function boolean integridad ()
end prototypes

event ue_recuperapallet_destino();Long	ll_fila_d, ll_fila_e, respuesta
Integer li_tipoenva, li_envase

DO
	dw_3.SetRedraw(False)
	dw_3.Reset()

	IF dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), &
						  Integer(istr_Mant.Argumento[2]), &
						  Long(istr_Mant.Argumento[4])) =  -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE

		DO
			IF dw_3.RowCount()>0 THEN
				li_tipoenva = dw_3.Object.enva_tipoen[1]
				li_envase   = dw_3.Object.enva_codigo[1]
				dw_3.GetChild("tpen_codigo", idwc_tipopaldest)
				idwc_tipopaldest.SetTransObject(SQLCA)
				IF idwc_tipopaldest.Retrieve(li_tipoenva,li_envase) = 0 THEN
					idwc_tipopaldest.InsertRow(0)
				END IF
				IF iuo_tipopallet.Existe(li_tipoenva, li_envase, dw_3.Object.tpen_codigo[1],True,SqlCa) THEN
					dw_3.SetItem(1, "tota_ccajas", iuo_tipopallet.CajasPallet)
				END IF
				Habilitaencab(FALSE)
			END IF 	
			
			IF dw_4.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  Long(istr_Mant.Argumento[4])) = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_nvopdest.Enabled	=	True
				dw_4.SetRow(1)
				dw_4.SetFocus()
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_3.SetRedraw(True)
	dw_4.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_recuperapallet_origen();Long	ll_fila_d, ll_fila_e, respuesta
Integer li_tipoenva, li_envase

DO
	dw_5.SetRedraw(False)
	dw_5.Reset()

	IF dw_5.Retrieve(Integer(istr_Mant.Argumento[1]), &
						  Integer(istr_Mant.Argumento[2]), &
						  Long(istr_Mant.Argumento[5])) =  -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE

		DO
			IF dw_5.RowCount()>0 THEN
				li_tipoenva = dw_5.Object.enva_tipoen[1]
				li_envase   = dw_5.Object.enva_codigo[1]
				dw_5.GetChild("tpen_codigo", idwc_tipopalorig)
				idwc_tipopalorig.SetTransObject(SQLCA)
				IF idwc_tipopalorig.Retrieve(li_tipoenva,li_envase) = 0 THEN
					idwc_tipopalorig.InsertRow(0)
				END If
				IF iuo_tipopallet.Existe(li_tipoenva, li_envase, dw_5.Object.tpen_codigo[1],True,SqlCa) THEN
					dw_5.SetItem(1, "tota_ccajas", iuo_tipopallet.CajasPallet)
				END IF
				Habilitaencab(FALSE)
			END IF 	
			
			IF dw_6.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  Long(istr_Mant.Argumento[5])) = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE

				pb_nvoporg.Enabled	=	True
				pb_acepta.Enabled	=	True

				dw_6.SetRow(1)
				dw_6.SetFocus()
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_5.SetRedraw(True)
	dw_5.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.expo_codigo.Protect 	=	0
	dw_2.Object.expo_codigo.BackGround.Color = RGB(255,255,255)	
	dw_2.Object.repa_fecrep.Protect	=	0
	dw_2.Object.repa_fecrep.BackGround.Color = RGB(255,255,255)
	dw_2.Object.repa_respon.Protect	=	0
	dw_2.Object.repa_respon.BackGround.Color = RGB(255,255,255)
	dw_2.Object.repa_tipopa.Protect	=	0
	dw_2.Object.repa_tipopa.BackGround.Color = RGB(255,255,255)
	dw_2.Object.repa_lugarp.Protect	=	0
	dw_2.Object.repa_lugarp.BackGround.Color = RGB(255,255,255)
	dw_2.Object.repa_observ.Protect	=	0
	dw_2.Object.repa_observ.BackGround.Color = RGB(255,255,255)
ELSE
	dw_2.Object.expo_codigo.Protect = 1
	dw_2.Object.expo_codigo.BackGround.Color = RGB(192,192,192)
	dw_2.Object.repa_fecrep.Protect	=	1
	dw_2.Object.repa_fecrep.BackGround.Color = RGB(192,192,192)
	dw_2.Object.repa_respon.Protect	=	1
	dw_2.Object.repa_respon.BackGround.Color = RGB(192,192,192)
	dw_2.Object.repa_tipopa.Protect	=	1
	dw_2.Object.repa_tipopa.BackGround.Color = RGB(192,192,192)
	dw_2.Object.repa_lugarp.Protect	=	1
	dw_2.Object.repa_lugarp.BackGround.Color = RGB(192,192,192)
	dw_2.Object.repa_observ.Protect	=	1
	dw_2.Object.repa_observ.BackGround.Color = RGB(192,192,192)
END IF
end subroutine

public function boolean habilitapallet ();DateTime		ld_fecha

dw_2.AcceptText()

IF IsNull(dw_2.Object.expo_codigo[1]) OR dw_2.Object.expo_codigo[1] = 0 THEN
	RETURN False
END IF	

IF IsNull(dw_2.Object.repa_fecrep[1]) OR dw_2.Object.repa_fecrep[1] = ld_fecha THEN
	RETURN False
END IF	

IF IsNull(dw_2.Object.repa_respon[1]) OR dw_2.Object.repa_respon[1] = 0 THEN
	RETURN False
END IF	

IF IsNull(dw_2.Object.repa_tipopa[1]) OR dw_2.Object.repa_tipopa[1] = 0 THEN
	RETURN False
END IF	

IF IsNull(dw_2.Object.repa_lugarp[1]) OR dw_2.Object.repa_lugarp[1] = 0 THEN
	RETURN False
END IF	

RETURN TRUE
end function

public function boolean existedetallepallet (long al_pallet);Integer li_cantidad, li_exportador, li_planta

li_exportador	=	Integer(istr_Mant.Argumento[1])
li_planta		=	Integer(istr_Mant.Argumento[2])

SELECT Count(*) INTO :li_cantidad
  FROM dba.spro_palletfruta
 WHERE expo_codigo = :li_exportador
   AND plde_codigo = :li_planta
	AND paen_numero = :al_pallet;
 
 IF isnull(li_cantidad) THEN li_cantidad = 0
 
 IF li_cantidad > 0 THEN RETURN TRUE
 
 RETURN FALSE
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_Retorno, lb_autocommit

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

IF NOT Borrando THEN
	IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		dw_2.SetItem(1,"repa_usuari",gstr_us.nombre)
		dw_2.SetItem(1,"repa_comput",gstr_us.computador)
		dw_2.SetItem(1,"repa_horacr",F_FechaHora())
	END IF
END IF

lb_autocommit = SQLCA.Autocommit
SQLCA.Autocommit = FALSE

IF Borrando THEN
	IF dw_4.Update(True, False) = 1 THEN
		IF dw_3.Update(True, False) = 1 THEN
			IF dw_6.Update(True,False) = 1 THEN
				IF dw_1.Update(True, False) = 1 THEN
					IF dw_2.Update(True, False) = 1 THEN
						Commit;
			
						IF sqlca.SQLCode <> 0 THEN
							F_ErrorBaseDatos(sqlca, This.Title)
						ELSE
							lb_Retorno	=	True
			
							dw_1.ResetUpdate()
							dw_2.ResetUpdate()
							dw_6.ResetUpdate()
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
	IF dw_3.Update(True, False) = 1 THEN
		IF dw_4.Update(True, False) = 1 THEN
			IF dw_6.Update(True, False) = 1 THEN
				IF dw_5.Update(True,False) = 1 THEN
					IF dw_2.Update(True, False) = 1 THEN
						IF dw_1.Update(True, False) = 1 THEN
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

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine asigna_palletnuevo ();
IF NOT ib_ExistePalletDestino THEN
	
		dw_3.Object.plde_codigo[1]	=	dw_5.Object.plde_codigo[1]
		dw_3.Object.espe_codigo[1]	=	dw_5.Object.espe_codigo[1]
		dw_3.Object.vari_codigo[1]	=	dw_5.Object.vari_codigo[1]
		dw_3.Object.cate_codigo[1]	=	dw_5.Object.cate_codigo[1]
		dw_3.Object.cama_codigo[1]	=	dw_5.Object.cama_codigo[1]
		dw_3.Object.enva_tipoen[1]	=	dw_5.Object.enva_tipoen[1]
		dw_3.Object.enva_codigo[1]	=	dw_5.Object.enva_codigo[1]
		//dw_3.Object.dest_codigo[1]	=	dw_5.Object.dest_codigo[1]
		//dw_3.Object.reci_codigo[1]	=	dw_5.Object.reci_codigo[1]
		dw_3.Object.tpen_codigo[1] =  dw_5.Object.tpen_codigo[1]
		dw_3.Object.frio_tipofr[1]	=	dw_5.Object.frio_tipofr[1]
		dw_3.Object.etiq_codigo[1]	=	dw_5.Object.etiq_codigo[1]
		dw_3.Object.paen_estado[1]	=	1
		dw_3.Object.paen_feccon[1]	=	dw_2.Object.repa_fecrep[1]
		dw_3.Object.paen_altura[1] =  dw_5.Object.paen_altura[1]
		dw_3.Object.paen_concal[1] =  dw_5.Object.paen_concal[1]
		dw_3.Object.paen_fumiga[1]	=	dw_5.Object.paen_fumiga[1]
		dw_3.Object.paen_nroban[1] =  dw_5.Object.paen_nroban[1]
		dw_3.Object.paen_nropos[1] =  dw_5.Object.paen_nropos[1]
		dw_3.Object.paen_nropis[1] =  dw_5.Object.paen_nropis[1]
		dw_3.Object.paen_temsal[1] =  dw_5.Object.paen_temsal[1]	
		dw_3.Object.paen_temlle[1] =  dw_5.Object.paen_temlle[1]
		dw_3.Object.paen_status[1] =  dw_5.Object.paen_status[1]
		dw_3.Object.paen_pmixto[1] =  dw_5.Object.paen_pmixto[1]
		dw_3.Object.sepl_codigo[1] =  dw_5.Object.sepl_codigo[1]
		dw_3.Object.paen_mesfac[1] =  dw_5.Object.paen_mesfac[1]
		dw_3.Object.paen_calexp[1] =  dw_5.Object.paen_calexp[1]
		dw_3.Object.paen_kilexp[1] =  dw_5.Object.paen_kilexp[1]				
		dw_3.Object.tota_ccajas[1] =  dw_5.Object.tota_ccajas[1]
		
		If dw_2.Object.repa_nrosag[1] > 0 Then
			dw_3.Object.paen_inspec[1] =  dw_5.Object.paen_inspec[1]
		End If 
		
		//dw_3.Object.paen_ccajas[1] =  dw_5.Object.paen_ccajas[1]
		
END IF
end subroutine

public function boolean existepallet (long al_pallet, boolean ab_mensaje);
IF iuo_spro_palletencab.Existe(Integer(istr_Mant.Argumento[1]), &
  						  	    	    Integer(istr_Mant.Argumento[2]), &
										 al_Pallet,ab_mensaje,SqlCa) THEN
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean modificafolio ();Integer li_exportador, li_planta
Long		ll_folioold, ll_folionew
Boolean	lb_AutoCommit, lb_Retorno=True

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	TRUE

li_exportador	=	dw_5.Object.expo_codigo[1]
li_planta		=	dw_5.Object.plde_codigo[1]
ll_folioold		=	dw_5.Object.paen_numero[1]
ll_folionew		=  90000000 + ll_folioold

DECLARE CambioFolio PROCEDURE FOR dba.FProc_Genera_Cambio_Folios
    @Exportador	=	:li_exportador,
	 @Planta 		=	:li_planta,
	 @PalletOld		=	:ll_folioold,
	 @PallerNew		=	:ll_folionew;
	 
EXECUTE CambioFolio;
	
CLOSE CambioFolio;

sqlca.AutoCommit	=	lb_AutoCommit

IF SqlCa.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Ejecución del Procedimiento Almacenado FProc_Genera_Cambio_Folios")
	lb_Retorno = FALSE
END IF	

RETURN lb_Retorno
end function

public function boolean controla_totalcajas (integer ai_traspa);Long	ll_Fila, ll_Total_CajasEnc, ll_Total_CajasDet, 	ll_Total_CajasTra

dw_3.AcceptText()
dw_4.AcceptText()
dw_6.AcceptText()
	
ll_Total_CajasEnc	=	dw_3.Object.tota_ccajas[1]

ll_Fila	=	dw_4.RowCount()

IF  ll_Fila > 0 THEN
	ll_Total_CajasDet	=	dw_4.Object.total_cajas[ll_Fila]
END IF

ll_Fila	=	dw_6.RowCount()

IF  ll_Fila > 0 THEN
	IF ai_traspa = 1 THEN
		ll_Total_CajasTra	=	dw_6.Object.pafr_ccajas[ll_Fila]
	ELSE
		ll_Total_CajasTra	=	dw_6.Object.total_traspa[ll_Fila]
	END IF	
END IF

IF (ll_Total_CajasDet + ll_Total_CajasTra) > ll_Total_CajasEnc THEN
	RETURN False
ELSE
	RETURN True
END IF
end function

public function long certifica ();Integer 	li_Retorno = -2
Long		ll_Fila, ll_Fila_det

For ll_Fila = 1 To dw_6.RowCount() 
	For ll_Fila_det = 1 To dw_4.RowCount()
		If dw_4.Object.pafr_eurepg[ll_Fila_det] <> dw_6.Object.pafr_eurepg[ll_Fila] Then
			Choose Case dw_6.Object.pafr_eurepg[ll_Fila]
				Case 1
					If dw_4.Object.pafr_eurepg[ll_Fila_det] = 2 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 3 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 4 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 5 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 0 Then 
						If MessageBox('Error', 'Advertencia: Esta mezclando productores con' + &
									' distinta certificación.', &
									INFORMATION!, OKCancel!, 2) = 2 Then 
							li_Retorno = -1
						Else
							Choose Case dw_4.Object.pafr_eurepg[ll_Fila_det]
								Case 0 
									li_retorno = 0
								Case 2
									li_retorno = 0
								Case 3
									li_retorno = 0
								Case 4
									li_retorno = 1
								Case 5
									li_retorno = 0
							End Choose
						
							Return li_Retorno
						End IF
					End IF
					
				Case 2
					If dw_4.Object.pafr_eurepg[ll_Fila_det] = 1 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 3 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 4 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 5 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 0 Then 
						If MessageBox('Error', 'Advertencia: Esta mezclando productores con' + &
									' distinta certificación.', &
									INFORMATION!, OKCancel!, 2) = 2 Then
							li_Retorno = -1
						Else
							Choose Case dw_4.Object.pafr_eurepg[ll_Fila_det]
								Case 0 
									li_retorno = 0
								Case 1
									li_retorno = 0
								Case 3
									li_retorno = 0
								Case 4
									li_retorno = 2
								Case 5
									li_retorno = 0
							End Choose
						
							Return li_Retorno
						End If
					End IF
					
				Case 3
					If dw_4.Object.pafr_eurepg[ll_Fila_det] = 1 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 2 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 4 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 5 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 0 Then 
						If MessageBox('Error', 'Advertencia: Esta mezclando productores con' + &
									' distinta certificación.', &
									INFORMATION!, OKCancel!, 2) = 2 Then
							li_Retorno = -1
						Else
							Choose Case dw_4.Object.pafr_eurepg[ll_Fila_det]
								Case 0 
									li_retorno = 0
								Case 1
									li_retorno = 0
								Case 2
									li_retorno = 0
								Case 4
									li_retorno = 0
								Case 5
									li_retorno = 3
							End Choose
						
							Return li_Retorno
						End If
					End IF
					
				Case 4
					If dw_4.Object.pafr_eurepg[ll_Fila_det] = 1 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 2 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 3 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 5 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 0 Then 
						If MessageBox('Error', 'Advertencia: Esta mezclando productores con' + &
									' distinta certificación.', &
									INFORMATION!, OKCancel!, 2) = 2 Then
							li_Retorno = -1
						Else
							Choose Case dw_4.Object.pafr_eurepg[ll_Fila_det]
								Case 0 
									li_retorno = 0
								Case 1
									li_retorno = 1
								Case 2
									li_retorno = 2
								Case 3
									li_retorno = 0
								Case 5
									li_retorno = 0
							End Choose
						
							Return li_Retorno
						End If
					End IF
					
				Case 5
					If dw_4.Object.pafr_eurepg[ll_Fila_det] = 1 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 2 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 3 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 4 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 0 Then 
						If MessageBox('Error', 'Advertencia: Esta mezclando productores con' + &
									' distinta certificación.', &
									INFORMATION!, OKCancel!, 2) = 2 Then
							li_Retorno = -1
						Else
							Choose Case dw_4.Object.pafr_eurepg[ll_Fila_det]
								Case 0 
									li_retorno = 0
								Case 1
									li_retorno = 1
								Case 2
									li_retorno = 0
								Case 3
									li_retorno = 3
								Case 4
									li_retorno = 0
							End Choose
						
							Return li_Retorno
						End If
					End IF
					
				Case 0
					If dw_4.Object.pafr_eurepg[ll_Fila_det] = 1 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 2 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 3 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 4 Or &
						dw_4.Object.pafr_eurepg[ll_Fila_det] = 5 Then 
						If MessageBox('Error', 'Advertencia: Esta mezclando productores con' + &
									' distinta certificación.', &
									INFORMATION!, OKCancel!, 2) = 2 Then
							li_Retorno = -1
						Else
							Choose Case dw_4.Object.pafr_eurepg[ll_Fila_det]
								Case 1
									li_retorno = 0
								Case 2
									li_retorno = 0
								Case 3
									li_retorno = 0
								Case 4
									li_retorno = 0
								Case 5
									li_retorno = 0
							End Choose
						
							Return li_Retorno
						End If
					End IF					
			End Choose				
		End If
	Next
Next

Return li_Retorno
end function

public subroutine agregatodos (boolean ab_todos);Long	ll_fila, ll_Nuevo


dw_4.SetRedraw(False)
dw_6.SetRedraw(False)

IF ab_Todos THEN
	
	ll_Fila = 1
	Do While ll_fila<= dw_6.RowCount()
	
		ll_Nuevo	=	dw_4.InsertRow(0)
		
		dw_4.Object.expo_codigo[ll_Nuevo]	=	dw_6.Object.expo_codigo[ll_Fila]
		dw_4.Object.plde_codigo[ll_Nuevo]	=	dw_6.Object.plde_codigo[ll_Fila]
		dw_4.Object.pafr_secuen[ll_Nuevo]	=	dw_6.Object.pafr_secuen[ll_Fila]
		dw_4.Object.frio_tipofr[ll_Nuevo]	=	dw_6.Object.frio_tipofr[ll_Fila]
		dw_4.Object.espe_codigo[ll_Nuevo]	=	dw_6.Object.espe_codigo[ll_Fila]
		dw_4.Object.vari_codigo[ll_Nuevo]	=	dw_6.Object.vari_codigo[ll_Fila]
		dw_4.Object.vari_nombre[ll_Nuevo]	=	dw_6.Object.vari_nombre[ll_Fila]
		dw_4.Object.vari_codrot[ll_Nuevo]	=	dw_6.Object.vari_codrot[ll_Fila]
		dw_4.Object.prod_codigo[ll_Nuevo]	=	dw_6.Object.prod_codigo[ll_Fila]
		dw_4.Object.prod_codrot[ll_Nuevo]	=	dw_6.Object.prod_codrot[ll_Fila]
		dw_4.Object.emba_codigo[ll_Nuevo]	=	dw_6.Object.emba_codigo[ll_Fila]
		dw_4.Object.cocc_codigo[ll_Nuevo]	=	dw_6.Object.cocc_codigo[ll_Fila]
		dw_4.Object.pefr_codigo[ll_Nuevo]	=	dw_6.Object.pefr_codigo[ll_Fila]
		dw_4.Object.plde_origen[ll_Nuevo]	=	dw_6.Object.plde_origen[ll_Fila]
		dw_4.Object.cate_codigo[ll_Nuevo]	=	dw_6.Object.cate_codigo[ll_Fila]
		dw_4.Object.etiq_codigo[ll_Nuevo]	=	dw_6.Object.etiq_codigo[ll_Fila]
		dw_4.Object.caen_calrot[ll_Nuevo]	=	dw_6.Object.caen_calrot[ll_Fila]
		dw_4.Object.pafr_calibr[ll_Nuevo]	=	dw_6.Object.pafr_calibr[ll_Fila]
		dw_4.Object.pafr_fecemb[ll_Nuevo]	=	dw_6.Object.pafr_fecemb[ll_Fila]
		dw_4.Object.pafr_estemb[ll_Nuevo]	=	dw_6.Object.pafr_estemb[ll_Fila]
		dw_4.Object.pafr_tipdoc[ll_Nuevo]	=	dw_6.Object.pafr_tipdoc[ll_Fila]
		dw_4.Object.pafr_docrel[ll_Nuevo]	=	dw_6.Object.pafr_docrel[ll_Fila]
		dw_4.Object.pafr_ccajas[ll_Nuevo]	=	dw_6.Object.pafr_ccajas[ll_Fila]
		dw_4.Object.prbr_codpre[ll_Nuevo]	=	dw_6.Object.prbr_codpre[ll_Fila]
		dw_4.Object.prcc_codigo[ll_Nuevo]	=	dw_6.Object.prcc_codigo[ll_Fila]
		dw_4.Object.lote_codigo[ll_Nuevo]	=	dw_6.Object.lote_codigo[ll_Fila]
		dw_4.Object.tpmv_codigo[ll_Nuevo]	=	dw_6.Object.tpmv_codigo[ll_Fila]
		dw_4.Object.mfee_numero[ll_Nuevo]	=	dw_6.Object.mfee_numero[ll_Fila]
		dw_4.Object.pafr_turno[ll_Nuevo]	   =	dw_6.Object.pafr_turno[ll_Fila]
		dw_4.Object.pafr_idcaja[ll_Nuevo]	=	dw_6.Object.pafr_idcaja[ll_Fila]
		dw_4.Object.cate_nombre[ll_nuevo]   =  dw_6.Object.cate_nombre[ll_fila]
		dw_4.Object.pefr_nombre[ll_Nuevo]	=	dw_6.Object.pefr_nombre[ll_Fila]
		dw_4.Object.prod_nombre[ll_Nuevo]	=	dw_6.Object.prod_nombre[ll_Fila]
		dw_4.Object.pafr_eurepg[ll_Nuevo]	=	dw_6.Object.pafr_eurepg[ll_Fila]
		dw_4.Object.comu_codigo[ll_Nuevo]	=	dw_6.Object.comu_codigo[ll_Fila]
		dw_4.Object.stic_codigo[ll_Nuevo]	=	dw_6.Object.stic_codigo[ll_Fila]		
		dw_4.Object.pafr_feccos[ll_Nuevo]	=	dw_6.Object.pafr_feccos[ll_Fila]
		dw_4.Object.frio_tiprot[ll_Nuevo]	=	dw_6.Object.frio_tiprot[ll_Fila]
		
		ll_Nuevo	=	dw_1.InsertRow(0)

		dw_1.Object.plde_codigo[ll_Nuevo]	=	dw_6.Object.plde_codigo[ll_Fila]
		dw_1.Object.expo_codigo[ll_Nuevo]	=	dw_6.Object.expo_codigo[ll_Fila]
		dw_1.Object.paen_numero[ll_Nuevo]	=	dw_3.Object.paen_numero[1]
		dw_1.Object.pafr_secuen[ll_Nuevo]	=	dw_6.Object.pafr_secuen[ll_Fila]
		dw_1.Object.pafr_fecemb[ll_Nuevo]	=	dw_6.Object.pafr_fecemb[ll_Fila]
		dw_1.Object.espe_codigo[ll_Nuevo]	=	dw_6.Object.espe_codigo[ll_Fila]
		dw_1.Object.vari_codigo[ll_Nuevo]	=	dw_6.Object.vari_codigo[ll_Fila]
		dw_1.Object.vari_codrot[ll_Nuevo]	=	dw_6.Object.vari_codrot[ll_Fila] 
		dw_1.Object.prod_codigo[ll_Nuevo]	=	dw_6.Object.prod_codigo[ll_Fila]
		dw_1.Object.emba_codigo[ll_Nuevo]	=	dw_6.Object.emba_codigo[ll_Fila]
		dw_1.Object.etiq_codigo[ll_Nuevo]	=	dw_6.Object.etiq_codigo[ll_Fila]
		dw_1.Object.frio_tipofr[ll_Nuevo]	=	dw_6.Object.frio_tipofr[ll_Fila]
		dw_1.Object.pefr_codigo[ll_Nuevo]	=	dw_6.Object.pefr_codigo[ll_Fila]
		dw_1.Object.cate_codigo[ll_Nuevo]	=	dw_6.Object.cate_codigo[ll_Fila]
		dw_1.Object.pafr_calibr[ll_Nuevo]	=	dw_6.Object.pafr_calibr[ll_Fila]
		dw_1.Object.plde_origen[ll_Nuevo]	=	dw_6.Object.plde_origen[ll_Fila]
		dw_1.Object.pafr_tipdoc[ll_Nuevo]	=	dw_6.Object.pafr_tipdoc[ll_Fila]
		dw_1.Object.pafr_docrel[ll_Nuevo]	=	dw_6.Object.pafr_docrel[ll_Fila]
		dw_1.Object.paen_nroori[ll_Nuevo]	=	dw_5.Object.paen_numero[1]
		dw_1.Object.pafr_secori[ll_Nuevo]	=	dw_6.Object.pafr_secuen[ll_Fila]
		dw_1.Object.pafr_ccajas[ll_Nuevo]	=	dw_6.Object.pafr_ccajas[ll_Fila]	
		
	   dw_6.DeleteRow(ll_fila)
		
	Loop
	
	pb_Cancela.Visible = TRUE
	pb_Acepta.Visible = FALSE
	
ELSE
	
	ll_Fila = 1
	
	Do While ll_fila<= dw_4.RowCount()
	 IF dw_4.GetItemStatus(ll_fila,0,Primary!) = NewModified! THEN
		ll_Nuevo	=	dw_6.InsertRow(0)
		
		dw_6.Object.expo_codigo[ll_Nuevo]	=	dw_4.Object.expo_codigo[ll_Fila]
		dw_6.Object.plde_codigo[ll_Nuevo]	=	dw_4.Object.plde_codigo[ll_Fila]
		dw_6.Object.pafr_secuen[ll_Nuevo]	=	dw_4.Object.pafr_secuen[ll_Fila]
		dw_6.Object.frio_tipofr[ll_Nuevo]	=	dw_4.Object.frio_tipofr[ll_Fila]
		dw_6.Object.espe_codigo[ll_Nuevo]	=	dw_4.Object.espe_codigo[ll_Fila]
		dw_6.Object.vari_codigo[ll_Nuevo]	=	dw_4.Object.vari_codigo[ll_Fila]
		dw_6.Object.vari_nombre[ll_Nuevo]	=	dw_4.Object.vari_nombre[ll_Fila]
		dw_6.Object.vari_codrot[ll_Nuevo]	=	dw_4.Object.vari_codrot[ll_Fila]
		dw_6.Object.prod_codigo[ll_Nuevo]	=	dw_4.Object.prod_codigo[ll_Fila]
		dw_6.Object.prod_codrot[ll_Nuevo]	=	dw_4.Object.prod_codrot[ll_Fila]
		dw_6.Object.emba_codigo[ll_Nuevo]	=	dw_4.Object.emba_codigo[ll_Fila]
		dw_6.Object.cocc_codigo[ll_Nuevo]	=	dw_4.Object.cocc_codigo[ll_Fila]
		dw_6.Object.pefr_codigo[ll_Nuevo]	=	dw_4.Object.pefr_codigo[ll_Fila]
		dw_6.Object.plde_origen[ll_Nuevo]	=	dw_4.Object.plde_origen[ll_Fila]
		dw_6.Object.cate_codigo[ll_Nuevo]	=	dw_4.Object.cate_codigo[ll_Fila]
		dw_6.Object.etiq_codigo[ll_Nuevo]	=	dw_4.Object.etiq_codigo[ll_Fila]
		dw_6.Object.pafr_calibr[ll_Nuevo]	=	dw_4.Object.pafr_calibr[ll_Fila]
		dw_6.Object.pafr_fecemb[ll_Nuevo]	=	dw_4.Object.pafr_fecemb[ll_Fila]
		dw_6.Object.pafr_estemb[ll_Nuevo]	=	dw_4.Object.pafr_estemb[ll_Fila]
		dw_6.Object.pafr_tipdoc[ll_Nuevo]	=	dw_4.Object.pafr_tipdoc[ll_Fila]
		dw_6.Object.pafr_docrel[ll_Nuevo]	=	dw_4.Object.pafr_docrel[ll_Fila]
		dw_6.Object.pafr_ccajas[ll_Nuevo]	=	dw_4.Object.pafr_ccajas[ll_Fila]
		dw_6.Object.prbr_codpre[ll_Nuevo]	=	dw_4.Object.prbr_codpre[ll_Fila]
		dw_6.Object.prcc_codigo[ll_Nuevo]	=	dw_4.Object.prcc_codigo[ll_Fila]
		dw_6.Object.lote_codigo[ll_Nuevo]	=	dw_4.Object.lote_codigo[ll_Fila]
		dw_6.Object.tpmv_codigo[ll_Nuevo]	=	dw_4.Object.tpmv_codigo[ll_Fila]
		dw_6.Object.mfee_numero[ll_Nuevo]	=	dw_4.Object.mfee_numero[ll_Fila]
		dw_6.Object.pafr_turno[ll_Nuevo]	   =	dw_4.Object.pafr_turno[ll_Fila]
		dw_6.Object.pafr_idcaja[ll_Nuevo]	=	dw_4.Object.pafr_idcaja[ll_Fila]
		dw_6.Object.cate_nombre[ll_nuevo]   =  dw_4.Object.cate_nombre[ll_fila]
		dw_6.Object.pefr_nombre[ll_Nuevo]	=	dw_4.Object.pefr_nombre[ll_Fila]
		dw_6.Object.prod_nombre[ll_Nuevo]	=	dw_4.Object.prod_nombre[ll_Fila]
		dw_6.Object.pafr_eurepg[ll_Nuevo]	=	dw_4.Object.pafr_eurepg[ll_Fila]
		dw_6.Object.comu_codigo[ll_Nuevo]	=	dw_4.Object.comu_codigo[ll_Fila]
		dw_6.Object.stic_codigo[ll_Nuevo]	=	dw_4.Object.stic_codigo[ll_Fila]
		dw_6.Object.pafr_feccos[ll_Nuevo]	=	dw_4.Object.pafr_feccos[ll_Fila]
		dw_6.Object.frio_tiprot[ll_Nuevo]	=	dw_4.Object.frio_tiprot[ll_Fila]

	   dw_4.DeleteRow(ll_fila)
		dw_1.DeleteRow(ll_fila)
    ELSE
		  ll_fila ++ 
	 END IF	
	Loop
	
	pb_Cancela.Visible = FALSE
	pb_Acepta.Visible = TRUE
	
END IF	
	
dw_4.SetRedraw(TRUE)
dw_6.SetRedraw(TRUE)
	
end subroutine

public subroutine asigna_inspec_palletnuevo ();Integer	li_tipoin, li_destino
Long		ll_numero, ll_pallet

IF NOT ib_ExistePalletDestino THEN
	
		ll_Pallet = dw_5.Object.paen_numero[1]
		
		Select Max(inpe_tipoin), Max(inpe_numero), Max(dest_codigo)
			Into	:li_tipoin, :ll_numero, :li_destino
			From	dba.spro_inspecpaldet
			Where	paen_numero	=	:ll_Pallet;
			
		dw_3.Object.plde_codigo[1]	=	dw_5.Object.plde_codigo[1]
		dw_3.Object.espe_codigo[1]	=	dw_5.Object.espe_codigo[1]
		dw_3.Object.vari_codigo[1]	=	dw_5.Object.vari_codigo[1]
		dw_3.Object.cate_codigo[1]	=	dw_5.Object.cate_codigo[1]
		dw_3.Object.cama_codigo[1]	=	dw_5.Object.cama_codigo[1]
		dw_3.Object.enva_tipoen[1]	=	dw_5.Object.enva_tipoen[1]
		dw_3.Object.enva_codigo[1]	=	dw_5.Object.enva_codigo[1]
		dw_3.Object.tpen_codigo[1] =  dw_5.Object.tpen_codigo[1]
		dw_3.Object.frio_tipofr[1]	=	dw_5.Object.frio_tipofr[1]
		dw_3.Object.etiq_codigo[1]	=	dw_5.Object.etiq_codigo[1]
		dw_3.Object.paen_estado[1]	=	1
		dw_3.Object.paen_feccon[1]	=	dw_2.Object.repa_fecrep[1]
		dw_3.Object.paen_altura[1] =  dw_5.Object.paen_altura[1]
		dw_3.Object.paen_concal[1] =  dw_5.Object.paen_concal[1]
		dw_3.Object.paen_fumiga[1]	=	dw_5.Object.paen_fumiga[1]
		dw_3.Object.paen_nroban[1] =  dw_5.Object.paen_nroban[1]
		dw_3.Object.paen_nropos[1] =  dw_5.Object.paen_nropos[1]
		dw_3.Object.paen_nropis[1] =  dw_5.Object.paen_nropis[1]
		dw_3.Object.paen_temsal[1] =  dw_5.Object.paen_temsal[1]	
		dw_3.Object.paen_temlle[1] =  dw_5.Object.paen_temlle[1]
		dw_3.Object.paen_status[1] =  dw_5.Object.paen_status[1]
		dw_3.Object.paen_pmixto[1] =  dw_5.Object.paen_pmixto[1]
		dw_3.Object.sepl_codigo[1] =  dw_5.Object.sepl_codigo[1]
		dw_3.Object.paen_mesfac[1] =  dw_5.Object.paen_mesfac[1]
		dw_3.Object.paen_calexp[1] =  dw_5.Object.paen_calexp[1]
		dw_3.Object.paen_kilexp[1] =  dw_5.Object.paen_kilexp[1]			
		dw_3.Object.paen_inspec[1] =  dw_5.Object.paen_inspec[1]
		dw_3.Object.tota_ccajas[1] =  dw_5.Object.tota_ccajas[1]		
END IF
end subroutine

public function boolean atributos_pallet ();Boolean		lb_estado = False

dw_3.AcceptText()

IF ib_ExistePalletDestino THEN
	
	IF ib_inspeccion_Origen <>  ib_inspeccion_Destino AND ib_inspeccion_Destino THEN
		RETURN FALSE
	END IF
	
	lb_estado = True
ELSE
	lb_estado = True
END IF

RETURN lb_estado
end function

public subroutine captura_total_cajas ();

IF dw_4.RowCount() > 0 THEN
	
	dw_3.Object.paen_ccajas[1] = dw_4.Object.total_cajas[1]
ELSE
	dw_3.Object.paen_ccajas[1] = 0
END IF

IF dw_6.RowCount() > 0 THEN
	
	dw_5.Object.paen_ccajas[1] = dw_6.Object.total_cajas[1]
ELSE
	dw_5.Object.paen_ccajas[1] = 0
END IF
end subroutine

public subroutine traspasa_cajas (integer ai_certifica);Long	ll_Fila, ll_BFilaRep, ll_BFilaPD, ll_Nuevo, ll_NuevoDet
Integer li_Secuen

dw_1.AcceptText()
dw_4.AcceptText()
dw_6.AcceptText()

FOR ll_Fila = 1 TO dw_6.RowCount()
	ll_BFilaRep	=	dw_1.Find("paen_nroori = " + String(dw_6.Object.paen_numero[ll_Fila]) + &
									 " AND pafr_secori = " + String(dw_6.Object.pafr_secuen[ll_Fila]) + &
									 " AND paen_numero = " + String(dw_3.Object.paen_numero[1]), + &
								   1, dw_1.RowCount())

	IF ll_BFilaRep > 0 THEN
		
		ll_BFilaPD	=	dw_4.Find("paen_numero = " + String(dw_3.Object.paen_numero[1]) + &
										 " AND pafr_secuen = " + String(dw_1.Object.pafr_secuen[ll_BFilaRep]), &
										 1, dw_4.RowCount())
    
		IF dw_6.Object.caja_traspa[ll_Fila]	> 0 THEN
			IF ll_BFilaPD > 0 THEN
				dw_1.Object.pafr_ccajas[ll_BFilaRep]	=	dw_1.Object.pafr_ccajas[ll_BFilaRep] + dw_6.Object.caja_traspa[ll_Fila]
				dw_4.Object.pafr_ccajas[ll_BFilaPD]		=	dw_4.Object.pafr_ccajas[ll_BFilaPD] + dw_6.Object.caja_traspa[ll_Fila]
				dw_6.Object.pafr_ccajas[ll_Fila]			=	dw_6.Object.pafr_ccajas[ll_Fila] - dw_6.Object.caja_traspa[ll_Fila]
				dw_6.Object.caja_traspa[ll_Fila]    	=  0	
			END IF
		END IF
	ELSE
		IF dw_6.Object.caja_traspa[ll_Fila]	> 0 THEN
			IF dw_4.RowCount() > 0 THEN
				li_secuen = dw_4.Object.pafr_secuen[dw_4.RowCount()] + 1
			ELSE
				li_secuen = 1
			END IF	
		   ll_Nuevo	=	dw_4.InsertRow(0)
	
			dw_4.Object.expo_codigo[ll_Nuevo]	=	dw_6.Object.expo_codigo[ll_Fila]
			dw_4.Object.plde_codigo[ll_Nuevo]	=	dw_6.Object.plde_codigo[ll_Fila]
			dw_4.Object.paen_numero[ll_Nuevo]	=	dw_3.Object.paen_numero[1]
			dw_4.Object.pafr_secuen[ll_Nuevo]	=	li_secuen   //ll_Nuevo
			dw_4.Object.frio_tipofr[ll_Nuevo]	=	dw_6.Object.frio_tipofr[ll_Fila]
			dw_4.Object.espe_codigo[ll_Nuevo]	=	dw_6.Object.espe_codigo[ll_Fila]
			dw_4.Object.vari_codigo[ll_Nuevo]	=	dw_6.Object.vari_codigo[ll_Fila]
			dw_4.Object.vari_nombre[ll_Nuevo]	=	dw_6.Object.vari_nombre[ll_Fila]
			dw_4.Object.vari_codrot[ll_Nuevo]	=	dw_6.Object.vari_codrot[ll_Fila]
			dw_4.Object.prod_codigo[ll_Nuevo]	=	dw_6.Object.prod_codigo[ll_Fila]
			dw_4.Object.prod_codrot[ll_Nuevo]	=	dw_6.Object.prod_codrot[ll_Fila]
			dw_4.Object.emba_codigo[ll_Nuevo]	=	dw_6.Object.emba_codigo[ll_Fila]
			dw_4.Object.cocc_codigo[ll_Nuevo]	=	dw_6.Object.cocc_codigo[ll_Fila]
			dw_4.Object.pefr_codigo[ll_Nuevo]	=	dw_6.Object.pefr_codigo[ll_Fila]
			dw_4.Object.plde_origen[ll_Nuevo]	=	dw_6.Object.plde_origen[ll_Fila]
			dw_4.Object.cate_codigo[ll_Nuevo]	=	dw_6.Object.cate_codigo[ll_Fila]
			dw_4.Object.etiq_codigo[ll_Nuevo]	=	dw_6.Object.etiq_codigo[ll_Fila]
			dw_4.Object.prbr_codpre[ll_Nuevo]	=	dw_6.Object.prbr_codpre[ll_Fila]
			dw_4.Object.prcc_codigo[ll_Nuevo]	=	dw_6.Object.prcc_codigo[ll_Fila]
			dw_4.Object.lote_codigo[ll_Nuevo]	=	dw_6.Object.lote_codigo[ll_Fila]
			dw_4.Object.caen_calrot[ll_Nuevo]	=	dw_6.Object.caen_calrot[ll_Fila]
			dw_4.Object.pafr_calibr[ll_Nuevo]	=	dw_6.Object.pafr_calibr[ll_Fila]
			dw_4.Object.pafr_fecemb[ll_Nuevo]	=	dw_6.Object.pafr_fecemb[ll_Fila]
			dw_4.Object.pafr_estemb[ll_Nuevo]	=	dw_6.Object.pafr_estemb[ll_Fila]
			dw_4.Object.pafr_tipdoc[ll_Nuevo]	=	dw_6.Object.pafr_tipdoc[ll_Fila]
			dw_4.Object.pafr_docrel[ll_Nuevo]	=	dw_6.Object.pafr_docrel[ll_Fila]
			dw_4.Object.pafr_ccajas[ll_Nuevo]	=	dw_6.Object.caja_traspa[ll_Fila]
			dw_4.Object.tpmv_codigo[ll_Nuevo]	=	dw_6.Object.tpmv_codigo[ll_Fila]
			dw_4.Object.mfee_numero[ll_Nuevo]	=	dw_6.Object.mfee_numero[ll_Fila]
			dw_4.Object.pafr_turno[ll_Nuevo]	   =	dw_6.Object.pafr_turno[ll_Fila]
			dw_4.Object.pafr_idcaja[ll_Nuevo]	=	dw_6.Object.pafr_idcaja[ll_Fila]
			dw_4.Object.cate_nombre[ll_nuevo]   =  dw_6.Object.cate_nombre[ll_fila]
			dw_4.Object.pefr_nombre[ll_Nuevo]	=	dw_6.Object.pefr_nombre[ll_Fila]
			dw_4.Object.prod_nombre[ll_Nuevo]	=	dw_6.Object.prod_nombre[ll_Fila]
			dw_4.Object.pafr_eurepg[ll_Nuevo]	=	ai_Certifica
			dw_4.Object.comu_codigo[ll_Nuevo]	=	dw_6.Object.comu_codigo[ll_Fila]
			dw_4.Object.stic_codigo[ll_Nuevo]	=	dw_6.Object.stic_codigo[ll_Fila]
			dw_4.Object.pafr_feccos[ll_Nuevo]	=	dw_6.Object.pafr_feccos[ll_Fila]
			dw_4.Object.frio_tiprot[ll_Nuevo]	=	dw_6.Object.frio_tiprot[ll_Fila]
		
			dw_4.accepttext()
			
//Inserta los Pallet			
			ll_NuevoDet	=	dw_1.InsertRow(0)

			dw_1.Object.plde_codigo[ll_NuevoDet]	=	dw_4.Object.plde_codigo[ll_Nuevo]
			dw_1.Object.expo_codigo[ll_NuevoDet]	=	dw_4.Object.expo_codigo[ll_Nuevo]
			dw_1.Object.paen_numero[ll_NuevoDet]	=	dw_4.Object.paen_numero[ll_Nuevo]
			dw_1.Object.pafr_secuen[ll_NuevoDet]	=	dw_4.Object.pafr_secuen[ll_Nuevo]
			dw_1.Object.pafr_fecemb[ll_NuevoDet]	=	dw_4.Object.pafr_fecemb[ll_Nuevo]
			dw_1.Object.espe_codigo[ll_NuevoDet]	=	dw_4.Object.espe_codigo[ll_Nuevo]
			dw_1.Object.vari_codigo[ll_NuevoDet]	=	dw_4.Object.vari_codigo[ll_Nuevo]
			dw_1.Object.vari_codrot[ll_NuevoDet]	=	dw_4.Object.vari_codrot[ll_Nuevo]
			dw_1.Object.prod_codigo[ll_NuevoDet]	=	dw_4.Object.prod_codigo[ll_Nuevo]
			dw_1.Object.emba_codigo[ll_NuevoDet]	=	dw_4.Object.emba_codigo[ll_Nuevo]
			dw_1.Object.etiq_codigo[ll_NuevoDet]	=	dw_4.Object.etiq_codigo[ll_Nuevo]
			dw_1.Object.frio_tipofr[ll_NuevoDet]	=	dw_4.Object.frio_tipofr[ll_Nuevo]
			dw_1.Object.pefr_codigo[ll_NuevoDet]	=	dw_4.Object.pefr_codigo[ll_Nuevo]
			dw_1.Object.cate_codigo[ll_NuevoDet]	=	dw_4.Object.cate_codigo[ll_Nuevo]
			dw_1.Object.pafr_calibr[ll_NuevoDet]	=	dw_4.Object.pafr_calibr[ll_Nuevo]
			dw_1.Object.plde_origen[ll_NuevoDet]	=	dw_4.Object.plde_origen[ll_Nuevo]
			dw_1.Object.pafr_tipdoc[ll_NuevoDet]	=	dw_4.Object.pafr_tipdoc[ll_Nuevo]
			dw_1.Object.pafr_docrel[ll_NuevoDet]	=	dw_4.Object.pafr_docrel[ll_Nuevo]
			dw_1.Object.pafr_ccajas[ll_NuevoDet]	=	dw_6.Object.caja_traspa[ll_Fila]
			dw_1.Object.paen_nroori[ll_NuevoDet]	=	dw_6.Object.paen_numero[ll_Fila]
			dw_1.Object.pafr_secori[ll_NuevoDet]	=	dw_6.Object.pafr_secuen[ll_Fila]
			dw_1.Object.repd_totcao[ll_NuevoDet]	=	dw_6.Object.total_cajas[1]
			dw_1.Object.repd_marcad[ll_NuevoDet]	=	is_marca
			dw_1.Object.repd_marcao[ll_NuevoDet]	=	is_marcao
			
			dw_6.Object.pafr_ccajas[ll_Fila]		=	dw_6.Object.pafr_ccajas[ll_Fila] - dw_6.Object.caja_traspa[ll_Fila]
			dw_6.Object.caja_traspa[ll_Fila]    =  0
			
			If dw_6.Object.pafr_ccajas[ll_Fila] = 0 OR IsNull(dw_6.Object.pafr_ccajas[ll_fila]) THEN
				dw_1.Object.repd_marcao[ll_NuevoDet]	=	'E'
			End If
		END IF
	END IF
NEXT

ll_fila = 1

DO WHILE ll_fila <= dw_6.RowCount()
	
	IF dw_6.Object.pafr_ccajas[ll_fila]=0 OR IsNull(dw_6.Object.pafr_ccajas[ll_fila]) THEN
		dw_6.DeleteRow(ll_fila)
	ELSE
		ll_fila++
	END IF
LOOP

For ll_Fila = 1 To dw_4.RowCount()
	dw_4.Object.pafr_eurepg[ll_Fila]	=	ai_Certifica
Next 
end subroutine

public function boolean integridad ();Boolean	lb_Retorno = True
Integer	li_especie, li_etiqueta
String	ls_embalaje
Long		ll_Busca, ll_Fila, ll_fila_e

dw_5.AcceptText()
dw_6.AcceptText()

li_especie	=	dw_5.Object.espe_codigo[1]

If li_especie <> dw_3.Object.espe_codigo[1] Then
	MessageBox('Error', 'Esta intentando juntar dos especies distintas', STOPSIGN!)
	lb_retorno	= False
End If

For ll_Fila = 1 To dw_6.RowCount()	
	If dw_6.Object.caja_traspa[ll_fila]	> 0 Then
		ls_Embalaje	=	dw_6.Object.emba_codigo[ll_Fila]
		li_etiqueta	=	dw_6.Object.etiq_codigo[ll_Fila]
		
		If dw_4.RowCount() > 0 Then
		
			If dw_4.Find("emba_codigo = '" + ls_embalaje + "'", 1, dw_4.RowCount()) = 0 Then
				For ll_fila_e = 1 To dw_4.RowCount()										
					If Not f_grupo_embalaje(li_especie, ls_embalaje, dw_4.Object.emba_codigo[ll_fila_e]) Then
						lb_Retorno = False
					End If
				Next 
			End If
			/*
			Requerimiento de Cabilfrut 10/10/2005 No bloquear distintas etiquetas
			
			If dw_4.Find('etiq_codigo = ' + String(li_etiqueta), 1, dw_4.RowCount()) = 0 Then
				MessageBox('Error', 'La Etiqueta que se esta ingresando no esta contenido en el pallet Destino.')
				lb_Retorno = False
			End If		
			*/
		End If
	End If
Next 


Return lb_retorno
end function

on w_maed_spro_repalasigenca.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.pb_nvopdest=create pb_nvopdest
this.pb_nvoporg=create pb_nvoporg
this.dw_5=create dw_5
this.cb_todos=create cb_todos
this.pb_acepta=create pb_acepta
this.dw_6=create dw_6
this.dw_4=create dw_4
this.pb_cancela=create pb_cancela
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.pb_nvopdest
this.Control[iCurrent+3]=this.pb_nvoporg
this.Control[iCurrent+4]=this.dw_5
this.Control[iCurrent+5]=this.cb_todos
this.Control[iCurrent+6]=this.pb_acepta
this.Control[iCurrent+7]=this.dw_6
this.Control[iCurrent+8]=this.dw_4
this.Control[iCurrent+9]=this.pb_cancela
end on

on w_maed_spro_repalasigenca.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.pb_nvopdest)
destroy(this.pb_nvoporg)
destroy(this.dw_5)
destroy(this.cb_todos)
destroy(this.pb_acepta)
destroy(this.dw_6)
destroy(this.dw_4)
destroy(this.pb_cancela)
end on

event open;

/*
Argumentos :	[1]	=>	Código de Exportador
					[2]	=>	Código de Planta
					[3]	=>	Número de Repalletizaje
					[4]	=>	Número de Pallet Destino
					[5]	=>	Número de Pallet Origen
*/

x	= 0
y	= 0

iuo_tipopallet						=	Create uo_tipopallenvase
iuo_spro_palletencab				=	Create uo_spro_palletencab
iuo_exportador						=	Create uo_exportadores
iuo_productor						=  Create uo_productores

This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)	// Detalle de Repaletizado
dw_2.SetTransObject(sqlca) // Encabezado de Repaletizado
dw_3.SetTransObject(sqlca)	// Encabezado de Pallet Destino
dw_4.SetTransObject(sqlca)	// Detalle de Pallet Destino
dw_5.SetTransObject(sqlca) // Encabezado de Pallet Origen
dw_6.SetTransObject(sqlca) // Detalle de Pallet Origen

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_6.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

dw_2.GetChild("plde_codigo", idwc_planta)

dw_3.GetChild("espe_codigo", idwc_espdest)
dw_3.GetChild("vari_codigo", idwc_vardest)
dw_3.GetChild("cate_codigo", idwc_catdest)
dw_3.GetChild("tpen_codigo", idwc_tipopaldest)

dw_4.GetChild("prod_codigo", idwc_proddest)
dw_4.GetChild("emba_codigo", idwc_embdest)
dw_4.GetChild("pafr_calibr", idwc_caldest)

dw_5.GetChild("espe_codigo", idwc_esporig)
dw_5.GetChild("vari_codigo", idwc_varorig)
dw_5.GetChild("cate_codigo", idwc_catorig)
dw_5.GetChild("tpen_codigo", idwc_tipopalorig)

dw_6.GetChild("prod_codigo", idwc_prodorig)
dw_6.GetChild("emba_codigo", idwc_emborig)
dw_6.GetChild("pafr_calibr", idwc_calorig)

idwc_planta.SetTransObject(SqlCa)
idwc_espdest.SetTransObject(SqlCa)
idwc_vardest.SetTransObject(SqlCa)
idwc_catdest.SetTransObject(SqlCa)
idwc_tipopaldest.SetTransObject(SqlCa)
idwc_proddest.SetTransObject(SqlCa)
idwc_embdest.SetTransObject(SqlCa)
idwc_caldest.SetTransObject(SqlCa)
idwc_esporig.SetTransObject(SqlCa)
idwc_varorig.SetTransObject(SqlCa)
idwc_catorig.SetTransObject(SqlCa)
idwc_tipopalorig.SetTransObject(SqlCa)
idwc_prodorig.SetTransObject(SqlCa)
idwc_emborig.SetTransObject(SqlCa)
idwc_calorig.SetTransObject(SqlCa)

idwc_planta.Retrieve()

IF idwc_espdest.Retrieve() = 0 THEN
	idwc_espdest.InsertRow(0)
END IF

IF idwc_vardest.Retrieve(0) = 0 THEN
	idwc_vardest.InsertRow(0)
END IF

IF idwc_catdest.Retrieve() = 0 THEN
	idwc_catdest.InsertRow(0)
END IF

idwc_tipopaldest.InsertRow(0)

IF idwc_esporig.Retrieve() = 0 THEN
	idwc_esporig.InsertRow(0)
END IF

IF idwc_varorig.Retrieve(0) = 0 THEN
	idwc_vardest.InsertRow(0)
END IF

IF idwc_catorig.Retrieve() = 0 THEN
	idwc_catorig.InsertRow(0)
END IF

idwc_tipopalorig.InsertRow(0)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

istr_Mant.Argumento[1]	=	String(gi_CodExport)							// Exportador
istr_Mant.Argumento[2]	=	String(gstr_ParamPlanta.CodigoPlanta)	// Planta

buscar	= "Productor:Nprod_codigo,Variedad:Nvari_codigo,Embalaje:Semba_codigo"
ordenar	= "Productor:prod_codigo,Variedad:vari_codigo,Embalaje:emba_codigo"

dw_3.Object.paen_numero.Protect	=	1
dw_3.Object.paen_numero.BackGround.Color = RGB(192,192,192)
dw_3.Object.paen_tipopa.Protect	=	1
dw_3.Object.paen_tipopa.BackGround.Color = RGB(192,192,192)
dw_3.Object.tpen_codigo.Protect	=	1
dw_3.Object.tpen_codigo.BackGround.Color = RGB(192,192,192)

dw_5.Object.paen_numero.Protect	=	1
dw_5.Object.paen_numero.BackGround.Color = RGB(192,192,192)
dw_5.Object.paen_tipopa.Protect	=	1
dw_5.Object.paen_tipopa.BackGround.Color = RGB(192,192,192)
dw_5.Object.tpen_codigo.Protect	=	1
dw_5.Object.tpen_codigo.BackGround.Color = RGB(192,192,192)

dw_5.Modify("paen_numero_t.Text = 'Nro.Pallet Origen'")


end event

event ue_recuperadatos();Long	ll_fila_d, ll_fila_e, respuesta

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	dw_3.SetRedraw(False)
	dw_3.Reset()

	IF dw_2.Retrieve(Integer(istr_Mant.Argumento[2]), &
						  Integer(istr_Mant.Argumento[3]), &
						  Long(istr_Mant.Argumento[4])) = -1 OR &
		dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), &
						  Integer(istr_Mant.Argumento[2]), &
						  Long(istr_Mant.Argumento[7])) =  -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE

		DO
			IF dw_1.Retrieve(Integer(istr_Mant.Argumento[2]), &
								  Integer(istr_Mant.Argumento[3]), &
								  Long(istr_Mant.Argumento[4])) = -1 OR &
				dw_4.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Long(istr_Mant.Argumento[2]), &
								  Integer(istr_Mant.Argumento[7])) = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				dw_3.Object.prod_codrot[1]	=	dw_4.Object.prod_codrot[1]

				IF iuo_productor.Existe(dw_4.Object.prod_codrot[1],False,SqlCa) THEN
					dw_3.SetItem(1, "prod_nomrot", iuo_productor.Nombre)
				END IF

				pb_eliminar.Enabled	=	True
				pb_grabar.Enabled		=	True
				pb_imprimir.Enabled	=	True
				pb_ins_det.Enabled	=	True

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
	dw_3.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo();Long		ll_modif1, ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_4.GetNextModified(0, Primary!)
			ll_modif1	=	ll_modif1 + dw_2.GetNextModified(0, Primary!)
		
			IF ll_modif1 > 0 THEN
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
dw_2.Reset()
dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True
dw_3.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_3.SetRedraw(False)
dw_3.InsertRow(0)
dw_3.SetRedraw(True)

dw_5.SetRedraw(False)
dw_5.InsertRow(0)
dw_5.SetRedraw(True)

dw_2.SetFocus()

HabilitaEncab(True)

pb_cancela.visible 		= FALSE
ib_ExistePalletDestino 	= FALSE
ib_inspeccion_origen  	= FALSE
ib_inspeccion_destino 	= FALSE

dw_3.Object.paen_numero.Protect = 1
dw_5.Object.paen_numero.Protect = 1
dw_3.Object.paen_tipopa.Protect = 1
dw_5.Object.paen_tipopa.Protect = 1
dw_3.Object.tpen_codigo.Protect = 1
dw_5.Object.tpen_codigo.Protect = 1
dw_3.Object.paen_numero.BackGround.Color = RGB(192,192,192)
dw_5.Object.paen_numero.BackGround.Color = RGB(192,192,192)
dw_3.Object.paen_tipopa.BackGround.Color = RGB(192,192,192)
dw_5.Object.paen_tipopa.BackGround.Color = RGB(192,192,192)
dw_3.Object.tpen_codigo.BackGround.Color = RGB(192,192,192)
dw_5.Object.tpen_codigo.BackGround.Color = RGB(192,192,192)

pb_nvopdest.Enabled 	= FALSE
pb_nvoporg.Enabled	= TRUE

dw_2.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))
dw_2.SetItem(1, "repa_fecrep", DateTime(Today()))
dw_2.SetItem(1, "expo_codigo", Integer(istr_Mant.Argumento[1]))
dw_3.SetItem(1, "expo_codigo", Integer(istr_Mant.Argumento[1]))
dw_3.SetItem(1, "paen_tipopa", 2)
dw_5.SetItem(1, "expo_codigo", Integer(istr_Mant.Argumento[1]))

is_palletorig = ""
is_palletdest = ""

dw_2.SetColumn("repa_tipopa")
end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0, li_alto

dw_2.width				= this.workspacewidth() - 403
maximo	= dw_2.width

//IF this.workspacewidth()<= Maximo THEN RETURN

dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					= 37

dw_3.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_3.y					= 348
dw_3.width				= this.workspacewidth() - 403

li_alto 					= ROUND((This.WorkSpaceHeight() - dw_2.height - dw_3.height - dw_5.height)/2,0)

dw_4.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_4.y					= dw_2.Height + dw_3.Height
dw_4.height				= li_alto
dw_4.width				= this.workspacewidth() - 403

dw_5.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_5.y					= dw_2.Height + dw_3.Height + dw_4.height
dw_5.width				= this.workspacewidth() - 403

dw_6.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_6.y					= dw_2.Height + dw_3.Height + dw_4.height + dw_5.height
dw_6.height				= li_alto
dw_6.width				= this.workspacewidth() - 403


//gb_1.x 					= This.WorkSpaceWidth() - 310
//gb_1.y 					= 5
//gb_1.width				= 275

li_posic_x				= This.WorkSpaceWidth() - 250
//li_posic_y				= gb_1.y + 88

IF pb_nuevo.Visible THEN
	pb_nuevo.x				= li_posic_x
	pb_nuevo.y				= li_posic_y
	pb_nuevo.width			= 156
	pb_nuevo.height		= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				= li_posic_x
	pb_grabar.y				= li_posic_y
	pb_grabar.width		= 156
	pb_grabar.height		= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			= li_posic_x
	pb_imprimir.y			= li_posic_y
	pb_imprimir.width		= 156
	pb_imprimir.height	= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_salir.Visible THEN
	pb_salir.x				= li_posic_x
	pb_salir.y				= li_posic_y
	pb_salir.width			= 156
	pb_salir.height		= 133
	li_visible ++
	li_posic_y += 180
END IF

//gb_1.height				= 180 * li_visible + 97 /*  (Según Botones Visibles)  */

//gb_3.x 					= gb_1.x
//gb_3.y 					= 804
//gb_3.width				= 275

pb_nvopdest.x			= li_posic_x
//pb_nvopdest.y			= gb_3.y + 93
pb_nvopdest.width		= 156
pb_nvopdest.height	= 133


//gb_4.x 					= gb_3.x
//gb_4.y 					= dw_5.y
//gb_4.width				= 275

pb_acepta.x				= li_posic_x
//pb_acepta.y				= gb_4.y + 93
pb_acepta.width		= 156
pb_acepta.height		= 133

pb_cancela.x			= li_posic_x
//pb_cancela.y			= gb_4.y + 93
pb_cancela.width		= 156
pb_cancela.height		= 133

//gb_5.x 					= gb_4.x
//gb_5.y 					= dw_6.y
//gb_5.width				= 275

pb_nvoporg.x			= li_posic_x
//pb_nvoporg.y			= gb_5.y + 93
pb_nvoporg.width		= 156
pb_nvoporg.height		= 133

//cb_todos.x			   = gb_5.x
//cb_todos.y				= gb_5.y + 300
end event

event ue_borra_detalle();IF dw_4.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_4.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

 IF dw_4.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

event ue_antesguardar();call super::ue_antesguardar;Long		ll_Fila, ll_NumeroRepal, ll_PalletDestino, ll_totalcajas
Integer	li_Exportador, li_Planta, li_TipoMovto, li_Secuencia

ib_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

li_Exportador		=	dw_3.Object.expo_codigo[1]
li_Planta			=	dw_2.Object.plde_codigo[1]
ll_NumeroRepal		=	dw_2.Object.repa_numero[1]

IF isnull(dw_3.Object.tpen_codigo[1]) THEN
   messagebox("Falta de Datos","Falta Seleccionar un Tipo de Pallet en Pallet Destino.")
	Message.DoubleParm = -1
	sqlca.AutoCommit 	=	ib_AutoCommit	
	Return
END IF

IF isnull(dw_5.Object.tpen_codigo[1]) THEN
   messagebox("Falta de Datos","Falta Seleccionar un Tipo de Pallet en Pallet Origen.")
	Message.DoubleParm = -1
	sqlca.AutoCommit 	=	ib_AutoCommit	
	Return
END IF

IF dw_4.RowCount() > 0 THEN
	ll_totalcajas = dw_4.Object.total_cajas[1]
ELSE
	ll_totalcajas = 0
END IF

IF ll_totalcajas < dw_3.Object.tota_ccajas[1] THEN
	dw_3.SetItem(1,"paen_tipopa",2)
ELSEIF ll_totalcajas = dw_3.Object.tota_ccajas[1] THEN
	dw_3.SetItem(1,"paen_tipopa",1)
ELSE
	MessageBox("Atención","El total de cajas supera al tipo pallet seleccionado.")
	Message.DoubleParm = -1
	sqlca.AutoCommit 	=	ib_AutoCommit	
	RETURN
END IF	

IF dw_6.RowCount() > 0 THEN
	ll_totalcajas = dw_6.Object.total_cajas[1]
ELSE
	ll_totalcajas = 0
END IF

IF ll_totalcajas = dw_5.Object.tota_ccajas[1] THEN
	dw_5.SetItem(1,"paen_tipopa",1)
ELSEIF ll_totalcajas > dw_5.Object.tota_ccajas[1] THEN
	MessageBox("Atención","El total de cajas supera al tipo pallet seleccionado.")
	Message.DoubleParm = -1
	sqlca.AutoCommit 	=	ib_AutoCommit	
	RETURN
END IF

IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
	UPDATE	dba.spro_repalasigenca
		SET	repa_numero = 0
		WHERE	1 = 2;

	SELECT	IsNull(Max(repa_numero), 0) + 1
		INTO	:ll_NumeroRepal
		FROM	dba.spro_repalasigenca
		WHERE	plde_codigo	=	:li_Planta ;

	dw_2.Object.repa_numero[1]	=	ll_NumeroRepal
END IF

istr_Mant.Argumento[3]	=	String(dw_2.Object.repa_numero[1])
istr_Mant.Argumento[4]	=	String(dw_3.Object.paen_numero[1])

ll_PalletDestino			=	dw_3.Object.paen_numero[1]

captura_total_cajas()

FOR ll_Fila = 1 TO dw_4.RowCount()
	IF dw_4.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
		dw_4.Object.paen_numero[ll_Fila]	=	dw_3.Object.paen_numero[1]
	END IF	
NEXT

FOR ll_Fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
		dw_1.Object.repa_numero[ll_Fila]	=	dw_2.Object.repa_numero[1]
	END IF	
NEXT

IF dw_6.RowCount()<=0 THEN dw_5.Object.paen_estado[1] = 3

dw_2.Object.repa_numero.Protect	=	1
dw_2.Object.repa_numero.BackGround.Color = RGB(192,192,192)

sqlca.AutoCommit 	=	ib_AutoCommit	

end event

event ue_guardar();IF dw_4.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
	
	IF dw_2.Object.repa_tipopa[1] = 6 THEN
		dw_3.SetColumn("paen_numero")
		ib_ExistePalletDestino = False
		
		pb_nvoporg.TriggerEvent(Clicked!)
		pb_nvopdest.TriggerEvent(Clicked!)
		
		dw_3.Object.tpen_codigo.Protect	=	1
		dw_3.Object.tpen_codigo.BackGround.Color = RGB(192,192,192)
	
		dw_5.Object.paen_numero.Protect	=	1
		dw_5.Object.paen_numero.BackGround.Color = RGB(192,192,192)
		dw_5.Object.paen_tipopa.Protect	=	1
		dw_5.Object.paen_tipopa.BackGround.Color = RGB(192,192,192)
		dw_5.Object.tpen_codigo.Protect	=	1
		dw_5.Object.tpen_codigo.BackGround.Color = RGB(192,192,192)
		pb_nvoporg.Enabled	=	FALSE
		pb_nvopdest.Enabled	=	FALSE
		pb_acepta.Enabled		=	False
		pb_acepta.Visible    =  TRUE
		pb_cancela.Visible   =  FALSE
		
	ELSE	
		dw_5.SetColumn("paen_numero")
				
		pb_nvoporg.TriggerEvent(Clicked!)
		
		dw_3.Object.tpen_codigo.Protect	=	1
		dw_3.Object.tpen_codigo.BackGround.Color = RGB(192,192,192)
	
		dw_5.Object.paen_numero.Protect	=	0
		dw_5.Object.paen_numero.BackGround.Color = RGB(255,255,255)
		dw_5.Object.paen_tipopa.Protect	=	1
		dw_5.Object.paen_tipopa.BackGround.Color = RGB(192,192,192)
		dw_5.Object.tpen_codigo.Protect	=	1
		dw_5.Object.tpen_codigo.BackGround.Color = RGB(192,192,192)
	
		pb_nvoporg.Enabled	=	TRUE
		pb_nvopdest.Enabled	=	TRUE
		pb_acepta.Enabled		=	False
		pb_acepta.Visible    =  TRUE
		pb_cancela.Visible   =  FALSE
	END IF	
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_imprimir();SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "REPALETIZAJE"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_repalasgenca"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(dw_2.Object.plde_codigo[1], dw_2.Object.repa_fecrep[1], &
		dw_2.Object.repa_fecrep[1], dw_2.Object.repa_numero[1], dw_2.Object.repa_numero[1])

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
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_repalasigenca
string tag = "Detalle Repalletizaje"
boolean visible = false
integer x = 727
integer y = 1332
integer width = 558
integer height = 284
string title = "Detalle de Repaletizado"
string dataobject = "dw_mues_spro_repalasigdeta"
boolean livescroll = false
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_repalasigenca
string tag = "Encabezado Repalletizaje"
integer x = 32
integer y = 32
integer width = 3154
integer height = 348
integer taborder = 10
string dataobject = "dw_mant_spro_repalasigenca"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Fecha
Integer  li_exportador, li_existe

ls_Columna = dwo.name

CHOOSE CASE ls_Columna

	CASE "repa_fecrep"
		ls_Fecha	=	Data
		This.SetItem(1, ls_Columna, DateTime(Date(Mid(ls_Fecha,1,10))))

   CASE "repa_tipopa"
		IF Data="1" THEN
			cb_todos.Visible = TRUE
		ELSE
			cb_todos.Visible = FALSE
      END IF

   CASE "expo_codigo"
		li_exportador = this.Object.expo_codigo[row]
		IF Not iuo_exportador.Existe(Integer(data), True, Sqlca) THEN
			This.SetItem(row,"expo_codigo",li_exportador)
			RETURN 1			
		ELSE
			istr_mant.argumento[1]	= data
			dw_3.SetItem(1, "expo_codigo", Integer(istr_Mant.Argumento[1]))
			dw_5.SetItem(1, "expo_codigo", Integer(istr_Mant.Argumento[1]))
		END IF	
	
END CHOOSE

IF HabilitaPallet() THEN
	dw_3.Object.paen_numero.Protect	=	0
	dw_3.Object.paen_numero.BackGround.Color = RGB(255,255,255)
	dw_3.Object.paen_tipopa.Protect	=	0
	dw_3.Object.paen_tipopa.BackGround.Color = RGB(255,255,255)
END IF
end event

event dw_2::itemerror;call super::itemerror;RETURN 1
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_repalasigenca
integer x = 3305
integer y = 96
integer taborder = 60
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_repalasigenca
boolean visible = false
integer x = 3296
integer y = 1796
integer taborder = 100
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_repalasigenca
integer x = 3305
integer y = 256
integer taborder = 110
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_repalasigenca
integer x = 3305
integer y = 416
integer taborder = 120
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_repalasigenca
integer x = 3305
integer y = 576
integer taborder = 130
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_repalasigenca
boolean visible = false
integer x = 3305
integer taborder = 150
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_repalasigenca
boolean visible = false
integer taborder = 160
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_repalasigenca
boolean visible = false
integer y = 1788
integer taborder = 140
boolean enabled = false
end type

type dw_3 from uo_dw within w_maed_spro_repalasigenca
string tag = "Encabezado Pallet Destino"
integer x = 32
integer y = 340
integer width = 3154
integer height = 268
integer taborder = 20
boolean bringtotop = true
string title = "Pallet Destino"
string dataobject = "dw_mant_palletencab_repaletizaje"
boolean vscrollbar = false
end type

event itemchanged;String	ls_Columna, ls_Null, ls_Estado[3] = {'Existencia','Despachado','Repaletizado'}

SetNull(ls_Null)

ls_Columna = dwo.name

CHOOSE CASE ls_Columna
		
	CASE "paen_numero"
		
		ib_Detalle = TRUE
		is_palletdest = ""
		
		IF dw_5.Object.paen_numero[1] = Long(data) THEN
		   MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
							", fue ingresado como pallet origen. " + &
							"~r~rIngrese otro número de pallet.")
			This.SetItem(1, ls_Columna, Long(ls_Null))
			istr_Mant.Argumento[4]	=	""
			RETURN 1
		ELSE
			dw_2.Object.expo_codigo.Protect = 1
			dw_2.Object.expo_codigo.BackGround.Color = RGB(192,192,192)
			istr_Mant.Argumento[4]	=	Data
			ib_inspeccion_destino   =  FALSE
			IF ExistePallet(Long(istr_Mant.Argumento[4]),False) THEN
				
				ib_detalle = existedetallepallet(Long(istr_Mant.Argumento[4]))
				
				IF dw_2.Object.repa_tipopa[1] = 6 AND ib_detalle THEN
					
					MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
							", ya se encuentra ingresado. Para Cambio de Folio, el pallet destino debe ser nuevo. " + &
							"~r~rIngrese otro número de pallet.")
					This.SetItem(1, ls_Columna, Long(ls_Null))
					istr_Mant.Argumento[4]	=	""
					RETURN 1
				END IF	
				
				IF iuo_spro_palletencab.Estado > 1 AND ib_detalle THEN
					MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
									", " + ls_Estado[iuo_spro_palletencab.Estado] + &
									"~r~rIngrese otro número de pallet.")
					This.SetItem(1, ls_Columna, Long(ls_Null))
					istr_Mant.Argumento[4]	=	""
					RETURN 1
				END IF	
				
				IF iuo_spro_palletencab.Inspeccion >= 1 THEN
					ib_inspeccion_destino =  TRUE
					MessageBox("Atención", "Trabajará con un pallet Inspeccionado.")
				END If			
			
				If dw_2.Object.repa_nrosag[1] > 0 Then 
					If IsNull(iuo_spro_palletencab.Inspeccion) Then iuo_spro_palletencab.Inspeccion = 0
					If iuo_spro_palletencab.Inspeccion = 0 Then
						MessageBox("Atención", "El pallet debe estar Inspeccionado.")
						This.SetItem(Row, ls_Columna, Long(ls_Null))
						Return 1
					End If
				End If
				
				Parent.TriggerEvent("ue_recuperapallet_destino")				
				
				dw_3.Object.paen_estado[1]			=	1
				is_marca = 'M'
				
				dw_5.Object.paen_numero.Protect	=	0
				dw_5.Object.paen_numero.BackGround.Color = RGB(255,255,255)
				
				dw_5.Object.paen_tipopa.Protect	=	0
				dw_5.Object.paen_tipopa.BackGround.Color = RGB(255,255,255)
				
				is_palletdest = Mid(data,len(Data) - 6, 7)
				
				IF ib_detalle THEN
					ib_ExistePalletDestino	=	True
				ELSE
					ib_ExistePalletDestino	=	FALSe
				END IF	
				
				dw_5.SetColumn("paen_numero")
				dw_5.SetFocus()
				
			ELSE
				ib_ExistePalletDestino	=	False
				is_marca = 'A'
				dw_3.Reset()
				dw_3.SetRedraw(False)
				dw_3.InsertRow(0)
				dw_3.SetRedraw(True)
				dw_4.Reset()
				dw_3.SetItem(1, "expo_codigo", Integer(istr_Mant.Argumento[1]))
				dw_3.SetItem(1, "paen_tipopa", 2)
				This.SetItem(1, ls_Columna, Long(istr_Mant.Argumento[4]))
				dw_5.Object.paen_numero.Protect	=	0
				dw_5.Object.paen_numero.BackGround.Color = RGB(255,255,255)
				dw_5.Object.paen_tipopa.Protect	=	0
				dw_5.Object.paen_tipopa.BackGround.Color = RGB(255,255,255)
				pb_nvopdest.Enabled	=	True
				
				is_palletdest = Mid(data,len(Data) - 6, 7)
				
				dw_5.SetColumn("paen_numero")
				dw_5.SetFocus()
					
			END IF
			
			This.Object.paen_numero.Protect	=	1
    		This.Object.paen_numero.BackGround.Color = RGB(192,192,192)
				
		END IF
		
	CASE "tpen_codigo"
		IF iuo_tipopallet.Existe(dw_5.Object.enva_tipoen[1], &
										 dw_5.Object.enva_codigo[1], &
										 Data,True,SqlCa) THEN
         //This.SetItem(1, "paen_ccajas", iuo_tipopallet.CajasPallet)
			This.SetItem(1, "tota_ccajas", iuo_tipopallet.CajasPallet)
			This.SetItem(1, "paen_altura", iuo_tipopallet.AlturaPallet)
		ELSE
			This.SetItem(1, ls_Columna, ls_Null)
			RETURN 1
		END IF

END CHOOSE
end event

event itemerror;RETURN 1
end event

event losefocus;call super::losefocus;accepttext()

IF is_palletdest <> "" THEN
	is_palletdest	=	String(long(Mid( is_palletdest, 1, Len(is_palletdest) - 1)), "0000000") + &
					      Mid(is_palletdest, Len(is_palletdest))
	
	this.SetItem(il_fila, "paen_numero", long(is_palletdest))
END IF 	
end event

event itemfocuschanged;call super::itemfocuschanged;IF dwo.Name = "paen_numero" THEN
	This.Object.paen_numero.EditMask.Mask = "##########"
END IF

end event

type pb_nvopdest from picturebutton within w_maed_spro_repalasigenca
string tag = "Nuevo Pallet Destino"
integer x = 3301
integer y = 880
integer width = 155
integer height = 132
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo\Bmp\Reglae.bmp"
string disabledname = "\Desarrollo\Bmp\Reglad.bmp"
alignment htextalign = left!
end type

event clicked;dw_3.Reset()
//dw_1.Reset()
dw_3.SetRedraw(False)
dw_3.InsertRow(0)
dw_3.SetRedraw(True)

dw_4.Reset()

dw_3.SetItem(1, "expo_codigo", Integer(istr_Mant.Argumento[1]))
dw_3.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))
dw_3.SetItem(1, "paen_tipopa", 2)
dw_3.Object.paen_numero.Protect	=	0
dw_3.Object.paen_numero.BackGround.Color = RGB(255,255,255)

dw_5.Reset()
dw_5.SetRedraw(False)
dw_5.InsertRow(0)
dw_5.SetRedraw(True)
dw_6.Reset()
dw_5.SetItem(1, "expo_codigo", Integer(istr_Mant.Argumento[1]))
dw_5.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))
dw_5.SetItem(1, "paen_tipopa", 2)

is_palletdest = ""

dw_3.SetColumn("paen_numero")
dw_3.SetFocus()
end event

type pb_nvoporg from picturebutton within w_maed_spro_repalasigenca
string tag = "Nuevo Pallet Origen"
integer x = 3305
integer y = 1468
integer width = 155
integer height = 132
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo\Bmp\Reglae.bmp"
string disabledname = "\Desarrollo\Bmp\Reglad.bmp"
alignment htextalign = left!
end type

event clicked;dw_5.Reset()
dw_5.SetRedraw(False)
dw_5.InsertRow(0)
dw_5.SetRedraw(True)
dw_6.Reset()
dw_5.SetItem(1, "expo_codigo", Integer(istr_Mant.Argumento[1]))
dw_5.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))
dw_5.SetItem(1, "paen_tipopa", 2)
dw_5.Object.paen_numero.Protect	=	0
dw_5.Object.paen_numero.BackGround.Color = RGB(255,255,255)

is_palletorig = ""

dw_5.SetColumn("paen_numero")
dw_5.SetFocus()
end event

type dw_5 from uo_dw within w_maed_spro_repalasigenca
string tag = "Encabezado Pallet Origen"
integer x = 32
integer y = 1120
integer width = 3154
integer height = 268
integer taborder = 30
string title = "Pallet Origen"
string dataobject = "dw_mant_palletencab_repaletizaje"
boolean vscrollbar = false
end type

event itemchanged;String	ls_Columna, ls_Null, ls_Fecha, ls_Estado[3] = {'Existencia','Despachado','Repaletizado'}
Boolean  lb_detalle

SetNull(ls_Null)

ls_Columna = dwo.name

CHOOSE CASE ls_Columna
	CASE "paen_numero"
		
		is_palletdest = ""
		
		IF dw_3.Object.paen_numero[1] = Long(data) THEN
			 	MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
						", fue ingresado como pallet destino. " + &
							"~r~rIngrese otro número de pallet.")
			This.SetItem(1, ls_Columna, Long(ls_Null))
			istr_Mant.Argumento[5]	=	""
			RETURN 1
		ELSE
			dw_2.Object.expo_codigo.Protect = 1
			dw_2.Object.expo_codigo.BackGround.Color = RGB(192,192,192)
			istr_Mant.Argumento[5]	=	Data
			ib_inspeccion_Origen 	=  FALSE
			IF ExistePallet(Long(istr_Mant.Argumento[5]),TRUE) THEN
				
				lb_detalle = existedetallepallet(Long(istr_Mant.Argumento[5]))
				
				IF iuo_spro_palletencab.inspeccion >= 1 THEN
					ib_inspeccion_Origen 	=  TRUE
				ELSE
					IF ib_inspeccion_destino THEN
						MessageBox("Atención", "Trabajará con un pallet NO inspeccionado")										
					END IF	
				END IF
					
				If dw_2.Object.repa_nrosag[1] > 0 Then 
					If IsNull(iuo_spro_palletencab.Inspeccion) Then iuo_spro_palletencab.Inspeccion = 0
					If iuo_spro_palletencab.Inspeccion = 0 Then
						MessageBox("Atención", "El pallet debe estar Inspeccionado.")
						This.SetItem(Row, ls_Columna, Long(ls_Null))
						Return 1
					End If
				End If
					
				IF iuo_spro_palletencab.Estado > 1 THEN
					IF lb_detalle THEN
						MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
										", " + ls_Estado[iuo_spro_palletencab.Estado] + &
										"~r~rIngrese otro número de pallet")
					ELSE
						MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
										", No posee Detalle de Fruta."  + &
										"~r~rIngrese otro número de pallet")										
					END IF					
					This.SetItem(1, ls_Columna, Long(ls_Null))
					istr_Mant.Argumento[5]	=	""
					RETURN 1
				ELSEIF Atributos_Pallet() THEN
					
					Parent.TriggerEvent("ue_recuperapallet_origen")
					
					Asigna_PalletNuevo()
					
					dw_5.GetChild("tpen_codigo", idwc_tipopaldest)
					idwc_tipopaldest.SetTransObject(SqlCa)
					idwc_tipopaldest.Retrieve(iuo_spro_palletencab.TipoEnvase, &
													  iuo_spro_palletencab.Envase)

					dw_5.SetItem(1, "enva_tipoen", iuo_spro_palletencab.TipoEnvase)
					dw_5.SetItem(1, "enva_codigo", iuo_spro_palletencab.Envase)
						
					dw_3.GetChild("tpen_codigo", idwc_tipopaldest)
					idwc_tipopaldest.SetTransObject(SqlCa)
					idwc_tipopaldest.Retrieve(iuo_spro_palletencab.TipoEnvase, &
													  iuo_spro_palletencab.Envase)
													  
					IF dw_4.RowCount() = 0 THEN													  
						dw_3.SetItem(1, "enva_tipoen", iuo_spro_palletencab.TipoEnvase)
						dw_3.SetItem(1, "enva_codigo", iuo_spro_palletencab.Envase)
					END IF
					
					IF ExisteEnvase(iuo_spro_palletencab.TipoEnvase, &
										 iuo_spro_palletencab.Envase, istr_envase) THEN
						
						IF dw_4.RowCount() = 0 THEN
							dw_3.SetItem(1, "enva_nombre", istr_envase.Nombre)
						END IF
						
						dw_5.SetItem(1, "enva_nombre", istr_envase.Nombre)
					END IF
					
					This.Object.paen_numero.Protect	=	1
					This.Object.paen_numero.BackGround.Color = RGB(192,192,192)
	
					dw_3.Object.tpen_codigo.Protect	=	0
					dw_3.Object.tpen_codigo.BackGround.Color = RGB(255,255,255)
					dw_5.Object.tpen_codigo.Protect	=	0
					dw_5.Object.tpen_codigo.BackGround.Color = RGB(255,255,255)
					
					is_palletorig = Mid(data,len(Data) - 6, 7)
					
					dw_3.SetFocus()
					dw_3.SetColumn("tpen_codigo")
				ELSE
					MessageBox("Atención", "Atributos pallet destino y pallet origen" + &
									" son distintos.~r~rIngrese otro número de pallet.")
					This.SetItem(1, ls_Columna, Long(ls_Null))
					istr_Mant.Argumento[5]	=	""					
					RETURN 1
				END IF
			ELSE
				MessageBox("Atención", "Número de pallet " + String(Long(Data), '00000000') + &
								", no ha sido registrado.~r~rIngrese o seleccione otro número.")
				This.SetItem(1, ls_Columna, Long(ls_Null))
				istr_Mant.Argumento[5]	=	""				
				RETURN 1
			END IF
 		END IF

	CASE "tpen_codigo"
		IF iuo_tipopallet.Existe(dw_5.Object.enva_tipoen[1], &
										 dw_5.Object.enva_codigo[1], &
										 Data,True,SqlCa) THEN
			This.SetItem(1, "tota_ccajas", iuo_tipopallet.CajasPallet)
			This.SetItem(1, "paen_altura", iuo_tipopallet.AlturaPallet)
		ELSE
			This.SetItem(1, ls_Columna, ls_Null)
			RETURN 1
		END IF
		
END CHOOSE
end event

event itemerror;RETURN 1
end event

event losefocus;call super::losefocus;accepttext()

IF is_palletorig <> "" THEN
	is_palletorig	=	String(long(Mid( is_palletorig, 1, Len(is_palletorig) - 1)), "0000000") + &
					      Mid(is_palletorig, Len(is_palletorig))
	
	this.SetItem(il_fila, "paen_numero", long(is_palletorig))
END IF 	
end event

event itemfocuschanged;call super::itemfocuschanged;IF dwo.Name = "paen_numero" THEN
	This.Object.paen_numero.EditMask.Mask = "##########"
END IF

end event

type cb_todos from commandbutton within w_maed_spro_repalasigenca
boolean visible = false
integer x = 3250
integer y = 1720
integer width = 274
integer height = 116
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Todos"
end type

event clicked;Long  ll_Fila
Integer	li_Certifica

IF dw_6.RowCount()>0 THEN
	IF Controla_Totalcajas(1) THEN
		dw_6.SetReDraw(False)
		FOR ll_Fila=1 TO dw_6.RowCount()
			dw_6.Object.caja_traspa[ll_fila] = dw_6.Object.pafr_ccajas[ll_fila]
		NEXT
		IF Not Integridad() THEN Return
		li_Certifica		=	certifica()
		IF li_Certifica	=	-1	THEN Return
		IF li_Certifica	=	-2 THEN li_Certifica	=	dw_6.Object.pafr_eurepg[1]
		
		Traspasa_cajas(li_Certifica)		
		dw_3.Object.paen_tipopa.Protect	=	0
		dw_3.Object.paen_tipopa.BackGround.Color = RGB(255,255,255)
		dw_3.SetFocus()
		dw_3.SetColumn("paen_tipopa")	
		pb_grabar.Enabled		=	True
		pb_nvoporg.Enabled	=  FALSE
		pb_nvopdest.Enabled	=	FALSE
		captura_total_cajas()
		dw_6.SetReDraw(True)
	ELSE
		MessageBox("","El Total de Cajas a Traspasar supera el Máximo permitido por el Tipo de Pallet.")
	END IF	
END IF	
end event

type pb_acepta from picturebutton within w_maed_spro_repalasigenca
string tag = "Traspasa"
integer x = 3305
integer y = 1160
integer width = 155
integer height = 132
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
boolean enabled = false
string picturename = "\Desarrollo\Bmp\Aceptae.bmp"
string disabledname = "\Desarrollo\Bmp\Aceptae.bmp"
alignment htextalign = left!
end type

event clicked;Integer	li_Certifica
String	ls_Tipopa
Long		ll_Cajas

dw_6.AcceptText()

IF dw_2.Object.repa_tipopa[1] = 6 THEN
	IF dw_6.RowCount() > 0 THEN AgregaTodos(TRUE)
		pb_grabar.Enabled		=	True
		pb_nvoporg.Enabled	=  False

		pb_nvopdest.Enabled	=	False
		captura_total_cajas()
ELSE
	IF dw_6.RowCount() >0 THEN
		IF dw_6.Object.total_traspa[dw_6.RowCount()] >0 THEN
			IF ExistePallet(dw_3.Object.paen_numero[1], False) THEN
				IF Not Integridad() THEN Return 1
				li_Certifica	=	certifica()
				IF li_certifica = -1 THEN Return 1
				IF li_certifica = -2 THEN li_certifica	=	dw_6.Object.pafr_eurepg[1]
			END IF
			IF Controla_TotalCajas(0) THEN
				Traspasa_Cajas(li_Certifica)
				pb_grabar.Enabled		=	True
				pb_nvoporg.Enabled	=  False
				pb_nvopdest.Enabled	=	False
				captura_total_cajas()
				dw_6.GroupCalc()
				ls_Tipopa	=	dw_5.Object.tpen_codigo[1]
				
				SELECT	tpen_cancaj  
    				INTO	:ll_Cajas 
					FROM	dba.tipopallenvase  
					WHERE	enva_tipoen = :iuo_spro_palletencab.TipoEnvase
					AND	enva_codigo = :iuo_spro_palletencab.Envase
					AND	tpen_codigo = :ls_Tipopa ;
				
				If dw_6.RowCount() > 0 Then 
					IF dw_6.Object.total_cajas[1]	< ll_Cajas THEN
						dw_5.Object.paen_tipopa[1]	=	2
					END IF
				Else
					dw_5.Object.paen_tipopa[1]	=	2
				End IF
			ELSE
				MessageBox("Atención", "Total de cajas del detalle del pallet destino" + &
						  "~rno puede ser mayor a cajas total de pallet.")
			END IF
		END IF
	ELSE	
		MessageBox("Atención", "El pallet origen no posee detalle a traspasar. " + &
					  "~rElija un nuevo pallet origen.")
	END IF	
END IF	
end event

type dw_6 from uo_dw within w_maed_spro_repalasigenca
string tag = "Detalle de Pallet Origen"
integer x = 32
integer y = 1384
integer width = 3154
integer height = 516
integer taborder = 40
string title = "Detalle de Pallet Origen"
string dataobject = "dw_mues_palletfruta_repaletizaje_origen"
boolean hscrollbar = true
boolean livescroll = true
end type

event itemchanged;call super::itemchanged;String	ls_Columna, ls_Null
Integer	li_Cajas

SetNull(ls_Null)

ls_Columna = dwo.name

CHOOSE CASE ls_Columna

	CASE "caja_traspa"
		li_Cajas	=	This.Object.pafr_ccajas[Row]
		IF Integer(Data) > li_Cajas THEN
			MessageBox("Atención", "Cantidad de Cajas a Traspasar" + &
						  "~rno puede ser mayor a Cajas del Pallet.")
			This.SetItem(Row, ls_Columna, Long(ls_Null))
			RETURN 1
		END IF

END CHOOSE
end event

event itemerror;call super::itemerror;RETURN 1
end event

type dw_4 from uo_dw within w_maed_spro_repalasigenca
string tag = "Detalle de Pallet Destino"
integer x = 32
integer y = 612
integer width = 3154
integer height = 516
integer taborder = 0
string title = "Detalle de Pallet Destino"
string dataobject = "dw_mues_palletfruta_repaletizaje"
boolean hscrollbar = true
boolean livescroll = true
end type

type pb_cancela from picturebutton within w_maed_spro_repalasigenca
boolean visible = false
integer x = 3310
integer y = 1160
integer width = 155
integer height = 132
integer taborder = 90
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo\Bmp\Cancelae.bmp"
string disabledname = "\Desarrollo\Bmp\Cancelad.bmp"
alignment htextalign = left!
end type

event clicked;IF dw_4.RowCount() > 0 THEN AgregaTodos(FALSE)
pb_nvoporg.Enabled	=  TRUE
pb_nvopdest.Enabled	=	TRUE
captura_total_cajas()
end event

