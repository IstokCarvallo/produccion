$PBExportHeader$w_maed_movtofrutaemba_proceso_cajasprod.srw
$PBExportComments$Ventana de Recepción Fruta Embalada de Proceso
forward
global type w_maed_movtofrutaemba_proceso_cajasprod from w_mant_encab_deta_csd
end type
type dw_4 from uo_dw within w_maed_movtofrutaemba_proceso_cajasprod
end type
type dw_5 from datawindow within w_maed_movtofrutaemba_proceso_cajasprod
end type
type dw_caja from datawindow within w_maed_movtofrutaemba_proceso_cajasprod
end type
type dw_tarja from datawindow within w_maed_movtofrutaemba_proceso_cajasprod
end type
type pb_tarjas from picturebutton within w_maed_movtofrutaemba_proceso_cajasprod
end type
type pb_cambio_folio from picturebutton within w_maed_movtofrutaemba_proceso_cajasprod
end type
type pb_ventanas from uo_botonventanas within w_maed_movtofrutaemba_proceso_cajasprod
end type
type dw_6 from datawindow within w_maed_movtofrutaemba_proceso_cajasprod
end type
type dw_3 from uo_dw within w_maed_movtofrutaemba_proceso_cajasprod
end type
end forward

global type w_maed_movtofrutaemba_proceso_cajasprod from w_mant_encab_deta_csd
integer width = 4398
integer height = 2516
string title = "INGRESO DE PROCESO"
string menuname = ""
boolean controlmenu = false
boolean maxbox = false
windowstate windowstate = maximized!
event ue_recuperapallet ( )
event ue_cambio_folio ( )
dw_4 dw_4
dw_5 dw_5
dw_caja dw_caja
dw_tarja dw_tarja
pb_tarjas pb_tarjas
pb_cambio_folio pb_cambio_folio
pb_ventanas pb_ventanas
dw_6 dw_6
dw_3 dw_3
end type
global w_maed_movtofrutaemba_proceso_cajasprod w_maed_movtofrutaemba_proceso_cajasprod

type variables
DataWindowChild							idwc_exportador, idwc_planta, idwc_tipofrio, idwc_periodofrio, &
                  								idwc_especie, idwc_variedad, idwc_tipoenvase, idwc_envase, &
												idwc_categoria, idwc_etiqueta, idwc_destino, idwc_recibidor, &
												idwc_tipopallet, idwc_calibre, idwc_camara, idwc_cliente
	
str_envase									istr_envase
str_calibreenvase							istr_calibre

uo_plantadesp								iuo_Planta
uo_exportadores							iuo_exportadores
uo_variedades								iuo_variedades
uo_categorias								iuo_categorias
uo_etiquetas								iuo_etiquetas
uo_destinos									iuo_destinos
uo_recibidores								iuo_recibidores
uo_tipopallet 								iuo_tipopallet
uo_productores								iuo_productores
uo_lotesfrutagranel						iuo_Lote
uo_spro_ordenproceso					iuo_spro_ordenproceso
uo_spro_palletencab						iuo_spro_palletencab
uo_spro_movtofrutaembaenca			iuo_spro_movtofrutaembaenca
uo_camarasfrigo							iuo_camara
uo_fechaMovto								iuo_FechaMovto
uo_AnalizaPallet							iuo_pallet
uo_valida_codigopallet					iuo_copa

Integer										ii_envacodigo, ii_envatipoen, ii_categoria
String											is_ultimacol, is_embalaje, is_embacodigo, is_buscemba
Boolean										ib_Modifica, ib_AutoCommit, lb_existe = FALSE, lb_nuevopallet

w_info_etiqetas_compactos_cajasprod iw_mantencion1
end variables

forward prototypes
public subroutine buscaproductor ()
public function boolean existelote ()
public subroutine fueradenorma (long al_fila)
public subroutine existemovtoproceso (long al_proceso)
protected function integer wf_modifica ()
public subroutine captura_totales ()
public function boolean existeembalaje (integer ai_tipoenvase, integer ai_envase, integer ai_etiqueta, string ls_columna)
public function boolean existecalibres ()
public subroutine habilitaingreso ()
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine limpia_pallet ()
public subroutine habilitapallet (boolean habilita)
public subroutine buscapallet ()
public subroutine habilitaencab (boolean habilita)
public subroutine buscaordenproceso ()
public function boolean existepallet (integer ai_tipodocrel, long al_doctorel)
public subroutine borradetalles ()
public subroutine buscaembalaje ()
public function boolean noexistecliente (integer cliente)
public function boolean existemovimiento (long al_numero, integer ai_cliente)
public function integer buscaexportador ()
public function boolean cargaembalaje (string as_embacodigo)
public function string cargacodigo (integer ai_cliente, long al_planta, long al_pallet, integer ai_procedencia)
public subroutine imprime_ventana ()
public subroutine imprime_tarja (integer ai_formato)
end prototypes

event ue_recuperapallet();Long		ll_fila_d, ll_fila_e, respuesta
Boolean	lb_Habilita	= True
String		ls_embalaje

DO
	dw_3.SetRedraw(False)
	dw_3.Reset()
	
	pb_ventanas.Enabled 			=	False

	IF dw_3.Retrieve(Integer(istr_Mant.Argumento[2]), Long(istr_Mant.Argumento[7]), Integer(istr_Mant.Argumento[1])) =  -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSE
		dw_3.GetChild("enva_codigo", idwc_envase)
		idwc_envase.SetTransObject(SqlCa)

		IF idwc_envase.Retrieve(dw_3.Object.enva_tipoen[1]) = 0 THEN
			MessageBox("Atención", "Falta Registrar Envase")
			idwc_envase.InsertRow(0)
		ELSE
			idwc_envase.SetSort("enva_nombre A")
			idwc_envase.Sort()
		END IF
		
		ls_embalaje						=	dw_3.Object.emba_codigo[1]
		dw_3.GetChild("tpen_codigo", idwc_tipopallet)
		idwc_tipopallet.SetTransObject(sqlca)
		idwc_tipopallet.Retrieve(Integer(istr_Mant.Argumento[1]),ls_embalaje)
		
		iuo_tipopallet.Existe_PorEmbalaje(Integer(istr_Mant.Argumento[1]),ls_embalaje, dw_3.Object.tpen_codigo[1],True, Sqlca)
	
		is_embacodigo					=	dw_3.Object.emba_codigo[1]
		CargaEmbalaje(is_embacodigo)
		istr_Mant.Argumento[9] 		= 	STRING(ii_envatipoen)
		istr_Mant.Argumento[10]	= 	STRING(ii_envacodigo)

		DO
			ll_Fila_d	=	dw_4.Retrieve( Integer(istr_Mant.Argumento[2]), Long(istr_Mant.Argumento[7]), Integer(istr_Mant.Argumento[1]))
								  
			IF ll_Fila_d = -1 THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			ELSE
				IF ll_Fila_d > 0 THEN
					HabilitaPallet(False)
					
					IF dw_3.Object.paen_estado[1] = 2 THEN lb_Habilita	=	False

					dw_4.Enabled					=	lb_Habilita
					pb_eliminar.Enabled			=	lb_Habilita
					pb_grabar.Enabled			=	lb_Habilita
					pb_imprimir.Enabled			=	True
					pb_ins_det.Enabled			=	lb_Habilita
					pb_eli_det.Enabled			=	lb_Habilita
	
					pb_cambio_folio.Enabled 	=	lb_Habilita
					pb_ventanas.Enabled 			=	dw_3.Object.paen_tipopa[1]	=	1
					
					pb_ventanas.ii_cliente		=	dw_3.Object.clie_codigo[1]
					pb_ventanas.ii_especie		=	dw_3.Object.espe_codigo[1]
					pb_ventanas.ii_cajas			=	dw_3.Object.paen_ccajas[1]
					pb_ventanas.il_planta			=	dw_3.Object.plde_codigo[1]
					pb_ventanas.il_pallet			=	dw_3.Object.paen_numero[1]
					pb_ventanas.ii_procedencia	=	1//Granel
					pb_ventanas.ii_operacion	=	1//Impresion
					pb_ventanas.ii_sistema		=	1
					
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
				ELSE
					lb_Habilita = True
					
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
	dw_3.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_cambio_folio();Integer 	li_fila, li_cliente
Long		ll_pallet, ll_planta, ll_antiguo

str_mant	lstr_mant

lstr_mant.Argumento[1]	=	String(dw_3.Object.clie_codigo[1])
lstr_mant.Argumento[2]	=	String(dw_3.Object.plde_codigo[1])
lstr_mant.Argumento[3]	=	String(dw_3.Object.paen_numero[1])

OpenWithParm(w_cambio_folio_pallets, lstr_mant)

lstr_mant = Message.PowerObjectParm

li_cliente		=	Integer(lstr_mant.Argumento[1])
ll_planta		=	Long(lstr_mant.Argumento[2])
ll_antiguo	=	Long(lstr_mant.Argumento[3])
ll_pallet		=	Long(lstr_mant.Argumento[4])

IF ll_pallet > 0 AND NOT IsNull(ll_pallet) THEN
	DECLARE Cambio_Folio_pallet PROCEDURE FOR dbo.fgran_cambio_folio_pallet  
			@cliente	= :li_cliente,   
			@planta 	= :ll_planta,   
			@pallet 	= :ll_antiguo,   
			@nuevo 	= :ll_pallet  
		USING SQLCA;
		
		EXECUTE Cambio_Folio_pallet;
		
		IF sqlca.SQLCode = -1 THEN
			F_errorbasedatos(sqlca,"Actualización de Folio de Pallet")
			Rollback;
			istr_mant.argumento[7]	=	String(ll_antiguo)
		ELSE
			Commit;
			istr_mant.argumento[7]	=	String(ll_pallet)
		END IF
	CLOSE Cambio_Folio_pallet;
ELSE
	istr_mant.argumento[7]	=	String(ll_antiguo)
END IF

TriggerEvent("ue_recuperapallet")
end event

public subroutine buscaproductor ();Str_Busqueda	lstr_Busq

OpenWithParm(w_busc_productores, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	dw_3.SetItem(1, "prod_codrot", Integer(lstr_Busq.Argum[1]))
	dw_3.SetItem(1, "prod_nomrot", lstr_Busq.Argum[2])

	dw_3.SetColumn("prod_codrot")
	dw_3.SetFocus()
END IF
end subroutine

public function boolean existelote ();RETURN True
end function

public subroutine fueradenorma (long al_fila);Integer	li_Especie, li_Variedad, li_Categoria, li_Cantidad
String	ls_Embalaje, ls_Calibre
Date		ld_FechaEmb

li_Especie		=	dw_4.Object.espe_codigo[al_Fila]
li_Variedad		=	dw_4.Object.vari_codigo[al_Fila]
li_Categoria	=	dw_4.Object.cate_codigo[al_Fila]
ld_FechaEmb		=	dw_4.Object.pafr_fecemb[al_Fila]
ls_Embalaje		=	dw_4.Object.emba_codigo[al_Fila]
ls_Calibre		=	dw_4.Object.pafr_calibr[al_Fila]

SELECT 	Count(emfn_calibr)
	INTO	:li_Cantidad
	FROM	dbo.spro_embafueranorma
	WHERE	espe_codigo =	:li_Especie
	AND	vari_codigo	=	:li_Variedad
	AND	cate_codigo	=	:li_Categoria
	AND	emba_codigo	=	:ls_Embalaje
	AND	:ld_FechaEmb between emfn_fecini and emfn_fecter
	AND	emfn_calibr	=	:ls_Calibre;

IF SQLCA.SQLCode = -1 THEN
	F_ErrorBaseDatos(SQLCA, "Lectura de Tabla de Fuera de Norma")
ELSE
	IF li_Cantidad = 0 THEN
		dw_4.SetItem(al_Fila, "pafr_estemb", 1)
	ELSE
		dw_4.SetItem(al_Fila, "pafr_estemb", 2)
	END IF
END IF

RETURN
end subroutine

public subroutine existemovtoproceso (long al_proceso);
end subroutine

protected function integer wf_modifica ();IF dw_4.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF dw_3.AcceptText() = -1 THEN RETURN -1
IF (dw_4.ModifiedCount() + dw_4.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0

RETURN 1
end function

public subroutine captura_totales ();Long	ll_Fila, ll_Total_Cajas

dw_4.AcceptText()

ll_Fila	=	dw_4.RowCount()

IF  ll_Fila > 0 THEN
	ll_Total_Cajas	=	dw_4.Object.total_cajas[ll_Fila]
END IF

dw_3.Object.paen_ccajas[1]	=	ll_Total_Cajas

RETURN
end subroutine

public function boolean existeembalaje (integer ai_tipoenvase, integer ai_envase, integer ai_etiqueta, string ls_columna);String	ls_codigo, ls_nombre
Integer	li_tipoen, li_codenvase, li_envacodigo, li_envatipoen, li_clie_codigo

ls_codigo			= ls_columna
li_clie_codigo 	= dw_2.Object.Clie_codigo[dw_2.GetRow()]

IF gstr_paramplanta.etiquetaembalaje = 1 THEN
	SELECT	emba_nombre, enva_tipoen, enva_codigo, enva_codigo, enva_tipoen
		INTO 	:ls_nombre, :li_tipoen, :li_codenvase, :li_envacodigo, :li_envatipoen
		FROM	dbo.embalajesprod
		WHERE emba_codigo	= :ls_codigo
		AND   etiq_codigo = :ai_etiqueta
		AND	enva_tipoen	= :ai_tipoenvase
		AND	enva_codigo	= :ai_envase
		AND   clie_codigo = :li_clie_codigo;
ELSE
	SELECT	emba_nombre, enva_tipoen, enva_codigo, enva_codigo, enva_tipoen
		INTO 	:ls_nombre, :li_tipoen, :li_codenvase, :li_envacodigo, :li_envatipoen
		FROM	dbo.embalajesprod
		WHERE emba_codigo	= :ls_codigo
		AND	enva_tipoen	= :ai_tipoenvase
		AND	enva_codigo	= :ai_envase
		AND   clie_codigo = :li_clie_codigo;
END IF		

IF SqlCa.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embalajes")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Embalaje (" + ls_codigo + &
					"), no ha sido creado en tabla respectiva.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
END IF

is_embalaje	=	ls_nombre
ii_envacodigo = li_envacodigo
ii_envatipoen = li_envatipoen
RETURN True
end function

public function boolean existecalibres ();Integer	li_Cantidad, li_Especie, li_TipoEnvase, li_Envase

li_Especie		=	dw_2.Object.espe_codigo[1]
li_TipoEnvase	=	dw_3.Object.enva_tipoen[1]
li_Envase		=	dw_3.Object.enva_codigo[1]

SELECT	Count(caen_calibr)
	INTO	:li_Cantidad
	FROM	dbo.calibresenvase
	WHERE	espe_codigo	=	:li_Especie
	AND	enva_tipoen	=	:li_TipoEnvase
	AND	enva_codigo	=	:li_Envase ;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla CalibresEnvase")

	RETURN False
ELSEIF li_Cantidad = 0 THEN

	RETURN False
END IF

RETURN True
end function

public subroutine habilitaingreso ();Date	ld_fecha
Boolean		lb_estado = True

dw_2.AcceptText()
dw_3.AcceptText()

IF IsNull(dw_2.Object.mfee_fecmov[1]) OR dw_2.Object.mfee_fecmov[1] = ld_fecha OR &
	IsNull(dw_2.Object.mfee_tipdoc[1]) OR dw_2.Object.mfee_tipdoc[1] = 0 OR &
	IsNull(dw_2.Object.mfee_docrel[1]) OR dw_2.Object.mfee_docrel[1] = 0 OR &
	IsNull(dw_3.Object.vari_codigo[1]) OR dw_3.Object.vari_codigo[1] = 0 OR &
	IsNull(dw_3.Object.cate_codigo[1]) OR dw_3.Object.cate_codigo[1] = 0 OR &
	IsNull(dw_3.Object.enva_tipoen[1]) OR dw_3.Object.enva_tipoen[1] = 0 OR &
	IsNull(dw_3.Object.enva_codigo[1]) OR dw_3.Object.enva_codigo[1] = 0 OR &
	IsNull(dw_3.Object.copa_codigo[1]) OR dw_3.Object.copa_codigo[1] = 0 OR &
	IsNull(dw_3.Object.tpen_codigo[1]) OR dw_3.Object.tpen_codigo[1] = "" OR &
	IsNull(dw_3.Object.etiq_codigo[1]) OR dw_3.Object.etiq_codigo[1] = 0 OR &
	IsNull(dw_3.Object.paen_feccon[1]) OR dw_3.Object.paen_feccon[1] = ld_fecha OR &
	IsNull(dw_3.Object.paen_altura[1]) OR dw_3.Object.paen_altura[1] = 0 OR &
	IsNull(dw_3.Object.cama_codigo[1]) OR dw_3.Object.paen_estado[1] = 2 THEN
	lb_estado = False

ELSEIF dw_3.Object.paen_tipopa[1] = 1 AND &
	IsNull(dw_3.Object.tpen_codigo[1]) OR dw_3.Object.tpen_codigo[1] = '' THEN
	lb_estado = False
	
END IF

pb_ins_det.Enabled 		=	lb_estado
pb_cambio_folio.Enabled =	lb_estado
pb_tarjas.Enabled 		=	lb_estado
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_Retorno
Integer 	Planta, movto, numero, li_Accion, li_fila
String		ls_pallet

If Not dw_2.uf_check_required(0) Then RETURN False
If Not dw_1.uf_validate(0) Then RETURN False

If NOT Borrando Then
	If dw_2.GetItemStatus(1, 0, Primary!) = NewModified! Then
		dw_2.SetItem(1,"mfee_horact",F_FechaHora())
	End If
End If

planta =		Integer(Istr_Mant.Argumento[2])
movto =		Integer(Istr_Mant.Argumento[3])				
numero=		Integer(Istr_Mant.Argumento[4])


If dw_caja.RowCount() > 0 Then
	dw_caja.RowsMove(1, dw_caja.RowCount(), Primary!, dw_caja, 1, Delete!)
End If

If Borrando Then
	If dw_6.Update(True, False) = 1 Then
		If dw_4.Update(True, False) = 1 Then
			If dw_1.Update(True, False) = 1 Then
				If dw_3.Update(True, False) = 1 Then
					If dw_5.Update(True, False) = 1 Then
						If dw_2.Update(True, False) = 1 Then
							If dw_caja.Update(True, False) = 1 Then
								Commit;
						
								If sqlca.SQLCode <> 0 Then
									F_ErrorBaseDatos(sqlca, This.Title)
								Else
									li_Accion		= 2
									
									lb_Retorno	=	True
						
									dw_1.ResetUpdate()
									dw_2.ResetUpdate()
									dw_3.Reset()
									dw_4.Reset()
									dw_5.ResetUpdate()
									dw_6.ResetUpdate()
									dw_caja.Reset()
									dw_caja.ResetUpdate()
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
	If dw_3.Update(True, False) = 1 Then
		If dw_4.Update(True, False) = 1 Then
			If dw_2.Update(True, False) = 1 Then
				If dw_1.Update(True, False) = 1 Then	
					If dw_caja.Update(True, False) = 1 Then
						
						DELETE dbo.spro_movtofrutaembaenca
						 WHERE plde_codigo =	:Planta
						   AND tpmv_codigo =	:Movto
						   AND mfee_numero =  0;
							
						Commit;
						dw_3.ResetUpdate()
						
						If sqlca.SQLCode <> 0 Then
							F_ErrorBaseDatos(sqlca, This.Title)
							
							RollBack;
						Else
							ls_pallet	=	String(dw_3.Object.clie_codigo[dw_3.GetRow()], '000') + &
												String(dw_3.Object.paen_numero[dw_3.GetRow()], '000000')
											
							If iuo_pallet.analiza_datos(ls_pallet, SqlCa) Then
								dw_3.Object.paen_sscc18[1] =	Mid(iuo_pallet.CodBarra, 1, 20)
							End If
							
							dw_3.Object.paen_gs1128[dw_3.GetRow()]	=	 CargaCodigo(dw_3.Object.clie_codigo[dw_3.GetRow()], &
																									dw_3.Object.plde_codigo[dw_3.GetRow()], &
																									dw_3.Object.paen_numero[dw_3.GetRow()], 1)
							If dw_3.Update(True, False) = 1 Then
								Commit;	
								If sqlca.SQLCode <> 0 Then
									F_ErrorBaseDatos(sqlca, This.Title)
								Else
							
									li_Accion		= 	1
											
									lb_Retorno		=	True
		
									dw_1.ResetUpdate()
									dw_2.ResetUpdate()
									dw_3.ResetUpdate()
									dw_4.ResetUpdate()
									dw_caja.Reset()
									dw_caja.ResetUpdate()
								End If
							Else
								F_ErrorBaseDatos(sqlca, This.Title)
								RollBack;
							End If
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
	//Nuevo Pallet
	If lb_nuevopallet Then
		dw_3.Reset()
		dw_3.InsertRow(0)
		dw_4.Reset()
		dw_3.SetItem(1, "clie_codigo", Integer(istr_Mant.Argumento[1]))
		dw_3.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))
		dw_3.SetItem(1, "paen_feccon", DateTime(Today()))
		dw_3.SetItem(1, "paen_inspec", 0)
		dw_3.SetItem(1, "paen_fumiga", 0)
		dw_3.SetItem(1, "paen_estado", 1)
		lb_nuevopallet	=	False
	End If
End If

sqlca.AutoCommit	=	ib_AutoCommit

RETURN lb_Retorno
end function

public subroutine limpia_pallet ();String		ls_Null

SetNull(ls_Null)

dw_3.SetItem(1, "clie_codigo", Integer(istr_Mant.Argumento[1]))
dw_3.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))

dw_3.SetItem(1, "cama_codigo", 0)

HabilitaEncab(FALSE)

dw_3.SetItem(1, "espe_codigo", iuo_spro_ordenproceso.Especie)
dw_3.SetItem(1, "vari_codigo", iuo_spro_ordenproceso.Variedad)
dw_3.SetItem(1, "prod_codrot", iuo_spro_ordenproceso.Productor)
dw_3.SetItem(1, "paen_feccon", iuo_spro_ordenproceso.FechaOrden)

dw_3.SetItem(1, "paen_tipopa", Integer(ls_Null))
dw_3.SetItem(1, "enva_tipoen", Integer(ls_Null))
dw_3.SetItem(1, "enva_codigo", Integer(ls_Null))
dw_3.SetItem(1, "tpen_codigo", ls_Null)
dw_3.SetItem(1, "paen_altura", Integer(ls_Null))
dw_3.SetItem(1, "cate_codigo", Integer(ls_Null))
dw_3.SetItem(1, "etiq_codigo", Integer(ls_Null))
dw_3.SetItem(1, "dest_codigo", Integer(ls_Null))
dw_3.SetItem(1, "reci_codigo", Long(ls_Null))
dw_3.SetItem(1, "paen_ccajas", Integer(ls_Null))

end subroutine

public subroutine habilitapallet (boolean habilita);IF Habilita THEN
	dw_3.Object.vari_codigo.Protect	=	0
	dw_3.Object.paen_tipopa.Protect	=	0
	dw_3.Object.enva_tipoen.Protect	=	0
	dw_3.Object.enva_codigo.Protect	=	0
	dw_3.Object.tpen_codigo.Protect	=	0
	dw_3.Object.cate_codigo.Protect	=	0
	dw_3.Object.etiq_codigo.Protect	=	0
	dw_3.Object.cama_codigo.Protect	=	0
	
	dw_3.Object.vari_codigo.Color 		= 0
	dw_3.Object.enva_tipoen.Color 	= 0
	dw_3.Object.enva_codigo.Color 	= 0
	dw_3.Object.tpen_codigo.Color 	= 0
	dw_3.Object.cate_codigo.Color 	= 0
	dw_3.Object.etiq_codigo.Color 		= 0
	dw_3.Object.cama_codigo.Color	= 0
	
	dw_3.Object.vari_codigo.BackGround.Color = RGB(255,255,255)
	dw_3.Object.enva_tipoen.BackGround.Color = RGB(255,255,255)
	dw_3.Object.enva_codigo.BackGround.Color = RGB(255,255,255)
	dw_3.Object.tpen_codigo.BackGround.Color = RGB(255,255,255)
	dw_3.Object.cate_codigo.BackGround.Color = RGB(255,255,255)
	dw_3.Object.etiq_codigo.BackGround.Color = RGB(255,255,255)
	dw_3.Object.cama_codigo.BackGround.Color = RGB(255,255,255)
	
	dw_3.SetColumn("vari_codigo")
	dw_3.SetFocus()
ELSE
	dw_3.Object.clie_codigo.Protect	=	1
	dw_3.Object.vari_codigo.Protect	=	1
	dw_3.Object.paen_tipopa.Protect	=	1
	dw_3.Object.enva_tipoen.Protect	=	1
	dw_3.Object.enva_codigo.Protect	=	1
	dw_3.Object.cate_codigo.Protect	=	1
	dw_3.Object.etiq_codigo.Protect	=	1
	dw_3.Object.cama_codigo.Protect	=	1
	
	dw_3.Object.clie_codigo.Color 		= RGB(255,255,255)
	dw_3.Object.vari_codigo.Color 		= RGB(255,255,255)
	dw_3.Object.enva_tipoen.Color 	= RGB(255,255,255)
	dw_3.Object.enva_codigo.Color 	= RGB(255,255,255)
	dw_3.Object.cate_codigo.Color 	= RGB(255,255,255)
	dw_3.Object.etiq_codigo.Color 		= RGB(255,255,255)
	dw_3.Object.cama_codigo.Color 	= RGB(255,255,255)
	
	dw_3.Object.clie_codigo.BackGround.Color 		= 553648127
	dw_3.Object.vari_codigo.BackGround.Color 		= 553648127
	dw_3.Object.enva_tipoen.BackGround.Color 	= 553648127
	dw_3.Object.enva_codigo.BackGround.Color 	= 553648127
	dw_3.Object.cate_codigo.BackGround.Color 	= 553648127
	dw_3.Object.etiq_codigo.BackGround.Color 		= 553648127
	dw_3.Object.cama_codigo.BackGround.Color 	= 553648127
END IF
end subroutine

public subroutine buscapallet ();String ls_Null
Str_Busqueda	lstr_Busq

SetNull(ls_Null)

lstr_Busq.Argum[1] = istr_Mant.Argumento[2]//planta
lstr_Busq.Argum[2] = "0"//tipo
lstr_Busq.Argum[3] = istr_Mant.Argumento[4]//numero
lstr_Busq.Argum[4] = istr_Mant.Argumento[8]
lstr_Busq.Argum[5] = istr_Mant.Argumento[1]//Cliente

OpenWithParm(w_busc_pallet_movimiento, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

IF lstr_Busq.Argum[2] <> "" THEN
	dw_3.SetItem(1, "paen_numero", Integer(lstr_Busq.Argum[2]))

	IF iuo_spro_palletencab.Existe(Integer(istr_Mant.Argumento[1]), &
											 Integer(istr_Mant.Argumento[2]), &
											 Long(lstr_Busq.Argum[2]),False,SqlCa) THEN
		
		IF iuo_spro_palletencab.especie	= dw_2.Object.espe_codigo[1] THEN
			istr_Mant.Argumento[7]	=	lstr_Busq.Argum[2]
			TriggerEvent("ue_recuperapallet")
			lb_existe = TRUE
			dw_3.Object.prod_codrot[1] = iuo_spro_ordenproceso.Productor	

		ELSE
			lb_existe = FALSE
			MessageBox("Error de Datos","El pallet ingresado pertenece a otra especie.")
			dw_3.SetItem(1,"paen_numero",long(ls_Null))
			Return

		END IF
	END IF
END IF
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect				=	0
	dw_2.Object.mfee_numero.Protect			=	0
	dw_2.Object.mfee_fecmov.Protect				=	0
	dw_2.Object.mfee_tipdoc.Protect				=	0
	dw_2.Object.mfee_docrel.Protect				=	0
	dw_3.Object.clie_codigo.Protect				=	0
	dw_3.Object.vari_codigo.Protect				=	0
	dw_3.Object.paen_tipopa.Protect				=	0
	dw_3.Object.tpen_codigo.Protect				=	0
	dw_3.Object.cate_codigo.Protect				=	0
	dw_3.Object.etiq_codigo.Protect				=	0

	dw_2.Object.clie_codigo.Color 		= 	0
	dw_2.Object.mfee_numero.Color 	= 	0
	dw_2.Object.mfee_fecmov.Color 	= 	0
	dw_2.Object.mfee_tipdoc.Color 	= 	0
	dw_2.Object.mfee_docrel.Color 	= 	0
	dw_3.Object.clie_codigo.Color 		=	0 	
	dw_3.Object.vari_codigo.Color 		= 	0	
	dw_3.Object.enva_codigo.Color 	= 	0
	dw_3.Object.tpen_codigo.Color 	= 	0
	dw_3.Object.cate_codigo.Color 	= 	0
	dw_3.Object.etiq_codigo.Color 		=	0 	
	
	dw_2.Object.clie_codigo.BackGround.Color 		= 	RGB(255,255,255)
	dw_2.Object.mfee_numero.BackGround.Color 	= 	RGB(255,255,255)
	dw_2.Object.mfee_fecmov.BackGround.Color 	= 	RGB(255,255,255)
	dw_2.Object.mfee_tipdoc.BackGround.Color 	= 	RGB(255,255,255)
	dw_2.Object.mfee_docrel.BackGround.Color 	= 	RGB(255,255,255)
	dw_3.Object.clie_codigo.BackGround.Color 		= 	RGB(255,255,255)
	dw_3.Object.vari_codigo.BackGround.Color 		= 	RGB(255,255,255)
	dw_3.Object.paen_tipopa.BackGround.Color 	= 	RGB(255,255,255)
	dw_3.Object.tpen_codigo.BackGround.Color 	= 	RGB(255,255,255)
	dw_3.Object.cate_codigo.BackGround.Color 	= 	RGB(255,255,255)
	dw_3.Object.etiq_codigo.BackGround.Color 		= 	RGB(255,255,255)
	
	dw_2.Object.b_ordenproceso.visible = 1
	
	dw_2.SetColumn("clie_codigo")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clie_codigo.Protect				=	1
	dw_2.Object.mfee_numero.Protect			=	1
	dw_2.Object.mfee_fecmov.Protect				=	1
	dw_2.Object.mfee_tipdoc.Protect				=	1
	dw_2.Object.mfee_docrel.Protect				=	1

	dw_2.Object.clie_codigo.Color 		= 	RGB(255,255,255)
	dw_2.Object.mfee_numero.Color 	= 	RGB(255,255,255)
	dw_2.Object.mfee_fecmov.Color 	= 	RGB(255,255,255)
	dw_2.Object.mfee_tipdoc.Color 	= 	RGB(255,255,255)
	dw_2.Object.mfee_docrel.Color 	= 	RGB(255,255,255)
	
	dw_2.Object.clie_codigo.BackGround.Color 		= 	553648127
	dw_2.Object.mfee_numero.BackGround.Color 	= 	553648127
	dw_2.Object.mfee_fecmov.BackGround.Color 	= 	553648127
	dw_2.Object.mfee_tipdoc.BackGround.Color 	= 	553648127
	dw_2.Object.mfee_docrel.BackGround.Color 	= 	553648127
	
	dw_2.Object.b_ordenproceso.visible = 0
END IF
end subroutine

public subroutine buscaordenproceso ();Str_Busqueda	lstr_Busq

lstr_Busq.Argum[1]	=	istr_Mant.Argumento[2]	// Planta
lstr_Busq.Argum[2]	=	'1' 							// Estado = Vigente 1. Proceso
lstr_Busq.Argum[3]	=	istr_Mant.Argumento[5]	// Tipo Orden
lstr_Busq.Argum[4]   =  String(dw_2.Object.clie_codigo[1])

OpenWithParm(w_busc_spro_ordenproceso, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	
	dw_2.SetItem(1, "mfee_tipdoc", Integer(lstr_Busq.Argum[10]))
	dw_2.SetItem(1, "mfee_docrel", Long(lstr_Busq.Argum[6]))

	IF iuo_spro_ordenproceso.Existe(Integer(lstr_Busq.Argum[3]), &
											  Integer(lstr_Busq.Argum[10]), &
											  Long(lstr_Busq.Argum[6]), &
											  True, SqlCa,Integer(lstr_Busq.Argum[16])) THEN

	iuo_spro_ordenproceso.CapturaReferencias(1,0,1,0,0,False,Sqlca)

		dw_2.SetItem(1, "mfee_fecmov", iuo_spro_ordenproceso.FechaOrden)
		dw_2.SetItem(1, "prod_codigo", iuo_spro_ordenproceso.productor)
		dw_2.SetItem(1, "prod_nombre", iuo_spro_ordenproceso.NombreProductor)
		dw_2.SetItem(1, "frio_tipofr", iuo_spro_ordenproceso.TipoFrio)
		dw_2.SetItem(1, "pefr_codigo", iuo_spro_ordenproceso.PeriodoFrio)
		dw_2.SetItem(1, "espe_codigo", iuo_spro_ordenproceso.Especie)
		dw_2.SetItem(1, "vari_codigo", iuo_spro_ordenproceso.variedad)
		dw_2.SetItem(1, "vari_nombre", iuo_spro_ordenproceso.NombreVariedad)
		
		dw_3.SetItem(1, "espe_codigo", iuo_spro_ordenproceso.Especie)
		dw_3.SetItem(1, "prod_codrot", iuo_spro_ordenproceso.Productor)
		dw_3.SetItem(1, "prod_nomrot", iuo_spro_ordenproceso.NombreProductor)
   
		IF iuo_productores.Existe(iuo_spro_ordenproceso.Productor,False,SqlCa) THEN
			dw_2.SetItem(1, "clie_codigo", long(lstr_Busq.Argum[16]))
			dw_3.SetItem(1, "clie_codigo", long(lstr_Busq.Argum[16]))
			istr_Mant.Argumento[1]	= String(lstr_Busq.Argum[16])
		END IF
		
		dw_3.Enabled = True
		dw_3.Object.paen_numero.Protect	=	0
		dw_3.Object.paen_numero.BackGround.Color = RGB(255,255,255)
	
		dw_3.GetChild("vari_codigo", idwc_variedad)
		idwc_variedad.SetTransObject(SqlCa)
		idwc_variedad.Retrieve(iuo_spro_ordenproceso.Especie)
		idwc_variedad.SetSort("vari_nombre A")
		idwc_variedad.Sort()
	
		istr_Mant.Argumento[5]	= lstr_Busq.Argum[10]
		istr_Mant.Argumento[6]	= lstr_Busq.Argum[6]
		istr_Mant.Argumento[8]	= String(iuo_spro_ordenproceso.Especie)
		
		IF iuo_spro_movtofrutaembaenca.ExisteDoctoRelacionado(Integer(istr_Mant.Argumento[2]), Integer(lstr_Busq.Argum[10]), &
										Long(lstr_Busq.Argum[6]), SqlCa,Integer(lstr_Busq.Argum[16])) THEN
			istr_Mant.Argumento[4]	=	String(iuo_spro_movtofrutaembaenca.NumeroMovto)
			TriggerEvent("ue_recuperadatos")
		END IF
	END IF
END IF
end subroutine

public function boolean existepallet (integer ai_tipodocrel, long al_doctorel);Integer	li_Cantidad, li_cliente, li_Planta
Long		ll_NumeroPallet

li_cliente	=	Integer(istr_Mant.Argumento[1])
li_Planta	=	Integer(istr_Mant.Argumento[2])

SELECT	Max(paen_numero)
	INTO	:ll_NumeroPallet
	FROM	dbo.spro_palletfruta
	WHERE	clie_codigo	=	:li_Cliente
	AND	plde_codigo	=	:li_Planta
	AND	pafr_tipdoc	=	:ai_tipodocrel
	AND	pafr_docrel	=	:al_doctorel;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_pallerfruta")

	RETURN False
ELSEIF ll_NumeroPallet > 0 THEN
	istr_Mant.Argumento[7]	=	String(ll_NumeroPallet)
	RETURN True
END IF

RETURN True
end function

public subroutine borradetalles ();Integer li_tipdoc
Long    ll_docrel, ll_fila, ll_filabusca, ll_pallet
String  ls_planta, ls_cliente
Boolean lb_elimina = False

li_tipdoc = dw_2.Object.mfee_tipdoc[1]
ll_docrel = dw_2.Object.mfee_docrel[1]
ll_pallet = dw_3.Object.paen_numero[1]
ls_cliente = string(dw_3.Object.clie_codigo[1])
ls_planta = string(dw_3.Object.plde_codigo[1])

ll_fila = 1
Do While ll_fila<= dw_6.RowCount()

	If dw_6.Object.pafr_tipdoc[ll_fila] = li_tipdoc AND dw_6.Object.pafr_docrel[ll_fila] = ll_docrel AND &
		dw_6.Object.paen_numero[ll_fila] = ll_pallet Then
		dw_6.DeleteRow(ll_fila)
	Else
		ll_fila ++
	End If	
Loop

ll_filabusca = -1
ll_filabusca = dw_6.Find("clie_codigo = " + ls_cliente + " AND plde_codigo = " + ls_planta + " AND " + &
								 "paen_numero = " + String(ll_pallet), 1, dw_6.RowCount())
If ll_filabusca = 0 Then
	lb_elimina = TRUE
Else
	lb_elimina = FALSE
End If	

If lb_elimina = TRUE Then
	
	ll_filabusca = -1
	ll_filabusca = dw_5.Find("clie_codigo = " + ls_cliente + " AND plde_codigo = " + ls_planta + " AND " + &
									 "paen_numero = " + String(ll_pallet), 1, dw_5.RowCount())
	If ll_filabusca > 0 Then
		dw_5.DeleteRow(ll_filaBusca)
		lb_elimina = TRUE
	End If
End If


ll_filabusca = -1
ll_filabusca = dw_6.Find("clie_codigo = " + ls_cliente + " AND plde_codigo = " + ls_planta + " AND " + &
								 "paen_numero = " + String(ll_pallet) + " AND pafr_tipdoc = " + String(li_tipdoc) + " AND " + &
								 "pafr_docrel = " + String(ll_docrel), 1, dw_6.RowCount())
If ll_filabusca = 0 Then
	lb_elimina = TRUE
Else
	lb_elimina = FALSE
End If

If lb_elimina  Then
	ll_filabusca = -1
	ll_filabusca = dw_1.Find("clie_codigo = " + ls_cliente + " AND  plde_codigo = " + ls_planta + " AND " + &
									 "paen_numero = " + String(ll_pallet), 1, dw_1.RowCount())
										 
	If ll_filabusca > 0 Then
		dw_1.DeleteRow(ll_filabusca)
	End If
End If

end subroutine

public subroutine buscaembalaje ();Str_Busqueda	lstr_Busq

lstr_Busq.Argum[1]	=	""//istr_Mant.Argumento[9]
lstr_Busq.Argum[2]	=	""//istr_Mant.Argumento[10]

OpenWithParm(w_busc_envase_embalajes, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

If UpperBound(lstr_Busq.Argum) > 1 Then
	IF il_fila < 1 then 
		IF lstr_Busq.Argum[1] <> "" THEN
			dw_3.Object.emba_codigo[dw_3.GetRow()] = 	lstr_Busq.argum[1]
			is_buscemba										=	lstr_Busq.argum[1]
			
			dw_3.SetColumn("vari_codigo")
			dw_3.SetFocus()
		ELSE
			
			dw_3.SetColumn("emba_codigo")
			dw_3.SetFocus()
			
		END IF
	ELSE
		IF lstr_Busq.Argum[1] <> "" THEN
			dw_4.SetItem(il_Fila, "emba_codigo", lstr_Busq.Argum[1])
			is_buscemba	=	lstr_Busq.argum[1]
		
			dw_4.SetColumn("cate_codigo")
			dw_4.SetFocus()
		ELSE
			dw_4.SetColumn("emba_codigo")
			dw_4.SetFocus()
		END IF
		
	END IF
End IF
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

public function boolean existemovimiento (long al_numero, integer ai_cliente);IF iuo_spro_movtofrutaembaenca.Existe(Integer(istr_Mant.Argumento[2]), &
												  Integer(istr_Mant.Argumento[3]), &
												  al_Numero, True, SqlCa,ai_cliente) THEN
	IF IsNull(iuo_spro_movtofrutaembaenca.TipoDoctoRel) AND &
		IsNull(iuo_spro_movtofrutaembaenca.NumeroDoctoRel) AND &
		iuo_spro_movtofrutaembaenca.NumeroDoctoRel > 0 THEN
		MessageBox("Atención", "Número de Movimiento " + String(al_Numero, '00000000') + &
	  				  ", no corresponde a Proceso.~r~rIngrese o seleccione otro Número.")
		RETURN False
	ELSE
		istr_Mant.Argumento[5]	=	String(iuo_spro_movtofrutaembaenca.TipoDoctoRel)
		istr_Mant.Argumento[6]	=	String(iuo_spro_movtofrutaembaenca.NumeroDoctoRel)
		RETURN True
	END IF
ELSE
	RETURN False
END IF

end function

public function integer buscaexportador ();Integer li_expocodigo, li_fila, li_cliecod
String ls_exponombre
DataWindowChild ldwc_cliente

dw_2.GetChild("clie_codigo", ldwc_cliente)
li_fila = ldwc_cliente.Find("clie_codigo = " + string(dw_2.object.clie_codigo[1]), 1, ldwc_cliente.RowCount())
ls_exponombre 	= ldwc_cliente.GetItemString(li_fila, "Clie_nombre")
li_cliecod 		= ldwc_cliente.GetItemNumber(li_fila, "Clie_codigo")

SELECT expo_codigo
INTO :li_expocodigo
FROM dbo.Exportadores
WHERE expo_nombre = :ls_exponombre;

IF SqlCa.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Exportadores")
	RETURN -1
ELSEIF sqlca.SQLCode = 100 THEN
	RETURN li_cliecod
END IF

RETURN li_expocodigo
end function

public function boolean cargaembalaje (string as_embacodigo);Boolean	lb_Retorno = True
String		ls_codigo, ls_nombre
Integer	li_tipoen, li_codenvase, li_envacodigo, li_envatipoen, li_cliente

li_cliente = dw_2.Object.Clie_codigo[dw_2.RowCount()]

SELECT	emba_nombre, enva_tipoen, enva_codigo, enva_codigo, enva_tipoen
	INTO 	:ls_nombre, :li_tipoen, :li_codenvase, :li_envacodigo, :li_envatipoen
	FROM	dbo.embalajesprod
	WHERE emba_codigo	= :as_embacodigo
	AND 	clie_codigo = :li_cliente
	Using	Sqlca;

IF SqlCa.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embalajes")
	lb_Retorno =  False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Embalaje (" + ls_codigo + &
					"), no ha sido creado en tabla respectiva.~r~rIngrese o seleccione otro Código.")
	lb_Retorno =  False
END IF

is_embalaje		=	ls_nombre
ii_envacodigo 	=	li_envacodigo
ii_envatipoen 	=	li_envatipoen

RETURN lb_Retorno
end function

public function string cargacodigo (integer ai_cliente, long al_planta, long al_pallet, integer ai_procedencia);String ls_respuesta

DECLARE Codigo PROCEDURE FOR dbo.genera_adhesivos_pallets  
        @Planta 		= 	:al_planta,   
        @Cliente 		= 	:ai_cliente,   
        @Pallet 		= 	:al_pallet,   
        @Procedencia = 	:ai_procedencia  
	USING SQLCA;
			
EXECUTE Codigo;

IF SQLCA.SQLCode = -1 THEN
	F_ErrorBaseDatos(SQLCA, "Lectura del Procedimiento Almacenado genera_adhesivos_pallets" )			
ELSE
	FEtCH Codigo INTO :ls_respuesta;
END IF	
	
CLOSE Codigo;

RETURN ls_respuesta 

end function

public subroutine imprime_ventana ();Integer	li_fila

Open(w_emision_adhesivos_pallets)

w_emision_adhesivos_pallets.Visible				=	False
w_emision_adhesivos_pallets.em_copias.Text	=	'5'
w_emision_adhesivos_pallets.ii_procedencia	=	0

w_emision_adhesivos_pallets.dw_1.Retrieve(dw_3.Object.plde_codigo[dw_3.GetRow()], &
						 		 						dw_3.Object.clie_codigo[dw_3.GetRow()], &
														dw_3.Object.espe_codigo[dw_3.GetRow()], &
														1, -1)

li_fila	=	w_emision_adhesivos_pallets.dw_1.RowCount()
IF li_fila < 1 THEN 
	MessageBox("Error", "No es posible desplegar los pallets en existencia", StopSign!)
ELSE
	CHOOSE CASE MessageBox("Formato Impresión", "Si desea imprimir solo los datos, presione [SI]. ~r~n" + &
												  "Para imprimir el formato completo, presione [NO]", Question!, YesNoCancel!, 3)
		CASE 1
			w_emision_adhesivos_pallets.cbx_visible.Checked	=	False
			li_fila	=	w_emision_adhesivos_pallets.dw_1.Find("paen_numero = " + String(dw_3.Object.paen_numero[dw_3.GetRow()]), &
																				1, w_emision_adhesivos_pallets.dw_1.RowCount())
			IF li_fila > 0 THEN
				w_emision_adhesivos_pallets.dw_1.SelectRow(0, False)
				w_emision_adhesivos_pallets.dw_1.SelectRow(li_fila, True)
				w_emision_adhesivos_pallets.pb_imprimir.TriggerEvent(Clicked!)
			ELSE
				MessageBox("Error", "No es posible imprimir ventana de pallet ingresado", StopSign!)
			END IF
			
		CASE 2
			li_fila	=	w_emision_adhesivos_pallets.dw_1.Find("paen_numero = " + String(dw_3.Object.paen_numero[dw_3.GetRow()]), &
																				1, w_emision_adhesivos_pallets.dw_1.RowCount())
			IF li_fila > 0 THEN
				w_emision_adhesivos_pallets.dw_1.SelectRow(0, False)
				w_emision_adhesivos_pallets.dw_1.SelectRow(li_fila, True)
				w_emision_adhesivos_pallets.pb_imprimir.TriggerEvent(Clicked!)
			ELSE
				MessageBox("Error", "No es posible imprimir ventana de pallet ingresado", StopSign!)
			END IF
			
		CASE 3
			//No hace nada
			
	END CHOOSE
END IF
		
Close(w_emision_adhesivos_pallets)


end subroutine

public subroutine imprime_tarja (integer ai_formato);Integer	li_fila, li_nueva

//IF ai_formato = 1 THEN
//	dw_tarja.DataObject	=	"dw_info_tarja_pallet"
//	
//ELSE
//	dw_tarja.DataObject	=	"dw_info_tarja_pallet_solodato"
//	
//END IF
//
//dw_tarja.SetTransObject(sqlca)
//li_fila	=	dw_tarja.Retrieve(dw_3.Object.clie_codigo[dw_3.GetRow()], &
//										dw_3.Object.plde_codigo[dw_3.GetRow()], &
//										dw_3.Object.paen_numero[dw_3.GetRow()])
//										
//IF li_fila > 0 THEN
//	FOR li_fila = dw_tarja.RowCount() TO 10
//		li_nueva											=	dw_tarja.InsertRow(0)
//		dw_tarja.Object.paen_numero[li_nueva]	=	dw_3.Object.paen_numero[dw_3.GetRow()]
//		
//	NEXT
//	dw_tarja.Print(True, False)
//END IF
end subroutine

on w_maed_movtofrutaemba_proceso_cajasprod.create
int iCurrent
call super::create
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_caja=create dw_caja
this.dw_tarja=create dw_tarja
this.pb_tarjas=create pb_tarjas
this.pb_cambio_folio=create pb_cambio_folio
this.pb_ventanas=create pb_ventanas
this.dw_6=create dw_6
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_4
this.Control[iCurrent+2]=this.dw_5
this.Control[iCurrent+3]=this.dw_caja
this.Control[iCurrent+4]=this.dw_tarja
this.Control[iCurrent+5]=this.pb_tarjas
this.Control[iCurrent+6]=this.pb_cambio_folio
this.Control[iCurrent+7]=this.pb_ventanas
this.Control[iCurrent+8]=this.dw_6
this.Control[iCurrent+9]=this.dw_3
end on

on w_maed_movtofrutaemba_proceso_cajasprod.destroy
call super::destroy
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_caja)
destroy(this.dw_tarja)
destroy(this.pb_tarjas)
destroy(this.pb_cambio_folio)
destroy(this.pb_ventanas)
destroy(this.dw_6)
destroy(this.dw_3)
end on

event open;/*
Argumentos :	[1]	=>	Código de Exportador
					[2]	=>	Código de Planta
					[3]	=>	Tipo de Movimiento
					[4]	=>	Numero de Movimiento
					[5]	=>	Tipo de Proceso
					[6]	=>	Numero de Proceso
					[7]	=>	Folio Pallet
					[8]	=>	Código de Especie
					[9]	=>	Tipo de Envase
					[10]	=>	Código de Envase
*/

x												= 	0
y												= 	0
This.Height									= 	2520
im_menu										= 	m_principal
This.Icon										=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

iuo_Planta									=	Create uo_plantadesp
iuo_variedades								=	Create uo_variedades
iuo_categorias								=	Create uo_categorias
iuo_etiquetas								=	Create uo_etiquetas
iuo_destinos									=	Create uo_destinos
iuo_recibidores								=	Create uo_recibidores
iuo_tipopallet								=	Create uo_tipopallet
iuo_productores							=	Create uo_productores
iuo_Lote										=	Create uo_lotesfrutagranel
iuo_spro_ordenproceso					=	Create uo_spro_ordenproceso
iuo_spro_palletencab						=	Create uo_spro_palletencab
iuo_spro_movtofrutaembaenca			=	Create uo_spro_movtofrutaembaenca
iuo_camara									=	Create uo_camarasfrigo
iuo_FechaMovto							=  Create uo_fechaMovto		
iuo_pallet									=	Create uo_AnalizaPallet
iuo_copa										=	Create uo_valida_codigopallet

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_caja.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

dw_2.GetChild("clie_codigo", idwc_cliente)
dw_2.GetChild("plde_codigo", idwc_planta)
dw_2.GetChild("frio_tipofr", idwc_tipofrio)
dw_2.GetChild("pefr_codigo", idwc_periodofrio)
dw_2.GetChild("espe_codigo", idwc_especie)
dw_3.GetChild("vari_codigo", idwc_variedad)
dw_3.GetChild("enva_tipoen", idwc_tipoenvase)
dw_3.GetChild("enva_codigo", idwc_envase)
dw_3.GetChild("cate_codigo", idwc_categoria)
dw_3.GetChild("etiq_codigo", idwc_etiqueta)
dw_3.GetChild("dest_codigo", idwc_destino)
dw_3.GetChild("reci_codigo", idwc_recibidor)
dw_3.GetChild("tpen_codigo", idwc_tipopallet)
dw_3.GetChild("cama_codigo", idwc_camara)

idwc_cliente.SetTransObject(SqlCa)
idwc_planta.SetTransObject(SqlCa)
idwc_tipofrio.SetTransObject(SqlCa)
idwc_periodofrio.SetTransObject(SqlCa)
idwc_especie.SetTransObject(SqlCa)
idwc_variedad.SetTransObject(SqlCa)
idwc_tipoenvase.SetTransObject(SqlCa)
idwc_envase.SetTransObject(SqlCa)
idwc_categoria.SetTransObject(SqlCa)
idwc_etiqueta.SetTransObject(SqlCa)
idwc_destino.SetTransObject(SqlCa)
idwc_recibidor.SetTransObject(SqlCa)
idwc_tipopallet.SetTransObject(SqlCa)
idwc_camara.SetTransObject(SqlCa)
dw_4.SetTransObject(SqlCa)

idwc_cliente.Retrieve()
idwc_planta.Retrieve(gi_codexport)

IF idwc_tipofrio.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Tipo de Frío")
	idwc_tipofrio.InsertRow(0)
END IF

IF idwc_periodofrio.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Periodo de Frío")
	idwc_periodofrio.InsertRow(0)
END IF

IF idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención", "Falta Registrar Especie")
	idwc_especie.InsertRow(0)
END IF

idwc_variedad.InsertRow(0)

IF idwc_tipoenvase.Retrieve(0) = 0 THEN
	MessageBox("Atención", "Falta Registrar Tipo de Envase")
	idwc_tipoenvase.InsertRow(0)
ELSE
	idwc_tipoenvase.SetSort("tiem_nombre A")
	idwc_tipoenvase.Sort()
END IF

idwc_envase.InsertRow(0)

IF idwc_categoria.Retrieve(0) = 0 THEN
	MessageBox("Atención", "Falta Registrar Categoría")
	idwc_categoria.InsertRow(0)
ELSE
	idwc_categoria.SetSort("cate_nombre A")
	idwc_categoria.Sort()

	idwc_categoria.SetFilter("cate_embala = 1")
	idwc_categoria.Filter()
END IF

IF idwc_etiqueta.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención", "Falta Registrar Etiqueta")
	idwc_etiqueta.InsertRow(0)
ELSE
	idwc_etiqueta.SetSort("etiq_nombre A")
	idwc_etiqueta.Sort()
END IF

IF idwc_destino.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención", "Falta Registrar Destino")
	idwc_destino.InsertRow(0)
ELSE
	idwc_destino.SetSort("dest_nombre A")
	idwc_destino.Sort()
END IF

IF idwc_recibidor.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención", "Falta Registrar Recibidores")
	idwc_recibidor.InsertRow(0)
ELSE
	idwc_recibidor.SetSort("reci_nombre A")
	idwc_recibidor.Sort()
END IF

IF idwc_camara.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	idwc_camara.InsertRow(0)
ELSE
	idwc_camara.SetSort("cama_nombre A")
	idwc_camara.Sort()
END IF

dw_5.GetChild("espe_codigo", idwc_especie)
dw_3.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
ELSE
	idwc_especie.SetSort("espe_nombre A")
	idwc_especie.Sort()
END IF

idwc_tipopallet.InsertRow(0)
idwc_calibre.InsertRow(0)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, 	This.Title, "Acceso a Aplicación", 1)

istr_Mant.Argumento[1]	=	String(gi_CodExport)										// Cliente
istr_Mant.Argumento[2]	=	String(gstr_ParamPlanta.CodigoPlanta)				// Planta
istr_Mant.Argumento[3]	=	"4"															// Tipo Movto. (Recep. de Proceso)
istr_Mant.Argumento[5]	=	"4"															// Tipo de Proceso (Proceso)
buscar						=	"Embalaje:Semba_codigo,Categoría:Ncate_codigo"  // Orden
ordenar						= 	"Embalaje:emba_codigo,Categoría:cate_codigo"		// Busqueda
is_ultimacol 					= 	"lote_codigo"												// Ultima columna

end event

event ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta

DO
	dw_2.SetRedraw(False)
	//dw_2.Reset()
   	dw_5.Reset()
	dw_6.Reset()
	
	IF dw_2.Retrieve(Integer(istr_Mant.Argumento[2]), Integer(istr_Mant.Argumento[3]), &
						  Long(istr_Mant.Argumento[4]),Integer(istr_Mant.Argumento[1])) = -1 OR &
		dw_5.Retrieve(Integer(istr_Mant.Argumento[2]), Integer(istr_Mant.Argumento[3]), &
						  Long(istr_Mant.Argumento[4]),Integer(istr_Mant.Argumento[1])) = -1 OR &
		dw_6.Retrieve(Integer(istr_Mant.Argumento[2]), Integer(istr_Mant.Argumento[3]), &
						  Long(istr_Mant.Argumento[4]),Integer(istr_Mant.Argumento[1])) = -1 THEN 
						  
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSE

		IF dw_2.RowCount() < 1 THEN RETURN
		
		istr_Mant.Argumento[8]	=	String(dw_2.Object.espe_codigo[1])
		
		dw_3.GetChild("vari_codigo", idwc_variedad)
		idwc_variedad.SetTransObject(SqlCa)
		idwc_variedad.Retrieve(dw_2.Object.espe_codigo[1])
		idwc_variedad.SetSort("vari_nombre A")
		idwc_variedad.Sort()
		DO
			IF dw_1.Retrieve(Integer(istr_Mant.Argumento[2]), Integer(istr_Mant.Argumento[3]), &
								  Long(istr_Mant.Argumento[4]), dw_2.Object.clie_codigo[1]) = -1 THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_imprimir.Enabled	=	True
				
				IF dw_4.RowCount() > 0 THEN
				  pb_eliminar.Enabled	=	True
				  pb_grabar.Enabled		=	True	
				  pb_ins_det.Enabled		=	True
				  pb_eli_det.Enabled		=	True
			   END IF  
				
				HabilitaEncab(False)
					
				dw_1.SetRow(1)
				dw_1.SelectRow(1,True)
				
				dw_3.SetColumn("paen_numero")
				dw_3.SetFocus()
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo;Long		ll_modif1, ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
			ll_modif1	=	ll_modif1 + dw_2.GetNextModified(0, Primary!)
		   ll_modif1   =  ll_modif1 + dw_4.GetNextModified(0, Primary!)
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
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()
dw_caja.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False

pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
pb_ventanas.Enabled		=	False

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

lb_existe = FALSE

habilitaencab(true)

dw_3.GetChild("enva_codigo", idwc_envase)
idwc_envase.SetTransObject(SqlCa)
IF idwc_envase.Retrieve(-1) = 0 THEN
	MessageBox("Atención", "Falta Registrar Envase")
	idwc_envase.InsertRow(0)
ELSE
	idwc_envase.SetSort("enva_nombre A")
	idwc_envase.Sort()
END IF

dw_2.SetItem(1, "clie_codigo", Integer(istr_Mant.Argumento[1]))
dw_2.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))
dw_2.SetItem(1, "tpmv_codigo", Integer(istr_Mant.Argumento[3]))
dw_2.SetItem(1, "mfee_fecmov", DateTime(Today()))
dw_3.SetItem(1, "clie_codigo", Integer(istr_Mant.Argumento[1]))
dw_3.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))
dw_3.SetItem(1, "paen_feccon", DateTime(Today()))
dw_3.SetItem(1, "paen_fecemb", Date(Today()))
dw_3.SetItem(1, "cama_codigo", 0)

dw_3.SetItem(1, "paen_inspec", 0)
dw_3.SetItem(1, "paen_fumiga", 0)
dw_3.SetItem(1, "paen_estado", 1)
end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0, &
			li_Ancho = 300, li_Alto = 245, li_Siguiente = 245

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
ELSE
	dw_1.width	=	This.WorkSpaceWidth() - 600
	maximo		=	dw_1.width
END IF

dw_2.x					=	37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					=	37

dw_3.x					=	dw_2.x
dw_3.y					=	34 + dw_2.Height
dw_3.Width				=	dw_2.Width


dw_1.x					=	37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					=	34 + dw_2.Height + dw_3.Height 
dw_1.height				=	This.WorkSpaceHeight() - dw_1.y - 41

dw_4.x					=	dw_1.x
dw_4.y					=	dw_1.y
dw_4.Height				=	dw_1.Height
dw_4.Width				=	dw_1.Width


li_posic_x				=	This.WorkSpaceWidth() - 370
li_posic_y				=	30 

IF pb_buscar.Visible THEN
	pb_buscar.x				=	li_posic_x
	pb_buscar.y				=	li_posic_y
	pb_buscar.width		=	li_Ancho
	pb_buscar.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				=	li_posic_x
	pb_nuevo.y				=	li_posic_y
	pb_nuevo.width			=	li_Ancho
	pb_nuevo.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			=	li_posic_x
	pb_eliminar.y			=	li_posic_y
	pb_eliminar.width		=	li_Ancho
	pb_eliminar.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				=	li_posic_x
	pb_grabar.y				=	li_posic_y
	pb_grabar.width		=	li_Ancho
	pb_grabar.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			=	li_posic_x
	pb_imprimir.y			=	li_posic_y
	pb_imprimir.width		=	li_Ancho
	pb_imprimir.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_ventanas.Visible THEN
	pb_ventanas.x			=	li_posic_x
	pb_ventanas.y			=	li_posic_y
	pb_ventanas.width		=	li_Ancho
	pb_ventanas.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_cambio_folio.Visible THEN
	pb_cambio_folio.x			=	li_posic_x
	pb_cambio_folio.y			=	li_posic_y
	pb_cambio_folio.width	=	li_Ancho
	pb_cambio_folio.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_salir.Visible THEN
	pb_salir.x				=	li_posic_x
	pb_salir.y				=	li_posic_y
	pb_salir.width			=	li_Ancho
	pb_salir.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

pb_eli_det.x				=	li_posic_x
pb_eli_det.y				=	dw_1.y + dw_1.Height - li_Siguiente
pb_eli_det.width		=	li_Ancho
pb_eli_det.height		=	li_Alto

pb_ins_det.x			=	li_posic_x
pb_ins_det.y			=	pb_eli_det.y - li_Siguiente - 10
pb_ins_det.width		=	li_Ancho
pb_ins_det.height		=	li_Alto
end event

event ue_nuevo_detalle;str_mant	lstr_mant

dw_4.SetColumn("emba_codigo")

If ExisteCalibres() Then
	If il_fila > 0 Then
		If dw_4.RowCount()>0 Then
			If isnull(dw_4.Object.pafr_calibr[dw_4.getrow()]) Then
				MessageBox("Error de datos","Falta ingreso de calibre.")
				RETURN
			End If
		End If	
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	End If
	/**/
	iuo_tipopallet.existe_porembalaje(dw_3.Object.clie_codigo[1], is_embacodigo, dw_3.Object.tpen_codigo[1],FALSE,SQLCA)
							  
	iuo_destinos.Existe(dw_3.Object.dest_codigo[1],FALSE,SqlCa)

	If IsNull(is_embacodigo) OR is_embacodigo = "" AND dw_4.RowCount() > 0 Then
		is_embacodigo	=	dw_4.Object.emba_codigo[1]
	Else
		is_embacodigo	=	dw_3.Object.emba_codigo[1]
	End If
	
	lstr_mant.argumento[1]		=	String(dw_2.Object.plde_codigo[1])
	lstr_mant.argumento[2]		=	String(dw_3.Object.espe_codigo[1])
	lstr_mant.argumento[3]		=	String(dw_2.Object.mfee_docrel[1])
	lstr_mant.argumento[4]		=	String(dw_2.Object.clie_codigo[1])
	lstr_mant.argumento[5]		=	String(dw_2.Object.mfee_fecmov[1])
	lstr_mant.argumento[6]		=	String(iuo_tipopallet.Cajas)
	lstr_mant.argumento[7]		=	is_embacodigo
	lstr_mant.argumento[8]		=	String(dw_2.Object.mfee_tipdoc[1])
	lstr_mant.argumento[9]		=	String(dw_3.Object.paen_numero[1])
	lstr_mant.argumento[10]		=	String(iuo_destinos.Codigo)
	lstr_mant.argumento[11]		=	String(dw_3.Object.paen_tipopa[1])
	lstr_mant.argumento[12]		=	String(dw_3.Object.copa_codigo[1])
	lstr_mant.argumento[13]		=	String(dw_2.Object.vari_codigo[1])
	lstr_mant.argumento[14]		=	String(dw_3.Object.etiq_codigo[1])
	lstr_mant.argumento[15]		=	String(dw_3.Object.cate_codigo[1])
		
	If iuo_destinos.Codigo = 0 OR IsNull(iuo_destinos.Codigo) Then
		MessageBox("Error", "Debe seleccionar un País de Destino para este pallet.")
		Return
	End If
	
	lstr_mant.dw				=	dw_4
	
	OpenWithParm(iw_mantencion1, lstr_mant)
	
	dw_3.Object.paen_ccajas[1]	=	0
	
	For il_fila = 1 to dw_4.RowCount()		
		dw_3.Object.paen_ccajas[1] =	 dw_3.Object.paen_ccajas[1] + dw_4.Object.pafr_ccajas[il_fila]
		dw_4.SetItem(il_fila,"plde_origen",gstr_paramplanta.codigoplanta)
		dw_4.SetItem(il_fila,"espe_codigo",dw_2.Object.espe_codigo[1])
		dw_4.SetItem(il_fila,"cate_codigo",dw_3.Object.cate_codigo[1])
		dw_4.SetItem(il_fila,"pafr_catrot",dw_3.Object.cate_codigo[1])
		//dw_4.SetItem(il_fila,"emba_codigo", is_embacodigo)
		//dw_4.Object.emba_nombre[il_fila] = 	is_embalaje
	Next
	
	If Long(dw_3.Object.tpen_codigo[1]) = dw_3.Object.paen_ccajas[1] Then
		dw_3.Object.paen_tipopa[1]	=	1
	Else
		dw_3.Object.paen_tipopa[1]	=	2
	End If
	
	If dw_4.RowCount() > 0 Then pb_eli_det.Enabled = True
		
Else
	MessageBox("Atención", "Falta Registrar Calibres para Especie (" + & 
					String(dw_2.Object.espe_codigo[1], '00') + ") - Tipo de Envase (" + &
					String(dw_3.Object.enva_tipoen[1]) + ") - Envase (" + &
					String(dw_3.Object.enva_codigo[1], '000') + ")")
End If
end event

event ue_borra_detalle;Long ll_FilaBusca, ll_fila

IF lb_existe = TRUE AND dw_4.rowcount() < 2 THEN RETURN

il_fila	=	dw_4.GetRow()

IF NOT isnull(dw_4.Object.pafr_tipdoc[il_fila]) AND (dw_4.Object.pafr_tipdoc[il_fila] <> dw_2.Object.mfee_tipdoc[1]) THEN
	MessageBox("Error de Datos","El detalle de pallet no pertenece al Proceso seleccionado.")
	Message.DoubleParm = -1
	RETURN
END IF	

IF NOT isnull(dw_4.Object.pafr_docrel[il_fila]) AND (dw_4.Object.pafr_docrel[il_fila] <> dw_2.Object.mfee_docrel[1]) THEN
	MessageBox("Error de Datos","El detalle de pallet no pertenece al Proceso seleccionado.")
	Message.DoubleParm = -1
	RETURN
END IF	

ll_FilaBusca = 0

FOR ll_fila = 1 To dw_4.RowCount()
	IF ll_fila <> il_fila and Not isnull(dw_2.Object.mfee_numero[1]) THEN
		IF ((dw_4.Object.pafr_tipdoc[ll_fila] = dw_2.Object.mfee_tipdoc[1]) AND &
			(dw_4.Object.pafr_docrel[ll_fila] = dw_2.Object.mfee_docrel[1])) OR &
			isnull(dw_4.Object.pafr_docrel[ll_fila]) THEN

			ll_filabusca++
		END IF
	END IF
NEXT	

IF ll_filabusca = 0 THEN
	triggerevent("ue_borrar")
//	RETURN
END IF	

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0
//This.TriggerEvent ("ue_validaborrar")
IF dw_4.GetItemStatus(dw_4.getrow(),0,Primary!)<>NewModified! THEN
	IF dw_3.Object.paen_estado[1]>1 THEN
		MessageBox("Error de Datos","El pallet se encuentra despachado. No se puede eliminar detalle.")
		Message.DoubleParm = -1
	END IF
END IF	

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	ll_fila	=	dw_caja.RowCount()
	
	dw_caja.Retrieve(dw_4.Object.clie_codigo[dw_4.GetRow()], &
						  dw_4.Object.plde_codigo[dw_4.GetRow()],	&
						  dw_4.Object.pafr_secuen[dw_4.GetRow()])

	IF dw_4.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)

	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
		IF ll_fila = dw_4.RowCount() + 1 THEN
			dw_caja.RowsDiscard(ll_fila, ll_fila, Primary!)
		END IF
	END IF

 IF dw_4.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_4.GetRow()
	END IF
END IF

Captura_Totales()
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_Fila, ll_NumeroMovto, ll_NumeroPallet, ll_FilaMov, ll_Nuevo
Integer	li_Cliente, li_Planta, li_TipoMovto, li_Secuencia, li_cont, li_i, li_expocodigo, li_cliecod
Boolean  lb_detalle = FALSE
String   ls_mensaje, ls_colu[], ls_pallet

ib_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

li_Cliente				=	dw_3.Object.clie_codigo[1]
li_Planta				=	dw_2.Object.plde_codigo[1]
li_TipoMovto		=	dw_2.Object.tpmv_codigo[1]
ll_NumeroMovto	=	dw_2.Object.mfee_numero[1]
	
dw_3.Object.paen_ccajas[1]	=	dw_4.RowCount()

ll_Fila = 1

DO WHILE ll_Fila <= dw_4.RowCount()
	IF dw_4.GetItemStatus(ll_Fila, 0, Primary!) = New! THEN
		dw_4.DeleteRow(ll_Fila)
	ELSE
		ll_Fila ++
	END IF
LOOP

IF dw_4.RowCount() = 0 THEN
	Message.DoubleParm = -1
	RETURN
END IF	

For il_fila = 1 to dw_4.RowCount()		
	dw_3.Object.paen_ccajas[1]	=	 dw_4.RowCount()// dw_4.Object.pafr_ccajas[il_fila]//dw_3.Object.paen_ccajas[1] + dw_4.Object.pafr_ccajas[il_fila]
	dw_4.SetItem(il_fila,"plde_origen", gstr_paramplanta.codigoplanta)
	dw_4.SetItem(il_fila,"espe_codigo", dw_2.Object.espe_codigo[1])
	dw_4.SetItem(il_fila,"cate_codigo", dw_3.Object.cate_codigo[1])
	dw_4.SetItem(il_fila,"pafr_catrot", dw_3.Object.cate_codigo[1])
	dw_4.SetItem(il_fila,"pafr_copack", gi_Packing)	
//	dw_4.SetItem(il_fila,"emba_codigo", is_embacodigo)
//	dw_4.Object.emba_nombre[il_fila] = is_embalaje
NEXT
	
FOR ll_Fila=1 TO dw_4.RowCount()
	IF Isnull(dw_4.Object.emba_codigo[ll_fila]) OR dw_4.Object.emba_codigo[ll_fila] = "" THEN
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nEmbalaje"
		ls_colu[li_cont]	= "emba_codigo"
	END IF

	IF Isnull(dw_4.Object.pafr_calibr[ll_fila]) OR dw_4.Object.pafr_calibr[ll_fila] = "" THEN
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nCalibre"
		ls_colu[li_cont]	= "pafr_calibr"
	END IF
	
	IF Isnull(dw_4.Object.cate_codigo[ll_fila]) OR dw_4.Object.cate_codigo[ll_fila] = 0 THEN
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nCategoria"
		ls_colu[li_cont]	= "cate_codigo"
	END IF

	IF Isnull(dw_4.Object.pafr_ccajas[ll_fila]) OR dw_4.Object.pafr_ccajas[ll_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCantidad de Cajas"
		ls_colu[li_cont]	= "pafr_ccajas"
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "En la Fila : "+ String(ll_fila) + " Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
		dw_4.SetRow(ll_fila)
		dw_4.SetColumn(ls_colu[1])
		dw_4.SetFocus()
		Message.DoubleParm = -1
		RETURN
	END IF
NEXT

IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! AND NOT gstr_paramplanta.packing THEN
	INSERT INTO	dbo.spro_movtofrutaembaenca (plde_codigo, tpmv_codigo, mfee_numero)
		VALUES (:li_Planta, :li_tipomovto, 0) ;
	
	SELECT	IsNull(Max(mfee_numero), 0) + 1
		INTO	:ll_NumeroMovto
		FROM	dbo.spro_movtofrutaembaenca
		WHERE	plde_codigo	=	:li_Planta
		AND	tpmv_codigo	=	:li_TipoMovto ;

	dw_2.Object.mfee_numero[1]	=	ll_NumeroMovto
ELSEIF gstr_paramplanta.packing THEN
	dw_2.Object.mfee_numero[1]	=	dw_2.Object.mfee_docrel[1]
END IF

istr_Mant.Argumento[4]	=	String(dw_2.Object.mfee_numero[1])
istr_Mant.Argumento[7]	=	String(dw_3.Object.paen_numero[1])

ll_NumeroPallet			=	Long(istr_Mant.Argumento[7])

dw_3.Object.frio_tipofr[1]	=	dw_2.Object.frio_tipofr[1]
dw_3.Object.sepl_codigo[1]	=	1  //Servicio Proceso

SELECT	IsNull(Max(pafr_secuen), 0) + 1
	INTO	:li_Secuencia
	FROM	dbo.spro_palletfruta
	WHERE	clie_codigo	=	:li_Cliente
	AND	plde_codigo		=	:li_Planta
	AND	paen_numero	=	:ll_NumeroPallet ;
	
FOR ll_Fila = 1 TO dw_4.RowCount()
	IF dw_4.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		
		dw_4.Object.clie_codigo[ll_Fila]	=	dw_3.Object.clie_codigo[1]
		dw_4.Object.plde_codigo[ll_Fila]	=	dw_3.Object.plde_codigo[1]
		dw_4.Object.paen_numero[ll_Fila]=	dw_3.Object.paen_numero[1]
		dw_4.Object.espe_codigo[ll_Fila]	=	dw_3.Object.espe_codigo[1]
		dw_4.Object.vari_codrot[ll_Fila]	=	dw_3.Object.vari_codigo[1]
		dw_4.Object.etiq_codigo[ll_Fila]	=	dw_3.Object.etiq_codigo[1]
		dw_4.Object.prod_codigo[ll_Fila]	=	dw_2.Object.prod_codigo[1]
		dw_4.Object.prod_codrot[ll_Fila]	=	dw_3.Object.prod_codrot[1]
		dw_4.Object.plde_origen[ll_Fila]	=	dw_3.Object.plde_codigo[1]
		dw_4.Object.pafr_fecemb[ll_Fila] = 	dw_3.Object.paen_fecemb[1]
		dw_4.Object.pafr_tipdoc[ll_Fila]	=	dw_2.Object.mfee_tipdoc[1]
		dw_4.Object.pafr_docrel[ll_Fila]	=	dw_2.Object.mfee_docrel[1]
		dw_4.Object.pafr_fecemb[ll_Fila]	=	dw_2.Object.mfee_fecmov[1]
		dw_4.Object.frio_tipofr[ll_Fila]		=	dw_2.Object.frio_tipofr[1]
		dw_4.Object.pefr_codigo[ll_Fila]	=	dw_2.Object.pefr_codigo[1]
		dw_4.Object.vari_codigo[ll_Fila]	=	dw_2.Object.vari_codigo[1]
		dw_4.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
		dw_4.Object.mfee_numero[ll_Fila]=	dw_2.Object.mfee_numero[1]
		dw_4.Object.cocc_codigo[ll_Fila] =	1

		li_Secuencia ++
		lb_detalle = TRUE
	END IF
	FueraDeNorma(ll_Fila)
NEXT

IF lb_detalle THEN
	ll_FilaMov	=	dw_1.Find("clie_codigo = " + String(dw_3.Object.clie_codigo[1]) + &
									 " AND paen_numero = " + String(dw_3.Object.paen_numero[1]), 1, dw_1.RowCount())
	
	IF ll_FilaMov = 0 THEN
		ll_Nuevo	=	dw_1.InsertRow(0)
		dw_1.Object.plde_codigo[ll_Nuevo]	=	dw_2.Object.plde_codigo[1]
		dw_1.Object.tpmv_codigo[ll_Nuevo]	=	dw_2.Object.tpmv_codigo[1]
		dw_1.Object.mfee_numero[ll_Nuevo]	=	dw_2.Object.mfee_numero[1]
		dw_1.Object.clie_codigo[ll_Nuevo]		=	dw_2.Object.clie_codigo[1]
		dw_1.Object.paen_numero[ll_Nuevo]	=	dw_3.Object.paen_numero[1]
	END IF
END IF

li_cliecod 		= dw_2.GetItemNumber(dw_2.GetRow(), "clie_codigo")

FOR li_i = 1 to dw_4.RowCount()
	dw_4.Object.clie_codigo[li_i] = li_cliecod
NEXT

IF li_cliecod = GI_codExport THEN
	li_expocodigo = buscaexportador()
ELSE
	li_expocodigo = li_cliecod
END IF

FOR li_i = 1 to dw_1.RowCount()
	dw_1.Object.expo_codigo[li_i] = li_expocodigo
NEXT

FOR li_i = 1 to dw_2.RowCount()
	dw_2.Object.expo_codigo[li_i] = li_expocodigo
NEXT

//ls_pallet	=	String(dw_3.Object.clie_codigo[dw_3.GetRow()], '000') + &
//					String(dw_3.Object.paen_numero[dw_3.GetRow()], '000000')
//					
//IF iuo_pallet.analiza_datos(ls_pallet, SqlCa) THEN
//	dw_3.Object.paen_sscc18[1] =	iuo_pallet.CodBarra
//	
//END IF
//
//dw_3.Object.paen_gs1128[dw_3.GetRow()]	=	cargacodigo(dw_3.Object.clie_codigo[dw_3.GetRow()], &
//																		dw_3.Object.plde_codigo[dw_3.GetRow()], &
//																		dw_3.Object.paen_numero[dw_3.GetRow()], 1)

Habilitaencab(FALSE)

end event

event ue_guardar;Integer respuesta

IF dw_4.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")
Message.DoubleParm = 0
TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
	DO
	   dw_5.Reset()
		dw_6.Reset()
		IF dw_5.Retrieve(Integer(istr_Mant.Argumento[2]), &
							  Integer(istr_Mant.Argumento[3]), &
							  Long(istr_Mant.Argumento[4]),&
							  dw_2.Object.clie_codigo[1]) = -1 OR &
			dw_6.Retrieve(Integer(istr_Mant.Argumento[2]), &
							  Integer(istr_Mant.Argumento[3]), &
							  Long(istr_Mant.Argumento[4]),&
							  dw_2.Object.clie_codigo[1]) = -1 THEN 
							  
			respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
		END IF
	LOOP While respuesta = 	1
	IF respuesta = 2 Then Close(this)
	dw_3.SetFocus()
	dw_3.SetColumn("paen_numero")
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_seleccion;call super::ue_seleccion;Str_Busqueda	lstr_Busq

lstr_Busq.Argum[1] = istr_Mant.Argumento[1]		// Código de Cliente
lstr_Busq.Argum[2] = istr_Mant.Argumento[2]		// Código de Planta
lstr_Busq.Argum[3] = istr_Mant.Argumento[3]		// Tipo de Movimiento
lstr_busq.Argum[20] = string(BuscaExportador())

OpenWithParm(w_busc_recfruprocee, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	istr_Mant.Argumento[4]	=	lstr_Busq.Argum[5]
	istr_Mant.Argumento[5]	=	lstr_Busq.Argum[7]
	istr_Mant.Argumento[6]	=	lstr_Busq.Argum[8]
	
	dw_3.Object.paen_numero.Protect	=	0
	dw_3.Object.paen_numero.Color 		= 0
	dw_3.Object.paen_numero.BackGround.Color = RGB(255,255,255)
	
	IF ExistePallet(Integer(istr_Mant.Argumento[5]), Long(istr_Mant.Argumento[6])) THEN
		This.TriggerEvent("ue_recuperadatos")
	END IF
END IF
end event

event closequery;IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			Message.ReturnValue = 1 
		CASE 0
			IF dw_4.RowCount() > 0 THEN
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

event ue_imprimir;Long		fila

SetPointer(HourGlass!)

istr_info.titulo	= "RECEPCION DE PROCESO"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_revision_proceso_embalada_compac"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(dw_2.Object.plde_codigo[1], dw_2.Object.mfee_tipdoc[1], &
								  dw_2.Object.mfee_docrel[1], dw_2.Object.clie_codigo[1])

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

event ue_borrar;Long	ll_Fila 

IF dw_2.RowCount() < 1 THEN RETURN

//IF isnull(dw_2.Object.mfee_numero[1]) THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

For ll_Fila = 1 To dw_4.RowCount()
	dw_caja.Retrieve(dw_4.Object.clie_codigo[ll_Fila], dw_4.Object.plde_codigo[ll_Fila], dw_4.Object.pafr_secuen[ll_Fila])
	IF dw_Caja.RowCount() > 0 THEN dw_Caja.RowsMove(1,dw_Caja.RowCount(),Primary!,dw_Caja,1,Delete!)
Next 

IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)
IF dw_4.RowCount() > 0 THEN dw_4.RowsMove(1,dw_4.RowCount(),Primary!,dw_4,1,Delete!)

IF Message.DoubleParm = -2 THEN
	w_main.SetMicroHelp("Borrando Registro...")
	IF wf_actualiza_db(True) THEN
		w_main.SetMicroHelp("Registro Borrado...")
		dw_3.Reset()
		dw_3.InsertRow(0)
		dw_4.Reset()
		dw_3.SetItem(1, "clie_codigo", Integer(istr_Mant.Argumento[1]))
		dw_3.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))
		dw_3.SetItem(1, "paen_feccon", DateTime(Today()))
		dw_3.SetItem(1, "paen_inspec", 0)
		dw_3.SetItem(1, "paen_fumiga", 0)
		dw_3.SetItem(1, "paen_estado", 1)
		dw_3.GetChild("vari_codigo", idwc_variedad)
		idwc_variedad.SetTransObject(SqlCa)
		idwc_variedad.Retrieve(iuo_spro_ordenproceso.Especie)
		idwc_variedad.SetSort("vari_nombre A")
		idwc_variedad.Sort()
		HabilitaPallet(TRUE)
		SetPointer(Arrow!)
	ELSE
		w_main.SetMicroHelp("Registro no Borrado...")
	END IF			
ELSE		
	IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF
END IF	
end event

event ue_validaborrar();LONG ll_pallet, ll_fila

IF MessageBox("Borrar Registro","Desea Borrar la Información?.~r~r" + &
              "Se eliminará detalle perteneciente al pallet y al Número de Proceso.~r" + &
				  "Si no queda información se borrará el pallet y el movimiento.", Question!, YesNo!) = 1 THEN
				  
	IF dw_3.Object.paen_estado[1] > 1 THEN
		MessageBox("Error de Datos","El pallet fue despachado. No puede eliminarlo.")
		Message.Doubleparm = -1
		RETURN
	END IF
	
	IF dw_1.RowCount() > 0 THEN
		Borradetalles()
		IF dw_1.RowCount() = 0 THEN
			Message.Doubleparm = -3
		ELSEIF dw_1.RowCount() > 0 THEN
			Message.Doubleparm = -2
		END IF	
		RETURN
	END IF
	
ELSE
	Message.DoubleParm = -1
	RETURN
END IF

	




end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutaemba_proceso_cajasprod
boolean visible = false
integer x = 18
integer y = 2428
integer width = 3456
integer height = 872
string title = "Detalle de Movimiento"
string dataobject = "dw_mues_movtofrutaembadeta"
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutaemba_proceso_cajasprod
integer x = 329
integer y = 36
integer width = 3067
integer height = 432
integer taborder = 10
string dataobject = "dw_mant_movtofrutaembaenca_proceso"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Null, ls_Fecha
Date     ld_fecha
Datetime ld_fechapru, ldt_fecha
string ls_cliente
SetNull(ls_Null)

ls_Columna = dwo.name
ls_cliente = string(This.Object.clie_codigo[Row])

CHOOSE CASE ls_Columna
	CASE "clie_codigo"
		If NoExisteCliente(Integer(data)) Then
			This.SetItem(row,"clie_codigo",Integer(ls_Null))
			Return 1
		Else
			istr_Mant.Argumento[1]	= Data
		   dw_3.SetItem(1, "clie_codigo", Integer(Data))
		End If	
		
	CASE "mfee_tipdoc"
		istr_Mant.Argumento[5]	= Data
		This.SetItem(1, "mfee_docrel", Long(ls_Null))
 		This.SetItem(1, "prod_nombre", Integer(ls_Null))
		This.SetItem(1, "frio_tipofr", ls_Null)
		This.SetItem(1, "pefr_codigo", Integer(ls_Null))
		This.SetItem(1, "espe_codigo", Integer(ls_Null))
		This.SetItem(1, "vari_nombre", ls_Null)

	CASE "mfee_docrel"
		If iuo_spro_ordenproceso.Existe(Integer(istr_Mant.Argumento[2]), This.Object.mfee_tipdoc[1], &
												  Long(Data),True,SqlCa, Integer(istr_Mant.Argumento[1])) Then
												  
			If iuo_spro_ordenproceso.Estado = 5 Or iuo_spro_ordenproceso.Estado = 3 Then
				MessageBox('Alerta', 'Orden de proceso cerrada...', StopSign!, OK!)				
				This.SetItem(1, "prod_nombre", ls_Null)
				This.SetItem(1, "frio_tipofr", ls_Null)
				This.SetItem(1, "pefr_codigo", Integer(ls_Null))
				This.SetItem(1, "espe_codigo", Integer(ls_Null))
				This.SetItem(1, "vari_nombre", ls_Null)
				This.SetItem(1, ls_Columna, Long(ls_Null))
				Return 1
			End If
			
			istr_Mant.Argumento[6]	= Data
			istr_Mant.Argumento[8]	= String(iuo_spro_ordenproceso.Especie)
			
			This.SetItem(1, "mfee_fecmov", iuo_spro_ordenproceso.FechaOrden)
			
			iuo_spro_ordenproceso.CapturaReferencias(1,0,1,0,0,False,Sqlca)
			
			this.SetItem(1, "prod_codigo", iuo_spro_ordenproceso.Productor)
			This.SetItem(1, "prod_nombre", iuo_spro_ordenproceso.NombreProductor)
			This.SetItem(1, "frio_tipofr", iuo_spro_ordenproceso.TipoFrio)
			This.SetItem(1, "pefr_codigo", iuo_spro_ordenproceso.PeriodoFrio)
			This.SetItem(1, "espe_codigo", iuo_spro_ordenproceso.Especie)
			This.SetItem(1, "vari_nombre", iuo_spro_ordenproceso.NombreVariedad)
			This.SetItem(1, "vari_codigo", iuo_spro_ordenproceso.variedad)
			This.SetItem(1, "frre_codigo",1)
			dw_3.SetItem(1, "espe_codigo", iuo_spro_ordenproceso.Especie)
			dw_3.SetItem(1, "frio_tipofr", iuo_spro_ordenproceso.TipoFrio)
			dw_3.SetItem(1, "vari_codigo", iuo_spro_ordenproceso.Variedad)
			dw_3.SetItem(1, "prod_codrot", iuo_spro_ordenproceso.Productor)
			
			If iuo_productores.Existe(iuo_spro_ordenproceso.Productor,False,SqlCa) Then
				dw_3.SetItem(1, "prod_nomrot", iuo_productores.Nombre)
				dw_2.SetItem(1, "clie_codigo", long(ls_cliente))
				dw_3.SetItem(1, "clie_codigo", long(ls_cliente))
				istr_Mant.Argumento[1]	= ls_cliente
			End If

			dw_3.Object.paen_numero.Protect	=	0
			dw_3.Object.paen_numero.BackGround.Color = RGB(255,255,255)

			dw_3.GetChild("vari_codigo", idwc_variedad)
			idwc_variedad.SetTransObject(SqlCa)
			idwc_variedad.Retrieve(iuo_spro_ordenproceso.Especie)
			idwc_variedad.SetSort("vari_nombre A")
			idwc_variedad.Sort()
			
//			If iuo_spro_movtofrutaembaenca.ExisteDoctoRelacionado(&
//											Integer(istr_Mant.Argumento[2]), &
//											This.Object.mfee_tipdoc[1], &
//											Long(Data), 1,SqlCa) Then
			If iuo_spro_movtofrutaembaenca.ExisteDoctoRelacionado(&
											Integer(istr_Mant.Argumento[2]), &
											This.Object.mfee_tipdoc[1], &
											Long(Data), SqlCa,&
											Integer(istr_Mant.Argumento[1]),  This.Object.mfee_tipdoc[1]) Then
				istr_Mant.Argumento[3]	=	String(iuo_spro_movtofrutaembaenca.TipoMovto)
				istr_Mant.Argumento[4]	=	String(iuo_spro_movtofrutaembaenca.NumeroMovto)
				Parent.TriggerEvent("ue_recuperadatos")
			End If
		Else
			This.SetItem(1, "prod_nombre", ls_Null)
			This.SetItem(1, "frio_tipofr", ls_Null)
			This.SetItem(1, "pefr_codigo", Integer(ls_Null))
			This.SetItem(1, "espe_codigo", Integer(ls_Null))
			This.SetItem(1, "vari_nombre", ls_Null)
			This.SetItem(1, ls_Columna, Long(ls_Null))
			Return 1
		End If

	CASE "mfee_numero"
		If ExisteMovimiento(Long(Data),Integer(istr_Mant.Argumento[1])) Then
			istr_Mant.Argumento[4]	=	Data
			If ExistePallet(Integer(istr_Mant.Argumento[5]), &
								 Long(istr_Mant.Argumento[6])) Then

				Parent.TriggerEvent("ue_recuperadatos")
				
				If iuo_spro_ordenproceso.Existe(Integer(istr_Mant.Argumento[2]), &
												                This.Object.mfee_tipdoc[1], &
												                This.Object.mfee_docrel[1],True,SqlCa,&
																	 Integer(istr_Mant.Argumento[1])) Then
					istr_Mant.Argumento[6]	= String(This.Object.mfee_docrel[1])
					istr_Mant.Argumento[8]	= String(iuo_spro_ordenproceso.Especie)
								
					iuo_spro_ordenproceso.CapturaReferencias(1,0,1,0,0,False,Sqlca)
			
					dw_3.SetItem(1, "espe_codigo", iuo_spro_ordenproceso.Especie)
					dw_3.SetItem(1, "frio_tipofr", iuo_spro_ordenproceso.TipoFrio)
					dw_3.SetItem(1, "vari_codigo", iuo_spro_ordenproceso.Variedad)
					dw_3.SetItem(1, "prod_codrot", iuo_spro_ordenproceso.Productor)
			
					If iuo_productores.Existe(iuo_spro_ordenproceso.Productor,False,SqlCa) Then
						dw_3.SetItem(1, "prod_nomrot", iuo_productores.Nombre)
						dw_3.SetItem(1, "clie_codigo", long(ls_cliente))
						istr_Mant.Argumento[1]	= ls_cliente
					End If
				End If	
			End If
		Else
			This.SetItem(1, ls_Columna, Long(ls_Null))
			Return 1
		End If

	CASE "mfee_fecmov"
		ld_fechapru = Datetime(dw_2.Object.mfee_fecmov[1])
		If isnull(data) Then
			MessageBox("Atención","Ingrese una fecha valida.")
			This.SetItem(1, ls_Columna, ld_Fechapru)
			Return 1
		End If	
		If Date(Mid(data,1,10)) = ld_fecha Then
			MessageBox("Atención","Ingrese una fecha valida.")
			This.SetItem(1, ls_Columna, ld_fechapru)
			Return 1
		Else
			ldt_Fecha	=	DateTime(Date(Mid(data,1,10)))
			If NOT iuo_FechaMovto.Valida_FechaMovto(Date(ldt_Fecha)) Then
				This.SetItem(1, ls_Columna, ld_fechapru)
				This.SetFocus()
				Return 1
			End If
			This.SetItem(1, ls_Columna, DateTime(Date(Mid(Data,1,10))))
		End If	
End CHOOSE

HabilitaIngreso()
end event

event dw_2::itemerror;call super::itemerror;RETURN 1
end event

event dw_2::buttonclicked;call super::buttonclicked;String	ls_Boton

ls_Boton = dwo.name

Choose Case ls_Boton
	Case "b_ordenproceso"
		BuscaOrdenProceso()

End Choose
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutaemba_proceso_cajasprod
integer x = 3991
integer y = 292
integer taborder = 90
long backcolor = 553648127
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutaemba_proceso_cajasprod
integer x = 3991
integer y = 476
integer taborder = 100
long backcolor = 553648127
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutaemba_proceso_cajasprod
integer x = 3991
integer y = 660
integer taborder = 60
long backcolor = 553648127
end type

event pb_grabar::clicked;lb_nuevopallet	=	True
Parent.TriggerEvent("ue_guardar")
end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutaemba_proceso_cajasprod
integer x = 3991
integer y = 844
integer taborder = 70
long backcolor = 553648127
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutaemba_proceso_cajasprod
integer x = 3991
integer y = 1028
integer taborder = 110
long backcolor = 553648127
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutaemba_proceso_cajasprod
integer x = 3991
integer y = 1648
integer taborder = 40
long backcolor = 553648127
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutaemba_proceso_cajasprod
integer x = 3991
integer y = 1824
integer taborder = 50
long backcolor = 553648127
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutaemba_proceso_cajasprod
integer x = 3991
integer y = 108
integer taborder = 80
long backcolor = 553648127
end type

type dw_4 from uo_dw within w_maed_movtofrutaemba_proceso_cajasprod
event uo_nomover pbm_syscommand
integer x = 210
integer y = 1144
integer width = 3461
integer height = 1104
integer taborder = 30
boolean titlebar = true
string title = "Detalle de Pallet"
string dataobject = "dw_mues_palletfruta_proceso_cajasprod"
boolean hscrollbar = true
boolean livescroll = true
end type

event uo_nomover;uint wParam, lParam

wParam = Message.WordParm

Choose Case wParam
	Case 61456, 61458
	     Message.Processed = True
	     Message.ReturnValue = 0

End Choose
end event

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!
		Message.DoubleParm = 0
		
		Parent.TriggerEvent("ue_validaregistro")
		
		IF Message.DoubleParm = -1 THEN
			This.SetRedraw(True)
			RETURN -1
		ELSEIF Key = KeyDownArrow! AND il_fila = dw_1.RowCount() THEN
			Parent.TriggerEvent("ue_nuevo_detalle")
		END IF
		
	CASE KeyTab!
		IF is_ultimacol = This.GetColumnName() AND il_fila = dw_1.RowCount() THEN
			Message.DoubleParm = 0
			
			Parent.TriggerEvent("ue_validaregistro")
			
			IF Message.DoubleParm = -1 THEN
				This.SetRedraw(True)
				RETURN -1
			ELSE
				Parent.TriggerEvent("ue_nuevo_detalle")
				
				This.SetFocus()
				This.SetRedraw(True)
				RETURN -1
			END IF
		END IF

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event itemerror;call super::itemerror;RETURN 1
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;call super::losefocus;AcceptText()
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);call super::ue_seteafila;il_fila	= This.GetRow()

RETURN 1
end event

event itemchanged;call super::itemchanged;String	ls_Columna, ls_Nula, ls_embalaje, ls_calibre
Integer	li_Planta, li_Lote, li_Cajas, li_categoria, li_ccaj, li_loteing

ls_Columna = dwo.Name

SetNull(ls_Nula)

CHOOSE CASE ls_Columna
	CASE "emba_codigo"
		ls_embalaje = dw_4.Object.emba_codigo[row]
		IF isnull(dw_4.Object.pafr_docrel[row]) OR (dw_4.Object.pafr_docrel[row] = dw_2.Object.mfee_docrel[1] AND &
		    dw_4.Object.pafr_tipdoc[row] = dw_2.Object.mfee_tipdoc[1])THEN
			IF Not ExisteEmbalaje(Integer(istr_Mant.Argumento[9]),  &
										 Integer(istr_Mant.Argumento[10]), &
										 dw_3.Object.etiq_codigo[1],Data) THEN
				This.SetItem(row, ls_Columna, ls_Nula)
				This.SetItem(row, "emba_nombre", ls_Nula)
				RETURN 1
			ELSE
				This.SetItem(row, "emba_nombre", is_embalaje)
			END IF
		ELSE	
			MessageBox("Atención","El registro pertenece a otra Orden de Proceso. Imposible Modificar.")
			This.SetItem(row, ls_Columna, ls_Embalaje)
			RETURN 1
			
		END IF
		
	CASE "cate_codigo"
		li_categoria = dw_4.Object.cate_codigo[row]
		IF isnull(dw_4.Object.pafr_docrel[row]) OR (dw_4.Object.pafr_docrel[row] = dw_2.Object.mfee_docrel[1] AND &
		    dw_4.Object.pafr_tipdoc[row] = dw_2.Object.mfee_tipdoc[1])THEN
			IF Not iuo_categorias.Existe(Integer(Data),True,SqlCa) THEN
				This.SetItem(row, ls_Columna, Integer(ls_Nula))
				RETURN 1
			END IF
		ELSE	
			MessageBox("Atención","El registro pertenece a otra Orden de Proceso. Imposible Modificar.")
			This.SetItem(row, ls_Columna, li_categoria)
			RETURN 1
		END IF
		
	CASE "pafr_calibr"
		ls_Calibre	=	dw_4.Object.pafr_calibr[row]
		Data			=	Mid(Data + "   ", 1, 3)
		
		IF IsNull(dw_4.Object.pafr_docrel[row]) OR (dw_4.Object.pafr_docrel[row] = dw_2.Object.mfee_docrel[1] AND &
		    dw_4.Object.pafr_tipdoc[row] = dw_2.Object.mfee_tipdoc[1])THEN
			IF Not ExisteCalibre(Integer(istr_Mant.Argumento[8]), &
								  		Integer(istr_Mant.Argumento[9]), &
								  		Integer(istr_Mant.Argumento[10]), &
										Data, istr_calibre) THEN
				This.SetItem(row, ls_Columna, ls_Nula)
				RETURN 1
			END IF
		ELSE	
			MessageBox("Atención","El registro pertenece a otra Orden de Proceso. Imposible Modificar.")
			This.SetItem(row, ls_Columna, ls_calibre)
			RETURN 1
		END IF
		
	CASE "pafr_ccajas"
		li_ccaj = This.Object.pafr_ccajas[row]
		IF isnull(dw_4.Object.pafr_docrel[row]) OR (dw_4.Object.pafr_docrel[row] = dw_2.Object.mfee_docrel[1] AND &
		    dw_4.Object.pafr_tipdoc[row] = dw_2.Object.mfee_tipdoc[1])THEN
			
			IF IsNull(This.Object.pafr_ccajas[row]) THEN
				li_Cajas	=	0
			ELSE
				li_Cajas	=	This.GetItemNumber(row, "pafr_ccajas")
			END IF
			
			li_Cajas	=	This.GetItemNumber(1, "total_cajas") - &
							li_Cajas + Integer(Data)
			
			IF li_Cajas > iuo_tipopallet.Cajas THEN
				MessageBox("Atención", "Cantidad de Cajas Total sobrepasa la~r" + &
								"Cantidad de Cajas del Pallet (" + &
								Trim(String(iuo_tipopallet.Cajas, "#,##0")) + &
								").~r~rRevise cantidades registradas.")
				This.SetItem(row, ls_Columna, Integer(ls_Nula))
				
				RETURN 1
			ELSEIF li_Cajas = iuo_tipopallet.Cajas THEN
				dw_3.Object.paen_tipopa[1] = 1
			ELSE
				dw_3.Object.paen_tipopa[1] = 2
			END IF
		ELSE	
			MessageBox("Atención","El registro pertenece a otra Orden de Proceso. Imposible Modificar.")
			This.SetItem(row, ls_Columna, li_ccaj)
			RETURN 1
		END IF
		
	CASE "lote_codigo"
		li_loteing = dw_4.Object.lote_codigo[row]
		IF isnull(dw_4.Object.pafr_docrel[row]) OR (dw_4.Object.pafr_docrel[row] = dw_2.Object.mfee_docrel[1] AND &
		    dw_4.Object.pafr_tipdoc[row] = dw_2.Object.mfee_tipdoc[1])THEN
			IF Not iuo_Lote.Existe(This.Object.plde_origen[row], &
						Integer(istr_Mant.Argumento[8]), Integer(Data), &
						True, Sqlca) &
				OR Not iuo_Lote.ExisteOrdenProceso(Integer(istr_Mant.Argumento[2]), &
						Integer(istr_Mant.Argumento[5]), Integer(istr_Mant.Argumento[6]), &
						This.Object.plde_origen[row], Integer(istr_Mant.Argumento[8]), &
						Integer(Data), True, Sqlca) THEN
				This.SetItem(row, ls_Columna, Integer(ls_Nula))
				
				RETURN 1
			END IF
		ELSE	
			MessageBox("Atención","El registro pertenece a otra Orden de Proceso. Imposible Modificar.")
			This.SetItem(row, ls_Columna, li_loteing)
			RETURN 1
		END IF
		
END CHOOSE

Captura_Totales()
end event

event buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.name
		
	CASE "b_embalaje"
		IF isnull(dw_4.Object.pafr_docrel[row]) OR (dw_4.Object.pafr_docrel[row] = dw_2.Object.mfee_docrel[1] AND &
		    dw_4.Object.pafr_tipdoc[row] = dw_2.Object.mfee_tipdoc[1])THEN
			BuscaEmbalaje()
		ELSE
			MessageBox("Atención","El registro pertenece a otra Orden de Proceso. Imposible Modificar.")
		END IF	

END CHOOSE
end event

type dw_5 from datawindow within w_maed_movtofrutaemba_proceso_cajasprod
string tag = "Muestra todos los Pallet del Movimiento"
boolean visible = false
integer x = 3593
integer y = 220
integer width = 183
integer height = 132
integer taborder = 120
boolean bringtotop = true
string title = "Muestra todos los Pallet del Movimiento"
string dataobject = "dw_mant_mues_palletencab_eliminacion"
end type

type dw_caja from datawindow within w_maed_movtofrutaemba_proceso_cajasprod
boolean visible = false
integer x = 3776
integer y = 236
integer width = 183
integer height = 124
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dw_caja_corta_elimina"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event retrievestart;Return 2
end event

type dw_tarja from datawindow within w_maed_movtofrutaemba_proceso_cajasprod
boolean visible = false
integer x = 3771
integer y = 96
integer width = 183
integer height = 132
integer taborder = 90
boolean bringtotop = true
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type pb_tarjas from picturebutton within w_maed_movtofrutaemba_proceso_cajasprod
boolean visible = false
integer x = 14
integer y = 12
integer width = 155
integer height = 132
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo\Bmp\ADHESIVOSE.bmp"
string disabledname = "\Desarrollo\Bmp\ADHESIVOSD.bmp"
alignment htextalign = left!
end type

event clicked;Integer	li_fila

Imprime_Ventana()
end event

type pb_cambio_folio from picturebutton within w_maed_movtofrutaemba_proceso_cajasprod
string tag = "Cambio de Folio Pallet"
integer x = 3991
integer y = 1396
integer width = 302
integer height = 244
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "FullBuild!"
string disabledname = "FullBuild!"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Parent.TriggerEvent("ue_cambio_folio")
end event

type pb_ventanas from uo_botonventanas within w_maed_movtofrutaemba_proceso_cajasprod
integer x = 3991
integer y = 1212
integer width = 302
integer height = 244
integer taborder = 80
boolean bringtotop = true
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Adhesivo.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Adhesivo-bn.png"
long backcolor = 553648127
end type

type dw_6 from datawindow within w_maed_movtofrutaemba_proceso_cajasprod
string tag = "Muestra todos los Detalle de Pallet del Movimiento"
boolean visible = false
integer x = 3593
integer y = 96
integer width = 183
integer height = 124
integer taborder = 130
string title = "Muestra todos los Detalle de Pallet del Movimiento"
string dataobject = "dw_mant_mues_palletfruta_eliminacion"
end type

type dw_3 from uo_dw within w_maed_movtofrutaemba_proceso_cajasprod
integer x = 334
integer y = 472
integer width = 3104
integer height = 648
integer taborder = 20
string dataobject = "dw_mant_palletencab_proceso"
boolean vscrollbar = false
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna, ls_Null, ls_Fecha
Date     ld_fecha
Datetime ld_fechapru, ldt_fecha
Integer	li_cliente
SetNull(ls_Null)

ls_Columna = dwo.name

CHOOSE CASE ls_Columna
	CASE "emba_codigo"
		If Not cargaembalaje(data) Then
			This.SetItem(1,'emba_codigo',ls_Null)
			Return 1		
		Else		
			This.Object.enva_tipoen.Color = RGB(255,255,255)
			This.Object.enva_codigo.Color = RGB(255,255,255)
			
			This.Object.enva_tipoen[row] = ii_envatipoen
			This.GetChild("enva_codigo", idwc_envase)
			idwc_envase.SetTransObject(SqlCa)
			idwc_envase.Retrieve(ii_envatipoen)
			istr_Mant.Argumento[9] = STRING(ii_envatipoen)
			
			THIS.Object.enva_codigo[row] = ii_envacodigo
			THIS.SetColumn("paen_tipopa")
			THIS.SetFocus()
			istr_Mant.Argumento[10] = STRING(ii_envacodigo)

			This.SetItem(1, "tpen_codigo", ls_Null)

			dw_3.GetChild("tpen_codigo", idwc_tipopallet)
			idwc_tipopallet.SetTransObject(SqlCa)
			
			li_cliente = Integer(istr_Mant.Argumento[1])
		 	idwc_tipopallet.Retrieve(li_cliente,data)
			is_embacodigo	=	data
		End If		
		
	CASE "paen_numero"
		pb_eli_det.enabled 	= FALSE
		pb_ins_det.enabled 	= FALSE
		pb_ventanas.Enabled 	= FALSE
		
		If isnull(dw_2.Object.mfee_docrel[1]) Then
			MessageBox("Atención","Necesita Ingresar un movimiento o una Orden de Proceso.")
        		This.SetItem(1, ls_Columna, long(ls_Null))
			Return 1	
		Else	
			If iuo_spro_palletencab.Existe(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), &
													 Long(Data),False,SqlCa) Then
				
					If iuo_spro_palletencab.Especie	= dw_2.Object.espe_codigo[1] Then
						istr_Mant.Argumento[7]	=	Data
						Parent.TriggerEvent("ue_recuperapallet")
						dw_3.Object.prod_codrot[1] = iuo_spro_ordenproceso.Productor
						If iuo_productores.Existe(iuo_spro_ordenproceso.Productor, False, SQLCA) Then
							This.SetItem(Row, "prod_nomrot", iuo_productores.Nombre)
						End If
						lb_existe = TRUE
					Else
						lb_existe = FALSE
						MessageBox("Error de Datos","El pallet ingresado pertenece a otra especie.")
						this.SetItem(1,"paen_numero",long(ls_Null))
						Return 1
					End If
			Else
				lb_existe = FALSE
				istr_Mant.Argumento[7]	=	Data
				dw_4.Reset()
				dw_3.Reset()
				dw_3.InsertRow(0)
				dw_4.Enabled = TRUE
				This.SetItem(1, "paen_numero", Long(istr_Mant.Argumento[7]))
				Limpia_Pallet()
				HabilitaPallet(True)
				
				dw_3.GetChild("vari_codigo", idwc_variedad)
				idwc_variedad.SetTransObject(sqlca)
				If idwc_variedad.Retrieve(dw_2.Object.espe_codigo[1]) = 0 Then
					MessageBox("Atención","Falta Registrar Variedades")
				idwc_variedad.InsertRow(0)
				Else
					idwc_variedad.SetSort("vari_nombre A")
					idwc_variedad.Sort()
				End If
				
				dw_3.Object.espe_codigo[1] 	=	dw_2.Object.espe_codigo[1]
				
				If iuo_productores.Existe(iuo_spro_ordenproceso.Productor,False,SqlCa) Then
					This.SetItem(1, "prod_nomrot", iuo_productores.Nombre)
				End If
				dw_3.SetItem(1, "paen_fecemb", Date(Today()))
				
			End If
		End If
		
	CASE "paen_feccon"
		ld_fechapru = datetime(dw_3.Object.paen_feccon[row])
		If isnull(data) Then
			MessageBox("Atención","Ingrese una fecha valida.")
			This.SetItem(row, ls_Columna, ld_Fechapru)
			Return 1
		End If	
		
		If Date(Mid(data,1,10)) = ld_fecha Then
			MessageBox("Atención","Ingrese una fecha valida.")
			This.SetItem(row, ls_Columna, ld_Fechapru)
			Return 1
		Else
			ldt_Fecha	=	DateTime(Date(Mid(data,1,10)))
			If NOT iuo_FechaMovto.Valida_FechaMovto(Date(ldt_Fecha)) Then
				This.SetItem(row, ls_Columna, ld_Fechapru)
				This.SetFocus()
				Return 1
			End If
					
			ls_Fecha	=	Data
			This.SetItem(row, ls_Columna, DateTime(Date(Mid(ls_Fecha,1,10))))
			
		End If	

	CASE "vari_codigo"
		If NOT IsNull(dw_2.Object.espe_codigo[1]) Then
			If NOT iuo_variedades.Existe(dw_2.Object.espe_codigo[1],Integer(Data),True,SqlCa) Then
				This.SetItem(1,ls_Columna,Integer(ls_Null))
				Return 1
			End If
		End If

	CASE "enva_tipoen"
		If dw_4.RowCount() > 0 Then
			MessageBox("","Ya posee detalle ingresado para el envase elegido. ~rElimine el detalle o seleccione otro pallet.")
			This.SetItem(1, ls_Columna, this.Object.enva_tipoen[1])
			Return 1		
		End If	
		If ExisteEnvase(Integer(Data),0,istr_envase) = False Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1
		Else
			istr_Mant.Argumento[9]	=	String(istr_envase.TipoEnvase)
			This.SetItem(1, "enva_codigo", Integer(ls_Null))
			This.SetItem(1, "tpen_codigo", ls_Null)

			dw_3.GetChild("enva_codigo", idwc_envase)
			idwc_envase.SetTransObject(SqlCa)
			idwc_envase.Retrieve(Integer(Data))
			idwc_envase.SetSort("enva_codigo A")
			idwc_envase.Sort()
		End If

	CASE "enva_codigo"
		If dw_4.RowCount() > 0 Then
			MessageBox("","Ya posee detalle ingresado para el envase elegido. ~rElimine el detalle o seleccione otro pallet.")
      	This.SetItem(1, ls_Columna, this.Object.enva_codigo[1])
			Return 1		
		End If	

		If Isnull(this.Object.enva_tipoen[1]) Then
			MessageBox("Error de Datos","Necesita elegir primero un tipo de envase.")
      	This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1		
		End If	

		If ExisteEnvase(This.Object.enva_tipoen[1],Integer(Data),istr_envase) = False Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1
		Else
			istr_Mant.Argumento[10]	=	String(istr_envase.Codigo)
			This.SetItem(1, "tpen_codigo", ls_Null)

			dw_3.GetChild("tpen_codigo", idwc_tipopallet)
			idwc_tipopallet.SetTransObject(SqlCa)
		 	idwc_tipopallet.Retrieve(istr_envase.TipoEnvase,istr_envase.Codigo)

		End If

	CASE "tpen_codigo"
		If iuo_tipopallet.Existe_porEmbalaje(Integer(istr_Mant.Argumento[1]),&
														 is_embacodigo, &
														 Data, True, SqlCa) Then
			This.SetItem(1, "paen_altura", iuo_tipopallet.Altura)
		Else
			This.SetItem(1, ls_Columna, ls_Null)
			Return 1
		End If

	CASE "cate_codigo"
		If NOT iuo_categorias.Existe(Integer(Data),True,SqlCa) Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1
		Else
			ii_categoria	=	Integer(data)
		End If

	CASE "etiq_codigo"
		If dw_4.RowCount() > 0 Then
			MessageBox("","Ya posee detalle ingresado para la etiqueta elegida. ~rElimine el detalle o seleccione otro pallet.")
			This.SetItem(1, ls_Columna, this.Object.etiq_codigo[1])
			Return 1		
		End If	

		If NOT iuo_etiquetas.Existe(Integer(Data),True,SqlCa) Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1
		End If

	CASE "prod_codrot"
		If NOT iuo_productores.Existe(Integer(Data),True,SqlCa) Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			This.SetItem(1, "prod_nomrot", ls_Null)
			Return 1
		Else
			This.SetItem(1, "prod_nomrot", iuo_productores.Nombre)
		End If

	CASE "dest_codigo"
		If NOT iuo_destinos.Existe(Integer(Data),True,SqlCa) Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1
		End If

	CASE "reci_codigo"
		If NOT iuo_recibidores.Existe(Long(Data),True,SqlCa) Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1
		End If

	CASE "cama_codigo"
		If NOT iuo_camara.Existe(gstr_paramplanta.codigoplanta,Integer(Data),True,SqlCa) Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1
		End If
		
	CASE "copa_codigo"
		If NOT iuo_copa.Existe(Integer(data), True, SQLCa) Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			Return 1
		End If
			
End CHOOSE

HabilitaIngreso()
end event

event buttonclicked;call super::buttonclicked;long ll_fila
Integer	li_cliente

CHOOSE CASE dwo.name

	CASE "b_prodrotulado"
		BuscaProductor()

	CASE "buscapallet"
		IF isnull(dw_2.Object.mfee_docrel[1]) THEN
			MessageBox("Atención","Necesita Ingresar un movimiento o una Orden de Proceso.")
      ELSE
			BuscaPallet()
		END IF	
		
	CASE "embalaje"
		IF isnull(This.Object.paen_numero[1]) THEN
			MessageBox("Atención","Necesita Ingresar un Nro de Pallet.")
      ELSE
			ll_fila = il_fila
			il_fila = 0
			
			BuscaEmbalaje()
			
			li_cliente = Integer(istr_Mant.Argumento[1])
		 	idwc_tipopallet.Retrieve(li_cliente,is_buscemba)
			
			il_fila = ll_fila
			
			
		END IF
		
END CHOOSE
end event

event itemerror;RETURN 1
end event

