$PBExportHeader$w_maed_movtofrutaemba_proceso.srw
$PBExportComments$Ventana de Recepción Fruta Embalada de Proceso
forward
global type w_maed_movtofrutaemba_proceso from w_mant_encab_deta_csd
end type
type dw_4 from uo_dw within w_maed_movtofrutaemba_proceso
end type
type dw_3 from uo_dw within w_maed_movtofrutaemba_proceso
end type
type dw_6 from datawindow within w_maed_movtofrutaemba_proceso
end type
type dw_5 from datawindow within w_maed_movtofrutaemba_proceso
end type
type pb_ventanas from uo_botonventanas within w_maed_movtofrutaemba_proceso
end type
end forward

global type w_maed_movtofrutaemba_proceso from w_mant_encab_deta_csd
integer width = 4896
integer height = 2476
string title = "INGRESO DE PROCESO"
string menuname = ""
event ue_recuperapallet ( )
dw_4 dw_4
dw_3 dw_3
dw_6 dw_6
dw_5 dw_5
pb_ventanas pb_ventanas
end type
global w_maed_movtofrutaemba_proceso w_maed_movtofrutaemba_proceso

type variables
DataWindowChild	idwc_exportador, idwc_planta, idwc_tipofrio, idwc_periodofrio, &
                  idwc_especie, idwc_variedad, idwc_tipoenvase, idwc_envase, &
						idwc_categoria, idwc_etiqueta, idwc_destino, idwc_recibidor, &
						idwc_tipopallet, idwc_calibre, idwc_camara, idwc_cliente, idwc_predio, idwc_varirot

str_envase						istr_envase
str_calibreenvase				istr_calibre

uo_plantadesp					iuo_Planta
uo_exportadores				iuo_exportadores
uo_variedades					iuo_variedades
uo_categorias					iuo_categorias
uo_etiquetas					iuo_etiquetas
uo_destinos						iuo_destinos
uo_recibidores					iuo_recibidores
uo_tipopallet					iuo_tipopallet
uo_productores					iuo_productores
uo_lotesfrutagranel			iuo_Lote
uo_spro_ordenproceso		iuo_spro_ordenproceso
uo_spro_palletencab			iuo_spro_palletencab
uo_spro_movtofrutaembaenca	iuo_spro_movtofrutaembaenca
uo_camarasfrigo				iuo_camara
uo_fechaMovto					iuo_FechaMovto
uo_AnalizaPallet				iuo_pallet
uo_valida_codigopallet		iuo_copa

Integer							ii_envacodigo, ii_envatipoen, ii_categoria
String							is_ultimacol, is_embalaje, is_embacodigo, is_nomcuartel
Boolean							ib_Modifica, ib_AutoCommit, lb_existe = FALSE

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
public subroutine buscacuartel ()
public function boolean existepredio (integer al_productor, integer ai_predio)
public function boolean existecuartel (long al_productor, integer ai_predio, integer ai_cuartel)
public function string cargacodigo (integer ai_cliente, long al_planta, long al_pallet, integer ai_procedencia)
end prototypes

event ue_recuperapallet();Long		ll_fila_d, ll_fila_e, respuesta
Boolean	lb_Habilita	= True
String 	ls_embalaje

DO
	dw_3.SetRedraw(False)
	dw_3.Reset()

	IF dw_3.Retrieve(Integer(istr_Mant.Argumento[2]), &
						  Long(istr_Mant.Argumento[7]),&
						  Integer(istr_Mant.Argumento[1])) =  -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
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
		
		dw_4.GetChild("pafr_huert1", idwc_predio)
		idwc_predio.SetTransObject(SqlCa)
		idwc_predio.Retrieve(dw_2.Object.prod_codigo[1])
		
		istr_Mant.Argumento[9]	=	String(dw_3.Object.enva_tipoen[1])
		istr_Mant.Argumento[10]	=	String(dw_3.Object.enva_codigo[1])

		DO
			ll_Fila_d	=	dw_4.Retrieve( Integer(istr_Mant.Argumento[2]), &
													Long(istr_Mant.Argumento[7]),&
													Integer(istr_Mant.Argumento[1]))
								  
			IF ll_Fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				IF ll_Fila_d > 0 THEN
				
					HabilitaPallet(False)
					
					IF dw_3.Object.paen_estado[1] = 2 THEN
						lb_Habilita	=	False
					END IF
				ELSE
					lb_Habilita = True
				END IF
				
				ls_embalaje	=	dw_4.Object.emba_codigo[1]
				idwc_tipopallet.Retrieve(Integer(istr_Mant.Argumento[1]),ls_embalaje)
				iuo_tipopallet.Existe_PorEmbalaje(Integer(istr_Mant.Argumento[1]),&
									ls_embalaje, dw_3.Object.tpen_codigo[1],True, Sqlca)
									
				dw_4.Enabled			=	lb_Habilita
				pb_eliminar.Enabled	=	lb_Habilita	
				pb_grabar.Enabled		=	lb_Habilita
				pb_imprimir.Enabled	=	True	
				pb_ins_det.Enabled	=	lb_Habilita	
				pb_eli_det.Enabled	=	lb_Habilita	
				/*
				control de boton de emision de tarjas	
				*/
				pb_ventanas.Enabled 			=	dw_3.Object.paen_tipopa[1]	=	1
				
				pb_ventanas.ii_cliente		=	dw_3.Object.clie_codigo[1]
				pb_ventanas.ii_especie		=	dw_3.Object.espe_codigo[1]
				pb_ventanas.ii_cajas			=	dw_3.Object.paen_ccajas[1]
				pb_ventanas.il_planta		=	dw_3.Object.plde_codigo[1]
				pb_ventanas.il_pallet		=	dw_3.Object.paen_numero[1]
				pb_ventanas.ii_procedencia	=	1//Granel
				pb_ventanas.ii_operacion	=	1//Impresion
				pb_ventanas.ii_sistema		=	1//Granel	
				/*
				fin control
				*/
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

//IF gstr_paramplanta.etiquetaembalaje = 1 THEN
//	SELECT	emba_nombre, enva_tipoen, enva_codigo, enva_codigo, enva_tipoen
//		INTO 	:ls_nombre, :li_tipoen, :li_codenvase, :li_envacodigo, :li_envatipoen
//		FROM	dbo.embalajesprod
//		WHERE emba_codigo	= :ls_codigo
//		AND   etiq_codigo = :ai_etiqueta
//		AND	enva_tipoen	= :ai_tipoenvase
//		AND	enva_codigo	= :ai_envase
//		AND   clie_codigo = :li_clie_codigo;
//ELSE
SELECT	emba_nombre, enva_tipoen, enva_codigo, enva_codigo, enva_tipoen
	INTO 	:ls_nombre, :li_tipoen, :li_codenvase, :li_envacodigo, :li_envatipoen
	FROM	dbo.embalajesprod
	WHERE emba_codigo	= :ls_codigo
	AND	enva_tipoen	= :ai_tipoenvase
	AND	enva_codigo	= :ai_envase
	AND   clie_codigo = :li_clie_codigo;
//END IF		

IF SqlCa.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embalajes")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Embalaje (" + ls_codigo + &
					"), no ha sido creado en tabla respectiva.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
END IF

is_embalaje		=	ls_nombre
ii_envacodigo 	= 	li_envacodigo
ii_envatipoen 	= 	li_envatipoen
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

pb_ins_det.Enabled = lb_estado
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_Retorno
Integer 	Planta, movto, numero, li_Accion
String	ls_pallet

IF Not dw_2.uf_check_required(0) THEN RETURN False
IF Not dw_1.uf_validate(0) THEN RETURN False

IF NOT Borrando THEN
	IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		dw_2.SetItem(1,"mfee_horact",F_FechaHora())
	END IF
END IF

planta 	=	Integer(Istr_Mant.Argumento[2])
movto 	=	Integer(Istr_Mant.Argumento[3])				
numero	=	Integer(Istr_Mant.Argumento[4])

IF Borrando THEN
	IF dw_6.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF dw_5.Update(True, False) = 1 THEN
				IF dw_2.Update(True, False) = 1 THEN
					Commit;
			
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
					ELSE
						li_Accion		= 2
						
						lb_Retorno	=	True
			
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
						dw_3.Reset()
						dw_4.Reset()
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
	IF dw_3.Update(True, False) = 1 THEN
		IF dw_4.Update(True, False) = 1 THEN
			IF dw_2.Update(True, False) = 1 THEN
				IF dw_1.Update(True, False) = 1 THEN	
					
					DELETE dbo.spro_movtofrutaembaenca
					WHERE	plde_codigo	=	:Planta
					AND	tpmv_codigo	=	:Movto
					AND	mfee_numero =  0;
					
					Commit;
					dw_3.ResetUpdate()
					
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
						Rollback;
						
					ELSE
					
						ls_pallet	=	String(dw_3.Object.clie_codigo[dw_3.GetRow()], '000') + &
											String(dw_3.Object.paen_numero[dw_3.GetRow()], '000000')
											
						IF iuo_pallet.analiza_datos(ls_pallet, SqlCa) THEN
							dw_3.Object.paen_sscc18[1] =	iuo_pallet.Codbarra
							
						END IF
						
						dw_3.Object.paen_gs1128[dw_3.GetRow()]	=	cargacodigo(dw_3.Object.clie_codigo[dw_3.GetRow()], &
																								dw_3.Object.plde_codigo[dw_3.GetRow()], &
																								dw_3.Object.paen_numero[dw_3.GetRow()], 1)
						IF dw_3.Update(True, False) = 1 THEN
							Commit;	
							IF sqlca.SQLCode <> 0 THEN
								F_ErrorBaseDatos(sqlca, This.Title)
							ELSE
								lb_Retorno	=	True
								li_Accion	= 	1
					
								dw_1.ResetUpdate()
								dw_2.ResetUpdate()
								dw_3.ResetUpdate()
								dw_4.ResetUpdate()
							END IF
						ELSE
							F_ErrorBaseDatos(sqlca, This.Title)
							
							RollBack;
						END IF
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
	//Nuevo Pallet
	dw_3.Reset()
	dw_3.InsertRow(0)
	dw_4.Reset()
	dw_3.SetItem(1, "clie_codigo", Integer(istr_Mant.Argumento[1]))
	dw_3.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[2]))
	dw_3.SetItem(1, "paen_feccon", DateTime(Today()))
	dw_3.SetItem(1, "paen_inspec", 0)
	dw_3.SetItem(1, "paen_fumiga", 0)
	dw_3.SetItem(1, "paen_estado", 1)
END IF

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
	dw_3.Object.paen_tipopa.Color 	= 0
	dw_3.Object.enva_tipoen.Color 	= 0
	dw_3.Object.enva_codigo.Color 	= 0
	dw_3.Object.tpen_codigo.Color 	= 0
	dw_3.Object.cate_codigo.Color 	= 0
	dw_3.Object.etiq_codigo.Color 		= 0
	dw_3.Object.cama_codigo.Color	= 0
	
	dw_3.Object.vari_codigo.BackGround.Color = RGB(255,255,255)
	dw_3.Object.paen_tipopa.BackGround.Color = RGB(255,255,255)
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
	dw_3.Object.tpen_codigo.Protect	=	1
	dw_3.Object.cate_codigo.Protect	=	1
	dw_3.Object.etiq_codigo.Protect	=	1
	dw_3.Object.cama_codigo.Protect	=	1

	dw_3.Object.clie_codigo.Color 		= RGB(255,255,255)
	dw_3.Object.vari_codigo.Color 		= RGB(255,255,255)
	dw_3.Object.paen_tipopa.Color 	= RGB(255,255,255)
	dw_3.Object.enva_tipoen.Color 	= RGB(255,255,255)
	dw_3.Object.enva_codigo.Color 	= RGB(255,255,255)
	dw_3.Object.tpen_codigo.Color 	= RGB(255,255,255)
	dw_3.Object.cate_codigo.Color 	= RGB(255,255,255)
	dw_3.Object.etiq_codigo.Color 		= RGB(255,255,255)
	dw_3.Object.cama_codigo.Color	= RGB(255,255,255)
	
	dw_3.Object.clie_codigo.BackGround.Color	= 553648127
	dw_3.Object.vari_codigo.BackGround.Color 	= 553648127
	dw_3.Object.paen_tipopa.BackGround.Color = 553648127
	dw_3.Object.enva_tipoen.BackGround.Color = 553648127
	dw_3.Object.enva_codigo.BackGround.Color = 553648127
	dw_3.Object.tpen_codigo.BackGround.Color = 553648127
	dw_3.Object.cate_codigo.BackGround.Color = 553648127
	dw_3.Object.etiq_codigo.BackGround.Color = 553648127
	dw_3.Object.cama_codigo.BackGround.Color = 553648127
	
	
END IF
end subroutine

public subroutine buscapallet ();String ls_Null
Str_Busqueda	lstr_Busq

SetNull(ls_Null)

lstr_Busq.Argum[1] = istr_Mant.Argumento[2]
lstr_Busq.Argum[2] = "0"
lstr_Busq.Argum[3] = istr_Mant.Argumento[4]
lstr_Busq.Argum[4] = istr_Mant.Argumento[8]
lstr_Busq.Argum[5] = istr_Mant.Argumento[1]

OpenWithParm(w_busc_pallet_movimiento, lstr_Busq)
//OpenWithParm(w_busc_pallet_archivo, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

IF lstr_Busq.Argum[2] <> "" THEN
	dw_3.SetItem(1, "paen_numero", Integer(lstr_Busq.Argum[2]))

	IF iuo_spro_palletencab.Existe(Integer(istr_Mant.Argumento[1]), &
											 Integer(istr_Mant.Argumento[2]), &
											    Long(lstr_Busq.Argum[2]), False, SqlCa) THEN
								 
		IF iuo_spro_palletencab.especie	= 	dw_2.Object.espe_codigo[1] THEN
			istr_Mant.Argumento[7]			=	lstr_Busq.Argum[2]
			TriggerEvent("ue_recuperapallet")
			lb_existe 							=	TRUE
			dw_3.Object.prod_codrot[1] 	=	iuo_spro_ordenproceso.Productor

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
	dw_2.Object.clie_codigo.Protect	=	0
	dw_2.Object.mfee_numero.Protect	=	0
	dw_2.Object.mfee_fecmov.Protect	=	0
	dw_2.Object.mfee_tipdoc.Protect	=	0
	dw_2.Object.mfee_docrel.Protect	=	0
	dw_3.Object.clie_codigo.Protect	=	0
	dw_3.Object.vari_codigo.Protect	=	0
	dw_3.Object.paen_tipopa.Protect	=	0
	dw_3.Object.enva_tipoen.Protect	=	0
	dw_3.Object.enva_codigo.Protect	=	0
	dw_3.Object.tpen_codigo.Protect	=	0
	dw_3.Object.cate_codigo.Protect	=	0
	dw_3.Object.etiq_codigo.Protect	=	0
	
	dw_2.Object.clie_codigo.Color 		= 0
	dw_2.Object.mfee_numero.Color 	= 0
	dw_2.Object.mfee_fecmov.Color 	= 0
	dw_2.Object.mfee_tipdoc.Color 	= 0
	dw_2.Object.mfee_docrel.Color 	= 0
	dw_3.Object.clie_codigo.Color 		= 0
	dw_3.Object.vari_codigo.Color 		= 0
	dw_3.Object.paen_tipopa.Color 	= 0
	dw_3.Object.enva_tipoen.Color 	= 0
	dw_3.Object.enva_codigo.Color 	= 0
	dw_3.Object.cate_codigo.Color 	= 0
	dw_3.Object.etiq_codigo.Color 		= 0
	dw_3.Object.tpen_codigo.Color 	= 0
	
	dw_2.Object.clie_codigo.BackGround.Color = RGB(255,255,255)
	dw_2.Object.mfee_numero.BackGround.Color = RGB(255,255,255)
	dw_2.Object.mfee_fecmov.BackGround.Color = RGB(255,255,255)
	dw_2.Object.mfee_tipdoc.BackGround.Color = RGB(255,255,255)
	dw_2.Object.mfee_docrel.BackGround.Color = RGB(255,255,255)
	dw_3.Object.clie_codigo.BackGround.Color = RGB(255,255,255)
	dw_3.Object.vari_codigo.BackGround.Color = RGB(255,255,255)
	dw_3.Object.paen_tipopa.BackGround.Color = RGB(255,255,255)
	dw_3.Object.enva_tipoen.BackGround.Color = RGB(255,255,255)
	dw_3.Object.enva_codigo.BackGround.Color = RGB(255,255,255)
	dw_3.Object.cate_codigo.BackGround.Color = RGB(255,255,255)
	dw_3.Object.etiq_codigo.BackGround.Color = RGB(255,255,255)
	dw_3.Object.tpen_codigo.BackGround.Color = RGB(255,255,255)
	
	
	dw_2.Object.b_ordenproceso.visible = 1
	
	dw_2.SetColumn("clie_codigo")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clie_codigo.Protect	=	1
	dw_2.Object.mfee_numero.Protect=	1
	dw_2.Object.mfee_fecmov.Protect	=	1
	dw_2.Object.mfee_tipdoc.Protect	=	1
	dw_2.Object.mfee_docrel.Protect	=	1

	dw_2.Object.clie_codigo.Color 		= RGB(255,255,255)
	dw_2.Object.mfee_numero.Color	= RGB(255,255,255)
	dw_2.Object.mfee_fecmov.Color 	= RGB(255,255,255)
	dw_2.Object.mfee_tipdoc.Color 	= RGB(255,255,255)
	dw_2.Object.mfee_docrel.Color 	= RGB(255,255,255)
	
	dw_2.Object.clie_codigo.BackGround.Color 		= 553648127
	dw_2.Object.mfee_numero.BackGround.Color	= 553648127
	dw_2.Object.mfee_fecmov.BackGround.Color 	= 553648127
	dw_2.Object.mfee_tipdoc.BackGround.Color 	= 553648127
	dw_2.Object.mfee_docrel.BackGround.Color 	= 553648127
	
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
   
//   IF NOT iuo_productores.Existe(gi_CodExport,Integer(Data),True,SqlCa) THEN
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
		
			
		idwc_varirot.Retrieve(iuo_spro_ordenproceso.Especie)
		idwc_varirot.SetSort("vari_nombre A")
		idwc_varirot.Sort()
		
		istr_Mant.Argumento[5]	= lstr_Busq.Argum[10]
		istr_Mant.Argumento[6]	= lstr_Busq.Argum[6]
		istr_Mant.Argumento[8]	= String(iuo_spro_ordenproceso.Especie)
		
//		IF iuo_spro_movtofrutaembaenca.ExisteDoctoRelacionado(&
//										Integer(istr_Mant.Argumento[2]), &
//										Integer(lstr_Busq.Argum[10]), &
//										Long(lstr_Busq.Argum[6]), 1, SqlCa) THEN
		IF iuo_spro_movtofrutaembaenca.ExisteDoctoRelacionado(&
										Integer(istr_Mant.Argumento[2]), &
										Integer(lstr_Busq.Argum[10]), &
										Long(lstr_Busq.Argum[6]), SqlCa,&
										Integer(lstr_Busq.Argum[16])) THEN
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

	IF dw_6.Object.pafr_tipdoc[ll_fila] = li_tipdoc AND &
      dw_6.Object.pafr_docrel[ll_fila] = ll_docrel AND &
		dw_6.Object.paen_numero[ll_fila] = ll_pallet THEN
	  
		dw_6.DeleteRow(ll_fila)
	ELSE
		ll_fila ++
	END IF	
Loop

ll_filabusca = -1
ll_filabusca = dw_6.Find("clie_codigo = " + ls_cliente + " AND " + &
                         "plde_codigo = " + ls_planta + " AND " + &
								 "paen_numero = " + String(ll_pallet), 1, dw_6.RowCount())
IF ll_filabusca = 0 THEN
	lb_elimina = TRUE
ELSE
	lb_elimina = FALSE
END IF	

IF lb_elimina = TRUE THEN
	
	ll_filabusca = -1
	ll_filabusca = dw_5.Find("clie_codigo = " + ls_cliente + " AND " + &
	                         "plde_codigo = " + ls_planta + " AND " + &
									 "paen_numero = " + String(ll_pallet), 1, dw_5.RowCount())
	IF ll_filabusca > 0 THEN
		dw_5.DeleteRow(ll_filaBusca)
		lb_elimina = TRUE
	END IF
END IF


ll_filabusca = -1
ll_filabusca = dw_6.Find("clie_codigo = " + ls_cliente + " AND " + &
                         "plde_codigo = " + ls_planta + " AND " + &
								 "paen_numero = " + String(ll_pallet) + " AND " + &
								 "pafr_tipdoc = " + String(li_tipdoc) + " AND " + &
								 "pafr_docrel = " + String(ll_docrel), 1, dw_6.RowCount())
IF ll_filabusca = 0 THEN
	lb_elimina = TRUE
ELSE
	lb_elimina = FALSE
END IF

IF lb_elimina  THEN
	ll_filabusca = -1
	ll_filabusca = dw_1.Find("clie_codigo = " + ls_cliente + " AND " + &
									 "plde_codigo = " + ls_planta + " AND " + &
									 "paen_numero = " + String(ll_pallet), 1, dw_1.RowCount())
										 
	IF ll_filabusca > 0 THEN
		dw_1.DeleteRow(ll_filabusca)
	END IF
END IF

end subroutine

public subroutine buscaembalaje ();Str_Busqueda	lstr_Busq

lstr_Busq.Argum[1]	=	""//istr_Mant.Argumento[9]
lstr_Busq.Argum[2]	=	""//istr_Mant.Argumento[10]

OpenWithParm(w_busc_envase_embalajes, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

IF il_fila < 1 then 
	IF lstr_Busq.Argum[1] <> "" THEN
		dw_3.Object.emba_codigo[dw_3.GetRow()] = lstr_Busq.argum[1]
		
		dw_3.SetColumn("vari_codigo")
		dw_3.SetFocus()
	ELSE
		
		dw_3.SetColumn("emba_codigo")
		dw_3.SetFocus()
		
	END IF
ELSE
	
	IF lstr_Busq.Argum[1] <> "" THEN
		dw_4.SetItem(il_Fila, "emba_codigo", lstr_Busq.Argum[1])
	
		dw_4.SetColumn("cate_codigo")
		dw_4.SetFocus()
	ELSE
		dw_4.SetColumn("emba_codigo")
		dw_4.SetFocus()
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

public function boolean cargaembalaje (string as_embacodigo);String	ls_codigo, ls_nombre
Integer	li_tipoen, li_codenvase, li_envacodigo, li_envatipoen, li_clie_codigo

li_clie_codigo = dw_2.Object.Clie_codigo[dw_2.RowCount()]

SELECT	emba_nombre, enva_tipoen, enva_codigo, enva_codigo, enva_tipoen
	INTO 	:ls_nombre, :li_tipoen, :li_codenvase, :li_envacodigo, :li_envatipoen
	FROM	dbo.embalajesprod
	WHERE emba_codigo	= :as_embacodigo
	AND 	clie_codigo = :li_clie_codigo;

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

public subroutine buscacuartel ();Str_Busqueda	lstr_Busq

lstr_Busq.Argum[1]	=	String(dw_2.Object.prod_codigo[1])
lstr_Busq.Argum[2]	=	istr_Mant.Argumento[20]

OpenWithParm(w_busc_cuartel, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	dw_4.Object.pafr_cuart1[il_Fila] = integer(lstr_Busq.argum[1])
	dw_4.Object.prcc_nombre[il_Fila] = lstr_Busq.argum[2]
END IF
end subroutine

public function boolean existepredio (integer al_productor, integer ai_predio);Integer	li_Cantidad, li_cliente, li_Planta
Long		ll_NumeroPallet


SELECT	prpr_codigo
	INTO	:ll_NumeroPallet
	FROM	dbo.spro_prodpredio
	WHERE	prod_codigo	=	:al_productor
	AND	prpr_codigo	=	:ai_predio;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_prodpredio")

	RETURN True
ELSEIF ll_NumeroPallet > 0 THEN
	
	RETURN False
ELSE	
	RETURN True
END IF

RETURN False
end function

public function boolean existecuartel (long al_productor, integer ai_predio, integer ai_cuartel);

SELECT	prcc_nombre
	INTO	:is_nomcuartel
	FROM	dbo.spro_prodcuarteles
	WHERE	prod_codigo	=	:al_productor
	AND	prpr_codigo	=	:ai_predio
	AND	prcc_codigo =  :ai_cuartel;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_prodcuarteles")

	RETURN True
ELSEIF is_nomcuartel <> '' THEN
	
	RETURN False
ELSE	
	RETURN True
END IF

RETURN False
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
	F_ErrorBaseDatos(SQLCA, "Lectura del Procedimiento Almacenado " + &
									"genera_adhesivos_pallets" )
							
ELSE
	FEtCH Codigo INTO :ls_respuesta;
END IF	
	
CLOSE Codigo;

RETURN ls_respuesta 

end function

on w_maed_movtofrutaemba_proceso.create
int iCurrent
call super::create
this.dw_4=create dw_4
this.dw_3=create dw_3
this.dw_6=create dw_6
this.dw_5=create dw_5
this.pb_ventanas=create pb_ventanas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_4
this.Control[iCurrent+2]=this.dw_3
this.Control[iCurrent+3]=this.dw_6
this.Control[iCurrent+4]=this.dw_5
this.Control[iCurrent+5]=this.pb_ventanas
end on

on w_maed_movtofrutaemba_proceso.destroy
call super::destroy
destroy(this.dw_4)
destroy(this.dw_3)
destroy(this.dw_6)
destroy(this.dw_5)
destroy(this.pb_ventanas)
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

x	= 0
y	= 0

iuo_Planta							=	Create uo_plantadesp
iuo_variedades						=	Create uo_variedades
iuo_categorias						=	Create uo_categorias
iuo_etiquetas						=	Create uo_etiquetas
iuo_destinos							=	Create uo_destinos
iuo_recibidores						=	Create uo_recibidores
iuo_tipopallet						=	Create uo_tipopallet
iuo_productores					=	Create uo_productores
iuo_Lote								=	Create uo_lotesfrutagranel
iuo_spro_ordenproceso			=	Create uo_spro_ordenproceso
iuo_spro_palletencab				=	Create uo_spro_palletencab
iuo_spro_movtofrutaembaenca	=	Create uo_spro_movtofrutaembaenca
iuo_camara							=	Create uo_camarasfrigo
iuo_FechaMovto					=  Create uo_fechaMovto	
iuo_pallet							=	Create uo_AnalizaPallet	
iuo_copa								=	Create uo_valida_codigopallet

This.Height	= 2550
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)

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

dw_4.GetChild("pafr_huert1", idwc_predio)
dw_4.GetChild("vari_codrot", idwc_varirot)

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
idwc_predio.SetTransObject(SqlCa)
idwc_varirot.SetTransObject(SqlCa)

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

IF idwc_predio.Retrieve(-1) = 0 THEN
	MessageBox("Atención", "Falta Registrar Predios")
	idwc_predio.InsertRow(0)
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

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

istr_Mant.Argumento[1]	=	String(gi_CodExport)							// Cliente
istr_Mant.Argumento[2]	=	String(gstr_ParamPlanta.CodigoPlanta)	// Planta
istr_Mant.Argumento[3]	=	"4"												// Tipo Movto. (Recep. de Proceso)
istr_Mant.Argumento[5]	=	"4"												// Tipo de Proceso (Proceso)

buscar	= "Embalaje:Semba_codigo,Categoría:Ncate_codigo"
ordenar	= "Embalaje:emba_codigo,Categoría:cate_codigo"
is_ultimacol = "lote_codigo"
//dw_3.Object.paen_numero.Protect	=	1
//dw_3.Object.paen_numero.BackGround.Color = RGB(192,192,192)

end event

event ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta

DO
	dw_2.SetRedraw(False)
	//dw_2.Reset()
   dw_5.Reset()
	dw_6.Reset()
	
	IF dw_2.Retrieve(Integer(istr_Mant.Argumento[2]), &
						  Integer(istr_Mant.Argumento[3]), &
						  Long(istr_Mant.Argumento[4]),&
						  Integer(istr_Mant.Argumento[1])) = -1 OR &
		dw_5.Retrieve(Integer(istr_Mant.Argumento[2]), &
						  Integer(istr_Mant.Argumento[3]), &
						  Long(istr_Mant.Argumento[4]),&
						  Integer(istr_Mant.Argumento[1])) = -1 OR &
		dw_6.Retrieve(Integer(istr_Mant.Argumento[2]), &
						  Integer(istr_Mant.Argumento[3]), &
						  Long(istr_Mant.Argumento[4]),&
						  Integer(istr_Mant.Argumento[1])) = -1 THEN 
						  
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE

		istr_Mant.Argumento[8]	=	String(dw_2.Object.espe_codigo[1])
		
		dw_3.GetChild("vari_codigo", idwc_variedad)
		idwc_variedad.SetTransObject(SqlCa)
		idwc_variedad.Retrieve(dw_2.Object.espe_codigo[1])
		idwc_variedad.SetSort("vari_nombre A")
		idwc_variedad.Sort()
		DO
			IF dw_1.Retrieve(Integer(istr_Mant.Argumento[2]), &
								  Integer(istr_Mant.Argumento[3]), &
								  Long(istr_Mant.Argumento[4]),&
						        Integer(istr_Mant.Argumento[1])) = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE

				pb_imprimir.Enabled	=	True
				IF dw_4.RowCount() > 0 THEN
				  	pb_eliminar.Enabled	=	True
				  	pb_grabar.Enabled		=	True
				  
				 	IF iuo_spro_ordenproceso.Estado < 4 THEN
						pb_ins_det.Enabled		=	True
					  	pb_eli_det.Enabled		=	True
					ELSE
						pb_ins_det.Enabled		=	False
					  	pb_eli_det.Enabled		=	False
					END IF
			   END IF  
				
				HabilitaEncab(False)
								
				dw_1.SetRow(1)
				dw_1.SelectRow(1,True)
				
				dw_3.SetColumn("paen_numero")
				dw_3.SetFocus()
//				this.TriggerEvent("ue_recuperapallet")
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
dw_3.SetItem(1, "cama_codigo", 0)

dw_3.SetItem(1, "paen_inspec", 0)
dw_3.SetItem(1, "paen_fumiga", 0)
dw_3.SetItem(1, "paen_estado", 1)
end event

event ue_nuevo_detalle;dw_4.SetColumn("emba_codigo")

IF ExisteCalibres() THEN
	IF il_fila > 0 THEN
		IF dw_4.RowCount()>0 THEN
			IF isnull(dw_4.Object.pafr_calibr[dw_4.getrow()]) THEN
				MessageBox("Error de datos","Falta ingreso de calibre.")
				RETURN
			END IF
		END IF	
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	END IF
	
	dw_4.GetChild("pafr_huert1", idwc_predio)
	idwc_predio.SetTransObject(SqlCa)
	idwc_predio.Retrieve(dw_2.Object.prod_codigo[1])

	il_fila = dw_4.InsertRow(0)

	IF il_fila > 1 THEN
		dw_4.Object.emba_codigo[il_fila] 		= dw_4.Object.emba_codigo[il_fila - 1]
		dw_4.Object.emba_nombre[il_fila] 	= dw_4.Object.emba_nombre[il_fila - 1]
	END IF
	
	IF dw_4.RowCount() > 0 THEN
		pb_eli_det.Enabled	= True
	END IF
	
	dw_4.SetItem(il_fila,"plde_origen", gstr_paramplanta.codigoplanta)
	dw_4.SetItem(il_fila,"espe_codigo", dw_2.Object.espe_codigo[1])
	dw_4.SetItem(il_fila,"cate_codigo", dw_3.Object.cate_codigo[1])
	dw_4.SetItem(il_fila,"emba_codigo", is_embacodigo)
	dw_4.SetItem(il_fila,"vari_codrot", dw_3.Object.vari_codigo[1])
	
	dw_4.ScrollToRow(il_fila)
	dw_4.SetRow(il_fila)
	dw_4.SetFocus()
	dw_4.SetColumn("pafr_calibr")
ELSE
	MessageBox("Atención", "Falta Registrar Calibres para Especie (" + & 
					String(dw_2.Object.espe_codigo[1], '00') + ") - Tipo de Envase (" + &
					String(dw_3.Object.enva_tipoen[1]) + ") - Envase (" + &
					String(dw_3.Object.enva_codigo[1], '000') + ")")
END IF
end event

event ue_borra_detalle;Long ll_FilaBusca, ll_fila

IF lb_existe = TRUE AND dw_4.rowcount() < 2 THEN RETURN

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
		il_fila = dw_4.GetRow()
	END IF
END IF

Captura_Totales()
end event

event ue_antesguardar;call super::ue_antesguardar;Long				ll_Fila, ll_NumeroMovto, ll_NumeroPallet, ll_FilaMov, ll_Nuevo
Integer			li_Cliente, li_Planta, li_TipoMovto, li_Secuencia, li_cont, li_i, li_expocodigo, li_totcaj
Boolean  		lb_detalle = FALSE
String   		ls_mensaje, ls_colu[], ls_pallet

uo_variedades	luo_variedad

ib_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

li_Cliente				=	dw_3.Object.clie_codigo[1]
li_Planta				=	dw_2.Object.plde_codigo[1]
li_TipoMovto		=	dw_2.Object.tpmv_codigo[1]
ll_NumeroMovto	=	dw_2.Object.mfee_numero[1]

ll_Fila = 1

DO WHILE ll_Fila <= dw_4.RowCount()
	If dw_4.GetItemStatus(ll_Fila, 0, Primary!) = New! Then
		dw_4.DeleteRow(ll_Fila)
	Else
		ll_Fila ++
	End If
LOOP

If dw_4.RowCount() = 0 Then
	Message.DoubleParm = -1
	RETURN
End If	

FOR ll_Fila=1 TO dw_4.RowCount()
	If Isnull(dw_4.Object.emba_codigo[ll_fila]) OR dw_4.Object.emba_codigo[ll_fila] = "" Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nEmbalaje"
		ls_colu[li_cont]	= "emba_codigo"
	End If

	If Isnull(dw_4.Object.pafr_calibr[ll_fila]) OR dw_4.Object.pafr_calibr[ll_fila] = "" Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCalibre"
		ls_colu[li_cont]	= "pafr_calibr"
	End If
	
	If Isnull(dw_4.Object.cate_codigo[ll_fila]) OR dw_4.Object.cate_codigo[ll_fila] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCategoria"
		ls_colu[li_cont]	= "cate_codigo"
	End If

	If Isnull(dw_4.Object.pafr_ccajas[ll_fila]) OR dw_4.Object.pafr_ccajas[ll_fila] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCantidad de Cajas"
		ls_colu[li_cont]	= "pafr_ccajas"
	End If
	
	If li_cont > 0 Then
		MessageBox("Error de Consistencia", "En la Fila : "+ String(ll_fila) + " Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
		dw_4.SetRow(ll_fila)
		dw_4.SetColumn(ls_colu[1])
		dw_4.SetFocus()
		Message.DoubleParm = -1
		RETURN
	End If
NEXT

If dw_2.GetItemStatus(1, 0, Primary!) = NewModIfied! AND NOT gstr_paramplanta.packing Then
	INSERT INTO	dbo.spro_movtofrutaembaenca (plde_codigo, tpmv_codigo, mfee_numero)
		VALUES (:li_Planta, :li_tipomovto, 0) ;
	
	SELECT	IsNull(Max(mfee_numero), 0) + 1
		INTO	:ll_NumeroMovto
		FROM	dbo.spro_movtofrutaembaenca
		WHERE	plde_codigo	=	:li_Planta
		AND	tpmv_codigo	=	:li_TipoMovto ;

	dw_2.Object.mfee_numero[1]	=	ll_NumeroMovto
ElseIf gstr_paramplanta.packing Then
	
	dw_2.Object.mfee_numero[1]	=	dw_2.Object.mfee_docrel[1]
	
End If

istr_Mant.Argumento[4]	=	String(dw_2.Object.mfee_numero[1])

istr_Mant.Argumento[7]	=	String(dw_3.Object.paen_numero[1])

ll_NumeroPallet			=	Long(istr_Mant.Argumento[7])

dw_3.Object.frio_tipofr[1]	=	dw_2.Object.frio_tipofr[1]
dw_3.Object.sepl_codigo[1]	=	1  //Servicio Proceso

SELECT	IsNull(Max(pafr_secuen), 0) + 1
	INTO	:li_Secuencia
	FROM	dbo.spro_palletfruta
	WHERE	clie_codigo	=	:li_Cliente
	AND	plde_codigo	=	:li_Planta
	AND	paen_numero	=	:ll_NumeroPallet ;

luo_variedad	=	Create uo_variedades

FOR ll_Fila = 1 TO dw_4.RowCount()
	dw_4.Object.pafr_calibr[ll_Fila]	=	dw_4.Object.pafr_calibr[ll_Fila]//Mid(dw_4.Object.pafr_calibr[ll_Fila] + "   ", 1, 3)

	If dw_4.GetItemStatus(ll_Fila, 0, Primary!) = NewModIfied! Then
		
//		luo_variedad.Existe(dw_2.Object.espe_codigo[1], dw_3.Object.vari_codigo[1], False, sqlca)
		
		dw_4.Object.clie_codigo[ll_Fila]		=	dw_3.Object.clie_codigo[1]
		dw_4.Object.plde_codigo[ll_Fila]		=	dw_3.Object.plde_codigo[1]
		dw_4.Object.paen_numero[ll_Fila]	=	dw_3.Object.paen_numero[1]
		dw_4.Object.pafr_secuen[ll_Fila]		=	li_Secuencia
		dw_4.Object.frio_tipofr[ll_Fila]			=	dw_2.Object.frio_tipofr[1]
		dw_4.Object.pefr_codigo[ll_Fila]		=	dw_2.Object.pefr_codigo[1]
		dw_4.Object.espe_codigo[ll_Fila]		=	dw_3.Object.espe_codigo[1]
		dw_4.Object.vari_codigo[ll_Fila]		=	dw_2.Object.vari_codigo[1]
		dw_4.Object.pafr_varrot[ll_Fila]		=	dw_4.Object.vari_codrot[ll_Fila]
		dw_4.Object.etiq_codigo[ll_Fila]		=	dw_3.Object.etiq_codigo[1]
		dw_4.Object.prod_codrot[ll_Fila]		=	dw_3.Object.prod_codrot[1]
		dw_4.Object.prod_codigo[ll_Fila]		=	dw_2.Object.prod_codigo[1]
		dw_4.Object.plde_origen[ll_Fila]		=	dw_3.Object.plde_codigo[1]
		dw_4.Object.pafr_tipdoc[ll_Fila]		=	dw_2.Object.mfee_tipdoc[1]
		dw_4.Object.pafr_docrel[ll_Fila]		=	dw_2.Object.mfee_docrel[1]
		dw_4.Object.pafr_fecemb[ll_Fila]		=	dw_2.Object.mfee_fecmov[1]
		dw_4.Object.cocc_codigo[ll_Fila] 		=	1
		dw_4.Object.tpmv_codigo[ll_Fila]		=	dw_2.Object.tpmv_codigo[1]
		dw_4.Object.mfee_numero[ll_Fila] 	=	dw_2.Object.mfee_numero[1]
		dw_4.Object.pafr_catrot[ll_Fila]		=	dw_3.Object.cate_codigo[1]
		dw_4.Object.pafr_copack[ll_Fila] 		=	gi_Packing
		
		li_Secuencia ++
		lb_detalle = TRUE		
		
	End If
	li_totcaj									=	li_totcaj + dw_4.Object.pafr_ccajas[ll_Fila]
	dw_3.Object.paen_ccajas[1]		=	li_totcaj
	FueraDeNorma(ll_Fila)
NEXT

If lb_detalle Then
	ll_FilaMov	=	dw_1.Find("clie_codigo = " + String(dw_3.Object.clie_codigo[1]) + &
									 " AND paen_numero = " + String(dw_3.Object.paen_numero[1]), 1, dw_1.RowCount())
	If ll_FilaMov = 0 Then
		ll_Nuevo	=	dw_1.InsertRow(0)
		dw_1.Object.plde_codigo[ll_Nuevo]	=	dw_2.Object.plde_codigo[1]
		dw_1.Object.tpmv_codigo[ll_Nuevo]	=	dw_2.Object.tpmv_codigo[1]
		dw_1.Object.mfee_numero[ll_Nuevo]	=	dw_2.Object.mfee_numero[1]
		dw_1.Object.clie_codigo[ll_Nuevo]		=	dw_2.Object.clie_codigo[1]
		dw_1.Object.paen_numero[ll_Nuevo]	=	dw_3.Object.paen_numero[1]
	End If
End If

Integer li_cliecod
li_cliecod 		= dw_2.GetItemNumber(dw_2.GetRow(), "Clie_codigo")

FOR li_i = 1 to dw_4.RowCount()
	dw_4.Object.Clie_codigo[li_i] = li_cliecod
NEXT

If li_cliecod = GI_codExport Then
	li_expocodigo = buscaexportador()
Else
	li_expocodigo = li_cliecod
End If

FOR li_i = 1 to dw_1.RowCount()
	dw_1.Object.expo_codigo[li_i] = li_expocodigo
NEXT

FOR li_i = 1 to dw_2.RowCount()
	dw_2.Object.expo_codigo[li_i] = li_expocodigo
NEXT

If Long(dw_3.Object.tpen_codigo[1]) = dw_3.Object.paen_ccajas[1] Then
	dw_3.Object.paen_tipopa[1]	=	1
Else
	dw_3.Object.paen_tipopa[1]	=	2
End If
																		
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
		IF dw_5.Retrieve(Integer(istr_Mant.Argumento[2]), Integer(istr_Mant.Argumento[3]), &
							  Long(istr_Mant.Argumento[4]),dw_2.Object.clie_codigo[1]) = -1 OR &
			dw_6.Retrieve(Integer(istr_Mant.Argumento[2]), Integer(istr_Mant.Argumento[3]), &
							  Long(istr_Mant.Argumento[4]),dw_2.Object.clie_codigo[1]) = -1 THEN 
							  
			respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
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
lstr_busq.Argum[20] = string(buscaexportador())

OpenWithParm(w_busc_recfruprocee, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	istr_Mant.Argumento[4]	=	lstr_Busq.Argum[5]
	istr_Mant.Argumento[5]	=	lstr_Busq.Argum[7]
	istr_Mant.Argumento[6]	=	lstr_Busq.Argum[8]
	
	dw_3.Object.paen_numero.Protect	=	0
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

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "RECEPCION DE PROCESO"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_revision_proceso_embalada"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(dw_2.Object.plde_codigo[1],dw_2.Object.mfee_tipdoc[1],dw_2.Object.mfee_docrel[1], dw_2.Object.clie_codigo[1])

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

IF isnull(dw_2.Object.mfee_numero[1]) THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

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

event resize;call super::resize;dw_3.x				=	dw_2.x
dw_3.y				=	dw_2.y + dw_2.Height +  10
dw_3.Width			=	dw_2.Width

dw_4.x				=	dw_1.x
dw_4.y				=	dw_3.y + dw_3.Height +  10
dw_4.Width			=	dw_1.Width
dw_4.Height			=	This.WorkSpaceHeight() - dw_4.y - 41

pb_Ventanas.x 		=  pb_salir.x
pb_Ventanas.y 		=  pb_salir.y + pb_salir.Height + 10
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutaemba_proceso
boolean visible = false
integer x = 0
integer y = 1760
integer width = 4133
integer height = 1160
string title = "Detalle de Movimiento"
string dataobject = "dw_mues_movtofrutaembadeta"
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutaemba_proceso
integer x = 754
integer y = 32
integer width = 3072
integer height = 444
integer taborder = 10
string dataobject = "dw_mant_movtofrutaembaenca_proceso"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Null, ls_Fecha
Date     ld_fecha
Datetime ld_fechapru, ldt_fecha
string ls_cliente
SetNull(ls_Null)

ls_Columna = dwo.name
ls_cliente = string(This.GetItemNumber(row,"clie_codigo"))
CHOOSE CASE ls_Columna
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(row,"clie_codigo",Integer(ls_Null))
			RETURN 1
		ELSE
			istr_Mant.Argumento[1]	= Data
		   dw_3.SetItem(1, "clie_codigo", Integer(Data))
		END IF	
		
	CASE "mfee_tipdoc"
		istr_Mant.Argumento[5]	= Data
		This.SetItem(1, "mfee_docrel", Long(ls_Null))
 		This.SetItem(1, "prod_nombre", Integer(ls_Null))
		This.SetItem(1, "frio_tipofr", ls_Null)
		This.SetItem(1, "pefr_codigo", Integer(ls_Null))
		This.SetItem(1, "espe_codigo", Integer(ls_Null))
		This.SetItem(1, "vari_nombre", ls_Null)

	CASE "mfee_docrel"
		IF iuo_spro_ordenproceso.Existe(Integer(istr_Mant.Argumento[2]), &
												  This.Object.mfee_tipdoc[1], &
												  Long(Data),True,SqlCa,&
												  Integer(istr_Mant.Argumento[1])) THEN
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
			
			idwc_varirot.Retrieve(iuo_spro_ordenproceso.Especie)
			
			dw_4.GetChild("pafr_huert1", idwc_predio)
			idwc_predio.SetTransObject(SqlCa)
			idwc_predio.Retrieve(dw_2.Object.prod_codigo[1])
			
			IF iuo_productores.Existe(iuo_spro_ordenproceso.Productor,False,SqlCa) THEN
				dw_3.SetItem(1, "prod_nomrot", iuo_productores.Nombre)
				dw_2.SetItem(1, "clie_codigo", long(ls_cliente))
				dw_3.SetItem(1, "clie_codigo", long(ls_cliente))
				istr_Mant.Argumento[1]	= ls_cliente
			END IF

			dw_3.Object.paen_numero.Protect	=	0
			dw_3.Object.paen_numero.BackGround.Color = RGB(255,255,255)

			dw_3.GetChild("vari_codigo", idwc_variedad)
			idwc_variedad.SetTransObject(SqlCa)
			idwc_variedad.Retrieve(iuo_spro_ordenproceso.Especie)
			idwc_variedad.SetSort("vari_nombre A")
			idwc_variedad.Sort()
			
//			IF iuo_spro_movtofrutaembaenca.ExisteDoctoRelacionado(&
//											Integer(istr_Mant.Argumento[2]), &
//											This.Object.mfee_tipdoc[1], &
//											Long(Data), 1,SqlCa) THEN
			IF iuo_spro_movtofrutaembaenca.ExisteDoctoRelacionado(&
											Integer(istr_Mant.Argumento[2]), &
											This.Object.mfee_tipdoc[1], &
											Long(Data), SqlCa,&
											Integer(istr_Mant.Argumento[1])) THEN
				istr_Mant.Argumento[4]	=	String(iuo_spro_movtofrutaembaenca.NumeroMovto)
				Parent.TriggerEvent("ue_recuperadatos")
			END IF
		ELSE
			This.SetItem(1, "prod_nombre", ls_Null)
			This.SetItem(1, "frio_tipofr", ls_Null)
			This.SetItem(1, "pefr_codigo", Integer(ls_Null))
			This.SetItem(1, "espe_codigo", Integer(ls_Null))
			This.SetItem(1, "vari_nombre", ls_Null)
			This.SetItem(1, ls_Columna, Long(ls_Null))
			RETURN 1
		END IF

	CASE "mfee_numero"
		IF ExisteMovimiento(Long(Data),Integer(istr_Mant.Argumento[1])) THEN
			istr_Mant.Argumento[4]	=	Data
			IF ExistePallet(Integer(istr_Mant.Argumento[5]), &
								 Long(istr_Mant.Argumento[6])) THEN

				Parent.TriggerEvent("ue_recuperadatos")
				
				IF iuo_spro_ordenproceso.Existe(Integer(istr_Mant.Argumento[2]), &
												                This.Object.mfee_tipdoc[1], &
												                This.Object.mfee_docrel[1],True,SqlCa,&
																	 Integer(istr_Mant.Argumento[1])) THEN
					istr_Mant.Argumento[6]	= String(This.Object.mfee_docrel[1])
					istr_Mant.Argumento[8]	= String(iuo_spro_ordenproceso.Especie)
								
					iuo_spro_ordenproceso.CapturaReferencias(1,0,1,0,0,False,Sqlca)
			
					dw_3.SetItem(1, "espe_codigo", iuo_spro_ordenproceso.Especie)
					dw_3.SetItem(1, "frio_tipofr", iuo_spro_ordenproceso.TipoFrio)
					dw_3.SetItem(1, "vari_codigo", iuo_spro_ordenproceso.Variedad)
					dw_3.SetItem(1, "prod_codrot", iuo_spro_ordenproceso.Productor)
			
					IF iuo_productores.Existe(iuo_spro_ordenproceso.Productor,False,SqlCa) THEN
						dw_3.SetItem(1, "prod_nomrot", iuo_productores.Nombre)
						dw_3.SetItem(1, "clie_codigo", long(ls_cliente))
						istr_Mant.Argumento[1]	= ls_cliente
					END IF
				END IF	
			END IF
		ELSE
			This.SetItem(1, ls_Columna, Long(ls_Null))
			RETURN 1
		END IF

	CASE "mfee_fecmov"
		ld_fechapru = dw_2.Object.mfee_fecmov[1]
		IF isnull(data) THEN
			MessageBox("Atención","Ingrese una fecha valida.")
			This.SetItem(1, ls_Columna, ld_Fechapru)
			Return 1
		END IF	
		IF Date(Mid(data,1,10)) = ld_fecha THEN
			MessageBox("Atención","Ingrese una fecha valida.")
			This.SetItem(1, ls_Columna, ld_fechapru)
			Return 1
		ELSE
			ldt_Fecha	=	DateTime(Date(Mid(data,1,10)))
			IF NOT iuo_FechaMovto.Valida_FechaMovto(Date(ldt_Fecha)) THEN
				This.SetItem(1, ls_Columna, ld_fechapru)
				This.SetFocus()
				RETURN 1
			END IF
			This.SetItem(1, ls_Columna, DateTime(Date(Mid(Data,1,10))))
		END IF	
		

END CHOOSE

HabilitaIngreso()
end event

event dw_2::itemerror;call super::itemerror;RETURN 1
end event

event dw_2::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.name

	CASE "b_ordenproceso"
		BuscaOrdenProceso()

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutaemba_proceso
integer x = 4411
integer y = 268
integer taborder = 90
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutaemba_proceso
integer x = 4411
integer y = 444
integer taborder = 100
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutaemba_proceso
integer x = 4411
integer y = 632
integer taborder = 60
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutaemba_proceso
integer x = 4411
integer y = 808
integer taborder = 70
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutaemba_proceso
integer x = 4411
integer y = 1160
integer taborder = 110
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutaemba_proceso
integer x = 4411
integer y = 1464
integer taborder = 40
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutaemba_proceso
integer x = 4411
integer y = 1636
integer taborder = 50
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutaemba_proceso
integer x = 4411
integer y = 84
integer taborder = 80
end type

type dw_4 from uo_dw within w_maed_movtofrutaemba_proceso
integer x = 87
integer y = 1140
integer width = 4256
integer height = 732
integer taborder = 30
string title = "Detalle de Pallet"
string dataobject = "dw_mues_palletfruta_proceso"
boolean hscrollbar = true
boolean livescroll = true
end type

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

Choose Case ls_Columna
	Case "emba_codigo"
		ls_embalaje = dw_4.Object.emba_codigo[row]
		If IsNull(dw_4.Object.pafr_docrel[row]) OR (dw_4.Object.pafr_docrel[row] = dw_2.Object.mfee_docrel[1] AND &
		    dw_4.Object.pafr_tipdoc[row] = dw_2.Object.mfee_tipdoc[1])Then
			If Not ExisteEmbalaje(Integer(istr_Mant.Argumento[9]),  &
										 Integer(istr_Mant.Argumento[10]), &
										 dw_3.Object.etiq_codigo[1],Data) Then
				This.SetItem(row, ls_Columna, ls_Nula)
				This.SetItem(row, "emba_nombre", ls_Nula)
				Return 1
			Else
				This.SetItem(row, "emba_nombre", is_embalaje)
			End If
		Else	
			MessageBox("Atención","El registro pertenece a otra Orden de Proceso. Imposible ModIficar.")
			This.SetItem(row, ls_Columna, ls_Embalaje)
			Return 1
		End If
		
	Case "cate_codigo"
		li_categoria = dw_4.Object.cate_codigo[row]
		If IsNull(dw_4.Object.pafr_docrel[row]) OR (dw_4.Object.pafr_docrel[row] = dw_2.Object.mfee_docrel[1] AND &
		    dw_4.Object.pafr_tipdoc[row] = dw_2.Object.mfee_tipdoc[1])Then
			If Not iuo_categorias.Existe(Integer(Data),True,SqlCa) Then
				This.SetItem(row, ls_Columna, Integer(ls_Nula))
				Return 1
			End If
		Else	
			MessageBox("Atención","El registro pertenece a otra Orden de Proceso. Imposible ModIficar.")
			This.SetItem(row, ls_Columna, li_categoria)
			Return 1
		End If
		
	Case "pafr_calibr"
		ls_Calibre	=	dw_4.Object.pafr_calibr[row]
		Data			=	Data//Mid(Data + "   ", 1, 3)
		
		If IsNull(dw_4.Object.pafr_docrel[row]) OR (dw_4.Object.pafr_docrel[row] = dw_2.Object.mfee_docrel[1] AND &
		    dw_4.Object.pafr_tipdoc[row] = dw_2.Object.mfee_tipdoc[1])Then
			If Not ExisteCalibre(Integer(istr_Mant.Argumento[8]), &
								  		Integer(istr_Mant.Argumento[9]), &
								  		Integer(istr_Mant.Argumento[10]), &
										Data, istr_calibre) Then
				This.SetItem(row, ls_Columna, ls_Nula)
				Return 1
			Else
				dw_4.Object.pafr_calrot[row]	=	Data
			End If
		Else	
			MessageBox("Atención","El registro pertenece a otra Orden de Proceso. Imposible ModIficar.")
			This.SetItem(row, ls_Columna, ls_calibre)
			Return 1
		End If
		
	Case "pafr_calrot"
		ls_Calibre	=	dw_4.Object.pafr_calrot[row]
		Data			=	Data//Mid(Data + "   ", 1, 3)
		
		If IsNull(dw_4.Object.pafr_docrel[row]) OR (dw_4.Object.pafr_docrel[row] = dw_2.Object.mfee_docrel[1] AND &
		    dw_4.Object.pafr_tipdoc[row] = dw_2.Object.mfee_tipdoc[1])Then
			If Not ExisteCalibre(Integer(istr_Mant.Argumento[8]), &
								  		Integer(istr_Mant.Argumento[9]), &
								  		Integer(istr_Mant.Argumento[10]), &
										Data, istr_calibre) Then
				This.SetItem(row, ls_Columna, ls_Nula)
				Return 1
			End If
		Else	
			MessageBox("Atención","El registro pertenece a otra Orden de Proceso. Imposible ModIficar.")
			This.SetItem(row, ls_Columna, ls_calibre)
			Return 1
		End If
		
	Case "pafr_ccajas"
		li_ccaj = This.Object.pafr_ccajas[row]
		If IsNull(dw_4.Object.pafr_docrel[row]) OR (dw_4.Object.pafr_docrel[row] = dw_2.Object.mfee_docrel[1] AND &
		    dw_4.Object.pafr_tipdoc[row] = dw_2.Object.mfee_tipdoc[1])Then
			
			If IsNull(This.Object.pafr_ccajas[row]) Then
				li_Cajas	=	0
			Else
				li_Cajas	=	This.GetItemNumber(row, "pafr_ccajas")
			End If
			
			li_Cajas	=	This.GetItemNumber(1, "total_cajas") - &
							li_Cajas + Integer(Data)
			
			If li_Cajas > iuo_tipopallet.cajas Then
				MessageBox("Atención", "Cantidad de Cajas Total sobrepasa la~r" + &
								"Cantidad de Cajas del Pallet (" + &
								Trim(String(iuo_tipopallet.cajas, "#,##0")) + &
								").~r~rRevise cantidades registradas.")
				This.SetItem(row, ls_Columna, Integer(ls_Nula))
				
				Return 1
			ElseIf li_Cajas = iuo_tipopallet.Cajas Then
				dw_3.Object.paen_tipopa[1] = 1
			Else
				dw_3.Object.paen_tipopa[1] = 2
			End If
		Else	
			MessageBox("Atención","El registro pertenece a otra Orden de Proceso. Imposible ModIficar.")
			This.SetItem(row, ls_Columna, li_ccaj)
			Return 1
		End If
		
	Case "lote_codigo"
		li_loteing = dw_4.Object.lote_codigo[row]
		If IsNull(dw_4.Object.pafr_docrel[row]) OR (dw_4.Object.pafr_docrel[row] = dw_2.Object.mfee_docrel[1] AND &
		    dw_4.Object.pafr_tipdoc[row] = dw_2.Object.mfee_tipdoc[1])Then
			If Not iuo_Lote.Existe(This.Object.plde_origen[row], &
						Integer(istr_Mant.Argumento[8]), Integer(Data), &
						True, Sqlca) &
				OR Not iuo_Lote.ExisteOrdenProceso(Integer(istr_Mant.Argumento[2]), &
						Integer(istr_Mant.Argumento[5]), Integer(istr_Mant.Argumento[6]), &
						This.Object.plde_origen[row], Integer(istr_Mant.Argumento[8]), &
						Integer(Data), True, Sqlca) Then
				This.SetItem(row, ls_Columna, Integer(ls_Nula))
				
				Return 1
			Else
				This.Object.pafr_huert1[Row] = iuo_Lote.Predio
				This.Object.pafr_cuart1[Row] = iuo_Lote.CentroCosto
				This.Object.pafr_ggncod[Row] = iuo_Lote.GGN
			End If
		Else	
			MessageBox("Atención","El registro pertenece a otra Orden de Proceso. Imposible ModIficar.")
			This.SetItem(row, ls_Columna, li_loteing)
			Return 1
		End If
				
	Case "pafr_huert1"
		If existepredio(dw_2.Object.prod_codigo[1],Integer(data)) Then
			MessageBox("Atención","El predio ingesado no existe.")
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			Return 1
		Else
			This.Object.prbr_codpre[Row] = Integer(Data)
			This.Object.pafr_ggncod[Row] = f_AsignaGGN(dw_2.Object.prod_codigo[1], Integer(Data), dw_2.Object.espe_codigo[1], dw_3.Object.paen_feccon[1])
		End If		
	
	Case "pafr_cuart1"
		If existecuartel(dw_2.Object.prod_codigo[1],dw_4.Object.pafr_huert1[row],Integer(data)) Then
			MessageBox("Atención","El predio ingesado no existe.")
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			Return 1
		Else
			dw_4.Object.prcc_nombre[il_fila] = is_nomcuartel
			This.Object.prcc_codigo[Row] = Integer(Data)
		End If		
			
	Case "vari_codrot"
		If NOT iuo_variedades.existe(dw_2.Object.espe_codigo[1],integer(data),TRUE,SQLCA) Then
			This.SetItem(row, ls_columna, Integer(ls_Nula))
			Return 1
		End If
End Choose

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

		
	CASE "b_cuartel"
		IF NOT isnull(dw_2.Object.prod_codigo[1]) OR NOT isnull(dw_4.Object.pafr_huert1[row]) THEN
			istr_Mant.Argumento[20] = String(dw_4.Object.pafr_huert1[row])
			buscacuartel()
		
		END IF	

END CHOOSE
end event

type dw_3 from uo_dw within w_maed_movtofrutaemba_proceso
integer x = 745
integer y = 476
integer width = 3095
integer height = 636
integer taborder = 20
boolean bringtotop = true
string dataobject = "dw_mant_palletencab_proceso"
boolean vscrollbar = false
boolean border = false
end type

event itemchanged;String	ls_Columna, ls_Null, ls_Fecha
Date     ld_fecha
Datetime ld_fechapru, ldt_fecha
SetNull(ls_Null)

ls_Columna = dwo.name

CHOOSE CASE ls_Columna
	CASE "emba_codigo"
		IF Not cargaembalaje(data) THEN
			
			THIS.object.Emba_codigo[row] = ls_null
			THIS.SetColumn("emba_codigo")
			THIS.SetFocus()			
		ELSE			
			THIS.Object.enva_tipoen[row] = ii_envatipoen
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
		 	idwc_tipopallet.Retrieve(Integer(istr_Mant.Argumento[1]),Data)
			is_embacodigo	=	data
		END IF
		
	CASE "paen_numero"
		pb_eli_det.enabled = FALSE
		pb_ins_det.enabled = FALSE
		IF isnull(dw_2.Object.mfee_docrel[1]) THEN
			MessageBox("Atención","Necesita Ingresar un movimiento o una Orden de Proceso.")
        	This.SetItem(1, ls_Columna, long(ls_Null))
			RETURN 1	
		ELSE	
			IF iuo_spro_palletencab.Existe(Integer(istr_Mant.Argumento[1]), &
													 Integer(istr_Mant.Argumento[2]), &
													 Long(Data),False,SqlCa) THEN
				
					IF iuo_spro_palletencab.especie	= dw_2.Object.espe_codigo[1] THEN
						istr_Mant.Argumento[7]	=	Data
						Parent.TriggerEvent("ue_recuperapallet")
						IF dw_4.RowCount() > 0 THEN
							dw_3.Object.prod_codrot[1] = dw_4.Object.prod_codrot[1]
							IF iuo_productores.Existe(dw_4.Object.prod_codrot[1],False,SqlCa) THEN
								This.SetItem(1, "prod_nomrot", iuo_productores.Nombre)
							END IF
						ELSE
						dw_3.Object.prod_codrot[1] = iuo_spro_ordenproceso.Productor
						IF iuo_productores.Existe(iuo_spro_ordenproceso.Productor,False,SqlCa) THEN
							This.SetItem(1, "prod_nomrot", iuo_productores.Nombre)
						END IF
						END IF	
						lb_existe = TRUE
						
						IF iuo_spro_ordenproceso.Estado < 4 THEN
							pb_ins_det.Enabled		=	True
							pb_eli_det.Enabled		=	True
						ELSE
							pb_ins_det.Enabled		=	False
							pb_eli_det.Enabled		=	False
						END IF
					ELSE
						lb_existe = FALSE
						MessageBox("Error de Datos","El pallet ingresado pertenece a otra especie.")
						this.SetItem(1,"paen_numero",long(ls_Null))
						Return 1
					END IF
			ELSE
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
				IF idwc_variedad.Retrieve(dw_2.Object.espe_codigo[1]) = 0 THEN
					MessageBox("Atención","Falta Registrar Variedades")
				idwc_variedad.InsertRow(0)
				ELSE
					idwc_variedad.SetSort("vari_nombre A")
					idwc_variedad.Sort()
				END IF
				
				dw_3.Object.espe_codigo[1] 	=	dw_2.Object.espe_codigo[1]
				
				IF iuo_productores.Existe(iuo_spro_ordenproceso.Productor,False,SqlCa) THEN
					This.SetItem(1, "prod_nomrot", iuo_productores.Nombre)
				END IF
			END IF
		END IF
		
	CASE "paen_feccon"
		ld_fechapru = datetime(dw_3.Object.paen_feccon[row])
		IF isnull(data) THEN
			MessageBox("Atención","Ingrese una fecha valida.")
			This.SetItem(row, ls_Columna, ld_Fechapru)
			Return 1
		END IF	
		
		IF Date(Mid(data,1,10)) = ld_fecha THEN
			MessageBox("Atención","Ingrese una fecha valida.")
			This.SetItem(row, ls_Columna, ld_Fechapru)
			Return 1
		ELSE
			ldt_Fecha	=	DateTime(Date(Mid(data,1,10)))
			IF NOT iuo_FechaMovto.Valida_FechaMovto(Date(ldt_Fecha)) THEN
				This.SetItem(row, ls_Columna, ld_Fechapru)
				This.SetFocus()
				RETURN 1
			END IF
					
			ls_Fecha	=	Data
			This.SetItem(row, ls_Columna, DateTime(Date(Mid(ls_Fecha,1,10))))
			
		END IF	

	CASE "vari_codigo"
		IF NOT IsNull(dw_2.Object.espe_codigo[1]) THEN
			IF NOT iuo_variedades.Existe(dw_2.Object.espe_codigo[1],Integer(Data),True,SqlCa) THEN
				This.SetItem(1,ls_Columna,Integer(ls_Null))
				RETURN 1
			END IF
		END IF

	CASE "enva_tipoen"
		IF dw_4.RowCount() > 0 THEN
			MessageBox("","Ya posee detalle ingresado para el envase elegido. ~rElimine el detalle o seleccione otro pallet.")
			This.SetItem(1, ls_Columna, this.Object.enva_tipoen[1])
			RETURN 1		
		END IF	
		IF ExisteEnvase(Integer(Data),0,istr_envase) = False THEN
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1
		ELSE
			istr_Mant.Argumento[9]	=	String(istr_envase.TipoEnvase)
			This.SetItem(1, "enva_codigo", Integer(ls_Null))
			This.SetItem(1, "tpen_codigo", ls_Null)

			dw_3.GetChild("enva_codigo", idwc_envase)
			idwc_envase.SetTransObject(SqlCa)
			idwc_envase.Retrieve(Integer(Data))
			idwc_envase.SetSort("enva_codigo A")
			idwc_envase.Sort()
		END IF

	CASE "enva_codigo"
		IF dw_4.RowCount() > 0 THEN
			MessageBox("","Ya posee detalle ingresado para el envase elegido. ~rElimine el detalle o seleccione otro pallet.")
      	This.SetItem(1, ls_Columna, this.Object.enva_codigo[1])
			RETURN 1		
		END IF	

		IF Isnull(this.Object.enva_tipoen[1]) THEN
			MessageBox("Error de Datos","Necesita elegir primero un tipo de envase.")
      	This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1		
		END IF	

		IF ExisteEnvase(This.Object.enva_tipoen[1],Integer(Data),istr_envase) = False THEN
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1
		ELSE
			istr_Mant.Argumento[10]	=	String(istr_envase.Codigo)
			This.SetItem(1, "tpen_codigo", ls_Null)
		END IF

	CASE "tpen_codigo"
		IF iuo_tipopallet.existe_porembalaje(Integer(istr_Mant.Argumento[1]), is_embacodigo, Data, True, SqlCa) THEN
			This.SetItem(1, "paen_altura", iuo_tipopallet.Altura)
		ELSE
			This.SetItem(1, ls_Columna, ls_Null)
			RETURN 1
		END IF

	CASE "cate_codigo"
		IF NOT iuo_categorias.Existe(Integer(Data),True,SqlCa) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1
		ELSE
			ii_categoria	=	Integer(data)
		END IF

	CASE "etiq_codigo"
		IF dw_4.RowCount() > 0 THEN
			MessageBox("","Ya posee detalle ingresado para la etiqueta elegida. ~rElimine el detalle o seleccione otro pallet.")
			This.SetItem(1, ls_Columna, this.Object.etiq_codigo[1])
			RETURN 1		
		END IF
		IF NOT iuo_etiquetas.Existe(Integer(Data),True,SqlCa) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF

	CASE "prod_codrot"
		IF NOT iuo_productores.Existe(Integer(Data),True,SqlCa) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			This.SetItem(1, "prod_nomrot", ls_Null)
			RETURN 1
		ELSE
			This.SetItem(1, "prod_nomrot", iuo_productores.Nombre)
		END IF

	CASE "dest_codigo"
		IF NOT iuo_destinos.Existe(Integer(Data),True,SqlCa) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF

	CASE "reci_codigo"
		IF NOT iuo_recibidores.Existe(Long(Data),True,SqlCa) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF

	CASE "cama_codigo"
		IF NOT iuo_camara.Existe(gstr_paramplanta.codigoplanta,Integer(Data),True,SqlCa) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF
		
	CASE "copa_codigo"
		IF NOT iuo_copa.Existe(Integer(data), True, SQLCa) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF
		
END CHOOSE

HabilitaIngreso()
end event

event buttonclicked;call super::buttonclicked;long ll_fila
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
			
			il_fila = ll_fila
		END IF
		
END CHOOSE
end event

event itemerror;RETURN 1
end event

type dw_6 from datawindow within w_maed_movtofrutaemba_proceso
string tag = "Muestra todos los Detalle de Pallet del Movimiento"
boolean visible = false
integer width = 2971
integer height = 472
integer taborder = 130
string title = "Muestra todos los Detalle de Pallet del Movimiento"
string dataobject = "dw_mant_mues_palletfruta_eliminacion"
end type

type dw_5 from datawindow within w_maed_movtofrutaemba_proceso
string tag = "Muestra todos los Pallet del Movimiento"
boolean visible = false
integer x = 1312
integer y = 1712
integer width = 2971
integer height = 428
integer taborder = 120
boolean bringtotop = true
boolean titlebar = true
string title = "Muestra todos los Pallet del Movimiento"
string dataobject = "dw_mant_mues_palletencab_eliminacion"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type pb_ventanas from uo_botonventanas within w_maed_movtofrutaemba_proceso
integer x = 4421
integer y = 1968
integer width = 302
integer height = 244
integer taborder = 120
boolean bringtotop = true
integer textsize = 0
integer weight = 0
fontpitch fontpitch = default!
fontfamily fontfamily = anyfont!
string facename = ""
string picturename = "\Desarrollo 17\Imagenes\Botones\Adhesivo.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Adhesivo-bn.png"
long ii_proceso = 0
end type

