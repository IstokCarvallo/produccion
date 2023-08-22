$PBExportHeader$w_maed_movtofrutagranel_mantreembala.srw
$PBExportComments$Mantención de Recepción de Fruta Granel de Huerto.
forward
global type w_maed_movtofrutagranel_mantreembala from w_mant_encab_deta_csd
end type
type dw_4 from datawindow within w_maed_movtofrutagranel_mantreembala
end type
type dw_6 from datawindow within w_maed_movtofrutagranel_mantreembala
end type
type cb_guia from commandbutton within w_maed_movtofrutagranel_mantreembala
end type
type tab_1 from tab within w_maed_movtofrutagranel_mantreembala
end type
type tp_1 from userobject within tab_1
end type
type dw_lotes from uo_dw within tp_1
end type
type tp_1 from userobject within tab_1
dw_lotes dw_lotes
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
type tab_1 from tab within w_maed_movtofrutagranel_mantreembala
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
end type
type dw_spro_bins from datawindow within w_maed_movtofrutagranel_mantreembala
end type
type dw_9 from datawindow within w_maed_movtofrutagranel_mantreembala
end type
type dw_exidetaborra from datawindow within w_maed_movtofrutagranel_mantreembala
end type
type dw_exismovtodetanulos from datawindow within w_maed_movtofrutagranel_mantreembala
end type
type dw_exideta from datawindow within w_maed_movtofrutagranel_mantreembala
end type
type dw_exiencab from datawindow within w_maed_movtofrutagranel_mantreembala
end type
type ole_puerta from olecustomcontrol within w_maed_movtofrutagranel_mantreembala
end type
type dw_desverd from datawindow within w_maed_movtofrutagranel_mantreembala
end type
type str_productores_envases from structure within w_maed_movtofrutagranel_mantreembala
end type
type str_pesaje from structure within w_maed_movtofrutagranel_mantreembala
end type
type str_envase from structure within w_maed_movtofrutagranel_mantreembala
end type
end forward

type str_productores_envases from structure
	long		productor[]
	long		guiasii[]
	integer		tipomovto[]
	integer		numero[]
end type

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

type str_envase from structure
	integer		tipenv
	integer		codenv
	string		calcos
	integer		cantid
end type

global type w_maed_movtofrutagranel_mantreembala from w_mant_encab_deta_csd
integer width = 5490
integer height = 3044
string title = "MANTENCION RECEPCION DE HUERTO"
string menuname = ""
boolean minbox = false
event ue_validapassword ( )
event ue_despuesguardar ( )
event ue_despuesborrar ( )
dw_4 dw_4
dw_6 dw_6
cb_guia cb_guia
tab_1 tab_1
dw_spro_bins dw_spro_bins
dw_9 dw_9
dw_exidetaborra dw_exidetaborra
dw_exismovtodetanulos dw_exismovtodetanulos
dw_exideta dw_exideta
dw_exiencab dw_exiencab
ole_puerta ole_puerta
dw_desverd dw_desverd
end type
global w_maed_movtofrutagranel_mantreembala w_maed_movtofrutagranel_mantreembala

type variables
w_mant_deta_lotesfrutagranel_recepcion	iw_mantencion_1
w_mant_deta_movtoenvadeta_recepfruta	iw_mantencion_2

DataWindowChild								idwc_Transp,idwc_Camion,idwc_Variedad,idwc_Predio,&
													idwc_Camara,idwc_especie,idwc_especiedet,idwc_especiedet1,&
													idwc_especiedet2,idwc_planta,idwc_plantadw4,idwc_plancodw4,&
													idwc_plantadw1,idwc_Cliente
DataWindow										dw_3,dw_5,dw_7

Str_mant											istr_mant3, istr_mant4
uo_transportista								iuo_Transport
uo_camiones										iuo_Camion
uo_especie										iuo_Especie
uo_Productores									iuo_Productor
uo_LotesCorrel									iuo_Correlativo
uo_pesoestanespe								iuo_PesoEstanEspe
uo_fechaMovto									iuo_FechaMovto
uo_tipomovtofruta								iuo_TipoMovtoFruta
uo_tipomovtofruta								iuo_TipoMovtoEnva
uo_calicosechero 								iuo_calicosechero
uo_bins											iuo_bins

Long     										il_NumFruta=0, il_NumEnva=0, il_lotes[], il_numeroenva
Boolean											ib_AutoCommit, ib_Salida, ib_Destare, ib_ocx,&
													ib_graba_destare, ib_ConectadoExistencia
String											is_rut, is_rutprod, is_RutProductor, is_NombreProductor, &
													is_chofer
Integer											ii_Cantidad, ii_Productor, ii_kildec=0, ii_especie, &
													ii_TipMov, il_conexiste, il_coneccion
DateTime											idt_FechaSistema
Transaction										sqlexi

Private:
str_Productores_Envases						wstr_Prod_Enva
str_pesaje              					wstr_pesaje
str_pesaje              					wstr_pesajeCarro
str_puertacomm		      					istr_puertacomm
end variables

forward prototypes
public subroutine determina_productoresenvase (integer ai_tipomovto)
public subroutine productoreslotes (ref string productores[])
protected function integer wf_modifica ()
public function boolean chequea_envasesproductor ()
public function boolean otrosmovtosdetalle ()
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine captura_totales ()
public function boolean actual_ultimo_lote ()
public function boolean verificalote (integer ai_planta, integer ai_especie, long al_lote)
public function boolean verificabultos (integer ai_planta, integer ai_loteplanta, integer ai_especie, integer ai_lotecod, integer ai_bultos)
public subroutine destare (boolean ab_actualiza)
public subroutine habilitasalida ()
public subroutine habilitaencab (boolean habilita)
public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, integer al_numero, integer ai_cliente)
public function boolean noexistecliente (integer cliente)
public subroutine actualizakiloslote ()
public subroutine binsapalletfruta ()
public subroutine asignapeso ()
public function boolean procesopacking ()
public function boolean conexionexistencia ()
public function boolean chequea_binscontraenvases ()
public subroutine codigoconexion ()
public subroutine updatemovtogranpesa ()
end prototypes

event ue_validapassword();str_mant	lstr_mant

lstr_mant.Argumento[1]	=	"Granel"
lstr_mant.Argumento[2]	=	gstr_paramplanta.Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public subroutine determina_productoresenvase (integer ai_tipomovto);str_Productores_Envases	lstr_Prod_Enva
DataWindow	ldw_envase
Long			ll_Fila, ll_GuiaSII, ll_Fila_Lote
Integer		li_Secuencia
Long			ll_Productor,ll_ProdAnt 

//Inicializa la Estructura de Instancia
wstr_Prod_Enva	= lstr_Prod_Enva

IF ai_TipoMovto = 41 THEN
	ldw_envase	=	dw_5
ELSE
	ldw_envase	=	dw_7
END IF

FOR ll_Fila	=	1 TO ldw_Envase.RowCount()
	ll_Productor	=	ldw_Envase.Object.prod_codigo[ll_Fila]
	
	IF ll_Productor <> ll_ProdAnt THEN
		ll_ProdAnt	=	ll_Productor
		li_Secuencia ++
		
		IF ai_TipoMovto = 41 THEN
			ll_Fila_Lote	=	dw_3.Find("prod_codigo = "+String(ll_Productor),1,dw_3.RowCount())
			
			IF ll_Fila_Lote > 0 THEN
				ll_GuiaSII	=	dw_3.Object.lote_guisii[ll_Fila_Lote]
			ELSE
				ll_GuiaSII	=	0
			END IF
		END IF
		
		wstr_Prod_Enva.Productor[li_Secuencia]	=	ll_Productor
		wstr_Prod_Enva.GuiaSII[li_Secuencia]	=	ll_GuiaSII
		wstr_Prod_Enva.TipoMovto[li_Secuencia]	=	ai_TipoMovto
	END IF
NEXT

RETURN
end subroutine

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

public function boolean chequea_envasesproductor ();Boolean	lb_Retorno	=	True
Long		ll_Fila,  ll_Fila_det,ll_Productor
Integer	li_TipoEnvase, li_Envase, li_Bultos, li_BultosAnt, li_posicion,&
			li_Bultos_Enc, li_Lote_pltcod, li_Lote_espcod, li_Lote_codigo, li_Total_Bultos
String	ls_linea_chequeo, ls_busqueda

li_Bultos_Enc	=	dw_2.Object.mfge_totbul[1]

FOR ll_Fila = 1 TO dw_3.RowCount()
	
	li_Lote_pltcod	=	dw_3.Object.lote_pltcod[ll_Fila]
	li_Lote_espcod	=	dw_3.Object.lote_espcod[ll_Fila]
	li_Lote_codigo	=	dw_3.Object.lote_codigo[ll_Fila]
	ll_Productor	=	dw_3.Object.prod_codigo[ll_Fila]
	
	dw_6.SetFilter("lote_pltcod = "+String(li_Lote_pltcod)+" and "+&
					   "lote_espcod = "+String(li_Lote_espcod)+" and "+&
						"lote_codigo = "+String(li_Lote_codigo))
	dw_6.Filter()
	
	FOR ll_Fila_det = 1 TO dw_6.RowCount()
		li_TipoEnvase		=	dw_6.Object.enva_tipoen[ll_Fila_det]
		li_Envase			=	dw_6.Object.enva_codigo[ll_Fila_det]
		li_Bultos			=	dw_6.Object.lotd_totbul[ll_Fila_det]
		li_Total_Bultos	=	li_Total_Bultos + li_Bultos
		
		ls_busqueda		= 'P'+String(ll_Productor,'00000')+'T'+String(li_TipoEnvase)+'E'+String(li_Envase,'000')
		li_posicion 	= 	Pos(ls_linea_chequeo,ls_busqueda)
		
		IF li_posicion = 0 THEN
			ls_linea_chequeo	=	ls_linea_chequeo + ls_busqueda + 'B' + String(li_Bultos,'0000') + 'I0000'
		ELSE
			li_BultosAnt		=	Integer(Mid(ls_linea_chequeo,li_posicion + 12, 4))
			li_Bultos			=	li_BultosAnt + li_Bultos
			ls_linea_chequeo	=	Mid(ls_linea_chequeo, 1, li_posicion - 1) + &
										ls_busqueda + 'B' + String(li_Bultos,'0000') + 'I0000' + &
										Mid(ls_linea_chequeo, li_posicion + 21)
		END IF
	NEXT
	dw_6.SetFilter("")
	dw_6.Filter()
NEXT

FOR ll_Fila = 1 TO dw_5.RowCount()
	ll_Productor	=	dw_5.Object.prod_codigo[ll_Fila]
	li_TipoEnvase	=	dw_5.Object.enva_tipoen[ll_Fila]
	li_Envase		=	dw_5.Object.enva_codigo[ll_Fila]
	li_Bultos		=	dw_5.Object.fgme_cantid[ll_Fila]
	
	IF dw_5.Object.fgme_conenv[ll_Fila] = 1 THEN
		ls_busqueda		= 'P'+String(ll_Productor,'00000')+'T'+String(li_TipoEnvase)+'E'+String(li_Envase,'000')
		li_posicion 	= 	Pos(ls_linea_chequeo,ls_busqueda)
		
		IF li_posicion = 0 THEN
			ls_linea_chequeo	=	ls_linea_chequeo + ls_busqueda + 'B0000' + 'I' + String(li_Bultos,'0000') 
		ELSE
			li_BultosAnt		=	Integer(Mid(ls_linea_chequeo,li_posicion + 17, 4))
			li_Bultos			=	li_BultosAnt + li_Bultos
			ls_linea_chequeo	=	Mid(ls_linea_chequeo, 1, li_posicion + 16 ) + &
										String(li_Bultos,'0000') + Mid(ls_linea_chequeo, li_posicion + 21)
		END IF
	END IF
NEXT

////Chequeo de Envases Por Productor
//FOR li_posicion = 1 TO Len(ls_linea_chequeo) STEP 21
//	ls_busqueda		=	Mid(ls_linea_chequeo, li_posicion, 21)
//	li_Bultos		=	Integer(Mid(ls_busqueda, 14, 4))
//	li_BultosAnt	=	Integer(Mid(ls_busqueda, 18, 4))
//	
//	IF li_Bultos <> li_BultosAnt THEN
//		MessageBox("Atención",String(li_BultosAnt,'#,##0')+" Envases con Fruta"+&
//						" declarados para el Productor "+Mid(ls_busqueda, 2, 4)+&
//						" y Envase "+Mid(ls_busqueda, 7, 1)+"-"+Mid(ls_busqueda, 9 ,3)+&
//						" no corresponde "+String(li_Bultos,'#,##0')+" a los Bultos"+&
//						" recepcionados en Detalle de Lotes")
//		//lb_Retorno	=	False
//		//EXIT
//	END IF
//NEXT

IF lb_Retorno THEN
	IF li_Total_Bultos <> li_Bultos_Enc THEN
		MessageBox("Atención","Total Envases con Fruta no corresponde a Total Bultos recepcionados")
		lb_Retorno	=	False
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean otrosmovtosdetalle ();Long	ll_Fila, ll_Fila_Busca

Boolean	lb_Retorno
Integer	li_Camara, li_PlantaLote, li_EspecieLote, li_NumeroLote, &
			li_TipoEnvase, li_Envase, li_Saldo, li_Cantidad

FOR ll_fila = 1 TO dw_6.RowCount()
	
	li_PlantaLote	=	dw_6.Object.lote_pltcod[ll_Fila]
	li_EspecieLote	=	dw_6.Object.lote_espcod[ll_Fila]
	li_NumeroLote	=	dw_6.Object.lote_codigo[ll_Fila]
	li_TipoEnvase	=	dw_6.Object.enva_tipoen[ll_Fila]
	li_Envase		=	dw_6.Object.enva_codigo[ll_Fila]
	li_Cantidad		=	dw_6.Object.lotd_totbul[ll_Fila]

	ll_Fila_Busca	=	dw_3.Find("lote_pltcod = "+String(li_PlantaLote)+" and "+&
										 "lote_espcod = "+String(li_EspecieLote)+" and "+&
										 "lote_codigo = "+String(li_NumeroLote),1,dw_3.RowCount())
	IF ll_Fila_Busca > 0 THEN
		li_Camara		=	dw_3.Object.cama_codigo[ll_Fila_Busca]
	END IF

	SELECT	IsNull(Sum(caex_canbul), 0)
		INTO	:li_Saldo
		FROM	dbo.spro_camaraexistefg
		WHERE	plde_codigo	=	:li_PlantaLote
		AND	cama_codigo	=	:li_Camara
		AND	lote_pltcod	=	:li_PlantaLote
		AND	lote_espcod	=	:li_EspecieLote
		AND	lote_codigo	=	:li_NumeroLote
		AND 	enva_tipoen	=	:li_TipoEnvase
		AND	enva_codigo	=	:li_Envase;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura tabla de Existencia Frigorífico")
		
		lb_Retorno	=	True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Lote en Cámara especificada.~r~r" + &
						"Ingrese o seleccione otros antecedentes.")
		
		lb_Retorno	=	True
		EXIT
	ELSEIF li_Cantidad <> li_Saldo THEN
		MessageBox("Atención", "Lote "+String(li_plantalote,'0000')+String(li_EspecieLote,'00')+&
						String(li_NumeroLote,'0000')+" ha sufrido cambios.  Saldo en Cámara es ~r" + &
						"distinto a la cantidad de bultos recepcionados.~r~r" + &
						"No se puede modificar antecedentes.")
		
		lb_Retorno	=	True
		EXIT
	END IF
NEXT

RETURN lb_Retorno
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

IF NOT Borrando THEN
	IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		dw_2.SetItem(1,"mfge_usuari",gstr_us.nombre)
		dw_2.SetItem(1,"mfge_comput",gstr_us.computador)
		dw_2.SetItem(1,"mfge_horacr",F_FechaHora())
	ELSEIF dw_2.GetItemStatus(1, 0, Primary!) = DataModified! THEN
		dw_2.SetItem(1,"mfge_usumod",gstr_us.nombre)
		dw_2.SetItem(1,"mfge_commod",gstr_us.computador)
		dw_2.SetItem(1,"mfge_horact",F_FechaHora())
		
		IF dw_3.DeletedCount() > 0 THEN Borrando = True
		IF dw_6.DeletedCount() > 0 THEN Borrando = True
	END IF
END IF

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN 								//Detalle
		IF dw_6.Update(True, False) = 1 THEN							//Detalle de Lotes
			IF dw_3.Update(True, False) = 1 THEN						//Lotes
				IF dw_9.Update(True, False) = 1 THEN					//Detalle de Pesaje
					IF dw_2.Update(True, False) = 1 THEN				//Encabezado
						IF dw_desverd.Update(True,False) = 1 THEN			//Envases Recepcionados
//							IF dw_7.Update(True,False) = 1 THEN		//Envases Retirados
//								IF dw_4.Update(True,False) = 1 THEN	//Encabezados Envases
									IF dw_spro_bins.Update(True,False) = 1 THEN
										Commit;
									
										IF sqlca.SQLCode <> 0 THEN
											F_ErrorBaseDatos(sqlca, This.Title)
											RollBack;
										ELSE
											lb_Retorno	=	True
										
											dw_9.ResetUpdate()
											dw_6.ResetUpdate()
											dw_7.ResetUpdate()
											dw_5.ResetUpdate()
											dw_4.ResetUpdate()
											dw_3.ResetUpdate()
											dw_2.ResetUpdate()
											dw_1.ResetUpdate()
											dw_spro_bins.ResetUpdate()
											dw_desverd.ResetUpdate()
										END IF
									ELSE
										F_ErrorBaseDatos(sqlca, This.Title)
										RollBack;
									END IF
//								ELSE
//									F_ErrorBaseDatos(sqlca, This.Title)
//									RollBack;
//								END IF
//							ELSE
//								F_ErrorBaseDatos(sqlca, This.Title)
//								RollBack;
//							END IF
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
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN		 						//Encabezado
		IF dw_3.Update(True, False) = 1 THEN							//Lotes
			IF dw_6.Update(True, False) = 1 THEN						//Detalle de Lotes
				IF dw_1.Update(True, False) = 1 THEN					//Detalle
					IF dw_desverd.Update(True,False) = 1 THEN				//Encabezado Envases
						//IF dw_5.Update(True,False) = 1 THEN			//Envases Recepcionados
							//IF dw_7.Update(True,False) = 1 THEN		//Envases Retirados
								IF dw_9.Update(True,False) = 1 THEN	//Detalle Pesaje
									IF dw_spro_bins.Update(True,False) = 1 THEN
										IF Actual_ultimo_lote() THEN
	
											IF ib_destare THEN //and gstr_paramplanta.packing THEN
												IF PROCESOPACKING() THEN
													MessageBox("Atención", "Se ha terminado el proceso Automático en forma normal.")
													Commit;
												ELSE
													MessageBox("Atención", "El proceso automático ha generado errores, No se ha podido grabar los datos.")
													ROLLBACK;
												END IF
											ELSE
												Commit;
											END IF
										
											IF sqlca.SQLCode <> 0 THEN
												F_ErrorBaseDatos(sqlca, This.Title)
											ELSE
												lb_Retorno	=	True
												
												dw_9.ResetUpdate()
												dw_6.ResetUpdate()
												dw_7.ResetUpdate()
												dw_5.ResetUpdate()
												dw_4.ResetUpdate()
												dw_3.ResetUpdate()
												dw_2.ResetUpdate()
												dw_1.ResetUpdate()
												dw_spro_bins.ResetUpdate()
												dw_desverd.ResetUpdate()
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
//							ELSE
//								F_ErrorBaseDatos(sqlca, This.Title)
//								RollBack;
//							END IF
//						ELSE
//							F_ErrorBaseDatos(sqlca, This.Title)
//							RollBack;
//						END IF
//					ELSE
//						F_ErrorBaseDatos(sqlca, This.Title)
//						RollBack;
//					END IF
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

public subroutine captura_totales ();Long			ll_Total_Bultos, ll_Fila
Decimal{3}	ld_Total_PesoEnv_Ent,ld_Total_PesoEnv_Sal, ld_Total_Neto,&
				ld_Total_KBE, ld_Total_KBS, ld_KBE_Camion, ld_KBE_Carro,&
				ld_KBS_Camion, ld_KBS_Carro, ld_TaraCamion, ld_TaraCarro

dw_2.AcceptText()
dw_3.AcceptText()
dw_5.AcceptText()
dw_7.AcceptText()

ll_Fila	=	dw_3.RowCount()

IF ll_Fila > 0 THEN
	dw_3.GroupCalc()
	IF dw_3.Object.total_bultos[ll_fila] > 10000 THEN
		Messagebox("Error de Consistencia","El total de bultos supera lo permitido")
		RETURN
	ELSE
		ll_Total_Bultos		=	dw_3.GetItemNumber(ll_Fila,"total_bultos")
	END IF
END IF


ll_Fila	=	dw_5.RowCount()

IF ll_Fila > 0 THEN
	dw_5.GroupCalc()
	IF dw_5.Object.total_pesoenv[ll_Fila] >= 100000 THEN
		Messagebox("Error de Consistencia","El peso de los envase supera lo permitido")
		RETURN
	ELSE
		ld_Total_PesoEnv_Ent	=	dw_5.Object.total_pesoenv[ll_Fila]
	END IF
	
	IF dw_5.Object.total_envases[ll_Fila] >= 10000 THEN
		Messagebox("Error de Consistencia","El total de bultos supera lo permitido")
		RETURN
	END IF
END IF

ll_Fila	=	dw_7.RowCount()

IF ll_Fila > 0 THEN
	dw_7.GroupCalc()
	ld_Total_PesoEnv_Sal	=	dw_7.Object.total_pesoenv[ll_Fila]
END IF

//Determina Peso Neto del Camión
IF ib_Salida THEN
	ld_KBE_Camion	=	dw_2.Object.refg_tkbent[1]
	ld_KBE_Carro		=	dw_2.Object.refg_tkbenc[1]
	
	IF Isnull(ld_KBE_Carro) THEN ld_KBE_Carro = 0
	
	ld_Total_KBE	=	ld_KBE_Camion + ld_KBE_Carro
	
	ld_KBS_Camion	=	dw_2.Object.refg_tkbsal[1]
	ld_KBS_Carro	=	dw_2.Object.refg_tkbsac[1]
	ld_TaraCamion	=	iuo_Camion.TaraCamion
	ld_TaraCarro	=	iuo_Camion.TaraCarro
	
	IF Isnull(ld_TaraCamion) THEN ld_TaraCamion = 0
	IF Isnull(ld_TaraCarro) THEN ld_TaraCarro = 0
	
	IF ld_KBS_Camion + ld_KBS_Carro > 0 AND &
		ld_KBS_Camion + ld_KBS_Carro < ld_Total_PesoEnv_Sal THEN
		MessageBox("Atención","Kilos de Salida no pueden ser menor a los Envases de Salida")
		RETURN
	ELSE
		ld_Total_KBS	=	ld_KBS_Camion + ld_KBS_Carro - ld_Total_PesoEnv_Sal
	END IF
		
	ld_Total_Neto	=	ld_Total_KBE - ld_Total_PesoEnv_Ent - ld_Total_KBS
	
	dw_2.Object.refg_tkensa[1]	=	ld_Total_PesoEnv_Sal
	dw_2.Object.mfge_tpneto[1]	=	ld_Total_Neto

END IF

//dw_2.Object.mfge_totbul[1]	=	ll_Total_Bultos
dw_2.Object.refg_tkenen[1]	=	ld_Total_PesoEnv_Ent

IF ib_Destare THEN
	Destare(True)
END IF

RETURN
end subroutine

public function boolean actual_ultimo_lote ();Long 		ll_Filas, ll_Fila, ll_Lote
Integer 	li_Lote_espcod, li_planta
String 	ls_nombrepc

li_Planta	=	Integer(istr_mant.Argumento[1])
ll_Filas	=	dw_3.RowCount()
ll_Fila	=	0
ll_Lote	=	0

DO WHILE ll_Fila < ll_Filas
		ll_Fila = dw_3.GetNextModified(ll_fila, Primary!)
		IF ll_Fila > 0 AND dw_3.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
			IF ll_Lote < dw_3.Object.lote_codigo[ll_Fila] THEN
				ll_lote 			=	dw_3.Object.lote_codigo[ll_Fila]
				li_Lote_espcod	=	dw_3.Object.lote_espcod[ll_Fila]
			END IF
		END IF	
//		ELSE
//			ll_Fila = ll_Fila + 1
//		END IF
LOOP

IF ll_Lote  > 0 THEN
	ls_nombrepc	=	gstr_us.computador
	
	UPDATE	dbo.spro_lotescorrelequipo
		SET	loco_ultcor	=	:ll_Lote
		WHERE	plde_codigo		=	:li_Planta
			AND	espe_codigo		=	:li_Lote_espcod
			AND 	Upper(equi_nombre) 	=	Upper(:ls_nombrepc)
		USING	sqlca;
	
	IF SQLCA.SQLCode = -1 THEN
		F_ErrorBaseDatos(SQLCA,"Actualización último Lote")
		Message.DoubleParm = -1
		SQLCA.AutoCommit	=	ib_AutoCommit
		RETURN FALSE
	END IF
END IF

RETURN TRUE
end function

public function boolean verificalote (integer ai_planta, integer ai_especie, long al_lote);Integer li_planta, li_cantidad

li_planta = dw_2.Object.plde_codigo[1]

SELECT count(*) INTO :li_Cantidad
FROM dbo.spro_movtofrutagrandeta
WHERE plde_codigo = :li_planta
AND lote_pltcod = :ai_planta
AND lote_espcod = :ai_especie
AND lote_codigo = :al_lote;

IF isnull(li_cantidad) THEN li_cantidad = 0

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Recepción Fruta Granel")
	
	RETURN FALSE
ELSEIF li_cantidad > 1 THEN
	MessageBox("Atención","El Lote "+String(al_lote,'0000')+ " posee otros movimientos. No se puede eliminar.")
	RETURN FALSE
END IF

RETURN TRUE
end function

public function boolean verificabultos (integer ai_planta, integer ai_loteplanta, integer ai_especie, integer ai_lotecod, integer ai_bultos);Integer  li_Resultado

	sqlca.AutoCommit = TRUE
	
	DECLARE	ExistenciaBultos PROCEDURE FOR dbo.Fgran_BultosExistencia
				@Planta     =: ai_Planta,
				@LotePld 	=:	ai_loteplanta,
				@Especie    =: ai_especie,
				@LoteCod		=: ai_lotecod,
				@Bultos		=: ai_bultos
		USING SQLCA;
						
	EXECUTE ExistenciaBultos;
	Fetch ExistenciaBultos into :li_Resultado;
	CLOSE ExistenciaBultos;
	
	sqlca.AutoCommit = FALSE
	
IF li_Resultado = 1 THEN
	RETURN TRUE		
ELSE
	RETURN FALSE
END IF
		


end function

public subroutine destare (boolean ab_actualiza);Long			ll_Fila, ll_Fila_lote
Integer		li_Especie, li_TipoEnvase, li_Envase, li_TipoAnt, li_EnvaAnt,&
				li_TotBultos, li_Secuencia, li_Lote, li_LoteAnt, li_BultosLote
Boolean		lb_FueraRango
Date			ld_Fechamovto
Decimal{3}	ld_TotalNeto, ld_PesoEstandar[], ld_PesoMinimo[], ld_PesoMaximo[],&
				ld_TotalEstandar, ld_PesoDistrib, ld_LoteDistrib, ld_NetoLoteEnvase,&
				ld_TotalDistrib, ld_Remanente
Double		ld_Factor

li_Especie		=	dw_2.Object.espe_codigo[1]
ld_FechaMovto	=	dw_2.Object.mfge_fecmov[1]
ld_TotalNeto	=	dw_2.Object.mfge_tpneto[1]

dw_3.SetSort("lote_pltcod A, lote_espcod A, lote_codigo A")
dw_3.Sort()

dw_6.SetSort("lote_pltcod A, lote_espcod A, lote_codigo A, enva_tipoen A, enva_codigo A")
dw_6.Sort()

FOR ll_Fila = 1 TO dw_6.RowCount()
	li_TipoEnvase	=	dw_6.Object.enva_tipoen[ll_Fila]
	li_Envase		=	dw_6.Object.enva_codigo[ll_Fila]
	
	IF li_TipoEnvase <> li_TipoAnt OR li_Envase <> li_EnvaAnt THEN
		IF iuo_PesoEstanEspe.Existe(li_Especie, li_TipoEnvase, li_Envase, ld_FechaMovto, True, SQLCA) THEN
			li_Secuencia ++
			ld_PesoEstandar[li_Secuencia]	=	iuo_PesoEstanEspe.PesoDistrib
			ld_PesoMinimo[li_Secuencia]	=	iuo_PesoEstanEspe.PesoMinimo
			ld_PesoMaximo[li_Secuencia]	=	iuo_PesoEstanEspe.PesoMaximo
		ELSE
			RETURN
		END IF
		li_TipoAnt	=	li_TipoEnvase
		li_EnvaAnt	=	li_Envase
	END IF
	
	li_TotBultos		=	dw_6.Object.lotd_totbul[ll_Fila]
	ld_TotalEstandar	=	ld_TotalEstandar + (li_TotBultos*ld_PesoEstandar[li_Secuencia])
	
NEXT

ld_Factor		=	ld_TotalNeto / ld_TotalEstandar
li_Secuencia	=	0
li_TipoAnt		=	0
li_EnvaAnt		=	0

FOR ll_Fila = 1 TO dw_6.RowCount()
	li_Lote			=	dw_6.Object.lote_codigo[ll_Fila]
	li_TipoEnvase	=	dw_6.Object.enva_tipoen[ll_Fila]
	li_Envase		=	dw_6.Object.enva_codigo[ll_Fila]
	
	IF li_Lote <> li_LoteAnt THEN
		IF li_LoteAnt > 0 THEN
			ld_PesoDistrib = (ld_LoteDistrib / li_BultosLote)
			IF ab_actualiza THEN
				dw_3.Object.lote_kilpro[ll_Fila_lote]	=	ld_PesoDistrib
				dw_3.Object.lote_totnet[ll_Fila_lote]	=	ld_LoteDistrib
			END IF
			IF lb_FueraRango THEN
				dw_3.Object.font[ll_Fila_lote]	=	1
			ELSE
				dw_3.Object.font[ll_Fila_lote]	=	0
			END IF
		END IF
		ll_Fila_lote ++
		li_BultosLote	=	dw_3.Object.lote_totbul[ll_Fila_lote]
		ld_LoteDistrib	=	0
		li_LoteAnt		=	li_Lote
		lb_FueraRango	=	False
	END IF
	
	IF li_TipoEnvase <> li_TipoAnt OR li_Envase <> li_EnvaAnt THEN
		li_Secuencia ++
		li_TipoAnt	=	li_TipoEnvase
		li_EnvaAnt	=	li_Envase
	END IF
	
	li_TotBultos		=	dw_6.Object.lotd_totbul[ll_Fila]
	ld_PesoDistrib		=	(ld_PesoEstandar[li_Secuencia] * ld_Factor)
	ld_NetoLoteEnvase	=	(ld_PesoDistrib * li_TotBultos)
	IF ld_PesoDistrib < ld_PesoMinimo[li_Secuencia] OR ld_PesoDistrib > ld_PesoMaximo[li_Secuencia] THEN
		dw_6.Object.font[ll_Fila]	=	1
		lb_FueraRango	=	True
	ELSE
		dw_6.Object.font[ll_Fila]	=	0
	END IF
	IF ab_actualiza THEN
		dw_6.Object.lotd_kilpro[ll_Fila]	=	ld_PesoDistrib
		dw_6.Object.lotd_totnet[ll_Fila]	=	ld_NetoLoteEnvase
	END IF
	ld_LoteDistrib		=	ld_LoteDistrib + ld_NetoLoteEnvase
	ld_TotalDistrib	=	ld_TotalDistrib + ld_NetoLoteEnvase
NEXT

IF li_LoteAnt > 0 THEN
	ld_PesoDistrib = (ld_LoteDistrib / li_BultosLote)
	IF ab_actualiza THEN
		dw_3.Object.lote_kilpro[ll_Fila_lote]	=	ld_PesoDistrib
		dw_3.Object.lote_totnet[ll_Fila_lote]	=	ld_LoteDistrib
	END IF
	IF lb_FueraRango THEN
		dw_3.Object.font[ll_Fila_lote]	=	1
	ELSE
		dw_3.Object.font[ll_Fila_lote]	=	0
	END IF
END IF

IF ab_actualiza THEN
	ld_Remanente = ld_TotalDistrib - ld_TotalNeto
	
	IF ld_Remanente <> 0 THEN
		ld_NetoLoteEnvase	=	dw_6.Object.lotd_totnet[dw_6.RowCount()]
		dw_6.Object.lotd_totnet[dw_6.RowCount()]	=	ld_NetoLoteEnvase - ld_Remanente
		
		ld_NetoLoteEnvase	=	dw_3.Object.lote_totnet[dw_3.RowCount()]
		dw_3.Object.lote_totnet[dw_3.RowCount()]	=	ld_NetoLoteEnvase - ld_Remanente
	END IF
	
	ib_Destare			=	True
	pb_grabar.Enabled	=	True
END IF
end subroutine

public subroutine habilitasalida ();dw_2.Object.refg_tkbsal.Protect				=	0
dw_2.Object.refg_tkbsal.BackGround.Color	=	RGB(255,255,255)
dw_2.Object.refg_tkbsac.Protect				=	0
dw_2.Object.refg_tkbsac.BackGround.Color	=	RGB(255,255,255)
tab_1.tp_3.Enabled					=	True

dw_2.Object.refg_tkbent.Protect					=	1
dw_2.Object.refg_tkbent.BackGround.Color		=	RGB(192,192,192)
dw_2.Object.refg_tkbenc.Protect					=	1
dw_2.Object.refg_tkbenc.BackGround.Color		=	RGB(192,192,192)
dw_2.Object.mfge_totbul.Protect					=	1
dw_2.Object.mfge_totbul.BackGround.Color		=	RGB(192,192,192)

ib_Salida						=	True

dw_2.Object.destare.Text	=	'Destare'
//
//idt_FechaSistema				=	F_FechaHora()
//
//dw_2.Object.fecha_sal[1]	=	DateTime(Date(idt_FechaSistema))
//dw_2.Object.refg_horasa[1]	=	idt_FechaSistema	

pb_grabar.Enabled				=	False
pb_imprimir.Enabled			=	False

captura_totales()
end subroutine

public subroutine habilitaencab (boolean habilita);If Habilita Then
	dw_2.Object.mfge_numero.Protect				=	0
	dw_2.Object.mfge_numero.Color					=	0
	dw_2.Object.mfge_numero.BackGround.Color	=	RGB(255,255,255)
	
	dw_2.Object.espe_codigo.Protect				=	1
	dw_2.Object.tran_codigo.Protect				=	1
	dw_2.Object.cami_patent.Protect				=	1
	dw_2.Object.cami_patcar.Protect				=	1
	dw_2.Object.mfge_rutcho.Protect				=	1
	dw_2.Object.mfge_chofer.Protect				=	1
	dw_2.Object.mfge_totbul.Protect				=	1
	dw_2.Object.refg_tkbent.Protect				=	1
	dw_2.Object.refg_tkbenc.Protect				=	1
	dw_2.Object.mfge_fecmov.Protect				=	1
	dw_2.Object.refg_horaen.Protect				=	1
	dw_2.Object.mfge_totbul.Protect				=	1
	dw_2.Object.refg_tkbsal.Protect				=	1
	dw_2.Object.refg_tkbsac.Protect				=	1
	dw_2.Object.refg_horasa.Protect				=	1
	dw_2.Object.refg_fecsal.Protect				=	1

	dw_2.Object.espe_codigo.Color				=	RGB(255,255,255)
	dw_2.Object.tran_codigo.Color					=	RGB(255,255,255)
	dw_2.Object.cami_patent.Color				=	RGB(255,255,255)
	dw_2.Object.cami_patcar.Color				=	RGB(255,255,255)
	dw_2.Object.mfge_rutcho.Color				=	RGB(255,255,255)
	dw_2.Object.mfge_chofer.Color				=	RGB(255,255,255)
	dw_2.Object.mfge_totbul.Color					=	RGB(255,255,255)
	dw_2.Object.refg_tkbent.Color					=	RGB(255,255,255)
	dw_2.Object.refg_tkbenc.Color					=	RGB(255,255,255)
	dw_2.Object.mfge_fecmov.Color				=	RGB(255,255,255)
	dw_2.Object.refg_horaen.Color				=	RGB(255,255,255)
	dw_2.Object.mfge_totbul.Color					=	RGB(255,255,255)
	dw_2.Object.refg_tkbsal.Color					=	RGB(255,255,255)
	dw_2.Object.refg_tkbsac.Color					=	RGB(255,255,255)
	dw_2.Object.refg_horasa.Color					=	RGB(255,255,255)
	dw_2.Object.refg_fecsal.Color					=	RGB(255,255,255)

	dw_2.Object.espe_codigo.BackGround.Color=	553648127
	dw_2.Object.tran_codigo.BackGround.Color	=	553648127
	dw_2.Object.cami_patent.BackGround.Color	=	553648127
	dw_2.Object.cami_patcar.BackGround.Color	=	553648127
	dw_2.Object.mfge_rutcho.BackGround.Color=	553648127
	dw_2.Object.mfge_chofer.BackGround.Color=	553648127
	dw_2.Object.mfge_totbul.BackGround.Color	=	553648127
	dw_2.Object.refg_tkbent.BackGround.Color	=	553648127
	dw_2.Object.refg_tkbenc.BackGround.Color	=	553648127
	dw_2.Object.mfge_fecmov.BackGround.Color=	553648127
	dw_2.Object.refg_horaen.BackGround.Color	=	553648127
	dw_2.Object.mfge_totbul.BackGround.Color	=	553648127
	dw_2.Object.refg_tkbsal.BackGround.Color	=	553648127
	dw_2.Object.refg_tkbsac.BackGround.Color	=	553648127
	dw_2.Object.refg_horasa.BackGround.Color	=	553648127
	dw_2.Object.refg_fecsal.BackGround.Color	=	553648127

	Tab_1.Tp_3.Enabled								=	False
ELSE
	dw_2.Object.mfge_numero.Protect			=	1
	dw_2.Object.espe_codigo.Protect				=	1
	dw_2.Object.tran_codigo.Protect				=	1
	dw_2.Object.cami_patent.Protect				=	1
	dw_2.Object.cami_patcar.Protect				=	1
	dw_2.Object.mfge_rutcho.Protect				=	1
	dw_2.Object.mfge_chofer.Protect				=	1
	
	dw_2.Object.mfge_numero.Color				=	RGB(255,255,255)
	dw_2.Object.espe_codigo.Color				=	RGB(255,255,255)
	dw_2.Object.tran_codigo.Color					=	RGB(255,255,255)
	dw_2.Object.cami_patent.Color				=	RGB(255,255,255)
	dw_2.Object.cami_patcar.Color				=	RGB(255,255,255)
	dw_2.Object.mfge_rutcho.Color				=	RGB(255,255,255)
	dw_2.Object.mfge_chofer.Color				=	RGB(255,255,255)
	
	dw_2.Object.mfge_numero.BackGround.Color	=	553648127
	dw_2.Object.espe_codigo.BackGround.Color	=	553648127
	dw_2.Object.tran_codigo.BackGround.Color		=	553648127
	dw_2.Object.cami_patent.BackGround.Color		=	553648127
	dw_2.Object.cami_patcar.BackGround.Color		=	553648127
	dw_2.Object.mfge_rutcho.BackGround.Color	=	553648127
	dw_2.Object.mfge_chofer.BackGround.Color	=	553648127
	
	dw_2.Object.refg_tkbent.Protect				=	0
	dw_2.Object.refg_tkbenc.Protect				=	0
	dw_2.Object.refg_tkbsal.Protect				=	0
	dw_2.Object.refg_tkbsac.Protect				=	0
	dw_2.Object.mfge_totbul.Protect				=	0
	
	dw_2.Object.refg_tkbent.Color					=	0
	dw_2.Object.refg_tkbenc.Color					=	0
	dw_2.Object.refg_tkbsal.Color					=	0
	dw_2.Object.refg_tkbsac.Color					=	0
	dw_2.Object.mfge_totbul.Color					=	0
	
	dw_2.Object.refg_tkbent.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.refg_tkbenc.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.refg_tkbsal.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.refg_tkbsac.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.mfge_totbul.BackGround.Color	=	RGB(255,255,255)	

	Tab_1.Tp_3.Enabled								=	True
End If
end subroutine

public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, integer al_numero, integer ai_cliente);Long		ll_Numero
Integer	li_estado
Boolean	lb_Retorno = True

SELECT	mfge_numero, mfge_estmov
	INTO	:ll_Numero, :li_estado
	FROM	dbo.spro_movtofrutagranenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfge_numero	=	:al_Numero 
	AND   clie_codigo =  :ai_cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Recepción Fruta Granel")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	IF li_estado <> 3 THEN
		MessageBox("Atención","Número de Correlativo no ha sido destarado. Ingrese Otro.")
		lb_Retorno	=	False
	ELSE
		istr_mant.Argumento[3]	=	String(al_Numero)
		This.TriggerEvent("ue_recuperadatos")
	END IF
ELSE
	MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

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

public subroutine actualizakiloslote ();Long			ll_Fila, ll_Filas
Integer		li_Planta_Lote, li_Especie_Lote, li_Numero_Lote
Decimal{3}	ld_Kilos_Neto
Boolean		lb_Actualiza

uo_duchacontrol	luo_duchacontrol

luo_duchacontrol	=	Create uo_duchacontrol

ll_Filas	=	dw_3.RowCount()

ib_AutoCommit		=	SQLCA.AutoCommit
SQLCA.AutoCommit	=	False

FOR	ll_Fila	=	1 TO ll_Filas
	IF dw_3.Object.lote_ducha[ll_Fila]	=	1 THEN
		li_Planta_Lote		=	dw_3.Object.lote_pltcod[ll_Fila]
		li_Especie_Lote	=	dw_3.Object.lote_espcod[ll_Fila]
		li_Numero_Lote		=	dw_3.Object.lote_codigo[ll_Fila]
		ld_Kilos_Neto		=	dw_3.Object.lote_totnet[ll_Fila]
		
		IF luo_duchaControl.TieneDuchaLote(li_Planta_Lote, li_Especie_Lote, li_Numero_Lote, False, SQLCA) THEN
			IF luo_duchaControl.ActualizaKilosLote(li_Planta_Lote, li_Especie_Lote, li_Numero_Lote, ld_Kilos_Neto, SQLCA) THEN
				lb_actualiza	=	True
			END IF
		END IF
	END IF
NEXT

Destroy luo_duchacontrol

IF lb_Actualiza THEN
	Commit;
END IF

sqlca.AutoCommit	=	ib_AutoCommit

RETURN
end subroutine

public subroutine binsapalletfruta ();Integer li_filas_dw3, li_filas_dw2, li_bulto

dw_2.RowsMove(1, dw_2.RowCount(), Primary!, dw_2, 1, Delete!)

FOR li_filas_dw3 = 1 TO dw_3.RowCount()
	
	li_filas_dw2	=	dw_2.find( 'enva_tipoen = ' + String(dw_3.Object.enva_tipoen[li_filas_dw3]) + ' and ' +&
									   'enva_codigo = ' + String(dw_3.Object.enva_codigo[li_filas_dw3]), 1, dw_2.RowCount() )
										
	IF li_filas_dw2 < 1 THEN
		li_filas_dw2	=	dw_2.InsertRow(0)
		dw_2.Object.lote_pltcod[li_filas_dw2]	=	Integer(istr_mant.Argumento[1])
		dw_2.Object.lote_espcod[li_filas_dw2]	=	Integer(istr_mant.Argumento[5])
		dw_2.Object.lote_codigo[li_filas_dw2]	=	dw_3.Object.lote_codigo[li_filas_dw3]
		dw_2.Object.enva_tipoen[li_filas_dw2]	=	dw_3.Object.enva_tipoen[li_filas_dw3]
		dw_2.Object.enva_codigo[li_filas_dw2]	=	dw_3.Object.enva_codigo[li_filas_dw3]
	END IF
	
	li_bulto 											=	dw_2.Object.lotd_totbul[li_filas_dw2]
	
	IF IsNull(li_bulto) THEN li_bulto = 0
	
	dw_2.Object.lotd_totbul[li_filas_dw2] 	=	li_bulto + 1
	
NEXT
end subroutine

public subroutine asignapeso ();Long 		ll_Fila
Decimal 	ld_pesos = 0

IF dw_9.RowCount() > 0 THEN
	FOR ll_Fila = 1 TO dw_9.RowCount()
		//IF dw_9.Object.mfgp_estado[ll_Fila] = 2 THEN
			ld_pesos =	ld_pesos  + dw_9.Object.mfgp_pesore[ll_fila]
		//END IF
	NEXT
END IF
dw_2.Object.refg_tkbent[1]	 = ld_pesos
end subroutine

public function boolean procesopacking ();integer 	li_clie_codigo, li_i, li_hora, li_mins, li_mfge_numero
date 		ld_date_actual

//---------------variables para spro_ordenprocvacdeta
String 	ls_cale_calida
Decimal	ld_KilosRecep, ld_Tara, ld_NetoOriginal, ld_Neto, ld_KilosPromedio
time 		lt_hora_ivaciado, lt_hora_tvaciado

//---------------variables para lotefrutagranel
integer 	li_vari_codigo, li_prod_codigo, li_proceso

//---------------variables para lotefrutagrandeta
integer 	li_plde_codigo, li_lote_espcod, li_lote_codigo, li_enva_tipoen, &
		  	li_enva_codigo, li_lotd_totbul, li_lotd_totnet, li_lotd_kilpro
			  
//---------------variables para orden de proceso 
integer 	li_orpr_tipoord, li_orpr_numero

//---------------variables para programa de proceso 
integer 	li_ppre_numero, li_ppre_feccre

li_clie_codigo 	= 	dw_2.Object.clie_codigo[1]
li_mfge_numero 	= 	dw_2.Object.mfge_numero[1]

li_plde_codigo 	= 	dw_6.Object.lote_pltcod[1]
li_lote_espcod		= 	dw_6.Object.lote_espcod[1]
li_lote_codigo		= 	dw_6.Object.lote_codigo[1]
li_enva_tipoen		= 	dw_6.Object.enva_tipoen[1]
li_enva_codigo		= 	dw_6.Object.enva_codigo[1]
li_lotd_totbul		= 	dw_6.Object.lotd_totbul[1]
li_lotd_totnet		= 	dw_6.Object.lotd_totnet[1]
li_lotd_kilpro		= 	dw_6.Object.lotd_kilpro[1]

SELECT	vari_codigo, prod_codigo, lote_totnet
	INTO	:li_vari_codigo, :li_prod_codigo, :ld_kilosRecep
	FROM	dbo.spro_lotesfrutagranel as lote
	WHERE 	lote.lote_pltcod 	= :li_plde_codigo
		AND 	lote.lote_espcod	= :li_lote_espcod
		AND 	lote.lote_codigo	= :li_lote_codigo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla LotesFrutaGranel")
	Return FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Encabezado del lote no ha sido generado, se aborta el proceso. Ingrese Otro.")
	Return FALSE
END IF

SELECT	distinct orpr_numero
	INTO	:li_proceso
	FROM	dbo.spro_ordenprocdeta as lote
	WHERE 	lote.lote_pltcod 	= :li_plde_codigo
		AND 	lote.lote_espcod	= :li_lote_espcod
		AND 	lote.lote_codigo	= :li_lote_codigo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocdeta")
	Return FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","El lote no esta asociado a ningun proceso. Ingrese Otro.")
	Return FALSE
END IF


ld_date_actual 	= 	DATE(STRING(TODAY(), "dd/mm/yyyy"))
li_hora 				= 	Hour(now())
li_mins 				= 	Minute(now())
lt_hora_ivaciado 	= 	time(li_hora, li_mins,0)
lt_hora_tvaciado 	= 	time(li_hora + 1, li_mins,0)

//*********************************** tablas de encabezados
//No es necesario actualizar tablas de encabezados
//*********************************** tablas de detalles
For Li_i = 1 to dw_6.RowCount()	
	li_hora = Hour(now())
	li_mins = Minute(now())
	lt_hora_ivaciado = time(li_hora, li_mins + li_i,0)
	lt_hora_tvaciado = time(li_hora + 1, li_mins + li_i,0)
	
	li_plde_codigo = dw_6.Object.lote_pltcod[li_i]
	li_lote_espcod	= dw_6.Object.lote_espcod[li_i]
	li_lote_codigo	= dw_6.Object.lote_codigo[li_i]
	li_enva_tipoen	= dw_6.Object.enva_tipoen[li_i]
	li_enva_codigo	= dw_6.Object.enva_codigo[li_i]
	li_lotd_totbul	= dw_6.Object.lotd_totbul[li_i]
	li_lotd_totnet	= dw_6.Object.lotd_totnet[li_i]
	li_lotd_kilpro	= dw_6.Object.lotd_kilpro[li_i]
	
	UPDATE dbo.spro_ordenproceso SET
	orpr_canbul =:li_lotd_totbul
	WHERE clie_codigo =:li_clie_codigo and
			plde_codigo	=:li_plde_codigo and
			orpr_tipord =7 and
		   orpr_numero =:li_proceso;
			 
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Actualización de Tabla spro_ordenproceso")
		Return FALSE
	ELSEIF sqlca.SQLCode <> 0 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF
	
	
	IF li_clie_codigo = 81 THEN
		ls_cale_calida = "1"
	ELSE
		ls_cale_calida = "2"
	END IF
	
	
	UPDATE dbo.spro_terminoproceso  SET
	tepr_bultra = :li_lotd_totbul,
	tepr_bulvac =:li_lotd_totbul
	WHERE plde_codigo	= 	:li_plde_codigo AND
			tpmv_codigo	= 	26 AND 
			tepr_numero = 	:li_proceso AND
			tepr_secuen = 	1 AND
			lote_pltcod = 	:li_plde_codigo AND
			lote_espcod = 	:li_lote_espcod AND   
			lote_codigo =	:li_lote_codigo AND
			lote_secuen	=	:li_i AND 
			enva_tipoen	=	:li_enva_tipoen AND
			enva_codigo	=	:li_enva_codigo AND
			clie_codigo	=	:li_clie_codigo;
			
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Actualización de Tabla spro_terminoproceso")
		Return FALSE
	ELSEIF sqlca.SQLCode <> 0 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF

	UPDATE dbo.spro_movtofrutagrandeta SET
		mfgd_bulent = :li_lotd_totbul,
		mfgd_kgnent =:li_lotd_totnet WHERE
		plde_codigo	=:li_plde_codigo AND
		tpmv_codigo	=26 AND
		mfge_numero	=:li_mfge_numero AND
		clie_codigo	=:li_clie_codigo AND
		mfgd_secuen	=:Li_i AND
		plde_coorde	=:li_plde_codigo AND
		cama_codigo	=1 AND
		lote_pltcod	=:li_plde_codigo AND
		lote_espcod =:li_lote_espcod AND
		lote_codigo	=:li_lote_codigo AND
		enva_tipoen	=:li_enva_tipoen AND
		enva_codigo	=:li_enva_codigo;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Actualización de Tabla spro_movtofrutagrandeta")
		Return FALSE
	ELSEIF sqlca.SQLCode <> 0 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF

	
	UPDATE dbo.spro_ordenprocdeta SET 
		orpd_canbul = :li_lotd_totbul WHERE
		plde_codigo	=: li_plde_codigo AND
		orpr_tipord	= 7 AND
		orpr_numero	=: li_proceso AND
		clie_codigo	=: li_clie_codigo AND
		orpd_secuen	=: Li_i AND
		lote_pltcod	=: li_plde_codigo AND
		lote_espcod	=: li_lote_espcod AND
		lote_codigo =: li_lote_codigo ;
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Actuaclización de Tabla spro_ordenprocdeta")
		Return FALSE
	ELSEIF sqlca.SQLCode <> 0 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF
	
	ld_NetoOriginal	=	ld_KilosRecep
	ld_Neto				=	ld_NetoOriginal
	ld_KilosPromedio	=	ld_Neto / li_lotd_totbul
	ld_KilosRecep		=	ld_KilosRecep + ( ld_tara * li_lotd_totbul )
	
	UPDATE dbo.spro_ordenprocvacdeta SET
		opvd_canbul =:li_lotd_totbul,
		opvd_kilpro =:ld_KilosPromedio,
		opvd_kilori	=:ld_NetoOriginal, 
		opvd_pesobr	=:ld_KilosRecep,
		opvd_pesone	=:ld_Neto WHERE 
		plde_codigo	=:li_plde_codigo AND
		orpr_tipord	=7 AND
		orpr_numero	=:li_proceso AND
		clie_codigo	=:li_clie_codigo AND
		lote_pltcod	=:li_plde_codigo AND
		lote_espcod	=:li_lote_espcod AND
		lote_codigo	=:li_lote_codigo;
	  
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Actualización de Tabla spro_ordenprocvacdeta")
		Return FALSE
	ELSEIF sqlca.SQLCode <> 0 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF

  	UPDATE dbo.spro_movtofrutagranenca 
	SET mfge_estmov = 2  
	WHERE ( dbo.spro_movtofrutagranenca.plde_codigo = :li_plde_codigo ) AND  
			  ( dbo.spro_movtofrutagranenca.clie_codigo = :li_clie_codigo ) AND  
			  ( dbo.spro_movtofrutagranenca.tpmv_codigo = 26 ) AND  
			  ( dbo.spro_movtofrutagranenca.mfge_numero = :li_lote_codigo );
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagranenca")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF


	UPDATE dbo.spro_ordenproceso  
   	SET orpr_estado = 2  
	WHERE ( dbo.spro_ordenproceso.plde_codigo = :li_plde_codigo ) AND  
      	( dbo.spro_ordenproceso.clie_codigo = :li_clie_codigo ) AND  
			( dbo.spro_ordenproceso.orpr_tipord = 7 ) AND  
         ( dbo.spro_ordenproceso.orpr_numero = :li_proceso ) ;
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenproceso")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF
	
NEXT 

RETURN TRUE
end function

public function boolean conexionexistencia ();ib_ConectadoExistencia	=	False

return ib_ConectadoExistencia
end function

public function boolean chequea_binscontraenvases ();Integer	li_fila, li_str, li_tipenv, li_codenv, li_cantid
Long		ll_prod[]
String	ls_calcos, ls_mensaje
boolean	lb_Respuesta = TRUE, lb_existe
str_envase	lstr_env[]
DataStore	lds_bins
uo_bins		luo_bins

luo_bins						=	Create uo_bins
lds_bins						=	Create DataStore
lds_bins.DataObject		=	dw_spro_bins.DataObject

dw_spro_bins.RowsCopy(1, dw_spro_bins.RowCount(), Primary!, lds_bins, 1, Primary!)
lds_bins.SetSort("enva_tipoen asc, enva_codigo asc, cale_calida asc")
lds_bins.Sort()

IF lds_bins.RowCount() 	= 	0 THEN RETURN lb_respuesta

li_str						=	1
li_fila						=	1
lstr_env[li_str].tipenv = 	lds_bins.Object.enva_tipoen[li_fila]
lstr_env[li_str].codenv = 	lds_bins.Object.enva_codigo[li_fila]
lstr_env[li_str].calcos = 	lds_bins.Object.cale_calida[li_fila]

FOR li_fila = 1 TO dw_3.RowCount()
	ll_prod[li_fila]	=	dw_3.Object.prod_codigo[li_fila]
NEXT

FOR li_fila = 1 TO lds_bins.RowCount()
	
	IF NOT luo_bins.Existe(lds_bins.Object.clie_codigo[li_fila],&
					 			  lds_bins.Object.plde_codigo[li_fila],&
							 	  lds_bins.Object.bins_numero[li_fila],sqlca,True) THEN
		RETURN FALSE
	END IF
	
	IF lstr_env[li_str].tipenv <> luo_bins.enva_tipoen OR &
		lstr_env[li_str].codenv <> luo_bins.enva_codigo OR &
		lstr_env[li_str].calcos <> luo_bins.cale_calida THEN
		
		li_str						=	li_str + 1
		lstr_env[li_str].tipenv = 	luo_bins.enva_tipoen
		lstr_env[li_str].codenv = 	luo_bins.enva_codigo
		lstr_env[li_str].calcos = 	luo_bins.cale_calida
		
		IF NOT gstr_paramplanta.palletdebins THEN
			lstr_env[li_str].cantid = 	1
		ELSE
			lstr_env[li_str].cantid =	lds_bins.Object.fgmb_canbul[li_fila]
		END IF
		
	ELSE
		
		IF NOT gstr_paramplanta.palletdebins THEN
			lstr_env[li_str].cantid =	lstr_env[li_str].cantid + 1
		ELSE
			lstr_env[li_str].cantid =	lstr_env[li_str].cantid + lds_bins.Object.fgmb_canbul[li_fila]
		END IF
		
	END IF

NEXT

lds_bins.SetSort("fgmb_tibapa asc")
lds_bins.Sort()

FOR li_fila = 1 TO lds_bins.RowCount()
	
	IF lds_bins.Object.fgmb_tibapa[li_fila] > 0 OR &
		Not IsNull(lds_bins.Object.fgmb_tibapa[li_fila]) THEN
		IF NOT luo_bins.Existe(lds_bins.Object.clie_codigo[li_fila],&
									  lds_bins.Object.plde_codigo[li_fila],&
									  lds_bins.Object.fgmb_tibapa[li_fila],sqlca,True) THEN
			RETURN FALSE
		END IF
		
		IF lstr_env[li_str].tipenv <> luo_bins.enva_tipoen OR &
			lstr_env[li_str].codenv <> luo_bins.enva_codigo OR &
			lstr_env[li_str].calcos <> luo_bins.cale_calida THEN
			
			li_str						=	li_str + 1
			lstr_env[li_str].tipenv = 	luo_bins.enva_tipoen
			lstr_env[li_str].codenv = 	luo_bins.enva_codigo
			lstr_env[li_str].calcos = 	luo_bins.cale_calida
			lstr_env[li_str].cantid = 	1
			
		ELSE
			lstr_env[li_str].cantid =	lstr_env[li_str].cantid + 1
			
		END IF
	END IF
NEXT


FOR li_fila = 1 TO dw_5.RowCount()
	
	li_tipenv	=	dw_5.Object.enva_tipoen[li_fila]
	li_codenv 	= 	dw_5.Object.enva_codigo[li_fila]
	ls_calcos 	= 	dw_5.Object.cale_calida[li_fila]
	li_cantid	=	dw_5.Object.fgme_cantid[li_fila]
	
	FOR li_str = 1 TO UpperBound(lstr_env)
		IF lstr_env[li_str].tipenv = li_tipenv AND &
			lstr_env[li_str].codenv = li_codenv AND &
			lstr_env[li_str].calcos = ls_calcos THEN
			
			lb_existe = TRUE
			IF lstr_env[li_str].cantid <> li_cantid THEN
				ls_mensaje = ls_mensaje + "* La cantidad de envases ("+String(li_tipenv)+")("+String(li_codenv)+")("+ls_calcos+") "
				ls_mensaje = ls_mensaje + "ingresada no corresponde con los bins ingresados con esas caracteristicas.~r~n"
			END IF			
		END IF
	NEXT
	IF NOT lb_existe THEN
		ls_mensaje = ls_mensaje + "* Los "+String(li_cantid)+" envases ingresados ("+String(li_tipenv)+")("+String(li_codenv)+")("+ls_calcos+") "
		ls_mensaje = ls_mensaje + "no corresponde a las caractisticas de ningún Bins ingresado.~r~n"
	END IF
	
	lb_existe = False
	FOR li_str = 1 TO UpperBound(ll_prod[])
		IF dw_5.Object.prod_codigo[li_fila]	=	ll_prod[li_str] THEN
			lb_existe = TRUE
		END IF
	NEXT
	IF NOT lb_existe THEN
		ls_mensaje = ls_mensaje + "* El productor ("+String(dw_5.Object.prod_codigo[li_fila])+") con envases ingresados ("+String(li_tipenv)+")("+String(li_codenv)+")("+ls_calcos+") "
		ls_mensaje = ls_mensaje + "no corresponde a el o los productores de los lotes recepcionados.~r~n"
	END IF
	
	lb_existe = False
NEXT

IF Len(ls_mensaje) > 0 THEN
	lb_respuesta = False
	MessageBox("Error de Consistencia", "Se han detectado los siguientes errores con los envases ingresados:~r~n" + ls_mensaje, StopSign!)
ELSE
	lb_respuesta = True
END IF
RETURN lb_Respuesta
end function

public subroutine codigoconexion ();Integer	li_cliente
li_Cliente	=	dw_2.Object.clie_codigo[1]
		
SELECT 	clie_conexi, cone_codigo
INTO   	:il_conexiste, :il_coneccion
FROM dbo.clientesprod
WHERE clie_codigo = :li_Cliente;
end subroutine

public subroutine updatemovtogranpesa ();Long	ll_fila, ll_FindLote

FOR ll_fila = 1 TO dw_9.RowCount()
	
	ll_FindLote	=	dw_3.Find("lote_codigo = " + String(dw_9.Object.lote_codigo[ll_fila]), 1, dw_3.RowCount())
	
	IF ll_FindLote	> 0 THEN
		
		iuo_bins.Existe(dw_2.Object.clie_codigo[1],&
							 dw_2.Object.plde_codigo[1],&
							 dw_9.Object.bins_numero[ll_fila], sqlca, TRUE)
		
		dw_9.Object.mfgp_pesore[ll_fila]	=	Dec(dw_3.Object.lote_kilpro[ll_FindLote] + iuo_bins.cale_pesoen)
		dw_9.Object.mfgp_valref[ll_fila]	=	Dec(dw_9.Object.mfgp_pesore[ll_fila] * dw_3.Object.lote_totbul[ll_FindLote])
		
	END IF
	
NEXT
end subroutine

on w_maed_movtofrutagranel_mantreembala.create
int iCurrent
call super::create
this.dw_4=create dw_4
this.dw_6=create dw_6
this.cb_guia=create cb_guia
this.tab_1=create tab_1
this.dw_spro_bins=create dw_spro_bins
this.dw_9=create dw_9
this.dw_exidetaborra=create dw_exidetaborra
this.dw_exismovtodetanulos=create dw_exismovtodetanulos
this.dw_exideta=create dw_exideta
this.dw_exiencab=create dw_exiencab
this.ole_puerta=create ole_puerta
this.dw_desverd=create dw_desverd
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_4
this.Control[iCurrent+2]=this.dw_6
this.Control[iCurrent+3]=this.cb_guia
this.Control[iCurrent+4]=this.tab_1
this.Control[iCurrent+5]=this.dw_spro_bins
this.Control[iCurrent+6]=this.dw_9
this.Control[iCurrent+7]=this.dw_exidetaborra
this.Control[iCurrent+8]=this.dw_exismovtodetanulos
this.Control[iCurrent+9]=this.dw_exideta
this.Control[iCurrent+10]=this.dw_exiencab
this.Control[iCurrent+11]=this.ole_puerta
this.Control[iCurrent+12]=this.dw_desverd
end on

on w_maed_movtofrutagranel_mantreembala.destroy
call super::destroy
destroy(this.dw_4)
destroy(this.dw_6)
destroy(this.cb_guia)
destroy(this.tab_1)
destroy(this.dw_spro_bins)
destroy(this.dw_9)
destroy(this.dw_exidetaborra)
destroy(this.dw_exismovtodetanulos)
destroy(this.dw_exideta)
destroy(this.dw_exiencab)
destroy(this.ole_puerta)
destroy(this.dw_desverd)
end on

event open;/* Argumentos
istr_mant.argumento[1] = Código Planta
istr_mant.argumento[2] = Tipo de Movimiento
istr_mant.argumento[3] = Número de Despacho
istr_mant.argumento[4] = Sentido del Movimiento de Envases
istr_mant.argumento[5] = Especie
istr_mant.argumento[6] = Productor no se asigna / va en el detalle del lote
istr_mant.argumento[7] = Ultimo Lote Ocupado
istr_mant.argumento[8] = decimales especie ( 0 - 2 ) 
istr_mant.argumento[9] = Parametro para definir si es [M]antencion o [R]ecepcion
istr_mant.argumento[10] = Código Cliente
*/
Integer	li_resultado
String	ls_parametros, ls_nombre

x												= 0
y												= 0
This.Height									= 2020
im_menu										= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_2.Object.oproceso.visible 			= 	gb_RecepcionDeProceso
dw_2.Object.defg_docrel.visible 		= 	gb_RecepcionDeProceso
dw_2.Object.ordenproceso.visible 	= 	gb_RecepcionDeProceso

ii_TipMov 									= 	7

This.Title 									= 	"MANTENCION RECEPCION REEMBALAJE"

IF gstr_ParamPlanta.etiquetaembalaje = 0 THEN
	tab_1.tp_1.dw_lotes.DataObject	=	"dw_mues_spro_lotesfrutagranel_rec_kguia"
END IF

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

istr_mant.argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[2]	=	String(ii_TipMov)
istr_mant.argumento[4]	=	'1'
istr_mant.argumento[5]	=	String(gstr_ParamPlanta.CodigoEspecie)
istr_mant.argumento[7]	=	'0'
istr_mant.argumento[9] 	=	'R3'
istr_mant.argumento[10] =	String(gi_codexport)

dw_spro_bins.SetTransObject(sqlca)

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve() 

dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
ELSE
	idwc_especie.SetSort("espe_nombre A")
	idwc_especie.Sort()
END IF

dw_2.GetChild("tran_codigo", idwc_Transp)
idwc_Transp.SetTransObject(sqlca)

IF idwc_Transp.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Transportistas")
	idwc_Transp.InsertRow(0)
ELSE
	idwc_Transp.SetSort("tran_nombre A")
	idwc_Transp.Sort()	
END IF

dw_1.GetChild("cama_codigo",idwc_Camara)
idwc_Camara.SetTransObject(SQLCA)
idwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_1.GetChild("lote_espcod", idwc_especiedet2)
idwc_especiedet2.SetTransObject(sqlca)
IF idwc_especiedet2.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especiedet2.InsertRow(0)
ELSE
	idwc_especiedet2.SetSort("espe_nombre A")
	idwc_especiedet2.Sort()
END IF

dw_3	=	tab_1.tp_1.dw_lotes
dw_5	=	tab_1.tp_2.dw_envrec
dw_7	=	tab_1.tp_3.dw_envdes

dw_3.GetChild("prbr_codpre",idwc_Predio)
idwc_Predio.SetTransObject(SQLCA)
idwc_Predio.InsertRow(0)

dw_3.GetChild("vari_codigo",idwc_Variedad)
idwc_Variedad.SetTransObject(SQLCA)
idwc_Variedad.Retrieve(gstr_ParamPlanta.CodigoEspecie)

dw_4.GetChild("plde_codigo",idwc_plantadw4)
idwc_plantadw4.SetTransObject(SQLCA)
idwc_plantadw4.Retrieve()

dw_4.GetChild("plde_coorde",idwc_plancodw4)
idwc_plancodw4.SetTransObject(SQLCA)
idwc_plancodw4.Retrieve()

dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)
dw_9.SetTransObject(sqlca)
dw_desverd.SetTransObject(sqlca)

dw_1.GetChild("lote_pltcod",idwc_plantadw1)
idwc_plantadw1.SetTransObject(SQLCA)
idwc_plantadw1.Retrieve()

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

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							
iuo_Transport			=	Create uo_transportista
iuo_Camion				=	Create uo_camiones
iuo_Especie				=	Create uo_especie
iuo_Correlativo		=	Create uo_LotesCorrel
iuo_PesoEstanEspe		=	Create uo_PesoEstanEspe
iuo_FechaMovto			=	Create uo_FechaMovto
iuo_tipomovtofruta	=	Create uo_tipomovtofruta
iuo_TipoMovtoEnva		=	Create uo_tipomovtofruta
iuo_calicosechero		=  Create uo_calicosechero 
iuo_bins					=	Create uo_bins

IF NOT IsNull(gstr_paramplanta.Password) AND Trim(gstr_paramplanta.Password) <> '' THEN
	PostEvent("ue_validapassword")
END IF

end event

event ue_borra_detalle();call super::ue_borra_detalle;Integer	li_tabpage, li_borra
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
	
	ProductoresLotes(lstr_mant.productores)
END IF

CHOOSE CASE li_tabpage
	CASE 1 
		IF NOT verificalote(dw_3.Object.lote_pltcod[il_fila],&
									dw_3.Object.lote_espcod[il_fila],&
									dw_3.Object.lote_codigo[il_fila]) THEN
			Message.DoubleParm = -1
			RETURN
		END IF	
		istr_mant.dw	=	dw_3
		istr_mant.dw2	=	dw_6
		OpenWithParm(iw_mantencion_1, istr_mant)
	CASE 2
		lstr_mant.dw	=	dw_5
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'1'
		OpenWithParm(iw_mantencion_2, lstr_mant)
	CASE 2
		lstr_mant.dw	=	dw_7
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'2'
		OpenWithParm(iw_mantencion_2, lstr_mant)
		
END CHOOSE

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	CHOOSE CASE li_tabpage
		CASE 1
			li_borra	=	dw_3.DeleteRow(0)
			IF li_Borra = 1 THEN
				li_Borra = dw_6.RowsMove(1,dw_6.RowCount(),Primary!,dw_6,1,Delete!)
			END IF
		CASE 2
			li_borra	=	dw_5.DeleteRow(0)
		CASE 3
			li_borra	=	dw_7.DeleteRow(0)
	END CHOOSE
 
	IF li_borra = 1 THEN
		dw_6.SetFilter("")
		dw_6.Filter()
		
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_3.RowCount() = 0 THEN pb_eli_det.Enabled = False
END IF

istr_mant.borra	 = False

captura_totales()
end event

event ue_borrar;Integer	li_Cliente
IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

li_Cliente	=	dw_2.Object.clie_codigo[1]

SELECT 	clie_conexi, cone_codigo
INTO   	:il_conexiste, :il_coneccion
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
		IF ib_ConectadoExistencia THEN
			DISCONNECT USING sqlexi;
		END IF
	ELSE
		MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)
	END IF
END IF	

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)
IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)
IF dw_4.RowCount() > 0 THEN dw_4.RowsMove(1,dw_4.RowCount(),Primary!,dw_4,1,Delete!)
IF dw_5.RowCount() > 0 THEN dw_5.RowsMove(1,dw_5.RowCount(),Primary!,dw_5,1,Delete!)
IF dw_6.RowCount() > 0 THEN dw_6.RowsMove(1,dw_6.RowCount(),Primary!,dw_6,1,Delete!)
IF dw_7.RowCount() > 0 THEN dw_7.RowsMove(1,dw_7.RowCount(),Primary!,dw_7,1,Delete!)
IF dw_9.RowCount() > 0 THEN dw_9.RowsMove(1,dw_9.RowCount(),Primary!,dw_9,1,Delete!)
IF dw_spro_bins.RowCount() > 0 THEN dw_spro_bins.RowsMove(1,dw_spro_bins.RowCount(),Primary!,dw_spro_bins,1,Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
	ib_borrar = False
	w_main.SetMicroHelp("Borrando Registro...")
	
	ib_AutoCommit		=	SQLCA.AutoCommit
	SQLCA.AutoCommit	=	False
	
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
end event

event ue_guardar;Integer li_cliente
IF dw_3.AcceptText() = -1 THEN RETURN
IF dw_4.AcceptText() = -1 THEN RETURN
IF dw_5.AcceptText() = -1 THEN RETURN

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
	
//	IF dw_2.Object.mfge_estmov[1] = 3 THEN
//		li_Cliente	=	dw_2.Object.clie_codigo[1]
//		
//		SELECT 	clie_conexi, cone_codigo
//		INTO   	:il_conexiste, :il_coneccion
//		FROM dba.clientesprod
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

IF Message.DoubleParm = -1 THEN RETURN

IF il_NumFruta>0 AND dw_2.GetItemStatus(1, 0, Primary!) = NotModified! THEN
	il_NumFruta = 0
	il_NumEnva	= 0
END IF

IF dw_2.Object.mfge_estmov[1]	=	3 THEN
	ActualizaKilosLote()
END IF

//IF gstr_paramplanta.packing THEN//Entra a proceso automatico
//	IF NOT ProcesoPacking() THEN//Proceso automatico fallido, no se puede continuar
//		
//	END IF
//END IF
end event

event ue_modifica_detalle;Integer	li_tabpage
Boolean	lb_estado

str_mant_envases	lstr_mant

li_tabpage	=	tab_1.SelectedTab

istr_mant.agrega	= False
istr_mant.borra	= False

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
	
	ProductoresLotes(lstr_mant.productores)
END IF

CHOOSE CASE li_tabpage
	CASE 1
		IF dw_3.RowCount() > 0 THEN
			istr_mant.dw	=	dw_3
			istr_mant.dw2	=	dw_6
			istr_mant.argumento[7]					=	String(dw_3.Object.lote_codigo[il_fila])
			dw_spro_bins.SetFilter("lote_codigo 	= " + istr_mant.argumento[7])
			dw_spro_bins.Filter()
			
			OpenWithParm(iw_mantencion_1, istr_mant)
			
			dw_spro_bins.SetFilter('')
			dw_spro_bins.Filter()
			
			IF NOT gstr_paramplanta.bultobins THEN
				AsignaPeso()
			END IF
		END IF
		istr_mant.Solo_Consulta	=	lb_estado

	CASE 2				
		IF dw_5.RowCount() > 0 THEN
			lstr_mant.dw	=	dw_5
			SetNull(lstr_mant.dw2)
			lstr_mant.Argumento[4]	=	'1'
			OpenWithParm(iw_mantencion_2, lstr_mant)
		END IF
		lstr_mant.Solo_Consulta	=	lb_estado
		
	CASE 3				
		IF dw_7.RowCount() > 0 THEN
			lstr_mant.dw	=	dw_7
			SetNull(lstr_mant.dw2)
			lstr_mant.Argumento[4]	=	'2'
			OpenWithParm(iw_mantencion_2, lstr_mant)
		END IF
END CHOOSE

dw_6.SetFilter("")
dw_6.Filter()

captura_totales()
end event

event ue_nuevo;Long		ll_modif
Integer  li_cliente

is_rut		=	''
is_rutprod	=	''

ib_ok	= True

//li_cliente = dw_2.object.clie_codigo[1]
istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta


IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif	=	dw_1.GetNextModified(0, Primary!)
			ll_modif	+=	dw_2.GetNextModified(0, Primary!)
			ll_modif	+=	dw_3.GetNextModified(0, Primary!)
			ll_modif	+=	dw_6.GetNextModified(0, Primary!)
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
dw_6.Reset()
dw_4.Reset()
dw_5.Reset()
dw_7.Reset()

dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.Object.buscacamion.Visible	=	0
dw_2.Object.camion.Visible 		=	0
dw_2.Object.carro.Visible			=	0
dw_2.Object.romanacamion.Visible =	0
dw_2.Object.romanacarro.Visible	=	0
dw_2.SetRedraw(True)

dw_2.SetFocus()


dw_2.object.clie_codigo[1] =  gi_codexport
istr_mant.argumento[10]    =  String(gi_codexport)
dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.tpmv_codigo[1]	=	ii_TipMov
dw_2.Object.espe_codigo[1]	=	gstr_ParamPlanta.CodigoEspecie

idt_FechaSistema	=	F_FechaHora()

dw_2.Object.mfge_fecmov[1]	=	Date(idt_FechaSistema)
dw_2.Object.refg_horaen[1]	=	Time(idt_FechaSistema)	

dw_2.Object.destare.Visible	=	False
dw_2.Object.destare.Text		=	'Salida'
cb_guia.Enabled					=	False
ib_Salida							=	False
ib_Destare							=	False

istr_Mant.Argumento[3]			=	''
istr_mant.Argumento[5]			=	String(dw_2.Object.espe_codigo[1])
istr_mant.Argumento[7]			=	'0'
		
IF iuo_especie.existe(gstr_ParamPlanta.CodigoEspecie,TRUE, sqlca) THEN
	IF iuo_especie.kildec = 1 THEN
		ii_kildec = 2
	ELSE
		ii_kildec = 0
	END IF	
END IF	
		
tab_1.tp_1.Enabled	=	False
tab_1.tp_2.Enabled	=	False
tab_1.tp_3.Enabled	=	False
pb_eliminar.Enabled  =	False
pb_grabar.Enabled		=	False
pb_ins_det.Enabled	=	False
pb_imprimir.Enabled	=	False
pb_eli_det.Enabled	=	False
cb_guia.Enabled		=	False

tab_1.SelectTab(1)
HabilitaEncab(True)		
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
	lstr_mant.Borra					=	istr_mant.Borra
	lstr_mant.Solo_Consulta			=	istr_mant.Solo_Consulta
	lstr_mant.respuesta				=	istr_mant.respuesta
	lstr_mant.tipo						=	istr_mant.tipo
	lstr_mant.usuariosoloconsulta	=	istr_mant.usuariosoloconsulta
	
	ProductoresLotes(lstr_mant.productores)
END IF

CHOOSE CASE li_tabpage
	CASE 1
		IF Integer(istr_mant.argumento[7]) = 0 THEN
			iuo_Correlativo.Obtiene_Correlativo(Integer(istr_mant.argumento[1]),&
															Integer(istr_mant.argumento[5]),&
															True, SQLCA)
															
			istr_mant.argumento[7]			=	String(iuo_Correlativo.Correl_LoteFG)
		END IF
		
		istr_mant.dw	=	dw_3
		istr_mant.dw2	=	dw_6
		OpenWithParm(iw_mantencion_1, istr_mant)
		istr_mant.Solo_Consulta	=	lb_estado
		IF NOT gstr_paramplanta.bultobins THEN
			AsignaPeso()
		END IF
		
	CASE 2
		lstr_mant.dw	=	dw_5
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'1'
		OpenWithParm(iw_mantencion_2, lstr_mant)
		lstr_mant.Solo_Consulta	=	lb_estado

	CASE 3
		lstr_mant.dw	=	dw_7
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'2'
		OpenWithParm(iw_mantencion_2, lstr_mant)
END CHOOSE

CHOOSE CASE li_tabpage 
	CASE 1
		istr_mant	=	Message.PowerObjectParm
		
		dw_3.SetRow(il_Fila)
		dw_3.SelectRow(il_Fila, True)
	CASE 2
		
		dw_5.SetRow(il_Fila)
		dw_5.SelectRow(il_Fila, True)
	CASE 3
		
		dw_7.SetRow(il_Fila)
		dw_7.SelectRow(il_Fila, True)
END CHOOSE

dw_6.SetFilter("")
dw_6.Filter()

captura_totales()
end event

event ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, ll_fila_env, ll_row
Boolean lb_habilita = TRUE


DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]),&
										 Integer(istr_mant.argumento[2]),&
										 Long(istr_mant.argumento[3]),&
										 Integer(istr_mant.argumento[10]))

	IF ll_fila_e = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila_e > 0 THEN
		
		tab_1.tp_3.Enabled				=	True
		tab_1.tp_1.Enabled				=	True
		tab_1.tp_2.Enabled				=	True
		dw_2.Object.destare.Visible	=	True
		istr_Mant.Solo_Consulta			=	False
		
		istr_mant.Argumento[5]	=	String(dw_2.Object.espe_codigo[1])
		
		dw_2.SetRedraw(True)
		IF Not gb_RecepcionDeProceso THEN
			iuo_Camion.Existe(1, dw_2.Object.cami_patent[1], True, sqlca)
		END IF
		
		HabilitaEncab(False)
		IF iuo_especie.existe(dw_2.object.espe_codigo[1],TRUE,SQLCA) THEN
			IF iuo_especie.kildec = 1 THEN
				ii_kildec = 2
			ELSE
				ii_kildec = 0
			END IF	
		END IF

		ll_fila_env	=	dw_4.Retrieve(Integer(istr_mant.argumento[1]),&
											  Integer(istr_mant.argumento[2]),&
											  Long(istr_mant.argumento[3]),1,&
											  Integer(istr_mant.argumento[10]))

		DO
			IF dw_1.Retrieve(Integer(istr_mant.argumento[1]),&
							     Integer(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),&
								  Integer(istr_mant.argumento[10])) = -1 OR &
				dw_3.Retrieve(Integer(istr_mant.argumento[1]),&
							     Integer(istr_mant.argumento[2]),&
								  Long(istr_mant.argumento[3]),&
								  Integer(istr_mant.argumento[10])) = -1 OR &
				dw_6.Retrieve(Integer(istr_mant.argumento[1]),&
							     Integer(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),&
								  Integer(istr_mant.argumento[10])) = -1 OR &
				dw_5.Retrieve(Integer(istr_mant.argumento[1]),&
							     Integer(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),1,&
								  Integer(istr_mant.argumento[10])) = -1 OR &
				dw_7.Retrieve(Integer(istr_mant.argumento[1]),&
							     Integer(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),2,&
								  Integer(istr_mant.argumento[10])) = -1 OR &
				dw_spro_bins.Retrieve(Long(istr_mant.argumento[1]),&
							    	Long(istr_mant.argumento[3]),&
								  Integer(istr_mant.argumento[10])) = -1 OR &
				dw_9.Retrieve(Long(istr_mant.argumento[1]),&
							     Long(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3])) = -1 OR &
				dw_desverd.Retrieve(Integer(istr_mant.argumento[10]),&
								  Integer(istr_mant.argumento[1]),&
							     Integer(istr_mant.argumento[2]),&
								  Long(istr_mant.argumento[3])) = -1  THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				idwc_variedad.Retrieve(dw_2.Object.espe_codigo[1])
				
				//Para determinar Fuera de Rango
				Destare(False)
				
//				IF OtrosMovtosDetalle() THEN
//					istr_Mant.Solo_Consulta	=	True
//				END IF
				FOR ll_Row = 1 TO dw_3.RowCount()
					 IF Not VerificaBultos(Integer(Istr_Mant.Argumento[1]),&
					 					 		  dw_3.Object.lote_pltcod[ll_row],&
										 		  dw_3.Object.lote_espcod[ll_row],&
										 		  dw_3.Object.lote_codigo[ll_row],&
										 		  dw_3.Object.lote_totbul[ll_row]) THEN
						 								lb_Habilita = False
								 						ll_Row = dw_3.RowCount()
					END IF
				NEXT

				IF lb_Habilita THEN
					Istr_Mant.Solo_Consulta = FALSE
				ELSE
					IF Messagebox("Advertencia","Movimiento Solo De Consulta, ¿Desea Modificar de todas formas?", Exclamation!, YesNo!, 2) = 1 THEN
						Istr_Mant.Solo_Consulta = FALSE
					ELSE
						Istr_Mant.Solo_Consulta = TRUE
					END IF
				END IF

				dw_2.Enabled 			= Not istr_Mant.Solo_Consulta
				pb_eliminar.Enabled  = Not istr_Mant.Solo_Consulta
				pb_grabar.Enabled		= Not istr_Mant.Solo_Consulta
				pb_ins_det.Enabled	= Not istr_Mant.Solo_Consulta
				pb_imprimir.Enabled	= True
				pb_eli_det.Enabled	= Not istr_Mant.Solo_Consulta
				cb_guia.Enabled		= Not istr_Mant.Solo_Consulta
				
				il_fila = 1
				dw_3.SetRow(1)
				dw_3.SelectRow(1,True)
				dw_3.SetFocus()
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_antesguardar;Date			ld_Fecha
Time			lt_hora
Boolean     lb_Actualiza_Fruta = FALSE, lb_Actualiza_Envase = FALSE
Integer 	 	li_TipoMovto, li_TipoMovtoEnva, li_Planta, li_pesaje, &
				li_Lote_pltcod, li_Lote_espcod, li_Lote_codigo, li_Secuencia, &
				li_TipoEnvase, li_Envase, li_Lote_Ant, li_camara, li_Cliente
Long 			ll_Filas, ll_Total_Bultos, ll_Envases_Fruta, ll_Fila, ll_Fila_Busca, &
  				ll_Fila_d, ll_Primer_NumEnva, ll_Productor, ll_Numero
Decimal{3}	ld_Total_PesoEnv_Sal, ld_KBS_Camion, ld_KBS_Carro

Message.DoubleParm = 0

li_Planta			=	Integer(istr_mant.Argumento[1])
li_TipoMovto		=	Integer(istr_mant.Argumento[2])
li_TipoMovtoenva  =  43  // Recepción de Envases
li_Cliente			=	Integer(istr_mant.Argumento[10])

//il_numeroenva = dw_4.Object.meen_numero[1]
 
ll_Filas = dw_1.RowCount()

//IF NOT chequea_BinsContraEnvases() THEN
//	Message.DoubleParm = -1
//	RETURN
//END IF

//IF dw_3.RowCount() > 0 THEN
//	IF dw_3.Object.total_bultos[dw_3.RowCount()] >= 10000 THEN
//		Messagebox("Error de Consistencia","El total de bultos supera lo permitido")
//		Message.DoubleParm = -1
//		RETURN
//	ELSE
//		ll_Total_Bultos	=	dw_3.Object.total_bultos[dw_3.RowCount()]
//	END IF
//END IF
//
//IF dw_5.RowCount() > 0 THEN
//	IF dw_5.Object.total_pesoenv[dw_5.RowCount()] >= 100000 THEN
//		Messagebox("Error de Consistencia","El peso de los envases supera lo permitido")
//		Message.DoubleParm = -1
//		RETURN
//	END IF
//	
//	IF dw_5.Object.total_envases[dw_5.RowCount()] >= 10000 THEN
//		Messagebox("Error de Consistencia","El total de envases supera lo permitido")
//		Message.DoubleParm = -1
//		RETURN
//	END IF
//END IF

//IF dw_5.RowCount() > 0 THEN
//	ll_Envases_Fruta	=	dw_5.Object.tot_bultos_fruta[dw_5.RowCount()]
//END IF

//IF Chequea_EnvasesProductor() = False THEN
//	Message.DoubleParm = -1
//	RETURN
//END IF

IF ib_Salida THEN
	IF Isnull(dw_2.Object.refg_tkbsal[1]) OR dw_2.Object.refg_tkbsal[1] = 0 THEN
		IF	MessageBox("Atención","No se ha registrado la Tara de Salida del Camión."+&
						" Desea Registrarla ?",Question!,YesNo!) = 1 THEN
			Message.DoubleParm = -1
			RETURN
		END IF
	END IF
	
	ll_Fila	=	dw_7.RowCount()

	IF ll_Fila > 0 THEN
		dw_7.GroupCalc()
		ld_Total_PesoEnv_Sal	=	dw_7.Object.total_pesoenv[ll_Fila]
	END IF

	ld_KBS_Camion	=	dw_2.Object.refg_tkbsal[1]
	ld_KBS_Carro	=	dw_2.Object.refg_tkbsac[1]

	IF ld_KBS_Camion + ld_KBS_Carro > 0 AND &
		ld_KBS_Camion + ld_KBS_Carro < ld_Total_PesoEnv_Sal THEN
		MessageBox("Atención","Kilos de Salida no pueden ser menor a los Envases de Salida")
		Message.DoubleParm = -1
		RETURN
	END IF
	
 	IF dw_2.Object.mfge_tpneto[1] >= 100000 THEN
		MessageBox("Atención","Los valores de la tara superan el calculo del Peso Neto permitido")
		Message.DoubleParm = -1
		RETURN
	END IF

	IF ib_Destare THEN
		//Pasa a Definitiva
		dw_2.Object.mfge_estmov[1]	=	3
		cb_guia.Enabled				=	True	
	ELSE
		MessageBox("Atención","Aún no realiza el destare, no puede grabar")
		Message.DoubleParm = -1
		RETURN
	END IF
	
END IF

//IF dw_9.RowCount() <> dw_2.Object.mfge_totbul[1] THEN
//	MessageBox("Atención","La cantidad de pesajes no corresponde a la cantidad de Bultos Recepcionados.")
//	Message.DoubleParm = -1
//	RETURN
//END IF

ib_AutoCommit		=	SQLCA.AutoCommit
SQLCA.AutoCommit	=	False

IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
  
//  	IF il_NumEnva=0 THEN
//		iuo_TipoMovtoEnva.bloqueacorrel()	
//		il_NumEnva = iuo_TipoMovtoEnva.UltimoCorrelativo(4,li_TipoMovtoEnva,li_Planta) 
//	
//		IF il_NumEnva = 0 OR IsNull(il_NumEnva) THEN
//			Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
//			Message.DoubleParm = -1
//			RETURN
//		ELSE
//			lb_Actualiza_Envase = TRUE
//			ll_Primer_NumEnva   = il_NumEnva
//		END IF
//	ELSE
//		ll_Primer_NumEnva   = il_NumEnva
//	END IF
	
	IF il_NumFruta=0 THEN
		iuo_TipoMovtoFruta.bloqueacorrel()
		il_NumFruta = iuo_TipoMovtoFruta.UltimoCorrelativo(1,li_TipoMovto,li_Planta) 
	
		IF il_NumFruta = 0 OR IsNull(il_NumFruta) THEN
			Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
			Message.DoubleParm = -1
			RETURN
		ELSE
			lb_Actualiza_Fruta = TRUE	
		END IF
   END IF
   
	dw_2.Object.mfge_numero[1]	=	il_NumFruta

//	Determina_ProductoresEnvase(li_TipoMovtoEnva)
	
	//Resetea Movto de Envase en caso de errores previos en la grabación.
	dw_4.Reset()
	
//	FOR ll_Productor = 1 TO UpperBound(wstr_Prod_Enva.Productor)
//		ll_Fila	=	dw_4.InsertRow(0)
//		
//		dw_4.Object.plde_codigo[ll_Fila]	=	li_Planta
//		dw_4.Object.tpmv_codigo[ll_Fila]	=	li_TipoMovtoEnva
//		dw_4.Object.meen_numero[ll_Fila]	=	il_NumEnva
//		dw_4.Object.tpmv_codrec[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
//		dw_4.Object.mfge_numero[ll_Fila]	=	il_NumFruta
//		dw_4.Object.meen_modulo[ll_Fila]	=	1
//		dw_4.Object.plde_coorde[ll_Fila]	=	dw_2.Object.plde_coorde[1]
//		dw_4.Object.prod_codigo[ll_Fila]	=	Long(wstr_Prod_Enva.Productor[ll_Productor])
//      dw_4.Object.clie_codigo[ll_Fila]	=	li_Cliente
//		dw_4.Object.meen_guisii[ll_Fila]	=	wstr_Prod_Enva.GuiaSII[ll_Productor]
//		dw_4.Object.meen_fecmov[ll_Fila]	=	dw_2.Object.mfge_fecmov[1]
//		dw_4.Object.tran_codigo[ll_Fila]	=	dw_2.Object.tran_codigo[1]
//		dw_4.Object.cami_clasifi[ll_Fila]=	dw_2.Object.cami_clasifi[1]
//		dw_4.Object.cami_patent[ll_Fila]	=	dw_2.Object.cami_patent[1]
//		dw_4.Object.cami_patcar[ll_Fila]	=	dw_2.Object.cami_patcar[1]
//		dw_4.Object.meen_rutcho[ll_Fila]	=	dw_2.Object.mfge_rutcho[1]
//		dw_4.Object.meen_chofer[ll_Fila]	=	dw_2.Object.mfge_chofer[1]
//		il_NumEnva++
//	NEXT
	//Descuenta último Correlativo Envases acumulado.
//   il_NumEnva --
	//Preguntar el Momento de Actualización
	IF lb_Actualiza_Fruta  THEN iuo_TipoMovtoFruta.Actualiza_Correlativo(1,li_Planta,li_TipoMovto,il_NumFruta) 
//   IF lb_Actualiza_Envase THEN iuo_TipoMovtoEnva.Actualiza_Correlativo(4,li_Planta,li_TipoMovtoEnva,il_NumEnva) 
   ///////////////////////////////////////
//	il_NumEnva = ll_Primer_NumEnva	
ELSE
	il_NumFruta	=	dw_2.Object.mfge_numero[1]
END IF

istr_mant.Argumento[3]	=	String(il_NumFruta)

SELECT	IsNull(Max(mfgd_secuen), 0) + 1
	INTO	:li_Secuencia
	FROM	dba.spro_movtofrutagrandeta
	WHERE	plde_codigo	=	:li_Planta
	AND	tpmv_codigo	=	:li_TipoMovto
	AND	mfge_numero	=	:il_NumFruta
	AND   clie_codigo =  :li_Cliente;
	
ll_filas = dw_1.RowCount()	

FOR ll_Fila = 1 TO dw_6.RowCount()
	
	li_Lote_pltcod	=	dw_6.Object.lote_pltcod[ll_Fila]
	li_Lote_espcod	=	dw_6.Object.lote_espcod[ll_Fila]
	li_Lote_codigo	=	dw_6.Object.lote_codigo[ll_Fila]
	li_TipoEnvase	=	dw_6.Object.enva_tipoen[ll_Fila]
	li_Envase		=	dw_6.Object.enva_codigo[ll_Fila]

	IF li_Lote_Codigo <> li_Lote_Ant THEN
		ll_Fila_Busca	=	dw_3.Find("lote_pltcod = "+String(li_Lote_pltcod)+" and "+&
											 "lote_espcod = "+String(li_Lote_espcod)+" and "+&
											 "lote_codigo = "+String(li_Lote_codigo),1,dw_3.RowCount())
		IF ll_Fila_Busca > 0 THEN
			dw_3.Object.fgcc_fecrec[ll_Fila_Busca]	=	dw_2.Object.mfge_fecmov[1]
			li_Camara	=	dw_3.Object.cama_codigo[ll_Fila_Busca]
		END IF
		li_Lote_Ant	=	li_Lote_codigo
	END IF
	
	ll_Fila_d	=	dw_1.Find("lote_pltcod = "+String(li_Lote_pltcod)+" and "+&
									 "clie_codigo = "+String(li_cliente)+" and "+&
									 "lote_espcod = "+String(li_Lote_espcod)+" and "+&
									 "lote_codigo = "+String(li_Lote_codigo)+" and "+&
									 "enva_tipoen = "+String(li_TipoEnvase)+" and "+&
									 "enva_codigo = "+String(li_Envase),1,dw_1.RowCount())
	IF ll_Fila_d = 0 THEN
		ll_Fila_d	=	dw_1.InsertRow(0)
		dw_1.Object.plde_codigo[ll_Fila_d]	=	li_Planta
		dw_1.Object.tpmv_codigo[ll_Fila_d]	=	dw_2.Object.tpmv_codigo[1]
		dw_1.Object.mfge_numero[ll_Fila_d]	=	dw_2.Object.mfge_numero[1]
		dw_1.Object.mfgd_secuen[ll_Fila_d]	=	li_Secuencia
		li_Secuencia ++
	END IF

	dw_1.Object.plde_coorde[ll_Fila_d]	=	li_Planta
	dw_1.Object.cama_codigo[ll_Fila_d]	=	li_Camara
	dw_1.Object.lote_pltcod[ll_Fila_d]	=	li_Lote_pltcod
	dw_1.Object.lote_espcod[ll_Fila_d]	=	li_Lote_espcod
	dw_1.Object.lote_codigo[ll_Fila_d]	=	li_Lote_codigo
	dw_1.Object.enva_tipoen[ll_Fila_d]	=	li_TipoEnvase
	dw_1.Object.enva_codigo[ll_Fila_d]	=	li_Envase
	dw_1.Object.mfgd_bulent[ll_Fila_d]	=	dw_6.Object.lotd_totbul[ll_Fila]
	dw_1.Object.mfgd_kgnent[ll_Fila_d]	=	dw_6.Object.lotd_totnet[ll_Fila]
	dw_1.Object.clie_codigo[ll_Fila_d]	=	li_Cliente	
NEXT

//Elimina Filas de Detalle de Movimiento que fueron eliminadas de Lotes
ll_Fila	= 1

DO WHILE ll_Fila <= dw_1.RowCount()
	
	li_Lote_pltcod	=	dw_1.Object.lote_pltcod[ll_Fila]
	li_Lote_espcod	=	dw_1.Object.lote_espcod[ll_Fila]
	li_Lote_codigo	=	dw_1.Object.lote_codigo[ll_Fila]
	li_TipoEnvase	=	dw_1.Object.enva_tipoen[ll_Fila]
	li_Envase		=	dw_1.Object.enva_codigo[ll_Fila]

	ll_Fila_d	=	dw_6.Find("lote_pltcod = "+String(li_Lote_pltcod)+" and "+&
	                         "lote_espcod = "+String(li_Lote_espcod)+" and "+&
									 "lote_codigo = "+String(li_Lote_codigo)+" and "+&
									 "enva_tipoen = "+String(li_TipoEnvase)+" and "+&
									 "enva_codigo = "+String(li_Envase),1,dw_6.RowCount())

	IF ll_Fila_d = 0 THEN
		dw_1.DeleteRow(ll_Fila)
	ELSE
		ll_Fila ++
	END IF
LOOP

FOR ll_Fila = 1 TO dw_5.RowCount()
	
	IF dw_5.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		ll_Productor	=	dw_5.Object.prod_codigo[ll_Fila]
		
		ll_Fila_Busca	=	dw_4.Find("plde_codigo = "+String(li_Planta) + " and " + &
											 "tpmv_codigo = "+String(li_TipoMovtoEnva) + " and " + &
											 "clie_codigo = "+String(li_Cliente) + " and " + &											 
											 "prod_codigo = "+String(ll_Productor),1,dw_4.RowCount())
		
		IF ll_Fila_Busca > 0 THEN
												 
			dw_5.Object.plde_codigo[ll_Fila]	=	dw_4.Object.plde_codigo[ll_Fila_Busca]
			dw_5.Object.tpmv_codigo[ll_Fila]	=	dw_4.Object.tpmv_codigo[ll_Fila_Busca]
			dw_5.Object.meen_numero[ll_Fila]	=	dw_4.Object.meen_numero[ll_Fila_Busca]
			dw_5.Object.clie_codigo[ll_Fila]	=	li_Cliente
		END IF
	END IF
NEXT

FOR ll_Fila = 1 TO dw_7.RowCount()
	
	IF dw_7.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		ll_Productor	=	dw_7.Object.prod_codigo[ll_Fila]
		
		ll_Fila_Busca	=	dw_4.Find("plde_codigo = "+String(li_Planta)+" and " + &
											 "tpmv_codigo = 41 and " + &
											 "clie_codigo = "+String(li_Cliente) + " and " + &											 
											 "prod_codigo = "+String(ll_Productor),1,dw_4.RowCount())
		
		IF ll_Fila_Busca > 0 THEN
												 
			dw_7.Object.plde_codigo[ll_Fila]	=	dw_4.Object.plde_codigo[ll_Fila_Busca]
			dw_7.Object.tpmv_codigo[ll_Fila]	=	dw_4.Object.tpmv_codigo[ll_Fila_Busca]
			dw_7.Object.meen_numero[ll_Fila]	=	dw_4.Object.meen_numero[ll_Fila_Busca]
			dw_7.Object.clie_codigo[ll_Fila]	=	li_Cliente			
		END IF
	END IF
NEXT
/*Selecciona secuencia para tabla de pesajes*/

ll_numero	=	dw_2.Object.mfge_numero[1]
li_Pesaje	=	0
 dw_9.SetSort("mfgp_nropes asc")
 dw_9.Sort()
 
FOR ll_Fila = 1 TO dw_9.RowCount()
	IF dw_9.Object.mfgp_nropes[ll_Fila] <> li_Pesaje THEN
		li_Pesaje	=	dw_9.Object.mfgp_nropes[ll_Fila]
		
		SELECT	IsNull(Max(mfgp_secuen),0) + 1
		  INTO	:li_Secuencia
		  FROM	dba.spro_movtofrutagranpesa
		 WHERE	plde_codigo	=	:li_Planta
			AND	tpmv_codigo =	:li_TipoMovto
			AND	mfge_numero =	:ll_Numero
			AND	mfgp_nropes	=	:li_Pesaje ;
	END IF
	
	IF dw_9.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_9.Object.clie_codigo[ll_fila] 	= 	dw_2.Object.clie_codigo[1]
		dw_9.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_9.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
		dw_9.Object.mfge_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]		
		dw_9.Object.mfgp_secuen[ll_Fila]	=	li_secuencia		
		li_secuencia ++
	END IF
NEXT

FOR ll_Fila = 1 TO dw_spro_bins.RowCount()
//	IF dw_spro_bins.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_spro_bins.Object.mfge_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]		
//	END IF
NEXT

//FOR ll_fila = 1 TO dw_desverd.RowCount()
//	dw_desverd.Object.lode_estado[ll_fila]	=	1
//	
//NEXT

//SETEA CLIENTE = CLIE_CODIGO SELECCIONADO
//Repara falla para proceso 3ros.
FOR ll_fila = 1 TO dw_1.Rowcount()
	dw_1.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

FOR ll_fila = 1 TO dw_3.Rowcount()
	dw_3.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

FOR ll_fila = 1 TO dw_4.Rowcount()
	dw_4.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

FOR ll_fila = 1 TO dw_5.Rowcount()
	dw_5.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

FOR ll_fila = 1 TO dw_7.Rowcount()
	dw_7.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

//AsignaPeso()
end event

event ue_seleccion;call super::ue_seleccion;IF ib_ok = False THEN RETURN

Str_Busqueda	lstr_busq

lstr_busq.argum[1] = String(gstr_param.plde_codigo)
lstr_busq.argum[2] = String(ii_TipMov)								// Movimiento de Recepción
lstr_busq.argum[3] = '3' 												// Sólo Definitivas
lstr_busq.argum[4] = String(idt_FechaSistema)   				// Desde Fecha de Inicio Ducha
lstr_busq.argum[10] = String(dw_2.Object.clie_codigo[1]) 	// cliente

OpenWithParm(w_busc_spro_movtofrutagranenca_recepcion, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	istr_mant.Argumento[1]	=	lstr_busq.argum[1]
	istr_mant.Argumento[2]	=	lstr_busq.argum[2]
	istr_mant.argumento[3]	=	lstr_busq.argum[3]
	istr_mant.argumento[10]	=	lstr_busq.argum[10]
	
	This.TriggerEvent("ue_recuperadatos")
END IF
end event

event ue_imprimir;Long	ll_modif
Date	ld_FechaRecepcion

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif	=	dw_1.GetNextModified(0, Primary!)
			ll_modif	+=	dw_2.GetNextModified(0, Primary!)
			ll_modif	+=	dw_3.GetNextModified(0, Primary!)
			ll_modif	+=	dw_6.GetNextModified(0, Primary!)
			ll_modif	+=	dw_5.GetNextModified(0, Primary!)
			ll_modif	+=	dw_7.GetNextModified(0, Primary!)
		
			IF dw_3.RowCount() > 0 and ll_modif > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información antes de Imprimir ?", Question!, YesNoCancel!)
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

SetPointer(HourGlass!)

Long		fila
Integer  li_estado, li_Kilos

istr_info.titulo	= "GUIA DE RECEPCION FRUTA GRANEL"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

li_estado	=	dw_2.object.mfge_estmov[1]

IF li_estado=1 THEN
   vinf.dw_1.DataObject = "dw_info_guia_recepcion_Transitoria"
ElSE
	vinf.dw_1.DataObject = "dw_info_guia_recepcion_Definitiva"
	li_Kilos	=	MessageBox("Emisión Definitiva","Desea emitir Guía de Recepción con Kilos",Question!,YesNo!,1)
	IF li_Kilos = 2 THEN li_Kilos = 0
END IF

ld_FechaRecepcion	=	dw_2.object.mfge_fecmov[1]

vinf.dw_1.SetTransObject(sqlca)

//fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]),&
//								  Integer(istr_mant.Argumento[2]),&
//								  Integer(istr_mant.Argumento[3]),li_Kilos)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]),&
								  Integer(istr_mant.Argumento[2]),&
								  Integer(istr_mant.Argumento[3]),&
								  Integer(istr_mant.Argumento[10]),&
								  ld_FechaRecepcion, ld_FechaRecepcion)


IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
	   	F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
  	END IF
END IF

SetPointer(Arrow!)
end event

event closequery; IF Not istr_mant.Solo_Consulta THEN
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

event ue_validaborrar;Long ll_fila
IF MessageBox("Borrar Registro","Desea Borrar la Información ?", Question!, YesNo!) = 1 THEN
	Message.DoubleParm = 1
	
	FOR ll_fila = 1 TO dw_3.RowCount()
		IF NOT verificalote(dw_3.Object.lote_pltcod[ll_fila],&
									dw_3.Object.lote_espcod[ll_fila],&
									dw_3.Object.lote_codigo[ll_fila]) THEN
			Message.DoubleParm = -1
			RETURN
		END IF	
	NEXT	
ELSE
	Message.DoubleParm = -1
END IF

RETURN
end event

event resize;call super::resize;Tab_1.x								=	dw_1.x
Tab_1.y								=	dw_1.y
Tab_1.Width						=	dw_1.Width
Tab_1.Height						=	dw_1.Height

Tab_1.Tp_1.dw_lotes.x			= 27
Tab_1.Tp_1.dw_lotes.y			= 36
Tab_1.Tp_1.dw_lotes.height	= Tab_1.height - 180
Tab_1.Tp_1.dw_lotes.width		= Tab_1.width - 92

Tab_1.Tp_2.dw_envrec.x		= 27
Tab_1.Tp_2.dw_envrec.y		= 36
Tab_1.Tp_2.dw_envrec.height	= Tab_1.height - 180
Tab_1.Tp_2.dw_envrec.width	= Tab_1.width - 92

Tab_1.Tp_3.dw_envdes.x		= 27
Tab_1.Tp_3.dw_envdes.y		= 36
Tab_1.Tp_3.dw_envdes.height	= Tab_1.height - 180
Tab_1.Tp_3.dw_envdes.width	= Tab_1.width - 92

cb_guia.x 							=  pb_salir.x
cb_guia.y 							=  pb_salir.y + pb_salir.Height + 10
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutagranel_mantreembala
boolean visible = false
integer x = 503
integer y = 2228
integer width = 3406
integer height = 488
string title = "Detalle de Movimientos"
string dataobject = "dw_mues_movtofrutagraneldeta_despacho"
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutagranel_mantreembala
integer x = 471
integer y = 0
integer width = 3547
integer height = 1052
string dataobject = "dw_mant_movtofrutagranenca"
boolean livescroll = true
end type

event dw_2::itemchanged;String	ls_Nula
String	ls_Columna
Date		ldt_Fecha

ls_Columna = GetColumnName()
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(row,"clie_codigo",Integer(ls_Nula))
			RETURN 1
		ELSE
			istr_mant.Argumento[10] = data
		END IF		
		
	CASE "mfge_numero"
		IF NOT ExisteRecepcion(gstr_ParamPlanta.CodigoPlanta, ii_TipMov, Integer(data),dw_2.object.clie_codigo[1]) THEN
			This.SetItem(1,"mfge_numero",Long(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF
	
	CASE "refg_tkbent", "refg_tkbenc", "refg_tkbsal", "refg_tkbsac"
		IF Not This.uf_validate(row) THEN
			This.SetItem(row,ls_Columna,Dec(ls_Nula))
			RETURN 1
		ELSE
			captura_Totales()
		END IF

END CHOOSE
end event

event dw_2::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name
	CASE "destare"
		IF Not ib_Salida THEN
			MessageBox("Advertencia","Las modificaciones a los valores ingresados, ~r~rpueden ocasionar cambios en los resultados.")
			HabilitaSalida()
		ELSE
			Destare(True)
			IF gstr_paramplanta.bultobins THEN
				UpdateMovtoGranPesa()
			END IF
		END IF
		
END CHOOSE
end event

event dw_2::itemfocuschanged;call super::itemfocuschanged;IF is_rut <> "" THEN
	This.Object.mfge_rutcho.Format = '@@@.@@@.@@@-@'
	
	IF dwo.Name <> "mfge_rutcho" THEN
		This.SetItem(1, "mfge_rutcho", is_rut)
	END IF
END IF
end event

event dw_2::constructor;call super::constructor;This.uf_add_validation( 'refg_tkbsal >= 0 and refg_tkbsal < 99999.999','Valor fuera de rango')
This.uf_add_validation( 'refg_tkbsac >= 0 and refg_tkbsac < 99999.999','Valor fuera de rango')
This.uf_add_validation( 'refg_tkbent >= 0 and refg_tkbent < 99999.999','Valor fuera de rango')
This.uf_add_validation( 'refg_tkbenc >= 0 and refg_tkbenc < 99999.999','Valor fuera de rango')
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutagranel_mantreembala
integer x = 4622
integer y = 232
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutagranel_mantreembala
integer x = 4622
integer y = 408
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutagranel_mantreembala
integer x = 4622
integer y = 592
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutagranel_mantreembala
integer x = 4622
integer y = 772
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutagranel_mantreembala
integer x = 4622
integer y = 952
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutagranel_mantreembala
integer x = 4622
integer y = 1424
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutagranel_mantreembala
integer x = 4622
integer y = 1596
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutagranel_mantreembala
integer x = 4622
integer y = 48
end type

type dw_4 from datawindow within w_maed_movtofrutagranel_mantreembala
integer x = 201
integer y = 2184
integer width = 3182
integer height = 696
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_movtoenvaenca_recepfruta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_6 from datawindow within w_maed_movtofrutagranel_mantreembala
boolean visible = false
integer x = 667
integer y = 2216
integer width = 2565
integer height = 444
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_spro_lotesfrutagradet_recepcion"
boolean maxbox = true
borderstyle borderstyle = stylelowered!
end type

type cb_guia from commandbutton within w_maed_movtofrutagranel_mantreembala
integer x = 4599
integer y = 1164
integer width = 279
integer height = 92
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

event clicked;Long		ll_Fila, ll_Fila_Busca,li_Productor
	

str_mant	lstr_mant

Determina_ProductoresEnvase(61)

FOR ll_Fila = 1 TO UpperBound(wstr_Prod_Enva.Productor)
	li_Productor	=	wstr_Prod_Enva.Productor[ll_Fila]
	
	ll_Fila_Busca	=	dw_4.Find("clie_codigo = "+istr_Mant.Argumento[10]+" and "+&
	                            "plde_codigo = "+istr_Mant.Argumento[1]+" and "+&
										 "tpmv_codigo = 41 and "+&
										 "prod_codigo = "+String(li_Productor),1,dw_4.RowCount())
		
	IF ll_Fila_Busca > 0 THEN
												 
		lstr_Mant.Argumento[1]	=	String(dw_4.Object.plde_codigo[ll_Fila_Busca])
		lstr_Mant.Argumento[2]	=	String(dw_4.Object.tpmv_codigo[ll_Fila_Busca])
		lstr_Mant.Argumento[3]	=	String(dw_4.Object.meen_numero[ll_Fila_Busca])
		lstr_mant.Argumento[5]  =  String(dw_4.Object.clie_codigo[ll_Fila_Busca])
		
		OpenWithParm(w_emis_guia_despacho_envases, lstr_Mant)
	END IF
NEXT


end event

type tab_1 from tab within w_maed_movtofrutagranel_mantreembala
integer x = 169
integer y = 1096
integer width = 4370
integer height = 1044
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
integer width = 4334
integer height = 916
boolean enabled = false
long backcolor = 16711680
string text = "Lotes Recepcionados"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "DatabaseProfile5!"
long picturemaskcolor = 536870912
dw_lotes dw_lotes
end type

on tp_1.create
this.dw_lotes=create dw_lotes
this.Control[]={this.dw_lotes}
end on

on tp_1.destroy
destroy(this.dw_lotes)
end on

type dw_lotes from uo_dw within tp_1
integer x = 23
integer y = 32
integer width = 4229
integer height = 840
integer taborder = 10
string dataobject = "dw_mues_spro_lotesfrutagranel_recepcion"
boolean hscrollbar = true
boolean livescroll = true
end type

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_mantreembala.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event dwnkey;call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutagranel_mantreembala.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_mantreembala.PostEvent("ue_seteafila")

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
integer x = 18
integer y = 112
integer width = 4334
integer height = 916
boolean enabled = false
long backcolor = 16711680
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
integer x = 27
integer y = 36
integer width = 3991
integer height = 840
integer taborder = 11
string dataobject = "dw_mues_movtoenvadeta_recepfruta"
boolean hscrollbar = true
boolean livescroll = true
end type

event dwnkey;call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_mantreembala.PostEvent("ue_seteafila")

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

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_mantreembala.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

type tp_3 from userobject within tab_1
string tag = "Registro de Envases que retira para cosecha"
integer x = 18
integer y = 112
integer width = 4334
integer height = 916
boolean enabled = false
long backcolor = 16711680
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
integer x = 27
integer y = 36
integer width = 3991
integer height = 840
integer taborder = 21
string dataobject = "dw_mues_movtoenvadeta_recepfruta"
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

event dwnkey;call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_mantreembala.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_mantreembala.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

type dw_spro_bins from datawindow within w_maed_movtofrutagranel_mantreembala
boolean visible = false
integer x = 123
integer y = 2228
integer width = 2331
integer height = 400
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string dataobject = "dw_spro_movtobins"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_9 from datawindow within w_maed_movtofrutagranel_mantreembala
boolean visible = false
integer x = 18
integer y = 2712
integer width = 2181
integer height = 400
integer taborder = 80
boolean bringtotop = true
boolean titlebar = true
string dataobject = "dw_pesaje_romana"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exidetaborra from datawindow within w_maed_movtofrutagranel_mantreembala
boolean visible = false
integer x = 2414
integer y = 2316
integer width = 686
integer height = 400
integer taborder = 80
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta_borra"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exismovtodetanulos from datawindow within w_maed_movtofrutagranel_mantreembala
boolean visible = false
integer x = 3168
integer y = 2312
integer width = 686
integer height = 400
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_inserta_exismovtodetanulos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exideta from datawindow within w_maed_movtofrutagranel_mantreembala
boolean visible = false
integer x = 3877
integer y = 2120
integer width = 686
integer height = 400
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exiencab from datawindow within w_maed_movtofrutagranel_mantreembala
boolean visible = false
integer x = 2734
integer y = 2312
integer width = 686
integer height = 400
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_exismovtoenca_analisis"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type ole_puerta from olecustomcontrol within w_maed_movtofrutagranel_mantreembala
event oncomm ( )
boolean visible = false
integer x = 18
integer y = 20
integer width = 174
integer height = 152
integer taborder = 10
boolean bringtotop = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "w_maed_movtofrutagranel_mantreembala.win"
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type

type dw_desverd from datawindow within w_maed_movtofrutagranel_mantreembala
boolean visible = false
integer x = 201
integer y = 36
integer width = 201
integer height = 168
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_spro_lotesfrutagranel_desverd_corto"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
0Bw_maed_movtofrutagranel_mantreembala.bin 
2B00000600e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe00000006000000000000000000000001000000010000000000001000fffffffe00000000fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1Bw_maed_movtofrutagranel_mantreembala.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
