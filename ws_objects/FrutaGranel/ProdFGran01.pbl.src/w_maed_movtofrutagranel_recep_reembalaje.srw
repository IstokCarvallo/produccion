$PBExportHeader$w_maed_movtofrutagranel_recep_reembalaje.srw
$PBExportComments$Recepción de Fruta Granel de Huerto
forward
global type w_maed_movtofrutagranel_recep_reembalaje from w_mant_encab_deta_csd
end type
type cb_guia from commandbutton within w_maed_movtofrutagranel_recep_reembalaje
end type
type dw_4 from datawindow within w_maed_movtofrutagranel_recep_reembalaje
end type
type ole_puerta1 from olecustomcontrol within w_maed_movtofrutagranel_recep_reembalaje
end type
type dw_spro_bins from datawindow within w_maed_movtofrutagranel_recep_reembalaje
end type
type dw_9 from datawindow within w_maed_movtofrutagranel_recep_reembalaje
end type
type dw_6 from datawindow within w_maed_movtofrutagranel_recep_reembalaje
end type
type ole_puerta from olecustomcontrol within w_maed_movtofrutagranel_recep_reembalaje
end type
type dw_desverd from datawindow within w_maed_movtofrutagranel_recep_reembalaje
end type
type tab_1 from tab within w_maed_movtofrutagranel_recep_reembalaje
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
type tab_1 from tab within w_maed_movtofrutagranel_recep_reembalaje
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
end type
type str_productores_envases from structure within w_maed_movtofrutagranel_recep_reembalaje
end type
type str_pesaje from structure within w_maed_movtofrutagranel_recep_reembalaje
end type
type productoresxlote from structure within w_maed_movtofrutagranel_recep_reembalaje
end type
end forward

type str_productores_envases from structure
	long		productor[]
	long		guiasii[]
	integer		tipomovto[]
	integer		numero[]
	integer		cliente[]
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

type productoresxlote from structure
	integer		prod_codigo[]
	string		prod_rut[]
	string		prod_nombre[]
	integer		lote_codigo[]
	integer		lote_espcod[]
	integer		lote_pltcod[]
end type

global type w_maed_movtofrutagranel_recep_reembalaje from w_mant_encab_deta_csd
integer width = 4549
integer height = 2208
string title = "RECEPCION PARA REEMBALAJE"
string menuname = ""
boolean center = true
event ue_imprimir_tarja ( )
cb_guia cb_guia
dw_4 dw_4
ole_puerta1 ole_puerta1
dw_spro_bins dw_spro_bins
dw_9 dw_9
dw_6 dw_6
ole_puerta ole_puerta
dw_desverd dw_desverd
tab_1 tab_1
end type
global w_maed_movtofrutagranel_recep_reembalaje w_maed_movtofrutagranel_recep_reembalaje

type variables
w_mant_deta_lotesfrutagranel_recepcion_c		iw_mantencion_1
w_mant_deta_movtoenvadeta_recepfruta		iw_mantencion_2

DataWindowChild	idwc_Transp,idwc_Camion,idwc_Variedad,idwc_Predio,&
						idwc_Camara,idwc_especie,idwc_especiedet,idwc_especiedet1,idwc_especiedet2,&
						idwc_planta,idwc_plantadw4,idwc_plancodw4, idwc_plantadw1,idwc_Cliente
DataWindow			dw_3,dw_5,dw_7

Str_mant				istr_mant3, istr_mant4
uo_transportista	iuo_Transport
uo_camiones			iuo_Camion
uo_especie			iuo_Especie
uo_Productores		iuo_Productor
uo_LotesCorrel		iuo_Correlativo
uo_pesoestanespe	iuo_PesoEstanEspe
uo_fechaMovto		iuo_FechaMovto
uo_tipomovtofruta	iuo_TipoMovtoFruta
uo_tipomovtofruta	iuo_TipoMovtoEnva
uo_bins				iuo_bins

Long     il_NumFruta=0, il_NumEnva=0, il_lotes[]
Boolean	ib_AutoCommit, ib_Salida, ib_Destare, ib_ocx, ib_graba_destare
String	is_rut, is_rutprod, is_RutProductor, is_NombreProductor, is_chofer
Integer	ii_Cantidad, ii_Productor, ii_kildec=0, ii_especie
DateTime	idt_FechaSistema
Decimal	id_KilosEnv
Private:
str_Productores_Envases	wstr_Prod_Enva
str_pesaje              wstr_pesaje
str_pesaje              wstr_pesajeCarro
str_puertacomm		      istr_puertacomm

end variables

forward prototypes
public subroutine determina_productoresenvase (integer ai_tipomovto)
protected function integer wf_modifica ()
public subroutine destare (boolean ab_actualiza)
public subroutine habilitaingreso (string as_columna)
public subroutine habilitaencab (boolean habilita)
public subroutine habilitagrabacion (string as_columna)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean chequea_envasesproductor ()
public subroutine productoreslotes (ref string productores[])
public function boolean noexistechofer (string rut)
public subroutine habilitasalida ()
public subroutine actualizakiloslote ()
public subroutine buscacamion ()
public subroutine captura_totales ()
public function boolean actual_ultimo_lote ()
public function boolean noexistecliente (integer cliente)
public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, integer al_numero, integer ai_cliente)
public function boolean procesopacking ()
public function boolean ordenproceso ()
public subroutine asignapeso ()
public function boolean existedocproceso (integer ai_planta, long al_numero)
public function boolean existemovtoproceso (integer ai_planta, integer ai_tipomovto, integer ai_tipodocto, long al_proceso)
public subroutine f_membreteds (datastore ds_1)
end prototypes

event ue_imprimir_tarja();SetPointer(HourGlass!)
Datawindowchild  	ldwc_lotes
DataStore 		  	lds_Informe 
Long				  	fila,ll_fila, respuesta
String 	      		n_lote, tmp, command
Integer 				li_imprimir, li_bultos, li_tarjas

li_imprimir 	=	MessageBox("Lote", "¿ Desea Imprimir una tarja para cada Bulto del Lote ?~r"+&
												"Presione No para imprimir solo una tarja~r"+&
												"o Cancelar para no imprimir tarjas", Question!, YesNoCancel!, 1)

IF li_imprimir <> 3 THEN
	
	lds_informe	= Create DataStore
	lds_Informe.DataObject = "dw_info_lotesfrutagranel_recepcion_grand"
	
	DO 
		lds_Informe.SetTransObject(Sqlca)
		ll_fila	= lds_Informe.Retrieve(dw_2.Object.plde_codigo[dw_2.GetRow()],Integer(istr_mant.argumento[10]),&
										  1,dw_2.Object.mfge_numero[dw_2.GetRow()])
		IF fila = -1 THEN
			MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
							"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
		ELSE
			FOR ll_fila = 1 TO tab_1.tp_1.dw_lotes.RowCount()
		 
				n_lote =  String(tab_1.tp_1.dw_lotes.object.compute_2[ll_fila])
			 
				IF li_imprimir = 1 THEN
					li_bultos	=	tab_1.tp_1.dw_lotes.Object.lote_totbul[ll_fila]
				ELSE
					li_bultos	=	1
				END IF
				
				lds_Informe.SetFilter("compute_2 = '"+ n_lote +" '")
				lds_Informe.Filter()
				
				FOR li_tarjas = 1 TO li_bultos
					lds_Informe.Print()
				NEXT	
				
			NEXT
			SetPointer(Arrow!)
		END IF
	LOOP WHILE respuesta = 1

END IF
	
	
Destroy lds_informe

IF NOT gstr_paramplanta.binsabins AND NOT gstr_paramplanta.palletdebins THEN Return

li_imprimir 	=	MessageBox("Lote", "¿ Desea Imprimir Anexo de Pesaje del Lote ?~r", Question!, YesNo!, 1)

IF li_imprimir = 1 THEN
	lds_informe	= Create DataStore
	
	IF NOT gstr_paramplanta.palletdebins THEN
		lds_Informe.DataObject = "dw_info_detalle_pesaje_bins"
	ELSE
		lds_Informe.DataObject = "dw_info_detalle_pesaje_bins_palletgranel"
	END IF
	
	lds_Informe.SetTransObject(Sqlca)
	ll_fila	= lds_Informe.Retrieve(dw_2.Object.plde_codigo[dw_2.GetRow()],&
										  1,dw_2.Object.mfge_numero[dw_2.GetRow()],&
										  Integer(istr_mant.argumento[10]))
	IF fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSE
		f_membreteds(lds_Informe)
		lds_Informe.Print()
	END IF
END IF
end event

public subroutine determina_productoresenvase (integer ai_tipomovto);str_Productores_Envases	lstr_Prod_Enva
DataWindow	ldw_envase
Long			ll_Fila, ll_GuiaSII, ll_Fila_Lote
Integer		li_Secuencia, li_Cliente, li_ClieAnt
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
   li_Cliente		=	ldw_Envase.Object.clie_codigo[ll_Fila]
	
	IF (ll_Productor <> ll_ProdAnt) AND (li_Cliente <> li_ClieAnt) THEN
		ll_ProdAnt	=	ll_Productor
		li_ClieAnt	=	li_Cliente
		li_Secuencia ++
		
		IF ai_TipoMovto = 41 THEN
			ll_Fila_Lote	=	dw_3.Find(" prod_codigo = "+String(ll_Productor),1,dw_3.RowCount())
			
			IF ll_Fila_Lote > 0 THEN
				ll_GuiaSII	=	dw_3.Object.lote_guisii[ll_Fila_Lote]
			ELSE
				ll_GuiaSII	=	0
			END IF
		END IF
		
		wstr_Prod_Enva.Productor[li_Secuencia]	=	ll_Productor
		wstr_Prod_Enva.GuiaSII[li_Secuencia]	=	ll_GuiaSII
		wstr_Prod_Enva.TipoMovto[li_Secuencia]	=	ai_TipoMovto
		wstr_Prod_Enva.Cliente[li_Secuencia]	=	li_Cliente
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

public subroutine destare (boolean ab_actualiza);Long			ll_Fila, li_filas, ll_Fila_lote
Integer		li_Especie, li_TipoEnvase, li_Envase, li_TipoAnt, li_EnvaAnt,&
				li_TotBultos, li_Secuencia, li_Lote, li_LoteAnt, li_BultosLote, li_Cliente
Boolean		lb_FueraRango
Date			ld_Fechamovto
Decimal{3}	ld_TotalNeto, ld_PesoEstandar[], ld_PesoMinimo[], ld_PesoMaximo[],&
				ld_TotalEstandar, ld_PesoDistrib, ld_LoteDistrib, ld_NetoLoteEnvase,&
				ld_TotalDistrib, ld_Remanente
Double		ld_Factor
/*
INICIO Cambio hecho en coquimbo
*/
Decimal Ld_entradaa, Ld_entradab, Ld_salidaa, Ld_salidab, ld_enven, ld_envsan, ld_TaraBultos

Ld_entradaa			=	dw_2.Object.refg_tkbent[1] 
Ld_entradab			=	dw_2.Object.refg_tkbenc[1]
Ld_salidaa			= 	dw_2.Object.refg_tkbsal[1]
Ld_salidab			=	dw_2.Object.refg_tkbsac[1] 

//IF gstr_paramplanta.palletdebins THEN
//	
//	//Busca los datos de la base pallet especificada en la tabla de parametros.
//	IF ab_actualiza THEN	
//		FOR li_filas = 1 to dw_spro_bins.RowCount()	
//			
//			iuo_bins.Existe(dw_1.Object.clie_codigo[1], dw_1.Object.lote_pltcod[1], &
//								 dw_spro_bins.Object.fgmb_nrotar[li_filas], sqlca, TRUE)
//								 
//			ld_TaraBultos	=	ld_TaraBultos + iuo_bins.cale_pesoen
//		NEXT																		
//			
//		dw_2.Object.refg_tkenen[1]	=	id_KilosEnv + ld_TaraBultos//Tara acumulada de las bases de pallets
//	END IF
//END IF

ld_enven				=	dw_2.Object.refg_tkenen[1]
ld_envsan			=	dw_2.Object.refg_tkensa[1]
dw_2.Object.mfge_tpneto[1] = (Ld_entradaa - Ld_salidaa) + ( Ld_entradab - Ld_salidab) - ( ld_enven + ld_envsan)
/*
FIN Cambio hecho en coquimbo
*/
li_Especie		=	dw_2.Object.espe_codigo[1]
ld_FechaMovto	=	dw_2.Object.mfge_fecmov[1]
ld_TotalNeto	=	dw_2.Object.mfge_tpneto[1]
li_Cliente			=	dw_2.Object.clie_codigo[1]

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
	
	li_TotBultos			=	dw_6.Object.lotd_totbul[ll_Fila]
	ld_TotalEstandar	=	ld_TotalEstandar + (li_TotBultos*ld_PesoEstandar[li_Secuencia])
	
NEXT

ld_Factor			=	ld_TotalNeto / ld_TotalEstandar
li_Secuencia		=	0
li_TipoAnt		=	0
li_EnvaAnt		=	0

FOR ll_Fila = 1 TO dw_6.RowCount()
	li_Lote			=	dw_6.Object.lote_codigo[ll_Fila]
	li_TipoEnvase	=	dw_6.Object.enva_tipoen[ll_Fila]
	li_Envase			=	dw_6.Object.enva_codigo[ll_Fila]
	
	IF li_Lote <> li_LoteAnt THEN
		IF li_LoteAnt > 0 THEN
			ld_PesoDistrib = Round(ld_LoteDistrib / li_BultosLote,ii_kildec)
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
	
	li_TotBultos			=	dw_6.Object.lotd_totbul[ll_Fila]
	ld_PesoDistrib		=	Round(ld_PesoEstandar[li_Secuencia] * ld_Factor,ii_kildec)
	ld_NetoLoteEnvase	=	Round(ld_PesoDistrib * li_TotBultos,ii_kildec)
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
	ld_PesoDistrib = Round(ld_LoteDistrib / li_BultosLote,ii_kildec)
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

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date		ld_Fecha
Time		lt_hora
Integer	li_Estado

li_estado	=	dw_2.Object.mfge_estmov[1]

IF Isnull(li_estado) THEN li_estado = 1

IF as_Columna <> "mfge_fecmov" AND &
	(dw_2.Object.mfge_fecmov[1] = ld_Fecha OR IsNull(dw_2.Object.mfge_fecmov[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "defg_docrel" and gb_RecepcionDeProceso AND &
	(dw_2.Object.defg_docrel[1] = 0 OR IsNull(dw_2.Object.defg_docrel[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "refg_horaen" AND &
	(dw_2.Object.refg_horaen[1] = lt_hora OR IsNull(dw_2.Object.refg_horaen[1])) THEN
	lb_Estado = False
END IF

IF NOT gb_RecepcionDeProceso THEN
	
	IF as_Columna <> "tran_codigo" AND &
		(dw_2.Object.tran_codigo[1] = 0 OR IsNull(dw_2.Object.tran_codigo[1])) THEN
		lb_Estado = False
	END IF
		
	IF as_Columna <> "cami_patent" AND &
		(dw_2.Object.cami_patent[1] = "" OR IsNull(dw_2.Object.cami_patent[1])) THEN
		lb_Estado = False
	END IF
		
	IF as_Columna <> "mfge_chofer" AND &
		(dw_2.Object.mfge_chofer[1] = "" OR IsNull(dw_2.Object.mfge_chofer[1])) THEN
		lb_Estado = False
	END IF
	
END IF

//IF as_Columna <> "mfge_totbul"
//AND (dw_2.Object.mfge_totbul[1] = 0 OR IsNull(dw_2.Object.mfge_totbul[1]))
//THEN
//	lb_Estado = False
//END IF

IF li_Estado = 3 THEN 
	lb_Estado	=	False
END IF

tab_1.tp_1.Enabled	=	lb_Estado
tab_1.tp_2.Enabled	=	lb_Estado
pb_ins_det.Enabled	=	lb_Estado
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
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
	
	dw_2.Object.espe_codigo.Color		=	Rgb(255,255,255)
	dw_2.Object.tran_codigo.Color			=	Rgb(255,255,255)
	dw_2.Object.cami_patent.Color		=	Rgb(255,255,255)
	dw_2.Object.cami_patcar.Color		=	Rgb(255,255,255)
	dw_2.Object.mfge_rutcho.Color		=	Rgb(255,255,255)
	dw_2.Object.mfge_chofer.Color		=	Rgb(255,255,255)
	dw_2.Object.mfge_totbul.Color			=	Rgb(255,255,255)
	dw_2.Object.refg_tkbent.Color			=	Rgb(255,255,255)
	dw_2.Object.refg_tkbenc.Color			=	Rgb(255,255,255)
	dw_2.Object.mfge_fecmov.Color		=	Rgb(255,255,255)
	dw_2.Object.refg_horaen.Color		=	Rgb(255,255,255)
	dw_2.Object.mfge_totbul.Color			=	Rgb(255,255,255)
	dw_2.Object.refg_tkbsal.Color			=	Rgb(255,255,255)
	dw_2.Object.refg_tkbsac.Color			=	Rgb(255,255,255)
	dw_2.Object.refg_horasa.Color			=	Rgb(255,255,255)
	dw_2.Object.refg_fecsal.Color			=	Rgb(255,255,255)

	dw_2.Object.espe_codigo.BackGround.Color	=	553648127
	dw_2.Object.tran_codigo.BackGround.Color		=	553648127
	dw_2.Object.cami_patent.BackGround.Color		=	553648127
	dw_2.Object.cami_patcar.BackGround.Color		=	553648127
	dw_2.Object.mfge_rutcho.BackGround.Color	=	553648127
	dw_2.Object.mfge_chofer.BackGround.Color	=	553648127
	dw_2.Object.mfge_totbul.BackGround.Color		=	553648127
	dw_2.Object.refg_tkbent.BackGround.Color		=	553648127
	dw_2.Object.refg_tkbenc.BackGround.Color		=	553648127
	dw_2.Object.mfge_fecmov.BackGround.Color	=	553648127
	dw_2.Object.refg_horaen.BackGround.Color		=	553648127
	dw_2.Object.mfge_totbul.BackGround.Color		=	553648127
	dw_2.Object.refg_tkbsal.BackGround.Color		=	553648127
	dw_2.Object.refg_tkbsac.BackGround.Color		=	553648127
	dw_2.Object.refg_horasa.BackGround.Color		=	553648127
	dw_2.Object.refg_fecsal.BackGround.Color		=	553648127
	tab_1.tp_3.Enabled								=	False
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
	
	dw_2.Object.refg_tkbent.Color	=	0
	dw_2.Object.refg_tkbenc.Color	=	0
	dw_2.Object.refg_tkbsal.Color	=	0
	dw_2.Object.refg_tkbsac.Color	=	0
	dw_2.Object.mfge_totbul.Color	=	0
	
	dw_2.Object.refg_tkbent.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.refg_tkbenc.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.refg_tkbsal.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.refg_tkbsac.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.mfge_totbul.BackGround.Color	=	RGB(255,255,255)	

	tab_1.tp_3.Enabled								=	True
END IF

dw_2.Object.tran_codigo.Protect					=	0
dw_2.Object.cami_patent.Protect					=	0
dw_2.Object.cami_patcar.Protect					=	0
dw_2.Object.mfge_rutcho.Protect					=	0
dw_2.Object.mfge_chofer.Protect					=	0

dw_2.Object.tran_codigo.Color		=	0	
dw_2.Object.cami_patent.Color	=	0
dw_2.Object.cami_patcar.Color	=	0
dw_2.Object.mfge_rutcho.Color	=	0
dw_2.Object.mfge_chofer.Color	=	0

dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
dw_2.Object.cami_patent.BackGround.Color		=	RGB(255,255,255)
dw_2.Object.cami_patcar.BackGround.Color		=	RGB(255,255,255)
dw_2.Object.mfge_rutcho.BackGround.Color	=	RGB(255,255,255)
dw_2.Object.mfge_chofer.BackGround.Color	=	RGB(255,255,255)
end subroutine

public subroutine habilitagrabacion (string as_columna);Boolean	lb_Estado = True
Date		ld_Fecha
Time		lt_Hora

IF as_Columna <> "refg_fecsal" AND &
	(dw_2.Object.refg_fecsal[1] = ld_Fecha OR IsNull(dw_2.Object.refg_fecsal[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "refg_horasa" AND &
	(dw_2.Object.refg_horasa[1] = lt_Hora OR IsNull(dw_2.Object.refg_horasa[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "refg_tkbsal" AND &
	(dw_2.Object.refg_tkbent[1] = 0 OR IsNull(dw_2.Object.refg_tkbent[1])) THEN
	lb_Estado = False
END IF

pb_Grabar.Enabled	=	lb_Estado
end subroutine

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
						IF dw_5.Update(True,False) = 1 THEN			//Envases Recepcionados
							IF dw_7.Update(True,False) = 1 THEN		//Envases Retirados
								IF dw_4.Update(True,False) = 1 THEN	//Encabezados Envases
									IF dw_spro_bins.Update(True,False) = 1 THEN
										IF dw_desverd.Update(True,False) = 1 THEN
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
//							ELSE
//								F_ErrorBaseDatos(sqlca, This.Title)
//								RollBack;
//							END IF
//						ELSE
//							F_ErrorBaseDatos(sqlca, This.Title)
//							RollBack;
//						END IF
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

public function boolean chequea_envasesproductor ();Boolean	lb_Retorno	=	True
Long		ll_Fila,  ll_Fila_det,ll_Productor 
Integer	li_TipoEnvase, li_Envase, li_Bultos, li_BultosAnt, li_posicion,&
			li_Bultos_Enc, li_Lote_pltcod, li_Lote_espcod, li_Lote_codigo, li_Total_Bultos,&
			li_Cliente
String	ls_linea_chequeo, ls_busqueda

li_Bultos_Enc	=	dw_2.Object.mfge_totbul[1]
li_Cliente		=	dw_2.Object.clie_codigo[1]

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
		
		ls_busqueda		= 'P'+String(ll_Productor,'0000')+'T'+String(li_TipoEnvase)+'E'+String(li_Envase,'000')
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
	ll_Productor		=	dw_5.Object.prod_codigo[ll_Fila]
	li_TipoEnvase	=	dw_5.Object.enva_tipoen[ll_Fila]
	li_Envase			=	dw_5.Object.enva_codigo[ll_Fila]
	li_Bultos			=	dw_5.Object.fgme_cantid[ll_Fila]
	
	IF dw_5.Object.fgme_conenv[ll_Fila] = 1 THEN
		ls_busqueda		= 'P'+String(ll_Productor,'00000')+'T'+String(li_TipoEnvase)+'E'+String(li_Envase,'000')
		li_posicion 	= 	Pos(ls_linea_chequeo,ls_busqueda)
		
		IF li_posicion = 0 THEN
			ls_linea_chequeo	=	ls_linea_chequeo + ls_busqueda + 'B0000' + 'I' + String(li_Bultos,'0000') 
		ELSE
			li_BultosAnt			=	Integer(Mid(ls_linea_chequeo,li_posicion + 17, 4))
			li_Bultos				=	li_BultosAnt + li_Bultos
			ls_linea_chequeo	=	Mid(ls_linea_chequeo, 1, li_posicion + 16 ) + &
										String(li_Bultos,'0000') + Mid(ls_linea_chequeo, li_posicion + 21)
		END IF
	END IF
NEXT

//Chequeo de Envases Por Productor
//FOR li_posicion = 1 TO Len(ls_linea_chequeo) STEP 21
//	ls_busqueda		=	Mid(ls_linea_chequeo, li_posicion, 21)
//	li_Bultos		=	Integer(Mid(ls_busqueda, 13, 4))
//	li_BultosAnt	=	Integer(Mid(ls_busqueda, 18, 4))
//	
//	IF li_Bultos <> li_BultosAnt THEN
//		MessageBox("Atención",String(li_BultosAnt,'#,##0')+" Envases con Fruta"+&
//						" declarados para el Productor "+Mid(ls_busqueda, 2, 4)+&
//						" y Envase "+Mid(ls_busqueda, 7, 1)+"-"+Mid(ls_busqueda, 9 ,3)+&
//						" no corresponde "+String(li_Bultos,'#,##0')+" a los Bultos"+&
//						" recepcionados en Detalle de Lotes")
//		lb_Retorno	=	False
//		EXIT
//	END IF
//NEXT

IF lb_Retorno THEN
//	IF li_Total_Bultos <> li_Bultos_Enc THEN
//		MessageBox("Atención","Total Envases con Fruta no corresponde a Total Bultos recepcionados")
//		lb_Retorno	=	False
//	END IF
END IF

RETURN lb_Retorno
end function

public subroutine productoreslotes (ref string productores[]);Long		ll_Fila
Integer	li_Secuencia
String	ls_productor, ls_ProdAnt, ls_Nula[]
productoresxlote	istr_pxl

Productores	=	ls_Nula

FOR ll_Fila = 1 TO dw_3.RowCount()
	ls_Productor	=	dw_3.Object.prod_rut[ll_Fila]
	
	istr_pxl.prod_rut[ll_fila]			=	dw_3.Object.prod_rut[ll_Fila]
	istr_pxl.prod_codigo[ll_fila]		=	dw_3.Object.prod_codigo[ll_Fila]
	istr_pxl.prod_nombre[ll_fila]	=	dw_3.Object.prod_nombre[ll_Fila]
	istr_pxl.lote_codigo[ll_fila]		=	dw_3.Object.lote_codigo[ll_Fila]
	istr_pxl.lote_pltcod[ll_fila]		=	dw_3.Object.lote_pltcod[ll_Fila]
	istr_pxl.lote_espcod[ll_fila]		=	dw_3.Object.lote_espcod[ll_Fila]

	IF ls_Productor <> ls_ProdAnt THEN
		li_Secuencia ++
		Productores[li_Secuencia]	=	ls_Productor
	
		ls_ProdAnt	=	ls_Productor
	END IF
NEXT

//Integer li_filas_dw6, li_filas_dw4, li_bulto
//
//dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
//
//FOR li_filas_dw6 = 1 TO dw_6.RowCount()
//	
//	li_filas_dw4	=	dw_4.find( 'enva_tipoen = ' + String(dw_6.Object.enva_tipoen[li_filas_dw6]) + ' and ' +&
//									   'enva_codigo = ' + String(dw_6.Object.enva_codigo[li_filas_dw6]) + ' and ' +&
//										 + ' and ' +&, 1, dw_2.RowCount() )
//										
//	IF li_filas_dw4 < 1 THEN
//		li_filas_dw4	=	dw_4.InsertRow(0)
//		dw_4.Object.lote_pltcod[li_filas_dw4]	=	Integer(istr_mant.Argumento[1])
//		dw_4.Object.lote_espcod[li_filas_dw4]	=	Integer(istr_mant.Argumento[5])
//		dw_4.Object.lote_codigo[li_filas_dw4]	=	dw_6.Object.lote_codigo[li_filas_dw6]
//		dw_4.Object.enva_tipoen[li_filas_dw4]	=	dw_6.Object.enva_tipoen[li_filas_dw6]
//		dw_4.Object.enva_codigo[li_filas_dw4]	=	dw_6.Object.enva_codigo[li_filas_dw6]
//	END IF
//	
//	li_bulto 											=	dw_4.Object.lotd_totbul[li_filas_dw4]
//	
//	IF IsNull(li_bulto) THEN li_bulto = 0
//	
//	dw_4.Object.lotd_totbul[li_filas_dw4] 	=	li_bulto + 1
//	
//NEXT

RETURN
end subroutine

public function boolean noexistechofer (string rut);String ls_nombre,ls_paterno,ls_materno

SELECT 	clpr_nombre,clpr_apepat,clpr_apemat
INTO		:ls_nombre, :ls_paterno , :ls_materno
FROM 		dbo.clienprove
WHERE		clpr_nrorut =:rut;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClienProve")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Rut No ha sido Generado. Ingrese Otro.")
	Return True
END IF

is_chofer	= 	ls_nombre + " " + ls_paterno + " " + ls_materno
Return False





end function

public subroutine habilitasalida ();
dw_2.Object.refg_tkbsal.Protect					=	0
dw_2.Object.refg_tkbsal.BackGround.Color		=	RGB(255,255,255)
dw_2.Object.refg_tkbsac.Protect					=	0
dw_2.Object.refg_tkbsac.BackGround.Color		=	RGB(255,255,255)
dw_2.Object.refg_horasa.Protect					=	0
dw_2.Object.refg_horasa.BackGround.Color		=	RGB(255,255,255)
dw_2.Object.refg_fecsal.Protect					=	0
dw_2.Object.refg_fecsal.BackGround.Color		=	RGB(255,255,255)
tab_1.tp_3.Enabled									=	True

dw_2.Object.refg_tkbent.Protect					=	1
dw_2.Object.refg_tkbent.BackGround.Color		=	RGB(192,192,192)
dw_2.Object.refg_tkbenc.Protect					=	1
dw_2.Object.refg_tkbenc.BackGround.Color		=	RGB(192,192,192)
dw_2.Object.mfge_fecmov.Protect					=	1
dw_2.Object.mfge_fecmov.BackGround.Color		=	RGB(192,192,192)
dw_2.Object.refg_horaen.Protect					=	1
dw_2.Object.refg_horaen.BackGround.Color		=	RGB(192,192,192)
dw_2.Object.mfge_totbul.Protect					=	1
dw_2.Object.mfge_totbul.BackGround.Color		=	RGB(192,192,192)

ib_Salida													=	True

dw_2.Object.destare.Text							=	'Destare'

idt_FechaSistema										=	F_FechaHora()


dw_2.Object.refg_fecsal[1]							=	Date(String(idt_FechaSistema,'dd/mm/yyyy'))
dw_2.Object.refg_horasa[1]						=	Time(Mid(String(idt_FechaSistema),12,8))

pb_grabar.Enabled									=	False
pb_imprimir.Enabled									=	False

captura_totales()
end subroutine

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

public subroutine buscacamion ();Str_busqueda	lstr_busq

lstr_busq.argum[1] = '1'

OpenWithParm(w_busc_camiones, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_2.SetColumn("cami_patent")
	dw_2.SetFocus()
ELSE
	dw_2.Object.cami_clasifi[il_fila]		=	Integer(lstr_busq.argum[1])
	dw_2.Object.cami_patent[il_fila]		=	lstr_busq.argum[2]
	dw_2.Object.cami_patcar[il_fila]		=	lstr_busq.argum[6]
	dw_2.Object.mfge_rutcho[il_fila]		=	lstr_busq.argum[5]
	is_rut 										= lstr_busq.argum[5]
	dw_2.Object.mfge_chofer[il_fila]		=	lstr_busq.argum[4]
	dw_2.SetFocus()
END IF

RETURN
end subroutine

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
	ld_KBE_Carro	=	dw_2.Object.refg_tkbenc[1]
	
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
		MessageBox("Atención","Kilos de salida no pueden ser menor a los envases de salida")
		RETURN
	ELSE
		ld_Total_KBS	=	ld_KBS_Camion + ld_KBS_Carro - ld_Total_PesoEnv_Sal
	END IF
		
//	ld_Total_Neto	=	ld_Total_KBE - ld_Total_PesoEnv_Ent - ld_Total_KBS
	ld_Total_Neto	=	ld_Total_KBE - ld_Total_PesoEnv_Ent

	dw_2.Object.refg_tkensa[1]	=	ld_Total_PesoEnv_Sal
	dw_2.Object.mfge_tpneto[1]	=	ld_Total_Neto

END IF

dw_2.Object.refg_tkenen[1]	=	ld_Total_PesoEnv_Ent

IF ib_Destare THEN
	Destare(True)
END IF

RETURN
end subroutine

public function boolean actual_ultimo_lote ();Long 		ll_Filas, ll_Fila, ll_Lote
Integer 	li_Lote_espcod, li_planta
String		ls_nombrepc

li_Planta	=	Integer(istr_mant.Argumento[1])
ll_Filas		=	dw_3.RowCount()
ll_Fila		=	0
ll_Lote		=	0

DO WHILE ll_Fila < ll_Filas
		ll_Fila = dw_3.GetNextModified(ll_fila, Primary!)
		IF ll_Fila > 0 AND dw_3.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
			IF ll_Lote < dw_3.Object.lote_codigo[ll_Fila] THEN
				ll_lote 			=	dw_3.Object.lote_codigo[ll_Fila]
				li_Lote_espcod	=	dw_3.Object.lote_espcod[ll_Fila]
			END IF
//		ELSE
//			ll_Fila = ll_Fila + 1
		END IF
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

public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, integer al_numero, integer ai_cliente);Long		ll_Numero
Boolean	lb_Retorno = True

SELECT	mfge_numero
	INTO	:ll_Numero
	FROM	dbo.spro_movtofrutagranenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfge_numero	=	:al_Numero
	AND   clie_codigo =  :ai_Cliente;
	
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

public function boolean procesopacking ();integer 	li_clie_codigo, li_i, li_hora, li_mins, li_mfge_numero
date 		ld_date_actual

//---------------variables para spro_ordenprocvacdeta
String 	ls_cale_calida
Decimal	ld_KilosRecep, ld_Tara, ld_NetoOriginal, ld_Neto, ld_KilosPromedio
time 		lt_hora_ivaciado, lt_hora_tvaciado

//---------------variables para lotefrutagranel
integer 	li_vari_codigo, li_prod_codigo, li_pefr_codigo
String	ls_frio_tipofr

//---------------variables para lotefrutagrandeta
integer 	li_plde_codigo, li_lote_espcod, li_lote_codigo, li_enva_tipoen, &
		  	li_enva_codigo, li_lotd_totbul, li_lotd_totnet, li_lotd_kilpro
			  
//---------------variables para orden de proceso 
integer 	li_orpr_tipoord, li_orpr_numero

//---------------variables para programa de proceso 
integer 	li_ppre_numero, li_ppre_feccre
Long 		ll_numero
uo_spro_ordenproceso luo_ordenproceso

luo_ordenproceso	=	Create uo_spro_ordenproceso

li_clie_codigo 	= 	dw_2.Object.clie_codigo[1]
li_mfge_numero 	= 	dw_2.Object.mfge_numero[1]

li_plde_codigo 	= 	dw_6.Object.lote_pltcod[1]

ll_Numero			=	luo_OrdenProceso.MaximoNumero(li_plde_codigo, 7, Sqlca, li_clie_codigo)

IF ll_Numero>0 THEN
	MessageBox("Atencion","El Número de Orden de Proceso Generado es " + String(ll_Numero) + ". ~rEste se debera usar para etiquetar las cajas recepcionadas.")
ELSE
	MessageBox("Error de Ingreso","No se pudo detectar la última orden de proceso. ~rCierre la ventana y vuelva a intentarlo.")
	Return FALSE
END IF

li_lote_espcod		= 	dw_6.Object.lote_espcod[1]
li_lote_codigo		= 	dw_6.Object.lote_codigo[1]
li_enva_tipoen		= 	dw_6.Object.enva_tipoen[1]
li_enva_codigo		= 	dw_6.Object.enva_codigo[1]
li_lotd_totbul		= 	dw_6.Object.lotd_totbul[1]
li_lotd_totnet		= 	dw_6.Object.lotd_totnet[1]
li_lotd_kilpro		= 	dw_6.Object.lotd_kilpro[1]

SELECT	vari_codigo, prod_codigo, lote_totnet, frio_tipofr, pefr_codigo
	INTO	:li_vari_codigo, :li_prod_codigo, :ld_kilosRecep, :ls_frio_tipofr, :li_pefr_codigo
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

ld_date_actual 	= 	DATE(STRING(TODAY(), "dd/mm/yyyy"))
li_hora 				= 	Hour(now())
li_mins 				= 	Minute(now())
lt_hora_ivaciado 	= 	time(li_hora, li_mins,0)
lt_hora_tvaciado 	= 	time(li_hora + 1, li_mins,0)

//*********************************** tablas de encabezados

INSERT INTO dbo.spro_movtofrutagranenca  
	( clie_codigo, plde_codigo, tpmv_codigo,   
	  mfge_numero, tran_codigo, espe_codigo,   
	  cami_clasifi, cami_patent, cami_patcar,   
	  mfge_fecmov, mfge_estmov, mfge_guisii,   
	  mfge_chofer, mfge_observ, mfge_tpneto,   
	  mfge_totbul, prod_codigo, sepl_codigo,   
	  refg_horaen, refg_horasa, refg_tkbent,   
	  refg_tkenen, refg_tkbsal, refg_tkensa,   
	  refg_guienv, refg_moenen, refg_mosaen,   
	  refg_diagra, moti_codigo, defg_tipdoc,   
	  defg_docrel, mfge_dirdes, mfge_despac,   
	  mfge_avisar, mfge_rutcho, refg_tkbenc,   
	  refg_tkbsac, refg_tipcom, refg_descas,   
	  rfge_kenrom, rfge_kenroc, rfge_ksarom,   
	  rfge_ksaroc, mfge_usuari, mfge_comput,   
	  mfge_horacr, mfge_usumod, mfge_commod,   
	  mfge_horact, refg_fecsal, plde_coorde )  
VALUES ( :li_clie_codigo, :li_plde_codigo, 26,   
	  :li_mfge_numero, null, :li_lote_espcod,   
	  null, null, null,   
	  :ld_date_actual, 1, null,   
	  null, null, null,   
	  null, :li_prod_codigo, null,   
	  null, null, null,   
	  null, null, null,   
	  null, null, null,   
	  null, null, 7,   
	  :li_lote_codigo, null, null,   
	  null, null, null,
	  null, null, null,
	  null, null, null,
	  null, null, null,
	  null, null, null,
	  null, null, :li_plde_codigo )  ;
	  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagranenca")
	Return FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
	Return FALSE
END IF


INSERT INTO dbo.spro_movtoenvaenca  
		( clie_codigo, plde_codigo, tpmv_codigo,   
		  meen_numero, plde_coorde, prod_codigo,   
		  tran_codigo, cami_clasifi, cami_patent,   
		  tpmv_codrec, mfge_numero, cami_patcar,   
		  meen_guisii, meen_fecmov, meen_dirdes,   
		  meen_despac, meen_avisar, meen_rutcho,   
		  meen_chofer, meen_usuari, meen_comput,   
		  meen_horacr, meen_usumod, meen_commod,   
		  meen_horact, clpr_rut, meen_modulo )  
VALUES ( :li_clie_codigo, :li_plde_codigo, 26,   
		  :li_lote_codigo, :li_plde_codigo, :li_prod_codigo,   
		  null, null, null,   
		  26, :li_mfge_numero, null,   
		  null, :ld_date_actual, null,   
		  null, null, null,
		  null, null, null,
		  null, null, null,
		  null, null, null )  ;
		  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtoenvaenca")
	Return FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
	Return FALSE
END IF


INSERT INTO dbo.spro_ordenproceso  
	( clie_codigo, plde_codigo, orpr_tipord,   
	  orpr_numero, orpr_fecpro, prod_codigo,   
	  espe_codigo, vari_codigo, line_codigo,   
	  frio_tipofr, pefr_codigo, ppre_numero,   
	  orpr_nrotur, orpr_estado, orpr_canbul,   
	  orpr_bulpro, orpr_niveld, orpr_tippro,   
	  ppre_feccre )  
VALUES (:li_clie_codigo, :li_plde_codigo, 7,   
	  :ll_Numero, :ld_date_actual, :li_prod_codigo,   
	  :li_lote_espcod, :li_vari_codigo, 1,   
	  :ls_frio_tipofr, :li_pefr_codigo, 99999,   //datos genericos
	  1, 1, :li_lotd_totbul,   
	  null, 1, 1, null )  ;
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenproceso")
	Return FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
	Return FALSE
END IF


INSERT INTO dbo.spro_ordenprocvacenca  
	( plde_codigo, orpr_tipord, orpr_numero,   
	  clie_codigo, opve_fecvac, opve_turno,   
	  opve_estado, line_codigo )  
VALUES ( :li_plde_codigo, 7, :ll_Numero,   
	  :li_clie_codigo, :ld_date_actual, 1,   
	  'V', 1 )  ;
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocvacenca")
	Return FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
	Return FALSE
END IF
	

IF li_clie_codigo = 81 THEN
	ls_cale_calida = "1"
ELSE
	ls_cale_calida = "2"
END IF


INSERT INTO dbo.spro_terminoproceso  
	( plde_codigo, tpmv_codigo, tepr_numero,   
	  tepr_secuen, lote_pltcod, lote_espcod,   
	  lote_codigo, lote_secuen, enva_tipoen,   
	  enva_codigo, clie_codigo, tepr_bultra,   
	  tepr_bulvac )  
VALUES (:li_plde_codigo, 26, :ll_Numero,   
	  1, :li_plde_codigo, :li_lote_espcod,   
	  :li_lote_codigo, :Li_i, :li_enva_tipoen,   
	  :li_enva_codigo, :li_clie_codigo, :li_lotd_totbul,   
	  :li_lotd_totbul )  ;
		  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_terminoproceso")
	Return FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
	Return FALSE
END IF

INSERT INTO dbo.spro_movtofrutaembaenca  
	( clie_codigo, plde_codigo, tpmv_codigo,   
	  mfee_numero, mfee_fecmov, plde_coorde,   
	  tran_codigo, cami_clasifi, cami_patent,   
	  cami_patcar, mfee_chofer, mfee_tipdoc,   
	  mfee_docrel, moti_codigo, puer_codigo,   
	  embq_codigo, mfee_guides, mfee_horsal,   
	  mfee_horarr, etiq_codigo, etiq_codnue,   
	  expo_codigo, mfee_nturno, mfee_plasag,   
	  mfee_termog, mfee_observ, mfee_cantar,   
	  mfee_tardef, mfee_cancaj, mfee_cajmer,   
	  mfee_usuari, mfee_comput, mfee_horacr,   
	  mfee_usumod, mfee_commod, mfee_horact,   
	  tica_codigo, mfee_rutcho, pcfl_numero,   
	  line_codigo, mfee_palins )  
VALUES ( :li_clie_codigo, :li_plde_codigo, 7,   
	  :ll_Numero, :ld_date_actual, null,   
	  null, null, null,   
	  null, null, 7,   
	  :li_lote_codigo, null, null,   
	  null, null, null,   
	  null, null, null,   
	  1, null, null,   
	  null, null, null,   
	  null, null, null,   
	  null, null, null,   
	  null, null, :lt_hora_ivaciado,   
	  null, null, null,   
	  null, null )  ;
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutaembaenca")
	Return FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
	Return FALSE
END IF
	
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
	
	INSERT INTO dbo.spro_movtofrutagrandeta  
         ( plde_codigo, tpmv_codigo, mfge_numero,   
           clie_codigo, mfgd_secuen, plde_coorde,   
           cama_codigo, lote_pltcod, lote_espcod,   
           lote_codigo, enva_tipoen, enva_codigo,   
           mfgd_bulent, mfgd_kgnent )  
  	VALUES ( :li_plde_codigo, 26, :li_mfge_numero,   
           :li_clie_codigo, :Li_i, :li_plde_codigo,   
           1, :li_plde_codigo, :li_lote_espcod,   
           :li_lote_codigo, :li_enva_tipoen, :li_enva_codigo,   
           :li_lotd_totbul, :li_lotd_totnet )  ;
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagrandeta")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF

	
	INSERT INTO dbo.spro_ordenprocdeta  
		( plde_codigo, orpr_tipord, orpr_numero,   
		  clie_codigo, orpd_secuen, lote_pltcod,   
		  lote_espcod, lote_codigo, enva_tipoen,   
		  enva_codigo, cama_codigo, orpd_nroban,   
		  orpd_nropos, orpd_nropis, orpd_canbul,   
		  orpd_canvac, orpd_horini, orpd_horfin,   
		  orpd_observ )  
	VALUES (:li_plde_codigo, 7, :ll_Numero,   
		  :li_clie_codigo, :Li_i, :li_plde_codigo,   
		  :li_lote_espcod, :li_lote_codigo, :li_enva_tipoen,   
		  :li_enva_codigo, 1, null,   
		  null, null, :li_lotd_totbul,   
		  null, null, null,   
		  null )  ;
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocdeta")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF
	
	ld_NetoOriginal	=	ld_KilosRecep
	ld_Neto				=	ld_NetoOriginal
	ld_KilosPromedio	=	ld_Neto / li_lotd_totbul
	ld_KilosRecep		=	ld_KilosRecep + ( ld_tara * li_lotd_totbul )
	
	INSERT INTO dbo.spro_ordenprocvacdeta  
		( plde_codigo, orpr_tipord, orpr_numero,   
		  clie_codigo, opve_fecvac, opve_turno,   
		  opvd_horava, opvd_horate, lote_pltcod,   
		  lote_espcod, lote_codigo, enva_tipoen,   
		  enva_codigo, cale_calida, opvd_canbul,
		  opvd_kilpro, opvd_kilori, opvd_pesobr, opvd_pesone)  
	VALUES ( :li_plde_codigo, 7, :ll_Numero,   
		  :li_clie_codigo, :ld_date_actual, 1,   
		  :lt_hora_ivaciado, :lt_hora_tvaciado, :li_plde_codigo,   
		  :li_lote_espcod, :li_lote_codigo, :li_enva_tipoen,   
		  :li_enva_codigo, :ls_cale_calida, :li_lotd_totbul,
		  :ld_KilosPromedio, :ld_NetoOriginal, :ld_KilosRecep, :ld_Neto)  ;
	  
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocvacdeta")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF

  	UPDATE dbo.spro_movtofrutagranenca 
   SET mfge_estmov = 2  
   WHERE ( dbo.spro_movtofrutagranenca.plde_codigo = :li_plde_codigo ) AND  
         ( dbo.spro_movtofrutagranenca.clie_codigo = :li_clie_codigo ) AND  
         ( dbo.spro_movtofrutagranenca.tpmv_codigo = 26 ) AND  
         ( dbo.spro_movtofrutagranenca.mfge_numero = :li_mfge_numero );
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
         ( dbo.spro_ordenproceso.orpr_numero = :ll_Numero ) ;
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

public function boolean ordenproceso ();//w_maed_spro_orden_proceso 			lw_ordenp
//
//open(lw_ordenp)
//lw_ordenp.Visible = False
//
//lw_ordenp.dw_2.Object.ppre_numero[1] 	= 1//programa de proceso standard
//lw_ordenp.istr_mant.argumento[3] 		= 1//programa de proceso standard
//
//lw_ordenp.dw_2.Object.ppre_fecpro[1] 	= Date(Today())
//
//lw_ordenp.dw_2.Object.espe_codigo[1] 	= ii_especie
//lw_ordenp.istr_mant.argumento[2]			= ii_especie
//
Return True
end function

public subroutine asignapeso ();Long 		ll_Fila
Decimal 	ld_pesos

IF dw_9.RowCount() > 0 THEN
	FOR ll_Fila = 1 TO dw_9.RowCount()
			ld_pesos =	ld_pesos + dw_9.Object.mfgp_pesore[ll_fila]
	NEXT
END IF
dw_2.Object.refg_tkbent[1]	 = ld_pesos
end subroutine

public function boolean existedocproceso (integer ai_planta, long al_numero);Long		ll_Numero,ll_Productor
Integer	li_Especie, li_Variedad, li_Cantidad, li_Vigencia, &
         li_periodo, li_linea, li_turno, li_Cliente
String   ls_Nombre, ls_frio
Boolean	lb_Retorno = True

li_Cliente	=	Integer(istr_mant.Argumento[10])
/*SELECT	op.orpr_numero, 	op.prod_codigo, 	op.espe_codigo, 
        op.vari_codigo, 	op.orpr_canbul, 	op.orpr_estado, 
    	op.frio_tipofr, 	op.pefr_codigo, 	op.orpr_nrotur, 
		op.line_codigo, 	va.vari_nombre,     va.vari_codigo,
		sum(ovd.opvd_kilori) as KiloProceso
FROM	dbo.spro_ordenproceso op, dbo.variedades va, dbo.spro_ordenprocvacdeta as ovd
WHERE	op.plde_codigo	=	1
	And	op.orpr_tipord	=	8
	And op.orpr_numero	=	104
    AND op.clie_codigo  =   81
	And	va.espe_codigo	=   op.espe_codigo
	And	va.vari_codigo	=	op.vari_codigo
    and op.plde_codigo	=	ovd.plde_codigo
    And	op.orpr_tipord	=	ovd.orpr_tipord
	And op.orpr_numero	=	ovd.orpr_numero
    AND op.clie_codigo  =   ovd.clie_codigo
Group by op.orpr_numero, 	op.prod_codigo, 	op.espe_codigo, 
        op.vari_codigo, 	op.orpr_canbul, 	op.orpr_estado, 
    	op.frio_tipofr, 	op.pefr_codigo, 	op.orpr_nrotur, 
		op.line_codigo, 	va.vari_nombre,     va.vari_codigo;
		*/
SELECT	op.orpr_numero, 
			op.prod_codigo, 
			op.espe_codigo, 
			op.vari_codigo, 
			op.orpr_canbul, 
			op.orpr_estado, 
			op.frio_tipofr, 
			op.pefr_codigo, 
			op.orpr_nrotur, 
			op.line_codigo, 
			va.vari_nombre
			
  INTO	:ll_Numero, 
  			:ll_Productor, 
			:li_Especie, 
			:li_Variedad, 
			:li_Cantidad, 
  			:li_Vigencia, 
			:ls_frio, 
			:li_periodo, 
			:li_turno, 
			:li_linea, 
			:ls_Nombre
			
	FROM	dbo.spro_ordenproceso op, 
			dbo.variedades va
			
	WHERE	op.plde_codigo	=	:ai_Planta
	And	op.orpr_tipord	=	8
	And   op.orpr_numero	=	:al_Numero 
	And	va.espe_codigo	=	op.espe_codigo
	And	va.vari_codigo	=	op.vari_codigo
//	And   va.clie_codigo =  op.clie_codigo
	AND   op.clie_codigo =  :li_Cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Ordenes de Proceso")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
//	IF Not ExisteMovtoProceso(ai_Planta,Integer(istr_mant.argumento[2]),4,ll_Numero) THEN
		istr_mant4.Argumento[5] 		=	String(ll_Numero)
		istr_mant4.Argumento[6]  		= 	String(ll_Productor)
		istr_mant4.Argumento[8]  		= 	String(li_Especie)
		istr_mant4.Argumento[9]  		= 	String(li_Variedad)
		istr_mant4.Argumento[10]		= 	ls_Nombre
		istr_mant4.Argumento[11] 		= 	String(li_Cantidad)
		istr_mant4.Argumento[12] 		=  	ls_frio
		istr_mant4.Argumento[13] 		=  	String(li_periodo)
		istr_mant4.Argumento[14] 		= 	String(li_turno)
		
//		dw_2.Setitem(1, "prod_codigo", Long(istr_mant.argumento[6]))
		dw_2.Object.espe_codigo[1]	=	li_Especie
//		dw_2.Setitem(1, "vari_codigo", Integer(istr_mant.argumento[9]))
//		dw_2.Setitem(1, "vari_nombre", istr_mant.argumento[10])
//		dw_2.SetItem(1, "frio_tipofr", istr_mant.argumento[12])
//		dw_2.SetItem(1, "pefr_codigo", Integer(istr_mant.argumento[13]))
//		dw_2.Setitem(1, "orpr_nrotur", Integer(istr_mant.argumento[14]))
//		dw_2.SetItem(1, "line_codigo", li_Linea)
		dw_2.Object.clie_codigo[1]		=	li_Cliente
//	END IF 
ELSE
	MessageBox("Atención","Número de Orden de Proceso No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean existemovtoproceso (integer ai_planta, integer ai_tipomovto, integer ai_tipodocto, long al_proceso);Long		ll_Numero
Boolean	lb_Retorno = True
Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.Argumento[10])

SELECT	mfge_numero 
	INTO	:ll_Numero
	FROM	dbo.spro_movtofrutagranenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	20
	AND	defg_tipdoc	=	8
	AND	defg_docrel	=	:al_Proceso
	AND   clie_codigo =  :li_Cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Despachos Fruta Granel")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
//	istr_mant.Argumento[3]	=	String(ll_Numero)
//	This.TriggerEvent("ue_recuperadatos")
ELSE
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine f_membreteds (datastore ds_1);ds_1.Modify("raz_social.text = '" + gstr_apl.Razon_Social + "'")
ds_1.Modify("nom_empresa.text = '" + gstr_apl.Nom_Empresa + "'")
ds_1.Modify("rut_empresa.text = 'R.U.T. " + Trim( String( Double( Mid( gstr_apl.Rut_Empresa , 1, 9 ) ), &
				'###,###,###') + '-' + Mid(gstr_apl.Rut_Empresa,10,1) + "'"))
ds_1.Modify("dir_empresa.text = '" + gstr_apl.Dir_Empresa + "'")
ds_1.Modify("tel_empresa.text = 'FONO " + gstr_apl.Tel_Empresa + "   FAX " + gstr_apl.Fax_Empresa + "'")
ds_1.Modify("ciu_empresa.text = '" + Trim(gstr_apl.Ciu_Empresa) + " / " + Trim(gstr_apl.Com_Empresa) + "'")
ds_1.Modify("gir_empresa.text = '" + gstr_apl.Gir_Empresa + "'")
ds_1.Modify("referencia.text = '" + gstr_apl.Referencia + "'")
ds_1.Modify("oficina.text = '" + gstr_apl.Oficina + "'")
ds_1.Modify("replegal.text = '" + gstr_apl.Rep_Legal + "'")
ds_1.Modify("rut_replegal.text = '" + Trim( String( Double( Mid( gstr_apl.Rut_RepLegal , 1, 9 ) ), &
				'###,###,###') + '-' + Mid(gstr_apl.Rut_RepLegal, 10, 1) + "'"))

RETURN
end subroutine

on w_maed_movtofrutagranel_recep_reembalaje.create
int iCurrent
call super::create
this.cb_guia=create cb_guia
this.dw_4=create dw_4
this.ole_puerta1=create ole_puerta1
this.dw_spro_bins=create dw_spro_bins
this.dw_9=create dw_9
this.dw_6=create dw_6
this.ole_puerta=create ole_puerta
this.dw_desverd=create dw_desverd
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_guia
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.ole_puerta1
this.Control[iCurrent+4]=this.dw_spro_bins
this.Control[iCurrent+5]=this.dw_9
this.Control[iCurrent+6]=this.dw_6
this.Control[iCurrent+7]=this.ole_puerta
this.Control[iCurrent+8]=this.dw_desverd
this.Control[iCurrent+9]=this.tab_1
end on

on w_maed_movtofrutagranel_recep_reembalaje.destroy
call super::destroy
destroy(this.cb_guia)
destroy(this.dw_4)
destroy(this.ole_puerta1)
destroy(this.dw_spro_bins)
destroy(this.dw_9)
destroy(this.dw_6)
destroy(this.ole_puerta)
destroy(this.dw_desverd)
destroy(this.tab_1)
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

x												= 	0
y												= 	0
This.Height									= 	2500
im_menu										= 	m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

IF gstr_ParamPlanta.etiquetaembalaje= 	0 THEN
	tab_1.tp_1.dw_lotes.DataObject	=	"dw_mues_spro_lotesfrutagranel_rec_kguia"
END IF

dw_2.Object.oproceso.visible 			= 	FALSE//gb_RecepcionDeProceso
dw_2.Object.defg_docrel.visible 		= 	FALSE//gb_RecepcionDeProceso
dw_2.Object.ordenproceso.visible 	= 	FALSE//gb_RecepcionDeProceso
dw_2.Object.plde_coorde.visible 		= 	TRUE
dw_2.Object.t_packing.visible 		= 	TRUE
This.Title = "RECEPCION DE CAJAS DE PACKING EXTERNO"
istr_mant.argumento[2]	=	'7'

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)


istr_mant.argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[4]	=	'1'
istr_mant.argumento[5]	=	String(gstr_ParamPlanta.CodigoEspecie)
istr_mant.argumento[7]	=	'0'
istr_mant.argumento[9] 	=	'R2'
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
iuo_bins					=	Create uo_bins
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
		IF NOT ib_Destare THEN
			istr_mant.dw	=	dw_3
			istr_mant.dw2	=	dw_6
			OpenWithParm(iw_mantencion_1, istr_mant)
		ELSE
			RETURN
		END IF
	CASE 2
		IF NOT ib_Destare THEN
			lstr_mant.dw	=	dw_5
			SetNull(lstr_mant.dw2)
			lstr_mant.Argumento[4]	=	'1'
			OpenWithParm(iw_mantencion_2, lstr_mant)
		ELSE
			RETURN
		END IF
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

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

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

event ue_guardar;IF dw_3.AcceptText() = -1 THEN RETURN
IF dw_4.AcceptText() = -1 THEN RETURN
IF dw_5.AcceptText() = -1 THEN RETURN

CALL SUPER::ue_guardar

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
		IF ib_Destare THEN
			istr_mant.Solo_Consulta	=	True
		END IF
		IF dw_3.RowCount() > 0 THEN
			istr_mant.dw	=	dw_3
			istr_mant.dw2	=	dw_6
			
			istr_mant.Argumento[7]	=	String(dw_3.Object.lote_codigo[il_fila])
			
			dw_spro_bins.SetFilter("lote_codigo = " + istr_mant.Argumento[7])
			dw_spro_bins.Filter()
			
			
			OpenWithParm(iw_mantencion_1, istr_mant)
			
			dw_spro_bins.SetFilter('')
			dw_spro_bins.Filter()
			asignapeso()
			
		END IF
		istr_mant.Solo_Consulta	=	lb_estado

	CASE 2				
		IF ib_Destare THEN
			lstr_mant.Solo_Consulta	=	True
		END IF
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

event ue_nuevo;Long			ll_modif
Time 			lt_hora
str_pesaje	lstr_pesaje
str_puertacomm  lstr_puertacomm

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
dw_9.Reset()
dw_spro_bins.Reset()

pb_eli_det.Enabled				=	False
pb_ins_det.Enabled				=	False
pb_grabar.Enabled				=	False
pb_eliminar.Enabled				=	False
pb_imprimir.Enabled				=	False
dw_2.Enabled						=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.tpmv_codigo[1]	=	Long(istr_mant.argumento[2])
dw_2.Object.espe_codigo[1]	=	gstr_ParamPlanta.CodigoEspecie
dw_2.Object.clie_codigo[1]	=	gi_CodExport

idt_FechaSistema				=	F_FechaHora()

lt_hora							=	Time(Mid(String(idt_FechaSistema, 'dd/mm/yyyy hh:mm:ss'),12,8))

dw_2.Object.mfge_fecmov[1]	=	Date(String(idt_FechaSistema,'dd/mm/yyyy'))
dw_2.Object.refg_horaen[1]	=	lt_hora

dw_2.Object.destare.Visible=	False
dw_2.Object.destare.Text	=	'Salida'
cb_guia.Enabled				=	False
ib_Salida						=	False
ib_Destare						=	False

istr_Mant.Argumento[3]		=	''
istr_mant.Argumento[5]		=	String(dw_2.Object.espe_codigo[1])
istr_mant.Argumento[7]		=	'0'

//IF iuo_especie.existe(gstr_ParamPlanta.CodigoEspecie,TRUE,SQLCA,Integer(istr_mant.argumento[10])) THEN
IF iuo_especie.existe(gstr_ParamPlanta.CodigoEspecie,TRUE,SQLCA) THEN
	IF iuo_especie.kildec 	= 	1 THEN
		ii_kildec 				= 	2
	ELSE
		ii_kildec 				= 	0
	END IF
END IF	
		
tab_1.tp_1.Enabled			=	False
tab_1.tp_2.Enabled			=	False
tab_1.tp_3.Enabled			=	False
pb_ins_det.Enabled			=	False

tab_1.SelectTab(1)
HabilitaEncab(True)

dw_2.SetColumn("mfge_numero")
dw_2.SetFocus()

//Inicializa Estructura de Pesajes
wstr_pesaje				=	lstr_pesaje
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;Integer	li_tabpage
Boolean	lb_estado, lb_Salir

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
	
	ProductoresLotes(lstr_mant.productores)
	
END IF

CHOOSE CASE li_tabpage
	CASE 1
		IF ib_Destare THEN
			istr_mant.Solo_Consulta	=	True
		END IF

		IF Integer(istr_mant.argumento[7]) = 0 THEN
			IF iuo_Correlativo.Obtiene_Correlativo(Integer(istr_mant.argumento[1]),&
																Integer(istr_mant.argumento[5]),&
																True, SQLCA) THEN
															
				istr_mant.argumento[7]			=	String(iuo_Correlativo.Correl_LoteFG)
				
			ELSE
				lb_Salir	=	True
				
			END IF
		END IF
		
		IF Not lb_Salir THEN
			istr_mant.dw	=	dw_3
			istr_mant.dw2	=	dw_6
			OpenWithParm(iw_mantencion_1, istr_mant)
			istr_mant.Solo_Consulta	=	lb_estado
			AsignaPeso()
			
		END IF

	CASE 2
		IF ib_Destare THEN
			lstr_mant.Solo_Consulta	=	True
		END IF
		
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

IF dw_3.RowCount() > 0 AND dw_5.RowCount() > 0 and NOT ib_Salida THEN HabilitaEncab(False)

IF dw_3.RowCount() > 0 AND dw_5.RowCount() > 0 AND Not pb_eliminar.Enabled AND NOT ib_Salida THEN
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
	pb_eli_det.Enabled	=	True
	
END IF

CHOOSE CASE li_tabpage 
	CASE 1
		IF Not lb_Salir THEN
			istr_mant	=	Message.PowerObjectParm
			
			dw_3.SetRow(il_Fila)
			dw_3.SelectRow(il_Fila, True)
		END IF
		
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

event ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, ll_fila_env
Boolean lb_cargo = TRUE
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Long(istr_mant.argumento[1]),&
										 Long(istr_mant.argumento[2]),&
										 Long(istr_mant.argumento[3]),&
										 Integer(istr_mant.argumento[10]))

	IF ll_fila_e = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila_e > 0 THEN
		
		is_rut = dw_2.Object.mfge_rutcho[1]
		
		tab_1.tp_1.Enabled		=	True
		tab_1.tp_2.Enabled		=	True
		
		IF dw_2.Object.mfge_estmov[1]	=	3 THEN
			tab_1.tp_3.Enabled		=	True
			istr_mant.Solo_Consulta	=	True
			dw_2.Enabled				=	False
			
		ELSE
			dw_2.Object.destare.Visible	=	True
			//istr_mant.Solo_Consulta			=	False
		END IF
		
		istr_mant.Argumento[5]	=	String(dw_2.Object.espe_codigo[1])
		
		iuo_Camion.Existe(1, dw_2.Object.cami_patent[1], True, sqlca)
		
		//HabilitaEncab(False)
		
		ll_fila_env	=	dw_4.Retrieve(Long(istr_mant.argumento[1]),&
											  Long(istr_mant.argumento[2]),&
											  Long(istr_mant.argumento[3]),1,&
											  Integer(istr_mant.argumento[10]))

		DO
			dw_1.GetChild("lote_espcod", idwc_especiedet1)
			idwc_especiedet1.SetTransObject(sqlca)
			IF idwc_especiedet1.Retrieve() = 0 THEN
				MessageBox("Atención","Falta Registrar Especies")
				idwc_especiedet1.InsertRow(0)
			ELSE
				idwc_especiedet1.SetSort("espe_nombre A")
				idwc_especiedet1.Sort()
			END IF
			
			IF dw_1.Retrieve(Long(istr_mant.argumento[1]),&
							     Long(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),&
								  Integer(istr_mant.argumento[10])) = -1 OR &
				dw_3.Retrieve(Long(istr_mant.argumento[1]),&
							     Long(istr_mant.argumento[2]),&
								  Long(istr_mant.argumento[3]),&
								  Integer(istr_mant.argumento[10])) = -1 OR &
				dw_6.Retrieve(Long(istr_mant.argumento[1]),&
							     Long(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),&
								  Integer(istr_mant.argumento[10])) = -1 OR &
				dw_5.Retrieve(Long(istr_mant.argumento[1]),&
							     Long(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),1,&
								  Integer(istr_mant.argumento[10])) = -1 OR &
				dw_7.Retrieve(Long(istr_mant.argumento[1]),&
							     Long(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),2,&
								  Integer(istr_mant.argumento[10])) = -1  OR &
				dw_spro_bins.Retrieve(Long(istr_mant.argumento[1]),&
							    	Long(istr_mant.argumento[3]),&
								  Integer(istr_mant.argumento[10])) = -1 OR &
				dw_9.Retrieve(Long(istr_mant.argumento[1]),&
							     Long(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3])) = -1 OR &
				dw_desverd.Retrieve(Integer(istr_mant.argumento[10]), &
								  Long(istr_mant.argumento[1]),&
							     Long(istr_mant.argumento[2]),&
								  Long(istr_mant.argumento[3])) = -1 THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				idwc_variedad.Retrieve(dw_2.Object.espe_codigo[1])
				
				id_KilosEnv	=	dw_2.Object.refg_tkenen[1]		
				
				//Para determinar Fuera de Rango
				IF dw_2.Object.mfge_estmov[1] = 3 THEN Destare(False)
				
				pb_eliminar.Enabled  = NOT istr_mant.Solo_Consulta
				pb_ins_det.Enabled	= NOT istr_mant.Solo_Consulta
				pb_imprimir.Enabled	= True
				pb_eli_det.Enabled	= NOT istr_mant.Solo_Consulta
				dw_3.SetRow(1)
				dw_3.SelectRow(1, True)
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
Boolean     	lb_Actualiza_Fruta = FALSE, lb_Actualiza_Envase = FALSE
Integer 	 	li_TipoMovto, li_TipoMovtoEnva, li_Planta, li_pesaje, &
				li_Lote_pltcod, li_Lote_espcod, li_Lote_codigo, li_Secuencia, &
				li_TipoEnvase, li_Envase, li_Lote_Ant, li_camara, li_Cliente
Long 			ll_Filas, ll_Total_Bultos, ll_Envases_Fruta, ll_Fila, ll_Fila_Busca, &
  				ll_Fila_d, ll_Primer_NumEnva, ll_Productor, ll_Numero
Decimal{3}	ld_Total_PesoEnv_Sal, ld_KBS_Camion, ld_KBS_Carro

Message.DoubleParm = 0

li_Planta				=	Integer(istr_mant.Argumento[1])
li_TipoMovto			=	Integer(istr_mant.Argumento[2])
li_TipoMovtoenva  	=  43  // Recepción de Envases
li_Cliente				=	Integer(istr_mant.Argumento[10])
 
ll_Filas = dw_1.RowCount()

IF dw_3.RowCount() > 0 THEN
	IF dw_3.Object.total_bultos[dw_3.RowCount()] >= 10000 THEN
		Messagebox("Error de Consistencia","El total de bultos supera lo permitido")
		Message.DoubleParm = -1
		RETURN
	ELSE
		ll_Total_Bultos	=	dw_3.Object.total_bultos[dw_3.RowCount()]
	END IF
END IF

IF dw_5.RowCount() > 0 THEN
	IF dw_5.Object.total_pesoenv[dw_5.RowCount()] >= 100000 THEN
		Messagebox("Error de Consistencia","El peso de los envases supera lo permitido")
		Message.DoubleParm = -1
		RETURN
	END IF
	
	IF dw_5.Object.total_envases[dw_5.RowCount()] >= 10000 THEN
		Messagebox("Error de Consistencia","El total de envases supera lo permitido")
		Message.DoubleParm = -1
		RETURN
	END IF
END IF

IF dw_5.RowCount() > 0 THEN
	ll_Envases_Fruta	=	dw_5.Object.tot_bultos_fruta[dw_5.RowCount()]
END IF

IF Chequea_EnvasesProductor() = False THEN
	Message.DoubleParm = -1
	RETURN
END IF

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

IF dw_9.RowCount() <> dw_2.Object.mfge_totbul[1] AND (FALSE OR  FALSE) THEN
	MessageBox("Atención","La cantidad de pesajes no corresponde a la cantidad de Bultos Recepcionados.")
	Message.DoubleParm = -1
	RETURN
END IF

ib_AutoCommit		=	SQLCA.AutoCommit
SQLCA.AutoCommit	=	False

IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
  
  	IF il_NumEnva=0 THEN
		iuo_TipoMovtoEnva.bloqueacorrel()	
		il_NumEnva = iuo_TipoMovtoEnva.UltimoCorrelativo(4,li_TipoMovtoEnva,li_Planta) 
	
		IF il_NumEnva = 0 OR IsNull(il_NumEnva) THEN
			Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
			Message.DoubleParm = -1
			RETURN
		ELSE
			lb_Actualiza_Envase = TRUE
			ll_Primer_NumEnva   = il_NumEnva
		END IF
	ELSE
		ll_Primer_NumEnva   = il_NumEnva
	END IF
	
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
//	dw_2.Object.plde_coorde[1]	= 	dw_2.Object.lote_pakori[1]
	
	Determina_ProductoresEnvase(li_TipoMovtoEnva)
	
	//Resetea Movto de Envase en caso de errores previos en la grabación.
	dw_4.Reset()
	
	FOR ll_Productor = 1 TO UpperBound(wstr_Prod_Enva.Productor)
		ll_Fila	=	dw_4.InsertRow(0)
		
		dw_4.Object.plde_codigo[ll_Fila]		=		li_Planta
		dw_4.Object.tpmv_codigo[ll_Fila]		=		li_TipoMovtoEnva
		dw_4.Object.meen_numero[ll_Fila]		=		il_NumEnva
		dw_4.Object.tpmv_codrec[ll_Fila]		=		dw_2.Object.tpmv_codigo[1]
		dw_4.Object.mfge_numero[ll_Fila]		=		il_NumFruta
		dw_4.Object.meen_modulo[ll_Fila]		=		1
		dw_4.Object.plde_coorde[ll_Fila]		=		dw_2.Object.plde_coorde[1]
		dw_4.Object.prod_codigo[ll_Fila]		=		Long(wstr_Prod_Enva.Productor[ll_Productor])
     	dw_4.Object.clie_codigo[ll_Fila]		=		li_Cliente
		dw_4.Object.meen_guisii[ll_Fila]		=		wstr_Prod_Enva.GuiaSII[ll_Productor]
		dw_4.Object.meen_fecmov[ll_Fila]		=		dw_2.Object.mfge_fecmov[1]
		dw_4.Object.tran_codigo[ll_Fila]		=		dw_2.Object.tran_codigo[1]
		dw_4.Object.cami_clasifi[ll_Fila]	=		dw_2.Object.cami_clasifi[1]
		dw_4.Object.cami_patent[ll_Fila]		=		dw_2.Object.cami_patent[1]
		dw_4.Object.cami_patcar[ll_Fila]		=		dw_2.Object.cami_patcar[1]
		dw_4.Object.meen_rutcho[ll_Fila]		=		dw_2.Object.mfge_rutcho[1]
		dw_4.Object.meen_chofer[ll_Fila]		=		dw_2.Object.mfge_chofer[1]
		il_NumEnva++
	NEXT
	//Descuenta último Correlativo Envases acumulado.
	il_NumEnva --
	//Preguntar el Momento de Actualización
	IF lb_Actualiza_Fruta  THEN iuo_TipoMovtoFruta.Actualiza_Correlativo(1,li_Planta,li_TipoMovto,il_NumFruta) 
	IF lb_Actualiza_Envase THEN iuo_TipoMovtoEnva.Actualiza_Correlativo(4,li_Planta,li_TipoMovtoEnva,il_NumEnva) 
	///////////////////////////////////////
	il_NumEnva = ll_Primer_NumEnva	
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
	
	li_Lote_pltcod											=		dw_6.Object.lote_pltcod[ll_Fila]
	li_Lote_espcod											=		dw_6.Object.lote_espcod[ll_Fila]
	li_Lote_codigo											=		dw_6.Object.lote_codigo[ll_Fila]
	li_TipoEnvase											=		dw_6.Object.enva_tipoen[ll_Fila]
	li_Envase													=		dw_6.Object.enva_codigo[ll_Fila]

	IF li_Lote_Codigo <> li_Lote_Ant THEN
		ll_Fila_Busca	=	dw_3.Find("lote_pltcod 	= "	+	String(li_Lote_pltcod)+" and "+&
											 "lote_espcod 	= "	+	String(li_Lote_espcod)+" and "+&
											 "lote_codigo 	= "	+	String(li_Lote_codigo),1,dw_3.RowCount())
		IF ll_Fila_Busca > 0 THEN
			dw_3.Object.fgcc_fecrec[ll_Fila_Busca]	=		dw_2.Object.mfge_fecmov[1]
			li_Camara										=		dw_3.Object.cama_codigo[ll_Fila_Busca]
		END IF
		li_Lote_Ant											=		li_Lote_codigo
	END IF
	
	ll_Fila_d	=	dw_1.Find("lote_pltcod 		= "	+	String(li_Lote_pltcod)+" and "+&
									 "clie_codigo 	= "	+	String(li_cliente)+" and "+&
									 "lote_espcod 	= "	+	String(li_Lote_espcod)+" and "+&
									 "lote_codigo 	= "	+	String(li_Lote_codigo), &
									 1,dw_1.RowCount())
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
	dw_1.Object.lote_pltcod[ll_Fila_d]		=	li_Lote_pltcod
	dw_1.Object.lote_espcod[ll_Fila_d]	=	li_Lote_espcod
	dw_1.Object.lote_codigo[ll_Fila_d]	=	li_Lote_codigo
	dw_1.Object.enva_tipoen[ll_Fila_d]	=	li_TipoEnvase
	dw_1.Object.enva_codigo[ll_Fila_d]	=	li_Envase
	dw_1.Object.mfgd_bulent[ll_Fila_d]	=	dw_6.Object.lotd_totbul[ll_Fila]
	dw_1.Object.mfgd_kgnent[ll_Fila_d]	=	dw_6.Object.lotd_totnet[ll_Fila]
	dw_1.Object.clie_codigo[ll_Fila_d]		=	li_Cliente	
NEXT

//Elimina Filas de Detalle de Movimiento que fueron eliminadas de Lotes
ll_Fila	= 1

DO WHILE ll_Fila <= dw_1.RowCount()
	
	li_Lote_pltcod	=	dw_1.Object.lote_pltcod[ll_Fila]
	li_Lote_espcod	=	dw_1.Object.lote_espcod[ll_Fila]
	li_Lote_codigo	=	dw_1.Object.lote_codigo[ll_Fila]
	li_TipoEnvase	=	dw_1.Object.enva_tipoen[ll_Fila]
	li_Envase		=	dw_1.Object.enva_codigo[ll_Fila]

	ll_Fila_d	=	dw_6.Find("lote_pltcod 		= "	+	String(li_Lote_pltcod)+" and "+&
	                         			"lote_espcod 	= "	+	String(li_Lote_espcod)+" and "+&
									"lote_codigo 	= "	+	String(li_Lote_codigo)+" and "+&
									"enva_tipoen 	= "	+	String(li_TipoEnvase)+" and "+&
									"enva_codigo 	= "	+	String(li_Envase),1,dw_6.RowCount())

	IF ll_Fila_d = 0 THEN
		dw_1.DeleteRow(ll_Fila)
	ELSE
		ll_Fila ++
	END IF
LOOP

FOR ll_Fila = 1 TO dw_5.RowCount()
	
	IF dw_5.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		ll_Productor	=	dw_5.Object.prod_codigo[ll_Fila]
		
		ll_Fila_Busca	=	dw_4.Find("plde_codigo	= "	+	String(li_Planta) + " and " + &
											 "tpmv_codigo 	= "	+	String(li_TipoMovtoEnva) + " and " + &
											 "clie_codigo 	= "	+	String(li_Cliente) + " and " + &											 
											 "prod_codigo 	= "	+	String(ll_Productor),1,dw_4.RowCount())
		
		IF ll_Fila_Busca > 0 THEN
												 
			dw_5.Object.plde_codigo[ll_Fila]		=	dw_4.Object.plde_codigo[ll_Fila_Busca]
			dw_5.Object.tpmv_codigo[ll_Fila]		=	dw_4.Object.tpmv_codigo[ll_Fila_Busca]
			dw_5.Object.meen_numero[ll_Fila]	=	dw_4.Object.meen_numero[ll_Fila_Busca]
			dw_5.Object.clie_codigo[ll_Fila]		=	li_Cliente
		END IF
	END IF
NEXT

FOR ll_Fila = 1 TO dw_7.RowCount()
	
	IF dw_7.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		ll_Productor	=	dw_7.Object.prod_codigo[ll_Fila]
		
		ll_Fila_Busca	=	dw_4.Find("plde_codigo 	= "			+	String(li_Planta)+" and " + &
											 "tpmv_codigo 	= 41 and " 	+ 	&
											 "clie_codigo 	= "			+	String(li_Cliente) + " and " + &											 
											 "prod_codigo 	= "			+	String(ll_Productor),1,dw_4.RowCount())
		
		IF ll_Fila_Busca > 0 THEN
												 
			dw_7.Object.plde_codigo[ll_Fila]		=	dw_4.Object.plde_codigo[ll_Fila_Busca]
			dw_7.Object.tpmv_codigo[ll_Fila]		=	dw_4.Object.tpmv_codigo[ll_Fila_Busca]
			dw_7.Object.meen_numero[ll_Fila]		=	dw_4.Object.meen_numero[ll_Fila_Busca]
			dw_7.Object.clie_codigo[ll_Fila]		=	li_Cliente			
		END IF
	END IF
NEXT
/*Selecciona secuencia para tabla de pesajes*/

ll_numero							=	dw_2.Object.mfge_numero[1]

//dw_2.Object.plde_coorde[1]		=	dw_2.Object.lote_pakori[1]

li_Pesaje								=	0

dw_9.SetSort("mfgp_nropes asc")
dw_9.Sort()
 
FOR ll_Fila = 1 TO dw_9.RowCount()
//	IF dw_9.Object.mfgp_nropes[ll_Fila] <> li_Pesaje THEN
//		li_Pesaje	=	dw_9.Object.mfgp_nropes[ll_Fila]
//		
//		SELECT	IsNull(Max(mfgp_secuen),0) + 1
//		  INTO	:li_Secuencia
//		  FROM	dba.spro_movtofrutagranpesa
//		 WHERE	plde_codigo	=	:li_Planta
//			AND	tpmv_codigo =	:li_TipoMovto
//			AND	mfge_numero =	:ll_Numero
//			AND	mfgp_nropes	=	:li_Pesaje ;
//	END IF
	
	IF dw_9.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_9.Object.clie_codigo[ll_fila] 	= 	dw_2.Object.clie_codigo[1]
		dw_9.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_9.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
		dw_9.Object.mfge_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]		
//		dw_9.Object.mfgp_secuen[ll_Fila]	=	li_secuencia		
//		li_secuencia ++
	END IF
NEXT

FOR ll_Fila = 1 TO dw_spro_bins.RowCount()
//	IF dw_spro_bins.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_spro_bins.Object.mfge_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]		
//	END IF
NEXT

FOR ll_fila = 1 TO dw_desverd.RowCount()
	dw_desverd.Object.lode_estado[ll_fila]	=	1
	
NEXT

//SETEA CLIENTE = CLIE_CODIGO SELECCIONADO
//Repara falla para proceso 3ros.
FOR ll_fila = 1 TO dw_1.Rowcount()
	dw_1.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

FOR ll_fila = 1 TO dw_3.Rowcount()
	dw_3.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
//	dw_3.Object.lote_codpak[ll_fila] = dw_2.Object.lote_pakori[1]
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

IF gb_RecepcionDeProceso THEN
	dw_2.Object.defg_tipdoc[1]		=	8
END IF

//AsignaPeso()
end event

event ue_seleccion;call super::ue_seleccion;IF ib_ok = False THEN RETURN

Str_Busqueda	lstr_busq

lstr_busq.argum[1] = String(gstr_ParamPlanta.CodigoPlanta)
lstr_busq.argum[2] = '7'								// Movimiento de Recepción
lstr_busq.argum[3] = ''								// Cualquier estado
lstr_busq.argum[4] = String(idt_FechaSistema)   // Desde Fecha de Inicio Ducha
lstr_busq.argum[5] = istr_mant.argumento[5]     // Especie
lstr_busq.argum[10] = istr_mant.argumento[10]   // Codigo Cliente

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
Integer li_estado, li_Kilos

istr_info.titulo	= "GUIA DE RECEPCION FRUTA GRANEL"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

li_estado					=	dw_2.object.mfge_estmov[1]
ld_FechaRecepcion	=	dw_2.object.mfge_fecmov[1]
IF li_estado=1 THEN
   vinf.dw_1.DataObject = "dw_info_guia_recepcion_Transitoria"
ELSE
	vinf.dw_1.DataObject = "dw_info_guia_recepcion_Definitiva"
	li_Kilos	=	MessageBox("Emisión Definitiva","Desea emitir Guía de Recepción con Kilos",Question!,YesNo!,1)
	IF li_Kilos = 2 THEN li_Kilos = 0
END IF

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Long(istr_mant.Argumento[1]), &
								  Long(istr_mant.Argumento[2]),&
								  Long(istr_mant.Argumento[3]),Integer(istr_mant.argumento[10]),&
								  ld_FechaRecepcion, ld_FechaRecepcion)
								  
								  /*,li_Kilos*/

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

event ue_validaborrar();IF dw_2.Object.mfge_estmov[1] > 1 THEN
	MessageBox("Atención","No puede ser eliminado un movimiento en estado definitivo")
	Message.DoubleParm = -1
	RETURN
ELSE
	IF MessageBox("Borrar Registro","Desea Borrar la Información ?", Question!, YesNo!) = 1 THEN
		Message.DoubleParm = 1
	ELSE
		Message.DoubleParm = -1
	END IF
END IF

RETURN 

end event

event mousemove;call super::mousemove;//
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

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutagranel_recep_reembalaje
boolean visible = false
integer x = 50
integer y = 1300
integer width = 4018
integer height = 1256
boolean titlebar = false
string title = "Detalle de Movimientos"
string dataobject = "dw_mues_movtofrutagraneldeta_despacho"
boolean hscrollbar = false
boolean vscrollbar = false
boolean livescroll = false
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutagranel_recep_reembalaje
integer x = 41
integer y = 12
integer width = 3287
integer height = 832
string dataobject = "dw_mant_movtofrutagranenca"
boolean livescroll = true
boolean righttoleft = true
end type

event dw_2::itemchanged;String	ls_Nula
String	ls_Columna
Date		ld_Fecha
Time		lt_Hora
uo_plantadesp			luo_plantadesp
 
ls_Columna = dwo.Name
SetNull(ls_Nula)
 
CHOOSE CASE ls_Columna
		
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(row,"clie_codigo",Integer(ls_Nula))
			RETURN 1
		ELSE
			dw_2.GetChild("espe_codigo", idwc_especie)
			idwc_especie.SetTransObject(sqlca)
			idwc_especie.Retrieve(integer(data))
			istr_mant.Argumento[10] = data
		END IF	
	
	CASE "mfge_numero"
		IF NOT ExisteRecepcion(gstr_ParamPlanta.CodigoPlanta, Integer(istr_mant.Argumento[2]), Integer(data),Integer(istr_mant.argumento[10])) THEN
			This.SetItem(row,"mfge_numero",Long(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF

	CASE "mfge_fecmov"
		ld_Fecha	=	Date(Mid(String(date(data),'dd/mm/yyyy'),1,10))
		
		IF NOT iuo_FechaMovto.Valida_FechaMovto(ld_Fecha) THEN
			This.SetItem(row,"mfge_fecmov",Date(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF	

	CASE "espe_codigo"
		IF Not manbin_especie(This.Object.plde_codigo[row], Integer(Data), True, sqlca) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		ELSEIF Not iuo_Especie.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		ELSEIF NOT iuo_Especie.ExisteCorrelativoLote(gstr_ParamPlanta.CodigoPlanta,Integer(Data),True,SQLCA) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		ELSE
			istr_mant.argumento[5]	=	data
			istr_mant.argumento[7]	=	'0'
			IF iuo_especie.kildec = 1 THEN
				ii_kildec = 2
			ELSE
				ii_kildec = 0
			END If
			
			idwc_Variedad.SetTransObject(SQLCA)
			idwc_Variedad.Retrieve(Integer(data))
		END IF

	CASE "tran_codigo"
		IF Not iuo_Transport.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		END IF

	CASE "cami_patent"
		IF data <> "" AND Not iuo_Camion.Existe(1, Data, True, sqlca) THEN
			This.SetItem(row, ls_Columna, ls_Nula)
			
			RETURN 1
		ELSE
			This.Object.cami_patcar[row]	=	iuo_Camion.PateCarro
			This.Object.mfge_rutcho[row]	=	iuo_Camion.RutChofer
			is_rut = iuo_Camion.RutChofer
			This.Object.mfge_chofer[row]	=	iuo_Camion.Chofer
		END IF

	CASE "mfge_rutcho"
		
		is_rut = F_verrut(data, True)
		IF is_rut = "" THEN
			dw_2.SetItem(row, "mfge_rutcho", ls_Nula)
			dw_2.SetItem(row, "mfge_chofer", ls_Nula)
			RETURN 1
		ELSE
			IF NoExisteChofer(Data) THEN
				dw_2.SetItem(row, "mfge_rutcho", ls_Nula)
				dw_2.SetItem(row, "mfge_chofer", ls_Nula)
				RETURN 1
			ELSE	
				dw_2.SetItem(row, "mfge_chofer",is_chofer)
			END IF
		END IF	
		
	CASE "refg_tkbent", "refg_tkbenc", "refg_tkbsal", "refg_tkbsac"
		IF Not This.uf_validate(row) THEN
			This.SetItem(row,ls_Columna,Dec(ls_Nula))
			RETURN 1
		ELSE
			captura_Totales()
		END IF
		
	CASE "mfge_totbul"
		IF Integer(data) > 9999 OR Integer(data) < 0 THEN
			This.SetItem(row,"mfge_totbul",Integer(ls_Nula))
			RETURN 1
		END IF

	CASE "fecha_sal"
		ld_Fecha	=	Date(String(Mid(data,1,10),'dd/mm/yyyy'))
		
		IF NOT iuo_FechaMovto.Valida_FechaMovto(date(ld_Fecha)) OR &
			ld_Fecha < This.Object.mfge_fecmov[row] THEN
			MessageBox("Atención","Fecha de Salida no puede ser anterior a Fecha de Entrada")
			This.SetItem(row,"fecha_sal",SetNull(ld_Fecha))
			RETURN 1
		END IF
		
		
	CASE "refg_horasa"
		lt_Hora	=	Time(This.Object.fecha_sal[Row])
	
		IF Time(data) < This.Object.refg_horaen[row] THEN
			MessageBox("Atención","Hora de Salida no puede ser anterior a Hora de Entrada")
			This.SetItem(row,"refg_horasa",SetNull(lt_Hora))
			RETURN 1
		END IF
		
	CASE "defg_docrel"
		IF Data <> "" and Data <> "0" THEN
			IF Not Existedocproceso(gstr_ParamPlanta.CodigoPlanta,Integer(Data)) THEN
				dw_2.SetItem(1, 'defg_docrel', integer(ls_Nula))
				RETURN 1
			ELSE
				dw_2.Object.defg_docrel[row]		=	Integer(Data)
			END IF
			dw_2.AcceptText()
		END IF
	
	CASE "plde_coorde"
		luo_plantadesp	=	Create uo_plantadesp
		
		IF NOT luo_PlantaDesp.Existe(Integer(data),True,Sqlca) THEN
			This.SetItem(1, "plde_coorde", Integer(ls_Nula) )
			This.SetFocus()
			RETURN 1
		ElSEIF luo_PlantaDesp.TipoPlanta <> 2 THEN
			This.SetItem(1, "plde_coorde", Integer(ls_Nula) )
			MessageBox("Error", "El Codigo ingresado no corresponde a un Packing. ~nIngrese o Seleccione otro codigo", None!)
			This.SetFocus()
			RETURN 1
		END IF
			
END CHOOSE

IF ls_Columna <> 'mfge_numero' THEN
	HabilitaIngreso(ls_Columna)
	
	IF ib_Salida THEN
		HabilitaGrabacion(ls_Columna)
	END IF
END IF
end event

event dw_2::buttonclicked;call super::buttonclicked;Long ll_Fila
String ls_nula
Str_busqueda		lstr_busq

SetNull(ls_nula)

CHOOSE CASE dwo.Name
	CASE "buscacamion"
		buscacamion()
		iuo_Camion.Existe(1, This.Object.cami_patent[row], True, sqlca)

	CASE "destare"
		IF Not ib_Salida THEN
			HabilitaSalida()
		ELSE
			Destare(True)
			ib_graba_destare = true
		END IF
		
	CASE "camion"
		IF ib_Salida THEN
			IF Isnull(iuo_Camion.TaraCamion) THEN
				This.Object.refg_tkbsal[row]	=	0
			ELSE
				This.Object.refg_tkbsal[row]	=	iuo_Camion.TaraCamion
				captura_totales()
			END IF
		END IF
		
	CASE "carro"
		IF ib_Salida THEN
			IF Isnull(iuo_Camion.TaraCarro) THEN
				This.Object.refg_tkbsac[row]	=	0
			ELSE
				This.Object.refg_tkbsac[row]	=	iuo_Camion.TaraCarro
				captura_totales()
			END IF
		END IF
		
//	CASE "romanacamion"
//		
//		IF NOT ib_salida THEN
//			IF gstr_ParamPlanta.PesajeBins = 1 THEN
//				wstr_pesaje.puerta	=	istr_puertacomm
//				OpenWithParm(w_pesaje_romana,wstr_pesaje)
//				
//				wstr_pesaje	=	Message.PowerObjectParm
//				
//				This.Object.refg_tkbent[1]	=	wstr_pesaje.total
//				captura_Totales()
//			END IF
//		END IF
//		
//	CASE "romanacarro"
//		IF NOT ib_salida THEN
//			IF gstr_ParamPlanta.PesajeBins = 1 THEN
//				wstr_pesaje.puerta	=	istr_puertacomm
//				OpenWithParm(w_pesaje_romana,wstr_pesaje)
//				
//				wstr_pesaje	=	Message.PowerObjectParm
//				
//				This.Object.refg_tkbent[1]	=	wstr_pesaje.total
//				captura_Totales()
//			END IF
//		END IF

	CASE "romanacamion"
		IF NOT ib_salida THEN
			wstr_pesaje.puerta	=	istr_puertacomm
			wstr_pesaje.dw			=	dw_9
			
			wstr_pesaje.argum[1]	=	istr_mant.argumento[1]
			wstr_pesaje.argum[2]	=	istr_mant.argumento[2]
			wstr_pesaje.argum[3]	=	istr_mant.argumento[3]
			wstr_pesaje.argum[7]	=	"-1"
			wstr_pesaje.argum[10]	=	istr_mant.argumento[10]
			
			OpenWithParm(w_pesaje_romana,wstr_pesaje)
			
			wstr_pesaje	=	Message.PowerObjectParm
			
			This.Object.refg_tkbent[1]	=	0
			
			captura_Totales()
		ELSE
			wstr_pesaje.puerta	=	istr_puertacomm
			wstr_pesaje.dw			=	dw_9
			wstr_pesaje.puerta.pesajebins	=	0
			
			OpenWithParm(w_pesaje_romana,wstr_pesaje)
			
			wstr_pesaje	=	Message.PowerObjectParm
			
			This.Object.refg_tkbsal[1]	=	0
			
			captura_Totales()
			
		END IF
		AsignaPeso()

	CASE "romanacarro"
		IF NOT ib_salida THEN
			wstr_pesajecarro.puerta	=	istr_puertacomm
			wstr_pesajecarro.dw		=	dw_9
			OpenWithParm(w_pesaje_romana,wstr_pesajecarro)
			
			wstr_pesajecarro	=	Message.PowerObjectParm
			
			This.Object.refg_tkbenc[1]	=	0
			
			IF dw_9.RowCount() > 0 THEN
				FOR ll_Fila = 1 TO dw_9.RowCount()
					IF dw_9.Object.mfgp_estado[ll_Fila] = 2 THEN
						This.Object.refg_tkbenc[1]	=+	dw_9.Object.mfgp_pesore[ll_fila]
					END IF
				NEXT
			END IF
			
			captura_Totales()
		ELSE
			wstr_pesajecarro.puerta	=	istr_puertacomm
			wstr_pesajecarro.dw		=	dw_9
			wstr_pesajecarro.puerta.pesajebins	=	0
			
			OpenWithParm(w_pesaje_romana,wstr_pesajecarro)
			
			wstr_pesajecarro	=	Message.PowerObjectParm
			
			This.Object.refg_tkbsac[1]	=	0
			
			IF dw_9.RowCount() > 0 THEN
				FOR ll_Fila = 1 TO dw_9.RowCount()
					IF dw_9.Object.mfgp_estado[ll_Fila] = 2 THEN
						This.Object.refg_tkbsac[1]	=+	dw_9.Object.mfgp_pesore[ll_fila]
					END IF
				NEXT
			END IF
			
			captura_Totales()
			
		END IF
		
	CASE "ordenproceso"
		lstr_busq.argum[1]	=	istr_mant.argumento[1]
		lstr_busq.argum[15]	=	"8"
		lstr_busq.argum[16]	=	istr_mant.Argumento[10]
		
		OpenWithParm(w_busc_spro_ordenproceso_traspaso, lstr_busq)
		lstr_busq	=	Message.PowerObjectParm

		IF lstr_busq.argum[1] <> "" and lstr_busq.argum[1]<> "0" THEN
			IF Not Existedocproceso(gstr_ParamPlanta.CodigoPlanta,Integer(lstr_busq.argum[1])) THEN
				dw_2.SetItem(1, 'defg_docrel', integer(ls_Nula))
				RETURN 1
			ELSE
				dw_2.Object.defg_docrel[row]		=	Integer(lstr_busq.argum[1])
			END IF
			dw_2.AcceptText()
		END IF
		
END CHOOSE
end event

event dw_2::itemfocuschanged;call super::itemfocuschanged;IF is_rut <> "" THEN
	This.Object.mfge_rutcho.Format = '@@@.@@@.@@@-@'
	
//	IF dwo.Name <> "mfge_rutcho" THEN
//		This.SetItem(1, "mfge_rutcho", is_rut)
//	END IF
END IF
end event

event dw_2::constructor;call super::constructor;This.uf_add_validation('refg_tkbsal >= 0 and refg_tkbsal <= 99999.999','Valor fuera de rango')
This.uf_add_validation('refg_tkbsac >= 0 and refg_tkbsac <= 99999.999','Valor fuera de rango')
This.uf_add_validation('refg_tkbent >= 0 and refg_tkbent <= 99999.999','Valor fuera de rango')
This.uf_add_validation('refg_tkbenc >= 0 and refg_tkbenc <= 99999.999','Valor fuera de rango')

end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutagranel_recep_reembalaje
integer x = 4133
integer y = 292
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutagranel_recep_reembalaje
integer x = 4133
integer y = 544
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutagranel_recep_reembalaje
integer x = 4137
integer y = 584
boolean enabled = true
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutagranel_recep_reembalaje
integer x = 4133
integer y = 1048
end type

event pb_imprimir::clicked;call super::clicked;//IF dw_2.object.mfge_estmov[dw_2.getrow()] = 3 then
//	Parent.TriggerEvent("ue_imprimir_tarja")
//END IF


end event

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutagranel_recep_reembalaje
integer x = 4133
integer y = 1300
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutagranel_recep_reembalaje
integer x = 4137
integer y = 1416
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutagranel_recep_reembalaje
integer x = 4142
integer y = 1588
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutagranel_recep_reembalaje
integer x = 4133
integer y = 40
end type

type cb_guia from commandbutton within w_maed_movtofrutagranel_recep_reembalaje
integer x = 4133
integer y = 1156
integer width = 302
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

event clicked;Long		ll_Fila, ll_Fila_Busca,ll_Productor

str_mant	lstr_mant

Determina_ProductoresEnvase(61)

FOR ll_Fila = 1 TO UpperBound(wstr_Prod_Enva.Productor)
	ll_Productor	=	wstr_Prod_Enva.Productor[ll_Fila]
	
	ll_Fila_Busca	=	dw_4.Find("clie_codigo = "+istr_Mant.Argumento[10]+" and "+&
	                            "plde_codigo = "+istr_Mant.Argumento[1]+" and "+&
										 "tpmv_codigo = 41 and "+&
										 "prod_codigo = "+String(ll_Productor),1,dw_4.RowCount())
		
	IF ll_Fila_Busca > 0 THEN
												 
		lstr_Mant.Argumento[1]	=	String(dw_4.Object.plde_codigo[ll_Fila_Busca])
		lstr_Mant.Argumento[2]	=	String(dw_4.Object.tpmv_codigo[ll_Fila_Busca])
		lstr_Mant.Argumento[3]	=	String(dw_4.Object.meen_numero[ll_Fila_Busca])
		lstr_mant.Argumento[5]  =  String(dw_4.Object.clie_codigo[ll_Fila_Busca])
		
		OpenWithParm(w_emis_guia_despacho_envases, lstr_Mant)
	END IF
NEXT


end event

type dw_4 from datawindow within w_maed_movtofrutagranel_recep_reembalaje
boolean visible = false
integer x = 3831
integer y = 296
integer width = 174
integer height = 152
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_movtoenvaenca_recepfruta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type ole_puerta1 from olecustomcontrol within w_maed_movtofrutagranel_recep_reembalaje
event iostatusevent ( long statustype,  long iostatus )
event iocompleteevent ( long jobtype,  long jobid,  long jobresult )
event ioqueueevent ( long numcharsinputque,  long numcharsoutputque )
event ioperiodicevent ( )
boolean visible = false
integer x = 3817
integer y = 76
integer width = 224
integer height = 192
integer taborder = 100
boolean bringtotop = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "w_maed_movtofrutagranel_recep_reembalaje.win"
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type

event externalexception;MessageBox(String(action),description)

MessageBox(String(exceptioncode),String(resultcode))
end event

type dw_spro_bins from datawindow within w_maed_movtofrutagranel_recep_reembalaje
boolean visible = false
integer x = 3808
integer y = 688
integer width = 174
integer height = 152
integer taborder = 60
boolean bringtotop = true
string title = "Detalle cajas"
string dataobject = "dw_spro_movtobins"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

type dw_9 from datawindow within w_maed_movtofrutagranel_recep_reembalaje
boolean visible = false
integer x = 3758
integer y = 480
integer width = 174
integer height = 152
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_pesaje_romana"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

type dw_6 from datawindow within w_maed_movtofrutagranel_recep_reembalaje
boolean visible = false
integer x = 3538
integer y = 96
integer width = 174
integer height = 152
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_lotesfrutagradet_recepcion"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

type ole_puerta from olecustomcontrol within w_maed_movtofrutagranel_recep_reembalaje
event oncomm ( )
boolean visible = false
integer x = 3552
integer y = 500
integer width = 174
integer height = 152
integer taborder = 100
boolean bringtotop = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "w_maed_movtofrutagranel_recep_reembalaje.win"
integer binaryindex = 1
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type

type dw_desverd from datawindow within w_maed_movtofrutagranel_recep_reembalaje
boolean visible = false
integer x = 3547
integer y = 284
integer width = 174
integer height = 152
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_spro_lotesfrutagranel_desverd_corto"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

type tab_1 from tab within w_maed_movtofrutagranel_recep_reembalaje
integer x = 78
integer y = 980
integer width = 4023
integer height = 1044
integer taborder = 60
boolean bringtotop = true
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
integer width = 3986
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
integer y = 16
integer width = 3927
integer height = 876
integer taborder = 10
boolean bringtotop = true
string dataobject = "dw_mues_spro_lotesfrutagranel_recepcion"
boolean hscrollbar = true
boolean livescroll = true
end type

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_recep_reembalaje.TriggerEvent("ue_modifica_detalle")

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
		w_maed_movtofrutagranel_recepcion_cajas.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_recepcion_cajas.PostEvent("ue_seteafila")

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
integer width = 3986
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
integer width = 3287
integer height = 840
integer taborder = 11
string dataobject = "dw_mues_movtoenvadeta_recepfruta"
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
		w_maed_movtofrutagranel_recepcion.PostEvent("ue_seteafila")

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

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_recep_reembalaje.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

type tp_3 from userobject within tab_1
string tag = "Registro de Envases que retira para cosecha"
integer x = 18
integer y = 112
integer width = 3986
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
integer width = 3269
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

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_recepcion.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_recepcion.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event


Start of PowerBuilder Binary Data Section : Do NOT Edit
08w_maed_movtofrutagranel_recep_reembalaje.bin 
2F00000600e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe00000006000000000000000000000001000000010000000000001000fffffffe00000000fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000600e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe00000006000000000000000000000001000000010000000000001000fffffffe00000000fffffffe0000000000000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
27fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
18w_maed_movtofrutagranel_recep_reembalaje.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
