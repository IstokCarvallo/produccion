$PBExportHeader$uo_palletencab.sru
forward
global type uo_palletencab from userobject
end type
type pb_ins_det from picturebutton within uo_palletencab
end type
type pb_eli_det from picturebutton within uo_palletencab
end type
type pb_salir from picturebutton within uo_palletencab
end type
type pb_cambio_folio from picturebutton within uo_palletencab
end type
type pb_imprimir from picturebutton within uo_palletencab
end type
type pb_compactos from picturebutton within uo_palletencab
end type
type pb_eliminar from picturebutton within uo_palletencab
end type
type pb_grabar from picturebutton within uo_palletencab
end type
type pb_nuevo from picturebutton within uo_palletencab
end type
type pb_buscar from picturebutton within uo_palletencab
end type
type dw_2 from uo_dw within uo_palletencab
end type
type dw_1 from uo_dw within uo_palletencab
end type
type gb_2 from groupbox within uo_palletencab
end type
type gb_1 from groupbox within uo_palletencab
end type
type sle_mensa from statictext within uo_palletencab
end type
end forward

global type uo_palletencab from userobject
string tag = "Mantenedor de Pallet"
integer width = 3374
integer height = 1908
long backcolor = 12632256
string text = "none"
borderstyle borderstyle = styleraised!
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
event resize pbm_size
event ue_antesguardar ( )
event ue_borra_detalle ( )
event ue_cambio_folio ( )
event ue_guardar ( )
event ue_imprimir ( )
event ue_imprimir_tarjas ( )
event ue_listo ( )
event ue_modifica_detalle ( )
event ue_nuevo ( )
event ue_nuevo_detalle ( )
event ue_recuperadatos ( )
event ue_seleccion ( )
event ue_cambionombre ( )
pb_ins_det pb_ins_det
pb_eli_det pb_eli_det
pb_salir pb_salir
pb_cambio_folio pb_cambio_folio
pb_imprimir pb_imprimir
pb_compactos pb_compactos
pb_eliminar pb_eliminar
pb_grabar pb_grabar
pb_nuevo pb_nuevo
pb_buscar pb_buscar
dw_2 dw_2
dw_1 dw_1
gb_2 gb_2
gb_1 gb_1
sle_mensa sle_mensa
end type
global uo_palletencab uo_palletencab

type variables
Integer						ii_yaexiste, ii_estadoventana
Long     					il_CajasTipoPallet

w_mant_deta_palletfruta iw_mantencion
w_maed_palletencab_tabs	iw_referencia

DataWindowChild			dw_especie, dw_etiqueta, dw_planta,&
                  		dw_cliente,dw_condiciones, &
								idwc_tipoenvase, idwc_categoria, idwc_recibidor, &
								idwc_destino, idwc_tipofrio, idwc_Camara, idwc_tippen

str_envase					istr_envase

uo_spro_serviciosplanta	iuo_serviciosplanta
uo_especie					iuo_especie
uo_tratamientofrio		iuo_tratamientofrio
uo_categorias				iuo_categorias
uo_etiquetas				iuo_etiquetas
uo_recibidores				iuo_recibidores
uo_destinos					iuo_destinos
uo_fechaMovto				iuo_FechaMovto
uo_camarasfrigo			iuo_camaras
uo_AnalizaPallet			iuo_pallet

protected:
Long							il_fila, il_AnchoDw_1
String						buscar, ordenar, ias_campo[]
Boolean						ib_datos_ok, ib_borrar, ib_ok, ib_traer, &
								ib_deshace = true, ib_ModEncab
Date							id_FechaAcceso
Time							it_HoraAcceso

Menu							im_menu

Str_parms					istr_parms
Str_mant						istr_mant
Str_busqueda				istr_busq
Str_info						istr_info
end variables

forward prototypes
public subroutine buscaenvase ()
public function boolean buscanombreembalaje (string as_embalaje)
public subroutine buscavariedad ()
public subroutine cargacajas ()
public function boolean creacion_cajas ()
public subroutine cuentacajas ()
public function boolean existecalibres ()
public function boolean existecondicion (integer as_valor)
public function boolean existeembalaje (string as_codigo)
public function boolean existepallet (string as_codigo)
public function boolean existeplanta (integer as_valor)
public function boolean existetipoembalaje (string as_codigo, boolean ab_captura)
public function boolean existevariecalibre (integer as_valor)
public function boolean existevariedad (integer li_columna, integer cliente)
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso ()
public function boolean noexistecliente (integer cliente)
public function boolean wf_actualiza_db (boolean borrando)
public function integer wf_modifica ()
public subroutine ue_mueve (string movimiento)
public subroutine cargapallet (window aw_referencia, long al_planta, integer ai_cliente, long al_pallet)
public function string cargacodigo (integer ai_cliente, long al_planta, long al_pallet, integer ai_procedencia)
end prototypes

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
ELSE
	dw_1.width	=	This.width - 400
	maximo		=	dw_1.width
END IF

dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					= 37

dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					= 64 + dw_2.Height
dw_1.height				= This.Height - dw_1.y - 41

gb_1.x 					= This.Width - 310
gb_1.y 					= 5
gb_1.width				= 275

li_posic_x				= This.Width - 250
li_posic_y				= gb_1.y + 88

IF pb_buscar.Visible THEN
	pb_buscar.x				= li_posic_x
	pb_buscar.y				= li_posic_y
	pb_buscar.width		= 156
	pb_buscar.height		= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				= li_posic_x
	pb_nuevo.y				= li_posic_y
	pb_nuevo.width			= 156
	pb_nuevo.height		= 133
	li_visible ++
	li_posic_y += 180
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			= li_posic_x
	pb_eliminar.y			= li_posic_y
	pb_eliminar.width		= 156
	pb_eliminar.height	= 133
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

IF pb_compactos.Visible THEN
	pb_compactos.x			= li_posic_x
	pb_compactos.y			= li_posic_y
	pb_compactos.width		= 156
	pb_compactos.height		= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_cambio_folio.Visible THEN
	pb_cambio_folio.x				= li_posic_x
	pb_cambio_folio.y				= li_posic_y
	pb_cambio_folio.width		= 156
	pb_cambio_folio.height		= 133
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

gb_1.height				= 180 * li_visible + 97 /*  (Según Botones Visibles)  */
gb_2.x 					= gb_1.x
gb_2.y 					= gb_1.height + 30
gb_2.width				= 275
gb_2.height				= 180 * 2 + 97 /*  (2 Botones)  */

pb_ins_det.x			= li_posic_x
pb_ins_det.y			= gb_2.y + 93
pb_ins_det.width		= 156
pb_ins_det.height		= 133

pb_eli_det.x			= li_posic_x
pb_eli_det.y			= pb_ins_det.y + 180
pb_eli_det.width		= 156
pb_eli_det.height		= 133
end event

event ue_antesguardar();Long		ll_Fila, ll_Numero
Integer	li_Planta, li_cliente, li_Secuencia

Message.DoubleParm	=	0

IF dw_1.RowCount() <= 0 THEN
	Message.DoubleParm	=	-1
	RETURN
END IF

IF dw_1.Object.total_cajas[1] = il_CajasTipoPallet THEN //	Pallet Completo
	dw_2.Object.paen_tipopa[1]	=	1
	iw_referencia.EscondeTab()
ELSEIF dw_1.Object.total_cajas[1] > il_CajasTipoPallet THEN
	MessageBox("Error", "Cantidad de Cajas del Detalle no corresponde~r" + &
					"a Cantidad de Cajas del Tipo de Pallet.~r~r" + &
					"Revise Detalle o Seleccione otro Tipo de Pallet.")
					
	Message.DoubleParm	=	-1
	RETURN
ELSE 	//	Pallet Pucho
	dw_2.Object.paen_tipopa[1]	=	2
END IF

li_cliente	=	dw_2.Object.clie_codigo[1]
ll_Numero	=	dw_2.Object.paen_numero[1]
li_Planta	=	dw_2.Object.plde_codigo[1]

IF Isnull(dw_2.Object.cama_codigo[1]) THEN dw_2.Object.cama_codigo[1]	=	0

SELECT	IsNull(Max(pafr_secuen), 0) + 1
	INTO	:li_Secuencia
	FROM	dba.spro_palletfruta
	WHERE	clie_codigo	=	:li_cliente
	AND	paen_numero	=	:ll_Numero
	AND	plde_codigo	=	:li_Planta ;

FOR ll_Fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_Fila,0,Primary!) = NewModified! THEN
		
		dw_1.Object.clie_codigo[ll_Fila]	=	dw_2.Object.clie_codigo[1]
		dw_1.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_1.Object.paen_numero[ll_Fila]	=	dw_2.Object.paen_numero[1]
		dw_1.Object.espe_codigo[ll_Fila]	=	dw_2.Object.espe_codigo[1]
		dw_1.Object.etiq_codigo[ll_Fila]	=	dw_2.Object.etiq_codigo[1]
		dw_1.Object.pafr_secuen[ll_Fila]	=	ll_Fila//li_Secuencia
	
		li_Secuencia ++
	END IF
NEXT

end event

event ue_borra_detalle();IF dw_1.rowcount() < 1 THEN
	dw_1.SetFocus()
	RETURN
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Tag,"No se puede borrar actual registro.")
	END IF

	IF dw_1.RowCount() = 0 THEN 
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_cambio_folio();Integer 	li_fila, li_cliente
Long		ll_pallet, ll_planta, ll_antiguo

str_mant	lstr_mant

lstr_mant.Argumento[1]	=	String(dw_2.Object.clie_codigo[1])
lstr_mant.Argumento[2]	=	String(dw_2.Object.plde_codigo[1])
lstr_mant.Argumento[3]	=	String(dw_2.Object.paen_numero[1])

OpenWithParm(w_cambio_folio_pallets, lstr_mant)

lstr_mant = Message.PowerObjectParm

li_cliente	=	Integer(lstr_mant.Argumento[1])
ll_planta	=	Long(lstr_mant.Argumento[2])
ll_antiguo	=	Long(lstr_mant.Argumento[3])
ll_pallet	=	Long(lstr_mant.Argumento[4])

IF ll_pallet > 0 AND NOT IsNull(ll_pallet) THEN
	DECLARE Cambio_Folio_pallet PROCEDURE FOR dba.fgran_cambio_folio_pallet  
			@cliente	= :li_cliente,   
			@planta 	= :ll_planta,   
			@pallet 	= :ll_antiguo,   
			@nuevo 	= :ll_pallet  
		USING SQLCA;
		
		EXECUTE Cambio_Folio_pallet;
		
		IF sqlca.SQLCode = -1 THEN
			F_errorbasedatos(sqlca,"Actualización de Folio de Pallet")
			Rollback;
			
			istr_mant.argumento[2]	=	String(ll_antiguo)
		ELSE
			Commit;
			
			istr_mant.argumento[2]	=	String(ll_pallet)
		END IF
	
ELSE
	istr_mant.argumento[2]	=	String(ll_antiguo)
	
END IF

TriggerEvent("ue_recuperadatos")
end event

event ue_guardar();IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled		=	True
	pb_imprimir.Enabled		=	True
	pb_compactos.Enabled		=	True
	
	IF dw_1.ModifiedCount() > 0 OR &
		dw_1.DeletedCount() > 0 OR &
		dw_2.ModifiedCount() > 0 THEN
		ii_estadoventana = 1
		
	ELSE
		ii_estadoventana = 0
		
	END IF
	
	iw_referencia.CambiaIcono()

ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_imprimir();SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "RECEPCION DE PALLETS"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_palletencab" 

vinf.dw_1.GetChild("clie_codigo", dw_cliente)
vinf.dw_1.GetChild("plde_codigo", dw_planta)
vinf.dw_1.GetChild("espe_codigo", dw_especie)
vinf.dw_1.GetChild("etiq_codigo", dw_etiqueta)
vinf.dw_1.GetChild("cond_codigo", dw_condiciones)
vinf.dw_1.GetChild("frio_tipofr", idwc_tipofrio)
vinf.dw_1.GetChild("cama_codigo", idwc_Camara)

vinf.dw_1.SetTransObject(sqlca)

dw_cliente.SetTransObject(sqlca)
dw_planta.SetTransObject(sqlca)
dw_especie.SetTransObject(sqlca)
dw_etiqueta.SetTransObject(sqlca)
dw_condiciones.SetTransObject(sqlca)
idwc_tipofrio.SetTransObject(sqlca)
idwc_Camara.SetTransObject(sqlca)

dw_cliente.Retrieve()
dw_planta.Retrieve()
dw_especie.Retrieve()
dw_etiqueta.Retrieve()
dw_condiciones.Retrieve()
idwc_tipofrio.Retrieve()
idwc_Camara.Retrieve(Long(istr_mant.argumento[6]))

Fila = vinf.dw_1.Retrieve(Long(istr_mant.argumento[2]), &
								  Long(istr_mant.argumento[6]),&
								  Long(istr_mant.argumento[1]))		

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

event ue_imprimir_tarjas();
IF Creacion_Cajas() THEN
	OpenSheetWithParm(w_mant_cajasprod_imprime_elimina,"2", w_main, 1, Original!)
	w_mant_cajasprod_imprime_elimina.Visible 									= 	False
	w_mant_cajasprod_imprime_elimina.dw_lotes.DataObject					=	"dw_mues_palletfruta_elimina_imprime_alter"
	w_mant_cajasprod_imprime_elimina.dw_lotes.SetTransObject(SQLCA)
	
	w_mant_cajasprod_imprime_elimina.uo_selcliente.codigo 				= 	Long(istr_mant.argumento[1])
	w_mant_cajasprod_imprime_elimina.uo_selplanta.codigo 					=	Long(istr_mant.argumento[6])
	w_mant_cajasprod_imprime_elimina.uo_selmercado.codigo 				=	Integer(Istr_mant.argumento[16])
	w_mant_cajasprod_imprime_elimina.em_recepcion.Text						=	istr_mant.argumento[2]

	w_mant_cajasprod_imprime_elimina.em_recepcion.TriggerEvent("Modified")
	
	w_mant_cajasprod_imprime_elimina.dw_lotes.SelectRow(0, True)
	
	w_mant_cajasprod_imprime_elimina.pb_acepta.TriggerEvent("Clicked")
	Close(w_mant_cajasprod_imprime_elimina)

	This.TriggerEvent("ue_recuperadatos")
END IF
end event

event ue_listo();IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	pb_Eliminar.Enabled	=	True
	
	IF istr_mant.Solo_Consulta THEN
		dw_2.Enabled			=	False
//		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled		=	False
		pb_ins_det.Enabled	=	False
		pb_eli_det.Enabled	=	False
	ELSE
		dw_2.Enabled			=	True
		pb_Eliminar.Enabled	=	True
		pb_Grabar.Enabled		=	True
		pb_ins_det.Enabled	=	True
		pb_eli_det.Enabled	=	True
	END IF
ELSE
	IF istr_mant.Solo_Consulta THEN
		pb_ins_det.Enabled	=	False
	ELSE
		pb_ins_det.Enabled	=	True
	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

event ue_modifica_detalle();
IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega = False
	
	istr_mant.Argumento[20]		= 	String(dw_2.Object.paen_ccajas[1])
	istr_mant.Argumento[9]		= 	String(dw_2.Object.etiq_codigo[1])
	
	IF IsNull(istr_mant.Argumento[20]) OR Integer(istr_mant.Argumento[20]) < 0 THEN
		istr_mant.Argumento[20]		= 	'0'
	END IF
	
	OpenWithParm(iw_mantencion, istr_mant)
	cuentacajas()
	
	IF dw_1.ModifiedCount() > 0 OR &
		dw_1.DeletedCount() > 0 OR &
		dw_2.ModifiedCount() > 0 THEN
		ii_estadoventana = 1
	ELSE
		ii_estadoventana = 0
	END IF
	
	iw_referencia.CambiaIcono()
END IF
end event

event ue_nuevo();Long		ll_modif1, ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
			ll_modif2	=	dw_2.GetNextModified(0, Primary!)
		
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

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()

HabilitaEncab(True)

dw_2.SetItem(1, "clie_codigo", Integer(istr_mant.Argumento[1]))
dw_2.SetItem(1, "plde_codigo", Integer(istr_mant.Argumento[6]))
dw_2.SetItem(1, "espe_codigo", gstr_paramplanta.CodigoEspecie)

istr_mant.argumento[3]	=	String(gstr_paramplanta.CodigoEspecie)

il_CajasTipoPallet								=	0

dw_2.Object.vari_codigo.Protect				=	0
dw_2.Object.vari_codigo.BackGround.Color 	= 	RGB(255,255,255)

dw_2.Object.paen_feccon[1]						=	Date(String(Today(), 'dd/mm/yyyy'))

idwc_Camara.SetTransObject(Sqlca)
idwc_Camara.Retrieve(Integer(istr_mant.Argumento[6]))

pb_cambio_folio.Enabled		=	False
end event

event ue_nuevo_detalle();istr_mant.borra	= False
istr_mant.agrega	= True
cuentacajas()

IF ExisteCalibres() THEN
	
	istr_mant.Argumento[20]		= 	String(dw_2.Object.paen_ccajas[1])
	
	IF IsNull(istr_mant.Argumento[20]) OR Integer(istr_mant.Argumento[20]) < 0 THEN
		istr_mant.Argumento[20]		= 	'0'
		
	END IF
	
	OpenWithParm(iw_mantencion, istr_mant)
	
	IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
	
	IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
		pb_eliminar.Enabled	= TRUE
		pb_grabar.Enabled		= TRUE
	END IF
	
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)
	cuentacajas()
ELSE
	MessageBox("Atención", "Falta Registrar Calibres para Especie (" + & 
					String(dw_2.Object.espe_codigo[1], '00') + ") - Tipo de Envase (" + &
					String(dw_2.Object.enva_tipoen[1]) + ") - Envase (" + &
					String(dw_2.Object.enva_codigo[1], '000') + ")")
END IF
end event

event ue_recuperadatos();/*En este script se debe extender el código, controlando la recuperacion de datos de dw_1 */
Long		ll_fila_d, ll_fila_e
Integer	li_Respuesta

w_main.SetMicroHelp("Recuperando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

pb_eliminar.Enabled		= False
pb_grabar.Enabled			= False
pb_eli_det.Enabled		= False

DO
	dw_2.SetRedraw(False)
	
	dw_2.Reset()
	
	ll_fila_e	=	dw_2.Retrieve(Long(istr_mant.argumento[2]), &
											Long(istr_mant.argumento[6]),&
											Long(istr_mant.argumento[1]))
	
	IF ll_fila_e = -1 THEN
		li_Respuesta	=	MessageBox("Error en Base de Datos", &
											"No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
	ELSEIF ll_fila_e < 1 THEN
		Messagebox("Advertencia", "El pallet ingresado no existe.~r~nFavor ingresar otra tarja", StopSign!)
	ELSE
		IF  iuo_destinos.Existe(dw_2.Object.dest_codigo[1],True,SqlCa) THEN
			 Istr_mant.argumento[16]	=	String(iuo_destinos.Codigo)
		END IF
		IF ll_fila_e > 0 THEN
			
			IF isnull(dw_2.Object.paen_inspec[1]) THEN
				dw_2.Object.paen_inspec[1] = 0
			ELSEIF dw_2.Object.paen_inspec[1] > 0 THEN
				dw_2.Object.dest_codigo.Protect = 1
				dw_2.Object.dest_codigo.Background.Color = RGB(192,192,192)
			ELSE
				dw_2.Object.dest_codigo.Protect = 0
				dw_2.Object.dest_codigo.Background.Color = RGB(255,255,255)
			END IF	
				
			istr_mant.Argumento[12]	=	String(dw_2.Object.paen_pmixto[1])
			istr_mant.Argumento[14]	=	dw_2.Object.emba_codigo[1]
			
			IF dw_2.Object.paen_estado[1]	=	2 THEN
				istr_Mant.Solo_Consulta	=	True
			ELSE
				istr_Mant.Solo_Consulta	=	False
			END IF
			
			ExisteEnvase(dw_2.Object.enva_tipoen[1],dw_2.Object.enva_codigo[1],istr_envase)
			
			dw_2.GetChild("tpen_codigo",idwc_tippen)
			idwc_tippen.SetTransObject(SQLCA)
			IF idwc_tippen.Retrieve(dw_2.Object.clie_codigo[1], dw_2.Object.emba_codigo[1]) = 0 THEN
				idwc_tippen.InsertRow(0)
			END IF	
			
			ExisteTipoEmbalaje(dw_2.Object.tpen_codigo[1],True)
			
		END IF
		
		DO
			//////////////////////////////////////////////////////////////////
			ll_fila_d	=	dw_1.Retrieve(	Long(istr_mant.argumento[1]), &
													Long(istr_mant.argumento[2]),&
													Long(istr_mant.argumento[6]))

			IF ll_fila_d = -1 THEN
				li_Respuesta	=	MessageBox("Error en Base de Datos", &
												"No es posible conectar la Base de Datos 2.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled			=	NOT istr_Mant.Solo_Consulta
				pb_grabar.Enabled				=	NOT istr_Mant.Solo_Consulta
				pb_imprimir.Enabled			=	True
				pb_compactos.Enabled			=	True
				pb_ins_det.Enabled			=	NOT istr_Mant.Solo_Consulta
				pb_cambio_folio.Enabled		=	True

				IF ll_fila_d > 0 THEN
					pb_eli_det.Enabled	=	NOT istr_Mant.Solo_Consulta
					dw_1.SetRow(1)
					dw_1.SelectRow(1, True)
					dw_1.SetFocus()
					
					HabilitaEncab(False)
					CuentaCajas()
				ELSE
					pb_grabar.Enabled	= FALSE
					//pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE li_Respuesta = 1

		IF li_Respuesta = 2 THEN Close(parent)
	END IF
	
	dw_2.SetRedraw(True)
LOOP WHILE li_Respuesta = 1

IF li_Respuesta = 2 THEN Close(parent)
end event

event ue_seleccion();str_busqueda lstr_busq

lstr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1]) 
lstr_busq.argum[2]	=	""
lstr_busq.argum[3]	=	""
lstr_busq.argum[4]	=	""
lstr_busq.argum[5]	=	String(dw_2.Object.plde_codigo[1])

OpenWithParm(w_busc_palletencab, lstr_busq)

lstr_busq = Message.PowerObjectParm

IF lstr_busq.argum[2] <> "" THEN
	istr_mant.argumento[1] = lstr_busq.argum[1]
	istr_mant.argumento[2] = lstr_busq.argum[2]
	istr_mant.argumento[6] = lstr_busq.argum[7]
	
	This.TriggerEvent("ue_recuperadatos")
	ExistePallet(String(dw_2.Object.paen_numero[1]))
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_cambionombre();iw_referencia.CambiaNombre()
end event

public subroutine buscaenvase ();Str_busqueda	lstr_busq
Integer li_especie, li_tipo, li_envase

lstr_busq.argum[1]	=	String(dw_2.GetItemNumber(1, "enva_tipoen"))

OpenWithParm(w_busc_envases, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[2] <> '' THEN
	
  
   istr_mant.argumento[7]	=	lstr_busq.argum[1]
	istr_mant.argumento[8]	=	lstr_busq.argum[2]
	
	dw_2.SetItem(1, "enva_codigo", Integer(lstr_busq.argum[2]))
	dw_2.SetItem(1, "enva_nombre", lstr_busq.argum[3])

	dw_2.GetChild("tpen_codigo", idwc_tippen)
	idwc_tippen.SetTransObject(SqlCa)
	IF idwc_tippen.Retrieve(Integer(lstr_busq.argum[1]),Integer(lstr_busq.argum[2])) = 0 THEN
		idwc_tippen.InsertRow(0)
	END IF	

	dw_2.SetColumn("enva_codigo")
	dw_2.SetFocus()
END IF
end subroutine

public function boolean buscanombreembalaje (string as_embalaje);String	ls_nombre
Integer 	li_cliente, li_tipoen, li_envase

li_cliente = dw_2.Object.clie_codigo[1]

SELECT emba_nombre, enva_tipoen, enva_codigo  
  INTO :ls_nombre, :li_tipoen, :li_envase
  FROM dba.embalajesprod
 WHERE emba_codigo	= :as_embalaje
 AND	 clie_codigo 	= :li_cliente;
 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embalajes")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Embalaje no Existe para este Cliente. Ingrese Otro.", Exclamation!, Ok!)
	RETURN False
ELSE
	dw_2.Object.enva_tipoen[1]	=	li_tipoen
	dw_2.Object.enva_codigo[1]	=	li_envase
	
	istr_Mant.Argumento[7]		=	String(li_tipoen)
	istr_Mant.Argumento[8]		=	String(li_envase)
	
	dw_2.GetChild("tpen_codigo", idwc_tippen)
	
	idwc_tippen.SetTransObject(SqlCa)
	IF idwc_tippen.Retrieve(li_cliente, as_embalaje) = 0 THEN
		idwc_tippen.InsertRow(0) 
	END IF	
	RETURN True
END IF 
end function

public subroutine buscavariedad ();Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	String(dw_2.GetItemNumber(1, "espe_codigo"))
lstr_busq.argum[2]	=	String(istr_mant.argumento[1])

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[4] <> '' THEN
	istr_mant.argumento[4]	=	lstr_busq.argum[3]
	istr_mant.argumento[5]	=	lstr_busq.argum[4]
	
	dw_2.setItem(1, "vari_codigo", Integer(lstr_busq.argum[3]))
	dw_2.setItem(1, "vari_nombre", lstr_busq.argum[4])
	dw_2.SetColumn("vari_codigo")
	dw_2.SetFocus()
END IF
end subroutine

public subroutine cargacajas ();//Integer	li_fila, li_cajas
//
//FOR li_fila = 1 TO dw_1.RowCount()
//	li_cajas	= li_cajas + dw_1.Object.pafr_ccajas[li_fila]
//NEXT
//
//dw_2.Object.paen_ccajas[1]	=	li_cajas
//
//istr_mant.argumento[11]	=	String(Long(istr_mant.argumento[11]) - li_cajas)
end subroutine

public function boolean creacion_cajas ();Integer 	li_fila, li_cliente, li_secuencia, li_control
Long		ll_pallet, ll_planta
String 	ls_pcname, ls_Formato

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", "ComputerName", RegString!, ls_pcname)

li_cliente	=	dw_2.Object.clie_codigo[1]
ll_planta	=	dw_2.Object.plde_codigo[1]
ll_pallet	=	dw_2.Object.paen_numero[1]

SELECT loco_dwcomp
  INTO :ls_Formato
  FROM dba.spro_correlcompequipo
 WHERE plde_codigo = :ll_planta
   AND Upper(equi_nombre) = Upper(:ls_pcname);
			

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Correativos de Compactos")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Nombre de Equipo No Tiene Asignado Formato de Compactos, Debe Asignar previamente.", &
					Exclamation!, OK!)
	RETURN False
ELSE
	FOR li_fila	=	1 TO dw_1.RowCount()
		li_secuencia	=	dw_1.Object.pafr_secuen[li_fila]
		
		IF li_secuencia > 999 THEN
			MEssageBox("Error de datos", "Imposible crear caja, ya que el correlativo indica que es una caja valida")
		ELSE
				
			DECLARE creacioncaja PROCEDURE FOR dba.fgran_creacion_caja_porbultos  
				@Cliente 	= :li_cliente,   
				@planta 		= :ll_planta,   
				@Pallet 		= :ll_pallet,   
				@Secuencia 	= :li_secuencia,   
				@computador = :ls_pcname 
			USING SQLCA;
			
			EXECUTE creacioncaja;
			
			IF SQLCA.SQLCode = -1 THEN
				F_ErrorBaseDatos(sqlca, "Lectura del Procedimiento Almacenado " + &
												"fgran_creacion_caja_porbultos" )
										
				sle_mensa.text	=	"Secuencia " + String(li_secuencia) + " imposible de crear"
				
			ELSE
				sle_mensa.text	=	"Secuencia " + String(li_secuencia) + " creada satisfactoriamente"
				li_control	++
			END IF	
				
			CLOSE creacioncaja;
			
		END IF
	NEXT
	
	IF li_control = dw_1.RowCount() THEN
		COMMIT;
		Return True
	ELSE
		ROLLBACK;
		Return False
	END IF
END IF
end function

public subroutine cuentacajas ();Long	ll_fila, ll_cajas

FOR ll_fila = 1 TO dw_1.RowCount()
	ll_cajas = ll_cajas + dw_1.Object.pafr_ccajas[ll_fila]
NEXT

dw_2.Object.paen_ccajas[1]	=	ll_cajas
end subroutine

public function boolean existecalibres ();Integer	li_Cantidad, li_Especie, li_TipoEnvase, li_Envase

li_Especie		=	dw_2.Object.espe_codigo[1]
li_TipoEnvase	=	dw_2.Object.enva_tipoen[1]
li_Envase		=	dw_2.Object.enva_codigo[1]

SELECT	Count(caen_calibr)
	INTO	:li_Cantidad
	FROM	dba.calibresenvase
	WHERE	espe_codigo	=	:li_Especie
	AND	enva_tipoen	=	:li_TipoEnvase
	AND	enva_codigo	=	:li_Envase ;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla CalibresEnvase")

	RETURN False
ELSEIF li_Cantidad = 0 OR IsNull(li_cantidad) THEN

	RETURN False
END IF

RETURN True
end function

public function boolean existecondicion (integer as_valor); integer	li_Condicion
 String	ls_DesCondicion
 
 li_Condicion	= as_valor
 
 SELECT	cond_nombre 
 	INTO	:ls_DesCondicion  
   FROM	dba.condicion
	WHERE	cond_codigo	=	:li_Condicion ;

IF sqlca.SQLCode = 0 THEN
	istr_mant.argumento[15]	=	ls_DesCondicion
ELSE
	istr_mant.argumento[15]	=	""
END IF

RETURN True
end function

public function boolean existeembalaje (string as_codigo);//String	ls_codigo, ls_nombre
//Integer	li_cliente, li_tipoen, li_codenvase
//
//li_cliente	= Integer(istr_mant.argumento[1])
//
//SELECT	emb.emba_nombre, env.enva_tipoen, env.enva_codigo
//	INTO 	:ls_nombre, :li_tipoen, :li_codenvase
//	FROM	dba.embalajes as emb, dba.envases as env
//	WHERE emb.emba_codigo	= :as_codigo
//	AND	env.enva_tipoen	= emb.enva_tipoen
//	AND	env.enva_codigo	= emb.enva_codigo;
//		
//IF sqlca.SQLCode = -1 THEN
//	F_errorbasedatos(sqlca,"Lectura tabla Emabalajes")
//	RETURN False
//ELSEIF sqlca.SQLCode = 100 THEN
//	MessageBox("Atención","Código de Embalaje no Existe para este Cliente. Ingrese Otro.", Exclamation!, Ok!)
//	RETURN False
//ELSE
//	istr_mant.argumento[7]	=	ls_codigo
//	istr_mant.argumento[8]	=	ls_nombre
//	
//	istr_mant.argumento[27]	=	String(li_tipoen)
//	istr_mant.argumento[28]	=	String(li_codenvase)
//	
//	dw_2.SetItem(1, "emba_nombre", ls_nombre)
//	dw_2.SetItem(1, "tien_codigo", li_tipoen)
	RETURN True
//END IF
end function

public function boolean existepallet (string as_codigo);Integer	li_cliente, li_cantid, li_planta, li_respuesta
Long		ll_nropal
Boolean	lb_Retorno = False

li_cliente					=	Integer(dw_2.GetItemNumber(1, "clie_codigo"))
li_planta					=	Integer(dw_2.GetItemNumber(1, "plde_codigo"))
ll_nropal 					=	Long(as_codigo)

SELECT	Count(*)
	INTO	:li_cantid
	FROM	dba.spro_palletencab
	WHERE	clie_codigo		= 	:li_cliente
	AND	paen_numero    = 	:ll_nropal
	AND	plde_codigo		=	:li_planta;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
	lb_retorno = True
ELSEIF li_cantid > 0 THEN
	
	istr_mant.argumento[2]	=	String(ll_nropal)
	
	This.TriggerEvent("ue_recuperadatos")
	
	istr_mant.argumento[3] 	= String(dw_2.Object.espe_codigo[1])
	istr_mant.argumento[4] 	= String(dw_2.Object.vari_codigo[1])
	istr_mant.argumento[5] 	= dw_2.Object.vari_nombre[1]
	istr_mant.argumento[6] 	= String(dw_2.Object.plde_codigo[1])
	istr_mant.argumento[7]  = String(dw_2.Object.enva_tipoen[1])
	istr_mant.argumento[8]  = String(dw_2.Object.enva_codigo[1])
	istr_mant.argumento[9] 	= String(dw_2.Object.etiq_codigo[1])
	istr_mant.argumento[10] = String(dw_2.Object.frio_tipofr[1])
	istr_mant.argumento[13] = String(dw_2.Object.cate_codigo[1])
	istr_mant.argumento[14] = dw_2.Object.emba_codigo[1]
	
	ExisteTipoEmbalaje(dw_2.Object.tpen_codigo[1],False)
	ExistePlanta(dw_2.Object.plde_codigo[1])
	
	lb_retorno = FALSE
ELSE
	lb_retorno = FALSE
END IF

RETURN lb_retorno
end function

public function boolean existeplanta (integer as_valor); integer li_planta, li_cliente
 string	ls_desplanta
 
 li_planta	=	as_valor
 li_cliente	=	Integer(istr_mant.argumento[1])
 
 SELECT	plde_nombre
	INTO	:ls_desplanta  
	FROM	dba.plantadesp
	WHERE	plde_codigo	=	:li_planta;

IF sqlca.SQLCode = 0 THEN
	istr_mant.argumento[14]	=	ls_desplanta
ELSE
	istr_mant.argumento[14]	=	""
END IF	

RETURN TRUE
end function

public function boolean existetipoembalaje (string as_codigo, boolean ab_captura);String	ls_embala
Integer	li_TipoEnvase, li_CodEnvase, li_cliente
Long		ll_altura

li_TipoEnvase	=	dw_2.Object.enva_tipoen[1]
li_CodEnvase	=	dw_2.Object.enva_codigo[1]
ls_embala		=	dw_2.Object.emba_codigo[1]
li_cliente		=	dw_2.Object.clie_codigo[1]

SELECT	tpem_cancaj, tpem_altura
	INTO	:il_CajasTipoPallet, :ll_altura
	FROM	dba.tipopallemba
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embala
	AND	tpem_codigo	=	:as_codigo ;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla tipopallemba")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Tipo de Pallet no Existe.~rIngrese Otro.", Exclamation!, Ok!)
	RETURN False
ELSE
	istr_mant.Argumento[11]		= 	String(il_CajasTipoPallet)
	istr_mant.Argumento[20]		= 	String(il_CajasTipoPallet)
	dw_2.Object.paen_altura[1]	=	ll_altura
//	ii_cajas							=	il_CajasTipoPallet
		
	CuentaCajas()
	
	RETURN True
END IF

RETURN False
end function

public function boolean existevariecalibre (integer as_valor);Integer	li_especie, li_variedad, li_cliente
String	ls_nombre, ls_secacod, ls_calibre
Long	   registros

li_especie	=	Integer(dw_2.Object.espe_codigo[1])
li_variedad	=	as_valor
li_cliente	=	Integer(istr_mant.argumento[1])

SELECT	Count(vaca_calibr)   
	INTO	:Registros
	FROM	dba.variecalibre
	WHERE espe_codigo	=	:li_especie
	AND	vari_codigo	=	:li_variedad ;

IF (sqlca.SQLCode) = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Variecalibre")
	RETURN False
	
ELSEIF Registros < 1 THEN
	MessageBox("Atención","Código de Variedad-Calibre no Existe.", Exclamation!, Ok!)
	RETURN False
END IF

RETURN True
end function

public function boolean existevariedad (integer li_columna, integer cliente);Integer	li_cliente, li_especie, li_variedad
String	ls_nombre

li_especie		=	dw_2.Object.espe_codigo[1]
li_variedad		=	li_Columna

SELECT	vari_nombre
	INTO	:ls_nombre
	FROM	dba.variedades
	WHERE espe_codigo	= 	:li_especie
	AND	vari_codigo = 	:li_variedad ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Variedades")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Variedad no Existe. Ingrese Otra.", Exclamation!, Ok!)
	RETURN False
ELSE
	istr_mant.argumento[3]	= String(li_especie)
	istr_mant.argumento[4]	= String(li_variedad)
	istr_mant.argumento[5]	= ls_nombre
	dw_2.SetItem(1, "vari_nombre", ls_nombre)
	RETURN True
END IF

RETURN False
end function

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.paen_numero.Protect				=	0
	dw_2.Object.paen_numero.BackGround.Color 	= 	RGB(255,255,255)
	dw_2.Object.paen_feccon.Protect				=	0
	dw_2.Object.paen_feccon.BackGround.Color 	= 	RGB(255,255,255)
	
	dw_2.Object.espe_codigo.Protect				=	0
	dw_2.Object.espe_codigo.BackGround.Color 	= 	RGB(255,255,255)
	dw_2.Object.vari_codigo.Protect				=	0
	dw_2.Object.vari_codigo.BackGround.Color 	= 	RGB(255,255,255)
	dw_2.Object.enva_tipoen.Protect				=	0
	dw_2.Object.enva_tipoen.BackGround.Color 	= 	RGB(255,255,255)
	dw_2.Object.enva_codigo.Protect				=	0
	dw_2.Object.enva_codigo.BackGround.Color 	= 	RGB(255,255,255)
	dw_2.Object.tpen_codigo.Protect				=	0
	dw_2.Object.tpen_codigo.BackGround.Color 	= 	RGB(255,255,255)
	
	dw_2.Object.b_buscavariedad.visible 		= 	1
	dw_2.Object.b_buscaenvase.visible 	 		= 	1
	
	dw_2.SetColumn("clie_codigo")
	dw_2.SetFocus()
ELSE
	dw_2.Object.paen_numero.Protect				=	1
	dw_2.Object.paen_numero.BackGround.Color 	= 	RGB(192,192,192)
	dw_2.Object.paen_feccon.Protect				=	1
	dw_2.Object.paen_feccon.BackGround.Color 	= 	RGB(192,192,192)
	dw_2.Object.espe_codigo.Protect				=	1
	dw_2.Object.espe_codigo.BackGround.Color 	= 	RGB(192,192,192)
	dw_2.Object.vari_codigo.Protect				=	1
	dw_2.Object.vari_codigo.BackGround.Color 	= 	RGB(192,192,192)
	dw_2.Object.enva_tipoen.Protect				=	1
	dw_2.Object.enva_tipoen.BackGround.Color 	= 	RGB(192,192,192)
	dw_2.Object.enva_codigo.Protect				=	1
	dw_2.Object.enva_codigo.BackGround.Color 	= 	RGB(192,192,192)
	dw_2.Object.tpen_codigo.Protect				=	1
	dw_2.Object.tpen_codigo.BackGround.Color 	= 	RGB(192,192,192)
	
	dw_2.Object.b_buscavariedad.visible 		= 	0
	dw_2.Object.b_buscaenvase.visible 	 		= 	0

	IF dw_2.Object.paen_pmixto[1]	=	1 AND Integer(dw_2.Object.paen_pmixto.Protect)	=	1 THEN
		dw_2.Object.vari_codigo.Protect				=	1
		dw_2.Object.vari_codigo.BackGround.Color 	= 	RGB(192,192,192)
	END IF
	
END IF
end subroutine

public subroutine habilitaingreso ();DateTime		ld_fecha
Boolean		lb_estado = True

dw_2.AcceptText()

IF IsNull(dw_2.Object.paen_numero[1]) OR dw_2.Object.paen_numero[1] = 0 THEN
	lb_estado = False
END IF

IF IsNull(dw_2.Object.espe_codigo[1]) OR dw_2.Object.espe_codigo[1] = 0 THEN
	lb_estado = False
END IF

IF IsNull(dw_2.Object.vari_codigo[1]) OR dw_2.Object.vari_codigo[1] = 0 THEN
	lb_estado = False
END IF

IF IsNull(dw_2.Object.etiq_codigo[1]) OR dw_2.Object.etiq_codigo[1] = 0 THEN
	lb_estado = False
END IF

IF IsNull(dw_2.Object.cate_codigo[1]) OR dw_2.Object.cate_codigo[1] = 0 THEN
	lb_estado = False
END IF

IF IsNull(dw_2.Object.cama_codigo[1]) OR dw_2.Object.cama_codigo[1] < 0 THEN
	lb_estado = False
END IF

IF IsNull(dw_2.Object.frio_tipofr[1]) OR dw_2.Object.frio_tipofr[1] = '' THEN
	lb_estado = False
END IF

IF IsNull(dw_2.Object.dest_codigo[1]) OR dw_2.Object.dest_codigo[1] = 0 THEN
	lb_estado = False
END IF

IF IsNull(dw_2.Object.paen_feccon[1]) OR dw_2.Object.paen_feccon[1] = Date(ld_fecha) THEN
	lb_estado = False
END IF

IF dw_2.Object.paen_tipopa[1] = 1 AND &
	IsNull(dw_2.Object.tpen_codigo[1]) OR dw_2.Object.tpen_codigo[1] = '' THEN
	lb_estado = False
END IF

pb_ins_det.Enabled = lb_estado
end subroutine

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT 	clie_nombre
INTO		:ls_nombre
FROM 		dba.clientesprod
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

public function boolean wf_actualiza_db (boolean borrando);String 	ls_pallet
Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()
dw_1.GrupoFecha	=	ldt_FechaHora
dw_2.GrupoFecha	=	ldt_FechaHora

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Tag)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Tag)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Tag)
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
					
			Commit;
			dw_2.ResetUpdate()
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Tag)
				Rollback;
				
			ELSE
				ls_pallet	=	String(dw_2.Object.clie_codigo[dw_2.GetRow()], '000') + &
									String(dw_2.Object.paen_numero[dw_2.GetRow()], '000000')
									
				IF iuo_pallet.analiza_datos(ls_pallet, SqlCa) THEN
					dw_2.Object.paen_sscc18[1] =	iuo_pallet.CodBarra
					
				END IF
				
				dw_2.Object.paen_gs1128[dw_2.GetRow()]	=	cargacodigo(dw_2.Object.clie_codigo[dw_2.GetRow()], &
																						dw_2.Object.plde_codigo[dw_2.GetRow()], &
																						dw_2.Object.paen_numero[dw_2.GetRow()], 1)
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Tag)
					
					RollBack;
				ELSE
					IF dw_2.Update(True, False) = 1 THEN
						Commit;
						lb_Retorno	=	True
						
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
					ELSE
						F_ErrorBaseDatos(sqlca, This.Tag)
						
						RollBack;
					END IF
				END IF
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Tag)
				
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Tag)
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function integer wf_modifica ();IF dw_1.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0

RETURN 1
end function

public subroutine ue_mueve (string movimiento);IF dw_1.RowCount() > 1 THEN
	dw_1.SetRedraw(False)
	
	CHOOSE CASE movimiento
		CASE "anterior"
			IF il_fila > 1 THEN il_fila --
		
		CASE "siguiente"
			IF il_fila < dw_1.RowCount() THEN il_fila ++
		
		CASE "primero"
			il_fila = 1 
		
		CASE ELSE
			il_fila = dw_1.RowCount()
		
	END CHOOSE

	dw_1.ScrolltoRow(il_fila)
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(0,False)
	dw_1.SelectRow(il_fila,True)
	dw_1.SetRedraw(True)
END IF
end subroutine

public subroutine cargapallet (window aw_referencia, long al_planta, integer ai_cliente, long al_pallet);istr_mant.argumento[6]	=	String(al_planta)
istr_mant.Argumento[1]	=	String(ai_cliente)
istr_mant.Argumento[2]	=	String(al_pallet)

iw_referencia				=	aw_referencia

TriggerEvent("ue_recuperadatos")
end subroutine

public function string cargacodigo (integer ai_cliente, long al_planta, long al_pallet, integer ai_procedencia);String ls_respuesta

DECLARE Codigo PROCEDURE FOR dba.genera_adhesivos_pallets  
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

on uo_palletencab.create
this.pb_ins_det=create pb_ins_det
this.pb_eli_det=create pb_eli_det
this.pb_salir=create pb_salir
this.pb_cambio_folio=create pb_cambio_folio
this.pb_imprimir=create pb_imprimir
this.pb_compactos=create pb_compactos
this.pb_eliminar=create pb_eliminar
this.pb_grabar=create pb_grabar
this.pb_nuevo=create pb_nuevo
this.pb_buscar=create pb_buscar
this.dw_2=create dw_2
this.dw_1=create dw_1
this.gb_2=create gb_2
this.gb_1=create gb_1
this.sle_mensa=create sle_mensa
this.Control[]={this.pb_ins_det,&
this.pb_eli_det,&
this.pb_salir,&
this.pb_cambio_folio,&
this.pb_imprimir,&
this.pb_compactos,&
this.pb_eliminar,&
this.pb_grabar,&
this.pb_nuevo,&
this.pb_buscar,&
this.dw_2,&
this.dw_1,&
this.gb_2,&
this.gb_1,&
this.sle_mensa}
end on

on uo_palletencab.destroy
destroy(this.pb_ins_det)
destroy(this.pb_eli_det)
destroy(this.pb_salir)
destroy(this.pb_cambio_folio)
destroy(this.pb_imprimir)
destroy(this.pb_compactos)
destroy(this.pb_eliminar)
destroy(this.pb_grabar)
destroy(this.pb_nuevo)
destroy(this.pb_buscar)
destroy(this.dw_2)
destroy(this.dw_1)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.sle_mensa)
end on

event constructor;
/*Argumentos :	[1]	=	Código de Exportador
						[2]	=	Numero de Pallet
						[3]	=	Código de Especie
						[4]	=	Código de Variedad
						[5]	=	Descripcion de Variedad
						[6]	=	Código de Planta
						[7]	=	Código de Tipo de Envase
						[8]	=	Código de Envase
						[9]	=	Código de Etiqueta
						[10]	=	Código de Tipo Frio
						[11]	=	Cantidad de Cajas
						[12]	=	Pallet Mixto 0 = No - 1 = Si
						[13]	=	Código de Categoria
						[15]	=	Código de Embalaje
						[16]	=	Código de Mercado
						
*/

x	= 0
y	= 0

iuo_serviciosplanta	=	Create uo_spro_serviciosplanta
iuo_especie				=	Create uo_especie
iuo_tratamientofrio	=	Create uo_tratamientofrio
iuo_categorias			=	Create uo_categorias
iuo_etiquetas			=	Create uo_etiquetas
iuo_recibidores		=	Create uo_recibidores
iuo_destinos			=	Create uo_destinos
iuo_FechaMovto			=	Create uo_FechaMovto
iuo_camaras 			=	Create uo_camarasfrigo
iuo_pallet				=	Create uo_AnalizaPallet

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				=	dw_1
istr_mant.solo_consulta	=	False
istr_mant.argumento[25]	=	''

dw_2.GetChild("clie_codigo", dw_cliente)
dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("espe_codigo", dw_especie)
dw_2.GetChild("etiq_codigo", dw_etiqueta)
dw_2.GetChild("tpen_codigo", idwc_tippen)
dw_2.GetChild("enva_tipoen", idwc_tipoenvase)
dw_2.GetChild("cate_codigo", idwc_categoria)
dw_2.GetChild("reci_codigo", idwc_recibidor)
dw_2.GetChild("dest_codigo", idwc_destino)
dw_2.GetChild("frio_tipofr", idwc_tipofrio)
dw_2.GetChild("cama_codigo", idwc_Camara)

dw_cliente.SetTransObject(sqlca)
dw_planta.SetTransObject(sqlca)
dw_especie.SetTransObject(sqlca)
dw_etiqueta.SetTransObject(sqlca)
idwc_tipoenvase.SetTransObject(sqlca)
idwc_tippen.SetTransObject(sqlca)
idwc_categoria.SetTransObject(sqlca)
idwc_recibidor.SetTransObject(sqlca)
idwc_destino.SetTransObject(sqlca)
idwc_tipofrio.SetTransObject(sqlca)
idwc_Camara.SetTransObject(sqlca)

dw_cliente.Retrieve()
dw_planta.Retrieve()
dw_especie.Retrieve()
dw_etiqueta.Retrieve()
idwc_tippen.InsertRow(0)
idwc_tipoenvase.Retrieve()
idwc_categoria.Retrieve()
idwc_recibidor.Retrieve()
idwc_destino.Retrieve()
idwc_tipofrio.Retrieve()
idwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta)

istr_mant.argumento[1]		=	String(gi_CodExport)
istr_mant.argumento[6]		=	String(gstr_ParamPlanta.CodigoPlanta)
dw_2.object.plde_codigo[1]	= 	gstr_ParamPlanta.CodigoPlanta
istr_mant.argumento[10] 	= 	'1'
istr_mant.argumento[12] 	= 	'0'
istr_mant.argumento[13] 	= 	'1'

buscar							=	"Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar							=	"Código:vari_codigo,Descripción:vari_nombre"

pb_compactos.Enabled			=	False
pb_cambio_folio.Enabled		=	False

end event

type pb_ins_det from picturebutton within uo_palletencab
string tag = "Ingresar Detalle"
integer x = 3113
integer y = 1564
integer width = 155
integer height = 132
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\desarrollo\bmp\inserte.bmp"
string disabledname = "\desarrollo\bmp\insertd.bmp"
alignment htextalign = left!
string powertiptext = "Ingresar Detalle"
end type

event clicked;Parent.TriggerEvent("ue_nuevo_detalle")
end event

type pb_eli_det from picturebutton within uo_palletencab
integer x = 3113
integer y = 1704
integer width = 155
integer height = 132
integer taborder = 60
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\desarrollo\bmp\suprime.bmp"
string disabledname = "\desarrollo\bmp\suprimd.bmp"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_borra_detalle")
end event

type pb_salir from picturebutton within uo_palletencab
string tag = "Salir [Cerrar Ventana Activa]"
boolean visible = false
integer x = 3666
integer y = 1928
integer width = 155
integer height = 132
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\exit.bmp"
alignment htextalign = left!
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

event clicked;//Close(Parent)
end event

type pb_cambio_folio from picturebutton within uo_palletencab
string tag = "Cambio de Folio Pallet"
integer x = 3113
integer y = 756
integer width = 155
integer height = 132
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "FullBuild!"
string disabledname = "FullBuild!"
alignment htextalign = left!
string powertiptext = "Cambio de Folio Pallet"
end type

event clicked;Parent.TriggerEvent("ue_cambio_folio")
end event

type pb_imprimir from picturebutton within uo_palletencab
string tag = "Imprimir información"
integer x = 3113
integer y = 436
integer width = 155
integer height = 132
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\imprimee.bmp"
string disabledname = "\desarrollo\bmp\imprimed.bmp"
alignment htextalign = left!
string powertiptext = "Imprimir información"
end type

event clicked;Parent.TriggerEvent("ue_imprimir")
end event

type pb_compactos from picturebutton within uo_palletencab
string tag = "Impresión de Adhesivos"
integer x = 3113
integer y = 596
integer width = 155
integer height = 132
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "ComputeToday!"
string disabledname = "ComputeToday!"
alignment htextalign = left!
string powertiptext = "Impresión de Adhesivos"
end type

event clicked;Parent.TriggerEvent("ue_imprimir_tarjas")
end event

type pb_eliminar from picturebutton within uo_palletencab
string tag = "Eliminar Información"
boolean visible = false
integer x = 3666
integer y = 1928
integer width = 155
integer height = 132
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\deletee.bmp"
string disabledname = "\desarrollo\bmp\deleted.bmp"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_borrar")
end event

type pb_grabar from picturebutton within uo_palletencab
string tag = "Grabar Información"
integer x = 3113
integer y = 276
integer width = 155
integer height = 132
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\disksave.bmp"
string disabledname = "\desarrollo\bmp\disksavd.bmp"
alignment htextalign = left!
string powertiptext = "Grabar Información"
end type

event clicked;Parent.TriggerEvent("ue_guardar")	
end event

type pb_nuevo from picturebutton within uo_palletencab
string tag = "Limpia para Nuevos Datos"
boolean visible = false
integer x = 3666
integer y = 1928
integer width = 155
integer height = 132
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\Nuevoe.bmp"
string disabledname = "\desarrollo\bmp\Nuevod.bmp"
alignment htextalign = left!
string powertiptext = "Limpia para Nuevos Datos"
end type

event clicked;Parent.TriggerEvent("ue_nuevo")
end event

type pb_buscar from picturebutton within uo_palletencab
string tag = "Buscar Información"
boolean visible = false
integer x = 3666
integer y = 1928
integer width = 155
integer height = 132
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\buscae.bmp"
string disabledname = "\desarrollo\bmp\buscad.bmp"
alignment htextalign = left!
string powertiptext = "Buscar Información"
end type

event clicked;Parent.TriggerEvent("ue_seleccion")
end event

type dw_2 from uo_dw within uo_palletencab
integer x = 23
integer y = 28
integer width = 2981
integer height = 836
integer taborder = 10
string dataobject = "dw_mant_palletencab"
boolean vscrollbar = false
end type

event buttonclicked;call super::buttonclicked;str_mant lstr_mant

CHOOSE CASE dwo.name
		
	CASE "b_buscavariedad"
		IF dw_2.Object.paen_pmixto[1]	=	1 AND Integer(dw_2.Object.paen_pmixto.Protect)	=	1 THEN RETURN
		buscavariedad()

	CASE "b_buscaenvase"
		buscaenvase()
		
END CHOOSE
end event

event doubleclicked;call super::doubleclicked;//
end event

event itemchanged;call super::itemchanged;Long		ll_null, ll_fila
String	ls_columna, ls_Null, ls_Fecha
Date 		ldt_fecha

DataWIndowChild	dw_calibres

SetNull(ll_Null)
SetNull(ls_Null)

ls_columna = dwo.name
ii_yaexiste	=	0

CHOOSE CASE ls_columna
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(row,"clie_codigo",Integer(ls_Null))
			RETURN 1
		ELSE
			dw_2.GetChild("espe_codigo", dw_especie)
			dw_especie.SetTransObject(sqlca)
			dw_especie.Retrieve(integer(data))
			istr_mant.Argumento[1] = data
		END IF	
		
	CASE "paen_numero"
		IF ExistePallet(data) THEN
			ii_yaexiste = 1
			dw_2.SetItem(1, "paen_numero", ll_null)
			RETURN 1
		ELSE
			istr_mant.Argumento[2] = data
			Parent.PostEvent("ue_CambioNombre")
		END IF

	CASE "paen_feccon"
		ldt_Fecha	=	Date(Mid(data,1,10))
		
		IF NOT iuo_FechaMovto.Valida_FechaMovto(ldt_Fecha) THEN
			This.SetItem(1,"paen_feccon",Date(ls_Null))
			This.SetFocus()
			RETURN 1
		END IF
		
		ls_Fecha	=	Data		
		This.SetItem(1, ls_Columna, Date(Mid(ls_Fecha,1,10)))
	
	CASE "espe_codigo"
		IF iuo_especie.Existe(Integer(Data),True,SqlCa) THEN
			istr_mant.argumento[3]	= data
		ELSE
			dw_2.SetItem(1, ls_Columna, Integer(ls_Null))
			dw_2.SetItem(1, "vari_nombre", "")
			dw_2.SetItem(1, "vari_codigo", ll_null)
			RETURN 1
		END IF

	CASE "vari_codigo"
		IF NOT ExisteVariedad(Integer(data),gi_codexport) THEN
			dw_2.SetItem(1, "vari_nombre", "")
			dw_2.SetItem(1, "vari_codigo", ll_null)
			RETURN 1
		END IF
					
	CASE "plde_codigo"
		istr_mant.argumento[6]	=	data
		idwc_Camara.Retrieve(data)
		
	CASE "emba_codigo"
		IF NOT buscanombreembalaje(data) THEN
			This.SetItem(1, ls_columna, ls_Null)
			RETURN 1		
		ELSE
			istr_mant.argumento[14]	=	data
		END IF

	CASE "enva_tipoen"
		IF NOT ExisteEnvase(Integer(data),0,istr_envase) THEN
			This.SetItem(1, ls_columna, ll_null)
			RETURN 1
		ELSE
			istr_Mant.Argumento[7]	=	Data
		END IF

	CASE "enva_codigo"
		This.Object.tpen_codigo[1]	=	ls_Null
		IF NOT ExisteEnvase(This.Object.enva_tipoen[1],Integer(Data),istr_envase) THEN
			This.SetItem(1, "enva_nombre", "")
			This.SetItem(1, ls_Columna, ll_null)
			RETURN 1
		ELSE
			istr_Mant.Argumento[8]	=	Data
			This.SetItem(1, "enva_nombre", istr_envase.Nombre)
			dw_2.GetChild("tpen_codigo", idwc_tippen)
			idwc_tippen.SetTransObject(SqlCa)
		 	IF idwc_tippen.Retrieve(istr_envase.TipoEnvase,istr_envase.Codigo)=0 THEN
				idwc_tippen.InsertRow(0) 
			END IF	
		END IF

	CASE "tpen_codigo"
//		IF dw_2.object.paen_tipopa[Row] = 1 THEN
			IF ExisteTipoEmbalaje(data, False) = False THEN
				dw_2.SetItem(1, ls_Columna, ls_Null)
				RETURN 1
//			END IF
		END IF

	CASE "etiq_codigo"
		IF iuo_etiquetas.Existe(Integer(Data),True,SqlCa) THEN
			istr_mant.argumento[9]	= data
			FOR	ll_fila	=	1	TO	dw_1.RowCount()
				dw_1.Object.etiq_codigo[ll_fila]	=	Integer(data)
			NEXT
		ELSE
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF
		
	CASE "frio_tipofr"
		IF iuo_tratamientofrio.ofp_recupera_tratamientofrio(SqlCA,Data,True) THEN
			istr_mant.argumento[10]	= data
		ELSE
			This.SetItem(1, ls_Columna, ls_Null)
			RETURN 1
		END IF

	CASE "cate_codigo"
		IF iuo_categorias.Existe(Integer(Data),True,SqlCa) THEN
			istr_mant.argumento[13]	= data
		ELSE
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF
		
	CASE "paen_ccajas"
		istr_mant.argumento[11]	= data
			
	CASE "paen_pexpor"
		istr_mant.argumento[22]	= data
			
	CASE "paen_pmixto"
		istr_mant.argumento[12]	= data

	CASE "sepl_codigo"
		IF NOT iuo_serviciosplanta.Existe(Integer(Data),True,SqlCa) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF

	CASE "reci_codigo"
		IF NOT iuo_recibidores.Existe(Long(Data),True,SqlCa) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF

	CASE "dest_codigo"
		IF NOT iuo_destinos.Existe(Integer(Data),True,SqlCa) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1
		ELSE
			 Istr_mant.argumento[16]	=	String(iuo_destinos.Codigo)
		END IF
	
	CASE "cama_codigo"
		IF NOT iuo_camaras.Existe(This.Object.plde_codigo[1], Integer(data), True, SQLCA) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1
		ELSE
			This.Object.frio_tipofr[1]	=	iuo_camaras.TipoFrio
			istr_mant.argumento[10]		=	String(iuo_camaras.TipoFrio)
		END IF
			
END CHOOSE

HabilitaIngreso()
	
IF dw_1.ModifiedCount() > 0 OR &
	dw_1.DeletedCount() > 0 OR &
	dw_2.ModifiedCount() > 0 THEN
	ii_estadoventana = 1
ELSE
	ii_estadoventana = 0
END IF

iw_referencia.CambiaIcono()
end event

type dw_1 from uo_dw within uo_palletencab
integer x = 23
integer y = 892
integer width = 2985
integer height = 988
integer taborder = 10
boolean titlebar = true
string dataobject = "dw_mues_palletfruta"
boolean hscrollbar = true
boolean livescroll = true
end type

event doubleclicked;call super::doubleclicked;Parent.TriggerEvent("ue_modifica_detalle")

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

event constructor;call super::constructor;il_AnchoDw_1	=	This.Width	
end event

event dwnkey;call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		Parent.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		Parent.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

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

event ue_seteafila;call super::ue_seteafila;il_fila	= This.GetRow()

This.SelectRow(0, False)
This.SelectRow(il_fila, True)

RETURN 0
end event

type gb_2 from groupbox within uo_palletencab
integer x = 3054
integer y = 1488
integer width = 274
integer height = 388
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_1 from groupbox within uo_palletencab
integer x = 3054
integer y = 192
integer width = 274
integer height = 748
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type sle_mensa from statictext within uo_palletencab
boolean visible = false
integer x = 23
integer y = 1128
integer width = 2985
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

