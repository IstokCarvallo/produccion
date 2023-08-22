$PBExportHeader$w_mant_spro_ordenpesovaciado.srw
forward
global type w_mant_spro_ordenpesovaciado from w_mant_encab_deta_csd
end type
type dw_3 from uo_dw within w_mant_spro_ordenpesovaciado
end type
end forward

global type w_mant_spro_ordenpesovaciado from w_mant_encab_deta_csd
integer width = 3438
integer height = 1992
dw_3 dw_3
end type
global w_mant_spro_ordenpesovaciado w_mant_spro_ordenpesovaciado

type variables


DatawindowChild				idwc_cliente, idwc_planta, idwc_tipoenvases, idwc_codigoenvase, idwc_variedad, idwc_cosechero
uo_spro_ordenproceso		iuo_ordenproceso
Integer							ii_correlativo
boolean							ib_graba
end variables

forward prototypes
public function boolean noexistecliente (integer ai_codigo)
public function boolean noexisteplanta (integer ai_codigo)
public function boolean validabultos ()
public function integer cargacorrelativo ()
public subroutine calculaneto (integer row)
end prototypes

public function boolean noexistecliente (integer ai_codigo);Integer	li_cliente

SELECT clie_codigo
INTO	:li_cliente
FROM	dba.clientesprod
WHERE	clie_codigo	=	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla ClientesProd" )
		Return True			
ELSEIF sqlca.SQLCode = 100 THEN
		messagebox("Atención","No Existe Código de cliente Seleccionado en Tabla ")			
		Return True						
END IF

Return False
end function

public function boolean noexisteplanta (integer ai_codigo);Integer	li_planta

SELECT plde_codigo
INTO	:li_planta
FROM	dba.plantadesp
WHERE	plde_codigo	=	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla PlantasDesp" )
		Return True			
ELSEIF sqlca.SQLCode = 100 THEN
		messagebox("Atención","No Existe Código de Planta Seleccionada en Tabla ")			
		Return True						
END IF

Return False
end function

public function boolean validabultos ();Integer	li_filas, li_bultosingresados, li_bultosorden

li_bultosorden 	=	dw_2.Object.orpr_canbul[1]

FOR li_filas = 1 to dw_1.RowCount()
	li_bultosingresados	=	li_bultosingresados + dw_1.Object.opva_canbul[li_filas]
NEXT

IF li_bultosorden < li_bultosingresados THEN
	Return False
ELSE
	Return True
END IF
end function

public function integer cargacorrelativo ();Integer planta, cliente, orden, loteplt, loteesp, lotecod, max_secuen

cliente	=	Integer(istr_mant.Argumento[1])
planta		=	Integer(istr_mant.Argumento[2])
orden		=	Integer(istr_mant.Argumento[3])
loteplt		=	Integer(istr_mant.Argumento[4])
loteesp	=	Integer(istr_mant.Argumento[5])
lotecod	=	Integer(istr_mant.Argumento[6])

SELECT max(opva_secuen)
INTO :max_secuen
FROM dba.spro_ordenpesovaciado
WHERE clie_codigo =: cliente
		and plde_codigo =: planta
		and orpr_tipord = 4
		and orpr_numero =: orden
		and lote_pltcod =: loteplt
		and lote_espcod =: loteesp
		and lote_codigo =: lotecod;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Spro_OrdenPesoVaciado" )
	max_secuen 	= 	0
ELSEIF IsNull(max_secuen) OR max_secuen < 1 THEN
	max_secuen 	= 	0
END IF

Return Max_secuen
end function

public subroutine calculaneto (integer row);Decimal 	ld_netos, KilosBrutos, KilosTara, bultos

KilosBrutos	=	dw_1.Object.opva_kilpro[row]
bultos			=	dw_1.Object.opva_canbul[row]
KilosTara		=	bultos	* Dec(istr_mant.Argumento[7])
ld_netos		=	KilosBrutos - KilosTara

dw_1.SetItem(row, 'opva_totnet', ld_netos)
dw_1.AcceptText()
end subroutine

on w_mant_spro_ordenpesovaciado.create
int iCurrent
call super::create
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
end on

on w_mant_spro_ordenpesovaciado.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_3)
end on

event resize;Integer	maximo

maximo	= dw_1.width + dw_3.width

IF dw_2.width > maximo THEN maximo = dw_2.width

dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					= 37

dw_1.y					= 64 + dw_2.Height
dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41
dw_3.y					= 64 + dw_2.Height
dw_3.height				= This.WorkSpaceHeight() - dw_3.y - 41

//gb_1.x 					= This.WorkSpaceWidth() - 310
//gb_1.y 					= 5
//gb_1.width				= 275
//gb_1.height				= 180 * 6 + 97 /*  (6 Botones)  */

pb_buscar.x				= This.WorkSpaceWidth() - 250
//pb_buscar.y				= gb_1.y + 88
pb_buscar.width		= 156
pb_buscar.height		= 133

pb_nuevo.x				= pb_buscar.x
pb_nuevo.y				= pb_buscar.y + 180
pb_nuevo.width			= 156
pb_nuevo.height		= 133

pb_eliminar.x			= pb_buscar.x
pb_eliminar.y			= pb_nuevo.y + 180
pb_eliminar.width		= 156
pb_eliminar.height		= 133

pb_grabar.x				= pb_buscar.x
pb_grabar.y				= pb_eliminar.y + 180
pb_grabar.width		= 156
pb_grabar.height		= 133

pb_imprimir.x			= pb_buscar.x
pb_imprimir.y			= pb_grabar.y + 180
pb_imprimir.width		= 156
pb_imprimir.height	= 133

pb_salir.x				= pb_buscar.x
pb_salir.y				= pb_imprimir.y + 180
pb_salir.width			= 156
pb_salir.height			= 133

//gb_2.x 					= gb_1.x
//gb_2.y 					= 1293
//gb_2.width				= 275
//gb_2.height				= 180 * 1 + 97 /*  (2 Botones)  */

//pb_ins_det.x			= pb_buscar.x
//pb_ins_det.y			= gb_2.y + 93
//pb_ins_det.width		= 156
//pb_ins_det.height		= 133

pb_eli_det.x				= pb_buscar.x
//pb_eli_det.y				= gb_2.y + 93 //pb_ins_det.y + 180
pb_eli_det.width		= 156
pb_eli_det.height		= 133
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta, ll_row, ll_bultos, ll_bultosrep, ll_bultosyaingresados
Integer  li_Protec=0
String   ls_lote

ll_fila_d	=	dw_2.Retrieve(Integer(istr_mant.Argumento[1]),Integer(istr_mant.Argumento[2]),Integer(istr_mant.Argumento[3]))

IF ll_fila_e = -1 THEN
	respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
ELSE
	dw_2.GetChild("vari_codigo", idwc_variedad)
	idwc_variedad.SetTransObject(sqlca)
	idwc_variedad.Retrieve(dw_2.Object.espe_codigo[1])
	
	IF dw_2.RowCount() > 0 THEN
		 dw_3.Retrieve(Integer(istr_mant.Argumento[1]),Integer(istr_mant.Argumento[2]),Integer(istr_mant.Argumento[3]))
	END IF
END IF

dw_1.Reset()
end event

event ue_nuevo;call super::ue_nuevo;Integer li_fila
dw_3.Reset()

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)

iuo_ordenproceso	=	Create uo_spro_ordenproceso

istr_mant.argumento[1]	=	String(gi_Codexport)
istr_mant.argumento[2]	=	String(gstr_ParamPlanta.CodigoPlanta)

dw_1.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve()

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve()

dw_3.GetChild("enva_tipoen", idwc_tipoenvases)
idwc_tipoenvases.SetTransObject(sqlca)
idwc_tipoenvases.Retrieve()

dw_3.GetChild("enva_codigo", idwc_codigoenvase)
idwc_codigoenvase.SetTransObject(sqlca)
idwc_codigoenvase.Retrieve(0)

li_fila	=	dw_1.InsertRow(0)

dw_1.SetItem(li_fila, "clie_codigo", gi_Codexport)
//dw_1.Object.clie_codigo[1]		=	gi_Codexport

dw_1.SetItem(li_fila, "plde_codigo", gstr_ParamPlanta.CodigoPlanta)
//dw_1.Object.plde_codigo[1]		=	gstr_ParamPlanta.CodigoPlanta
end event

event ue_antesguardar;call super::ue_antesguardar;Boolean lb_modif

IF dw_1.GetRow() > 0 THEN
	IF dw_1.GetItemStatus(dw_1.GetRow(), "cale_calida", Primary!) <> NotModified! THEN lb_modif = TRUE
	IF dw_1.GetItemStatus(dw_1.GetRow(), "opva_canbul", Primary!) <> NotModified! THEN lb_modif = TRUE
	IF dw_1.GetItemStatus(dw_1.GetRow(), "opva_kilpro", Primary!) <> NotModified! THEN lb_modif = TRUE
	IF ib_graba THEN 
		IF NOT lb_modif AND IsNull(dw_1.GetItemNumber(dw_1.GetRow(), "cale_calida")) THEN dw_1.DeleteRow(dw_1.RowCount())
		lb_modif = TRUE
	END IF
	 
	IF lb_modif THEN
		IF dw_1.RowCount() > 0 AND dw_1.GetRow() > 0  THEN
			dw_1.Object.opva_secuen[dw_1.GetRow()]		=	cargacorrelativo() + 1
			dw_1.Object.opva_fechap[dw_1.GetRow()]		=	f_fechahora()
		END IF
		ib_graba = False
		
	ELSE
		Message.DoubleParm = -1
	END IF
ELSE
	Message.DoubleParm = -1
END IF
end event

event ue_borra_detalle;/* Este Script debe ser traspasado a la ventana descendiente, pués normalmente se adicionan controles y asignaciones
	de argumentos.*/

IF dw_1.rowcount() < 2 THEN RETURN

SetPointer(HourGlass!)

ib_borrar 			= True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

ib_graba				=	True

istr_mant.borra		= True
istr_mant.agrega	= False

istr_mant.respuesta = MessageBox("Confirmacion", "Desea eliminar el detalle seleccionado",Question!, YesNo!)

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF
 IF dw_1.RowCount() = 0 THEN pb_eli_det.Enabled = False
END IF

istr_mant.borra	 = False
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_mant_spro_ordenpesovaciado
integer y = 552
integer width = 1723
integer height = 1196
string title = "Detalle de Kilos por Orden"
string dataobject = "dw_mant_ordenpesovaciado"
boolean resizable = true
end type

event dw_1::itemchanged;call super::itemchanged;Integer	li_codigo, li_null, li_find, li_calidad, li_envatip, li_envacod
String		ls_columna
Date     	ld_fecha
Decimal	ld_peso

SetNull(li_null)

ls_columna = dwo.Name

CHOOSE CASE ls_columna
	CASE "cale_calida"
		li_find	=	idwc_cosechero.Find("cale_calida  = '" + data + "'", 1, idwc_cosechero.RowCount())
								
		IF li_find	= 0 THEN
			MessageBox("Error", "La calidad de envase ingresada no existe")
			This.SetItem(row, ls_columna, li_null)
			Return 1
		ELSE
			ld_peso	=	idwc_cosechero.GetItemDecimal(li_find, "cale_pesoen")
			istr_mant.Argumento[9]	=	String(ld_peso)
		END IF
	
	CASE "opva_canbul"
		IF NOT validabultos() THEN
			MessageBox("Error", "La calidad de envase ingresada no existe")
			This.SetItem(row, ls_columna, li_null)
			Return 1
		END IF
		
END CHOOSE

This.AcceptText()
CalculaNeto(row)
end event

event dw_1::clicked;//
end event

event dw_1::getfocus;//
This.SelectRow(0, False)
end event

event dw_1::rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = getrow()
	This.SelectRow(0,False)
	//This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event dw_1::ue_seteafila;il_fila	= This.GetRow()

This.SelectRow(0, False)
//This.SelectRow(il_fila, True)

RETURN 0
end event

event dw_1::doubleclicked;//
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_mant_spro_ordenpesovaciado
integer width = 2153
integer height = 432
string dataobject = "dw_mues_ordenproceso_pesovaciado"
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null
Integer	li_codigo, li_null
String	ls_columna
Date     ld_fecha

SetNull(ll_null)
SetNull(li_null)

ls_columna = dwo.Name

CHOOSE CASE ls_columna
		
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(Data)) THEN
			This.SetItem(row, ls_Columna, li_Null)
		RETURN 1
		END IF	
		istr_mant.argumento[1]	=	Data
		
	CASE "plde_codigo"
		IF NoExistePlanta(Integer(Data)) THEN
			This.SetItem(row, ls_Columna, li_Null)
		RETURN 1
		END IF	
		istr_mant.argumento[2]	=	Data
		
	CASE "orpr_numero"
		IF iuo_ordenproceso.Existe(Integer(istr_mant.argumento[2]), 4, Integer(data), True, sqlca, Integer(istr_mant.argumento[1])) THEN
			istr_mant.argumento[3]	=	Data
			Parent.TriggerEvent("ue_recuperadatos")
		END IF
END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_mant_spro_ordenpesovaciado
integer x = 3131
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_mant_spro_ordenpesovaciado
integer x = 3131
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_mant_spro_ordenpesovaciado
integer x = 3131
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_mant_spro_ordenpesovaciado
integer x = 3131
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_mant_spro_ordenpesovaciado
integer x = 3131
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_mant_spro_ordenpesovaciado
boolean visible = false
integer x = 3136
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_mant_spro_ordenpesovaciado
integer x = 3136
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_mant_spro_ordenpesovaciado
integer x = 3131
boolean enabled = false
end type

type dw_3 from uo_dw within w_mant_spro_ordenpesovaciado
integer x = 1810
integer y = 552
integer width = 1216
integer height = 1196
integer taborder = 11
boolean bringtotop = true
boolean titlebar = true
string title = "Lotes Proceso"
string dataobject = "dw_mues_ordenproceso_pesaje"
boolean resizable = true
end type

event clicked;Long	ll_filas, ll_fila_ins, respuesta

This.SelectRow(0, FALSE)

IF Row = 0 THEN Return -1

//IF dw_1.Object.total_bultos[1]

Parent.TriggerEvent("ue_guardar")

This.SelectRow(Row, TRUE)
istr_mant.argumento[4]	=	String(This.Object.lote_pltcod[row])
istr_mant.argumento[5]	=	String(This.Object.lote_espcod[row])
istr_mant.argumento[6] 	= 	String(This.Object.lote_codigo[row])
istr_mant.argumento[7] 	= 	String(This.Object.enva_tipoen[row])
istr_mant.argumento[8] 	= 	String(This.Object.enva_codigo[row])

DO
	ll_filas	=	dw_1.Retrieve(Integer(istr_mant.Argumento[1]),&
									    	Integer(istr_mant.Argumento[2]),&
									    	Integer(istr_mant.Argumento[3]),&
									    	Integer(istr_mant.Argumento[4]),&
									 	Integer(istr_mant.Argumento[5]),&
									    	Integer(istr_mant.Argumento[6]))
											 
	IF ll_filas = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
	ELSE
		dw_1.GetChild("cale_calida", idwc_cosechero)
		idwc_cosechero.SetTRansObject(sqlca)
		idwc_cosechero.Retrieve(Integer(istr_mant.Argumento[7]), Integer(istr_mant.Argumento[8]))
//		
//		ll_fila_ins		=	dw_1.InsertRow(0)
//		
//		/*
//		inserta datos correspondientes al lote
//		*/
//		dw_1.Object.clie_codigo[ll_fila_ins] 		= 	Integer(istr_mant.Argumento[1])
//		dw_1.Object.plde_codigo[ll_fila_ins] 		= 	Integer(istr_mant.Argumento[2])
//		dw_1.Object.orpr_tipord[ll_fila_ins] 		= 	4
//		dw_1.Object.orpr_numero[ll_fila_ins] 	= 	Integer(istr_mant.Argumento[3])
//		dw_1.Object.lote_pltcod[ll_fila_ins] 		= 	Integer(istr_mant.Argumento[4])
//		dw_1.Object.lote_espcod[ll_fila_ins] 		= 	Integer(istr_mant.Argumento[5])
//		dw_1.Object.lote_codigo[ll_fila_ins] 		= 	Integer(istr_mant.Argumento[6])
//		dw_1.Object.enva_tipoen[ll_fila_ins] 		= 	Integer(istr_mant.Argumento[7])
//		dw_1.Object.enva_codigo[ll_fila_ins] 		= 	Integer(istr_mant.Argumento[8])
//		dw_1.SetRow(ll_fila_ins)
//		dw_1.ScrollToRow(ll_fila_ins)
//		dw_1.SelectRow(0, False)
//		dw_1.SetItemStatus(ll_fila_ins, 0, Primary!, NotModified!)
//		dw_1.SetFocus()
//		pb_eli_det.Enabled 	=	True
	END IF
	
LOOP WHILE respuesta = 1
IF respuesta = 2 THEN Close(Parent)
end event

event doubleclicked;call super::doubleclicked;Long	ll_filas, ll_fila_ins, respuesta

This.SelectRow(0, FALSE)

IF Row = 0 THEN Return -1

//IF dw_1.Object.total_bultos[1]

Parent.TriggerEvent("ue_guardar")

This.SelectRow(Row, TRUE)
istr_mant.argumento[4]	=	String(This.Object.lote_pltcod[row])
istr_mant.argumento[5]	=	String(This.Object.lote_espcod[row])
istr_mant.argumento[6] 	= 	String(This.Object.lote_codigo[row])
istr_mant.argumento[7] 	= 	String(This.Object.enva_tipoen[row])
istr_mant.argumento[8] 	= 	String(This.Object.enva_codigo[row])

DO
	ll_filas	=	dw_1.Retrieve(Integer(istr_mant.Argumento[1]),&
									    	Integer(istr_mant.Argumento[2]),&
									    	Integer(istr_mant.Argumento[3]),&
									    	Integer(istr_mant.Argumento[4]),&
									 	Integer(istr_mant.Argumento[5]),&
									    	Integer(istr_mant.Argumento[6]))
											 
	IF ll_filas = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
	ELSE
		dw_1.GetChild("cale_calida", idwc_cosechero)
		idwc_cosechero.SetTRansObject(sqlca)
		idwc_cosechero.Retrieve(Integer(istr_mant.Argumento[7]), Integer(istr_mant.Argumento[8]))
		
		ll_fila_ins		=	dw_1.InsertRow(0)
		
		/*
		inserta datos correspondientes al lote
		*/
		dw_1.Object.clie_codigo[ll_fila_ins] 		= 	Integer(istr_mant.Argumento[1])
		dw_1.Object.plde_codigo[ll_fila_ins] 		= 	Integer(istr_mant.Argumento[2])
		dw_1.Object.orpr_tipord[ll_fila_ins] 		= 	4
		dw_1.Object.orpr_numero[ll_fila_ins] 	= 	Integer(istr_mant.Argumento[3])
		dw_1.Object.lote_pltcod[ll_fila_ins] 		= 	Integer(istr_mant.Argumento[4])
		dw_1.Object.lote_espcod[ll_fila_ins] 		= 	Integer(istr_mant.Argumento[5])
		dw_1.Object.lote_codigo[ll_fila_ins] 		= 	Integer(istr_mant.Argumento[6])
		dw_1.Object.enva_tipoen[ll_fila_ins] 		= 	Integer(istr_mant.Argumento[7])
		dw_1.Object.enva_codigo[ll_fila_ins] 		= 	Integer(istr_mant.Argumento[8])
		dw_1.SetRow(ll_fila_ins)
		dw_1.ScrollToRow(ll_fila_ins)
		dw_1.SelectRow(0, False)
		dw_1.SetItemStatus(ll_fila_ins, 0, Primary!, NotModified!)
		dw_1.SetFocus()
		pb_eli_det.Enabled 	=	True
	END IF
	
LOOP WHILE respuesta = 1
IF respuesta = 2 THEN Close(Parent)
end event

