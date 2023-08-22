$PBExportHeader$w_mant_deta_calidadpackdetdanos.srw
$PBExportComments$Mantención Detalle de Daños y Defectos
forward
global type w_mant_deta_calidadpackdetdanos from w_mant_detalle_csd
end type
type dw_2 from uo_dw within w_mant_deta_calidadpackdetdanos
end type
type pb_ins_det from picturebutton within w_mant_deta_calidadpackdetdanos
end type
type pb_eli_det from picturebutton within w_mant_deta_calidadpackdetdanos
end type

end forward

global type w_mant_deta_calidadpackdetdanos from w_mant_detalle_csd
integer width = 2281
integer height = 2000
string title = "DETALLE DAÑOS Y DEFECTOS"
event ue_nuevo_detalle ( )
event ue_borra_detalle ( )
dw_2 dw_2
pb_ins_det pb_ins_det
pb_eli_det pb_eli_det
end type
global w_mant_deta_calidadpackdetdanos w_mant_deta_calidadpackdetdanos

type variables
DataWindowChild		idwc_Embalaje, idwc_Categoria, idwc_especie, idwc_danos, &
							idwc_Calibre
uo_DanosyDefectos		iuo_Danos
Integer					il_Fila_det
str_embalaje			istr_Embalaje
str_calibreenvase		istr_Calibre
end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
public function boolean duplicadocolumna (string as_valor)
end prototypes

event ue_nuevo_detalle();il_Fila_det = dw_2.InsertRow(0)

dw_2.ScrollToRow(il_Fila_det)
dw_2.SetRow(il_Fila_det)
dw_2.SetFocus()

dw_2.Object.plde_codigo[il_Fila_det]	=	Integer(istr_Mant.Argumento[1])
dw_2.Object.ccap_secuen[il_Fila_det] 	=  il_Fila

dw_2.SetColumn("dade_tipodd")
IF dw_2.RowCount() > 0 THEN
	pb_eli_det.Enabled = True
ELSE
	pb_eli_det.Enabled = False
END IF	
end event

event ue_borra_detalle();IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_2.RowCount() = 0 THEN
		pb_eli_det.Enabled = False
	ELSE
		il_fila = dw_2.GetRow()
	END IF
END IF
end event

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
String	ls_TipoDano, ls_CodDano

ls_TipoDano		=	String(dw_2.Object.dade_tipodd[il_Fila_det])
ls_CodDano		=	String(dw_2.Object.dade_codigo[il_Fila_det])

CHOOSE CASE as_Columna
	CASE "dade_tipodd"
		ls_TipoDano	=	as_Valor

	CASE "dade_codigo"
		ls_CodDano	=	as_Valor

END CHOOSE

ll_Fila	=	dw_2.Find("dade_tipodd = " + ls_TipoDano + " AND " + &
							"dade_codigo = " + ls_CodDano , &
							1, dw_2.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila_det THEN
	MessageBox("Error", "Registro ya fue ingresada anteriormente", Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean duplicadocolumna (string as_valor);Long		ll_Fila

ll_Fila	=	dw_1.Find("ccap_secuen = " + as_valor ,1, dw_1.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	MessageBox("Error", "Registro ya fue ingresada anteriormente", Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_deta_calidadpackdetdanos.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.pb_ins_det=create pb_ins_det
this.pb_eli_det=create pb_eli_det
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.pb_ins_det
this.Control[iCurrent+3]=this.pb_eli_det
end on

on w_mant_deta_calidadpackdetdanos.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.pb_ins_det)
destroy(this.pb_eli_det)
end on

event ue_recuperadatos();w_main.SetMicroHelp("Recuperando Datos...")

SetPointer(HourGlass!)
PostEvent("ue_listo")

IF istr_Mant.Agrega THEN
	pb_Cancela.Enabled	=	False
	pb_Primero.Enabled	=	False
	pb_Anterior.Enabled	=	False
	pb_Siguiente.Enabled	=	False
	pb_Ultimo.Enabled		=	False
	
	wf_nuevo()
	This.Title			= "INGRESO DE REGISTRO"
ELSE
	IF dw_1.RowCount() > 1 THEN
		pb_Primero.Enabled	=	True
		pb_Anterior.Enabled	=	True
		pb_Siguiente.Enabled	=	True
		pb_Ultimo.Enabled		=	True
	END IF
	
	il_Fila	=	istr_Mant.dw.GetRow()
	
	dw_1.SetRedraw(False)
	dw_1.SetRow(il_Fila)
	dw_1.ScrollToRow(il_Fila)
	dw_1.SetRedraw(True)

	IF istr_Mant.Borra THEN
		dw_1.Enabled			=	False
		dw_2.Enabled			=	False
		pb_Salir.Enabled		=	False
		pb_ins_det.Enabled	=	False
		pb_eli_det.Enabled	=	False
		This.Title			=	"ELIMINACION DE REGISTRO"
	ELSEIF istr_Mant.Solo_Consulta THEN
		dw_1.Enabled			=	False
		dw_2.Enabled			=	False
		pb_Acepta.Enabled		=	False
		pb_Cancela.Enabled	=	False
		pb_ins_det.Enabled	=	False
		pb_eli_det.Enabled	=	False
		This.Title				=	"CONSULTA DE REGISTRO"
	ELSE
		pb_Salir.Enabled	=	False
		This.Title			=	"MANTENCION DE REGISTRO"
	END IF
END IF

ias_Campo[1]	= String(dw_1.Object.cate_codigo[il_Fila])
ias_Campo[2]	= String(dw_1.Object.emba_codigo[il_Fila])
ias_Campo[3]	= dw_1.Object.ccpd_calibre[il_Fila]
ias_Campo[4]	= String(dw_1.Object.ccpd_pesnet[il_Fila])
ias_Campo[5]	= String(dw_1.Object.ccpd_pordes[il_Fila])
ias_Campo[6]	= String(dw_1.Object.ccpd_porplu[il_Fila])
ias_Campo[7]	= String(dw_1.Object.ccpd_dcacla[il_Fila])
ias_Campo[8]	= String(dw_1.Object.ccpd_dcasev[il_Fila])
ias_Campo[9]	= String(dw_1.Object.ccpd_dcocla[il_Fila])
ias_Campo[10]	= String(dw_1.Object.ccpd_dcosev[il_Fila])

IF istr_Mant.Agrega	THEN
	dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
	dw_1.Object.ccap_folio[il_Fila]	=	Long(istr_Mant.Argumento[2])	
	//dw_1.Object.ccap_secuen[il_Fila] =  il_Fila
ELSE
//	istr_Mant.Argumento[8]	=	String(dw_1.Object.enva_tipoen[il_Fila])
//	istr_Mant.Argumento[9]	=	String(dw_1.Object.enva_codigo[il_Fila])
	ExisteEmbalaje(dw_1.Object.emba_codigo[il_Fila], istr_Embalaje)
	
	istr_Mant.Argumento[8]	=	String(istr_Embalaje.TipoEnvase)
	istr_Mant.Argumento[9]	=	String(istr_Embalaje.envase)
	
	dw_1.GetChild("ccpd_calibre",idwc_Calibre)
	idwc_Calibre.SetTransObject(SQLCA)
	idwc_Calibre.Retrieve(Integer(istr_Mant.Argumento[3]), &
								 Integer(istr_Mant.Argumento[8]), &
								 Integer(istr_Mant.Argumento[9]))
END IF

dw_2.SetFilter("plde_codigo = " + istr_Mant.Argumento[1] + " and " + &
					"ccap_secuen = " + String(il_Fila))
dw_2.Filter()

IF istr_Mant.Solo_Consulta THEN
	dw_2.Enabled			=	False
	pb_ins_det.Enabled	=	False
	pb_eli_det.Enabled	=	False
END IF
end event

event ue_deshace();call super::ue_deshace;IF UpperBound(ias_Campo) > 0 THEN
	dw_1.Object.cate_codigo[il_Fila]		=	Integer(ias_Campo[1])
	dw_1.Object.emba_codigo[il_Fila]		=	ias_Campo[2]
	dw_1.Object.ccpd_calibre[il_Fila]	=	ias_Campo[3]
	dw_1.Object.ccpd_pesnet[il_Fila]		=	Dec(ias_Campo[4])
	dw_1.Object.ccpd_pordes[il_Fila]		=	Dec(ias_Campo[5])
	dw_1.Object.ccpd_porplu[il_Fila]		=	Dec(ias_Campo[6])
	dw_1.Object.ccpd_dcacla[il_Fila]		=	Integer(ias_Campo[7])
	dw_1.Object.ccpd_dcasev[il_Fila]		=	Integer(ias_Campo[8])
	dw_1.Object.ccpd_dcocla[il_Fila]		=	Integer(ias_Campo[9])
	dw_1.Object.ccpd_dcosev[il_Fila]		=	Integer(ias_Campo[10])
END IF

end event

event ue_antesguardar();Integer		li_cont
Long			ll_Fila
String		ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.cate_codigo[il_fila]) OR dw_1.Object.cate_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Categoria"
	ls_colu[li_cont]	= "cate_codigo"
END IF

IF Isnull(dw_1.Object.emba_codigo[il_fila]) OR dw_1.Object.emba_codigo[il_fila] = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Embalje"
	ls_colu[li_cont]	= "emba_codigo"
END IF

IF Isnull(dw_1.Object.ccpd_calibre[il_fila]) OR dw_1.Object.ccpd_calibre[il_fila] = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCalibre"
	ls_colu[li_cont]	= "ccpd_calibre"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
	RETURN
END IF

FOR ll_Fila	=	1 TO dw_2.RowCount()
	IF Isnull(dw_2.Object.dade_tipodd[ll_fila]) OR dw_2.Object.dade_tipodd[ll_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nTipo de Daño"
		ls_colu[li_cont]	= "dade_tipodd"
	END IF

	IF Isnull(dw_2.Object.dade_codigo[ll_fila]) OR dw_2.Object.dade_codigo[ll_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo de Daño"
		ls_colu[li_cont]	= "dade_codigo"
	END IF

	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + &
						" en la Línea de Detalle Nro."+String(ll_Fila), StopSign!, Ok!)
		dw_2.SetRow(ll_Fila)						
		dw_2.SetColumn(ls_colu[1])
		dw_2.SetFocus()
		Message.DoubleParm = -1
		EXIT
	END IF
NEXT

FOR ll_Fila	=	1 TO dw_2.RowCount()
	dw_2.Object.ccap_secuen[ll_Fila]	=	dw_1.Object.ccap_secuen[il_Fila]
	dw_2.Object.dade_secuen[ll_Fila]	=	0
NEXT

end event

event ue_nuevo();call super::ue_nuevo;IF ib_ok = False THEN RETURN

Integer	li_Secuencia

dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
//li_Secuencia							=	il_Fila + 1
//dw_1.Object.ccap_secuen[il_Fila] =  li_Secuencia

dw_2.SetRedraw(False)

dw_2.SetFilter("plde_codigo = " + istr_Mant.Argumento[1] + " AND " + &
					"ccap_secuen = " + String(li_Secuencia))

dw_2.Filter()
dw_2.SetRedraw(True)

end event

event open;/* Argumentos
istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Numero de Folio
istr_Mant.Argumento[3]	=	Código Especie
istr_Mant.Argumento[4]	=	Tipo de Orden
istr_Mant.Argumento[5]	=	Numero de Orden
istr_Mant.Argumento[6]	=	Código Variedad Real
istr_Mant.Argumento[7]	=	Código Variedad Rotulada
istr_Mant.Argumento[8]	=	Tipo de Envase
istr_Mant.Argumento[9]	=	Código de Envase
istr_Mant.Argumento[10]	=	Código de Tipo de Daño
*/

x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

DataWindowChild	ldwc_tipo

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.GetChild("cate_codigo",idwc_categoria)
idwc_categoria.SetTransObject(SQLCA)
idwc_categoria.Retrieve()
idwc_categoria.SetSort("cate_nombre A")

dw_1.GetChild("emba_codigo",idwc_embalaje)
idwc_embalaje.SetTransObject(SQLCA)
idwc_embalaje.Retrieve(0,0)

dw_1.GetChild("ccpd_calibre",idwc_Calibre)
idwc_Calibre.SetTransObject(SQLCA)
idwc_Calibre.Retrieve(Integer(istr_Mant.Argumento[3]),0,0)

dw_2.GetChild("dade_codigo",idwc_danos)
idwc_danos.SetTransObject(SQLCA)
idwc_danos.Retrieve(Integer(istr_Mant.Argumento[3]),0)
idwc_danos.InsertRow(0)

dw_1.SetTransObject(Sqlca)
istr_mant.dw.ShareData(dw_1)

dw_2.Modify("datawindow.message.title='Error '+ is_titulo")
dw_2.SetRowFocusIndicator(Hand!)
dw_2.Modify("DataWindow.Footer.Height = 84")

dw_2.SetTransObject(Sqlca)
istr_mant.dw2.ShareData(dw_2)

iuo_Danos		=	Create uo_DanosyDefectos
end event

event resize;//
end event

event closequery;IF Not istr_mant.Borra THEN

	IF istr_mant.Agrega AND istr_mant.Respuesta <> 1 THEN 
		dw_1.DeleteRow(il_fila)
		dw_2.RowsMove(1,dw_2.RowCount(),Primary!,dw_2,1,Delete!)
		
		IF dw_1.RowCount() > 0 THEN
			dw_1.SelectRow(0, False)
			dw_1.SelectRow(dw_1.RowCount(), True)
		END IF
		
		RETURN
	END IF

	IF ib_Modifica AND istr_mant.Respuesta = 1 THEN
		This.TriggerEvent("ue_guardar")
		
		IF Message.DoubleParm = -1 THEN Message.ReturnValue = 1
		
		RETURN
	ELSEIF istr_mant.Respuesta = 2 THEN
		This.TriggerEvent("ue_deshace")
	END IF
END IF
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_calidadpackdetdanos
boolean visible = false
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_calidadpackdetdanos
boolean visible = false
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_calidadpackdetdanos
boolean visible = false
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_calidadpackdetdanos
boolean visible = false
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_calidadpackdetdanos
integer x = 1979
integer y = 268
integer taborder = 50
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_calidadpackdetdanos
integer x = 1979
integer y = 92
integer taborder = 40
boolean default = false
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_calidadpackdetdanos
integer x = 1979
integer y = 440
integer taborder = 60
end type

event pb_salir::clicked;IF istr_mant.Agrega THEN 
	//Descuenta último Lote generado
	istr_Mant.Argumento[7]	=	String(Integer(istr_Mant.Argumento[7])-1)
END IF

CALL SUPER::Clicked
end event

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_calidadpackdetdanos
integer x = 55
integer y = 36
integer width = 1797
integer height = 860
string dataobject = "dw_mant_spro_ccalidadpackdetcate"
end type

event dw_1::itemchanged;String  	ls_columna, ls_Null

SetNull(ls_Null)

ls_columna = dwo.Name

CHOOSE CASE ls_columna
		
	CASE "ccap_secuen"	
		
		IF DuplicadoColumna(data) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF			

	CASE "emba_codigo"
		IF NOT ExisteEmbalaje(Data, istr_Embalaje) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Null))
			RETURN 1
		ELSE
			pb_ins_det.Enabled = True
			
			This.SetItem(il_Fila, "ccpd_calibre", ls_Null)
			istr_Mant.Argumento[8]	=	String(istr_Embalaje.TipoEnvase)
			istr_Mant.Argumento[9]	=	String(istr_Embalaje.envase)
			dw_1.GetChild("ccpd_calibre",idwc_Calibre)
			idwc_Calibre.SetTransObject(SQLCA)
			idwc_Calibre.Retrieve(Integer(istr_Mant.Argumento[3]), &
										 Integer(istr_Mant.Argumento[8]), &
										 Integer(istr_Mant.Argumento[9]))
		END IF

	CASE "ccpd_calibre"
		IF NOT ExisteCalibre(Integer(istr_Mant.Argumento[3]), &
									Integer(istr_Mant.Argumento[8]), &
									Integer(istr_Mant.Argumento[9]), &
									Data, istr_Calibre) THEN
			This.SetItem(il_Fila, ls_Columna, ls_Null)
			RETURN 1
		END IF

END CHOOSE
end event

type dw_2 from uo_dw within w_mant_deta_calidadpackdetdanos
integer x = 201
integer y = 960
integer width = 1486
integer height = 852
integer taborder = 20
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle de Daños y Defectos"
string dataobject = "dw_mues_spro_ccalidadpackdetdanos"
boolean hscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila_det = Row
END IF

RETURN 0
end event

event itemerror;call super::itemerror;RETURN 1
end event

event losefocus;call super::losefocus;Accepttext()
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila_det = CurrentRow
END IF
end event

event itemchanged;String  ls_Columna, ls_Nula

SetNull(ls_Nula)

ls_Columna = dwo.Name

ib_modifica = True

CHOOSE CASE ls_Columna

	CASE "dade_tipodd"
		IF Duplicado(ls_Columna, Data) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))

			RETURN 1
		ELSE
			istr_Mant.Argumento[10]	=	Data
			dw_2.GetChild("dade_codigo",idwc_danos)
			idwc_danos.SetTransObject(SQLCA)
			idwc_danos.Retrieve(Integer(istr_Mant.Argumento[3]), &
									  Integer(istr_Mant.Argumento[10]))
			idwc_danos.SetSort("dade_nombre A")
		END IF

	CASE "dade_codigo"
		IF NOT iuo_Danos.Existe(Integer(istr_Mant.Argumento[3]), &
										Integer(istr_Mant.Argumento[7]), &
										Integer(istr_Mant.Argumento[10]), &
										Integer(Data), True, SqlCa) OR &
										Duplicado(ls_Columna, Data) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))

			RETURN 1
			
		END IF

END CHOOSE
end event

type pb_ins_det from picturebutton within w_mant_deta_calidadpackdetdanos
integer x = 1979
integer y = 1248
integer width = 155
integer height = 132
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\desarrollo\bmp\inserte.bmp"
string disabledname = "\desarrollo\bmp\insertd.bmp"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_nuevo_detalle")
end event

type pb_eli_det from picturebutton within w_mant_deta_calidadpackdetdanos
integer x = 1979
integer y = 1428
integer width = 155
integer height = 132
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
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

