$PBExportHeader$w_mant_mues_cajasembavalfaenas.srw
$PBExportComments$Mantenedor de Agrónomos.
forward
global type w_mant_mues_cajasembavalfaenas from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_cajasembavalfaenas
end type
type st_4 from statictext within w_mant_mues_cajasembavalfaenas
end type
type dw_entidad from datawindow within w_mant_mues_cajasembavalfaenas
end type
type em_fecha from editmask within w_mant_mues_cajasembavalfaenas
end type
type cbx_todos from checkbox within w_mant_mues_cajasembavalfaenas
end type
type st_2 from statictext within w_mant_mues_cajasembavalfaenas
end type
type dw_conexi from datawindow within w_mant_mues_cajasembavalfaenas
end type
type sle_rut from singlelineedit within w_mant_mues_cajasembavalfaenas
end type
type cb_buscar from commandbutton within w_mant_mues_cajasembavalfaenas
end type
type sle_nombre from singlelineedit within w_mant_mues_cajasembavalfaenas
end type
type cbx_mescompleto from checkbox within w_mant_mues_cajasembavalfaenas
end type
end forward

global type w_mant_mues_cajasembavalfaenas from w_mant_tabla
integer width = 5285
string title = "MANTENCIÓN DE FAENAS VALORIZADAS"
st_1 st_1
st_4 st_4
dw_entidad dw_entidad
em_fecha em_fecha
cbx_todos cbx_todos
st_2 st_2
dw_conexi dw_conexi
sle_rut sle_rut
cb_buscar cb_buscar
sle_nombre sle_nombre
cbx_mescompleto cbx_mescompleto
end type
global w_mant_mues_cajasembavalfaenas w_mant_mues_cajasembavalfaenas

type variables
DataWindowChild	idwc_entidad
Transaction			it_sqlremu

Integer				ii_tipo, ii_orden, ii_mescompleto
String				is_rut
Decimal				id_porcafp
Long					il_nrotar

uo_conectividad	iuo_conexion

w_mant_deta_cajasembavalfaenas	iw_mantencion
end variables

forward prototypes
public function integer existeentidad (string as_rut)
end prototypes

public function integer existeentidad (string as_rut);String 	ls_rut, ls_nombre, ls_apepat, ls_apemat

select 	pers_rutemp, pers_nombre, pers_apepat, pers_apemat, pers_nrotar
into 		:ls_rut, 	 :ls_nombre,  :ls_apepat,  :ls_apemat,  :il_nrotar
from 		dbo.remupersonal
where	pers_rutemp = :as_rut;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla remupersonal" )
	RETURN -1
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "El Rut ingresado no pertenece a un trabajador de Rio Blanco." + &
								"~r~nIngrese o seleccione otro", StopSign!)
	Return -1
END IF


sle_nombre.Text	=	ls_apepat + ' ' + ls_apemat + ' ' + ls_nombre

RETURN 1
end function

event open;call super::open;/*
istr_mant.argumento[1]	=	Mes proceso
istr_mant.argumento[2]	=	Empleado
istr_mant.argumento[3]	=	Conexion
*/
iuo_conexion				=	Create uo_conectividad

dw_entidad.SetTransObject(sqlca)
dw_entidad.GetChild("zona_codigo", idwc_entidad)
idwc_entidad.SetTransObject(sqlca)
idwc_entidad.Retrieve()
dw_entidad.InsertRow(0)

dw_conexi.SetTransObject(sqlca)
dw_conexi.InsertRow(0)

istr_mant.dw				= 	dw_1

em_fecha.Text				=	String(today(), 'dd/mm/yyyy')
istr_mant.argumento[1]	=	em_fecha.Text
is_rut						=	'-1'
istr_mant.argumento[2]	=	'-1'

buscar						= 	"Tipo Faena:Nfaen_codigo,Codigo Persona:Npers_codigo,Embalaje:Semba_codigo"
ordenar						= 	"Tipo Faena:faen_codigo,Codigo Persona:pers_codigo,Embalaje:emba_codigo"

cbx_todos.TriggerEvent(clicked!)
cbx_mescompleto.TriggerEvent(clicked!)
end event

event ue_recuperadatos;Long		ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(date(istr_mant.argumento[1]), is_rut, ii_mescompleto)

	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN

		dw_1.SetFocus()
		il_fila					= 1
		pb_imprimir.Enabled	= True
	
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	END IF
	
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_mant_mues_cajasembavalfaenas.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_4=create st_4
this.dw_entidad=create dw_entidad
this.em_fecha=create em_fecha
this.cbx_todos=create cbx_todos
this.st_2=create st_2
this.dw_conexi=create dw_conexi
this.sle_rut=create sle_rut
this.cb_buscar=create cb_buscar
this.sle_nombre=create sle_nombre
this.cbx_mescompleto=create cbx_mescompleto
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.dw_entidad
this.Control[iCurrent+4]=this.em_fecha
this.Control[iCurrent+5]=this.cbx_todos
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.dw_conexi
this.Control[iCurrent+8]=this.sle_rut
this.Control[iCurrent+9]=this.cb_buscar
this.Control[iCurrent+10]=this.sle_nombre
this.Control[iCurrent+11]=this.cbx_mescompleto
end on

on w_mant_mues_cajasembavalfaenas.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_4)
destroy(this.dw_entidad)
destroy(this.em_fecha)
destroy(this.cbx_todos)
destroy(this.st_2)
destroy(this.dw_conexi)
destroy(this.sle_rut)
destroy(this.cb_buscar)
destroy(this.sle_nombre)
destroy(this.cbx_mescompleto)
end on

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_borrar;call super::ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra		= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)  // La Ventana iw_mantencion debe ser una Variable de Instancia del Tipo de
													 // ventana del mantenedor de la tabla.

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

 IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
		dw_1.SelectRow(il_fila,True)
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "MANTENCIÓN DE FAENAS VALORIZADAS"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_valorizacion_faenas"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(date(istr_mant.argumento[1]), is_rut, ii_mescompleto)

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

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_cajasembavalfaenas
integer x = 82
integer y = 404
integer width = 4635
integer height = 1436
integer taborder = 60
string dataobject = "dw_mues_cajasembavalfaenas"
boolean hscrollbar = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_cajasembavalfaenas
integer x = 82
integer y = 60
integer width = 4635
integer height = 316
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_cajasembavalfaenas
integer x = 4754
integer y = 148
integer taborder = 50
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_cajasembavalfaenas
integer x = 4754
integer y = 448
integer taborder = 70
end type

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_cajasembavalfaenas
boolean visible = false
integer x = 4850
integer y = 1292
integer taborder = 80
string picturename = "\Desarrollo\Bmp\ACEPTAE.BMP"
string disabledname = "\Desarrollo\Bmp\ACEPTAd.BMP"
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_cajasembavalfaenas
integer x = 4754
integer y = 800
integer taborder = 90
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_cajasembavalfaenas
integer x = 4754
integer y = 980
integer taborder = 100
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_cajasembavalfaenas
integer x = 4754
integer y = 1156
integer taborder = 110
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_cajasembavalfaenas
integer x = 4754
integer y = 1544
integer taborder = 120
end type

type st_1 from statictext within w_mant_mues_cajasembavalfaenas
integer x = 1339
integer y = 136
integer width = 443
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Fecha Proceso"
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_cajasembavalfaenas
integer x = 1339
integer y = 244
integer width = 233
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Entidad"
boolean focusrectangle = false
end type

type dw_entidad from datawindow within w_mant_mues_cajasembavalfaenas
boolean visible = false
integer x = 123
integer y = 1716
integer width = 905
integer height = 112
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_entidadespacking"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null
String		ls_columna

SetNull(ll_null)

ls_columna =	Dwo.Name

CHOOSE CASE ls_columna
	CASE "enpa_rutper"
		is_rut	=	F_verrut(data, True)
		IF is_rut	=	""	THEN
			This.SetItem(row,ls_columna,String(ll_null))
			Return 1
		ELSE
			istr_mant.argumento[2]	=	data
		END IF
			
END CHOOSE
end event

type em_fecha from editmask within w_mant_mues_cajasembavalfaenas
integer x = 1815
integer y = 120
integer width = 498
integer height = 88
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
long calendartextcolor = 0
long calendartitletextcolor = 0
long calendartrailingtextcolor = 8421504
long calendarbackcolor = 12632256
long calendartitlebackcolor = 10789024
end type

event modified;istr_mant.argumento[1]	=	This.Text
end event

type cbx_todos from checkbox within w_mant_mues_cajasembavalfaenas
integer x = 3493
integer y = 240
integer width = 274
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF this.checked THEN
	cb_buscar.Enabled		=	False
	sle_rut.Enabled		=	False
	Is_Rut					=	'-1'
ELSE
	cb_buscar.Enabled		=	True
	sle_rut.Enabled		=	True
	Is_Rut					=	''
END IF

sle_nombre.Text	=	''
sle_rut.Text		=	''
istr_mant.argumento[2]	=	''
end event

type st_2 from statictext within w_mant_mues_cajasembavalfaenas
boolean visible = false
integer x = 3099
integer y = 1916
integer width = 293
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Conexión"
boolean focusrectangle = false
end type

type dw_conexi from datawindow within w_mant_mues_cajasembavalfaenas
boolean visible = false
integer x = 3483
integer y = 1888
integer width = 1248
integer height = 112
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_conectividad"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null
String		ls_columna

SetNull(ll_null)

ls_columna =	Dwo.Name

CHOOSE CASE ls_columna
	CASE "cone_codigo"
		IF NOT iuo_conexion.Existe(Long(data), sqlca, True)THEN
			This.Object.cone_codigo[1]	=	ll_null
			Return 1
		ELSE
			istr_mant.argumento[3]	=	data
		END IF
			
END CHOOSE
end event

event itemerror;Return 1
end event

type sle_rut from singlelineedit within w_mant_mues_cajasembavalfaenas
integer x = 1815
integer y = 236
integer width = 498
integer height = 88
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

event modified;Integer	li_validarut

IF Len(This.Text) < 1 THEN Return

is_rut	=	F_verrut(This.Text, True)
IF is_rut	=	""	THEN
	This.Text					=	''
	istr_mant.argumento[2]	=	''
	Return
	
END IF

li_validarut = ExisteEntidad(This.Text) 
IF li_validarut = -1 THEN
	This.Text					=	''
	istr_mant.argumento[2]	=	''
	Return
	
END IF

This.Text	=	is_rut
end event

type cb_buscar from commandbutton within w_mant_mues_cajasembavalfaenas
integer x = 2327
integer y = 236
integer width = 101
integer height = 88
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;Integer 			li_null, li_validarut
String			ls_columna
Str_busqueda	lstr_busq

SetNull(li_null)

lstr_busq.argum[1]	=	''

OpenWithParm(w_busc_remupersonal, lstr_busq)
lstr_busq				=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> '' THEN
	li_validarut = ExisteEntidad(lstr_busq.argum[1]) 
	IF li_validarut = -1 THEN
		sle_nombre.Text	=	String(li_null)
		sle_rut.Text		=	String(li_null)
		Return 1
	ELSE
		sle_rut.Text		=	lstr_busq.argum[1]
		is_rut				=	lstr_busq.argum[1]
	END IF
END IF
end event

type sle_nombre from singlelineedit within w_mant_mues_cajasembavalfaenas
integer x = 2437
integer y = 236
integer width = 997
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type cbx_mescompleto from checkbox within w_mant_mues_cajasembavalfaenas
integer x = 2386
integer y = 124
integer width = 512
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Mes Completo"
boolean checked = true
end type

event clicked;IF this.checked THEN
	ii_mescompleto		=	1
	em_fecha.Enabled	=	False
ELSE
	ii_mescompleto		=	0
	em_fecha.Enabled	=	True
END IF

end event

