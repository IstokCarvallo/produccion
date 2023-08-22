$PBExportHeader$w_busc_embalajes_cliente.srw
forward
global type w_busc_embalajes_cliente from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type ddlb_especie from dropdownlistbox within tabpage_1
end type
type st_argum3 from statictext within tabpage_3
end type
type sle_argumento3 from singlelineedit within tabpage_3
end type
end forward

global type w_busc_embalajes_cliente from w_busqueda
integer x = 123
integer y = 304
integer width = 3081
string title = "Búsqueda de Embalajes"
end type
global w_busc_embalajes_cliente w_busc_embalajes_cliente

type variables
w_mant_deta_embalajes iw_mantencion

String	is_especie
end variables

forward prototypes
public function integer pueblaespecies ()
end prototypes

public function integer pueblaespecies ();String	ls_add_string, ls_select_string, ls_nombre
Integer	li_pos

ls_select_string = 'SELECT DISTINCT Substr(emba_codigo, 1, 2) FROM  dba.embalajes'

PREPARE sqlsa FROM :ls_select_string;

DECLARE dyn_cursor DYNAMIC CURSOR FOR sqlsa;

OPEN DYNAMIC dyn_cursor;  

IF sqlca.sqlcode < 0 THEN
	MessageBox("Error en Lista!", sqlca.sqlerrtext)
	
	RETURN sqlca.SQLCode
END IF

tab_1.tabpage_1.ddlb_especie.SetRedraw(False)
tab_1.tabpage_1.ddlb_especie.Reset()

DO WHILE sqlca.sqlcode = 0
	FETCH dyn_cursor INTO :ls_nombre ;
	
	IF sqlca.sqlcode = 0 THEN
		ls_add_string = ls_nombre
		tab_1.tabpage_1.ddlb_especie.AddItem(ls_add_string)
	ELSE
		EXIT
	END IF
LOOP

tab_1.tabpage_1.ddlb_especie.SetRedraw(True)

CLOSE dyn_cursor;

RETURN 0
end function

on w_busc_embalajes_cliente.create
int iCurrent
call super::create
end on

on w_busc_embalajes_cliente.destroy
call super::destroy
end on

event open;call super::open;istr_busq.argum[1]	= ""
istr_busq.argum[2]	= ""
istr_busq.argum[3]	= ""
istr_busq.argum[4]	= ""

is_ordena = 'Código Embalaje:emba_codigo,Nombre:emba_nombre,Abreviación:emba_abrevi'

PueblaEspecies()
end event

event ue_asignacion;istr_busq.argum[1]	= dw_1.GetItemString(dw_1.GetRow(),"emba_codigo")
istr_busq.argum[2]	= dw_1.GetItemString(dw_1.GetRow(),"emba_nombre")
istr_busq.argum[3]	= dw_1.GetItemString(dw_1.GetRow(),"emba_abrevi")
//istr_busq.argum[4]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"emba_cajpal"))

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_embalajes_cliente
boolean visible = false
integer x = 2747
integer y = 1116
integer taborder = 50
end type

event pb_insertar::clicked;call super::clicked;istr_mant.argumento[1]	= istr_busq.argum[4]
istr_mant.argumento[2]	= istr_busq.argum[5]

OpenWithParm(iw_mantencion, istr_mant)

Parent.TriggerEvent("ue_grabacion")
end event

type dw_1 from w_busqueda`dw_1 within w_busc_embalajes_cliente
integer y = 756
integer width = 2523
integer taborder = 60
string dataobject = "dw_mues_embalajes_3"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_embalajes_cliente
integer x = 2747
integer y = 1436
end type

type tab_1 from w_busqueda`tab_1 within w_busc_embalajes_cliente
integer x = 91
integer y = 48
integer width = 2523
integer height = 596
boolean fixedwidth = true
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
event create ( )
event destroy ( )
integer width = 2487
integer height = 468
string text = "Filtros                 "
st_1 st_1
ddlb_especie ddlb_especie
end type

on tabpage_1.create
this.st_1=create st_1
this.ddlb_especie=create ddlb_especie
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.ddlb_especie
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.ddlb_especie)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2309
integer taborder = 30
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;IF dw_1.Retrieve(is_especie) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2487
integer height = 468
string text = "Ordenamiento                   "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 2295
integer y = 312
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2487
integer height = 468
string text = "Búsqueda         "
st_argum3 st_argum3
sle_argumento3 sle_argumento3
end type

on tabpage_3.create
this.st_argum3=create st_argum3
this.sle_argumento3=create sle_argumento3
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_argum3
this.Control[iCurrent+2]=this.sle_argumento3
end on

on tabpage_3.destroy
call super::destroy
destroy(this.st_argum3)
destroy(this.sle_argumento3)
end on

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 640
integer y = 212
end type

event sle_argumento2::getfocus;This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
sle_argumento3.Text			= ""
sle_argumento3.BackColor	= RGB(192,192,192)
sle_argumento3.TabOrder		= 0
es_numero						= False
is_busca							= "emba_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer y = 220
string text = "Nombre"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 640
integer y = 92
integer width = 233
end type

event sle_argumento1::getfocus;This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
sle_argumento3.Text			= ""
sle_argumento3.BackColor	= RGB(192,192,192)
sle_argumento3.TabOrder		= 0
es_numero						= True
is_busca							= "emba_codigo"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer y = 104
integer width = 512
string text = "Código Embalaje"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 2295
integer y = 308
end type

type st_1 from statictext within tabpage_1
integer x = 110
integer y = 204
integer width = 832
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Identificación de la Especie"
boolean focusrectangle = false
end type

type ddlb_especie from dropdownlistbox within tabpage_1
integer x = 983
integer y = 200
integer width = 768
integer height = 372
integer taborder = 16
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean vscrollbar = true
end type

event selectionchanged;is_especie = Mid(This.Text(Index), 1, 2)
end event

type st_argum3 from statictext within tabpage_3
integer x = 46
integer y = 340
integer width = 384
integer height = 76
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Abreviación"
boolean focusrectangle = false
end type

type sle_argumento3 from singlelineedit within tabpage_3
integer x = 640
integer y = 332
integer width = 494
integer height = 92
integer taborder = 21
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

event getfocus;This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
es_numero						= False
is_busca							= "emba_abrevi"
end event

event modified;is_argume	= This.Text
end event

