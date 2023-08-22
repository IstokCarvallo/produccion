$PBExportHeader$uo_seleccion_prodestadocert_mod.sru
$PBExportComments$Objeto público para selección de estado de Certificación, Todos o Consolidado
forward
global type uo_seleccion_prodestadocert_mod from userobject
end type
type cbx_consolida from checkbox within uo_seleccion_prodestadocert_mod
end type
type cbx_todos from checkbox within uo_seleccion_prodestadocert_mod
end type
type dw_seleccion from datawindow within uo_seleccion_prodestadocert_mod
end type
end forward

global type uo_seleccion_prodestadocert_mod from userobject
integer width = 1253
integer height = 132
long backcolor = 33543637
string text = "none"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
event ue_cambio ( )
cbx_consolida cbx_consolida
cbx_todos cbx_todos
dw_seleccion dw_seleccion
end type
global uo_seleccion_prodestadocert_mod uo_seleccion_prodestadocert_mod

type variables
DataWindowChild	idwc_Seleccion

uo_prodestadocert	iuo_prodestadocert	

Integer		Codigo
String		Nombre 
end variables

forward prototypes
public subroutine seleccion (boolean ab_todos, boolean ab_consolida)
public subroutine todos (boolean ab_todos)
public subroutine limpiardatos ()
end prototypes

public subroutine seleccion (boolean ab_todos, boolean ab_consolida);cbx_Todos.Visible			=	ab_Todos
cbx_Consolida.Visible	=	ab_Consolida
 
IF Not ab_Todos AND Not ab_Consolida THEN
	dw_Seleccion.y			=	0
	dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
ELSE
	dw_Seleccion.y			=	20
	dw_Seleccion.Object.codigo.BackGround.Color	=	553648127
END IF

RETURN
end subroutine

public subroutine todos (boolean ab_todos);IF ab_Todos THEN
	Codigo						=	-1
	Nombre						=	'Todos'
	
	dw_Seleccion.Reset()
	cbx_consolida.Checked = False

	dw_Seleccion.InsertRow(0)
ELSE
	SetNull(Codigo)
	SetNull(Nombre)
	dw_seleccion.SetFocus()	
	dw_Seleccion.Object.codigo[1]						=	Codigo
END IF

RETURN
end subroutine

public subroutine limpiardatos ();String	ls_Nula

SetNull(ls_Nula)

dw_Seleccion.SetItem(1, "codigo", Long(ls_Nula))
Codigo = Long(ls_Nula)
end subroutine

on uo_seleccion_prodestadocert_mod.create
this.cbx_consolida=create cbx_consolida
this.cbx_todos=create cbx_todos
this.dw_seleccion=create dw_seleccion
this.Control[]={this.cbx_consolida,&
this.cbx_todos,&
this.dw_seleccion}
end on

on uo_seleccion_prodestadocert_mod.destroy
destroy(this.cbx_consolida)
destroy(this.cbx_todos)
destroy(this.dw_seleccion)
end on

event constructor;dw_Seleccion.Object.Codigo.Dddw.Name			=	'dw_mues_prodestadocert'
dw_Seleccion.Object.codigo.Dddw.DisplayColumn	=	'prec_nombre'
dw_Seleccion.Object.codigo.Dddw.DataColumn		=	'prec_codigo'

dw_Seleccion.GetChild("codigo", idwc_Seleccion)

idwc_Seleccion.SetTransObject(sqlca)

IF	idwc_Seleccion.Retrieve() = 0 THEN
	MessageBox("Atención", "No existen Estados en tabla respectiva")
	
	SetNull(Codigo)
	SetNull(Nombre)
ELSE
	idwc_Seleccion.SetSort("prec_codigo A")
	idwc_Seleccion.Sort()
	
	dw_Seleccion.Object.codigo.Font.Height	=	'-8'
	dw_Seleccion.Object.codigo.Height		=	64
	
	dw_Seleccion.SetTransObject(sqlca)
	dw_Seleccion.InsertRow(0)
	
	Codigo			=	-1
	Nombre			=	'Todas'
	iuo_prodestadocert	=	Create uo_prodestadocert	
	
	This.Seleccion(True, True)
END IF

end event

type cbx_consolida from checkbox within uo_seleccion_prodestadocert_mod
integer x = 1120
integer y = 24
integer width = 155
integer height = 80
integer taborder = 20
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = " "
end type

event clicked;IF This.Checked THEN
	Codigo	=	-9
	Nombre	=	'Consolidada'
	cbx_todos.Checked = False
	dw_Seleccion.Reset()
	dw_Seleccion.InsertRow(0)
ELSE
	Codigo	=	-1
	Nombre	=	'Todas'
	dw_seleccion.SetFocus()
END IF

Parent.TriggerEvent("ue_cambio")
end event

type cbx_todos from checkbox within uo_seleccion_prodestadocert_mod
integer x = 919
integer y = 24
integer width = 151
integer height = 80
integer taborder = 10
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = " "
boolean checked = true
end type

event clicked;Todos(This.Checked)
cbx_consolida.Checked = False

Parent.TriggerEvent("ue_cambio")
end event

type dw_seleccion from datawindow within uo_seleccion_prodestadocert_mod
integer y = 20
integer width = 882
integer height = 92
integer taborder = 30
string title = "none"
string dataobject = "dddw_codnumero"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Nula

SetNull(li_Nula)

dw_seleccion.PostEvent(Clicked!)

IF iuo_prodestadocert.Existe(Integer(data), True, sqlca) THEN
	Codigo	=	iuo_prodestadocert.Codigo
	Nombre	=	iuo_prodestadocert.Nombre
ELSE
	This.SetItem(1, "codigo", li_Nula)

	RETURN 1
END IF

Parent.TriggerEvent("ue_cambio")
end event

event itemerror;RETURN 1
end event

event clicked;cbx_todos.Checked = False
cbx_consolida.Checked = False
end event

