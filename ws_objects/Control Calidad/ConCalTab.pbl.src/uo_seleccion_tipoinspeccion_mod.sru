$PBExportHeader$uo_seleccion_tipoinspeccion_mod.sru
$PBExportComments$Objeto público para selección de Tipo Inspeccion, Todos o Consolidado
forward
global type uo_seleccion_tipoinspeccion_mod from userobject
end type
type cbx_consolida from checkbox within uo_seleccion_tipoinspeccion_mod
end type
type cbx_todos from checkbox within uo_seleccion_tipoinspeccion_mod
end type
type dw_seleccion from datawindow within uo_seleccion_tipoinspeccion_mod
end type
end forward

global type uo_seleccion_tipoinspeccion_mod from userobject
integer width = 1275
integer height = 128
long backcolor = 553648127
string text = "none"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
event ue_cambio ( )
cbx_consolida cbx_consolida
cbx_todos cbx_todos
dw_seleccion dw_seleccion
end type
global uo_seleccion_tipoinspeccion_mod uo_seleccion_tipoinspeccion_mod

type variables
DataWindowChild	idwc_Seleccion

uo_ctlcaltipoinspec		iuo_TipoInspeccion

Integer					Codigo
String						Nombre 
end variables

forward prototypes
public subroutine seleccion (boolean ab_todos, boolean ab_consolida)
public subroutine todos (boolean ab_todos)
public subroutine bloquear (boolean ab_opcion)
public subroutine limpiardatos ()
end prototypes

public subroutine seleccion (boolean ab_todos, boolean ab_consolida);cbx_Todos.Visible			=	ab_Todos
cbx_Consolida.Visible	=	ab_Consolida

IF Not ab_Todos AND Not ab_Consolida THEN
	dw_Seleccion.y			=	0
	//dw_Seleccion.Enabled	=	True
	
	//dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
ELSE
	dw_Seleccion.y			=	20
	//dw_Seleccion.Enabled	=	False
	
	//dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(192, 192, 192)
END IF

RETURN
end subroutine

public subroutine todos (boolean ab_todos);IF ab_Todos THEN
	Codigo						=	-1
	Nombre						=	'Todos'
	//cbx_Todos.Checked			=	True
	//cbx_Consolida.Enabled	=	True
	//dw_Seleccion.Enabled		=	False
	
	//dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(192, 192, 192)
	
	dw_Seleccion.Reset()
	//cbx_consolida.Checked = False
	dw_Seleccion.InsertRow(0)
ELSE
	SetNull(Codigo)
	SetNull(Nombre)
	
	//cbx_Todos.Checked			=	False
	//cbx_Consolida.Checked	=	False
	//cbx_Consolida.Enabled	=	False
	//dw_Seleccion.Enabled		=	True
	
	dw_Seleccion.Object.codigo[1]						=	Codigo
	//dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_Seleccion.SetFocus()
END IF

RETURN
end subroutine

public subroutine bloquear (boolean ab_opcion);If ab_opcion Then
	dw_Seleccion.Enabled	= False
	dw_Seleccion.Object.Codigo.BackGround.Color	=	RGB(255, 255, 255)
Else
	dw_Seleccion.Enabled	= True
	dw_Seleccion.Object.Codigo.BackGround.Color	=	553648127
End If

Return
end subroutine

public subroutine limpiardatos ();String	ls_Nula

SetNull(ls_Nula)

dw_Seleccion.SetItem(1, "codigo", Long(ls_Nula))
Codigo = Long(ls_Nula)
end subroutine

on uo_seleccion_tipoinspeccion_mod.create
this.cbx_consolida=create cbx_consolida
this.cbx_todos=create cbx_todos
this.dw_seleccion=create dw_seleccion
this.Control[]={this.cbx_consolida,&
this.cbx_todos,&
this.dw_seleccion}
end on

on uo_seleccion_tipoinspeccion_mod.destroy
destroy(this.cbx_consolida)
destroy(this.cbx_todos)
destroy(this.dw_seleccion)
end on

event constructor;dw_Seleccion.Object.Codigo.Dddw.Name			=	'dw_mues_ctlcaltipoinspecion'
dw_Seleccion.Object.codigo.Dddw.DisplayColumn	=	'ccti_nombre'
dw_Seleccion.Object.codigo.Dddw.DataColumn		=	'ccti_codigo'

dw_Seleccion.GetChild("codigo", idwc_Seleccion)

idwc_Seleccion.SetTransObject(sqlca)

IF	idwc_Seleccion.Retrieve() = 0 THEN
	MessageBox("Atención", "No existen Inspectores en tabla respectiva")
	
	SetNull(Codigo)
	SetNull(Nombre)
ELSE
	idwc_Seleccion.SetSort("ccti_nombre A")
	idwc_Seleccion.Sort()
	
	dw_Seleccion.Object.codigo.Font.Height	=	'-8'
	dw_Seleccion.Object.codigo.Height			=	64
	
	dw_Seleccion.SetTransObject(sqlca)
	dw_Seleccion.InsertRow(0)
	
	Codigo					=	-1
	Nombre					=	'Todos'
	iuo_TipoInspeccion	=	Create uo_ctlcaltipoinspec
	
	Seleccion(True, True)
END IF
end event

type cbx_consolida from checkbox within uo_seleccion_tipoinspeccion_mod
integer x = 1120
integer y = 24
integer width = 183
integer height = 80
integer taborder = 20
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = " "
boolean checked = true
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

type cbx_todos from checkbox within uo_seleccion_tipoinspeccion_mod
integer x = 919
integer y = 24
integer width = 178
integer height = 80
integer taborder = 10
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = " "
end type

event clicked; Todos(This.Checked)
 cbx_consolida.Checked = False
Parent.TriggerEvent("ue_cambio")
end event

type dw_seleccion from datawindow within uo_seleccion_tipoinspeccion_mod
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

IF iuo_TipoInspeccion.Existe(Integer(data), True, Sqlca) THEN
	Codigo	=	iuo_TipoInspeccion.Codigo
	Nombre	=	iuo_TipoInspeccion.Descripcion
ELSE
	This.SetItem(1, "codigo", li_Nula)

	RETURN 1
END IF

Parent.TriggerEvent("ue_cambio")
end event

event clicked;cbx_todos.Checked = False
cbx_consolida.Checked = False
end event

event itemerror;Return 1
end event

