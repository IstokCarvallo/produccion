$PBExportHeader$uo_seleccion_tipoinspeccion.sru
$PBExportComments$Objeto público para selección de Tipo Inspeccion, Todos o Consolidado
forward
global type uo_seleccion_tipoinspeccion from userobject
end type
type cbx_consolida from checkbox within uo_seleccion_tipoinspeccion
end type
type cbx_todos from checkbox within uo_seleccion_tipoinspeccion
end type
type dw_seleccion from datawindow within uo_seleccion_tipoinspeccion
end type
end forward

global type uo_seleccion_tipoinspeccion from userobject
integer width = 896
integer height = 176
long backcolor = 553648127
string text = "none"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
event ue_cambio ( )
cbx_consolida cbx_consolida
cbx_todos cbx_todos
dw_seleccion dw_seleccion
end type
global uo_seleccion_tipoinspeccion uo_seleccion_tipoinspeccion

type variables
DataWindowChild	idwc_Seleccion

uo_ctlcaltipoinspec		iuo_TipoInspeccion

Integer					Codigo
String						Nombre 
end variables

forward prototypes
public subroutine seleccion (boolean ab_todos, boolean ab_consolida)
public subroutine todos (boolean ab_todos)
end prototypes

public subroutine seleccion (boolean ab_todos, boolean ab_consolida);cbx_Todos.Visible			=	ab_Todos
cbx_Consolida.Visible	=	ab_Consolida

IF Not ab_Todos AND Not ab_Consolida THEN
	dw_Seleccion.y			=	0
	dw_Seleccion.Enabled	=	True
	
	dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
ELSE
	dw_Seleccion.y			=	100
	dw_Seleccion.Enabled	=	False
	
	dw_Seleccion.Object.codigo.BackGround.Color	=	553648127
END IF

RETURN
end subroutine

public subroutine todos (boolean ab_todos);IF ab_todos THEN
	Codigo						=	-1
	Nombre						=	'Todos'
	cbx_Todos.Checked			=	True
	cbx_Consolida.Enabled	=	True
	dw_Seleccion.Enabled		=	False
	
	dw_Seleccion.Object.codigo.BackGround.Color	=	553648127
	
	dw_Seleccion.DeleteRow(1)
	
	dw_Seleccion.InsertRow(0)
ELSE
	SetNull(Codigo)
	SetNull(Nombre)
	
	cbx_Todos.Checked			=	False
	cbx_Consolida.Checked	=	False
	cbx_Consolida.Enabled	=	False
	dw_Seleccion.Enabled		=	True
	
	dw_Seleccion.Object.codigo[1]						=	Codigo
	dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
END IF

RETURN
end subroutine

on uo_seleccion_tipoinspeccion.create
this.cbx_consolida=create cbx_consolida
this.cbx_todos=create cbx_todos
this.dw_seleccion=create dw_seleccion
this.Control[]={this.cbx_consolida,&
this.cbx_todos,&
this.dw_seleccion}
end on

on uo_seleccion_tipoinspeccion.destroy
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
	MessageBox("Atención", "No existen Ispectores en tabla respectiva")
	
	SetNull(Codigo)
	SetNull(Nombre)
ELSE
	idwc_Seleccion.SetSort("ccti_nombre A")
	idwc_Seleccion.Sort()
	
	dw_Seleccion.Object.codigo.Font.Height	=	'-8'
	dw_Seleccion.Object.codigo.Height			=	64
	
	dw_Seleccion.SetTransObject(sqlca)
	dw_Seleccion.InsertRow(0)
	
	Codigo			         =	-1
	Nombre			         =	'Todos'
	iuo_TipoInspeccion	=	Create uo_ctlcaltipoinspec
	
	Seleccion(True, True)
END IF
end event

type cbx_consolida from checkbox within uo_seleccion_tipoinspeccion
integer x = 480
integer width = 407
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
string text = "Consolidado"
end type

event clicked;IF This.Checked THEN
	Codigo	=	-9
	Nombre	=	'Consolidado'
ELSE
	Codigo	=	-1
	Nombre	=	'Todos'
END IF

Parent.TriggerEvent("ue_cambio")
end event

type cbx_todos from checkbox within uo_seleccion_tipoinspeccion
integer width = 402
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
string text = "Todos"
boolean checked = true
end type

event clicked;Todos(This.Checked)

Parent.TriggerEvent("ue_cambio")
end event

type dw_seleccion from datawindow within uo_seleccion_tipoinspeccion
integer y = 80
integer width = 878
integer height = 80
integer taborder = 30
string title = "none"
string dataobject = "dddw_codnumero"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Nula

SetNull(li_Nula)

IF iuo_TipoInspeccion.Existe(Integer(data), True, Sqlca) THEN
	Codigo	=	iuo_TipoInspeccion.Codigo
	Nombre	=	iuo_TipoInspeccion.Descripcion
ELSE
	This.SetItem(1, "codigo", li_Nula)

	RETURN 1
END IF

Parent.TriggerEvent("ue_cambio")

end event

event itemerror;RETURN 1
end event

