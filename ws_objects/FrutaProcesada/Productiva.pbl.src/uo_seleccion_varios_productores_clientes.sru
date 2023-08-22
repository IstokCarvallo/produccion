$PBExportHeader$uo_seleccion_varios_productores_clientes.sru
$PBExportComments$Objeto público para selección de Productor, Todos o Consolidado
forward
global type uo_seleccion_varios_productores_clientes from userobject
end type
type em_lista from editmask within uo_seleccion_varios_productores_clientes
end type
type cbx_consolida from checkbox within uo_seleccion_varios_productores_clientes
end type
type cbx_todos from checkbox within uo_seleccion_varios_productores_clientes
end type
type dw_seleccion from datawindow within uo_seleccion_varios_productores_clientes
end type
end forward

global type uo_seleccion_varios_productores_clientes from userobject
integer width = 873
integer height = 272
long backcolor = 553648127
string text = "none"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
event ue_cambio ( )
em_lista em_lista
cbx_consolida cbx_consolida
cbx_todos cbx_todos
dw_seleccion dw_seleccion
end type
global uo_seleccion_varios_productores_clientes uo_seleccion_varios_productores_clientes

type variables
DataWindowChild	idwc_Seleccion

uo_Productores		iuo_Codigo

Integer	Exportador, Zona, Tipo
Long     	Codigo, Cliente
String		Nombre, Lista 
end variables

forward prototypes
public subroutine seleccion (boolean ab_todos, boolean ab_consolida)
public subroutine todos (boolean ab_todos)
public subroutine limpiardatos ()
public subroutine bloquea_seleccion (boolean ab_bloquea)
public subroutine bloquear (boolean ab_opcion)
public subroutine filtra (integer ai_zona, integer ai_tipo, integer ai_cliente)
end prototypes

public subroutine seleccion (boolean ab_todos, boolean ab_consolida);cbx_Todos.Visible		=	ab_Todos
cbx_Consolida.Visible	=	ab_Consolida

IF Not ab_Todos AND Not ab_Consolida THEN
//	dw_Seleccion.y				=	0
	dw_Seleccion.Enabled	=	True
	
	dw_Seleccion.Object.codigo.Color					=	0
	dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
ELSE
//	dw_Seleccion.y				=	100
	dw_Seleccion.Enabled	=	False
	
	dw_Seleccion.Object.codigo.Color					=	RGB(255, 255, 255)
	dw_Seleccion.Object.codigo.BackGround.Color	=	553648127
End If
end subroutine

public subroutine todos (boolean ab_todos);If ab_Todos Then
	Codigo						=	-1
	Nombre						=	'Todos'
	Lista							=	'-1'
	cbx_Todos.Checked		=	True
	cbx_Consolida.Enabled	=	True
	dw_Seleccion.Enabled	=	False
	em_lista.Enabled 			= False
	em_lista.Text 				= ''
	
	idwc_Seleccion.Retrieve(-1,-1,-1)
	
	dw_Seleccion.Object.codigo.Color					=	RGB(255, 255, 255)
	dw_Seleccion.Object.codigo.BackGround.Color	=	553648127

	dw_Seleccion.Reset()
	dw_Seleccion.InsertRow(0)
Else
	SetNull(Codigo)
	SetNull(Nombre)
	
	cbx_Todos.Checked		=	False
	cbx_Consolida.Checked	=	False
	cbx_Consolida.Enabled	=	False
	dw_Seleccion.Enabled	=	True
	em_lista.Enabled 			= True
	
	idwc_Seleccion.Retrieve(-1, -1, -1)
	
	dw_Seleccion.Object.codigo[1]						=	Codigo
	dw_Seleccion.Object.codigo.Color					=	0
	dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
End If
end subroutine

public subroutine limpiardatos ();String	ls_Nula

SetNull(ls_Nula)

dw_Seleccion.SetItem(1, "codigo", Long(ls_Nula))
Codigo 				= Long(ls_Nula)
Lista					= String(ls_Nula)
em_lista.Text		= ''
end subroutine

public subroutine bloquea_seleccion (boolean ab_bloquea);IF ab_bloquea THEN
	cbx_Todos.Enabled		=	False
	cbx_consolida.Enabled	=	False
ELSE
	cbx_Todos.Enabled		=	True
	cbx_consolida.Enabled	=	True
END IF
end subroutine

public subroutine bloquear (boolean ab_opcion);If ab_opcion Then
	dw_Seleccion.Enabled								= False
	dw_Seleccion.Object.Codigo.Color					=	RGB(255, 255, 255)
	dw_Seleccion.Object.Codigo.BackGround.Color	=	553648127
Else
	dw_Seleccion.Enabled								= True
	dw_Seleccion.Object.Codigo.Color					=	0
	dw_Seleccion.Object.Codigo.BackGround.Color	=	RGB(255, 255, 255)
End If
end subroutine

public subroutine filtra (integer ai_zona, integer ai_tipo, integer ai_cliente);Zona	=	ai_zona
Tipo	=	ai_tipo
Cliente	=	ai_Cliente
 
dw_Seleccion.GetChild("codigo", idwc_Seleccion)
idwc_Seleccion.SetTransObject(sqlca)

IF idwc_Seleccion.Retrieve(Zona,Tipo,Cliente) = 0 THEN
	MessageBox("Atención", "No existen Produtores para este Cliente.")
	
	SetNull(Codigo)
	SetNull(Nombre)
END IF
end subroutine

on uo_seleccion_varios_productores_clientes.create
this.em_lista=create em_lista
this.cbx_consolida=create cbx_consolida
this.cbx_todos=create cbx_todos
this.dw_seleccion=create dw_seleccion
this.Control[]={this.em_lista,&
this.cbx_consolida,&
this.cbx_todos,&
this.dw_seleccion}
end on

on uo_seleccion_varios_productores_clientes.destroy
destroy(this.em_lista)
destroy(this.cbx_consolida)
destroy(this.cbx_todos)
destroy(this.dw_seleccion)
end on

event constructor;dw_Seleccion.Object.Codigo.Dddw.Name			=	'dw_mues_productores_filtra_clientes'
dw_Seleccion.Object.codigo.Dddw.DisplayColumn	=	'compute_2'
dw_Seleccion.Object.codigo.Dddw.DataColumn		=	'prod_codigo'
em_lista.Text												=	''

dw_Seleccion.GetChild("codigo", idwc_Seleccion)
idwc_Seleccion.SetTransObject(sqlca)

cbx_Todos.FaceName		=	"Tahoma"
cbx_consolida.FaceName	=	"Tahoma"
em_Lista.FaceName		=	"Tahoma"
cbx_Todos.TextColor		=	RGB(255,255,255)
cbx_consolida.TextColor	=	RGB(255,255,255)

IF	idwc_Seleccion.Retrieve(-1,-1, -1) = 0 THEN
	MessageBox("Atención", "No existen Productores en tabla respectiva")
	
	SetNull(Codigo)
	SetNull(Nombre)
ELSE
	//idwc_Seleccion.SetSort("prod_nombre A")
	idwc_Seleccion.Sort()
	
	dw_Seleccion.Object.codigo.Font.Height	=	'-8'
	//dw_Seleccion.Object.codigo.Height			=	64
	
	dw_Seleccion.SetTransObject(sqlca)
	dw_Seleccion.InsertRow(0)
	
	Codigo			=	-1
	Nombre			=	'Todos'
	Lista				=	'-1'
	iuo_Codigo		=	Create uo_Productores
	
	Seleccion(True, True)
END IF
end event

type em_lista from editmask within uo_seleccion_varios_productores_clientes
integer y = 4
integer width = 878
integer height = 96
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 16777215
maskdatatype maskdatatype = stringmask!
end type

event modified;lista = em_lista.Text
end event

type cbx_consolida from checkbox within uo_seleccion_varios_productores_clientes
integer x = 471
integer y = 104
integer width = 411
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
	Codigo						=	-9
	Nombre						=	'Consolidado'
	Lista							=	'-9'	
ELSE
	Codigo						=	-1
	Lista							=	'-1'
	Nombre						=	'Todos'
	em_lista.Enabled 			= False
	em_lista.Text 				= ''
END IF

Parent.TriggerEvent("ue_cambio")
end event

type cbx_todos from checkbox within uo_seleccion_varios_productores_clientes
integer y = 104
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

type dw_seleccion from datawindow within uo_seleccion_varios_productores_clientes
integer y = 180
integer width = 878
integer height = 88
integer taborder = 30
string title = "none"
string dataobject = "dddw_codnumero"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Long	ll_Nula

SetNull(ll_Nula)

IF iuo_Codigo.Existe(Long(data), True, Sqlca) THEN
	Codigo	=	iuo_Codigo.Codigo
	Nombre	=	iuo_Codigo.Nombre
	
	IF em_lista.Text	= '' THEN	
		em_lista.Text	=	String(Codigo)
		Lista				=  em_lista.Text
	ELSE
		em_lista.Text	=  String(em_lista.Text)+','+String(Codigo)			
		Lista				=	em_lista.Text
	END IF	
	
ELSE
	This.SetItem(1, "codigo", ll_Nula)
	RETURN 1
END IF

Parent.TriggerEvent("ue_cambio")
end event

event itemerror;RETURN 1
end event

