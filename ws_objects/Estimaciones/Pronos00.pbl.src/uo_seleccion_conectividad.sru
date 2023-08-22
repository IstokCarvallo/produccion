﻿$PBExportHeader$uo_seleccion_conectividad.sru
$PBExportComments$Objeto público para selección de Conectividad, Todos o Consolidado
forward
global type uo_seleccion_conectividad from userobject
end type
type cbx_consolida from checkbox within uo_seleccion_conectividad
end type
type cbx_todos from checkbox within uo_seleccion_conectividad
end type
type dw_seleccion from datawindow within uo_seleccion_conectividad
end type
end forward

global type uo_seleccion_conectividad from userobject
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
global uo_seleccion_conectividad uo_seleccion_conectividad

type variables
DataWindowChild	idwc_Seleccion

uo_pronconectividad	iuo_Codigo
uo_ODBC					iuo_ODBC

Integer	Codigo, Temporada
String		Nombre, ODBC, Servidor, Base, Usuario, Password, DBMS, Ip, Puerto, Ubicacion
end variables

forward prototypes
public subroutine seleccion (boolean ab_todos, boolean ab_consolida)
public subroutine todos (boolean ab_todos)
public subroutine bloquear (boolean ab_opcion)
public subroutine limpiadatos ()
public function boolean inicia (integer ai_codigo)
end prototypes

public subroutine seleccion (boolean ab_todos, boolean ab_consolida);cbx_Todos.Visible		=	ab_Todos
cbx_Consolida.Visible	=	ab_Consolida
 
IF Not ab_Todos AND Not ab_Consolida THEN
	dw_Seleccion.y				=	0
	dw_Seleccion.Enabled	=	True
	
	dw_Seleccion.Object.codigo.Color					=	0
	dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
ELSE
	dw_Seleccion.y				=	84
	dw_Seleccion.Enabled	=	False
	
	dw_Seleccion.Object.codigo.Color					=	RGB(255, 255, 255)
	dw_Seleccion.Object.codigo.BackGround.Color	=	553648127
END IF
end subroutine

public subroutine todos (boolean ab_todos);If ab_Todos Then
	Codigo						=	-1
	Nombre						=	'Todos'
	cbx_Todos.Checked		=	True
	cbx_Consolida.Enabled	=	True
	dw_Seleccion.Enabled	=	False
	
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
	
	dw_Seleccion.Object.codigo[1]						=	Codigo
	dw_Seleccion.Object.codigo.Color					=	0
	dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
End If
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

public subroutine limpiadatos ();String	ls_Nula

SetNull(ls_Nula)

dw_Seleccion.SetItem(1, "codigo", Long(ls_Nula))
Codigo = Long(ls_Nula)
end subroutine

public function boolean inicia (integer ai_codigo);Integer	li_Nula
Boolean	lb_Retorno = False

SetNull(li_Nula)

If iuo_Codigo.Existe(Integer(ai_Codigo), False, sqlca) Then
	Codigo		=	iuo_Codigo.Codigo
	Nombre		=	iuo_Codigo.Nombre
	ODBC			=	iuo_Codigo.ODBC
	Servidor		=	iuo_Codigo.Servidor
	Base			=	iuo_Codigo.Base
	Usuario		=	iuo_Codigo.Usuario
	Password	=	iuo_Codigo.Password
	DBMS			=	iuo_Codigo.DBMS
	Ip				=	iuo_Codigo.IP
	Puerto		=	iuo_Codigo.Puerto
	Ubicacion	=	iuo_Codigo.Ubicacion
	Temporada	=	iuo_Codigo.Temporada
	
	dw_Seleccion.SetItem(1, "codigo", ai_Codigo)
	lb_Retorno = True
Else
	dw_Seleccion.SetItem(1, "codigo", li_Nula)
End If

Return lb_Retorno 
end function

on uo_seleccion_conectividad.create
this.cbx_consolida=create cbx_consolida
this.cbx_todos=create cbx_todos
this.dw_seleccion=create dw_seleccion
this.Control[]={this.cbx_consolida,&
this.cbx_todos,&
this.dw_seleccion}
end on

on uo_seleccion_conectividad.destroy
destroy(this.cbx_consolida)
destroy(this.cbx_todos)
destroy(this.dw_seleccion)
end on

event constructor;dw_Seleccion.Object.Codigo.Dddw.Name			=	'dw_mues_pron_conexion'
dw_Seleccion.Object.codigo.Dddw.DisplayColumn	=	'prco_nombre'
dw_Seleccion.Object.codigo.Dddw.DataColumn		=	'prco_codigo'

dw_Seleccion.GetChild("codigo", idwc_Seleccion)
idwc_Seleccion.SetTransObject(sqlca)

cbx_Todos.FaceName		=	"Tahoma"
cbx_consolida.FaceName	=	"Tahoma"
cbx_Todos.TextColor		=	RGB(255,255,255)
cbx_consolida.TextColor	=	RGB(255,255,255)

IF	idwc_Seleccion.Retrieve() = 0 THEN
	MessageBox("Atención", "No existen Conexiones en tabla respectiva")
	
	SetNull(Codigo)
	SetNull(Nombre)
ELSE
	idwc_Seleccion.SetSort("prco_odbc A")
	idwc_Seleccion.Sort()
	
	dw_Seleccion.Object.codigo.Font.Height	=	'-8'
	dw_Seleccion.Object.codigo.Height		=	64
	
	dw_Seleccion.InsertRow(0)
	
	Codigo			=	-1
	Nombre			=	'Todas'
	iuo_Codigo		=	Create uo_pronconectividad
	iuo_ODBC		=  Create uo_ODBC
	
	This.Seleccion(True, True)
END IF
end event

type cbx_consolida from checkbox within uo_seleccion_conectividad
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
	Nombre	=	'Consolidada'
ELSE
	Codigo	=	-1
	Nombre	=	'Todas'
END IF

Parent.TriggerEvent("ue_cambio")
end event

type cbx_todos from checkbox within uo_seleccion_conectividad
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

type dw_seleccion from datawindow within uo_seleccion_conectividad
integer y = 80
integer width = 882
integer height = 96
integer taborder = 30
string title = "none"
string dataobject = "dddw_codnumero"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Nula

SetNull(li_Nula)

IF iuo_Codigo.Existe(Integer(data), true, sqlca) THEN
	Codigo		=	iuo_Codigo.Codigo
	Nombre		=	iuo_Codigo.Nombre
	ODBC			=	iuo_Codigo.ODBC
	Servidor		=	iuo_Codigo.Servidor
	Base			=	iuo_Codigo.Base
	Usuario		=	iuo_Codigo.Usuario
	Password	=	iuo_Codigo.Password
	DBMS			=	iuo_Codigo.DBMS
	Ip				=	iuo_Codigo.IP
	Puerto		=	iuo_Codigo.Puerto
	Ubicacion	=	iuo_Codigo.Ubicacion
	Temporada	=	iuo_Codigo.Temporada
ELSE
	This.SetItem(1, "codigo", li_Nula)
	RETURN 1
END IF

If DBMS = 'ODBC' Then 
	iuo_ODBC.Crea(String(Nombre), String(Usuario), String(Password), String(Base),String(Ubicacion) ,&
					String(Servidor), String(Ip), String(Puerto), String(Ubicacion))					
End If

Parent.TriggerEvent("ue_cambio")
end event

event itemerror;RETURN 1
end event

