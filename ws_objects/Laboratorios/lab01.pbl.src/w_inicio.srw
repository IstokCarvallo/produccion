$PBExportHeader$w_inicio.srw
forward
global type w_inicio from window
end type
type dw_1 from datawindow within w_inicio
end type
type cb_3 from commandbutton within w_inicio
end type
type cb_2 from commandbutton within w_inicio
end type
type cb_1 from commandbutton within w_inicio
end type
end forward

global type w_inicio from window
integer width = 1253
integer height = 1304
boolean titlebar = true
string title = "Lab_01 - PB19"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
dw_1 dw_1
cb_3 cb_3
cb_2 cb_2
cb_1 cb_1
end type
global w_inicio w_inicio

type variables
eon_appeon_resize	iuo_Resize
end variables

on w_inicio.create
this.dw_1=create dw_1
this.cb_3=create cb_3
this.cb_2=create cb_2
this.cb_1=create cb_1
this.Control[]={this.dw_1,&
this.cb_3,&
this.cb_2,&
this.cb_1}
end on

on w_inicio.destroy
destroy(this.dw_1)
destroy(this.cb_3)
destroy(this.cb_2)
destroy(this.cb_1)
end on

event open;SQLCA.DBMS = "SNC SQL Native Client(OLE DB)"
SQLCA.LogPass = "prag"
SQLCA.ServerName = "192.168.204.253"
SQLCA.LogId = "istok.carvallo"
SQLCA.AutoCommit = False
SQLCA.DBParm = "Database='ProdCoquimbo_2019',Provider='SQLNCLI11'"

Connect Using SQLCA;

dw_1.SetTransObject(SQLCA)
dw_1.Retrieve()

iuo_Resize	=	Create eon_appeon_resize

iuo_Resize.of_Init(This, True)
iuo_Resize.of_FontResize(True, 2)
iuo_Resize.of_Zoom(True, 2)
iuo_Resize.of_SetFlag(dw_1, '0055')
iuo_Resize.of_SetFlag(cb_1, '2222')
iuo_Resize.of_SetFlag(cb_2, '2222')
iuo_Resize.of_SetFlag(cb_3, '2222')



end event

event resize;iuo_Resize.of_Resize(This, NewWidth, NewHeight, True)
end event

type dw_1 from datawindow within w_inicio
integer x = 23
integer y = 8
integer width = 1161
integer height = 1016
integer taborder = 10
string title = "none"
string dataobject = "dw_mues_especie"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type cb_3 from commandbutton within w_inicio
integer x = 805
integer y = 1056
integer width = 384
integer height = 112
integer taborder = 10
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string pointer = "Arrow!"
string text = "Salir"
boolean cancel = true
boolean flatstyle = true
end type

event clicked;Close(Parent)
end event

type cb_2 from commandbutton within w_inicio
integer x = 402
integer y = 1056
integer width = 384
integer height = 112
integer taborder = 20
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string pointer = "Arrow!"
string text = "Hora Actual"
boolean flatstyle = true
end type

event clicked;MessageBox('Atencion', 'Fecha Actual es: ' + String(Now(), 'hh:mm:ss'))
end event

type cb_1 from commandbutton within w_inicio
integer y = 1056
integer width = 384
integer height = 112
integer taborder = 10
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string pointer = "Arrow!"
string text = "Fecha Actual"
boolean default = true
boolean flatstyle = true
end type

event clicked;MessageBox('Atencion', 'Fecha Actual es: ' + String(Today(), 'dd/mm/yyyy'))
end event

