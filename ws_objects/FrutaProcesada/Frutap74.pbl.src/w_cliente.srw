$PBExportHeader$w_cliente.srw
forward
global type w_cliente from window
end type
type uo_selcliente from uo_seleccion_clientesprod within w_cliente
end type
type pb_2 from picturebutton within w_cliente
end type
type pb_1 from picturebutton within w_cliente
end type
type st_1 from statictext within w_cliente
end type
end forward

global type w_cliente from window
integer width = 1861
integer height = 312
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
string icon = "AppIcon!"
boolean center = true
uo_selcliente uo_selcliente
pb_2 pb_2
pb_1 pb_1
st_1 st_1
end type
global w_cliente w_cliente

type variables
Str_Busqueda	istr_Busq
end variables

on w_cliente.create
this.uo_selcliente=create uo_selcliente
this.pb_2=create pb_2
this.pb_1=create pb_1
this.st_1=create st_1
this.Control[]={this.uo_selcliente,&
this.pb_2,&
this.pb_1,&
this.st_1}
end on

on w_cliente.destroy
destroy(this.uo_selcliente)
destroy(this.pb_2)
destroy(this.pb_1)
destroy(this.st_1)
end on

event open;Boolean	lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	x	= 100
	y	= 450
	
	uo_SelCliente.Seleccion(False, False)
End If
end event

type uo_selcliente from uo_seleccion_clientesprod within w_cliente
event destroy ( )
integer x = 375
integer y = 60
integer height = 88
integer taborder = 40
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type pb_2 from picturebutton within w_cliente
integer x = 1573
integer y = 8
integer width = 233
integer height = 196
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
boolean flatstyle = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Cancelar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Cancelar-bn.png"
alignment htextalign = left!
end type

event clicked;istr_Busq.Argum[2]	= ''

CloseWithReturn(Parent, istr_Busq)
end event

type pb_1 from picturebutton within w_cliente
integer x = 1303
integer y = 8
integer width = 233
integer height = 196
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
boolean flatstyle = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
alignment htextalign = left!
end type

event clicked;istr_Busq.Argum[1]	= String(uo_SelCliente.Codigo)
istr_Busq.Argum[2]	= String(uo_SelCliente.Nombre)

CloseWithReturn(Parent, istr_Busq)
end event

type st_1 from statictext within w_cliente
integer x = 27
integer y = 72
integer width = 361
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 16777215
string text = "Sel. Cliente"
boolean focusrectangle = false
end type

