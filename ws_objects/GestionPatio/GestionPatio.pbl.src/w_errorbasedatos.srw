﻿$PBExportHeader$w_errorbasedatos.srw
forward
global type w_errorbasedatos from window
end type
type dw_1 from datawindow within w_errorbasedatos
end type
type pb_correo from picturebutton within w_errorbasedatos
end type
type pb_acepta from picturebutton within w_errorbasedatos
end type
type mle_mensaje from multilineedit within w_errorbasedatos
end type
type gb_1 from groupbox within w_errorbasedatos
end type
end forward

global type w_errorbasedatos from window
integer x = 649
integer y = 300
integer width = 1943
integer height = 1080
boolean titlebar = true
string title = "CONTROL DE ERROR"
windowtype windowtype = response!
long backcolor = 16777215
string icon = "StopSign!"
dw_1 dw_1
pb_correo pb_correo
pb_acepta pb_acepta
mle_mensaje mle_mensaje
gb_1 gb_1
end type
global w_errorbasedatos w_errorbasedatos

type variables
Str_ErrorBaseDatos	istr_ErrBD
end variables

event open;istr_ErrBD			=	Message.PowerObjectParm
mle_mensaje.text	=	istr_ErrBD.MensajePantalla
end event

on w_errorbasedatos.create
this.dw_1=create dw_1
this.pb_correo=create pb_correo
this.pb_acepta=create pb_acepta
this.mle_mensaje=create mle_mensaje
this.gb_1=create gb_1
this.Control[]={this.dw_1,&
this.pb_correo,&
this.pb_acepta,&
this.mle_mensaje,&
this.gb_1}
end on

on w_errorbasedatos.destroy
destroy(this.dw_1)
destroy(this.pb_correo)
destroy(this.pb_acepta)
destroy(this.mle_mensaje)
destroy(this.gb_1)
end on

type dw_1 from datawindow within w_errorbasedatos
boolean visible = false
integer x = 1641
integer y = 816
integer width = 494
integer height = 360
integer taborder = 40
string dataobject = "dw_errorbasedatos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type pb_correo from picturebutton within w_errorbasedatos
event mousemove pbm_mousemove
integer x = 1605
integer y = 564
integer width = 300
integer height = 245
integer taborder = 30
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Email.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Email-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;//String	ls_error = ""
//
//
//ls_error	=	ls_error + istr_ErrBD.Titulo + "~r~n"
//ls_error	=	ls_error + "Cod.Err.: " + String(istr_ErrBD.Numero) + "~r~n"
//ls_error	=	ls_error + "Descrip.: " + istr_ErrBD.Texto + "~r~n"
//
//ls_error	=	ls_error + "Usuario : "  + gstr_us.Nombre + "~r~n"
//ls_error	=	ls_error + "Estacion: " + gstr_us.Computador + "~r~n"
//
//ClipBoard(ls_error)
CloseWithReturn(Parent, "1")
end event

type pb_acepta from picturebutton within w_errorbasedatos
event mousemove pbm_mousemove
integer x = 1595
integer y = 212
integer width = 300
integer height = 245
integer taborder = 10
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;CloseWithReturn(Parent, "0")
end event

type mle_mensaje from multilineedit within w_errorbasedatos
integer x = 46
integer y = 32
integer width = 1490
integer height = 908
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
string pointer = "arrow!"
long textcolor = 8388608
long backcolor = 16777215
boolean vscrollbar = true
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type gb_1 from groupbox within w_errorbasedatos
boolean visible = false
integer x = 1595
integer y = 148
integer width = 283
integer height = 500
integer taborder = 20
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

