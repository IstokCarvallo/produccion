$PBExportHeader$w_camaras.srw
forward
global type w_camaras from window
end type
type pb_2 from picturebutton within w_camaras
end type
type pb_1 from picturebutton within w_camaras
end type
type st_1 from statictext within w_camaras
end type
type uo_selcamara from uo_seleccion_camarasbode within w_camaras
end type
end forward

global type w_camaras from window
integer width = 1861
integer height = 312
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
string icon = "AppIcon!"
boolean center = true
pb_2 pb_2
pb_1 pb_1
st_1 st_1
uo_selcamara uo_selcamara
end type
global w_camaras w_camaras

type variables
Str_Busqueda	istr_Busq
end variables

on w_camaras.create
this.pb_2=create pb_2
this.pb_1=create pb_1
this.st_1=create st_1
this.uo_selcamara=create uo_selcamara
this.Control[]={this.pb_2,&
this.pb_1,&
this.st_1,&
this.uo_selcamara}
end on

on w_camaras.destroy
destroy(this.pb_2)
destroy(this.pb_1)
destroy(this.st_1)
destroy(this.uo_selcamara)
end on

event open;Boolean	lb_Cerrar = False

If IsNull(uo_SelCamara.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	x	= 100
	y	= 450

	istr_Busq	= Message.PowerObjectParm
	
	uo_SelCamara.Seleccion(False, False)
	uo_SelCamara.Filtra(Integer(istr_Busq.Argum[1]))
End If
end event

type pb_2 from picturebutton within w_camaras
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

event clicked;istr_Busq.Cancela = 1
istr_Busq.Argum[2]	= ''

CloseWithReturn(Parent, istr_Busq)
end event

type pb_1 from picturebutton within w_camaras
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

event clicked;istr_Busq.Cancela 		= 0
istr_Busq.Argum[2]	= String(uo_SelCamara.Codigo)
istr_Busq.Argum[3]	= String(uo_SelCamara.TipoFrio)

CloseWithReturn(Parent, istr_Busq)
end event

type st_1 from statictext within w_camaras
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
string text = "Sel. Camara"
boolean focusrectangle = false
end type

type uo_selcamara from uo_seleccion_camarasbode within w_camaras
integer x = 402
integer y = 64
integer height = 80
integer taborder = 40
end type

on uo_selcamara.destroy
call uo_seleccion_camarasbode::destroy
end on

