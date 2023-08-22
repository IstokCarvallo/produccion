$PBExportHeader$w_password_guia.srw
forward
global type w_password_guia from window
end type
type pb_rechaza from picturebutton within w_password_guia
end type
type sle_password from singlelineedit within w_password_guia
end type
type pb_acepta from picturebutton within w_password_guia
end type
type gb_1 from groupbox within w_password_guia
end type
end forward

global type w_password_guia from window
integer x = 581
integer y = 732
integer width = 1298
integer height = 644
boolean titlebar = true
string title = "Password Acceso"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 30586022
string icon = "\Desarrollo\Bmp\Secur05.ico"
pb_rechaza pb_rechaza
sle_password sle_password
pb_acepta pb_acepta
gb_1 gb_1
end type
global w_password_guia w_password_guia

type variables
Str_mant		istr_mant

Integer		ii_Errores
end variables

event open;//	istr_mant.Argumento[1]	=	"Sistema"
//	istr_mant.Argumento[2]	=	"Password"
//	istr_mant.Argumento[3]	=	"Usuario"

istr_mant	=	Message.PowerObjectParm
This.Title	=	"Password Acceso " + istr_mant.Argumento[1]
 
sle_password.SetFocus()
end event

on w_password_guia.create
this.pb_rechaza=create pb_rechaza
this.sle_password=create sle_password
this.pb_acepta=create pb_acepta
this.gb_1=create gb_1
this.Control[]={this.pb_rechaza,&
this.sle_password,&
this.pb_acepta,&
this.gb_1}
end on

on w_password_guia.destroy
destroy(this.pb_rechaza)
destroy(this.sle_password)
destroy(this.pb_acepta)
destroy(this.gb_1)
end on

event close;CloseWithReturn(This, istr_mant)
end event

type pb_rechaza from picturebutton within w_password_guia
event mousemove pbm_mousemove
string tag = "Rechazar"
integer x = 910
integer y = 292
integer width = 215
integer height = 184
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo\Bmp\Cancelae.bmp"
string disabledname = "\Desarrollo\Bmp\Cancelad.bmp"
alignment htextalign = right!
string powertiptext = "Rechazar"
end type

event mousemove;RETURN w_main.SetMicroHelp(This.Tag)
end event

event clicked;istr_mant.Respuesta	=	0

Parent.TriggerEvent(close!)
end event

type sle_password from singlelineedit within w_password_guia
integer x = 119
integer y = 216
integer width = 626
integer height = 96
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean password = true
borderstyle borderstyle = stylelowered!
end type

type pb_acepta from picturebutton within w_password_guia
event mousemove pbm_mousemove
string tag = "Aceptar"
integer x = 910
integer y = 84
integer width = 215
integer height = 184
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 12\Imagenes\Botones\AceptarEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\AceptarDisab.png"
alignment htextalign = right!
string powertiptext = "Aceptar"
end type

event mousemove;RETURN w_main.SetMicroHelp(This.Tag)
end event

event clicked;IF sle_password.Text = 'spison' OR sle_password.Text = 'pvaldes' OR sle_password.Text = 'gmadariaga' THEN
	
		istr_mant.Respuesta	=	1
	
ELSE
	IF ii_Errores = 3 THEN
		MessageBox("Error", "Usted no tiene Acceso a esta Aplicación.~r~r" + &
						"Si desea Accesar, contáctese con Encargado.", Exclamation!)
						
		istr_mant.Respuesta	=	0
	ELSE
		IF MessageBox("Error", "Clave de Acceso a esta Aplicación es incorrecta.~r~r" + &
						"Desea intentar nuevamente.", Exclamation!, YesNo!, 1) = 1 THEN
			ii_Errores ++
			
			sle_password.SetFocus()
			
			RETURN
		ELSE
			istr_mant.Respuesta	=	0
		END IF
	END IF
END IF
	
Parent.TriggerEvent(close!)
end event

type gb_1 from groupbox within w_password_guia
boolean visible = false
integer x = 869
integer y = 28
integer width = 274
integer height = 448
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

