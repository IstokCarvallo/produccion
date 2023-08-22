$PBExportHeader$w_systray.srw
$PBExportComments$Sample window
forward
global type w_systray from window
end type
type st_encabezado from statictext within w_systray
end type
type dw_2 from uo_dw within w_systray
end type
type dw_1 from uo_dw within w_systray
end type
type pb_salir from picturebutton within w_systray
end type
end forward

global type w_systray from window
integer x = 142
integer y = 144
integer width = 4622
integer height = 2000
boolean titlebar = true
boolean controlmenu = true
long backcolor = 15793151
string icon = "AppIcon!"
event trayevent pbm_custom01
event m_restore ( )
event m_exit ( )
st_encabezado st_encabezado
dw_2 dw_2
dw_1 dw_1
pb_salir pb_salir
end type
global w_systray w_systray

type variables
n_icontray in_tray
Integer ii_index
Boolean ib_timer
end variables

event trayevent;m_icontray lm_icontray

// process tray events
CHOOSE CASE lparam
	CASE in_tray.WM_LBUTTONDBLCLK
		// remove icon from system tray
		in_tray.of_delete_icon(this, True)
	CASE in_tray.WM_RBUTTONDOWN
		// display popup menu
		in_tray.of_SetFocus(this)
		lm_icontray = CREATE m_icontray
		lm_icontray.m_popup.PopMenu(PointerX(),PointerY())
		DESTROY lm_icontray
END CHOOSE

end event

event m_restore;// remove icon from system tray
in_tray.of_delete_icon(this, True)

end event

event m_exit;// remove icon from system tray
in_tray.of_delete_icon(this, True)

// close window
Close(this)

end event

on w_systray.create
this.st_encabezado=create st_encabezado
this.dw_2=create dw_2
this.dw_1=create dw_1
this.pb_salir=create pb_salir
this.Control[]={this.st_encabezado,&
this.dw_2,&
this.dw_1,&
this.pb_salir}
end on

on w_systray.destroy
destroy(this.st_encabezado)
destroy(this.dw_2)
destroy(this.dw_1)
destroy(this.pb_salir)
end on

event open;dw_1.SetTransObject(SqlCa)
dw_2.SetTransObject(SqlCa)

// add window icon to tray
in_tray.of_add_icon(this)

// register hotkey
If Not in_tray.of_RegisterHotKey(this, 1, in_tray.MOD_WIN, in_tray.KeyF8) Then
	MessageBox(this.title, "RegisterHotKey failed, hotkey already in use!")
End If

end event

event close;// unregister hotkey
in_tray.of_UnRegisterHotKey(This, 1)

end event

event other;// detect HotKey event
If in_tray.of_isHotKey(wparam, lparam) Then
	// remove icon from system tray
	in_tray.of_delete_icon(this, True)
End If

end event

event resize;
st_encabezado.x 			= 40
st_encabezado.y 			= 40
st_encabezado.Width		= This.WorkSpaceWidth() - 400

dw_1.y		=	st_encabezado.y + st_encabezado.Height + 25
dw_1.x		=	st_encabezado.x
dw_1.Height	=	This.WorkSpaceHeight() - (dw_1.y + 60)

dw_2.y		=	st_encabezado.y + st_encabezado.Height + 300
dw_2.x		=	st_encabezado.Width - dw_2.Width
dw_2.Height	=	This.WorkSpaceHeight() - (dw_2.y + 60)

pb_salir.x			= This.WorkSpaceWidth() - 292
pb_salir.y			= This.WorkSpaceHeight() - 258
pb_salir.Width		= 230
pb_salir.Height		= 195
end event

type st_encabezado from statictext within w_systray
integer x = 37
integer y = 32
integer width = 4082
integer height = 292
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_2 from uo_dw within w_systray
integer x = 2450
integer y = 344
integer width = 1669
integer height = 1516
integer taborder = 20
string dragicon = "|"
end type

type dw_1 from uo_dw within w_systray
integer x = 37
integer y = 344
integer width = 2267
integer height = 1516
integer taborder = 10
end type

type pb_salir from picturebutton within w_systray
integer x = 4288
integer y = 1676
integer width = 229
integer height = 196
integer taborder = 10
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string picturename = "\Desarrollo 12\Imagenes\Botones\SalirEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\SalirDisab.png"
alignment htextalign = left!
end type

event clicked;// modify icon
in_tray.of_add_icon(Parent, gstr_apl.icono)

end event

