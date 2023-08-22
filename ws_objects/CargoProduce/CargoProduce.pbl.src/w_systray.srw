$PBExportHeader$w_systray.srw
$PBExportComments$Sample window
forward
global type w_systray from window
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
long backcolor = 16777215
string icon = "AppIcon!"
event trayevent pbm_custom01
event m_restore ( )
event m_exit ( )
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
this.pb_salir=create pb_salir
this.Control[]={this.pb_salir}
end on

on w_systray.destroy
destroy(this.pb_salir)
end on

event open;

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

event resize;Integer		li_posic_x, li_posic_y, li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

li_posic_x				=	This.WorkSpaceWidth() - 370
li_posic_y				=	This.WorkSpaceHeight() - 258

pb_salir.x			= li_posic_x
pb_salir.y			= li_posic_y
pb_salir.Width		= li_Ancho 
pb_salir.Height		= li_Alto
li_posic_y 			+= li_Siguiente * 1.25
end event

type pb_salir from picturebutton within w_systray
integer x = 4165
integer y = 1560
integer width = 302
integer height = 236
integer taborder = 10
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = left!
end type

event clicked;// modify icon
in_tray.of_add_icon(Parent, gstr_apl.icono)

end event

