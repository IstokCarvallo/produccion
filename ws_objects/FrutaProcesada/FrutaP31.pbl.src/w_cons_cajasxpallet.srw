$PBExportHeader$w_cons_cajasxpallet.srw
forward
global type w_cons_cajasxpallet from window
end type
type dw_1 from uo_dw within w_cons_cajasxpallet
end type
type pb_salir from picturebutton within w_cons_cajasxpallet
end type
end forward

global type w_cons_cajasxpallet from window
integer x = 5
integer y = 16
integer width = 1198
integer height = 1740
windowtype windowtype = response!
long backcolor = 33554431
boolean clientedge = true
boolean center = true
event ue_asignacion ( )
dw_1 dw_1
pb_salir pb_salir
end type
global w_cons_cajasxpallet w_cons_cajasxpallet

type variables
String   ls_año_periodo
Long 		il_fila
Date		id_FechaAcceso
Time		it_HoraAcceso
Integer	ii_NroMes

Menu		im_menu

Str_parms		istr_parms
Str_mant			istr_mant
Str_busqueda	istr_busq
Str_info			istr_info
end variables

on w_cons_cajasxpallet.create
this.dw_1=create dw_1
this.pb_salir=create pb_salir
this.Control[]={this.dw_1,&
this.pb_salir}
end on

on w_cons_cajasxpallet.destroy
destroy(this.dw_1)
destroy(this.pb_salir)
end on

event open;str_Busqueda lstr_Busq

This.Icon										=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True

dw_1.SetTransObject(sqlca)
dw_1.SetRowFocusIndicator(Hand!)

lstr_Busq = Message.PowerObjectParm
dw_1.Retrieve(Long(lstr_Busq.Argum[1]), Long(lstr_Busq.Argum[2]), Long(lstr_Busq.Argum[3]))

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

end event

event close;Boolean	Valida
Window	ventana
Integer	li_vta

ventana	= This.ParentWindow().GetFirstSheet()

IF IsValid(ventana) THEN
	li_vta++

	DO
		ventana	= This.ParentWindow().GetNextSheet(ventana)
		valida	= IsValid(ventana)
		IF valida THEN li_vta++
	LOOP WHILE valida
END IF

IF li_vta = 1 THEN
	This.ParentWindow().ToolBarVisible	= False
	im_menu.Item[1].Item[6].Enabled		= False
	im_menu.Item[7].Visible					= False
END IF

GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
end event

event resize;pb_salir.x			= This.WorkSpaceWidth() - 292
pb_salir.width		= 300
pb_salir.height		= 250
end event

type dw_1 from uo_dw within w_cons_cajasxpallet
integer x = 37
integer y = 36
integer width = 686
integer height = 1620
integer taborder = 100
string dataobject = "dw_cons_cajasxpallet"
boolean hscrollbar = true
end type

event rbuttondown;//m_consulta	l_Menu
//
//IF RowCount() =	0	THEN
//	Return
//ELSE
//	gstr_us.OpcionActiva	=	Parent.ClassName()
//	il_fila 					=	Row
//	This.SetRow(il_fila)
//	
//	l_Menu = CREATE m_consulta
//	
//	m_consulta.m_m_edicion.consultarecepcion.Visible									=	True
////	
////	IF This.Object.rfpe_numerot[il_fila] > 0 THEN
////		m_consulta.m_m_edicion.m_ctacte.Visible									=	True
////		m_consulta.m_m_edicion.resumencuentacorrientecuenta.Visible			=	True
////	END IF
//	
////	m_consulta.m_m_edicion.consultacomprobante.Enabled = 	False
//	l_Menu.m_m_edicion.PopMenu(This.PointerX(),This.PointerY()+750)
//	
//END IF
//
end event

type pb_salir from picturebutton within w_cons_cajasxpallet
integer x = 795
integer y = 1404
integer width = 302
integer height = 252
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = left!
end type

event pb_salir::clicked;call super::clicked;Close(parent)
end event

