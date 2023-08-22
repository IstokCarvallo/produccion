$PBExportHeader$w_info_procesoshistoricosunpallet_nuevo.srw
$PBExportComments$Informe de Revisión Repalletizajes.
forward
global type w_info_procesoshistoricosunpallet_nuevo from w_para_informes
end type
type st_4 from statictext within w_info_procesoshistoricosunpallet_nuevo
end type
type st_5 from statictext within w_info_procesoshistoricosunpallet_nuevo
end type
type st_2 from statictext within w_info_procesoshistoricosunpallet_nuevo
end type
type em_numero from editmask within w_info_procesoshistoricosunpallet_nuevo
end type
type st_6 from statictext within w_info_procesoshistoricosunpallet_nuevo
end type
type cb_buscarepa from commandbutton within w_info_procesoshistoricosunpallet_nuevo
end type
type dw_1 from datawindow within w_info_procesoshistoricosunpallet_nuevo
end type
type uo_selclientes from uo_seleccion_clientesprod within w_info_procesoshistoricosunpallet_nuevo
end type
end forward

global type w_info_procesoshistoricosunpallet_nuevo from w_para_informes
integer x = 14
integer y = 32
integer width = 2505
integer height = 1088
string title = "Procesos Históricos Pallet"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_5 st_5
st_2 st_2
em_numero em_numero
st_6 st_6
cb_buscarepa cb_buscarepa
dw_1 dw_1
uo_selclientes uo_selclientes
end type
global w_info_procesoshistoricosunpallet_nuevo w_info_procesoshistoricosunpallet_nuevo

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_cliente
end variables

on w_info_procesoshistoricosunpallet_nuevo.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_5=create st_5
this.st_2=create st_2
this.em_numero=create em_numero
this.st_6=create st_6
this.cb_buscarepa=create cb_buscarepa
this.dw_1=create dw_1
this.uo_selclientes=create uo_selclientes
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_5
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_numero
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.cb_buscarepa
this.Control[iCurrent+7]=this.dw_1
this.Control[iCurrent+8]=this.uo_selclientes
end on

on w_info_procesoshistoricosunpallet_nuevo.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_numero)
destroy(this.st_6)
destroy(this.cb_buscarepa)
destroy(this.dw_1)
destroy(this.uo_selclientes)
end on

event open;call super::open;Boolean lb_Cerrar = False

If IsNull(uo_SelClientes.Codigo) Then lb_Cerrar = True


If lb_Cerrar Then
	Close(This)
Else
	uo_SelClientes.Seleccion(False, False)
	uo_SelClientes.Inicia(gi_CodExport)
	
	istr_mant.argumento[1]	=	String(gi_codexport)
	istr_mant.argumento[2]	=	'0'
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_procesoshistoricosunpallet_nuevo
end type

type st_computador from w_para_informes`st_computador within w_info_procesoshistoricosunpallet_nuevo
end type

type st_usuario from w_para_informes`st_usuario within w_info_procesoshistoricosunpallet_nuevo
end type

type st_temporada from w_para_informes`st_temporada within w_info_procesoshistoricosunpallet_nuevo
end type

type p_logo from w_para_informes`p_logo within w_info_procesoshistoricosunpallet_nuevo
end type

type st_titulo from w_para_informes`st_titulo within w_info_procesoshistoricosunpallet_nuevo
integer width = 1751
string text = "Procesos Históricos Pallet (Nuevo)"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_procesoshistoricosunpallet_nuevo
integer x = 2121
integer y = 420
integer taborder = 30
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	fila,li_cbx_planta,li_varirotula
Long		li_planta,li_cliente
String	ls_region

IF Long(istr_mant.argumento[2]) = 0 THEN
		MessageBox("Atención", "Falta Número del Pallet.", Exclamation!, Ok!)
		em_numero.SetFocus()
ELSE
	istr_info.titulo	= 'INFORME DETALLE DE COMPOSICIÓN DE PALLET.'
	OpenWithParm(vinf, istr_info)
	vinf.dw_1.DataObject = "dw_info_procesoshistoricosunpalletdet"
	vinf.dw_1.SetTransObject(sqlca)
	
	fila	=	vinf.dw_1.Retrieve(uo_SelClientes.Codigo, Long(istr_mant.Argumento[2]))
										
	IF fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
	ELSE
		F_Membrete(vinf.dw_1)
		IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_procesoshistoricosunpallet_nuevo
integer x = 2121
integer y = 688
integer taborder = 40
end type

type st_4 from statictext within w_info_procesoshistoricosunpallet_nuevo
integer x = 247
integer y = 416
integer width = 1751
integer height = 236
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_procesoshistoricosunpallet_nuevo
integer x = 247
integer y = 652
integer width = 1751
integer height = 228
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_procesoshistoricosunpallet_nuevo
integer x = 311
integer y = 728
integer width = 517
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Número Pallet"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_procesoshistoricosunpallet_nuevo
integer x = 750
integer y = 716
integer width = 466
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[2]	=	This.Text

end event

type st_6 from statictext within w_info_procesoshistoricosunpallet_nuevo
integer x = 302
integer y = 496
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type cb_buscarepa from commandbutton within w_info_procesoshistoricosunpallet_nuevo
integer x = 1234
integer y = 720
integer width = 91
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;istr_busq.argum[1]	=	istr_mant.argumento[1]
istr_busq.argum[5]   = ''

OpenWithParm(w_busc_palletencab, istr_busq)

istr_busq = Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	istr_mant.argumento[2] 	=	istr_busq.argum[2]
	em_numero.Text 			=	istr_busq.argum[2]
END IF
end event

type dw_1 from datawindow within w_info_procesoshistoricosunpallet_nuevo
boolean visible = false
integer x = 1952
integer y = 772
integer width = 151
integer height = 132
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_procesoshistoricosunpalletdet"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type uo_selclientes from uo_seleccion_clientesprod within w_info_procesoshistoricosunpallet_nuevo
integer x = 750
integer y = 480
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selclientes.destroy
call uo_seleccion_clientesprod::destroy
end on

