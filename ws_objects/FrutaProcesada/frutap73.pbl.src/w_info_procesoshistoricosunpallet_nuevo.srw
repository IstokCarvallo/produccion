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
type cbx_varirotula from checkbox within w_info_procesoshistoricosunpallet_nuevo
end type
type cbx_prodrotula from checkbox within w_info_procesoshistoricosunpallet_nuevo
end type
type cbx_calrotula from checkbox within w_info_procesoshistoricosunpallet_nuevo
end type
type st_1 from statictext within w_info_procesoshistoricosunpallet_nuevo
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_procesoshistoricosunpallet_nuevo
end type
end forward

global type w_info_procesoshistoricosunpallet_nuevo from w_para_informes
integer x = 14
integer y = 32
integer width = 2464
integer height = 1552
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
cbx_varirotula cbx_varirotula
cbx_prodrotula cbx_prodrotula
cbx_calrotula cbx_calrotula
st_1 st_1
uo_selcliente uo_selcliente
end type
global w_info_procesoshistoricosunpallet_nuevo w_info_procesoshistoricosunpallet_nuevo

type variables
str_busqueda istr_busq
str_mant istr_mant
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
this.cbx_varirotula=create cbx_varirotula
this.cbx_prodrotula=create cbx_prodrotula
this.cbx_calrotula=create cbx_calrotula
this.st_1=create st_1
this.uo_selcliente=create uo_selcliente
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_5
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_numero
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.cb_buscarepa
this.Control[iCurrent+7]=this.cbx_varirotula
this.Control[iCurrent+8]=this.cbx_prodrotula
this.Control[iCurrent+9]=this.cbx_calrotula
this.Control[iCurrent+10]=this.st_1
this.Control[iCurrent+11]=this.uo_selcliente
end on

on w_info_procesoshistoricosunpallet_nuevo.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_numero)
destroy(this.st_6)
destroy(this.cb_buscarepa)
destroy(this.cbx_varirotula)
destroy(this.cbx_prodrotula)
destroy(this.cbx_calrotula)
destroy(this.st_1)
destroy(this.uo_selcliente)
end on

event open;call super::open;Boolean lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)

	istr_mant.Argumento[2]	=	'0'
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_procesoshistoricosunpallet_nuevo
end type

type st_computador from w_para_informes`st_computador within w_info_procesoshistoricosunpallet_nuevo
integer x = 1920
end type

type st_usuario from w_para_informes`st_usuario within w_info_procesoshistoricosunpallet_nuevo
integer x = 1920
end type

type st_temporada from w_para_informes`st_temporada within w_info_procesoshistoricosunpallet_nuevo
integer x = 1920
end type

type p_logo from w_para_informes`p_logo within w_info_procesoshistoricosunpallet_nuevo
end type

type st_titulo from w_para_informes`st_titulo within w_info_procesoshistoricosunpallet_nuevo
integer width = 1751
string text = "Procesos Históricos Pallet"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_procesoshistoricosunpallet_nuevo
integer x = 2121
integer y = 888
integer taborder = 60
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila,li_varirotula, li_prodrotula, li_calrotula

If Long(istr_mant.argumento[2]) = 0 Then
	MessageBox("Atención", "Falta Número del Pallet.", Exclamation!, Ok!)
	em_numero.SetFocus()
Else
		istr_info.titulo	= 'INFORME PROCESO HISTORICO UN PALLET.'
		OpenWithParm(vinf, istr_info)
		
		If cbx_varirotula.Checked Then
			li_varirotula = 1
		Else
			li_varirotula = 0
		End If

		If cbx_prodrotula.Checked Then
			li_prodrotula = 1
		Else
			li_prodrotula = 0
		End If
		
		If cbx_calrotula.Checked Then
			li_calrotula = 1
		Else
			li_calrotula = 0
		End If
		
		vinf.dw_1.DataObject = "dw_info_procesoshistoricosunpalletenc_nu"
		vinf.dw_1.SetTransObject(sqlca)
		fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, Long(istr_mant.Argumento[2]),li_varirotula,li_prodrotula,li_calrotula)
											
		If fila = -1 Then
			MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
		ElseIf fila = 0 Then
			MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
		Else
			F_Membrete(vinf.dw_1)
			vinf.dw_1.ModIfy("cliente.text = '" + uo_SelCliente.Nombre + "'")
			vinf.dw_1.ModIfy("pallet.text = '" + istr_mant.Argumento[2] + "'")			
			If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
		End If
End If

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_procesoshistoricosunpallet_nuevo
integer x = 2121
integer y = 1156
integer taborder = 70
end type

type st_4 from statictext within w_info_procesoshistoricosunpallet_nuevo
integer x = 242
integer y = 440
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
integer x = 242
integer y = 680
integer width = 1751
integer height = 392
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
integer x = 306
integer y = 756
integer width = 517
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Número Pallet"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_procesoshistoricosunpallet_nuevo
integer x = 745
integer y = 744
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
integer x = 297
integer y = 532
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type cb_buscarepa from commandbutton within w_info_procesoshistoricosunpallet_nuevo
integer x = 1230
integer y = 748
integer width = 91
integer height = 84
integer taborder = 30
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

type cbx_varirotula from checkbox within w_info_procesoshistoricosunpallet_nuevo
integer x = 745
integer y = 892
integer width = 626
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad Rotulada"
end type

type cbx_prodrotula from checkbox within w_info_procesoshistoricosunpallet_nuevo
integer x = 745
integer y = 1152
integer width = 1015
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor Rotulado del Cliente  "
end type

type cbx_calrotula from checkbox within w_info_procesoshistoricosunpallet_nuevo
integer x = 745
integer y = 968
integer width = 699
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Calibre Rotulado   "
end type

type st_1 from statictext within w_info_procesoshistoricosunpallet_nuevo
integer x = 242
integer y = 1072
integer width = 1751
integer height = 224
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

type uo_selcliente from uo_seleccion_clientesprod within w_info_procesoshistoricosunpallet_nuevo
integer x = 745
integer y = 516
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

