$PBExportHeader$w_info_factura_guias.srw
forward
global type w_info_factura_guias from w_para_informes
end type
type rb_1 from radiobutton within w_info_factura_guias
end type
type rb_2 from radiobutton within w_info_factura_guias
end type
type rb_3 from radiobutton within w_info_factura_guias
end type
type rb_4 from radiobutton within w_info_factura_guias
end type
type em_1 from editmask within w_info_factura_guias
end type
type st_1 from statictext within w_info_factura_guias
end type
type gb_3 from groupbox within w_info_factura_guias
end type
type st_4 from statictext within w_info_factura_guias
end type
end forward

global type w_info_factura_guias from w_para_informes
integer x = 14
integer y = 32
integer width = 2501
integer height = 1244
string title = "Informes Guías Valorizadas"
boolean minbox = false
string icon = "F:\Desarrollo\Productiva\FrutaProcesada\Informes.ico"
rb_1 rb_1
rb_2 rb_2
rb_3 rb_3
rb_4 rb_4
em_1 em_1
st_1 st_1
gb_3 gb_3
st_4 st_4
end type
global w_info_factura_guias w_info_factura_guias

type variables
Integer codigo
String  nombre

str_mant istr_mant

end variables

on w_info_factura_guias.create
int iCurrent
call super::create
this.rb_1=create rb_1
this.rb_2=create rb_2
this.rb_3=create rb_3
this.rb_4=create rb_4
this.em_1=create em_1
this.st_1=create st_1
this.gb_3=create gb_3
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_1
this.Control[iCurrent+2]=this.rb_2
this.Control[iCurrent+3]=this.rb_3
this.Control[iCurrent+4]=this.rb_4
this.Control[iCurrent+5]=this.em_1
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.gb_3
this.Control[iCurrent+8]=this.st_4
end on

on w_info_factura_guias.destroy
call super::destroy
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.rb_3)
destroy(this.rb_4)
destroy(this.em_1)
destroy(this.st_1)
destroy(this.gb_3)
destroy(this.st_4)
end on

event open;
istr_mant = Message.PowerObjectParm





end event

type pb_excel from w_para_informes`pb_excel within w_info_factura_guias
end type

type st_computador from w_para_informes`st_computador within w_info_factura_guias
end type

type st_usuario from w_para_informes`st_usuario within w_info_factura_guias
end type

type st_temporada from w_para_informes`st_temporada within w_info_factura_guias
end type

type p_logo from w_para_informes`p_logo within w_info_factura_guias
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_factura_guias
integer width = 1751
string text = "Informe Guías Valorizadas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_factura_guias
integer x = 2130
integer y = 388
integer taborder = 50
end type

event pb_acepta::clicked;Date	ld_desde, ld_hasta
Long		ll_Filas, ll_factura
Integer	li_tipo

SetPointer(HourGlass!)

str_info	lstr_info

lstr_info.titulo	= "FACTURACION GUIAS DE DESPACHO"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_facturacion_guias"

vinf.dw_1.SetTransObject(sqlca)

ld_desde = Date(istr_mant.argumento[5])
ld_hasta = Date(istr_mant.argumento[6])

If rb_1.Checked Then
	ll_factura = Long(em_1.Text)
	li_tipo = 0
ElseIf	rb_2.Checked Then
	ll_factura = -1
	li_tipo = 1
ElseIf	rb_3.Checked Then
	ll_factura = 0
	li_tipo = 2
ElseIf	rb_4.Checked Then
	ll_factura = 0
	li_tipo = 3	
End If	
	
ll_Filas	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[2]),integer(istr_mant.argumento[1]),ld_desde,ld_hasta,li_tipo,ll_factura)

If ll_Filas = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Filas = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	vinf.dw_1.Modify("t_desde.text = '" + String(Date(istr_mant.argumento[5])) + "'")
	vinf.dw_1.Modify("t_hasta.text = '" + String(Date(istr_mant.argumento[6])) + "'")	
End If

SetPointer(Arrow!)	
end event

type pb_salir from w_para_informes`pb_salir within w_info_factura_guias
integer x = 2126
integer y = 656
integer taborder = 60
end type

type rb_1 from radiobutton within w_info_factura_guias
integer x = 315
integer y = 768
integer width = 229
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Una"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_1.Enabled = True
	em_1.SetFocus()
END IF	
end event

type rb_2 from radiobutton within w_info_factura_guias
integer x = 617
integer y = 772
integer width = 311
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Todas"
end type

event clicked;IF This.Checked THEN
	em_1.Enabled = False
	em_1.Text = ''
END IF	
end event

type rb_3 from radiobutton within w_info_factura_guias
integer x = 965
integer y = 772
integer width = 530
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Sin Factura"
end type

event clicked;IF This.Checked THEN
	em_1.Enabled = False
	em_1.Text = ''
END IF	
end event

type rb_4 from radiobutton within w_info_factura_guias
integer x = 1499
integer y = 768
integer width = 453
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Facturadas"
end type

event clicked;IF This.Checked THEN
	em_1.Enabled = False
	em_1.Text = ''
END IF	
end event

type em_1 from editmask within w_info_factura_guias
integer x = 722
integer y = 516
integer width = 434
integer height = 112
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type st_1 from statictext within w_info_factura_guias
integer x = 325
integer y = 540
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Nº Factura"
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_factura_guias
integer x = 283
integer y = 680
integer width = 1691
integer height = 224
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Selección"
end type

type st_4 from statictext within w_info_factura_guias
integer x = 251
integer y = 440
integer width = 1751
integer height = 520
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554431
long backcolor = 33554431
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

