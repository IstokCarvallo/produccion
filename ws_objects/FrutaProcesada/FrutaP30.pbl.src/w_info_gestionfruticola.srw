$PBExportHeader$w_info_gestionfruticola.srw
forward
global type w_info_gestionfruticola from w_para_informes
end type
type rb_1 from radiobutton within w_info_gestionfruticola
end type
type rb_2 from radiobutton within w_info_gestionfruticola
end type
type gb_3 from groupbox within w_info_gestionfruticola
end type
type st_4 from statictext within w_info_gestionfruticola
end type
type uo_selespecie from uo_seleccion_especie within w_info_gestionfruticola
end type
type st_1 from statictext within w_info_gestionfruticola
end type
end forward

global type w_info_gestionfruticola from w_para_informes
integer x = 14
integer y = 32
integer width = 2501
integer height = 1244
string title = "Informes Guías Valorizadas"
boolean minbox = false
string icon = "F:\Desarrollo\Productiva\FrutaProcesada\Informes.ico"
rb_1 rb_1
rb_2 rb_2
gb_3 gb_3
st_4 st_4
uo_selespecie uo_selespecie
st_1 st_1
end type
global w_info_gestionfruticola w_info_gestionfruticola

type variables
Integer codigo
String  nombre

str_mant istr_mant

end variables

on w_info_gestionfruticola.create
int iCurrent
call super::create
this.rb_1=create rb_1
this.rb_2=create rb_2
this.gb_3=create gb_3
this.st_4=create st_4
this.uo_selespecie=create uo_selespecie
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_1
this.Control[iCurrent+2]=this.rb_2
this.Control[iCurrent+3]=this.gb_3
this.Control[iCurrent+4]=this.st_4
this.Control[iCurrent+5]=this.uo_selespecie
this.Control[iCurrent+6]=this.st_1
end on

on w_info_gestionfruticola.destroy
call super::destroy
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.gb_3)
destroy(this.st_4)
destroy(this.uo_selespecie)
destroy(this.st_1)
end on

event open;call super::open;Boolean lb_Cerrar = False

If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelEspecie.Seleccion(True, False)
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_gestionfruticola
end type

type st_computador from w_para_informes`st_computador within w_info_gestionfruticola
end type

type st_usuario from w_para_informes`st_usuario within w_info_gestionfruticola
end type

type st_temporada from w_para_informes`st_temporada within w_info_gestionfruticola
end type

type p_logo from w_para_informes`p_logo within w_info_gestionfruticola
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_gestionfruticola
integer width = 1751
string text = "Informe Gestión Fruticola"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_gestionfruticola
integer x = 2130
integer y = 384
integer taborder = 50
end type

event pb_acepta::clicked;Long		ll_Filas
SetPointer(HourGlass!)

str_info	lstr_info

lstr_info.titulo	= "FACTURACION GUIAS DE DESPACHO"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

If rb_1.Checked Then
	vinf.dw_1.DataObject = "dw_info_gestfru_despachos"
ElseIF	rb_2.Checked Then
	vinf.dw_1.DataObject = "dw_info_gestfru_recepciones"
End If

vinf.dw_1.SetTransObject(sqlca)
	
ll_Filas	=	vinf.dw_1.Retrieve(uo_SelEspecie.Codigo)

If ll_Filas = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Filas = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)	
end event

type pb_salir from w_para_informes`pb_salir within w_info_gestionfruticola
integer x = 2126
integer y = 656
integer taborder = 60
end type

type rb_1 from radiobutton within w_info_gestionfruticola
integer x = 567
integer y = 768
integer width = 393
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Despachos"
boolean checked = true
end type

type rb_2 from radiobutton within w_info_gestionfruticola
integer x = 1170
integer y = 772
integer width = 453
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Recepciones"
end type

type gb_3 from groupbox within w_info_gestionfruticola
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
long textcolor = 16777215
long backcolor = 553648127
string text = "Selección"
end type

type st_4 from statictext within w_info_gestionfruticola
integer x = 251
integer y = 440
integer width = 1751
integer height = 520
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_gestionfruticola
integer x = 782
integer y = 480
integer taborder = 60
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type st_1 from statictext within w_info_gestionfruticola
integer x = 389
integer y = 576
integer width = 402
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
string text = "Especie"
boolean focusrectangle = false
end type

