$PBExportHeader$w_info_sellosag.srw
forward
global type w_info_sellosag from w_para_informes
end type
type st_1 from statictext within w_info_sellosag
end type
type st_2 from statictext within w_info_sellosag
end type
type em_desde from editmask within w_info_sellosag
end type
type st_3 from statictext within w_info_sellosag
end type
type st_4 from statictext within w_info_sellosag
end type
type em_hasta from editmask within w_info_sellosag
end type
type st_5 from statictext within w_info_sellosag
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_sellosag
end type
type uo_selespecie from uo_seleccion_especie within w_info_sellosag
end type
type st_6 from statictext within w_info_sellosag
end type
type ddlb_tipo from dropdownlistbox within w_info_sellosag
end type
type cbx_todos from checkbox within w_info_sellosag
end type
end forward

global type w_info_sellosag from w_para_informes
integer width = 2574
integer height = 1380
string title = "Informe Sellos SAG"
string icon = "AppIcon!"
st_1 st_1
st_2 st_2
em_desde em_desde
st_3 st_3
st_4 st_4
em_hasta em_hasta
st_5 st_5
uo_selcliente uo_selcliente
uo_selespecie uo_selespecie
st_6 st_6
ddlb_tipo ddlb_tipo
cbx_todos cbx_todos
end type
global w_info_sellosag w_info_sellosag

type variables
Integer ii_Tipo = -1
end variables

on w_info_sellosag.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.st_3=create st_3
this.st_4=create st_4
this.em_hasta=create em_hasta
this.st_5=create st_5
this.uo_selcliente=create uo_selcliente
this.uo_selespecie=create uo_selespecie
this.st_6=create st_6
this.ddlb_tipo=create ddlb_tipo
this.cbx_todos=create cbx_todos
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_desde
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.em_hasta
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.uo_selcliente
this.Control[iCurrent+9]=this.uo_selespecie
this.Control[iCurrent+10]=this.st_6
this.Control[iCurrent+11]=this.ddlb_tipo
this.Control[iCurrent+12]=this.cbx_todos
end on

on w_info_sellosag.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.em_hasta)
destroy(this.st_5)
destroy(this.uo_selcliente)
destroy(this.uo_selespecie)
destroy(this.st_6)
destroy(this.ddlb_tipo)
destroy(this.cbx_todos)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(True, False)
	uo_SelEspecie.Seleccion(True, False)
	
	em_Desde.Text = String(Today(), 'dd/mm/yyyy')
	em_Hasta.Text = String(Today(), 'dd/mm/yyyy')
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_sellosag
end type

type st_computador from w_para_informes`st_computador within w_info_sellosag
end type

type st_usuario from w_para_informes`st_usuario within w_info_sellosag
end type

type st_temporada from w_para_informes`st_temporada within w_info_sellosag
end type

type p_logo from w_para_informes`p_logo within w_info_sellosag
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_sellosag
integer width = 1769
string text = "Sellos SAG"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_sellosag
integer x = 2217
integer y = 484
integer taborder = 40
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "INFORME SELLOS SAG"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_sellossag"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelEspecie.Codigo, ii_Tipo , Date(em_Desde.Text), Date(em_Hasta.Text))

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_sellosag
integer x = 2217
integer y = 780
integer taborder = 50
end type

type st_1 from statictext within w_info_sellosag
integer x = 247
integer y = 440
integer width = 1769
integer height = 788
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_sellosag
integer x = 302
integer y = 1028
integer width = 219
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
string text = "Desde"
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_sellosag
integer x = 590
integer y = 1020
integer width = 389
integer height = 100
integer taborder = 20
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
maskdatatype maskdatatype = datetimemask!
string mask = "dd/mm/yyyy"
end type

type st_3 from statictext within w_info_sellosag
integer x = 302
integer y = 756
integer width = 233
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_sellosag
integer x = 1051
integer y = 1028
integer width = 210
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
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_sellosag
integer x = 1312
integer y = 1020
integer width = 389
integer height = 100
integer taborder = 30
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
maskdatatype maskdatatype = datetimemask!
string mask = "dd/mm/yyyy"
end type

type st_5 from statictext within w_info_sellosag
integer x = 302
integer y = 556
integer width = 261
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

type uo_selcliente from uo_seleccion_clientesprod within w_info_sellosag
integer x = 750
integer y = 464
integer taborder = 50
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_info_sellosag
integer x = 750
integer y = 668
integer taborder = 20
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type st_6 from statictext within w_info_sellosag
integer x = 302
integer y = 896
integer width = 439
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
string text = "Tipo Despacho"
boolean focusrectangle = false
end type

type ddlb_tipo from dropdownlistbox within w_info_sellosag
integer x = 750
integer y = 880
integer width = 896
integer height = 400
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
boolean sorted = false
boolean vscrollbar = true
string item[] = {"Productos Agríc. de Export. Certificados","Productos Agr.Export. Cert. (USDA)","Fruta a ser Fumigada en U.S.A.","Fumigados","Fruta a ser Fumigada en México"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_Tipo  = Index
end event

type cbx_todos from checkbox within w_info_sellosag
integer x = 1687
integer y = 884
integer width = 297
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
string text = "Todos"
boolean checked = true
end type

event clicked;If This.Checked Then
	ddlb_tipo.Enabled = False
	ii_Tipo  = -1
Else
	ddlb_tipo.Enabled = True
	ii_Tipo  = 0
End If
end event

