$PBExportHeader$w_info_despachos.srw
$PBExportComments$Informe de Existencia de Fruta Comercial en Camara por Productor y Calibre.
forward
global type w_info_despachos from w_para_informes
end type
type st_1 from statictext within w_info_despachos
end type
type uo_selproductor from uo_seleccion_productor within w_info_despachos
end type
type st_2 from statictext within w_info_despachos
end type
type st_3 from statictext within w_info_despachos
end type
type st_4 from statictext within w_info_despachos
end type
type uo_selplanta from uo_seleccion_plantas within w_info_despachos
end type
type st_6 from statictext within w_info_despachos
end type
type uo_selespecie from uo_seleccion_especie within w_info_despachos
end type
type st_11 from statictext within w_info_despachos
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_despachos
end type
type st_5 from statictext within w_info_despachos
end type
type st_7 from statictext within w_info_despachos
end type
type st_8 from statictext within w_info_despachos
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_despachos
end type
type st_9 from statictext within w_info_despachos
end type
type st_10 from statictext within w_info_despachos
end type
type em_desde from editmask within w_info_despachos
end type
type em_hasta from editmask within w_info_despachos
end type
type rb_comercial from radiobutton within w_info_despachos
end type
type st_12 from statictext within w_info_despachos
end type
type rb_exportacion from radiobutton within w_info_despachos
end type
type rb_ambas from radiobutton within w_info_despachos
end type
end forward

global type w_info_despachos from w_para_informes
integer x = 0
integer y = 0
integer width = 3456
integer height = 1476
string title = "INFORME DE TRASVASIJE"
st_1 st_1
uo_selproductor uo_selproductor
st_2 st_2
st_3 st_3
st_4 st_4
uo_selplanta uo_selplanta
st_6 st_6
uo_selespecie uo_selespecie
st_11 st_11
uo_selcliente uo_selcliente
st_5 st_5
st_7 st_7
st_8 st_8
uo_selvariedad uo_selvariedad
st_9 st_9
st_10 st_10
em_desde em_desde
em_hasta em_hasta
rb_comercial rb_comercial
st_12 st_12
rb_exportacion rb_exportacion
rb_ambas rb_ambas
end type
global w_info_despachos w_info_despachos

type variables

end variables

forward prototypes
public subroutine consolidalote ()
end prototypes

public subroutine consolidalote ();
end subroutine

on w_info_despachos.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_selproductor=create uo_selproductor
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.uo_selplanta=create uo_selplanta
this.st_6=create st_6
this.uo_selespecie=create uo_selespecie
this.st_11=create st_11
this.uo_selcliente=create uo_selcliente
this.st_5=create st_5
this.st_7=create st_7
this.st_8=create st_8
this.uo_selvariedad=create uo_selvariedad
this.st_9=create st_9
this.st_10=create st_10
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.rb_comercial=create rb_comercial
this.st_12=create st_12
this.rb_exportacion=create rb_exportacion
this.rb_ambas=create rb_ambas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selproductor
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.uo_selplanta
this.Control[iCurrent+7]=this.st_6
this.Control[iCurrent+8]=this.uo_selespecie
this.Control[iCurrent+9]=this.st_11
this.Control[iCurrent+10]=this.uo_selcliente
this.Control[iCurrent+11]=this.st_5
this.Control[iCurrent+12]=this.st_7
this.Control[iCurrent+13]=this.st_8
this.Control[iCurrent+14]=this.uo_selvariedad
this.Control[iCurrent+15]=this.st_9
this.Control[iCurrent+16]=this.st_10
this.Control[iCurrent+17]=this.em_desde
this.Control[iCurrent+18]=this.em_hasta
this.Control[iCurrent+19]=this.rb_comercial
this.Control[iCurrent+20]=this.st_12
this.Control[iCurrent+21]=this.rb_exportacion
this.Control[iCurrent+22]=this.rb_ambas
end on

on w_info_despachos.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selproductor)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.uo_selplanta)
destroy(this.st_6)
destroy(this.uo_selespecie)
destroy(this.st_11)
destroy(this.uo_selcliente)
destroy(this.st_5)
destroy(this.st_7)
destroy(this.st_8)
destroy(this.uo_selvariedad)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.rb_comercial)
destroy(this.st_12)
destroy(this.rb_exportacion)
destroy(this.rb_ambas)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelProductor.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelEspecie.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelVariedad.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelCliente.Seleccion(True, False)
	uo_SelPlanta.Seleccion(True,False)
	uo_SelProductor.Seleccion(True,False)
	uo_SelEspecie.Seleccion(True, False)	
	uo_SelVariedad.Seleccion(True,False)
	uo_SelProductor.Filtra(-1)
	em_desde.Text	= String(Today(), 'dd/mm/yyyy')
	em_hasta.Text	= String(Today(), 'dd/mm/yyyy')
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_despachos
integer x = 3099
integer y = 248
end type

type st_computador from w_para_informes`st_computador within w_info_despachos
integer x = 1006
integer width = 2405
end type

type st_usuario from w_para_informes`st_usuario within w_info_despachos
integer x = 1006
integer width = 2405
end type

type st_temporada from w_para_informes`st_temporada within w_info_despachos
integer x = 1006
integer width = 2405
end type

type p_logo from w_para_informes`p_logo within w_info_despachos
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_despachos
integer width = 2770
string text = "Informe Recepciones"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_despachos
integer x = 3099
integer y = 588
integer taborder = 120
integer textsize = -8
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila
INteger	li_Tipo

istr_info.titulo	= "INFORME DESPACHOS"
istr_info.copias	= 1
	
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_despachos"
vinf.dw_1.SetTransObject(sqlca)

If rb_Comercial.Checked Then 
	li_Tipo = 1
ElseIf rb_Exportacion.Checked Then 
	li_Tipo = 2
Else
	li_Tipo = -1
End If

ll_Fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, li_Tipo, -1, uo_SelProductor.Codigo, uo_SelEspecie.Codigo, &
							uo_SelVariedad.Codigo, DateTime(em_Desde.Text), DateTime(em_Hasta.Text))

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
//	vinf.dw_1.Object.DataWindow.Zoom = 95
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_despachos
integer x = 3104
integer y = 888
integer taborder = 140
integer textsize = -8
fontcharset fontcharset = ansi!
end type

type st_1 from statictext within w_info_despachos
integer x = 251
integer y = 416
integer width = 1394
integer height = 500
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selproductor from uo_seleccion_productor within w_info_despachos
integer x = 649
integer y = 928
integer taborder = 10
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type st_2 from statictext within w_info_despachos
integer x = 329
integer y = 1016
integer width = 338
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_despachos
integer x = 315
integer y = 544
integer width = 338
integer height = 64
boolean bringtotop = true
integer textsize = -8
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

type st_4 from statictext within w_info_despachos
integer x = 1705
integer y = 544
integer width = 320
integer height = 64
boolean bringtotop = true
integer textsize = -8
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

type uo_selplanta from uo_seleccion_plantas within w_info_despachos
integer x = 649
integer y = 672
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
	//	uo_SelCama.Todos(True)
		
		//uo_SelCama.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		//uo_SelCama.Filtra(This.Codigo)
		
		//uo_SelCama.cbx_Todos.Enabled	=	True
		
END CHOOSE

ConsolidaLote()
end event

type st_6 from statictext within w_info_despachos
integer x = 329
integer y = 772
integer width = 274
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_despachos
integer x = 2071
integer y = 440
integer height = 188
integer taborder = 40
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;If IsNull(This.Codigo) Then Return

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_SelVariedad.Todos(True)
		
	CASE ELSE
		uo_SelVariedad.Filtra(This.Codigo)
				
END CHOOSE


end event

type st_11 from statictext within w_info_despachos
integer x = 1646
integer y = 416
integer width = 1394
integer height = 500
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_despachos
event destroy ( )
integer x = 649
integer y = 444
integer taborder = 50
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_5 from statictext within w_info_despachos
integer x = 1705
integer y = 772
integer width = 320
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_despachos
integer x = 251
integer y = 916
integer width = 1394
integer height = 224
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_despachos
integer x = 1646
integer y = 916
integer width = 1394
integer height = 224
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selvariedad from uo_seleccion_variedad within w_info_despachos
integer x = 2071
integer y = 672
integer taborder = 30
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type st_9 from statictext within w_info_despachos
integer x = 1705
integer y = 1016
integer width = 219
integer height = 64
boolean bringtotop = true
integer textsize = -8
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

type st_10 from statictext within w_info_despachos
integer x = 2354
integer y = 1016
integer width = 201
integer height = 64
boolean bringtotop = true
integer textsize = -8
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

type em_desde from editmask within w_info_despachos
integer x = 1883
integer y = 1004
integer width = 443
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type em_hasta from editmask within w_info_despachos
integer x = 2546
integer y = 1000
integer width = 443
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type rb_comercial from radiobutton within w_info_despachos
integer x = 649
integer y = 1180
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
string text = "Comercial"
end type

type st_12 from statictext within w_info_despachos
integer x = 251
integer y = 1140
integer width = 2789
integer height = 172
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_exportacion from radiobutton within w_info_despachos
integer x = 1449
integer y = 1180
integer width = 430
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
string text = "Exportacion"
end type

type rb_ambas from radiobutton within w_info_despachos
integer x = 2277
integer y = 1180
integer width = 430
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
string text = "Ambas"
boolean checked = true
end type

