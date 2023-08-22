$PBExportHeader$w_info_existencia.srw
$PBExportComments$Informe de Existencia de Fruta Comercial en Camara por Productor y Calibre.
forward
global type w_info_existencia from w_para_informes
end type
type st_4 from statictext within w_info_existencia
end type
type uo_selespecie from uo_seleccion_especie within w_info_existencia
end type
type st_11 from statictext within w_info_existencia
end type
type st_5 from statictext within w_info_existencia
end type
type st_8 from statictext within w_info_existencia
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_existencia
end type
type st_9 from statictext within w_info_existencia
end type
type st_10 from statictext within w_info_existencia
end type
type em_desde from editmask within w_info_existencia
end type
type em_hasta from editmask within w_info_existencia
end type
type rb_comercial from radiobutton within w_info_existencia
end type
type rb_exportacion from radiobutton within w_info_existencia
end type
type rb_resumen from radiobutton within w_info_existencia
end type
type rb_detalle from radiobutton within w_info_existencia
end type
type rb_fecha from radiobutton within w_info_existencia
end type
type rb_categoria from radiobutton within w_info_existencia
end type
type rb_color from radiobutton within w_info_existencia
end type
type rb_ambos from radiobutton within w_info_existencia
end type
type gb_1 from groupbox within w_info_existencia
end type
type gb_2 from groupbox within w_info_existencia
end type
type gb_3 from groupbox within w_info_existencia
end type
end forward

global type w_info_existencia from w_para_informes
integer x = 0
integer y = 0
integer width = 2382
integer height = 1960
string title = "INFORME DE TRASVASIJE"
st_4 st_4
uo_selespecie uo_selespecie
st_11 st_11
st_5 st_5
st_8 st_8
uo_selvariedad uo_selvariedad
st_9 st_9
st_10 st_10
em_desde em_desde
em_hasta em_hasta
rb_comercial rb_comercial
rb_exportacion rb_exportacion
rb_resumen rb_resumen
rb_detalle rb_detalle
rb_fecha rb_fecha
rb_categoria rb_categoria
rb_color rb_color
rb_ambos rb_ambos
gb_1 gb_1
gb_2 gb_2
gb_3 gb_3
end type
global w_info_existencia w_info_existencia

type variables

end variables

forward prototypes
public subroutine consolidalote ()
end prototypes

public subroutine consolidalote ();
end subroutine

on w_info_existencia.create
int iCurrent
call super::create
this.st_4=create st_4
this.uo_selespecie=create uo_selespecie
this.st_11=create st_11
this.st_5=create st_5
this.st_8=create st_8
this.uo_selvariedad=create uo_selvariedad
this.st_9=create st_9
this.st_10=create st_10
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.rb_comercial=create rb_comercial
this.rb_exportacion=create rb_exportacion
this.rb_resumen=create rb_resumen
this.rb_detalle=create rb_detalle
this.rb_fecha=create rb_fecha
this.rb_categoria=create rb_categoria
this.rb_color=create rb_color
this.rb_ambos=create rb_ambos
this.gb_1=create gb_1
this.gb_2=create gb_2
this.gb_3=create gb_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.uo_selespecie
this.Control[iCurrent+3]=this.st_11
this.Control[iCurrent+4]=this.st_5
this.Control[iCurrent+5]=this.st_8
this.Control[iCurrent+6]=this.uo_selvariedad
this.Control[iCurrent+7]=this.st_9
this.Control[iCurrent+8]=this.st_10
this.Control[iCurrent+9]=this.em_desde
this.Control[iCurrent+10]=this.em_hasta
this.Control[iCurrent+11]=this.rb_comercial
this.Control[iCurrent+12]=this.rb_exportacion
this.Control[iCurrent+13]=this.rb_resumen
this.Control[iCurrent+14]=this.rb_detalle
this.Control[iCurrent+15]=this.rb_fecha
this.Control[iCurrent+16]=this.rb_categoria
this.Control[iCurrent+17]=this.rb_color
this.Control[iCurrent+18]=this.rb_ambos
this.Control[iCurrent+19]=this.gb_1
this.Control[iCurrent+20]=this.gb_2
this.Control[iCurrent+21]=this.gb_3
end on

on w_info_existencia.destroy
call super::destroy
destroy(this.st_4)
destroy(this.uo_selespecie)
destroy(this.st_11)
destroy(this.st_5)
destroy(this.st_8)
destroy(this.uo_selvariedad)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.rb_comercial)
destroy(this.rb_exportacion)
destroy(this.rb_resumen)
destroy(this.rb_detalle)
destroy(this.rb_fecha)
destroy(this.rb_categoria)
destroy(this.rb_color)
destroy(this.rb_ambos)
destroy(this.gb_1)
destroy(this.gb_2)
destroy(this.gb_3)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelEspecie.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelVariedad.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelEspecie.Seleccion(True, False)	
	uo_SelVariedad.Seleccion(True,False)
	em_desde.Text	= String(Today(), 'dd/mm/yyyy')
	em_hasta.Text	= String(Today(), 'dd/mm/yyyy')
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_existencia
integer x = 1865
integer y = 324
end type

type st_computador from w_para_informes`st_computador within w_info_existencia
integer x = 1006
integer width = 2405
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia
integer x = 1006
integer width = 2405
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia
integer x = 1006
integer width = 2405
end type

type p_logo from w_para_informes`p_logo within w_info_existencia
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia
integer width = 1394
string text = "Informe Existencia BINS"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia
integer x = 1865
integer y = 664
integer taborder = 120
integer textsize = -8
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila
INteger	li_Tipo, li_Detalle

istr_info.titulo	= "INFORME EXISTENCIA BINS"
istr_info.copias	= 1
	
OpenWithParm(vinf, istr_info)

If rb_Detalle.Checked Then 
	li_Detalle = 0
	If rb_Fecha.Checked Then
		vinf.dw_1.DataObject = "dw_info_existencia_Fecha"
	ElseIf rb_Categoria.Checked Then
		vinf.dw_1.DataObject = "dw_info_existencia_Cate"
	ElseIf rb_Color.Checked Then
		vinf.dw_1.DataObject = "dw_info_existencia_Color"
	End If
ElseIf rb_Resumen.Checked Then 
	li_Detalle = 1
	If rb_Fecha.Checked Then
		vinf.dw_1.DataObject = "dw_info_existencia_resu_Fecha"
	ElseIf rb_Categoria.Checked Then
		vinf.dw_1.DataObject = "dw_info_existencia_resu_Cate"
	ElseIf rb_Color.Checked Then
		vinf.dw_1.DataObject = "dw_info_existencia_resu_Color"
	End If
End If

vinf.dw_1.SetTransObject(sqlca)

If rb_Comercial.Checked Then 
	li_Tipo = 120
ElseIf rb_Exportacion.Checked Then 
	li_Tipo = 100
Else 
	li_Tipo = -1
End If

ll_Fila	=	vinf.dw_1.Retrieve(uo_SelEspecie.Codigo, uo_SelVariedad.Codigo, DateTime(em_Desde.Text), DateTime(em_Hasta.Text), li_Tipo, li_Detalle)

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

type pb_salir from w_para_informes`pb_salir within w_info_existencia
integer x = 1870
integer y = 964
integer taborder = 140
integer textsize = -8
fontcharset fontcharset = ansi!
end type

type st_4 from statictext within w_info_existencia
integer x = 306
integer y = 624
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

type uo_selespecie from uo_seleccion_especie within w_info_existencia
integer x = 672
integer y = 520
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

type st_11 from statictext within w_info_existencia
integer x = 251
integer y = 496
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

type st_5 from statictext within w_info_existencia
integer x = 306
integer y = 852
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

type st_8 from statictext within w_info_existencia
integer x = 251
integer y = 996
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

type uo_selvariedad from uo_seleccion_variedad within w_info_existencia
integer x = 672
integer y = 752
integer taborder = 30
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type st_9 from statictext within w_info_existencia
integer x = 306
integer y = 1096
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

type st_10 from statictext within w_info_existencia
integer x = 955
integer y = 1096
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

type em_desde from editmask within w_info_existencia
integer x = 485
integer y = 1084
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

type em_hasta from editmask within w_info_existencia
integer x = 1125
integer y = 1080
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

type rb_comercial from radiobutton within w_info_existencia
integer x = 311
integer y = 1276
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

type rb_exportacion from radiobutton within w_info_existencia
integer x = 773
integer y = 1276
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

type rb_resumen from radiobutton within w_info_existencia
integer x = 370
integer y = 1448
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
string text = "Resumen"
end type

type rb_detalle from radiobutton within w_info_existencia
integer x = 1065
integer y = 1448
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
string text = "Detalle"
boolean checked = true
end type

type rb_fecha from radiobutton within w_info_existencia
integer x = 357
integer y = 1660
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
string text = "Fecha"
boolean checked = true
end type

type rb_categoria from radiobutton within w_info_existencia
integer x = 741
integer y = 1660
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
string text = "Categoria"
end type

type rb_color from radiobutton within w_info_existencia
integer x = 1266
integer y = 1660
integer width = 279
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
string text = "Color"
end type

type rb_ambos from radiobutton within w_info_existencia
integer x = 1266
integer y = 1276
integer width = 315
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
string text = "Ambos"
boolean checked = true
end type

type gb_1 from groupbox within w_info_existencia
integer x = 251
integer y = 1220
integer width = 1394
integer height = 172
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
end type

type gb_2 from groupbox within w_info_existencia
integer x = 251
integer y = 1392
integer width = 1394
integer height = 172
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
end type

type gb_3 from groupbox within w_info_existencia
integer x = 251
integer y = 1564
integer width = 1394
integer height = 252
integer taborder = 50
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
string text = "Ordenado por"
end type

