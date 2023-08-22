$PBExportHeader$w_info_condicioncalidad.srw
$PBExportComments$Informe_General_Planilla_Cuantitativa
forward
global type w_info_condicioncalidad from w_para_informes
end type
type gb_6 from groupbox within w_info_condicioncalidad
end type
type st_1 from statictext within w_info_condicioncalidad
end type
type st_33 from statictext within w_info_condicioncalidad
end type
type st_variedad from statictext within w_info_condicioncalidad
end type
type em_desde from editmask within w_info_condicioncalidad
end type
type st_embalaje from statictext within w_info_condicioncalidad
end type
type em_hasta from editmask within w_info_condicioncalidad
end type
type st_fembalaje from statictext within w_info_condicioncalidad
end type
type st_desdeins from statictext within w_info_condicioncalidad
end type
type st_hastains from statictext within w_info_condicioncalidad
end type
type cbx_fechaemba from checkbox within w_info_condicioncalidad
end type
type st_2 from statictext within w_info_condicioncalidad
end type
type st_7 from statictext within w_info_condicioncalidad
end type
type st_6 from statictext within w_info_condicioncalidad
end type
type st_44 from statictext within w_info_condicioncalidad
end type
type uo_selfrigo from uo_seleccion_frigorifico_mod within w_info_condicioncalidad
end type
type uo_selespecie from uo_seleccion_especie within w_info_condicioncalidad
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_info_condicioncalidad
end type
type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_condicioncalidad
end type
type uo_selembalaje from uo_seleccion_embalajesprod_mod within w_info_condicioncalidad
end type
type st_4 from statictext within w_info_condicioncalidad
end type
type gb_3 from groupbox within w_info_condicioncalidad
end type
type st_3 from statictext within w_info_condicioncalidad
end type
type rb_exis from radiobutton within w_info_condicioncalidad
end type
type rb_prod from radiobutton within w_info_condicioncalidad
end type
end forward

global type w_info_condicioncalidad from w_para_informes
integer x = 14
integer y = 32
integer width = 2519
integer height = 1940
string title = "Listado de Lotes "
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
gb_6 gb_6
st_1 st_1
st_33 st_33
st_variedad st_variedad
em_desde em_desde
st_embalaje st_embalaje
em_hasta em_hasta
st_fembalaje st_fembalaje
st_desdeins st_desdeins
st_hastains st_hastains
cbx_fechaemba cbx_fechaemba
st_2 st_2
st_7 st_7
st_6 st_6
st_44 st_44
uo_selfrigo uo_selfrigo
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
uo_selproductor uo_selproductor
uo_selembalaje uo_selembalaje
st_4 st_4
gb_3 gb_3
st_3 st_3
rb_exis rb_exis
rb_prod rb_prod
end type
global w_info_condicioncalidad w_info_condicioncalidad

type variables
str_busqueda	istr_busq
str_mant 		istr_mant

uo_ctlcaldanoespecie iuo_ctlcaldanoespecie
end variables

on w_info_condicioncalidad.create
int iCurrent
call super::create
this.gb_6=create gb_6
this.st_1=create st_1
this.st_33=create st_33
this.st_variedad=create st_variedad
this.em_desde=create em_desde
this.st_embalaje=create st_embalaje
this.em_hasta=create em_hasta
this.st_fembalaje=create st_fembalaje
this.st_desdeins=create st_desdeins
this.st_hastains=create st_hastains
this.cbx_fechaemba=create cbx_fechaemba
this.st_2=create st_2
this.st_7=create st_7
this.st_6=create st_6
this.st_44=create st_44
this.uo_selfrigo=create uo_selfrigo
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.uo_selproductor=create uo_selproductor
this.uo_selembalaje=create uo_selembalaje
this.st_4=create st_4
this.gb_3=create gb_3
this.st_3=create st_3
this.rb_exis=create rb_exis
this.rb_prod=create rb_prod
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_6
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_33
this.Control[iCurrent+4]=this.st_variedad
this.Control[iCurrent+5]=this.em_desde
this.Control[iCurrent+6]=this.st_embalaje
this.Control[iCurrent+7]=this.em_hasta
this.Control[iCurrent+8]=this.st_fembalaje
this.Control[iCurrent+9]=this.st_desdeins
this.Control[iCurrent+10]=this.st_hastains
this.Control[iCurrent+11]=this.cbx_fechaemba
this.Control[iCurrent+12]=this.st_2
this.Control[iCurrent+13]=this.st_7
this.Control[iCurrent+14]=this.st_6
this.Control[iCurrent+15]=this.st_44
this.Control[iCurrent+16]=this.uo_selfrigo
this.Control[iCurrent+17]=this.uo_selespecie
this.Control[iCurrent+18]=this.uo_selvariedad
this.Control[iCurrent+19]=this.uo_selproductor
this.Control[iCurrent+20]=this.uo_selembalaje
this.Control[iCurrent+21]=this.st_4
this.Control[iCurrent+22]=this.gb_3
this.Control[iCurrent+23]=this.st_3
this.Control[iCurrent+24]=this.rb_exis
this.Control[iCurrent+25]=this.rb_prod
end on

on w_info_condicioncalidad.destroy
call super::destroy
destroy(this.gb_6)
destroy(this.st_1)
destroy(this.st_33)
destroy(this.st_variedad)
destroy(this.em_desde)
destroy(this.st_embalaje)
destroy(this.em_hasta)
destroy(this.st_fembalaje)
destroy(this.st_desdeins)
destroy(this.st_hastains)
destroy(this.cbx_fechaemba)
destroy(this.st_2)
destroy(this.st_7)
destroy(this.st_6)
destroy(this.st_44)
destroy(this.uo_selfrigo)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.uo_selproductor)
destroy(this.uo_selembalaje)
destroy(this.st_4)
destroy(this.gb_3)
destroy(this.st_3)
destroy(this.rb_exis)
destroy(this.rb_prod)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelFrigo.Codigo) 		Then lb_Cerrar	=	True
If IsNull(uo_SelEspecie.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelVariedad.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelProductor.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelEmbalaje.Codigo) 	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelFrigo.Seleccion(True, False)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelEmbalaje.Seleccion(True, False)
		
	uo_SelEspecie.Inicia(11)
	uo_SelEspecie.Nombre	= uo_SelEspecie.Nombre
	uo_SelVariedad.Filtra(11)
	uo_SelFrigo.dw_Seleccion.Object.codigo.Dddw.PercentWidth	=	190

	uo_SelEmbalaje.Filtra(gi_CodExport, uo_SelEspecie.Codigo)

	em_Hasta.text		=	String(Today())
	em_Desde.text			=	String(Date(01/01/1999))
End If
end event

type st_computador from w_para_informes`st_computador within w_info_condicioncalidad
integer x = 1531
end type

type st_usuario from w_para_informes`st_usuario within w_info_condicioncalidad
integer x = 1531
end type

type st_temporada from w_para_informes`st_temporada within w_info_condicioncalidad
integer x = 1531
end type

type p_logo from w_para_informes`p_logo within w_info_condicioncalidad
end type

type st_titulo from w_para_informes`st_titulo within w_info_condicioncalidad
boolean visible = false
integer x = 3045
integer y = 2376
integer width = 224
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_condicioncalidad
string tag = "Imprimir Reporte"
integer x = 2176
integer y = 440
integer taborder = 370
integer weight = 400
fontcharset fontcharset = ansi!
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_fila

SetPointer(HourGlass!)

istr_info.titulo	= 'Listado de Lotes de [' + Upper(uo_SelEspecie.Nombre) + "]"
OpenWithParm(vinf,istr_info)

if rb_Exis.Checked Then
	vinf.dw_1.DataObject = "dw_info_listadolotes_exi"
Else
	vinf.dw_1.DataObject = "dw_info_listadolotes_pro"
	vinf.dw_1.ModIfy('DataWindow.Zoom = 90')
End If

vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(uo_SelFrigo.Codigo, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo,&
								  uo_SelProductor.Codigo, uo_SelEmbalaje.Codigo, Date(em_Desde.Text), Date(em_Hasta.Text))

If li_fila = -1 Then
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
End If

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_condicioncalidad
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2171
integer y = 764
integer taborder = 380
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_6 from groupbox within w_info_condicioncalidad
integer x = 384
integer y = 1420
integer width = 1563
integer height = 292
integer taborder = 120
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 8388608
long backcolor = 33543637
string text = " Tipo de Informe "
end type

type st_1 from statictext within w_info_condicioncalidad
integer x = 439
integer y = 376
integer width = 311
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_33 from statictext within w_info_condicioncalidad
integer x = 439
integer y = 724
integer width = 325
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_condicioncalidad
integer x = 439
integer y = 600
integer width = 279
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Variedad"
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_condicioncalidad
integer x = 827
integer y = 1184
integer width = 416
integer height = 80
integer taborder = 280
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;cbx_fechaemba.Checked = False
end event

type st_embalaje from statictext within w_info_condicioncalidad
integer x = 439
integer y = 852
integer width = 315
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_condicioncalidad
integer x = 1262
integer y = 1184
integer width = 411
integer height = 80
integer taborder = 290
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;cbx_fechaemba.Checked = False
end event

type st_fembalaje from statictext within w_info_condicioncalidad
integer x = 434
integer y = 1184
integer width = 338
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type st_desdeins from statictext within w_info_condicioncalidad
integer x = 928
integer y = 1124
integer width = 206
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type st_hastains from statictext within w_info_condicioncalidad
integer x = 1339
integer y = 1124
integer width = 178
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Hasta"
boolean focusrectangle = false
end type

type cbx_fechaemba from checkbox within w_info_condicioncalidad
integer x = 1733
integer y = 1188
integer width = 73
integer height = 64
integer taborder = 300
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean checked = true
end type

event clicked;If This.Checked Then
	em_Desde.Enabled=	False
	em_Hasta.Enabled	=	False
	em_Desde.Text 	=	String(Date("01/01/1999"))
	em_Hasta.text 		= 	String(Today())	
Else
	em_Desde.Enabled=	True
	em_Hasta.Enabled	=	True
	em_Desde.Text 	=	String(Today())
	em_Hasta.Text 	= 	String(Today())
	em_Desde.Setfocus()
End If
end event

type st_2 from statictext within w_info_condicioncalidad
integer x = 1733
integer y = 284
integer width = 178
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_condicioncalidad
integer x = 439
integer y = 488
integer width = 311
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Especie"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_condicioncalidad
integer x = 1664
integer y = 1128
integer width = 210
integer height = 52
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_44 from statictext within w_info_condicioncalidad
integer x = 338
integer y = 240
integer width = 1664
integer height = 792
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selfrigo from uo_seleccion_frigorifico_mod within w_info_condicioncalidad
event destroy ( )
integer x = 878
integer y = 348
integer width = 1042
integer height = 140
integer taborder = 20
boolean bringtotop = true
long backcolor = 33543637
end type

on uo_selfrigo.destroy
call uo_seleccion_frigorifico_mod::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_info_condicioncalidad
event destroy ( )
integer x = 878
integer y = 488
integer height = 84
integer taborder = 30
boolean bringtotop = true
long backcolor = 33543637
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_SelVariedad.Filtra(This.Codigo)
		uo_SelEmbalaje.Filtra(gi_CodExport, This.Codigo)
		
End Choose
end event

type uo_selvariedad from uo_seleccion_variedad_mod within w_info_condicioncalidad
event destroy ( )
integer x = 878
integer y = 572
integer width = 1042
integer taborder = 50
boolean bringtotop = true
long backcolor = 33543637
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_condicioncalidad
event destroy ( )
integer x = 878
integer y = 700
integer width = 1042
integer taborder = 390
boolean bringtotop = true
long backcolor = 33543637
end type

on uo_selproductor.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

type uo_selembalaje from uo_seleccion_embalajesprod_mod within w_info_condicioncalidad
integer x = 878
integer y = 828
integer width = 1042
integer taborder = 430
boolean bringtotop = true
long backcolor = 33543637
end type

on uo_selembalaje.destroy
call uo_seleccion_embalajesprod_mod::destroy
end on

type st_4 from statictext within w_info_condicioncalidad
integer x = 338
integer y = 1396
integer width = 1664
integer height = 360
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_condicioncalidad
integer x = 389
integer y = 1052
integer width = 1563
integer height = 292
integer taborder = 130
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 8388608
long backcolor = 33543637
string text = "Fechas "
end type

type st_3 from statictext within w_info_condicioncalidad
integer x = 338
integer y = 1032
integer width = 1664
integer height = 360
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_exis from radiobutton within w_info_condicioncalidad
integer x = 581
integer y = 1536
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 8388608
long backcolor = 33543637
string text = "Existencia"
boolean checked = true
end type

type rb_prod from radiobutton within w_info_condicioncalidad
integer x = 1349
integer y = 1536
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 8388608
long backcolor = 33543637
string text = "Producción"
end type

