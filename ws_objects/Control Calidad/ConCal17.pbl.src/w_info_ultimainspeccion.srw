$PBExportHeader$w_info_ultimainspeccion.srw
$PBExportComments$Ventana de Consulta ultima inspeccion
forward
global type w_info_ultimainspeccion from w_para_informes
end type
type gb_6 from groupbox within w_info_ultimainspeccion
end type
type st_1 from statictext within w_info_ultimainspeccion
end type
type st_33 from statictext within w_info_ultimainspeccion
end type
type st_variedad from statictext within w_info_ultimainspeccion
end type
type st_packing from statictext within w_info_ultimainspeccion
end type
type em_desde from editmask within w_info_ultimainspeccion
end type
type st_embalaje from statictext within w_info_ultimainspeccion
end type
type em_hasta from editmask within w_info_ultimainspeccion
end type
type st_desdeins from statictext within w_info_ultimainspeccion
end type
type st_hastains from statictext within w_info_ultimainspeccion
end type
type cbx_fechaemba from checkbox within w_info_ultimainspeccion
end type
type st_2 from statictext within w_info_ultimainspeccion
end type
type st_5 from statictext within w_info_ultimainspeccion
end type
type st_7 from statictext within w_info_ultimainspeccion
end type
type st_44 from statictext within w_info_ultimainspeccion
end type
type uo_selfrigo from uo_seleccion_frigorifico_mod within w_info_ultimainspeccion
end type
type uo_selespecie from uo_seleccion_especie within w_info_ultimainspeccion
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_info_ultimainspeccion
end type
type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_ultimainspeccion
end type
type uo_selpacking from uo_seleccion_frigopacking_mod within w_info_ultimainspeccion
end type
type uo_selembalaje from uo_seleccion_embalajesprod_mod within w_info_ultimainspeccion
end type
type uo_selcalibre from uo_seleccion_variecalibre_mod within w_info_ultimainspeccion
end type
type st_3 from statictext within w_info_ultimainspeccion
end type
type rb_calidad from radiobutton within w_info_ultimainspeccion
end type
type rb_existencia from radiobutton within w_info_ultimainspeccion
end type
type st_8 from statictext within w_info_ultimainspeccion
end type
type em_desde1 from editmask within w_info_ultimainspeccion
end type
type st_9 from statictext within w_info_ultimainspeccion
end type
type em_hasta1 from editmask within w_info_ultimainspeccion
end type
type cbx_fecha from checkbox within w_info_ultimainspeccion
end type
type gb_3 from groupbox within w_info_ultimainspeccion
end type
type st_4 from statictext within w_info_ultimainspeccion
end type
end forward

global type w_info_ultimainspeccion from w_para_informes
integer x = 14
integer y = 32
integer width = 2464
integer height = 2448
string title = "Ultima Inspeccion"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
gb_6 gb_6
st_1 st_1
st_33 st_33
st_variedad st_variedad
st_packing st_packing
em_desde em_desde
st_embalaje st_embalaje
em_hasta em_hasta
st_desdeins st_desdeins
st_hastains st_hastains
cbx_fechaemba cbx_fechaemba
st_2 st_2
st_5 st_5
st_7 st_7
st_44 st_44
uo_selfrigo uo_selfrigo
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
uo_selproductor uo_selproductor
uo_selpacking uo_selpacking
uo_selembalaje uo_selembalaje
uo_selcalibre uo_selcalibre
st_3 st_3
rb_calidad rb_calidad
rb_existencia rb_existencia
st_8 st_8
em_desde1 em_desde1
st_9 st_9
em_hasta1 em_hasta1
cbx_fecha cbx_fecha
gb_3 gb_3
st_4 st_4
end type
global w_info_ultimainspeccion w_info_ultimainspeccion

type variables
str_busqueda	istr_busq
str_mant 		istr_mant

uo_ctlcaldanoespecie iuo_ctlcaldanoespecie
end variables

on w_info_ultimainspeccion.create
int iCurrent
call super::create
this.gb_6=create gb_6
this.st_1=create st_1
this.st_33=create st_33
this.st_variedad=create st_variedad
this.st_packing=create st_packing
this.em_desde=create em_desde
this.st_embalaje=create st_embalaje
this.em_hasta=create em_hasta
this.st_desdeins=create st_desdeins
this.st_hastains=create st_hastains
this.cbx_fechaemba=create cbx_fechaemba
this.st_2=create st_2
this.st_5=create st_5
this.st_7=create st_7
this.st_44=create st_44
this.uo_selfrigo=create uo_selfrigo
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.uo_selproductor=create uo_selproductor
this.uo_selpacking=create uo_selpacking
this.uo_selembalaje=create uo_selembalaje
this.uo_selcalibre=create uo_selcalibre
this.st_3=create st_3
this.rb_calidad=create rb_calidad
this.rb_existencia=create rb_existencia
this.st_8=create st_8
this.em_desde1=create em_desde1
this.st_9=create st_9
this.em_hasta1=create em_hasta1
this.cbx_fecha=create cbx_fecha
this.gb_3=create gb_3
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_6
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_33
this.Control[iCurrent+4]=this.st_variedad
this.Control[iCurrent+5]=this.st_packing
this.Control[iCurrent+6]=this.em_desde
this.Control[iCurrent+7]=this.st_embalaje
this.Control[iCurrent+8]=this.em_hasta
this.Control[iCurrent+9]=this.st_desdeins
this.Control[iCurrent+10]=this.st_hastains
this.Control[iCurrent+11]=this.cbx_fechaemba
this.Control[iCurrent+12]=this.st_2
this.Control[iCurrent+13]=this.st_5
this.Control[iCurrent+14]=this.st_7
this.Control[iCurrent+15]=this.st_44
this.Control[iCurrent+16]=this.uo_selfrigo
this.Control[iCurrent+17]=this.uo_selespecie
this.Control[iCurrent+18]=this.uo_selvariedad
this.Control[iCurrent+19]=this.uo_selproductor
this.Control[iCurrent+20]=this.uo_selpacking
this.Control[iCurrent+21]=this.uo_selembalaje
this.Control[iCurrent+22]=this.uo_selcalibre
this.Control[iCurrent+23]=this.st_3
this.Control[iCurrent+24]=this.rb_calidad
this.Control[iCurrent+25]=this.rb_existencia
this.Control[iCurrent+26]=this.st_8
this.Control[iCurrent+27]=this.em_desde1
this.Control[iCurrent+28]=this.st_9
this.Control[iCurrent+29]=this.em_hasta1
this.Control[iCurrent+30]=this.cbx_fecha
this.Control[iCurrent+31]=this.gb_3
this.Control[iCurrent+32]=this.st_4
end on

on w_info_ultimainspeccion.destroy
call super::destroy
destroy(this.gb_6)
destroy(this.st_1)
destroy(this.st_33)
destroy(this.st_variedad)
destroy(this.st_packing)
destroy(this.em_desde)
destroy(this.st_embalaje)
destroy(this.em_hasta)
destroy(this.st_desdeins)
destroy(this.st_hastains)
destroy(this.cbx_fechaemba)
destroy(this.st_2)
destroy(this.st_5)
destroy(this.st_7)
destroy(this.st_44)
destroy(this.uo_selfrigo)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.uo_selproductor)
destroy(this.uo_selpacking)
destroy(this.uo_selembalaje)
destroy(this.uo_selcalibre)
destroy(this.st_3)
destroy(this.rb_calidad)
destroy(this.rb_existencia)
destroy(this.st_8)
destroy(this.em_desde1)
destroy(this.st_9)
destroy(this.em_hasta1)
destroy(this.cbx_fecha)
destroy(this.gb_3)
destroy(this.st_4)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelFrigo.Codigo) 		Then lb_Cerrar	=	True
If IsNull(uo_SelEspecie.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelVariedad.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelProductor.Codigo)	Then lb_Cerrar=	True
If IsNull(uo_SelPacking.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelEmbalaje.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelCalibre.Codigo) 		Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelFrigo.Seleccion(True, False)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelPacking.Seleccion(True, False)
	uo_SelEmbalaje.Seleccion(True, False)
	uo_SelCalibre.Seleccion(True, False)
		
	uo_SelEspecie.dw_Seleccion.Object.codigo[1]	= 11
	uo_SelEspecie.Codigo = 11
	uo_SelVariedad.Filtra(11)
	
	uo_SelFrigo.dw_Seleccion.Object.codigo.Dddw.PercentWidth	=	190	
	uo_SelEmbalaje.Filtra(gi_CodExport, uo_SelEspecie.Codigo)
	
	iuo_ctlcaldanoespecie =  Create uo_ctlcaldanoespecie

	em_Desde.text		=	'01/01/1900'
	em_Hasta.text		=	String(Today())
	em_Desde1.text		=	'01/01/1900'
	em_Hasta1.text		=	String(Today())
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_ultimainspeccion
end type

type st_computador from w_para_informes`st_computador within w_info_ultimainspeccion
end type

type st_usuario from w_para_informes`st_usuario within w_info_ultimainspeccion
end type

type st_temporada from w_para_informes`st_temporada within w_info_ultimainspeccion
end type

type p_logo from w_para_informes`p_logo within w_info_ultimainspeccion
end type

type st_titulo from w_para_informes`st_titulo within w_info_ultimainspeccion
integer width = 1664
string text = "Ultima Inspeccion"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_ultimainspeccion
string tag = "Imprimir Reporte"
integer x = 2121
integer y = 744
integer taborder = 370
integer weight = 400
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;Integer	li_fila, li_Estado

SetPointer(HourGlass!)

If rb_Existencia.Checked Then
	li_Estado = 1
	istr_info.titulo = 'Existencia de fruta embalada con segregacion de Calidad'
ElseIf rb_Calidad.Checked Then
	li_Estado = -1
	istr_info.titulo = 'Producto terminado con segregacion de Calidad'
End If

//istr_info.titulo	= 'Ultima Inspeccion ' + Upper(uo_SelEspecie.Nombre)
OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_ultimainspeccioncon"	
vinf.dw_1.SetTransObject(sqlca)
	
li_fila = vinf.dw_1.Retrieve(uo_SelEspecie.Codigo, uo_SelVariedad.Codigo, uo_SelProductor.Codigo, uo_SelFrigo.Codigo, &
			                        uo_SelPacking.Codigo, uo_SelCalibre.Codigo, uo_SelEmbalaje.Codigo, Date(em_Desde1.Text), &
							Date(em_Hasta1.Text), li_Estado, Date(em_Desde.Text), Date(em_Hasta.Text))

If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_ultimainspeccion
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2117
integer y = 1052
integer taborder = 380
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_6 from groupbox within w_info_ultimainspeccion
integer x = 302
integer y = 1580
integer width = 1563
integer height = 240
integer taborder = 120
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha inspección "
end type

type st_1 from statictext within w_info_ultimainspeccion
integer x = 352
integer y = 908
integer width = 311
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_33 from statictext within w_info_ultimainspeccion
integer x = 352
integer y = 768
integer width = 325
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_ultimainspeccion
integer x = 352
integer y = 636
integer width = 279
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Variedad"
boolean focusrectangle = false
end type

type st_packing from statictext within w_info_ultimainspeccion
integer x = 352
integer y = 1044
integer width = 247
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Packing"
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_ultimainspeccion
integer x = 416
integer y = 1700
integer width = 402
integer height = 80
integer taborder = 280
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_embalaje from statictext within w_info_ultimainspeccion
integer x = 352
integer y = 1180
integer width = 315
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_ultimainspeccion
integer x = 1029
integer y = 1704
integer width = 402
integer height = 80
integer taborder = 290
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_desdeins from statictext within w_info_ultimainspeccion
integer x = 416
integer y = 1648
integer width = 402
integer height = 56
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Desde"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_hastains from statictext within w_info_ultimainspeccion
integer x = 1029
integer y = 1648
integer width = 402
integer height = 56
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Hasta"
alignment alignment = center!
boolean focusrectangle = false
end type

type cbx_fechaemba from checkbox within w_info_ultimainspeccion
integer x = 1499
integer y = 1712
integer width = 256
integer height = 64
integer taborder = 300
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;If This.Checked Then
	em_Desde.Enabled		=	False
	em_Hasta.Enabled		=	False
	em_Desde.text				=	'01/01/1900'
	em_Hasta.text				=	String(Today())
Else
	em_Desde.Enabled		=	True
	em_Hasta.Enabled		=	True
	em_Desde.Text 			=	String(Today())
	em_Hasta.Text 			= 	String(Today())
	em_Desde.Setfocus()
End If
end event

type st_2 from statictext within w_info_ultimainspeccion
integer x = 1618
integer y = 456
integer width = 197
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_ultimainspeccion
integer x = 352
integer y = 1316
integer width = 315
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Calibre"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_ultimainspeccion
integer x = 352
integer y = 524
integer width = 311
integer height = 72
boolean bringtotop = true
integer textsize = -9
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

type st_44 from statictext within w_info_ultimainspeccion
integer x = 251
integer y = 440
integer width = 1664
integer height = 1120
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selfrigo from uo_seleccion_frigorifico_mod within w_info_ultimainspeccion
event destroy ( )
integer x = 791
integer y = 876
integer width = 1042
integer height = 140
integer taborder = 20
boolean bringtotop = true
end type

on uo_selfrigo.destroy
call uo_seleccion_frigorifico_mod::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_info_ultimainspeccion
event destroy ( )
integer x = 791
integer y = 520
integer height = 84
integer taborder = 30
boolean bringtotop = true
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
		uo_SelCalibre.Filtra(This.Codigo, -1)
		
End Choose
end event

type uo_selvariedad from uo_seleccion_variedad_mod within w_info_ultimainspeccion
event destroy ( )
integer x = 791
integer y = 608
integer width = 1042
integer taborder = 50
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelCalibre.Filtra(uo_SelEspecie.Codigo, This.Codigo)
		uo_SelCalibre.Todos(True)
		uo_SelCalibre.Bloquear(True)
		
	Case Else
		uo_SelCalibre.Filtra(uo_SelEspecie.Codigo, This.Codigo)
		uo_SelCalibre.Bloquear(False)
		
End Choose
end event

type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_ultimainspeccion
event destroy ( )
integer x = 791
integer y = 740
integer width = 1042
integer taborder = 390
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

type uo_selpacking from uo_seleccion_frigopacking_mod within w_info_ultimainspeccion
integer x = 791
integer y = 1016
integer width = 1042
integer taborder = 420
boolean bringtotop = true
end type

on uo_selpacking.destroy
call uo_seleccion_frigopacking_mod::destroy
end on

type uo_selembalaje from uo_seleccion_embalajesprod_mod within w_info_ultimainspeccion
integer x = 791
integer y = 1152
integer width = 1042
integer taborder = 430
boolean bringtotop = true
end type

on uo_selembalaje.destroy
call uo_seleccion_embalajesprod_mod::destroy
end on

type uo_selcalibre from uo_seleccion_variecalibre_mod within w_info_ultimainspeccion
integer x = 791
integer y = 1284
integer width = 1042
integer height = 128
integer taborder = 70
boolean bringtotop = true
end type

on uo_selcalibre.destroy
call uo_seleccion_variecalibre_mod::destroy
end on

type st_3 from statictext within w_info_ultimainspeccion
integer x = 352
integer y = 1452
integer width = 384
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Tipo Informe"
boolean focusrectangle = false
end type

type rb_calidad from radiobutton within w_info_ultimainspeccion
integer x = 773
integer y = 1448
integer width = 571
integer height = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Total producción"
boolean checked = true
end type

type rb_existencia from radiobutton within w_info_ultimainspeccion
integer x = 1449
integer y = 1456
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Existencia"
end type

type st_8 from statictext within w_info_ultimainspeccion
integer x = 416
integer y = 1900
integer width = 402
integer height = 56
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Desde"
alignment alignment = center!
boolean focusrectangle = false
end type

type em_desde1 from editmask within w_info_ultimainspeccion
integer x = 416
integer y = 1956
integer width = 402
integer height = 80
integer taborder = 290
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_9 from statictext within w_info_ultimainspeccion
integer x = 1029
integer y = 1900
integer width = 402
integer height = 56
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Hasta"
alignment alignment = center!
boolean focusrectangle = false
end type

type em_hasta1 from editmask within w_info_ultimainspeccion
integer x = 1029
integer y = 1956
integer width = 402
integer height = 80
integer taborder = 290
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type cbx_fecha from checkbox within w_info_ultimainspeccion
integer x = 1499
integer y = 1964
integer width = 256
integer height = 64
integer taborder = 300
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;If This.Checked Then
	em_Desde1.Enabled		=	False
	em_Hasta1.Enabled		=	False
	em_Desde1.text			=	'01/01/1900'
	em_Hasta1.text			=	String(Today())
Else
	em_Desde1.Enabled		=	True
	em_Hasta1.Enabled		=	True
	em_Desde1.Text 			=	String(Today())
	em_Hasta1.Text 			= 	String(Today())
	em_Desde1.Setfocus()
End If
end event

type gb_3 from groupbox within w_info_ultimainspeccion
integer x = 297
integer y = 1828
integer width = 1563
integer height = 240
integer taborder = 130
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 16711680
string text = "Fecha embalaje "
end type

type st_4 from statictext within w_info_ultimainspeccion
integer x = 251
integer y = 1560
integer width = 1664
integer height = 556
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

