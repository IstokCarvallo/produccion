$PBExportHeader$w_info_ctlcalpesonetoenpesofijo.srw
$PBExportComments$Ventana de Informe de Peso Promedio por Racimo
forward
global type w_info_ctlcalpesonetoenpesofijo from w_para_informes
end type
type gb_7 from groupbox within w_info_ctlcalpesonetoenpesofijo
end type
type em_desde from editmask within w_info_ctlcalpesonetoenpesofijo
end type
type em_hasta from editmask within w_info_ctlcalpesonetoenpesofijo
end type
type st_5 from statictext within w_info_ctlcalpesonetoenpesofijo
end type
type st_6 from statictext within w_info_ctlcalpesonetoenpesofijo
end type
type st_44 from statictext within w_info_ctlcalpesonetoenpesofijo
end type
type st_1 from statictext within w_info_ctlcalpesonetoenpesofijo
end type
type st_2 from statictext within w_info_ctlcalpesonetoenpesofijo
end type
type st_3 from statictext within w_info_ctlcalpesonetoenpesofijo
end type
type st_4 from statictext within w_info_ctlcalpesonetoenpesofijo
end type
type st_7 from statictext within w_info_ctlcalpesonetoenpesofijo
end type
type st_8 from statictext within w_info_ctlcalpesonetoenpesofijo
end type
type st_9 from statictext within w_info_ctlcalpesonetoenpesofijo
end type
type st_10 from statictext within w_info_ctlcalpesonetoenpesofijo
end type
type cbx_todosfecha from checkbox within w_info_ctlcalpesonetoenpesofijo
end type
type cbx_consfecha from checkbox within w_info_ctlcalpesonetoenpesofijo
end type
type uo_selzonas from uo_seleccion_zonas_mod within w_info_ctlcalpesonetoenpesofijo
end type
type uo_selespecies from uo_seleccion_especie within w_info_ctlcalpesonetoenpesofijo
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_info_ctlcalpesonetoenpesofijo
end type
type uo_selproductor from uo_seleccion_productor_mod within w_info_ctlcalpesonetoenpesofijo
end type
type uo_selpacking from uo_seleccion_frigopacking_mod within w_info_ctlcalpesonetoenpesofijo
end type
end forward

global type w_info_ctlcalpesonetoenpesofijo from w_para_informes
integer x = 14
integer y = 32
integer width = 3008
integer height = 1888
string title = "Informe de Peso Neto en Peso Fijo "
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
gb_7 gb_7
em_desde em_desde
em_hasta em_hasta
st_5 st_5
st_6 st_6
st_44 st_44
st_1 st_1
st_2 st_2
st_3 st_3
st_4 st_4
st_7 st_7
st_8 st_8
st_9 st_9
st_10 st_10
cbx_todosfecha cbx_todosfecha
cbx_consfecha cbx_consfecha
uo_selzonas uo_selzonas
uo_selespecies uo_selespecies
uo_selvariedad uo_selvariedad
uo_selproductor uo_selproductor
uo_selpacking uo_selpacking
end type
global w_info_ctlcalpesonetoenpesofijo w_info_ctlcalpesonetoenpesofijo

type variables

end variables

on w_info_ctlcalpesonetoenpesofijo.create
int iCurrent
call super::create
this.gb_7=create gb_7
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.st_5=create st_5
this.st_6=create st_6
this.st_44=create st_44
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.st_7=create st_7
this.st_8=create st_8
this.st_9=create st_9
this.st_10=create st_10
this.cbx_todosfecha=create cbx_todosfecha
this.cbx_consfecha=create cbx_consfecha
this.uo_selzonas=create uo_selzonas
this.uo_selespecies=create uo_selespecies
this.uo_selvariedad=create uo_selvariedad
this.uo_selproductor=create uo_selproductor
this.uo_selpacking=create uo_selpacking
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_7
this.Control[iCurrent+2]=this.em_desde
this.Control[iCurrent+3]=this.em_hasta
this.Control[iCurrent+4]=this.st_5
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.st_44
this.Control[iCurrent+7]=this.st_1
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.st_3
this.Control[iCurrent+10]=this.st_4
this.Control[iCurrent+11]=this.st_7
this.Control[iCurrent+12]=this.st_8
this.Control[iCurrent+13]=this.st_9
this.Control[iCurrent+14]=this.st_10
this.Control[iCurrent+15]=this.cbx_todosfecha
this.Control[iCurrent+16]=this.cbx_consfecha
this.Control[iCurrent+17]=this.uo_selzonas
this.Control[iCurrent+18]=this.uo_selespecies
this.Control[iCurrent+19]=this.uo_selvariedad
this.Control[iCurrent+20]=this.uo_selproductor
this.Control[iCurrent+21]=this.uo_selpacking
end on

on w_info_ctlcalpesonetoenpesofijo.destroy
call super::destroy
destroy(this.gb_7)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.st_44)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.st_7)
destroy(this.st_8)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.cbx_todosfecha)
destroy(this.cbx_consfecha)
destroy(this.uo_selzonas)
destroy(this.uo_selespecies)
destroy(this.uo_selvariedad)
destroy(this.uo_selproductor)
destroy(this.uo_selpacking)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelZonas.Codigo)		Then lb_Cerrar	=	True
If IsNull(uo_SelProductor.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelPacking.Codigo)		Then lb_Cerrar	=	True
If IsNull(uo_SelEspecies.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelVariedad.Codigo)	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelZonas.Seleccion(True, True)
	uo_SelProductor.Seleccion(True, True)
	uo_SelPacking.Seleccion(True, True)
	uo_SelEspecies.Seleccion(False, False)
	uo_SelVariedad.Seleccion(True, True)
	
	uo_SelEspecies.Codigo = 11
	uo_SelEspecies.dw_Seleccion.Object.Codigo[1] = 11
	uo_SelVariedad.Filtra(11)
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_ctlcalpesonetoenpesofijo
integer x = 2450
integer y = 1112
end type

type st_computador from w_para_informes`st_computador within w_info_ctlcalpesonetoenpesofijo
end type

type st_usuario from w_para_informes`st_usuario within w_info_ctlcalpesonetoenpesofijo
end type

type st_temporada from w_para_informes`st_temporada within w_info_ctlcalpesonetoenpesofijo
end type

type p_logo from w_para_informes`p_logo within w_info_ctlcalpesonetoenpesofijo
end type

type st_titulo from w_para_informes`st_titulo within w_info_ctlcalpesonetoenpesofijo
integer width = 2048
string text = "Informe de Peso Neto en Peso Fijo "
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_ctlcalpesonetoenpesofijo
string tag = "Imprimir Reporte"
integer x = 2455
integer y = 548
integer taborder = 200
end type

event pb_acepta::clicked;Integer	li_fila, li_filas, li_ConsFechaEmb
Date		ld_FechaEmbaini, ld_FechaEmbafin

istr_info.titulo	= 'INFORME PESO NETO DE PESO FIJO'

OpenWithParm(vinf,istr_info)

If cbx_todosfecha.Checked Then
	ld_FechaEmbaini	=	Date(19000101)
	ld_FechaEmbafin	=	Today()
	em_desde.text 		=	String(Date(19000101))
	em_hasta.text 		=	String(Today())
ElseIf cbx_consfecha.Checked Then
	li_ConsFechaEmb	= 	1
	ld_FechaEmbaini 	=	Date(19000101)
	ld_FechaEmbafin 	=	Today()
Else
	ld_FechaEmbaini 	=	Date(em_desde.Text)
	ld_FechaEmbafin 	=	Date(em_hasta.Text)
End If	

If uo_SelEspecies.Codigo = 11 Then 
	vinf.dw_1.DataObject = "dw_info_ctlcalpesonetoenpesofijo"
Else
	vinf.dw_1.DataObject = "dw_info_ctlcalpesonetoenpesofijo_esp"
End If

vinf.dw_1.SetTransObject(sqlca)
	
li_fila = vinf.dw_1.Retrieve(gi_codexport, uo_SelZonas.Codigo, uo_SelProductor.Codigo, uo_SelPacking.Codigo,&	
									uo_SelEspecies.Codigo, uo_SelVariedad.Codigo,ld_FechaEmbaini,ld_FechaEmbafin,li_ConsFechaEmb)
		
If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No Existen pesos fuera de rango para su selección.",StopSign!, Ok!)
Else	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("desde.text = '" + em_desde.text + "'")
	vinf.dw_1.ModIfy("hasta.text = '" + em_hasta.text + "'")	
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_ctlcalpesonetoenpesofijo
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2450
integer y = 828
integer taborder = 210
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_7 from groupbox within w_info_ctlcalpesonetoenpesofijo
integer x = 288
integer y = 1256
integer width = 1911
integer height = 248
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
end type

type em_desde from editmask within w_info_ctlcalpesonetoenpesofijo
integer x = 809
integer y = 1364
integer width = 407
integer height = 88
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type em_hasta from editmask within w_info_ctlcalpesonetoenpesofijo
integer x = 1225
integer y = 1364
integer width = 407
integer height = 88
integer taborder = 190
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_5 from statictext within w_info_ctlcalpesonetoenpesofijo
integer x = 896
integer y = 1304
integer width = 192
integer height = 52
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

type st_6 from statictext within w_info_ctlcalpesonetoenpesofijo
integer x = 1312
integer y = 1304
integer width = 178
integer height = 52
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

type st_44 from statictext within w_info_ctlcalpesonetoenpesofijo
integer x = 251
integer y = 440
integer width = 2048
integer height = 1132
integer textsize = -10
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

type st_1 from statictext within w_info_ctlcalpesonetoenpesofijo
integer x = 1637
integer y = 492
integer width = 256
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
string text = "Todos"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_ctlcalpesonetoenpesofijo
integer x = 1915
integer y = 492
integer width = 201
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
string text = "Cons."
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_ctlcalpesonetoenpesofijo
integer x = 352
integer y = 584
integer width = 462
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
string text = "Zona Origen"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_ctlcalpesonetoenpesofijo
integer x = 352
integer y = 720
integer width = 297
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_ctlcalpesonetoenpesofijo
integer x = 352
integer y = 992
integer width = 297
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

type st_8 from statictext within w_info_ctlcalpesonetoenpesofijo
integer x = 352
integer y = 856
integer width = 297
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
string text = "Packing"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_ctlcalpesonetoenpesofijo
integer x = 352
integer y = 1128
integer width = 297
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
string text = "Variedad"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_ctlcalpesonetoenpesofijo
integer x = 325
integer y = 1360
integer width = 485
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Embalaje"
boolean focusrectangle = false
end type

type cbx_todosfecha from checkbox within w_info_ctlcalpesonetoenpesofijo
integer x = 1728
integer y = 1380
integer width = 91
integer height = 60
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
string text = " "
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_desde.Enabled		=	False
	em_hasta.Enabled	=	False
	cbx_consfecha.checked	=	FALSE
ELSE
	em_desde.Enabled	   =	TRUE
	em_hasta.Enabled	=	TRUE
	em_desde.Setfocus()
END IF

RETURN 0
end event

type cbx_consfecha from checkbox within w_info_ctlcalpesonetoenpesofijo
integer x = 1943
integer y = 1368
integer width = 82
integer height = 80
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
string text = " "
end type

event clicked;IF This.Checked THEN
	em_desde.Enabled		=	False
	em_hasta.Enabled	=	False
	cbx_todosfecha.checked = FALSE
ELSE
	em_desde.Enabled	   =	TRUE
	em_hasta.Enabled	=	TRUE
	em_desde.Setfocus()
END IF

RETURN 0
end event

type uo_selzonas from uo_seleccion_zonas_mod within w_info_ctlcalpesonetoenpesofijo
integer x = 823
integer y = 552
integer taborder = 130
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelPacking.Filtra(-1)
		
	Case Else
		uo_SelPacking.Filtra(This.Codigo)
		
End Choose
end event

on uo_selzonas.destroy
call uo_seleccion_zonas_mod::destroy
end on

type uo_selespecies from uo_seleccion_especie within w_info_ctlcalpesonetoenpesofijo
integer x = 823
integer y = 972
integer height = 84
integer taborder = 200
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelVariedad.Filtra(-1)
		
	Case Else
		uo_SelVariedad.Filtra(This.Codigo)
		
End Choose
end event

on uo_selespecies.destroy
call uo_seleccion_especie::destroy
end on

type uo_selvariedad from uo_seleccion_variedad_mod within w_info_ctlcalpesonetoenpesofijo
integer x = 823
integer y = 1080
integer taborder = 210
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type uo_selproductor from uo_seleccion_productor_mod within w_info_ctlcalpesonetoenpesofijo
integer x = 823
integer y = 684
integer taborder = 80
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

type uo_selpacking from uo_seleccion_frigopacking_mod within w_info_ctlcalpesonetoenpesofijo
integer x = 823
integer y = 816
integer taborder = 90
boolean bringtotop = true
end type

on uo_selpacking.destroy
call uo_seleccion_frigopacking_mod::destroy
end on

