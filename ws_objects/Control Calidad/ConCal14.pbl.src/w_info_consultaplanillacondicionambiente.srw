$PBExportHeader$w_info_consultaplanillacondicionambiente.srw
$PBExportComments$Ventana de consulta planilla de condicion de Ambiente y Gases
forward
global type w_info_consultaplanillacondicionambiente from w_para_informes
end type
type em_desde from editmask within w_info_consultaplanillacondicionambiente
end type
type st_12 from statictext within w_info_consultaplanillacondicionambiente
end type
type st_13 from statictext within w_info_consultaplanillacondicionambiente
end type
type em_hasta from editmask within w_info_consultaplanillacondicionambiente
end type
type st_33 from statictext within w_info_consultaplanillacondicionambiente
end type
type st_14 from statictext within w_info_consultaplanillacondicionambiente
end type
type gb_3 from groupbox within w_info_consultaplanillacondicionambiente
end type
type st_44 from statictext within w_info_consultaplanillacondicionambiente
end type
type uo_selespecie from uo_seleccion_especie within w_info_consultaplanillacondicionambiente
end type
type st_1 from statictext within w_info_consultaplanillacondicionambiente
end type
type st_2 from statictext within w_info_consultaplanillacondicionambiente
end type
type st_3 from statictext within w_info_consultaplanillacondicionambiente
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_info_consultaplanillacondicionambiente
end type
type uo_selzonas from uo_seleccion_zonas_mod within w_info_consultaplanillacondicionambiente
end type
type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_consultaplanillacondicionambiente
end type
type uo_selinspectores from uo_seleccion_inspectores_mod within w_info_consultaplanillacondicionambiente
end type
type st_4 from statictext within w_info_consultaplanillacondicionambiente
end type
type uo_selpredio from uo_seleccion_predioproduc_mod within w_info_consultaplanillacondicionambiente
end type
type uo_selcuartel from uo_seleccion_prodcuarteles_mod within w_info_consultaplanillacondicionambiente
end type
type st_5 from statictext within w_info_consultaplanillacondicionambiente
end type
type st_6 from statictext within w_info_consultaplanillacondicionambiente
end type
type uo_selcamaras from uo_seleccion_camarasbode_mod within w_info_consultaplanillacondicionambiente
end type
type st_7 from statictext within w_info_consultaplanillacondicionambiente
end type
type uo_selplanta from uo_seleccion_planta_mod within w_info_consultaplanillacondicionambiente
end type
type st_8 from statictext within w_info_consultaplanillacondicionambiente
end type
end forward

global type w_info_consultaplanillacondicionambiente from w_para_informes
string tag = "Condición y Ambiente en Camara AC Kiwis"
integer x = 14
integer y = 32
integer width = 2546
integer height = 2276
string title = "Condición y Ambiente en Camara AC Kiwis"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
em_desde em_desde
st_12 st_12
st_13 st_13
em_hasta em_hasta
st_33 st_33
st_14 st_14
gb_3 gb_3
st_44 st_44
uo_selespecie uo_selespecie
st_1 st_1
st_2 st_2
st_3 st_3
uo_selvariedad uo_selvariedad
uo_selzonas uo_selzonas
uo_selproductor uo_selproductor
uo_selinspectores uo_selinspectores
st_4 st_4
uo_selpredio uo_selpredio
uo_selcuartel uo_selcuartel
st_5 st_5
st_6 st_6
uo_selcamaras uo_selcamaras
st_7 st_7
uo_selplanta uo_selplanta
st_8 st_8
end type
global w_info_consultaplanillacondicionambiente w_info_consultaplanillacondicionambiente

type variables



end variables

on w_info_consultaplanillacondicionambiente.create
int iCurrent
call super::create
this.em_desde=create em_desde
this.st_12=create st_12
this.st_13=create st_13
this.em_hasta=create em_hasta
this.st_33=create st_33
this.st_14=create st_14
this.gb_3=create gb_3
this.st_44=create st_44
this.uo_selespecie=create uo_selespecie
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.uo_selvariedad=create uo_selvariedad
this.uo_selzonas=create uo_selzonas
this.uo_selproductor=create uo_selproductor
this.uo_selinspectores=create uo_selinspectores
this.st_4=create st_4
this.uo_selpredio=create uo_selpredio
this.uo_selcuartel=create uo_selcuartel
this.st_5=create st_5
this.st_6=create st_6
this.uo_selcamaras=create uo_selcamaras
this.st_7=create st_7
this.uo_selplanta=create uo_selplanta
this.st_8=create st_8
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_desde
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.st_13
this.Control[iCurrent+4]=this.em_hasta
this.Control[iCurrent+5]=this.st_33
this.Control[iCurrent+6]=this.st_14
this.Control[iCurrent+7]=this.gb_3
this.Control[iCurrent+8]=this.st_44
this.Control[iCurrent+9]=this.uo_selespecie
this.Control[iCurrent+10]=this.st_1
this.Control[iCurrent+11]=this.st_2
this.Control[iCurrent+12]=this.st_3
this.Control[iCurrent+13]=this.uo_selvariedad
this.Control[iCurrent+14]=this.uo_selzonas
this.Control[iCurrent+15]=this.uo_selproductor
this.Control[iCurrent+16]=this.uo_selinspectores
this.Control[iCurrent+17]=this.st_4
this.Control[iCurrent+18]=this.uo_selpredio
this.Control[iCurrent+19]=this.uo_selcuartel
this.Control[iCurrent+20]=this.st_5
this.Control[iCurrent+21]=this.st_6
this.Control[iCurrent+22]=this.uo_selcamaras
this.Control[iCurrent+23]=this.st_7
this.Control[iCurrent+24]=this.uo_selplanta
this.Control[iCurrent+25]=this.st_8
end on

on w_info_consultaplanillacondicionambiente.destroy
call super::destroy
destroy(this.em_desde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_hasta)
destroy(this.st_33)
destroy(this.st_14)
destroy(this.gb_3)
destroy(this.st_44)
destroy(this.uo_selespecie)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.uo_selvariedad)
destroy(this.uo_selzonas)
destroy(this.uo_selproductor)
destroy(this.uo_selinspectores)
destroy(this.st_4)
destroy(this.uo_selpredio)
destroy(this.uo_selcuartel)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.uo_selcamaras)
destroy(this.st_7)
destroy(this.uo_selplanta)
destroy(this.st_8)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelProductor.Codigo) 		THEN lb_Cerrar	=	True
IF IsNull(uo_SelEspecie.Codigo) 		THEN lb_Cerrar	=	True
IF IsNull(uo_SelVariedad.Codigo)		THEN lb_Cerrar	=	True
IF IsNull(uo_SelZonas.Codigo) 			THEN lb_Cerrar	=	True
IF IsNull(uo_SelInspectores.Codigo)	THEN lb_Cerrar	=	True
IF IsNull(uo_SelPredio.Codigo) 			THEN lb_Cerrar	=	True
IF IsNull(uo_SelCuartel.Codigo)			THEN lb_Cerrar	=	True
IF IsNull(uo_SelCamaras.Codigo)		THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo)			THEN lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelProductor.Seleccion(True, False)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelZonas.Seleccion(True, False)
	uo_SelInspectores.Seleccion(True, False)
	uo_SelPredio.Seleccion(True, False)
	uo_SelCuartel.Seleccion(True, False)
	uo_SelCamaras.Seleccion(True, False)
	uo_SelPlanta.Seleccion(True, False)
	
	uo_SelEspecie.dw_Seleccion.Object.codigo[1]	= 41
	uo_SelEspecie.Codigo = 41
	uo_SelVariedad.Filtra(41)

	uo_SelPlanta.dw_Seleccion.Object.codigo[1]	= gi_CodPlanta
	uo_SelPlanta.Codigo = gi_CodPlanta
	uo_SelCamaras.Filtra(gi_CodPlanta)
	
	uo_SelInspectores.cbx_Todos.Checked = True
	uo_SelPredio.cbx_Todos.Checked = True
	uo_SelCuartel.cbx_Todos.Checked = True
	
	em_desde.text	  = '01/' + String(Today(), 'mm/yyyy')
	em_hasta.text	  = String(Today())
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_consultaplanillacondicionambiente
end type

type st_computador from w_para_informes`st_computador within w_info_consultaplanillacondicionambiente
end type

type st_usuario from w_para_informes`st_usuario within w_info_consultaplanillacondicionambiente
end type

type st_temporada from w_para_informes`st_temporada within w_info_consultaplanillacondicionambiente
end type

type p_logo from w_para_informes`p_logo within w_info_consultaplanillacondicionambiente
end type

type st_titulo from w_para_informes`st_titulo within w_info_consultaplanillacondicionambiente
integer width = 1833
string text = "Condición y Ambiente en Camara AC Kiwis"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_consultaplanillacondicionambiente
string tag = "Imprimir Reporte"
integer x = 2185
integer y = 472
integer taborder = 190
integer weight = 400
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;Integer	li_fila
Date		ld_FechaEmbaini, ld_FechaEmbafin

ld_FechaEmbaini = Date(em_desde.Text)
ld_FechaEmbafin = Date(em_hasta.Text)

istr_info.titulo	= 'CONSULTA CONDICION DE ' + Upper(uo_SelEspecie.Nombre)

OpenWithParm(vinf,istr_info)

If uo_SelEspecie.Codigo = 41 Then
	vinf.dw_1.DataObject = "dw_info_planillacondicionambiente"
End If
	
vinf.dw_1.SetTransObject(sqlca)	
li_fila = vinf.dw_1.Retrieve(gi_CodExport, -1, uo_SelEspecie.Codigo, -1, uo_SelVariedad.Codigo, uo_SelZonas.Codigo, uo_SelProductor.Codigo, &
						uo_SelPredio.Codigo, uo_SelCuartel.Codigo, uo_SelCamaras.Codigo, uo_SelInspectores.Codigo, ld_FechaEmbaini, ld_FechaEmbafin)
						  
If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy('DataWindow.Zoom = 94')
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_consultaplanillacondicionambiente
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2181
integer y = 756
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type em_desde from editmask within w_info_consultaplanillacondicionambiente
integer x = 722
integer y = 1864
integer width = 407
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_12 from statictext within w_info_consultaplanillacondicionambiente
integer x = 498
integer y = 1872
integer width = 215
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
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_consultaplanillacondicionambiente
integer x = 1298
integer y = 1868
integer width = 178
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
boolean enabled = false
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_consultaplanillacondicionambiente
integer x = 1490
integer y = 1860
integer width = 407
integer height = 92
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_33 from statictext within w_info_consultaplanillacondicionambiente
boolean visible = false
integer x = 338
integer y = 1620
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Inspector"
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_consultaplanillacondicionambiente
integer x = 338
integer y = 812
integer width = 370
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
boolean enabled = false
string text = "Zona"
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_consultaplanillacondicionambiente
integer x = 274
integer y = 1780
integer width = 1769
integer height = 196
integer taborder = 150
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = " Fecha Control "
end type

type st_44 from statictext within w_info_consultaplanillacondicionambiente
integer x = 251
integer y = 440
integer width = 1833
integer height = 1588
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

type uo_selespecie from uo_seleccion_especie within w_info_consultaplanillacondicionambiente
integer x = 782
integer y = 540
integer height = 84
integer taborder = 210
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
		
End Choose
end event

type st_1 from statictext within w_info_consultaplanillacondicionambiente
integer x = 338
integer y = 548
integer width = 370
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
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_consultaplanillacondicionambiente
integer x = 338
integer y = 680
integer width = 370
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
boolean enabled = false
string text = "Variedad"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_consultaplanillacondicionambiente
integer x = 338
integer y = 1484
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Camaras"
boolean focusrectangle = false
end type

type uo_selvariedad from uo_seleccion_variedad_mod within w_info_consultaplanillacondicionambiente
integer x = 782
integer y = 656
integer taborder = 200
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type uo_selzonas from uo_seleccion_zonas_mod within w_info_consultaplanillacondicionambiente
integer x = 782
integer y = 788
integer taborder = 210
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_SelProductor.Filtra(This.Codigo)
		
End Choose
end event

type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_consultaplanillacondicionambiente
integer x = 782
integer y = 920
integer taborder = 210
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_SelPredio.Filtra(This.Codigo)
		uo_SelCuartel.Filtra(This.Codigo, 0)
		
End Choose
end event

type uo_selinspectores from uo_seleccion_inspectores_mod within w_info_consultaplanillacondicionambiente
boolean visible = false
integer x = 782
integer y = 1596
integer taborder = 220
boolean bringtotop = true
boolean enabled = false
end type

on uo_selinspectores.destroy
call uo_seleccion_inspectores_mod::destroy
end on

type st_4 from statictext within w_info_consultaplanillacondicionambiente
integer x = 1646
integer y = 476
integer width = 192
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
boolean focusrectangle = false
end type

type uo_selpredio from uo_seleccion_predioproduc_mod within w_info_consultaplanillacondicionambiente
integer x = 782
integer y = 1052
integer taborder = 120
boolean bringtotop = true
end type

on uo_selpredio.destroy
call uo_seleccion_predioproduc_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_SelCuartel.Filtra(uo_SelProductor.Codigo, This.Codigo)
		
End Choose
end event

type uo_selcuartel from uo_seleccion_prodcuarteles_mod within w_info_consultaplanillacondicionambiente
integer x = 782
integer y = 1188
integer taborder = 130
boolean bringtotop = true
end type

on uo_selcuartel.destroy
call uo_seleccion_prodcuarteles_mod::destroy
end on

type st_5 from statictext within w_info_consultaplanillacondicionambiente
integer x = 338
integer y = 1084
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Huerto"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_consultaplanillacondicionambiente
integer x = 338
integer y = 944
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
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

type uo_selcamaras from uo_seleccion_camarasbode_mod within w_info_consultaplanillacondicionambiente
integer x = 782
integer y = 1460
integer taborder = 140
boolean bringtotop = true
end type

on uo_selcamaras.destroy
call uo_seleccion_camarasbode_mod::destroy
end on

type st_7 from statictext within w_info_consultaplanillacondicionambiente
integer x = 334
integer y = 1348
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
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

type uo_selplanta from uo_seleccion_planta_mod within w_info_consultaplanillacondicionambiente
integer x = 782
integer y = 1324
integer taborder = 140
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_SelCamaras.Filtra(This.Codigo)
		
End Choose
end event

on uo_selplanta.destroy
call uo_seleccion_planta_mod::destroy
end on

type st_8 from statictext within w_info_consultaplanillacondicionambiente
integer x = 338
integer y = 1212
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Cuartel"
boolean focusrectangle = false
end type

