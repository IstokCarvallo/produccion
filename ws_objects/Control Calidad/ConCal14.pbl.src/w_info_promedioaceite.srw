$PBExportHeader$w_info_promedioaceite.srw
$PBExportComments$Ventana de reporte promedio aceite
forward
global type w_info_promedioaceite from w_para_informes
end type
type em_desde from editmask within w_info_promedioaceite
end type
type st_12 from statictext within w_info_promedioaceite
end type
type st_13 from statictext within w_info_promedioaceite
end type
type em_hasta from editmask within w_info_promedioaceite
end type
type st_zona from statictext within w_info_promedioaceite
end type
type st_33 from statictext within w_info_promedioaceite
end type
type st_1 from statictext within w_info_promedioaceite
end type
type uo_selpredio from uo_seleccion_predioproduc_mod within w_info_promedioaceite
end type
type st_2 from statictext within w_info_promedioaceite
end type
type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_promedioaceite
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_info_promedioaceite
end type
type uo_selzonas from uo_seleccion_zonas_mod within w_info_promedioaceite
end type
type st_3 from statictext within w_info_promedioaceite
end type
type st_4 from statictext within w_info_promedioaceite
end type
type gb_4 from groupbox within w_info_promedioaceite
end type
type st_44 from statictext within w_info_promedioaceite
end type
type cbx_todosfecha from checkbox within w_info_promedioaceite
end type
end forward

global type w_info_promedioaceite from w_para_informes
string tag = "Consulta Planilla - Evolucion de muestreo"
integer x = 14
integer y = 32
integer width = 2638
integer height = 1560
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
em_desde em_desde
st_12 st_12
st_13 st_13
em_hasta em_hasta
st_zona st_zona
st_33 st_33
st_1 st_1
uo_selpredio uo_selpredio
st_2 st_2
uo_selproductor uo_selproductor
uo_selvariedad uo_selvariedad
uo_selzonas uo_selzonas
st_3 st_3
st_4 st_4
gb_4 gb_4
st_44 st_44
cbx_todosfecha cbx_todosfecha
end type
global w_info_promedioaceite w_info_promedioaceite

type variables
str_busqueda      istr_busq
str_mant         istr_mant
Integer	        ii_tipo
String	        is_report, is_nula, is_informe
uo_Especie     iuo_Especie


end variables

on w_info_promedioaceite.create
int iCurrent
call super::create
this.em_desde=create em_desde
this.st_12=create st_12
this.st_13=create st_13
this.em_hasta=create em_hasta
this.st_zona=create st_zona
this.st_33=create st_33
this.st_1=create st_1
this.uo_selpredio=create uo_selpredio
this.st_2=create st_2
this.uo_selproductor=create uo_selproductor
this.uo_selvariedad=create uo_selvariedad
this.uo_selzonas=create uo_selzonas
this.st_3=create st_3
this.st_4=create st_4
this.gb_4=create gb_4
this.st_44=create st_44
this.cbx_todosfecha=create cbx_todosfecha
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_desde
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.st_13
this.Control[iCurrent+4]=this.em_hasta
this.Control[iCurrent+5]=this.st_zona
this.Control[iCurrent+6]=this.st_33
this.Control[iCurrent+7]=this.st_1
this.Control[iCurrent+8]=this.uo_selpredio
this.Control[iCurrent+9]=this.st_2
this.Control[iCurrent+10]=this.uo_selproductor
this.Control[iCurrent+11]=this.uo_selvariedad
this.Control[iCurrent+12]=this.uo_selzonas
this.Control[iCurrent+13]=this.st_3
this.Control[iCurrent+14]=this.st_4
this.Control[iCurrent+15]=this.gb_4
this.Control[iCurrent+16]=this.st_44
this.Control[iCurrent+17]=this.cbx_todosfecha
end on

on w_info_promedioaceite.destroy
call super::destroy
destroy(this.em_desde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_hasta)
destroy(this.st_zona)
destroy(this.st_33)
destroy(this.st_1)
destroy(this.uo_selpredio)
destroy(this.st_2)
destroy(this.uo_selproductor)
destroy(this.uo_selvariedad)
destroy(this.uo_selzonas)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.gb_4)
destroy(this.st_44)
destroy(this.cbx_todosfecha)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelProductor.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelPredio.Codigo)		Then lb_Cerrar	=	True
If IsNull(uo_SelVariedad.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelZonas.Codigo) 		Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelProductor.Seleccion(True, False)
	uo_SelPredio.Seleccion(True, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelZonas.Seleccion(True, False)
	uo_SelPredio.Todos(True)
	
	uo_SelVariedad.Filtra(81)
	
	uo_SelZonas.dw_Seleccion.Object.codigo.Dddw.PercentWidth		=	160
	uo_SelVariedad.dw_Seleccion.Object.codigo.Dddw.PercentWidth		=	150
	uo_SelProductor.dw_Seleccion.Object.codigo.Dddw.PercentWidth	=	210
	uo_SelPredio.dw_Seleccion.Object.codigo.Dddw.PercentWidth		=	250

	uo_SelZonas.dw_Seleccion.Object.codigo.Dddw.HScrollBar		= 'No'
	uo_SelVariedad.dw_Seleccion.Object.codigo.Dddw.HScrollBar	= 'No'
	uo_SelProductor.dw_Seleccion.Object.codigo.Dddw.HScrollBar= 'No'
	uo_SelPredio.dw_Seleccion.Object.codigo.Dddw.HScrollBar		= 'No'	
	
	em_desde.text	  = String(Date('2005-01-01'))
	em_hasta.text	  = String(Today())
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_promedioaceite
end type

type st_computador from w_para_informes`st_computador within w_info_promedioaceite
integer x = 1586
integer y = 156
end type

type st_usuario from w_para_informes`st_usuario within w_info_promedioaceite
integer x = 1586
integer y = 84
end type

type st_temporada from w_para_informes`st_temporada within w_info_promedioaceite
integer x = 1586
integer y = 12
end type

type p_logo from w_para_informes`p_logo within w_info_promedioaceite
end type

type st_titulo from w_para_informes`st_titulo within w_info_promedioaceite
integer width = 1833
string text = "Precosecha Paltas - Resumen por Predio"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_promedioaceite
string tag = "Imprimir Reporte"
integer x = 2272
integer y = 496
integer taborder = 90
end type

event pb_acepta::clicked;Integer	li_fila, li_zona, li_especie, li_planta ,mm 
Date		ld_FechaEmbaini, ld_FechaEmbafin
Long     ll_Productor
String   ls_especie

SetPointer(HourGlass!)

ld_FechaEmbaini = Date(em_desde.Text)
ld_FechaEmbafin = Date(em_hasta.Text)

istr_info.titulo	= 'EVALUACION % DE ACEITE EN PALTAS PRECOSECHA'

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_promedioaceite_enca"
	
vinf.dw_1.SetTransObject(sqlca)	
li_fila = vinf.dw_1.Retrieve(uo_SelZonas.Codigo, uo_SelProductor.Codigo, uo_SelPredio.Codigo, 81, uo_SelVariedad.codigo, ld_FechaEmbaini,ld_FechaEmbafin)
						  
IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos :~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_promedioaceite
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2267
integer y = 776
integer taborder = 100
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type em_desde from editmask within w_info_promedioaceite
integer x = 558
integer y = 1088
integer width = 480
integer height = 92
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_12 from statictext within w_info_promedioaceite
integer x = 357
integer y = 1096
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

type st_13 from statictext within w_info_promedioaceite
integer x = 1047
integer y = 1096
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

type em_hasta from editmask within w_info_promedioaceite
integer x = 1221
integer y = 1084
integer width = 475
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_zona from statictext within w_info_promedioaceite
integer x = 347
integer y = 776
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
string text = "Predio"
boolean focusrectangle = false
end type

type st_33 from statictext within w_info_promedioaceite
integer x = 343
integer y = 552
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
string text = "Zona"
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_promedioaceite
integer x = 343
integer y = 900
integer width = 370
integer height = 72
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

type uo_selpredio from uo_seleccion_predioproduc_mod within w_info_promedioaceite
integer x = 786
integer y = 752
integer taborder = 30
boolean bringtotop = true
end type

on uo_selpredio.destroy
call uo_seleccion_predioproduc_mod::destroy
end on

type st_2 from statictext within w_info_promedioaceite
integer x = 343
integer y = 664
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

type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_promedioaceite
event destroy ( )
integer x = 786
integer y = 640
integer taborder = 20
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return 

Choose Case This.Codigo
	Case -1, -9
		uo_SelPredio.Filtra(-1)
		
	Case Else
		uo_SelPredio.Filtra(This.Codigo)
		
End Choose 
end event

type uo_selvariedad from uo_seleccion_variedad_mod within w_info_promedioaceite
event destroy ( )
integer x = 786
integer y = 872
integer taborder = 40
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type uo_selzonas from uo_seleccion_zonas_mod within w_info_promedioaceite
event destroy ( )
integer x = 786
integer y = 528
integer taborder = 10
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return 

Choose Case This.Codigo
	Case -1, -9
		uo_SelProductor.Filtra(-1)
		
	Case Else
		uo_SelProductor.Filtra(This.Codigo)
		
End Choose 
end event

type st_3 from statictext within w_info_promedioaceite
integer x = 1646
integer y = 460
integer width = 187
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

type st_4 from statictext within w_info_promedioaceite
boolean visible = false
integer x = 1847
integer y = 460
integer width = 219
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
string text = "Consol."
boolean focusrectangle = false
end type

type gb_4 from groupbox within w_info_promedioaceite
integer x = 274
integer y = 1020
integer width = 1769
integer height = 196
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = " Fecha Muestreo "
end type

type st_44 from statictext within w_info_promedioaceite
integer x = 251
integer y = 440
integer width = 1833
integer height = 816
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

type cbx_todosfecha from checkbox within w_info_promedioaceite
integer x = 1705
integer y = 1096
integer width = 78
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
boolean checked = true
end type

event clicked;IF	This.Checked THEN
	em_desde.text	  = String(Date('2005-01-01'))
	em_hasta.text	  = String(Today())
	em_desde.Enabled  = False
	em_hasta.Enabled	  = False
ELSE
	em_desde.text	  = String(Relativedate(Today(),-365))
	em_hasta.text	  = String(Today())
	em_desde.Enabled  = True
	em_hasta.Enabled	  = True
END IF
end event

