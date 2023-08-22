$PBExportHeader$w_info_tiempos_muertos.srw
forward
global type w_info_tiempos_muertos from w_para_informes
end type
type st_1 from statictext within w_info_tiempos_muertos
end type
type st_2 from statictext within w_info_tiempos_muertos
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_tiempos_muertos
end type
type uo_selplanta from uo_seleccion_plantas within w_info_tiempos_muertos
end type
type st_3 from statictext within w_info_tiempos_muertos
end type
type em_desde from editmask within w_info_tiempos_muertos
end type
type em_hasta from editmask within w_info_tiempos_muertos
end type
type st_5 from statictext within w_info_tiempos_muertos
end type
type em_periodo from editmask within w_info_tiempos_muertos
end type
type gb_3 from groupbox within w_info_tiempos_muertos
end type
type gb_4 from groupbox within w_info_tiempos_muertos
end type
type gb_5 from groupbox within w_info_tiempos_muertos
end type
end forward

global type w_info_tiempos_muertos from w_para_informes
integer x = 14
integer y = 32
integer width = 1888
integer height = 1324
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
st_3 st_3
em_desde em_desde
em_hasta em_hasta
st_5 st_5
em_periodo em_periodo
gb_3 gb_3
gb_4 gb_4
gb_5 gb_5
end type
global w_info_tiempos_muertos w_info_tiempos_muertos

type variables

end variables

on w_info_tiempos_muertos.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.st_3=create st_3
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.st_5=create st_5
this.em_periodo=create em_periodo
this.gb_3=create gb_3
this.gb_4=create gb_4
this.gb_5=create gb_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.uo_selcliente
this.Control[iCurrent+4]=this.uo_selplanta
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.em_desde
this.Control[iCurrent+7]=this.em_hasta
this.Control[iCurrent+8]=this.st_5
this.Control[iCurrent+9]=this.em_periodo
this.Control[iCurrent+10]=this.gb_3
this.Control[iCurrent+11]=this.gb_4
this.Control[iCurrent+12]=this.gb_5
end on

on w_info_tiempos_muertos.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.st_3)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.st_5)
destroy(this.em_periodo)
destroy(this.gb_3)
destroy(this.gb_4)
destroy(this.gb_5)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelCliente.Seleccion(True,True)
	uo_SelPlanta.Seleccion(True,True)

	em_desde.Text		=	String (today(), 'dd/mm/yyyy')
	em_hasta.Text		=	String (today(), 'dd/mm/yyyy')
	em_periodo.Text	=	'30'
End If
end event

type st_titulo from w_para_informes`st_titulo within w_info_tiempos_muertos
integer x = 37
integer y = 16
integer width = 1454
string text = "Informe Rendimiento Personal"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_tiempos_muertos
string tag = "Imprimir Reporte"
integer x = 1595
integer y = 196
integer taborder = 100
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		Fila, li_periodo
Date		ld_desde, ld_hasta


istr_info.titulo	= "INFORME TIEMPOS MUERTOS"
istr_info.copias	= 1

ld_desde		=	Date(em_desde.Text)
ld_hasta		=	Date(em_hasta.Text)
li_periodo	=	Integer(em_periodo.text)

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_tiempos_muertos"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_SelPlanta.Codigo, uo_SelCliente.codigo, ld_desde, ld_hasta, li_periodo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_tiempos_muertos
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1591
integer y = 456
integer taborder = 110
string disabledname = "\desarrollo\bmp\exitd.bmp"
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_tiempos_muertos
integer x = 105
integer y = 480
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_tiempos_muertos
integer x = 805
integer y = 784
integer width = 206
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Hasta"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_tiempos_muertos
event destroy ( )
integer x = 489
integer y = 200
integer taborder = 110
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_tiempos_muertos
integer x = 489
integer y = 384
integer taborder = 120
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_3 from statictext within w_info_tiempos_muertos
integer x = 105
integer y = 296
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Cliente"
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_tiempos_muertos
integer x = 375
integer y = 768
integer width = 402
integer height = 88
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type em_hasta from editmask within w_info_tiempos_muertos
integer x = 1010
integer y = 768
integer width = 402
integer height = 88
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_5 from statictext within w_info_tiempos_muertos
integer x = 101
integer y = 784
integer width = 206
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Desde"
boolean focusrectangle = false
end type

type em_periodo from editmask within w_info_tiempos_muertos
integer x = 562
integer y = 1040
integer width = 402
integer height = 96
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "0000"
end type

event getfocus;This.SelectText(1, len(This.Text))
end event

type gb_3 from groupbox within w_info_tiempos_muertos
integer x = 41
integer y = 136
integer width = 1449
integer height = 488
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type gb_4 from groupbox within w_info_tiempos_muertos
integer x = 41
integer y = 648
integer width = 1449
integer height = 264
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " Periodo "
end type

type gb_5 from groupbox within w_info_tiempos_muertos
integer x = 41
integer y = 936
integer width = 1449
integer height = 264
integer taborder = 130
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long backcolor = 12632256
string text = " Tiempos muertos en Minutos "
end type

