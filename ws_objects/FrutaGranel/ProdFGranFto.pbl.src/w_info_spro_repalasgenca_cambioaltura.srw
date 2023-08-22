$PBExportHeader$w_info_spro_repalasgenca_cambioaltura.srw
forward
global type w_info_spro_repalasgenca_cambioaltura from w_para_informes
end type
type st_1 from statictext within w_info_spro_repalasgenca_cambioaltura
end type
type st_2 from statictext within w_info_spro_repalasgenca_cambioaltura
end type
type em_desde from editmask within w_info_spro_repalasgenca_cambioaltura
end type
type st_3 from statictext within w_info_spro_repalasgenca_cambioaltura
end type
type dw_planta from datawindow within w_info_spro_repalasgenca_cambioaltura
end type
type st_4 from statictext within w_info_spro_repalasgenca_cambioaltura
end type
type em_hasta from editmask within w_info_spro_repalasgenca_cambioaltura
end type
end forward

global type w_info_spro_repalasgenca_cambioaltura from w_para_informes
integer width = 2359
integer height = 980
string title = "Informe de Cambio de Altura"
string icon = "AppIcon!"
st_1 st_1
st_2 st_2
em_desde em_desde
st_3 st_3
dw_planta dw_planta
st_4 st_4
em_hasta em_hasta
end type
global w_info_spro_repalasgenca_cambioaltura w_info_spro_repalasgenca_cambioaltura

type variables
str_mant  istr_mant
datawindowchild idwc_planta
end variables

on w_info_spro_repalasgenca_cambioaltura.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.st_3=create st_3
this.dw_planta=create dw_planta
this.st_4=create st_4
this.em_hasta=create em_hasta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_desde
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.dw_planta
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.em_hasta
end on

on w_info_spro_repalasgenca_cambioaltura.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_3)
destroy(this.dw_planta)
destroy(this.st_4)
destroy(this.em_hasta)
end on

event open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

//PLANTA//
dw_Planta.Getchild("plde_codigo",idwc_planta)
idwc_planta.SettransObject(sqlca)
idwc_planta.Retrieve()
dw_Planta.InsertRow(0)

dw_Planta.SetItem(1,"plde_codigo",gstr_paramplanta.codigoplanta)
em_desde.Text	= String(gstr_paramtempo.FechaInicio, 'dd/mm/yyyy')
em_hasta.Text	= String(gstr_paramtempo.Fechatermino, 'dd/mm/yyyy')
end event

type st_titulo from w_para_informes`st_titulo within w_info_spro_repalasgenca_cambioaltura
integer x = 73
integer width = 1769
string text = "Informe  Cambio de Altura"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_spro_repalasgenca_cambioaltura
integer x = 2025
integer y = 312
integer taborder = 40
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		fila
String	ls_FechaDesde, ls_fecha, ls_FechaHasta
DateTime ldt_Fechadesde, ldt_FechaHasta, ldt_fechaprueba

istr_info.titulo	= "CAMBIO DE ALTURA DE PALLET"
istr_info.copias	= 1

ldt_fechadesde	=	Datetime(Date(em_desde.Text))
ls_FechaDesde	=	String(ldt_fechadesde, 'dd/mm/yyyy')

If IsNull(ldt_fechadesde) or ldt_fechadesde<= ldt_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN 1				 
END If

ls_fecha			=	em_hasta.Text
ldt_fechahasta = 	DateTime(Date(ls_fecha),Time('23:59'))
ls_FechaHasta	=	String(ldt_fechahasta, 'dd/mm/yyyy')

If IsNull(ldt_fechahasta) or ldt_fechahasta<=ldt_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", &
	             StopSign!, Ok!)
	RETURN 1				 
END If

IF ldt_fechadesde>=ldt_fechahasta THEN
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor a la Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN 1
END IF


OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_repalasgenca_cambioaltura"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(dw_planta.Object.plde_codigo[1], ldt_fechadesde, &
									ldt_fechahasta)

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

type pb_salir from w_para_informes`pb_salir within w_info_spro_repalasgenca_cambioaltura
integer x = 2025
integer y = 608
integer taborder = 50
end type

type st_1 from statictext within w_info_spro_repalasgenca_cambioaltura
integer x = 64
integer y = 276
integer width = 1769
integer height = 504
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_spro_repalasgenca_cambioaltura
integer x = 229
integer y = 592
integer width = 443
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Desde"
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_spro_repalasgenca_cambioaltura
integer x = 480
integer y = 584
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

type st_3 from statictext within w_info_spro_repalasgenca_cambioaltura
integer x = 229
integer y = 408
integer width = 206
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_spro_repalasgenca_cambioaltura
integer x = 681
integer y = 392
integer width = 896
integer height = 100
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
end type

type st_4 from statictext within w_info_spro_repalasgenca_cambioaltura
integer x = 974
integer y = 592
integer width = 210
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_spro_repalasgenca_cambioaltura
integer x = 1202
integer y = 584
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

