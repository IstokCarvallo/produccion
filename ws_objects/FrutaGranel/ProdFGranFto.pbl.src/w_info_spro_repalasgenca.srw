$PBExportHeader$w_info_spro_repalasgenca.srw
forward
global type w_info_spro_repalasgenca from w_para_informes
end type
type st_1 from statictext within w_info_spro_repalasgenca
end type
type st_3 from statictext within w_info_spro_repalasgenca
end type
type dw_planta from datawindow within w_info_spro_repalasgenca
end type
type em_hasta from editmask within w_info_spro_repalasgenca
end type
type st_4 from statictext within w_info_spro_repalasgenca
end type
type em_desde from editmask within w_info_spro_repalasgenca
end type
type st_2 from statictext within w_info_spro_repalasgenca
end type
type st_5 from statictext within w_info_spro_repalasgenca
end type
type cbx_movto from checkbox within w_info_spro_repalasgenca
end type
type em_movtod from editmask within w_info_spro_repalasgenca
end type
type em_movtoh from editmask within w_info_spro_repalasgenca
end type
end forward

global type w_info_spro_repalasgenca from w_para_informes
integer width = 2359
integer height = 1000
st_1 st_1
st_3 st_3
dw_planta dw_planta
em_hasta em_hasta
st_4 st_4
em_desde em_desde
st_2 st_2
st_5 st_5
cbx_movto cbx_movto
em_movtod em_movtod
em_movtoh em_movtoh
end type
global w_info_spro_repalasgenca w_info_spro_repalasgenca

type variables
str_mant  istr_mant
datawindowchild idwc_planta
end variables

on w_info_spro_repalasgenca.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_3=create st_3
this.dw_planta=create dw_planta
this.em_hasta=create em_hasta
this.st_4=create st_4
this.em_desde=create em_desde
this.st_2=create st_2
this.st_5=create st_5
this.cbx_movto=create cbx_movto
this.em_movtod=create em_movtod
this.em_movtoh=create em_movtoh
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.dw_planta
this.Control[iCurrent+4]=this.em_hasta
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.em_desde
this.Control[iCurrent+7]=this.st_2
this.Control[iCurrent+8]=this.st_5
this.Control[iCurrent+9]=this.cbx_movto
this.Control[iCurrent+10]=this.em_movtod
this.Control[iCurrent+11]=this.em_movtoh
end on

on w_info_spro_repalasgenca.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_3)
destroy(this.dw_planta)
destroy(this.em_hasta)
destroy(this.st_4)
destroy(this.em_desde)
destroy(this.st_2)
destroy(this.st_5)
destroy(this.cbx_movto)
destroy(this.em_movtod)
destroy(this.em_movtoh)
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

type st_titulo from w_para_informes`st_titulo within w_info_spro_repalasgenca
integer x = 73
integer width = 1769
string text = "Informe  Repalletizaje"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_spro_repalasgenca
integer x = 2025
integer y = 312
integer taborder = 70
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		fila, ll_desde, ll_hasta
String	ls_FechaDesde, ls_fecha, ls_FechaHasta
DateTime ldt_Fechadesde, ldt_FechaHasta, ldt_fechaprueba


istr_info.titulo	= "REPALLETIZAJE"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_repalasgenca"

vinf.dw_1.SetTransObject(sqlca)

If cbx_movto.Checked Then
	ll_desde	=	0
	ll_hasta	=	99999999
Else
	ll_desde	=	Long(em_movtod.Text)
	ll_hasta	=	Long(em_movtoh.Text)	
End IF

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

fila = vinf.dw_1.Retrieve(dw_planta.Object.plde_codigo[1], ldt_fechadesde, &
									ldt_fechahasta, ll_desde, ll_hasta)

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

type pb_salir from w_para_informes`pb_salir within w_info_spro_repalasgenca
integer x = 2025
integer y = 608
integer taborder = 80
end type

type st_1 from statictext within w_info_spro_repalasgenca
integer x = 64
integer y = 276
integer width = 1769
integer height = 548
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

type st_3 from statictext within w_info_spro_repalasgenca
integer x = 192
integer y = 408
integer width = 352
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

type dw_planta from datawindow within w_info_spro_repalasgenca
integer x = 585
integer y = 388
integer width = 928
integer height = 100
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
end type

type em_hasta from editmask within w_info_spro_repalasgenca
integer x = 1317
integer y = 644
integer width = 352
integer height = 100
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datetimemask!
string mask = "dd/mm/yyyy"
end type

type st_4 from statictext within w_info_spro_repalasgenca
integer x = 1019
integer y = 652
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

type em_desde from editmask within w_info_spro_repalasgenca
integer x = 585
integer y = 644
integer width = 352
integer height = 100
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datetimemask!
string mask = "dd/mm/yyyy"
end type

type st_2 from statictext within w_info_spro_repalasgenca
integer x = 192
integer y = 652
integer width = 352
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

type st_5 from statictext within w_info_spro_repalasgenca
integer x = 192
integer y = 532
integer width = 352
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Movimiento"
boolean focusrectangle = false
end type

type cbx_movto from checkbox within w_info_spro_repalasgenca
integer x = 1417
integer y = 524
integer width = 311
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
borderstyle borderstyle = stylelowered!
end type

event clicked;If This.Checked Then
	em_movtod.Enabled	=	False
	em_movtoh.Enabled	=	False
	em_desde.Enabled	=	True
	em_hasta.Enabled	=	True
	em_movtod.Text		=	''
	em_movtoh.Text		=	''
Else
	em_movtod.Enabled	=	True
	em_movtoh.Enabled	=	True
	em_desde.Enabled	=	False
	em_hasta.Enabled	=	False
	em_movtod.Text		=	''
	em_movtoh.Text		=	''
End IF
end event

type em_movtod from editmask within w_info_spro_repalasgenca
integer x = 585
integer y = 516
integer width = 352
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
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long	ll_desde, ll_hasta

ll_desde	=	Long(This.Text)
ll_hasta	=	Long(em_movtoh.Text)

If ll_hasta > 0 Then 
	If ll_desde > ll_hasta Then
		MessageBox('Alerta...', 'El movimento Inicial no puede ser mayor al moviemnto Final.')
		This.Text	=	''
		This.SetFocus()
		Return
	End If
End IF
end event

type em_movtoh from editmask within w_info_spro_repalasgenca
integer x = 1019
integer y = 516
integer width = 352
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
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long	ll_desde, ll_hasta

ll_desde	=	Long(em_movtod.Text)
ll_hasta	=	Long(This.Text)

If ll_desde > 0 Then 
	If ll_hasta < ll_desde Then
		MessageBox('Alerta...', 'El movimento Final no puede ser menor al moviemnto Inicial.')
		This.Text	=	''
		This.SetFocus()
		Return
	End If
End If 
end event

