$PBExportHeader$w_info_produccion_diaria.srw
forward
global type w_info_produccion_diaria from w_para_informes
end type
type st_1 from statictext within w_info_produccion_diaria
end type
type st_2 from statictext within w_info_produccion_diaria
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_produccion_diaria
end type
type uo_selplanta from uo_seleccion_plantas within w_info_produccion_diaria
end type
type st_3 from statictext within w_info_produccion_diaria
end type
type cbx_folio from checkbox within w_info_produccion_diaria
end type
type em_desde from editmask within w_info_produccion_diaria
end type
type em_hasta from editmask within w_info_produccion_diaria
end type
type st_4 from statictext within w_info_produccion_diaria
end type
type st_5 from statictext within w_info_produccion_diaria
end type
type uo_selentidades from uo_seleccion_entidades within w_info_produccion_diaria
end type
type st_6 from statictext within w_info_produccion_diaria
end type
type em_horini from editmask within w_info_produccion_diaria
end type
type st_7 from statictext within w_info_produccion_diaria
end type
type em_horter from editmask within w_info_produccion_diaria
end type
type ddlb_horario from dropdownlistbox within w_info_produccion_diaria
end type
type st_8 from statictext within w_info_produccion_diaria
end type
type gb_3 from groupbox within w_info_produccion_diaria
end type
type gb_4 from groupbox within w_info_produccion_diaria
end type
end forward

global type w_info_produccion_diaria from w_para_informes
integer x = 14
integer y = 32
integer width = 2272
integer height = 1912
string title = "Informe Productividad Horaria"
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
cbx_folio cbx_folio
em_desde em_desde
em_hasta em_hasta
st_4 st_4
st_5 st_5
uo_selentidades uo_selentidades
st_6 st_6
em_horini em_horini
st_7 st_7
em_horter em_horter
ddlb_horario ddlb_horario
st_8 st_8
gb_3 gb_3
gb_4 gb_4
end type
global w_info_produccion_diaria w_info_produccion_diaria

type variables
Integer	ii_horario
end variables

on w_info_produccion_diaria.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.st_3=create st_3
this.cbx_folio=create cbx_folio
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.st_4=create st_4
this.st_5=create st_5
this.uo_selentidades=create uo_selentidades
this.st_6=create st_6
this.em_horini=create em_horini
this.st_7=create st_7
this.em_horter=create em_horter
this.ddlb_horario=create ddlb_horario
this.st_8=create st_8
this.gb_3=create gb_3
this.gb_4=create gb_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.uo_selcliente
this.Control[iCurrent+4]=this.uo_selplanta
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.cbx_folio
this.Control[iCurrent+7]=this.em_desde
this.Control[iCurrent+8]=this.em_hasta
this.Control[iCurrent+9]=this.st_4
this.Control[iCurrent+10]=this.st_5
this.Control[iCurrent+11]=this.uo_selentidades
this.Control[iCurrent+12]=this.st_6
this.Control[iCurrent+13]=this.em_horini
this.Control[iCurrent+14]=this.st_7
this.Control[iCurrent+15]=this.em_horter
this.Control[iCurrent+16]=this.ddlb_horario
this.Control[iCurrent+17]=this.st_8
this.Control[iCurrent+18]=this.gb_3
this.Control[iCurrent+19]=this.gb_4
end on

on w_info_produccion_diaria.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.st_3)
destroy(this.cbx_folio)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.uo_selentidades)
destroy(this.st_6)
destroy(this.em_horini)
destroy(this.st_7)
destroy(this.em_horter)
destroy(this.ddlb_horario)
destroy(this.st_8)
destroy(this.gb_3)
destroy(this.gb_4)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelEntidades.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelCliente.Seleccion(True,True)
	uo_SelPlanta.Seleccion(True,True)
	uo_SelEntidades.Seleccion(True,True)

	em_desde.Text	=	String (today(), 'dd/mm/yyyy')
	em_hasta.Text	=	String (today(), 'dd/mm/yyyy')
	em_horini.Text = '08:00'
	em_horter.Text = '23:00'
	ddlb_horario.Selectitem(1)
	ii_horario		=	1
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_produccion_diaria
end type

type st_computador from w_para_informes`st_computador within w_info_produccion_diaria
end type

type st_usuario from w_para_informes`st_usuario within w_info_produccion_diaria
end type

type st_temporada from w_para_informes`st_temporada within w_info_produccion_diaria
end type

type p_logo from w_para_informes`p_logo within w_info_produccion_diaria
end type

type st_titulo from w_para_informes`st_titulo within w_info_produccion_diaria
integer width = 1454
string text = "Informe Productividad Horaria"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_produccion_diaria
string tag = "Imprimir Reporte"
integer x = 1806
integer y = 480
integer taborder = 100
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		Fila
Datetime	ldt_desde, ldt_hasta
Integer	li_Folio
String 	ls_horario, ls_rango

istr_info.titulo	= "INFORME PRODUCTIVIDAD HORARIA"
istr_info.copias	= 1

CHOOSE CASE ii_horario
	CASE 1
		ls_horario = '0.01041669'
		ls_rango = '1/2 Hora'
	CASE 2
		ls_horario = '0.02083338'
		ls_rango = '1/2 Hora'
	CASE 3
		ls_horario = '0.04166677'
		ls_rango = '1 Hora'
	CASE 4
		ls_horario = '0.0833334'
		ls_rango = '2 Horas'
	CASE 5
		ls_horario = '0.1250001'
		ls_rango = '3 Horas'
	CASE 6
		ls_horario = '0.1666668'
		ls_rango = '4 Horas'
	CASE 7
		ls_horario = '0.2083334'
		ls_rango = '5 Horas'
	CASE 8
		ls_horario = '0.2500001'
		ls_rango = '6 Horas'
	CASE 9
		ls_horario = '0.2916668'
		ls_rango = '7 Horas'
	CASE 10
		ls_horario = '0.3333334'
		ls_rango = '8 Horas'
	CASE 11
		ls_horario = '0.3750001'
		ls_rango = '9 Horas'
	CASE 12
		ls_horario = '0.4166668'
		ls_rango = '10 Horas'
	CASE 13
		ls_horario = '0.4583334'
		ls_rango = '11 Horas'
	CASE 14
		ls_horario = '0.5000001'
		ls_rango = '12 Horas'
End Choose 

ldt_desde	=	Datetime(Date(em_desde.Text), Time(em_horini.Text))
ldt_hasta	=	Datetime(Date(em_hasta.Text), Time(em_horter.Text))

If cbx_folio.Checked Then
	li_Folio = -9
Else
	li_Folio = -1
End If

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_productividad_horaria"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelPlanta.Codigo, uo_SelCliente.codigo, uo_SelEntidades.Codigo, &
								  ldt_desde, ls_Horario, ldt_desde, ldt_hasta,  li_Folio)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_produccion_diaria
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1801
integer y = 740
integer taborder = 110
string disabledname = "\desarrollo\bmp\exitd.bmp"
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_produccion_diaria
integer x = 315
integer y = 764
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_produccion_diaria
integer x = 1015
integer y = 1176
integer width = 206
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_produccion_diaria
event destroy ( )
integer x = 699
integer y = 484
integer taborder = 110
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_produccion_diaria
integer x = 699
integer y = 668
integer taborder = 120
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_3 from statictext within w_info_produccion_diaria
integer x = 315
integer y = 580
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type cbx_folio from checkbox within w_info_produccion_diaria
integer x = 599
integer y = 1292
integer width = 777
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolida Fecha"
boolean checked = true
end type

type em_desde from editmask within w_info_produccion_diaria
integer x = 585
integer y = 1160
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

type em_hasta from editmask within w_info_produccion_diaria
integer x = 1221
integer y = 1160
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

type st_4 from statictext within w_info_produccion_diaria
integer x = 311
integer y = 948
integer width = 288
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Personal"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_produccion_diaria
integer x = 311
integer y = 1176
integer width = 206
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Desde"
boolean focusrectangle = false
end type

type uo_selentidades from uo_seleccion_entidades within w_info_produccion_diaria
integer x = 699
integer y = 852
integer taborder = 120
boolean bringtotop = true
end type

on uo_selentidades.destroy
call uo_seleccion_entidades::destroy
end on

type st_6 from statictext within w_info_produccion_diaria
integer x = 311
integer y = 1416
integer width = 206
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Desde"
boolean focusrectangle = false
end type

type em_horini from editmask within w_info_produccion_diaria
integer x = 635
integer y = 1400
integer width = 306
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
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = timemask!
string mask = "hh:mm"
end type

type st_7 from statictext within w_info_produccion_diaria
integer x = 1015
integer y = 1416
integer width = 206
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
end type

type em_horter from editmask within w_info_produccion_diaria
integer x = 1271
integer y = 1400
integer width = 306
integer height = 88
integer taborder = 140
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
maskdatatype maskdatatype = timemask!
string mask = "hh:mm"
end type

type ddlb_horario from dropdownlistbox within w_info_produccion_diaria
integer x = 699
integer y = 1548
integer width = 896
integer height = 400
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 16777215
boolean allowedit = true
boolean sorted = false
boolean vscrollbar = true
string item[] = {"1/4 Hora","1/2 Hora","1 Horas","2 Horas","3 Horas","4 Horas","5 Horas","6 Horas","7 Horas","8 Horas","9 Horas","10 Horas","11 Horas","12 Horas"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_horario = Index
end event

type st_8 from statictext within w_info_produccion_diaria
integer x = 338
integer y = 1568
integer width = 297
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Horario"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_produccion_diaria
integer x = 251
integer y = 420
integer width = 1449
integer height = 644
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
end type

type gb_4 from groupbox within w_info_produccion_diaria
integer x = 251
integer y = 1064
integer width = 1449
integer height = 728
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
end type

