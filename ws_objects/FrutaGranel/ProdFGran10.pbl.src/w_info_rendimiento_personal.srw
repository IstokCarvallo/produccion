$PBExportHeader$w_info_rendimiento_personal.srw
forward
global type w_info_rendimiento_personal from w_para_informes
end type
type st_1 from statictext within w_info_rendimiento_personal
end type
type st_2 from statictext within w_info_rendimiento_personal
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_rendimiento_personal
end type
type st_3 from statictext within w_info_rendimiento_personal
end type
type em_desde from editmask within w_info_rendimiento_personal
end type
type em_hasta from editmask within w_info_rendimiento_personal
end type
type st_5 from statictext within w_info_rendimiento_personal
end type
type cbx_grafico from checkbox within w_info_rendimiento_personal
end type
type uo_sellinea from uo_seleccion_lineapacking within w_info_rendimiento_personal
end type
type st_4 from statictext within w_info_rendimiento_personal
end type
type uo_selplanta from uo_seleccion_plantas within w_info_rendimiento_personal
end type
type st_6 from statictext within w_info_rendimiento_personal
end type
type uo_selcontratista from uo_seleccion_contratista within w_info_rendimiento_personal
end type
type gb_3 from groupbox within w_info_rendimiento_personal
end type
type gb_4 from groupbox within w_info_rendimiento_personal
end type
type gb_5 from groupbox within w_info_rendimiento_personal
end type
end forward

global type w_info_rendimiento_personal from w_para_informes
integer x = 14
integer y = 32
integer width = 2281
integer height = 1912
string title = "Informe Rendimiento Personal"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
uo_selcliente uo_selcliente
st_3 st_3
em_desde em_desde
em_hasta em_hasta
st_5 st_5
cbx_grafico cbx_grafico
uo_sellinea uo_sellinea
st_4 st_4
uo_selplanta uo_selplanta
st_6 st_6
uo_selcontratista uo_selcontratista
gb_3 gb_3
gb_4 gb_4
gb_5 gb_5
end type
global w_info_rendimiento_personal w_info_rendimiento_personal

type variables
DataWindowChild 	idwc_cont

uo_contratista		iuo_cont
end variables

on w_info_rendimiento_personal.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.uo_selcliente=create uo_selcliente
this.st_3=create st_3
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.st_5=create st_5
this.cbx_grafico=create cbx_grafico
this.uo_sellinea=create uo_sellinea
this.st_4=create st_4
this.uo_selplanta=create uo_selplanta
this.st_6=create st_6
this.uo_selcontratista=create uo_selcontratista
this.gb_3=create gb_3
this.gb_4=create gb_4
this.gb_5=create gb_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.uo_selcliente
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.em_desde
this.Control[iCurrent+6]=this.em_hasta
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.cbx_grafico
this.Control[iCurrent+9]=this.uo_sellinea
this.Control[iCurrent+10]=this.st_4
this.Control[iCurrent+11]=this.uo_selplanta
this.Control[iCurrent+12]=this.st_6
this.Control[iCurrent+13]=this.uo_selcontratista
this.Control[iCurrent+14]=this.gb_3
this.Control[iCurrent+15]=this.gb_4
this.Control[iCurrent+16]=this.gb_5
end on

on w_info_rendimiento_personal.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selcliente)
destroy(this.st_3)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.st_5)
destroy(this.cbx_grafico)
destroy(this.uo_sellinea)
destroy(this.st_4)
destroy(this.uo_selplanta)
destroy(this.st_6)
destroy(this.uo_selcontratista)
destroy(this.gb_3)
destroy(this.gb_4)
destroy(this.gb_5)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelLinea.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelContratista.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelCliente.Seleccion(True,True)
	uo_SelPlanta.Seleccion(True,True)
	uo_SelLinea.Seleccion(True,True)
	uo_SelContratista.Seleccion(True,True)
	
	em_desde.Text			=	String (today(), 'dd/mm/yyyy')
	em_hasta.Text			=	String (today(), 'dd/mm/yyyy')
	cbx_grafico.checked	=	True
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_rendimiento_personal
end type

type st_computador from w_para_informes`st_computador within w_info_rendimiento_personal
end type

type st_usuario from w_para_informes`st_usuario within w_info_rendimiento_personal
end type

type st_temporada from w_para_informes`st_temporada within w_info_rendimiento_personal
end type

type p_logo from w_para_informes`p_logo within w_info_rendimiento_personal
end type

type st_titulo from w_para_informes`st_titulo within w_info_rendimiento_personal
integer width = 1454
string text = "Informe Rendimiento Personal"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_rendimiento_personal
string tag = "Imprimir Reporte"
integer x = 1819
integer y = 468
integer taborder = 90
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		Fila
Date		ld_desde, ld_hasta
Integer	li_visible, li_contratista

istr_info.titulo	= "INFORME RENDIMIENTO PERSONAL"
istr_info.copias	= 1

ld_desde	=	Date(em_desde.Text)
ld_hasta	=	Date(em_hasta.Text)

IF cbx_grafico.checked THEN
	li_visible	=	0
ELSE
	li_visible	=	1
END IF

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_rendimiento_personal"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelPlanta.Codigo, uo_SelCliente.codigo, ld_desde, ld_hasta, &
								  uo_SelLinea.codigo, uo_SelContratista.Codigo, li_visible)
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

type pb_salir from w_para_informes`pb_salir within w_info_rendimiento_personal
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1806
integer y = 728
integer taborder = 100
string disabledname = "\desarrollo\bmp\exitd.bmp"
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_rendimiento_personal
integer x = 320
integer y = 744
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

type st_2 from statictext within w_info_rendimiento_personal
integer x = 1019
integer y = 1364
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

type uo_selcliente from uo_seleccion_clientesprod within w_info_rendimiento_personal
event destroy ( )
integer x = 704
integer y = 472
integer taborder = 10
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_3 from statictext within w_info_rendimiento_personal
integer x = 320
integer y = 568
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

type em_desde from editmask within w_info_rendimiento_personal
integer x = 590
integer y = 1348
integer width = 402
integer height = 88
integer taborder = 50
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

type em_hasta from editmask within w_info_rendimiento_personal
integer x = 1225
integer y = 1348
integer width = 402
integer height = 88
integer taborder = 60
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

type st_5 from statictext within w_info_rendimiento_personal
integer x = 320
integer y = 1364
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

type cbx_grafico from checkbox within w_info_rendimiento_personal
integer x = 320
integer y = 1636
integer width = 521
integer height = 80
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Sin gráfico"
boolean checked = true
boolean lefttext = true
end type

type uo_sellinea from uo_seleccion_lineapacking within w_info_rendimiento_personal
event destroy ( )
integer x = 704
integer y = 840
integer taborder = 30
boolean bringtotop = true
end type

on uo_sellinea.destroy
call uo_seleccion_lineapacking::destroy
end on

type st_4 from statictext within w_info_rendimiento_personal
integer x = 320
integer y = 920
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
string text = "Linea"
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_info_rendimiento_personal
integer x = 704
integer y = 656
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;IF This.Codigo <> -1 THEN uo_selLinea.Filtra(This.Codigo)
end event

type st_6 from statictext within w_info_rendimiento_personal
integer x = 320
integer y = 1096
integer width = 325
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
string text = "Contratista"
boolean focusrectangle = false
end type

type uo_selcontratista from uo_seleccion_contratista within w_info_rendimiento_personal
event destroy ( )
integer x = 704
integer y = 1016
integer taborder = 50
boolean bringtotop = true
end type

on uo_selcontratista.destroy
call uo_seleccion_contratista::destroy
end on

type gb_3 from groupbox within w_info_rendimiento_personal
integer x = 256
integer y = 408
integer width = 1449
integer height = 808
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
end type

type gb_4 from groupbox within w_info_rendimiento_personal
integer x = 256
integer y = 1228
integer width = 1449
integer height = 264
integer taborder = 110
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
string text = " Periodo "
end type

type gb_5 from groupbox within w_info_rendimiento_personal
integer x = 256
integer y = 1512
integer width = 1449
integer height = 264
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
string text = " Gráfico"
end type

