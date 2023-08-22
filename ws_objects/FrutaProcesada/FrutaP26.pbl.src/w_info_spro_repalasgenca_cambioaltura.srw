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
type st_5 from statictext within w_info_spro_repalasgenca_cambioaltura
end type
type dw_cliente from datawindow within w_info_spro_repalasgenca_cambioaltura
end type
end forward

global type w_info_spro_repalasgenca_cambioaltura from w_para_informes
integer width = 2574
integer height = 1276
string title = "Informe de Cambio de Altura"
string icon = "AppIcon!"
st_1 st_1
st_2 st_2
em_desde em_desde
st_3 st_3
dw_planta dw_planta
st_4 st_4
em_hasta em_hasta
st_5 st_5
dw_cliente dw_cliente
end type
global w_info_spro_repalasgenca_cambioaltura w_info_spro_repalasgenca_cambioaltura

type variables
str_mant  istr_mant
datawindowchild idwc_planta, idwc_cliente
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
this.st_5=create st_5
this.dw_cliente=create dw_cliente
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_desde
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.dw_planta
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.em_hasta
this.Control[iCurrent+8]=this.st_5
this.Control[iCurrent+9]=this.dw_cliente
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
destroy(this.st_5)
destroy(this.dw_cliente)
end on

event open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

//PLANTA//
dw_Planta.Getchild("plde_codigo",idwc_planta)
idwc_planta.SettransObject(sqlca)
idwc_planta.Retrieve()
dw_Planta.InsertRow(0)

dw_Planta.SetItem(1,"plde_codigo",gi_CodPlanta)
//CLIENTE//
dw_cliente.Getchild("clie_codigo",idwc_cliente)
idwc_cliente.SettransObject(sqlca)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)

dw_cliente.SetItem(1,"clie_codigo",gi_CodExport)

end event

type st_computador from w_para_informes`st_computador within w_info_spro_repalasgenca_cambioaltura
end type

type st_usuario from w_para_informes`st_usuario within w_info_spro_repalasgenca_cambioaltura
end type

type st_temporada from w_para_informes`st_temporada within w_info_spro_repalasgenca_cambioaltura
end type

type p_logo from w_para_informes`p_logo within w_info_spro_repalasgenca_cambioaltura
end type

type st_titulo from w_para_informes`st_titulo within w_info_spro_repalasgenca_cambioaltura
integer width = 1769
string text = "Informe  Cambio de Altura"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_spro_repalasgenca_cambioaltura
integer x = 2217
integer y = 484
integer taborder = 40
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		fila
String	ls_FechaDesde, ls_fecha, ls_FechaHasta
Date ldt_Fechadesde, ldt_FechaHasta, ldt_fechaprueba

istr_info.titulo	= "CAMBIO DE ALTURA DE PALLET"
istr_info.copias	= 1

ldt_fechadesde	=	Date(em_desde.Text)
ls_FechaDesde	=	String(ldt_fechadesde, 'dd/mm/yyyy')

If IsNull(ldt_fechadesde) or ldt_fechadesde<= ldt_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN 1				 
END If

ls_fecha			=	em_hasta.Text
ldt_fechahasta = 	Date(ls_fecha)
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

fila = vinf.dw_1.Retrieve(dw_cliente.Object.clie_codigo[1],dw_planta.Object.plde_codigo[1], ldt_fechadesde, &
									ldt_fechahasta)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_spro_repalasgenca_cambioaltura
integer x = 2217
integer y = 780
integer taborder = 50
end type

type st_1 from statictext within w_info_spro_repalasgenca_cambioaltura
integer x = 247
integer y = 440
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
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_spro_repalasgenca_cambioaltura
integer x = 411
integer y = 796
integer width = 219
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Desde"
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_spro_repalasgenca_cambioaltura
integer x = 695
integer y = 788
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
integer x = 411
integer y = 672
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
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_spro_repalasgenca_cambioaltura
integer x = 686
integer y = 656
integer width = 1157
integer height = 100
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantas"
boolean border = false
boolean livescroll = true
end type

type st_4 from statictext within w_info_spro_repalasgenca_cambioaltura
integer x = 1157
integer y = 796
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
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_spro_repalasgenca_cambioaltura
integer x = 1417
integer y = 788
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

type st_5 from statictext within w_info_spro_repalasgenca_cambioaltura
integer x = 407
integer y = 556
integer width = 261
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_spro_repalasgenca_cambioaltura
integer x = 690
integer y = 536
integer width = 1138
integer height = 96
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

