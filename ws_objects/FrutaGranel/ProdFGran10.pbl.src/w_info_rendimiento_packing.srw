$PBExportHeader$w_info_rendimiento_packing.srw
$PBExportComments$Informes entrega Rendimiento del packing
forward
global type w_info_rendimiento_packing from w_para_informes
end type
type st_1 from statictext within w_info_rendimiento_packing
end type
type st_2 from statictext within w_info_rendimiento_packing
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_rendimiento_packing
end type
type st_3 from statictext within w_info_rendimiento_packing
end type
type em_desde from editmask within w_info_rendimiento_packing
end type
type em_hasta from editmask within w_info_rendimiento_packing
end type
type uo_sellinea from uo_seleccion_lineapacking within w_info_rendimiento_packing
end type
type st_4 from statictext within w_info_rendimiento_packing
end type
type uo_selplanta from uo_seleccion_plantas within w_info_rendimiento_packing
end type
type st_porc from statictext within w_info_rendimiento_packing
end type
type em_porcen from editmask within w_info_rendimiento_packing
end type
type st_5 from statictext within w_info_rendimiento_packing
end type
type gb_3 from groupbox within w_info_rendimiento_packing
end type
type gb_4 from groupbox within w_info_rendimiento_packing
end type
end forward

global type w_info_rendimiento_packing from w_para_informes
integer x = 14
integer y = 32
integer width = 2281
integer height = 1716
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
uo_sellinea uo_sellinea
st_4 st_4
uo_selplanta uo_selplanta
st_porc st_porc
em_porcen em_porcen
st_5 st_5
gb_3 gb_3
gb_4 gb_4
end type
global w_info_rendimiento_packing w_info_rendimiento_packing

type variables
DataWindowChild 	idwc_cont

uo_contratista		iuo_cont
end variables

on w_info_rendimiento_packing.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.uo_selcliente=create uo_selcliente
this.st_3=create st_3
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.uo_sellinea=create uo_sellinea
this.st_4=create st_4
this.uo_selplanta=create uo_selplanta
this.st_porc=create st_porc
this.em_porcen=create em_porcen
this.st_5=create st_5
this.gb_3=create gb_3
this.gb_4=create gb_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.uo_selcliente
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.em_desde
this.Control[iCurrent+6]=this.em_hasta
this.Control[iCurrent+7]=this.uo_sellinea
this.Control[iCurrent+8]=this.st_4
this.Control[iCurrent+9]=this.uo_selplanta
this.Control[iCurrent+10]=this.st_porc
this.Control[iCurrent+11]=this.em_porcen
this.Control[iCurrent+12]=this.st_5
this.Control[iCurrent+13]=this.gb_3
this.Control[iCurrent+14]=this.gb_4
end on

on w_info_rendimiento_packing.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selcliente)
destroy(this.st_3)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.uo_sellinea)
destroy(this.st_4)
destroy(this.uo_selplanta)
destroy(this.st_porc)
destroy(this.em_porcen)
destroy(this.st_5)
destroy(this.gb_3)
destroy(this.gb_4)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_sellinea.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelCliente.Seleccion(False,False)
	uo_SelPlanta.Seleccion(False,False)
	uo_sellinea.Seleccion(False,False)	
	em_desde.Text			=	String (today(), 'dd/mm/yyyy')
	em_hasta.Text			=	String (today(), 'dd/mm/yyyy')
	
	
	uo_selcliente.dw_seleccion.Object.codigo[1]	=	gi_CodExport
	uo_selcliente.Codigo 									=	gi_CodExport
	uo_SelPlanta.dw_seleccion.Object.codigo[1]	=	gstr_paramplanta.codigoplanta
	uo_SelPlanta.Codigo 									=	gstr_paramplanta.codigoplanta
	uo_selLinea.Filtra(gstr_paramplanta.codigoplanta)
	em_porcen.Text	= '30'
	uo_sellinea.dw_seleccion.Object.codigo[1]	=	1
	uo_sellinea.Codigo									=	1	
	
End If

end event

type pb_excel from w_para_informes`pb_excel within w_info_rendimiento_packing
end type

type st_computador from w_para_informes`st_computador within w_info_rendimiento_packing
end type

type st_usuario from w_para_informes`st_usuario within w_info_rendimiento_packing
end type

type st_temporada from w_para_informes`st_temporada within w_info_rendimiento_packing
end type

type p_logo from w_para_informes`p_logo within w_info_rendimiento_packing
end type

type st_titulo from w_para_informes`st_titulo within w_info_rendimiento_packing
integer width = 1454
string text = "Informe Rendimiento Packing"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_rendimiento_packing
string tag = "Imprimir Reporte"
integer x = 1810
integer y = 468
integer taborder = 90
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		Fila
Date		ld_desde, ld_hasta
Integer	li_Porcentaje

istr_info.titulo	= "INFORME RENDIMIENTO PACKING"
istr_info.copias	= 1

ld_desde			=	Date(em_desde.Text)
ld_hasta			=	Date(em_hasta.Text)
li_Porcentaje	=	Integer(em_porcen.Text)

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_rdto_personal_packing"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Dec(uo_SelPlanta.Codigo), Dec(uo_SelCliente.codigo), ld_desde, ld_hasta, &
								Dec(uo_sellinea.codigo), Dec(li_Porcentaje))

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

type pb_salir from w_para_informes`pb_salir within w_info_rendimiento_packing
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1806
integer y = 728
integer taborder = 100
string disabledname = "\desarrollo\bmp\exitd.bmp"
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_rendimiento_packing
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

type st_2 from statictext within w_info_rendimiento_packing
integer x = 1019
integer y = 1344
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

type uo_selcliente from uo_seleccion_clientesprod within w_info_rendimiento_packing
event destroy ( )
integer x = 704
integer y = 564
integer width = 891
integer height = 76
integer taborder = 10
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_3 from statictext within w_info_rendimiento_packing
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

type em_desde from editmask within w_info_rendimiento_packing
integer x = 590
integer y = 1328
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

type em_hasta from editmask within w_info_rendimiento_packing
integer x = 1225
integer y = 1328
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

type uo_sellinea from uo_seleccion_lineapacking within w_info_rendimiento_packing
event destroy ( )
integer x = 704
integer y = 916
integer width = 891
integer height = 76
integer taborder = 30
boolean bringtotop = true
end type

on uo_sellinea.destroy
call uo_seleccion_lineapacking::destroy
end on

type st_4 from statictext within w_info_rendimiento_packing
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

type uo_selplanta from uo_seleccion_plantas within w_info_rendimiento_packing
integer x = 704
integer y = 740
integer width = 891
integer height = 76
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;IF This.Codigo <> -1 THEN uo_selLinea.Filtra(This.Codigo)
end event

type st_porc from statictext within w_info_rendimiento_packing
integer x = 320
integer y = 1060
integer width = 343
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
string text = "% Medición"
boolean focusrectangle = false
end type

type em_porcen from editmask within w_info_rendimiento_packing
integer x = 704
integer y = 1048
integer width = 242
integer height = 92
integer taborder = 40
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
string mask = "###"
end type

type st_5 from statictext within w_info_rendimiento_packing
integer x = 343
integer y = 1348
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

type gb_3 from groupbox within w_info_rendimiento_packing
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
long backcolor = 16711680
end type

type gb_4 from groupbox within w_info_rendimiento_packing
integer x = 256
integer y = 1228
integer width = 1449
integer height = 268
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

