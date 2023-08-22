$PBExportHeader$w_info_rendimiento_cosechero.srw
forward
global type w_info_rendimiento_cosechero from w_para_informes
end type
type st_1 from statictext within w_info_rendimiento_cosechero
end type
type st_2 from statictext within w_info_rendimiento_cosechero
end type
type st_3 from statictext within w_info_rendimiento_cosechero
end type
type cbx_folio from checkbox within w_info_rendimiento_cosechero
end type
type em_desde from editmask within w_info_rendimiento_cosechero
end type
type em_hasta from editmask within w_info_rendimiento_cosechero
end type
type st_4 from statictext within w_info_rendimiento_cosechero
end type
type st_5 from statictext within w_info_rendimiento_cosechero
end type
type uo_selproductor from uo_seleccion_productor within w_info_rendimiento_cosechero
end type
type uo_selpredio from uo_seleccion_prodpredio within w_info_rendimiento_cosechero
end type
type uo_selcuarteles from uo_seleccion_prodcuarteles within w_info_rendimiento_cosechero
end type
type gb_3 from groupbox within w_info_rendimiento_cosechero
end type
type gb_4 from groupbox within w_info_rendimiento_cosechero
end type
end forward

global type w_info_rendimiento_cosechero from w_para_informes
integer x = 14
integer y = 32
integer width = 2112
integer height = 1536
string title = "Informe Productividad Horaria"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
st_3 st_3
cbx_folio cbx_folio
em_desde em_desde
em_hasta em_hasta
st_4 st_4
st_5 st_5
uo_selproductor uo_selproductor
uo_selpredio uo_selpredio
uo_selcuarteles uo_selcuarteles
gb_3 gb_3
gb_4 gb_4
end type
global w_info_rendimiento_cosechero w_info_rendimiento_cosechero

type variables
Integer	ii_horario
end variables

on w_info_rendimiento_cosechero.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.cbx_folio=create cbx_folio
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.st_4=create st_4
this.st_5=create st_5
this.uo_selproductor=create uo_selproductor
this.uo_selpredio=create uo_selpredio
this.uo_selcuarteles=create uo_selcuarteles
this.gb_3=create gb_3
this.gb_4=create gb_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.cbx_folio
this.Control[iCurrent+5]=this.em_desde
this.Control[iCurrent+6]=this.em_hasta
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.st_5
this.Control[iCurrent+9]=this.uo_selproductor
this.Control[iCurrent+10]=this.uo_selpredio
this.Control[iCurrent+11]=this.uo_selcuarteles
this.Control[iCurrent+12]=this.gb_3
this.Control[iCurrent+13]=this.gb_4
end on

on w_info_rendimiento_cosechero.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.cbx_folio)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.uo_selproductor)
destroy(this.uo_selpredio)
destroy(this.uo_selcuarteles)
destroy(this.gb_3)
destroy(this.gb_4)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelProductor.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelPredio.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelCuarteles.Codigo) THEN lb_Cerrar	=	True


IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelProductor.Seleccion(True,True)
	uo_SelPredio.Seleccion(True,True)
	uo_SelCuarteles.Seleccion(True,True)

	em_desde.Text	=	String (today(), 'dd/mm/yyyy')
	em_hasta.Text	=	String (today(), 'dd/mm/yyyy')
End If

end event

type pb_excel from w_para_informes`pb_excel within w_info_rendimiento_cosechero
end type

type st_computador from w_para_informes`st_computador within w_info_rendimiento_cosechero
end type

type st_usuario from w_para_informes`st_usuario within w_info_rendimiento_cosechero
end type

type st_temporada from w_para_informes`st_temporada within w_info_rendimiento_cosechero
end type

type p_logo from w_para_informes`p_logo within w_info_rendimiento_cosechero
end type

type st_titulo from w_para_informes`st_titulo within w_info_rendimiento_cosechero
integer width = 1454
string text = "Informe Rendimiento Cosecheros"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_rendimiento_cosechero
string tag = "Imprimir Reporte"
integer x = 1806
integer y = 480
integer taborder = 100
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		Fila
Date		ldt_desde, ldt_hasta
Integer	li_Folio
String 	ls_horario, ls_rango

istr_info.titulo	= "INFORME PRODUCTIVIDAD HORARIA"
istr_info.copias	= 1

ldt_desde	=	Date(em_desde.Text)
ldt_hasta		=	Date(em_hasta.Text)

If cbx_folio.Checked Then
	li_Folio = -9
Else
	li_Folio = -1
End If

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_rendimientopersonalcosechero"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_SelProductor.Codigo, uo_SelPredio.Codigo, uo_SelCuarteles.Codigo, ldt_desde, ldt_hasta)

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

type pb_salir from w_para_informes`pb_salir within w_info_rendimiento_cosechero
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1801
integer y = 740
integer taborder = 110
string disabledname = "\desarrollo\bmp\exitd.bmp"
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_rendimiento_cosechero
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
string text = "Predio"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_rendimiento_cosechero
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

type st_3 from statictext within w_info_rendimiento_cosechero
integer x = 315
integer y = 580
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
string text = "Productor"
boolean focusrectangle = false
end type

type cbx_folio from checkbox within w_info_rendimiento_cosechero
boolean visible = false
integer x = 389
integer y = 1008
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
end type

type em_desde from editmask within w_info_rendimiento_cosechero
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

type em_hasta from editmask within w_info_rendimiento_cosechero
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

type st_4 from statictext within w_info_rendimiento_cosechero
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
string text = "Cuarteles"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_rendimiento_cosechero
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

type uo_selproductor from uo_seleccion_productor within w_info_rendimiento_cosechero
integer x = 718
integer y = 480
integer taborder = 110
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_selPredio.Filtra(-1)
		
	Case Else
		uo_selPredio.Filtra(This.Codigo)
		
End Choose
end event

type uo_selpredio from uo_seleccion_prodpredio within w_info_rendimiento_cosechero
integer x = 718
integer y = 664
integer taborder = 120
boolean bringtotop = true
end type

on uo_selpredio.destroy
call uo_seleccion_prodpredio::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelCuarteles.Filtra(-1, -1)
		
	Case Else
		uo_SelCuarteles.Filtra(uo_SelProductor.Codigo, This.Codigo)
		
End Choose
end event

type uo_selcuarteles from uo_seleccion_prodcuarteles within w_info_rendimiento_cosechero
integer x = 718
integer y = 836
integer taborder = 120
boolean bringtotop = true
end type

on uo_selcuarteles.destroy
call uo_seleccion_prodcuarteles::destroy
end on

type gb_3 from groupbox within w_info_rendimiento_cosechero
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

type gb_4 from groupbox within w_info_rendimiento_cosechero
integer x = 251
integer y = 1064
integer width = 1449
integer height = 340
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
end type

