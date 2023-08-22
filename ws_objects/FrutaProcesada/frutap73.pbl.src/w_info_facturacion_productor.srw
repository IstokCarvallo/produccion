$PBExportHeader$w_info_facturacion_productor.srw
$PBExportComments$Informe de Facturación Productor Mensual.
forward
global type w_info_facturacion_productor from w_para_informes
end type
type gb_3 from groupbox within w_info_facturacion_productor
end type
type st_4 from statictext within w_info_facturacion_productor
end type
type st_1 from statictext within w_info_facturacion_productor
end type
type st_5 from statictext within w_info_facturacion_productor
end type
type st_2 from statictext within w_info_facturacion_productor
end type
type em_fecha from editmask within w_info_facturacion_productor
end type
type dw_cliente from datawindow within w_info_facturacion_productor
end type
type st_6 from statictext within w_info_facturacion_productor
end type
type dw_planta from datawindow within w_info_facturacion_productor
end type
type st_3 from statictext within w_info_facturacion_productor
end type
type dw_zona from datawindow within w_info_facturacion_productor
end type
type cbx_zona from checkbox within w_info_facturacion_productor
end type
type st_8 from statictext within w_info_facturacion_productor
end type
type st_9 from statictext within w_info_facturacion_productor
end type
type em_valorcambio from editmask within w_info_facturacion_productor
end type
type em_iva from editmask within w_info_facturacion_productor
end type
type dw_productor from datawindow within w_info_facturacion_productor
end type
type st_7 from statictext within w_info_facturacion_productor
end type
type rb_consolidado from radiobutton within w_info_facturacion_productor
end type
type rb_todos from radiobutton within w_info_facturacion_productor
end type
type rb_uno from radiobutton within w_info_facturacion_productor
end type
end forward

global type w_info_facturacion_productor from w_para_informes
integer x = 14
integer y = 32
integer width = 2665
integer height = 2136
string title = "Facturación Mensual de Productores"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
gb_3 gb_3
st_4 st_4
st_1 st_1
st_5 st_5
st_2 st_2
em_fecha em_fecha
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_3 st_3
dw_zona dw_zona
cbx_zona cbx_zona
st_8 st_8
st_9 st_9
em_valorcambio em_valorcambio
em_iva em_iva
dw_productor dw_productor
st_7 st_7
rb_consolidado rb_consolidado
rb_todos rb_todos
rb_uno rb_uno
end type
global w_info_facturacion_productor w_info_facturacion_productor

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_zona, idwc_planta, idwc_cliente, idwc_productor
end variables

on w_info_facturacion_productor.create
int iCurrent
call super::create
this.gb_3=create gb_3
this.st_4=create st_4
this.st_1=create st_1
this.st_5=create st_5
this.st_2=create st_2
this.em_fecha=create em_fecha
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_3=create st_3
this.dw_zona=create dw_zona
this.cbx_zona=create cbx_zona
this.st_8=create st_8
this.st_9=create st_9
this.em_valorcambio=create em_valorcambio
this.em_iva=create em_iva
this.dw_productor=create dw_productor
this.st_7=create st_7
this.rb_consolidado=create rb_consolidado
this.rb_todos=create rb_todos
this.rb_uno=create rb_uno
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_3
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_5
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.em_fecha
this.Control[iCurrent+7]=this.dw_cliente
this.Control[iCurrent+8]=this.st_6
this.Control[iCurrent+9]=this.dw_planta
this.Control[iCurrent+10]=this.st_3
this.Control[iCurrent+11]=this.dw_zona
this.Control[iCurrent+12]=this.cbx_zona
this.Control[iCurrent+13]=this.st_8
this.Control[iCurrent+14]=this.st_9
this.Control[iCurrent+15]=this.em_valorcambio
this.Control[iCurrent+16]=this.em_iva
this.Control[iCurrent+17]=this.dw_productor
this.Control[iCurrent+18]=this.st_7
this.Control[iCurrent+19]=this.rb_consolidado
this.Control[iCurrent+20]=this.rb_todos
this.Control[iCurrent+21]=this.rb_uno
end on

on w_info_facturacion_productor.destroy
call super::destroy
destroy(this.gb_3)
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_fecha)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.dw_zona)
destroy(this.cbx_zona)
destroy(this.st_8)
destroy(this.st_9)
destroy(this.em_valorcambio)
destroy(this.em_iva)
destroy(this.dw_productor)
destroy(this.st_7)
destroy(this.rb_consolidado)
destroy(this.rb_todos)
destroy(this.rb_uno)
end on

event open;call super::open;dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)

dw_zona.GetChild("zona_codigo", idwc_zona)
idwc_zona.SetTransObject(sqlca)
idwc_zona.Retrieve()
dw_zona.InsertRow(0)

dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(gi_CodExport, 0)
dw_productor.InsertRow(0)

em_Fecha.Text				=	String(Today())
em_iva.Text					=	"18"
istr_mant.argumento[1]	= 	String(gi_CodExport)
istr_mant.argumento[2]	= 	String(gi_CodPlanta)
istr_mant.argumento[3]	= 	em_Fecha.Text
istr_mant.argumento[4]	=	"18"		// I.V.A.
istr_mant.argumento[5]	=	"0"		//	Valor Cambio
istr_mant.argumento[6]	=	"0"		//	Código de Zona
istr_mant.argumento[7]	=	"0"		//	Código de Productor
istr_mant.argumento[8]	=	"0"		//	Consolidado Productores

dw_zona.Object.zona_codigo.BackGround.Color			=	RGB(166,180,210)
dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(166,180,210)
end event

type st_computador from w_para_informes`st_computador within w_info_facturacion_productor
end type

type st_usuario from w_para_informes`st_usuario within w_info_facturacion_productor
end type

type st_temporada from w_para_informes`st_temporada within w_info_facturacion_productor
end type

type p_logo from w_para_informes`p_logo within w_info_facturacion_productor
end type

type st_titulo from w_para_informes`st_titulo within w_info_facturacion_productor
integer width = 1897
string text = "Informe de Facturación Mensual de Productores"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_facturacion_productor
string tag = "Imprimir Reporte"
integer x = 2299
integer y = 1184
integer taborder = 100
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	ll_Filas, li_Cliente, li_Planta, li_Zona,li_Consolidado
Date		ld_MesProceso
Decimal{2}	ld_PorIVA, ld_ValCambio
Long		ll_Productor

DataWindowChild	ldwc_planta

istr_info.titulo	= 'FACTURACION MENSUAL DE PRODUCTORES'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_facturacion_productor"

li_Cliente 		=	Integer(istr_mant.argumento[1])
li_Planta		=	Integer(istr_mant.argumento[2])
ld_MesProceso 	= 	Date("01/" + em_Fecha.Text)
ld_PorIVA		=	Dec(em_iva.Text)
ld_ValCambio	=	Dec(em_ValorCambio.Text)
li_Zona			=	Integer(istr_mant.argumento[6])
ll_Productor	=	Long(istr_mant.argumento[7])
li_Consolidado	=	Integer(istr_mant.argumento[8])

vinf.dw_1.GetChild("plde_codigo", ldwc_planta)
ldwc_planta.SetTransObject(sqlca)
ldwc_planta.Retrieve(li_Cliente)

vinf.dw_1.SetTransObject(sqlca)

ll_Filas	=	vinf.dw_1.Retrieve(li_Cliente, li_Planta, ld_MesProceso, &
										li_Zona, ll_Productor, li_Consolidado, &
										ld_ValCambio, 0, ld_PorIVA)

IF ll_Filas = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF ll_Filas = 0 THEN
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

type pb_salir from w_para_informes`pb_salir within w_info_facturacion_productor
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2299
integer y = 1524
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_3 from groupbox within w_info_facturacion_productor
integer x = 343
integer y = 1476
integer width = 1719
integer height = 336
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Productor"
end type

type st_4 from statictext within w_info_facturacion_productor
integer x = 247
integer y = 424
integer width = 1897
integer height = 704
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_facturacion_productor
integer x = 343
integer y = 612
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_facturacion_productor
integer x = 247
integer y = 1132
integer width = 1897
integer height = 308
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_facturacion_productor
integer x = 343
integer y = 740
integer width = 485
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Mes de Proceso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fecha from editmask within w_info_facturacion_productor
integer x = 850
integer y = 724
integer width = 393
integer height = 96
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "mm/yyyy"
end type

event modified;istr_mant.argumento[3]	=	This.Text
end event

type dw_cliente from datawindow within w_info_facturacion_productor
integer x = 850
integer y = 484
integer width = 1225
integer height = 92
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	idwc_planta.Retrieve(Integer(istr_mant.argumento[1]),1)
	istr_mant.argumento[2]	=	String(dw_planta.Object.plde_codigo[1])
	dw_planta.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_facturacion_productor
integer x = 343
integer y = 492
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_facturacion_productor
integer x = 850
integer y = 604
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2]	=	data
ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_info_facturacion_productor
integer x = 343
integer y = 1204
integer width = 462
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Zona"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_zona from datawindow within w_info_facturacion_productor
integer x = 850
integer y = 1292
integer width = 850
integer height = 92
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_zonas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.Argumento[6]	=	data
end event

event itemerror;RETURN 1
end event

type cbx_zona from checkbox within w_info_facturacion_productor
integer x = 855
integer y = 1196
integer width = 402
integer height = 80
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todas"
boolean checked = true
end type

event clicked;Integer	li_Nula

SetNull(li_Nula)

IF This.Checked THEN
	rb_todos.Checked			=	True
	rb_uno.Enabled				=	False
	rb_uno.Checked				=	False
	dw_zona.Enabled			=	False
	dw_productor.Enabled		=	False
	istr_mant.argumento[6]	=	'0'
	istr_mant.argumento[7]	=	'0'
	istr_mant.argumento[8]	=	'0'
	
	dw_zona.SetItem(1, "zona_codigo", li_Nula)
	dw_productor.SetItem(1, "prod_codigo", li_Nula)
	dw_zona.Object.zona_codigo.BackGround.Color			=	RGB(166,180,210)
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	rb_uno.Enabled				=	True
	dw_zona.Enabled			=	True
	
	dw_zona.Object.zona_codigo.BackGround.Color			=	RGB(255,255,255)
	dw_zona.SetFocus()
END IF
end event

type st_8 from statictext within w_info_facturacion_productor
integer x = 343
integer y = 864
integer width = 411
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Valor Cambio"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_facturacion_productor
integer x = 343
integer y = 988
integer width = 503
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Porcentaje I.V.A."
boolean focusrectangle = false
end type

type em_valorcambio from editmask within w_info_facturacion_productor
integer x = 850
integer y = 848
integer width = 393
integer height = 96
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "##,###.##"
end type

type em_iva from editmask within w_info_facturacion_productor
integer x = 850
integer y = 972
integer width = 393
integer height = 96
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "#0.00"
end type

type dw_productor from datawindow within w_info_facturacion_productor
integer x = 850
integer y = 1664
integer width = 1106
integer height = 92
integer taborder = 90
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_productores_zona"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.Argumento[7]	=	data
end event

type st_7 from statictext within w_info_facturacion_productor
integer x = 247
integer y = 1440
integer width = 1897
integer height = 440
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_consolidado from radiobutton within w_info_facturacion_productor
integer x = 850
integer y = 1552
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
long backcolor = 33543637
string text = "Consolidado"
end type

event clicked;istr_mant.argumento[7]	=	'0'
istr_mant.argumento[8]	=	'1'
dw_productor.Enabled		=	False

dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(166,180,210)
end event

type rb_todos from radiobutton within w_info_facturacion_productor
integer x = 416
integer y = 1552
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;istr_mant.argumento[7]	=	'0'
istr_mant.argumento[8]	=	'0'
dw_productor.Enabled		=	False

dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(166,180,210)
end event

type rb_uno from radiobutton within w_info_facturacion_productor
integer x = 1449
integer y = 1552
integer width = 507
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Uno Específico"
end type

event clicked;istr_mant.argumento[7]	=	'0'
istr_mant.argumento[8]	=	'0'
dw_productor.Enabled		=	True

dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(255,255,255)
end event

