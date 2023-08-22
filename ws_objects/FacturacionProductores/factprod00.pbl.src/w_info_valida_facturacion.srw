$PBExportHeader$w_info_valida_facturacion.srw
$PBExportComments$Informe de Facturación Productor Mensual.
forward
global type w_info_valida_facturacion from w_para_informes
end type
type st_4 from statictext within w_info_valida_facturacion
end type
type st_1 from statictext within w_info_valida_facturacion
end type
type st_5 from statictext within w_info_valida_facturacion
end type
type st_2 from statictext within w_info_valida_facturacion
end type
type em_fecha from editmask within w_info_valida_facturacion
end type
type st_6 from statictext within w_info_valida_facturacion
end type
type st_8 from statictext within w_info_valida_facturacion
end type
type em_cambio from editmask within w_info_valida_facturacion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_valida_facturacion
end type
type uo_selplanta from uo_seleccion_plantas within w_info_valida_facturacion
end type
type uo_selproductor from uo_seleccion_productor within w_info_valida_facturacion
end type
type st_3 from statictext within w_info_valida_facturacion
end type
type st_10 from statictext within w_info_valida_facturacion
end type
type em_desde from editmask within w_info_valida_facturacion
end type
type st_11 from statictext within w_info_valida_facturacion
end type
type em_hasta from editmask within w_info_valida_facturacion
end type
type cbx_consolida from checkbox within w_info_valida_facturacion
end type
end forward

global type w_info_valida_facturacion from w_para_informes
integer x = 14
integer y = 32
integer width = 2789
integer height = 1608
string title = "Facturación Mensual de Productores"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_5 st_5
st_2 st_2
em_fecha em_fecha
st_6 st_6
st_8 st_8
em_cambio em_cambio
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
uo_selproductor uo_selproductor
st_3 st_3
st_10 st_10
em_desde em_desde
st_11 st_11
em_hasta em_hasta
cbx_consolida cbx_consolida
end type
global w_info_valida_facturacion w_info_valida_facturacion

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_zona, idwc_planta, idwc_cliente, idwc_productor
end variables

on w_info_valida_facturacion.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_5=create st_5
this.st_2=create st_2
this.em_fecha=create em_fecha
this.st_6=create st_6
this.st_8=create st_8
this.em_cambio=create em_cambio
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.uo_selproductor=create uo_selproductor
this.st_3=create st_3
this.st_10=create st_10
this.em_desde=create em_desde
this.st_11=create st_11
this.em_hasta=create em_hasta
this.cbx_consolida=create cbx_consolida
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_fecha
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.st_8
this.Control[iCurrent+8]=this.em_cambio
this.Control[iCurrent+9]=this.uo_selcliente
this.Control[iCurrent+10]=this.uo_selplanta
this.Control[iCurrent+11]=this.uo_selproductor
this.Control[iCurrent+12]=this.st_3
this.Control[iCurrent+13]=this.st_10
this.Control[iCurrent+14]=this.em_desde
this.Control[iCurrent+15]=this.st_11
this.Control[iCurrent+16]=this.em_hasta
this.Control[iCurrent+17]=this.cbx_consolida
end on

on w_info_valida_facturacion.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_fecha)
destroy(this.st_6)
destroy(this.st_8)
destroy(this.em_cambio)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.uo_selproductor)
destroy(this.st_3)
destroy(this.st_10)
destroy(this.em_desde)
destroy(this.st_11)
destroy(this.em_hasta)
destroy(this.cbx_consolida)
end on

event open;call super::open;Boolean lb_Cerrar = False

If isNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If isNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If isNull(uo_SelProductor.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelProductor.Seleccion(True, True)
	uo_SelProductor.Filtra(-1)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gi_Codplanta)
	
	em_Fecha.Text				=	String(Today())
	em_desde.Text				=	'01/' + em_Fecha.Text
	em_hasta.Text				=	String(Today())
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_valida_facturacion
end type

type st_computador from w_para_informes`st_computador within w_info_valida_facturacion
end type

type st_usuario from w_para_informes`st_usuario within w_info_valida_facturacion
end type

type st_temporada from w_para_informes`st_temporada within w_info_valida_facturacion
end type

type p_logo from w_para_informes`p_logo within w_info_valida_facturacion
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_valida_facturacion
integer width = 1897
string text = "Informe de Validacion Facturación Mensual de Productores"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_valida_facturacion
integer x = 2258
integer y = 876
integer taborder = 100
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	ll_Filas, li_Consolida = 0

DataWindowChild	ldwc_planta

istr_info.titulo	= 'VALIDACION FACTURACION MENSUAL DE PRODUCTORES'

If cbx_consolida.Checked Then li_Consolida = 1

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_validaproforma"
vinf.dw_1.SetTransObject(sqlca)

ll_Filas	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Date('01/' + em_fecha.Text), -1, Datetime(em_desde.Text), Datetime(em_hasta.text), &
					Dec(em_cambio.Text), uo_SelProductor.Codigo, li_Consolida)

IF ll_Filas = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF ll_Filas = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)

ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_valida_facturacion
integer x = 2254
integer y = 1216
integer taborder = 110
end type

type st_4 from statictext within w_info_valida_facturacion
integer x = 251
integer y = 444
integer width = 1897
integer height = 704
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_valida_facturacion
integer x = 347
integer y = 644
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_valida_facturacion
integer x = 251
integer y = 1152
integer width = 1897
integer height = 308
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_valida_facturacion
integer x = 347
integer y = 788
integer width = 485
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
string text = "Mes de Proceso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fecha from editmask within w_info_valida_facturacion
integer x = 855
integer y = 772
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

type st_6 from statictext within w_info_valida_facturacion
integer x = 347
integer y = 512
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

type st_8 from statictext within w_info_valida_facturacion
integer x = 1271
integer y = 792
integer width = 411
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
string text = "Valor Cambio"
boolean focusrectangle = false
end type

type em_cambio from editmask within w_info_valida_facturacion
integer x = 1687
integer y = 776
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

type uo_selcliente from uo_seleccion_clientesprod within w_info_valida_facturacion
integer x = 855
integer y = 496
integer height = 96
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_valida_facturacion
integer x = 855
integer y = 632
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_valida_facturacion
integer x = 855
integer y = 1228
integer taborder = 120
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type st_3 from statictext within w_info_valida_facturacion
integer x = 334
integer y = 1312
integer width = 485
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
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_valida_facturacion
integer x = 347
integer y = 916
integer width = 485
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
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_valida_facturacion
integer x = 855
integer y = 900
integer width = 379
integer height = 96
integer taborder = 60
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
string mask = "dd/mm/yyyy"
end type

type st_11 from statictext within w_info_valida_facturacion
integer x = 1495
integer y = 916
integer width = 187
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
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_valida_facturacion
integer x = 1701
integer y = 900
integer width = 379
integer height = 96
integer taborder = 70
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
string mask = "dd/mm/yyyy"
end type

type cbx_consolida from checkbox within w_info_valida_facturacion
integer x = 347
integer y = 1044
integer width = 567
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
boolean lefttext = true
end type

