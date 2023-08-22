$PBExportHeader$w_info_cambioalturacajas.srw
$PBExportComments$Informe de Revisión Repalletizajes.
forward
global type w_info_cambioalturacajas from w_para_informes
end type
type st_4 from statictext within w_info_cambioalturacajas
end type
type st_1 from statictext within w_info_cambioalturacajas
end type
type st_6 from statictext within w_info_cambioalturacajas
end type
type st_2 from statictext within w_info_cambioalturacajas
end type
type em_desde from editmask within w_info_cambioalturacajas
end type
type em_hasta from editmask within w_info_cambioalturacajas
end type
type st_9 from statictext within w_info_cambioalturacajas
end type
type st_10 from statictext within w_info_cambioalturacajas
end type
type gb_3 from groupbox within w_info_cambioalturacajas
end type
type st_5 from statictext within w_info_cambioalturacajas
end type
type st_3 from statictext within w_info_cambioalturacajas
end type
type uo_SelCliente from uo_seleccion_clientesprod within w_info_cambioalturacajas
end type
type uo_SelPlanta from uo_seleccion_plantas within w_info_cambioalturacajas
end type
type uo_SelEspecie from uo_seleccion_especie within w_info_cambioalturacajas
end type
type uo_SelProductor from uo_seleccion_productor within w_info_cambioalturacajas
end type
end forward

global type w_info_cambioalturacajas from w_para_informes
integer x = 14
integer y = 32
integer width = 2235
integer height = 1716
string title = "Informe Comparativo Productor-Calibre"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_6 st_6
st_2 st_2
em_desde em_desde
em_hasta em_hasta
st_9 st_9
st_10 st_10
gb_3 gb_3
st_5 st_5
st_3 st_3
uo_SelCliente uo_SelCliente
uo_SelPlanta uo_SelPlanta
uo_SelEspecie uo_SelEspecie
uo_SelProductor uo_SelProductor
end type
global w_info_cambioalturacajas w_info_cambioalturacajas

type variables

end variables

on w_info_cambioalturacajas.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_6=create st_6
this.st_2=create st_2
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.st_9=create st_9
this.st_10=create st_10
this.gb_3=create gb_3
this.st_5=create st_5
this.st_3=create st_3
this.uo_SelCliente=create uo_SelCliente
this.uo_SelPlanta=create uo_SelPlanta
this.uo_SelEspecie=create uo_SelEspecie
this.uo_SelProductor=create uo_SelProductor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_desde
this.Control[iCurrent+6]=this.em_hasta
this.Control[iCurrent+7]=this.st_9
this.Control[iCurrent+8]=this.st_10
this.Control[iCurrent+9]=this.gb_3
this.Control[iCurrent+10]=this.st_5
this.Control[iCurrent+11]=this.st_3
this.Control[iCurrent+12]=this.uo_SelCliente
this.Control[iCurrent+13]=this.uo_SelPlanta
this.Control[iCurrent+14]=this.uo_SelEspecie
this.Control[iCurrent+15]=this.uo_SelProductor
end on

on w_info_cambioalturacajas.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_6)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.st_3)
destroy(this.uo_SelCliente)
destroy(this.uo_SelPlanta)
destroy(this.uo_SelEspecie)
destroy(this.uo_SelProductor)
end on

event open;call super::open;Boolean lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar = True


If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelEspecie.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	
	uo_SelCliente.Inicia(gi_CodExport)
	
	em_desde.text	=	String(RelativeDate(Today() , -365))
	em_Hasta.text	=	String(Today())	
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_cambioalturacajas
integer x = 2117
integer y = 620
end type

type st_computador from w_para_informes`st_computador within w_info_cambioalturacajas
end type

type st_usuario from w_para_informes`st_usuario within w_info_cambioalturacajas
end type

type st_temporada from w_para_informes`st_temporada within w_info_cambioalturacajas
end type

type p_logo from w_para_informes`p_logo within w_info_cambioalturacajas
end type

type st_titulo from w_para_informes`st_titulo within w_info_cambioalturacajas
integer width = 1431
string text = "Informe Comparativo Productor-Calibre"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_cambioalturacajas
string tag = "Imprimir Reporte"
integer x = 1778
integer y = 688
integer taborder = 70
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila

istr_info.titulo	= 'INFORME COMPARATIVO CAMBIO PRODUCTOR / CALIBRE'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_cambioalturacajas"
vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelEspecie.Codigo, uo_SelPlanta.Codigo,&
							Date(em_Desde.text),Date(em_Hasta.text), uo_SelProductor.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("t_desde.text = '" + em_Desde.text + "'")
	vinf.dw_1.Modify("t_hasta.text = '" + em_Hasta.text + "'")
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF


SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_cambioalturacajas
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1774
integer y = 1008
integer taborder = 80
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_cambioalturacajas
integer x = 251
integer y = 440
integer width = 1431
integer height = 764
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

type st_1 from statictext within w_info_cambioalturacajas
integer x = 302
integer y = 708
integer width = 306
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_cambioalturacajas
integer x = 302
integer y = 524
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_cambioalturacajas
integer x = 302
integer y = 900
integer width = 274
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_cambioalturacajas
integer x = 544
integer y = 1324
integer width = 402
integer height = 96
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type em_hasta from editmask within w_info_cambioalturacajas
integer x = 1179
integer y = 1324
integer width = 402
integer height = 96
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_9 from statictext within w_info_cambioalturacajas
integer x = 352
integer y = 1336
integer width = 247
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_cambioalturacajas
integer x = 978
integer y = 1336
integer width = 229
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Hasta"
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_cambioalturacajas
integer x = 302
integer y = 1228
integer width = 1330
integer height = 244
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Proceso"
end type

type st_5 from statictext within w_info_cambioalturacajas
integer x = 251
integer y = 1200
integer width = 1431
integer height = 344
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

type st_3 from statictext within w_info_cambioalturacajas
integer x = 302
integer y = 1092
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type uo_SelCliente from uo_seleccion_clientesprod within w_info_cambioalturacajas
event destroy ( )
integer x = 613
integer y = 500
integer height = 96
integer taborder = 40
boolean bringtotop = true
end type

on uo_SelCliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_SelPlanta from uo_seleccion_plantas within w_info_cambioalturacajas
event destroy ( )
integer x = 613
integer y = 616
integer taborder = 50
boolean bringtotop = true
end type

on uo_SelPlanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_SelEspecie from uo_seleccion_especie within w_info_cambioalturacajas
event destroy ( )
integer x = 613
integer y = 808
integer taborder = 80
boolean bringtotop = true
end type

on uo_SelEspecie.destroy
call uo_seleccion_especie::destroy
end on

type uo_SelProductor from uo_seleccion_productor within w_info_cambioalturacajas
event destroy ( )
integer x = 613
integer y = 1004
integer taborder = 90
boolean bringtotop = true
end type

on uo_SelProductor.destroy
call uo_seleccion_productor::destroy
end on

